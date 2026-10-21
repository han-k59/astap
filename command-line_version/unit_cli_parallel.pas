unit unit_cli_parallel;
{Row-range parallel helper for the ASTAP command line solver.
 A persistent pool of worker threads (created on first use) splits a row range
 over the available cores. Workers block on RTL events, so a call costs
 microseconds, unlike TThread.Create/WaitFor which polls at 100 ms steps on Unix.

 Thread count: the -threads option, else the ASTAP_THREADS environment variable,
 else the number of cores this process may run on (CPU affinity on Linux, so
 taskset and Docker --cpus are respected), capped at 16.

 Errors: an exception inside a worker is caught, the worker still signals
 completion, and the first message is re-raised on the calling thread after all
 chunks are done. So callers see the same exception they would see in serial code.

 The pool is not reentrant: parallel_rows called from inside a running chunk
 runs the range serially on the calling thread.

 This Source Code Form is subject to the terms of the Mozilla Public
 License, v. 2.0. If a copy of the MPL was not distributed with this
 file, You can obtain one at https://mozilla.org/MPL/2.0/.   }

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, math{$IFDEF UNIX}, ctypes{$ENDIF};

type
  TRowProc = procedure(startRow, endRow: integer; data: pointer);

  TTaskProc = function(idx: integer; data: pointer): boolean; {return true to stop handing out tasks with a higher index}

procedure parallel_rows(nrows: integer; proc: TRowProc; data: pointer); {run proc over [0..nrows-1] split in chunks on all threads; returns when all chunks are done}
procedure parallel_tasks(ntasks: integer; proc: TTaskProc; data: pointer); {run proc(0..ntasks-1) on all threads in index order; a task returning true stops tasks with a higher index. Returns when all started tasks are done}
function cli_thread_count: integer;
procedure set_thread_count(n: integer); {from the -threads option; 0 = automatic}
function cli_timing: boolean; {ASTAP_TIMING=1 in the environment prints phase timings to stderr}
procedure cli_log_lock;   {serialise console/log output from worker threads}
procedure cli_log_unlock;

implementation

type
  Tworker = record
    go, done  : PRTLEvent;
    s, e      : integer;
    proc      : TRowProc;
    data      : pointer;
    err       : string; {exception message of the last chunk, '' if none}
    handle    : TThreadID;
  end;
  Pworker = ^Tworker;

var
  thread_count : integer = 0;
  workers      : array of Tworker;
  pool_ready   : boolean = false;
  timing_state : integer = -1;
  log_lock     : TRTLCriticalSection;

threadvar
  in_parallel  : boolean; {true while this thread executes a chunk, see the reentrancy guard}

procedure cli_log_lock;
begin
  EnterCriticalSection(log_lock);
end;

procedure cli_log_unlock;
begin
  LeaveCriticalSection(log_lock);
end;

procedure set_thread_count(n: integer);
begin
  if pool_ready then exit; {pool already running}
  if n<=0 then thread_count:=0 {automatic}
  else thread_count:=min(16,n);
end;

function cli_timing: boolean;
begin
  if timing_state<0 then
    if GetEnvironmentVariable('ASTAP_TIMING')='1' then timing_state:=1 else timing_state:=0;
  result:=(timing_state=1);
end;

{$IFDEF UNIX}
function sysconf(name: longint): clong; cdecl; external 'c' name 'sysconf';
{$ENDIF}

{$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
function sched_getaffinity(pid: cint; cpusetsize: csize_t; mask: pointer): cint; cdecl; external 'c' name 'sched_getaffinity';
{$ENDIF}

function online_cpu_count: integer;
{Cores this process may run on. TThread.ProcessorCount returns 1 on FPC 3.2 Linux, so ask the OS}
{$IFDEF UNIX}
var
  n     : clong;
  {$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  mask  : array[0..127] of byte; {1024 cpu bits}
  i, b  : integer;
  {$ENDIF}
{$ENDIF}
begin
  result:=TThread.ProcessorCount;
  {$IFDEF UNIX}
  {_SC_NPROCESSORS_ONLN differs per libc: glibc/musl 84, bionic (Android) $61, Darwin and FreeBSD 58}
  {$IF DEFINED(ANDROID)}
  n:=sysconf($61);
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
  n:=sysconf(58);
  {$ELSE}
  n:=sysconf(84);
  {$ENDIF}
  if n>0 then result:=n;

  {$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  {honour the affinity mask (taskset, Docker --cpus, cgroup cpusets) when it is smaller than the online count}
  fillchar(mask, sizeof(mask), 0);
  if sched_getaffinity(0, sizeof(mask), @mask[0])=0 then
  begin
    n:=0;
    for i:=0 to high(mask) do
      if mask[i]<>0 then
        for b:=0 to 7 do
          if (mask[i] and (1 shl b))<>0 then inc(n);
    if ((n>0) and (n<result)) then result:=n;
  end;
  {$ENDIF}
  {$ENDIF}
  if result<1 then result:=1;
end;

function cli_thread_count: integer;
var
  s : string;
  n : integer;
begin
  if thread_count=0 then
  begin
    thread_count:=online_cpu_count;
    s:=GetEnvironmentVariable('ASTAP_THREADS');
    if s<>'' then
    begin
      n:=StrToIntDef(s,0);
      if n>0 then thread_count:=n;
    end;
    if thread_count<1 then thread_count:=1;
    if thread_count>16 then thread_count:=16;
  end;
  result:=thread_count;
end;

function worker_main(p: pointer): ptrint;
var
  w : Pworker;
begin
  w:=Pworker(p);
  in_parallel:=true;
  repeat
    RTLEventWaitFor(w^.go);
    RTLEventResetEvent(w^.go);
    w^.err:='';
    try
      w^.proc(w^.s, w^.e, w^.data);
    except
      on E: Exception do w^.err:=E.ClassName+': '+E.Message
      else w^.err:='unknown exception';
    end;
    RTLEventSetEvent(w^.done); {always signal, else the caller waits forever}
  until false;
  result:=0;
end;

procedure start_pool(n: integer);
{start n-1 workers. If the OS refuses a thread (low memory, thread limit) the pool is simply smaller; with no worker at all the calls run serially}
var
  i, started : integer;
begin
  setlength(workers, n-1); {the calling thread does chunk 0 itself}
  started:=0;
  for i:=0 to n-2 do
  begin
    workers[started].go:=RTLEventCreate;
    workers[started].done:=RTLEventCreate;
    workers[started].err:='';
    workers[started].handle:=BeginThread(@worker_main, @workers[started]);
    if workers[started].handle<>TThreadID(0) then inc(started)
    else
    begin
      RTLEventDestroy(workers[started].go);
      RTLEventDestroy(workers[started].done);
    end;
  end;
  setlength(workers, started);
  thread_count:=started+1;
  pool_ready:=true;
end;

procedure parallel_rows(nrows: integer; proc: TRowProc; data: pointer);
var
  n, i, chunk, s, e : integer;
  err               : string;
begin
  if nrows<=0 then exit;
  n:=cli_thread_count;
  if ((n<=1) or (nrows<n*16) or in_parallel) then
  begin
    proc(0, nrows-1, data); {serial. Also when called from inside a chunk: the pool is not reentrant}
    exit;
  end;
  if not pool_ready then
  begin
    start_pool(n);
    n:=cli_thread_count; {may be smaller when threads could not be started}
    if n<=1 then
    begin
      proc(0, nrows-1, data);
      exit;
    end;
  end;
  chunk:=(nrows+n-1) div n;
  for i:=1 to n-1 do {hand out chunks 1..n-1 to the workers}
  begin
    s:=i*chunk;
    e:=min(nrows-1, s+chunk-1);
    workers[i-1].s:=s;
    workers[i-1].e:=e;
    workers[i-1].proc:=proc;
    workers[i-1].data:=data;
    workers[i-1].err:='';
    if s<=e then RTLEventSetEvent(workers[i-1].go)
    else RTLEventSetEvent(workers[i-1].done); {nothing to do for this worker}
  end;
  err:='';
  in_parallel:=true;
  try
    proc(0, min(nrows-1, chunk-1), data); {chunk 0 on the calling thread}
  except
    on E: Exception do err:=E.ClassName+': '+E.Message
    else err:='unknown exception';
  end;
  in_parallel:=false;
  for i:=0 to n-2 do {wait for all workers, also after an error, so the pool is consistent for the next call}
  begin
    RTLEventWaitFor(workers[i].done);
    RTLEventResetEvent(workers[i].done);
    if ((err='') and (workers[i].err<>'')) then err:=workers[i].err;
  end;
  if err<>'' then
    raise Exception.Create('Worker thread: '+err);
end;

type
  Ttaskshared = record
    next, stop_at : longint;
    ntasks        : integer;
    proc          : TTaskProc;
    data          : pointer;
  end;
  Ptaskshared = ^Ttaskshared;

procedure task_loop(startRow, endRow: integer; data: pointer);
var
  sh       : Ptaskshared;
  idx, old : longint;
begin
  sh:=Ptaskshared(data);
  repeat
    idx:=InterlockedIncrement(sh^.next)-1;
    if ((idx>=sh^.ntasks) or (idx>=sh^.stop_at)) then break;
    if sh^.proc(idx, sh^.data) then
      repeat {lower stop_at to idx unless somebody stopped earlier}
        old:=sh^.stop_at;
        if idx>=old then break;
      until InterlockedCompareExchange(sh^.stop_at, idx, old)=old;
  until false;
end;

procedure parallel_tasks(ntasks: integer; proc: TTaskProc; data: pointer);
var
  sh : Ttaskshared;
begin
  sh.next:=0;
  sh.stop_at:=high(longint);
  sh.ntasks:=ntasks;
  sh.proc:=proc;
  sh.data:=data;
  parallel_rows(cli_thread_count*16, @task_loop, @sh); {one task_loop per thread}
end;

initialization
  InitCriticalSection(log_lock);
finalization
  DoneCriticalSection(log_lock);
end.
