unit unit_cli_parallel;
{Row-range parallel helper for the ASTAP command line solver.
 A persistent pool of worker threads (created on first use) splits a row range
 over the available cores. Workers block on RTL events, so a call costs
 microseconds, unlike TThread.Create/WaitFor which polls at 100 ms steps on Unix.

 Thread count: the -threads option, else the ASTAP_THREADS environment variable,
 else the number of cores this process may run on (CPU affinity on Linux, so
 taskset, Docker --cpuset-cpus and cgroup cpusets are respected; a CFS quota such
 as Docker --cpus does not change the mask, use -threads for that), capped at 16.

 Errors: an exception inside a worker is caught, the worker still signals
 completion, and the first exception object is re-raised unchanged on the calling
 thread after all chunks are done. So callers see the same exception class and
 message they would see in serial code.

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
procedure parallel_each_thread(proc: TRowProc; data: pointer); {run proc(0,0,data) on the calling thread and proc(i,i,data) on worker i, once per pool thread, for per-thread (threadvar) state. Returns when all are done}
procedure parallel_tasks(ntasks: integer; proc: TTaskProc; data: pointer); {run proc(0..ntasks-1) on all threads in index order; a task returning true stops tasks with a higher index. Returns when all started tasks are done}
function cli_thread_count: integer;
procedure set_thread_count(n: integer); {from the -threads option; 0 = automatic}
function cli_timing: boolean; {ASTAP_TIMING=1 in the environment prints phase timings to stderr}
function cli_pool_running: boolean; {true once worker threads exist. Lets callers skip a parallel_rows call whose only purpose is to touch per-thread state}
procedure cli_log_lock;   {serialise console/log output from worker threads}
procedure cli_log_unlock;

implementation

type
  Tworker = record
    go, done  : PRTLEvent;
    s, e      : integer;
    proc      : TRowProc;
    data      : pointer;
    exc       : TObject; {exception object of the last chunk, nil if none. Owned until re-raised or freed by parallel_rows}
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

function cli_pool_running: boolean;
begin
  result:=pool_ready;
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
  {_SC_NPROCESSORS_ONLN differs per libc: glibc/musl 84, bionic (Android) $61, Darwin and FreeBSD 58.
   Other systems (OpenBSD 503, NetBSD 1002, Solaris 15) are not enumerated here and keep TThread.ProcessorCount}
  {$IF DEFINED(ANDROID)}
  n:=sysconf($61);
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
  n:=sysconf(58);
  {$ELSEIF DEFINED(LINUX)}
  n:=sysconf(84);
  {$ELSE}
  n:=0;
  {$ENDIF}
  if n>0 then result:=n;

  {$IF DEFINED(LINUX) OR DEFINED(ANDROID)}
  {honour the affinity mask (taskset, Docker --cpuset-cpus, cgroup cpusets) when it is smaller than the online count}
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
    w^.exc:=nil;
    try
      w^.proc(w^.s, w^.e, w^.data);
    except
      w^.exc:=TObject(AcquireExceptionObject); {keep the object alive past this handler, parallel_rows re-raises it on the calling thread}
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
    workers[started].exc:=nil;
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

procedure run_chunks(nrows, chunk: integer; proc: TRowProc; data: pointer);
{rows [i*chunk, min(nrows-1,(i+1)*chunk-1)] on thread i: chunk 0 on the calling thread, chunk i on worker i-1. The pool must be running}
var
  n, i, s, e : integer;
  exc        : TObject;
begin
  n:=cli_thread_count;
  for i:=1 to n-1 do {hand out chunks 1..n-1 to the workers}
  begin
    s:=i*chunk;
    e:=min(nrows-1, s+chunk-1);
    workers[i-1].s:=s;
    workers[i-1].e:=e;
    workers[i-1].proc:=proc;
    workers[i-1].data:=data;
    workers[i-1].exc:=nil;
    if s<=e then RTLEventSetEvent(workers[i-1].go)
    else RTLEventSetEvent(workers[i-1].done); {nothing to do for this worker}
  end;
  exc:=nil;
  in_parallel:=true;
  try
    proc(0, min(nrows-1, chunk-1), data); {chunk 0 on the calling thread}
  except
    exc:=TObject(AcquireExceptionObject);
  end;
  in_parallel:=false;
  for i:=0 to n-2 do {wait for all workers, also after an error, so the pool is consistent for the next call}
  begin
    RTLEventWaitFor(workers[i].done);
    RTLEventResetEvent(workers[i].done);
    if workers[i].exc<>nil then
    begin
      if exc=nil then exc:=workers[i].exc {the first exception wins, as the serial code would have stopped there}
      else workers[i].exc.Free;
      workers[i].exc:=nil;
    end;
  end;
  if exc<>nil then
    raise exc; {the original object, so class and message are the same as in serial code. The RTL frees it when handled}
end;

function pool_available: boolean; {start the pool on first use. False when the work has to run on the calling thread}
var
  n : integer;
begin
  n:=cli_thread_count;
  if ((n<=1) or in_parallel) then exit(false); {no cores, or called from inside a chunk: the pool is not reentrant}
  if not pool_ready then
  begin
    start_pool(n);
    if cli_thread_count<=1 then exit(false); {no worker could be started}
  end;
  result:=true;
end;

procedure parallel_rows(nrows: integer; proc: TRowProc; data: pointer);
var
  n : integer;
begin
  if nrows<=0 then exit;
  n:=cli_thread_count;
  if ((nrows<n*16) or (not pool_available)) then
  begin
    proc(0, nrows-1, data); {serial}
    exit;
  end;
  n:=cli_thread_count; {may be smaller after start_pool when threads could not be started}
  run_chunks(nrows, (nrows+n-1) div n, proc, data);
end;

procedure parallel_each_thread(proc: TRowProc; data: pointer);
begin
  if not pool_available then
  begin
    proc(0, 0, data);
    exit;
  end;
  run_chunks(cli_thread_count, 1, proc, data); {one row per thread, so every pool thread runs proc exactly once}
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
  sh  : Ptaskshared;
  idx : longint;

  procedure lower_stop(v: longint); {lower sh^.stop_at to v unless somebody stopped earlier}
  var
    old : longint;
  begin
    repeat
      old:=sh^.stop_at;
      if v>=old then exit;
    until InterlockedCompareExchange(sh^.stop_at, v, old)=old;
  end;

begin
  sh:=Ptaskshared(data);
  repeat
    idx:=InterlockedIncrement(sh^.next)-1;
    if ((idx>=sh^.ntasks) or (idx>=sh^.stop_at)) then break;
    try
      if sh^.proc(idx, sh^.data) then lower_stop(idx);
    except
      lower_stop(0); {the other threads stop taking tasks, as the serial loop would have stopped here. run_chunks re-raises on the caller}
      raise;
    end;
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
  parallel_each_thread(@task_loop, @sh); {one task_loop per thread}
end;

initialization
  InitCriticalSection(log_lock);
finalization
  DoneCriticalSection(log_lock);
end.
