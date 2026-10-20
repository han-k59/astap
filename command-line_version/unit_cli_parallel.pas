unit unit_cli_parallel;
{Row-range parallel helper for the ASTAP command line solver.
 A persistent pool of worker threads (created on first use) splits a row range
 over the available cores. Workers block on RTL events, so a call costs
 microseconds, unlike TThread.Create/WaitFor which polls at 100 ms steps on Unix.
 Thread count follows the ASTAP_THREADS environment variable
 (default: processor count, max 16).

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

implementation

type
  Tworker = record
    go, done  : PRTLEvent;
    s, e      : integer;
    proc      : TRowProc;
    data      : pointer;
    quit      : boolean;
    handle    : TThreadID;
  end;
  Pworker = ^Tworker;

var
  thread_count : integer = 0;
  workers      : array of Tworker;
  pool_ready   : boolean = false;
  timing_state : integer = -1;

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

function online_cpu_count: integer;
{TThread.ProcessorCount returns 1 on FPC 3.2 Linux, so ask libc for the online core count}
{$IFDEF UNIX}
var
  n : clong;
{$ENDIF}
begin
  result:=TThread.ProcessorCount;
  {$IFDEF UNIX}
  {$IFDEF DARWIN}
  n:=sysconf(58); {_SC_NPROCESSORS_ONLN on macOS}
  {$ELSE}
  n:=sysconf(84); {_SC_NPROCESSORS_ONLN on Linux}
  {$ENDIF}
  if n>0 then result:=n;
  {$ENDIF}
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
  repeat
    RTLEventWaitFor(w^.go);
    RTLEventResetEvent(w^.go);
    if w^.quit then break;
    w^.proc(w^.s, w^.e, w^.data);
    RTLEventSetEvent(w^.done);
  until false;
  result:=0;
end;

procedure start_pool(n: integer);
var
  i : integer;
begin
  setlength(workers, n-1); {the calling thread does chunk 0 itself}
  for i:=0 to n-2 do
  begin
    workers[i].go:=RTLEventCreate;
    workers[i].done:=RTLEventCreate;
    workers[i].quit:=false;
    workers[i].handle:=BeginThread(@worker_main, @workers[i]);
  end;
  pool_ready:=true;
end;

procedure parallel_rows(nrows: integer; proc: TRowProc; data: pointer);
var
  n, i, chunk, s, e : integer;
begin
  if nrows<=0 then exit;
  n:=cli_thread_count;
  if ((n<=1) or (nrows<n*16)) then
  begin
    proc(0, nrows-1, data);
    exit;
  end;
  if not pool_ready then start_pool(n);
  chunk:=(nrows+n-1) div n;
  for i:=1 to n-1 do {hand out chunks 1..n-1 to the workers}
  begin
    s:=i*chunk;
    e:=min(nrows-1, s+chunk-1);
    workers[i-1].s:=s;
    workers[i-1].e:=e;
    workers[i-1].proc:=proc;
    workers[i-1].data:=data;
    if s<=e then RTLEventSetEvent(workers[i-1].go)
    else RTLEventSetEvent(workers[i-1].done); {nothing to do for this worker}
  end;
  proc(0, min(nrows-1, chunk-1), data); {chunk 0 on the calling thread}
  for i:=0 to n-2 do
  begin
    RTLEventWaitFor(workers[i].done);
    RTLEventResetEvent(workers[i].done);
  end;
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

end.
