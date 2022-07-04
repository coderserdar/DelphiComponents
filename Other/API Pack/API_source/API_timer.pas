unit API_timer;

//------------------------------------------------------------------------------
// timer component that uses performance counter
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.03, 25.7.2006, ari pikivirta
//  * added restart function
//  * added setactive to the active property as possibility
//
// r1.02, 01042006, ari pikivirta
//  * changed component to work trough functions
//  * added onstart and onstop events
//  * added possibility to put offset to start point
//
// r1.01, ari pikivirta
// * removed htimer component (htimer replaced timer now)
// * start and stop is now done through setting active mode
// * all values are read (and write) through properties

interface

uses
  Windows, SysUtils, Classes, ExtCtrls;

type
  TAPI_timer_OnStart = procedure(sender: tcomponent; var StartMS: double) of object;
  TAPI_timer_OnStop = procedure(sender: tcomponent; ElapsedMS: double) of object;
  TAPI_timer_OnNotify = procedure(sender: tcomponent; ElapsedMS, IntervalMS: double; var ResetInterval: boolean) of object;

  TAPI_timer = class(TComponent)
  private
    fversion: string;
    factive: boolean;
    fonstart: TAPI_timer_OnStart;
    fonstop: TAPI_timer_OnStop;
    fonnotify: TAPI_timer_OnNotify;
    fc1: int64;
    fc2: int64;
    fc3: int64;
    ffr: int64;
    fstart: double;
    ftimer: ttimer;
    finterval: integer;
    procedure dummys(s: string);
    procedure intstart(st: double);
    function  intInterval: double;
    function  intElapsed: double;
    procedure intStop;
    procedure OnTimer(sender: tobject);              // internal checking..
    procedure setinterval(i: integer);
    procedure setactive(b: boolean);

  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    procedure Start; overload;                          // start timer
    procedure Start(StartMs: double); overload;         // start timer (set starting point)
    function  InstervalMs: double;                      // return interval, reset interval
    function  intInterval2: double;                     // return interval, do not reset interval
    function  ElapsedMs: double;                        // currently elapsed time
    function  Stop: double;                             // returns elapsed time in millisec
    function  ReStart: double;                          // restart timer

  published
    property Version: string read fversion write dummys stored false;
    property Active: boolean read factive write setactive;
    property NotifyInterval: integer read finterval write setinterval;
    property OnStart: TAPI_timer_OnStart read fonstart write fonstart;
    property OnStop: TAPI_timer_OnStop read fonstop write fonstop;
    property OnNotify: TAPI_Timer_OnNotify read fonnotify write fonnotify;
  end;

procedure Register;

implementation

{$WARN UNSAFE_CODE OFF}
{$r *.res}

uses
  dateutils;

const
  versioninfostring_timer: string = 'r1.03/ari.pikivirta(at)kolumbus.fi';

procedure TAPI_timer.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_timer.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring_timer;
  queryperformancefrequency(int64((@ffr)^));
  queryperformancecounter(int64((@fc1)^));
  fc2:=fc1;
  fc3:=0;
  factive:=false;
  finterval:= 10;
  ftimer:= ttimer.create(self);
  ftimer.Interval:= 1;
  ftimer.OnTimer:= OnTimer;
  ftimer.Interval:= finterval;
end;

//------------------------------------------------------------------------------
destructor TAPI_timer.destroy;
begin
  ftimer.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.setactive(b: boolean);
begin
  if not b then stop
    else start;
end;

//------------------------------------------------------------------------------
function TAPI_timer.ReStart: double;
begin
  result:= stop;
  start;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.setinterval(i: integer);
begin
  if i<>finterval then
  begin
    finterval:= i;
    if finterval>0 then
      ftimer.Interval:= finterval
      else ftimer.Enabled:= false;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.OnTimer;
var
  resetinterval: boolean;
begin
  if assigned(fonnotify) then
  begin
    resetinterval:= false;
    fonnotify(self, intElapsed, intInterval2, resetinterval);   // do not reset by default
    if resetinterval then IntInterval;                          // reset interval here
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.intstart(st: double);
begin
  if assigned(fonstart) then fonstart(self, st);
  factive:=true;
  fstart:= st;
  queryperformancefrequency(int64((@ffr)^));
  queryperformancecounter(int64((@fc1)^));
  fc2:=fc1;
  fc3:=0;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.intstop;
begin
  factive:=False;
  queryperformancecounter(int64((@fc2)^));
  fc3:=fc2;
end;

//------------------------------------------------------------------------------
function TAPI_timer.intInterval: double;
var
  ct: double;
  t: int64;
begin
  if factive then
  begin
    queryperformancecounter(int64((@t)^));
    ct:=(1000*(t-fc2)/ffr);
    fc2:=t;
    result:=ct + fstart;
  end else
    result:=0;
end;

//------------------------------------------------------------------------------
function TAPI_timer.intInterval2: double;
var
  ct: double;
  t: int64;
begin
  if factive then
  begin
    queryperformancecounter(int64((@t)^));
    ct:=(1000*(t-fc2)/ffr);
    result:=ct + fstart;
  end else
    result:=0;
end;

//------------------------------------------------------------------------------
function TAPI_timer.intElapsed: double;
var
  ct: double;
begin
  if factive then
  begin
    queryperformancecounter(int64((@fc3)^));
    if (ffr<>0) and (fc3-fc1<>0) then ct:=(1000*(fc3-fc1)/ffr)
      else ct:= 0;
    result:=ct + fstart;
  end else
  begin
    if (ffr<>0) and (fc3-fc1<>0) then
    begin
      ct:=(1000*(fc3-fc1)/ffr);
      result:=ct + fstart;
    end else
      result:=0;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.Start;
begin
  intStart(0);
  if finterval>0 then
    ftimer.Enabled:= true;
end;

//------------------------------------------------------------------------------
procedure TAPI_timer.Start(StartMs: double);
begin
  intStart(StartMs);
end;

//------------------------------------------------------------------------------
function TAPI_timer.InstervalMs: double;
begin
  result:= intInterval;
end;

//------------------------------------------------------------------------------
function TAPI_timer.ElapsedMs: double;
begin
  result:= intElapsed;
end;

//------------------------------------------------------------------------------
function TAPI_timer.Stop: double;
begin
  ftimer.Enabled:= false;
  intStop;
  result:= intElapsed;
  if assigned(fonstop) then
    fonstop(self, result);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_timer]);
end;

end.

