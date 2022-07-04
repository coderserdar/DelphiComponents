unit API_winprocess;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// r1.01, ari pikivirta
// * added restore, minimze, etc. functions
// * added afterrun, afterterminate and onerror events
// * added running state check via window's getexitcode function

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  tprocesspriorities = (
    _high_priority_class,
    _idle_priority_class,
    _normal_priority_class,
    _realtime_priority_class
    );

type
  TAPI_winprocess = class(TComponent)
  private
    fversion: string;
    ffilename: string;
    fprocess: tprocessinformation;
    fppriority: integer;
    ftpriority: integer;
    flasterror: string;
    fpriority: boolean;
    fwaitidle: cardinal;

    fafterrun: tnotifyevent;
    fafterkill: tnotifyevent;
    fonerror: tnotifyevent;

    procedure _setppriority(i:integer);
    procedure _settpriority(i:integer);
    procedure dummys(s: string);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

    procedure show;
    procedure hide;
    procedure restore;
    procedure minimize;
    procedure shownoactive;

    function running: boolean;

    function run(showmode: integer = SW_SHOWNA; waitforidle: boolean = TRUE): boolean;
    function kill(justhandle: boolean = FALSE): boolean;

  published
    property Version: string read fversion write dummys stored false;
    property SetPriorities:boolean read fpriority write fpriority;
    property WaitIdleTime:cardinal read fwaitidle write fwaitidle;
    property Filename: string read ffilename write ffilename;
    property ProcessPriority: integer read fppriority write _setppriority;
    property ThreadPriority: integer read ftpriority write _settpriority;
    property LastError: string read flasterror write dummys stored false;

    property OnError: tnotifyevent read fonerror write fonerror;
    property AfterRun: tnotifyevent read fafterrun write fafterrun;
    property AfterKill: tnotifyevent read fafterkill write fafterkill;
  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfo = 'r1.01/ari.pikivirta@kolumbus.fi';

procedure TAPI_winprocess.dummys(s: string); begin end;

//------------------------------------------------------------------------------
const
  errormsg: array [0..15] of string =(
    'Error: Already running', //0
    'Error: No process running', //1
    'Error: Waiting process to come idle', //2
    'Error: Open process failed', //3
    'Error: Failed to terminate process', //4
    'Error: Process is running', //5
    'Error: Failed to set process priority class', //6
    'Error: Failed to set thread priority', //7
    'Error: ', //8
    'Error: ', //9
    'Error: ', //10
    'Error: ', //11
    'Error: ', //12
    'Error: ', //13
    'Error: ', //14
    'Error: '); //15

//------------------------------------------------------------------------------
constructor tAPI_winprocess.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  ffilename:='';
  fwaitidle:=infinite;
  fpriority:=false;
  fillchar(fprocess,sizeof(fprocess),0);
  fppriority:=NORMAL_PRIORITY_CLASS;
  ftpriority:=THREAD_PRIORITY_NORMAL;
  flasterror:='';
end;

//------------------------------------------------------------------------------
destructor tAPI_winprocess.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_winprocess.running: boolean;
var
  exitcode: cardinal;
begin
  getexitcodeprocess(fprocess.hProcess, exitcode);
  result:=(exitcode=STILL_ACTIVE);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess.show;
begin
  if running then
    showwindow(fprocess.hProcess,SW_SHOW);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess.hide;
begin
  if running then
    showwindow(fprocess.hProcess,SW_HIDE);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess.restore;
begin
  if running then
    showwindow(fprocess.hProcess,SW_RESTORE);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess.minimize;
begin
  if running then
    showwindow(fprocess.hProcess,SW_MINIMIZE);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess.shownoactive;
begin
  if running then
    showwindow(fprocess.hProcess,SW_SHOWNOACTIVATE);
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess._setppriority(i: integer);
begin
  flasterror:='';
  if (csdesigning in componentstate) then exit;

  if running then
  begin
    if setpriorityclass(fprocess.hProcess,i) then
    begin
      fppriority:=i;
    end else
    begin
      flasterror:=errormsg[6];
      if assigned(fonerror) then
        fonerror(self);
    end;
  end else
  begin
    flasterror:=errormsg[1];
    if assigned(fonerror) then
      fonerror(self);
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_winprocess._settpriority(i:integer);
begin
  flasterror:='';
  if (csdesigning in componentstate) then exit;

  if running then
  begin
    if setthreadpriority(fprocess.hThread,i) then
    begin
      ftpriority:=i;
    end else
    begin
      flasterror:=errormsg[7];
      if assigned(fonerror) then
        fonerror(self);
    end;
  end else
  begin
    flasterror:=errormsg[1];
    if assigned(fonerror) then
      fonerror(self);
  end;
end;

//------------------------------------------------------------------------------
function tAPI_winprocess.run(showmode:integer;waitforidle:boolean): boolean;
var
  suinfo : TStartupInfo;
begin
  result:=false;
  flasterror:='';

  if not running then
  begin
    fillchar(suinfo,sizeof(suinfo),0);
    fillchar(fprocess,sizeof(fprocess),0);
    suinfo.cb:= SizeOf(suinfo);
    suinfo.dwFlags:= STARTF_USESHOWWINDOW;
    suinfo.wShowWindow:= showmode;

    if CreateProcess(
      nil,
      pchar(ffilename),
      nil,
      nil,
      false,
      CREATE_NEW_CONSOLE or fppriority,
      nil,
      pchar (extractfilename (ffilename)) ,
      suinfo,fprocess) then
    begin
      if waitforidle then
        if waitforinputidle(fprocess.hProcess,fwaitidle)<>0 then
        begin
          flasterror:=errormsg[3];
          if assigned(fonerror) then
            fonerror(self);
        end;

      result:=true;
      if assigned(fafterrun) then
        fafterrun(self);

      if fpriority then
        _settpriority(ftpriority);

    end else
    begin
      flasterror:=errormsg[2];
      if assigned(fonerror) then
        fonerror(self);
    end;
  end else
  begin
    flasterror:=errormsg[0];
    if assigned(fonerror) then
      fonerror(self);
  end;
end;

//------------------------------------------------------------------------------
function tAPI_winprocess.kill(justhandle:boolean): boolean;
begin
  result:=false;
  flasterror:='';
  if running then
  begin
    if justhandle then
    begin
      closehandle(fprocess.hProcess);
      closehandle(fprocess.hThread);
      fillchar(fprocess,sizeof(fprocess),0);
      result:=true;
      if assigned(fafterkill) then
        fafterkill(self);
    end else
    begin
      if terminateprocess(fprocess.hProcess,0) then
      begin
        closehandle(fprocess.hProcess);
        closehandle(fprocess.hThread);
        FillChar(fprocess,sizeof(fprocess),0);
        result:=true;
        if assigned(fafterkill) then
          fafterkill(self);
      end else
      begin
        flasterror:=errormsg[4];
        if assigned(fonerror) then
          fonerror(self);
      end;
    end;
  end else
  begin
    flasterror:=errormsg[1];
    if assigned(fonerror) then
      fonerror(self);
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_winprocess]);
end;

end.
