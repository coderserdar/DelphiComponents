unit API_dirmonitor;

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
// r1.01, 29062009, ari pikivirta
//  * changed "relaunch" to "ContinueMonitoring"
//

interface

uses
  Windows, SysUtils, Classes, Messages, Dialogs, API_base;

type
  TAPI_FileNotify = procedure(sender: tobject; var ContinueMonitoring: boolean) of object;

  TAPI_NotifyOption = (
    fmFilename,
    // Any file name change in the watched directory or subtree causes a change
    // notification wait operation to return. Changes include renaming, creating,
    // or deleting a file name.
    fmDirname,
    // Any directory-name change in the watched directory or subtree causes
    // a change notification wait operation to return. Changes include creating
    // or deleting a directory.
    fmAttributes,
    // Any attribute change in the watched directory or subtree causes a change
    // notification wait operation to return.
    fmSize,
    // Any file-size change in the watched directory or subtree causes a change
    // notification wait operation to return. The operating system detects a
    // change in file size only when the file is written to the disk.
    // For operating systems that use extensive caching, detection occurs only
    // when the cache is sufficiently flushed.
    fmLastWrite,
    // Any change to the last write-time of files in the watched directory or
    // subtree causes a change notification wait operation to return.
    // The operating system detects a change to the last write-time only when
    // the file is written to the disk. For operating systems that use extensive
    // caching, detection occurs only when the cache is sufficiently flushed.
    fmSecurity
    // Any security-descriptor change in the watched directory or subtree
    // causes a change notification wait operation to return.
  );
  TAPI_NotifyOptions = set of TAPI_NotifyOption;

  TAPI_DirMonitorThread = class(TThread)
  private
    fowner: thandle;
    ffolder: string;
    fsubdirs: boolean;
    foptions: dword;
    fmutex: thandle;
  protected
    procedure Execute; override;
  public
  end;

  TAPI_DirMonitor = class(TAPI_Custom_Component)
  private
    { Private declarations }
    fhandle: thandle;
    fmutex: thandle;
    factive: boolean;
    fthread: TAPI_DirMonitorThread;
    ffolder: string;
    fsubdirs: boolean;
    fonchange: TAPI_FileNotify;
    foptions: TAPI_NotifyOptions;
    procedure setactive(b: boolean);
    procedure OnTerminateThread(sender: tobject);
    function CreateNotifyOptions( opt: tapi_notifyoptions ): dword;
    procedure WndProc(var aMsg: TMessage);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Active: boolean read factive write setactive;
    property Folder: string read ffolder write ffolder;
    property Subs: boolean read fsubdirs write fsubdirs;
    property Options: TAPI_NotifyOptions read foptions write foptions;
    property OnChange: TAPI_FileNotify read fonchange write fonchange;
  end;

procedure Register;

implementation

{$R *.RES}

uses
  syncobjs;

const
  NOTIFYCHANGE_MESSAGE = WM_USER + 1;
  NOTIFYTHREAD_END = WM_USER + 2;

//------------------------------------------------------------------------------
constructor TAPI_DirMonitor.Create(aowner: tcomponent);
begin
  inherited Create(AOwner);
  factive:= FALSE;
  version:= 'r1.01/ari.pikivirta{at}kolumbus.fi';
  ffolder:= '';
  fsubdirs:= TRUE;
  fhandle:= AllocateHWnd(WndProc);
  foptions:= [fmFilename, fmDirname, fmLastWrite];
end;

//------------------------------------------------------------------------------
destructor TAPI_DirMonitor.Destroy;
begin
  active:= FALSE; // stop monitoring thread
  DeallocateHWnd(fhandle);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_DirMonitor.WndProc(var aMsg: TMessage);
var
  relaunch: boolean;
begin
  if amsg.Msg = NOTIFYCHANGE_MESSAGE then
  begin
    if assigned(fonchange) then
    try
      relaunch:= TRUE;
      fonchange(self, relaunch);
      if relaunch then active:= TRUE; // start monitoring again
    except
    end;
  end else
  if amsg.msg = NOTIFYTHREAD_END then
  begin
    factive:= FALSE;
  end else
    amsg.result:= DefWindowProc(fhandle, amsg.Msg, amsg.wparam, amsg.lparam);
end;

//------------------------------------------------------------------------------
function TAPI_DirMonitor.CreateNotifyOptions( opt: tapi_notifyoptions ): cardinal;
var
  options: dword;
begin
  options:= 0;
  if fmFileName in opt then options:= options or FILE_NOTIFY_CHANGE_FILE_NAME;
  if fmDirName in opt then options:= options or FILE_NOTIFY_CHANGE_DIR_NAME;
  if fmAttributes in opt then options:= options or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if fmLastWrite in opt then options:= options or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if fmSize in opt then options:= options or FILE_NOTIFY_CHANGE_SIZE;
  if fmSecurity in opt then options:= options or FILE_NOTIFY_CHANGE_SECURITY;
  result:= options;
end;

//------------------------------------------------------------------------------
procedure TAPI_DirMonitor.setactive(b: boolean);
var
  tempopt: cardinal;
begin
  if (factive<>b) then
  begin
    if (b) then
    begin
      // check directory
      if not (directoryexists(ffolder)) then exit;      // exit if folder does not exist

      // create options
      tempopt:= createnotifyoptions(foptions);          // create options list
      if tempopt = 0 then exit;                         // at least some must exist

      // create mutex
      fmutex:= createmutex(nil, true, nil);             // create mutex
      if fmutex=0 then exit;                            // failed to create mutex

      // set active
      fthread:= TAPI_DirMonitorthread.Create(true);     // create suspended
      fthread.Priority:= tpidle;                        // idle priority
      fthread.OnTerminate:= OnTerminateThread;          // terminate event
      fthread.FreeOnTerminate:= true;                   // free when terminated
      fthread.ffolder:= ffolder;                        // folder to monitor
      fthread.fsubdirs:= fsubdirs;                      // monitor subs also
      fthread.foptions:= tempopt;                       // give out options
      fthread.fowner:= fhandle;                         // message(s)..
      fthread.fmutex:= fmutex;                          // sendout mutex handle
      fthread.Resume;                                   // start the thread
      factive:= true;                                   // set active flag
    end else
    begin
      // release mutex
      if fmutex<>0 then releasemutex(fmutex);           // release mutex

      // set thread to nil
      //waitforsingleobject(fthread.Handle, INFINITE);    // wait for thread..

      // close mutex handle
      if fmutex<>0 then closehandle(fmutex);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_DirMonitor.OnTerminateThread(sender: tobject);
begin
  factive:= FALSE;                                      // reset active flag
end;

//------------------------------------------------------------------------------
procedure TAPI_DirMonitorThread.Execute;
var
  obj: array[0..1] of thandle;
  res: dword;
begin
  obj[1]:= fmutex;
  obj[0]:= windows.FindFirstChangeNotification(pchar(ffolder), longbool(integer(fsubdirs)), foptions);
  if (obj[0]<>INVALID_HANDLE_VALUE) then
  begin
    while not terminated do
    begin
      res:= windows.WaitForMultipleObjects(2, @obj, false, INFINITE);
      if res=WAIT_OBJECT_0 then
      begin
        PostMessage(fowner, NOTIFYCHANGE_MESSAGE, 0, 0);
        FindNextChangeNotification(obj[0]);
      end else
        break;
    end; // first change notification
    windows.FindCloseChangeNotification(obj[0]);
  end; // outer while
  postmessage(fowner, NOTIFYTHREAD_END, 0, 0);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_DirMonitor]);
end;

end.
