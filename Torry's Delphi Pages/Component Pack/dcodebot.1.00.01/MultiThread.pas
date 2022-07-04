unit MultiThread;

interface

uses
  ActiveX, Messages, Windows, ComObj, SysUtils;

{ The TThreadDebugMode is used to control if thread activity should be logged to a
  file. Information logged includes the current proccess, thread, module name,
  thread status, and duration the thread. This information is written to the
  file c:\thrdebug.log. }

type
  TThreadDebugMode = (dmNone, dmRecord);

procedure SetDebugMode(Mode: TThreadDebugMode);
function GetDebugMode: TThreadDebugMode;

{ The InitMultiThread procedure should be called from a library's project source
  file. The procedure sets up debugging and synchronization for a library. }

procedure InitMultiThread(Mode: TThreadDebugMode = dmNone);

{ The EnterLock and ExitLock procedures use reference counting to safeguard
  code in a multithreaded enivronment. }

procedure EnterLock;
procedure ExitLock;

{ The WriteLog procedure is a thread safe file access routine. The optional
  Recreate parameter will cause the file to be overwritten if it exists when set
  to True. }

procedure WriteLog(const FileName: string; const Line: string;
  Recreate: Boolean = False);

implementation

var
  ModuleName: string;
  Mutex: THandle;

threadvar
  LockCount: Integer;
  Start: TDateTime;

procedure EnterLock;
begin
  if LockCount = 0 then
    WaitForSingleObject(Mutex, INFINITE);
  Inc(LockCount);
end;

procedure ExitLock;
begin
  if LockCount > 0 then
  begin
    Dec(LockCount);
    if LockCount = 0 then
      ReleaseMutex(Mutex);
  end;
end;

procedure WriteLog(const FileName: string; const Line: string;
  Recreate: Boolean = False);
var
  LogFile: TextFile;
begin
  EnterLock;
  try
    AssignFile(LogFile, FileName);
    try
      if Recreate or (not FileExists(FileName)) then
        ReWrite(LogFile)
      else
        Append(LogFile);
      WriteLn(LogFile, Line);
    finally
      CloseFile(LogFile);
    end;
  finally
    ExitLock;
  end;
end;

var
  DebugMode: TThreadDebugMode = dmNone;

procedure SetDebugMode(Mode: TThreadDebugMode);
begin
  EnterLock;
  try
    DebugMode := Mode;
  finally
    ExitLock;
  end;
end;

function GetDebugMode: TThreadDebugMode;
begin
  Result := DebugMode;
end;

procedure ThreadDebug(Reason: Integer);
var
  Line: string;
begin
  if DebugMode = dmNone then Exit;
  EnterLock;
  try
    Line := FormatDateTime('mm/dd/yyyy hh:mm:ss am/pm ', Now) +
      Format('Process: %d Thread: %d Module: %s ', [GetCurrentProcessId,
       GetCurrentThreadId, ModuleName]);
    case Reason of
      DLL_PROCESS_ATTACH:
        Line := Line + '<process attach>';
      DLL_THREAD_ATTACH:
        Line := Line + '<thread attach>';
      DLL_PROCESS_DETACH:
        Line := Line + '<process detach> Duration: ' + FloatToStr((Now - Start) * 3600) + ' seconds';
      DLL_THREAD_DETACH:
        Line := Line + '<thread detach> Duration: ' + FloatToStr((Now - Start) * 3600) + ' seconds';
    end;
    WriteLog('c:\thrdebug.log', Line);
  finally
    ExitLock;
  end;
end;

procedure LibraryProc(Reason: Integer);

  function TargetName: string;
  var
    Buffer: array[0..MAX_PATH] of Char;
    FilePart: PChar;
  begin
    GetModuleFileName(HInstance, Buffer, SizeOf(Buffer) - 1);
    GetFullPathName(Buffer, SizeOf(Buffer) - 1, Buffer, FilePart);
    Result := Buffer;
  end;

begin
  if Reason = DLL_PROCESS_ATTACH then
  begin
    Mutex := CreateMutex(nil, False, PChar('vclmtx' +
      IntToStr(GetCurrentProcessID)));
    ModuleName := TargetName;
  end;
  case Reason of
    DLL_PROCESS_ATTACH, DLL_THREAD_ATTACH:
      begin
        LockCount := 0;
        Start := Now;
      end;
  end;
  ThreadDebug(Reason);
  case Reason of
    DLL_PROCESS_DETACH, DLL_THREAD_DETACH:
      begin
        if LockCount > 0 then
          ReleaseMutex(Mutex);
        CloseHandle(Mutex);
      end;
  end;
end;

procedure InitMultiThread(Mode: TThreadDebugMode = dmNone);
begin
  if IsLibrary then
  begin
    IsMultiThread := True;
    DebugMode := Mode;
    DllProc := @LibraryProc;
    LibraryProc(DLL_PROCESS_ATTACH);
  end;
end;



initialization
  InitWriteLog('c:\my.log', 'init ' + IntToStr(GetCurrentThreadID));
finalization
  InitWriteLog('c:\my.log', 'close ' + IntToStr(GetCurrentThreadID));
end.
