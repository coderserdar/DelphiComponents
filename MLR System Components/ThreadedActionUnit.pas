unit ThreadedActionUnit;

interface

uses
  Classes, ActnList, SyncObjs;

type
  TThreadedAction = class(TAction)
  private
    FSpawnThread: Boolean;
    FDisableWhileRunning: Boolean;
    FRunningCount: Cardinal;
    FLockUpdate: Boolean;
  protected
    FCountLock :TCriticalSection;
    procedure ChangeLockCount(const Increment :Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    function Update: Boolean; override;
    property RunningCount :Cardinal read FRunningCount;
  published
    property SpawnThread :Boolean read FSpawnThread write FSpawnThread default True;
    property DisableWhileRunning :Boolean read FDisableWhileRunning write FDisableWhileRunning default True;
  end;

  procedure Register;

implementation

uses
  SysConst, SysUtils, Windows;

type
  TActionThread = class(TThread)
  private
    FAction: TThreadedAction;
  public
    procedure Execute; override;
    property Action: TThreadedAction read FAction write FAction;
  end;

procedure Register;
begin
  RegisterActions('', [TThreadedAction], nil);
end;

procedure RaiseLastWin32ErrorDescription(const Description :string);
var
  LastError: DWORD;
  Error: EWin32Error;
  s :string;
begin
  LastError := GetLastError;
  if LastError <> ERROR_SUCCESS then
    s := Format(SWin32Error, [LastError, SysErrorMessage(LastError)]) else
    s := SUnkWin32Error;
  Error := EWin32Error.Create(s + #13 + Description);
  Error.ErrorCode := LastError;
  raise Error;
end;

{ TThreadedAction }

procedure TThreadedAction.ChangeLockCount(const Increment: Boolean);
begin
  FCountLock.Enter;
  try
    if Increment then
      Inc(FRunningCount) else
      Dec(FRunningCount);
  finally
    FCountLock.Leave;
  end;
  if (FRunningCount = 0) or
     (FRunningCount = 1) then begin
    FLockUpdate := True;
    Update;
  end;
end;

constructor TThreadedAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSpawnThread := True;
  FDisableWhileRunning := True;
  FCountLock := TCriticalSection.Create;
end;

function GenericStartAddress(P :Pointer): Cardinal; stdcall;
var
  TA :TThreadedAction;
  ev :TNotifyEvent;
begin
  Result := 0;
  if not Assigned(P) then exit; // This function expect a pointer to a TThreadedAction.
  TA := TThreadedAction(P);
  TA.ChangeLockCount(True);
  ev := TA.OnExecute;
  if Assigned(ev) then ev(TA);
  TA.ChangeLockCount(False);
end;

destructor TThreadedAction.Destroy;
begin
  FCountLock.Free;
  inherited Destroy;
end;

function TThreadedAction.Execute: Boolean;
var
  ThreadHandle, ThreadID :Cardinal;
  Ev :TNotifyEvent;
  AThread: TActionThread;
begin
  Ev := OnExecute;
  Result := False;
  if not FSpawnThread then begin
    Result := inherited Execute;
    exit;
  end else begin
    Update;
    if not Enabled then exit;
    if not Assigned(Ev) then exit;
    {
    ThreadHandle := CreateThread(nil, 0, @GenericStartAddress, Self,
      CREATE_SUSPENDED, ThreadID);
    if ThreadHandle = 0 then
      RaiseLastWin32ErrorDescription('Could not create thread for ' + Name);
    ResumeThread(ThreadHandle);
    CloseHandle(ThreadHandle);
    }
    AThread := TActionThread.Create(True);
    try
      AThread.FreeOnTerminate := True;
      AThread.Action := Self;
    except
      AThread.Free;
    end;
    AThread.Execute;
    Result := True;
  end;
end;

function TThreadedAction.Update: Boolean;
begin
  if FLockUpdate then begin
    FLockUpdate := False;
    Enabled := (not FDisableWhileRunning) or (FRunningCount = 0);
    Result := True;
  end else begin
    if FDisableWhileRunning and (FRunningCount > 0) then begin
      Enabled := False;
      Result := True;
    end else
      Result := inherited Update;
  end;
end;

{ TActionThread }

procedure TActionThread.Execute;
var
  ev :TNotifyEvent;
begin
  if not Assigned(FAction) then exit;
  FAction.ChangeLockCount(True);
  try
    ev := FAction.OnExecute;
    if Assigned(ev) then ev(FAction);
  finally
    FAction.ChangeLockCount(False);
  end;
end;

end.

