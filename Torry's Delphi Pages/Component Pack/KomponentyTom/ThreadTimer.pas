{ *************************************************************************** }
{                                                                             }
{ Komponent TRealTimer. Wersja komponentu TTimer oparta na pojedynczym watku  }
{                                                                             }
{ Copyright (c) 2003 Tomasz Bojara                                            }
{                                                                             }
{ *************************************************************************** }

unit ThreadTimer;

interface

uses
  Windows, SysUtils, Classes, SyncObjs;

type

  TCustomThreadComp = class;

  TCustomEventThread = class(TThread)
  private
    FOwner: TCustomThreadComp;
    FEvent: TEvent;
    procedure Start; virtual;
    function Stop: Boolean; virtual;
    procedure Enabled;
    procedure Disabled;
    constructor Create(const AOwner: TCustomThreadComp);
  public
    destructor Destroy; override;
    property Event: TEvent read FEvent;
  end;

  TAsynTimer = class;

  TThreadTimer = class(TCustomEventThread)
  private
    function Stop: Boolean; override;
  protected
    procedure Execute; override;
  end;

  TAsynEvent = class;

  TThreadAsynEvent = class(TCustomEventThread)
  private
    procedure ESet;
    procedure EReset;
  protected
    procedure Execute; override;
  end;


  ETerminateError = class(Exception);

  TThreadClass = class of TCustomEventThread;

  TCustomThreadComp = class(TComponent)
  private
    FEnabled: Boolean;
    FThread: TCustomEventThread;
    FSynchronize: Boolean;
    FPriority: TThreadPriority;
    FWaitForTerminate: Integer;
    FThreadClass: TThreadClass;
    FOnEvent: TNotifyEvent;    
    procedure EnabledSet;
    procedure DisabledSet;
    procedure DoEnabled;
    procedure DoDisabled;
    procedure SetEnabled(const Value: Boolean);
    procedure SetPriority(const Value: TThreadPriority);
    function GetPriority: TThreadPriority;
    procedure DoExecute;
    procedure DoExecuteSyn;
    function GetTerminated: Boolean;
  protected
    property ThreadClass: TThreadClass read FThreadClass write FThreadClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Terminate;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Synchronize: Boolean read FSynchronize write FSynchronize;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property WaitForTerminate: Integer read FWaitForTerminate write FWaitForTerminate;
    property Terminated: Boolean read GetTerminated;
    property Thread: TCustomEventThread read FThread;
    property OnEvent: TNotifyEvent read FOnEvent write FOnEvent;
  end;

  TAsynTimer = class(TCustomThreadComp)
  private
    FInterval: Cardinal;
    procedure SetInterval(const Value: Cardinal);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled;
    property Synchronize;
    property Priority;
    property WaitForTerminate;
    property OnEvent;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
  end;

  TAsynEvent = class(TCustomThreadComp)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetEvent;
    procedure ResetEvent;
  published
    property Enabled;
    property Synchronize;
    property Priority;
    property WaitForTerminate;
    property OnEvent;
  end;

implementation

const
  C_TerminateErrorStr = 'Terminate thread timeout error';

{ TThreadTimer }

constructor TAsynTimer.Create(AOwner: TComponent);
begin
  inherited;
  FThreadClass:= TThreadTimer;
  FInterval:= 1000;
end;

procedure TAsynTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if FEnabled then
    begin
      DoDisabled;
      DoEnabled;
    end;
  end;
end;

{ TThreadTimer }

procedure TThreadTimer.Execute;
begin
  try
    Enabled;
    try
      while not Terminated do
      begin
        case WaitForSingleObject(FEvent.Handle, TAsynTimer(FOwner).Interval) of
          WAIT_FAILED, WAIT_ABANDONED, WAIT_OBJECT_0: Terminate;
          WAIT_TIMEOUT: TAsynTimer(FOwner).DoExecute;
        end;
      end;
    finally
      Disabled;
    end;
  except
    ApplicationHandleException(Self);
  end;
end;

{ TCustomThreadEvent }

constructor TCustomThreadComp.Create(AOwner: TComponent);
begin
  inherited;
  FPriority:= tpNormal;
  FWaitForTerminate:= 15;
end;

destructor TCustomThreadComp.Destroy;
begin
  Enabled:= False;
  inherited;
end;

procedure TCustomThreadComp.DisabledSet;
begin
  FEnabled:= False;
end;

procedure TCustomThreadComp.DoDisabled;
begin
  if not FThread.Stop then
  begin
    FreeAndNil(FThread);
    raise ETerminateError.Create(C_TerminateErrorStr);
  end;
end;

procedure TCustomThreadComp.DoEnabled;
begin
  FThread:= FThreadClass.Create(Self);
  FThread.Start;
end;

procedure TCustomThreadComp.DoExecute;
begin
  If Assigned(FOnEvent) then
    if FSynchronize then
      DoExecuteSyn
     else
      FOnEvent(FThread);
end;

procedure TCustomThreadComp.DoExecuteSyn;
begin
  FOnEvent(Self);
end;

procedure TCustomThreadComp.EnabledSet;
begin
  FEnabled:= True;
end;

function TCustomThreadComp.GetPriority: TThreadPriority;
begin
  if FThread <> nil then FPriority:= FThread.Priority;
  Result:= FPriority;
end;

function TCustomThreadComp.GetTerminated: Boolean;
begin
  if FEnabled then
    Result:= FThread.Terminated
  else
    Result:= False;
end;

procedure TCustomThreadComp.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if Value then
      DoEnabled
    else
      DoDisabled;
  end;
end;

procedure TCustomThreadComp.SetPriority(const Value: TThreadPriority);
begin
  if FPriority <> Value then
  begin
    if Assigned(FThread) then
      FThread.Priority:= Value
    else
      FPriority:= Value;
  end;
end;

function TThreadTimer.Stop: Boolean;
begin
  Result:= False;
  Terminate;
  FEvent.SetEvent;
  with TAsynTimer(FOwner) do
  begin
    if (csDestroying in ComponentState) and (Interval < 100) then Sleep(100);
    if not FEnabled then
    begin
      Result:= True;
      Exit;
    end;
  end;
  if WaitForSingleObject(Handle, FOwner.FWaitForTerminate) = WAIT_TIMEOUT then
  begin
    TerminateThread(Handle, 0);
    Disabled;
  end else
    Result:= True;
end;

{ TCustomEventThread }

constructor TCustomEventThread.Create(const AOwner: TCustomThreadComp);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FOwner:= AOwner;
  Priority:= FOwner.FPriority;
  FEvent:= TEvent.Create(nil, True, False, '');
end;

destructor TCustomEventThread.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TCustomEventThread.Disabled;
begin
  FOwner.DisabledSet;
  FOwner.FThread:= nil;
  FEvent.ResetEvent;
end;

procedure TCustomEventThread.Enabled;
begin
  FOwner.EnabledSet;
  if csDesigning in FOwner.ComponentState then FEvent.SetEvent;
end;

procedure TCustomEventThread.Start;
begin
  Resume;
  if csDesigning in FOwner.ComponentState then
  begin
    WaitForSingleObject(FEvent.Handle, 1);
    FEvent.ResetEvent;
  end;
end;

function TCustomEventThread.Stop: Boolean;
begin
  Result:= False;
  Terminate;
  FEvent.SetEvent;
  if WaitForSingleObject(Handle, FOwner.FWaitForTerminate) = WAIT_TIMEOUT then
  begin
    TerminateThread(Handle, 0);
    Disabled;
  end else
    Result:= True;
end;

{ TThreadAsynEvent }

procedure TThreadAsynEvent.EReset;
begin
  FEvent.ResetEvent;
end;

procedure TThreadAsynEvent.ESet;
begin
  FEvent.SetEvent;
end;

procedure TThreadAsynEvent.Execute;
begin
  try
    Enabled;
    try
      while not Terminated do
      begin
        case WaitForSingleObject(FEvent.Handle, INFINITE) of
          WAIT_FAILED, WAIT_ABANDONED: Terminate;
          WAIT_OBJECT_0: if not Terminated then
          begin
            EReset;
            FOwner.DoExecute;
          end;
        end;
      end;
    finally
      Disabled;
    end;
  except
    ApplicationHandleException(Self);
  end;
end;

{ TAsynEvent }

constructor TAsynEvent.Create(AOwner: TComponent);
begin
  inherited;
  FThreadClass:= TThreadAsynEvent;
end;

procedure TAsynEvent.ResetEvent;
begin
  if FEnabled and (FThread <> nil) then TThreadAsynEvent(FThread).EReset; 
end;

procedure TAsynEvent.SetEvent;
begin
  if FEnabled and (FThread <> nil) then TThreadAsynEvent(FThread).ESet;
end;

procedure TCustomThreadComp.Terminate;
begin
  if FEnabled and (FThread <> nil) then FThread.Terminate;
end;

end.
