unit CHThreadTimer;

{ ##############################################################################
  TCHThreadTimer

  Version   		:   1.1.2
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 27.10.2002    - CHANGE: optimize "DoTimer" procedure
  1.1.0 - 29.10.2002    - ADD: OnStopTimer Event
                        - BUG: procedure Execute send now Processmessages
  1.1.1 - 15.12.2002    - BUG: memory leak in SetEnabled
  1.1.2 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, Forms, Classes;

type
  TBreakMode = (thSuspend, thTerminate);

  TCHThreadTimer = class;

  TExtThread = class(TThread)
  private
    FOwner : TCHThreadTimer;
    FStop : THandle;
  protected
    procedure Execute; override;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

  TCHThreadTimer = class(TComponent)
  private
    FOnTimer: TNotifyEvent;
    FExtThread : TExtThread;
    FBreakMode : TBreakMode;

    FEnabled : Boolean;
    FAllowZero : Boolean;
    FSynchronized : Boolean;
    FEndThreadWaiting : Boolean;
    FThreadPriority : TThreadPriority;
    FInterval : Cardinal;
    FOnStopTimer: TNotifyEvent;

    procedure DoTimer;
    procedure StopTimer;

    procedure SetEnabled(const Value : Boolean);
    procedure SetInterval(const Value : Cardinal);
    procedure SetThreadPriority(const Value : TThreadPriority);
    procedure SetSynchronized(const Value: Boolean);
    procedure SetAllowZero(const Value: Boolean);
    procedure setBreakMode(const Value: TBreakMode);
    procedure SetEndThreadWaiting(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnStopTimer: TNotifyEvent read FOnStopTimer write FOnStopTimer;

    property AllowZero: Boolean read FAllowZero Write SetAllowZero;
    property BreakMode : TBreakMode read FBreakMode Write setBreakMode;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property EndThreadWaiting : Boolean read FEndThreadWaiting Write SetEndThreadWaiting;
    property Interval: Cardinal read FInterval write SetInterval;
    property Synchronized: Boolean read FSynchronized write SetSynchronized;
    property ThreadPriority: TThreadPriority read FThreadPriority  write SetThreadPriority;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHThreadTimer]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAllowZero := False;
  FEnabled := False;
  FInterval := 1000;
  FSynchronized := True;
  FEndThreadWaiting := False;
  FThreadPriority := tpNormal;
  FBreakMode := thSuspend;

  FExtThread := TExtThread.Create(True);
  FExtThread.FOwner := Self;
  FExtThread.Priority := tpNormal;
  FExtThread.FStop := CreateEvent(nil, False, False, nil);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHThreadTimer.Destroy;
begin
  Enabled := False;

  FExtThread.Terminate;
  SetEvent(FExtThread.FStop);
  if FExtThread.Suspended then
    FExtThread.Resume;

  FExtThread.WaitFor;
  CloseHandle(FExtThread.FStop);
  FExtThread.Free;

  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.DoTimer;
begin
  try
    FOnTimer(Self);
  except
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.StopTimer;
begin
  if Assigned(FOnStopTimer) then
    FOnStopTimer(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TExtThread.Execute;
begin
  repeat
    try
      if WaitForSingleObject(FStop, FOwner.Interval) = WAIT_TIMEOUT then
      begin
        if FOwner.Synchronized then
          Synchronize(FOwner.DoTimer)
        else
          FOwner.DoTimer;
      end;
      Application.ProcessMessages;
    except
    end;
  until Terminated;

  if Terminated then
    FOwner.StopTimer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetAllowZero(const Value: Boolean);
begin
  if FAllowZero <> Value then
    FAllowZero := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.setBreakMode(const Value: TBreakMode);
begin
  if FBreakMode <> Value then
    FBreakMode := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;

    if Assigned(FOnTimer) and not (csDestroying in ComponentState) then
    begin
      // Enable = True
      if FEnabled then
      begin
        if (FInterval > 0) or ((FInterval = 0) and FAllowZero) then
        begin
          SetEvent(FExtThread.FStop);

          if FBreakMode = thSuspend then
            FExtThread.Resume
          else
          begin
            if Assigned(FExtThread) then
            begin
              FExtThread.Terminate;
              FExtThread.Free;
            end;
            FExtThread := TExtThread.Create(True);
            FExtThread.FOwner := Self;
            FExtThread.Priority := FThreadPriority;
            FExtThread.FStop := CreateEvent(nil, False, False, nil);
            FExtThread.Resume;
          end;
        end;
      end
      // Enable = False
      else
      begin
        // Suspend
        if FBreakMode = thSuspend then
        begin
          FExtThread.Suspend;
        end
        // Terminate
        else
        begin
          if FEndThreadWaiting then
          begin
            FExtThread.FreeOnTerminate := False;
            FExtThread.Terminate;
            FExtThread.WaitFor;
          end
          else
          begin
            FExtThread.Terminate;
          end;
        end;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetEndThreadWaiting(const Value: Boolean);
begin
  if FEndThreadWaiting <> Value then
    FEndThreadWaiting := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetInterval(const Value: Cardinal);
var
  tmpEnabled : Boolean;
  tmpInterval : Cardinal;
begin
  if Value <> FInterval then
  begin
    tmpEnabled := FEnabled;
    tmpInterval := FInterval;
    Enabled := False;

    if (FInterval = 0) and (not FAllowZero) then
      FInterval := tmpInterval
    else
      FInterval := Value;

    Enabled := tmpEnabled;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetSynchronized(const Value: Boolean);
begin
  if FSynchronized <> Value then
  begin
    FSynchronized := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHThreadTimer.SetThreadPriority(const Value: TThreadPriority);
begin
  if FThreadPriority <> Value then
  begin
    FExtThread.Priority := Value;
    FThreadPriority := FExtThread.Priority;
  end;
end;


end.
