unit CHHighResTimer;

{ ##############################################################################
  TCHHighResTimer

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 27.10.2002    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed


  ############################################################################ }

interface

uses
  Windows, Forms, Messages, Classes, Controls, MMSystem;

const
  WM_PeriodicTimer = WM_USER + $1000;
  WM_StopTimer = WM_USER + $1001;

type
  TFNTimeCallBack = procedure(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD); pascal;

type
  THRTimerEvent = procedure(Sender: TObject) of object;

  TCHHighResTimer = class(TComponent)
  private
    FWnd: HWND;
    FTimeCaps: TTimeCaps;
    FEnabled: Boolean;
    FInterval: Cardinal;
    FAccuracy: Cardinal;
    FPeriodic: Boolean;
    FID1: Integer;
    FID2: Integer;
    FOnTimer: THRTimerEvent;
    FOnStopTimer: THRTimerEvent;

    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetAccuracy(const Value: Cardinal);
  protected
    procedure UpdateTimer;
    procedure WndProc(var Msg: TMessage);
    procedure DoOnTimer; dynamic;
    procedure DoOnStopTimer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnTimer: THRTimerEvent read FOnTimer write FOnTimer;
    property OnStopTimer: THRTimerEvent read FOnStopTimer write FOnStopTimer;

    property Interval: Cardinal read FInterval write SetInterval;
    property Accuracy: Cardinal read FAccuracy write SetAccuracy;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHHighResTimer]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure PeriodicTimeCallback(TimerID, Msg: uint; dwUser, dw1, dw2: DWord); pascal;
begin
  PostMessage(dwUser, WM_PeriodicTimer, TimeGetTime, 0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure StopTimeCallback(TimerID, Msg: uint; dwUser, dw1, dw2: DWord); pascal;
begin
  PostMessage(dwUser, WM_StopTimer, TimeGetTime, 0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHHighResTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWnd := AllocateHWnd(WndProc);
  timeGetDevCaps(@FTimeCaps, SizeOf(FTimeCaps));
  timeBeginPeriod(FAccuracy);

  FAccuracy := 0;
  FInterval := 1000;
  FPeriodic := True;
  FEnabled := False;
  FID1 := 0;
  FID2 := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHHighResTimer.Destroy;
begin
  if FID1 <> 0 then
    timeKillEvent(FID1);
  if FID2 <> 0 then
    timeKillEvent(FID2);

  timeEndPeriod(FAccuracy);
  DeallocateHWnd(FWnd);

  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.WndProc(var Msg: TMessage);
begin
	with Msg do
  begin
  	if Msg = WM_PeriodicTimer then
      DoOnTimer
    else if Msg = WM_StopTimer then
      DoOnStopTimer
    else
    	DefWindowProc(FWnd, Msg, wParam, lParam);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.DoOnStopTimer;
begin
  if Assigned(FOnStopTimer) then
    FOnStopTimer(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.UpdateTimer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FID1 <> 0 then
      timeKillEvent(FID1);
    if FID2 <> 0 then
      timeKillEvent(FID2);
    if FEnabled then
    begin
      FID1:= timeSetEvent(FInterval, FAccuracy, @PeriodicTimeCallback, FWnd, TIME_PERIODIC);
      FID2:= timeSetEvent(FInterval, FAccuracy, @StopTimeCallback, FWnd, TIME_ONESHOT);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.SetInterval(const Value: Cardinal);
begin
  if (Value <> FInterval) and (Value >= FTimeCaps.wPeriodMin) and (Value <= FTimeCaps.wPeriodMax) then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHHighResTimer.SetAccuracy(const Value: Cardinal);
begin
  if (Value <> FAccuracy) and (Value <= FInterval) then
  begin
    FAccuracy := Value;
    UpdateTimer;
  end;
end;


end.
