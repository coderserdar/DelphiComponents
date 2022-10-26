{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxTimer;

interface

{$I RX.INC}

uses
  Windows,
  Messages, SysUtils, Classes, Controls;

type

{ TRxTimer }

  TRxTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FWindowHandle: HWND;
    FSyncEvent: Boolean;
    FThreaded: Boolean;
    FTimerThread: TThread;
    FThreadPriority: TThreadPriority;
    procedure SetThreaded(Value: Boolean);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateTimer;
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
    property Threaded: Boolean read FThreaded write SetThreaded default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write
      SetThreadPriority default tpNormal;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

implementation

uses
  Forms, Consts, rxVCLUtils;

{ TTimerThread }

type
  TTimerThread = class(TThread)
  private
    FOwner: TRxTimer;
    FInterval: Cardinal;
    FException: Exception;
    procedure HandleException;
  protected
    procedure Execute; override;
  public
    constructor Create(Timer: TRxTimer; Enabled: Boolean);
  end;

constructor TTimerThread.Create(Timer: TRxTimer; Enabled: Boolean);
begin
  FOwner := Timer;
  inherited Create(not Enabled);
  FInterval := 1000;
  FreeOnTerminate := True;
end;

procedure TTimerThread.HandleException;
begin
  if not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
      Application.OnException(Self, FException)
    else
      Application.ShowException(FException);
  end;
end;

procedure TTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  repeat
    if not ThreadClosed then
      if SleepEx(FInterval, False) = 0 then
        if not ThreadClosed and FOwner.FEnabled then
          with FOwner do
            if SyncEvent then
              Synchronize(Timer)
            else
              try
                Timer;
              except
                on E: Exception do
                begin
                  FException := E;
                  HandleException;
                end;
              end;
  until Terminated;
end;

{ TRxTimer }

constructor TRxTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  FSyncEvent := True;
  FThreaded := True;
  FThreadPriority := tpNormal;
  FTimerThread := TTimerThread.Create(Self, False);
end;

destructor TRxTimer.Destroy;
begin
  Destroying;
  FEnabled := False;
  FOnTimer := nil;
  {TTimerThread(FTimerThread).FOwner := nil;}
  while FTimerThread.Suspended do FTimerThread.Resume;
  FTimerThread.Terminate;
  {if not SyncEvent then FTimerThread.WaitFor;}
  if FWindowHandle <> 0 then
  begin
    KillTimer(FWindowHandle, 1);
    {$IFDEF RX_D6}Classes.{$ENDIF}DeallocateHWnd(FWindowHandle);  // Polaris
  end;
  inherited Destroy;
end;

procedure TRxTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        Timer;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TRxTimer.UpdateTimer;
begin
  if FThreaded then
  begin
    if FWindowHandle <> 0 then
    begin
      KillTimer(FWindowHandle, 1);
      {$IFDEF RX_D6}Classes.{$ENDIF}DeallocateHWnd(FWindowHandle);  // Polaris
      FWindowHandle := 0;
    end;
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    TTimerThread(FTimerThread).FInterval := FInterval;
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    begin
      FTimerThread.Priority := FThreadPriority;
      while FTimerThread.Suspended do
        FTimerThread.Resume;
    end;
  end
  else
  begin
    if not FTimerThread.Suspended then
      FTimerThread.Suspend;
    if FWindowHandle = 0 then
      FWindowHandle := {$IFDEF RX_D6}Classes.{$ENDIF}AllocateHWnd(WndProc)  // Polaris
    else
      KillTimer(FWindowHandle, 1);
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
      if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
        raise EOutOfResources.Create(ResStr(SNoTimers));
  end;
end;

procedure TRxTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetThreaded(Value: Boolean);
begin
  if Value <> FThreaded then
  begin
    FThreaded := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then
  begin
    FThreadPriority := Value;
    if FThreaded then UpdateTimer;
  end;
end;

procedure TRxTimer.Synchronize(Method: TThreadMethod);
begin
  if (FTimerThread <> nil) then
    with TTimerThread(FTimerThread) do
    begin
      if Suspended or Terminated then
        Method
      else
        TTimerThread(FTimerThread).Synchronize(Method);
    end
  else
    Method;
end;

procedure TRxTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if Assigned(FOnTimer) <> Assigned(Value) then
  begin
    FOnTimer := Value;
    UpdateTimer;
  end
  else
    FOnTimer := Value;
end;

procedure TRxTimer.Timer;
begin
  if FEnabled and not (csDestroying in ComponentState) and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

end.