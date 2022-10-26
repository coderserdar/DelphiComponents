{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit rxTimerlst;

{$I RX.INC}

interface

uses
  Windows,
  Messages, Classes;

const
  DefaultInterval = 1000;
  HInvalidEvent = -1;

type
  TAllTimersEvent = procedure(Sender: TObject; Handle: Longint) of object;

  TRxTimerEvent = class;

  TRxTimerList = class(TComponent)
  private
    FEvents: TList;
    FWndHandle: hWnd;
    FActive: Boolean;
    FInterval: Longint;
    FSequence: Longint;
    FStartInterval: Longint;
    FOnFinish: TNotifyEvent;
    FOnTimers: TAllTimersEvent;
    procedure CalculateInterval(StartTicks: Longint);
    function CreateNewEvent: TRxTimerEvent;
    function GetCount: Integer;
    function GetEnabledCount: Integer;
    function ProcessEvents: Boolean;
    procedure RemoveItem(Item: TRxTimerEvent);
    procedure SetActive(Value: Boolean);
    procedure SetEvents(StartTicks: Longint);
    procedure Sort;
    procedure TimerWndProc(var Msg: TMessage);
    procedure UpdateTimer;
  protected
    procedure GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
      Root: TComponent {$ENDIF}); override;
    procedure DoTimer(Event: TRxTimerEvent); dynamic;
    function NextHandle: Longint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(AOnTimer: TNotifyEvent; AInterval: Longint;
      ACycled: Boolean): Longint; virtual;
    function AddItem(Item: TRxTimerEvent): Longint;
    procedure Clear;
    procedure Delete(AHandle: Longint); virtual;
    procedure Activate;
    procedure Deactivate;
    function ItemByHandle(AHandle: Longint): TRxTimerEvent;
    function ItemIndexByHandle(AHandle: Longint): Integer;
    property Count: Integer read GetCount;
    property EnabledCount: Integer read GetEnabledCount;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Events: TList read FEvents;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnTimers: TAllTimersEvent read FOnTimers write FOnTimers;
  end;

  TRxTimerEvent = class(TComponent)
  private
    FCycled: Boolean;
    FEnabled: Boolean;
    FExecCount: Integer;
    FHandle: Longint;
    FInterval: Longint;
    FLastExecute: Longint;
    FParentList: TRxTimerList;
    FRepeatCount: Integer;
    FOnTimer: TNotifyEvent;
    function GetAsSeconds: Cardinal;
    procedure SetAsSeconds(Value: Cardinal);
    procedure SetRepeatCount(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Longint);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    property AsSeconds: Cardinal read GetAsSeconds write SetAsSeconds;
    property Handle: Longint read FHandle;
    property ExecCount: Integer read FExecCount;
    property TimerList: TRxTimerList read FParentList;
  published
    property Cycled: Boolean read FCycled write FCycled default True;
    property RepeatCount: Integer read FRepeatCount write SetRepeatCount default 0;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Longint read FInterval write SetInterval default DefaultInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses
  Consts, Controls, Forms, SysUtils, rxVCLUtils, rxMaxMin;

const
  MinInterval = 100; { 0.1 sec }
{$IFDEF RX_D4}
  MaxTimerInterval: Longint = High(Longint);
{$ELSE}
  MaxTimerInterval: Longint = High(Cardinal);
{$ENDIF}
  Registered: Boolean = False;

{ TRxTimerEvent }

constructor TRxTimerEvent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentList := nil;
  FCycled := True;
  FRepeatCount := 0;
  FEnabled := True;
  FExecCount := 0;
  FInterval := DefaultInterval;
  FLastExecute := GetTickCount;
  FHandle := HInvalidEvent;
end;

destructor TRxTimerEvent.Destroy;
begin
  FOnTimer := nil;
  inherited Destroy;
end;

function TRxTimerEvent.HasParent: Boolean;
begin
  Result := True;
end;

function TRxTimerEvent.GetParentComponent: TComponent;
begin
  Result := FParentList;
end;

procedure TRxTimerEvent.SetParentComponent(Value: TComponent);
begin
  if FParentList <> nil then
    FParentList.RemoveItem(Self);
  if (Value <> nil) and (Value is TRxTimerList) then
    TRxTimerList(Value).AddItem(Self);
end;

procedure TRxTimerEvent.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      FExecCount := 0;
      FLastExecute := GetTickCount;
      if FParentList <> nil then
        with FParentList do
        begin
          CalculateInterval(GetTickCount);
          UpdateTimer;
        end;
    end;
  end;
end;

procedure TRxTimerEvent.SetInterval(Value: Longint);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FParentList <> nil then
      with FParentList do
      begin
        CalculateInterval(GetTickCount);
        UpdateTimer;
      end;
  end;
end;

procedure TRxTimerEvent.SetRepeatCount(Value: Integer);
begin
  if FRepeatCount <> Value then
  begin
    Value := Max(Value, Integer(not FCycled));
    if not (csDesigning in ComponentState)and FEnabled and (Value <= FExecCount) then
      Enabled := False;
    FRepeatCount := Value;
  end;
end;

function TRxTimerEvent.GetAsSeconds: Cardinal;
begin
  Result := Interval div 1000;
end;

procedure TRxTimerEvent.SetAsSeconds(Value: Cardinal);
begin
  Interval := Value * 1000;
end;

{ TRxTimerList }

constructor TRxTimerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEvents := TList.Create;
  FWndHandle := INVALID_HANDLE_VALUE;
  FSequence := 0;
  FStartInterval := 0;
  Deactivate;
  if not Registered then
  begin
    RegisterClasses([TRxTimerEvent]);
    Registered := True;
  end;
end;

destructor TRxTimerList.Destroy;
begin
  OnFinish := nil;
  OnTimers := nil;
  Deactivate;
  Clear;
  FEvents.Free;
  inherited Destroy;
end;

procedure TRxTimerList.Activate;
begin
  Active := True;
end;

procedure TRxTimerList.Deactivate;
begin
  if not (csLoading in ComponentState) then
    Active := False;
end;

procedure TRxTimerList.SetEvents(StartTicks: Longint);
var
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if TRxTimerEvent(FEvents[I]).Enabled then
      TRxTimerEvent(FEvents[I]).FLastExecute := StartTicks;
end;

procedure TRxTimerList.SetActive(Value: Boolean);
var
  StartTicks: Longint;
begin
  if FActive <> Value then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then
      begin
        FWndHandle := {$IFDEF RX_D6}Classes.{$ENDIF}AllocateHWnd(TimerWndProc);
        StartTicks := GetTickCount;
        SetEvents(StartTicks);
        CalculateInterval(StartTicks);
        Sort;
        UpdateTimer;
      end
      else
      begin
        KillTimer(FWndHandle, 1);
        {$IFDEF RX_D6}Classes.{$ENDIF}DeallocateHWnd(FWndHandle);
        FWndHandle := INVALID_HANDLE_VALUE;
        if Assigned(FOnFinish) then FOnFinish(Self);
      end;
      FStartInterval := 0;
    end;
    FActive := Value;
  end;
end;

procedure TRxTimerList.GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
  Root: TComponent {$ENDIF});
var
  I: Integer;
begin
  inherited GetChildren(Proc {$IFDEF RX_D3}, Root {$ENDIF});
  for I := 0 to FEvents.Count - 1 do
    Proc(TRxTimerEvent(FEvents[I]));
end;

procedure TRxTimerList.Sort;
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in ComponentState) then
    repeat
      ExitLoop := True;
      for I := 0 to Count - 2 do
      begin
        if TRxTimerEvent(FEvents[I]).Interval > TRxTimerEvent(FEvents[I + 1]).Interval then
        begin
          FEvents.Exchange(I, I + 1);
          ExitLoop := False;
        end;
      end;
    until ExitLoop;
end;

function TRxTimerList.NextHandle: Longint;
begin
  Inc(FSequence);
  Result := FSequence;
end;

function TRxTimerList.CreateNewEvent: TRxTimerEvent;
begin
  Result := TRxTimerEvent.Create(Owner);
end;

function TRxTimerList.AddItem(Item: TRxTimerEvent): Longint;
begin
  if FEvents.Add(Item) >= 0 then
  begin
    Item.FHandle := NextHandle;
    Item.FParentList := Self;
    Result := Item.FHandle;
    CalculateInterval(GetTickCount);
    Sort;
    UpdateTimer;
  end
  else
    Result := HInvalidEvent; { invalid handle }
end;

{ Create a new timer event and returns a handle }
function TRxTimerList.Add(AOnTimer: TNotifyEvent; AInterval: Longint;
  ACycled: Boolean): Longint;
var
  T: TRxTimerEvent;
begin
  T := CreateNewEvent;
  if (FEvents.Add(T) >= 0) then
  begin
    with T do
    begin
      OnTimer := AOnTimer;
      FParentList := Self;
      FHandle := NextHandle;
      Interval := AInterval;
      Cycled := ACycled;
      Result := FHandle;
    end;
    CalculateInterval(GetTickCount);
    Sort;
    UpdateTimer;
  end
  else
  begin
    T.Free;
    Result := HInvalidEvent; { invalid handle }
  end;
end;

function TRxTimerList.ItemIndexByHandle(AHandle: Longint): Integer;
begin
  for Result := 0 to FEvents.Count - 1 do
    if TRxTimerEvent(FEvents[Result]).Handle = AHandle then
      Exit;
  Result := -1;
end;

function TRxTimerList.ItemByHandle(AHandle: Longint): TRxTimerEvent;
var
  I: Integer;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then
    Result := TRxTimerEvent(FEvents[I])
  else
    Result := nil;
end;

procedure TRxTimerList.Delete(AHandle: Longint);
var
  I: Integer;
  Item: TRxTimerEvent;
begin
  I := ItemIndexByHandle(AHandle);
  if I >= 0 then
  begin
    Item := TRxTimerEvent(FEvents[I]);
    RemoveItem(Item);
    if not (csDestroying in Item.ComponentState) then
      Item.Free;
    if Active then
    begin
      CalculateInterval(GetTickCount);
      UpdateTimer;
    end;
  end;
end;

function TRxTimerList.GetCount: Integer;
begin
  Result := FEvents.Count;
end;

function TRxTimerList.GetEnabledCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if TRxTimerEvent(FEvents[I]).Enabled then
      Inc(Result);
end;

procedure TRxTimerList.RemoveItem(Item: TRxTimerEvent);
begin
  FEvents.Remove(Item);
  Item.FParentList := nil;
end;

procedure TRxTimerList.Clear;
var
  I: Integer;
  Item: TRxTimerEvent;
begin
  for I := FEvents.Count - 1 downto 0 do
  begin
    Item := TRxTimerEvent(FEvents[I]);
    RemoveItem(Item);
    if not (csDestroying in Item.ComponentState) then
      Item.Free;
  end;
end;

procedure TRxTimerList.DoTimer(Event: TRxTimerEvent);
begin
  with Event do
    if Assigned(FOnTimer) then
      FOnTimer(Event);
  if Assigned(FOnTimers) then
    FOnTimers(Self, Event.Handle);
end;

function TRxTimerList.ProcessEvents: Boolean;
var
  I: Integer;
  Item: TRxTimerEvent;
  StartTicks: Longint;
begin
  Result := False;
  if not (csDesigning in ComponentState) then
  begin
    StartTicks := GetTickCount;
    for I := Count - 1 downto 0 do
    begin
      Item := TRxTimerEvent(FEvents[I]);
      if (Item <> nil) and Item.Enabled then
        with Item do
          if (StartTicks - FLastExecute) >= (Interval - (MinInterval div 2)) then
          begin
            FLastExecute := StartTicks;
            Inc(FExecCount);
            Enabled := not ((not Cycled) and (FExecCount >= RepeatCount));
            if not Enabled then
              Result := True;
            DoTimer(Item);
          end;
    end;
  end;
end;

procedure TRxTimerList.TimerWndProc(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    with Msg do
      if Msg = WM_TIMER then
        try
          if (not (csDesigning in ComponentState)) and
            (FStartInterval = 0) and Active then
          begin
            if ProcessEvents then
            begin
              if EnabledCount = 0 then
                Deactivate
              else
              begin
                CalculateInterval(GetTickCount);
                UpdateTimer;
              end;
            end;
          end
          else
            UpdateTimer;
        except
          Application.HandleException(Self);
        end
      else
        Result := DefWindowProc(FWndHandle, Msg, wParam, lParam);
  end;
end;

procedure TRxTimerList.CalculateInterval(StartTicks: Longint);
var
  I: Integer;
  ExitLoop: Boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Count = 0 then
      FInterval := 0
    else
    begin
      FStartInterval := 0;
      FInterval := MaxLongInt;
      for I := 0 to Count - 1 do
        with TRxTimerEvent(FEvents[I]) do
          if Enabled and (Interval > 0) then
          begin
            if Interval < Self.FInterval then
              Self.FInterval := Interval;
            if Self.FInterval > (Interval - (StartTicks - FLastExecute)) then
              Self.FInterval := (Interval - (StartTicks - FLastExecute));
          end;
      if FInterval < MinInterval then
        FInterval := MinInterval;
      if FInterval = MaxLongint then
        FInterval := 0
      else
      begin
        repeat
          ExitLoop := True;
          for I := 0 to Count - 1 do
            with TRxTimerEvent(FEvents[I]) do
              if (Interval mod Self.FInterval) <> 0 then
              begin
                Dec(Self.FInterval, Interval mod Self.FInterval);
                ExitLoop := False;
                Break;
              end;
        until ExitLoop or (FInterval <= MinInterval);
        if FInterval < MinInterval then
          FInterval := MinInterval;
      end;
    end;
  end;
end;

procedure TRxTimerList.UpdateTimer;
var
  FTimerInterval: Cardinal;
begin
  if not (csDesigning in ComponentState) then
  begin
    if FInterval <= MaxTimerInterval then
      FTimerInterval := FInterval
    else
      if (FInterval - FStartInterval) <= MaxTimerInterval then
      begin
        FTimerInterval := Cardinal(FInterval - FStartInterval);
        FStartInterval := 0;
      end
      else
      begin
        FTimerInterval := MaxTimerInterval;
        FStartInterval := FStartInterval + MaxTimerInterval;
      end;
    if not (csDesigning in ComponentState) and (FWndHandle <> INVALID_HANDLE_VALUE) then
    begin
      KillTimer(FWndHandle, 1);
      if EnabledCount = 0 then
        Deactivate
      else
      if FInterval > 0 then
        if SetTimer(FWndHandle, 1, FTimerInterval, nil) = 0 then
        begin
          Deactivate;
          raise EOutOfResources.Create(ResStr(SNoTimers));
        end;
    end;
  end;
end;

end.