{*********************************************************}
{*                VPTIMERPOOL.PAS 1.03                   *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I Vp.INC}

unit VpTimerPool;
  {-Timer Pool Class}

interface

uses
  Windows, Classes, Messages, SysUtils, Forms, VpException;

type
  TVpTimerTriggerEvent =
    procedure(Sender : TObject; Handle : Integer;
              Interval : Cardinal; ElapsedTime : LongInt) of object;

type
  PEventRec       = ^TEventRec;
  TEventRec       = packed record
    erHandle      : Integer;                {handle of this event record}
    erInitTime    : LongInt;                {time when trigger was created}
    erElapsed     : LongInt;                {total elapsed time (ms)}
    erInterval    : Cardinal;               {trigger interval}
    erLastTrigger : LongInt;                {time last trigger was fired}
    erOnTrigger   : TVpTimerTriggerEvent;  {method to call when fired}
    erEnabled     : Boolean;                {true if trigger is active}
    erRecurring   : Boolean;                {false for one time trigger}
  end;

type
  TVpTimerPool = class(TComponent)
  protected {private}
    {property variables}
    FOnAllTriggers : TVpTimerTriggerEvent;

    {internal variables}
    tpList         : TList;    {list of event TEventRec records}
    tpHandle       : hWnd;     {our window handle}
    tpInterval     : Cardinal; {the actual Window's timer interval}
    tpEnabledCount : Integer;  {count of active triggers}

    {property methods}
    function GetElapsedTriggerTime(Handle : Integer) : LongInt;
    function GetElapsedTriggerTimeSec(Handle : Integer) : LongInt;
    function GetOnTrigger(Handle : Integer) : TVpTimerTriggerEvent;
    function GetTriggerCount : Integer;
    function GetTriggerEnabled(Handle : Integer) : Boolean;
    function GetTriggerInterval(Handle : Integer) : Cardinal;
    procedure SetOnTrigger(Handle : Integer; Value: TVpTimerTriggerEvent);
    procedure SetTriggerEnabled(Handle : Integer; Value: Boolean);
    procedure SetTriggerInterval(Handle : Integer; Value: Cardinal);

    {internal methods}
    procedure tpCalcNewInterval;
      {-calculates the needed interval for the window's timer}
    function tpCountEnabledTriggers : Integer;
      {-returns the number of enabled/active timer triggers}
    function tpCreateTriggerHandle : Integer;
      {-returns a unique timer trigger handle}
    function tpEventIndex(Handle : Integer) : Integer;
      {-returns the internal list index corresponding to the trigger handle}
    procedure tpSortTriggers;
      {-sorts the internal list of timer trigger event records}
    procedure tpTimerWndProc(var Msg : TMessage);
      {-window procedure to catch timer messages}
    procedure tpUpdateTimer;
      {-re-create the windows timer with a new timer interval}

  protected
    procedure DoTriggerNotification; virtual;
      {-conditionally sends notification of all events}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddOneShot(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
      {-adds or updates one timer trigger. removed automatically after one firing}
    function AddOneTime(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
      {-adds a new timer trigger. removed automatically after one firing}
    function Add(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
      {-adds a new timer trigger and returns a handle}
    procedure Remove(Handle : Integer);
      {-removes the timer trigger}
    procedure RemoveAll;
      {-disable and destroy all timer triggers}
    procedure ResetElapsedTime(Handle : Integer);
      {-resets ElapsedTime for a given Trigger to 0}

    {public properties}
    property Count : Integer read GetTriggerCount;

    property ElapsedTime[Handle : Integer] : LongInt read GetElapsedTriggerTime;
    property ElapsedTimeSec[Handle : Integer] : LongInt
      read GetElapsedTriggerTimeSec;
    property Enabled[Handle : Integer] : Boolean read GetTriggerEnabled
      write SetTriggerEnabled;
    property Interval[Handle : Integer] : Cardinal read GetTriggerInterval
      write SetTriggerInterval;

    {events}
    property OnTrigger[Handle : Integer] : TVpTimerTriggerEvent read GetOnTrigger
      write SetOnTrigger;
    property OnAllTriggers : TVpTimerTriggerEvent read FOnAllTriggers
      write FOnAllTriggers;
  end;

implementation
{$R-,Q-}

const
  tpDefMinInterval     = 55;     {smallest timer interval allowed}
  tpDefHalfMinInterval = tpDefMinInterval div 2;

{*** internal routines ***}

function NewEventRec : PEventRec;
begin
  GetMem(Result, SizeOf(TEventRec));
  FillChar(Result^, SizeOf(TEventRec), #0);
end;

procedure FreeEventRec(ER : PEventRec);
begin
  if (ER <> nil) then
    FreeMem(ER, SizeOf(TEventRec));
end;


{*** TVpTimerPool ***}

constructor TVpTimerPool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {create internal list for trigger event records}
  tpList := TList.Create;

  {allocate a window handle for the timer}
  tpHandle := {$IFDEF VERSION6}Classes.{$ENDIF}AllocateHWnd(tpTimerWndProc);
end;

destructor TVpTimerPool.Destroy;
var
  I : Integer;
begin
  {force windows timer to be destroyed}
  tpInterval := 0;
  tpUpdateTimer;

  {free contents of list}
  for I := 0 to tpList.Count-1 do
    FreeEventRec(tpList[I]);

  {destroy the internal list}
  tpList.Free;
  tpList := nil;

  {deallocate our window handle}
  {$IFDEF VERSION6}Classes.{$ENDIF}DeallocateHWnd(tpHandle);

  inherited Destroy;
end;

function TVpTimerPool.AddOneShot(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
  {-adds or updates one timer trigger. removed automatically after one firing}
var
  I : Integer;
begin
  {if this OnTrigger handler is already installed, remove it}
  if Assigned(OnTrigger) then begin
    for I := 0 to tpList.Count-1 do
      with PEventRec(tpList[I])^ do
        if @erOnTrigger = @OnTrigger then begin
          Remove(erHandle);
          Break;
        end;
  end;
  {add the one-time trigger}
  Result := AddOneTime(OnTrigger, Interval);
end;

function TVpTimerPool.AddOneTime(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
  {-adds a new timer trigger. removed automatically after one firing}
var
  I : Integer;
begin
  {add trigger}
  Result := Add(OnTrigger, Interval);

  {if added, set to non-recurring}
  if (Result > -1) then begin
    I := tpEventIndex(Result);
    if I > -1 then
      PEventRec(tpList[I])^.erRecurring := False
    else
      Result := -1;
  end;
end;

function TVpTimerPool.Add(OnTrigger : TVpTimerTriggerEvent; Interval : Cardinal) : Integer;
  {-adds a new timer trigger and returns a handle}
var
  ER : PEventRec;
begin
  Result := -1;  {assume error}
  {create new event record}
  ER := NewEventRec;
  if (ER = nil) then
    Exit;

  {force interval to be at least the minimum}
  if Interval < tpDefMinInterval then
    Interval := tpDefMinInterval;

  {fill event record}
  with ER^ do begin
    erEnabled     := True;
    erHandle      := tpCreateTriggerHandle;
    erInitTime    := GetTickCount;
    erElapsed     := 0;
    erInterval    := Interval;
    erLastTrigger := erInitTime;
    erOnTrigger   := OnTrigger;
    erRecurring   := True;
  end;

  {add trigger record to the list}
  tpList.Add(ER);

  {return the trigger event handle}
  Result := ER^.erHandle;

  {re-calculate the number of active triggers}
  tpEnabledCount := tpCountEnabledTriggers;

  {calculate new interval for the windows timer}
  tpCalcNewInterval;
  tpSortTriggers;
  tpUpdateTimer;
end;

procedure TVpTimerPool.DoTriggerNotification;
  {-conditionally sends notification for all events}
var
  ER : PEventRec;                                                           
  TC : LongInt;
  I  : Integer;
  ET : longint;
begin
  TC := GetTickCount;

  {cycle through all triggers}
  I := 0;
  while I < tpList.Count do begin
    ER := PEventRec(tpList[I]);
    if ER^.erEnabled then begin
      {is it time to fire this trigger}
      if (TC < ER^.erLastTrigger) then
        ET := (High(LongInt) - ER^.erLastTrigger) + (TC - Low(LongInt))
      else
        ET := TC - ER^.erLastTrigger;

      if (ET >= LongInt(ER^.erInterval)-tpDefHalfMinInterval) then begin
        {update event record with this trigger time}
        ER^.erLastTrigger := TC;

        {check if total elapsed time for trigger >= MaxLongInt}
        if ((MaxLongInt - ER^.erElapsed) < ET) then
          ER^.erElapsed := MaxLongInt
        else
          ER^.erElapsed := ER^.erElapsed + ET;

        {call user event handler, if assigned}
        if Assigned(ER^.erOnTrigger) then
          ER^.erOnTrigger(Self, ER^.erHandle, ER^.erInterval, ER^.erElapsed);

        {call general event handler, if assigned}
        if Assigned(FOnAllTriggers) then
          FOnAllTriggers(Self, ER^.erHandle, ER^.erInterval, ER^.erElapsed);

        if not ER^.erRecurring then begin
          Remove(ER^.erHandle);
          Dec(I); {adjust loop index for this deletion}
        end;
      end;
    end;
    Inc(I);
  end;
end;

function TVpTimerPool.GetElapsedTriggerTime(Handle : Integer) : LongInt;
  {-return the number of miliseconds since the timer trigger was created}
var
  I  : Integer;
  ET : longint;
  ER : PEventRec;
  TC : LongInt;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then begin
    ER := PEventRec(tpList[I]);
    if ER^.erElapsed = High(LongInt) then
      Result := High(LongInt)
    else begin
      TC := GetTickCount;
      if (TC < ER^.erInitTime) then begin
        ET := (High(LongInt) - ER^.erInitTime) + (TC - Low(LongInt));
        if (ET < ER^.erElapsed) then
          ER^.erElapsed := High(LongInt)
        else
          ER^.erElapsed := ET;
      end else
        ER^.erElapsed := TC - ER^.erInitTime;
      Result := ER^.erElapsed;
    end;
  end else
    raise EInvalidTriggerHandle.Create;
end;

function TVpTimerPool.GetElapsedTriggerTimeSec(Handle : Integer) : LongInt;
  {-return the number of seconds since the timer trigger was created}
begin
  Result := GetElapsedTriggerTime(Handle) div 1000;
end;

function TVpTimerPool.GetOnTrigger(Handle : Integer) : TVpTimerTriggerEvent;
  {-returns the timer trigger's event method address}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then
    Result := PEventRec(tpList[I])^.erOnTrigger
  else
    raise EInvalidTriggerHandle.Create;
end;

function TVpTimerPool.GetTriggerCount : Integer;
  {-returns the number of maintained timer triggers}
begin
  Result := tpList.Count;
end;

function TVpTimerPool.GetTriggerEnabled(Handle : Integer) : Boolean;
  {-returns the timer trigger's enabled status}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then
    Result := PEventRec(tpList[I])^.erEnabled
  else
    raise EInvalidTriggerHandle.Create;
end;

function TVpTimerPool.GetTriggerInterval(Handle : Integer) : Cardinal;
  {-returns the interval for the timer trigger with Handle}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then
    Result := PEventRec(tpList[I])^.erInterval
  else
    raise EInvalidTriggerHandle.Create;
end;

procedure TVpTimerPool.Remove(Handle : Integer);
  {-removes the timer trigger}
var
  ER : PEventRec;
  I  : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then begin
    ER := PEventRec(tpList[I]);
    tpList.Delete(I);
    FreeEventRec(ER);
    tpEnabledCount := tpCountEnabledTriggers;
    tpCalcNewInterval;
    tpUpdateTimer;
  end;
end;

procedure TVpTimerPool.RemoveAll;
  {-disable and destroy all timer triggers}
var
  ER : PEventRec;
  I  : Integer;
begin
  for I := tpList.Count-1 downto 0 do begin
    ER := PEventRec(tpList[I]);
    tpList.Delete(I);
    FreeEventRec(ER);
  end;
  tpEnabledCount := 0;
  tpInterval := 0;
  tpUpdateTimer;
end;

procedure TVpTimerPool.ResetElapsedTime(Handle : Integer);
  {-resets ElapsedTime for a given Trigger to 0}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then
    PEventRec(tpList[I])^.erInitTime := LongInt(GetTickCount)
  else
    raise EInvalidTriggerHandle.Create;
end;

procedure TVpTimerPool.SetOnTrigger(Handle : Integer; Value: TVpTimerTriggerEvent);
  {-sets the method to call when the timer trigger fires}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then
    PEventRec(tpList[I])^.erOnTrigger := Value
  else
    raise EInvalidTriggerHandle.Create;
end;

procedure TVpTimerPool.SetTriggerEnabled(Handle : Integer; Value: Boolean);
  {-sets the timer trigger's enabled status}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then begin
    if (Value <> PEventRec(tpList[I])^.erEnabled) then begin
      PEventRec(tpList[I])^.erEnabled := Value;
      {If the timer is being activated, then initialize LastTrigger}          
      if PEventRec(tpList[I])^.erEnabled then                                 
        PEventRec(tpList[I])^.erLastTrigger := GetTickCount;                  
      tpEnabledCount := tpCountEnabledTriggers;
      tpCalcNewInterval;
      tpUpdateTimer;
    end;
  end else
    raise EInvalidTriggerHandle.Create;
end;

procedure TVpTimerPool.SetTriggerInterval(Handle : Integer; Value : Cardinal);
  {-sets the timer trigger's interval}
var
  I : Integer;
begin
  I := tpEventIndex(Handle);
  if (I > -1) then begin
    if Value <> PEventRec(tpList[I])^.erInterval then begin
      PEventRec(tpList[I])^.erInterval := Value;
      tpCalcNewInterval;
      tpUpdateTimer;
    end;
  end else
    raise EInvalidTriggerHandle.Create;
end;

procedure TVpTimerPool.tpCalcNewInterval;
  {-calculates the needed interval for the window's timer}
var
  I    : Integer;
  N, V : LongInt;
  TR   : LongInt;
  ER   : PEventRec;
  TC   : LongInt;
  Done : Boolean;
begin
  {find shortest trigger interval}
  TC := GetTickCount;
  tpInterval := High(Cardinal);
  for I := 0 to tpList.Count-1 do begin
    ER := PEventRec(tpList[I]);
    if ER^.erEnabled then begin
      if (ER^.erInterval < tpInterval) then
        tpInterval := ER^.erInterval;

      {is this interval greater than the remaining time on any existing triggers}
      TR := 0;
      if (TC < ER^.erLastTrigger) then
        TR := TR + MaxLongInt
      else
        TR := TC - ER^.erLastTrigger;
      if LongInt(tpInterval) > (LongInt(ER^.erInterval) - TR) then
        tpInterval := (LongInt(ER^.erInterval) - TR);
    end;
  end;

  {limit to smallest allowable interval}
  if tpInterval < tpDefMinInterval then
    tpInterval := tpDefMinInterval;

  if tpInterval = High(Cardinal) then
    tpInterval := 0
  else begin
    {find interval that evenly divides into all trigger intervals}
    V := tpInterval; {use LongInt so it is possible for it to become (-)}
    repeat
      Done := True;
      for I := 0 to tpList.Count-1 do begin
        N := PEventRec(tpList[I])^.erInterval;
        if (N mod V) <> 0 then begin
          Dec(V, N mod V);
          Done := False;
          Break;
        end;
      end;
    until Done or (V <= tpDefMinInterval);

    {limit to smallest allowable interval}
    if V < tpDefMinInterval then
      V := tpDefMinInterval;

    tpInterval := V;
  end;
end;

function TVpTimerPool.tpCountEnabledTriggers : Integer;
  {-returns the number of enabled/active timer triggers}
var
  I : Integer;
begin
  Result := 0;
  for I := 0 to tpList.Count-1 do
    if PEventRec(tpList[I])^.erEnabled then
      Inc(Result);
end;

function TVpTimerPool.tpCreateTriggerHandle : Integer;
  {-returns a unique timer trigger handle}
var
  I : Integer;
  H : Integer;
begin
  Result := 0;
  for I := 0 to tpList.Count-1 do begin
    H := PEventRec(tpList[I])^.erHandle;
    if H >= Result then
      Result := H + 1;
  end;
end;

function TVpTimerPool.tpEventIndex(Handle : Integer) : Integer;
  {-returns the internal list index corresponding to Handle}
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to tpList.Count-1 do
    if PEventRec(tpList[I])^.erHandle = Handle then begin
      Result := I;
      Break;
    end;
end;

procedure TVpTimerPool.tpSortTriggers;
  {-sorts the internal list of timer trigger event records}
var
  I    : Integer;
  Done : Boolean;
begin
  repeat
    Done := True;
    for I := 0 to tpList.Count-2 do begin
      if (PEventRec(tpList[I])^.erInterval >
          PEventRec(tpList[I+1])^.erInterval) then begin
        tpList.Exchange(I, I+1);
        Done := False;
      end;
    end;
  until Done;
end;

procedure TVpTimerPool.tpTimerWndProc(var Msg : TMessage);
  {-window procedure to catch timer messages}
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        DoTriggerNotification;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(tpHandle, Msg, wParam, lParam);
end;

procedure TVpTimerPool.tpUpdateTimer;
  {-re-create the windows timer with a new timer interval}
begin
  {remove existing timer, if any}
  if KillTimer(tpHandle, 1) then {ignore return value};

  if (tpInterval <> 0) and (tpEnabledCount > 0) then
    if SetTimer(tpHandle, 1, tpInterval, nil) = 0 then
      raise ENoTimersAvailable.Create;
end;


end.
