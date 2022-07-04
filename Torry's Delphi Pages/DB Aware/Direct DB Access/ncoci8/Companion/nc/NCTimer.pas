{*******************************************************}
{File:      NCTimer.PAS                                 }
{Revision:  1.03 / 03.11.1999                           }
{Comment:   Thread driving timer                        }
{Copyright: (c) 1997-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, dmitrya@inthink.com         }
{*******************************************************}
{$I NCOciDef.inc}

unit NCTimer;

interface

Uses Classes, Windows;

type
    TNCTimerOption = (toSynchronized, toSingleFire);
    TNCTimerOptions = set of TNCTimerOption;
    TNCTimerState = (tsDisabled, tsWaiting, tsRaised, tsReqRemove);

    TNCTimer = class(TComponent)
    private
        FInterval: DWORD;
        FEnabled: Boolean;
        FOptions: TNCTimerOptions;
        FOnTimer: TNotifyEvent;
        FNextFire: DWORD;
        FPrev, FNext: TNCTimer;
        FState: TNCTimerState;
        procedure SetInterval(AValue: DWORD);
        procedure SetEnabled(AValue: Boolean);
        procedure Update;
    protected
        procedure Loaded; override;
        procedure DoFire; virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        property State: TNCTimerState read FState;
    published
        property Interval: DWORD read FInterval write SetInterval default 1000;
        property Enabled: Boolean read FEnabled write SetEnabled default False;
        property Options: TNCTimerOptions read FOptions write FOptions
            default [];
        property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    end;

{ =========================================================================== }
{ =========================================================================== }

implementation

type
    TNCTimerPool = class(TThread)
    private
        FTimers: TNCTimer;
        FCriticalSection: TRTLCriticalSection;
        FChangedEvent: THandle;
        procedure BeginUpdate;
        procedure EndUpdate;
        procedure ExtractTimer(ATimer: TNCTimer);
        procedure Shedule(ATimer: TNCTimer; ANewNextFire: DWORD);
    protected
        procedure Execute; override;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Update(ATimer: TNCTimer);
        procedure Remove(ATimer: TNCTimer);
    end;

var
    TimerPool: TNCTimerPool;

{ =========================================================================== }
{ =========================================================================== }

constructor TNCTimerPool.Create;
begin
    inherited Create(True);
    InitializeCriticalSection(FCriticalSection);
    FChangedEvent := CreateEvent(nil, False, False, nil);
    Resume;
end;

destructor TNCTimerPool.Destroy;
begin
    Suspend;
    DeleteCriticalSection(FCriticalSection);
    CloseHandle(FChangedEvent);
    inherited Destroy;
end;

procedure TNCTimerPool.BeginUpdate;
begin
    EnterCriticalSection(FCriticalSection);
end;

procedure TNCTimerPool.EndUpdate;
begin
    if FTimers <> nil then
        SetEvent(FChangedEvent);
    LeaveCriticalSection(FCriticalSection);
end;

procedure TNCTimerPool.Shedule(ATimer: TNCTimer; ANewNextFire: DWORD);
var
    pCur, p1: TNCTimer;
begin
    ATimer.FNextFire := ANewNextFire;
    if ATimer.FState <> tsDisabled then
        ExtractTimer(ATimer);
    if FTimers = nil then begin
        FTimers := ATimer;
        ATimer.FNext := nil;
        ATimer.FPrev := nil;
    end
    else begin
        pCur := FTimers;
        while (pCur.FNext <> nil) and (pCur.FNextFire < ATimer.FNextFire) do
            pCur := pCur.FNext;
        if pCur.FNextFire < ATimer.FNextFire then begin
            pCur.FNext := ATimer;
            ATimer.FPrev := pCur;
            ATimer.FNext := nil;
        end
        else begin
            p1 := pCur.FPrev;
            ATimer.FPrev := p1;
            if p1 <> nil then
                p1.FNext := ATimer
            else
                FTimers := ATimer;
            ATimer.FNext := pCur;
            pCur.FPrev := ATimer;
        end;
    end;
end;

procedure TNCTimerPool.ExtractTimer(ATimer: TNCTimer);
var
    p1, p2: TNCTimer;
begin
    p1 := ATimer.FNext;
    p2 := ATimer.FPrev;
    if p1 <> nil then
        p1.FPrev := p2;
    if p2 <> nil then
        p2.FNext := p1
    else
        FTimers := p1;
end;

procedure TNCTimerPool.Update(ATimer: TNCTimer);
begin
    BeginUpdate;
    try
        if ATimer.FState in [tsDisabled, tsWaiting] then begin
            Shedule(ATimer, GetTickCount() + ATimer.Interval);
            ATimer.FState := tsWaiting;
        end;
    finally
        EndUpdate;
    end;
end;

procedure TNCTimerPool.Remove(ATimer: TNCTimer);
begin
    BeginUpdate;
    try
        if ATimer.FState = tsWaiting then begin
            ExtractTimer(ATimer);
            ATimer.FState := tsDisabled;
        end
        else if ATimer.FState = tsRaised then
            ATimer.FState := tsReqRemove;
    finally
        EndUpdate;
    end;
end;

{$WARNINGS OFF}
procedure TNCTimerPool.Execute;
var
    lastTime: DWORD;
    t: Integer;
    ATimer: TNCTimer;
    prevState: TNCTimerState;
begin
    repeat
        while True do begin
            EnterCriticalSection(FCriticalSection);
            try
                lastTime := GetTickCount();
                if FTimers = nil then
                    t := INFINITE
                else begin
                    t := FTimers.FNextFire - lastTime;
                    if t < 0 then
                        Break;
                end;
            finally
                LeaveCriticalSection(FCriticalSection);
            end;
            WaitForSingleObject(FChangedEvent, t);
        end;
        EnterCriticalSection(FCriticalSection);
        try
            ATimer := FTimers;
            ATimer.FState := tsRaised;
        finally
            LeaveCriticalSection(FCriticalSection);
        end;
        with ATimer do begin
            try
                if toSynchronized in Options then
                    Synchronize(DoFire)
                else
                    DoFire;
            except
            end;
            EnterCriticalSection(FCriticalSection);
            try
                if toSingleFire in Options then
                    Enabled := False;
                prevState := FState;
                FState := tsWaiting;
                if prevState = tsReqRemove then
                    Remove(ATimer)
                else
                    Shedule(ATimer, lastTime + Interval);
            finally
                LeaveCriticalSection(FCriticalSection);
            end;
        end;
    until Terminated;
end;
{$WARNINGS ON}

{ =========================================================================== }
{ =========================================================================== }

constructor TNCTimer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FInterval := 1000;
end;

destructor TNCTimer.Destroy;
begin
    Enabled := False;
    while State <> tsDisabled do;
    inherited Destroy;
end;

procedure TNCTimer.SetInterval(AValue: DWORD);
begin
    if FInterval <> AValue then begin
        FInterval := AValue;
        Update;
    end;
end;

procedure TNCTimer.SetEnabled(AValue: Boolean);
begin
    if FEnabled <> AValue then begin
        FEnabled := AValue;
        Update;
    end;
end;

procedure TNCTimer.DoFire;
begin
    if Assigned(FOnTimer) then
        FOnTimer(Self);
end;

procedure TNCTimer.Loaded;
begin
    inherited Loaded;
    Update;
end;

procedure TNCTimer.Update;
begin
    if [csDesigning, csLoading] * ComponentState <> [] then
        Exit;
    if TimerPool = nil then
        TimerPool := TNCTimerPool.Create;
    if Enabled then
        TimerPool.Update(Self)
    else if State <> tsDisabled then
        TimerPool.Remove(Self);
end;

initialization
    TimerPool := nil;
finalization
    TimerPool.Free;
end.
