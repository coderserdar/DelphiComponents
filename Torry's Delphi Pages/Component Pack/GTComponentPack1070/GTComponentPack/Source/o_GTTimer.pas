{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       GT Threaded Timer                               }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTTimer;

interface

uses
  Classes
  ;

type
{------------------------------------------------------------------------------}
  TgtTimer = class;
{------------------------------------------------------------------------------}
 TgtTimerThread = class(TThread)
 private
   { Private declarations }
   FTimer : TgtTimer;
 protected
   { Protected declarations }
   procedure DoTimer;
 public
   { Public declarations }
   constructor Create(ATimer : TgtTimer);
   destructor  Destroy;override;
   procedure Execute;override;
  end;
{------------------------------------------------------------------------------}
  TgtTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    { Private declarations }
  protected
    { Protected declarations }
    FTimerThread : TgtTimerThread;
    procedure UpdateTimer;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    { Published declarations}
    property Enabled : Boolean  read FEnabled  write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
  published
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
  end;
{------------------------------------------------------------------------------}

implementation
uses
   Windows
  ,SysUtils
  ;

{ TgtTimerThread }
{------------------------------------------------------------------------------}
constructor TgtTimerThread.Create(ATimer: TgtTimer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTimer          := ATimer;
end;
{------------------------------------------------------------------------------}
destructor TgtTimerThread.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtTimerThread.DoTimer;
begin
  if Assigned(FTimer.OnTimer) then
    FTimer.OnTimer(FTimer);
end;
{------------------------------------------------------------------------------}
procedure TgtTimerThread.Execute;
begin
  while (not Self.Terminated) and (FTimer.Enabled) do
  begin
    WaitForSingleObject(Self.Handle,FTimer.Interval);
    Synchronize(DoTimer);
  end;
end;
{------------------------------------------------------------------------------}

{ TgtTimer }
{------------------------------------------------------------------------------}
constructor TgtTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled  := True;
  FInterval := 1000;
end;
{------------------------------------------------------------------------------}
destructor TgtTimer.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtTimer.UpdateTimer;
begin
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    FTimerThread := nil;
  end;
  if Enabled then
  begin
    if FInterval > 0 then
    begin
      FTimerThread := TgtTimerThread.Create(Self);
      FTimerThread.Resume;
    end
    else
      Enabled := False;
  end;
end;
{------------------------------------------------------------------------------}

//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtTimer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;
{------------------------------------------------------------------------------}
procedure TgtTimer.SetInterval(const Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;
{------------------------------------------------------------------------------}

end.

