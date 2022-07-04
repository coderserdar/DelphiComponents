// Komponenty do do komunikacji przez nazwane potoki
// w architekturze klient-serwer
// Tomasz Bojara

unit PipeStatus;

interface
uses
  Windows, Controls, ExtCtrls, SysUtils, Classes, Graphics, Pipes;

type

  TPipeStatusLt = class(TGraphicControl)
  private
    { Private declarations }
    FLit: Boolean;
    FColorLitOn: TColor;
    FColorLitOff: TColor;
    TimerLit: TTimer;
    procedure SetLit(const Value: Boolean);
    procedure DoLit(Sender: TObject);
    function GetTimeLit: Cardinal;
    procedure SetTimeLit(const Value: Cardinal);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Flash;
  published
    { Published declarations }
    property Visible;
    property Align;
    property Constraints;
    property Anchors;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    property Lit: Boolean read FLit write SetLit;
    property ColorLitOn: TColor read FColorLitOn write FColorLitOn;
    property ColorLitOff: TColor read FColorLitOff write FColorLitOff;
    property TimeLit: Cardinal read GetTimeLit write SetTimeLit;
  end;

  TPipeStatus = class(TComponent)
  private
    { Private declarations }
    FPipe: TNamedPipe;
    FLitIn: TPipeStatusLt;
    FLitActive: TPipeStatusLt;
    FLitOut: TPipeStatusLt;
    FLitConnected: TPipeStatusLt;
    TimerRefresh: TTimer;
    InBytes: Cardinal;
    OutBytes: Cardinal;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure DoRefresh(Sender: TObject);
    function GetInterval: Cardinal;
    procedure SetInterval(const Value: Cardinal);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Pipe: TNamedPipe read FPipe write FPipe;
    property LitActive: TPipeStatusLt read FLitActive write FLitActive;
    property LitConnected: TPipeStatusLt read FLitConnected write FLitConnected;
    property LitIn: TPipeStatusLt read FLitIn write FLitIn;
    property LitOut: TPipeStatusLt read FLitOut write FLitOut;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

implementation

{ TPipeStatusLt }

constructor TPipeStatusLt.Create(AOwner: TComponent);
begin
  inherited;
  Width:= 16;
  Height:= 16;
  FColorLitOn:= clYellow;
  FColorLitOff:= clGreen;
  TimerLit:= TTimer.Create(Self);
  TimerLit.Enabled:= False;
  TimerLit.OnTimer:= DoLit;
  TimerLit.Interval:= 50;
end;

destructor TPipeStatusLt.Destroy;
begin
  TimerLit.Free;
  inherited;
end;

procedure TPipeStatusLt.DoLit(Sender: TObject);
begin
  TimerLit.Enabled:= False;
  Lit:= not Lit;
end;

procedure TPipeStatusLt.Flash;
begin
  Lit:= not Lit;
  TimerLit.Enabled:= True;
end;

function TPipeStatusLt.GetTimeLit: Cardinal;
begin
  Result:= TimerLit.Interval;
end;

procedure TPipeStatusLt.Paint;
begin
  Canvas.Brush.Style:= bsSolid;
  if FLit then
    Canvas.Brush.Color:= FColorLitOn
  else
    Canvas.Brush.Color:= FColorLitOff;
  Canvas.FillRect(ClientRect);
  Canvas.Pen.Color := clWhite;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width, 0);
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height);
  Canvas.Pen.Color := clDkGray;
  Canvas.MoveTo(Width - 1, 1);
  Canvas.LineTo(Width - 1, Height);
  Canvas.MoveTo(1, Height - 1);
  Canvas.LineTo(Width, Height - 1);
end;

procedure TPipeStatusLt.SetLit(const Value: Boolean);
begin
  if FLit <> Value then
  begin
    FLit := Value;
    Refresh;
  end;
end;

procedure TPipeStatusLt.SetTimeLit(const Value: Cardinal);
begin
  TimerLit.Interval:= Value;
end;

{ TPipeStatus }

constructor TPipeStatus.Create(AOwner: TComponent);
begin
  inherited;
  TimerRefresh:= TTimer.Create(Self);
  TimerRefresh.Enabled:= False;
  TimerRefresh.Interval:= 250;
  TimerRefresh.OnTimer:= DoRefresh; 
end;

destructor TPipeStatus.Destroy;
begin
  TimerRefresh.Free;
  inherited;
end;

procedure TPipeStatus.DoRefresh(Sender: TObject);
begin
  if not Assigned(FPipe) then Exit;
  if Assigned(FLitActive) then FLitActive.Lit:= FPipe.Active;
  if Assigned(FLitIn) then
  begin
    if InBytes <> FPipe.InByt then
    begin
      FLitIn.Flash;
      InBytes:= FPipe.InByt;
    end;
  end;
  if Assigned(FLitOut) then
  begin
    if OutBytes <> FPipe.OutByt then
    begin
      FLitOut.Flash;
      OutBytes:= FPipe.OutByt;
    end;
  end;
  if Assigned(FLitConnected) then
  begin
    FLitConnected.Lit:= FPipe.Connected;
    if not FLitConnected.Lit then
    begin
      InBytes:= 0;
      OutBytes:= 0;
    end;
  end;
end;

function TPipeStatus.GetEnabled: Boolean;
begin
  Result:= TimerRefresh.Enabled;
end;

function TPipeStatus.GetInterval: Cardinal;
begin
  Result:= TimerRefresh.Interval;
end;

procedure TPipeStatus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FPipe then FPipe:= nil
    else
    if AComponent = FLitIn then FLitIn:= nil
    else
    if AComponent = FLitActive then FLitActive:= nil
    else
    if AComponent = FLitOut then FLitOut:= nil
    else
    if AComponent = FLitConnected then FLitConnected:= nil;
  end;
end;

procedure TPipeStatus.SetEnabled(const Value: Boolean);
begin
  TimerRefresh.Enabled:= Value;
  InBytes:= 0;
  OutBytes:= 0;
end;

procedure TPipeStatus.SetInterval(const Value: Cardinal);
begin
  TimerRefresh.Interval:= Value;
end;

end.
 