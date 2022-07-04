unit InPanel;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Graphics;

type
  TInPanel = class(TPanel)
  private
    FChecked: Boolean;
    FInColor: TColor;
    FTimer: TTimer;
    FRysujRam: Boolean;
    procedure SetChecked(const Value: Boolean);
    procedure SetInColor(const Value: TColor);
    procedure UpdatePen;
    procedure DoTimer(Sender: TObject);
    procedure SetPuls(const Value: Cardinal);
    function GetPuls: Cardinal;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Checked: Boolean read FChecked write SetChecked;
    property InColor: TColor read FInColor write SetInColor;
    property Puls: Cardinal read GetPuls write SetPuls default 0;
  end;

implementation
uses Windows, Types;

{ TInPanel }

constructor TInPanel.Create(AOwner: TComponent);
begin
  inherited;
  FInColor:= clRed;
  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= False;
  FTimer.Interval:= 0;
  FTimer.OnTimer:= DoTimer; 
end;

destructor TInPanel.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TInPanel.DoTimer(Sender: TObject);
begin
  Invalidate;
  FRysujRam:= not FRysujRam; 
end;

function TInPanel.GetPuls: Cardinal;
begin
  Result:= FTimer.Interval;
end;

procedure TInPanel.Paint;
var R: TRect;
begin
  inherited;
  if FChecked and (FRysujRam or not FTimer.Enabled) then
  begin
    R:= ClientRect;
    InflateRect(R, -8, -8);
    UpdatePen;
    Canvas.MoveTo(R.Left, R.Top + 8);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Left + 8, R.Top);
    Canvas.MoveTo(R.Right - 9, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top + 8);
    Canvas.MoveTo(R.Right - 1, R.Bottom - 9);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);
    Canvas.LineTo(R.Right - 9, R.Bottom - 1);
    Canvas.MoveTo(R.Left + 8, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Bottom - 9);
  end;
end;

procedure TInPanel.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    if FChecked then
    begin
      if FTimer.Enabled then
      begin
        FTimer.Enabled:= False;
        FRysujRam:= False;
      end;
    end else begin
      if FTimer.Interval > 0 then
      begin
        FTimer.Enabled:= True;
      end;
    end;
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TInPanel.SetInColor(const Value: TColor);
begin
  if FInCOlor <> Value then
  begin
    FInCOlor := Value;
    Invalidate;
  end;
end;

procedure TInPanel.SetPuls(const Value: Cardinal);
begin
  if FTimer.Interval <> Value then
  begin
    FTimer.Interval:= Value;
    if FTimer.Enabled then
    begin
      if Value > 0 then
      begin
        FTimer.Enabled:= False;
        FTimer.Enabled:= True;
      end else begin
        FTimer.Enabled:= False;
        FRysujRam:= False;
      end;
    end else
      if FTimer.Interval > 0 then FTimer.Enabled:= True;
    Invalidate;    
  end;
end;

procedure TInPanel.UpdatePen;
var
  _LogBrush: LOGBRUSH;
  
begin
  with _LogBrush do
  begin
    lbStyle:= PS_SOLID;
    lbColor:= FInColor;
    lbHatch:= 0;
  end;
  Canvas.Pen.Handle:= ExtCreatePen(PS_SOLID or
    PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_MITER, 2, _LogBrush, 0, nil);
end;

end.
