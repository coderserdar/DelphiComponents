//---------------------------------------------------------------------------
//  TVolgaMeter - Progress bar with text
//---------------------------------------------------------------------------
//  Copyright © 2000-2002, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
unit VolMeter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TVolgaMeter = class(TCustomPanel)
  private
    FPosition: integer;
    FPercent: integer;
    FMin: integer;
    FMax: integer;
    FFillColor: TColor;
    FMeterCaption: string;
    FMeterText: string;
    procedure SetFillColor(const Value: TColor);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure CalcMeter;
    procedure SetMeterText(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure IncMeter;
  published
    { Published declarations }
    property FillColor: TColor read FFillColor write SetFillColor default clYellow;
    property Max: integer read FMax write SetMax default 100;
    property Min: integer read FMin write SetMin default 0;
    property Position: integer read FPosition write SetPosition default 0;
    property MeterText: string read FMeterText write SetMeterText;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property FullRepaint;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnDblClick;
  end;

implementation

{ TVolgaMeter }

constructor TVolgaMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csDoubleClicks, csReplicatable, csNoStdEvents];
  FFillColor := clYellow;
  BevelOuter := bvLowered;
  FMax := 100;
  FMin := 0;
  FPosition := 0;
  FPercent := 0;
  FMeterText := '';
end;

procedure TVolgaMeter.IncMeter;
begin
  Position := Position + 1;  //обновление Caption и Invalidate
end;

procedure TVolgaMeter.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect, NavyRect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(Rect);
    NavyRect := Rect;
    Brush.Color := FFillColor;
    NavyRect.Right := Rect.Left + Round((Rect.Right - Rect.Left) * FPercent / 100);
    FillRect(NavyRect);
    Brush.Style := bsClear;
    Font := Self.Font;
    FontHeight := TextHeight('W');
    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) div 2;
      Bottom := Top + FontHeight;
    end;
    Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Handle, PChar(FMeterCaption), -1, Rect, Flags);
  end;
end;

procedure TVolgaMeter.CalcMeter;
var newperc, p: integer;
  newcapt: string;
begin
  if FMax = FMin then Exit;
  newperc := Round((FPosition - FMin)*100/(FMax - FMin));
  if FMeterText = '' then
    newcapt := IntToStr(newperc) + '%'
  else
  begin
    p := Pos('%', FMeterText);
    if p > 0 then
      newcapt := Copy(FMeterText, 1, p-1) + IntToStr(newperc) + '%' +
        Copy(FMeterText, p+1, MaxInt)
    else
      newcapt := FMeterText;
  end;
  if (FPercent <> newperc) or (FMeterCaption <> newcapt) then
  begin
    FPercent := newperc;
    FMeterCaption := newcapt;
    Repaint;
  end;
end;

procedure TVolgaMeter.SetFillColor(const Value: TColor);
begin
  FFillColor := Value;
  Invalidate;
end;

procedure TVolgaMeter.SetMax(const Value: integer);
begin
  FMax := Value;
  if FMax > FMin then CalcMeter;
end;

procedure TVolgaMeter.SetMin(const Value: integer);
begin
  FMin := Value;
  if FMax > FMin then CalcMeter;
end;

procedure TVolgaMeter.SetPosition(const Value: integer);
begin
  if (Value >= FMin) and (Value <= FMax) then
  begin
    FPosition := Value;
    CalcMeter;
  end;
end;

procedure TVolgaMeter.SetMeterText(const Value: string);
begin
  FMeterText := Value;
  CalcMeter;
end;

end.
