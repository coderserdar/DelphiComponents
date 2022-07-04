unit mbXPHorizontalRuler;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms;

type
  TCSSMeasureUnit = (muCentimeters, muMilimeters, muInches, muPixels, muPoints, muPicas);
  TRulerPosition = (rpTop, rpBottom);

  TPaintLabelEvent = procedure (LabelFont: TFont; var LabelText: string) of object;

  TmbXPHorizontalRuler = class(TCustomControl)
  private
   FSOffset, FEOffset, FVOffset: integer;
   FUnits: TCSSMeasureUnit;
   FWholeSkip, FHalfSkip, FQuartSkip, FOneUnit: extended;
   R: TRect;
   FieldLength: integer;
   FStartAt, FSkipUnits: extended;
   mx: integer;
   FPos: TRulerPosition;
   FMLeft, FMRight: integer;
   FScale: integer;
   FOuterCl, FInnerCl, FLineCl, FMarkCl: TColor;
   FOnPaint: TNotifyEvent;
   FCustomDPI: integer;
   FOnPaintLabel: TPaintLabelEvent;
   FShowFirst, FShowLast: boolean;
   FAddaptRectLeft, FAddaptRectRight: boolean;

   function GetMaxWidth: integer;
   procedure SetShowFirst(Value: boolean);
   procedure SetShowLast(Value: boolean);
   procedure SetAddaptRectLeft(Value: boolean);
   procedure SetAddaptRectRight(Value: boolean);
   procedure SetCustomDPI(Value: integer);
   procedure SetOuterCl(c: TColor);
   procedure SetInnerCl(c: TColor);
   procedure SetLineCl(c: TColor);
   procedure SetMarkCl(c: TColor);
   procedure SetScale(s: integer);
   procedure SetPos(p: TRulerPosition);
   procedure SetSkipUnits(s: extended);
   procedure SetStartAt(s: extended);
   procedure SetUnit(u: TCSSMeasureUnit);
   procedure SetSOffset(o: integer);
   procedure SetEOffset(o: integer);
   procedure SetVOffset(o: integer);
  protected
   procedure Paint; override;
   procedure DrawWholeLines;
   procedure DrawHalfLines;
   procedure DrawQuarterLines;
  public
   constructor Create(AOwner: TComponent); override;
   procedure UpdateDisplay(x: integer);
   procedure MarkFragment(ALeft, ARight: integer);
   function GetPosition(x: integer): extended;

   property MarkLeft: integer read FMLeft;
   property MarkRight: integer read FMRight;
   property Canvas;
  published
   property OffsetLeft: integer read FSOffset write SetSOffset default 23;
   property OffsetRight: integer read FEOffset write SetEOffset default 23;
   property VerticalOffset: integer read FVOffset write SetVOffset default 5;
   property Units: TCSSMeasureUnit read FUnits write SetUnit default muPixels;
   property StartAt: extended read FStartAt write SetStartAt;
   property SkipUnits: extended read FSkipUnits write SetSkipUnits;
   property RulerPosition: TRulerPosition read FPos write SetPos default rpTop;
   property ZoomPercent: integer read FScale write SetScale default 100;
   property OuterColor: TColor read FOuterCl write SetOuterCl default clBtnFace;
   property InnerColor: TColor read FInnerCl write SetInnerCl default clWindow;
   property LineColor: TColor read FLineCl write SetLineCl default clBtnShadow;
   property MarkedColor: TColor read FMarkCl write SetMarkCl default clHighlight;
   property CustomDPI: integer read FCustomDPI write SetCustomDPI default 0;
   property ShowFirst: boolean read FShowFirst write SetShowFirst default false;
   property ShowLast: boolean read FShowLast write SetShowLast default false;
   property AddaptRectLeft: boolean read FAddaptRectLeft write SetAddaptRectLeft default false;
   property AddaptRectRight: boolean read FAddaptRectRight write SetAddaptRectRight default false;
   property Align;
   property Anchors;
   property Enabled;
   property Constraints;
   property Hint;
   property Font;
   property ShowHint;
   property ParentShowHint;
   property PopupMenu;
   property Visible;

   property OnContextPopup;
   property OnMouseMove;
   property OnMouseDown;
   property OnMouseUp;
   property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
   property OnPaintLabelText: TPaintLabelEvent read FOnPaintLabel write FOnPaintLabel;
  end;

implementation

constructor TmbXPHorizontalRuler.Create(AOwner: TComponent);
begin
 inherited;
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 FSOffset := 23;
 FEOffset := 23;
 FVOffset := 5;
 Width := 23;
 Height := 23;
 FUnits := muPixels;
 Align := alTop;
 FStartAt := 0;
 FSkipUnits := 50;
 mx := -1;
 FPos := rpTop;
 FMLeft := 0;
 FMRight := 0;
 FScale := 100;
 FCustomDPI := 0;
 FOneUnit := FScale/100;
 FOuterCl := clBtnFace;
 FInnerCl := clWindow;
 FLineCl := clBtnShadow;
 FMarkCl := clHighlight;
 FShowFirst := false;
 FShowLast := false;
 FAddaptRectLeft := false;
 FAddaptRectRight := false;
end;

procedure TmbXPHorizontalRuler.Paint;
var
 markL, markR, max: integer;
 ppi: integer;
begin
 R := ClientRect;
 //paint back rect
 Canvas.Brush.Color := FOuterCl;
 Canvas.FillRect(R);
 //set field rect
 Inc(R.Left, FSOffset);
 Dec(R.Right, FEOffset);
 InflateRect(R, 0, -FVOffset);
 //paint field rect
 Canvas.Brush.Color := FInnerCl;
 //calc max
 if FWholeSkip = 0 then FWholeSkip := FSkipUnits;
 max := Round(FieldLength/FWholeSkip);
 //Addapt the rect for the first number
 if FShowFirst and FAddaptRectLeft then Dec(R.Left, Canvas.TextWidth(IntToStr(FSOffset)) div 2);
 //now for the last number
 if FShowLast and FAddaptRectRight then Inc(R.Right, Canvas.TextWidth(IntToStr(max)) div 2);
 //paint the inner area
 Canvas.FillRect(R);
 //restore the rect
 if FShowFirst and FAddaptRectLeft then Inc(R.Left, Canvas.TextWidth(IntToStr(FSOffset)) div 2);
 if FShowLast and FAddaptRectRight then Dec(R.Right, Canvas.TextWidth(IntToStr(max)) div 2);
 // set pixels per inch
 if FCustomDPI > 0 then
  ppi := FCustomDPI
 else
  ppi := Screen.PixelsPerInch;
 // set measures
 case FUnits of
  muCentimeters: FOneUnit := ppi/2.54;
  muMilimeters: FOneUnit := ppi/25.4;
  muInches: FOneUnit := ppi;
  muPixels: FOneUnit := 1;
  muPoints: FOneUnit := ppi/72;
  muPicas: FOneUnit := (ppi/72)*12;
 end;
 FOneUnit := FOneUnit * (FScale/100);
 if FOneUnit = 0 then FOneUnit := 1/100;
 // calc vars
 FieldLength := R.Right - R.Left;
 FWholeSkip := FOneUnit * FSkipUnits;
 FHalfSkip := FWholeSkip/2;
 FQuartSkip := FHalfSkip/2;
 //mark fragment
 Canvas.Brush.Color := FMarkCl;
 markL := FMLeft;
 markR := FMRight;
 if FMLeft < R.Left then
  markL := R.Left;
 if FMLeft > R.Right then
  markL := R.Right;
 if FMRight > R.Right then
  markR := R.Right;
 if FMRight < R.Left then
  markR := R.Left;
 Canvas.FillRect(Rect(markL, R.Top, markR, R.Bottom));
 //paint ticks & text
 Canvas.Pen.Color := FLineCl;
 DrawWholeLines;
 if Assigned(FOnPaint) then
  FOnPaint(Self);
end;

function TmbXPHorizontalRuler.GetMaxWidth: integer;
var
 i, max: integer;
begin
 max := 0;
 for i := 1 to Round(FieldLength/FWholeSkip) do
  if Canvas.TextWidth(FloatToStr(i*FSkipUnits + FStartAt)) > max then
   max := Canvas.TextWidth(FloatToStr(i*FSkipUnits + FStartAt));
 Result := max;
end;

procedure TmbXPHorizontalRuler.DrawWholeLines;
var
 i, x, l, maxwidth, s: integer;
 TR: TRect;
 lblText: string;
begin
 maxwidth := GetMaxWidth;
 if FShowFirst then
  s := 0
 else
  s := 1;
 for i := s to Round(FieldLength/FWholeSkip) do
  begin
   x := R.Left + Round(i*FWholeSkip);
   if (x < R.Right - FQuartSkip/2) or ((x <= R.Right) and FShowLast) then
    begin
     //draw whole unit lines if they fit
     if FWholeSkip > 1 then
      case FPos of
       rpTop:
        begin
         Canvas.MoveTo(x, R.Bottom);
         Canvas.LineTo(x, Height);
        end;
       rpBottom:
        begin
         Canvas.MoveTo(x, 0);
         Canvas.LineTo(x, R.Top);
        end;
      end;
     Canvas.Font := Font;
     lblText := FloatToStr(i*FSkipUnits + FStartAt);
     if Assigned(FOnPaintLabel) then FOnPaintLabel(Canvas.Font, lblText);
     //draw the numbers if they fit
     if maxwidth < FWholeSkip then
      begin
       l := Round(x - (Canvas.TextWidth(lblText)/2)) + 1;
       TR := Rect(l, R.Top, l + Canvas.TextWidth(lblText), R.Bottom);
       Canvas.Brush.Style := bsClear;
       if (TR.Right < R.Right) or ((TR.Right < R.Right + Canvas.TextWidth(lblText)/2) and FShowLast) then
        DrawText(Canvas.Handle, PChar(lblText), Length(lblText), TR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
       Canvas.Brush.Style := bsSolid;
      end;
    end;
  end;
 // draw the half & quarter lines
 //if the numbers fit
 if maxwidth < FWholeSkip then
  begin
   if (FWholeSkip - maxwidth)/2  > 2 then
    begin
     DrawHalfLines;
     if (FWholeSkip/2 - maxwidth) > 5 then
      DrawQuarterLines;
    end;
  end
 else
  // if the numbers don't fit
  if FHalfSkip > 2 then
   begin
    DrawHalfLines;
    if FQuartSkip > 2 then
     DrawQuarterLines;
   end;
 //draw the mouse coord line
 if (mx > R.Left - 1) and (mx < R.Right + 1) then
  begin
   Canvas.Pen.Color := FLineCl;
   Canvas.MoveTo(mx, R.Top);
   Canvas.LineTo(mx, R.Bottom);
  end;
end;

procedure TmbXPHorizontalRuler.DrawHalfLines;
var
 i, x: integer;
begin
 for i := 1 to Round(FieldLength/FHalfSkip) - 1 do
  if i mod 2 <> 0 then
   begin
    x := R.Left + Round(i*FHalfSkip);
    if x < R.Right - FQuartSkip/2 then
     begin
      Canvas.MoveTo(x, R.Top + 2);
      Canvas.LineTo(x, R.Bottom - 2);
     end;
   end;
end;

procedure TmbXPHorizontalRuler.DrawQuarterLines;
var
 i, x: integer;
begin
 for i := 1 to Round(FieldLength/FQuartSkip) - 1 do
  if i mod 2 <> 0 then
   begin
    x := R.Left + Round(i*FQuartSkip);
    Canvas.MoveTo(x, R.Top + 4);
    Canvas.LineTo(x, R.Bottom - 4);
   end;
end;

procedure TmbXPHorizontalRuler.SetSOffset(o: integer);
begin
 if FSOffset <> o then
  begin
   FSOffset := o;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetEOffset(o: integer);
begin
 if FEOffset <> o then
  begin
   FEOffset := o;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetVOffset(o: integer);
begin
 if FVOffset <> o then
  begin
   FVOffset := o;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetUnit(u: TCSSMeasureUnit);
begin
 if FUnits <> u then
  begin
   FUnits := u;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetStartAt(s: extended);
begin
 if FStartAt <> s then
  begin
   FStartAt := s;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetSkipUnits(s: extended);
begin
 if FSkipUnits <> s then
  begin
   FSkipUnits := s;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.UpdateDisplay(x: integer);
begin
 mx := x;
 invalidate;
end;

procedure TmbXPHorizontalRuler.SetPos(p: TRulerPosition);
begin
 if FPos <> p then
  begin
   FPos := p;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.MarkFragment(ALeft, ARight: integer);
begin
 FMLeft := ALeft;
 FMRight := ARight;
 invalidate;
end;

function TmbXPHorizontalRuler.GetPosition(x: integer): extended;
begin
 if x > Width - FEOffset then
  x := Width - FEOffset;
 if x < FSOffset then
  x := FSOffset;
 Result := (x - FSOffset)/FOneUnit;
end;

procedure TmbXPHorizontalRuler.SetScale(s: integer);
begin
 if s < 1 then s := 1;
 if FScale <> s then
  begin
   FScale := s;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetOuterCl(c: TColor);
begin
 if FOuterCl <> c then
  begin
   FOuterCl := c;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetInnerCl(c: TColor);
begin
 if FInnerCl <> c then
  begin
   FInnerCl := c;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetLineCl(c: TColor);
begin
 if FLineCl <> c then
  begin
   FLineCl := c;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetMarkCl(c: TColor);
begin
 if FMarkCl <> c then
  begin
   FMarkCl := c;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetCustomDPI(Value: integer);
begin
 if FCustomDPI <> Value then
  begin
   FCustomDPI := Value;
   Invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetShowFirst(Value: boolean);
begin
 if FShowFirst <> Value then
  begin
   FShowFirst := Value;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetShowLast(Value: boolean);
begin
 if FShowLast <> Value then
  begin
   FShowLast := Value;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetAddaptRectLeft(Value: boolean);
begin
 if FAddaptRectLeft <> Value then
  begin
   FAddaptRectLeft := Value;
   invalidate;
  end;
end;

procedure TmbXPHorizontalRuler.SetAddaptRectRight(Value: boolean);
begin
 if FAddaptRectRight <> Value then
  begin
   FAddaptRectRight := Value;
   invalidate;
  end;
end;

end.
