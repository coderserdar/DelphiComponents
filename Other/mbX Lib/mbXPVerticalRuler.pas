unit mbXPVerticalRuler;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, dialogs;

type
  TCSSMeasureUnit = (muCentimeters, muMilimeters, muInches, muPixels, muPoints, muPicas);
  TRulerPosition = (rpLeft, rpRight);

  TPaintLabelEvent = procedure (LabelFont: TFont; var LabelText: string) of object;

  TmbXPVerticalRuler = class(TCustomControl)
  private
   FSOffset, FEOffset, FHOffset: integer;
   FUnits: TCSSMeasureUnit;
   FWholeSkip, FHalfSkip, FQuartSkip, FOneUnit: extended;
   R: TRect;
   FieldLength: integer;
   FStartAt, FSkipUnits: extended;
   my: integer;
   FPos: TRulerPosition;
   FMTop, FMBottom: integer;
   FScale: integer;
   FOuterCl, FInnerCl, FMarkCl, FLineCl: TColor;
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
   procedure SetScale(s: integer);
   procedure SetPos(p: TRulerPosition);
   procedure SetSkipUnits(s: extended);
   procedure SetStartAt(s: extended);
   procedure SetUnit(u: TCSSMeasureUnit);
   procedure SetSOffset(o: integer);
   procedure SetEOffset(o: integer);
   procedure SetHOffset(o: integer);
   procedure SetOuterCl(c: TColor);
   procedure SetInnerCl(c: TColor);
   procedure SetLineCl(c: TColor);
   procedure SetMarkCl(c: TColor);
  protected
   procedure Paint; override;
   procedure DrawWholeLines;
   procedure DrawHalfLines;
   procedure DrawQuarterLines;
  public
   constructor Create(AOwner: TComponent); override;
   procedure UpdateDisplay(y: integer);
   procedure MarkFragment(ATop, ABottom: integer);
   function GetPosition(y: integer): extended;

   property MarkTop: integer read FMTop;
   property MarkBottom: integer read FMBottom;
   property Canvas;
  published
   property OffsetTop: integer read FSOffset write SetSOffset default 0;
   property OffsetBottom: integer read FEOffset write SetEOffset default 23;
   property HorizontalOffset: integer read FHOffset write SetHOffset default 5;
   property Units: TCSSMeasureUnit read FUnits write SetUnit default muPixels;
   property StartAt: extended read FStartAt write SetStartAt;
   property SkipUnits: extended read FSkipUnits write SetSkipUnits;
   property RulerPosition: TRulerPosition read FPos write SetPos default rpLeft;
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

procedure DrawRotatedText(Canvas: TCanvas; Angle: Integer; Text: WideString; R: TRect);
var
 lf : TLogFont;
 tf : TFont;
begin
 with Canvas do
  begin
   Brush.Style := bsClear;
   tf := TFont.Create;
   try
    tf.Assign(Font);
    GetObject(tf.Handle, sizeof(lf), @lf);
    lf.lfEscapement := Angle * 10;
    lf.lfOrientation := Angle * 10;
    tf.Handle := CreateFontIndirect(lf);
    Font.Assign(tf);
   finally
    tf.Free;
   end;
   if Angle = 90 then
    begin
     R.Left := R.Left + ((R.Right - R.Left) - Canvas.TextHeight(Text)) div 2;
     R.Right := R.Left + Canvas.TextHeight(Text);
     DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, DT_SINGLELINE or DT_NOCLIP);
    end;
   if Angle = 270 then
    begin
     R := Rect(R.Right, R.Bottom, R.Left, R.Top);
     R.Right := R.Right + ((R.Left - R.Right) - Canvas.TextHeight(Text)) div 2;
     R.Left := R.Right + Canvas.TextHeight(Text);
     DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, DT_SINGLELINE or DT_NOCLIP);
    end;
  end;
end;

constructor TmbXPVerticalRuler.Create(AOwner: TComponent);
begin
 inherited;
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 FSOffset := 0;
 FEOffset := 23;
 FHOffset := 5;
 Width := 23;
 Height := 23;
 FUnits := muPixels;
 Align := alLeft;
 FStartAt := 0;
 FSkipUnits := 50;
 my := -1;
 FPos := rpLeft;
 FMTop := 0;
 FMBottom := 0;
 FScale := 100;
 FOneUnit := 1 * (FScale/100);
 FCustomDPI := 0;
 //set colors
 FOuterCl := clBtnFace;
 FInnerCl := clWindow;
 FLineCl := clBtnShadow;
 FMarkCl := clHighlight;
 FShowFirst := false;
 FShowLast := false;
 FAddaptRectLeft := false;
 FAddaptRectRight := false;
 Font.Name := 'MS Shell Dlg 2';
end;

procedure TmbXPVerticalRuler.Paint;
var
 markT, markB, max: integer;
 ppi: integer;
begin
 R := ClientRect;
 //paint back rect
 Canvas.Brush.Color := FOuterCl;
 Canvas.FillRect(R);
 //set field rect
 Inc(R.Top, FSOffset);
 Dec(R.Bottom, FEOffset);
 InflateRect(R, -FHOffset, 0);
 //paint field rect
 Canvas.Brush.Color := FInnerCl;
 //calc max
 if FWholeSkip = 0 then FWholeSkip := FSkipUnits;
 max := Round(FieldLength/FWholeSkip);
 //Addapt the rect for the first number
 if FShowFirst and FAddaptRectLeft then Dec(R.Top, Canvas.TextWidth(IntToStr(FSOffset)) div 2);
 //now for the last number
 if FShowLast and FAddaptRectRight then Inc(R.Bottom, Canvas.TextWidth(IntToStr(max)) div 2);
 //paint the inner area
 Canvas.FillRect(R);
 //restore the rect
 if FShowFirst and FAddaptRectLeft then Inc(R.Top, Canvas.TextWidth(IntToStr(FSOffset)) div 2);
 if FShowLast and FAddaptRectRight then Dec(R.Bottom, Canvas.TextWidth(IntToStr(max)) div 2);
 // set pixels per inch
 if FCustomDPI > 0 then
  ppi := FCustomDPI
 else
  ppi := Screen.PixelsPerInch;
 //set measures
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
 FieldLength := R.Bottom - R.Top;
 FWholeSkip := FOneUnit * FSkipUnits;
 FHalfSkip := FWholeSkip/2;
 FQuartSkip := FHalfSkip/2;
 //mark fragment
 Canvas.Brush.Color := FMarkCl;
 markT := FMTop;
 markB := FMBottom;
 if FMTop < R.Top then
  markT := R.Top;
 if FMTop > R.Bottom then
  markT := R.Bottom;
 if FMBottom > R.Bottom then
  markB := R.Bottom;
 if FMBottom < R.Top then
  markB := R.Top;
 Canvas.FillRect(Rect(R.Left, markT, R.Right, markB));
 //paint ticks & text
 Canvas.Pen.Color := FLineCl;
 DrawWholeLines;
 if Assigned(FOnPaint) then
  FOnPaint(Self);
end;

function TmbXPVerticalRuler.GetMaxWidth: integer;
var
 i, max: integer;
begin
 max := 0;
 for i := 1 to Round(FieldLength/FWholeSkip) do
  if Canvas.TextWidth(FloatToStr(i*FSkipUnits + FStartAt)) > max then
   max := Canvas.TextWidth(FloatToStr(i*FSkipUnits + FStartAt));
 Result := max;
end;

procedure TmbXPVerticalRuler.DrawWholeLines;
var
 FAngle, i, y, t, maxwidth, s: integer;
 TR: TRect;
 lblText: string;
begin
 FAngle := 90;
 maxwidth := GetMaxWidth;
 if FShowFirst then
  s := 0
 else
  s := 1;
 for i := s to Round(FieldLength/FWholeSkip) do
  begin
   y := R.Top + Round(i*FWholeSkip);
   if (y < R.Bottom - FQuartSkip/2) or ((y <= R.Bottom) and FShowLast) then
    begin
     //draw whole unit lines if they fit
     if FWholeSkip > 1 then
      case FPos of
       rpLeft:
        begin
         Canvas.MoveTo(R.Right, y);
         Canvas.LineTo(Width, y);
        end;
       rpRight:
        begin
         Canvas.MoveTo(0, y);
         Canvas.LineTo(R.Left, y);
        end;
      end;
     Canvas.Font := Font;
     lblText := FloatToStr(i*FSkipUnits + FStartAt);
     if Assigned(FOnPaintLabel) then FOnPaintLabel(Canvas.Font, lblText);
     //draw the numbers if they fit
     if maxwidth < FWholeSkip then
      begin
       t := Round(y - (Canvas.TextWidth(lblText)/2));
       TR := Rect(R.Left, t + Canvas.TextWidth(lblText), R.Right, t);
       case FPos of
        rpLeft: FAngle := 90;
        rpRight: FAngle := 270;
       end;
       if (TR.Bottom < R.Bottom) or ((TR.Bottom < R.Bottom + Canvas.TextWidth(lblText)/2) and FShowLast) then
        DrawRotatedText(Canvas, FAngle, lblText, TR);
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
 if (my > R.Top - 1) and (my < R.Bottom + 1) then
  begin
   Canvas.Pen.Color := FLineCl;
   Canvas.MoveTo(R.Left, my);
   Canvas.LineTo(R.Right, my);
  end;
end;

procedure TmbXPVerticalRuler.DrawHalfLines;
var
 i, y: integer;
begin
 for i := 1 to Round(FieldLength/FHalfSkip) - 1 do
  if i mod 2 <> 0 then
   begin
    y := R.Top + Round(i*FHalfSkip);
    if y < R.Bottom - FQuartSkip/2 then
     begin
      Canvas.MoveTo(R.Left + 2, y);
      Canvas.LineTo(R.Right - 2, y);
     end;
   end;
end;

procedure TmbXPVerticalRuler.DrawQuarterLines;
var
 i, y: integer;
begin
 for i := 1 to Round(FieldLength/FQuartSkip) - 1 do
  if i mod 2 <> 0 then
   begin
    y := R.Top + Round(i*FQuartSkip);
    Canvas.MoveTo(R.Left + 4, y);
    Canvas.LineTo(R.Right - 4, y);
   end;
end;

procedure TmbXPVerticalRuler.SetSOffset(o: integer);
begin
 if FSOffset <> o then
  begin
   FSOffset := o;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetEOffset(o: integer);
begin
 if FEOffset <> o then
  begin
   FEOffset := o;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetHOffset(o: integer);
begin
 if FHOffset <> o then
  begin
   FHOffset := o;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetUnit(u: TCSSMeasureUnit);
begin
 if FUnits <> u then
  begin
   FUnits := u;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetStartAt(s: extended);
begin
 if FStartAt <> s then
  begin
   FStartAt := s;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetSkipUnits(s: extended);
begin
 if FSkipUnits <> s then
  begin
   FSkipUnits := s;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.UpdateDisplay(y: integer);
begin
 my := y;
 invalidate;
end;

procedure TmbXPVerticalRuler.SetPos(p: TRulerPosition);
begin
 if FPos <> p then
  begin
   FPos := p;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.MarkFragment(ATop, ABottom: integer);
begin
 FMTop := ATop;
 FMBottom := ABottom;
 invalidate;
end;

function TmbXPVerticalRuler.GetPosition(y: integer): extended;
begin
 if y > Height - FEOffset then
  y := Height - FEOffset;
 if y < FSOffset then
  y := FSOffset;
 Result := (y - FSOffset)/FOneUnit;
end;

procedure TmbXPVerticalRuler.SetScale(s: integer);
begin
 if s < 1 then s := 1;
 if FScale <> s then
  begin
   FScale := s;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetOuterCl(c: TColor);
begin
 if FOuterCl <> c then
  begin
   FOuterCl := c;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetInnerCl(c: TColor);
begin
 if FInnerCl <> c then
  begin
   FInnerCl := c;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetLineCl(c: TColor);
begin
 if FLineCl <> c then
  begin
   FLineCl := c;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetMarkCl(c: TColor);
begin
 if FMarkCl <> c then
  begin
   FMarkCl := c;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetCustomDPI(Value: integer);
begin
 if FCustomDPI <> Value then
  begin
   FCustomDPI := Value;
   Invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetShowFirst(Value: boolean);
begin
 if FShowFirst <> Value then
  begin
   FShowFirst := Value;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetShowLast(Value: boolean);
begin
 if FShowLast <> Value then
  begin
   FShowLast := Value;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetAddaptRectLeft(Value: boolean);
begin
 if FAddaptRectLeft <> Value then
  begin
   FAddaptRectLeft := Value;
   invalidate;
  end;
end;

procedure TmbXPVerticalRuler.SetAddaptRectRight(Value: boolean);
begin
 if FAddaptRectRight <> Value then
  begin
   FAddaptRectRight := Value;
   invalidate;
  end;
end;

end.
