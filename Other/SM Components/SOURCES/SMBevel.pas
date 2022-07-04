{ Copyright (C) 1998-2004, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  TSMBevel component is extension of the standart TBevel
  and have additional features:
   - except standart styles (mbsLowered, mbsRaised) I add a new style
     mbsNone - usual line or triangle
   - except standart Shapes (mbsBox, mbsFrame, mbsTopLine,
    mbsBottomLine, mbsLeftLine, mbsRightLine) I add the new shapes
      mbsBoxIn - beautiful frame
      mbsTopLeftLine - diagonal right-to-left line
      mbsTopRightLine - diagonal left-to-right line
      mbsTriangle - standard triangle figure
      mbsOctogon - standard octogon figure
      mbsStar - star figure
      mbsRoundRect
      mbsSquare
      mbsRoundSquare
      mbsEllipse

    - added feature of the arrow drawing
      property Arrow (if Style not in [mbsBox, mbsBoxIn, mbsFrame]):
        smaFinish - arrow on end line
        smaStart - arrow on start line
        smaNone - arrow not available

      property ArrowType:
        atOutline - drawing a arrow contour only
        atSolid - drawing a fill arrow

      property ArrowAngle - angle of the arrow deviation from main line (in degrees)
      property ArrowLength - length of oblique side


    Written by Tommy Dillard request (dillard@cyberramp.net):
    1. added two properties TopColor/BottomColor - colors for Raised/Lowered lines
    2. fixed error:
      - not correct draw the arrow
        with Arrow = smaFinish and Shape = mbsTopRightLine
}

unit SMBevel;

interface

uses
  Windows, Messages, Classes, Graphics, Controls;

type
  TSMBevelStyle = (mbsLowered, mbsNone, mbsRaised);
  TSMBevelShape = (mbsBox, mbsBoxIn, mbsFrame, mbsTopLine, mbsBottomLine, mbsLeftLine,
    mbsRightLine, mbsTopLeftLine, mbsTopRightLine,
    mbsStar, mbsTriangle, mbsOctogon, mbsRoundRect, mbsSquare, mbsRoundSquare, mbsEllipse);

  TSMArrowType = (atOutline, atSolid);
  TSMArrow = (smaNone, smaStart, smaFinish);
  TTextLayout = (tlTop, tlCenter, tlBottom);

  TSMBevel = class(TGraphicControl)
  private
    { Private declarations }

    FFocusControl: TWinControl;

    {arrows for line}
    FArrow: TSMArrow;
    FArrowType: TSMArrowType;
    FArrowAngle: Integer;
    FArrowLength: Integer;

    {relation description}
    FAlignment: TAlignment;
    FLayout: TTextLayout;

    FBevelWidth: Integer;
    FStyle: TSMBevelStyle;
    FShape: TSMBevelShape;
    FPenStyle: TPenStyle;

    {Added by Tommy Dillard}
    FTopColor: TColor;
    FBottomColor: TColor;
    procedure SetTopColor(Value: TColor);
    procedure SetBottomColor(Value: TColor);
    {End Addition by Tommy Dillard}

    procedure SetFocusControl(Value: TWinControl);

    procedure SetArrow(Value: TSMArrow);
    procedure SetArrowType(Value: TSMArrowType);
    procedure SetArrowAngle(Value: Integer);
    procedure SetArrowLength(Value: Integer);

    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(Value: TTextLayout);

    procedure SetBevelWidth(Value: Integer);
    procedure SetStyle(Value: TSMBevelStyle);
    procedure SetShape(Value: TSMBevelShape);
    procedure SetPenStyle(Value: TPenStyle);

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure DoDrawText(var Rect: TRect; Flags: Word);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Arrow: TSMArrow read FArrow write SetArrow;
    property ArrowType: TSMArrowType read FArrowType write SetArrowType;
    property ArrowAngle: Integer read FArrowAngle write SetArrowAngle;
    property ArrowLength: Integer read FArrowLength write SetArrowLength;

    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle;

    {Added by Tommy Dillard}
    property TopColor: TColor read FTopColor write SetTopColor;
    property BottomColor: TColor read FBottomColor write SetBottomColor;
    {End Addition by Tommy Dillard}

    property Align;
    property Caption;
    property Font;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ParentShowHint;
    property Shape: TSMBevelShape read FShape write SetShape default mbsBox;
    property ShowHint;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth;
    property Style: TSMBevelStyle read FStyle write SetStyle default mbsLowered;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMBevel]);
end;

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

{ TSMBevel }
constructor TSMBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable];

  FBevelWidth := 1;
  FStyle := mbsLowered;
  FShape := mbsBox;
  Width := 50;
  Height := 50;

  FPenStyle := psSolid;

  FArrow := smaFinish;
  FArrowAngle := 25;
  FArrowLength := 15;
  FAlignment := taCenter;
  FArrowType := atSolid;

  {Added by Tommy Dillard}
  FTopColor := clBtnHighlight;
  FBottomColor := clBtnShadow;
end;

procedure TSMBevel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TSMBevel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure TSMBevel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

{Added by Tommy Dillard}
procedure TSMBevel.SetTopColor(Value: TColor);
begin
  if (Value <> FTopColor) then
  begin
    FTopColor := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetBottomColor(Value: TColor);
begin
  if (Value <> FBottomColor) then
  begin
    FBottomColor := Value;
    Invalidate;
  end;
end;
{End Additon by Tommy Dillard}

procedure TSMBevel.SetArrow(Value: TSMArrow);
begin
  if (Value <> FArrow) then
  begin
    FArrow := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetArrowType(Value: TSMArrowType);
begin
  if (Value <> FArrowType) then
  begin
    FArrowType := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetArrowAngle(Value: Integer);
begin
  if (Value <> FArrowAngle) then
  begin
    FArrowAngle := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetArrowLength(Value: Integer);
begin
  if (Value <> FArrowLength) then
  begin
    FArrowLength := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetAlignment(Value: TAlignment);
begin
  if (Value <> FAlignment) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetLayout(Value: TTextLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetBevelWidth(Value: Integer);
begin
  if (Value <> FBevelWidth) and (Value > 0) then
  begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetStyle(Value: TSMBevelStyle);
begin
  if (Value <> FStyle) then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetShape(Value: TSMBevelShape);
begin
  if (Value <> FShape) then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.SetPenStyle(Value: TPenStyle);
begin
  if (Value <> FPenStyle) then
  begin
    FPenStyle := Value;
    Invalidate;
  end;
end;

procedure TSMBevel.DoDrawText(var Rect: TRect; Flags: Word);
var
  Text: string;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and (Text = '') then
    Text := Text + ' ';
  Flags := Flags or DT_NOPREFIX;
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
end;

procedure TSMBevel.Paint;
var
  Color1, Color2: TColor;
  Temp: TColor;

  function DegToRad(Degrees: Extended): Extended;
  begin
    Result := Degrees*(PI/180);
  end;

  procedure BevelRect(const R: TRect);
  begin
    with Canvas do
    begin
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
        Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
        Point(R.Left, R.Bottom)]);
    end;
  end;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure BevelArrow(C1, C2: TColor; ARect: TRect);
  var
    Alpha, Beta: Extended;
  begin
    with Canvas do
    begin
      Pen.Color := C1;
      Brush.Color := C2;

      Beta := DegToRad(FArrowAngle)/2;
      if FArrowType = atOutline then
      begin
        if (FArrow = smaStart) then
        begin
          Alpha := ArcTan((ARect.Right - ARect.Left)/(ARect.Bottom - ARect.Top));

          MoveTo(Round(ARect.Left + FArrowLength*Sin(Alpha-Beta)), Round(ARect.Top + FArrowLength*Cos(Alpha-Beta)));
          LineTo(ARect.Left, ARect.Top);
          LineTo(Round(ARect.Left + FArrowLength*Sin(Alpha+Beta)), Round(ARect.Top + FArrowLength*Cos(Alpha+Beta)))
        end
        else // smaFinish
        begin
          Alpha := ArcTan((ARect.Bottom - ARect.Top)/(ARect.Right - ARect.Left));

          if (ARect.Right > ARect.Left) then
          begin
            MoveTo(Round(ARect.Right - FArrowLength*Cos(Alpha-Beta)), Round(ARect.Bottom - FArrowLength*Sin(Alpha-Beta)));
            LineTo(ARect.Right, ARect.Bottom);
            LineTo(Round(ARect.Right - FArrowLength*Cos(Alpha+Beta)), Round(ARect.Bottom - FArrowLength*Sin(Alpha+Beta)))
          end
          else
          begin
            MoveTo(Round(ARect.Right + FArrowLength*Cos(Alpha-Beta)), Round(ARect.Bottom + FArrowLength*Sin(Alpha-Beta)));
            LineTo(ARect.Right, ARect.Bottom);
            LineTo(Round(ARect.Right + FArrowLength*Cos(Alpha+Beta)), Round(ARect.Bottom + FArrowLength*Sin(Alpha+Beta)))
          end
        end;
      end
      else
      begin //atSolid
        if (FArrow = smaStart) then
        begin
          Alpha := ArcTan((ARect.Right - ARect.Left)/(ARect.Bottom - ARect.Top));
          Polygon([Point(ARect.Left, ARect.Top),
                   Point(Round(ARect.Left + FArrowLength*Sin(Alpha+Beta)), Round(ARect.Top + FArrowLength*Cos(Alpha+Beta))),
                   Point(Round(ARect.Left + FArrowLength*Sin(Alpha-Beta)), Round(ARect.Top + FArrowLength*Cos(Alpha-Beta)))]);
        end
        else // smaFinish
        begin
          Alpha := ArcTan((ARect.Bottom - ARect.Top)/(ARect.Right - ARect.Left));

          if (ARect.Right > ARect.Left) then
            Polygon([Point(ARect.Right, ARect.Bottom),
                     Point(Round(ARect.Right - FArrowLength*Cos(Alpha+Beta)), Round(ARect.Bottom - FArrowLength*Sin(Alpha+Beta))),
                     Point(Round(ARect.Right - FArrowLength*Cos(Alpha-Beta)), Round(ARect.Bottom - FArrowLength*Sin(Alpha-Beta)))])
          else
            Polygon([Point(ARect.Right, ARect.Bottom),
                     Point(Round(ARect.Right + FArrowLength*Cos(Alpha+Beta)), Round(ARect.Bottom + FArrowLength*Sin(Alpha+Beta))),
                     Point(Round(ARect.Right + FArrowLength*Cos(Alpha-Beta)), Round(ARect.Bottom + FArrowLength*Sin(Alpha-Beta)))])
        end;
      end;
    end;
  end;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ArrowHeight, S: Integer;
  ARect, CalcRect: TRect;
  DrawStyle: Integer;

  PtsS: array[0..5] of TPoint;
  PtsT: array[0..3] of TPoint;
  PtsO: array[0..8] of TPoint;
  GapX, GapY: Integer;
begin
  ArrowHeight := Round(FArrowLength*Sin(DegToRad(FArrowAngle)/2));

  with Canvas do
  begin
    Pen.Width := BevelWidth;
    Pen.Style := FPenStyle;

    case FStyle of
      mbsLowered: begin
                   Color1 := FBottomColor;
                   Color2 := FTopColor;
                 end;
      mbsRaised: begin
                   Color1 := FTopColor;
                   Color2 := FBottomColor;
                 end;
      else
      begin
        Color1 := clWindowFrame;
        Color2 := clWindowFrame;
      end
    end;

    case FShape of
      mbsBox: begin
                ARect := Rect(0, 0, Width, Height);
//                BevelRect(ARect);
                Frame3D(Canvas, ARect, Color1, Color2, BevelWidth);
              end;

      mbsFrame: begin
                  ARect := Rect(BevelWidth, BevelWidth, Width, Height);
                  Frame3D(Canvas, ARect, Color1, Color1, BevelWidth);
                  ARect := Rect(0, 0, Width-BevelWidth, Height-BevelWidth);
                  Frame3D(Canvas, ARect, Color2, Color2, BevelWidth);

                  ARect := Rect(BevelWidth, BevelWidth, Width - 2*BevelWidth, Height - 2*BevelWidth);
                end;

      mbsBoxIn: begin
                   ARect := Rect(0, 0, Width, Height);
                   Frame3D(Canvas, ARect, Color1, Color2, BevelWidth);
                   ARect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, Height - BevelWidth);
                   Frame3D(Canvas, ARect, Color2, Color1, BevelWidth);

                   ARect := Rect(BevelWidth, BevelWidth, Width - 2*BevelWidth, Height - 2*BevelWidth);
                 end;

      mbsTopLine: if (FArrow = smaNone) then
                  begin
                    BevelLine(Color1, 0, BevelWidth div 2, Width, BevelWidth div 2);
                    BevelLine(Color2, 0, 3*BevelWidth div 2, Width, 3*BevelWidth div 2);
                    ARect := Rect(0, 0, Width, BevelWidth);
                  end
                  else
                  begin
                    BevelLine(Color1, 0, ArrowHeight, Width, ArrowHeight);
                    BevelLine(Color2, 0, ArrowHeight + BevelWidth, Width, ArrowHeight + BevelWidth);

                    ARect := Rect(0, ArrowHeight, Width, ArrowHeight + BevelWidth);
                    BevelArrow(Color1, Color2, ARect);
                  end;

      mbsBottomLine: if (FArrow = smaNone) then
                     begin
                       BevelLine(Color1, 0, Height - 3*BevelWidth div 2, Width, Height - 3*BevelWidth div 2);
                       BevelLine(Color2, 0, Height - BevelWidth div 2, Width, Height - BevelWidth div 2);
                       ARect := Rect(0, Height - 2*BevelWidth, Width, Height - BevelWidth);
                     end
                     else
                     begin
                       BevelLine(Color1, 0, Height - ArrowHeight - 2*BevelWidth, Width, Height - ArrowHeight - 2*BevelWidth);
                       BevelLine(Color2, 0, Height - ArrowHeight - BevelWidth, Width, Height - ArrowHeight - BevelWidth);

                       ARect := Rect(0, Height - ArrowHeight - 2*BevelWidth, Width, Height - ArrowHeight - BevelWidth);
                       BevelArrow(Color1, Color2, ARect);
                     end;

      mbsLeftLine: if (FArrow = smaNone) then
                   begin
                     BevelLine(Color1, BevelWidth div 2, 0, BevelWidth div 2, Height);
                     BevelLine(Color2, 3*BevelWidth div 2, 0, 3*BevelWidth div 2, Height);
                     ARect := Rect(0, 0, BevelWidth, Height);
                   end
                   else
                   begin
                     BevelLine(Color1, ArrowHeight, 0, ArrowHeight, Height);
                     BevelLine(Color2, ArrowHeight + BevelWidth, 0, ArrowHeight + BevelWidth, Height);

                     ARect := Rect(ArrowHeight, 0, ArrowHeight + BevelWidth, Height);
                     BevelArrow(Color1, Color2, ARect);
                   end;

      mbsRightLine: if (FArrow = smaNone) then
                    begin
                      BevelLine(Color1, Width - 3*BevelWidth div 2, 0, Width - 3*BevelWidth div 2, Height);
                      BevelLine(Color2, Width - BevelWidth div 2, 0, Width - BevelWidth div 2, Height);
                      ARect := Rect(Width - 2*BevelWidth, 0, Width - BevelWidth, Height);
                    end
                    else
                    begin
                      BevelLine(Color1, Width - ArrowHeight - 2*BevelWidth, 0, Width - ArrowHeight - 2*BevelWidth, Height);
                      BevelLine(Color2, Width - ArrowHeight - BevelWidth, 0, Width - ArrowHeight - BevelWidth, Height);

                      ARect := Rect(Width - ArrowHeight - 2*BevelWidth, 0, Width - ArrowHeight - BevelWidth, Height);
                      BevelArrow(Color1, Color2, ARect);
                    end;

      mbsTopLeftLine: if (FArrow = smaNone) then
                      begin
                        BevelLine(Color1, 0, 0, Width, Height - 2*BevelWidth);
                        BevelLine(Color2, 0, BevelWidth, Width, Height - BevelWidth);
                        ARect := Rect(0, 0, Width, Height - BevelWidth);
                      end
                      else
                      begin
                        BevelLine(Color1, 0, 0, Width, Height - 2*BevelWidth);
                        BevelLine(Color2, 0, BevelWidth, Width, Height - BevelWidth);

                        ARect := Rect(0, 0, Width, Height - BevelWidth);
                        BevelArrow(Color1, Color2, ARect);
                      end;

      mbsTopRightLine: if (FArrow = smaNone) then
                       begin
                         BevelLine(Color1, Width, 0, 0, Height - 2*BevelWidth);
                         BevelLine(Color2, Width, BevelWidth, BevelWidth, Height - BevelWidth);
                         ARect := Rect(Width, 0, BevelWidth, Height - BevelWidth);
                       end
                       else
                       begin
                         BevelLine(Color1, Width, 0, 0, Height - 2*BevelWidth);
                         BevelLine(Color2, Width, BevelWidth, BevelWidth, Height - BevelWidth);

                         ARect := Rect(Width, 0, BevelWidth, Height - BevelWidth);
                         BevelArrow(Color1, Color2, ARect);
                       end;

      mbsStar: begin
                 Pen.Color := Color1;
                 ARect := Rect(BevelWidth, BevelWidth, Width, Height);

                 GapX := (ARect.Right - ARect.Left) div 6;
                 GapY := (ARect.Bottom - ARect.Top) div 6;
                 PtsS[0].X := (ARect.Right - ARect.Left) div 2;
                 PtsS[0].Y := ARect.Top;
                 PtsS[1].X := ARect.Left + GapX;
                 PtsS[1].Y := ARect.Bottom - GapY;
                 PtsS[2].X := ARect.Right;
                 PtsS[2].Y := ((ARect.Bottom - ARect.Top) div 2) - GapY;
                 PtsS[3].X := ARect.Left;
                 PtsS[3].Y := ((ARect.Bottom - ARect.Top) div 2) - GapY;
                 PtsS[4].X := ARect.Right - GapX;
                 PtsS[4].Y := ARect.Bottom - GapY;
                 PtsS[5].X := (ARect.Right - ARect.Left) div 2;
                 PtsS[5].Y := ARect.Top;
                 Canvas.Polyline(PtsS);
               end;

      mbsTriangle: begin
                     Pen.Color := Color1;
                     ARect := Rect(BevelWidth, BevelWidth, Width, Height);

                     PtsT[0].X := ARect.Left;
                     PtsT[0].Y := ARect.Bottom-1;
                     PtsT[1].X := (ARect.Right - ARect.Left) div 2;
                     PtsT[1].Y := ARect.Top;
                     PtsT[2].X := ARect.Right;
                     PtsT[2].Y := ARect.Bottom-1;
                     PtsT[3].X := ARect.Left;
                     PtsT[3].Y := ARect.Bottom-1;
                     Canvas.Polyline(PtsT);
                   end;

      mbsOctogon: begin
                    Pen.Color := Color1;
                    ARect := Rect(BevelWidth, BevelWidth, Width, Height);

                    GapX := (ARect.Right - ARect.Left) div 3;
                    GapY := (ARect.Bottom - ARect.Top) div 3;
                    PtsO[0].X := ARect.Left + GapX;
                    PtsO[0].Y := ARect.Top;
                    PtsO[1].X := ARect.Right - GapX;
                    PtsO[1].Y := ARect.Top;
                    PtsO[2].X := ARect.Right-1;
                    PtsO[2].Y := ARect.Top + GapY;
                    PtsO[3].X := ARect.Right-1;
                    PtsO[3].Y := ARect.Bottom - GapY;
                    PtsO[4].X := ARect.Right - GapX;
                    PtsO[4].Y := ARect.Bottom-1;
                    PtsO[5].X := ARect.Left + GapX;
                    PtsO[5].Y := ARect.Bottom-1;
                    PtsO[6].X := ARect.Left;
                    PtsO[6].Y := ARect.Bottom - GapY;
                    PtsO[7].X := ARect.Left;
                    PtsO[7].Y := ARect.Top + GapY;
                    PtsO[8].X := ARect.Left + GapX;
                    PtsO[8].Y := ARect.Top;
                    Canvas.Polyline(PtsO);
                  end;

      mbsRoundRect: begin
                      Pen.Color := Color1;
                      Canvas.RoundRect(0, 0, Width, Height, 10, 10);
                    end;

      mbsSquare: begin
                   if Width < Height then
                     S := Width
                   else
                     S := Height;

                   ARect := Rect(BevelWidth + (Width-S) div 2, BevelWidth + (Height-S) div 2, (Width+S) div 2, (Height+S) div 2);
                   Frame3D(Canvas, ARect, Color1, Color1, BevelWidth);
                   ARect := Rect((Width-S) div 2, (Height-S) div 2, (Width+S) div 2 - BevelWidth, (Height+S) div 2 -BevelWidth);
                   Frame3D(Canvas, ARect, Color2, Color2, BevelWidth);
                 end;

      mbsRoundSquare: begin
                        Pen.Color := Color1;
                        if Width < Height then
                          S := Width
                        else
                          S := Height;
                        Canvas.RoundRect((Width-S) div 2, (Height-S) div 2, (Width+S) div 2, (Height+S) div 2, 10, 10);
                      end;

      mbsEllipse: begin
                    Pen.Color := Color1;
                    Canvas.Ellipse(0, 0, Width, Height);
                  end;
    end;

    {draw caption}
    Brush.Style := bsClear;
    DrawStyle := DT_EXPANDTABS or DT_WORDBREAK or Alignments[FAlignment];

    { Calculate vertical layout }
    ARect := ClientRect;
    if (FLayout <> tlTop) then
    begin
      CalcRect := ARect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if (FLayout = tlBottom) then
        OffsetRect(ARect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(ARect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(ARect, DrawStyle);
  end;
end;

end.
