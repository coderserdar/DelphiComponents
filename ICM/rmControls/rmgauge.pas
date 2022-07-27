{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmGuage
Purpose  : Visual UI eye-candy type gages.
Date     : 09-03-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmGauge;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, math;

type
  TGaugeShape = (gsTriangle, gsVertRect, gsHorzRect, gsEllipse, gsArc, gsPole);
  TGaugeBorder = (gbNone, gbSingle, gbRaised, gbLowered);
  TrmGauge = class(TGraphicControl)
  private
    { Private declarations }
    fBorder : TGaugeBorder;
    fShape : TGaugeShape;
    fgradient, fusemiddle: boolean;
    fpercent : integer;
    fStartColor, fMiddleColor, fEndColor : TColor;
    fOnChangeEvent : TNotifyEvent;
    procedure SetGradient(Value:Boolean);
    procedure SetShape(Value : TGaugeShape);
    procedure SetBorder(Value : TGaugeBorder);
    procedure SetPercent(value:integer);
    procedure SetStartColor(Value : TColor);
    procedure SetMiddleColor(Value:TColor);
    procedure SetEndColor(Value :TColor);
    procedure SetUseMiddle(Value:Boolean);
  protected
    { Protected declarations }
    function ColorUsed(TestColor:TColor):boolean;
    function UniqueColor:TColor;
    function CalcColorIndex(StartColor, EndColor:TColor; Steps, ColorIndex:integer):TColor;
    function GradientColorIndex(ColorIndex:integer):TColor;
    procedure GetBorderColors(var TopColor, BottomColor:TColor);
    procedure PaintTriangle;
    procedure PaintEllipse;
    procedure PaintHorzRect;
    procedure PaintVertRect;
    procedure PaintArc;
    procedure PaintPole;
    procedure paint; override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
  published
    { Published declarations }
    property Border : TGaugeBorder read fborder write SetBorder default gbsingle;
    property Shape : TGaugeShape read fshape write SetShape default gsVertRect;
    property GradientFill : boolean read fGradient write SetGradient default False;
    property Percent:integer read fpercent write setpercent default 0;
    property FillColor : TColor read fStartColor write SetStartColor default clLime;
    property GradientColor : TColor read fEndColor write SetEndColor default clRed;
    property MedianColor : TColor read fMiddleColor write SetMiddleColor default clYellow;
    property UseMedianColor : Boolean read fusemiddle write setusemiddle default false;
    property OnChange : TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
  end;

implementation

{ TrmGauge }

constructor TrmGauge.Create(AOwner:TComponent);
begin
     inherited create(AOwner);
     width := 100;
     height := 50;
     fgradient := false;
     fshape := gsVertRect;
     fborder := gbSingle;
     fstartcolor := cllime;
     fendcolor := clred;
     fmiddlecolor := clyellow;
     fusemiddle := false;
     fpercent := 0;
end;

procedure TrmGauge.paint;
begin
     case shape of
          gsTriangle : PaintTriangle;
          gsHorzRect : PaintHorzRect;
          gsEllipse  : PaintEllipse;
          gsArc      : PaintArc;
          gsVertRect : PaintVertRect;
          gsPole     : PaintPole;
     end;
end;

procedure TrmGauge.GetBorderColors(var TopColor, BottomColor:TColor);
begin
     case border of
          gbSingle:begin
                        topColor := clWindowFrame;
                        bottomcolor := topcolor;
                   end;
          gbRaised:begin
                        topcolor := clbtnhighlight;
                        bottomcolor := clbtnshadow;
                   end;
          gbLowered:begin
                         topcolor := clbtnshadow;
                         bottomcolor := clbtnhighlight;
                    end;
          else
          begin
               topcolor := clbtnface;
               bottomcolor := topcolor;
          end;
     end;
end;

procedure TrmGauge.PaintHorzRect;
var
   bmp : tbitmap;
   wrect, wr2 : TRect;
   topColor, bottomcolor : TColor;
   NewBrush, OldBrush : HBrush;
   loop : integer;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     GetBorderColors(TopColor,BottomColor);
     with bmp.Canvas do
     begin
          wrect := rect(0,0,width,height);
          if border <> gbNone then
          begin
             pen.color := TopColor;
             PolyLine([point(width-1,0),point(0,0),point(0,height-1)]);
             pen.color := BottomColor;
             PolyLine([point(0,height-1),point(width-1,height-1),point(width-1,0)]);
             inflaterect(wrect,-1,-1);
          end;
          brush.color := clbtnface;
          fillrect(wrect);
          if gradientfill then
          begin
               for loop := 0 to percent-1 do
               begin
                    wr2 := rect(0,wrect.top, 0,wrect.bottom);
                    wr2.Left  := wrect.Left+ MulDiv (loop    , wrect.Right-wrect.Left, 100);
                    wr2.Right := wrect.Left+ MulDiv (loop + 1, wrect.Right-wrect.Left, 100);

                    NewBrush := CreateSolidBrush(GradientColorIndex(loop+1));
                    OldBrush := SelectObject(bmp.Canvas.handle, NewBrush);
                    try
                      PatBlt(bmp.Canvas.handle, wr2.Left, wr2.Top, wr2.Right-wr2.Left, wr2.Bottom-wr2.Top, PATCOPY);
                    finally
                      SelectObject(bmp.Canvas.handle, OldBrush);
                      DeleteObject(NewBrush);
                    end;
               end;
          end
          else
          begin
               wrect.Right := wrect.Left + (((wrect.Right-Wrect.left) * percent) div 100);
               brush.color := fillcolor;
               fillrect(wrect);
          end;
     end;
     bitblt(canvas.handle,0,0,width,height,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.PaintVertRect;
var
   bmp : TBitmap;
   wrect, wr2 : TRect;
   topColor, bottomcolor : TColor;
   NewBrush, OldBrush : HBrush;
   loop : integer;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     GetBorderColors(TopColor,BottomColor);
     with bmp.canvas do
     begin
          wrect := rect(0,0,width,height);
          if border <> gbNone then
          begin
             pen.color := TopColor;
             PolyLine([point(width-1,0),point(0,0),point(0,height-1)]);
             pen.color := BottomColor;
             PolyLine([point(0,height-1),point(width-1,height-1),point(width-1,0)]);
             inflaterect(wrect,-1,-1);
          end;
          brush.color := clbtnface;
          fillrect(wrect);
          if gradientfill then
          begin
               for loop := 0 to percent-1 do
               begin
                    wr2 := rect(wrect.left,0,wrect.right,0);
                    wr2.Bottom  := wrect.Bottom- MulDiv (loop    , wrect.Bottom-wrect.Top, 100);
                    wr2.Top := wrect.Bottom- MulDiv (loop + 1, wrect.Bottom-wrect.Top, 100);

                    NewBrush := CreateSolidBrush(GradientColorIndex(loop+1));
                    OldBrush := SelectObject(bmp.canvas.handle, NewBrush);
                    try
                      PatBlt(bmp.canvas.handle, wr2.Left, wr2.Top, wr2.Right-wr2.Left, wr2.Bottom-wr2.Top, PATCOPY);
                    finally
                      SelectObject(bmp.canvas.handle, OldBrush);
                      DeleteObject(NewBrush);
                    end;
               end;
          end
          else
          begin
               wrect.Top := wrect.Bottom - (((wrect.Bottom-Wrect.Top) * percent) div 100);
               brush.color := fillcolor;
               fillrect(wrect);
          end;
     end;
     bitblt(canvas.handle,0,0,width,height,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.PaintTriangle;
var
   bmp : TBitMap;
   topColor, bottomcolor : TColor;
   theta, adjacent : double;
   NewBrush, OldBrush : HBrush;
   NewPen, OldPen : HPen;
   loop : integer;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     GetBorderColors(TopColor,BottomColor);
     with bmp.canvas do
     begin
          brush.color := clbtnface;
          pen.color := brush.color;
          Polygon([point(width-1,0),point(0,height-1),point(width-1,height-1)]);
          if percent > 0 then
          begin
               if gradientfill then
               begin
                    theta := ArcTan(height/width);
                    for loop := Percent downto 1 do
                    begin
                         NewBrush := CreateSolidBrush(GradientColorIndex(loop));
                         OldBrush := SelectObject(bmp.canvas.handle, NewBrush);
                         NewPen := CreatePen(ps_Solid,1,GradientColorIndex(loop));
                         OldPen := SelectObject(bmp.canvas.handle, NewPen);
                         try
                            adjacent := ((width-1) * loop) / 100;
                            polygon([point(0,height-1),
                                     point(round(adjacent),height-1),
                                     point(round(adjacent),(height)-trunc(tan(theta) * adjacent))]);
                         finally
                           SelectObject(bmp.canvas.handle, OldPen);
                           DeleteObject(NewPen);
                           SelectObject(bmp.canvas.handle, OldBrush);
                           DeleteObject(NewBrush);
                         end;
                    end;
               end
               else
               begin
                    brush.color := fillcolor;
                    pen.color := fillcolor;
                    theta := ArcTan(height/width);
                    adjacent := ((width-1) * percent) / 100;
                    polygon([point(0,height-1),
                             point(round(adjacent),height-1),
                             point(round(adjacent),(height)-trunc(tan(theta) * adjacent))]);
               end;
          end;
          if border <> gbNone then
          begin
             pen.color := TopColor;
             PolyLine([point(width-1,0),point(0,height-1)]);
             pen.color := BottomColor;
             PolyLine([point(0,height-1),point(width-1,height-1),point(width-1,0)]);
          end;
     end;
     bitblt(canvas.handle,0,0,width,height,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.PaintArc;
var
   bmp : TBitMap;
   topColor, bottomcolor : TColor;
   angle, incangle : double;
   lastx, lasty : integer;
   NewBrush, OldBrush : HBrush;
   NewPen, OldPen : HPen;
   loop : integer;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     GetBorderColors(TopColor,BottomColor);
     with bmp.canvas do
     begin
       Brush.Color := clbtnface;
       pen.color := clbtnface;
       Pie(0, 0, Width-1, (Height shl 1) - 1, Width-1, height - 1, 0, height - 1);

       if percent > 0 then
       begin
            if gradientfill then
            begin
                 lastx := 0;
                 lasty := height-1;
                 for loop := 1 to percent do
                 begin
                      NewBrush := CreateSolidBrush(GradientColorIndex(loop));
                      OldBrush := SelectObject(bmp.canvas.handle, NewBrush);
                      NewPen := CreatePen(ps_Solid,1,GradientColorIndex(loop));
                      OldPen := SelectObject(bmp.canvas.handle, NewPen);
                      try
                         if loop < percent then incangle := 0.027
                         else
                         incangle := 0;
                         Angle := (Pi * ((loop / 100)));
                         pie(0, 0, width-1, (Height shl 1) - 1, Round(((Width shr 1)-1) * (1 - Cos(Angle+incangle))), Round((height - 1) * (1 - Sin(Angle+incangle))), lastx, lasty);
                         lastx := Round(((Width shr 1)-1) * (1 - Cos(Angle)));
                         lasty := Round((height - 1) * (1 - Sin(Angle)));
                      finally
                        SelectObject(bmp.canvas.handle, OldPen);
                        DeleteObject(NewPen);
                        SelectObject(bmp.canvas.handle, OldBrush);
                        DeleteObject(NewBrush);
                      end;
                 end;
            end
            else
            begin
                 Pen.Color := clblack;
                 Pen.Width := 1;
                 brush.color := fillcolor;
                 Angle := (Pi * ((Percent / 100)));
                 pie(0, 0, width-1, (Height shl 1) - 1, Round(((Width shr 1)-1) * (1 - Cos(Angle))), Round((height - 1) * (1 - Sin(Angle))), 0, height-1);
            end;
       end;

       if border <> gbNone then
       begin
            Pen.Width := 1;
            Pen.Color :=  TopColor;
            Arc (0, 0, width-1, (height shl 1)-1, // ellipse
                 width-1, 0, // start
                 0, (height shl 1)-1); // end

            Pen.Color :=  BottomColor;
            Arc (0, 0, width - 1, (height shl 1)-1, // ellipse
                 0, (height shl 1)-1, // start
                 width - 1, 0); // end
            moveto(0,height-1);
            lineto(width-1,height-1);
       end;
     end;
     bitblt(canvas.handle,0,0,width-1,height,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.PaintEllipse;
var
   bmp : TBitMap;
   topColor, bottomcolor : TColor;
   angle : double;
   lastx, lasty : integer;
   NewBrush, OldBrush : HBrush;
   NewPen, OldPen : HPen;
   loop : integer;
   incangle : double;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));

     GetBorderColors(TopColor,BottomColor);
     with bmp.canvas do
     begin
       Brush.Color := clbtnface;
       pen.color := clbtnface;
       Ellipse(0, 0, Width-1, Height - 1);

       if percent > 0 then
       begin
            if gradientfill then
            begin
                 lastx := 0;
                 lasty := (height shr 1)-1;
                 for loop := 1 to percent do
                 begin
                      NewBrush := CreateSolidBrush(GradientColorIndex(loop));
                      OldBrush := SelectObject(bmp.canvas.handle, NewBrush);
                      NewPen := CreatePen(ps_Solid,1,GradientColorIndex(loop));
                      OldPen := SelectObject(bmp.canvas.handle, NewPen);
                      try
                         Angle := (2 * Pi * ((loop / 100)));
                         if loop < percent then incangle := 0.027
                         else
                         incangle := 0;
                         pie(0, 0, width-1, Height-1, Round(((width shr 1)-1) * (1 - Cos(Angle+incangle))), Round(((height shr 1) - 1) * (1 - Sin(Angle+incangle))), lastx, lasty);
                         lastx := Round(((width shr 1)-1) * (1 - Cos(Angle)));
                         lasty := Round(((height shr 1) - 1) * (1 - Sin(Angle)));
                      finally
                        SelectObject(bmp.canvas.handle, OldPen);
                        DeleteObject(NewPen);
                        SelectObject(bmp.canvas.handle, OldBrush);
                        DeleteObject(NewBrush);
                      end;
                 end;
            end
            else
            begin
                 Pen.Width := 1;
                 brush.color := fillcolor;
                 Pen.Color := clblack;
                 Angle := (2*Pi * ((Percent / 100)));
                 pie(0, 0, width-1, Height-1, Round(((width shr 1)-1) * (1 - Cos(Angle))), Round(((height shr 1) - 1) * (1 - Sin(Angle))), 0, (height shr 1)-1);
            end;
       end;

       if border <> gbNone then
       begin
            Pen.Color :=  TopColor;
            Arc (0, 0, width-1, height-1, // ellipse
                 width-1, 0, // start
                 0, height-1); // end

            Pen.Color :=  BottomColor;
            Arc (0, 0, width-1, height-1, // ellipse
                 0, height-1, // start
                 width, 0); // end
       end;
     end;
     bitblt(canvas.handle,0,0,width-1,height-1,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.PaintPole;
const
     bw = 15;
var
   bmp : TBitMap;
   wrect : TRect;
   NewBrush, OldBrush : HBrush;
   ph, loop : integer;
begin
     bmp := tbitmap.create;
     bmp.width := width;
     bmp.height := height;
     bmp.canvas.brush.color := clbtnface;
     bmp.canvas.fillrect(rect(0,0,width-1,height-1));
     with bmp.canvas do
     begin
          moveto(width-1,0);
          pen.Color := clblack;
          lineto(0,0);
          lineto(0,height-1);
          lineto(width-1,height-1);
          wrect := rect(((width-1)-3)-bw, 1,((width-1)-3),(height-1));
          brush.color := clbtnface;
          fillrect(wrect);
          ph := round((((height-1)-1) * percent) / 100);
          wrect := rect(((width-1)-3)-bw,(height-1) - ph,((width-1)-3),(height-1));
          for loop := 1 to (bw shr 1) do
          begin
               if loop <= percent then
               begin
                    NewBrush := CreateSolidBrush(CalcColorIndex(fendcolor,fstartcolor,bw shr 1,loop));
                    OldBrush := SelectObject(bmp.canvas.handle, NewBrush);
                    try
                       PatBlt(bmp.canvas.handle, wrect.Left, wrect.Top, wrect.Right-wrect.Left, wrect.Bottom-wrect.Top, PATCOPY);
                       inflaterect(wrect,-1,-1);
                    finally
                      SelectObject(bmp.canvas.handle, OldBrush);
                      DeleteObject(NewBrush);
                    end;
               end;
          end;
     end;
     bitblt(canvas.handle,0,0,width-1,height,bmp.canvas.handle,0,0,srccopy);
     bmp.free;
end;

procedure TrmGauge.SetGradient(Value:Boolean);
begin
     if fGradient <> value then
     begin
          fgradient := value;
          invalidate;
     end;
end;

procedure TrmGauge.SetShape(Value : TGaugeShape);
begin
     if fshape <> value then
     begin
          fshape := value;
          invalidate;
     end;
end;

procedure TrmGauge.SetBorder(Value : TGaugeBorder);
begin
     if fborder <> value then
     begin
          fborder := value;
          invalidate;
     end;
end;

procedure TrmGauge.SetPercent(value:integer);
begin
     if (value < 0) or (value > 100) then exit;
     if fpercent <> value then
     begin
          fpercent := value;
          paint;
          if assigned(fOnChangeEvent) then fOnChangeEvent(self);
     end;
end;

procedure TrmGauge.SetStartColor(Value : TColor);
begin
     if fStartcolor <> value then
     begin
          fStartColor := Value;
          paint;
     end;
end;

procedure TrmGauge.SetUseMiddle(Value:Boolean);
begin
     if fUseMiddle <> value then
     begin
          fUseMiddle := Value;
          paint;
     end;
end;

procedure TrmGauge.SetMiddleColor(Value:TColor);
begin
     if fMiddleColor <> value then
     begin
          fMiddleColor := Value;
          paint;
     end;
end;

procedure TrmGauge.SetEndColor(Value :TColor);
begin
     if fendcolor <> value then
     begin
          fendcolor := value;
          paint;
     end;
end;

function TrmGauge.GradientColorIndex(ColorIndex:integer):TColor;
var
  BeginRGBValue  : array[0..2] of Byte;
  RGBDifference  : array[0..2] of integer;
  Red       : Byte;
  Green     : Byte;
  Blue      : Byte;
  StartColor, EndColor : TColor;
  NumColors : integer;
begin
  if (Colorindex < 1) or (colorindex > 100) then
     raise ERangeError.create('ColorIndex can''t be less than 1 or greater than 100');
  if UseMedianColor then
  begin
       NumColors := 50;
       if Colorindex <= 50 then
       begin
            StartColor := fStartColor;
            EndColor := fMiddleColor;
       end
       else
       begin
            dec(ColorIndex,50);
            StartColor := fMiddleColor;
            EndColor := fEndColor;
       end;
  end
  else
  begin
       NumColors := 100;
       StartColor := fStartColor;
       EndColor := fEndColor;
  end;
  dec(ColorIndex);
  BeginRGBValue[0] := GetRValue (ColorToRGB (StartColor));
  BeginRGBValue[1] := GetGValue (ColorToRGB (StartColor));
  BeginRGBValue[2] := GetBValue (ColorToRGB (StartColor));

  RGBDifference[0] := GetRValue (ColorToRGB (EndColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue (ColorToRGB (EndColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue (ColorToRGB (EndColor)) - BeginRGBValue[2];

  { Calculate the color band's color }
  Red   := BeginRGBValue[0] + MulDiv (ColorIndex, RGBDifference[0], NumColors - 1);
  Green := BeginRGBValue[1] + MulDiv (ColorIndex, RGBDifference[1], NumColors - 1);
  Blue  := BeginRGBValue[2] + MulDiv (ColorIndex, RGBDifference[2], NumColors - 1);

  result := rgb(red, green, blue);
end;

function TrmGauge.CalcColorIndex(StartColor, EndColor:TColor; Steps, ColorIndex:integer):TColor;
var
  BeginRGBValue  : array[0..2] of Byte;
  RGBDifference  : array[0..2] of integer;
  Red       : Byte;
  Green     : Byte;
  Blue      : Byte;
  NumColors : integer;
begin
  if (Colorindex < 1) or (colorindex > steps) then
     raise ERangeError.create('ColorIndex can''t be less than 1 or greater than '+inttostr(steps));
  NumColors := steps;
  dec(ColorIndex);
  BeginRGBValue[0] := GetRValue (ColorToRGB (StartColor));
  BeginRGBValue[1] := GetGValue (ColorToRGB (StartColor));
  BeginRGBValue[2] := GetBValue (ColorToRGB (StartColor));

  RGBDifference[0] := GetRValue (ColorToRGB (EndColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue (ColorToRGB (EndColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue (ColorToRGB (EndColor)) - BeginRGBValue[2];

  { Calculate the color band's color }
  Red   := BeginRGBValue[0] + MulDiv (ColorIndex, RGBDifference[0], NumColors - 1);
  Green := BeginRGBValue[1] + MulDiv (ColorIndex, RGBDifference[1], NumColors - 1);
  Blue  := BeginRGBValue[2] + MulDiv (ColorIndex, RGBDifference[2], NumColors - 1);

  result := rgb(red, green, blue);
end;

function TrmGauge.ColorUsed(TestColor:TColor):boolean;
var
   loop : integer;
   tc, bc : TColor;
begin
     for loop := 1 to 100 do
     begin
          result := GradientColorIndex(loop) = testcolor;
          if result then exit;
     end;
     GetBorderColors(Tc,Bc);
     result := (TestColor = TC) or (TestColor = BC);
end;

function TrmGauge.UniqueColor:TColor;
begin
     randomize;
     result := random(rgb(255,255,255)+1);
     while ColorUsed(result) do
           result := random(rgb(255,255,255)+1);
end;


end.
