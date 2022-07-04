//  Author : Milan R. Zavisic

unit M7SegDis;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  M7SegDisp = class(TGraphicControl)
  private
    { Private declarations }
    fValue:Byte;
    fDispColor:tColor;
    fBackColor:tColor;
    fDispOffCl:tColor;
    procedure Paint; override;
    procedure SetValue(Num:byte);
    procedure SetDispColor(Color:tColor);
    procedure SetBackColor(Color:tColor);
    procedure SetDispOffCl(Color:tColor);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOWner : TComponent); override;
  published
    { Published declarations }
    property Value:Byte read fValue write SetValue;
    property DispColor:TColor read fDispColor write SetDispColor;
    property BackColor:TColor read fBackColor write SetBackColor;
    property DispOffColor:TColor read fDispOffCl write SetDispOffCl;
  end;

procedure Register;

implementation

const Display:array [0..9,1..7] of boolean =
    (
    (True,True,True,True,False,True,True),
    (False,False,True,True,False,False,False),
    (False,True,True,False,True,True,True),
    (False,False,True,True,True,True,True),
    (True,False,True,True,True,False,False),
    (True,False,False,True,True,True,True),
    (True,True,False,True,True,True,True),
    (False,False,True,True,False,True,False),
    (True,True,True,True,True,True,True),
    (True,False,True,True,True,True,True)
    );

constructor M7SegDisp.Create(AOWner : TComponent);
begin
  inherited Create(AOwner);
  fBackColor:=clBtnFace;
  fDispColor:=clLime;
  fDispOffCl:=clGray;
  Height:=70;
  Width:=40;
end;

procedure M7SegDisp.SetValue(Num:byte);
begin
   If Num in [0..9] then begin
      fValue:=Num;
      Paint;
   end;
end;

procedure M7SegDisp.SetDispColor(Color:tColor);
begin
      fDispColor:=Color;
      Paint;
end;

procedure M7SegDisp.SetBackColor(Color:tColor);
begin
      fBackColor:=Color;
      Paint;
end;

procedure M7SegDisp.SetDispOffCl(Color:tColor);
begin
      fDispOffCl:=Color;
      Paint;
end;

procedure M7SegDisp.Paint;
var x1,x2,x3,x4,x5,x6,x7,x8,x9,x0:integer;
    y1,y2,y3,y4,y5,y6,y7,y8,y9,y0,ya,yb,yc,yd,ye:integer;
    Slika : TBitmap;
begin
   Slika:=TBitmap.Create;
   Slika.Width:=ClientWidth;
   Slika.Height:=ClientHeight;
   with Slika.Canvas do begin
      Brush.Color:=fBackColor;
      Brush.Style:=bsSolid;
      FillRect(Rect(0,0,Slika.Width,Slika.Height));
      x1:=Trunc(0.1*Slika.Width);
      x2:=Trunc(0.925*Slika.Width);
      x3:=Trunc(0.225*Slika.Width);
      x4:=Trunc(0.8*Slika.Width);
      x5:=Trunc(0.125*Slika.Width);
      x6:=Trunc(0.9*Slika.Width);
      x7:=Trunc(0.75*Slika.Width);
      x8:=Trunc(0.275*Slika.Width);
      x9:=Trunc(0.15*Slika.Width);
      x0:=Trunc(0.875*Slika.Width);
      y1:=Trunc(0.086*Slika.Height);
      y2:=Trunc(0.157*Slika.Height);
      y3:=Trunc(0.529*Slika.Height);
      y4:=Trunc(0.514*Slika.Height);
      y5:=Trunc(0.957*Slika.Height);
      y6:=Trunc(0.5*Slika.Height);
      y7:=Trunc(0.6*Slika.Height);
      y8:=Trunc(0.07*Slika.Height);
      y9:=Trunc(0.47*Slika.Height);
      y0:=Trunc(0.87*Slika.Height);
      ya:=Trunc(0.143*Slika.Height);
      yb:=Trunc(0.557*Slika.Height);
      yc:=Trunc(0.886*Slika.Height);
      yd:=Trunc(0.429*Slika.Height);
      ye:=Trunc(0.942*Slika.Height);
      { 1. linija }
      if Display[Value,1] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x1,y1),Point(x3,y2),Point(x3,yd),Point(x1,y6),Point(x1,y1)]);
      FloodFill(((x1+x3)div 2),((y1+y6) div 2), Brush.Color, fsBorder);
      { 2. linija }
      if Display[Value,2] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x1,y3),Point(x3,y7),Point(x3,y0),Point(x1,ye),Point(x1,y3)]);
      FloodFill(((x1+x3)div 2),((y3+ye) div 2), Brush.Color, fsBorder);
      { 3. linija }
      if Display[Value,3] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x2,y1),Point(x4,y2),Point(x4,yd),Point(x2,y6),Point(x2,y1)]);
      FloodFill(((x2+x4)div 2),((y1+y6) div 2), Brush.Color, fsBorder);
      { 4. linija }
      if Display[Value,4] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x2,y3),Point(x4,y7),Point(x4,y0),Point(x2,ye),Point(x2,y3)]);
      FloodFill(((x2+x4)div 2),((y3+ye) div 2), Brush.Color, fsBorder);
      { 5. linija }
      if Display[Value,5] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x9,y4),Point(x3,y9),Point(x4,y9),Point(x0,y4),Point(x4,yb),Point(x3,yb),Point(x9,y4)]);
      FloodFill(((x9+x0)div 2),y4, Brush.Color, fsBorder);
      { 6. linija }
      if Display[Value,6] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x5,y8),Point(x6,y8),Point(x7,ya),Point(x8,ya),Point(x5,y8)]);
      FloodFill(((x5+x6)div 2),((y8+ya) div 2), Brush.Color, fsBorder);
      { 7. linija }
      if Display[Value,7] then begin
         Pen.Color:=FDispColor;
         Brush.Color:=FDispColor;
      end
      else begin
         Pen.Color:=FDispOffCl;
         Brush.Color:=FDispOffCl;
      end;
      PolyLine([Point(x5,y5),Point(x6,y5),Point(x7,yc),Point(x8,yc),Point(x5,y5)]);
      FloodFill(((x5+x6)div 2),((y5+yc) div 2), Brush.Color, fsBorder);
   end;
   Canvas.CopyMode:=cmSrcCopy;
   Canvas.Draw(0, 0, Slika);
   Slika.Free;
end;

procedure Register;
begin
  RegisterComponents('Milka', [M7SegDisp]);
end;

end.
