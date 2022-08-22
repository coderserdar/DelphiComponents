unit Hole;

interface
uses WinTypes, WinProcs, Messages, SysUtils, Graphics, classes, Controls, forms;

const MAXPoints = 100;

type
    TEdgeDirection =  (hdUp, hdDown, hdLeft, hdRight,
                      hdUpLeft, hdUpRight, hdDownLeft, hdDownRight);

    THoleShape =  (hsPerfect, hsRandom);

    THole = class(TGraphicControl)
      constructor Create(Aowner: TComponent); override;
      destructor Destroy; override;
      private
       FPoints: integer;
       FBackColor: TColor;
       FHoleColor: TColor;
       FEdgeColor: TColor;
       FOutlineColor: TColor;
       FEdgeWidth: integer;
       FEdgeDirection : TEdgeDirection;
       FHoleShape: THoleShape;
       aClip: array[0..MAXPoints-1] of TPoint;
       aBox: array[0..MAXPoints-1] of TPoint;
       hRgnClip: hRgn;
       hRgnBox:  hRgn;
       hRgnComb: hRgn;
       procedure SetBackColor(c: TColor);
       procedure SetHoleColor(c: TColor);
       procedure SetEdgeColor(c: TColor);
       procedure SetOutlineColor(c: TColor);
       procedure SetPoints(i: integer);
       procedure SetEdgeWidth(i: integer);
       procedure SetEdgeDirection(e: TEdgeDirection);
       procedure SetHoleShape(e: THoleShape);
       procedure CreateData;
       function gRand(lbound, hbound: integer):integer;
       procedure WndProc(var aMessage: TMessage); override;
      protected
        procedure Paint; override;
      public
        procedure ClippingOn;
        procedure ClippingOff;
      published
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDrag;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property Points: integer read FPoints write SetPoints default 8;
        property BackColor: TColor
           read FBackColor write SetBackColor default clBtnFace;
        property HoleColor: TColor
           read FHoleColor write SetHoleColor default clBlack;
        property EdgeColor: TColor
           read FEdgeColor write SetEdgeColor default clDkGray;
        property OutlineColor: TColor
           read FOutlineColor write SetOutlineColor default clBlack;
        property EdgeDirection: TEdgeDirection
           read FEdgeDirection write SetEdgeDirection default hdDownLeft;
        property HoleShape: THoleShape
           read FHoleShape write SetHoleShape default hsRandom;
        property EdgeWidth: integer
           read FEdgeWidth write SetEdgeWidth default 8;

   end; {class}

procedure Register;
{---------------------------------}
implementation
constructor THole.Create(Aowner: TComponent);
begin
   inherited Create(Aowner);
   randomize;
   hRgnClip := 0;
   hRgnBox := 0;
   hRgnComb := 0;
   FPoints := 12;
   FBackColor:= clBtnFace;
   FHoleColor:= clBlack;
   FEdgeColor:= clDkGray;
   FOutlineColor:= clBlack;
   FEdgeDirection := hdDownLeft;
   FHoleShape := hsRandom;
   FEdgeWidth := 8;
   width := 64;  height := 64;
   CreateData;
end;

destructor THole.Destroy;
begin
     DeleteObject(hRgnClip);
     DeleteObject(hRgnBox);
     DeleteObject(hRgnComb);
     inherited Destroy;
end;

{------------------------------------}
{  Create the point set              }
{------------------------------------}

procedure THole.CreateData;
var iCtr, jCtr: integer;
    iRad, jRad: integer;
    i: integer;
    ang, anginc, angRad: single;
    iDir, jDir: integer;       {based on EdgeDirection}
begin
   
   {delete regions if they exist}
   if hRgnClip <> 0 then DeleteObject(hRgnClip);
   if hRgnBox  <> 0 then DeleteObject(hRgnBox);
   if hRgnComb <> 0 then DeleteObject(hRgnComb);

   {create points around an ellipse}
   iCtr := width div 2;
   jCtr := height div 2;
   iRad := iCtr;
   jRad := jCtr;

    anginc := 360 / FPoints;
    ang := 0.0;
    For i := 0 to FPoints - 1 do begin
        angRad := (ang * Pi) / 180;
        
        { x = r Cos(ang), y = r Sin(ang) }
        { subtract random amount if desired }

        if FHoleShape = hsRandom then begin
           aClip[i].x := iCtr + Trunc((iRad-gRand(0,iRad div 4)) * Cos(angRad));
           aClip[i].y := jCtr + Trunc((jRad-gRand(0,jRad div 4)) * Sin(angRad));
        end else begin
           aClip[i].x := iCtr + Trunc(iRad * Cos(angRad));
           aClip[i].y := jCtr + Trunc(jRad * Sin(angRad));
        end;

        case FEdgeDirection of
        hdDownLeft:  begin iDir := -FEdgeWidth; jDir :=  FEdgeWidth end;
        hdUpLeft:    begin iDir := -FEdgeWidth; jDir := -FEdgeWidth end;
        hdDownRight: begin iDir := FEdgeWidth;  jDir :=  FEdgeWidth;end;
        hdUpRight:   begin iDir := FEdgeWidth;  jDir := -FEdgeWidth;end;
        hdUp:        begin iDir := 0;           jDir := -FEdgeWidth end;
        hdDown:      begin iDir := 0;           jDir :=  FEdgeWidth end;
        hdLeft:      begin iDir := -FEdgeWidth; jDir :=  0          end;
        hdRight:     begin iDir :=  FEdgeWidth; jDir :=  0          end;
        end; {case}

        { second region offset from first}
        aBox[i].x := aClip[i].x + iDir;
        aBox[i].y := aClip[i].y + jDir;

        ang := ang + anginc ;
   end; {for}

   aClip[FPoints] := aClip[0];        {need a dupe for POLYLINE to work}
   aBox[FPoints] := aBox[0];

   {create regions.  Comb = Clip AND Box}
   hRgnClip := CreatePolygonRgn(aClip[0], FPoints, WINDING);
   hRgnBox  := CreatePolygonRgn(aBox[0], FPoints, WINDING);
   hRgnComb  := CreatePolygonRgn(aBox[0], FPoints, WINDING);
   CombineRgn(hRgnComb, hRgnClip, hRgnBox, RGN_AND);
   
   {offset combined  region by position on parent}
   OffSetRgn(hRgnComb, left, top);     
end;

{--------------------------------------}
procedure THole.ClippingOn;
begin SelectClipRgn(TForm(owner).canvas.handle, hRgnComb) end;

procedure THole.ClippingOff;
begin SelectClipRgn(TForm(owner).canvas.handle, 0) end;
{--------------------------------------}
procedure THole.Paint;
var i, nLast: integer;
    aPoly: array[0..3] of TPoint;
    OldBrush : THandle;
    myBitmap: TBitmap;
    aBrush: TBrush;

begin
myBitmap := TBitmap.create;
myBitmap.Width := width;
myBitmap.height := height;

{fille to back color}
mybitmap.canvas.brush.color := FBackColor;
mybitmap.canvas.pen.color   := FBackColor;
mybitmap.canvas.rectangle(0,0,width,height);

{turn on hole, fill to holecolor}
SelectClipRgn(mybitmap.canvas.Handle, hRgnClip);
mybitmap.canvas.brush.color := FHoleColor;
mybitmap.canvas.pen.color   := FHoleColor;
mybitmap.canvas.rectangle(0,0,width,height);

{draw bevel, keep clipping on}
mybitmap.canvas.brush.color := FEdgeColor;
mybitmap.canvas.pen.color   := FOutlineColor;
For i := 0 to FPoints-1 do begin
   aPoly[0].x := aClip[i].x;
   aPoly[0].y := aClip[i].y;
   aPoly[3].x := aBox[i].x;
   aPoly[3].y := aBox[i].y;
   if i = FPoints-1 then nLast := 0 else nLast := i + 1;

   aPoly[1].x := aClip[nLast].x;
   aPoly[1].y := aClip[nLast].y;
   aPoly[2].x := aBox[nLast].x;
   aPoly[2].y := aBox[nLast].y;

   mybitmap.canvas.Polygon(aPoly);
end;

{ draw outlines around regions                               }
{ have to use API as opposed to TCanvas methods here because }
{ TCanvas methods don't let us speicfy # of points           }
PolyLine(mybitmap.canvas.handle, aBox, FPoints+1);
SelectClipRgn(mybitmap.canvas.handle,0);
PolyLine(mybitmap.canvas.handle, aClip, FPoints+1);        

canvas.CopyRect(rect(0,0,width,height),mybitmap.canvas,rect(0,0,width,height));
mybitmap.free;

end;

procedure THole.SetEdgeDirection(e: TEdgeDirection);
begin
   if e <> FEdgeDirection then begin
       FEdgeDirection := e;
       CreateData;
       Paint;
   end; {if}
end;

procedure THole.SetHoleShape(e: THoleShape);
begin
   if e <> FHoleShape then begin
       FHoleShape := e;
       CreateData;
       Paint;
   end; {if}
end;

procedure THole.SetBackColor(c: TColor);
begin
   if c <> FBackColor then begin
       FBackColor := c;
       Paint;
   end; {if}
end;

procedure THole.SetOutlineColor(c: TColor);
begin
   if c <> FOutlineColor then begin
       FOutlineColor := c;
       Paint;
   end; {if}
end;

procedure THole.SetHoleColor(c: TColor);
begin
   if c <> FHoleColor then begin
       FHoleColor := c;
       Paint;
   end; {if}
end;

procedure THole.SetEdgeColor(c: TColor);
begin
   if c <> FEdgeColor then begin
       FEdgeColor := c;
       Paint;
   end; {if}
end;

procedure THole.SetPoints(i: integer);
begin
   if (i <= 2) or (i > 100) then
       {bad data}
   else if i <> FPoints then begin
       FPoints := i;
       CreateData;
       Paint;
   end; {if}
end;

procedure THole.SetEdgeWidth(i: integer);
begin
   if (i < 0) or (i > 100) then
       {bad data}
   else if i <> FEdgeWidth then begin
       FEdgeWidth := i;
       CreateData;
       Paint;
   end; {if}
end;


function THole.gRand(lbound, hbound: integer):integer;
begin gRand := Random(hbound - lbound) + lbound end;


{ trap for resize }
procedure THole.WndProc(var aMessage: TMessage);
begin
  if (aMessage.Msg = WM_WINDOWPOSCHANGED) then
       CreateData;

  inherited WndProc(aMessage);
end;

procedure Register;
begin RegisterComponents('Lib', [THole]) end;


end.
