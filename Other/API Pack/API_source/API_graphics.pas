unit API_graphics;

//------------------------------------------------------------------------------
// API_graphics
//------------------------------------------------------------------------------
//
// 03072009, ari pikivirta
//  * added TextOut with support of angled text
//  * added InverColor function
//  * added BitmapToRTF
//
// 04062009, ari pikivirta
//  * ResizeBitmap function rechecked, did some changes to not to increase bitmap size
//
// 05092008, ari pikivirta
//  * added Mosaic, Posterize, FlipHorizontal and FlipVertical functions
//
// 25082008, ari pikivirta
//  * added resize bitmap procedure to resize according to specific dimensions

interface

uses
  SysUtils, Types, Windows, Controls, Graphics;

procedure TextOut(ACanvas: TCanvas; Angle, X, Y: Integer; Text: string);
function  CaptureScreenRect( ARect: TRect ): TBitmap;
function  CaptureClientImage( Control: TControl ): TBitmap;
function  InvertColor(color: TColor): TColor;
function  InvertBitmap(MyBitmap: TBitmap): TBitmap;
procedure ScaleBitmap(bitmp: TBitmap; Percent: Double); overload;
procedure ScaleBitmap(bitmp: TBitmap; Percent: Integer); overload;
procedure ReSizeBitmap(bitmp: TBitmap; Const MaxWidth, MaxHeight: Integer);
procedure Highlight(aSource, ATarget: TBitmap; AColor: TColor); overload; //Author: Renate Schaaf
procedure Highlight(src: TBitmap; ARect : TRect; WhiteWashValue : integer = 128); overload;
procedure Emboss(ABitmap: TBitmap; Amount: Integer);
procedure Grayscale(Bmp: TBitmap);
procedure Mosaic(Bitmap: TBitmap; Size: Integer);
procedure FlipHorizontal(src: Tbitmap);
procedure FlipVertical(src: Tbitmap);
procedure Posterize(src, dst: tbitmap; amount: integer);
function  BitmapToRTF(Const pict: TBitmap): Ansistring;

type
  TGradientStyle = (        // gradient filling styles
    gsHorizontal,           // from left to right
    gsVertical,             // from top to bottom
    gsEllipse,              // from middle to the corners
    gsHorCenter,            // from center horizontal
    gsVerCenter             // from center vertical
  );

procedure GradientFill (
  Canvas: TCanvas;          // canvas to draw the gradient on
  Rect: TRect;              // area to be filled
  StartColor,               // start color
  EndColor: TColor;         // end color
  Style: TGradientStyle);   // filling style

implementation

{$WARN UNSAFE_CODE OFF}

uses
  API_base;

//------------------------------------------------------------------------------
function BitmapToRTF(Const pict: TBitmap): Ansistring;
var
  bi, bb, rtf: Ansistring;
  bis, bbs: Cardinal;
  achar: AnsiString;
  hexpict: Ansistring;
  I: Integer;
begin
  GetDIBSizes(pict.Handle, bis, bbs);
  SetLength(bi, bis);
  SetLength(bb, bbs);
  GetDIB(pict.Handle, pict.Palette, PAnsiChar(bi)^, PAnsiChar(bb)^);
  rtf := '{\rtf1 {\pict\dibitmap0 ';
  SetLength(hexpict, (Length(bb) + Length(bi)) * 2);
  I := 2;
  for bis := 1 to Length(bi) do
  begin
    achar:= AnsiString(sysutils.IntToHex(Integer(bi[bis]), 2));
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  for bbs := 1 to Length(bb) do
  begin
    achar := AnsiString(IntToHex(Integer(bb[bbs]), 2));
    hexpict[I - 1] := achar[1];
    hexpict[I] := achar[2];
    Inc(I, 2);
  end;
  rtf := rtf + hexpict + ' }}';
  Result := rtf;
end;

//------------------------------------------------------------------------------
function InvertColor(color: TColor): TColor;
var
  r,g,b: byte;
begin
  Color := ColorToRGB(color);
  b := (Color shr 16) and $FF;
  g := (Color shr 8) and $FF;
  r := (Color and $FF);
  b := 255 - b;
  g := 255 - g;
  r := 255 - r;
  result := (b shl 16) or (g shl 8) or r;
end;

//------------------------------------------------------------------------------
function CaptureScreenRect( ARect: TRect ): TBitmap;
var
  ScreenDC: HDC;
begin
  result:=tbitmap.Create;
  with Result, ARect do
  begin
    width:=Right-Left;
    height:=Bottom-Top;
    screenDC:=GetDC(0);
    try
      bitblt(canvas.handle,0,0,width,height,screenDC,left,top,SRCCOPY);
    finally
      releaseDC(0,ScreenDC);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TextOut(ACanvas: TCanvas; Angle, X, Y: Integer; Text: string);
var
  NewFontHandle,
  OldFontHandle: hFont;
  LogRec       : TLogFont;
begin
  GetObject(ACanvas.Font.Handle, SizeOf(LogRec), Addr(LogRec));
  LogRec.lfEscapement := Angle * 10;
  LogRec.lfOrientation := LogRec.lfEscapement;
  NewFontHandle := CreateFontIndirect(LogRec);
  OldFontHandle := SelectObject(ACanvas.Handle, NewFontHandle);
  ACanvas.TextOut(X, Y, Text);
  NewFontHandle := SelectObject(ACanvas.Handle, OldFontHandle);
  DeleteObject(NewFontHandle);
end;

//------------------------------------------------------------------------------
function CaptureClientImage( Control: TControl ): TBitmap;
begin
  with control, control.ClientOrigin do
    Result:=CaptureScreenRect( Bounds( X, Y, ClientWidth, ClientHeight ));
end;

//------------------------------------------------------------------------------
function InvertBitmap(MyBitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ByteArray: PByteArray;
begin
  MyBitmap.PixelFormat:= pf24Bit;
  for y:= 0 to MyBitmap.Height-1 do
  begin
    ByteArray := MyBitmap.ScanLine[y];
    for x:= 0 to MyBitmap.Width*3-1 do
      ByteArray[x] := 255 - ByteArray[x];
  end;
  Result:= MyBitmap;
end;

//------------------------------------------------------------------------------
procedure ScaleBitmap(bitmp: TBitmap; Percent: Double);
var
  Bmp: TBitmap;
  h, w: integer;
begin
  bmp:= TBitmap.Create;
  try
    h:= trunc(bitmp.Height*(Percent/100));
    w:= trunc(bitmp.Width*(Percent/100));
    Bmp.Width:= w;
    Bmp.Height:= h;
    Bmp.Canvas.StretchDraw(Rect(0, 0, w, h), Bitmp);
    bitmp.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure ScaleBitmap(bitmp: TBitmap; Percent: Integer);
begin
  ScaleBitmap(bitmp, percent);
end;

//------------------------------------------------------------------------------
procedure ReSizeBitmap(bitmp: TBitmap; Const MaxWidth, MaxHeight: Integer);
var
  f, f1, f2: double;
begin
  // calculate factor (greater counts)
  if (bitmp.width>maxWidth) then  f1:= (MaxWidth/bitmp.Width) else f1:= 1;
  if (bitmp.height>maxHeight) then f2:= (MaxHeight/bitmp.Height) else f2:= 1;
  if (f1<f2) then f:= f1 else f:= f2;
  // do scaling, select smaller from above
  ScaleBitmap(bitmp, f*100);
end;

//------------------------------------------------------------------------------
procedure Posterize(src, dst: tbitmap; amount: integer);
var
  w,h,x,y: integer;
  ps,pd: pbytearray;
begin
  w:= src.width;
  h:= src.height;
  src.PixelFormat:= pf24bit;
  dst.PixelFormat:= pf24bit;
  for y:=0 to h-1 do
  begin
    ps:=src.scanline[y];
    pd:=dst.scanline[y];
    for x:=0 to w-1 do
    begin
      pd[x*3]:= round(ps[x*3]/amount)*amount;
      pd[x*3+1]:= round(ps[x*3+1]/amount)*amount;
      pd[x*3+2]:= round(ps[x*3+2]/amount)*amount;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Highlight(aSource, ATarget: TBitmap; AColor: TColor);
//alters ASource to ATarget by making it appear as if looked through
//colored glass as given by AColor
//ASource, ATarget must have been created.
var i, j: Integer;
  s, t: pRGBTriple;
  r, g, b: byte;
  cl: TColor;
begin
  cl := ColorToRGB(AColor);
  r := GetRValue(cl);
  g := GetGValue(cl);
  b := GetBValue(cl);
  aSource.PixelFormat := pf24bit;
  ATarget.PixelFormat := pf24bit;
  ATarget.Width := aSource.Width;
  ATarget.Height := aSource.Height;
  for i := 0 to aSource.Height - 1 do
  begin
    s := ASource.Scanline[i];
    t := ATarget.Scanline[i];
    for j := 0 to aSource.Width - 1 do
    begin
      t^.rgbtBlue := (b * s^.rgbtBlue) div 255;
      t^.rgbtGreen := (g * s^.rgbtGreen) div 255;
      t^.rgbtRed := (r * s^.rgbtRed) div 255;
      inc(s);
      inc(t);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure FlipVertical(src: Tbitmap);
var
  dest: tbitmap;
  w,h,x,y: integer;
  pd,ps: pbytearray;
begin
  w:= src.width;
  h:= src.height;
  dest:=tbitmap.create;
  try
    dest.width:=w;
    dest.height:=h;
    dest.pixelformat:=pf24bit;
    src.pixelformat:=pf24bit;
    for y:=0 to h-1 do
    begin
      pd:=dest.scanline[y];
      ps:=src.scanline[h-1-y];
      for x:=0 to w-1 do
      begin
        pd[x*3]:=ps[x*3];
        pd[x*3+1]:=ps[x*3+1];
        pd[x*3+2]:=ps[x*3+2];
      end;
    end;
    src.assign(dest);
  finally
    dest.free;
  end;
end;

//------------------------------------------------------------------------------
procedure FlipHorizontal(src: Tbitmap);
var
  dest: tbitmap;
  w,h,x,y: integer;
  pd,ps: pbytearray;
begin
  w:= src.width;
  h:= src.height;
  dest:= tbitmap.create;
  try
    dest.width:= w;
    dest.height:= h;
    dest.pixelformat:=pf24bit;
    src.pixelformat:=pf24bit;
    for y:=0 to h-1 do
    begin
      pd:= dest.scanline[y];
      ps:= src.scanline[y];
      for x:=0 to w-1 do
      begin
        pd[x*3]:=ps[(w-1-x)*3];
        pd[x*3+1]:=ps[(w-1-x)*3+1];
        pd[x*3+2]:=ps[(w-1-x)*3+2];
      end;
    end;
    src.assign(dest);
  finally
    dest.free;
  end;
end;

//------------------------------------------------------------------------------
procedure Emboss(ABitmap: TBitmap; Amount: Integer);
var
  x, y, i : integer;
  p1, p2: PByteArray;
begin
  for i := 0 to AMount do
  begin
    for y := 0 to ABitmap.Height-2 do
    begin
      p1 := ABitmap.ScanLine[y];
      p2 := ABitmap.ScanLine[y+1];
      for x := 0 to ABitmap.Width do
      begin
        p1[x*3] := (p1[x*3]+(p2[(x+3)*3] xor $FF)) shr 1;
        p1[x*3+1] := (p1[x*3+1]+(p2[(x+3)*3+1] xor $FF)) shr 1;
        p1[x*3+2] := (p1[x*3+1]+(p2[(x+3)*3+1] xor $FF)) shr 1;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Highlight(src: TBitmap; ARect : TRect; WhiteWashValue : integer = 128);
  function GetPixel(x,y : integer) : pRGBTriple;
  var
    line : pbytearray;
  begin
    line := src.ScanLine[y];
    result := @line[x*3];
  end;
var
  x,y : integer;
begin
  src.PixelFormat:=pf24bit;

  if ARect.Top < 0 then exit;
  if ARect.left < 0 then exit;
  if ARect.bottom > src.Height then exit;
  if ARect.right > src.Width then exit;

  for y := ARect.top to ARect.bottom-1  do
    for x := ARect.left to Arect.right-1 do
    begin
      getpixel(x,y).rgbtRed := byte(WhiteWashValue + getpixel(x,y).rgbtRed );
      getpixel(x,y).rgbtGreen := byte(WhiteWashValue + getpixel(x,y).rgbtGreen );
      getpixel(x,y).rgbtBlue := byte(WhiteWashValue + getpixel(x,y).rgbtBlue );
    end;
end;

//------------------------------------------------------------------------------
procedure Mosaic(Bitmap: TBitmap; Size: Integer);
var
  x,y,i,j: integer;
  p1,p2: pbytearray;
  r,g,b: byte;
begin
  y:=0;
  repeat
    p1:= Bitmap.scanline[y];
    repeat
      j:= 1;
      repeat
        p2:= Bitmap.scanline[y];
        x:= 0;
        repeat
          r:= p1[x*3];
          g:= p1[x*3+1];
          b:= p1[x*3+2];
          i:= 1;
          repeat
            p2[x*3]:= r;
            p2[x*3+1]:= g;
            p2[x*3+2]:= b;
            inc(x);
            inc(i);
          until (x>=Bitmap.width) or (i>size);
        until x>=Bitmap.width;
        inc(j);
        inc(y);
      until (y>=Bitmap.height) or (j>size);
    until (y>=Bitmap.height) or (x>=Bitmap.width);
  until y>=Bitmap.height;
end;

//------------------------------------------------------------------------------
procedure Grayscale(Bmp: TBitmap);
{From: Pascal Enz, pascal.enz@datacomm.ch }
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row: PRGBArray;
begin
  Bmp.PixelFormat := pf24Bit;
  for y := 0 to Bmp.Height - 1 do
  begin
    Row := Bmp.ScanLine[y];
    for x := 0 to Bmp.Width - 1 do
    begin
      Gray           := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue := Gray;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure GradientFill (
  Canvas: TCanvas;          // canvas to draw the gradient on
  Rect: TRect;              // area to be filled
  StartColor,               // start color
  EndColor: TColor;         // end color
  Style: TGradientStyle);   // filling style

  procedure SplitColor(Col: TColor; var rr: byte; var gg: byte; var bb: byte);
  var
    ColRef: COLORREF;
  begin
    ColRef:= ColorToRGB(Col);
    rr:= GetRValue(ColRef);
    gg:= GetGValue(ColRef);
    bb:= GetBValue(ColRef);
  end;

var
  cnt, i: integer;
  r1, r2, g1, g2, b1, b2: Byte;
  dr, dg, db: Extended;
  steps: integer;
  J, K: Real;
  R: TRect;
begin
  // get start and end colors
  SplitColor(StartColor, R1, G1, B1);
  SplitColor(EndCOlor, R2, G2, B2);

  // fill background
  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= StartColor;
  Canvas.FillRect(Rect);

  // calc gradients
  if (style=gsHorizontal) then
  begin
    steps:= Rect.Right-Rect.Left;
    if steps<1 then steps:= 1;
    dr:= (R2-R1)/steps;
    dg:= (G2-G1)/steps;
    db:= (B2-B1)/steps;
    cnt:= 0;
    for i:= Rect.Left to Rect.Right-1 do
    begin
      Canvas.Pen.Color:= RGB(
        round(R1+dr*cnt),
        round(G1+dg*cnt),
        round(B1+db*cnt)
      );
      Canvas.MoveTo(i,Rect.Top);
      Canvas.LineTo(i,Rect.Bottom);
      inc(cnt);
    end;
  end else

  if (style=gsVertical) then
  begin
    steps:= Rect.Bottom-Rect.Top;
    if steps<1 then steps:= 1;
    dr:= (R2-R1)/steps;
    dg:= (G2-G1)/steps;
    db:= (B2-B1)/steps;
    cnt:= 0;
    for i:= Rect.Top to Rect.Bottom-1 do
    begin
      Canvas.Pen.Color:= RGB(
        round(R1+dr*cnt),
        round(G1+dg*cnt),
        round(B1+db*cnt)
      );
      Canvas.MoveTo(Rect.Left, i);
      Canvas.LineTo(Rect.Right, i);
      inc(cnt);
    end;
  end else

  if (style=gsEllipse) then
  begin
    if (rect.right-rect.left<rect.Bottom-rect.top) then
      steps:= Rect.right-Rect.left else
      steps:= rect.bottom-rect.Top;
    if steps<1 then steps:= 1;
    dr:= (R2-R1)/steps;
    dg:= (G2-G1)/steps;
    db:= (B2-B1)/steps;
    j:= ((rect.Right-rect.left)/steps)/2;
    k:= ((rect.Bottom-rect.top)/steps)/2;
    for i:=0 to steps do
    begin
      R.Top:= Round(I * K);
      R.Bottom:= (rect.Bottom-rect.top) - R.Top;
      R.Right:= Round(I * J);
      R.Left:= (rect.Right-rect.left) - R.Right;
      Canvas.Brush.Color := RGB(
        Round(GetRValue(StartColor) + I * dr),
        Round(GetGValue(StartColor) + I * dg),
        Round(GetBValue(StartColor) + I * db));
      Canvas.Pen.Color:= Canvas.Brush.Color;
      Canvas.Ellipse(R.Right, R.Top, R.Left, R.Bottom);
    end;
  end else

  if (style=gsHorCenter) then
  begin
    steps:= ceil((rect.right-rect.left)/2);
    if steps<1 then steps:= 1;
    dr:= (R2-R1)/steps;
    dg:= (G2-G1)/steps;
    db:= (B2-B1)/steps;
    cnt:= 0;
    for i:=0 to steps do
    begin
      Canvas.Pen.Color:= RGB(
        round(R1+dr*cnt),
        round(G1+dg*cnt),
        round(B1+db*cnt)
      );
      // to right
      Canvas.MoveTo(i, Rect.Top);
      Canvas.LineTo(i, Rect.Bottom);
      // to left
      Canvas.MoveTo(rect.Right-i, Rect.Top);
      Canvas.LineTo(rect.right-i, Rect.Bottom);
      inc(cnt);
    end;
  end else

  //if (style=gsVerCenter) then
  begin
    steps:= ceil((Rect.Bottom-Rect.Top)/2);
    if steps<1 then steps:= 1;
    dr:= (R2-R1)/steps;
    dg:= (G2-G1)/steps;
    db:= (B2-B1)/steps;
    cnt:= 0;
    for i:=0 to steps do
    begin
      Canvas.Pen.Color:= RGB(
        round(R1+dr*cnt),
        round(G1+dg*cnt),
        round(B1+db*cnt)
      );
      Canvas.MoveTo(Rect.Left, i);
      Canvas.LineTo(Rect.Right, i);
      Canvas.MoveTo(Rect.Left, rect.bottom-i);
      Canvas.LineTo(Rect.Right, rect.bottom-i);
      inc(cnt);
    end;
  end;

end;

end.
