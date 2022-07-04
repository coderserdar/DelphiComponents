unit EkUtil;

//=============
//  EkUtil
//=============

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//======================================

// Latest Changes
// - Lots of new functions
// - Much faster region code, as it now uses scanline

// Region code based on code by Grinder
// - http://groups.google.com/group/borland.public.delphi.vcl.components.using/browse_thread/thread/544a5b59292b3c30/0d6b3931507bdd79?lnk=st&q=ShapeFormClient_Mask#0d6b3931507bdd79

// Transparency code based on code posted by Geir Wikran
// - "Transparent BitMaps"

//==============================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations
{$Include EkVCL.inc}

//==============================================================================

interface

uses Windows, Graphics, Controls, Forms, SysUtils, EkTypes;

//==============================================================================

procedure SetDefaultBmpType(var src : TBitmap); {$ifdef COMPILER10_UP} inline; {$endif}
function Clip(i: Integer): Byte; {$ifdef COMPILER10_UP} inline; {$endif}
procedure PaintFocusRect(r : TRect; pCanvas : TCanvas);
procedure ResizeImage(const src : TBitmap; var dest : TBitmap; const srcOffset, ogWidth, ogHeight, newWidth, newHeight : Integer);
procedure MakeMaskMono(const src : TBitmap; var destMask, destMono : TBitmap; transparentColor : TColor);
procedure DrawTransparent(const src : TBitmap; dest : TCanvas; transparentColor : TColor);
procedure MakeTransparent(var src : TBitmap; transparentColor : TColor);
procedure SplitStdBmp(const src : TBitmap; var dest : array of TBitmap; const arraySize : Integer; var transparentColor : TColor);
procedure SplitBtnBmp(const src : TBitmap; var dest1, dest2, dest3, dest4 : TBitmap; var transparentColor : TColor);
procedure SplitCheckboxBmp(const src : TBitmap; var dest1, dest2, dest3, dest4, dest5, dest6, dest7, dest8 : TBitmap; var transparentColor : TColor);
function ConvertToGreyscale(const bmp : TBitmap) : TBitmap;
function ChangeBrightness(const bmp : TBitmap; const percent : Integer) : TBitmap;
function NonClientRgn(const oForm: TCustomForm): HRGN;
function ShapeForm(const oForm: TCustomForm; const
oMask: TBitmap; const iMaskColor: TColor): Boolean;

function ShapeImg(const src: TBitmap; const maskColor: TColor; const hnd : HWND) : Boolean;

implementation

//==============================================================================

procedure SetDefaultBmpType(var src : TBitmap);
begin

if src.PixelFormat <> pf24bit then
  src.PixelFormat := pf24bit;

if src.HandleType <> bmDIB then
  src.HandleType := bmDIB;

end;

//==============================================================================

function Clip(i: Integer): Byte;
begin
  if i > 255 then
    Result := 255
  else if i < 0 then
    Result := 0
  else
    Result := i;
end;

//==============================================================================

// Get transparency from top left pixel
function GetTransparentColor(const src : TBitmap): TColor;
var
  pixelPtr : PRGBTriple;
begin

pixelPtr := src.ScanLine[0];
Result :=
TColor(pixelPtr^.rgbtRed or (pixelPtr^.rgbtGreen shl 8)
or (pixelPtr^.rgbtBlue shl 16));

end;

//==============================================================================

function BmpTest(const bmp : TBitmap) : Boolean; {$ifdef COMPILER10_UP} inline; {$endif}
begin

Result := False;

if bmp.Empty then Exit;

if not (bmp.PixelFormat in[pf24Bit, pf32Bit]) then
begin
  raise Exception.Create('Error: Only 24/32 bit pixel formats are supported');
end;

Result := True;

end;

//==============================================================================

procedure PaintFocusRect(r : TRect; pCanvas : TCanvas);
begin

InflateRect( r, -2, -2);
pCanvas.Brush.Color := clBlack;
DrawFocusRect(pCanvas.Handle, r);

end;

//==============================================================================

procedure ResizeImage(const src : TBitmap; var dest : TBitmap;
              const srcOffset, ogWidth, ogHeight, newWidth, newHeight : Integer);
var
  i : Integer;
  center : Integer;
  srcX, srcY : Integer;
  destCopy : TBitmap;
begin

center := (ogWidth div 2) -1;
i := 0;
srcX := srcOffset;

dest.Width := newWidth;
dest.Height := newHeight;

// Make smaller width image
if newWidth < ogWidth then
  while i < newWidth do
  begin

    BitBlt(dest.Canvas.Handle, i, 0,
    1, src.Height,
    src.Canvas.Handle, srcX, 0, SRCCOPY);

    Inc(i, 1);
    Inc(srcX, 1);

    // Setup so last pixels to the right are blitted next
    if (srcX - srcOffset = newWidth - 5) then
      srcX := srcOffset + ogWidth -5;

  end
else if newWidth > ogWidth then // Make larger width image
  while i < newWidth do
  begin

    BitBlt(dest.Canvas.Handle, i, 0,
    1, src.Height,
    src.Canvas.Handle, srcX, 0, SRCCOPY);

    Inc(i, 1);

    //if ((srcX - srcOffset <= center) or (newWidth - i < center+2)) then
    if ((srcX - srcOffset +1<= center) or (newWidth - i < center+2)) then
      Inc(srcX, 1);

    // Setup so last pixels to the right are blitted next
    if (i - srcOffset = newWidth - srcOffset - 4) then
      srcX := srcOffset + ogWidth -4;

  end;

srcY := 0;
center := (ogHeight div 2) -1;
i := 0;

destCopy := TBitmap.Create;
destCopy.Width := dest.Width;
destCopy.Height := dest.Height;
destCopy.PixelFormat := pf24bit;
destCopy.Assign(dest);

// Make smaller height image
if newHeight < ogHeight then
  while i < newHeight do
  begin // This is not working right yet

    BitBlt(destCopy.Canvas.Handle, 0, i,
    dest.Width, 1,
    dest.Canvas.Handle, 0, SrcY, SRCCOPY);

    Inc(i, 1);

    if (srcY+1 <= center) or (srcY > newHeight div 2) then
      Inc(srcY, 1);

    if (srcY = newHeight div 2) then
      srcY := center;

    // Setup so last pixels to the bottom are blitted next
    if (i = newHeight - 3) then
      srcY := ogHeight -3;

  end
else if newHeight > ogHeight then // Make larger height image
  while i < newHeight do
  begin

    BitBlt(destCopy.Canvas.Handle, 0, i,
    dest.Width, 1,
    dest.Canvas.Handle, 0, SrcY, SRCCOPY);

    Inc(i, 1);

    if (srcY+1 <= center) then
      Inc(srcY, 1)
    else if (i-1 = newHeight div 2) then
      Inc(srcY, 1)
    else if ((newHeight - i < 4) and (newHeight - i > 0)) then
    Inc(srcY, 1);

    // Setup so last pixels to the bottom are blitted next
    if (i = newHeight - 3) then
      srcY := ogHeight -3;

  end;

dest.Assign(destCopy);
destCopy.Free;

end;

//==============================================================================

procedure MakeMaskMono(const src : TBitmap; var destMask, destMono : TBitmap; transparentColor : TColor);
var
  DDB    : TBitmap;
  width, height : Integer;
begin

width := src.Width;
height := src.Height;

destMask.Width := width;
destMask.Height := height;
destMono.Width := width;
destMono.Height := height;

DDB := TBitmap.Create;
DDB.Assign(src);
DDB.HandleType := bmDDB;
DDB.Canvas.Brush.Color := transparentColor;
DDB.Canvas.Handle;

destMono.Monochrome := True;

BitBlt(destMono.Canvas.Handle, 0, 0,
width, height, DDB.Canvas.Handle, 0, 0, SRCCOPY);

DDB.Free;

BitBlt(destMask.Canvas.Handle,0,0,width,height,src.Canvas.Handle,0,0,SRCCOPY);
BitBlt(destMono.Canvas.Handle,0,0,width,height,0,0,0,DSTINVERT);
BitBlt(destMask.Canvas.Handle,0,0,width,height,destMono.Canvas.Handle,0,0,SRCAND);
BitBlt(destMono.Canvas.Handle,0,0,width,height,0,0,0, DSTINVERT);

end;
//==============================================================================

procedure DrawTransparent(const src : TBitmap; dest : TCanvas; transparentColor : TColor);
var
  bmpMask : TBitmap;
  bmpMono : TBitmap;
  DDB    : TBitmap;
  width, height : Integer;
begin

width := src.Width;
height := src.Height;

bmpMask := TBitmap.Create;
bmpMask.Width := width;
bmpMask.Height := height;

bmpMono := TBitmap.Create;
bmpMono.Width := width;
bmpMono.Height := height;

DDB := TBitmap.Create;
DDB.Assign(src);
DDB.HandleType := bmDDB;
DDB.Canvas.Brush.Color := transparentColor;
DDB.Canvas.Handle;

bmpMono.Monochrome := True;

BitBlt(bmpMono.Canvas.Handle, 0, 0,
width, height, DDB.Canvas.Handle, 0, 0, SRCCOPY);

DDB.Free;

BitBlt(bmpMask.Canvas.Handle,0,0,width,height,src.Canvas.Handle,0,0,SRCCOPY);
BitBlt(bmpMono.Canvas.Handle,0,0,width,height,0,0,0,DSTINVERT);
BitBlt(bmpMask.Canvas.Handle,0,0,width,height,bmpMono.Canvas.Handle,0,0,SRCAND);
BitBlt(bmpMono.Canvas.Handle,0,0,width,height,0,0,0, DSTINVERT);

// Paint

BitBlt(dest.Handle, 0, 0,
width, height, bmpMono.Canvas.Handle, 0, 0, SRCAND);
BitBlt(dest.Handle, 0, 0,
width, height, bmpMask.Canvas.Handle, 0, 0, SRCPAINT);

bmpMask.Free;
bmpMono.Free;

end;

//==============================================================================

procedure MakeTransparent(var src : TBitmap; transparentColor : TColor);
var
  bmpMask : TBitmap;
  bmpMono : TBitmap;
  DDB    : TBitmap;
  width, height : Integer;
begin

width := src.Width;
height := src.Height;

bmpMask := TBitmap.Create;
bmpMask.Width := width;
bmpMask.Height := height;

bmpMono := TBitmap.Create;
bmpMono.Width := width;
bmpMono.Height := height;

DDB := TBitmap.Create;
DDB.Assign(src);
DDB.HandleType := bmDDB; // MUST come AFTER assign DDB
DDB.Canvas.Brush.Color := transparentColor;
DDB.Canvas.Handle;

bmpMono.Monochrome := True;

// - Make mono
BitBlt(bmpMono.Canvas.Handle, 0, 0,
width, height, DDB.Canvas.Handle, 0, 0, SRCCOPY);

DDB.Free;

// - Make mask
// Copy source to masked bmp
BitBlt(bmpMask.Canvas.Handle,0,0,width,height,src.Canvas.Handle,0,0,SRCCOPY);

// Set transparent pixels to black in masked bmp
BitBlt(bmpMono.Canvas.Handle,0,0,width,height,0,0,0,DSTINVERT); // Invert mono
BitBlt(bmpMask.Canvas.Handle,0,0,width,height,bmpMono.Canvas.Handle,0,0,SRCAND);
BitBlt(bmpMono.Canvas.Handle,0,0,width,height,0,0,0, DSTINVERT); // Restore mono

// Paint

BitBlt(src.Canvas.Handle, 0, 0,
width, height, bmpMono.Canvas.Handle, 0, 0, SRCAND);

BitBlt(src.Canvas.Handle, 0, 0,
width, height, bmpMask.Canvas.Handle, 0, 0, SRCPAINT);

bmpMask.Free;
bmpMono.Free;

end;

//==============================================================================

procedure SplitStdBmp(const src : TBitmap; var dest : array of TBitmap; const arraySize : Integer;  var transparentColor : TColor);
var
  bmp : TBitmap;
  width, height : Integer;
  left : Integer;
  i : Integer;
begin

if src.Empty then Exit;
                               
width := src.Width div arraySize;
height := src.Height;

bmp := TBitmap.Create;
bmp.Height := height;
bmp.Width := width;
transparentColor := GetTransparentColor(src);
bmp.TransparentColor := transparentColor;

left := 0;

for i := 0 to arraySize -1 do
begin

  BitBlt(bmp.Canvas.Handle, 0, 0,
  width,
  height,
  src.Canvas.Handle,
  left, 0, SRCCOPY);

  dest[i].Width := width;
  dest[i].Height := height;

  dest[i].Assign(bmp);
  left := left + width;

end;

bmp.Free;

end;


//==============================================================================

procedure SplitBtnBmp(const src : TBitmap; var dest1, dest2, dest3, dest4 : TBitmap; var transparentColor : TColor);
var
  bmp : TBitmap;
  width, height : Integer;
  left : Integer;
  i : Integer;
begin

if src.Empty then Exit;
                               
width := (src.Width - 1) div 4;
height := src.Height;

dest1.Width := width;
dest2.Width := width;
dest3.Width := width;
dest4.Width := width;
dest1.Height := height;
dest2.Height := height;
dest3.Height := height;
dest4.Height := height;

bmp := TBitmap.Create;
bmp.Height := height;
bmp.Width := width;
transparentColor := GetTransparentColor(src);
bmp.TransparentColor := transparentColor;

left := 1;

for i := 0 to 3 do
begin

  BitBlt(bmp.Canvas.Handle, 0, 0,
  width,
  height,
  src.Canvas.Handle,
  left, 0, SRCCOPY);

  //MakeTransparent(bmp, transparentColor);

  if i = 0 then
    dest1.Assign(bmp)
  else if i = 1 then
    dest2.Assign(bmp)
  else if i = 2 then
    dest3.Assign(bmp)
  else if i = 3 then
    dest4.Assign(bmp);

  left := left + width;

end;

bmp.Free;

end;

//==============================================================================

procedure SplitCheckboxBmp(const src : TBitmap; var dest1, dest2, dest3, dest4, dest5, dest6, dest7, dest8 : TBitmap; var transparentColor : TColor);
var
  bmp : TBitmap;
  width, height : Integer;
  left : Integer;
  i : Integer;
begin

if src.Empty then Exit;
                               
width := (src.Width - 1) div 8;
height := src.Height;

dest1.Width := width;
dest2.Width := width;
dest3.Width := width;
dest4.Width := width;
dest5.Width := width;
dest6.Width := width;
dest7.Width := width;
dest8.Width := width;
dest1.Height := height;
dest2.Height := height;
dest3.Height := height;
dest4.Height := height;
dest5.Height := height;
dest6.Height := height;
dest7.Height := height;
dest8.Height := height;

bmp := TBitmap.Create;
bmp.Height := height;
bmp.Width := width;
transparentColor := GetTransparentColor(src);
bmp.TransparentColor := transparentColor;

left := 1;

for i := 0 to 7 do
begin

  BitBlt(bmp.Canvas.Handle, 0, 0,
  width,
  height,
  src.Canvas.Handle,
  left, 0, SRCCOPY);

  if i = 0 then
    dest1.Assign(bmp)
  else if i = 1 then
    dest2.Assign(bmp)
  else if i = 2 then
    dest3.Assign(bmp)
  else if i = 3 then
    dest4.Assign(bmp)
  else if i = 4 then
    dest5.Assign(bmp)
  else if i = 5 then
    dest6.Assign(bmp)
  else if i = 6 then
    dest7.Assign(bmp)
  else if i = 7 then
    dest8.Assign(bmp);

  left := left + width;

end;

bmp.Free;

end;

//==============================================================================

function ConvertToGreyscale(const bmp : TBitmap) : TBitmap;
var
  y : Integer;
  x : Integer;
  grey : Byte;
  p : PRGBTriple; // Pixel Pointer
begin

Result := bmp;
if not BmpTest(bmp) then Exit;

for y := 0 to bmp.Height-1 do
begin
  p := bmp.ScanLine[y];
  for x := 0 to bmp.Width-1 do
  begin
    grey := (p.rgbtBlue * + p.rgbtGreen * + p.rgbtRed) div 3;
    p.rgbtBlue := grey;
    p.rgbtGreen := grey;
    p.rgbtRed := grey;
    Inc(p);
  end;
end;

end;

//==============================================================================

function ChangeBrightness(const bmp : TBitmap; const percent : Integer) : TBitmap;
var
  y : Integer;
  x : Integer;
  amount : Integer;
  p : PRGBTriple;
begin

Result := bmp;
if not BmpTest(bmp) then Exit;

amount := (255 * percent) div 200;

for y := 0 to Result.Height-1 do
begin
  p := Result.ScanLine[y];
  for x := 0 to Result.Width-1 do
  begin
    p.rgbtBlue := Clip(p.rgbtBlue + amount);
    p.rgbtGreen := Clip(p.rgbtGreen + amount);
    p.rgbtRed := Clip(p.rgbtRed + amount);
    Inc(p);
  end;
end;

end;

//==============================================================================

function NonClientRgn(const oForm: TCustomForm): HRGN;
var
  iLeft: Integer;
  iTop: Integer;
  hAdd: HRGN;
begin

iLeft := oForm.ClientOrigin.x - oForm.Left;
iTop := oForm.ClientOrigin.y - oForm.Top;

Result := CreateRectRgn(0, 0, oForm.Width, oForm.Height);
hAdd := CreateRectRgn(iLeft, iTop, iLeft + oForm.ClientWidth, iTop + oForm.ClientHeight);
CombineRgn(Result, Result, hAdd, RGN_XOR);
DeleteObject(hAdd);

end;

//==============================================================================

function ShapeForm(const oForm: TCustomForm; const
oMask: TBitmap; const iMaskColor: TColor): Boolean;
var
  iMaxWidth : Integer;
  iLeft : Integer;
  iTop : Integer;
  iRow : Integer;
  iCol : Integer;
  iStart : Integer;
  bMask : Boolean;
  hAdd : HRGN;
  hComp : HRGN;
  pixelPtr : PRGBTriple;
begin

Result := False;
iStart := -1;
iMaxWidth := 0;

iLeft := oForm.ClientOrigin.x - oForm.Left;
iTop := oForm.ClientOrigin.y - oForm.Top;
hComp := NonClientRgn(oForm);

if hComp = 0 then
  Exit;

for iRow := 0 to oMask.Height - 1 do
begin
  pixelPtr := oMask.scanline[iRow];

  for iCol := 0 to oMask.Width - 1 do
  begin
    bMask := TColor(pixelPtr^.rgbtRed or (pixelPtr^.rgbtGreen shl 8) or (pixelPtr^.rgbtBlue shl 16)) = iMaskColor;
    Inc(pixelPtr);
    // Old, slow methods
    //bMask := TColor(RGB(pixelPtr^.rgbtRed, pixelPtr^.rgbtGreen, pixelPtr^.rgbtBlue)) = iMaskColor;
    //bMask := TColor(GetPixel(hMask, iCol, iRow)) = iMaskColor;

    if not bMask and (iCol >= iMaxWidth) then
      iMaxWidth := iCol+1;

    if not bMask and (iStart < 0) then
      iStart := iCol
    else if bMask and (iStart >= 0) then
    begin
      hAdd := CreateRectRgn(iStart + iLeft, iRow + iTop,
      iCol + iLeft, iRow + iTop + 1);
      CombineRgn(hComp, hComp, hAdd, RGN_OR);
      DeleteObject(hAdd);
      iStart := -1;
    end;

  end;

  if iStart >= 0 then
  begin
    hAdd := CreateRectRgn(iStart + iLeft, iRow + iTop,
    oMask.Width + iLeft, iRow + iTop + 1);
    CombineRgn(hComp, hComp, hAdd, RGN_OR);
    DeleteObject(hAdd);
    iStart := -1;
  end;

end; // End loop

Result := SetWindowRgn(oForm.Handle, hComp, True) <> 0;

oForm.Width := iMaxWidth;
oForm.Height := oMask.Height;

end;

//==============================================================================

function ShapeImg(const src: TBitmap; const maskColor: TColor; const hnd : HWND) : Boolean;
var
  iRow : Integer;
  iCol : Integer;
  iStart : Integer;
  bMask : Boolean;
  hAdd : HRGN;
  hComp : HRGN;
  pixelPtr : PRGBTriple;
begin
         
Result := False;
iStart := -1;

hComp := CreateRectRgn(0, 0, src.Width, src.Height);
hAdd := CreateRectRgn(0, 0, src.Width, src.Height);
CombineRgn(hComp, hComp, hAdd, RGN_XOR);
DeleteObject(hAdd);

if hComp = 0 then
  Exit;

for iRow := 0 to src.Height - 1 do
begin
  pixelPtr := src.scanline[iRow];

  for iCol := 0 to src.Width - 1 do
  begin
    bMask := TColor(pixelPtr^.rgbtRed or (pixelPtr^.rgbtGreen shl 8) or (pixelPtr^.rgbtBlue shl 16)) = maskColor;
    Inc(pixelPtr);

    if not bMask and (iStart < 0) then
      iStart := iCol
    else if bMask and (iStart >= 0) then
    begin
      hAdd := CreateRectRgn(iStart, iRow,
      iCol, iRow + 1);
      CombineRgn(hComp, hComp, hAdd, RGN_OR);
      DeleteObject(hAdd);
      iStart := -1;
    end;

  end;

  if iStart >= 0 then
  begin
    hAdd := CreateRectRgn(iStart, iRow,
    src.Width, iRow + 1);
    CombineRgn(hComp, hComp, hAdd, RGN_OR);
    DeleteObject(hAdd);
    iStart := -1;
  end;

end; // End loop

Result := SetWindowRgn(hnd, hComp, True) <> 0;

end;

//==============================================================================


//==============================================================================


end.


