(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMScrnCap;

interface

uses WinTypes, WinProcs, Forms, Classes, Graphics, Controls;


function CaptureWindowRect(AHWND: HWND; ARect : TRect) : TBitmap;

function CaptureScreenRect(ARect : TRect) : TBitmap;

function CaptureScreen : TBitmap;

function CaptureClientImage(Control : TControl) : TBitmap;
 
function CaptureControlImage(Control : TControl) : TBitmap;
{===============================================================}

implementation

function GetSystemPalette : HPalette;
var
 PaletteSize  : integer;
 LogSize      : integer;
 LogPalette   : PLogPalette;
 DC           : HDC;
 Focus        : HWND;
begin
 Focus:=GetFocus;
 DC:=GetDC(Focus);
 try
   PaletteSize:=GetDeviceCaps(DC, SIZEPALETTE);
   LogSize:=SizeOf(TLogPalette)+(PaletteSize-1)*SizeOf(TPaletteEntry);
   GetMem(LogPalette, LogSize);
   try
     with LogPalette^ do
     begin
       palVersion:=$0300;
       palNumEntries:=PaletteSize;
       GetSystemPaletteEntries(DC, 0, PaletteSize, palPalEntry);
     end;
     result:=CreatePalette(LogPalette^);
   finally
     FreeMem(LogPalette, LogSize);
   end;
 finally
   ReleaseDC(Focus, DC);
 end;
end;

function CaptureWindowRect(AHWND: HWND; ARect : TRect) : TBitmap;
var
 ScreenDC : HDC;
begin
 Result:=TBitmap.Create;
 with result, ARect do begin
  Width:=Right-Left;
  Height:=Bottom-Top;
  ScreenDC:=GetDC(AHWND);
  try
    BitBlt(Canvas.Handle, 0,0,Width,Height,ScreenDC, Left, Top, SRCCOPY);
  finally
    ReleaseDC(Handle, ScreenDC);
  end;
 end;
end;

function CaptureScreenRect(ARect: TRect) : TBitmap;
begin
 result:= CaptureWindowRect(0,ARect);
end;

function CaptureScreen : TBitmap;
begin
 with Screen do
  Result:=CaptureScreenRect(Rect(0,0,Width,Height));
end;

function CaptureClientImage(Control : TControl) : TBitmap;
begin
 with Control, Control.ClientOrigin do
  result:=CaptureScreenRect(Bounds(X,Y,ClientWidth,ClientHeight));
end;

function CaptureControlImage(Control : TControl) : TBitmap;
begin
 with Control do
  if Parent=Nil then
    result:=CaptureScreenRect(Bounds(Left,Top,Width,Height))
  else
   with Parent.ClientToScreen(Point(Left, Top)) do
    result:=CaptureScreenRect(Bounds(X,Y,Width,Height));
end;

end.
