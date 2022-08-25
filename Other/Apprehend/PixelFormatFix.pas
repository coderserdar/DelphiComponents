//------------------------------------------------------------------------------
//  Apprehend Version  : 5.1
//  Copyright © 1986-2011 : Adirondack Software & Graphics
//  Created            : 01-09-1992
//  Last Modification  : 08-08-2011
//  Compiler           : Delphi 2010
//  Description        : PixelFormatFix Unit
// This file is copyright (C) W W Miller, 1986-2011.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

// PixelFormatFix.pas
// used to prevent strange delphi/c++builder crash if pixelformat is not forced:
//
// "Mouser" (www.donationcoder.com), 5/12/05
// Capturing large bitmaps very frequently caused crashing of my applications
// with insufficient memory type errors.
// After some time I tracked down this newsgroup post which suggests that
// the cure is to set the bitmap object to 24bits per pixel prior to filling it.
// See -> http://groups-beta.google.com/group/borland.public.delphi.graphics/browse_thread/
// thread/2575992b6bd62e66/203f09c94f0b1396?q=bitmap+delphi+%22not+enough+storage+is+available+
// to+process+this+command%22&rnum=1&hl=en#203f09c94f0b1396
//  ".. if you dont declare the pixel format to be 24 bit the Bitmap will be seen as a device
// dependent bitmap and this may cause the problem. Setting pixelformat to pf24bit ensures
// that you will create a DIB. "
//

unit PixelFormatFix;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, StdCtrls, Menus, Clipbrd;

// another constant used to grab layered images + tooltips
//const BitBltRopMode=SRCCOPY;

// from wingdi.h#define CAPTUREBLT (DWORD)0x40000000 /* Include layered windows */ 
// const BitBltRopMode=CAPTUREBLT;
const BitBltRopMode_Win9x=SRCCOPY;
const BitBltRopMode_WinNT=$40000000 or SRCCOPY;
const BitBltRopMode=$40000000 or SRCCOPY;

procedure DoPixelFormatFix (bitmapp: TBitmap);
function GetBitBlt_RopMode() : Integer;

implementation


procedure DoPixelFormatFix (bitmapp: TBitmap);
begin
  bitmapp.PixelFormat := pf24bit;
//  bitmapp.PixelFormat := pfDevice;
//  bitmapp.PixelFormat := pf32bit;
end;


function GetBitBlt_RopMode() : Integer;
begin
  GetVersion();
  if (Win32Platform=VER_PLATFORM_WIN32_NT) then
    Result := BitBltRopMode_WinNT
  else
    Result := BitBltRopMode_Win9x;
end;



end.


