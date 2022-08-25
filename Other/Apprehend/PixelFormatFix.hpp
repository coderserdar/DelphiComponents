// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PixelFormatFix.pas' rev: 6.00

#ifndef PixelFormatFixHPP
#define PixelFormatFixHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Clipbrd.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pixelformatfix
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const int BitBltRopMode_Win9x = 0xcc0020;
static const int BitBltRopMode_WinNT = 0x40cc0020;
static const int BitBltRopMode = 0x40cc0020;
extern PACKAGE void __fastcall DoPixelFormatFix(Graphics::TBitmap* bitmapp);
extern PACKAGE int __fastcall GetBitBlt_RopMode(void);

}	/* namespace Pixelformatfix */
using namespace Pixelformatfix;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PixelFormatFix
