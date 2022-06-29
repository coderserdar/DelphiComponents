// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Utf8.pas' rev: 5.00

#ifndef Utf8HPP
#define Utf8HPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Utf8
{
//-- type declarations -------------------------------------------------------
typedef AnsiString UTF8String;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE AnsiString __fastcall ToUtf8Ansi(const AnsiString Data);
extern PACKAGE AnsiString __fastcall ToUtf8Wide(const WideString Data);
extern PACKAGE AnsiString __fastcall ToAnsiString(const AnsiString Utf8);
extern PACKAGE WideString __fastcall ToWideString(const AnsiString Utf8);

}	/* namespace Utf8 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Utf8;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Utf8
