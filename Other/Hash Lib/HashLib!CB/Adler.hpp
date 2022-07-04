// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Adler.pas' rev: 4.00

#ifndef AdlerHPP
#define AdlerHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Adler
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Adler32Init(unsigned &adler);
extern PACKAGE void __fastcall Adler32Update(unsigned &adler, const void * buf, unsigned len);
extern PACKAGE AnsiString __fastcall Adler32Final(unsigned &adler);

}	/* namespace Adler */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Adler;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Adler
