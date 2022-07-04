// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CRC.pas' rev: 4.00

#ifndef CRCHPP
#define CRCHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Crc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall CRC32Init(unsigned &crc);
extern PACKAGE void __fastcall CRC32Update(unsigned &crc, const void * buf, unsigned len);
extern PACKAGE void __fastcall CRC32BUpdate(unsigned &crc, const void * buf, unsigned len);
extern PACKAGE AnsiString __fastcall CRC32Final(unsigned &crc);

}	/* namespace Crc */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Crc;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CRC
