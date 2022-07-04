// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CryptoUtils.pas' rev: 4.00

#ifndef CryptoUtilsHPP
#define CryptoUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cryptoutils
{
//-- type declarations -------------------------------------------------------
typedef __int64 TInt64Array[1];

typedef __int64 *PInt64Array;

typedef unsigned TDWordArray[1];

typedef unsigned *PDWordArray;

typedef Byte TByteArray[1];

typedef Byte *PByteArray;

typedef Byte *PByte;

typedef unsigned *PLongWord;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE AnsiString __fastcall IntToHex(__int64 Int, Byte IntSize);
extern PACKAGE unsigned __fastcall ror(unsigned x, Byte y);
extern PACKAGE unsigned __fastcall rol(unsigned x, Byte y);
extern PACKAGE __int64 __fastcall ror64(__int64 x, Byte y);
extern PACKAGE unsigned __fastcall Endian(unsigned X);
extern PACKAGE __int64 __fastcall Endian64(__int64 X);

}	/* namespace Cryptoutils */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Cryptoutils;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CryptoUtils
