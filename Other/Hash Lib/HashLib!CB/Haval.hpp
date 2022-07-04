// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HAVAL.pas' rev: 4.00

#ifndef HAVALHPP
#define HAVALHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Haval
{
//-- type declarations -------------------------------------------------------
struct THavalCtx;
typedef THavalCtx *PHavalCtx;

#pragma pack(push, 4)
struct THavalCtx
{
	unsigned count[2];
	unsigned fingerprint[8];
	unsigned block[32];
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall HavalInit(THavalCtx &state);
extern PACKAGE void __fastcall HavalUpdate(THavalCtx &state, void * str, unsigned str_len, unsigned 
	PASS);
extern PACKAGE AnsiString __fastcall HavalFinal(THavalCtx &state, unsigned PASS, unsigned HashLen);

}	/* namespace Haval */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Haval;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HAVAL
