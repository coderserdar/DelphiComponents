// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Gost.pas' rev: 4.00

#ifndef GostHPP
#define GostHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gost
{
//-- type declarations -------------------------------------------------------
typedef unsigned TArr8LW[8];

struct TGostCtx;
typedef TGostCtx *PGostCtx;

#pragma pack(push, 4)
struct TGostCtx
{
	unsigned sum[8];
	unsigned hash[8];
	unsigned len[8];
	Byte partial[32];
	unsigned partial_bytes;
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall GostInit(TGostCtx &ctx);
extern PACKAGE void __fastcall GostUpdate(TGostCtx &ctx, const void * buf, unsigned len);
extern PACKAGE AnsiString __fastcall GostFinal(TGostCtx &ctx);

}	/* namespace Gost */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Gost;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gost
