// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Tiger.pas' rev: 4.00

#ifndef TigerHPP
#define TigerHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tiger
{
//-- type declarations -------------------------------------------------------
struct TTigerCtx;
typedef TTigerCtx *PTigerCtx;

#pragma pack(push, 4)
struct TTigerCtx
{
	__int64 state[3];
	unsigned length;
	unsigned curlen;
	Byte buf[64];
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall TigerInit(TTigerCtx &ctx);
extern PACKAGE void __fastcall TigerUpdate(TTigerCtx &ctx, void * buf, unsigned len, unsigned PASSES
	);
extern PACKAGE AnsiString __fastcall TigerFinal(TTigerCtx &ctx, Word sz, unsigned PASSES);

}	/* namespace Tiger */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Tiger;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Tiger
