// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'RIPEMD.pas' rev: 4.00

#ifndef RIPEMDHPP
#define RIPEMDHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Ripemd
{
//-- type declarations -------------------------------------------------------
struct TRMDCtx;
typedef TRMDCtx *PRMDCtx;

#pragma pack(push, 4)
struct TRMDCtx
{
	unsigned MDBuf[5];
	unsigned length;
	unsigned curlen;
	Byte buf[64];
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall RMD128Init(TRMDCtx &md);
extern PACKAGE void __fastcall RMD160Init(TRMDCtx &md);
extern PACKAGE void __fastcall RMDUpdate(TRMDCtx &md, void * buf, unsigned len, Word sz);
extern PACKAGE AnsiString __fastcall RMDFinal(TRMDCtx &md, unsigned sz);

}	/* namespace Ripemd */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Ripemd;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// RIPEMD
