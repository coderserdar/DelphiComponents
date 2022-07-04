// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MD.pas' rev: 4.00

#ifndef MDHPP
#define MDHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Md
{
//-- type declarations -------------------------------------------------------
struct TMD2CTX;
typedef TMD2CTX *PMD2CTX;

#pragma pack(push, 1)
struct TMD2CTX
{
	Byte D[48];
	Byte C[16];
	Byte i;
	Byte L;
} ;
#pragma pack(pop)

struct TMD4Ctx;
typedef TMD4Ctx *PMD4Ctx;

#pragma pack(push, 4)
struct TMD4Ctx
{
	unsigned state[4];
	unsigned count[2];
	Byte buffer[64];
} ;
#pragma pack(pop)

typedef void __fastcall (*TMDTransform)(unsigned * state, const int state_Size, void * block);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall MD2Init(PMD2CTX md);
extern PACKAGE void __fastcall MD2Update(PMD2CTX md, void * SrcBuf, unsigned BufLen);
extern PACKAGE AnsiString __fastcall MD2Final(PMD2CTX md);
extern PACKAGE void __fastcall MDInit(PMD4Ctx context);
extern PACKAGE void __fastcall MD4Transform(unsigned * state, const int state_Size, void * block);
extern PACKAGE void __fastcall MDUpdate(PMD4Ctx context, void * input, unsigned inputLen, TMDTransform 
	tr_func);
extern PACKAGE AnsiString __fastcall MDFinal(PMD4Ctx context, TMDTransform tr_func);
extern PACKAGE void __fastcall MD5Transform(unsigned * state, const int state_Size, void * block);

}	/* namespace Md */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Md;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MD
