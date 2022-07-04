// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SHA.pas' rev: 4.00

#ifndef SHAHPP
#define SHAHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Sha
{
//-- type declarations -------------------------------------------------------
struct TSHA512Ctx;
typedef TSHA512Ctx *PSHA512Ctx;

#pragma pack(push, 4)
struct TSHA512Ctx
{
	__int64 state[8];
	__int64 length;
	__int64 curlen;
	Byte buf[128];
} ;
#pragma pack(pop)

struct TSHA256Ctx;
typedef TSHA256Ctx *PSHA256Ctx;

#pragma pack(push, 4)
struct TSHA256Ctx
{
	unsigned state[8];
	unsigned length;
	unsigned curlen;
	Byte buf[64];
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall SHA1Init(TSHA256Ctx &md);
extern PACKAGE void __fastcall SHA256Init(TSHA256Ctx &md);
extern PACKAGE void __fastcall SHA384Init(TSHA512Ctx &md);
extern PACKAGE void __fastcall SHA512Init(TSHA512Ctx &md);
extern PACKAGE void __fastcall SHA256Update(TSHA256Ctx &md, void * buf, unsigned len, Word sz);
extern PACKAGE void __fastcall SHA512Update(TSHA512Ctx &md, void * buf, unsigned len);
extern PACKAGE AnsiString __fastcall SHA512Final(TSHA512Ctx &md, Word sz);
extern PACKAGE AnsiString __fastcall SHA256Final(TSHA256Ctx &md, Word sz);

}	/* namespace Sha */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Sha;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SHA
