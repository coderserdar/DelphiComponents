// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CryptoAPI.pas' rev: 4.00

#ifndef CryptoAPIHPP
#define CryptoAPIHPP

#pragma delphiheader begin
#pragma option push -w-
#include <CryptoUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <RIPEMD.hpp>	// Pascal unit
#include <Tiger.hpp>	// Pascal unit
#include <SHA.hpp>	// Pascal unit
#include <HAVAL.hpp>	// Pascal unit
#include <Gost.hpp>	// Pascal unit
#include <Adler.hpp>	// Pascal unit
#include <CRC.hpp>	// Pascal unit
#include <MD.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cryptoapi
{
//-- type declarations -------------------------------------------------------
struct THashContext;
typedef THashContext *PHashContext;

#pragma pack(push, 4)
struct THashContext
{
	void *IntData;
	unsigned HashType;
	unsigned lParam;
	unsigned wParam;
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Word HASH_INITIAL = 0x100;
static const Word HASH_MD2 = 0x101;
static const Word HASH_MD4 = 0x102;
static const Word HASH_MD5 = 0x103;
static const Word HASH_CRC32 = 0x104;
static const Word HASH_CRC32B = 0x105;
static const Word HASH_ADLER32 = 0x106;
static const Word HASH_GOST = 0x107;
static const Word HASH_HAVAL128 = 0x108;
static const Word HASH_HAVAL160 = 0x109;
static const Word HASH_HAVAL192 = 0x10a;
static const Word HASH_HAVAL224 = 0x10b;
static const Word HASH_HAVAL256 = 0x10c;
static const Word HASH_SHA1 = 0x10d;
static const Word HASH_SHA256 = 0x10e;
static const Word HASH_SHA384 = 0x10f;
static const Word HASH_SHA512 = 0x110;
static const Word HASH_TIGER128 = 0x111;
static const Word HASH_TIGER160 = 0x112;
static const Word HASH_TIGER192 = 0x113;
static const Word HASH_RIPEMD128 = 0x114;
static const Word HASH_RIPEMD160 = 0x115;
static const Shortint HASH_NOERROR = 0x0;
static const Shortint HASH_UNK_TYPE = 0x1;
static const Shortint HASH_NIL_CONTEXT = 0x2;
static const Shortint HASH_INV_CONTEXT = 0x3;
static const Shortint HASH_FR_ERROR = 0x4;
static const Shortint HASH_FO_ERROR = 0x5;
static const Shortint HASH_TEST_FAILED = 0x6;
static const Shortint HASH_MAX_TYPES = 0x15;
extern PACKAGE unsigned __fastcall HashInit(PHashContext Context, unsigned HashType);
extern PACKAGE unsigned __fastcall HashUpdate(PHashContext Context, void * SrcBuf, unsigned BufLen);
	
extern PACKAGE unsigned __fastcall HashFinal(PHashContext Context, AnsiString &DestHash);
extern PACKAGE unsigned __fastcall HashStr(unsigned HashType, AnsiString SrcStr, AnsiString &DestHash
	);
extern PACKAGE unsigned __fastcall HashBuf(unsigned HashType, void * SrcBuf, unsigned BufLen, AnsiString 
	&DestHash);
extern PACKAGE unsigned __fastcall HashFile(unsigned HashType, AnsiString FileName, AnsiString &DestHash
	);
extern PACKAGE unsigned __fastcall HashFilePartial(unsigned HashType, AnsiString FileName, unsigned 
	FlOffsetLow, unsigned FlOffsetHigh, AnsiString &DestHash);
extern PACKAGE unsigned __fastcall EnumHashTypes(void * StoreToArr, unsigned MaxItems);
extern PACKAGE AnsiString __fastcall HashErrorToStr(unsigned Error);

}	/* namespace Cryptoapi */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Cryptoapi;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CryptoAPI
