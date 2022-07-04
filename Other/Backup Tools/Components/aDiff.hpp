// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'aDiff.pas' rev: 3.00

#ifndef aDiffHPP
#define aDiffHPP
#include <aCRC32.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Adiff
{
//-- type declarations -------------------------------------------------------
typedef int *PInteger;

typedef void __fastcall (*TMyProcedure)(void);

typedef Byte TByteArray[2147483647];

typedef TByteArray *PByteArray;

struct TDiffCompData
{
	TByteArray *InBuf;
	TByteArray *UseBuf;
	int InBufSize;
	int UseBufSize;
	int MaxLevel;
	int MaxLength;
	int MinLength;
	TByteArray *OutBuf;
	TByteArray *OutSpBuf;
	int *OutBufSize;
	int *OutSpBufSize;
	int EnoughLen;
} ;

//-- var, const, procedure ---------------------------------------------------
#define BufSize (int)(2097152)
#define SSmallBuffer "Buffer size should be at least 4 bytes"
extern PACKAGE int __fastcall Max(int a, int b);
extern PACKAGE int __fastcall Min(int a, int b);
extern PACKAGE int __fastcall comp(void *a, void *b, int len);
extern PACKAGE void __fastcall DiffCompress(const TDiffCompData &D);
extern PACKAGE void __fastcall DiffStreamCompress(Classes::TStream* InStream, Classes::TStream* UseStream
	, Classes::TStream* OutStream, TMyProcedure Notify, int MaxLev);
extern PACKAGE void __fastcall DiffStreamExtract(Classes::TStream* InStream, Classes::TStream* UseStream
	, Classes::TStream* OutStream, TMyProcedure Notify);
extern PACKAGE int __fastcall CalcHash(int a, int b, int c);

}	/* namespace Adiff */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Adiff;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// aDiff
