// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDbUtils.pas' rev: 6.00

#ifndef DsDbUtilsHPP
#define DsDbUtilsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbApi.hpp>	// Pascal unit
#include <DsConsts.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsdbutils
{
//-- type declarations -------------------------------------------------------
typedef Set<char, 0, 255>  TCharSet;

typedef Byte TBits8[8];

typedef Byte *pBits8;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall Ds_StrScan(const AnsiString S, char Ch, int StartPos = 0x1);
extern PACKAGE bool __fastcall Ds_SameStr(const AnsiString S1, const AnsiString S2);
extern PACKAGE bool __fastcall Ds_SameText(const AnsiString S1, const AnsiString S2);
extern PACKAGE int __fastcall Ds_CompStr(const AnsiString S1, const AnsiString S2);
extern PACKAGE int __fastcall Ds_CompText(const AnsiString S1, const AnsiString S2);
extern PACKAGE AnsiString __fastcall Ds_UpperCase(const AnsiString S);
extern PACKAGE AnsiString __fastcall Ds_LowerCase(const AnsiString S);
extern PACKAGE bool __fastcall Ds_TestByMask(const AnsiString S, const AnsiString Mask, char MaskChar = '\x58');
extern PACKAGE bool __fastcall Ds_TestWildStr(const AnsiString S, const AnsiString Mask, char MaskChar = '\x3f', char WildCard = '\x2a');
extern PACKAGE bool __fastcall Ds_TestWildText(const AnsiString S, const AnsiString Mask, char MaskChar = '\x3f', char WildCard = '\x2a');
extern PACKAGE void __fastcall Ds_StrToAnsi(AnsiString &S);
extern PACKAGE char * __fastcall Ds_PStrToAnsi(char * P);
extern PACKAGE void __fastcall Ds_TinyCopy(void * Source, void * Dest, unsigned L);
extern PACKAGE void __fastcall Ds_Move(void * Source, void * Dest, unsigned Count);
extern PACKAGE void __fastcall Ds_ReverseByteArr(void * P, unsigned Count);
extern PACKAGE unsigned __fastcall Ds_CountByte(int N, void * ArrPtr, unsigned L);
extern PACKAGE AnsiString __fastcall Ds_PadRight(const AnsiString S, int Length, char PadCh = '\x20', bool Cut = false);
extern PACKAGE AnsiString __fastcall Ds_GetWordN(int OrdN, const AnsiString S, const TCharSet &Delimiters);
extern PACKAGE int __fastcall Ds_PosText(const AnsiString FindString, const AnsiString SourceString, int StartPos);
extern PACKAGE AnsiString __fastcall Ds_ReplaceText(const AnsiString SourceString, const AnsiString FindString, const AnsiString ReplaceString);
extern PACKAGE bool __fastcall Ds_DeleteFiles(const AnsiString FileMask);
extern PACKAGE int __pascal Ds_SwapInt(const int Value);
extern PACKAGE __int64 __pascal Ds_SwapInt64(const __int64 Value);
extern PACKAGE void __fastcall Ds_PdxToSmall(void * pData);
extern PACKAGE void __fastcall Ds_SmallToPdx(void * pData);
extern PACKAGE void __fastcall Ds_PdxToInt(void * pData);
extern PACKAGE void __fastcall Ds_IntToPdx(void * pData);
extern PACKAGE void __fastcall Ds_PdxToDouble(void * pData);
extern PACKAGE void __fastcall Ds_DoubleToPdx(void * pData);
extern PACKAGE void __fastcall Ds_BCDToPdx(void * pData);
extern PACKAGE void __fastcall Ds_PdxToBCD(void * pData);
extern PACKAGE bool __fastcall Ds_IsDir(const AnsiString DatabaseName);
extern PACKAGE System::TDateTime __fastcall Ds_GetDate(const AnsiString Value);
extern PACKAGE System::TDateTime __fastcall Ds_GetTime(const AnsiString Value);
extern PACKAGE System::TDateTime __fastcall Ds_GetDateTime(const AnsiString Value);
extern PACKAGE AnsiString __fastcall Ds_NormDir(const AnsiString DirName);
extern PACKAGE System::TDateTime __fastcall Ds_DataToDateTime(Db::TFieldType DataType, const Db::TDateTimeRec &Data);
extern PACKAGE AnsiString __fastcall Ds_BoolToStr(const bool Value);
extern PACKAGE void __fastcall Ds_ByteToBits(Byte B, pBits8 pBuf);
extern PACKAGE void __fastcall Ds_BitsToByte(Byte &B, pBits8 pBuf);
extern PACKAGE Byte __fastcall Ds_IF(bool Cond, Byte ValTrue, Byte ValFalse)/* overload */;
extern PACKAGE int __fastcall Ds_IF(bool Cond, int ValTrue, int ValFalse)/* overload */;
extern PACKAGE double __fastcall Ds_IF(bool Cond, double ValTrue, double ValFalse)/* overload */;
extern PACKAGE int __fastcall Ds_FindInFile(int Handle, const int FromPos, const Sysutils::PByteArray Pattern, int PatternLen);

}	/* namespace Dsdbutils */
using namespace Dsdbutils;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDbUtils
