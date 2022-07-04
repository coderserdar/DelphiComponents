// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'aCRC32.pas' rev: 3.00

#ifndef aCRC32HPP
#define aCRC32HPP
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Acrc32
{
//-- type declarations -------------------------------------------------------
typedef int int;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall CalculateCRC32(void *Buf, int Count);
extern PACKAGE int __fastcall CRC32Calc(void *Buf, int CRC, int BufSize);
extern PACKAGE bool __fastcall TestCRC(void);
extern PACKAGE int __fastcall CRC32Finish(int CRC);
extern PACKAGE int __fastcall CRC32Start(void);

}	/* namespace Acrc32 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Acrc32;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// aCRC32
