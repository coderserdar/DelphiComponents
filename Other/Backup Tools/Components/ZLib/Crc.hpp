// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Crc.pas' rev: 3.00

#ifndef CrcHPP
#define CrcHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Crc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Zutil::puLong __fastcall get_crc_table(void);
extern PACKAGE int __fastcall crc32(int crc, Zutil::pBytef buf, Cardinal len);

}	/* namespace Crc */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Crc;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Crc
