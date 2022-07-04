// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'zUnCompr.pas' rev: 3.00

#ifndef zUnComprHPP
#define zUnComprHPP
#include <zInflate.hpp>
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zuncompr
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall uncompress(Zutil::pBytef dest, int &destLen, const Byte * source, const 
	int source_Size, int sourceLen);

}	/* namespace Zuncompr */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zuncompr;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// zUnCompr
