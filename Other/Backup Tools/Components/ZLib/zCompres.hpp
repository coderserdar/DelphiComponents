// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'zCompres.pas' rev: 3.00

#ifndef zCompresHPP
#define zCompresHPP
#include <zDeflate.hpp>
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zcompres
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall compress2(Zutil::pBytef dest, int &destLen, const Byte * source, const 
	int source_Size, int sourceLen, int level);
extern PACKAGE int __fastcall compress(Zutil::pBytef dest, int &destLen, const Byte * source, const 
	int source_Size, int sourceLen);

}	/* namespace Zcompres */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zcompres;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// zCompres
