// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'InfFast.pas' rev: 3.00

#ifndef InfFastHPP
#define InfFastHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Inffast
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall inflate_fast(Cardinal bl, Cardinal bd, Bzlib::pInflate_huft tl, Bzlib::pInflate_huft 
	td, Bzlib::inflate_blocks_state &s, Bzlib::z_stream &z);

}	/* namespace Inffast */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Inffast;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// InfFast
