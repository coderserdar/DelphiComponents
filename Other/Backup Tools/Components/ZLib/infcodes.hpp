// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'InfCodes.pas' rev: 3.00

#ifndef InfCodesHPP
#define InfCodesHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Infcodes
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Bzlib::pInflate_codes_state __fastcall inflate_codes_new(Cardinal bl, Cardinal bd, Bzlib::pInflate_huft 
	tl, Bzlib::pInflate_huft td, Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflate_codes(Bzlib::inflate_blocks_state &s, Bzlib::z_stream &z, int 
	r);
extern PACKAGE void __fastcall inflate_codes_free(Bzlib::pInflate_codes_state c, Bzlib::z_stream &z)
	;

}	/* namespace Infcodes */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Infcodes;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// InfCodes
