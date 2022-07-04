// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'InfBlock.pas' rev: 3.00

#ifndef InfBlockHPP
#define InfBlockHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Infblock
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall inflate_blocks_reset(Bzlib::inflate_blocks_state &s, Bzlib::z_stream 
	&z, Zutil::puLong c);
extern PACKAGE Bzlib::pInflate_blocks_state __fastcall inflate_blocks_new(Bzlib::z_stream &z, Bzlib::check_func 
	c, Cardinal w);
extern PACKAGE int __fastcall inflate_blocks(Bzlib::inflate_blocks_state &s, Bzlib::z_stream &z, int 
	r);
extern PACKAGE int __fastcall inflate_blocks_free(Bzlib::pInflate_blocks_state s, Bzlib::z_stream &z
	);
extern PACKAGE void __fastcall inflate_set_dictionary(Bzlib::inflate_blocks_state &s, const Byte * d
	, const int d_Size, Cardinal n);
extern PACKAGE int __fastcall inflate_blocks_sync_point(Bzlib::inflate_blocks_state &s);

}	/* namespace Infblock */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Infblock;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// InfBlock
