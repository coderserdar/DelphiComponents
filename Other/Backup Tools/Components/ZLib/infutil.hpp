// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'infutil.pas' rev: 3.00

#ifndef infutilHPP
#define infutilHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Infutil
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Cardinal inflate_mask[17];
extern PACKAGE int __fastcall inflate_flush(Bzlib::inflate_blocks_state &s, Bzlib::z_stream &z, int 
	r);

}	/* namespace Infutil */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Infutil;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// infutil
