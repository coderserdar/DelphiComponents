// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'InfTrees.pas' rev: 3.00

#ifndef InfTreesHPP
#define InfTreesHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Inftrees
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
#define MANY (Word)(1440)
extern PACKAGE int __fastcall inflate_trees_bits(Cardinal * c, const int c_Size, Cardinal &bb, Bzlib::pInflate_huft 
	&tb, Bzlib::inflate_huft * hp, const int hp_Size, Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflate_trees_dynamic(Cardinal nl, Cardinal nd, Cardinal * c, const int 
	c_Size, Cardinal &bl, Cardinal &bd, Bzlib::pInflate_huft &tl, Bzlib::pInflate_huft &td, Bzlib::inflate_huft 
	* hp, const int hp_Size, Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflate_trees_fixed(Cardinal &bl, Cardinal &bd, Bzlib::pInflate_huft &
	tl, Bzlib::pInflate_huft &td, Bzlib::z_stream &z);

}	/* namespace Inftrees */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Inftrees;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// InfTrees
