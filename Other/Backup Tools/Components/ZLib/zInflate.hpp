// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'zInflate.pas' rev: 3.00

#ifndef zInflateHPP
#define zInflateHPP
#include <infutil.hpp>
#include <InfBlock.hpp>
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zinflate
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int __fastcall inflateReset(Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflateEnd(Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflateInit2_(Bzlib::z_stream &z, int w, const System::AnsiString version
	, int stream_size);
extern PACKAGE int __fastcall inflateInit(Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflateInit_(Bzlib::z_streamp z, const System::AnsiString version, int 
	stream_size);
extern PACKAGE int __fastcall inflate(Bzlib::z_stream &z, int f);
extern PACKAGE int __fastcall inflateSetDictionary(Bzlib::z_stream &z, Zutil::pBytef dictionary, Cardinal 
	dictLength);
extern PACKAGE int __fastcall inflateSync(Bzlib::z_stream &z);
extern PACKAGE int __fastcall inflateSyncPoint(Bzlib::z_stream &z);

}	/* namespace Zinflate */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zinflate;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// zInflate
