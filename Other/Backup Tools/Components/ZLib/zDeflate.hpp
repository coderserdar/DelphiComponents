// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'zDeflate.pas' rev: 3.00

#ifndef zDeflateHPP
#define zDeflateHPP
#include <BZlib.hpp>
#include <ZUtil.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Zdeflate
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::AnsiString deflate_copyright;
extern PACKAGE int __fastcall deflateInit2(Bzlib::z_stream &strm, int level, int method, int windowBits
	, int memLevel, int strategy);
extern PACKAGE int __fastcall deflateInit_(Bzlib::z_streamp strm, int level, const System::AnsiString 
	version, int stream_size);
extern PACKAGE int __fastcall deflateInit(Bzlib::z_stream &strm, int level);
extern PACKAGE int __fastcall deflateSetDictionary(Bzlib::z_stream &strm, Zutil::pBytef dictionary, 
	Cardinal dictLength);
extern PACKAGE int __fastcall deflateReset(Bzlib::z_stream &strm);
extern PACKAGE int __fastcall deflateParams(Bzlib::z_stream &strm, int level, int strategy);
extern PACKAGE int __fastcall deflate(Bzlib::z_stream &strm, int flush);
extern PACKAGE int __fastcall deflateEnd(Bzlib::z_stream &strm);
extern PACKAGE int __fastcall deflateCopy(Bzlib::z_stream &dest, Bzlib::z_stream &source);

}	/* namespace Zdeflate */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Zdeflate;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// zDeflate
