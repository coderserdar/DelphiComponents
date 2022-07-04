// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Extractor.pas' rev: 3.00

#ifndef ExtractorHPP
#define ExtractorHPP
#include <CustExtractor.hpp>
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <Cryptcon.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Extractor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TExtractor;
class PASCALIMPLEMENTATION TExtractor : public Custextractor::TCustomExtractor 
{
	typedef Custextractor::TCustomExtractor inherited;
	
protected:
	Cryptcon::TCrypto* FCryptoObject;
	virtual void __fastcall InitCrypting(void);
	virtual void __fastcall DecryptBlock(char * DestBlock, char * SrcBlock, int &DestSize, int SrcSize)
		;
	virtual int __fastcall NeededBlockSize(void);
	virtual bool __fastcall UncompressBlock(char * DestBlock, int &DestSize, char * SrcBlock, int SrcSize
		);
public:
	/* TCustomExtractor.Create */ __fastcall virtual TExtractor(Classes::TComponent* AOwner) : Custextractor::
		TCustomExtractor(AOwner) { }
	
public:
	/* TArchiverRoot.Destroy */ __fastcall virtual ~TExtractor(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Extractor */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Extractor;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Extractor
