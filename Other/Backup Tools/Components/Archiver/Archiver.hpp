// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Archiver.pas' rev: 3.00

#ifndef ArchiverHPP
#define ArchiverHPP
#include <CustExtractor.hpp>
#include <CustArchiver.hpp>
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <Cryptcon.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Archiver
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TArchiver;
class PASCALIMPLEMENTATION TArchiver : public Custarchiver::TCustomArchiver 
{
	typedef Custarchiver::TCustomArchiver inherited;
	
protected:
	Cryptcon::TCrypto* FCryptoObject;
	virtual void __fastcall InitCrypting(void);
	virtual void __fastcall CryptBlock(char * DestBlock, char * SrcBlock, int &DestSize, int SrcSize);
	virtual void __fastcall DecryptBlock(char * DestBlock, char * SrcBlock, int &DestSize, int SrcSize)
		;
	virtual int __fastcall NeededBlockSize(void);
	virtual bool __fastcall CompressBlock(char * DestBlock, int &DestSize, char * SrcBlock, int SrcSize
		);
	virtual bool __fastcall UncompressBlock(char * DestBlock, int &DestSize, char * SrcBlock, int SrcSize
		);
	virtual bool __fastcall SelectDirectory(System::AnsiString &Directory, Archiverroot::TMySelectDirOpts 
		Options, int HelpCtx);
	virtual bool __fastcall SelectFile(const System::AnsiString aTitle, System::AnsiString &aFileName);
		
	virtual int __fastcall CompressionLevelAsInteger(void);
public:
	/* TCustomArchiver.Create */ __fastcall virtual TArchiver(Classes::TComponent* AOwner) : Custarchiver::
		TCustomArchiver(AOwner) { }
	
public:
	/* TArchiverRoot.Destroy */ __fastcall virtual ~TArchiver(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Archiver */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Archiver;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// Archiver
