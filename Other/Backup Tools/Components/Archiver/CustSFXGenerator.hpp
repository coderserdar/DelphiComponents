// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustSFXGenerator.pas' rev: 3.00

#ifndef CustSFXGeneratorHPP
#define CustSFXGeneratorHPP
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Custsfxgenerator
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCustomSFXGenerator;
class PASCALIMPLEMENTATION TCustomSFXGenerator : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	TLanguage FLanguage;
	int FNewTagInfoSize;
	int FNewSFXCodeSize;
	int FCurrentTagInfoSize;
	int FCurrentSFXCodeSize;
	void __fastcall SetLanguage(Archiverroot::TLanguage val);
	virtual void __fastcall UpdateLanguage(void);
	
public:
	__fastcall virtual TCustomSFXGenerator(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomSFXGenerator(void);
	virtual void __fastcall WriteSFXCodeToStream(Classes::TStream* S);
	virtual void __fastcall UpdateTagInfos(Classes::TStream* S);
	void __fastcall ExtractSizeInfosFromFile(const System::AnsiString FileName);
	void __fastcall DefineSizeFromFile(const System::AnsiString FileName);
	__property int NewTagInfoSize = {read=FNewTagInfoSize, write=FNewTagInfoSize, nodefault};
	__property int NewSFXCodeSize = {read=FNewSFXCodeSize, write=FNewSFXCodeSize, nodefault};
	__property int CurrentTagInfoSize = {read=FCurrentTagInfoSize, write=FCurrentTagInfoSize, nodefault
		};
	__property int CurrentSFXCodeSize = {read=FCurrentSFXCodeSize, write=FCurrentSFXCodeSize, nodefault
		};
	
__published:
	__property Archiverroot::TLanguage Language = {read=FLanguage, write=SetLanguage, nodefault};
};

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Custsfxgenerator */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Custsfxgenerator;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// CustSFXGenerator
