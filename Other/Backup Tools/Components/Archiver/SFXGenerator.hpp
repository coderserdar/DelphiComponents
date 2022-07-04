// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SFXGenerator.pas' rev: 3.00

#ifndef SFXGeneratorHPP
#define SFXGeneratorHPP
#include <CustSFXGenerator.hpp>
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sfxgenerator
{
//-- type declarations -------------------------------------------------------
typedef int DWord;

enum TOverwritemode { confirm, overwrite, skip, update, existing, updateexisting };

enum TCommentMode { none, Before, After, Both };

#pragma pack(push, 1)
struct TTagInfo
{
	bool ExecuteFileAfterExtract;
	bool UserChooseFilesToExtract;
	bool UserChooseOverwriteMode;
	bool UserAllowedToDontRunTheFile;
	TOverwritemode DefaultOwerwriteMode;
	int SFXFileSize;
	System::SmallString<80>  CommandLine;
	System::SmallString<60>  Caption;
	System::SmallString<80>  DefaultExtractPath;
	System::SmallString<80>  CopyrightLine;
	TLanguage Language;
	TCommentMode Comment;
} ;
#pragma pack(pop)

class DELPHICLASS TSFXGenerator;
class PASCALIMPLEMENTATION TSFXGenerator : public Custsfxgenerator::TCustomSFXGenerator 
{
	typedef Custsfxgenerator::TCustomSFXGenerator inherited;
	
protected:
	int __fastcall GetSFXCodeSize(void);
	virtual void __fastcall UpdateLanguage(void);
	
public:
	TTagInfo TagInfo;
	__fastcall virtual TSFXGenerator(Classes::TComponent* AOwner);
	__fastcall virtual ~TSFXGenerator(void);
	virtual void __fastcall WriteSFXCodeToStream(Classes::TStream* S);
	virtual void __fastcall UpdateTagInfos(Classes::TStream* S);
};

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Sfxgenerator */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Sfxgenerator;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// SFXGenerator
