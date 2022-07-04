// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustExtractor.pas' rev: 3.00

#ifndef CustExtractorHPP
#define CustExtractorHPP
#include <Classes.hpp>
#include <CustSFXGenerator.hpp>
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Custextractor
{
//-- type declarations -------------------------------------------------------
enum TRestoreAction { raOverwrite, raSkip, raUpdate, raAsk, raExistingOnly, raUpdateExisting };

typedef void __fastcall (__closure *TOnEnumerationEvent)(System::TObject* Sender, const Archiverroot::TFileEntry 
	&FileEntry);

typedef void __fastcall (__closure *TOnExtractFileEvent)(System::TObject* Sender, const Archiverroot::TFileEntry 
	&FileEntry, System::AnsiString &DestPath, bool &Accept);

typedef void __fastcall (__closure *TOnExtractFileByIndexEvent)(System::TObject* Sender, int Index, 
	System::AnsiString &DestPath, bool &Accept);

typedef void __fastcall (__closure *TOnFileExtractedEvent)(System::TObject* Sender, const Archiverroot::TFileEntry 
	&FileEntry, const System::AnsiString DestPath);

typedef bool __fastcall (__closure *TOnUncompressBlockEvent)(System::TObject* Sender, char * DestBlock
	, int &DestSize, char * SrcBlock, int SrcSize);

typedef void __fastcall (__closure *TOnDecryptBlockEvent)(System::TObject* Sender, char * DestBlock, 
	char * SrcBlock, int &DestSize, int SrcSize);

typedef void __fastcall (__closure *TOnInsertDiskEvent)(System::TObject* Sender, int Segment, System::AnsiString 
	&Drive);

typedef void __fastcall (__closure *TOnInsertLastDiskEvent)(System::TObject* Sender, System::AnsiString 
	&Drive);

typedef void __fastcall (__closure *TOnLocateSegmentEvent)(System::TObject* Sender, int Segment, System::AnsiString 
	&FileName);

typedef void __fastcall (__closure *TOnLocateLastSegmentEvent)(System::TObject* Sender, System::AnsiString 
	&Path);

class DELPHICLASS EArchiverUncompress;
class PASCALIMPLEMENTATION EArchiverUncompress : public Archiverroot::EArchiver 
{
	typedef Archiverroot::EArchiver inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiverUncompress(const System::AnsiString Msg) : Archiverroot::
		EArchiver(Msg) { }
	/* Exception.CreateFmt */ __fastcall EArchiverUncompress(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Archiverroot::EArchiver(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiverUncompress(int Ident, Extended Dummy) : Archiverroot::
		EArchiver(Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiverUncompress(int Ident, const System::TVarRec * Args
		, const int Args_Size) : Archiverroot::EArchiver(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiverUncompress(const System::AnsiString Msg, int AHelpContext
		) : Archiverroot::EArchiver(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiverUncompress(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiverUncompress(int Ident, int AHelpContext) : Archiverroot::
		EArchiver(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiverUncompress(int Ident, const System::TVarRec * 
		Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiverUncompress(void) { }
	
};

class DELPHICLASS EArchiverBadCRC;
class PASCALIMPLEMENTATION EArchiverBadCRC : public Archiverroot::EArchiver 
{
	typedef Archiverroot::EArchiver inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiverBadCRC(const System::AnsiString Msg) : Archiverroot::EArchiver(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall EArchiverBadCRC(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Archiverroot::EArchiver(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiverBadCRC(int Ident, Extended Dummy) : Archiverroot::EArchiver(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiverBadCRC(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Archiverroot::EArchiver(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiverBadCRC(const System::AnsiString Msg, int AHelpContext
		) : Archiverroot::EArchiver(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiverBadCRC(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiverBadCRC(int Ident, int AHelpContext) : Archiverroot::
		EArchiver(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiverBadCRC(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiverBadCRC(void) { }
	
};

class DELPHICLASS EArchiverBadKey;
class PASCALIMPLEMENTATION EArchiverBadKey : public Archiverroot::EArchiver 
{
	typedef Archiverroot::EArchiver inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiverBadKey(const System::AnsiString Msg) : Archiverroot::EArchiver(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall EArchiverBadKey(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Archiverroot::EArchiver(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiverBadKey(int Ident, Extended Dummy) : Archiverroot::EArchiver(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiverBadKey(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Archiverroot::EArchiver(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiverBadKey(const System::AnsiString Msg, int AHelpContext
		) : Archiverroot::EArchiver(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiverBadKey(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiverBadKey(int Ident, int AHelpContext) : Archiverroot::
		EArchiver(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiverBadKey(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiverBadKey(void) { }
	
};

class DELPHICLASS TExtrMessages;
class PASCALIMPLEMENTATION TExtrMessages : public Archiverroot::TMessages 
{
	typedef Archiverroot::TMessages inherited;
	
protected:
	System::AnsiString FCouldNotUncompressBlock;
	System::AnsiString FAskOverwrite;
	System::AnsiString FNeedExtractPath;
	System::AnsiString FInsertDisk;
	System::AnsiString FLocateSegment;
	System::AnsiString FWrongSegment;
	System::AnsiString FInsertLastSegment;
	System::AnsiString FLocateLastSegment;
	System::AnsiString FWrongLastSegment;
	System::AnsiString FWrongNextSegment;
	System::AnsiString FBadCRC;
	System::AnsiString FBadKey;
	System::AnsiString FReplaceFile;
	System::AnsiString FWithFile;
	System::AnsiString FConfirmFileOverwrite;
	System::AnsiString FExtractingFile;
	System::AnsiString FLoadingArchiveContnent;
	System::AnsiString FUncompressingSolidArchive;
	System::AnsiString FCheckingFile;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual void __fastcall SetGlobalStrings(void);
	
public:
	virtual void __fastcall SetLanguage(Archiverroot::TLanguage language);
	
__published:
	__property System::AnsiString CouldNotUncompressBlock = {read=FCouldNotUncompressBlock, write=FCouldNotUncompressBlock
		};
	__property System::AnsiString AskOverwrite = {read=FAskOverwrite, write=FAskOverwrite};
	__property System::AnsiString NeedExtractPath = {read=FNeedExtractPath, write=FNeedExtractPath};
	__property System::AnsiString InsertDisk = {read=FInsertDisk, write=FInsertDisk};
	__property System::AnsiString LocateSegment = {read=FLocateSegment, write=FLocateSegment};
	__property System::AnsiString WrongSegment = {read=FWrongSegment, write=FWrongSegment};
	__property System::AnsiString InsertLastSegment = {read=FInsertLastSegment, write=FInsertLastSegment
		};
	__property System::AnsiString LocateLastSegment = {read=FLocateLastSegment, write=FLocateLastSegment
		};
	__property System::AnsiString WrongLastSegment = {read=FWrongLastSegment, write=FWrongLastSegment};
		
	__property System::AnsiString WrongNextSegment = {read=FWrongNextSegment, write=FWrongNextSegment};
		
	__property System::AnsiString BadCRC = {read=FBadCRC, write=FBadCRC};
	__property System::AnsiString BadKey = {read=FBadKey, write=FBadKey};
	__property System::AnsiString ReplaceFile = {read=FReplaceFile, write=FReplaceFile};
	__property System::AnsiString WithFile = {read=FWithFile, write=FWithFile};
	__property System::AnsiString ConfirmFileOverwrite = {read=FConfirmFileOverwrite, write=FConfirmFileOverwrite
		};
	__property System::AnsiString ExtractingFile = {read=FExtractingFile, write=FExtractingFile};
	__property System::AnsiString LoadingArchiveContnent = {read=FLoadingArchiveContnent, write=FLoadingArchiveContnent
		};
	__property System::AnsiString UncompressingSolidArchive = {read=FUncompressingSolidArchive, write=FUncompressingSolidArchive
		};
	__property System::AnsiString CheckingFile = {read=FCheckingFile, write=FCheckingFile};
public:
	/* TMessages.Create */ __fastcall TExtrMessages(void) : Archiverroot::TMessages() { }
	
public:
	/* TPersistent.Destroy */ __fastcall virtual ~TExtrMessages(void) { }
	
};

class DELPHICLASS TCustomExtractor;
class PASCALIMPLEMENTATION TCustomExtractor : public Archiverroot::TArchiverRoot 
{
	typedef Archiverroot::TArchiverRoot inherited;
	
protected:
	System::AnsiString FExtractPath;
	TRestoreAction FRestoreAction;
	Custsfxgenerator::TCustomSFXGenerator* FSFXGenerator;
	bool FAlwaysOverwrite;
	TOnEnumerationEvent FOnEnumeration;
	TOnExtractFileEvent FOnExtractFile;
	TOnExtractFileByIndexEvent FOnExtractFileByIndex;
	TOnFileExtractedEvent FOnFileExtracted;
	TOnUncompressBlockEvent FOnUncompressBlock;
	TOnDecryptBlockEvent FOnDecryptBlock;
	TOnInsertDiskEvent FOnInsertDisk;
	TOnInsertLastDiskEvent FOnInsertLastDisk;
	TOnLocateSegmentEvent FOnLocateSegment;
	TOnLocateLastSegmentEvent FOnLocateLastSegment;
	Classes::TNotifyEvent FOnSegmentChanged;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual Archiverroot::TMessages* __fastcall CreateMessages(void);
	TExtrMessages* __fastcall GetMessages(void);
	HIDESBASE void __fastcall SetMessages(TExtrMessages* val);
	void __fastcall UncompressStream(Classes::TStream* dest);
	virtual bool __fastcall UncompressBlock(char * DestBlock, int &DestSize, char * SrcBlock, int SrcSize
		);
	virtual void __fastcall DecryptBlock(char * DestBlock, char * SrcBlock, int &DestSize, int SrcSize)
		;
	void __fastcall SkipFile(int anOffset);
	bool __fastcall Eof(Classes::TStream* S);
	bool __fastcall SegmentBelongsToArchive(const System::AnsiString aFileName, Archiverroot::TArchiveHeader 
		&AHeader);
	void __fastcall OpenSegment(int val);
	void __fastcall CloseSegment(void);
	void __fastcall NeedFirstSegment(void);
	void __fastcall NeedLastSegment(void);
	virtual bool __fastcall CheckEOF(void);
	virtual void __fastcall GetProgressInformations(void);
	void __fastcall ExtractFileData(const Archiverroot::TFileEntry &fileEntry, const System::AnsiString 
		DestFileName);
	System::AnsiString __fastcall GetDestinationPath(const Archiverroot::TFileEntry &fileEntry);
	void __fastcall CheckCurrentFile(const Archiverroot::TFileEntry &last, const Archiverroot::TFileEntry 
		&new);
	virtual void __fastcall AfterOpen(void);
	virtual void __fastcall BeforeClose(void);
	virtual void __fastcall CheckSFX(const System::AnsiString aFileName);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	void __fastcall AdjustSolidOptions(void);
	virtual void __fastcall OpenSolidData(void);
	virtual void __fastcall CloseSolidData(void);
	virtual int __fastcall GetOpenMode(void);
	virtual void __fastcall Start(void);
	void __fastcall DoEnumerateFiles(void);
	void __fastcall DoExtractFile(Word aSegment, int anOffset, int compressedSize);
	void __fastcall DoExtractFileTo(Word aSegment, int anOffset, int compressedSize, const System::AnsiString 
		DestFileName);
	void __fastcall DoExtractFiles(void);
	void __fastcall DoCheckIntegrity(void);
	int __fastcall GetMaxSegmentSize(void);
	
public:
	__fastcall virtual TCustomExtractor(Classes::TComponent* AOwner);
	void __fastcall EnumerateFiles(void);
	void __fastcall ExtractFile(Word aSegment, int anOffset, int compressedSize);
	void __fastcall ExtractFileTo(Word aSegment, int anOffset, int compressedSize, const System::AnsiString 
		DestFileName);
	void __fastcall ExtractFiles(void);
	void __fastcall CheckIntegrity(void);
	
__published:
	__property System::AnsiString ExtractPath = {read=FExtractPath, write=FExtractPath};
	__property TExtrMessages* Messages = {read=GetMessages, write=SetMessages};
	__property TRestoreAction RestoreAction = {read=FRestoreAction, write=FRestoreAction, nodefault};
	__property Custsfxgenerator::TCustomSFXGenerator* SFXGenerator = {read=FSFXGenerator, write=FSFXGenerator
		};
	__property TOnEnumerationEvent OnEnumeration = {read=FOnEnumeration, write=FOnEnumeration};
	__property TOnExtractFileEvent OnExtractFile = {read=FOnExtractFile, write=FOnExtractFile};
	__property TOnExtractFileByIndexEvent OnExtractFileByIndex = {read=FOnExtractFileByIndex, write=FOnExtractFileByIndex
		};
	__property TOnFileExtractedEvent OnFileExtracted = {read=FOnFileExtracted, write=FOnFileExtracted};
		
	__property TOnUncompressBlockEvent OnUncompressBlock = {read=FOnUncompressBlock, write=FOnUncompressBlock
		};
	__property TOnDecryptBlockEvent OnDecryptBlock = {read=FOnDecryptBlock, write=FOnDecryptBlock};
	__property TOnInsertDiskEvent OnInsertDisk = {read=FOnInsertDisk, write=FOnInsertDisk};
	__property TOnInsertLastDiskEvent OnInsertLastDisk = {read=FOnInsertLastDisk, write=FOnInsertLastDisk
		};
	__property TOnLocateSegmentEvent OnLocateSegment = {read=FOnLocateSegment, write=FOnLocateSegment};
		
	__property TOnLocateLastSegmentEvent OnLocateLastSegment = {read=FOnLocateLastSegment, write=FOnLocateLastSegment
		};
	__property Classes::TNotifyEvent OnSegmentChanged = {read=FOnSegmentChanged, write=FOnSegmentChanged
		};
public:
	/* TArchiverRoot.Destroy */ __fastcall virtual ~TCustomExtractor(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Custextractor */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Custextractor;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// CustExtractor
