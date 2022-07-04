// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CustArchiver.pas' rev: 3.00

#ifndef CustArchiverHPP
#define CustArchiverHPP
#include <CustExtractor.hpp>
#include <ArchiverRoot.hpp>
#include <ArchiverMisc.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Custarchiver
{
//-- type declarations -------------------------------------------------------
enum TPathStorage { psNone, psWhole, psRelative };

enum TCompressionLevel { clMaximum, clNormal, clFast, clSuperFast, clNone };

enum TAddMode { amAdd, amAddAndReplace, amUpdate, amFreshen };

typedef void __fastcall (__closure *TOnAddFileEvent)(System::TObject* Sender, Archiverroot::TFileEntry 
	&FileEntry, bool &Accept);

typedef void __fastcall (__closure *TOnFileAddedEvent)(System::TObject* Sender, const Archiverroot::TFileEntry 
	&FileEntry);

typedef bool __fastcall (__closure *TOnCompressBlockEvent)(System::TObject* Sender, char * DestBlock
	, int &DestSize, char * SrcBlock, int SrcSize, TCompressionLevel Level);

typedef void __fastcall (__closure *TOnDeleteFileEvent)(System::TObject* Sender, const Archiverroot::TFileEntry 
	&FileEntry, bool &Accept);

typedef void __fastcall (__closure *TOnDeleteFileByIndexEvent)(System::TObject* Sender, int Index, bool 
	&Accept);

typedef void __fastcall (__closure *TOnCryptBlockEvent)(System::TObject* Sender, char * DestBlock, char * 
	SrcBlock, int &DestSize, int SrcSize);

typedef void __fastcall (__closure *TOnNeedNewDiskEvent)(System::TObject* Sender, int Segment, System::AnsiString 
	&Drive);

typedef void __fastcall (__closure *TOnNeedNewFolderEvent)(System::TObject* Sender, int Segment, System::AnsiString 
	&Path);

typedef void __fastcall (__closure *TOnClearDiskEvent)(System::TObject* Sender, const System::AnsiString 
	Drive);

typedef void __fastcall (__closure *TOnWriteSFXCodeEvent)(System::TObject* Sender, Classes::TStream* 
	Stream);

class DELPHICLASS EArchiverCompress;
class PASCALIMPLEMENTATION EArchiverCompress : public Archiverroot::EArchiver 
{
	typedef Archiverroot::EArchiver inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiverCompress(const System::AnsiString Msg) : Archiverroot::EArchiver(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall EArchiverCompress(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Archiverroot::EArchiver(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiverCompress(int Ident, Extended Dummy) : Archiverroot::EArchiver(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiverCompress(int Ident, const System::TVarRec * Args, 
		const int Args_Size) : Archiverroot::EArchiver(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiverCompress(const System::AnsiString Msg, int AHelpContext
		) : Archiverroot::EArchiver(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiverCompress(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiverCompress(int Ident, int AHelpContext) : Archiverroot::
		EArchiver(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiverCompress(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiverCompress(void) { }
	
};

class DELPHICLASS TArchMessages;
class PASCALIMPLEMENTATION TArchMessages : public Custextractor::TExtrMessages 
{
	typedef Custextractor::TExtrMessages inherited;
	
protected:
	System::AnsiString FCouldNotCompressBlock;
	System::AnsiString FDeleteFileEventNeeded;
	System::AnsiString FCouldNotRenameArchive;
	System::AnsiString FInsertNewDisk;
	System::AnsiString FSelectNewPlace;
	System::AnsiString FNotEnoughFreeSpaceOn;
	System::AnsiString FUnableToDeleteFiles;
	System::AnsiString FConfirmFileDeletion;
	System::AnsiString FMaxSegmentSizeTooSmall;
	System::AnsiString FCantPerformThisOp;
	System::AnsiString FAddingFile;
	System::AnsiString FDeletingFiles;
	System::AnsiString FDeleteFile;
	System::AnsiString FMakingSFXArchive;
	System::AnsiString FCompressingSolidArchive;
	System::AnsiString FCopyingArchive;
	System::AnsiString FCouldNotCopyArchive;
	System::AnsiString FUpdatingFile;
	System::AnsiString FReplacingFile;
	System::AnsiString FCantDeleteInExtStr;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	
public:
	virtual void __fastcall SetLanguage(Archiverroot::TLanguage language);
	
__published:
	__property System::AnsiString CouldNotCompressBlock = {read=FCouldNotCompressBlock, write=FCouldNotCompressBlock
		};
	__property System::AnsiString DeleteFileEventNeeded = {read=FDeleteFileEventNeeded, write=FDeleteFileEventNeeded
		};
	__property System::AnsiString CouldNotRenameArchive = {read=FCouldNotRenameArchive, write=FCouldNotRenameArchive
		};
	__property System::AnsiString InsertNewDisk = {read=FInsertNewDisk, write=FInsertNewDisk};
	__property System::AnsiString SelectNewPlace = {read=FSelectNewPlace, write=FSelectNewPlace};
	__property System::AnsiString NotEnoughFreeSpaceOn = {read=FNotEnoughFreeSpaceOn, write=FNotEnoughFreeSpaceOn
		};
	__property System::AnsiString UnableToDeleteFiles = {read=FUnableToDeleteFiles, write=FUnableToDeleteFiles
		};
	__property System::AnsiString ConfirmFileDeletion = {read=FConfirmFileDeletion, write=FConfirmFileDeletion
		};
	__property System::AnsiString MaxSegmentSizeTooSmall = {read=FMaxSegmentSizeTooSmall, write=FMaxSegmentSizeTooSmall
		};
	__property System::AnsiString CantPerformThisOp = {read=FCantPerformThisOp, write=FCantPerformThisOp
		};
	__property System::AnsiString AddingFile = {read=FAddingFile, write=FAddingFile};
	__property System::AnsiString DeletingFiles = {read=FDeletingFiles, write=FDeletingFiles};
	__property System::AnsiString DeleteFile = {read=FDeleteFile, write=FDeleteFile};
	__property System::AnsiString MakingSFXArchive = {read=FMakingSFXArchive, write=FMakingSFXArchive};
		
	__property System::AnsiString CompressingSolidArchive = {read=FCompressingSolidArchive, write=FCompressingSolidArchive
		};
	__property System::AnsiString CopyingArchive = {read=FCopyingArchive, write=FCopyingArchive};
	__property System::AnsiString CouldNotCopyArchive = {read=FCouldNotCopyArchive, write=FCouldNotCopyArchive
		};
	__property System::AnsiString UpdatingFile = {read=FUpdatingFile, write=FUpdatingFile};
	__property System::AnsiString ReplacingFile = {read=FReplacingFile, write=FReplacingFile};
	__property System::AnsiString CantDeleteInExtStr = {read=FCantDeleteInExtStr, write=FCantDeleteInExtStr
		};
public:
	/* TMessages.Create */ __fastcall TArchMessages(void) : Custextractor::TExtrMessages() { }
	
public:
	/* TPersistent.Destroy */ __fastcall virtual ~TArchMessages(void) { }
	
};

class DELPHICLASS TCustomArchiver;
class PASCALIMPLEMENTATION TCustomArchiver : public Custextractor::TCustomExtractor 
{
	typedef Custextractor::TCustomExtractor inherited;
	
protected:
	TPathStorage FPathStorage;
	System::AnsiString FRelativePath;
	int FMinFreeSpace;
	TCompressionLevel FCompressionLevel;
	int FReserveSpace;
	TAddMode FAddMode;
	TOnAddFileEvent FOnAddFile;
	TOnFileAddedEvent FOnFileAdded;
	TOnCompressBlockEvent FOnCompressBlock;
	TOnDeleteFileEvent FOnDeleteFile;
	TOnDeleteFileByIndexEvent FOnDeleteFileByIndex;
	TOnCryptBlockEvent FOnCryptBlock;
	TOnNeedNewDiskEvent FOnNeedNewDisk;
	TOnNeedNewFolderEvent FOnNeedNewFolder;
	TOnClearDiskEvent FOnClearDisk;
	TOnWriteSFXCodeEvent FOnWriteSFXCode;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual Archiverroot::TMessages* __fastcall CreateMessages(void);
	HIDESBASE TArchMessages* __fastcall GetMessages(void);
	HIDESBASE void __fastcall SetMessages(TArchMessages* val);
	void __fastcall CompressStream(Classes::TStream* src);
	virtual bool __fastcall CompressBlock(char * DestBlock, int &DestSize, char * SrcBlock, int SrcSize
		);
	virtual void __fastcall CryptBlock(char * DestBlock, char * SrcBlock, int &DestSize, int SrcSize);
	void __fastcall SetMaxSegmentSize(int val);
	virtual bool __fastcall RequestSpace(int val);
	bool __fastcall IsValidDrive(const System::AnsiString drive);
	bool __fastcall CanUseDrive(const System::AnsiString drive);
	void __fastcall AskNewDisk(void);
	void __fastcall NextSegment(void);
	void __fastcall CreateSegment(void);
	virtual void __fastcall CreateArchive(void);
	int __fastcall GetFreeSpace(const System::AnsiString drive);
	virtual int __fastcall CompressionLevelAsInteger(void);
	virtual void __fastcall WriteSFXCode(Classes::TStream* S);
	virtual void __fastcall AfterUpdate(void);
	System::AnsiString __fastcall CompressSolidData();
	virtual void __fastcall CloseSolidData(void);
	virtual int __fastcall GetOpenMode(void);
	void __fastcall UpdateArchiveSize(void);
	virtual void __fastcall BeforeClose(void);
	bool __fastcall DoAddFile(const System::AnsiString FileName);
	bool __fastcall DoAddFiles(Classes::TStrings* files);
	bool __fastcall DoAddDirectory(const System::AnsiString Directory);
	void __fastcall DoDeleteFiles(void);
	bool __fastcall DoMakeSFX(void);
	void __fastcall DoSetArchiveComment(const System::AnsiString comment);
	
public:
	__fastcall virtual TCustomArchiver(Classes::TComponent* AOwner);
	bool __fastcall AddFile(const System::AnsiString FileName);
	bool __fastcall AddFiles(Classes::TStrings* files);
	bool __fastcall AddDirectory(const System::AnsiString Directory);
	void __fastcall DeleteFiles(void);
	bool __fastcall MakeSFX(void);
	void __fastcall SetArchiveComment(const System::AnsiString comment);
	void __fastcall EraseDrive(const System::AnsiString drive);
	__property int ReserveSpace = {read=FReserveSpace, write=FReserveSpace, nodefault};
	
__published:
	__property TAddMode AddMode = {read=FAddMode, write=FAddMode, nodefault};
	__property TCompressionLevel CompressionLevel = {read=FCompressionLevel, write=FCompressionLevel, nodefault
		};
	__property int MaxSegmentSize = {read=GetMaxSegmentSize, write=SetMaxSegmentSize, nodefault};
	__property TArchMessages* Messages = {read=GetMessages, write=SetMessages};
	__property int MinFreeSpace = {read=FMinFreeSpace, write=FMinFreeSpace, nodefault};
	__property TPathStorage PathStorage = {read=FPathStorage, write=FPathStorage, nodefault};
	__property TOnAddFileEvent OnAddFile = {read=FOnAddFile, write=FOnAddFile};
	__property TOnCompressBlockEvent OnCompressBlock = {read=FOnCompressBlock, write=FOnCompressBlock};
		
	__property TOnDeleteFileEvent OnDeleteFile = {read=FOnDeleteFile, write=FOnDeleteFile};
	__property TOnDeleteFileByIndexEvent OnDeleteFileByIndex = {read=FOnDeleteFileByIndex, write=FOnDeleteFileByIndex
		};
	__property TOnFileAddedEvent OnFileAdded = {read=FOnFileAdded, write=FOnFileAdded};
	__property TOnCryptBlockEvent OnCryptBlock = {read=FOnCryptBlock, write=FOnCryptBlock};
	__property TOnNeedNewDiskEvent OnNeedNewDisk = {read=FOnNeedNewDisk, write=FOnNeedNewDisk};
	__property TOnNeedNewFolderEvent OnNeedNewFolder = {read=FOnNeedNewFolder, write=FOnNeedNewFolder};
		
	__property TOnClearDiskEvent OnClearDisk = {read=FOnClearDisk, write=FOnClearDisk};
	__property TOnWriteSFXCodeEvent OnWriteSFXCode = {read=FOnWriteSFXCode, write=FOnWriteSFXCode};
public:
		
	/* TArchiverRoot.Destroy */ __fastcall virtual ~TCustomArchiver(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Custarchiver */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Custarchiver;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// CustArchiver
