// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ArchiverRoot.pas' rev: 3.00

#ifndef ArchiverRootHPP
#define ArchiverRootHPP
#include <ArchiverMisc.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Archiverroot
{
//-- type declarations -------------------------------------------------------
#pragma pack(push, 1)
struct TUserData
{
	System::SmallString<20>  UserName;
	System::SmallString<20>  Company;
	System::SmallString<20>  SerialNumber;
	System::SmallString<20>  BackupName;
	System::TDateTime Date;
	int ProductId;
	int ProductVersion;
	Byte Free[32];
} ;
#pragma pack(pop)

typedef TUserData *PUserData;

typedef Extended TArchiveSize;

typedef int TFileSize;

#pragma pack(push, 1)
struct TDataInfo
{
	int FileCount;
	Extended Size;
	Extended CompressedSize;
} ;
#pragma pack(pop)

enum ArchiverRoot__1 { afCrypted, afCompressed, afSolid, afReadOnly, afFinalSegment };

typedef Set<ArchiverRoot__1, afCrypted, afFinalSegment>  TArchiveFlag;

#pragma pack(push, 1)
struct TArchiveHeader
{
	int Signature;
	int Version;
	int RandomID;
	int BlockSize;
	int EndOffset;
	Word Segment;
	TArchiveFlag ArchiveFlag;
	TDataInfo ArchiveInfo;
	TDataInfo SegmentInfo;
	TUserData UserData;
	Byte Reserved[64];
	System::AnsiString Comment;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TFileInfo
{
	int Size;
	int CompressedSize;
} ;
#pragma pack(pop)

enum ArchiverRoot__2 { ffFile, ffEmptyFolder, ffFinalSegment, ffCrypted, ffPatch };

typedef Set<ArchiverRoot__2, ffFile, ffPatch>  TFileFlag;

#pragma pack(push, 1)
struct TFileEntry
{
	System::AnsiString Name;
	System::TDateTime Date;
	int Attr;
	Word Segment;
	int Offset;
	int FileOffset;
	TFileFlag FileFlag;
	TFileInfo ArchiveInfo;
	TFileInfo SegmentInfo;
} ;
#pragma pack(pop)

typedef TFileEntry *PFileEntry;

class DELPHICLASS TFileObject;
class PASCALIMPLEMENTATION TFileObject : public System::TObject 
{
	typedef System::TObject inherited;
	
public:
	TFileEntry FileEntry;
	int DirectoryIndex;
	int ImageIndex;
	int StateIndex;
	int Tag;
public:
	/* TObject.Create */ __fastcall TFileObject(void) : System::TObject() { }
	/* TObject.Destroy */ __fastcall virtual ~TFileObject(void) { }
	
};

enum TErrorAction { eaContinue, eaAbort, eaAsk };

enum TOperation { opNone, opAdd, opExtract, opEnumerate, opDelete, opMakeSFX, opCheck };

enum TInternalOperationEnum { ioCompressingStream, ioUncompressingStream, ioSkippingStream, ioSwappingSegment, 
	ioOpening, ioClosing, ioOpenSolid, ioCloseSolid, ioEnumAfterOpen };

typedef Set<TInternalOperationEnum, ioCompressingStream, ioEnumAfterOpen>  TInternalOperation;

enum TOption { oStoreEmptyFolders, oShowEmptyFolders, oCreateReadOnly, oCreateSolidArchives, oCompress, 
	oCrypt, oEraseFirstDisk, oEraseNewDisk, oConfirmFileDeletion, oEnumerateAfterOpen, oIncludeStartingDirectory, 
	oRecurseFolders, oOpenSingleSegment, oRestorePath, oSecureAccess, oWriteSFXCode, oEncryptFiles, oMaintainFileDirectory, 
	oNoSpanning, oShowBusyMessage, oDoNotUpdateFirstSegment };

typedef Set<TOption, oStoreEmptyFolders, oDoNotUpdateFirstSegment>  TOptions;

enum TLanguage { lgAutomatic, lgEnglish, lgFrench, lgChinese, lgChineseGB, lgPortuguese, lgGerman, lgItalian, 
	lgRussian, lgSpanish, lgDanish, lgDutch, lgCzech };

enum TMySelectDirOpt { sdAllowCreate, sdPerformCreate, sdPrompt };

typedef Set<TMySelectDirOpt, sdAllowCreate, sdPrompt>  TMySelectDirOpts;

typedef void __fastcall (__closure *TOnFileProgressEvent)(System::TObject* Sender, int Percent);

typedef void __fastcall (__closure *TOnErrorEvent)(System::TObject* Sender, Sysutils::Exception* E, 
	const TFileEntry &FileEntry, TErrorAction &ErrorAction);

typedef void __fastcall (__closure *TOnAcceptArchiveEvent)(System::TObject* Sender, const TArchiveHeader 
	&Header, bool &Accept);

typedef void __fastcall (__closure *TOnWriteUserDataEvent)(System::TObject* Sender, TUserData &UserData
	);

typedef void __fastcall (__closure *TOnEnterCryptKeyEvent)(System::TObject* Sender, System::AnsiString 
	&Key);

typedef void __fastcall (__closure *TOnRequestCryptKeyEvent)(System::TObject* Sender, System::AnsiString 
	&Key);

typedef void __fastcall (__closure *TOnGetSignatureEvent)(System::TObject* Sender, int &Signature);

typedef void __fastcall (__closure *TOnShowCommentEvent)(System::TObject* Sender, const System::AnsiString 
	Comment);

typedef void __fastcall (__closure *TOnShowTimingEvent)(System::TObject* Sender, System::TDateTime ElapsedTime
	, System::TDateTime RemainingTime);

typedef void __fastcall (__closure *TOnDisplayMessageEvent)(System::TObject* Sender, const System::AnsiString 
	msg);

typedef void __fastcall (__closure *TOnAddToLogEvent)(System::TObject* Sender, const System::AnsiString 
	msg);

class DELPHICLASS EArchiver;
class PASCALIMPLEMENTATION EArchiver : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiver(const System::AnsiString Msg) : Sysutils::Exception(Msg
		) { }
	/* Exception.CreateFmt */ __fastcall EArchiver(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiver(int Ident, Extended Dummy) : Sysutils::Exception(Ident
		, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiver(int Ident, const System::TVarRec * Args, const int 
		Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiver(const System::AnsiString Msg, int AHelpContext) : Sysutils::
		Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiver(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiver(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiver(int Ident, const System::TVarRec * Args, const 
		int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiver(void) { }
	
};

class DELPHICLASS EArchiverBusy;
class PASCALIMPLEMENTATION EArchiverBusy : public Archiverroot::EArchiver 
{
	typedef Archiverroot::EArchiver inherited;
	
public:
	/* Exception.Create */ __fastcall EArchiverBusy(const System::AnsiString Msg) : Archiverroot::EArchiver(
		Msg) { }
	/* Exception.CreateFmt */ __fastcall EArchiverBusy(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Archiverroot::EArchiver(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ __fastcall EArchiverBusy(int Ident, Extended Dummy) : Archiverroot::EArchiver(
		Ident, Dummy) { }
	/* Exception.CreateResFmt */ __fastcall EArchiverBusy(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Archiverroot::EArchiver(Ident, Args, Args_Size) { }
	/* Exception.CreateHelp */ __fastcall EArchiverBusy(const System::AnsiString Msg, int AHelpContext)
		 : Archiverroot::EArchiver(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ __fastcall EArchiverBusy(const System::AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Msg, Args, Args_Size, AHelpContext
		) { }
	/* Exception.CreateResHelp */ __fastcall EArchiverBusy(int Ident, int AHelpContext) : Archiverroot::
		EArchiver(Ident, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ __fastcall EArchiverBusy(int Ident, const System::TVarRec * Args, 
		const int Args_Size, int AHelpContext) : Archiverroot::EArchiver(Ident, Args, Args_Size, AHelpContext
		) { }
	
public:
	/* TObject.Destroy */ __fastcall virtual ~EArchiverBusy(void) { }
	
};

class DELPHICLASS TMessages;
class PASCALIMPLEMENTATION TMessages : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
protected:
	TLanguage FLanguage;
	System::AnsiString FBadSignature;
	System::AnsiString FFileNameNeeded;
	System::AnsiString FSystemMessage;
	System::AnsiString FAcceptArchiveFailed;
	System::AnsiString FEnterCryptKey;
	System::AnsiString FEnterDecryptKey;
	System::AnsiString FKeyTooShort;
	System::AnsiString FConfirmCryptKey;
	System::AnsiString FKeyNotConfirmed;
	System::AnsiString FArchiveIsReadOnly;
	System::AnsiString FCanNotCreateArchive;
	System::AnsiString FCannotCreateDir;
	System::AnsiString FSelectADirectory;
	System::AnsiString FOk;
	System::AnsiString FCancel;
	System::AnsiString FInformation;
	System::AnsiString FWarning;
	System::AnsiString FConfirmation;
	System::AnsiString FError;
	System::AnsiString FCanNotBuildTempFileName;
	System::AnsiString FYes;
	System::AnsiString FYesToAll;
	System::AnsiString FNo;
	System::AnsiString FFile;
	System::AnsiString FCanContinue;
	System::AnsiString FUnknownVersion;
	System::AnsiString FCannotCreateFile;
	System::AnsiString FCannotOpenFile;
	System::AnsiString FArchiverBusy;
	System::AnsiString FCantUseSolidArchiveWithExternalStream;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	void __fastcall PropSetLanguage(TLanguage language);
	virtual void __fastcall SetLanguage(TLanguage language);
	virtual void __fastcall SetGlobalStrings(void);
	
public:
	__fastcall TMessages(void);
	__property TLanguage Language = {read=FLanguage, write=PropSetLanguage, nodefault};
	
__published:
	__property System::AnsiString BadSignature = {read=FBadSignature, write=FBadSignature};
	__property System::AnsiString FileNameNeeded = {read=FFileNameNeeded, write=FFileNameNeeded};
	__property System::AnsiString SystemMessage = {read=FSystemMessage, write=FSystemMessage};
	__property System::AnsiString AcceptArchiveFailed = {read=FAcceptArchiveFailed, write=FAcceptArchiveFailed
		};
	__property System::AnsiString EnterCryptKey = {read=FEnterCryptKey, write=FEnterCryptKey};
	__property System::AnsiString EnterDecryptKey = {read=FEnterDecryptKey, write=FEnterDecryptKey};
	__property System::AnsiString KeyTooShort = {read=FKeyTooShort, write=FKeyTooShort};
	__property System::AnsiString ConfirmCryptKey = {read=FConfirmCryptKey, write=FConfirmCryptKey};
	__property System::AnsiString KeyNotConfirmed = {read=FKeyNotConfirmed, write=FKeyNotConfirmed};
	__property System::AnsiString ArchiveIsReadOnly = {read=FArchiveIsReadOnly, write=FArchiveIsReadOnly
		};
	__property System::AnsiString CanNotCreateArchive = {read=FCanNotCreateArchive, write=FCanNotCreateArchive
		};
	__property System::AnsiString CannotCreateDir = {read=FCannotCreateDir, write=FCannotCreateDir};
	__property System::AnsiString SelectADirectory = {read=FSelectADirectory, write=FSelectADirectory};
		
	__property System::AnsiString Ok = {read=FOk, write=FOk};
	__property System::AnsiString Cancel = {read=FCancel, write=FCancel};
	__property System::AnsiString Information = {read=FInformation, write=FInformation};
	__property System::AnsiString Warning = {read=FWarning, write=FWarning};
	__property System::AnsiString Confirmation = {read=FConfirmation, write=FConfirmation};
	__property System::AnsiString Error = {read=FError, write=FError};
	__property System::AnsiString CanNotBuildTempFileName = {read=FCanNotBuildTempFileName, write=FCanNotBuildTempFileName
		};
	__property System::AnsiString Yes = {read=FYes, write=FYes};
	__property System::AnsiString YesToAll = {read=FYesToAll, write=FYesToAll};
	__property System::AnsiString No = {read=FNo, write=FNo};
	__property System::AnsiString AFile = {read=FFile, write=FFile};
	__property System::AnsiString CanContinue = {read=FCanContinue, write=FCanContinue};
	__property System::AnsiString UnknownVersion = {read=FUnknownVersion, write=FUnknownVersion};
	__property System::AnsiString CannotCreateFile = {read=FCannotCreateFile, write=FCannotCreateFile};
		
	__property System::AnsiString CannotOpenFile = {read=FCannotOpenFile, write=FCannotOpenFile};
	__property System::AnsiString ArchiverBusy = {read=FArchiverBusy, write=FArchiverBusy};
	__property System::AnsiString CantUseSolidArchiveWithExternalStream = {read=FCantUseSolidArchiveWithExternalStream
		, write=FCantUseSolidArchiveWithExternalStream};
public:
	/* TPersistent.Destroy */ __fastcall virtual ~TMessages(void) { }
	
};

class DELPHICLASS TArchiverRoot;
class PASCALIMPLEMENTATION TArchiverRoot : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	TArchiveHeader FHeader;
	System::AnsiString FFileName;
	Classes::TStream* FStream;
	System::AnsiString FFilter;
	Extended FBytesToProcess;
	Extended FBytesProcessed;
	int FPercent;
	int FStartCount;
	int FBlockSize;
	char *FSrcBlock;
	char *FDestBlock;
	TErrorAction FErrorAction;
	TFileEntry FCurrentFileEntry;
	int FCurrentFileIdx;
	TOperation FOperation;
	TMessages* FMessages;
	int FMaxSegmentSize;
	bool FCheckAvailableSpace;
	TInternalOperation FInternalOperation;
	System::AnsiString FArchiveDrive;
	System::AnsiString FArchiveName;
	System::AnsiString FArchiveDir;
	System::AnsiString FArchiveExt;
	Extended FCompressedArchiveSize;
	bool FIsOpen;
	System::AnsiString FCryptKey;
	bool FReadOnly;
	int FStartOffset;
	TOptions FOptions;
	System::AnsiString FOldFileName;
	TOptions FOldOptions;
	int FOldMaxSegmentSize;
	int FSegmentNeeded;
	int FSFXCodeSize;
	bool FIsSolidArchive;
	System::TDateTime FTmpFileDate;
	bool FAlwaysContinue;
	Classes::TList* FFiles;
	bool FArchiveChanged;
	bool FMustAbort;
	bool FExternalStream;
	System::TDateTime FStartTime;
	System::TDateTime FEndTime;
	Extended FBytesPerMSec;
	int FLastTicks;
	int FTotTicks;
	TOnFileProgressEvent FOnFileProgress;
	Classes::TNotifyEvent FOnStartOperation;
	Classes::TNotifyEvent FOnFinishOperation;
	TOnErrorEvent FOnError;
	TOnAcceptArchiveEvent FOnAcceptArchive;
	TOnWriteUserDataEvent FOnWriteUserData;
	TOnEnterCryptKeyEvent FOnEnterCryptKey;
	TOnRequestCryptKeyEvent FOnRequestCryptKey;
	Classes::TNotifyEvent FOnBeforeOpen;
	Classes::TNotifyEvent FOnAfterOpen;
	Classes::TNotifyEvent FOnBeforeClose;
	Classes::TNotifyEvent FOnAfterClose;
	TOnGetSignatureEvent FOnGetSignature;
	Classes::TNotifyEvent FOnAfterHeaderUpdate;
	TOnShowCommentEvent FOnShowComment;
	TOnShowTimingEvent FOnShowTiming;
	TOnDisplayMessageEvent FOnDisplayMessage;
	Classes::TNotifyEvent FOnClearFileList;
	TOnAddToLogEvent FOnAddToLog;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	virtual TMessages* __fastcall CreateMessages(void);
	virtual int __fastcall GetSignature(void);
	virtual int __fastcall GetHeaderSize(void);
	virtual void __fastcall WriteHeader(void);
	void __fastcall ReadHeader(void);
	bool __fastcall ReadHeaderOfFile(const System::AnsiString fileName, TArchiveHeader &AHeader);
	virtual bool __fastcall ReadHeaderOfStream(Classes::TStream* S, TArchiveHeader &AHeader);
	void __fastcall CheckOpen(void);
	int __fastcall GetDirectorySize(const System::AnsiString dir, const System::AnsiString filter);
	virtual void __fastcall Start(void);
	virtual void __fastcall Finish(void);
	void __fastcall UpdateProgress(void);
	virtual void __fastcall InitCompression(void);
	virtual void __fastcall InitCrypting(void);
	virtual void __fastcall EnterCryptKey(void);
	virtual void __fastcall RequestCryptKey(void);
	virtual int __fastcall GetMinKeySize(void);
	void __fastcall SetBlockSize(int val);
	void __fastcall AllocBlocks(void);
	void __fastcall DeallocBlocks(void);
	bool __fastcall CanContinue(Sysutils::Exception* E);
	virtual int __fastcall NeededBlockSize(void);
	TLanguage __fastcall GetLanguage(void);
	void __fastcall SetLanguage(TLanguage val);
	void __fastcall SetMessages(TMessages* val);
	void __fastcall SetFileName(const System::AnsiString val);
	void __fastcall ExplodeFileName(void);
	int __fastcall GetFileEntrySize(const TFileEntry &fileEntry);
	void __fastcall WriteFileEntry(TFileEntry &fileEntry);
	void __fastcall ReadFileEntry(TFileEntry &fileEntry);
	bool __fastcall IsRemovableDisk(const System::AnsiString drive);
	virtual System::AnsiString __fastcall GetSegmentName(Word segment);
	Classes::TStream* __fastcall NewStreamObject(const System::AnsiString FileName, Word mode);
	void __fastcall CreateStream(void);
	void __fastcall OpenStream(void);
	void __fastcall CloseStream(void);
	void __fastcall CheckReadOnly(void);
	void __fastcall CheckKey(void);
	virtual void __fastcall BeforeOpen(void);
	virtual void __fastcall AfterOpen(void);
	virtual void __fastcall BeforeClose(void);
	virtual void __fastcall AfterClose(void);
	virtual void __fastcall AfterUpdate(void);
	System::AnsiString __fastcall GetTempFileName();
	virtual void __fastcall GetProgressInformations(void);
	virtual void __fastcall CreateArchive(void);
	virtual int __fastcall GetOpenMode(void);
	virtual bool __fastcall RequestSpace(int val);
	virtual bool __fastcall CheckEOF(void);
	virtual bool __fastcall SelectDirectory(System::AnsiString &Directory, TMySelectDirOpts Options, int 
		HelpCtx);
	void __fastcall ForceDirectories(System::AnsiString Dir);
	virtual bool __fastcall SelectFile(const System::AnsiString Title, System::AnsiString &FileName);
	virtual int __fastcall MessageDlg(const System::AnsiString Msg, Archivermisc::TMyMsgDlgType DlgType
		, Archivermisc::TMyMsgDlgButtons Buttons, int HelpCtx);
	virtual bool __fastcall InputQuery(const System::AnsiString ACaption, const System::AnsiString APrompt
		, System::AnsiString &AValue);
	virtual bool __fastcall QueryPassword(const System::AnsiString ACaption, const System::AnsiString APrompt
		, System::AnsiString &AValue);
	int __fastcall GetStartOffset(void);
	virtual void __fastcall CheckSFX(const System::AnsiString aFileName);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall OpenSolidData(void);
	virtual void __fastcall CloseSolidData(void);
	void __fastcall StartTimer(void);
	void __fastcall StopTimer(void);
	void __fastcall ShowTiming(void);
	System::TDateTime __fastcall GetElapsedTime(void);
	void __fastcall DisplayMessage(const System::AnsiString msg);
	void __fastcall AddToLog(const System::AnsiString msg);
	void __fastcall CopyStream(Classes::TStream* Src, Classes::TStream* Dest, bool trapExceptions);
	bool __fastcall CopyFile(const System::AnsiString srcName, const System::AnsiString destName, bool 
		failIfExists, bool trapExceptions);
	void __fastcall ClearFiles(void);
	void __fastcall AddFileToList(const TFileEntry &entry);
	TFileObject* __fastcall GetFiles(int idx);
	int __fastcall GetFileCount(void);
	void __fastcall AdjustArchiveSize(void);
	bool __fastcall CheckBusy(void);
	void __fastcall SetStream(Classes::TStream* val);
	
public:
	__fastcall virtual TArchiverRoot(Classes::TComponent* AOwner);
	__fastcall virtual ~TArchiverRoot(void);
	void __fastcall Open(void);
	void __fastcall OpenNew(void);
	void __fastcall CreateTempFile(void);
	void __fastcall Close(void);
	bool __fastcall Reset(void);
	bool __fastcall Delete(void);
	bool __fastcall Rename(const System::AnsiString NewName);
	bool __fastcall IsStreamOpen(void);
	bool __fastcall DeleteDirectory(const System::AnsiString dir);
	bool __fastcall DeleteDriveContent(const System::AnsiString drive);
	bool __fastcall IsSegmented(void);
	bool __fastcall IsEmpty(void);
	bool __fastcall IsBusy(void);
	int __fastcall IndexOfFile(const System::AnsiString FileName);
	bool __fastcall CanAbort(void);
	void __fastcall RequestAbort(void);
	__property System::AnsiString ArchiveDrive = {read=FArchiveDrive};
	__property System::AnsiString ArchiveName = {read=FArchiveName};
	__property System::AnsiString ArchiveDir = {read=FArchiveDir};
	__property System::AnsiString ArchiveExt = {read=FArchiveExt};
	__property bool CheckAvailableSpace = {read=FCheckAvailableSpace, write=FCheckAvailableSpace, nodefault
		};
	__property TFileEntry CurrentFileEntry = {read=FCurrentFileEntry};
	__property TOperation Operation = {read=FOperation, nodefault};
	__property Classes::TStream* Stream = {read=FStream, write=SetStream};
	__property TArchiveHeader Header = {read=FHeader};
	__property bool IsOpen = {read=FIsOpen, write=FIsOpen, nodefault};
	__property bool ReadOnly = {read=FReadOnly, nodefault};
	__property int StartOffset = {read=FStartOffset, write=FStartOffset, nodefault};
	__property int SFXCodeSize = {read=FSFXCodeSize, write=FSFXCodeSize, nodefault};
	__property bool IsSolidArchive = {read=FIsSolidArchive, nodefault};
	__property System::TDateTime ElapsedTime = {read=GetElapsedTime};
	__property System::TDateTime StartTime = {read=FStartTime};
	__property System::TDateTime EndTime = {read=FEndTime};
	__property Extended BytesPerMSec = {read=FBytesPerMSec};
	__property Extended BytesToProcess = {read=FBytesToProcess};
	__property Extended BytesProcessed = {read=FBytesProcessed};
	__property int Percent = {read=FPercent, nodefault};
	__property int FileCount = {read=GetFileCount, nodefault};
	__property TFileObject* Files[int idx] = {read=GetFiles};
	__property bool ArchiveChanged = {read=FArchiveChanged, nodefault};
	__property int MinKeySize = {read=GetMinKeySize, nodefault};
	__property bool ExternalStream = {read=FExternalStream, nodefault};
	
__published:
	__property int BlockSize = {read=FBlockSize, write=SetBlockSize, nodefault};
	__property TErrorAction ErrorAction = {read=FErrorAction, write=FErrorAction, nodefault};
	__property System::AnsiString FileName = {read=FFileName, write=SetFileName};
	__property System::AnsiString Filter = {read=FFilter, write=FFilter};
	__property TLanguage Language = {read=GetLanguage, write=SetLanguage, nodefault};
	__property TMessages* Messages = {read=FMessages, write=SetMessages};
	__property TOptions Options = {read=FOptions, write=FOptions, nodefault};
	__property TOnAcceptArchiveEvent OnAcceptArchive = {read=FOnAcceptArchive, write=FOnAcceptArchive};
		
	__property TOnWriteUserDataEvent OnWriteUserData = {read=FOnWriteUserData, write=FOnWriteUserData};
		
	__property TOnErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TOnFileProgressEvent OnFileProgress = {read=FOnFileProgress, write=FOnFileProgress};
	__property Classes::TNotifyEvent OnFinishOperation = {read=FOnFinishOperation, write=FOnFinishOperation
		};
	__property Classes::TNotifyEvent OnStartOperation = {read=FOnStartOperation, write=FOnStartOperation
		};
	__property TOnEnterCryptKeyEvent OnEnterCryptKey = {read=FOnEnterCryptKey, write=FOnEnterCryptKey};
		
	__property TOnRequestCryptKeyEvent OnRequestCryptKey = {read=FOnRequestCryptKey, write=FOnRequestCryptKey
		};
	__property Classes::TNotifyEvent OnBeforeOpen = {read=FOnBeforeOpen, write=FOnBeforeOpen};
	__property Classes::TNotifyEvent OnAfterOpen = {read=FOnAfterOpen, write=FOnAfterOpen};
	__property Classes::TNotifyEvent OnBeforeClose = {read=FOnBeforeClose, write=FOnBeforeClose};
	__property Classes::TNotifyEvent OnAfterClose = {read=FOnAfterClose, write=FOnAfterClose};
	__property TOnGetSignatureEvent OnGetSignature = {read=FOnGetSignature, write=FOnGetSignature};
	__property Classes::TNotifyEvent OnAfterHeaderUpdate = {read=FOnAfterHeaderUpdate, write=FOnAfterHeaderUpdate
		};
	__property TOnShowCommentEvent OnShowComment = {read=FOnShowComment, write=FOnShowComment};
	__property TOnShowTimingEvent OnShowTiming = {read=FOnShowTiming, write=FOnShowTiming};
	__property TOnDisplayMessageEvent OnDisplayMessage = {read=FOnDisplayMessage, write=FOnDisplayMessage
		};
	__property Classes::TNotifyEvent OnClearFileList = {read=FOnClearFileList, write=FOnClearFileList};
		
	__property TOnAddToLogEvent OnAddToLog = {read=FOnAddToLog, write=FOnAddToLog};
};

//-- var, const, procedure ---------------------------------------------------
#define kVersion (Byte)(1)
#define kMaxCryptBuffer (Byte)(8)
#define kMinKeySize (Byte)(10)
#define kDefaultExt ".mmm"
extern PACKAGE TLanguage __fastcall GetUserLanguage(void);

}	/* namespace Archiverroot */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Archiverroot;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// ArchiverRoot
