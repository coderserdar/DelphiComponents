// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CopyFile.pas' rev: 3.00

#ifndef CopyFileHPP
#define CopyFileHPP
#include <prgform.hpp>
#include <FileCtrl.hpp>
#include <ExtCtrls.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Copyfile
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOperationProgressEvent)(System::TObject* Sender, int processed, 
	bool &Cancel);

typedef void __fastcall (__closure *TEachFileEvent)(System::TObject* Sender, const System::AnsiString 
	FileName);

enum TProgressKind { pkFile, pkDirectory };

class DELPHICLASS TCustomCopyFile;
class PASCALIMPLEMENTATION TCustomCopyFile : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	System::AnsiString FCopyFrom;
	System::AnsiString FCopyTo;
	bool FShowProgress;
	int FProcessed;
	System::AnsiString FOnNotExists;
	bool FShowFileNames;
	System::AnsiString FCaption;
	bool FMoveFile;
	bool FTransferTimeDate;
	int FStartCount;
	Classes::TNotifyEvent FOnStartOperation;
	Classes::TNotifyEvent FOnFinishOperation;
	TOperationProgressEvent FOnOperationProgress;
	TEachFileEvent FOnEachFile;
	bool FCopyMultipleFiles;
	Prgform::TProgForm* FProgressForm;
	bool FTransferFileAttributes;
	bool FRecursive;
	bool FSendEvents;
	bool FCancelOperation;
	TProgressKind FProgressKind;
	int FBytesToCopy;
	int FBytesCopied;
	System::AnsiString FFilter;
	virtual void __fastcall PrecalcDirSize(const System::AnsiString dir);
	bool __fastcall GetIsWorking(void);
	void __fastcall BringToFront(void);
	
public:
	__fastcall virtual TCustomCopyFile(Classes::TComponent* AOWner);
	__fastcall virtual ~TCustomCopyFile(void);
	void __fastcall CopyNow(void);
	virtual void __fastcall Start(void);
	virtual void __fastcall Finish(void);
	void __fastcall WriteFileName(const System::AnsiString filename);
	void __fastcall SetProgress(int progress);
	void __fastcall SetCaption(const System::AnsiString str);
	System::AnsiString __fastcall AppendSlash(const System::AnsiString sDir);
	System::AnsiString __fastcall RemoveSlash(const System::AnsiString sDir);
	bool __fastcall CopyDirectory(const System::AnsiString from_dir, const System::AnsiString to_dir);
	int __fastcall GetDirectorySize(const System::AnsiString dir);
	int __fastcall GetDirectoryCount(const System::AnsiString dir);
	int __fastcall GetFileSize(const System::AnsiString fileName);
	bool __fastcall DeleteDirectory(const System::AnsiString dir);
	bool __fastcall IsDirectoryEmpty(const System::AnsiString dir);
	bool __fastcall IsDirectoryInUse(const System::AnsiString dir);
	bool __fastcall FindFile(const System::AnsiString FileName, const System::AnsiString DirectoryStart
		);
	void __fastcall CopyFilesWithJoker(const System::AnsiString FileName, const System::AnsiString DestDirectory
		);
	void __fastcall CopyFiles(Classes::TStrings* AList, const System::AnsiString DestDirectory);
	bool __fastcall DiskInDrive(char Drive);
	__property System::AnsiString CopyFrom = {read=FCopyFrom, write=FCopyFrom};
	__property System::AnsiString CopyTo = {read=FCopyTo, write=FCopyTo};
	__property System::AnsiString Filter = {read=FFilter, write=FFilter};
	__property bool IsWorking = {read=GetIsWorking, nodefault};
	__property bool Progress = {read=FShowProgress, write=FShowProgress, nodefault};
	__property System::AnsiString OnNotExists = {read=FOnNotExists, write=FOnNotExists};
	__property bool ShowFileNames = {read=FShowFileNames, write=FShowFileNames, nodefault};
	__property System::AnsiString Caption = {read=FCaption, write=FCaption};
	__property bool Movefile = {read=FMoveFile, write=FMoveFile, nodefault};
	__property int Processed = {read=FProcessed, nodefault};
	__property bool TransferTimeDate = {read=FTransferTimeDate, write=FTransferTimeDate, nodefault};
	__property bool TransferFileAttributes = {read=FTransferFileAttributes, write=FTransferFileAttributes
		, default=1};
	__property bool Recursive = {read=FRecursive, write=FRecursive, default=1};
	__property bool SendEvents = {read=FSendEvents, write=FSendEvents, default=1};
	__property TProgressKind ProgressKind = {read=FProgressKind, write=FProgressKind, default=1};
	__property bool CancelOperation = {read=FCancelOperation, write=FCancelOperation, nodefault};
	__property Classes::TNotifyEvent OnStartOperation = {read=FOnStartOperation, write=FOnStartOperation
		};
	__property Classes::TNotifyEvent OnFinishOperation = {read=FOnFinishOperation, write=FOnFinishOperation
		};
	__property TOperationProgressEvent OnOperationProgress = {read=FOnOperationProgress, write=FOnOperationProgress
		};
	__property TEachFileEvent OnEachFile = {read=FOnEachFile, write=FOnEachFile};
};

class DELPHICLASS TCopyFile;
class PASCALIMPLEMENTATION TCopyFile : public Copyfile::TCustomCopyFile 
{
	typedef Copyfile::TCustomCopyFile inherited;
	
__published:
	__property CopyFrom ;
	__property CopyTo ;
	__property Filter ;
	__property Progress ;
	__property OnNotExists ;
	__property ShowFileNames ;
	__property Caption ;
	__property Movefile ;
	__property Processed ;
	__property TransferTimeDate ;
	__property TransferFileAttributes ;
	__property Recursive ;
	__property SendEvents ;
	__property ProgressKind ;
	__property OnStartOperation ;
	__property OnFinishOperation ;
	__property OnOperationProgress ;
	__property OnEachFile ;
public:
	/* TCustomCopyFile.Create */ __fastcall virtual TCopyFile(Classes::TComponent* AOWner) : Copyfile::
		TCustomCopyFile(AOWner) { }
	/* TCustomCopyFile.Destroy */ __fastcall virtual ~TCopyFile(void) { }
	
};

//-- var, const, procedure ---------------------------------------------------
#define sDirectoryDoesNotExist "Le répertoire \"%s\" n'existe pas"
extern PACKAGE void __fastcall Register(void);

}	/* namespace Copyfile */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Copyfile;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// CopyFile
