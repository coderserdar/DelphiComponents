// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MyBackup.pas' rev: 3.00

#ifndef MyBackupHPP
#define MyBackupHPP
#include <Classes.hpp>
#include <CopyFile.hpp>
#include <Dialogs.hpp>
#include <Forms.hpp>
#include <Controls.hpp>
#include <Graphics.hpp>
#include <SysUtils.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mybackup
{
//-- type declarations -------------------------------------------------------
enum TOperation { opBackup, opRestore, opNone };

enum TLanguage { lgEnglish, lgFrench, lgChinese, lgChineseGB, lgPortuguese, lgGerman, lgItalian, lgSpanish, 
	lgDutch, lgCzech };

typedef void __fastcall (__closure *TOnExtractFileEvent)(System::TObject* Sender, const System::AnsiString 
	FileName, int Size);

typedef void __fastcall (__closure *TOnAddFileEvent)(System::TObject* Sender, const System::AnsiString 
	FileName, int Size);

typedef void __fastcall (__closure *TOnFileExtractedEvent)(System::TObject* Sender, const System::AnsiString 
	FileName, const System::AnsiString DestPath, int Size);

typedef void __fastcall (__closure *TOnFileProgress)(System::TObject* Sender, int percent);

class DELPHICLASS TArchiverInterface;
class PASCALIMPLEMENTATION TArchiverInterface : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
protected:
	TOnExtractFileEvent FOnExtractFile;
	TOnAddFileEvent FOnAddFile;
	TOnFileExtractedEvent FOnFileExtracted;
	TOnFileProgress FOnFileProgress;
	virtual void __fastcall SetFileName(const System::AnsiString aFileName);
	virtual System::AnsiString __fastcall GetFileName();
	virtual void __fastcall SetExtractPath(const System::AnsiString aPath);
	virtual System::AnsiString __fastcall GetExtractPath();
	void __fastcall SetLanguage(TLanguage lang);
	TLanguage __fastcall GetLanguage(void);
	
public:
	virtual void __fastcall Open(void);
	virtual void __fastcall Close(void);
	virtual bool __fastcall AddFile(const System::AnsiString aFileName);
	virtual bool __fastcall AddFiles(Classes::TStrings* files);
	virtual bool __fastcall AddDirectory(const System::AnsiString Directory);
	virtual void __fastcall ExtractFiles(void);
	virtual void __fastcall SetRecursive(bool val);
	virtual void __fastcall Delete(void);
	__property System::AnsiString FileName = {read=GetFileName, write=SetFileName};
	__property System::AnsiString ExtractPath = {read=GetExtractPath, write=SetExtractPath};
	__property TLanguage Language = {read=GetLanguage, write=SetLanguage, nodefault};
	__property TOnExtractFileEvent OnExtractFile = {read=FOnExtractFile, write=FOnExtractFile};
	__property TOnAddFileEvent OnAddFile = {read=FOnAddFile, write=FOnAddFile};
	__property TOnFileExtractedEvent OnFileExtracted = {read=FOnFileExtracted, write=FOnFileExtracted};
		
	__property TOnFileProgress OnFileProgress = {read=FOnFileProgress, write=FOnFileProgress};
public:
	/* TComponent.Create */ __fastcall virtual TArchiverInterface(Classes::TComponent* AOwner) : Classes::
		TComponent(AOwner) { }
	/* TComponent.Destroy */ __fastcall virtual ~TArchiverInterface(void) { }
	
};

class DELPHICLASS TMessages;
class PASCALIMPLEMENTATION TMessages : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
protected:
	System::AnsiString FSystemMessage;
	System::AnsiString FNeedValidDrivePath;
	System::AnsiString FInsertDiskXInUnit;
	System::AnsiString FDeletingFilesOfDrivePath;
	System::AnsiString FAskForDeletionOfFiles;
	System::AnsiString FCouldNotDeleteAllFiles;
	System::AnsiString FThisDiskIsNotTheFirstOne;
	System::AnsiString FThePathDoesNotExist;
	System::AnsiString FCopyOf;
	System::AnsiString FCouldNotWriteInFile;
	System::AnsiString FThisDiskContainsNoBackup;
	System::AnsiString FWrongBackupSet;
	System::AnsiString FWrongBackupName;
	System::AnsiString FAborted;
	System::AnsiString FReopeningTables;
	System::AnsiString FCompressingFile;
	System::AnsiString FUncompressingFile;
	System::AnsiString FNeedBackupName;
	virtual void __fastcall AssignTo(Classes::TPersistent* Dest);
	
public:
	__fastcall TMessages(void);
	void __fastcall SetLanguage(TLanguage language);
	
__published:
	__property System::AnsiString SystemMessage = {read=FSystemMessage, write=FSystemMessage};
	__property System::AnsiString NeedValidDrivePath = {read=FNeedValidDrivePath, write=FNeedValidDrivePath
		};
	__property System::AnsiString InsertDiskXInUnit = {read=FInsertDiskXInUnit, write=FInsertDiskXInUnit
		};
	__property System::AnsiString DeletingFilesOfDrivePath = {read=FDeletingFilesOfDrivePath, write=FDeletingFilesOfDrivePath
		};
	__property System::AnsiString AskForDeletionOfFiles = {read=FAskForDeletionOfFiles, write=FAskForDeletionOfFiles
		};
	__property System::AnsiString CouldNotDeleteAllFiles = {read=FCouldNotDeleteAllFiles, write=FCouldNotDeleteAllFiles
		};
	__property System::AnsiString ThisDiskIsNotTheFirstOne = {read=FThisDiskIsNotTheFirstOne, write=FThisDiskIsNotTheFirstOne
		};
	__property System::AnsiString ThePathDoesNotExist = {read=FThePathDoesNotExist, write=FThePathDoesNotExist
		};
	__property System::AnsiString CopyOf = {read=FCopyOf, write=FCopyOf};
	__property System::AnsiString CouldNotWriteInFile = {read=FCouldNotWriteInFile, write=FCouldNotWriteInFile
		};
	__property System::AnsiString ThisDiskContainsNoBackup = {read=FThisDiskContainsNoBackup, write=FThisDiskContainsNoBackup
		};
	__property System::AnsiString WrongBackupSet = {read=FWrongBackupSet, write=FWrongBackupSet};
	__property System::AnsiString WrongBackupName = {read=FWrongBackupName, write=FWrongBackupName};
	__property System::AnsiString Aborted = {read=FAborted, write=FAborted};
	__property System::AnsiString ReopeningTables = {read=FReopeningTables, write=FReopeningTables};
	__property System::AnsiString CompressingFile = {read=FCompressingFile, write=FCompressingFile};
	__property System::AnsiString UncompressingFile = {read=FUncompressingFile, write=FUncompressingFile
		};
	__property System::AnsiString NeedBackupName = {read=FNeedBackupName, write=FNeedBackupName};
public:
		
	/* TPersistent.Destroy */ __fastcall virtual ~TMessages(void) { }
	
};

class DELPHICLASS TMyBackup;
class PASCALIMPLEMENTATION TMyBackup : public Copyfile::TCustomCopyFile 
{
	typedef Copyfile::TCustomCopyFile inherited;
	
protected:
	System::AnsiString FDrivePath;
	System::AnsiString FFilesPath;
	System::AnsiString FBackupCaption;
	System::AnsiString FRestoreCaption;
	bool FConfirmDelete;
	TOperation FOperation;
	System::AnsiString FVersion;
	System::AnsiString FID;
	System::AnsiString FIdLabel;
	System::AnsiString FBackupName;
	char FDrive;
	System::AnsiString FUserName;
	System::AnsiString FUserCompany;
	System::AnsiString FUserLicence;
	int FDiskCount;
	System::AnsiString FCurrentFile;
	System::AnsiString FInfosFileName;
	bool FOneMoreDisk;
	System::AnsiString FCurBackup;
	Classes::TStringList* FFilesToBackup;
	Classes::TStringList* FFilesRestored;
	TMessages* FMessages;
	TLanguage FLanguage;
	Classes::TNotifyEvent FOnDiskChanged;
	bool FDisplayAbort;
	bool FUseArchiver;
	TArchiverInterface* FArchiver;
	void __fastcall SetFilesToBackup(Classes::TStringList* val);
	void __fastcall SetFilesRestored(Classes::TStringList* val);
	void __fastcall CheckDrive(void);
	void __fastcall NextDisk(void);
	bool __fastcall CheckDiskContent(void);
	void __fastcall CheckPath(void);
	void __fastcall WriteFiles(void);
	void __fastcall WriteFile(const System::AnsiString FileName);
	void __fastcall WriteInfos(void);
	void __fastcall FileExtractedFromArchive(System::TObject* Sender, const System::AnsiString FileName
		, const System::AnsiString DestPath, int Size);
	void __fastcall ReadFiles(void);
	void __fastcall ReadFile(const System::AnsiString FileName);
	void __fastcall AskForDisk(void);
	virtual void __fastcall PrecalcDirSize(const System::AnsiString dir);
	void __fastcall SetLanguage(TLanguage lang);
	void __fastcall SetMessages(TMessages* msg);
	void __fastcall DoAbort(void);
	virtual TArchiverInterface* __fastcall CreateArchiver(void);
	TArchiverInterface* __fastcall GetArchiver(void);
	System::AnsiString __fastcall GetTempDir();
	void __fastcall AddFileEvent(System::TObject* Sender, const System::AnsiString FileName, int Size);
		
	void __fastcall ExtractFileEvent(System::TObject* Sender, const System::AnsiString FileName, int Size
		);
	void __fastcall FileProgressEvent(System::TObject* Sender, int Percent);
	bool __fastcall GetUseArchiver(void);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	
public:
	__fastcall virtual TMyBackup(Classes::TComponent* AOwner);
	__fastcall virtual ~TMyBackup(void);
	void __fastcall Backup(void);
	void __fastcall Restore(void);
	virtual void __fastcall Finish(void);
	bool __fastcall CheckFirstDisk(void);
	bool __fastcall GetInfos(Classes::TStringList* SL);
	
__published:
	__property System::AnsiString DrivePath = {read=FDrivePath, write=FDrivePath};
	__property System::AnsiString FilesPath = {read=FFilesPath, write=FFilesPath};
	__property Classes::TStringList* FilesToBackup = {read=FFilesToBackup, write=SetFilesToBackup};
	__property Classes::TStringList* FilesRestored = {read=FFilesRestored, write=SetFilesRestored};
	__property System::AnsiString BackupCaption = {read=FBackupCaption, write=FBackupCaption};
	__property System::AnsiString RestoreCaption = {read=FRestoreCaption, write=FRestoreCaption};
	__property bool ConfirmDelete = {read=FConfirmDelete, write=FConfirmDelete, default=1};
	__property System::AnsiString Version = {read=FVersion, write=FVersion};
	__property System::AnsiString ID = {read=FID, write=FID};
	__property System::AnsiString IdLabel = {read=FIdLabel, write=FIdLabel};
	__property System::AnsiString BackupName = {read=FBackupName, write=FBackupName};
	__property System::AnsiString UserName = {read=FUserName, write=FUserName};
	__property System::AnsiString UserCompany = {read=FUserCompany, write=FUserCompany};
	__property System::AnsiString UserLicence = {read=FUserLicence, write=FUserLicence};
	__property System::AnsiString InfosFileName = {read=FInfosFileName, write=FInfosFileName};
	__property TLanguage Language = {read=FLanguage, write=SetLanguage, nodefault};
	__property TMessages* Messages = {read=FMessages, write=SetMessages};
	__property bool DisplayAbort = {read=FDisplayAbort, write=FDisplayAbort, nodefault};
	__property bool UseArchiver = {read=GetUseArchiver, write=FUseArchiver, nodefault};
	__property TArchiverInterface* Archiver = {read=FArchiver, write=FArchiver};
	__property Classes::TNotifyEvent OnDiskChanged = {read=FOnDiskChanged, write=FOnDiskChanged};
	__property Filter ;
	__property Progress ;
	__property Recursive ;
	__property OnStartOperation ;
	__property OnFinishOperation ;
	__property OnOperationProgress ;
	__property OnEachFile ;
};

//-- var, const, procedure ---------------------------------------------------
#define kMinDiskSpace (Word)(8192)
#define kInfosFile "infos.txt"
extern PACKAGE void __fastcall Register(void);

}	/* namespace Mybackup */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Mybackup;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// MyBackup
