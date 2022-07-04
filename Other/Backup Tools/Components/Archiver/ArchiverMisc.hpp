// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ArchiverMisc.pas' rev: 3.00

#ifndef ArchiverMiscHPP
#define ArchiverMiscHPP
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Messages.hpp>
#include <Windows.hpp>
#include <SysInit.hpp>
#include <System.hpp>

//-- user supplied -----------------------------------------------------------

namespace Archivermisc
{
//-- type declarations -------------------------------------------------------
enum TMyMsgDlgType { mtWarning, mtError, mtInformation, mtConfirmation, mtCustom };

enum TMyMsgDlgBtn { mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbHelp };

typedef Set<TMyMsgDlgBtn, mbYes, mbHelp>  TMyMsgDlgButtons;

//-- var, const, procedure ---------------------------------------------------
#define mrNone (Byte)(0)
#define mrOk (Byte)(1)
#define mrCancel (Byte)(2)
#define mrAbort (Byte)(3)
#define mrRetry (Byte)(4)
#define mrIgnore (Byte)(5)
#define mrYes (Byte)(6)
#define mrNo (Byte)(7)
#define mrAll (Byte)(8)
extern PACKAGE int MyModalResults[9];
extern PACKAGE System::AnsiString strOk;
extern PACKAGE System::AnsiString strCancel;
extern PACKAGE System::AnsiString strInformation;
extern PACKAGE System::AnsiString strWarning;
extern PACKAGE System::AnsiString strConfirmation;
extern PACKAGE System::AnsiString strError;
extern PACKAGE System::AnsiString strYes;
extern PACKAGE System::AnsiString strYesToAll;
extern PACKAGE System::AnsiString strNo;
extern PACKAGE System::AnsiString strReplaceFile;
extern PACKAGE System::AnsiString strWithFile;
extern PACKAGE System::AnsiString strConfirmFileOverwrite;
extern PACKAGE System::AnsiString strFile;
extern PACKAGE System::AnsiString strCanContinue;
extern PACKAGE int __fastcall MessageDlg(const System::AnsiString Msg, TMyMsgDlgType DlgType, TMyMsgDlgButtons 
	Buttons, int HelpCtx);
extern PACKAGE bool __fastcall InputQuery(const System::AnsiString ACaption, const System::AnsiString 
	APrompt, System::AnsiString &AValue);
extern PACKAGE bool __fastcall QueryPassword(const System::AnsiString ACaption, const System::AnsiString 
	APrompt, System::AnsiString &AValue);
extern PACKAGE int __fastcall QueryFileOverwrite(const System::AnsiString oldFileName, const System::AnsiString 
	newFileName, int oldFileSize, int newFileSize, System::TDateTime oldFileDate, System::TDateTime newFileDate
	);
extern PACKAGE int __fastcall QueryContinue(const System::AnsiString ErrorMsg, const System::AnsiString 
	FileName, int FileSize, System::TDateTime FileDate);
extern PACKAGE int __fastcall ReadInteger(Classes::TStream* S);
extern PACKAGE void __fastcall WriteInteger(Classes::TStream* S, int val);
extern PACKAGE Word __fastcall ReadWord(Classes::TStream* S);
extern PACKAGE void __fastcall WriteWord(Classes::TStream* S, Word val);
extern PACKAGE Extended __fastcall ReadFloat(Classes::TStream* S);
extern PACKAGE void __fastcall WriteFloat(Classes::TStream* S, Extended val);
extern PACKAGE bool __fastcall ReadBoolean(Classes::TStream* S);
extern PACKAGE void __fastcall WriteBoolean(Classes::TStream* S, bool val);
extern PACKAGE System::AnsiString __fastcall ReadString(Classes::TStream* S);
extern PACKAGE void __fastcall WriteString(Classes::TStream* S, System::AnsiString val);
extern PACKAGE System::AnsiString __fastcall RemoveSlash(const System::AnsiString sDir);
extern PACKAGE System::AnsiString __fastcall AppendSlash(const System::AnsiString sDir);
extern PACKAGE System::AnsiString __fastcall AdjustPath(const System::AnsiString path, int maxSize);
	
extern PACKAGE bool __fastcall DiskInDrive(char Drive);
extern PACKAGE int __fastcall CRC32R(int CRC, const void *Data, int cbData);
extern PACKAGE int __fastcall Min(int a, int b);
extern PACKAGE int __fastcall Max(int a, int b);
extern PACKAGE int __fastcall Abs(int val);
extern PACKAGE int __fastcall EncodeBlockSize(bool IsCompressed, int BlockSize);
extern PACKAGE void __fastcall DecodeBlockSize(int size, bool &IsCompressed, int &BlockSize);
extern PACKAGE int __fastcall CalcRatio(int size, int compressedSize);
extern PACKAGE bool __fastcall DirectoryExists(const System::AnsiString Name);
extern PACKAGE System::TDateTime __fastcall GetFileDate(const System::AnsiString FileName);
extern PACKAGE int __fastcall GetFileSize(const System::AnsiString fileName);
extern PACKAGE bool __fastcall IsExeFile(const System::AnsiString FileName);
extern PACKAGE System::AnsiString __fastcall GetTempDir();
extern PACKAGE void __fastcall GetVersionInfo(const System::AnsiString FileName, Classes::TStrings* 
	Infos);
extern PACKAGE System::TDateTime __fastcall MSecsAsTime(int secs);
extern PACKAGE int __fastcall TimeAsMSecs(System::TDateTime time);
extern PACKAGE void __fastcall GetDiskSizeAvail2(char * TheDrive, double &TotalBytes, double &TotalFree
	);
extern PACKAGE void __fastcall GetDiskSizeAvail(char * TheDrive, double &TotalBytes, double &TotalFree
	);
extern PACKAGE int __fastcall EnumerateDirectory(const System::AnsiString dir, const System::AnsiString 
	filter, bool recurseDir, Classes::TStrings* dest);
extern PACKAGE bool __fastcall GetVolumeInfo(const System::AnsiString drive, System::AnsiString &VolumeName
	, int &SerialNum);

}	/* namespace Archivermisc */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Archivermisc;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// ArchiverMisc
