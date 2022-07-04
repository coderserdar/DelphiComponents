// Borland C++ Builder
// Copyright (c) 1995, 1998 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ExtBackup.pas' rev: 3.00

#ifndef ExtBackupHPP
#define ExtBackupHPP
#include <DBTables.hpp>
#include <Db.hpp>
#include <MyBackup.hpp>
#include <CopyFile.hpp>
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

namespace Extbackup
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TExtBackup;
class PASCALIMPLEMENTATION TExtBackup : public Mybackup::TMyBackup 
{
	typedef Mybackup::TMyBackup inherited;
	
protected:
	Classes::TList* FClosedDatasets;
	bool FAutoCloseDatasets;
	bool FAutoReopenDatasets;
	Dbtables::TSession* FSession;
	int __fastcall GetClosedDatasetCount(void);
	Db::TDataSet* __fastcall GetClosedDatasets(int idx);
	
public:
	__fastcall virtual TExtBackup(Classes::TComponent* AOwner);
	__fastcall virtual ~TExtBackup(void);
	virtual void __fastcall Start(void);
	virtual void __fastcall Finish(void);
	void __fastcall ClearClosedDatasetsList(void);
	void __fastcall CloseSession(Dbtables::TSession* sess);
	void __fastcall CloseDatabase(Dbtables::TDatabase* aDatabase);
	void __fastcall CloseDataset(Db::TDataSet* aDataset);
	void __fastcall ReopenDatasets(void);
	System::AnsiString __fastcall GetAliasPath(const System::AnsiString alias);
	__property int ClosedDatasetCount = {read=GetClosedDatasetCount, nodefault};
	__property Db::TDataSet* ClosedDatasets[int idx] = {read=GetClosedDatasets};
	
__published:
	__property bool AutoCloseDatasets = {read=FAutoCloseDatasets, write=FAutoCloseDatasets, nodefault};
		
	__property bool AutoReopenDatasets = {read=FAutoReopenDatasets, write=FAutoReopenDatasets, nodefault
		};
	__property Dbtables::TSession* ASession = {read=FSession, write=FSession};
};

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Extbackup */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Extbackup;
#endif
//-- end unit ----------------------------------------------------------------
#endif	// ExtBackup
