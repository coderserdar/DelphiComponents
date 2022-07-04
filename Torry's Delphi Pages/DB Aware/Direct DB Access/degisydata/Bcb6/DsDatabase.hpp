// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDatabase.pas' rev: 6.00

#ifndef DsDatabaseHPP
#define DsDatabaseHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDbUtils.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
#include <DsConsts.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsdatabase
{
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TDatabaseLoginEvent)(System::TObject* Sender, Classes::TStrings* LoginParams);

class DELPHICLASS TDsDatabase;
class PASCALIMPLEMENTATION TDsDatabase : public Db::TCustomConnection 
{
	typedef Db::TCustomConnection inherited;
	
private:
	AnsiString FDatabaseName;
	Dsdbapi::TDsDbHandle* FHandle;
	bool FReadOnly;
	bool FExclusive;
	TDatabaseLoginEvent FOnLogin;
	void __fastcall SetDatabaseName(const AnsiString Value);
	void __fastcall SetExclusive(const bool Value);
	void __fastcall SetReadOnly(const bool Value);
	
public:
	__fastcall virtual TDsDatabase(Classes::TComponent* AOwner);
	__fastcall virtual ~TDsDatabase(void);
	HIDESBASE void __fastcall Open(void)/* overload */;
	HIDESBASE void __fastcall Close(void);
	void __fastcall CloseDataSets(void);
	virtual void __fastcall DoConnect(void);
	virtual void __fastcall DoDisconnect(void);
	void __fastcall CheckDatabaseName(void);
	virtual bool __fastcall GetConnected(void);
	__property Dsdbapi::TDsDbHandle* Handle = {read=FHandle, write=FHandle};
	
__published:
	__property Connected  = {default=0};
	__property AnsiString DatabaseName = {read=FDatabaseName, write=SetDatabaseName};
	__property bool Exclusive = {read=FExclusive, write=SetExclusive, default=0};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, default=0};
	__property AfterConnect ;
	__property AfterDisconnect ;
	__property BeforeConnect ;
	__property BeforeDisconnect ;
	__property TDatabaseLoginEvent OnLogin = {read=FOnLogin, write=FOnLogin};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dsdatabase */
using namespace Dsdatabase;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDatabase
