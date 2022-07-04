// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsSqlApi.pas' rev: 6.00

#ifndef DsSqlApiHPP
#define DsSqlApiHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsSqlParser.hpp>	// Pascal unit
#include <DsMemTable.hpp>	// Pascal unit
#include <DsDbApi.hpp>	// Pascal unit
#include <DsDatabase.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <FMTBcd.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dssqlapi
{
//-- type declarations -------------------------------------------------------
struct TTableItem;
typedef TTableItem *pTableItem;

#pragma pack(push, 1)
struct TTableItem
{
	char Alias[32];
	Db::TDataSet* Table;
} ;
#pragma pack(pop)

class DELPHICLASS TDsSqlQuery;
class PASCALIMPLEMENTATION TDsSqlQuery : public Dsmemtable::TDsCustomMemTable 
{
	typedef Dsmemtable::TDsCustomMemTable inherited;
	
private:
	Dsdbapi::TDsDbHandle* FDbHandle;
	Classes::TStrings* FSql;
	Dsdatabase::TDsDatabase* FDatabase;
	bool FPrepared;
	Classes::TList* FTables;
	Dssqlparser::TSQLScriptParser* FSqlParser;
	void __fastcall SetSql(const Classes::TStrings* Value);
	void __fastcall SetDatabase(const Dsdatabase::TDsDatabase* Value);
	void __fastcall QueryChanged(System::TObject* Sender);
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall SetActive(bool Value);
	void __fastcall ClearTables(void);
	bool __fastcall OpenTables(void);
	void __fastcall CloseTables(void);
	void __fastcall AddTable(AnsiString Alias, Db::TDataSet* Table);
	Db::TDataSet* __fastcall FindTable(AnsiString Alias);
	DYNAMIC Db::TDataSet* __fastcall GetTable(Classes::TComponent* AOwner, AnsiString TableName);
	Dssqlparser::TSQLSchema __fastcall GetSqlSchema(void);
	
public:
	__fastcall virtual TDsSqlQuery(Classes::TComponent* AOwner);
	__fastcall virtual ~TDsSqlQuery(void);
	void __fastcall Prepare(void);
	void __fastcall UnPrepare(void);
	bool __fastcall IsSelect(void);
	bool __fastcall LoadTables(void);
	__property bool Prepared = {read=FPrepared, write=FPrepared, nodefault};
	
__published:
	__property Active  = {default=0};
	__property Dsdatabase::TDsDatabase* Database = {read=FDatabase, write=SetDatabase};
	__property Classes::TStrings* Sql = {read=FSql, write=SetSql};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dssqlapi */
using namespace Dssqlapi;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsSqlApi
