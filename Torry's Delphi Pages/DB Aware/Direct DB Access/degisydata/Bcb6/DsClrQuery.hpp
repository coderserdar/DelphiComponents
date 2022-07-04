// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsClrQuery.pas' rev: 6.00

#ifndef DsClrQueryHPP
#define DsClrQueryHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsMemTable.hpp>	// Pascal unit
#include <DsSqlApi.hpp>	// Pascal unit
#include <DsClrTable.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <FMTBcd.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsclrquery
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDsClrQuery;
class PASCALIMPLEMENTATION TDsClrQuery : public Dssqlapi::TDsSqlQuery 
{
	typedef Dssqlapi::TDsSqlQuery inherited;
	
protected:
	DYNAMIC Db::TDataSet* __fastcall GetTable(Classes::TComponent* AOwner, AnsiString TableName);
public:
	#pragma option push -w-inl
	/* TDsSqlQuery.Create */ inline __fastcall virtual TDsClrQuery(Classes::TComponent* AOwner) : Dssqlapi::TDsSqlQuery(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDsSqlQuery.Destroy */ inline __fastcall virtual ~TDsClrQuery(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dsclrquery */
using namespace Dsclrquery;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsClrQuery
