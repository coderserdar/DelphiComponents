// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OQBEbde.pas' rev: 4.00

#ifndef OQBEbdeHPP
#define OQBEbdeHPP

#pragma delphiheader begin
#pragma option push -w-
#include <DBTables.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit
#include "QBuilder.hpp"	// Pascal unit
//-- user supplied -----------------------------------------------------------

namespace Oqbebde
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TOQBEngineBDE;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBEngineBDE : public Qbuilder::TOQBEngine 
{
	typedef Qbuilder::TOQBEngine inherited;
	
private:
	Dbtables::TQuery* FResultQuery;
	
public:
	__fastcall virtual TOQBEngineBDE(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBEngineBDE(void);
	virtual void __fastcall SetDatabaseName(const AnsiString Value);
	virtual bool __fastcall SelectDatabase(void);
	virtual void __fastcall ReadTableList(void);
	virtual void __fastcall ReadFieldList(AnsiString ATableName);
	virtual void __fastcall ClearQuerySQL(void);
	virtual void __fastcall SetQuerySQL(AnsiString Value);
	virtual Db::TDataSet* __fastcall ResultQuery(void);
	virtual void __fastcall OpenResultQuery(void);
	virtual void __fastcall CloseResultQuery(void);
	virtual void __fastcall SaveResultQueryData(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::ResourceString _resSaveResFilter;
#define Oqbebde_resSaveResFilter System::LoadResourceString(&Oqbebde::_resSaveResFilter)

}	/* namespace Oqbebde */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Oqbebde;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OQBEbde
