// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DsDdaEditors.pas' rev: 6.00

#ifndef DsDdaEditorsHPP
#define DsDdaEditorsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DsDdaTable.hpp>	// Pascal unit
#include <DsDatabase.hpp>	// Pascal unit
#include <DsDbUtils.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dsddaeditors
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDsDdaTableNameProperty;
class PASCALIMPLEMENTATION TDsDdaTableNameProperty : public Designeditors::TStringProperty 
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TDsDdaTableNameProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDsDdaTableNameProperty(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dsddaeditors */
using namespace Dsddaeditors;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DsDdaEditors
