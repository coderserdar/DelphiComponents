// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GridEhEd.pas' rev: 6.00

#ifndef GridEhEdHPP
#define GridEhEdHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DesignWindows.hpp>	// Pascal unit
#include <ToolWnds.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <ToolWin.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <DBLookupEh.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <DesignIntf.hpp>	// Pascal unit
#include <DesignEditors.hpp>	// Pascal unit
#include <Variants.hpp>	// Pascal unit
#include <ColnEdit.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Gridehed
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDBGridEhColumnsEditor;
class PASCALIMPLEMENTATION TDBGridEhColumnsEditor : public Colnedit::TCollectionEditor 
{
	typedef Colnedit::TCollectionEditor inherited;
	
__published:
	Menus::TMenuItem* N1;
	Menus::TMenuItem* AddAllFields1;
	Menus::TMenuItem* RestoreDefaults1;
	Comctrls::TToolButton* ToolButton6;
	Comctrls::TToolButton* ToolButton7;
	Comctrls::TToolButton* ToolButton8;
	Actnlist::TAction* AddAllFieldsCmd;
	Actnlist::TAction* RestoreDefaultsCmd;
	void __fastcall AddAllFieldsCmdExecute(System::TObject* Sender);
	void __fastcall RestoreDefaultsCmdExecute(System::TObject* Sender);
	void __fastcall AddAllFieldsCmdUpdate(System::TObject* Sender);
	void __fastcall RestoreDefaultsCmdUpdate(System::TObject* Sender);
	
protected:
	virtual bool __fastcall CanAdd(int Index);
public:
	#pragma option push -w-inl
	/* TDesignWindow.Create */ inline __fastcall virtual TDBGridEhColumnsEditor(Classes::TComponent* AOwner) : Colnedit::TCollectionEditor(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDesignWindow.Destroy */ inline __fastcall virtual ~TDBGridEhColumnsEditor(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDBGridEhColumnsEditor(Classes::TComponent* AOwner, int Dummy) : Colnedit::TCollectionEditor(AOwner, Dummy) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBGridEhColumnsEditor(HWND ParentWindow) : Colnedit::TCollectionEditor(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhColumnsProperty;
class PASCALIMPLEMENTATION TDBGridEhColumnsProperty : public Designeditors::TPropertyEditor 
{
	typedef Designeditors::TPropertyEditor inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual AnsiString __fastcall GetValue();
	virtual void __fastcall Edit(void);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TDBGridEhColumnsProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TPropertyEditor(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDBGridEhColumnsProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhEditor;
class PASCALIMPLEMENTATION TDBGridEhEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TDBGridEhEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDBGridEhEditor(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhFieldProperty;
class PASCALIMPLEMENTATION TDBGridEhFieldProperty : public Designeditors::TStringProperty 
{
	typedef Designeditors::TStringProperty inherited;
	
public:
	virtual Designintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall GetValueList(Classes::TStrings* List);
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TDBGridEhFieldProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : Designeditors::TStringProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDBGridEhFieldProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBGridEhFieldAggProperty;
class PASCALIMPLEMENTATION TDBGridEhFieldAggProperty : public TDBGridEhFieldProperty 
{
	typedef TDBGridEhFieldProperty inherited;
	
public:
	virtual void __fastcall GetValueList(Classes::TStrings* List);
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TDBGridEhFieldAggProperty(const Designintf::_di_IDesigner ADesigner, int APropCount) : TDBGridEhFieldProperty(ADesigner, APropCount) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TDBGridEhFieldAggProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBLookupComboboxEhEditor;
class PASCALIMPLEMENTATION TDBLookupComboboxEhEditor : public Designeditors::TComponentEditor 
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual AnsiString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount(void);
public:
	#pragma option push -w-inl
	/* TComponentEditor.Create */ inline __fastcall virtual TDBLookupComboboxEhEditor(Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TDBLookupComboboxEhEditor(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TDBGridEhColumnsEditor* DBGridEhColumnsEditor;

}	/* namespace Gridehed */
using namespace Gridehed;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GridEhEd
