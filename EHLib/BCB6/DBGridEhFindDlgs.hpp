// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBGridEhFindDlgs.pas' rev: 6.00

#ifndef DBGridEhFindDlgsHPP
#define DBGridEhFindDlgsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <DBGridEh.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <DBCtrlsEh.hpp>	// Pascal unit
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

//-- user supplied -----------------------------------------------------------

namespace Dbgridehfinddlgs
{
//-- type declarations -------------------------------------------------------
#pragma pack(push, 4)
struct TColumnFieldItemEh
{
	AnsiString Caption;
	Dbgrideh::TColumnEh* Column;
} ;
#pragma pack(pop)

typedef DynamicArray<TColumnFieldItemEh >  TColumnFieldsArrEh;

class DELPHICLASS TDBGridEhFindDlg;
class PASCALIMPLEMENTATION TDBGridEhFindDlg : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Dbctrlseh::TDBComboBoxEh* cbText;
	Stdctrls::TButton* bFind;
	Stdctrls::TButton* bCancel;
	Stdctrls::TLabel* Label1;
	Dbctrlseh::TDBComboBoxEh* cbFindIn;
	Stdctrls::TLabel* Label2;
	Dbctrlseh::TDBComboBoxEh* cbMatchinType;
	Stdctrls::TLabel* cbMatchType;
	Dbctrlseh::TDBComboBoxEh* cbFindDirection;
	Stdctrls::TLabel* Label3;
	Dbctrlseh::TDBCheckBoxEh* cbCharCase;
	Dbctrlseh::TDBCheckBoxEh* cbUseFormat;
	void __fastcall bFindClick(System::TObject* Sender);
	void __fastcall bCancelClick(System::TObject* Sender);
	void __fastcall cbFindInChange(System::TObject* Sender);
	void __fastcall cbTextChange(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	
private:
	Dbgrideh::TCustomDBGridEh* FGrid;
	bool IsFirstTry;
	Dbgrideh::TColumnsEhList* FFindColumnsList;
	int FCurInListColIndex;
	DynamicArray<TColumnFieldItemEh >  FColumnFields;
	AnsiString __fastcall ColText(Dbgrideh::TColumnEh* Col);
	
public:
	void __fastcall FillFindColumnsList(void);
	void __fastcall FillColumnsList(void);
	void __fastcall Excecute(Dbgrideh::TCustomDBGridEh* AGrid, AnsiString Text, AnsiString ColumnFieldName, TColumnFieldsArrEh ColumnFields, bool Modal);
	__property Dbgrideh::TCustomDBGridEh* Grid = {read=FGrid};
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TDBGridEhFindDlg(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TDBGridEhFindDlg(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TDBGridEhFindDlg(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBGridEhFindDlg(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


typedef void __fastcall (*TExcecuteDBGridEhFindDialogProc)(Dbgrideh::TCustomDBGridEh* Grid, AnsiString Text, AnsiString FieldName, TColumnFieldsArrEh ColumnFields, bool Modal);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TDBGridEhFindDlg* DBGridEhFindDlg;
extern PACKAGE TExcecuteDBGridEhFindDialogProc ExcecuteDBGridEhFindDialogProc;
extern PACKAGE void __fastcall ExcecuteDBGridEhFindDialog(Dbgrideh::TCustomDBGridEh* Grid, AnsiString Text, AnsiString FieldName, TColumnFieldsArrEh ColumnFields, bool Modal);

}	/* namespace Dbgridehfinddlgs */
using namespace Dbgridehfinddlgs;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DBGridEhFindDlgs
