// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'main.pas' rev: 5.00

#ifndef mainHPP
#define mainHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ExtDlgs.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <colorado.hpp>	// Pascal unit
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

namespace Main
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMainForm;
class PASCALIMPLEMENTATION TMainForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Colorado::TConnection* Connection1;
	Colorado::TCTable* CTable1;
	Db::TDataSource* DataSource1;
	Dbgrids::TDBGrid* DBGrid1;
	Dbctrls::TDBNavigator* DBNavigator1;
	Dbctrls::TDBMemo* DBMemo1;
	Dbctrls::TDBImage* DBImage1;
	Stdctrls::TButton* Button1;
	Extdlgs::TOpenPictureDialog* OpenPictureDialog1;
	void __fastcall Button1Click(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMainForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMainForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMainForm* MainForm;

}	/* namespace Main */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Main;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// main
