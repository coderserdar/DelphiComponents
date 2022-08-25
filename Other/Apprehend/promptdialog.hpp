// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'promptdialog.pas' rev: 6.00

#ifndef promptdialogHPP
#define promptdialogHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
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

//-- user supplied -----------------------------------------------------------

namespace Promptdialog
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TfrmPosition;
class PASCALIMPLEMENTATION TfrmPosition : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label1;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	void __fastcall FormKeyPress(System::TObject* Sender, char &Key);
	
public:
	int TMx;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TfrmPosition(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TfrmPosition(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TfrmPosition(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TfrmPosition(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TfrmPosition* frmPosition;

}	/* namespace Promptdialog */
using namespace Promptdialog;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// promptdialog
