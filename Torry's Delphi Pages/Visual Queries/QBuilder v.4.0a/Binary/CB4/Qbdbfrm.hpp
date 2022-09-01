// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'QBdbFrm.pas' rev: 4.00

#ifndef QBdbFrmHPP
#define QBdbFrmHPP

#pragma delphiheader begin
#pragma option push -w-
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

namespace Qbdbfrm
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TOQBDBForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBDBForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* BtnOk;
	Stdctrls::TButton* BtnCancel;
	Extctrls::TBevel* Bevel1;
	Stdctrls::TComboBox* ComboDB;
	Stdctrls::TCheckBox* CheckDB;
	Stdctrls::TEdit* EdtDir;
	Stdctrls::TButton* btnDir;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Dialogs::TOpenDialog* DlgOpen;
	void __fastcall btnDirClick(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TOQBDBForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TOQBDBForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TOQBDBForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBDBForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Qbdbfrm */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Qbdbfrm;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// QBdbFrm
