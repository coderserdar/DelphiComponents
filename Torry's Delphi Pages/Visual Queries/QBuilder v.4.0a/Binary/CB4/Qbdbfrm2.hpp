// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'QBdbFrm2.pas' rev: 4.00

#ifndef QBdbFrm2HPP
#define QBdbFrm2HPP

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

namespace Qbdbfrm2
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TOQBDBForm2;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBDBForm2 : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* BtnOk;
	Stdctrls::TButton* BtnCancel;
	Extctrls::TBevel* Bevel1;
	Stdctrls::TCheckBox* CheckDB;
	Stdctrls::TEdit* EdtDB;
	Stdctrls::TButton* btnDir;
	Stdctrls::TLabel* Label1;
	Dialogs::TOpenDialog* DlgSelect;
	Stdctrls::TCheckBox* CheckView;
	void __fastcall btnDbClick(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TOQBDBForm2(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TOQBDBForm2(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TOQBDBForm2(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBDBForm2(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Qbdbfrm2 */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Qbdbfrm2;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// QBdbFrm2
