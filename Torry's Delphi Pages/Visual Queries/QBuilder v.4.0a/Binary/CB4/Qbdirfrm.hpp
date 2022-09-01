// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'QBDirFrm.pas' rev: 4.00

#ifndef QBDirFrmHPP
#define QBDirFrmHPP

#pragma delphiheader begin
#pragma option push -w-
#include <FileCtrl.hpp>	// Pascal unit
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

namespace Qbdirfrm
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TOQBDirForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBDirForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* BtnOk;
	Stdctrls::TButton* BtnCancel;
	Extctrls::TBevel* Bevel;
	Filectrl::TDriveComboBox* ComboDrive;
	Filectrl::TDirectoryListBox* DirLbx;
	Filectrl::TFileListBox* FileLbx;
	void __fastcall ComboDriveChange(System::TObject* Sender);
	void __fastcall DirLbxChange(System::TObject* Sender);
	
private:
	void __fastcall SetDir(AnsiString aDir);
	AnsiString __fastcall GetDir();
	
public:
	__property AnsiString Directory = {read=GetDir, write=SetDir};
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TOQBDirForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TOQBDirForm(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TOQBDirForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBDirForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Qbdirfrm */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Qbdirfrm;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// QBDirFrm
