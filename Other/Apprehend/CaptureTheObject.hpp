// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CaptureTheObject.pas' rev: 6.00

#ifndef CaptureTheObjectHPP
#define CaptureTheObjectHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PixelFormatFix.hpp>	// Pascal unit
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

namespace Capturetheobject
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCaptureObjectForm;
class PASCALIMPLEMENTATION TCaptureObjectForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormActivate(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	
private:
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Messages::TWMEraseBkgnd &Msg);
	
public:
	int M;
	Graphics::TBitmap* fBmp;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCaptureObjectForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCaptureObjectForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCaptureObjectForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCaptureObjectForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCaptureObjectForm* CaptureObjectForm;
extern PACKAGE int ForceMonitorNum;

}	/* namespace Capturetheobject */
using namespace Capturetheobject;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CaptureTheObject
