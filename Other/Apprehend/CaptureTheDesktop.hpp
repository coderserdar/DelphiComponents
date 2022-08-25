// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CaptureTheDesktop.pas' rev: 6.00

#ifndef CaptureTheDesktopHPP
#define CaptureTheDesktopHPP

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

namespace Capturethedesktop
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCaptureDesktopForm;
class PASCALIMPLEMENTATION TCaptureDesktopForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormActivate(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall FormMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	
private:
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Messages::TWMEraseBkgnd &Msg);
	
public:
	Graphics::TBitmap* fBmp;
	int M;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCaptureDesktopForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCaptureDesktopForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCaptureDesktopForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCaptureDesktopForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCaptureDesktopForm* CaptureDesktopForm;
extern PACKAGE int ForceMonitorNum;

}	/* namespace Capturethedesktop */
using namespace Capturethedesktop;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CaptureTheDesktop
