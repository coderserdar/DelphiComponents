// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CaptureIcon.pas' rev: 6.00

#ifndef CaptureIconHPP
#define CaptureIconHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PixelFormatFix.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Captureicon
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCaptureIconForm;
class PASCALIMPLEMENTATION TCaptureIconForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Extctrls::TTimer* Timer1;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormMouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormPaint(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall Timer1Timer(System::TObject* Sender);
	
private:
	int X1;
	int Y1;
	int X2;
	int Y2;
	void __fastcall RemoveTheRect(void);
	void __fastcall DrawTheRect(void);
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Messages::TWMEraseBkgnd &Msg);
	
public:
	#pragma pack(push, 1)
	Types::TRect fRect;
	#pragma pack(pop)
	
	Graphics::TBitmap* fBmp;
	Graphics::TBitmap* RectBitmap;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCaptureIconForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCaptureIconForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCaptureIconForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCaptureIconForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCaptureIconForm* CaptureIconForm;
extern PACKAGE Byte Counter;
extern PACKAGE Byte CounterStart;
extern PACKAGE int Looper;

}	/* namespace Captureicon */
using namespace Captureicon;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CaptureIcon
