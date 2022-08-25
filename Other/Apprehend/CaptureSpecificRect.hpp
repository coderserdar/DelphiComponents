// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CaptureSpecificRect.pas' rev: 6.00

#ifndef CaptureSpecificRectHPP
#define CaptureSpecificRectHPP

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

namespace Capturespecificrect
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCaptureSpecificRectForm;
class PASCALIMPLEMENTATION TCaptureSpecificRectForm : public Forms::TForm 
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
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall SetRectCoords(int CaptureX, int CaptureY, int inCaptureWidth, int inCaptureHeight);
	
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
	int CaptureWidth;
	int CaptureHeight;
	int M;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCaptureSpecificRectForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCaptureSpecificRectForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCaptureSpecificRectForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCaptureSpecificRectForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCaptureSpecificRectForm* CaptureSpecificRectForm;
extern PACKAGE Byte Counter;
extern PACKAGE Byte CounterStart;
extern PACKAGE int Looper;
extern PACKAGE int ForceMonitorNum;

}	/* namespace Capturespecificrect */
using namespace Capturespecificrect;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CaptureSpecificRect
