// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CaptureFreehand.pas' rev: 6.00

#ifndef CaptureFreehandHPP
#define CaptureFreehandHPP

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

namespace Capturefreehand
{
//-- type declarations -------------------------------------------------------
typedef Types::TPoint TData[1];

class DELPHICLASS TCaptureFreehandForm;
class PASCALIMPLEMENTATION TCaptureFreehandForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormMouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	void __fastcall FormPaint(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	
public:
	Graphics::TBitmap* fBMP;
	Graphics::TBitmap* fCapBMP;
	Graphics::TBitmap* fOrigBmp;
	bool fDragging;
	#pragma pack(push, 1)
	Types::TRect fRect;
	#pragma pack(pop)
	
	int fXMin;
	int fYMin;
	int fXMax;
	int fYMax;
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Messages::TWMEraseBkgnd &Msg);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCaptureFreehandForm(Classes::TComponent* AOwner) : Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCaptureFreehandForm(Classes::TComponent* AOwner, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCaptureFreehandForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCaptureFreehandForm(HWND ParentWindow) : Forms::TForm(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCaptureFreehandForm* CaptureFreehandForm;
extern PACKAGE int i;
extern PACKAGE Types::TPoint *Memdat;

}	/* namespace Capturefreehand */
using namespace Capturefreehand;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CaptureFreehand
