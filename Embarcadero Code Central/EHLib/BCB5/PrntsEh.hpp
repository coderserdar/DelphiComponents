// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PrntsEh.pas' rev: 5.00

#ifndef PrntsEhHPP
#define PrntsEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Printers.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Prntseh
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVirtualPrinter;
class PASCALIMPLEMENTATION TVirtualPrinter : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	virtual bool __fastcall GetAborted(void);
	virtual Graphics::TCanvas* __fastcall GetCanvas(void);
	virtual Printers::TPrinterCapabilities __fastcall GetCapabilities(void);
	virtual Classes::TStrings* __fastcall GetFonts(void);
	virtual HDC __fastcall GetHandle(void);
	virtual int __fastcall GetNumCopies(void);
	virtual Printers::TPrinterOrientation __fastcall GetOrientation(void);
	virtual int __fastcall GetPageHeight(void);
	virtual int __fastcall GetPageWidth(void);
	virtual int __fastcall GetPageNumber(void);
	virtual bool __fastcall GetPrinting(void);
	virtual AnsiString __fastcall GetTitle();
	virtual int __fastcall GetFullPageWidth(void);
	virtual int __fastcall GetFullPageHeight(void);
	virtual int __fastcall GetPrinterIndex(void);
	virtual Classes::TStrings* __fastcall GetPrinters(void);
	virtual void __fastcall SetPrinterIndex(const int Value);
	virtual void __fastcall SetNumCopies(const int Value);
	virtual void __fastcall SetOrientation(const Printers::TPrinterOrientation Value);
	virtual void __fastcall SetTitle(const AnsiString Value);
	
public:
	__fastcall TVirtualPrinter(void);
	__fastcall virtual ~TVirtualPrinter(void);
	virtual void __fastcall Abort(void);
	virtual void __fastcall BeginDoc(void);
	virtual void __fastcall EndDoc(void);
	virtual void __fastcall NewPage(void);
	virtual void __fastcall GetPrinter(char * ADevice, char * ADriver, char * APort, unsigned &ADeviceMode
		);
	virtual void __fastcall SetPrinter(char * ADevice, char * ADriver, char * APort, unsigned ADeviceMode
		);
	__property bool Aborted = {read=GetAborted, nodefault};
	__property Graphics::TCanvas* Canvas = {read=GetCanvas};
	__property Printers::TPrinterCapabilities Capabilities = {read=GetCapabilities, nodefault};
	__property int Copies = {read=GetNumCopies, write=SetNumCopies, nodefault};
	__property Classes::TStrings* Fonts = {read=GetFonts};
	__property HDC Handle = {read=GetHandle, nodefault};
	__property Printers::TPrinterOrientation Orientation = {read=GetOrientation, write=SetOrientation, 
		nodefault};
	__property int PageHeight = {read=GetPageHeight, nodefault};
	__property int PageWidth = {read=GetPageWidth, nodefault};
	__property int PageNumber = {read=GetPageNumber, nodefault};
	__property int PrinterIndex = {read=GetPrinterIndex, write=SetPrinterIndex, nodefault};
	__property int FullPageWidth = {read=GetFullPageWidth, nodefault};
	__property int FullPageHeight = {read=GetFullPageHeight, nodefault};
	__property bool Printing = {read=GetPrinting, nodefault};
	__property Classes::TStrings* Printers = {read=GetPrinters};
	__property AnsiString Title = {read=GetTitle, write=SetTitle};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TVirtualPrinter* VirtualPrinter;

}	/* namespace Prntseh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Prntseh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PrntsEh
