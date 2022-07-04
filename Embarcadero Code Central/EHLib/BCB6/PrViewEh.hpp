// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PrViewEh.pas' rev: 6.00

#ifndef PrViewEhHPP
#define PrViewEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Printers.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <PrntsEh.hpp>	// Pascal unit
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

namespace Prvieweh
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TViewMode { vm500, vm200, vm150, vm100, vm75, vm50, vm25, vm10, vmPageWidth, vmFullPage };
#pragma option pop

class DELPHICLASS TDrawPanel;
class PASCALIMPLEMENTATION TDrawPanel : public Extctrls::TPanel 
{
	typedef Extctrls::TPanel inherited;
	
private:
	#pragma pack(push, 1)
	Types::TPoint FOldMousePos;
	#pragma pack(pop)
	
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Messages::TWMNoParams &Message);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall Paint(void);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	
public:
	__fastcall virtual TDrawPanel(Classes::TComponent* AOwner);
	__property Canvas ;
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TDrawPanel(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDrawPanel(HWND ParentWindow) : Extctrls::TPanel(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TPreviewBox;
class DELPHICLASS TPrinterPreview;
class PASCALIMPLEMENTATION TPrinterPreview : public Prntseh::TVirtualPrinter 
{
	typedef Prntseh::TVirtualPrinter inherited;
	
private:
	bool FAborted;
	Graphics::TMetafileCanvas* FMetafileCanvas;
	Classes::TList* FMetafileList;
	Classes::TNotifyEvent FOnPrinterSetupChanged;
	Classes::TNotifyEvent FOnPrinterSetupDialog;
	int FPageNumber;
	TPreviewBox* FPreviewer;
	Printers::TPrinter* FPrinter;
	Classes::TComponent* FPrinterSetupOwner;
	bool FPrinting;
	Printers::TPrinter* __fastcall GetPropPrinter(void);
	void __fastcall SetOnPrinterSetupDialog(const Classes::TNotifyEvent Value);
	void __fastcall SetPreviewer(const TPreviewBox* Value);
	
protected:
	virtual bool __fastcall GetAborted(void);
	virtual Graphics::TCanvas* __fastcall GetCanvas(void);
	virtual Printers::TPrinterCapabilities __fastcall GetCapabilities(void);
	virtual Classes::TStrings* __fastcall GetFonts(void);
	virtual int __fastcall GetFullPageHeight(void);
	virtual int __fastcall GetFullPageWidth(void);
	virtual HDC __fastcall GetHandle(void);
	virtual int __fastcall GetNumCopies(void);
	virtual Printers::TPrinterOrientation __fastcall GetOrientation(void);
	virtual int __fastcall GetPageHeight(void);
	virtual int __fastcall GetPageNumber(void);
	virtual int __fastcall GetPageWidth(void);
	virtual int __fastcall GetPrinterIndex(void);
	virtual Classes::TStrings* __fastcall GetPrinters(void);
	virtual bool __fastcall GetPrinting(void);
	virtual AnsiString __fastcall GetTitle();
	void __fastcall DrawPage(System::TObject* Sender, Graphics::TCanvas* Canvas, int PageNumber);
	virtual void __fastcall SetNumCopies(const int Value);
	virtual void __fastcall SetOrientation(const Printers::TPrinterOrientation Value);
	virtual void __fastcall SetPrinterIndex(const int Value);
	virtual void __fastcall SetTitle(const AnsiString Value);
	virtual void __fastcall ShowProgress(int Percent);
	
public:
	__fastcall TPrinterPreview(void);
	__fastcall virtual ~TPrinterPreview(void);
	virtual void __fastcall Abort(void);
	virtual void __fastcall BeginDoc(void);
	virtual void __fastcall EndDoc(void);
	virtual void __fastcall GetPrinter(char * ADevice, char * ADriver, char * APort, unsigned &ADeviceMode);
	virtual void __fastcall SetPrinter(char * ADevice, char * ADriver, char * APort, unsigned ADeviceMode);
	virtual void __fastcall NewPage(void);
	void __fastcall OpenPreview(void);
	void __fastcall Print(void);
	__property Classes::TNotifyEvent OnPrinterSetupChanged = {read=FOnPrinterSetupChanged, write=FOnPrinterSetupChanged};
	__property Classes::TNotifyEvent OnPrinterSetupDialog = {read=FOnPrinterSetupDialog, write=SetOnPrinterSetupDialog};
	__property TPreviewBox* Previewer = {read=FPreviewer, write=SetPreviewer};
	__property Printers::TPrinter* Printer = {read=GetPropPrinter};
	__property Classes::TComponent* PrinterSetupOwner = {read=FPrinterSetupOwner, write=FPrinterSetupOwner};
};


class PASCALIMPLEMENTATION TPreviewBox : public Forms::TScrollBox 
{
	typedef Forms::TScrollBox inherited;
	
private:
	TDrawPanel* FDrawPanel;
	Classes::TNotifyEvent FOnOpenPreviewer;
	Classes::TNotifyEvent FOnPrinterPreviewChanged;
	Classes::TNotifyEvent FOnPrinterSetupChanged;
	Classes::TNotifyEvent FOnPrinterSetupDialog;
	int FPageCount;
	int FPageIndex;
	TPrinterPreview* FPrinter;
	Classes::TComponent* FPrinterSetupOwner;
	TViewMode FViewMode;
	Extctrls::TPanel* pnlShadow;
	void __fastcall SetPageIndex(int Value);
	void __fastcall SetPrinter(const TPrinterPreview* Value);
	void __fastcall SetPrinterSetupOwner(const Classes::TComponent* Value);
	void __fastcall SetViewMode(const TViewMode Value);
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	int FScalePercent;
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Types::TPoint &MousePos);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Types::TPoint &MousePos);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	
public:
	__fastcall virtual TPreviewBox(Classes::TComponent* AOwner);
	__fastcall virtual ~TPreviewBox(void);
	void __fastcall PrintDialog(void);
	void __fastcall PrinterSetupDialog(void);
	void __fastcall UpdatePageSetup(void);
	void __fastcall UpdatePreview(void);
	__property Classes::TNotifyEvent OnPrinterSetupChanged = {read=FOnPrinterSetupChanged};
	__property Classes::TNotifyEvent OnPrinterSetupDialog = {read=FOnPrinterSetupDialog};
	__property int PageCount = {read=FPageCount, nodefault};
	__property int PageIndex = {read=FPageIndex, write=SetPageIndex, nodefault};
	__property TPrinterPreview* Printer = {read=FPrinter, write=SetPrinter};
	__property Classes::TComponent* PrinterSetupOwner = {read=FPrinterSetupOwner, write=SetPrinterSetupOwner};
	__property TViewMode ViewMode = {read=FViewMode, write=SetViewMode, nodefault};
	
__published:
	__property Classes::TNotifyEvent OnOpenPreviewer = {read=FOnOpenPreviewer, write=FOnOpenPreviewer};
	__property Classes::TNotifyEvent OnPrinterPreviewChanged = {read=FOnPrinterPreviewChanged, write=FOnPrinterPreviewChanged};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TPreviewBox(HWND ParentWindow) : Forms::TScrollBox(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE int DefaultPrinterPhysicalOffSetX;
extern PACKAGE int DefaultPrinterPhysicalOffSetY;
extern PACKAGE int DefaultPrinterPageWidth;
extern PACKAGE int DefaultPrinterPageHeight;
extern PACKAGE int DefaultPrinterPixelsPerInchX;
extern PACKAGE int DefaultPrinterPixelsPerInchY;
extern PACKAGE int DefaultPrinterVerticalSizeMM;
extern PACKAGE int DefaultPrinterHorizontalSizeMM;
extern PACKAGE TPrinterPreview* __fastcall PrinterPreview(void);
extern PACKAGE TPrinterPreview* __fastcall SetPrinterPreview(TPrinterPreview* NewPrinterPreview);

}	/* namespace Prvieweh */
using namespace Prvieweh;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PrViewEh
