// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PrnDbgeh.pas' rev: 6.00

#ifndef PrnDbgehHPP
#define PrnDbgehHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ComCtrls.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DB.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <PrViewEh.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <PrntsEh.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Prndbgeh
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TMeasureUnits { MM, Inches };
#pragma option pop

#pragma option push -b-
enum TPrintDBGridEhOption { pghFitGridToPageWidth, pghColored, pghRowAutoStretch, pghFitingByColWidths, pghOptimalColWidths };
#pragma option pop

typedef Set<TPrintDBGridEhOption, pghFitGridToPageWidth, pghOptimalColWidths>  TPrintDBGridEhOptions;

class DELPHICLASS TPageParams;
class PASCALIMPLEMENTATION TPageParams : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	System::Currency FBottomMargin;
	System::Currency FRightMargin;
	System::Currency FLeftMargin;
	System::Currency FTopMargin;
	void __fastcall SetBottomMargin(const System::Currency Value);
	void __fastcall SetLeftMargin(const System::Currency Value);
	void __fastcall SetRightMargin(const System::Currency Value);
	void __fastcall SetTopMargin(const System::Currency Value);
	
public:
	__fastcall TPageParams(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property System::Currency BottomMargin = {read=FBottomMargin, write=SetBottomMargin};
	__property System::Currency LeftMargin = {read=FLeftMargin, write=SetLeftMargin};
	__property System::Currency RightMargin = {read=FRightMargin, write=SetRightMargin};
	__property System::Currency TopMargin = {read=FTopMargin, write=SetTopMargin};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TPageParams(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TPageColontitleLineType { pcltNon, pcltSingleLine, pcltDoubleLine };
#pragma option pop

class DELPHICLASS TPageColontitle;
class PASCALIMPLEMENTATION TPageColontitle : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	Classes::TStrings* FCenterText;
	Graphics::TFont* FFont;
	Classes::TStrings* FLeftText;
	TPageColontitleLineType FLineType;
	Classes::TStrings* FRightText;
	void __fastcall SetCenterText(const Classes::TStrings* Value);
	void __fastcall SetFont(const Graphics::TFont* Value);
	void __fastcall SetLeftText(const Classes::TStrings* Value);
	void __fastcall SetLineType(const TPageColontitleLineType Value);
	void __fastcall SetRightText(const Classes::TStrings* Value);
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	__fastcall TPageColontitle(void);
	__fastcall virtual ~TPageColontitle(void);
	
__published:
	__property Classes::TStrings* CenterText = {read=FCenterText, write=SetCenterText};
	__property Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property Classes::TStrings* LeftText = {read=FLeftText, write=SetLeftText};
	__property TPageColontitleLineType LineType = {read=FLineType, write=SetLineType, default=0};
	__property Classes::TStrings* RightText = {read=FRightText, write=SetRightText};
};


class DELPHICLASS TPrintDBGridEh;
class PASCALIMPLEMENTATION TPrintDBGridEh : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	Classes::TStrings* FAfterGridText;
	Classes::TStrings* FBeforeGridText;
	Dbgrideh::TColCellParamsEh* FColCellParamsEh;
	Dbgrideh::TDBGridEh* FDBGridEh;
	Classes::TNotifyEvent FOnAfterPrint;
	Classes::TNotifyEvent FOnBeforePrint;
	Classes::TNotifyEvent FOnPrinterSetupDialog;
	TPrintDBGridEhOptions FOptions;
	TPageParams* FPage;
	TPageColontitle* FPageFooter;
	TPageColontitle* FPageHeader;
	AnsiString FPrintFontName;
	Classes::TStrings* FSubstitutesNames;
	Classes::TStrings* FSubstitutesValues;
	Classes::TStrings* FTitle;
	TMeasureUnits FUnits;
	Classes::TStrings* __fastcall GetAfterGridText(void);
	Classes::TStrings* __fastcall GetBeforeGridText(void);
	int __fastcall GridTextReplace(Classes::TStrings* RichStrings, const AnsiString SearchStr, const AnsiString ReplaceStr, int StartPos, int Length, Comctrls::TSearchTypes Options, bool ReplaceAll);
	void __fastcall ReadAfterGridText(Classes::TStream* Stream);
	void __fastcall ReadBeforeGridText(Classes::TStream* Stream);
	void __fastcall SetAfterGridText(const Classes::TStrings* Value);
	void __fastcall SetBeforeGridText(const Classes::TStrings* Value);
	void __fastcall SetDBGridEh(const Dbgrideh::TDBGridEh* Value);
	void __fastcall SetOptions(const TPrintDBGridEhOptions Value);
	void __fastcall SetPage(const TPageParams* Value);
	void __fastcall SetPageFooter(const TPageColontitle* Value);
	void __fastcall SetPageHeader(const TPageColontitle* Value);
	void __fastcall SetPrintFontName(const AnsiString Value);
	void __fastcall SetTitle(const Classes::TStrings* Value);
	void __fastcall SetUnits(const TMeasureUnits Value);
	void __fastcall WriteAfterGridText(Classes::TStream* Stream);
	void __fastcall WriteBeforeGridText(Classes::TStream* Stream);
	
protected:
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall PrinterSetupDialogPreview(System::TObject* Sender);
	
public:
	__fastcall virtual TPrintDBGridEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TPrintDBGridEh(void);
	bool __fastcall PrinterSetupDialog(void);
	void __fastcall Preview(void);
	void __fastcall Print(void);
	void __fastcall PrintTo(Prntseh::TVirtualPrinter* VPrinter);
	void __fastcall SetSubstitutes(const System::TVarRec * ASubstitutes, const int ASubstitutes_Size);
	
__published:
	__property Classes::TStrings* AfterGridText = {read=GetAfterGridText, write=SetAfterGridText, stored=false};
	__property Classes::TStrings* BeforeGridText = {read=GetBeforeGridText, write=SetBeforeGridText, stored=false};
	__property Dbgrideh::TDBGridEh* DBGridEh = {read=FDBGridEh, write=SetDBGridEh};
	__property TPrintDBGridEhOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property TPageParams* Page = {read=FPage, write=SetPage};
	__property TPageColontitle* PageFooter = {read=FPageFooter, write=SetPageFooter};
	__property TPageColontitle* PageHeader = {read=FPageHeader, write=SetPageHeader};
	__property AnsiString PrintFontName = {read=FPrintFontName, write=SetPrintFontName};
	__property Classes::TStrings* Title = {read=FTitle, write=SetTitle};
	__property TMeasureUnits Units = {read=FUnits, write=SetUnits, nodefault};
	__property Classes::TNotifyEvent OnAfterPrint = {read=FOnAfterPrint, write=FOnAfterPrint};
	__property Classes::TNotifyEvent OnBeforePrint = {read=FOnBeforePrint, write=FOnBeforePrint};
	__property Classes::TNotifyEvent OnPrinterSetupDialog = {read=FOnPrinterSetupDialog, write=FOnPrinterSetupDialog};
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Prndbgeh */
using namespace Prndbgeh;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PrnDbgeh
