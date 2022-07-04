// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ExProps.pas' rev: 6.00

#ifndef ExPropsHPP
#define ExPropsHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Exprops
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum EXpEditType { etNone, etEdit, etEditFunction, etFunction, etDropEdit, etDropList };
#pragma option pop

typedef void __fastcall (__closure *TXpPropertyDataEvent)(System::TObject* oOwner, int wIndex, AnsiString &sData);

typedef void __fastcall (__closure *TXpQueryEditEvent)(System::TObject* oOwner, int wIndex, EXpEditType &oEditType, AnsiString &sFunctionName);

typedef void __fastcall (__closure *TXpValueChangeEvent)(System::TObject* oOwner, int wIndex, AnsiString sValue);

typedef void __fastcall (__closure *TXpFunctionEvent)(System::TObject* oOwner, int wIndex, AnsiString sFunction, AnsiString &sValue, bool &bSendValue);

class DELPHICLASS TXpPropertyEdit;
class PASCALIMPLEMENTATION TXpPropertyEdit : public Mask::TCustomMaskEdit 
{
	typedef Mask::TCustomMaskEdit inherited;
	
protected:
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	
public:
	__fastcall virtual TXpPropertyEdit(Classes::TComponent* oOwner);
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TXpPropertyEdit(HWND ParentWindow) : Mask::TCustomMaskEdit(ParentWindow) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TWinControl.Destroy */ inline __fastcall virtual ~TXpPropertyEdit(void) { }
	#pragma option pop
	
};


class DELPHICLASS TXpPropertiesPane;
class PASCALIMPLEMENTATION TXpPropertiesPane : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
private:
	Graphics::TColor FBkgndColor;
	int FRowCount;
	int FRowHeight;
	int FRowTop;
	bool FShowLines;
	int FSelected;
	int FIndent;
	bool FEditable;
	EXpEditType FEditType;
	AnsiString FFunction;
	Graphics::TFont* FFont;
	Graphics::TFont* FSelectedFont;
	TXpPropertyEdit* FEdit;
	Stdctrls::TButton* FFunctionBtn;
	TXpPropertyDataEvent FOnPropertyData;
	TXpQueryEditEvent FOnQueryEdit;
	Classes::TNotifyEvent FOnSelectedChange;
	TXpValueChangeEvent FOnValueChange;
	Classes::TNotifyEvent FOnRowTopChanged;
	TXpFunctionEvent FOnFunction;
	void __fastcall SetRowCount(const int Value);
	void __fastcall SetRowHeight(const int Value);
	int __fastcall GetVisibleRowCount(void);
	void __fastcall SetRowTop(const int Value);
	void __fastcall SetShowLines(const bool Value);
	void __fastcall SetSelected(const int Value);
	void __fastcall SetEditable(const bool Value);
	HIDESBASE void __fastcall SetFont(const Graphics::TFont* Value);
	void __fastcall SetSelectedFont(const Graphics::TFont* Value);
	bool __fastcall ControlsVisible(void);
	void __fastcall ClearControls(void);
	void __fastcall SetControls(void);
	void __fastcall StoreResult(void);
	
protected:
	void __fastcall DoFunctionClick(System::TObject* oOwner);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &oMsg);
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &oMsg);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &oMsg);
	MESSAGE void __fastcall WMMovePrevious(Messages::TMessage &oMsg);
	MESSAGE void __fastcall WMMoveNext(Messages::TMessage &oMsg);
	MESSAGE void __fastcall WMStoreResult(Messages::TMessage &oMsg);
	MESSAGE void __fastcall WMCancelControls(Messages::TMessage &oMsg);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	
public:
	__fastcall virtual TXpPropertiesPane(Classes::TComponent* oOwner);
	__fastcall virtual ~TXpPropertiesPane(void);
	virtual void __fastcall Paint(void);
	__property Graphics::TColor BackgroundColor = {read=FBkgndColor, write=FBkgndColor, nodefault};
	__property bool Editable = {read=FEditable, write=SetEditable, nodefault};
	__property Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property int RowCount = {read=FRowCount, write=SetRowCount, nodefault};
	__property int RowHeight = {read=FRowHeight, write=SetRowHeight, nodefault};
	__property int RowTop = {read=FRowTop, write=SetRowTop, nodefault};
	__property int Selected = {read=FSelected, write=SetSelected, nodefault};
	__property Graphics::TFont* SelectedFont = {read=FSelectedFont, write=SetSelectedFont};
	__property bool ShowLines = {read=FShowLines, write=SetShowLines, nodefault};
	__property int VisibleRowCount = {read=GetVisibleRowCount, nodefault};
	__property TXpFunctionEvent OnFunction = {read=FOnFunction, write=FOnFunction};
	__property TXpPropertyDataEvent OnPropertyData = {read=FOnPropertyData, write=FOnPropertyData};
	__property TXpQueryEditEvent OnQueryEdit = {read=FOnQueryEdit, write=FOnQueryEdit};
	__property Classes::TNotifyEvent OnRowTopChanged = {read=FOnRowTopChanged, write=FOnRowTopChanged};
	__property Classes::TNotifyEvent OnSelectedChange = {read=FOnSelectedChange, write=FOnSelectedChange};
	__property TXpValueChangeEvent OnValueChange = {read=FOnValueChange, write=FOnValueChange};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TXpPropertiesPane(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TXpPropertiesWindow;
class PASCALIMPLEMENTATION TXpPropertiesWindow : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
private:
	Comctrls::THeaderControl* FHeader;
	TXpPropertiesPane* FNamePane;
	Extctrls::TSplitter* FSplitter;
	TXpPropertiesPane* FValuePane;
	Stdctrls::TScrollBar* FScroller;
	bool FShowHeader;
	AnsiString FNameHeader;
	AnsiString FValueHeader;
	int __fastcall GetRowCount(void);
	int __fastcall GetRowHeight(void);
	void __fastcall SetRowCount(const int Value);
	void __fastcall SetRowHeight(const int Value);
	int __fastcall GetRowTop(void);
	void __fastcall SetRowTop(const int Value);
	bool __fastcall GetShowLines(void);
	void __fastcall SetShowLines(const bool Value);
	void __fastcall SetNameHeader(const AnsiString Value);
	void __fastcall SetShowHeader(const bool Value);
	void __fastcall SetValueHeader(const AnsiString Value);
	int __fastcall GetSelected(void);
	void __fastcall SetSelected(const int Value);
	TXpPropertyDataEvent __fastcall GetOnPropertyName();
	TXpPropertyDataEvent __fastcall GetOnPropertyValue();
	void __fastcall SetOnPropertyName(const TXpPropertyDataEvent Value);
	void __fastcall SetOnPropertyValue(const TXpPropertyDataEvent Value);
	Graphics::TFont* __fastcall GetNameFont(void);
	Graphics::TFont* __fastcall GetNameFontSelected(void);
	Graphics::TFont* __fastcall GetValueFont(void);
	Graphics::TFont* __fastcall GetValueFontSelected(void);
	void __fastcall SetNameFont(const Graphics::TFont* Value);
	void __fastcall SetNameFontSelected(const Graphics::TFont* Value);
	void __fastcall SetValueFont(const Graphics::TFont* Value);
	void __fastcall SetValueFontSelected(const Graphics::TFont* Value);
	TXpQueryEditEvent __fastcall GetOnNameQueryEdit();
	TXpQueryEditEvent __fastcall GetOnValueQueryEdit();
	void __fastcall SetOnNameQueryEdit(const TXpQueryEditEvent Value);
	void __fastcall SetOnValueQueryEdit(const TXpQueryEditEvent Value);
	TXpValueChangeEvent __fastcall GetOnValueChange();
	void __fastcall SetOnValueChange(const TXpValueChangeEvent Value);
	void __fastcall SetNameWidth(const int Value);
	int __fastcall GetNameWidth(void);
	TXpFunctionEvent __fastcall GetOnNameFunction();
	TXpFunctionEvent __fastcall GetOnValueFunction();
	void __fastcall SetOnNameFunction(const TXpFunctionEvent Value);
	void __fastcall SetOnValueFunction(const TXpFunctionEvent Value);
	
protected:
	MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall Resize(void);
	void __fastcall DoHeaderResize(Comctrls::THeaderControl* oHeaderControl, Comctrls::THeaderSection* oSection);
	void __fastcall DoSplitterMoved(System::TObject* oOwner);
	void __fastcall DoScroll(System::TObject* oSender, Stdctrls::TScrollCode oScrollCode, int &oScrollPos);
	void __fastcall DoSelectedName(System::TObject* oOwner);
	void __fastcall DoSelectedValue(System::TObject* oOwner);
	void __fastcall DoRowTopChanged(System::TObject* oOwner);
	
public:
	__fastcall virtual TXpPropertiesWindow(Classes::TComponent* oOwner);
	__fastcall virtual ~TXpPropertiesWindow(void);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall Paint(void);
	
__published:
	__property Align  = {default=0};
	__property PopupMenu ;
	__property Graphics::TFont* NameFont = {read=GetNameFont, write=SetNameFont};
	__property Graphics::TFont* NameFontSelected = {read=GetNameFontSelected, write=SetNameFontSelected};
	__property AnsiString NameHeader = {read=FNameHeader, write=SetNameHeader};
	__property int NameWidth = {read=GetNameWidth, write=SetNameWidth, nodefault};
	__property Graphics::TFont* ValueFont = {read=GetValueFont, write=SetValueFont};
	__property Graphics::TFont* ValueFontSelected = {read=GetValueFontSelected, write=SetValueFontSelected};
	__property AnsiString ValueHeader = {read=FValueHeader, write=SetValueHeader};
	__property bool ShowHeader = {read=FShowHeader, write=SetShowHeader, nodefault};
	__property bool ShowLines = {read=GetShowLines, write=SetShowLines, nodefault};
	__property int RowCount = {read=GetRowCount, write=SetRowCount, nodefault};
	__property int RowHeight = {read=GetRowHeight, write=SetRowHeight, nodefault};
	__property int RowTop = {read=GetRowTop, write=SetRowTop, nodefault};
	__property int Selected = {read=GetSelected, write=SetSelected, nodefault};
	__property OnClick ;
	__property OnDblClick ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property TXpFunctionEvent OnNameFunction = {read=GetOnNameFunction, write=SetOnNameFunction};
	__property TXpQueryEditEvent OnNameQueryEdit = {read=GetOnNameQueryEdit, write=SetOnNameQueryEdit};
	__property TXpPropertyDataEvent OnPropertyName = {read=GetOnPropertyName, write=SetOnPropertyName};
	__property TXpPropertyDataEvent OnPropertyValue = {read=GetOnPropertyValue, write=SetOnPropertyValue};
	__property TXpValueChangeEvent OnValueChange = {read=GetOnValueChange, write=SetOnValueChange};
	__property TXpFunctionEvent OnValueFunction = {read=GetOnValueFunction, write=SetOnValueFunction};
	__property TXpQueryEditEvent OnValueQueryEdit = {read=GetOnValueQueryEdit, write=SetOnValueQueryEdit};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TXpPropertiesWindow(HWND ParentWindow) : Controls::TCustomControl(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word WM_XPSTORERESULT = 0x465;
static const Word WM_XPMOVEPREV = 0x466;
static const Word WM_XPMOVENEXT = 0x467;
static const Word WM_XPCANCELCONTROLS = 0x468;

}	/* namespace Exprops */
using namespace Exprops;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ExProps
