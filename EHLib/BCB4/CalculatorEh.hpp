// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CalculatorEh.pas' rev: 4.00

#ifndef CalculatorEhHPP
#define CalculatorEhHPP

#pragma delphiheader begin
#pragma option push -w-
#include <ToolCtrlsEh.hpp>	// Pascal unit
#include <Clipbrd.hpp>	// Pascal unit
#include <Math.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
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

namespace Calculatoreh
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TCalcStateEh { csFirstEh, csValidEh, csErrorEh };
#pragma option pop

class DELPHICLASS TCalculatorEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TCalculatorEh : public Controls::TCustomControl 
{
	typedef Controls::TCustomControl inherited;
	
__published:
	Extctrls::TPanel* Panel1;
	Toolctrlseh::TSpeedButtonEh* SpeedButton1;
	Toolctrlseh::TSpeedButtonEh* SpeedButton2;
	Toolctrlseh::TSpeedButtonEh* SpeedButton3;
	Toolctrlseh::TSpeedButtonEh* SpeedButton4;
	Toolctrlseh::TSpeedButtonEh* SpeedButton5;
	Toolctrlseh::TSpeedButtonEh* SpeedButton6;
	Toolctrlseh::TSpeedButtonEh* SpeedButton7;
	Toolctrlseh::TSpeedButtonEh* SpeedButton8;
	Toolctrlseh::TSpeedButtonEh* SpeedButton9;
	Toolctrlseh::TSpeedButtonEh* SpeedButton10;
	Toolctrlseh::TSpeedButtonEh* SpeedButton11;
	Toolctrlseh::TSpeedButtonEh* SpeedButton12;
	Toolctrlseh::TSpeedButtonEh* SpeedButton13;
	Toolctrlseh::TSpeedButtonEh* SpeedButton14;
	Toolctrlseh::TSpeedButtonEh* SpeedButton15;
	Toolctrlseh::TSpeedButtonEh* SpeedButton16;
	Toolctrlseh::TSpeedButtonEh* SpeedButton18;
	Toolctrlseh::TSpeedButtonEh* SpeedButton19;
	Toolctrlseh::TSpeedButtonEh* SpeedButton20;
	Toolctrlseh::TSpeedButtonEh* SpeedButton22;
	Toolctrlseh::TSpeedButtonEh* SpeedButton23;
	Toolctrlseh::TSpeedButtonEh* SpeedButton24;
	Toolctrlseh::TSpeedButtonEh* spEqual;
	Stdctrls::TLabel* TextBox;
	void __fastcall SpeedButtonClick(System::TObject* Sender);
	
private:
	Forms::TFormBorderStyle FBorderStyle;
	int FClientHeight;
	int FClientWidth;
	double FOperand;
	char FOperator;
	int FPixelsPerInch;
	TCalcStateEh FStatus;
	int FTextHeight;
	AnsiString __fastcall GetDisplayText();
	double __fastcall GetDisplayValue(void);
	int __fastcall GetPixelsPerInch(void);
	void __fastcall CheckFirst(void);
	void __fastcall Clear(void);
	void __fastcall Error(void);
	void __fastcall ReadTextHeight(Classes::TReader* Reader);
	void __fastcall SetBorderStyle(const Forms::TBorderStyle Value);
	HIDESBASE void __fastcall SetClientHeight(int Value);
	HIDESBASE void __fastcall SetClientWidth(int Value);
	void __fastcall SetDisplayText(const AnsiString Value);
	void __fastcall SetDisplayValue(const double Value);
	void __fastcall SetOldCreateOrder(const bool Value);
	void __fastcall SetPixelsPerInch(const int Value);
	void __fastcall UpdateEqualButton(void);
	
protected:
	virtual bool __fastcall CanAutoSize(int &NewWidth, int &NewHeight);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	virtual int __fastcall GetBorderSize(void);
	int __fastcall GetTextHeight(void);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	DYNAMIC void __fastcall KeyPress(char &Key);
	virtual void __fastcall ReadState(Classes::TReader* Reader);
	
public:
	__fastcall virtual TCalculatorEh(Classes::TComponent* AOwner);
	void __fastcall DoCopy(void);
	void __fastcall Paste(void);
	virtual void __fastcall ProcessKey(char Key);
	__property AnsiString DisplayText = {read=GetDisplayText, write=SetDisplayText};
	__property double DisplayValue = {read=GetDisplayValue, write=SetDisplayValue};
	
__published:
	__property Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, nodefault};
	__property ClientHeight  = {write=SetClientHeight};
	__property ClientWidth  = {write=SetClientWidth};
	__property Color ;
	__property Font ;
	__property bool OldCreateOrder = {write=SetOldCreateOrder, nodefault};
	__property int PixelsPerInch = {read=GetPixelsPerInch, write=SetPixelsPerInch, stored=false, nodefault
		};
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TCalculatorEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCalculatorEh(HWND ParentWindow) : Controls::TCustomControl(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

__interface IPopupCalculatorEh;
typedef System::DelphiInterface<IPopupCalculatorEh> _di_IPopupCalculatorEh;
__interface IPopupCalculatorEh  : public IUnknown /* __guid="{697F81AD-0E0F-4A4A-A016-A713620660DE}" */
	
{
	
public:
	virtual bool __fastcall GetEnterCanClose(void) = 0 ;
	virtual bool __fastcall GetFlat(void) = 0 ;
	virtual Variant __fastcall GetValue(void) = 0 ;
	virtual void __fastcall SetFlat(const bool Value) = 0 ;
	virtual void __fastcall SetValue(const Variant &Value) = 0 ;
	__property Variant Value = {read=GetValue, write=SetValue};
	__property bool Flat = {read=GetFlat, write=SetFlat};
	__property bool EnterCanClose = {read=GetEnterCanClose};
};

class DELPHICLASS TPopupCalculatorEh;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPopupCalculatorEh : public TCalculatorEh 
{
	typedef TCalculatorEh inherited;
	
private:
	int FBorderWidth;
	bool FFlat;
	MESSAGE void __fastcall CMCloseUpEh(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMMouseActivate(Messages::TWMMouseActivate &Message);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TWMNoParams &Message);
	
protected:
	bool __fastcall GetEnterCanClose(void);
	bool __fastcall GetFlat(void);
	Variant __fastcall GetValue();
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetValue(const Variant &Value);
	virtual bool __fastcall CanAutoSize(int &NewWidth, int &NewHeight);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DrawBorder(void);
	void __fastcall UpdateBorderWidth(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	
public:
	__fastcall virtual TPopupCalculatorEh(Classes::TComponent* AOwner);
	HIDESBASE bool __fastcall CanFocus(void);
	virtual void __fastcall ProcessKey(char Key);
	__property bool Flat = {read=GetFlat, write=SetFlat, default=1};
	__property Ctl3D ;
public:
	#pragma option push -w-inl
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TPopupCalculatorEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TPopupCalculatorEh(HWND ParentWindow) : TCalculatorEh(
		ParentWindow) { }
	#pragma option pop
	
private:
	void *__IPopupCalculatorEh;	/* Calculatoreh::IPopupCalculatorEh */
	
public:
	operator IUnknown*(void) { return (IUnknown*)&__IPopupCalculatorEh; }
	operator IPopupCalculatorEh*(void) { return (IPopupCalculatorEh*)&__IPopupCalculatorEh; }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Shortint DefCalcPrecision = 0xf;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Calculatoreh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Calculatoreh;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CalculatorEh
