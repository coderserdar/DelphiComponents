// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ASGCapture.pas' rev: 6.00

#ifndef ASGCaptureHPP
#define ASGCaptureHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PixelFormatFix.hpp>	// Pascal unit
#include <Clipbrd.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Asgcapture
{
//-- type declarations -------------------------------------------------------
typedef Types::TRect *PTRect;

class DELPHICLASS TASGScreenCapture;
class PASCALIMPLEMENTATION TASGScreenCapture : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	bool fAutomatic;
	Graphics::TBitmap* fBitmap;
	int fDelay;
	bool fMinimized;
	bool fShowHint;
	bool fShowCursor;
	Classes::TNotifyEvent fOnCapture;
	int fMonitorNum;
	int fWidth;
	int fHeight;
	void __fastcall SetDelay(const int Value);
	void __fastcall SetAutomatic(const bool Value);
	void __fastcall SetMinimized(const bool Value);
	void __fastcall SetCursor(const bool Value);
	void __fastcall SetShowHint(const bool Value);
	AnsiString __fastcall GetVersion();
	void __fastcall SetVersion(const AnsiString Val);
	void __fastcall CopyToClipboard(void);
	int fObjectLeft;
	int fObjectTop;
	int fObjectWidth;
	int fObjectHeight;
	int fObjectRLeft;
	int fObjectRTop;
	int fObjectRWidth;
	int fObjectRHeight;
	
public:
	__property Graphics::TBitmap* Bitmap = {read=fBitmap};
	__fastcall virtual TASGScreenCapture(Classes::TComponent* AOwner);
	__fastcall virtual ~TASGScreenCapture(void);
	Graphics::TBitmap* __fastcall CaptureDesktop(void);
	Graphics::TBitmap* __fastcall SpeedCaptureDesktop(void);
	Graphics::TBitmap* __fastcall CaptureWholeDesktop(void);
	Graphics::TBitmap* __fastcall CaptureActiveWindow(void);
	Graphics::TBitmap* __fastcall CaptureObject(void);
	Graphics::TBitmap* __fastcall CaptureSelection(void);
	Graphics::TBitmap* __fastcall CapturePolygon(void);
	Graphics::TBitmap* __fastcall CaptureIcon(void);
	Graphics::TBitmap* __fastcall CaptureSpecificSizeSelection(int CaptureWidth, int CaptureHeight);
	Graphics::TBitmap* __fastcall CaptureSpecificRegion(int CaptureX, int CaptureY, int CaptureWidth, int CaptureHeight);
	Graphics::TBitmap* __fastcall CaptureObjectByHWND(int Handles, AnsiString &scrapetext, BOOL onlygrabclient = false);
	Graphics::TBitmap* __fastcall CaptureObjectByTControlp(const Controls::TControl* tcontrolp, AnsiString &scrapetext);
	Graphics::TBitmap* __fastcall CaptureObjectByHWND_AutoScroll(int Handles, int scrollmode, AnsiString Exceptionstrings = "");
	Graphics::TBitmap* __fastcall CaptureObjectByTControlp_AutoScroll(const Controls::TControl* tcontrolp, int scrollmode, AnsiString Exceptionstrings = "");
	Graphics::TBitmap* __fastcall CaptureSpecificRegionPure(int CaptureX, int CaptureY, int CaptureWidth, int CaptureHeight);
	int __fastcall MarkDc(HDC ScreenDc, int left, int top, int CapW, int CapH, HWND canvas);
	int __fastcall LocateMarkerOffsetVertical(Graphics::TBitmap* abitmap, int CapW, int CapH, int left, int top);
	int __fastcall LocateMarkerOffsetHorizontal(Graphics::TBitmap* abitmap, int CapW, int CapH, int left, int top);
	bool __fastcall IsBlankBitmap(Graphics::TBitmap* abitmap, int CapW, int CapH);
	void __fastcall ScrollWindow_FullLeft(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall ScrollWindow_FullTop(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall ScrollWindow_OneDown(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall ScrollWindow_OneRight(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall ScrollWindow_OneUp(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall ScrollWindow_OneLeft(int Handles, HDC DrawDC, int scrollmode, bool dorepaint);
	void __fastcall InducePartialRepaint(int Handles, HDC DrawDC);
	void __fastcall ForceWinRectRedraw(int Handles, const Types::TRect &CRect, HDC DrawDC);
	int __fastcall LetUserPicksWindowHandle(void);
	int __fastcall CalcWindowExceptions(AnsiString windowclassname, AnsiString exceptionstrings, int &x1o, int &y1o, int &x2o, int &y2o, int &scrollmode, bool &stoponnochange, AnsiString &extras);
	void __fastcall ParseDelimited(const Classes::TStrings* sl, const AnsiString value, const AnsiString delimiter);
	void __fastcall SimulateKeyDown(unsigned MyKey);
	void __fastcall SimulateKeyUp(unsigned MyKey);
	void __fastcall SimulateMouseEvent(unsigned MyKey);
	int __fastcall JR_GetSystemMetrics_YHScroll(void);
	int __fastcall JR_GetSystemMetrics_XVScroll(void);
	
__published:
	__property bool Auto = {read=fAutomatic, write=fAutomatic, default=1};
	__property int Delay = {read=fDelay, write=fDelay, default=500};
	__property bool Minimize = {read=fMinimized, write=fMinimized, default=1};
	__property bool ShowCursor = {read=fShowCursor, write=fShowCursor, default=1};
	__property bool ShowHint = {read=fShowHint, write=fShowHint, default=1};
	__property Classes::TNotifyEvent OnCapture = {read=fOnCapture, write=fOnCapture};
	__property AnsiString Version = {read=GetVersion, write=SetVersion, stored=false};
	__property int MonitorNum = {read=fMonitorNum, write=fMonitorNum, default=-1};
	__property int ObjectLeft = {read=fObjectLeft, write=fObjectLeft, default=0};
	__property int ObjectTop = {read=fObjectTop, write=fObjectTop, default=0};
	__property int ObjectWidth = {read=fObjectWidth, write=fObjectWidth, default=0};
	__property int ObjectHeight = {read=fObjectHeight, write=fObjectHeight, default=0};
	__property int ObjectRLeft = {read=fObjectRLeft, write=fObjectRLeft, default=0};
	__property int ObjectRTop = {read=fObjectRTop, write=fObjectRTop, default=0};
	__property int ObjectRWidth = {read=fObjectRWidth, write=fObjectRWidth, default=0};
	__property int ObjectRHeight = {read=fObjectRHeight, write=fObjectRHeight, default=0};
};


//-- var, const, procedure ---------------------------------------------------
#define ASG_COMPONENT_VERSION "4.1"
extern PACKAGE int GlobalSleepingTime;
extern PACKAGE int MinGlobalSleepingTime;
extern PACKAGE int MaxGlobalSleepingTime;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Asgcapture */
using namespace Asgcapture;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ASGCapture
