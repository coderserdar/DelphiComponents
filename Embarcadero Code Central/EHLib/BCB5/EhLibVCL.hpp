// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'EhLibVCL.pas' rev: 5.00

#ifndef EhLibVCLHPP
#define EhLibVCLHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Messages.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <TypInfo.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Ehlibvcl
{
//-- type declarations -------------------------------------------------------
typedef Messages::TMessage  TCMParentFontChanged;

typedef void *IntPtr;

typedef DynamicArray<Byte >  TBytes;

typedef DynamicArray<Typinfo::PPropInfo >  TPropListArray;

class DELPHICLASS TFilerAccess;
class PASCALIMPLEMENTATION TFilerAccess : public System::TInterfacedObject 
{
	typedef System::TInterfacedObject inherited;
	
private:
	Classes::TPersistent* FPersistent;
	
public:
	__fastcall TFilerAccess(Classes::TPersistent* APersistent);
	void __fastcall DefineProperties(Classes::TFiler* AFiler);
	void __fastcall GetChildren(Classes::TGetChildProc Proc, Classes::TComponent* Root);
	Classes::TComponent* __fastcall GetChildOwner(void);
	Classes::TComponent* __fastcall GetChildParent(void);
	void __fastcall SetAncestor(bool Value);
	void __fastcall SetChildOrder(Classes::TComponent* Child, int Order);
	void __fastcall Updated(void);
	void __fastcall Updating(void);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TFilerAccess(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool __fastcall IsObjectAndIntegerRefSame(System::TObject* AObject, int IntRef);
extern PACKAGE System::TObject* __fastcall IntPtrToObject(int AIntPtr);
extern PACKAGE int __fastcall ObjectToIntPtr(System::TObject* AObject);
extern PACKAGE AnsiString __fastcall IntPtrToString(int AIntPtr);
extern PACKAGE void __fastcall FillDWord(void *Dest, int Count, int Value);
extern PACKAGE void * __fastcall StackAlloc(int Size);
extern PACKAGE void __fastcall StackFree(void * P);
extern PACKAGE int __fastcall DataSetCompareBookmarks(Db::TDataSet* DataSet, AnsiString Bookmark1, AnsiString 
	Bookmark2);
extern PACKAGE bool __fastcall DataSetBookmarkValid(Db::TDataSet* DataSet, AnsiString Bookmark);
extern PACKAGE Db::TDataSet* __fastcall GetMasterDataSet(Db::TDataSet* FDataSet, Typinfo::PPropInfo 
	APropInfo);
extern PACKAGE int __fastcall DrawTextEh(HDC hDC, AnsiString Text, int nCount, Windows::TRect &lpRect
	, unsigned uFormat);
extern PACKAGE int __fastcall WindowsDrawTextEx(HDC DC, AnsiString lpchText, int cchText, Windows::TRect 
	&p4, unsigned dwDTFormat, const tagDRAWTEXTPARAMS &DTParams)/* overload */;
extern PACKAGE int __fastcall WindowsDrawTextEx(HDC DC, AnsiString lpchText, int cchText, Windows::TRect 
	&p4, unsigned dwDTFormat)/* overload */;
extern PACKAGE BOOL __fastcall WindowsExtTextOut(HDC DC, int X, int Y, int Options, Windows::TRect &
	Rect, AnsiString Str, int Count);
extern PACKAGE unsigned __fastcall WindowsGetOutlineTextMetrics(HDC DC, unsigned p2, _OUTLINETEXTMETRICA 
	&OTMetricStructs);
extern PACKAGE int __fastcall SendStructMessage(HWND hWnd, unsigned Msg, int wParam, void *lParam);
extern PACKAGE int __fastcall SendTextMessage(HWND hWnd, unsigned Msg, int wParam, AnsiString lParam
	);
extern PACKAGE int __fastcall SendGetTextMessage(HWND hWnd, unsigned Msg, int wParam, AnsiString &lParam
	, int BufferSize);
extern PACKAGE BOOL __fastcall SystemParametersInfoEh(unsigned uiAction, unsigned uiParam, void *pvParam
	, unsigned fWinIni);
extern PACKAGE BOOL __fastcall WindowsInvalidateRect(HWND hWnd, Windows::TRect &Rect, BOOL bErase);
extern PACKAGE BOOL __fastcall WindowsValidateRect(HWND hWnd, Windows::TRect &Rect);
extern PACKAGE BOOL __fastcall WindowsScrollWindowEx(HWND hWnd, int dx, int dy, Windows::TRect &prcScroll
	, Windows::TRect &prcClip, HRGN hrgnUpdate, unsigned flags);
extern PACKAGE HWND __fastcall FindWindowEh(AnsiString lpClassName, AnsiString lpWindowName);
extern PACKAGE void __fastcall VarToMessage(void *VarMessage, Messages::TMessage &Message);
extern PACKAGE Messages::TMessage __fastcall MessageToTMessage(void *Message);
extern PACKAGE Messages::TWMMouse __fastcall MessageToTWMMouse(void *Message);
extern PACKAGE Messages::TWMKey __fastcall MessageToTWMKey(void *Message);
extern PACKAGE Messages::TMessage __fastcall UnwrapMessageEh(void *Message);
extern PACKAGE int __fastcall SmallPointToInteger(Windows::TSmallPoint SmallPoint);
extern PACKAGE BOOL __fastcall WindowsLPtoDP(HDC DC, Windows::TRect &ARect);
extern PACKAGE HRGN __fastcall WindowsCreatePolygonRgn(const Windows::TPoint * Points, const int Points_Size
	, int Count, int FillMode);
extern PACKAGE void __fastcall MessageSendGetSel(HWND hWnd, int &SelStart, int &SelEnd);
extern PACKAGE AnsiString __fastcall NlsUpperCase(const AnsiString S);
extern PACKAGE AnsiString __fastcall NlsLowerCase(const AnsiString S);
extern PACKAGE int __fastcall NlsCompareStr(const AnsiString S1, const AnsiString S2);
extern PACKAGE int __fastcall NlsCompareText(const AnsiString S1, const AnsiString S2);
extern PACKAGE void __fastcall BitmapLoadFromResourceName(Graphics::TBitmap* Bmp, unsigned Instance, 
	const AnsiString ResName);
extern PACKAGE HBITMAP __fastcall LoadBitmapEh(Windows::HINST hInstance, int lpBitmapID);
extern PACKAGE TPropListArray __fastcall GetPropListAsArray(Typinfo::PTypeInfo ATypeInfo, Typinfo::TTypeKinds 
	TypeKinds);
extern PACKAGE int __fastcall HexToBinEh(TBytes Text, TBytes &Buffer, int Count);
extern PACKAGE void __fastcall BinToHexEh(TBytes Buffer, AnsiString &Text, int Count);
extern PACKAGE void __fastcall StreamWriteBytes(Classes::TStream* Stream, TBytes Buffer);
extern PACKAGE void __fastcall StreamReadBytes(Classes::TStream* Stream, TBytes &Buffer, int Count);
	
extern PACKAGE TBytes __fastcall BytesOf(AnsiString S);
extern PACKAGE Typinfo::PTypeInfo __fastcall PropInfo_getPropType(Typinfo::PPropInfo APropInfo);
extern PACKAGE AnsiString __fastcall PropInfo_getName(Typinfo::PPropInfo APropInfo);
extern PACKAGE Typinfo::TTypeKind __fastcall PropType_getKind(Typinfo::PTypeInfo APropType);
extern PACKAGE void __fastcall VarArrayRedimEh(Variant &A, int HighBound);
extern PACKAGE System::TObject* __fastcall GetObjectProp(System::TObject* Instance, Typinfo::PPropInfo 
	PropInfo);
extern PACKAGE TMetaClass* __fastcall GetObjectPropClass(System::TObject* Instance, Typinfo::PPropInfo 
	PropInfo);
extern PACKAGE void __fastcall SetObjectProp(System::TObject* Instance, Typinfo::PPropInfo PropInfo, 
	System::TObject* Value);
extern PACKAGE Classes::TPersistent* __fastcall GetUltimateOwner(Classes::TPersistent* APersistent);
	

}	/* namespace Ehlibvcl */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Ehlibvcl;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EhLibVCL
