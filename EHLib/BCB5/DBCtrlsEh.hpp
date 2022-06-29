// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBCtrlsEh.pas' rev: 5.00

#ifndef DBCtrlsEhHPP
#define DBCtrlsEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Consts.hpp>	// Pascal unit
#include <ActnList.hpp>	// Pascal unit
#include <ToolCtrlsEh.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <Contnrs.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dbctrlseh
{
//-- type declarations -------------------------------------------------------
__interface IInplaceEditHolderEh;
typedef System::DelphiInterface<IInplaceEditHolderEh> _di_IInplaceEditHolderEh;
__interface INTERFACE_UUID("{4BE708F1-4EA2-4AC7-BA64-89D7D2B83E09}") IInplaceEditHolderEh  : public IUnknown 
	
{
	
public:
	virtual bool __fastcall InplaceEditCanModify(Controls::TWinControl* Control) = 0 ;
	virtual void __fastcall GetMouseDownInfo(Windows::TPoint &Pos, int &Time) = 0 ;
	virtual void __fastcall InplaceEditWndProc(Controls::TWinControl* Control, Messages::TMessage &Message
		) = 0 ;
	virtual void __fastcall InplaceEditKeyDown(Controls::TWinControl* Control, Word &Key, Classes::TShiftState 
		Shift) = 0 ;
	virtual void __fastcall InplaceEditKeyPress(Controls::TWinControl* Control, char &Key) = 0 ;
	virtual void __fastcall InplaceEditKeyUp(Controls::TWinControl* Control, Word &Key, Classes::TShiftState 
		Shift) = 0 ;
};

__interface IInplaceEditEh;
typedef System::DelphiInterface<IInplaceEditEh> _di_IInplaceEditEh;
__interface INTERFACE_UUID("{81F0C558-B001-4477-BAA6-2DC373FCDF88}") IInplaceEditEh  : public IUnknown 
	
{
	
public:
	virtual Graphics::TFont* __fastcall GetFont(void) = 0 ;
	virtual void __fastcall SetInplaceEditHolder(Controls::TWinControl* AInplaceEditHolder) = 0 ;
	virtual void __fastcall SetBorderStyle(Forms::TBorderStyle ABorderStyle) = 0 ;
	virtual void __fastcall SetFont(Graphics::TFont* AFont) = 0 ;
	virtual void __fastcall SetColor(Graphics::TColor AColor) = 0 ;
	virtual void __fastcall SetOnKeyPress(Controls::TKeyPressEvent AKeyPressEvent) = 0 ;
	virtual void __fastcall SetOnExit(Classes::TNotifyEvent AKeyPressEvent) = 0 ;
};

class DELPHICLASS TEditImageEh;
class PASCALIMPLEMENTATION TEditImageEh : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	Controls::TWinControl* FEditControl;
	int FImageIndex;
	Imglist::TCustomImageList* FImages;
	bool FUseImageHeight;
	bool FVisible;
	int FWidth;
	void __fastcall SetImageIndex(const int Value);
	void __fastcall SetImages(const Imglist::TCustomImageList* Value);
	void __fastcall SetUseImageHeight(const bool Value);
	void __fastcall SetVisible(const bool Value);
	void __fastcall SetWidth(const int Value);
	
public:
	__fastcall TEditImageEh(Controls::TWinControl* EditControl);
	__fastcall virtual ~TEditImageEh(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property Imglist::TCustomImageList* Images = {read=FImages, write=SetImages};
	__property bool UseImageHeight = {read=FUseImageHeight, write=SetUseImageHeight, default=1};
	__property bool Visible = {read=FVisible, write=SetVisible, default=0};
	__property int Width = {read=FWidth, write=SetWidth, default=0};
};


class DELPHICLASS TFieldDataLinkEh;
class PASCALIMPLEMENTATION TFieldDataLinkEh : public Db::TDataLink 
{
	typedef Db::TDataLink inherited;
	
private:
	DynamicArray<Db::TField* >  FFields;
	AnsiString FFieldName;
	Classes::TComponent* FControl;
	Classes::TNotifyEvent FOnDataChange;
	Classes::TNotifyEvent FOnEditingChange;
	Classes::TNotifyEvent FOnUpdateData;
	Classes::TNotifyEvent FOnActiveChange;
	bool FMultiFields;
	bool FDataIndepended;
	bool FEditing;
	bool FModified;
	bool __fastcall GetActive(void);
	bool __fastcall GetCanModify(void);
	bool __fastcall GetDataSetActive(void);
	Db::TDataSource* __fastcall GetDataSource(void);
	Db::TField* __fastcall GetField(void);
	int __fastcall GetFieldsCount(void);
	Db::TField* __fastcall GetFieldsField(int Index);
	HIDESBASE void __fastcall SetDataSource(const Db::TDataSource* Value);
	HIDESBASE void __fastcall SetEditing(bool Value);
	void __fastcall SetField(Contnrs::TObjectList* Value);
	void __fastcall SetFieldName(const AnsiString Value);
	void __fastcall SetMultiFields(const bool Value);
	void __fastcall UpdateField(void);
	void __fastcall UpdateRightToLeft(void);
	
protected:
	bool __fastcall FieldFound(Db::TField* Value);
	virtual void __fastcall ActiveChanged(void);
	virtual void __fastcall DataEvent(Db::TDataEvent Event, int Info);
	virtual void __fastcall EditingChanged(void);
	virtual void __fastcall FocusControl(Db::TFieldRef Field);
	virtual void __fastcall LayoutChanged(void);
	virtual void __fastcall RecordChanged(Db::TField* Field);
	virtual void __fastcall UpdateData(void);
	void __fastcall UpdateDataIndepended(void);
	
public:
	Variant DataIndependentValue;
	__fastcall TFieldDataLinkEh(void);
	HIDESBASE bool __fastcall Edit(void);
	void __fastcall Modified(void);
	void __fastcall SetModified(bool Value);
	void __fastcall SetText(AnsiString Text);
	void __fastcall SetValue(const Variant &Value);
	void __fastcall Reset(void);
	__property bool Active = {read=GetActive, nodefault};
	__property bool CanModify = {read=GetCanModify, nodefault};
	__property Classes::TComponent* Control = {read=FControl, write=FControl};
	__property bool DataIndepended = {read=FDataIndepended, nodefault};
	__property bool DataSetActive = {read=GetDataSetActive, nodefault};
	__property Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool Editing = {read=FEditing, nodefault};
	__property Db::TField* Field = {read=GetField};
	__property AnsiString FieldName = {read=FFieldName, write=SetFieldName};
	__property Db::TField* Fields[int Index] = {read=GetFieldsField};
	__property int FieldsCount = {read=GetFieldsCount, nodefault};
	__property bool MultiFields = {read=FMultiFields, write=SetMultiFields, nodefault};
	__property Classes::TNotifyEvent OnActiveChange = {read=FOnActiveChange, write=FOnActiveChange};
	__property Classes::TNotifyEvent OnDataChange = {read=FOnDataChange, write=FOnDataChange};
	__property Classes::TNotifyEvent OnEditingChange = {read=FOnEditingChange, write=FOnEditingChange};
		
	__property Classes::TNotifyEvent OnUpdateData = {read=FOnUpdateData, write=FOnUpdateData};
public:
	#pragma option push -w-inl
	/* TDataLink.Destroy */ inline __fastcall virtual ~TFieldDataLinkEh(void) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TGetImageIndexEventEh)(System::TObject* Sender, int &ImageIndex)
	;

#pragma option push -b-
enum TDBEditEhValue { evAlignmentEh, evEditMaskEh };
#pragma option pop

typedef Set<TDBEditEhValue, evAlignmentEh, evEditMaskEh>  TDBEditEhValues;

class DELPHICLASS TCustomDBEditEh;
class PASCALIMPLEMENTATION TCustomDBEditEh : public Mask::TCustomMaskEdit 
{
	typedef Mask::TCustomMaskEdit inherited;
	
private:
	bool FAlwaysShowBorder;
	TDBEditEhValues FAssignedValues;
	Controls::TControlCanvas* FCanvas;
	AnsiString FCompleteKeyPress;
	Toolctrlseh::TEditButtonEh* FEditButton;
	Toolctrlseh::TEditButtonsEh* FEditButtons;
	TEditImageEh* FEditImage;
	bool FFlat;
	Toolctrlseh::TMRUListEh* FMRUList;
	Controls::TWinControl* FMRUListControl;
	Toolctrlseh::TButtonClickEventEh FOnButtonClick;
	Toolctrlseh::TButtonDownEventEh FOnButtonDown;
	Toolctrlseh::TUpdateDataEventEh FOnUpdateData;
	bool FReadOnly;
	bool FWantTabs;
	bool FWantReturns;
	bool FWordWrap;
	TGetImageIndexEventEh FOnGetImageIndex;
	Classes::TAlignment __fastcall GetAlignment(void);
	bool __fastcall GetAutoSize(void);
	AnsiString __fastcall GetEditMask();
	Db::TField* __fastcall GetField(void);
	Controls::TWinControl* __fastcall GetMRUListControl(void);
	char __fastcall GetPasswordChar(void);
	bool __fastcall GetReadOnly(void);
	HIDESBASE AnsiString __fastcall GetText();
	Windows::TPoint __fastcall GetTextMargins();
	Variant __fastcall GetValue();
	bool __fastcall GetVisible(void);
	Windows::TRect __fastcall ImageRect();
	bool __fastcall IsAlignmentStored(void);
	bool __fastcall IsEditMaskStored(void);
	bool __fastcall IsTextStored(void);
	bool __fastcall IsValueStored(void);
	void __fastcall ActiveChange(System::TObject* Sender);
	HIDESBASE void __fastcall CheckCursor(void);
	MESSAGE void __fastcall CMCancelMode(Controls::TCMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogKey(Messages::TWMKey &Message);
	MESSAGE void __fastcall CMEditImageChangedEh(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall CMGetDataLink(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMRecreateWnd(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMShowingChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CNCommand(Messages::TWMCommand &Message);
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall DrawBorder(HDC DC, bool ActiveBorder);
	void __fastcall DrawEditImage(HDC DC);
	void __fastcall EditButtonChanged(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	void __fastcall InternalMove(const Windows::TRect &Loc, bool Redraw);
	void __fastcall InternalUpdateData(System::TObject* Sender);
	void __fastcall ReadEditMask(Classes::TReader* Reader);
	void __fastcall SetAlignment(const Classes::TAlignment Value);
	void __fastcall SetAlwaysShowBorder(const bool Value);
	void __fastcall SetEditButton(const Toolctrlseh::TEditButtonEh* Value);
	void __fastcall SetEditButtons(const Toolctrlseh::TEditButtonsEh* Value);
	void __fastcall SetEditImage(const TEditImageEh* Value);
	HIDESBASE void __fastcall SetEditMask(const AnsiString Value);
	void __fastcall SetEditRect(void);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetMRUList(const Toolctrlseh::TMRUListEh* Value);
	HIDESBASE void __fastcall SetPasswordChar(const char Value);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	HIDESBASE void __fastcall SetText(const AnsiString Value);
	void __fastcall SetValue(const Variant &Value);
	HIDESBASE void __fastcall SetVisible(const bool Value);
	void __fastcall SetWordWrap(const bool Value);
	void __fastcall UpdateDrawBorder(void);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMCut(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Messages::TWMSetFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	MESSAGE void __fastcall WMUndo(Messages::TWMNoParams &Message);
	void __fastcall WriteEditMask(Classes::TWriter* Writer);
	Imglist::TCustomImageList* __fastcall GetImages(void);
	void __fastcall SetImages(const Imglist::TCustomImageList* Value);
	void __fastcall SetOnGetImageIndex(const TGetImageIndexEventEh Value);
	
protected:
	Classes::TAlignment FAlignment;
	bool FBorderActive;
	int FButtonHeight;
	int FButtonWidth;
	TFieldDataLinkEh* FDataLink;
	bool FDataPosting;
	int FDownButton;
	bool FDroppedDown;
	DynamicArray<Toolctrlseh::TEditButtonControlLineRec >  FEditButtonControlList;
	bool FFocused;
	int FImageWidth;
	Controls::TWinControl* FInplaceEditHolder;
	bool FInplaceMode;
	_di_IInplaceEditHolderEh FIntfInplaceEditHolder;
	bool FMouseAboveControl;
	bool FNoClickCloseUp;
	bool FPressed;
	Windows::TRect FPressedRect;
	bool FUserTextChanged;
	virtual bool __fastcall ButtonEnabled(void);
	Windows::TRect __fastcall ButtonRect();
	virtual TFieldDataLinkEh* __fastcall CreateDataLink(void);
	virtual Toolctrlseh::TEditButtonEh* __fastcall CreateEditButton(void);
	virtual Toolctrlseh::TEditButtonControlEh* __fastcall CreateEditButtonControl(void);
	virtual Toolctrlseh::TEditButtonsEh* __fastcall CreateEditButtons(void);
	virtual TEditImageEh* __fastcall CreateEditImage(void);
	virtual Controls::TWinControl* __fastcall CreateMRUListControl(void);
	virtual bool __fastcall DataIndepended(void);
	virtual Classes::TAlignment __fastcall DefaultAlignment(void);
	virtual AnsiString __fastcall DefaultEditMask();
	virtual int __fastcall DefaultImageIndex(void);
	virtual bool __fastcall EditCanModify(void);
	Windows::TRect __fastcall EditRect();
	virtual AnsiString __fastcall GetDataField();
	virtual Db::TDataSource* __fastcall GetDataSource(void);
	virtual AnsiString __fastcall GetDisplayTextForPaintCopy();
	Toolctrlseh::TEditButtonEh* __fastcall GetEditButtonByShortCut(Classes::TShortCut ShortCut);
	Graphics::TFont* __fastcall GetFont(void);
	virtual Variant __fastcall GetVariantValue();
	virtual bool __fastcall IsValidChar(char InputChar);
	bool __fastcall PostDataEvent(void);
	virtual void __fastcall ActiveChanged(void);
	HIDESBASE virtual void __fastcall AdjustHeight(void);
	virtual void __fastcall ButtonDown(bool IsDownButton);
	virtual void __fastcall CalcEditRect(Windows::TRect &ARect);
	DYNAMIC void __fastcall Change(void);
	void __fastcall CheckInplaceEditHolderKeyDown(Word &Key, Classes::TShiftState Shift);
	void __fastcall CheckInplaceEditHolderKeyPress(char &Key);
	void __fastcall CheckInplaceEditHolderKeyUp(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall CloseUp(bool Accept);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DataChanged(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual void __fastcall DropDown(void);
	virtual void __fastcall EditButtonClick(System::TObject* Sender);
	virtual void __fastcall EditButtonDown(System::TObject* Sender, bool TopButton, bool &AutoRepeat, bool 
		&Handled);
	virtual void __fastcall EditButtonMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int 
		X, int Y);
	virtual void __fastcall EditButtonMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	virtual void __fastcall EditingChanged(void);
	virtual void __fastcall FilterMRUItem(AnsiString AText, bool &Accept);
	virtual void __fastcall InternalSetText(AnsiString AText);
	virtual void __fastcall InternalSetValue(const Variant &AValue);
	virtual void __fastcall InternalUpdatePostData(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall Loaded(void);
	void __fastcall MRUListCloseUp(System::TObject* Sender, bool Accept);
	void __fastcall MRUListControlMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	virtual void __fastcall MRUListControlResized(System::TObject* Sender);
	void __fastcall MRUListDropDown(System::TObject* Sender);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	virtual void __fastcall PaintWindow(HDC DC);
	virtual void __fastcall ResetMaxLength(void);
	HIDESBASE virtual void __fastcall SetAutoSize(bool Value);
	HIDESBASE void __fastcall SetBorderStyle(Forms::TBorderStyle ABorderStyle);
	HIDESBASE void __fastcall SetColor(Graphics::TColor AColor);
	void __fastcall SetControlEditMask(AnsiString Value);
	void __fastcall SetControlReadOnly(bool Value);
	virtual void __fastcall SetDataField(const AnsiString Value);
	virtual void __fastcall SetDataSource(Db::TDataSource* Value);
	virtual void __fastcall SetFocused(bool Value);
	HIDESBASE void __fastcall SetFont(Graphics::TFont* AFont);
	void __fastcall SetInplaceEditHolder(Controls::TWinControl* AInplaceEditHolder);
	void __fastcall SetOnExit(Classes::TNotifyEvent AKeyPressEvent);
	void __fastcall SetOnKeyPress(Controls::TKeyPressEvent AKeyPressEvent);
	virtual void __fastcall SetVariantValue(const Variant &VariantValue);
	virtual void __fastcall UpdateControlReadOnly(void);
	void __fastcall UpdateEditButtonControlList(void);
	void __fastcall UpdateEditButtonControlsState(void);
	HIDESBASE virtual void __fastcall UpdateHeight(void);
	virtual void __fastcall UpdateImageIndex(void);
	virtual void __fastcall UserChange(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property TDBEditEhValues AssignedValues = {read=FAssignedValues, nodefault};
	__property bool AutoSize = {read=GetAutoSize, write=SetAutoSize, default=1};
	__property Toolctrlseh::TEditButtonEh* EditButton = {read=FEditButton, write=SetEditButton};
	__property Toolctrlseh::TEditButtonsEh* EditButtons = {read=FEditButtons, write=SetEditButtons};
	__property TEditImageEh* EditImage = {read=FEditImage, write=SetEditImage};
	__property Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages};
	__property Toolctrlseh::TMRUListEh* MRUList = {read=FMRUList, write=SetMRUList};
	__property Controls::TWinControl* MRUListControl = {read=GetMRUListControl};
	__property char PasswordChar = {read=GetPasswordChar, write=SetPasswordChar, default=0};
	__property bool WantReturns = {read=FWantReturns, write=FWantReturns, default=0};
	__property bool WantTabs = {read=FWantTabs, write=FWantTabs, default=0};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
	__property Toolctrlseh::TButtonClickEventEh OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick
		};
	__property Toolctrlseh::TButtonDownEventEh OnButtonDown = {read=FOnButtonDown, write=FOnButtonDown}
		;
	__property TGetImageIndexEventEh OnGetImageIndex = {read=FOnGetImageIndex, write=SetOnGetImageIndex
		};
	
public:
	__fastcall virtual TCustomDBEditEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBEditEh(void);
	DYNAMIC bool __fastcall ExecuteAction(Classes::TBasicAction* Action);
	AnsiString __fastcall GetCompleteKeyPress();
	DYNAMIC bool __fastcall UpdateAction(Classes::TBasicAction* Action);
	DYNAMIC bool __fastcall UseRightToLeftAlignment(void);
	virtual void __fastcall DefaultHandler(void *Message);
	void __fastcall Deselect(void);
	HIDESBASE void __fastcall Hide(void);
	void __fastcall Move(const Windows::TRect &Loc);
	virtual void __fastcall Reset(void);
	virtual void __fastcall SetFocus(void);
	virtual void __fastcall UpdateData(void);
	void __fastcall UpdateLoc(const Windows::TRect &Loc);
	__property Classes::TAlignment Alignment = {read=GetAlignment, write=SetAlignment, stored=IsAlignmentStored
		, nodefault};
	__property bool AlwaysShowBorder = {read=FAlwaysShowBorder, write=SetAlwaysShowBorder, default=0};
	__property AnsiString DataField = {read=GetDataField, write=SetDataField};
	__property Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property AnsiString DisplayTextForPaintCopy = {read=GetDisplayTextForPaintCopy};
	__property AnsiString EditMask = {read=GetEditMask, write=SetEditMask, stored=false};
	__property Db::TField* Field = {read=GetField};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property AnsiString Text = {read=GetText, write=SetText, stored=IsTextStored};
	__property Variant Value = {read=GetValue, write=SetValue, stored=IsValueStored};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property Toolctrlseh::TUpdateDataEventEh OnUpdateData = {read=FOnUpdateData, write=FOnUpdateData}
		;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBEditEh(HWND ParentWindow) : Mask::TCustomMaskEdit(
		ParentWindow) { }
	#pragma option pop
	
private:
	void *__IInplaceEditEh;	/* Dbctrlseh::IInplaceEditEh */
	void *__IComboEditEh;	/* Toolctrlseh::IComboEditEh */
	
public:
	operator IUnknown*(void) { return (IUnknown*)&__IComboEditEh; }
	operator IComboEditEh*(void) { return (IComboEditEh*)&__IComboEditEh; }
	operator IInplaceEditEh*(void) { return (IInplaceEditEh*)&__IInplaceEditEh; }
	
};


class DELPHICLASS TDBEditEh;
class PASCALIMPLEMENTATION TDBEditEh : public TCustomDBEditEh 
{
	typedef TCustomDBEditEh inherited;
	
__published:
	__property Alignment ;
	__property AlwaysShowBorder ;
	__property Anchors ;
	__property AutoSelect ;
	__property AutoSize ;
	__property BiDiMode ;
	__property BorderStyle ;
	__property CharCase ;
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property DataField ;
	__property DataSource ;
	__property DragCursor ;
	__property DragKind ;
	__property DragMode ;
	__property EditButtons ;
	__property Enabled ;
	__property EditMask ;
	__property Font ;
	__property Flat ;
	__property Images ;
	__property ImeMode ;
	__property ImeName ;
	__property MaxLength ;
	__property MRUList ;
	__property ParentBiDiMode ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PasswordChar ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property TabOrder ;
	__property TabStop ;
	__property Text ;
	__property Visible ;
	__property WantTabs ;
	__property WantReturns ;
	__property WordWrap ;
	__property OnChange ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnUpdateData ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBEditEh.Create */ inline __fastcall virtual TDBEditEh(Classes::TComponent* AOwner) : TCustomDBEditEh(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBEditEh.Destroy */ inline __fastcall virtual ~TDBEditEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBEditEh(HWND ParentWindow) : TCustomDBEditEh(ParentWindow
		) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDateTimeKindEh { dtkDateEh, dtkTimeEh, dtkDateTimeEh, dtkCustomEh };
#pragma option pop

struct TElementMaskPosEh
{
	int Pos;
	int Length;
	bool Present;
} ;

struct TDateTimeElementsMaskPosEh
{
	TElementMaskPosEh Year;
	TElementMaskPosEh Month;
	TElementMaskPosEh Day;
	TElementMaskPosEh Hour;
	TElementMaskPosEh Min;
	TElementMaskPosEh Sec;
} ;

class DELPHICLASS TCustomDBDateTimeEditEh;
class PASCALIMPLEMENTATION TCustomDBDateTimeEditEh : public TCustomDBEditEh 
{
	typedef TCustomDBEditEh inherited;
	
private:
	bool FCalendarVisible;
	Controls::TWinControl* FDropDownCalendar;
	bool FEditValidating;
	bool FInternalTextSetting;
	TDateTimeKindEh FKind;
	Variant FValue;
	Toolctrlseh::TCloseUpEventEh FOnCloseUp;
	Classes::TNotifyEvent FOnDropDown;
	AnsiString FEditFormat;
	AnsiString FDateTimeFormat;
	Controls::TWinControl* __fastcall GetDropDownCalendar(void);
	bool __fastcall IsEditFormatStored(void);
	bool __fastcall IsKindStored(void);
	HIDESBASE MESSAGE void __fastcall CMCancelMode(Controls::TCMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	void __fastcall ReadEditFormat(Classes::TReader* Reader);
	void __fastcall SetEditFormat(const AnsiString Value);
	void __fastcall SetKind(const TDateTimeKindEh Value);
	void __fastcall UpdateValueFromText(void);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	void __fastcall WriteEditFormat(Classes::TWriter* Writer);
	
protected:
	TDateTimeElementsMaskPosEh FDateTimeMaskPos;
	bool FFourDigitYear;
	virtual Toolctrlseh::TEditButtonEh* __fastcall CreateEditButton(void);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	virtual AnsiString __fastcall GetDisplayTextForPaintCopy();
	virtual Variant __fastcall GetVariantValue();
	virtual void __fastcall ButtonDown(bool IsDownButton);
	DYNAMIC void __fastcall Change(void);
	virtual void __fastcall DataChanged(void);
	virtual void __fastcall DefineProperties(Classes::TFiler* Filer);
	virtual void __fastcall EditButtonMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int 
		X, int Y);
	virtual void __fastcall FilterMRUItem(AnsiString AText, bool &Accept);
	void __fastcall IncrementItemAtCurPos(bool IsIncrease);
	void __fastcall InternalSetControlText(AnsiString AText);
	virtual void __fastcall InternalSetText(AnsiString AText);
	virtual void __fastcall InternalSetValue(const Variant &AValue);
	virtual void __fastcall InternalUpdatePostData(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	virtual void __fastcall UpdateFourDigitYear(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property Controls::TWinControl* DropDownCalendar = {read=GetDropDownCalendar};
	
public:
	__fastcall virtual TCustomDBDateTimeEditEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBDateTimeEditEh(void);
	AnsiString __fastcall DateTimeFormat();
	virtual void __fastcall CloseUp(bool Accept);
	virtual void __fastcall DropDown(void);
	virtual void __fastcall UpdateMask(void);
	virtual void __fastcall ValidateEdit(void);
	__property bool CalendarVisible = {read=FCalendarVisible, nodefault};
	__property AnsiString EditFormat = {read=FEditFormat, write=SetEditFormat, stored=false};
	__property TDateTimeKindEh Kind = {read=FKind, write=SetKind, stored=IsKindStored, nodefault};
	__property Toolctrlseh::TCloseUpEventEh OnCloseUp = {read=FOnCloseUp, write=FOnCloseUp};
	__property Classes::TNotifyEvent OnDropDown = {read=FOnDropDown, write=FOnDropDown};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBDateTimeEditEh(HWND ParentWindow) : TCustomDBEditEh(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDBDateTimeEditEh;
class PASCALIMPLEMENTATION TDBDateTimeEditEh : public TCustomDBDateTimeEditEh 
{
	typedef TCustomDBDateTimeEditEh inherited;
	
__published:
	__property Alignment ;
	__property AlwaysShowBorder ;
	__property Anchors ;
	__property AutoSelect ;
	__property AutoSize ;
	__property BiDiMode ;
	__property BorderStyle ;
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property DataField ;
	__property DataSource ;
	__property DragCursor ;
	__property DragKind ;
	__property DragMode ;
	__property Enabled ;
	__property EditButton ;
	__property EditButtons ;
	__property EditFormat ;
	__property Font ;
	__property Flat ;
	__property Images ;
	__property ImeMode ;
	__property ImeName ;
	__property Kind ;
	__property MRUList ;
	__property ParentBiDiMode ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property TabOrder ;
	__property TabStop ;
	__property Value ;
	__property Visible ;
	__property OnButtonClick ;
	__property OnButtonDown ;
	__property OnChange ;
	__property OnClick ;
	__property OnCloseUp ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDropDown ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnUpdateData ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBDateTimeEditEh.Create */ inline __fastcall virtual TDBDateTimeEditEh(Classes::TComponent* 
		AOwner) : TCustomDBDateTimeEditEh(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBDateTimeEditEh.Destroy */ inline __fastcall virtual ~TDBDateTimeEditEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBDateTimeEditEh(HWND ParentWindow) : TCustomDBDateTimeEditEh(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDropDownBoxEh;
class PASCALIMPLEMENTATION TDropDownBoxEh : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	Dbctrls::TDropDownAlign FAlign;
	bool FAutoDrop;
	int FRows;
	bool FSizable;
	int FWidth;
	
public:
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property Dbctrls::TDropDownAlign Align = {read=FAlign, write=FAlign, default=0};
	__property bool AutoDrop = {read=FAutoDrop, write=FAutoDrop, default=0};
	__property int Rows = {read=FRows, write=FRows, default=7};
	__property bool Sizable = {read=FSizable, write=FSizable, default=0};
	__property int Width = {read=FWidth, write=FWidth, default=0};
public:
	#pragma option push -w-inl
	/* TPersistent.Destroy */ inline __fastcall virtual ~TDropDownBoxEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TDropDownBoxEh(void) : Classes::TPersistent() { }
	#pragma option pop
	
};


class DELPHICLASS TCustomDBComboBoxEh;
class PASCALIMPLEMENTATION TCustomDBComboBoxEh : public TCustomDBEditEh 
{
	typedef TCustomDBEditEh inherited;
	
private:
	TDropDownBoxEh* FDropDownBox;
	int FItemIndex;
	Classes::TStrings* FItems;
	Classes::TStrings* FKeyItems;
	bool FListVisible;
	Toolctrlseh::TNotInListEventEh FOnNotInList;
	Controls::TWinControl* FPopupListbox;
	Toolctrlseh::TCloseUpEventEh FOnCloseUp;
	Classes::TNotifyEvent FOnDropDown;
	Toolctrlseh::TListGetImageIndexEventEh FOnGetItemImageIndex;
	HIDESBASE Imglist::TCustomImageList* __fastcall GetImages(void);
	Controls::TWinControl* __fastcall GetPopupListbox(void);
	HIDESBASE MESSAGE void __fastcall CMCancelMode(Controls::TCMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	void __fastcall ItemsChanged(System::TObject* Sender);
	void __fastcall KeyItemsChanged(System::TObject* Sender);
	void __fastcall ListMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall SetDropDownBox(const TDropDownBoxEh* Value);
	HIDESBASE void __fastcall SetImages(const Imglist::TCustomImageList* Value);
	void __fastcall SetItemIndex(const int Value);
	void __fastcall SetItems(const Classes::TStrings* Value);
	void __fastcall SetKeyItems(const Classes::TStrings* Value);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Message);
	
protected:
	int FItemsCount;
	bool FKeyBased;
	Variant FVarValue;
	int FDefaultItemIndex;
	AnsiString __fastcall ConvertDataText(const AnsiString Value);
	virtual Toolctrlseh::TEditButtonEh* __fastcall CreateEditButton(void);
	virtual Classes::TAlignment __fastcall DefaultAlignment(void);
	virtual int __fastcall DefaultImageIndex(void);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	virtual AnsiString __fastcall GetDisplayTextForPaintCopy();
	virtual Variant __fastcall GetVariantValue();
	virtual bool __fastcall IsValidChar(char InputChar);
	virtual bool __fastcall LocateStr(AnsiString Str, bool PartialKey);
	virtual bool __fastcall ProcessSearchStr(AnsiString Str);
	bool __fastcall TextListIndepended(void);
	bool __fastcall TraceMouseMoveForPopupListbox(System::TObject* Sender, Classes::TShiftState Shift, 
		int X, int Y);
	virtual void __fastcall ButtonDown(bool IsDownButton);
	DYNAMIC void __fastcall Change(void);
	DYNAMIC void __fastcall Click(void);
	virtual void __fastcall DataChanged(void);
	virtual void __fastcall EditButtonClick(System::TObject* Sender);
	virtual void __fastcall EditButtonMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int 
		X, int Y);
	void __fastcall InternalSetItemIndex(const int Value);
	virtual void __fastcall InternalSetText(AnsiString AText);
	virtual void __fastcall InternalSetValue(const Variant &AValue);
	virtual void __fastcall InternalUpdatePostData(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall ResetMaxLength(void);
	void __fastcall PopupListboxGetImageIndex(System::TObject* Sender, int ItemIndex, int &ImageIndex);
		
	virtual void __fastcall SetVariantValue(const Variant &VariantValue);
	virtual void __fastcall UpdateControlReadOnly(void);
	void __fastcall UpdateItemIndex(void);
	virtual void __fastcall UpdateImageIndex(void);
	void __fastcall UpdateItems(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property Controls::TWinControl* PopupListbox = {read=GetPopupListbox};
	
public:
	__fastcall virtual TCustomDBComboBoxEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBComboBoxEh(void);
	virtual void __fastcall Clear(void);
	virtual void __fastcall CloseUp(bool Accept);
	virtual void __fastcall DefaultHandler(void *Message);
	virtual void __fastcall DropDown(void);
	void __fastcall SelectNextValue(bool IsPrior);
	virtual void __fastcall UpdateData(void);
	__property TDropDownBoxEh* DropDownBox = {read=FDropDownBox, write=SetDropDownBox};
	__property Imglist::TCustomImageList* Images = {read=GetImages, write=SetImages};
	__property int ItemIndex = {read=FItemIndex, write=SetItemIndex, nodefault};
	__property Classes::TStrings* Items = {read=FItems, write=SetItems};
	__property Classes::TStrings* KeyItems = {read=FKeyItems, write=SetKeyItems};
	__property bool ListVisible = {read=FListVisible, nodefault};
	__property Toolctrlseh::TCloseUpEventEh OnCloseUp = {read=FOnCloseUp, write=FOnCloseUp};
	__property Classes::TNotifyEvent OnDropDown = {read=FOnDropDown, write=FOnDropDown};
	__property Toolctrlseh::TNotInListEventEh OnNotInList = {read=FOnNotInList, write=FOnNotInList};
	__property Toolctrlseh::TListGetImageIndexEventEh OnGetItemImageIndex = {read=FOnGetItemImageIndex, 
		write=FOnGetItemImageIndex};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBComboBoxEh(HWND ParentWindow) : TCustomDBEditEh(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDBComboBoxEh;
class PASCALIMPLEMENTATION TDBComboBoxEh : public TCustomDBComboBoxEh 
{
	typedef TCustomDBComboBoxEh inherited;
	
__published:
	__property Alignment ;
	__property AlwaysShowBorder ;
	__property Anchors ;
	__property AutoSelect ;
	__property AutoSize ;
	__property BiDiMode ;
	__property BorderStyle ;
	__property CharCase ;
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property DataField ;
	__property DataSource ;
	__property DragCursor ;
	__property DragKind ;
	__property DragMode ;
	__property DropDownBox ;
	__property Enabled ;
	__property EditButton ;
	__property EditButtons ;
	__property EditMask ;
	__property Font ;
	__property Flat ;
	__property Images ;
	__property ImeMode ;
	__property ImeName ;
	__property Items ;
	__property KeyItems ;
	__property MaxLength ;
	__property MRUList ;
	__property ParentBiDiMode ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property TabOrder ;
	__property TabStop ;
	__property Text ;
	__property Visible ;
	__property WordWrap ;
	__property OnButtonClick ;
	__property OnButtonDown ;
	__property OnChange ;
	__property OnClick ;
	__property OnCloseUp ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDropDown ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnGetItemImageIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnNotInList ;
	__property OnUpdateData ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBComboBoxEh.Create */ inline __fastcall virtual TDBComboBoxEh(Classes::TComponent* AOwner
		) : TCustomDBComboBoxEh(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBComboBoxEh.Destroy */ inline __fastcall virtual ~TDBComboBoxEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBComboBoxEh(HWND ParentWindow) : TCustomDBComboBoxEh(
		ParentWindow) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDBNumberValue { evDisplayFormatEh, evCurrencyEh, evMaxValueEh, evMinValueEh };
#pragma option pop

typedef Set<TDBNumberValue, evDisplayFormatEh, evMinValueEh>  TDBNumberValues;

class DELPHICLASS TCustomDBNumberEditEh;
class PASCALIMPLEMENTATION TCustomDBNumberEditEh : public TCustomDBEditEh 
{
	typedef TCustomDBEditEh inherited;
	
private:
	TDBNumberValues FAssignedValues;
	bool FCalculatorVisible;
	bool FCurrency;
	unsigned FDecimalPlaces;
	AnsiString FDisplayFormat;
	Controls::TWinControl* FDropDownCalculator;
	AnsiString FEditFormat;
	Extended FIncrement;
	bool FInternalTextSetting;
	Extended FMinValue;
	Extended FMaxValue;
	Toolctrlseh::TCloseUpEventEh FOnCloseUp;
	Classes::TNotifyEvent FOnDropDown;
	Variant FValue;
	Extended __fastcall CheckValue(Extended NewValue);
	AnsiString __fastcall DisplayFormatToEditFormat(const AnsiString AFormat);
	bool __fastcall GetCurrency(void);
	AnsiString __fastcall GetDisplayFormat();
	Extended __fastcall GetMaxValue(void);
	Extended __fastcall GetMinValue(void);
	bool __fastcall IsCurrencyStored(void);
	bool __fastcall IsDisplayFormatStored(void);
	bool __fastcall IsIncrementStored(void);
	bool __fastcall IsMaxValueStored(void);
	bool __fastcall IsMinValueStored(void);
	AnsiString __fastcall TextToValText(const AnsiString AValue);
	HIDESBASE MESSAGE void __fastcall CMCancelMode(Controls::TCMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	void __fastcall SetCurrency(const bool Value);
	void __fastcall SetDecimalPlaces(unsigned Value);
	void __fastcall SetDisplayFormat(const AnsiString Value);
	void __fastcall SetMaxValue(Extended AValue);
	void __fastcall SetMinValue(Extended AValue);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Messages::TMessage &Message);
	
protected:
	virtual Toolctrlseh::TEditButtonEh* __fastcall CreateEditButton(void);
	virtual Classes::TAlignment __fastcall DefaultAlignment(void);
	bool __fastcall DefaultCurrency(void);
	AnsiString __fastcall DefaultDisplayFormat();
	Extended __fastcall DefaultMaxValue(void);
	Extended __fastcall DefaultMinValue(void);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	AnsiString __fastcall FormatDisplayText(Extended Value);
	virtual AnsiString __fastcall GetDisplayText();
	virtual Controls::TWinControl* __fastcall GetDropDownCalculator(void);
	virtual Variant __fastcall GetVariantValue();
	virtual bool __fastcall IsValidChar(char Key);
	virtual void __fastcall ButtonDown(bool IsDownButton);
	DYNAMIC void __fastcall Change(void);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DataChanged(void);
	void __fastcall InternalSetControlText(AnsiString AText);
	virtual void __fastcall InternalSetText(AnsiString AText);
	virtual void __fastcall InternalSetValue(const Variant &AValue);
	virtual void __fastcall InternalUpdatePostData(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall ReformatEditText(AnsiString NewText);
	void __fastcall UpdateValueFromText(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property TDBNumberValues AssignedValues = {read=FAssignedValues, nodefault};
	__property bool currency = {read=GetCurrency, write=SetCurrency, stored=IsCurrencyStored, nodefault
		};
	__property unsigned DecimalPlaces = {read=FDecimalPlaces, write=SetDecimalPlaces, default=2};
	__property AnsiString DisplayFormat = {read=GetDisplayFormat, write=SetDisplayFormat, stored=IsDisplayFormatStored
		};
	__property Controls::TWinControl* DropDownCalculator = {read=GetDropDownCalculator};
	__property Extended Increment = {read=FIncrement, write=FIncrement, stored=IsIncrementStored};
	__property MaxLength ;
	__property Extended MaxValue = {read=GetMaxValue, write=SetMaxValue, stored=IsMaxValueStored};
	__property Extended MinValue = {read=GetMinValue, write=SetMinValue, stored=IsMinValueStored};
	
public:
	__fastcall virtual TCustomDBNumberEditEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBNumberEditEh(void);
	virtual void __fastcall CloseUp(bool Accept);
	virtual void __fastcall DropDown(void);
	__property bool CalculatorVisible = {read=FCalculatorVisible, nodefault};
	void __fastcall IncrementValue(bool IsIncrease);
	__property AnsiString DisplayText = {read=GetDisplayText};
	__property Toolctrlseh::TCloseUpEventEh OnCloseUp = {read=FOnCloseUp, write=FOnCloseUp};
	__property Classes::TNotifyEvent OnDropDown = {read=FOnDropDown, write=FOnDropDown};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBNumberEditEh(HWND ParentWindow) : TCustomDBEditEh(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDBNumberEditEh;
class PASCALIMPLEMENTATION TDBNumberEditEh : public TCustomDBNumberEditEh 
{
	typedef TCustomDBNumberEditEh inherited;
	
__published:
	__property Alignment ;
	__property AlwaysShowBorder ;
	__property Anchors ;
	__property AutoSelect ;
	__property AutoSize ;
	__property BiDiMode ;
	__property BorderStyle ;
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property currency ;
	__property DataField ;
	__property DataSource ;
	__property DecimalPlaces ;
	__property DisplayFormat ;
	__property DragCursor ;
	__property DragKind ;
	__property DragMode ;
	__property Enabled ;
	__property EditButton ;
	__property EditButtons ;
	__property Font ;
	__property Flat ;
	__property Images ;
	__property ImeMode ;
	__property ImeName ;
	__property Increment ;
	__property MaxValue ;
	__property MinValue ;
	__property MRUList ;
	__property ParentBiDiMode ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PasswordChar ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property TabOrder ;
	__property TabStop ;
	__property Value ;
	__property Visible ;
	__property OnButtonClick ;
	__property OnButtonDown ;
	__property OnChange ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnUpdateData ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBNumberEditEh.Create */ inline __fastcall virtual TDBNumberEditEh(Classes::TComponent* AOwner
		) : TCustomDBNumberEditEh(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBNumberEditEh.Destroy */ inline __fastcall virtual ~TDBNumberEditEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBNumberEditEh(HWND ParentWindow) : TCustomDBNumberEditEh(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TCustomDBCheckBoxEh;
class PASCALIMPLEMENTATION TCustomDBCheckBoxEh : public Stdctrls::TButtonControl 
{
	typedef Stdctrls::TButtonControl inherited;
	
private:
	Classes::TAlignment FAlignment;
	bool FAllowGrayed;
	bool FAlwaysShowBorder;
	bool FClicksDisabled;
	TFieldDataLinkEh* FDataLink;
	bool FFlat;
	bool FModified;
	bool FMouseAboveControl;
	Toolctrlseh::TUpdateDataEventEh FOnUpdateData;
	Stdctrls::TCheckBoxState FState;
	AnsiString FValueCheck;
	AnsiString FValueUncheck;
	Graphics::TCanvas* FCanvas;
	HIDESBASE MESSAGE void __fastcall WMPaint(Messages::TWMPaint &Message);
	AnsiString __fastcall GetDataField();
	Db::TDataSource* __fastcall GetDataSource(void);
	Db::TField* __fastcall GetField(void);
	Stdctrls::TCheckBoxState __fastcall GetFieldState(void);
	bool __fastcall GetModified(void);
	bool __fastcall GetReadOnly(void);
	bool __fastcall IsStateStored(void);
	bool __fastcall ValueMatch(const AnsiString ValueList, const AnsiString Value);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMDialogChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	MESSAGE void __fastcall CMTextChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	MESSAGE void __fastcall CNCommand(Messages::TWMCommand &Message);
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall InternalUpdateData(System::TObject* Sender);
	void __fastcall SetAlignment(const Classes::TLeftRight Value);
	void __fastcall SetAlwaysShowBorder(const bool Value);
	void __fastcall SetDataField(const AnsiString Value);
	void __fastcall SetDataSource(Db::TDataSource* Value);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetReadOnly(bool Value);
	void __fastcall SetState(const Stdctrls::TCheckBoxState Value);
	void __fastcall SetValueCheck(const AnsiString Value);
	void __fastcall SetValueUncheck(const AnsiString Value);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Messages::TWMSetFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	
protected:
	bool FDataPosting;
	bool FToggleKeyDown;
	virtual void __fastcall Paint(void);
	virtual void __fastcall PaintWindow(HDC DC);
	__property Graphics::TCanvas* Canvas = {read=FCanvas};
	virtual bool __fastcall DataIndepended(void);
	virtual bool __fastcall GetChecked(void);
	bool __fastcall PostDataEvent(void);
	DYNAMIC void __fastcall Click(void);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DrawCaptionRect(const Windows::TRect &ARect, bool AFocused, bool AMouseAboveControl
		, bool ADown);
	virtual void __fastcall DrawCheckBoxRect(const Windows::TRect &ARect, Stdctrls::TCheckBoxState AState
		, bool AFocused, bool AMouseAboveControl, bool ADown);
	virtual void __fastcall DrawState(Stdctrls::TCheckBoxState AState, bool AFocused, bool AMouseAboveControl
		, bool ADown);
	virtual void __fastcall InternalSetState(Stdctrls::TCheckBoxState Value);
	virtual void __fastcall InternalUpdatePostData(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int 
		Y);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	virtual void __fastcall SetChecked(bool Value);
	virtual void __fastcall Toggle(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	__property bool ClicksDisabled = {read=FClicksDisabled, write=FClicksDisabled, nodefault};
	
public:
	__fastcall virtual TCustomDBCheckBoxEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBCheckBoxEh(void);
	DYNAMIC bool __fastcall ExecuteAction(Classes::TBasicAction* Action);
	DYNAMIC Classes::TAlignment __fastcall GetControlsAlignment(void);
	DYNAMIC bool __fastcall UpdateAction(Classes::TBasicAction* Action);
	DYNAMIC bool __fastcall UseRightToLeftAlignment(void);
	virtual void __fastcall UpdateData(void);
	__property Classes::TLeftRight Alignment = {read=FAlignment, write=SetAlignment, default=1};
	__property bool AllowGrayed = {read=FAllowGrayed, write=FAllowGrayed, default=0};
	__property bool AlwaysShowBorder = {read=FAlwaysShowBorder, write=SetAlwaysShowBorder, default=0};
	__property Checked ;
	__property AnsiString DataField = {read=GetDataField, write=SetDataField};
	__property Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Db::TField* Field = {read=GetField};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property bool Modified = {read=GetModified, nodefault};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property Stdctrls::TCheckBoxState State = {read=FState, write=SetState, stored=IsStateStored, nodefault
		};
	__property AnsiString ValueChecked = {read=FValueCheck, write=SetValueCheck};
	__property AnsiString ValueUnchecked = {read=FValueUncheck, write=SetValueUncheck};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBCheckBoxEh(HWND ParentWindow) : Stdctrls::TButtonControl(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDBCheckBoxEh;
class PASCALIMPLEMENTATION TDBCheckBoxEh : public TCustomDBCheckBoxEh 
{
	typedef TCustomDBCheckBoxEh inherited;
	
__published:
	__property Action ;
	__property Alignment ;
	__property AllowGrayed ;
	__property AlwaysShowBorder ;
	__property Anchors ;
	__property BiDiMode ;
	__property Caption ;
	__property Checked ;
	__property Color ;
	__property Constraints ;
	__property Ctl3D ;
	__property DataField ;
	__property DataSource ;
	__property DragCursor ;
	__property DragKind ;
	__property DragMode ;
	__property Enabled ;
	__property Flat ;
	__property Font ;
	__property ParentBiDiMode ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property State ;
	__property TabOrder ;
	__property TabStop ;
	__property ValueChecked ;
	__property ValueUnchecked ;
	__property Visible ;
	__property OnClick ;
	__property OnContextPopup ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBCheckBoxEh.Create */ inline __fastcall virtual TDBCheckBoxEh(Classes::TComponent* AOwner
		) : TCustomDBCheckBoxEh(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBCheckBoxEh.Destroy */ inline __fastcall virtual ~TDBCheckBoxEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBCheckBoxEh(HWND ParentWindow) : TCustomDBCheckBoxEh(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word CM_EDITIMAGECHANGEDEH = 0x465;
static const Word CM_IGNOREEDITDOWN = 0x466;

}	/* namespace Dbctrlseh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dbctrlseh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DBCtrlsEh
