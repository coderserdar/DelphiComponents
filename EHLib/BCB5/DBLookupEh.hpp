// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBLookupEh.pas' rev: 5.00

#ifndef DBLookupEhHPP
#define DBLookupEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ImgList.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <DBLookupGridsEh.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ToolCtrlsEh.hpp>	// Pascal unit
#include <DBCtrlsEh.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
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

namespace Dblookupeh
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TLookupComboboxDropDownBoxEh;
class PASCALIMPLEMENTATION TLookupComboboxDropDownBoxEh : public Dbgrideh::TColumnDropDownBoxEh 
{
	typedef Dbgrideh::TColumnDropDownBoxEh inherited;
	
__published:
	__property Align ;
	__property AutoDrop ;
	__property Rows ;
	__property ShowTitles ;
	__property Sizable ;
	__property SpecRow ;
	__property Width ;
public:
	#pragma option push -w-inl
	/* TColumnDropDownBoxEh.Create */ inline __fastcall TLookupComboboxDropDownBoxEh(Classes::TPersistent* 
		Owner) : Dbgrideh::TColumnDropDownBoxEh(Owner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TColumnDropDownBoxEh.Destroy */ inline __fastcall virtual ~TLookupComboboxDropDownBoxEh(void) { }
		
	#pragma option pop
	
};


class DELPHICLASS TDataSourceLinkEh;
class DELPHICLASS TCustomDBLookupComboboxEh;
class DELPHICLASS TListSourceLinkEh;
class PASCALIMPLEMENTATION TListSourceLinkEh : public Db::TDataLink 
{
	typedef Db::TDataLink inherited;
	
private:
	TCustomDBLookupComboboxEh* FDBLookupControl;
	
protected:
	virtual void __fastcall ActiveChanged(void);
	virtual void __fastcall DataSetChanged(void);
	virtual void __fastcall LayoutChanged(void);
	
public:
	__fastcall TListSourceLinkEh(void);
public:
	#pragma option push -w-inl
	/* TDataLink.Destroy */ inline __fastcall virtual ~TListSourceLinkEh(void) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TDBLookupComboboxEhStyle { csDropDownListEh, csDropDownEh };
#pragma option pop

class PASCALIMPLEMENTATION TCustomDBLookupComboboxEh : public Dbctrlseh::TCustomDBEditEh 
{
	typedef Dbctrlseh::TCustomDBEditEh inherited;
	
private:
	DynamicArray<Db::TField* >  FDataFields;
	AnsiString FDataFieldName;
	bool FDataFieldsUpdating;
	Dblookupgridseh::TPopupDataGridEh* FDataList;
	TLookupComboboxDropDownBoxEh* FDropDownBox;
	bool FInternalTextSetting;
	DynamicArray<Db::TField* >  FKeyFields;
	AnsiString FKeyFieldName;
	bool FKeyTextIndependent;
	Variant FKeyValue;
	bool FListActive;
	bool FListColumnMothed;
	Db::TField* FListField;
	int FListFieldIndex;
	AnsiString FListFieldName;
	Classes::TList* FListFields;
	TListSourceLinkEh* FListLink;
	Db::TDataSource* FListSource;
	bool FListVisible;
	bool FLockUpdateKeyTextIndependent;
	bool FLookupMode;
	Db::TDataSource* FLookupSource;
	DynamicArray<Db::TField* >  FMasterFields;
	AnsiString FMasterFieldNames;
	Toolctrlseh::TCloseUpEventEh FOnCloseUp;
	Classes::TNotifyEvent FOnDropDown;
	Classes::TNotifyEvent FOnKeyValueChanged;
	Toolctrlseh::TNotInListEventEh FOnNotInList;
	TDBLookupComboboxEhStyle FStyle;
	bool FTextBeenChanged;
	TDataSourceLinkEh* __fastcall GetDataLink(void);
	AnsiString __fastcall GetKeyFieldName();
	Db::TDataSource* __fastcall GetListSource(void);
	Toolctrlseh::TButtonClickEventEh __fastcall GetOnButtonClick();
	Toolctrlseh::TButtonDownEventEh __fastcall GetOnButtonDown();
	Dbgrideh::TCheckTitleEhBtnEvent __fastcall GetOnDropDownBoxCheckButton();
	Dbgrideh::TDrawColumnEhCellEvent __fastcall GetOnDropDownBoxDrawColumnCell();
	Dbgrideh::TGetCellEhParamsEvent __fastcall GetOnDropDownBoxGetCellParams();
	Classes::TNotifyEvent __fastcall GetOnDropDownBoxSortMarkingChanged();
	Dbgrideh::TTitleEhClickEvent __fastcall GetOnDropDownBoxTitleBtnClick();
	void __fastcall CheckNotCircular(void);
	void __fastcall CheckNotLookup(void);
	HIDESBASE MESSAGE void __fastcall CMCancelMode(Controls::TCMCancelMode &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseWheel(Controls::TCMMouseWheel &Message);
	HIDESBASE MESSAGE void __fastcall CMWantSpecialKey(Messages::TWMKey &Message);
	void __fastcall DataListKeyValueChanged(System::TObject* Sender);
	MESSAGE void __fastcall EMReplacesel(Messages::TMessage &Message);
	void __fastcall ListMouseCloseUp(System::TObject* Sender, bool Accept);
	void __fastcall ListColumnMoved(System::TObject* Sender, int FromIndex, int ToIndex);
	void __fastcall SetDataFieldName(const AnsiString Value);
	void __fastcall SetDropDownBox(const TLookupComboboxDropDownBoxEh* Value);
	void __fastcall SetKeyFieldName(const AnsiString Value);
	void __fastcall SetKeyValue(const Variant &Value);
	void __fastcall SetListFieldName(const AnsiString Value);
	void __fastcall SetListSource(Db::TDataSource* Value);
	void __fastcall SetLookupMode(bool Value);
	void __fastcall SetOnButtonClick(const Toolctrlseh::TButtonClickEventEh Value);
	void __fastcall SetOnButtonDown(const Toolctrlseh::TButtonDownEventEh Value);
	void __fastcall SetOnDropDownBoxCheckButton(const Dbgrideh::TCheckTitleEhBtnEvent Value);
	void __fastcall SetOnDropDownBoxDrawColumnCell(const Dbgrideh::TDrawColumnEhCellEvent Value);
	void __fastcall SetOnDropDownBoxGetCellParams(const Dbgrideh::TGetCellEhParamsEvent Value);
	void __fastcall SetOnDropDownBoxSortMarkingChanged(const Classes::TNotifyEvent Value);
	void __fastcall SetOnDropDownBoxTitleBtnClick(const Dbgrideh::TTitleEhClickEvent Value);
	void __fastcall SetStyle(const TDBLookupComboboxEhStyle Value);
	void __fastcall UpdateKeyTextIndependent(void);
	void __fastcall UpdateReadOnly(void);
	HIDESBASE MESSAGE void __fastcall WMChar(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMCut(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Message);
	
protected:
	virtual bool __fastcall ButtonEnabled(void);
	virtual bool __fastcall CanModify(bool TryEdit);
	virtual Dbctrlseh::TFieldDataLinkEh* __fastcall CreateDataLink(void);
	virtual Toolctrlseh::TEditButtonEh* __fastcall CreateEditButton(void);
	virtual bool __fastcall CompatibleVarValue(Toolctrlseh::TFieldsArrEh AFieldsArr, const Variant &AVlaue
		);
	virtual Classes::TAlignment __fastcall DefaultAlignment(void);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	HIDESBASE Db::TField* __fastcall GetDataField(void);
	virtual AnsiString __fastcall GetDisplayTextForPaintCopy();
	virtual int __fastcall GetListFieldsWidth(void);
	virtual Variant __fastcall GetVariantValue();
	virtual bool __fastcall IsValidChar(char InputChar);
	virtual bool __fastcall LocateStr(AnsiString Str, bool PartialKey);
	virtual bool __fastcall LocateDataSourceKey(Db::TDataSource* DataSource);
	virtual bool __fastcall SpecListMode(void);
	Db::TDataSource* __fastcall FullListSource(void);
	bool __fastcall TraceMouseMoveForPopupListbox(System::TObject* Sender, Classes::TShiftState Shift, 
		int X, int Y);
	Db::TDataSource* __fastcall UsedListSource(void);
	virtual void __fastcall ActiveChanged(void);
	virtual void __fastcall ButtonDown(bool IsDownButton);
	DYNAMIC void __fastcall Click(void);
	virtual void __fastcall DataChanged(void);
	virtual void __fastcall EditButtonMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int 
		X, int Y);
	virtual void __fastcall InternalSetText(AnsiString AText);
	virtual void __fastcall InternalSetValue(const Variant &AValue);
	void __fastcall HookOnChangeEvent(System::TObject* Sender);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	virtual void __fastcall KeyValueChanged(void);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall ListLinkDataChanged(void);
	virtual void __fastcall Loaded(void);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	virtual void __fastcall ProcessSearchStr(AnsiString Str);
	virtual void __fastcall SelectKeyValue(const Variant &Value);
	HIDESBASE void __fastcall SetEditText(AnsiString Value);
	virtual void __fastcall SetFocused(bool Value);
	virtual void __fastcall SpecRowChanged(System::TObject* Sender);
	virtual void __fastcall UpdateDataFields(void);
	virtual void __fastcall UpdateListFields(void);
	virtual void __fastcall UpdateListLinkDataSource(void);
	__property TDataSourceLinkEh* DataLink = {read=GetDataLink};
	__property bool ListActive = {read=FListActive, nodefault};
	__property Classes::TList* ListFields = {read=FListFields};
	__property TListSourceLinkEh* ListLink = {read=FListLink};
	__property Toolctrlseh::TButtonClickEventEh OnButtonClick = {read=GetOnButtonClick, write=SetOnButtonClick
		};
	__property Toolctrlseh::TButtonDownEventEh OnButtonDown = {read=GetOnButtonDown, write=SetOnButtonDown
		};
	void __fastcall SetDropDownBoxListSource(Db::TDataSource* AListSource);
	Dbgrideh::TCustomDBGridEh* __fastcall GetLookupGrid(void);
	Dbgrideh::TDBLookupGridEhOptions __fastcall GetOptions(void);
	void __fastcall SetOptions(Dbgrideh::TDBLookupGridEhOptions Value);
	
public:
	__fastcall virtual TCustomDBLookupComboboxEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBLookupComboboxEh(void);
	virtual bool __fastcall LocateKey(void);
	void __fastcall ClearDataProps(void);
	virtual void __fastcall CloseUp(bool Accept);
	virtual void __fastcall DefaultHandler(void *Message);
	virtual void __fastcall DropDown(void);
	HIDESBASE virtual void __fastcall SelectAll(void);
	void __fastcall SelectNextValue(bool IsPrior);
	virtual void __fastcall UpdateData(void);
	__property AnsiString DataField = {read=FDataFieldName, write=SetDataFieldName};
	__property Dblookupgridseh::TPopupDataGridEh* DataList = {read=FDataList};
	__property TLookupComboboxDropDownBoxEh* DropDownBox = {read=FDropDownBox, write=SetDropDownBox};
	__property Db::TField* Field = {read=GetDataField};
	__property AnsiString KeyField = {read=GetKeyFieldName, write=SetKeyFieldName};
	__property Variant KeyValue = {read=FKeyValue, write=SelectKeyValue};
	__property AnsiString ListField = {read=FListFieldName, write=SetListFieldName};
	__property int ListFieldIndex = {read=FListFieldIndex, write=FListFieldIndex, default=0};
	__property Db::TDataSource* ListSource = {read=GetListSource, write=SetListSource};
	__property bool ListVisible = {read=FListVisible, nodefault};
	__property TDBLookupComboboxEhStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property Text ;
	__property Toolctrlseh::TCloseUpEventEh OnCloseUp = {read=FOnCloseUp, write=FOnCloseUp};
	__property Classes::TNotifyEvent OnDropDown = {read=FOnDropDown, write=FOnDropDown};
	__property Classes::TNotifyEvent OnKeyValueChanged = {read=FOnKeyValueChanged, write=FOnKeyValueChanged
		};
	__property Toolctrlseh::TNotInListEventEh OnNotInList = {read=FOnNotInList, write=FOnNotInList};
	__property Dbgrideh::TCheckTitleEhBtnEvent OnDropDownBoxCheckButton = {read=GetOnDropDownBoxCheckButton
		, write=SetOnDropDownBoxCheckButton};
	__property Dbgrideh::TDrawColumnEhCellEvent OnDropDownBoxDrawColumnCell = {read=GetOnDropDownBoxDrawColumnCell
		, write=SetOnDropDownBoxDrawColumnCell};
	__property Dbgrideh::TGetCellEhParamsEvent OnDropDownBoxGetCellParams = {read=GetOnDropDownBoxGetCellParams
		, write=SetOnDropDownBoxGetCellParams};
	__property Classes::TNotifyEvent OnDropDownBoxSortMarkingChanged = {read=GetOnDropDownBoxSortMarkingChanged
		, write=SetOnDropDownBoxSortMarkingChanged};
	__property Dbgrideh::TTitleEhClickEvent OnDropDownBoxTitleBtnClick = {read=GetOnDropDownBoxTitleBtnClick
		, write=SetOnDropDownBoxTitleBtnClick};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBLookupComboboxEh(HWND ParentWindow) : Dbctrlseh::TCustomDBEditEh(
		ParentWindow) { }
	#pragma option pop
	
private:
	void *__ILookupGridOwner;	/* Dbgrideh::ILookupGridOwner [SetListSource=SetDropDownBoxListSource] */
		
	
public:
	operator ILookupGridOwner*(void) { return (ILookupGridOwner*)&__ILookupGridOwner; }
	
};


class PASCALIMPLEMENTATION TDataSourceLinkEh : public Dbctrlseh::TFieldDataLinkEh 
{
	typedef Dbctrlseh::TFieldDataLinkEh inherited;
	
private:
	bool FDataIndependentValueAsText;
	TCustomDBLookupComboboxEh* FDBLookupControl;
	
protected:
	__fastcall TDataSourceLinkEh(void);
	virtual void __fastcall RecordChanged(Db::TField* Field);
	virtual void __fastcall LayoutChanged(void);
public:
	#pragma option push -w-inl
	/* TDataLink.Destroy */ inline __fastcall virtual ~TDataSourceLinkEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TDBLookupComboboxEh;
class PASCALIMPLEMENTATION TDBLookupComboboxEh : public TCustomDBLookupComboboxEh 
{
	typedef TCustomDBLookupComboboxEh inherited;
	
__published:
	__property Alignment ;
	__property AlwaysShowBorder ;
	__property AutoSelect ;
	__property AutoSize ;
	__property BorderStyle ;
	__property Anchors ;
	__property BiDiMode ;
	__property Constraints ;
	__property DragKind ;
	__property Images ;
	__property ParentBiDiMode ;
	__property OnEndDock ;
	__property OnStartDock ;
	__property OnContextPopup ;
	__property Color ;
	__property Ctl3D ;
	__property DataField ;
	__property DataSource ;
	__property DragCursor ;
	__property DragMode ;
	__property DropDownBox ;
	__property Enabled ;
	__property EditButton ;
	__property EditButtons ;
	__property Font ;
	__property Flat ;
	__property ImeMode ;
	__property ImeName ;
	__property KeyField ;
	__property ListField ;
	__property ListFieldIndex ;
	__property ListSource ;
	__property ParentColor ;
	__property ParentCtl3D ;
	__property ParentFont ;
	__property ParentShowHint ;
	__property PopupMenu ;
	__property ReadOnly ;
	__property ShowHint ;
	__property Style ;
	__property TabOrder ;
	__property TabStop ;
	__property Visible ;
	__property WordWrap ;
	__property OnButtonClick ;
	__property OnButtonDown ;
	__property OnChange ;
	__property OnClick ;
	__property OnCloseUp ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnDropDown ;
	__property OnDropDownBoxCheckButton ;
	__property OnDropDownBoxDrawColumnCell ;
	__property OnDropDownBoxGetCellParams ;
	__property OnDropDownBoxSortMarkingChanged ;
	__property OnDropDownBoxTitleBtnClick ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnGetImageIndex ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnKeyValueChanged ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnNotInList ;
	__property OnUpdateData ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomDBLookupComboboxEh.Create */ inline __fastcall virtual TDBLookupComboboxEh(Classes::TComponent* 
		AOwner) : TCustomDBLookupComboboxEh(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomDBLookupComboboxEh.Destroy */ inline __fastcall virtual ~TDBLookupComboboxEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBLookupComboboxEh(HWND ParentWindow) : TCustomDBLookupComboboxEh(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dblookupeh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dblookupeh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DBLookupEh
