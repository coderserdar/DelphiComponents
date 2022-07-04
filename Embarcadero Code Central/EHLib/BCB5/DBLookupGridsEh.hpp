// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DBLookupGridsEh.pas' rev: 5.00

#ifndef DBLookupGridsEhHPP
#define DBLookupGridsEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Contnrs.hpp>	// Pascal unit
#include <ToolCtrlsEh.hpp>	// Pascal unit
#include <DBGridEh.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dblookupgridseh
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TLookupGridDataLinkEh;
class DELPHICLASS TDBLookupGridEh;
class PASCALIMPLEMENTATION TDBLookupGridEh : public Dbgrideh::TCustomDBGridEh 
{
	typedef Dbgrideh::TCustomDBGridEh inherited;
	
private:
	AnsiString FDataFieldName;
	DynamicArray<Db::TField* >  FDataFields;
	TLookupGridDataLinkEh* FDataLink;
	bool FHasFocus;
	AnsiString FKeyFieldName;
	DynamicArray<Db::TField* >  FKeyFields;
	bool FKeyRowVisible;
	bool FKeySelected;
	Variant FKeyValue;
	bool FListActive;
	Db::TField* FListField;
	int FListFieldIndex;
	AnsiString FListFieldName;
	Contnrs::TObjectList* FListFields;
	bool FLockPosition;
	bool FLookupMode;
	Db::TDataSource* FLookupSource;
	AnsiString FMasterFieldNames;
	DynamicArray<Db::TField* >  FMasterFields;
	int FMousePos;
	bool FPopup;
	int FRecordCount;
	int FRecordIndex;
	int FRowCount;
	AnsiString FSearchText;
	AnsiString FSelectedItem;
	Toolctrlseh::TSpecRowEh* FSpecRow;
	Dbgrideh::TDBLookupGridEhOptions FOptions;
	bool __fastcall GetAutoFitColWidths(void);
	Db::TField* __fastcall GetDataField(void);
	HIDESBASE Db::TDataSource* __fastcall GetDataSource(void);
	AnsiString __fastcall GetKeyFieldName();
	Dbgrideh::TGridDataLinkEh* __fastcall GetListLink(void);
	Db::TDataSource* __fastcall GetListSource(void);
	bool __fastcall GetReadOnly(void);
	bool __fastcall GetShowTitles(void);
	int __fastcall GetTitleRowHeight(void);
	bool __fastcall GetUseMultiTitle(void);
	void __fastcall CheckNotCircular(void);
	void __fastcall CheckNotLookup(void);
	HIDESBASE MESSAGE void __fastcall CMRecreateWnd(Messages::TMessage &Message);
	void __fastcall DataLinkRecordChanged(Db::TField* Field);
	void __fastcall SelectCurrent(void);
	void __fastcall SelectItemAt(int X, int Y);
	void __fastcall SelectSpecRow(void);
	void __fastcall SetAutoFitColWidths(const bool Value);
	void __fastcall SetDataFieldName(const AnsiString Value);
	HIDESBASE void __fastcall SetDataSource(Db::TDataSource* Value);
	void __fastcall SetKeyFieldName(const AnsiString Value);
	void __fastcall SetKeyValue(const Variant &Value);
	void __fastcall SetListFieldName(const AnsiString Value);
	void __fastcall SetListSource(Db::TDataSource* Value);
	void __fastcall SetLookupMode(bool Value);
	HIDESBASE void __fastcall SetOptions(const Dbgrideh::TDBLookupGridEhOptions Value);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	HIDESBASE void __fastcall SetRowCount(int Value);
	void __fastcall SetShowTitles(const bool Value);
	void __fastcall SetSpecRow(const Toolctrlseh::TSpecRowEh* Value);
	void __fastcall SetUseMultiTitle(const bool Value);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Messages::TWMKillFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Messages::TWMSetCursor &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Messages::TWMSetFocus &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Messages::TWMScroll &Message);
	
protected:
	int FSpecRowHeight;
	bool FLGAutoFitColWidths;
	bool FInternalWidthSetting;
	bool FInternalHeightSetting;
	virtual bool __fastcall CanDrawFocusRowRect(void);
	virtual bool __fastcall CanModify(void);
	virtual bool __fastcall CellHave3DRect(int ACol, int ARow, const Windows::TRect &ARect, Grids::TGridDrawState 
		AState);
	virtual bool __fastcall CompatibleVarValue(Toolctrlseh::TFieldsArrEh AFieldsArr, const Variant &AVlaue
		);
	DYNAMIC Dbgrideh::TColumnDefValuesEh* __fastcall CreateColumnDefValues(void);
	DYNAMIC Dbgrideh::TDBGridColumnsEh* __fastcall CreateColumns(void);
	DYNAMIC bool __fastcall DoMouseWheelDown(Classes::TShiftState Shift, const Windows::TPoint &MousePos
		);
	DYNAMIC bool __fastcall DoMouseWheelUp(Classes::TShiftState Shift, const Windows::TPoint &MousePos)
		;
	virtual int __fastcall GetBorderSize(void);
	int __fastcall GetKeyIndex(void);
	virtual int __fastcall GetSpecRowHeight(void);
	virtual int __fastcall GetSubTitleRows(void);
	virtual int __fastcall GetDataRowHeight(void);
	virtual bool __fastcall HighlightDataCellColor(int DataCol, int DataRow, const AnsiString Value, Grids::TGridDrawState 
		AState, Graphics::TColor &AColor, Graphics::TFont* AFont);
	DYNAMIC void __fastcall ColWidthsChanged(void);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DataChanged(void);
	virtual void __fastcall DefineFieldMap(void);
	virtual void __fastcall DrawSubTitleCell(int ACol, int ARow, int DataCol, int DataRow, Dbgrideh::TCellTypeEh 
		CellType, const Windows::TRect &ARect, Grids::TGridDrawState AState, bool &Highlighted);
	virtual void __fastcall GetDatasetFieldList(Classes::TList* FieldList);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	virtual void __fastcall KeyValueChanged(void);
	virtual void __fastcall LayoutChanged(void);
	virtual void __fastcall ListLinkDataChanged(void);
	virtual void __fastcall LinkActive(bool Value);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int 
		Y);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	virtual void __fastcall ProcessSearchKey(char Key);
	virtual void __fastcall Scroll(int Distance);
	virtual void __fastcall SelectKeyValue(const Variant &Value);
	virtual void __fastcall SpecRowChanged(System::TObject* Sender);
	virtual void __fastcall TimerScroll(void);
	virtual void __fastcall UpdateActive(void);
	virtual void __fastcall UpdateColumnsList(void);
	virtual void __fastcall UpdateDataFields(void);
	virtual void __fastcall UpdateListFields(void);
	virtual void __fastcall UpdateRowCount(void);
	virtual void __fastcall UpdateScrollBar(void);
	__property bool HasFocus = {read=FHasFocus, nodefault};
	__property ParentColor ;
	__property int TitleRowHeight = {read=GetTitleRowHeight, nodefault};
	
public:
	__fastcall virtual TDBLookupGridEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TDBLookupGridEh(void);
	HIDESBASE Windows::TRect __fastcall DataRect();
	int __fastcall GetColumnsWidthToFit(void);
	virtual bool __fastcall LocateKey(void);
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	__property bool AutoFitColWidths = {read=GetAutoFitColWidths, write=SetAutoFitColWidths, nodefault}
		;
	__property AnsiString DataField = {read=FDataFieldName, write=SetDataFieldName};
	__property Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Db::TField* Field = {read=GetDataField};
	__property AnsiString KeyField = {read=GetKeyFieldName, write=SetKeyFieldName};
	__property Variant KeyValue = {read=FKeyValue, write=SetKeyValue};
	__property bool ListActive = {read=FListActive, nodefault};
	__property AnsiString ListField = {read=FListFieldName, write=SetListFieldName};
	__property int ListFieldIndex = {read=FListFieldIndex, write=FListFieldIndex, default=0};
	__property Contnrs::TObjectList* ListFields = {read=FListFields};
	__property Dbgrideh::TGridDataLinkEh* ListLink = {read=GetListLink};
	__property Db::TDataSource* ListSource = {read=GetListSource, write=SetListSource};
	__property Dbgrideh::TDBLookupGridEhOptions Options = {read=FOptions, write=SetOptions, default=2};
		
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property AnsiString SearchText = {read=FSearchText, write=FSearchText};
	__property AnsiString SelectedItem = {read=FSelectedItem};
	__property Toolctrlseh::TSpecRowEh* SpecRow = {read=FSpecRow, write=SetSpecRow};
	__property bool ShowTitles = {read=GetShowTitles, write=SetShowTitles, nodefault};
	__property int RowCount = {read=FRowCount, write=SetRowCount, stored=false, nodefault};
	__property Color ;
	__property bool UseMultiTitle = {read=GetUseMultiTitle, write=SetUseMultiTitle, nodefault};
	__property OnClick ;
	__property OnColumnMoved ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDBLookupGridEh(HWND ParentWindow) : Dbgrideh::TCustomDBGridEh(
		ParentWindow) { }
	#pragma option pop
	
};


class PASCALIMPLEMENTATION TLookupGridDataLinkEh : public Db::TDataLink 
{
	typedef Db::TDataLink inherited;
	
private:
	TDBLookupGridEh* FDBLookupGrid;
	
protected:
	virtual void __fastcall ActiveChanged(void);
	virtual void __fastcall FocusControl(Db::TFieldRef Field);
	virtual void __fastcall LayoutChanged(void);
	virtual void __fastcall RecordChanged(Db::TField* Field);
	
public:
	__fastcall TLookupGridDataLinkEh(void);
public:
	#pragma option push -w-inl
	/* TDataLink.Destroy */ inline __fastcall virtual ~TLookupGridDataLinkEh(void) { }
	#pragma option pop
	
};


class DELPHICLASS TGridColumnSpecCellEh;
class PASCALIMPLEMENTATION TGridColumnSpecCellEh : public Classes::TPersistent 
{
	typedef Classes::TPersistent inherited;
	
private:
	Classes::TPersistent* FOwner;
	Graphics::TFont* FFont;
	Graphics::TColor FColor;
	AnsiString FText;
	Graphics::TColor __fastcall GetColor(void);
	Graphics::TFont* __fastcall GetFont(void);
	AnsiString __fastcall GetText();
	bool __fastcall IsColorStored(void);
	bool __fastcall IsFontStored(void);
	bool __fastcall IsTextStored(void);
	void __fastcall FontChanged(System::TObject* Sender);
	void __fastcall SetColor(const Graphics::TColor Value);
	void __fastcall SetFont(const Graphics::TFont* Value);
	void __fastcall SetText(const AnsiString Value);
	
protected:
	bool FColorAssigned;
	bool FFontAssigned;
	bool FTextAssigned;
	Graphics::TColor __fastcall DefaultColor(void);
	Graphics::TFont* __fastcall DefaultFont(void);
	AnsiString __fastcall DefaultText();
	DYNAMIC Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TGridColumnSpecCellEh(Classes::TPersistent* Owner);
	__fastcall virtual ~TGridColumnSpecCellEh(void);
	virtual void __fastcall Assign(Classes::TPersistent* Source);
	
__published:
	__property AnsiString Text = {read=GetText, write=SetText, stored=IsTextStored};
	__property Graphics::TColor Color = {read=GetColor, write=SetColor, stored=IsColorStored, nodefault
		};
	__property Graphics::TFont* Font = {read=GetFont, write=SetFont, stored=IsFontStored};
};


class DELPHICLASS TDBLookupGridColumnEh;
class PASCALIMPLEMENTATION TDBLookupGridColumnEh : public Dbgrideh::TColumnEh 
{
	typedef Dbgrideh::TColumnEh inherited;
	
private:
	TGridColumnSpecCellEh* FSpecCell;
	HIDESBASE TDBLookupGridEh* __fastcall GetGrid(void);
	void __fastcall SetSpecCell(const TGridColumnSpecCellEh* Value);
	
protected:
	virtual void __fastcall SetWidth(int Value);
	virtual void __fastcall SetIndex(int Value);
	
public:
	__fastcall virtual TDBLookupGridColumnEh(Classes::TCollection* Collection);
	__fastcall virtual ~TDBLookupGridColumnEh(void);
	__property TDBLookupGridEh* Grid = {read=GetGrid};
	
__published:
	__property Alignment ;
	__property AutoFitColWidth ;
	__property Checkboxes ;
	__property Color ;
	__property EndEllipsis ;
	__property FieldName ;
	__property Font ;
	__property ImageList ;
	__property ImeMode ;
	__property ImeName ;
	__property KeyList ;
	__property MaxWidth ;
	__property MinWidth ;
	__property NotInKeyListIndex ;
	__property PickList ;
	__property PopupMenu ;
	__property ShowImageAndText ;
	__property TGridColumnSpecCellEh* SpecCell = {read=FSpecCell, write=SetSpecCell};
	__property Tag ;
	__property Title ;
	__property ToolTips ;
	__property Visible ;
	__property Width ;
	__property OnGetCellParams ;
};


class DELPHICLASS TDBLookupGridColumnDefValuesEh;
class PASCALIMPLEMENTATION TDBLookupGridColumnDefValuesEh : public Dbgrideh::TColumnDefValuesEh 
{
	typedef Dbgrideh::TColumnDefValuesEh inherited;
	
__published:
	__property EndEllipsis ;
	__property Title ;
	__property ToolTips ;
public:
	#pragma option push -w-inl
	/* TColumnDefValuesEh.Create */ inline __fastcall TDBLookupGridColumnDefValuesEh(Dbgrideh::TCustomDBGridEh* 
		Grid) : Dbgrideh::TColumnDefValuesEh(Grid) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TColumnDefValuesEh.Destroy */ inline __fastcall virtual ~TDBLookupGridColumnDefValuesEh(void) { }
		
	#pragma option pop
	
};


class DELPHICLASS TPopupDataGridEh;
class PASCALIMPLEMENTATION TPopupDataGridEh : public TDBLookupGridEh 
{
	typedef TDBLookupGridEh inherited;
	
private:
	Toolctrlseh::TCloseUpEventEh FOnMouseCloseUp;
	Classes::TNotifyEvent FOnUserKeyValueChange;
	Toolctrlseh::TSizeGripEh* FSizeGrip;
	bool FSizeGripResized;
	bool FUserKeyValueChanged;
	HIDESBASE bool __fastcall CheckNewSize(int &NewWidth, int &NewHeight);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Messages::TMessage &Message);
	MESSAGE void __fastcall CMSetSizeGripChangePosition(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Messages::TWMEraseBkgnd &Message);
	MESSAGE void __fastcall WMMouseActivate(Messages::TWMMouseActivate &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanging(Messages::TWMWindowPosMsg &Message);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall DrawBorder(void);
	virtual void __fastcall KeyValueChanged(void);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int 
		Y);
	void __fastcall UpdateBorderWidth(void);
	
public:
	__fastcall virtual TPopupDataGridEh(Classes::TComponent* AOwner);
	__fastcall virtual ~TPopupDataGridEh(void);
	DYNAMIC bool __fastcall CanFocus(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyUp(Word &Key, Classes::TShiftState Shift);
	__property Ctl3D ;
	__property ParentCtl3D ;
	__property Toolctrlseh::TSizeGripEh* SizeGrip = {read=FSizeGrip};
	__property bool SizeGripResized = {read=FSizeGripResized, write=FSizeGripResized, nodefault};
	__property OnDrawColumnCell ;
	__property Classes::TNotifyEvent OnUserKeyValueChange = {read=FOnUserKeyValueChange, write=FOnUserKeyValueChange
		};
	__property Toolctrlseh::TCloseUpEventEh OnMouseCloseUp = {read=FOnMouseCloseUp, write=FOnMouseCloseUp
		};
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TPopupDataGridEh(HWND ParentWindow) : TDBLookupGridEh(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Dblookupgridseh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dblookupgridseh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DBLookupGridsEh
