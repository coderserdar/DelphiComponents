{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_listbox;

interface
{$I psc_defines.inc}

Uses
  dbgrids,
{$IFDEF D6}
  variants,
{$ENDIF}
  winapi.Windows,
  Forms,
  Types,
  Controls,
  winapi.Messages,
  Menus,
  Classes,
  DB,
  System.Actions,
  Vcl.ActnList,
  Vcl.Graphics,
  System.UITypes,
  myla_system,
  myla_interfaces,
  myla_parser,

  psc_edit,
  psc_edit_date,
  psc_edit_parts,
  psc_calculator,
  psc_parser_date,
  psc_wrapper,
  psc_procs,
  psc_const;

{------------------------------------------------------------------------------}

type
  TPSCLookupDef = Packed Record
    KeyField: String;
    DisplayField: String;
    DataSet: TDataSet;
    GridFields: String;
  End;

  TPSCListBoxOption = (
    fboSortFields,
    fboSortConditions,

    fboCanAdd,
    fboCanDelete,
    fboCanIndent,
    fboCanOpen,
    fboCanSave,
    fboCanSaveToMem,
    fboCanLoadFromMem,

    fboLookupDSOpen,
    fboLookupDSClose,
    fboLookupDSRefresh,

    lboAlwaysShowSelection,

    fboHideSortOrder,
    fboHideDragLine,
    fboHideFindRecord,
    fboHideToggleFilter,
    fboHideUnderline,
    fboHideAutoUpdate,
    fboHideCalendar,
    fboHideIgnoreCase,

    fboDeleteSubItems,
    fboClearWithSortOrder,
    fboChecksInDateEdit,
    fboCheckReadOnly,
    fboAutoSizePopups
  );

  TPSCListBoxOptions = Set Of TPSCListBoxOption;
{------------------------------------------------------------------------------}

Const
  SPSCValue = 'Value'; //don't resource
  SPSCValue1 = 'Value1'; //don't resource
  SPSCValue2 = 'Value2'; //don't resource

  cPSCListBoxOutDentKey: TPSCKeyDef = (KeyCode: VK_Left; ShiftState: [ssCtrl]);
  cPSCListBoxLoadKey: TPSCKeyDef = (KeyCode: Word('O'); ShiftState: [ssCtrl]);
  cPSCListBoxSaveKey: TPSCKeyDef = (KeyCode: Word('S'); ShiftState: [ssCtrl]);
  cPSCListBoxAddKey: TPSCKeyDef = (KeyCode: VK_INSERT; ShiftState: []);
  cPSCListBoxDelKey: TPSCKeyDef = (KeyCode: VK_DELETE; ShiftState: []);
  cPSCListBoxClearKey: TPSCKeyDef = (KeyCode: VK_DELETE; ShiftState: [ssCtrl]);
  cPSCListBoxIndentKey: TPSCKeyDef = (KeyCode: VK_RIGHT; ShiftState: [ssCtrl]);
  cPSCListBoxSaveToMemKey: TPSCKeyDef = (KeyCode: Word('S'); ShiftState:
    [ssCtrl,ssShift]);
  cPSCFltBoxLoadFromMemKey: TPSCKeyDef = (KeyCode: Word('L'); ShiftState:
    [ssCtrl,ssShift]);

  SPSCPrmListActionCat                       = 'FilterBox';
  cPSCDefaultIndentLev = 0;
  cPSCDefaultChecked = True;
  cPSCDefaultItemReadOnly = False;
  cPSCMinPopupListWidth: Integer = 121;
  cPSCMinPopupListHeight: Integer = 97;
  cPSCDefaultUnderline = True;
  cPSCDefaultVisible = True;
  CPSCEmptyLookupDef: TPSCLookupDef = (KeyField:''; DisplayField:''; DataSet: Nil);
  cPSCDefaultSystemSlot = False;
  cPSCDefaultEditWithMouse = False;
  cPSCDefaultNewItemChecked = True;
  cPSCDefaultShowDefaultPopup = False;
  cPSCDefaultMergePopupMenus = False;
  cPSCDefaultClickHereColor = clPSCGrayText{in 1.6 was clPSCInactiveCaption};
  cPSCDefaultHoverLinkColor = clPSCRed;
  cPSCDefaultLinkColor = clPSCHotLight{in 1.6 was clPSCNavy};
  cPSCDefaultSelColor = clPSCSilver;
  cPSCDefaultSelFontColor = clPSCWindowText;
  cPSCDefaultHideHoverLink = False;
  cPSCDefaultHideSelected = False;
  cPSCDefaultHideFocus = True;
  cPSCDefaultCheckBoxes = False;
  cPSCDefaultShowAddItem = False;
  cPSCDefaultReadOnly = False;

  cPSCListBoxDefaultOptions = [fboAutoSizePopups,fboLookupDSOpen];
{------------------------------------------------------------------------------}

type
  TPSCPickType = (ptAuto,ptDate,ptTime,ptDateTime,ptCalculator,ptText,
    ptPickList,ptLookup,ptBoolean);

  TPSCListItems=class;

  TPSCListParam = Class(TPSCField)
  private
    FPickType: TPSCPickType;
    FReadOnly: boolean;
    FDisplayValue: String;
    FUnderline: boolean;
    FPickList: TStrings;
    FLookupKeyField: String;
    FLookupDisplayField: String;
    FLookupDataSet: TDataSet;
    FLookupGridFields: String;
    Procedure SetDisplayValue(const V:String);
    Procedure SetLookupDataSet(V: TDataSet);
    Procedure SetUnderline(V: boolean);
    Procedure SetVisible(V: boolean);
    Procedure SetPickList(V: TStrings);
    function GetDisplayValueUsePickList(const PickList:IPSCStrings): String;
    function GetDisplayValueUseLookupDef(LookupDef:TPSCLookupDef):String;
    function IsDisplayValueStored:Boolean;virtual;
    function IsPickTypeStored:Boolean;virtual;
    function GetDisplayValue:String;virtual;
  protected
    Function GetListItems:TPSCListItems;
    Procedure Notification(Instance: TComponent); override;
    Procedure SetAsVariantSilent(Const Value: Variant; CallChanged: Boolean;
      ACheckType: Boolean); override;
    Function ValueToStrEx(const AValue:Variant;
      AForDisplay: boolean): String;override;
  public
    FVisible: boolean;
    procedure UpdateDisplayValue(ALookupDef:TPSCLookupDef;
      const APickList:IPSCStrings);
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Function GetLookupDef: TPSCLookupDef;
  published
    Property DisplayValue: String read GetDisplayValue write SetDisplayValue Stored IsDisplayValueStored;
    Property Underline: boolean read FUnderline write SetUnderline default
      cPSCDefaultUnderline;
    Property Visible: boolean read FVisible write SetVisible default
      cPSCDefaultVisible;
    Property ReadOnly: boolean read FReadOnly write FReadOnly default
      cPSCDefaultReadOnly;
    Property PickList: TStrings read FPickList write SetPickList;
    Property LookupKeyField: String read FLookupKeyField write FLookupKeyField;
    Property LookupDisplayField: String read FLookupDisplayField write
      FLookupDisplayField;
    Property LookupGridFields: String Read FLookupGridFields Write FLookupGridFields;

    Property LookupDataSet: TDataSet read FLookupDataSet write SetLookupDataSet;
    Property PickType: TPSCPickType read FPickType write FPickType Stored IsPickTypeStored;
    Property ValuePrefix;
    Property ValueSuffix;
  End;

  TPSCListParams=class(TPSCFields)
  private
    Function GetItem(Index: Integer): TPSCListParam;
    Procedure SetItem(Index: Integer; Value: TPSCListParam);
  public
    Property Items[Index: Integer]: TPSCListParam read GetItem write SetItem;
      default;
  end;

  TPSCListParamClass = Class Of TPSCListParam;

  TPSCListItemClass = Class Of TPSCListItem;

  TPSCListItemOption=(
    ListItem_IgnoreCase,
    ListItem_AutoUpdate
  );

  TPSCListItemOptions=set of TPSCListItemOption;

  TPSCListItem = Class(TPSCNamedItem)
  private
    FOptions:TPSCListItemOptions;
    FParams: TPSCListParams;
    FCurrentParam: Integer;
    FChecked: boolean;
    FIndentLev: Integer;
    FReadOnly: boolean;
    FCaption: String;
    FImageIndex: TPSCImageIndex;

    Procedure SetImageIndex(Value: TPSCImageIndex);
    Procedure SetOptions(V:TPSCListItemOptions);
    Procedure SetIndentLev(V: Integer);
    Procedure SetChecked(V: boolean);
    Procedure SetParams(V: TPSCListParams);
    Procedure SetCurrentParam(V: Integer);

    Function ParamVisible(Index: Integer): boolean;
    Function IsOptionsStored:Boolean;
    Function IsParamsStored:Boolean;
  protected
    Procedure CaptionChanged; virtual;
    Function GetListParamClass: TPSCListParamClass; virtual;
    Procedure SetParamValueAndStyle(Const AParamName: String; Const AValue:
      Variant;Underlined: boolean=True; Const DisplayValue:String= '');
    Procedure SetCaption(Const V: String); virtual;
    Property ImageIndex: TPSCImageIndex Read FImageIndex Write SetImageIndex Default -1;
  public
    Function IsParamReadOnly(Param: TPSCListParam): boolean;
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Property CurrentParam: Integer read FCurrentParam write SetCurrentParam;
    Property Checked: boolean read FChecked write SetChecked default
      cPSCDefaultChecked;
    Property IndentLev: Integer read FIndentLev write SetIndentLev default
      cPSCDefaultIndentLev;
    Property Params: TPSCListParams read FParams write SetParams Stored IsParamsStored;
    Property ReadOnly: boolean read FReadOnly write FReadOnly default
      cPSCDefaultItemReadOnly;
    Property Caption: String read FCaption write SetCaption;
    Property Options:TPSCListItemOptions Read FOptions Write SetOptions Stored IsOptionsStored;
  published
  End;

  TPSCListItems = Class(TPSCNamedItems)
  private
    FOnQuoteStr: TPSCOnQuoteStr;
    FOnGetAsString: TPSCOnGetAsString;
    FDisplayDateTimeFormat: TPSCDateTimeFormat;
    FFilterDateTimeFormat: TPSCDateTimeFormat;
    FSQLDateTimeFormat: TPSCDateTimeFormat;
    FForceSQLDateTime: boolean;
    FReadOnly: boolean;
    FUseLocalSettings: Boolean;

    Function GetItem(Index: Integer): TPSCListItem;
    Function CanOutDentItem(AIndex: Integer): boolean;

    Procedure SetItem(Index: Integer; V: TPSCListItem);
    Procedure SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetUseLocalSettings(AValue: Boolean);

  protected
    FForceSQLBooleans: boolean;
    function GetRealDisplayDateTimeFormat:TPSCDateTimeFormat;virtual;
    function GetRealSQLDateTimeFormat:TPSCDateTimeFormat;virtual;
    function GetRealFilterDateTimeFormat:TPSCDateTimeFormat;virtual;
  public
    procedure DeleteItem(AIndex: Integer;AOutDentSubItems:Boolean);virtual;
    Procedure GetSubItemsList(Item: TPSCListItem;
      const SubItems: IPSCObjectList;AddSelf:boolean);virtual;
    Procedure DoQuoteStr(Sender: TObject; Field: TPSCField; Var QuotedStr:
      String); virtual;
    Procedure GetAsString(Sender: TObject; Field: TPSCField;
      Var ResultStr: String; ForDisplay: boolean); overload; virtual;

    Procedure OutDentItem(AIndex: Integer);
    Constructor Create(AOwner: TPersistent; ItemClass: TPSCNamedItemClass);
      override;
    Destructor Destroy; override;
    Function IsPrmVisible(Item: TPSCListItem; Param: TPSCListParam): boolean;
      virtual;
    Function IsItemReadOnly(Item: TPSCListItem): boolean; virtual;
    Property ReadOnly: boolean read FReadOnly write FReadOnly;
    Property Items[Index: Integer]: TPSCListItem read GetItem write SetItem;
      default;
    Property OnGetAsString: TPSCOnGetAsString read FOnGetAsString write
      FOnGetAsString;
    Property OnQuoteStr: TPSCOnQuoteStr read FOnQuoteStr write FOnQuoteStr;
    Property ForceSQLDateTime: boolean read FForceSQLDateTime write
      FForceSQLDateTime;
    Property FilterDateTimeFormat: TPSCDateTimeFormat read
      FFilterDateTimeFormat write SetFilterDateTimeFormat;
    Property SQLDateTimeFormat: TPSCDateTimeFormat read FSQLDateTimeFormat
      write SetSQLDateTimeFormat;
    Property DisplayDateTimeFormat: TPSCDateTimeFormat read
      FDisplayDateTimeFormat write SetDisplayDateTimeFormat;
    Property UseLocalSettings: Boolean read FUseLocalSettings write
      SetUseLocalSettings default False;
  published
  End;

  TPSCListItemsClass = Class Of TPSCListItems;

  TPSCCustomListBox = Class;

  TPSCListNamedItem = Class(TPSCNamedItem)
  public
    Function GetListBox: TPSCCustomListBox; virtual;
  End;

  TPSCListBoxItem = Class(TPSCListItem)
  private
  protected
    Procedure SetCaption(Const V: String); override;
  published
    Property Caption;
    Property Options;
    Property Checked;
    Property IndentLev;
    Property ReadOnly;
    Property Params;
    Property UserFields;
  End;

  TPSCOnPickParameter = Procedure(Sender: TObject; AItem: TPSCListItem;
    AParam:TPSCListParam;Const AParamRect: TRect;
    Var AProcessed: boolean) Of Object;

  TPSCOnPickLookupParam = Procedure(Sender: TObject; AItem: TPSCListItem;
    AParam: TPSCListParam; Const AParamRect: TRect; Var ALookupDef:
      TPSCLookupDef;
    Var AProcessed: boolean) Of Object;

  TPSCOnGetLookupDef = Procedure(Sender: TObject; AItem: TPSCListItem;
    AParam: TPSCListParam; Var ALookupDef: TPSCLookupDef) Of Object;

  TPSCListMemorySlot = Class(TPSCNamedItem)
  private
    FSystemSlot: boolean;
    FItems: TPSCListItems;
    Procedure SetItems(V: TPSCListItems);
  protected
    Function GetListItemsClass: TPSCListItemsClass; virtual;
  public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
  published
    Property Name;
    Property SystemSlot: boolean read FSystemSlot write FSystemSlot default
      cPSCDefaultSystemSlot;
    Property Items: TPSCListItems read FItems write SetItems;
  End;

  TPSCListMemorySlotClass = Class Of TPSCListMemorySlot;

  TPSCListMemorySlots = Class(TPSCNamedItems)
  private
    FListItemClass: TPSCListItemClass;
    Function IsSystemSlot(Const SlotName: String): boolean;
    Function GetItem(Index: Integer): TPSCListMemorySlot;
    Procedure SetItem(Index: Integer; V: TPSCListMemorySlot);
  public
    Function CallMemoryDlg(SaveDlg: boolean; ListItems: TPSCListItems): boolean;
    Function CanDeleteMemSlot(Const SlotName: String): boolean;
    Function LoadFromMemory(Const SlotName: String; ListItems: TPSCListItems):
      boolean;
    Function SaveToMemory(Const SlotName: String; ListItems: TPSCListItems):
      TPSCListMemorySlot;
    Property ListItemClass: TPSCListItemClass read FListItemClass write
      FListItemClass;
    Property Items[Index: Integer]: TPSCListMemorySlot read GetItem write SetItem;
      default;
  End;

  TPSCAddItemEvent = Procedure(Sender: TObject; AItem: TPSCListItem) Of Object;

  TPSCListBoxImageKind=(
    Image_NewItem,
    Image_DeleteItem,
    Image_IndentItem,
    Image_OutDentItem,
    Image_OpenFilter,
    Image_SaveFilter,
    Image_SortAscSmall,
    Image_SortDescSmall,
    Image_SortAsc,
    Image_SortDesc,
    Image_Filter,
    Image_RunFilter,
    Image_SearchFirst,
    Image_SearchLast,
    Image_SearchPrev,
    Image_SearchNext,
    Image_Search
  );

  TPSCComponentActionClass = Class Of TPSCComponentAction;

  TPSCComponentAction = class(TPSCAction)
  protected
    procedure SetComponent(V:TComponent);virtual;abstract;
  end;

  TPSCPaintItemParamsEvent=procedure(Sender:TObject;AItem:TPSCListItem) of object; 

  TPSCOnUpdateListBoxOptions=procedure(Sender:TObject;
    var AOptions:TPSCListBoxOptions) of object;

  TPSCCustomListBox = Class(TPSCCustomControl)
  private
    FOnUpdateOptions:TPSCOnUpdateListBoxOptions;
    FLastTime: Cardinal;
    FAutoComplete:Boolean;
    FAutoCompleteFilter:String;

    FTimer:TObject;
    FOnEditKeyPress:TKeyPressEvent;
    FPickStrings:IPSCStrings;
    FDummyStrings:IPSCStrings;
    FImages: TPSCImageList;
    FShowImages: Boolean;
    FHoverLinkColorStr: String;
    FLinkColorStr: String;

    FDatePopupKind: TPSCFieldType;
    FOnLoaded: TPSCNotifyEvent;
    FOnUpdatePopup: TPSCUpdatePopupEvent;
    FPopupParams: TPSCPopupParams;
    FModified: boolean;
    FOnAdditem: TPSCAddItemEvent;
    FShowAddItem: Boolean;
    FItemHeight: Integer;
    FTopIndex: Integer;
    FItemIndex: Integer;
    FIntegralHeight: Boolean;
    FAddItemText: String;
    FMaxLength: Integer;
    FHorzPosition: Integer;
    FClickHereColor: TPSCColor;
    FHideHoverLink: boolean;
    FOptions: TPSCListBoxOptions;
    FNewItemChecked: boolean;
    FHideSelected: boolean;
    FMemorySlots: TPSCListMemorySlots;
    FOnGetLookupDef: TPSCOnGetLookupDef;
    FSelColor: TPSCColor;
    FSelFontColor: TPSCColor;
    FHoverLinkColor: TPSCColor;
    FLinkColor: TPSCColor;
    FOnPickLookupParam: TPSCOnPickLookupParam;
    FDragOrigin: TPoint;
    FImageList: TPSCImageList;
    FActionList: TPSCActionList;
    FOnPickParameter: TPSCOnPickParameter;
    FItemParam: TPSCListParam;
    FPopupItem: TPSCListItem;
    FPopupLookupDef: TPSCLookupDef;
    FEditWithMouse: boolean;
    FHideFocus: boolean;
    FOnChange: TPSCNotifyEvent;
    FItems: TPSCListItems;
    FCheckBoxes: boolean;
    FPopupRect: TRect;
    FVersion: String;
    FOnGetPaintItemParams:TPSCPaintItemParamsEvent;

    procedure FindItem(const S:String;AFromIndex:Integer);
    Procedure EnableTimer;
    Procedure KillTimer;
    Procedure TimerEvent(Timer: TObject; EventID: Integer);
    
    Function GetParamWithTags(Const S: String;
      Underline,Hover,AIsParameter:boolean): String;
    Function CanDragItems: boolean;
    Function RealShowAddItem: boolean;
    Function GetForceSQLDateTime: boolean;
    Function GetFilterDateTimeFormat: TPSCDateTimeFormat;
    Function GetSQLDateTimeFormat: TPSCDateTimeFormat;
    Function GetDisplayDateTimeFormat: TPSCDateTimeFormat;
    Function GetGetAsString: TPSCOnGetAsString;
    Function GetMemorySlotsStored: boolean;
    Function GetItemsStored: boolean;
    Function GetBackRect: TRect;
    Function GetCheckIndent: Integer;
    Function HorzScrollPos: Integer;
    Function IsAnyItemReadOnly: boolean;
    Function ItemPtrAtPos(Pos: TPoint): TPSCListItem;
    Function GetItemIndex: Integer;
    Function GetReadOnly: boolean;
    Function VisibleClientHeight: Integer;
    Function VisibleItemCount: Integer;

    Procedure SetImages(Value: TPSCImageList);
    Procedure SetShowImages(Value: Boolean);
    Procedure OnGetDisplayName(Sender: TObject;
      AItem: TPSCNamedItem;Var ADisplayName: String);
    procedure SetVersion(const V:String);
    Procedure PickDateEx(AKind: TPSCFieldType);
    Procedure RightButtonClick(XPos,YPos: Integer);
    Procedure ChangeItemAnsSubItemsIndex(AItemIndex: Integer; Delta: Integer);
    Procedure UpdateFocusRect;
    Procedure SetPopupParams(V: TPSCPopupParams);
    Procedure InvalidateItem(Item: Integer);
    Procedure SetOptions(V: TPSCListBoxOptions);
    Procedure SetReadOnly(V: boolean);
    Procedure ChangeCurrentItemHoverParam(Delta: Integer);
    Procedure UpdateScrollPos;
    Procedure UpdateScrollSize;
    Procedure SetAddItemText(Value: String);
    Procedure SetItemHeight(Value: Integer);
    Procedure SetTopIndex(Value: Integer);
    Procedure SetItemIndex(Value: Integer);
    Procedure SetIntegralHeight(Value: Boolean);
    Procedure SetShowAddItem(Value: Boolean);
    Procedure SetHorzPosition(Value: Integer);
    Procedure SetClickHereColor(Value: TPSCColor);
    Procedure SetGetAsString(V: TPSCOnGetAsString);
    Procedure SetForceSQLDateTime(V: boolean);
    Procedure SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
    Procedure SetMemorySlots(V: TPSCListMemorySlots);
    Procedure SetSelColor(V: TPSCColor);
    Procedure SetSelFontColor(V: TPSCColor);
    Procedure SetHideSelected(V: boolean);
    Procedure SetItems(V: TPSCListItems);
    Procedure SetCheckBoxes(V: boolean);
    Procedure SetHideFocus(V: boolean);
    Procedure SetHoverLinkColor(V: TPSCColor);
    Procedure SetLinkColor(V: TPSCColor);
    Procedure SetHideHoverLink(V: boolean);
    Procedure UpdateItemsLength(UpdateScroll: boolean);
    Procedure SetCheckedSecure(Item: TPSCListItem; AChecked: Boolean);
    Procedure ChangeItemIndex(Item: TPSCListItem; NewIndex: Integer);
    procedure DisplayDateTimeFormatChanged(Sender:TObject);
    Procedure SetOnQuoteStr(V: TPSCOnQuoteStr);
    procedure PickWithPopupCalendar(AKind: TPSCFieldType;const ADateTime:TDateTime);
    procedure PickWithDateEdit(AKind: TPSCFieldType;const ADateTime:TDateTime);

    Function GetAsStrings:IPSCStrings;
  protected
    FImageIndexes:Array[TPSCListBoxImageKind] of Integer;
    FPopupEdit: TForm;

    Function MousePosToStrPos(P: TPoint): TPoint;
    Function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    Function GetRealOptions:TPSCListBoxOptions;virtual;
    function GetRightFixedWidth:Integer;virtual;
    Function GetItemDisplayCaption(Item: TPSCListItem; HoverCurrentParam:
      boolean): String;virtual;
    function IsItemOptionEnabled(AOption:TPSCListItemOption):Boolean;virtual;
    Function CanAddItem:Boolean;virtual;
    Function GetItems: TPSCListItems;virtual;
    Function GetMemorySlots: TPSCListMemorySlots;virtual;
    Function GetOnQuoteStr: TPSCOnQuoteStr;virtual;
    Function GetParamRect(Item: TPSCListItem; Param: TPSCField; CharPos:
      Integer): TRect; virtual;
    function AddActionsToPopup:Integer;virtual;
    Function IsOverCreateObject(Pos: TPoint): Boolean;virtual;
    Function IsURLPos(P: TPoint): boolean;virtual;
    Function GetImageIndex(AImageKind:TPSCListBoxImageKind):Integer;
    Function ItemIndentLevInPixels(Item: TPSCListItem): Integer; virtual;
    Function PaintClickHere(ALeft,ATop,ARight: Integer): boolean;virtual;
    Function GetPopupMenu: TPopupMenu; override;
    Function HorzScrollVisible: boolean; override;
    Function VertScrollVisible: boolean; override;
    Function GetListMemorySlotClass: TPSCListMemorySlotClass; virtual;
    Function GetListItemClass: TPSCListItemClass; virtual;
    Function MoveItem(SourceItem,TargetItem: TPSCListItem;
      AOnlyCheck:Boolean): boolean; virtual;
    Function DragAcceptSource(Source: TObject): boolean; virtual;
    Function GetParamLookupDef(Item: TPSCListItem; Param: TPSCListParam):
      TPSCLookupDef; virtual;
    Function ItemRect(Item: Integer): TRect;virtual;
    Function GetListItemsClass: TPSCListItemsClass; virtual;
    Function GetParamPickType(Item: TPSCListItem; Param: TPSCListParam):
      TPSCPickType; virtual;
    Function AddItemTextStored: boolean; virtual;

    procedure SetAsStrings(const V:IPSCStrings);
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure ParamValueOnChange(Sender:TObject);virtual;
    procedure PrepareLookupDataSet(ADataSet:TDataSet);virtual;
    Procedure PerformHorzScroll(ScrollCode,ScrollPos: Integer); override;
    Procedure PerformVertScroll(ScrollCode,ScrollPos: Integer); override;
    Procedure UpdateScrollBars; override;
    Procedure InitDefaultKeyMapping; override;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure Paint; override;
    Procedure SetParent(AParent: TWinControl); override;
    Procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; Var
      Accept: Boolean); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
      override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState;
      X, Y: Integer);override;
    Procedure Loaded; override;

    Procedure CreateAction(C: TPSCComponentActionClass);virtual;
    Procedure CreateActions;virtual;
    Procedure AddActionToPopup(C:TPSCComponentActionClass;var AGroupIndex:Integer);virtual;
    Procedure PaintItem(AItem: Integer; ARect: TRect; AItemState:
      TPSCOwnerDrawState); virtual;
    Procedure OnPopupDateClosed(Sender: TObject; Canceled: boolean);
    Procedure OnPopupPickListClosed(Sender: TObject; Canceled: boolean);
    Procedure OnPopupEditClosed(Sender: TObject; Canceled: boolean);
    Procedure OnPopupLookupClosed(Sender: TObject; Canceled: boolean);
    Procedure OnPopupCalcClosed(Sender: TObject; Canceled: boolean);
    Procedure OnPopupBoolClosed(Sender: TObject; Canceled: boolean);
    Procedure ItemAdded(Item: TPSCListItem); virtual;
    Procedure FontChanged;virtual;
    Procedure DefaultPickLookupParam(LookupDef: TPSCLookupDef); virtual;
    Procedure PickLookupParam(LookupDef: TPSCLookupDef);
    Procedure GetParamPickList(Item: TPSCListItem; Param: TPSCListParam;
      const PickList: IPSCStrings); virtual;
    Procedure PickCurrentParameter;virtual;
    Procedure PickStrings(const Strings: IPSCStrings; GetProc: TPSCGetStringsProc;
      Const SelString: String; OnPopupClosed: TPSCOnPopupClosed;
      Sorted: boolean; PopupSize: TPoint);virtual;
    Procedure DoPopupListBox(const Strings: IPSCStrings; SelItem: Integer;
      CallBack: TPSCOnPopupClosed; ParamRect: TRect; PopupSize: TPoint);virtual;
    Procedure ShowPopupParameter; virtual;
    Procedure PickParameter; virtual;
    Procedure OnUpdate(Sender: TObject; Item: TPSCNamedItem); virtual;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    Procedure WMSetFocus(Var Message: TWMSetFocus); message WM_SetFocus;
    Procedure WMKillFocus(Var Message: TWMSetFocus); message WM_KillFocus;
    Procedure WMEraseBkgnd(Var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    Procedure WMGetDlgCode(Var Msg: TWMGetDlgCode); message WM_GetDlgCode;
    Procedure WMSetCursor(Var Message: TWMSetCursor); message WM_SetCursor;
    Procedure WMSize(Var Message: TWMSize); message WM_Size;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FontChanged;

    Property InternalImages:TPSCImageList Read FImageList;
    Property HorzPosition: Integer read FHorzPosition write SetHorzPosition;
    Property Images: TPSCImageList Read FImages Write SetImages;
    Property ShowImages: Boolean Read FShowImages Write SetShowImages
      Default false;
  public
    property Canvas;
    
    Function RemovePaintTags(Const S: String): String;
    Function GetWithRemovedTags(Item: TPSCListItem): String;
    Function InsertChar(Ch: Char): Boolean; override;
    Function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;virtual;
    Function ParamHasPickList(Item: TPSCListItem; Param: TPSCListParam): boolean;
      virtual;
    Function CanDeleteCurrentItem: boolean;virtual;
    Function CanIndentItem(AIndex: Integer): boolean;virtual;
    Function CanOutDentCurrentItem: boolean;virtual;
    Function CanIndentCurrentItem: boolean;virtual;
    Function CanClear: boolean;virtual;
    Function CanMoveItemUp:Boolean;
    Function CanMoveItemDown:Boolean;

    procedure DragCanceled;override;
    Procedure Assign(Source: TPersistent); override;
    Procedure DragDrop(Source: TObject; X,Y: Integer); override;
    Procedure UpdatePopupParams(APopupForm: TForm); virtual;
    Procedure SaveToMemoryDlg;virtual;
    Procedure LoadFromMemoryDlg;virtual;
    procedure JumpToNextItem;virtual;
    procedure JumpToPrevItem;virtual;
    procedure JumpPageDown;virtual;
    procedure JumpPageUp;virtual;
    Procedure MoveItemUp;virtual;
    Procedure MoveItemDown;virtual;
    Procedure JumpToFirstItem;virtual;
    Procedure JumpToLastItem;virtual;
    Procedure JumpToPrevParam;virtual;
    Procedure JumpToNextParam;virtual;
    Procedure DeleteAllItems; virtual;
    function AddItem(ACheckCanAdd:Boolean):TPSCListItem; virtual;
    procedure AddItemProc;
    Procedure DeleteItem(AIndex:Integer;AOutDentSubItems:Boolean); virtual;
    Procedure ScrollCharLeft;virtual;
    Procedure ScrollCharRight;virtual;
    Procedure ScrollPageLeft;virtual;
    Procedure ScrollPageRight;virtual;
    Procedure ScrollLineUp;virtual;
    Procedure ScrollLineDown;virtual;
    Procedure ScrollPageUp;virtual;
    Procedure ScrollPageDown;virtual;
    Procedure PickItemParam(Item: TPSCListItem; ParamNum: Integer);
    Procedure PickListParam; virtual;
    Procedure PickWithCalc; virtual;
    Procedure PickTextParam; virtual;
    Procedure PickBoolParam; virtual;
    Procedure PickDateTimeParam; virtual;
    Procedure PickDateParam; virtual;
    Procedure PickTimeParam; virtual;
    Procedure DeleteCurrentItem;virtual;
    Procedure IndentItem(AIndex: Integer);virtual;
    Procedure IndentCurrentItem;virtual;
    Procedure OutDentCurrentItem;virtual;
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property AsStrings:IPSCStrings Read GetAsStrings Write SetAsStrings;
    Property PopupLookupDef: TPSCLookupDef read FPopupLookupDef write
      FPopupLookupDef;
    Property PopupEdit: TForm read FPopupEdit write FPopupEdit;
    Property PopupParam: TPSCListParam read FItemParam write FItemParam;
    Property PopupItem: TPSCListItem read FPopupItem write FPopupItem;
    Property PopupParamRect: TRect read FPopupRect write FPopupRect;
    Property TopIndex: Integer read FTopIndex write SetTopIndex;
    Property ItemIndex: Integer read FItemIndex write SetItemIndex;
    Property ItemHeight: Integer read FItemHeight write SetItemHeight;
    Property Modified: boolean read FModified write FModified;
    Property OnLoaded: TPSCNotifyEvent read FOnLoaded write
      FOnLoaded;
    Property PopupParams: TPSCPopupParams read FPopupParams write
      SetPopupParams;
    Property OnUpdatePopup: TPSCUpdatePopupEvent read FOnUpdatePopup write
      FOnUpdatePopup;
    Property OnAdditem: TPSCAddItemEvent read FOnAddItem write FOnAddItem;
    Property WantReturns;
    Property Options: TPSCListBoxOptions read FOptions write SetOptions default
      cPSCListBoxDefaultOptions;
    Property OnGetAsString: TPSCOnGetAsString read GetGetAsString write
      SetGetAsString;
    Property OnQuoteStr: TPSCOnQuoteStr read GetOnQuoteStr write SetOnQuoteStr;
    Property ForceSQLDateTime: boolean read GetForceSQLDateTime write
      SetForceSQLDateTime default False;
    Property FilterDateTimeFormat: TPSCDateTimeFormat read
      GetFilterDateTimeFormat write SetFilterDateTimeFormat;
    Property SQLDateTimeFormat: TPSCDateTimeFormat read GetSQLDateTimeFormat
      write SetSQLDateTimeFormat;
    Property DisplayDateTimeFormat: TPSCDateTimeFormat read
      GetDisplayDateTimeFormat write SetDisplayDateTimeFormat;
    Property NewItemChecked: boolean read FNewItemChecked write FNewItemChecked
      default cPSCDefaultNewItemChecked;
    Property ShowDefaultPopup default cPSCDefaultShowDefaultPopup;
    Property MergePopupMenus default cPSCDefaultMergePopupMenus;
    Property MemorySlots: TPSCListMemorySlots read GetMemorySlots write
      SetMemorySlots stored GetMemorySlotsStored;
    Property ReadOnly: boolean read GetReadOnly write SetReadOnly default
      cPSCDefaultReadOnly;
    Property HoverLinkColor: TPSCColor read FHoverLinkColor write SetHoverLinkColor
      default cPSCDefaultHoverLinkColor;
    Property LinkColor: TPSCColor read FLinkColor write SetLinkColor default
      cPSCDefaultLinkColor;
    Property HideHoverLink: boolean read FHideHoverLink write SetHideHoverLink
      default cPSCDefaultHideHoverLink;
    Property OnGetLookupDef: TPSCOnGetLookupDef read FOnGetLookupDef write
      FOnGetLookupDef;
    Property ClickHereColor: TPSCColor read FClickHereColor write SetClickHereColor
      default cPSCDefaultClickHereColor;
    Property HideSelected: boolean read FHideSelected write SetHideSelected
      default cPSCDefaultHideSelected;
    Property SelColor: TPSCColor read FSelColor write SetSelColor default
      cPSCDefaultSelColor;
    Property SelFontColor: TPSCColor read FSelFontColor write SetSelFontColor default
      cPSCDefaultSelFontColor;
    Property ShowAddItem: Boolean read FShowAddItem write SetShowAddItem default
      cPSCDefaultShowAddItem;
    Property AddItemText: String read FAddItemText write SetAddItemText stored
      AddItemTextStored;
    Property EditWithMouse: boolean read FEditWithMouse write FEditWithMouse
      default cPSCDefaultEditWithMouse;
    Property HideFocus: boolean read FHideFocus write SetHideFocus default
      cPSCDefaultHideFocus;
    Property Items: TPSCListItems read GetItems write SetItems stored
      GetItemsStored;
    Property CheckBoxes: boolean read FCheckBoxes write SetCheckBoxes default
      cPSCDefaultCheckBoxes;
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
    Property OnPickParameter: TPSCOnPickParameter read FOnPickParameter write
      FOnPickParameter;
    Property OnPickLookupParam: TPSCOnPickLookupParam read FOnPickLookupParam
      write FOnPickLookupParam;
    Property Version: String read FVersion write SetVersion stored false;
    Property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight
      default True;
    Property ParentColor default False;
    Property TabStop default True;
    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
  published
    property OnEditKeyPress:TKeyPressEvent Read FOnEditKeyPress Write FOnEditKeyPress;
    property OnGetPaintItemParams:TPSCPaintItemParamsEvent Read FOnGetPaintItemParams Write FOnGetPaintItemParams;
    property OnUpdateOptions:TPSCOnUpdateListBoxOptions
      Read FOnUpdateOptions Write FOnUpdateOptions;
  End;

  TPSCListBox = Class(TPSCCustomListBox)
  published
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;
    Property BevelEdges;
    Property BorderWidth;
    
    property AutoComplete;
    Property OnLoaded;
    Property PopupParams;
    Property OnUpdatePopup;
    Property OnAdditem;
    Property WantReturns;
    Property Options;
    Property OnGetAsString;
    Property OnQuoteStr;
    Property DisplayDateTimeFormat;
    Property NewItemChecked;
    Property ShowDefaultPopup;
    Property MergePopupMenus;
    Property MemorySlots;
    Property ReadOnly;
    Property HoverLinkColor;
    Property LinkColor;
    Property HideHoverLink;
    Property OnGetLookupDef;
    Property ClickHereColor;
    Property HideSelected;
    Property SelColor;
    Property SelFontColor;
    Property ShowAddItem;
    Property AddItemText;
    Property EditWithMouse;
    Property HideFocus;
    Property Items;
    Property CheckBoxes;
    Property OnChange;
    Property OnPickParameter;
    Property OnPickLookupParam;
    Property Version;
    Property Align;
    Property Anchors;
    Property BiDiMode;
    Property DragCursor;
    Property DragKind;
    Property ImeMode;
    Property ImeName;
    Property ParentBiDiMode;
    Property Color;
    Property Constraints;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property IntegralHeight;
    Property ParentColor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;
    Property OnClick;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnStartDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnStartDrag;
  end;

  TPSCListBoxClass = Class Of TPSCListBox;

  TPSCPopupListBox = Class(TPSCPopupForm)
  private
    FListBox: TPSCListBox;
    Procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
  protected
    Function GetListBoxClass: TPSCListBoxClass; virtual;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property ListBox: TPSCListBox read FListBox;
  End;

  TPSCLBPopupForm=class(TPSCPopupForm)
  private
    FListBox:TPSCCustomListBox;
  protected
  public
    procedure AddButton(AActionClass:TPSCComponentActionClass);overload;
    property ListBox:TPSCCustomListBox Read FListBox Write FListBox;
  end;

  TPSCPopupEdit = Class(TPSCLBPopupForm)
  private
    FEditor: TPSCEdit;
    FCheckBoxIgnoreCase: TPSCCheckBox;
    FCheckBoxAutoUpdate: TPSCCheckBox;
  protected
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property Editor: TPSCEdit read FEditor;
    Property CheckBoxIgnoreCase: TPSCCheckBox Read FCheckBoxIgnoreCase;
    Property CheckBoxAutoUpdate: TPSCCheckBox Read FCheckBoxAutoUpdate;
  End;

  TPSCDBGridPopup = Class(TPSCLBPopupForm)
  private
    FGrid: TPSCDBGrid;
    FSubPanel:TPSCPanel;

    Function GetDataSource: TDataSource;virtual;
    Function GetDataSet: TDataSet;virtual;

    Procedure SetDataSource(Value: TDataSource);virtual;
    Procedure SetDataSet(Value: TDataSet);virtual;
    procedure CellClicked(Column: TColumn);
  protected
    procedure UpdateGrid;virtual;
  public
    procedure SetColumns(const S:String);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
    procedure Resize; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;

    Property Grid: TPSCDBGrid read FGrid;
    Property DataSet: TDataSet read GetDataSet write SetDataSet;
    Property DataSource: TDataSource read GetDataSource write SetDataSource;
    Property SubPanel:TPSCPanel Read FSubPanel;
  End;

  TPSCValueListPopup=class(TPSCLBPopupForm)
  private
    FListBox:TPSCListBox;
    FDataType:TPSCFieldType;
    FPickList:IPSCStrings;
    FLookupDef:TPSCLookupDef;
    FPickType:TPSCPickType;
  protected
    procedure ListBoxAdditem(Sender: TObject;AItem: TPSCListItem);
    function GetListBoxClass:TPSCListBoxClass;virtual;
  public
    procedure SetValueList(const AValue:Variant);
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    destructor Destroy;override;
    property ListBox:TPSCListBox Read FListBox;
    property DataType:TPSCFieldType Read FDataType Write FDataType;
    property PickList:IPSCStrings Read FPickList;
    property LookupDef:TPSCLookupDef Read FLookupDef Write FLookupDef;
    property PickType:TPSCPickType Read FPickType Write FPickType;
  end;

  TPSCPopupDateEdit=class(TPSCPopupForm)
  private
    FTimeEdit: TPSCDateTimeUpDown;
    FDateEdit: TPSCDateTimeUpDown;
    FDateLabel: TPSCImage;
    FTimeLabel: TPSCImage;
    FNowButton:TPSCSpeedButton;
    FNoneButton:TPSCSpeedButton;
    FNonePressed: Boolean;
  protected
    procedure OnTodayClick(Sender:TObject);virtual;
    procedure OnNoneClick(Sender:TObject);virtual;
  public
    Procedure PopupEx(Control: TControl; Const PopupRect: TRect;
      PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
      ShowOptions: TPSCPopupOptions);override;
    Procedure ResizeFormControls; override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Property NowButton:TPSCSpeedButton Read FNowButton;
    Property NoneButton:TPSCSpeedButton Read FNoneButton;
    Property TimeEdit: TPSCDateTimeUpDown Read FTimeEdit;
    Property DateEdit: TPSCDateTimeUpDown Read FDateEdit;
    Property DateLabel: TPSCImage Read FDateLabel;
    Property TimeLabel: TPSCImage Read FTimeLabel;
    Property NonePressed: Boolean Read FNonePressed Write FNonePressed;
  end;

  TPSCPopupDateEditClass=class of TPSCPopupDateEdit;

{------------------------------------------------------------------------------}

Function PSCIsValidLookupDef(LookupDef: TPSCLookupDef): boolean;
Function PSCCharPosFromPixelPos(Acanvas:TPSCCanvas;Const S: String;
  PixelPos: Integer): Integer;
Function PSCActionByClass(ActionList: TPSCActionList; C: TClass): TPSCAction;
Function PSCGetParamDisplayValue(Param: TPSCField): String;
Function PSCGetBoldSectionNum(Const S: String; CharPos: Integer): Integer;
Function PSCISPrmName(Param: TPSCField; Const CheckName: String): boolean;
Function PSCIsCharInBoldSection(Const S: String; CharPos: Integer): boolean;
Function PSCGetBoldSectionPos(Const S: String; SecNum: Integer): Integer;
Procedure PSCGetBoldPartInStrings(const Strings: IPSCStrings);
Function PSCGetBoldPartOrString(Const S: String): String;
Function PSCGetStringsMaxWidth(ACanvas:TPSCCanvas;
  const Strings: IPSCStrings): Integer;

type
  TPSCPopupListBoxClass=class of TPSCPopupListBox;
  TPSCDBGridPopupClass=class of TPSCDBGridPopup;
  TPSCPopupEditClass=class of TPSCPopupEdit;
  TPSCValueListPopupClass=class of TPSCValueListPopup;
var
  CPSCUsedPopupListBoxClass:TPSCPopupListBoxClass=TPSCPopupListBox;
  CPSCUsedPopupLookupClass:TPSCDBGridPopupClass=TPSCDBGridPopup;
  CPSCUsedPopupEditClass:TPSCPopupEditClass=TPSCPopupEdit;
  CPSCUsedPopupValueListClass:TPSCValueListPopupClass=TPSCValueListPopup;
  CPSCUsedPopupDateEditClass:TPSCPopupDateEditClass=TPSCPopupDateEdit;

  CPSCUsedListBoxClass:TPSCListBoxClass=TPSCListBox;
{------------------------------------------------------------------------------}

implementation

{------------------------------------------------------------------------------}

Procedure TPSCPopupDateEdit.PopupEx(Control: TControl; Const PopupRect: TRect;
  PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
  ShowOptions: TPSCPopupOptions);
begin
  FNonePressed:=False;
  inherited;
end;

{-------------------------------------------}

Procedure TPSCPopupDateEdit.ResizeFormControls;
var
  A:Integer;
begin
  inherited;

  FDateEdit.HandleNeeded;
  FTimeEdit.HandleNeeded;

  FDateLabel.Visible:=FDateEdit.Visible;
  FTimeLabel.Visible:=FTimeEdit.Visible;

  With FDateLabel do
    SetBounds(5,5 + (FDateEdit.Height - Height) Div 2, Width,Height);

  With FDateEdit do
    SetBounds(FDateLabel.Width+15,5,GetMaxTextWidth(True)+3,Height);

  With FTimeLabel do
    If FDateLabel.Visible then
      SetBounds(5,FDateEdit.BoundsRect.Bottom + 10 +
        (FTimeEdit.Height - Height) Div 2, Width,Height)
    else
      FTimeLabel.BoundsRect:=FDateLabel.BoundsRect;

  With FTimeEdit do
    If FDateLabel.Visible then
      SetBounds(FTimeLabel.Width+15,FDateEdit.BoundsRect.Bottom + 10,
        PSCMax(GetMaxTextWidth(True),FDateEdit.Width),Height)
    else
      FTimeEdit.BoundsRect:=FDateEdit.BoundsRect;

  BorderStyle:=bsSizeable;

  ClientWidth:= FTimeEdit.BoundsRect.Right + 5;

  If FTimeEdit.Visible then
    A:=FTimeEdit.BoundsRect.Bottom
  else
    A:=FDateEdit.BoundsRect.Bottom;

  ClientHeight:= A+5+GetFooterPanel.Height;

  BorderStyle:=bsSingle;
end;

{-------------------------------------------}

procedure TPSCPopupDateEdit.OnTodayClick(Sender:TObject);
begin
  TimeEdit.DateTime:=PSCNow;
  DateEdit.DateTime:=PSCNow;
  FNonePressed:=False;
  ClosePopup(False,True);
end;

{-------------------------------------------}

procedure TPSCPopupDateEdit.OnNoneClick(Sender:TObject);
begin
  FNonePressed:=True;
  ClosePopup(False,True);
end;

{-------------------------------------------}

constructor TPSCPopupDateEdit.CreateNew(AOwner: TComponent; Dummy: Integer=0);
begin
  FTimeEdit :=TPSCDateTimeUpDown.Create(Self);
  FDateEdit :=TPSCDateTimeUpDown.Create(Self);
  FDateLabel := TPSCImage.Create(Self);
  FTimeLabel := TPSCImage.Create(Self);

  inherited;
  GetFooterPanel;

  With FTimeEdit do
  begin
    Kind:=cpkTime;
    Parent:=Self;
    HandleNeeded;
  end;

  With FDateEdit do
  begin
    Kind:=cpkDate;
    Parent:=Self;
    HandleNeeded;
  end;

  With FDateLabel Do
    Begin
      Width:=27;
      Height:=27;
      PSCLoadBitmapFromResource(Picture.Bitmap,SPSCResName_Img_Date);
      Transparent := True;
      Parent := Self;
    End;

  With FTimeLabel Do
    Begin
      Width:=FDateLabel.Width;
      Height:=FDateLabel.Height;
      PSCLoadBitmapFromResource(Picture.Bitmap,SPSCResName_Img_Clock);
      Transparent := True;
      Parent := Self;
    End;

  FNowButton:=AddButton;

  With FNowButton do
  begin
    OnClick:=OnTodayClick;
    Hint := PSCRemoveCharSet(['&'],PSCConsts.NowButton);
    PSCLoadBitmapFromResource(Glyph,SPSCResName_Btn_SelectToday);
  end;

  FNoneButton:=AddButton;
  With FNoneButton do
  begin
    OnClick:=OnNoneClick;
    Hint := PSCRemoveCharSet(['&'],PSCConsts.NoneButton);
    PSCLoadBitmapFromResource(Glyph,SPSCResName_Btn_SelectNULL);
  end;
end;

{-------------------------------------------}

Procedure TPSCPopupListBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton;Shift: TShiftState; X,Y: Integer);
var
  MyHidePopup:Boolean;
Begin
  With ListBox Do
    If (Button = mbLeft) And (ItemAtPos(Point(X,Y),True) >= 0) then
    Begin
      MyHidePopup:=not (ssCtrl in Shift);
      If ([ssShift,ssAlt] * Shift = []) Then
        ClosePopup(Not PtInRect(ClientRect,Point(X,Y)),MyHidePopup);
    End;
End;

{-------------------------------------------}

Function TPSCPopupListBox.GetListBoxClass: TPSCListBoxClass;
Begin
  Result := TPSCListBox;
End;

{-------------------------------------------}

constructor TPSCPopupListBox.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  BorderStyle := bsSizeToolWin;

  FListBox := GetListBoxClass.Create(Self);
  FListBox.Parent := Self;
  FListBox.OnMouseUp := ListBoxMouseUp;
  FListBox.BorderStyle:=bsNone;
  FListBox.Align:=alClient;

  ActiveControl := FListBox;
  ClientHeight := FListBox.Height+GetFooterPanel.Height;
  ClientWidth := FListBox.Width;
End;

{-------------------------------------------}

Function PSCIsCharInBoldSection(Const S: String; CharPos: Integer): boolean;
Begin
  Result := PSCGetBoldSectionNum(S,CharPos) >= 0;
End;

{-------------------------------------------}

Function PSCGetBoldSectionPos(Const S: String; SecNum: Integer): Integer;
Var
  A: Integer;
  i: Integer;
  StartBold: boolean;
Begin
  A := -1;
  StartBold := True;
  Result := -1;
  For i := 1 To Length(S) Do
    Begin
      If S[i] = '~' Then
        Begin
          If StartBold Then
            Result := i + 1
          Else
            Begin
              inc(A);
              If A = SecNum Then
                Begin
                  Result := Result - (SecNum * 2 + 1);
                  exit;
                End;
            End;
          StartBold := Not StartBold;
        End;
    End;
End;

{------------------------------------------------------------------}

Function PSCISPrmName(Param: TPSCField; Const CheckName: String): boolean;
Begin
  Result := PSCCompareText(Param.Name,PSCRemoveColons(CheckName)) = 0;
End;

{-------------------------------------------}

Function PSCGetBoldSectionNum(Const S: String; CharPos: Integer): Integer;
Var
  i,a: Integer;
  InBold: boolean;
Begin
  InBold := False;
  Result := -1;
  a := 0;
  For i := 1 To length(S) Do
    Begin
      If S[i] = '~' Then
        Begin
          If Not InBold Then
            inc(Result);
          InBold := Not inBold;
        End
      Else
        inc(a);
      If CharPos = a Then
        break;
    End;
  If (Not InBold) Then
    Result := -1;
End;

{-------------------------------------------}

Function PSCGetParamDisplayValue(Param: TPSCField): String;
Begin
  If Param.IsNull Then
    Result := '<?>'
  Else
    Begin
      If (Param Is TPSCListParam) And (TPSCListParam(Param).DisplayValue <> '')
        Then
        Result := TPSCListParam(Param).DisplayValue
      Else
        Result := Param.GetAsText;
    End;
End;

{------------------------------------------------------------------------------}

type
  TPSCPrmListAction = Class(TPSCComponentAction)
  private
    FParamList: TPSCCustomListBox;
    Procedure SetParamList(V: TPSCCustomListBox);
  protected
    Function GetParamList(Target: TObject): TPSCCustomListBox; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetComponent(V:TComponent);override;
  public
    Function HandlesTarget(Target: TObject): Boolean; override;
  published
    Property ParamList: TPSCCustomListBox read FParamList write SetParamList;
  End;

  TPSCPrmListActionLoadFromMem = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionSaveToMem = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionOutDent = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionIndent = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionClear = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionDelete = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;

  TPSCPrmListActionAdd = Class(TPSCPrmListAction)
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure ExecuteTarget(Target: TObject); override;
    Procedure UpdateTarget(Target: TObject); override;
  End;
{------------------------------------------------------------------}

Const
  cGroupIndexDelta = 20;

Function PSCActionByClass(ActionList: TPSCActionList; C: TClass): TPSCAction;
Var
  i: Integer;
Begin
  With ActionList Do
    Begin
      For i := 0 To ActionCount - 1 Do
        If Actions[i].ClassType = C Then
          Begin
            Result := TPSCAction(Actions[i]);
            exit;
          End;
      Result := Nil;
    End;
End;

{-------------------------------------------}

Function PSCCharPosFromPixelPos(Acanvas:TPSCCanvas;Const S: String;
  PixelPos: Integer): Integer;
Var
  OldPos: Integer;
  SizeX: Integer;
Begin
  OldPos := 0;
  For Result := 1 To Length(S) Do
    Begin
      SizeX := ACanvas.TextWidth(Copy(S,1,Result));
      If (PixelPos >= OldPos) And (PixelPos <= SizeX) Then
        exit;
      OldPos := SizeX;
    End;
  Result := 0;
End;

{------------------------------------------------------------------}

Function PSCIsValidLookupDef(LookupDef: TPSCLookupDef): boolean;
Begin
  With LookupDef Do
    Result := (DataSet <> Nil) And (KeyField <> '')
      And (DataSet.FindField(KeyField) <> Nil)
      And ((DisplayField = '') Or (DataSet.FindField(DisplayField) <> Nil));
End;

{------------------------------------------------------------------}

Function TPSCListParams.GetItem(Index: Integer): TPSCListParam;
begin
  Result:=TPSCListParam(inherited Items[Index]);
end;

{------------------------------------------------------------------}

Procedure TPSCListParams.SetItem(Index: Integer; Value: TPSCListParam);
begin
  inherited Items[Index]:=Value;
end;

{------------------------------------------------------------------}

Procedure TPSCListParam.SetAsVariantSilent(Const Value: Variant; CallChanged:
  Boolean; ACheckType: Boolean);
Begin
  Inherited;
  FDisplayValue := '';
End;

{------------------------------------------------------------------}

Procedure TPSCListParam.Assign(Source: TPersistent);
Begin
  If Source Is TPSCListParam Then
    PSCAssignAllPropsExcludeStr(Source,Self,
      PSCGetNotStoredProps(Self) + ',Name,DataType'); //don't resource
  Inherited;
  If Source Is TPSCListParam Then
    FDisplayValue := TPSCListParam(Source).DisplayValue;
End;

{------------------------------------------------------------------}

Procedure TPSCListParam.SetPickList(V: TStrings);
Begin
  FPickList.Assign(V);
End;

{------------------------------------------------------------------}

Destructor TPSCListParam.Destroy;
Begin
  FPickList.Free;
  Inherited;
End;

{------------------------------------------------------------------}

Constructor TPSCListParam.Create(ACollection: TCollection);
Begin
  FPickList := TStringList.Create;
  Inherited;
  FUnderline := cPSCDefaultUnderline;
  FVisible := cPSCDefaultVisible;
  FReadOnly := cPSCDefaultReadOnly;
End;

{------------------------------------------------------------------}

Procedure TPSCListParam.SetVisible(V: boolean);
Begin
  If FVisible <> V Then
    Begin
      FVisible := V;
      Changed(False);
    End;
End;

{------------------------------------------------------------------}

function TPSCListParam.IsPickTypeStored:Boolean;
begin
  Result:=FPickType<>ptAuto;
end;

{------------------------------------------------------------------}

function TPSCListParam.GetDisplayValue:String;
begin
  Result:=FDisplayValue;
end;

{------------------------------------------------------------------}

function TPSCListParam.IsDisplayValueStored:Boolean;
begin
  Result:=not ((DataType=FT_STRING) and (AsString=DisplayValue));
end;

{------------------------------------------------------------------}

Procedure TPSCListParam.SetDisplayValue(const V:String);
begin
  If FDisplayValue<>V then
  begin
    FDisplayValue:=V;
    Changed(False);
  end;
end;

{------------------------------------------------------------------}

Procedure TPSCListParam.SetUnderline(V: boolean);
Begin
  If FUnderline <> V Then
    Begin
      FUnderline := V;
      Changed(False);
    End;
End;

{-------------------------------------------}

Procedure TPSCListParam.SetLookupDataSet(V: TDataSet);
Begin
  If FLookupDataSet <> V Then
    Begin
      FLookupDataSet := V;
      RegisterNotifier(V);
    End;
End;

{-------------------------------------------}

Function TPSCListParam.GetLookupDef: TPSCLookupDef;
Begin
  With Result Do
    Begin
      KeyField := LookupKeyField;
      DisplayField := LookupDisplayField;
      DataSet := LookupDataSet;
      GridFields:=FLookupGridFields;
    End;
End;

{-------------------------------------------}

Procedure TPSCListParam.Notification(Instance: TComponent);
Begin
  If Instance = FLookupDataSet Then
    FLookupDataSet := Nil;
End;

{-------------------------------------------}

Constructor TPSCListItem.Create(ACollection: TCollection);
Begin
  FImageIndex := -1;
  FChecked := cPSCDefaultChecked;
  FIndentLev := cPSCDefaultIndentLev;
  FReadOnly := cPSCDefaultItemReadOnly;
  FParams := TPSCListParams.Create(Self,GetListParamClass);
  FParams.AddFieldIfNeeded := True;
  FParams.OnChange := OnChange;
  FParams.OnGetAsString := TPSCListItems(ACollection).GetAsString;
  FParams.OnQuoteStr := TPSCListItems(ACollection).DoQuoteStr;
  FOptions:=[]; // must not include IgnoreCase by default
  Inherited;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetImageIndex(Value: TPSCImageIndex);
Begin
  If FImageIndex <> Value Then
    Begin
      FImageIndex := Value;
      Changed(false)
    End
End;

{-------------------------------------------}

Procedure TPSCListItem.SetIndentLev(V: Integer);
Begin
  If (FIndentLev <> V) And (V >= 0) Then
    Begin
      FIndentLev := V;
      Changed(False);
    End;
End;

{-------------------------------------------}

Destructor TPSCListItems.Destroy;
Begin
  FFilterDateTimeFormat.Free;
  FSQLDateTimeFormat.Free;
  FDisplayDateTimeFormat.Free;
  Inherited;
End;

{-------------------------------------------}

Constructor TPSCListItems.Create(AOwner: TPersistent; ItemClass:
  TPSCNamedItemClass);
Begin
  Inherited;
  FFilterDateTimeFormat := TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
  FSQLDateTimeFormat := TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
  FDisplayDateTimeFormat := TPSCDateTimeFormat.Create(Self,PSCGetDefaultDateTimeFormat);
End;

{-------------------------------------------}

Function TPSCListItems.IsItemReadOnly(Item: TPSCListItem): boolean;
Begin
  Result := Item.ReadOnly Or ReadOnly;
End;

{--------------------------------------------}

Function TPSCListItems.CanOutDentItem(AIndex: Integer): boolean;
Begin
  Result := (AIndex > 0) And (TPSCListItem(Items[AIndex]).IndentLev > 0) And
    Not IsItemReadOnly(TPSCListItem(Items[AIndex]));
End;

{--------------------------------------------}

Procedure TPSCListItems.OutDentItem(AIndex: Integer);
Begin
  If CanOutDentItem(AIndex) Then
    With TPSCListItem(Items[AIndex]) Do
      Begin
        IndentLev := IndentLev - 1;
        inc(AIndex);
        While (AIndex < Count) And (TPSCListItem(Items[AIndex]).IndentLev >
          IndentLev + 1) Do
          Begin
            TPSCListItem(Items[AIndex]).IndentLev :=
              TPSCListItem(Items[AIndex]).IndentLev - 1;
            inc(AIndex);
          End;
      End;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetOptions(V:TPSCListItemOptions);
Begin
  If FOptions <> V Then
    Begin
      FOptions := V;
      Changed(False);
    End;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetChecked(V: boolean);
Begin
  If FChecked <> V Then
    Begin
      FChecked := V;
      Changed(False);
    End;
End;

{-------------------------------------------}

Function TPSCListItem.IsParamReadOnly(Param: TPSCListParam): boolean;
Begin
  Result := Param.ReadOnly Or TPSCListItems(Collection).IsItemReadOnly(Self);
End;

{-------------------------------------------}

Function TPSCListItem.IsParamsStored:Boolean;
begin
  Result:=Params.Count>0;
end;

{-------------------------------------------}

Function TPSCListItem.IsOptionsStored:Boolean;
begin
  Result:=Options<>[];
end;

{-------------------------------------------}

Function TPSCListItem.ParamVisible(Index: Integer): boolean;
Begin
  Result :=
    TPSCListItems(Collection).ISPrmVisible(Self,TPSCListParam(Params[Index]));
End;

{-------------------------------------------}

Destructor TPSCListItem.Destroy;
Begin
  FParams.Free;
  Inherited;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetParams(V: TPSCListParams);
Begin
  FParams.Assign(V);
End;

{-------------------------------------------}

Function TPSCListItems.ISPrmVisible(Item: TPSCListItem; Param: TPSCListParam):
  boolean;
Begin
  Result := Param.Visible;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetCurrentParam(V: Integer);

  Function FindVisibleParam(OldValue,MinIndex,MaxIndex: Integer): Integer;
  Var
    i: Integer;
  Begin
    Result := OldValue;
    For i := MinIndex To MaxIndex Do
      If ParamVisible(i) Then
        Begin
          Result := i;
          exit;
        End;
  End;

Begin
  V := PSCMax(PSCMin(V,Params.Count - 1),0);
  If (V < Params.Count) And (Not ParamVisible(V)) Then
    Begin
      V := FindVisibleParam(V,V,Params.Count - 1);
      If Not ParamVisible(V) Then
        exit;
    End;
  If FCurrentParam <> V Then
    Begin
      FCurrentParam := V;
    End;
End;

{-------------------------------------------}

Procedure TPSCListItem.CaptionChanged;
Begin
  Params.BeginUpdate;
  Try
    Params.SafeParseParams(Caption);
    FCurrentParam := PSCMin(FCurrentParam,Params.Count - 1);
  Finally
    Params.EndUpdate;
  End;
End;

{-------------------------------------------}

Procedure TPSCListItem.SetCaption(Const V: String);
Begin
  FCaption := V;
End;

{-------------------------------------------}

Procedure TPSCListBoxItem.SetCaption(Const V: String);
Begin
  If V <> FCaption Then
    Begin
      FCaption := V;
      CaptionChanged;
      Changed(False);
    End;
End;

{-------------------------------------------}

Function TPSCListItems.GetItem(Index: Integer): TPSCListItem;
Begin
  Result := TPSCListItem(Inherited Items[Index]);
End;

{-------------------------------------------}

Procedure TPSCListItems.SetItem(Index: Integer; V: TPSCListItem);
Begin
  Inherited Items[Index] := V;
End;

{-------------------------------------------}

Function TPSCListItem.GetListParamClass: TPSCListParamClass;
Begin
  Result := TPSCListParam;
End;

{---------------------------------------------------}

Function TPSCListParam.GetListItems:TPSCListItems;
begin
  Result:=TPSCListItems(TPSCListItem(TPSCListParams(Collection).Owner).Collection);
end;

{---------------------------------------------------}

Function TPSCListParam.ValueToStrEx(const AValue:Variant;
  AForDisplay: boolean): String;
Var
  DateTimeFormat: TPSCDateTimeFormat;
  MyisNull: Boolean;
  MyDateTime:TDateTime;

  procedure ProcessBoolean;
  begin
      If AForDisplay Then
        Begin
          If AValue Then
            Result := PSCConsts.BoolDispTrue
          Else
            Result := PSCConsts.BoolDispFalse;
        End
      Else
        Begin
          If GetListItems.FForceSQLBooleans Then
            Begin
              If AValue Then
                Result := SPSCSQLTrue
              Else
                Result := SPSCSQLFalse;
            End
          Else
            Begin
              If AValue Then
                Result := SPSCTextTrue
              Else
                Result := SPSCTextFalse;
            End;
        End;
  end;

  procedure ProcessFloat;
  Begin
    Result:=PSCFloatToStr(AValue);
    If AForDisplay Then
      Begin
        If PSCGetDecimalSeparator <> PSCSysDecimalSeparator Then
          Result :=
            PSCChangeCharTo(PSCSysDecimalSeparator,PSCGetDecimalSeparator,Result);
      End
    Else
      If Not GetListItems.UseLocalSettings Then
        Begin
          If '.' <> PSCSysDecimalSeparator Then
            Result := PSCChangeCharTo(PSCSysDecimalSeparator,
              '.',Result);
          If '.' <> PSCGetDecimalSeparator Then
            Result := PSCChangeCharTo(PSCGetDecimalSeparator, '.',Result);
        End;
  End;

begin
  MyisNull:=VarIsNull(AValue) Or varIsEmpty(AValue);

  If MyIsNull then
  begin
    Result:=inherited ValueToStrEx(AValue,AForDisplay);
    exit;
  end;

  Case DataType Of
    FT_DATETIME,FT_DATE,FT_TIME:
    begin
      If MyIsNull Then
        MyDateTime := 0
      Else
        MyDateTime := VarToDateTime(AValue);

      If AForDisplay Then
        DateTimeFormat := GetListItems.GetRealDisplayDateTimeFormat
      Else
        Begin
          If GetListItems.ForceSQLDateTime Then
            DateTimeFormat := GetListItems.GetRealSQLDateTimeFormat
          Else
            DateTimeFormat := GetListItems.GetRealFilterDateTimeFormat;
        End;
    end;
  else
    begin
      DateTimeFormat:=nil;
      MyDateTime:=0;
    end;
  end;

  Case DataType Of
    FT_DATETIME:
      Result := DateTimeFormat.ToStringEx(MyDateTime,cpkDateTime);
    FT_DATE:
      Result := DateTimeFormat.ToStringEx(MyDateTime,cpkDate);
    FT_TIME:
      Result := DateTimeFormat.ToStringEx(MyDateTime,cpkTime);
    FT_CURRENCY,FT_FLOAT:
      ProcessFloat;
    FT_BOOL:
      ProcessBoolean;
    else
      begin
        Result:=inherited ValueToStrEx(AValue,AForDisplay);
        exit;
      end;
  End;
  If VarIsArray(FData) then
    Result:=GetAsFilterString(Result);
end;

{---------------------------------------------------}

Procedure TPSCListItems.GetAsString(Sender: TObject; Field: TPSCField;
  Var ResultStr: String; ForDisplay: boolean);
begin
  If Assigned(FOnGetAsString) Then
    FOnGetAsString(Self,Field,ResultStr,ForDisplay);
End;

{-------------------------------------------}

Procedure TPSCListItems.DoQuoteStr(Sender: TObject; Field: TPSCField; Var
  QuotedStr: String);
Begin
  If Assigned(FOnQuoteStr) Then
    FOnQuoteStr(Self,Field,QuotedStr);
End;

{-------------------------------------------}

Procedure TPSCListItems.SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  FFilterDateTimeFormat.Assign(V);
End;

{-------------------------------------------}

Procedure TPSCListItems.SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  FDisplayDateTimeFormat.Assign(V);
End;

{-------------------------------------------}

Procedure TPSCListItems.SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  FSQLDateTimeFormat.Assign(V);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.WMSize(Var Message: TWMSize);
Begin
  Inherited;
  TopIndex:=TopIndex;
  UpdateScrollbars;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.JumpToFirstItem;
Begin
  ItemIndex := 0;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.JumpToLastItem;
Begin
  ItemIndex := Items.Count - 1;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.JumpToPrevParam;
Begin
  ChangeCurrentItemHoverParam(-1);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.JumpToNextParam;
Begin
  ChangeCurrentItemHoverParam(1);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.DeleteAllItems;
Begin
  Items.Clear;
End;

{-------------------------------------------}

Type
  TMFields = Class(TPSCFields)
  End;

Procedure TPSCListItem.SetParamValueAndStyle(Const AParamName: String; Const
  AValue: Variant;Underlined: boolean=True; Const DisplayValue:String= '');
Var
  AParam: TPSCListParam;
  ParamName: String;
Begin
  ParamName := PSCRemoveColons(AParamName);
  AParam := TPSCListParam(Params.ItemByName(ParamName));
  If AParam = Nil Then
    Begin
      AParam := TPSCListParam(Params.Add);
      AParam.Name := ParamName;
      Params.DoInitNewField(AParam);
    End;
  Params.BeginUpdate;
  Try
    TPSCListParam(AParam).Value := AValue;
    TPSCListParam(AParam).Underline := Underlined;
    TPSCListParam(AParam).DisplayValue := DisplayValue;
  Finally
    Params.EndUpdate;
  End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetPopupParams(V: TPSCPopupParams);
Begin
  FPopupParams.Assign(V);
End;

{------------------------------------------------------------------}

Procedure TPSCListItems.SetUseLocalSettings(AValue: Boolean);
Begin
  If FUseLocalSettings = AValue Then
    Exit;
  FUseLocalSettings := AValue;
  Changed;
End;

{------------------------------------------------------------------------------}

procedure TPSCListParam.UpdateDisplayValue(ALookupDef:TPSCLookupDef;
  const APickList:IPSCStrings);
Begin
  DisplayValue := '';
  If (DataType In cNoDispValueTypes) Or IsNull Then
    exit;
  If APickList.Count>0 Then
    DisplayValue := GetDisplayValueUsePickList(APickList)
  else
  If PSCIsValidLookupDef(ALookupDef) Then
    DisplayValue := GetDisplayValueUseLookupDef(ALookupDef);
End;

{------------------------------------------------------------------------------}

function TPSCListParam.GetDisplayValueUseLookupDef(LookupDef:TPSCLookupDef):String;

  function MyGetDisplayValue(const AKeyValue:Variant):String;
  begin
    With LookupDef do
      If DataSet.Locate(KeyField,AKeyValue, []) Then
        Result := DataSet.FieldByName(DisplayField).Value
      else
        Result := '';
  end;

  function MyGetDisplayValueIfArray(const AValue:Variant):String;
  var
    MyLowBound:Integer;
    MyHighBound:Integer;
    i:Integer;
  begin
    MyLowBound:=VarArrayLowBound(AValue,1);
    MyHighBound:=VarArrayHighBound(AValue,1);
    Result:='';
    for i:=MyLowBound to MyHighBound do
      begin
        If Result<>'' then
          Result:=Result+', ';
        Result:=Result+MyGetDisplayValue(AValue[i]);
      end;
    If Result<>'' then
      Result:='('+Result+')';
  end;

Begin
  With LookupDef Do
    Begin
      Result := '';
      If (DisplayField = '') Or (DisplayField = KeyField) Then
        exit;

      If VarIsArray(Value) then
        Result:=MyGetDisplayValueIfArray(Value)
      else
        Result:=MyGetDisplayValue(Value);
    End;
End;

{------------------------------------------------------------------------------}

function TPSCListParam.GetDisplayValueUsePickList(const PickList:IPSCStrings): String;

  function MyGetDisplayValue(const APickList:IPSCStrings;
    const AValue:String): String;
  Var
    DisplValStr,ValStr: String;
    i: Integer;
  begin
    Result := '';
    For i := 0 To PickList.Count - 1 Do
      Begin
        PSCExtractPickListValues(PickList[i],DisplValStr,ValStr);
        If PSCCompareText(AValue,ValStr) = 0 Then
          Begin
            Result := DisplValStr;
            exit;
          End;
      End;
  end;

  function MyCaseArray(const PickList:IPSCStrings): String;
  var
    MyLowBound,MyHighBound:Integer;
    i:Integer;
    S:String;
  begin
    Result:='';
    MyLowBound:=VarArrayLowBound(Value,1);
    MyHighBound:=VarArrayHighBound(Value,1);
    for i:=MyLowBound to MyHighBound do
    begin
      S:=MyGetDisplayValue(PickList,Value[i]);
      If S='' then
        S:=Value;
      If Result<>'' then
        Result:=Result+', ';
      Result:=Result+S;
    end;
  end;

begin
  If IsArray then
    Result:=MyCaseArray(PickList)
  else
    Result:=MyGetDisplayValue(PickList,AsString);
end;

{-------------------------------------------}

function TPSCCustomListBox.MousePosToStrPos(P: TPoint): TPoint;
Var
  S: String;
Begin
  inc(P.X,HorzScrollPos);
  Result := P;

  If not HandleAllocated then
    exit;

  With Result Do
    Begin
      Y := ItemAtPos(p,True);
      If Y >= Items.Count Then
        Y := -1;
      If Y < 0 Then
        exit;

      Dec(X,GetCheckIndent);

      With Items[Y] Do
        Begin
          X := X - ItemIndentLevInPixels(Items[Y]);

          If X < 0 Then
            exit;

          S := GetWithRemovedTags(Items[Y]);

          Canvas.Font:=Font;
          X := PSCCharPosFromPixelPos(Canvas,S,X);
        End;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.WMSetCursor(Var Message: TWMSetCursor);
Var
  P: TPoint;
Begin
  If (csDesigning in ComponentState) then
    inherited
  else
    begin
      P := PSCGetClientCursorPos(Self);
      If PtInRect(ClientRect,P) And (IsURLPos(P) Or IsOverCreateObject(P)) Then
        SetCursor(Screen.Cursors[crHandpoint])
      Else
        Inherited;
    end;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.ChangeItemIndex(Item: TPSCListItem; NewIndex: Integer);

  Procedure MoveBottom;
  Begin
    Item.Index := Items.Count - 1;
  End;

  Procedure MoveTop;
  Begin
    Item.Index := 0;
  End;

Begin
  If Item.Index = NewIndex Then
    exit;
  If NewIndex >= Items.Count Then
    MoveBottom
  Else
    If NewIndex < 0 Then
      MoveTop
    Else
      Item.Index := NewIndex;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.ChangeItemAnsSubItemsIndex(AItemIndex: Integer;
  Delta: Integer);
Var
  i: Integer;
  List:IPSCObjectList;
Begin
  List:=PSCCreateObjectList(ioReferenced);
  With Items Do
    Begin
      GetSubItemsList(Items[AItemIndex],List,True);
      BeginUpdate;
      try
        For i := 0 To List.Count - 1 Do
          ChangeItemIndex(TPSCListItem(List[i]),TPSCListItem(List[i]).Index +
            Delta);
      finally
        EndUpdate;
      end;
    End;
End;

{-------------------------------------------}

Function TPSCCustomListBox.CanMoveItemUp:Boolean;
Begin
  Result:=(ItemIndex > 0) And (Items.Count > 0) And (Not ReadOnly) And
    (fboCanIndent In Options);
end;

{-------------------------------------------}

//maybe implement other 2 cases (when IndentLevs are not equal)

procedure TPSCCustomListBox.MoveItemUp;
Begin
  If CanMoveItemUp Then
    Begin
      If Items[ItemIndex - 1].IndentLev = Items[ItemIndex].IndentLev Then
        Begin
          ChangeItemAnsSubItemsIndex(ItemIndex, -1);
          JumpToPrevItem;
        End;
    End;
End;

{-------------------------------------------}

Function TPSCCustomListBox.CanMoveItemDown:Boolean;
begin
  Result:=(ItemIndex >= 0) And (Items.Count > 0) And (Not ReadOnly) And
    (fboCanIndent In Options) And (ItemIndex < Items.Count - 1);
end;

{-------------------------------------------}

procedure TPSCCustomListBox.MoveItemDown;
Begin
  If CanMoveItemDown Then
    Begin
      If Items[ItemIndex].IndentLev = Items[ItemIndex + 1].IndentLev Then
        Begin
          ChangeItemAnsSubItemsIndex(ItemIndex,1);
          JumpToNextItem;
        End;
    End;
End;

{-------------------------------------------}

function TPSCCustomListBox.MoveItem(SourceItem,TargetItem: TPSCListItem;
  AOnlyCheck:Boolean): boolean;
Var
  SourceList,TargetList: IPSCObjectList;
  IndentLevDelta: Integer;
  i: Integer;
  NewIndex: Integer;
  IncNewIndex: boolean;
Begin
  Result := False;
  SourceList := PSCCreateObjectList;
  TargetList := PSCCreateObjectList;
  Items.GetSubItemsList(SourceItem,SourceList,True);
  Items.GetSubItemsList(TargetItem,TargetList,True);
  If PSCListsIntersect(SourceList,TargetList) Then
    exit;
  If AOnlyCheck then
  begin
    Result:=True;
    exit;
  end;
  IndentLevDelta := TargetItem.IndentLev - SourceItem.IndentLev + 1;
  NewIndex := TargetItem.Index + 1;
  IncNewIndex := TargetItem.Index < SourceItem.Index;
  For i := 0 To SourceList.Count - 1 Do
    With TPSCListItem(SourceList[i]) Do
      Begin
        ChangeItemIndex(TPSCListItem(SourceList[i]),NewIndex);
        IndentLev := IndentLev + IndentLevDelta;
        If IncNewIndex Then
          inc(NewIndex);
      End;

  ItemIndex:=SourceItem.Index;
  Result := True;
End;

{-------------------------------------------}

function TPSCCustomListBox.ItemPtrAtPos(Pos: TPoint): TPSCListItem;
Var
  Index: Integer;
Begin
  Index := ItemAtPos(Pos,True);
  If Index >= 0 Then
    Result := Items[Index]
  Else
    Result := Nil;
End;

{-------------------------------------------}

function TPSCCustomListBox.CanDragItems: boolean;
Begin
  Result := EditWithMouse And (Not ReadOnly) And (fboCanIndent In FOptions);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.DragCanceled;
begin
  inherited;
  Invalidate;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.DragDrop(Source: TObject; X,Y: Integer);
Var
  SourceItem,TargetItem: TPSCListItem;
Begin
  If (Source Is TPSCCustomListBox) And CanDragItems Then
    Begin
      SourceItem := ItemPtrAtPos(FDragOrigin);
      TargetItem := ItemPtrAtPos(Point(X,Y));
      If (SourceItem = Nil) Or (TargetItem = Nil) Then
        Inherited
      Else
        Begin
          If ReadOnly Then
            exit;
          If SourceItem = TargetItem Then
            Begin
              If X > FDragOrigin.X Then
                IndentItem(TargetItem.Index)
              Else
                Items.OutDentItem(TargetItem.Index);
            End
          Else
            MoveItem(SourceItem,TargetItem,False);
        End;
    End
  Else
    Inherited;
  Invalidate;  
End;

{-------------------------------------------}

function TPSCCustomListBox.DragAcceptSource(Source: TObject): boolean;
Begin
  Result := Source = Self;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.DragOver(Source: TObject; X,Y: Integer; State:
  TDragState; Var Accept: Boolean);
var
  MyTargetItem:TPSCListItem;
  MySourceItem:TPSCListItem;
  MyItemRect:TRect;
Begin
  If (DragAcceptSource(Source)) And (CanDragItems) Then
    Begin
      Accept := Not PtInRect(GetBackRect,Point(X,Y));

      Repaint;

      If Accept and (not (fboHideDragLine in Options)) then
      begin
        MyTargetItem := ItemPtrAtPos(Point(X,Y));
        MySourceItem := ItemPtrAtPos(FDragOrigin);

        If (MyTargetItem<>nil) and (MySourceItem<>MyTargetItem)
           and (MyTargetItem<>nil) and MoveItem(MySourceItem,MyTargetItem,True) then
        begin
          MyItemRect:=ItemRect(MyTargetItem.Index);
          Canvas.Pen.Color:=Font.Color;
          Canvas.MoveTo(MyItemRect.Left,MyItemRect.Bottom);
          Canvas.LineTo(MyItemRect.Right,MyItemRect.Bottom);
        end;
      end;
    End
  Else
    Inherited;
End;

{--------------------------------------------}

Function TPSCCustomListBox.RemovePaintTags(Const S: String): String;
Begin
  Result := S;
  PSCReplaceAllOccurEx(Result,FLinkColorStr, '',True);
  PSCReplaceAllOccurEx(Result,FHoverLinkColorStr, '',True);
  PSCReplaceAllOccurEx(Result, '</C>', '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '<U>' , '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '</U>', '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '<B>' , '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '</B>', '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '<I>' , '',True); //don't resource
  PSCReplaceAllOccurEx(Result, '</I>', '',True); //don't resource
End;

{--------------------------------------------}

function TPSCCustomListBox.GetItemIndex: Integer;
Begin
  Result := PSCMin(ItemIndex,Items.Count - 1);
End;

{--------------------------------------------}

function TPSCCustomListBox.CanDeleteCurrentItem: boolean;
Begin
  Result := (GetItemIndex >= 0) And (Not
    Items.IsItemReadOnly(Items[GetItemIndex]));
End;

{--------------------------------------------}

function TPSCCustomListBox.CanIndentItem(AIndex: Integer): boolean;
Begin
  Result := (AIndex > 0) And (Items[AIndex - 1].IndentLev + 1 <>
    Items[AIndex].IndentLev)
    And Not Items.IsItemReadOnly(Items[AIndex]);
End;

{-------------------------------------------}

Procedure TPSCListItems.GetSubItemsList(Item: TPSCListItem;
  const SubItems: IPSCObjectList;AddSelf: boolean);
Var
  AIndex: Integer;
  AIndentLev: Integer;
Begin
  AIndex := Item.Index + 1;
  AindentLev := Item.IndentLev;
  SubItems.Clear;
  If AddSelf Then
    SubItems.Add(Item);
  While (AIndex < Count) And (Items[AIndex].IndentLev > AIndentLev) Do
    Begin
      SubItems.Add(Items[AIndex]);
      inc(AIndex);
    End;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.IndentItem(AIndex: Integer);
Begin
  If CanIndentItem(AIndex) Then
    With Items[AIndex] Do
      Begin
        IndentLev := IndentLev + 1;
        inc(AIndex);
        While (AIndex < Items.Count) And (Items[AIndex].IndentLev >= IndentLev)
          Do
          Begin
            Items[AIndex].IndentLev := Items[AIndex].IndentLev + 1;
            inc(AIndex);
          End;
      End;
End;

{--------------------------------------------}

function TPSCCustomListBox.CanOutDentCurrentItem: boolean;
Begin
  Result := Items.CanOutDentItem(GetItemIndex);
End;

{--------------------------------------------}

function TPSCCustomListBox.CanIndentCurrentItem: boolean;
Begin
  Result := CanIndentItem(GetItemIndex);
End;

{--------------------------------------------}

Procedure TPSCCustomListBox.DeleteItem(AIndex:Integer;
  AOutDentSubItems:Boolean);
begin
  Items.DeleteItem(AIndex,AOutDentSubItems);
  If Items.Count > 0 Then
    ItemIndex := PSCMin(AIndex,Items.Count - 1);
end;

{--------------------------------------------}

function TPSCListItems.GetRealDisplayDateTimeFormat:TPSCDateTimeFormat;
begin
  Result:=FDisplayDateTimeFormat;
end;

{--------------------------------------------}

function TPSCListItems.GetRealSQLDateTimeFormat:TPSCDateTimeFormat;
begin
  Result:=FSQLDateTimeFormat;
end;

{--------------------------------------------}

function TPSCListItems.GetRealFilterDateTimeFormat:TPSCDateTimeFormat;
begin
  Result:=FFilterDateTimeFormat;
end;

{--------------------------------------------}

procedure TPSCListItems.DeleteItem(AIndex: Integer;
  AOutDentSubItems:Boolean);
Var
  IndentLev: Integer;
Begin
  If AIndex >= 0 Then
    Begin
      If IsItemReadOnly(Items[AIndex]) Then
        exit;
      IndentLev := Items[AIndex].IndentLev;
      Items[AIndex].Free;

      If AOutDentSubItems then
        While (AIndex < Count) And (Items[AIndex].IndentLev > IndentLev) Do
          begin
            Items[AIndex].FIndentLev:=Items[AIndex].FIndentLev-1;
            inc(AIndex);
          end
      else
        While (AIndex < Count) And (Items[AIndex].IndentLev > IndentLev) Do
          Items[AIndex].Free;
    End;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.DeleteCurrentItem;
Begin
  If fboCanDelete In Options Then
    DeleteItem(GetItemIndex,not (fboDeleteSubItems in Options));
End;

{--------------------------------------------}

procedure TPSCCustomListBox.IndentCurrentItem;
Begin
  If fboCanIndent In Options Then
    IndentItem(GetItemIndex);
End;

{--------------------------------------------}

procedure TPSCCustomListBox.OutDentCurrentItem;
Begin
  If fboCanIndent In Options Then
    Items.OutdentItem(GetItemIndex);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetItems(V: TPSCListItems);
Begin
  Items.Assign(V);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetCheckBoxes(V: boolean);
Begin
  If FCheckBoxes <> V Then
    Begin
      FCheckBoxes := V;
      Invalidate;
    End;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.Loaded;
Begin
  Inherited;
  ItemIndex := 0;
  FontChanged;
  If Assigned(FOnLoaded) Then
    FOnLoaded(Self);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.UpdateItemsLength(UpdateScroll: boolean);
Var
  I,L: Integer;

  Procedure UpdStrLength(Const S: String; Additional: Integer);
  Begin
    L := PSCMax(L,Canvas.TextWidth(S) + Additional);
  End;

  Procedure UpdItemLength(Item: TPSCListItem);
  Begin
    UpdStrLength(GetWithRemovedTags(Item),
      ItemIndentLevInpixels(Item));
  End;

Begin
  If not HandleAllocated then
    exit;

  Canvas.Font:=Font;  

  L := 0;

  For i := 0 To Items.Count - 1 Do
    UpdItemLength(Items[i]);

  If RealShowAddItem Then
    UpdStrLength(AddItemText,0);

  If (Items.Count <> 0) Then
    L := L + GetCheckIndent;

  FMaxLength := L+GetRightFixedWidth;

  If UpdateScroll Then
    UpdateScrollBars;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.OnUpdate(Sender: TObject; Item: TPSCNamedItem);
Begin
  If ([csloading,csReading] * ComponentState = []) And
    ((Owner = Nil) Or ([csloading,csReading] * Owner.ComponentState = [])) Then
    Begin
      If Item = Nil Then
        Begin
          If Items.Count = 0 Then
            TopIndex := 0;
          Invalidate;
        End
      Else
        InvalidateItem(Item.Index);

      UpdateItemsLength(True);

      FModified := True;

      If Assigned(OnChange) Then
        OnChange(Self);
    End;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.UpdateFocusRect;
Var
  Index: Integer;
Begin
  If (Not FHideFocus) or (not (lboAlwaysShowSelection in GetRealOptions)) Then
    Begin
      If (ItemIndex < 0) Or (ItemIndex >= Items.Count) Then
        Index := -2
      Else
        Index := ItemIndex;
      InvalidateItem(Index);
    End;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.WMKillFocus(Var Message: TWMSetFocus);
Begin
  Inherited;
  UpdateFocusRect;
End;

{--------------------------------------------}

procedure TPSCCustomListBox.WMSetFocus(Var Message: TWMSetFocus);
Begin
  Inherited;
  UpdateFocusRect;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetHideFocus(V: boolean);
Begin
  If FHideFocus <> V Then
    Begin
      FHideFocus := V;
      Invalidate;
    End;
End;

{--------------------------------------------}

function TPSCCustomListBox.GetCheckIndent: Integer;
Begin
  If CheckBoxes Then
    Result := PSCCheckWidth + 2
  Else
    Result := 0;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.OnPopupBoolClosed(Sender: TObject; Canceled: boolean);
Begin
  If Not Canceled Then
    Begin
      If TPSCPopupListBox(FPopupEdit).ListBox.ItemIndex = 0 Then
        FItemParam.AsBoolean := False
      Else
        FItemParam.AsBoolean := True;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.OnPopupEditClosed(Sender: TObject; Canceled: boolean);
Var
  S: String;

  procedure MyUpdateOption(ASelected:Boolean;AOption:TPSCListItemOption);
  begin
    If ASelected then
      PopupItem.Options:=PopupItem.Options+[AOption]
    else
      PopupItem.Options:=PopupItem.Options-[AOption];
  end;

Begin
  If Not Canceled Then
    Begin
      Try
        S := TPSCPopupEdit(FPopupEdit).Editor.Text;
        If S = '' Then
          PopupParam.Clear
        Else
          PopupParam.Value := S;
        PopupParam.DisplayValue := PopupParam.Value;
        With TPSCPopupEdit(FPopupEdit) do
        begin
          If CheckBoxIgnoreCase.Enabled then
            MyUpdateOption(CheckBoxIgnoreCase.Checked,ListItem_IgnoreCase);
          If CheckBoxAutoUpdate.Enabled then
            MyUpdateOption(CheckBoxAutoUpdate.Checked,ListItem_AutoUpdate);
        end;
      Except
        FItemParam.Clear;
      End;
    End;
End;

{--------------------------------------------}

function TPSCCustomListBox.GetBackRect: TRect;
Begin
  Result := ClientRect;
  Result.Top := (Items.Count - TopIndex) * ItemHeight;
End;

{--------------------------------------------}

function TPSCCustomListBox.HorzScrollPos: Integer;
Begin
  Result := PSCGetScrollPosEx(Self.Handle,SB_HORZ)
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickWithDateEdit(AKind: TPSCFieldType;const ADateTime:TDateTime);
begin
  FPopupEdit:=CPSCUsedPopupDateEditClass.CreateNew(nil,0);
  With TPSCPopupDateEdit(FPopupEdit) do
  begin

    If fboChecksInDateEdit in GetRealOptions then
    begin
      FDateEdit.ShowCheckBoxes:=True;
      With FDateEdit.TextParts.Items[2] do
      begin
        Alignment:=haLeft;
        ExpandWidth:=False;
      end;
      FTimeEdit.ShowCheckBoxes:=True;
    end;

    FDateEdit.DateTime:=PSCDateOf(ADateTime);
    FTimeEdit.DateTime:=PSCTimeOf(ADateTime);

    Case AKind Of
      FT_DATETIME:
        begin
        end;
      FT_DATE:
        begin
          FTimeEdit.Visible:=False;
        end;
      FT_TIME:
        begin
          FDateEdit.Visible:=False;
        end;
    else
      exit;
    End;

    Self.UpdatePopupParams(FPopupEdit);

    Popup(Self,PopupParamRect,OnPopupDateClosed);

    If AKind=FT_TIME then
      ActiveControl:=FTimeEdit
    else
      ActiveControl:=FDateEdit;

    If ActiveControl.CanFocus then
      ActiveControl.SetFocus;
  end;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.PickDateEx(AKind: TPSCFieldType);
var
  MyDateTime: TDateTime;
begin
  FDatePopupKind := AKind;
  FPopupEdit.Free;
  FPopupEdit:=nil;

  If PopupParam.IsNull Then
    MyDateTime := PSCNow
  Else
    MyDateTime := PopupParam.AsDateTime;

  If (fboHideCalendar in GetRealOptions) or (AKind=FT_TIME) then
    PickWithDateEdit(AKind,MyDateTime)
  else
    PickWithPopupCalendar(AKind,MyDateTime);
end;

{------------------------------------------------------------------}
type
  THackForm=class(TForm)
  end;

procedure TPSCCustomListBox.UpdatePopupParams(APopupForm: TForm);
Begin
  With THackForm(APopupForm) Do
    Begin
      BevelEdges := PopupParams.BevelEdges;
      BevelInner := PopupParams.BevelInner;
      BevelOuter := PopupParams.BevelOuter;
      BevelKind := PopupParams.BevelKind;
      BevelWidth := PopupParams.BevelWidth;
    End;
  If Assigned(FOnUpdatePopup) Then
    FOnUpdatePopup(Self,APopupForm);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.OnPopupDateClosed(Sender: TObject; Canceled: boolean);

  procedure WithPopupDateEdit;
  begin
    With TPSCPopupDateEdit(FPopupEdit) Do
      If NonePressed then
        FItemParam.Clear
      else
        Case FDatePopupKind Of
          FT_DATETIME:
            FItemParam.AsDateTime := PSCDateof(FDateEdit.DateTime)+
              PSCTimeof(FTimeEdit.DateTime);
          FT_DATE:
            FItemParam.AsDate := PSCDateof(FDateEdit.DateTime);
          FT_TIME:
            FItemParam.AsTime := PSCTimeOf(FTimeEdit.DateTime);
        End;
  end;

  procedure WithPopupCalendar;
  begin
    With TPSCPopupCalendar(FPopupEdit) Do
      If NonePressed then
        FItemParam.Clear
      else
        Case FDatePopupKind Of
          FT_DATETIME:
            FItemParam.AsDateTime := CalendarPanel.DateTime;
          FT_DATE:
            FItemParam.AsDate := CalendarPanel.Date;
          FT_TIME:
            FItemParam.AsTime := CalendarPanel.Time;
        End;
  end;

begin
  If Not Canceled Then
    If FPopupEdit is TPSCPopupCalendar then
      WithPopupCalendar
    else
    If FPopupEdit is TPSCPopupDateEdit then
      WithPopupDateEdit;
end;

{-------------------------------------------}

Function TPSCCustomListBox.GetRealOptions:TPSCListBoxOptions;
begin
  Result:=Options;

  If (FPopupEdit<>nil) and FPopupEdit.Visible then
    Include(Result,lboAlwaysShowSelection);

  If Assigned(FOnUpdateOptions) then
    FOnUpdateOptions(Self,Result);
end;

{-------------------------------------------}

procedure TPSCCustomListBox.PickWithPopupCalendar(AKind: TPSCFieldType;const ADateTime:TDateTime);
begin
  FPopupEdit:=CPSCUsedPopupCalendarClass.CreateNew(nil,0);
  With TPSCPopupCalendar(FPopupEdit) do
  begin
    If fboChecksInDateEdit in GetRealOptions then
    begin
      CalendarPanel.DateEdit.ShowCheckBoxes:=True;
      With CalendarPanel.DateEdit.TextParts.Items[2] do
      begin
        Alignment:=haLeft;
        ExpandWidth:=False;
      end;
      CalendarPanel.TimeEdit.ShowCheckBoxes:=True;
    end;

    CalendarPanel.DateTime:=ADateTime;

    Case AKind Of
      FT_DATETIME:
        begin
        end;
      FT_DATE:
        begin
          CalendarPanel.TimeEdit.Enabled:=False;
        end;
      FT_TIME:
        begin
          CalendarPanel.DateEdit.Enabled:=False;
          CalendarPanel.Calendar.Enabled:=False;
        end;
    else
      exit;
    End;

    Self.UpdatePopupParams(FPopupEdit);

    Popup(Self,PopupParamRect,OnPopupDateClosed);

    If AKind=FT_TIME then
      ActiveControl:=CalendarPanel.TimeEdit
    else
      ActiveControl:=CalendarPanel.DateEdit;

    If ActiveControl.CanFocus then
      ActiveControl.SetFocus;
  end;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.SetVersion(const V:String);
begin
end;

{-------------------------------------------}

procedure TPSCCustomListBox.PickTimeParam;
Begin
  PickDateEx(FT_TIME);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickDateTimeParam;
Begin
  PickDateEx(FT_DATETIME);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickDateParam;
Begin
  PickDateEx(FT_DATE);
End;

{------------------------------------------------------------------}

Procedure TPSCCustomListBox.AddActionToPopup(C:TPSCComponentActionClass;
  var AGroupIndex:Integer);
Var
  MenuItem: TMenuItem;
Begin
  If C = Nil Then
    MenuItem := NewLine
  Else
    Begin
      MenuItem := TMenuItem.Create(Nil);
      MenuItem.Action := PSCActionByClass(FActionList,C);
      MenuItem.GroupIndex := AGroupIndex;
    End;
  DefaultPopup.Items.Add(MenuItem);
  inc(AGroupIndex,cGroupIndexDelta);
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.AddActionsToPopup:Integer;
Begin
  Result := 0;
  AddActionToPopup(TPSCPrmListActionAdd,Result);
  AddActionToPopup(TPSCPrmListActionDelete,Result);
  AddActionToPopup(TPSCPrmListActionClear,Result);
  AddActionToPopup(Nil,Result);
  AddActionToPopup(TPSCPrmListActionInDent,Result);
  AddActionToPopup(TPSCPrmListActionOutDent,Result);
  AddActionToPopup(Nil,Result);
  AddActionToPopup(TPSCPrmListActionSaveToMem,Result);
  AddActionToPopup(TPSCPrmListActionLoadFromMem,Result);
End;

{------------------------------------------------------------------}

Procedure TPSCCustomListBox.CreateAction(C: TPSCComponentActionClass);
Var
  Action: TPSCComponentAction;
Begin
  Action := C.Create(FActionList.Owner);
  Action.ActionList := FActionList;
  With Action Do
    Begin
      Name := PSCUniqueName(ActionList.Owner,Action.ClassName);
      Category := SPSCPrmListActionCat;
      SetComponent(Self);
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomListBox.CreateActions;
Begin
  CreateAction(TPSCPrmListActionOutDent);
  CreateAction(TPSCPrmListActionInDent);
  CreateAction(TPSCPrmListActionClear);
  CreateAction(TPSCPrmListActionDelete);
  CreateAction(TPSCPrmListActionAdd);
  CreateAction(TPSCPrmListActionSaveToMem);
  CreateAction(TPSCPrmListActionLoadFromMem);
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetPopupMenu: TPopupMenu;
Begin
  Result := GetPopup;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.RightButtonClick(XPos,YPos: Integer);
Var
  Index: Integer;
  i: Integer;
Begin
  Index := ItemAtPos(Point(XPos,YPos),True);
  ItemIndex := Index;

  For i := 0 To FActionList.ActionCount - 1 Do
    FActionList.Actions[i].Update;
End;

{--------------------------------------------}

Function TPSCCustomListBox.GetWithRemovedTags(Item: TPSCListItem): String;
Begin
  Result := GetItemDisplayCaption(Item,False);
  Result := PSCRemoveSpChars(Result);
  Result := RemovePaintTags(Result);
End;

{--------------------------------------------}

procedure TPSCCustomListBox.PaintItem(AItem: Integer; ARect: TRect; AItemState:
  TOwnerDrawState);
Var
  R: TRect;
  ACheckWidth: Integer;
  SkipX: Integer;
  S,PlainS: String;
  SaveRect: TRect;
Begin
  Try
    With Canvas Do
      Begin
        If (AItem >= Items.Count) Or (AItem < 0) Then
          Exit;

        If (odSelected In AItemState) And (Not FHideSelected) Then
        begin
          If Items[AItem].Params.Count=0 then
            begin
              Canvas.Brush.Color := clPSCHighlight;
              Canvas.Font.Color:= clPSCHighlightText;
            end
          else
            begin
              Canvas.Brush.Color := SelColor;
              Canvas.Font.Color:= SelFontColor;
            end;
        end;

        If Assigned(FOnGetPaintItemParams) then
          FOnGetPaintItemParams(Self,Items[AItem]);

        SaveRect := ARect;

        SkipX := ItemIndentLevInPixels(Items[AItem]);

        if not CheckBoxes then
          inc(SkipX);

        R := ARect;
        R.Right := R.Left + SkipX;
        If SkipX > 0 Then
          FillRect(R);
        ACheckWidth := GetCheckIndent;
        Inc(ARect.Left,SkipX);

        If CheckBoxes Then
          Begin
            R := ARect;
            If Not UseRightToLeftAlignment Then
              Begin
                Inc(ARect.Left,ACheckWidth);
                R.Right := ARect.Left;
              End
            Else
              Begin
                Dec(ARect.Right,ACheckWidth);
                R.Left := ARect.Right;
              End;

            PSCDrawCheck(Canvas,R,
              PSCCheckedToCheckState(Items[AItem].Checked),
              True,Canvas.Brush.Color,not (odDisabled in AItemState));
          End;

        If (odSelected In AItemState) And (Not HideHoverLink) Then
          S := GetItemDisplayCaption(Items[AItem],True)
        Else
          S := GetItemDisplayCaption(Items[AItem],False);
        S := PSCRemoveSpChars(s);

        PSCItemHtDrawEx(Canvas,ARect,S,PlainS,SkipX,False);
        Inc(ARect.Left,SkipX);
        FillRect(ARect);
      End;
  Except
    Canvas.FillRect(SaveRect);
  End;
End;

{-------------------------------------------}

Destructor TPSCCustomListBox.Destroy;
Begin
  try
    KillTimer;
    FMemorySlots.Free;
    FPopupEdit.Free;
    FPopupEdit:=nil;
    FItems.Free;
    FPopupParams.Free;
    Inherited;
  except
    PSCErrorFmt(PSCConsts.ExceptionIn,['TPSCCustomListBox.Destroy']);
  end;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetHideHoverLink(V: boolean);
Begin
  If HideHoverLink <> V Then
    Begin
      FHideHoverLink := V;
      Invalidate;
    End;
End;

{-------------------------------------------}

Function TPSCListNamedItem.GetListBox: TPSCCustomListBox;
Begin
  Result := TPSCCustomListBox(PSCFindOwnerWithClass(Collection,TPSCListBox));
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetParamPickType(Item: TPSCListItem;
  Param: TPSCListParam): TPSCPickType;
Begin
  Result := Param.PickType;
End;

{------------------------------------------------------------------}
type
  TPSCObjectProc = Procedure Of Object;

procedure TPSCCustomListBox.ShowPopupParameter;
Var
  HasPickList: boolean;
  LookupDef: TPSCLookupDef;

  Procedure _Pick(P: TPSCObjectProc);
  Begin
    If HasPickList Then
      PickListParam
    Else
      begin
        PrepareLookupDataSet(LookupDef.DataSet);
        If PSCIsValidLookupDef(LookupDef) Then
          PickLookupParam(LookupDef)
        Else
          P;
      end;
  End;

  Procedure AutoPick;
  Begin
    Case PopupParam.DataType Of
      FT_BOOL:
        PickBoolParam;
      FT_DATE:
        PickDateParam;
      FT_DATETIME:
        PickDateTimeParam;
      FT_INT,FT_CURRENCY,FT_FLOAT:
        _Pick(PickWithCalc);
      FT_TIME:
        PickTimeParam;
      FT_STRING:
        _Pick(PickTextParam);
    Else
      PSCbeep;
    End;
  End;

Begin
  HasPickList := ParamHasPickList(PopupItem,TPSCListParam(PopupParam));
  LookupDef := GetParamLookupDef(PopupItem,TPSCListParam(PopupParam));

  Case GetParamPickType(PopupItem,PopupParam) Of
    ptAuto:
      AutoPick;
    ptDate:
      PickDateParam;
    ptTime:
      PickTimeParam;
    ptDateTime:
      PickDateTimeParam;
    ptCalculator:
      PickWithCalc;
    ptText:
      PickTextParam;
    ptPickList:
      If HasPickList Then
        PickListParam
      Else
        PSCbeep;
    ptLookup:
      begin
        PrepareLookupDataSet(LookupDef.DataSet);
        If PSCIsValidLookupDef(LookupDef) Then
          PickLookupParam(LookupDef)
        Else
          PSCbeep;
      end;
    ptBoolean:
      PickBoolParam;
  Else
    PSCbeep;
  End;
End;

{------------------------------------------------------------------------------}

procedure TPSCCustomListBox.PickParameter;
Var
  Processed: Boolean;
Begin
  If PopupItem.IsParamReadOnly(PopupParam) Then
    exit;
  If Assigned(OnPickParameter) Then
    Begin
      Processed := False;
      OnPickParameter(Self,PopupItem,PopupParam,PopupParamRect,Processed);
      If Processed Then
        exit;
    End;

  ShowPopupParameter;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.CMFontChanged(Var Message: TMessage);
Begin
  Inherited;
  FontChanged;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PickListParam;
Var
  Temp: IPSCStrings;
  idx: Integer;
Begin
  Temp := PSCCreateStringList;
  GetParamPickList(PopupItem,TPSCListParam(PopupParam),Temp);
  PSCRemoveStringsPart(Temp,False);
  Temp.Sorted := True;
  Idx := Temp.IndexOf(PSCGetParamDisplayValue(PopupParam));
  Idx := PSCMax(Idx,0);
  DoPopupListBox(Temp,Idx,OnPopupPickListClosed,PopupParamRect,Point(0,0));
End;

{-------------------------------------------}

function TPSCCustomListBox.GetParamRect(Item: TPSCListItem;
  Param: TPSCField; CharPos: Integer): TRect;
Var
  H,W,L,T: Integer;
Begin
  If not HandleAllocated then
  begin
    Result:=Rect(0,0,0,0);
    exit;
  end;

  H := ItemHeight;

  Canvas.Font:=Font;

  W := Canvas.TextWidth(PSCGetParamDisplayValue(Param));

  T := (Item.Index - TopIndex) * ItemHeight;
  L := ItemIndentLevInPixels(Item) + GetCheckIndent +
    Canvas.TextWidth(Copy(GetWithRemovedTags(Item),1,CharPos - 1)) -
    HorzScrollPos;

  With Result Do
    Begin
      Left := L;
      Top := T;
      Right := L + W;
      Bottom := T + H;
    End;
End;

{-------------------------------------------}

Function TPSCCustomListBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  MyFewItems:Boolean;
begin
  inherited DoMouseWheel(Shift,WheelDelta,MousePos);

  MyFewItems:=(ClientHeight Div ItemHeight)>Items.Count;

  If WheelDelta>0 then
    begin
      If MyFewItems then
        JumpToPrevItem
      else
        ScrollLineUp;
    end
  else
    begin
      If MyFewItems then
        JumpToNextItem
      else
        ScrollLineDown;
    end;
  Result:=True;
end;


{-------------------------------------------}

procedure TPSCCustomListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KillTimer;
  inherited;
end;

{-----------------------------------------}

procedure TPSCCustomListBox.KillTimer;
begin
  PSCKillTimer(FTimer);
end;

{-----------------------------------------}

procedure TPSCCustomListBox.EnableTimer;
begin
  if (FTimer = nil) then
    FTimer := PSCSetTimer(0,35,TimerEvent);
end;

{-----------------------------------------}

procedure TPSCCustomListBox.TimerEvent(Timer: TObject;EventID: Integer);
var
  P:TPoint;
begin
  GetCursorPos(P);
  P:=ScreenToClient(P);
  If PtInRect(ClientRect,P) then
    ItemIndex := ItemAtPos(P,False)
  else
    KillTimer;
end;

{-------------------------------------------}

Procedure TPSCCustomListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  If (ssLeft in Shift) and Focused then
    EnableTimer;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Var
  Index: Integer;
  FromPos: Integer;
  P: TPoint;
  URLNum: Integer;
  Temp: String;
Begin
  If Not (csDesigning In ComponentState) And (Not Focused) And CanFocus Then
    Begin
      Winapi.Windows.SetFocus(Handle);
      SetFocus;
    End;

  If Button <> mbLeft Then
    Begin
      RightButtonClick(X,Y);
      Inherited;
      exit;
    End;
  Click;
  P := Point(X,Y);
  If IsOverCreateObject(P) Then
    Begin
      AddItem(True);
      exit;
    End
  Else
    begin
      ItemIndex := ItemAtPos(P,False);
      If not CanDragItems then
        EnableTimer;
    end;

  Inherited;

  FDragOrigin := Point(-1, -1);
  Index := ItemAtPos(P,True);

  If Index >= 0 Then
    With Items[Index] Do
      Begin
        If CheckBoxes Then
          Begin
            FromPos := ItemIndentLevInPixels(Items[Index]) - HorzScrollPos;
            If (X > FromPos) And (X < FromPos + PSCCheckWidth) Then
              Begin
                SetCheckedSecure(Items[Index], Not Checked);
                exit;
              End;
          End;

        If X>=ClientRect.Right-GetRightFixedWidth then
          exit;

        P := MousePosToStrPos(Point(X,Y));
        If P.X > 0 Then
          Begin
            Temp :=
              RemovePaintTags(GetItemDisplayCaption(Items[Index],False));
            URLNum := PSCGetBoldSectionNum(Temp,P.X);
            PickItemParam(Items[Index],URLNum);
            exit;
          End;
        If CanDragItems Then
          Begin
            BeginDrag(False);
            FDragOrigin := Point(X,Y);
          End;
      End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetCheckedSecure(Item: TPSCListItem; AChecked:
  Boolean);
Begin
  If Item.ReadOnly Or (fboCheckReadOnly In Options) Then
    exit;
  Item.Checked := AChecked;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.FindItem(const S:String;AFromIndex:Integer);
var
  i:Integer;
begin
  For i:=PSCMax(0,AFromIndex+1) to Items.Count-1 do
    If PSCPosIgnoreCase(S,Items[i].Caption)=1 then
    begin
      ItemIndex:=i;
      exit;
    end;
end;

{-------------------------------------------}

function TPSCCustomListBox.InsertChar(Ch: Char): Boolean;
Begin
  Result := False;
  If (Ch=' ') and CheckBoxes then
    begin
      If (GetItemIndex >= 0) Then
        With Items[GetItemIndex] Do
          SetCheckedSecure(Items[GetItemIndex], Not Checked);
    end
  Else
  If AutoComplete then
    begin
      if GetTickCount - FLastTime >= 500 then
        FAutoCompleteFilter := '';
      FLastTime := GetTickCount;
      FAutoCompleteFilter:=FAutoCompleteFilter+Ch;
      If Length(FAutoCompleteFilter)=1 then
        FindItem(FAutoCompleteFilter,-1)
      else
        FindItem(FAutoCompleteFilter,ItemIndex);
    end
  else
    exit;

  Result := True;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickItemParam(Item: TPSCListItem; ParamNum: Integer);
Var
  StartPos: Integer;
Begin
  PopupItem := Item;
  PopupItem.CaptionChanged;
  With PopupItem Do
    Begin
      If (Params.Count <= ParamNum) Or (Params.Count = 0)
        Or (ParamNum < 0) Then
        exit;
      PopupParam := TPSCListParam(Params[ParamNum]);
      StartPos := PSCGetBoldSectionPos(RemovePaintTags(
        GetItemDisplayCaption(PopupItem,False)),ParamNum);
    End;
  PopupParamRect := GetParamRect(PopupItem,PopupParam,StartPos);
  PopupItem.CurrentParam := PopupParam.Index;
  InvalidateItem(PopupItem.Index);
  Self.Update;
  PickParameter;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.GetParamPickList(Item:TPSCListItem;
  Param:TPSCListParam; const PickList:IPSCStrings);
begin
  If Param.PickList.Count>0 then
    PickList.Assign(PSCCreateStringsAdapter(Param.PickList))
  else
    PickList.Clear;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.PickCurrentParameter;
Var
  Item: TPSCListItem;
Begin
  If GetItemIndex < 0 Then
    exit;
  Item := Items[GetItemIndex];
  With Item Do
    Begin
      PickItemParam(Item,CurrentParam);
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.ChangeCurrentItemHoverParam(Delta: Integer);
Begin
  If GetItemIndex >= 0 Then
    Begin
      With TPSCListItem(Items[GetItemIndex]) Do
        CurrentParam := CurrentParam + Delta;
      InvalidateItem(GetItemIndex);
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetRightFixedWidth:Integer;
begin
  Result:=0;
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.Paint;
Var
  Top,TopItem,C: Integer;
  Bound: TRect;
  DrawState: TOwnerDrawState;
Begin
  If Not HandleAllocated Then
    exit;
  Try
    Top := ClientRect.Top;
    TopItem := TopIndex;
    C := TopItem + VisibleItemCount;
    Bound.Left := ClientRect.Left - HorzScrollPos;

    Bound.Right := ClientRect.Right-GetRightFixedWidth;

    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    While TopItem < C Do
      Begin
        Bound.Top := Top;
        Bound.Bottom := Top + ItemHeight;

        DrawState := [];
        If TopItem = ItemIndex Then
          Begin
            If (lboAlwaysShowSelection in GetRealOptions) or Focused then
              DrawState := [odSelected]
            else
              DrawState := [];

            If Focused Then
              Include(DrawState,odFocused);
          End;

        PaintItem(TopItem,Bound,DrawState);

        If (odFocused In DrawState) And (Not FHideFocus) Then
        begin
          Bound.Right:=ClientWidth;
          winapi.Windows.DrawFocusRect(Canvas.Handle,Bound);
        end;

        Canvas.Brush.Color := Color;
        Canvas.Font := Font;

        Inc(Top,ItemHeight);
        Inc(TopItem);
      End;

    PaintClickHere(Bound.Left,Top,Bound.Right+GetRightFixedWidth);

    Top := Top + ItemHeight;

    Canvas.FillRect(Rect(Bound.Left,Top,Bound.Right+GetRightFixedWidth,ClientHeight));
  Except
  End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.PaintClickHere(ALeft,ATop,ARight: Integer): Boolean;
Var
  Bound: TRect;
Begin
  Result := RealShowAddItem;
  Bound := Rect(ALeft,ATop,ARight,ATop + ItemHeight);

  With Canvas Do
    Begin
      If Result Then
        Begin
          Font.Color := ClickHereColor;
          TextRect(Bound,ALeft+2,ATop +
            (ItemHeight - TextHeight(AddItemText)) Div 2,AddItemText);
        End
      Else
        Begin
          Canvas.FillRect(Bound);
        End;

      If ((ItemIndex < 0) Or (ItemIndex >= Items.Count))
        And (Focused) And Not FHideFocus Then
        Begin
          Brush.Color := Self.Color;
          Pen.Color := Self.Font.Color;
          Font.Color := Self.Font.Color;
          Winapi.Windows.DrawFocusRect(Handle,Bound);
        End;
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.RealShowAddItem: boolean;
Begin
  Result := ShowAddItem And (fboCanAdd In Options) And (Not ReadOnly);
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
Var
  I,Top,H: Integer;
Begin
  If IsOverCreateObject(Pos) Then
    Begin
      Result := -2;
      Exit;
    End;
  Top := 0;
  I := TopIndex;
  H := VisibleClientHeight;
  While (Top < H) And (I < Items.Count) Do
    Begin
      If (Top < Pos.Y) And (Pos.Y <= Top + ItemHeight) Then
        Begin
          Result := I;
          Exit;
        End;
      Inc(Top,ItemHeight);
      Inc(I);
    End;
  If Existing Then
    Result := -1
  Else
    Result := Items.Count - 1;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.ItemRect(Item: Integer): TRect;
Begin
  If Item = -2 Then
    Result.Top := VisibleItemCount * ItemHeight
  Else
    Begin
      Item := Item - TopIndex;
      Result.Top := Item * FItemHeight;
    End;
  Result.Bottom := Result.Top + FItemHeight;
  Result.Left := 0;
  Result.Right := ClientWidth;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.IsOverCreateObject(Pos: TPoint): Boolean;
Var
  LastItemBottom: Integer;
Begin
  Result := False;
  If Not RealShowAddItem Then
    Exit;
  If Items.Count = 0 Then
    LastItemBottom := 0
  Else
    If (Items.Count - TopIndex) * ItemHeight <= ClientHeight Then
      LastItemBottom := (Items.Count - TopIndex) * ItemHeight
    Else
      LastItemBottom := (ClientHeight Div ItemHeight - 1) * ItemHeight;
  Result := (LastItemBottom < Pos.Y) And (Pos.Y <= LastItemBottom + ItemHeight)
    And
    (Pos.X < ClientWidth);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.AddItemProc;
begin
  AddItem(True);
end;

{------------------------------------------------------------------}

function TPSCCustomListBox.AddItem(ACheckCanAdd:Boolean):TPSCListItem;
Begin
  Result:=nil;
  If ACheckCanAdd and (not CanAddItem) then
    exit;
  Result := TPSCListItem(Items.Add);
  With Result Do
    Begin
      Checked := NewItemChecked;
    End;
  ItemAdded(Result);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ItemAdded(Item: TPSCListItem);
Begin
  If Assigned(OnAdditem) Then
    OnAddItem(Self,Item);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PerformHorzScroll(ScrollCode,ScrollPos: Integer);
Begin
  Case ScrollCode Of
    SB_LINEDOWN: ScrollCharLeft;
    SB_LINEUP: ScrollCharRight;
    SB_PAGEUp: ScrollPageRight;
    SB_PAGEDown: ScrollPageLeft;
    SB_THUMBTRACK: HorzPosition := ScrollPos;
  End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PerformVertScroll(ScrollCode,ScrollPos: Integer);
Begin
  Case ScrollCode Of
    SB_LINEDOWN:
      ScrollLineDown;
    SB_LINEUP:
      ScrollLineUp;
    SB_PAGEUp:
      ScrollPageUp;
    SB_PAGEDown:
      ScrollPageDown;
    SB_THUMBTRACK:
      TopIndex := ScrollPos;
  End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetHorzPosition(Value: Integer);
Begin
  If FHorzPosition <> Value Then
    Begin
      If Value < 0 Then
        Value := 0;
      FHorzPosition := Value;
      UpdateScrollPos;
      Invalidate;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.WMGetDlgCode(Var Msg: TWMGetDlgCode);
Begin
  Msg.Result := DLGC_WANTARROWS;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollCharLeft;
Begin
  HorzPosition := HorzPosition + 1;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollCharRight;
Begin
  HorzPosition := HorzPosition - 1;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollPageLeft;
Begin
  HorzPosition := HorzPosition + ClientWidth;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollPageRight;
Begin
  HorzPosition := HorzPosition - ClientWidth;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.JumpToNextItem;
begin
  ItemIndex := ItemIndex + 1;
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.JumpToPrevItem;
begin
  If ItemIndex > 0 Then
    ItemIndex := ItemIndex - 1;
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.JumpPageDown;
begin
  ItemIndex := ItemIndex + VisibleItemCount - 1;
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.JumpPageUp;
Var
  PageSize: Integer;
Begin
  PageSize := VisibleItemCount - 1;
  If ItemIndex - PageSize >= 0 Then
    ItemIndex := ItemIndex - PageSize
  Else
    ItemIndex := 0;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollLineUp;
Begin
  If TopIndex > 0 Then
    TopIndex := TopIndex - 1;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollLineDown;
Begin
  TopIndex := TopIndex + 1;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollPageUp;
Var
  PageSize: Integer;
Begin
  PageSize := VisibleItemCount - 1;
  If TopIndex - PageSize >= 0 Then
    TopIndex := TopIndex - PageSize
  Else
    TopIndex := 0;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.ScrollPageDown;
Begin
  TopIndex := TopIndex + VisibleItemCount - 1;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.VisibleClientHeight: Integer;
Begin
  Result := ClientHeight;
  If RealShowAddItem And (Result Div ItemHeight <= Items.Count - TopIndex) Then
    Result := Result - ItemHeight;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetClickHereColor(Value: TPSCColor);
Var
  ALeft,ATop: Integer;
Begin
  If FClickHereColor <> Value Then
    Begin
      FClickHereColor := Value;
      If RealShowAddItem Then
        Begin
          ALeft := -HorzScrollPos;
          ATop := VisibleItemCount * ItemHeight;
          PaintClickHere(ALeft,ATop,ClientWidth - ALeft);
        End;
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.VisibleItemCount: Integer;
Begin
  Result := ClientHeight Div ItemHeight;
  If RealShowAddItem Then
    Dec(Result);
  If Result > Items.Count - TopIndex Then
    Result := Items.Count - TopIndex;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetGetAsString: TPSCOnGetAsString;
Begin
  Result := Items.OnGetAsString;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetForceSQLDateTime: boolean;
Begin
  Result := Items.ForceSQLDateTime;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetFilterDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := Items.FilterDateTimeFormat;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetSQLDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := Items.SQLDateTimeFormat;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetDisplayDateTimeFormat: TPSCDateTimeFormat;
Begin
  Result := Items.DisplayDateTimeFormat;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetGetAsString(V: TPSCOnGetAsString);
Begin
  Items.OnGetAsString := V;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetForceSQLDateTime(V: boolean);
Begin
  Items.ForceSQLDateTime := V;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetFilterDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  Items.FilterDateTimeFormat := V;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetSQLDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  Items.SQLDateTimeFormat := V;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetDisplayDateTimeFormat(V: TPSCDateTimeFormat);
Begin
  Items.DisplayDateTimeFormat := V;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetItemsStored: boolean;
Begin
  Result := Items.Count > 0
End;

{-------------------------------------------}

function TPSCCustomListBox.GetMemorySlotsStored: boolean;
Begin
  Result := MemorySlots.Count > 0;
End;

{-------------------------------------------------------------------------}

Function TPSCListMemorySlots.GetItem(Index: Integer): TPSCListMemorySlot;
begin
  Result:=TPSCListMemorySlot(inherited Items[Index]);
end;

{-------------------------------------------------------------------------}

Procedure TPSCListMemorySlots.SetItem(Index: Integer; V: TPSCListMemorySlot);
begin
  inherited Items[Index]:=V;
end;

{-------------------------------------------}

Function TPSCListMemorySlots.LoadFromMemory(Const SlotName: String;
  ListItems: TPSCListItems): boolean;
Var
  Index: Integer;
Begin
  Result := False;
  If SlotName = '' Then
    exit;
  Index := IndexOfName(SlotName);
  Result := Index >= 0;
  If Result Then
    ListItems.Assign(TPSCListMemorySlot(Items[Index]).Items);
End;

{-------------------------------------------}

Function TPSCListMemorySlots.SaveToMemory(Const SlotName: String;
  ListItems: TPSCListItems): TPSCListMemorySlot;
Var
  Index: Integer;
Begin
  Result := Nil;
  If SlotName = '' Then
    exit;
  Index := IndexOfName(SlotName);
  If Index >= 0 Then
    Begin
      Result := TPSCListMemorySlot(Items[Index]);
      If Result.SystemSlot And (Not (DesignTime)) Then
        Begin
          Result := Nil;
          exit;
        End;
    End
  Else
    Result := TPSCListMemorySlot(Add);

  With Result Do
    Begin
      Name := SlotName;
      SystemSlot := DesignTime;
      Items.Assign(ListItems);
    End;
End;

{-------------------------------------------}

type
  TPSCFilterBoxMemForm = class(TForm)
  private
    Edit: TPSCEdit;
    ListBox: TPSCListBox;
    OkButton: TPSCButton;
    CancelButton: TPSCButton;
    DeleteButton: TPSCButton;
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    FDeleteList:IPSCStrings;
    FMemorySlots:TPSCListMemorySlots;
    procedure UpdateControls;
    function ValidItemIndex:boolean;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;

    property DeleteList:IPSCStrings Read FDeleteList;
    property MemorySlots:TPSCListMemorySlots Read FMemorySlots Write FMemorySlots;
  end;

{-------------------------------------------}

constructor TPSCFilterBoxMemForm.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  FDeleteList:=PSCCreateStringList;
  inherited;

  ClientHeight := 228;
  ClientWidth := 263;
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Scaled := False;
  PSCSetFormFont(Self);

  Edit:= TPSCEdit.Create(Self);
  With Edit do
  begin
    Left:= 8;
    Top:= 14;
    Width:= 154;
    Height:= 21;
    OnChange:= EditChange;
    Parent:=Self;
  end;

  ListBox:= TPSCListBox.Create(Self);
  With ListBox do
  begin
    Left := 8;
    Top := 47;
    Width := 154;
    Height := 166;
    Enabled := False;
    OnClick := ListBoxClick;
    OnDblClick := ListBoxDblClick;
    Parent:=Self;
    Options:=Options-[fboCanAdd,fboCanDelete,fboCanIndent,
      fboCanSaveToMem,fboCanLoadFromMem];
    BorderStyle:=bsNone;
    BevelInner:=bvNone;
    BevelOuter:=bvRaised;
    BevelKind:=bkFlat;
  end;

  OkButton:= TPSCButton.Create(Self);
  With OkButton do
  begin
    Left := 176;
    Top := 14;
    Width := 75;
    Height := 25;
    Caption := PSCConsts.OKButton;
    Default := True;
    ModalResult := 1;
    Parent:=Self;
  end;

  CancelButton:= TPSCButton.Create(Self);
  With CancelButton do
  begin
    Left := 176;
    Top := 50;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := PSCConsts.CancelButton;
    ModalResult := 2;
    Parent:=Self;
  end;

  DeleteButton:=TPSCButton.Create(Self);
  With DeleteButton do
  begin
    Left := 177;
    Top := 116;
    Width := 75;
    Height := 25;
    Caption := PSCConsts.DeleteButton;
    OnClick := DeleteButtonClick;
    Parent:=Self;
  end;

  UpdateControls;
end;

{-------------------------------------------}

procedure TPSCFilterBoxMemForm.UpdateControls;
begin
  DeleteButton.Enabled:=(Edit.Text<>'') and
    (ListBox.AsStrings.IndexOf(Edit.Text)>=0);
  OkButton.Enabled:=Edit.Text<>'';
  ListBox.Enabled:=ListBox.Items.Count>0;
end;

{-------------------------------------------}

function TPSCFilterBoxMemForm.ValidItemIndex:boolean;
begin
  Result:=(ListBox.ItemIndex>=0) and (ListBox.Items.Count>0);
end;

{-------------------------------------------}

procedure TPSCFilterBoxMemForm.ListBoxClick(Sender: TObject);
begin
  If ValidItemIndex then
    Edit.Text:=ListBox.Items[ListBox.ItemIndex].Caption;
end;

{-------------------------------------------}

procedure TPSCFilterBoxMemForm.ListBoxDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

{-------------------------------------------}

procedure TPSCFilterBoxMemForm.DeleteButtonClick(Sender: TObject);
begin
  If ValidItemIndex then
    with ListBox do
    begin
      If not FMemorySlots.CanDeleteMemSlot(Items[ItemIndex].Caption) then
        PSCShowMessage(PSCConsts.ErrMemSlotDelete)
      else
        begin
          FDeleteList.Add(Items[ItemIndex].Caption);
          Items.Delete(ItemIndex);
        end;
      UpdateControls;
    end;
end;

{-------------------------------------------}

procedure TPSCFilterBoxMemForm.EditChange(Sender: TObject);
begin
  UpdateControls;
end;

{-------------------------------------------}

Function TPSCListMemorySlots.CallMemoryDlg(SaveDlg: boolean;
  ListItems: TPSCListItems): boolean;
Var
  Form: TPSCFilterBoxMemForm;
  a,i: Integer;
Begin
  Form := TPSCFilterBoxMemForm.CreateNew(Nil);
  With Form Do
  Try
    If SaveDlg then
      Caption:=PSCConsts.PrmBoxSaveToMem
    else
      Caption:=PSCConsts.PrmBoxLoadFromMem;

    Caption := PSCRemoveCharSet(['.', '&'],Caption);
    MemorySlots := Self;

    for i:=0 to Self.Count-1 do
      ListBox.AddItem(False).Caption:=Self.Items[i].Name;

    UpdateControls;
      
    Result := ShowModal = mrOk;
    If Result Then
      Begin
        For i := 0 To DeleteList.Count - 1 Do
          Begin
            A := IndexOfName(DeleteList[i]);
            If A >= 0 Then
              Items[A].Free;
          End;
        If SaveDlg Then
          Begin
            If SaveToMemory(Edit.Text,ListItems) = Nil Then
              PSCShowMessage(PSCConsts.ErrSystemSlot);
          End
        Else
          LoadFromMemory(Edit.Text,ListItems);
      End;
  Finally
    Form.Free;
  End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SaveToMemoryDlg;
Begin
  If (fboCanSaveToMem In Options) Or (csDesigning In ComponentState) Then
    MemorySlots.CallMemoryDlg(True,Items);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.LoadFromMemoryDlg;
Begin
  If (fboCanLoadFromMem In Options) Or (csDesigning In ComponentState) Then
    MemorySlots.CallMemoryDlg(False,Items);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetMemorySlots(V: TPSCListMemorySlots);
Begin
  MemorySlots.Assign(V);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.Assign(Source: TPersistent);
Begin
  If Source Is TPSCCustomListBox Then
    Items.Assign(TPSCCustomListBox(Source).Items)
  Else
    Inherited;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetParent(AParent: TWinControl);
Begin
  Inherited;
  If AParent <> Nil Then
    Begin
      ItemIndex := 0;
      FontChanged;
    End;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetParamLookupDef(Item: TPSCListItem;
  Param: TPSCListParam): TPSCLookupDef;
Begin
  Result := Param.GetLookupDef;
  If Assigned(FOnGetLookupDef) Then
    FOnGetLookupDef(Self,Item,Param,Result);
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetSelColor(V: TPSCColor);
Begin
  If FSelColor <> V Then
    Begin
      FSelColor := V;
      If Not FHideSelected Then
        Invalidate;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetSelFontColor(V: TPSCColor);
Begin
  If FSelFontColor <> V Then
    Begin
      FSelFontColor := V;
      FontChanged;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.SetHideSelected(V: boolean);
Begin
  If FHideSelected <> V Then
    Begin
      FHideSelected := V;
      Invalidate;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickBoolParam;
Var
  Temp: IPSCStrings;
  idx: Integer;
Begin
  Temp := PSCCreateStringList;
  Temp.Add(PSCConsts.BoolDispFalse);
  Temp.Add(PSCConsts.BoolDispTrue);
  If PopupParam.IsNull Or (PopupParam.AsBoolean = False) Then
    Idx := 0
  Else
    Idx := 1;
  DoPopupListBox(Temp,Idx,OnPopupBoolClosed,PopupParamRect,Point(0,0));
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PickStrings(const Strings: IPSCStrings; GetProc:
  TPSCGetStringsProc;
  Const SelString: String; OnPopupClosed: TPSCOnPopupClosed;
  Sorted: boolean; PopupSize: TPoint);
Var
  i,idx: Integer;
Begin
  FPickStrings := PSCCreateStringList(ioOwned);

  If Assigned(Strings) Then
    FPickStrings.Assign(Strings)
  Else
    GetProc(FPickStrings);

  PSCGetBoldPartInStrings(FPickStrings);

  For i := 0 To FPickStrings.Count - 1 Do
    FPickStrings.Objects[i] := TPSCValuesContainer.Create(i);

  If Sorted Then
    FPickStrings.Sorted := True;

  Idx := FPickStrings.IndexOf(PSCGetBoldPartOrString(SelString));

  DoPopupListBox(FPickStrings,Idx,OnPopupClosed,PopupParamRect,PopupSize);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.OnPopupPickListClosed(Sender: TObject; Canceled:
  boolean);
Var
  PickList: IPSCStrings;
  ValStr,DisplValStr: String;
  idx: integer;
  S: String;
Begin
  If Not Canceled Then
    Begin
      With TPSCPopupListBox(FPopupEdit).ListBox Do
        S := Items[ItemIndex].Caption;
      PickList := PSCCreateStringList;
      GetParamPickList(PopupItem,PopupParam,PickList);
      idx := PickList.IndexOf(S);
      If Idx < 0 Then
        Idx := PickList.IndexOfName(S);
      S := PickList[idx];
      PSCExtractPickListValues(S,DisplValStr,ValStr);
      With PopupParam Do
        Begin
          Collection.BeginUpdate;
          Try
            SafeSetValue(ValStr);
            DisplayValue := DisplValStr;
          Finally
            Collection.EndUpdate;
          End;
        End;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.OnPopupLookupClosed(Sender: TObject; Canceled:
  boolean);
Var
  LookupTable: TDataSet;
Begin
  LookupTable := FPopupLookupDef.DataSet;

  If LookupTable=nil then
    exit;

  try
    If Not Canceled Then
      Try
        If (Not LookupTable.IsEmpty) Then
          Begin
            PopupParam.Value := LookupTable.FieldByName(
              FPopupLookupDef.KeyField).Value;
            PopupParam.DisplayValue := LookupTable.FieldByName(
              FPopupLookupDef.DisplayField).Value;
          End;
      Except
        FItemParam.Clear;
      End;
  finally
    If (fboLookupDSClose in Options) and (LookupTable.Active) then
      LookupTable.Active:=False;
  end;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.ParamHasPickList(Item:TPSCListItem;
  Param:TPSCListParam):boolean;
var
  PickList:IPSCStrings;
begin
  PickList:=PSCCreateStringList;
  GetParamPickList(Item,Param,PickList);
  Result:=PickList.Count>0;
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PrepareLookupDataSet(ADataSet:TDataSet);
begin
  If ADataSet=nil then
    exit;
  If (fboLookupDSRefresh in Options) or
     ((fboLookupDSOpen in Options) and (not ADataSet.Active))
  then
    begin
      If ADataSet.Active then
        ADataSet.Active:=False;
      ADataSet.Active:=True;
    end;
end;

{------------------------------------------------------------------}

procedure TPSCDBGridPopup.SetColumns(const S:String);
var
  MyGridFields:IPSCStrings;
  i:Integer;
begin
  MyGridFields:=PSCCreateStringList;
  PSCParseString(S,';',MyGridFields);
  FGrid.Columns.Clear;
  for i:=0 to MyGridFields.Count-1 do
    With TColumn(FGrid.Columns.Add) do
    begin
      FieldName:=MyGridFields[i];
    end;
  If FGrid.Columns.Count>1 then
    FGrid.Options:=FGrid.Options+[dgTitles,dgRowLines,dgRowSelect,
      dgColLines,dgColumnResize];
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.DefaultPickLookupParam(LookupDef: TPSCLookupDef);
Begin
  FPopupEdit.Free;
  FPopupEdit:=nil;

  If LookupDef.DataSet=nil then
    exit;

  FPopupEdit := CPSCUsedPopupLookupClass.CreateNew(Nil,0);
  With TPSCDBGridPopup(FPopupEdit) Do
    Begin
      DataSet := LookupDef.DataSet;
      FGrid.Options:=[];

      If LookupDef.GridFields<>'' then
        SetColumns(LookupDef.GridFields)
      else
        SetColumns(LookupDef.DisplayField);

      PrepareLookupDataSet(DataSet);

      If not DataSet.Active then
        begin
          PSCbeep;
          exit;
        end;

      If PopupParam.IsNull Then
        DataSet.First
      Else
        DataSet.Locate(LookupDef.KeyField,PopupParam.Value,[]);

      Self.UpdatePopupParams(FPopupEdit);
      Popup(Self,PopupParamRect,OnPopupLookupClosed);
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.PickLookupParam(LookupDef: TPSCLookupDef);
Var
  Processed: boolean;
Begin
  If LookupDef.DisplayField = '' Then
    LookupDef.DisplayField := LookupDef.KeyField;
  FPopupLookupDef := LookupDef;
  If Assigned(FOnPickLookupParam) Then
    Begin
      Processed := False;
      FOnPickLookupParam(Self,PopupItem,PopupParam,PopupParamRect,
        FPopupLookupDef,Processed);
      If Processed Then
        exit;
    End;

  DefaultPickLookupParam(FPopupLookupDef);
End;

{--------------------------------------------}

procedure TPSCCustomListBox.FontChanged;
Begin
  If not HandleAllocated then
    exit;
  Canvas.Font:=Font;
  Canvas.Font.Style:=Canvas.Font.Style+[FontStyle_Underline];
  ItemHeight := PSCMax(Canvas.TextHeight('Wg') + 2,16);
  UpdateItemsLength(True);
End;

{-------------------------------------------}

Function TPSCCustomListBox.GetParamWithTags(Const S: String;
  Underline,Hover,AIsParameter:boolean): String;
Begin
  Result := '~' + S + '~';
  If Hover Then
    Result := FHoverLinkColorStr + Result + '</c>';
  If Underline Then
    Result := SPSCUnderlineBeg + Result + SPSCUnderlineEnd;
  If AIsParameter Then
    Result := FLinkColorStr + Result + '</c>';
End;

{---------------------------------------------------}

function TPSCCustomListBox.GetListItemClass: TPSCListItemClass;
Begin
  Result := TPSCListBoxItem;
End;

{-------------------------------------------}

function TPSCCustomListBox.GetListItemsClass: TPSCListItemsClass;
Begin
  Result := TPSCListItems;
End;

{--------------------------------------------}

Const
  DefaultItemHeight = 15;

{-------------------------------------------}

Constructor TPSCCustomListBox.Create(AOwner: TComponent);

  procedure MyLoadBitmap(AKind:TPSCListBoxImageKind;const AResName:String);
  begin
    FImageIndexes[AKind]:=PSCAddBitmapFromResource(FImageList,AResName);
  end;

Begin
  FVersion:=SPSCProductVerNo;
  Inherited;
  AutoComplete:=True;
  FDummyStrings:=PSCCreateStringList;
  FPopupParams := TPSCPopupParams.Create(Self);
  If NewStyleControls Then
    ControlStyle := [csSetCaption,csDoubleClicks,csOpaque,csClickEvents]
  Else
    ControlStyle := [csSetCaption,csDoubleClicks,csFramed,csOpaque,csClickEvents];

  FShowAddItem := cPSCDefaultShowAddItem;
  FItemHeight := DefaultItemHeight;
  FItemIndex := -1;
  FIntegralHeight := True;
  FAddItemText := PSCConsts.ClickHereToAddItem;
  FClickHereColor := cPSCDefaultClickHereColor;
  Ctl3d := True;
  ParentColor := False;
  TabStop := True;
  FOptions := cPSCListBoxDefaultOptions;
  FEditWithMouse := cPSCDefaultEditWithMouse;
  MergePopupMenus := cPSCDefaultMergePopupMenus;
  FNewItemChecked := cPSCDefaultNewItemChecked;
  FItems := GetListItemsClass.Create(Self,GetListItemClass);
  FItems.OnGetDisplayName:=OnGetDisplayName;
  ReadOnly := cPSCDefaultReadOnly; //after create of the FItems!
  FItems.OnUpdate := OnUpdate;
  FCheckBoxes := cPSCDefaultCheckBoxes;
  IntegralHeight := True;
  FHideFocus := cPSCDefaultHideFocus;
  FSelColor := cPSCDefaultSelColor;
  FSelFontColor := cPSCDefaultSelFontColor;
  DefaultPopup := TPopupMenu.Create(Self);
  ShowDefaultPopup := cPSCDefaultShowDefaultPopup;
  FImageList := TPSCImageList.Create(Self);

  {0}MyLoadBitmap(Image_NewItem,SPSCResName_BTN_NEWDOC);
  {1}MyLoadBitmap(Image_DeleteItem,SPSCResName_BTN_DELETE);
  {2}MyLoadBitmap(Image_IndentItem,SPSCResName_BTN_INDENT);
  {3}MyLoadBitmap(Image_OutDentItem,SPSCResName_BTN_OUTDENT);
  {4}MyLoadBitmap(Image_OpenFilter,SPSCResName_BTN_OPEN);
  {5}MyLoadBitmap(Image_SaveFilter,SPSCResName_BTN_SAVE);
  {6}MyLoadBitmap(Image_SortAscSmall,SPSCResName_BTN_SORTASC_SMALL);
  {7}MyLoadBitmap(Image_SortDescSmall,SPSCResName_BTN_SORTDESC_SMALL);
  {8}MyLoadBitmap(Image_SortAsc,SPSCResName_BTN_SORTASC);
  {9}MyLoadBitmap(Image_SortDesc,SPSCResName_BTN_SORTDESC);

 {10}MyLoadBitmap(Image_Filter,SPSCResName_BTN_FILTER);
 {11}MyLoadBitmap(Image_RunFilter,SPSCResName_BTN_RUNFILTER);
 {12}MyLoadBitmap(Image_SearchFirst,SPSCResName_BTN_SEARCHFIRST);
 {13}MyLoadBitmap(Image_SearchLast,SPSCResName_BTN_SEARCHLAST);
 {14}MyLoadBitmap(Image_SearchPrev,SPSCResName_BTN_SEARCHPREV);
 {15}MyLoadBitmap(Image_SearchNext,SPSCResName_BTN_SEARCHNEXT);
 {16}MyLoadBitmap(Image_Search,SPSCResName_BTN_SEARCH);

  FActionList := TPSCActionList.Create(Self);
  CreateActions;
  AddActionsToPopup;
  FActionList.Images := FImageList;
  DefaultPopup.Images := FImageList;

  FHideHoverLink := cPSCDefaultHideHoverLink;
  FLinkColor := cPSCDefaultLinkColor;
  FHideSelected := cPSCDefaultHideSelected;
  FHoverLinkColor := cPSCDefaultHoverLinkColor;
  FLinkColorStr := PSCColorToHTString(LinkColor);
  FHoverLinkColorStr := PSCColorToHTString(HoverLinkColor);
  FMemorySlots := TPSCListMemorySlots.Create(Self,GetListMemorySlotClass);
  FMemorySlots.ListItemClass := GetListItemClass;
  Width := 160;
  Height := 97;

  Items.DisplayDateTimeFormat.OnChange := DisplayDateTimeFormatChanged;

End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.DisplayDateTimeFormatChanged(Sender:TObject);
begin
  Invalidate;
end;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetListMemorySlotClass: TPSCListMemorySlotClass;
Begin
  Result := TPSCListMemorySlot;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetHoverLinkColor(V: TPSCColor);
Begin
  If FHoverLinkColor <> V Then
    Begin
      FHoverLinkColor := V;
      FHoverLinkColorStr := PSCColorToHTString(HoverLinkColor);
      Invalidate;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetLinkColor(V: TPSCColor);
Begin
  If FLinkColor <> V Then
    Begin
      FLinkColor := V;
      FLinkColorStr := PSCColorToHTString(LinkColor);
      Invalidate;
    End;
End;

{-------------------------------------------}

procedure TPSCLBPopupForm.AddButton(AActionClass:TPSCComponentActionClass);
var
  MyAction:TPSCAction;
begin
  If FListBox=nil then
    exit;
  MyAction := PSCActionByClass(FListBox.FActionList,
    AActionClass);
  If MyAction<>nil then
    With AddButton do
    begin
      Action:=MyAction;
      Caption:='';
    end;
end;

{-------------------------------------------}

constructor TPSCPopupEdit.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Var
  FParent: TWinControl;
Begin
  Inherited;
  FParent := Self;
  FEditor := TPSCEdit.Create(Self);
  With FEditor Do
  Begin
    Parent := FParent;
    Width:=Width*2;
    Top:=4;
    Left:=4;
  End;

  FCheckBoxIgnoreCase := TPSCCheckBox.Create(Self);

  With FCheckBoxIgnoreCase Do
  Begin
    Enabled:=False;
    Checked:=False;
    FCheckBoxIgnoreCase.Caption:=PSCConsts.IgnoreCase;
    Top:=FEditor.BoundsRect.Bottom+4;
    Left:=4;
    Width:=Width*2;
    Parent := FParent;
  end;

  FCheckBoxAutoUpdate := TPSCCheckBox.Create(Self);

  With FCheckBoxAutoUpdate Do
  Begin
    Enabled:=False;
    FCheckBoxAutoUpdate.Caption:=PSCConsts.AutoUpdate;
    Top:=FCheckBoxIgnoreCase.BoundsRect.Bottom+4;
    Left:=4;
    Width:=Width*2;
    Parent := FParent;
  end;

  ClientHeight := FCheckBoxAutoUpdate.BoundsRect.Bottom+4+GetFooterPanel.Height;
  ClientWidth := FEditor.BoundsRect.Right+8;
  ActiveControl := FEditor;
End;

{------------------------------}

procedure TPSCCustomListBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{-------------------------------------------}

Procedure PSCListBoxInitDefaultKeyboard(Instance: TObject);
Begin
  With TPSCCustomListBox(Instance),Keys Do
    Begin
      AddSimpleKey(VK_Down, [ssCtrl],MoveItemDown);
      AddSimpleKey(VK_Up, [ssCtrl],MoveItemUp);

      AddNoShiftKey(VK_Home,JumpToFirstItem);
      AddNoShiftKey(VK_End,JumpToLastItem);
      AddNoShiftKey(VK_Down,JumpToNextItem);
      AddNoShiftKey(VK_Up,JumpToPrevItem);
      AddNoShiftKey(VK_NEXT,JumpPageDown);
      AddNoShiftKey(VK_PRIOR,JumpPageUp);
      AddNoShiftKey(VK_DELETE,DeleteCurrentItem);
      AddNoShiftKey(VK_INSERT,AddItemProc);
      AddNoShiftKey(VK_LEFT,JumpToPrevParam);
      AddNoShiftKey(VK_RIGHT,JumpToNextParam);
      AddNoShiftKey(VK_Return,PickCurrentParameter);

      AddSimpleKey(VK_Down, [ssAlt],PickCurrentParameter);
      AddSimpleKey(VK_DELETE, [ssCtrl],DeleteAllItems);
      AddSimpleKey(VK_LEFT, [ssCtrl],OutDentCurrentItem);
      AddSimpleKey(VK_RIGHT, [ssCtrl],IndentCurrentItem);

      AddSimpleKey(Word('S'), [ssCtrl,ssShift],SaveToMemoryDlg);
      AddSimpleKey(Word('L'), [ssCtrl,ssShift],LoadFromMemoryDlg);

    End;

End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetOptions(V: TPSCListBoxOptions);
Begin
  If FOptions <> V Then
    Begin
      FOptions := V;
      Invalidate;
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetReadOnly: boolean;
Begin
  Result := Items.ReadOnly;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetReadOnly(V: boolean);
Begin
  If V <> Items.ReadOnly Then
    Begin
      Items.ReadOnly := V;
      Invalidate;
    End;
End;

{------------------------------------------------------------------}

Function TPSCCustomListBox.GetItems: TPSCListItems;
begin
  Result:=FItems;
end;

{------------------------------------------------------------------}

Function TPSCCustomListBox.GetMemorySlots: TPSCListMemorySlots;
begin
  Result:=FMemorySlots;
end;

{------------------------------------------------------------------}

function TPSCCustomListBox.GetOnQuoteStr: TPSCOnQuoteStr;
Begin
  Result := Items.OnQuoteStr;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetOnQuoteStr(V: TPSCOnQuoteStr);
Begin
  Items.OnQuoteStr := V;
End;

{------------------------------------------------------------------}

Function TPSCCustomListBox.GetAsStrings:IPSCStrings;
var
  i:Integer;
begin
  Result:=FDummyStrings;
  Result.Clear;
  for i:=0 to Items.Count-1 do
    With Items[i] do
      Result.AddObject(Caption,DataObject);
end;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetAsStrings(const V:IPSCStrings);
var
  i:Integer;
begin
  With Items do
  begin
    BeginUpdate;
    try
      Clear;
      for i:=0 to V.Count-1 do
        With Self.AddItem(False) do
        begin
          Caption:=V[i];
          DataObject:=V.Objects[i];
        end;
    finally
      EndUpdate;
    end;
  end;
end;

{------------------------------------------------------------------}

function TPSCCustomListBox.AddItemTextStored: boolean;
Begin
  Result := AddItemText <> PSCConsts.ClickHereToAddItem;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  Params.Style := Params.Style Or WS_HSCROLL Or WS_VSCROLL Or WS_TABSTOP;
  Params.WindowClass.style := Params.WindowClass.style Or CS_DBLCLKS;
  PSCUpdateParamsWithBorderStyle(Params,BorderStyle,Ctl3d);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.WMEraseBkgnd(Var Msg: TWMEraseBkgnd);
Begin
  Msg.Result := 1;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.UpdateScrollPos;

  Procedure SetVertScroll;
  Begin
    SetScrollPos(SB_Vert,TopIndex);
  End;

  Procedure SetHorzScroll;
  Begin
    SetScrollPos(SB_Horz,FHorzPosition);
  End;

Begin
  If HandleAllocated Then
    Begin
      If VertScrollVisible Then
        SetVertScroll;
      If HorzScrollVisible Then
        SetHorzScroll;
    End;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.HorzScrollVisible: boolean;
Begin
  Result := FMaxLength > ClientWidth;
End;

{------------------------------------------------------------------}

function TPSCCustomListBox.VertScrollVisible: boolean;
Begin
  Result := Items.Count >= ClientHeight Div ItemHeight;
  If (Not Result) And (ClientHeight Mod ItemHeight = 0) Then
    Result := Items.Count >= ClientHeight Div ItemHeight + 1
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.UpdateScrollSize;

  Procedure SetHorzScroll;
  Var
    ScrollVisible: Boolean;
  Begin
    ScrollVisible := HorzScrollVisible;
    If ScrollVisible Then
      SetScrollSize(SB_Horz,0,FMaxLength,ClientWidth)
    Else
      SetScrollSize(SB_HORZ,0,FMaxLength, -1);
  End;

  Procedure SetVertScroll;
  Var
    ScrollVisible: Boolean;
    Index: Integer;
    MyPageSize: Integer;
  Begin
    ScrollVisible := VertScrollVisible;

    Index := Items.Count-1;

    MyPageSize := ClientHeight Div ItemHeight;
    If RealShowAddItem Then
      Dec(MyPageSize);

    If ScrollVisible Then
      SetScrollSize(SB_Vert,0,Index,MyPageSize)
    Else
      SetScrollSize(SB_Vert,0,Index, -1);

  End;

Begin
  If HandleAllocated Then
    Begin
      SetVertScroll;
      SetHorzScroll;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.UpdateScrollBars;
Begin
  Inherited;
  UpdateScrollSize;
  UpdateScrollPos;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetIntegralHeight(Value: Boolean);
Begin
  If Value <> FIntegralHeight Then
    Begin
      FIntegralHeight := Value;
      If Value Then
        Height := Height;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetAddItemText(Value: String);
Begin
  If FAddItemText <> Value Then
    Begin
      FAddItemText := Value;
      If HandleAllocated Then
        InvalidateItem(-2);
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.InvalidateItem(Item: Integer);
Var
  Rect: TRect;
Begin
  If ((Item <> -2) And (Item < 0)) Or (Item >= Items.Count) Then
    exit;
  Rect := ItemRect(Item);
  InvalidateRect(Rect);
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetItemHeight(Value: Integer);
Begin
  If FItemHeight <> Value Then
    Begin
      FItemHeight := Value;
      If IntegralHeight Then
        Height := Height;
      Invalidate;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetTopIndex(Value: Integer);
var
  MyPageSize:Integer;
Begin
  MyPageSize := ClientHeight Div ItemHeight;
  If RealShowAddItem Then
    Dec(MyPageSize);

  Value:=PSCMin(Value,Items.Count-MyPageSize);
  Value:=PSCMax(Value,0);

  If FTopIndex <> Value Then
    Begin
      FTopIndex := Value;
      If Items.UpdateCount = 0 Then
      begin
        UpdateScrollPos;
        Invalidate;
      end;  
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetItemIndex(Value: Integer);
Begin
  If Value >= Items.Count Then
    Value := Items.Count - 1;
  If (FItemIndex <> Value) And (Value >= -1) Then
    Begin
      If Value < -1 Then
        Value := 0;
      FItemIndex := Value;
      If (Value < TopIndex) And (Value <> -1) Then
        TopIndex := Value
      Else
        If Value - TopIndex >= VisibleItemCount Then
          TopIndex := Value - VisibleItemCount+1
        Else
          If Items.UpdateCount = 0 Then
            Invalidate;
    End;
  Click;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.SetShowAddItem(Value: Boolean);
Begin
  If FShowAddItem <> Value Then
    Begin
      FShowAddItem := Value;
      If HandleAllocated Then
        Invalidate;
    End;
End;

{--------------------------------------}

Function PSCGetStringsMaxWidth(ACanvas:TPSCCanvas;
  const Strings: IPSCStrings): Integer;
Var
  i: Integer;
Begin
  Result := 0;
  For i := 0 To Strings.Count - 1 Do
    Result := PSCMax(Result,ACanvas.TextWidth(Strings[i]));
End;

{-------------------------------------------}

procedure TPSCCustomListBox.DoPopupListBox(const Strings: IPSCStrings;
  SelItem: Integer;CallBack: TPSCOnPopupClosed; ParamRect: TRect;
  PopupSize: TPoint);
Var
  TempItemHeight: Integer;
  MyClientWidth:Integer;
Begin
  if not HandleAllocated then
    exit;

  FPopupEdit.Free;
  FPopupEdit:=nil;
  FPopupEdit := CPSCUsedPopupListBoxClass.CreateNew(Nil,0);
  With TPSCPopupListBox(FPopupEdit) Do
    Begin
      ListBox.SetAsStrings(Strings);
      ListBox.HandleNeeded;
      SelItem := PSCMax(SelItem,0);
      ListBox.ItemIndex := SelItem;
      If PopupSize.X <> 0 Then
        Begin
          Width := PopupSize.X;
          Height := PopupSize.Y;
        End;

      If fboAutoSizePopups In Options Then
        Begin
          Self.Canvas.Font:=Self.Font;

          MyClientWidth := PSCGetStringsMaxWidth(Self.Canvas,Strings)
            + (ListBox.Width - ListBox.ClientWidth) + 28;

          ClientWidth:=PSCMin(MyClientWidth,Screen.Width);

          TempItemHeight := Self.Canvas.TextHeight(Strings[0]);

          ClientHeight := PSCMin(20,ListBox.Items.Count+2) * TempItemHeight +
            (ListBox.Height - ListBox.ClientHeight)+GetFooterPanel.Height+16;
        End;

      Width := PSCMax(Width,cPSCMinPopupListWidth);
      Height := PSCMax(Height,cPSCMinPopupListHeight);

      Self.UpdatePopupParams(FPopupEdit);

      Popup(Self,ParamRect,CallBack);
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.OnPopupCalcClosed(Sender: TObject; Canceled: boolean);
Begin
  If Not Canceled Then
    Begin
      If Not (PopupParam.DataType In [FT_FLOAT,FT_CURRENCY]) Then
        PopupParam.Value :=
          Integer(Round(TPSCCalculatorPopup(FPopupEdit).Calculator.Value))
      Else
        PopupParam.Value := TPSCCalculatorPopup(FPopupEdit).Calculator.Value;
      PopupParam.DisplayValue := PopupParam.Value;
    End;
End;

{-------------------------------------------}

procedure TPSCCustomListBox.PickWithCalc;
Begin
  FPopupEdit.Free;
  FPopupEdit:=nil;
  FPopupEdit := CPSCUsedPopupCalcClass.CreateNew(Nil,0);
  With TPSCCalculatorPopup(FPopupEdit) Do
    Begin
      If Not PopupParam.IsNull Then
        Calculator.Value := PopupParam.Value;
      Self.UpdatePopupParams(FPopupEdit);
      Popup(Self,PopupParamRect,OnPopupCalcClosed);
    End;
End;

{-------------------------------------------}

function TPSCCustomListBox.IsItemOptionEnabled(
  AOption:TPSCListItemOption):Boolean;
begin
  Result:=False;
end;

{-------------------------------------------}

procedure TPSCCustomListBox.ParamValueOnChange(Sender:TObject);
begin
end;

{-------------------------------------------}

procedure TPSCCustomListBox.PickTextParam;
Begin
  FPopupEdit.Free;
  FPopupEdit:=nil;
  FPopupEdit := CPSCUsedPopupEditClass.CreateNew(Nil,0);
  With TPSCPopupEdit(FPopupEdit) Do
    Begin
      Editor.OnKeyPress:=OnEditKeyPress;
      Editor.Text := PopupParam.GetAsText;
      CheckBoxIgnoreCase.Enabled:=IsItemOptionEnabled(ListItem_IgnoreCase);
      CheckBoxIgnoreCase.Checked:=(ListItem_IgnoreCase in PopupItem.Options) and
        CheckBoxIgnoreCase.Enabled;
      CheckBoxAutoUpdate.Enabled:=IsItemOptionEnabled(ListItem_AutoUpdate);
      CheckBoxAutoUpdate.Checked:=(ListItem_AutoUpdate in PopupItem.Options)
        and CheckBoxAutoUpdate.Enabled;
      CheckBoxIgnoreCase.OnClick:=ParamValueOnChange;
      CheckBoxAutoUpdate.OnClick:=ParamValueOnChange;
      FEditor.OnChange:=ParamValueOnChange;
      Self.UpdatePopupParams(FPopupEdit);
      Popup(Self,PopupParamRect,OnPopupEditClosed);
      Editor.SelectAll;
      Invalidate;
    End;
End;

{-------------------------------------------------------------}

procedure TPSCCustomListBox.InitDefaultKeyMapping;
Begin
  PSCListBoxInitDefaultKeyboard(Self);
End;

{-------------------------------------------}

Procedure TPSCPrmListAction.SetParamList(V: TPSCCustomListBox);
Begin
  If V <> FParamList Then
    Begin
      FParamList := V;
      If V <> Nil Then
        V.FreeNotification(Self);
    End;
End;

{-------------------------------------------}

procedure TPSCPrmListAction.SetComponent(V:TComponent);
begin
  ParamList:=TPSCCustomListBox(V);
end;

{-------------------------------------------}

Function TPSCPrmListAction.GetParamList(Target: TObject): TPSCCustomListBox;
Begin
  Result := (Target As TPSCCustomListBox);
End;

{-------------------------------------------}

Procedure TPSCPrmListAction.Notification(AComponent: TComponent; Operation:
  TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (AComponent = ParamList) Then
    ParamList := Nil;
End;

{-------------------------------------------}

Function TPSCPrmListAction.HandlesTarget(Target: TObject): Boolean;
Begin
  Result := ((ParamList <> Nil) And (Target = ParamList))
    Or (Target Is TPSCCustomListBox);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionAdd.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).AddItem(True);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionAdd.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).CanAddItem;
  Visible := fboCanAdd In GetParamList(Target).Options;
End;

{-------------------------------------------}

Function TPSCCustomListBox.CanAddItem:Boolean;
begin
  Result:=(Not ReadOnly) and (fboCanAdd in GetRealOptions);
end;

{-------------------------------------------}

Procedure TPSCPrmListActionDelete.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).DeleteCurrentItem;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionDelete.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).CanDeleteCurrentItem;
  Visible := fboCanDelete In GetParamList(Target).Options;
End;

{-------------------------------------------}

Constructor TPSCPrmListActionClear.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := -1;
  Caption := PSCConsts.ListPopupClear;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxClearKey);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionClear.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).DeleteAllItems;
End;

{-------------------------------------------}

function TPSCCustomListBox.IsAnyItemReadOnly: boolean;
Var
  i: Integer;
Begin
  Result := ReadOnly;
  If Result Then
    exit;
  For i := 0 To Items.Count - 1 Do
    If TPSCListItem(Items[i]).ReadOnly Then
      Begin
        Result := True;
        exit;
      End;
End;

{-------------------------------------------}

function TPSCCustomListBox.CanClear: boolean;
Begin
  Result := (Items.Count > 0) And (Not IsAnyItemReadOnly);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionClear.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).CanClear;
  Visible := fboCanDelete In GetParamList(Target).Options;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionInDent.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).IndentCurrentItem;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionInDent.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).CanIndentCurrentItem;
  Visible := fboCanIndent In GetParamList(Target).Options;
End;

{-------------------------------------------}

Constructor TPSCPrmListActionLoadFromMem.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := -1;
  Caption := PSCConsts.PrmBoxLoadFromMem;
  ShortCut := PSCShortCutFromKeyDef(cPSCFltBoxLoadFromMemKey);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionLoadFromMem.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).LoadFromMemoryDlg;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionLoadFromMem.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).MemorySlots.Count > 0;
  Visible := fboCanLoadFromMem In GetParamList(Target).Options;
End;

{-------------------------------------------}

Constructor TPSCPrmListActionSaveToMem.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := -1;
  Caption := PSCConsts.PrmBoxSaveToMem;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxSaveToMemKey);
End;

{-------------------------------------------}

Procedure TPSCPrmListActionSaveToMem.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).SaveToMemoryDlg;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionSaveToMem.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).Items.Count > 0;
  Visible := fboCanSaveToMem In GetParamList(Target).Options;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionOutDent.ExecuteTarget(Target: TObject);
Begin
  GetParamList(Target).OutDentCurrentItem;
End;

{-------------------------------------------}

Procedure TPSCPrmListActionOutDent.UpdateTarget(Target: TObject);
Begin
  Enabled := GetParamList(Target).CanOutDentCurrentItem;
  Visible := fboCanIndent In GetParamList(Target).Options;
End;

{-------------------------------------------}

Constructor TPSCPrmListActionAdd.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := Integer(Image_NewItem);
  Caption := PSCConsts.ListPopupAdd;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxAddKey);
End;

{-------------------------------------------}

Constructor TPSCPrmListActionDelete.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := Integer(Image_DeleteItem);
  Caption := PSCConsts.ListPopupDelete;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxDelKey);
End;

{-------------------------------------}

Procedure TPSCCustomListBox.SetImages(Value: TPSCImageList);
Begin
  If FImages <> Value Then
    Begin
      If FImages <> Nil Then
        RemoveFreeNotification(FImages);
      FImages := Value;
      If FImages <> Nil Then
        FreeNotification(FImages);
      If FShowImages Then
        Invalidate;
    End
End;

{-------------------------------------}

Procedure TPSCCustomListBox.SetShowImages(Value: Boolean);
Begin
  If FShowImages <> Value Then
    Begin
      FShowImages := Value;
      Invalidate;
    End
End;

{-------------------------------------------}

Procedure TPSCCustomListBox.OnGetDisplayName(Sender: TObject;
  AItem: TPSCNamedItem;Var ADisplayName: String);
Begin
  ADisplayName := GetWithRemovedTags(TPSCListItem(AItem));
  If ADisplayName <> '' Then
    ADisplayName := PSCStringOfSpace(TPSCListItem(AItem).IndentLev * 3) +
      ADisplayName;
End;

{-------------------------------------------}

Function TPSCListMemorySlots.CanDeleteMemSlot(Const SlotName: String): boolean;
Begin
  Result := (DesignTime) Or (Not IsSystemSlot(SlotName));
End;

{-------------------------------------------}

Constructor TPSCPrmListActionInDent.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := Integer(Image_IndentItem);
  Caption := PSCConsts.ListPopupIndent;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxIndentKey);
End;

{-------------------------------------------}

Constructor TPSCPrmListActionOutDent.Create(AOwner: TComponent);
Begin
  Inherited;
  ImageIndex := Integer(Image_OutDentItem);
  Caption := PSCConsts.ListPopupOutDent;
  ShortCut := PSCShortCutFromKeyDef(cPSCListBoxOutDentKey);
End;

{-------------------------------------------}

function TPSCCustomListBox.GetImageIndex(AImageKind:TPSCListBoxImageKind):Integer;
begin
  Result:=FImageIndexes[AImageKind];
end;

{-------------------------------------------}

function TPSCCustomListBox.ItemIndentLevInPixels(Item: TPSCListItem): Integer;
Begin
  Result := Item.IndentLev * PSCCheckWidth;
End;

{-------------------------------------------}

Function TPSCListMemorySlots.IsSystemSlot(Const SlotName: String): boolean;
Var
  Index: Integer;
Begin
  Result := False;
  Index := IndexOfName(SlotName);
  If Index >= 0 Then
    Result := TPSCListMemorySlot(Items[Index]).SystemSlot;
End;

{-------------------------------------------}

Procedure TPSCListMemorySlot.SetItems(V: TPSCListItems);
Begin
  Items.Assign(V);
End;

{-------------------------------------------}

Function TPSCListMemorySlot.GetListItemsClass: TPSCListItemsClass;
Begin
  Result := TPSCListItems;
End;

{-------------------------------------------}

Constructor TPSCListMemorySlot.Create(ACollection: TCollection);
Begin
  Inherited;
  FItems := GetListItemsClass.Create(Self,
    TPSCListMemorySlots(Collection).ListItemClass);
  FSystemSlot := cPSCDefaultSystemSlot;
End;

{-------------------------------------------}

Destructor TPSCListMemorySlot.Destroy;
Begin
  FItems.Free;
  Inherited;
End;

{-------------------------------------------}

Function TPSCCustomListBox.GetItemDisplayCaption(Item: TPSCListItem;
  HoverCurrentParam: boolean): String;
Var
  i: Integer;
  V: String;
  MyUnderline:Boolean;
  MyHover:Boolean;
  MyIsParameter:Boolean;
Begin
  With Item Do
    Begin
      Result := Caption;
      If Result <> '' Then
        Begin
          For i := 0 To Params.Count - 1 Do
            Begin
              If Items.IsPrmVisible(Item,Params[i]) Then
                Begin
                  V := PSCGetParamDisplayValue(Params[i]);
                  MyHover:=HoverCurrentParam And (i = CurrentParam);
                  MyIsParameter:=Params[i].Underline;
                  MyUnderline:= MyIsParameter and
                    (not (fboHideUnderline in Self.Options));
                  V := GetParamWithTags(V,MyUnderline,MyHover,MyIsParameter);
                End
              Else
                V := '';
              PSCReplaceAllOccur(Result, ':' + Params[i].Name,V);
            End;
        End;
    End;
End;

{------------------------------------------------------------------}

procedure TPSCCustomListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  If (Operation=OpRemove) and (AComponent=Images) then
    Images:=nil;
end;

{-------------------------------------------}

function TPSCCustomListBox.IsURLPos(P: TPoint): boolean;
Begin
  Result := False;
  P := MousePosToStrPos(P);
  With P Do
    Begin
      If (X <= 0) Or (Y < 0) Then
        exit;
      Result := PSCIsCharInBoldSection(
        RemovePaintTags(GetItemDisplayCaption(Items[Y],False)),X);
    End;
End;

{------------------------------------------------------------------------------}

procedure TPSCDBGridPopup.UpdateGrid;
begin
  If FGrid<>nil then
  begin
    If FGrid.Columns.Count=1 then
      FGrid.Columns.Items[0].Width:=FGrid.ClientWidth;
  end;
end;

{------------------------------------------------------------------------------}

procedure TPSCDBGridPopup.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateGrid;
end;

{------------------------------------------------------------------------------}

procedure TPSCDBGridPopup.Resize;
begin
  inherited;
  UpdateGrid;
end;

{------------------------------------------------------------------------------}

constructor TPSCDBGridPopup.CreateNew(AOwner: TComponent; Dummy: Integer=0);
Begin
  Inherited;
  BorderStyle := bsSizeToolWin;
  GetFooterPanel;

  FSubPanel:=TPSCPanel.Create(Self);

  With FSubPanel do
  begin
    Parent:=Self;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    Align:=alClient;
    ParentColor:=True;
  end;

  FGrid := TPSCDBGrid.Create(Self);
  With FGrid Do
    Begin
      Parent := FSubPanel;
      Self.ClientWidth := Width;
      Self.ClientHeight := Height+GetFooterPanel.Height;
      Align := alClient;
      BorderStyle := bsNone;
      OnCellClick := CellClicked;
      ParentColor:=True;
      Options:=Options-[dgAlwaysShowSelection];
    End;

  BorderStyle := bsSizeToolWin;
  ActiveControl := FGrid;
End;

{------------------------------------------------------------------------------}

Function TPSCDBGridPopup.GetDataSource: TDataSource;
Begin
  Result := FGrid.DataSource;
End;

{------------------------------------------------------------------------------}

Procedure TPSCDBGridPopup.SetDataSource(Value: TDataSource);
Begin
  If (FGrid.DataSource <> Nil) And (FGrid.DataSource.Owner = Self) Then
    FGrid.DataSource.Destroy;
  FGrid.DataSource := Value;
End;

{------------------------------------------------------------------------------}

Procedure TPSCDBGridPopup.SetDataSet(Value: TDataSet);
Begin
  If DataSource = Nil Then
    DataSource := TDataSource.Create(Self);
  DataSource.DataSet := Value;
End;

{------------------------------------------------------------------------------}

Function TPSCDBGridPopup.GetDataSet: TDataSet;
Begin
  If DataSource <> Nil Then
    Result := DataSource.DataSet
  Else
    Result := Nil
End;

{------------------------------------------------------------------------------}

procedure TPSCDBGridPopup.CellClicked(Column: TColumn);
Begin
  ClosePopup(false,not (ssCtrl in PSCKeysToShift));
End;

{------------------------------------------------------------------}

Function PSCGetBoldPartOrString(Const S: String): String;
Var
  A: Integer;
Begin
  A := Pos('~',S);
  Result := S;
  If A <> 0 Then
    Begin
      Result := Copy(Result,A + 1,MaxInt);
      A := Pos('~',Result);
      Result := Copy(Result,1,A - 1);
    End;
End;

{------------------------------------------------------------------}

Procedure PSCGetBoldPartInStrings(const Strings: IPSCStrings);
Var
  i: Integer;
Begin
  For i := 0 To Strings.Count - 1 Do
    Strings[i] := PSCGetBoldPartOrString(Strings[i]);
End;

{------------------------------------------------------------------------------}

destructor TPSCValueListPopup.Destroy;
begin
  inherited;
end;

{------------------------------------------------------------------------------}

procedure TPSCValueListPopup.SetValueList(const AValue:Variant);
var
  MyLowBound,MyHighBound:Integer;
  i:Integer;
  MyItem:TPSCListItem;
  MyField:TPSCListParam;
begin
  FListBox.DeleteAllItems;
  If VarIsNull(AValue) Or varIsEmpty(AValue) then
    exit;

  If not VarIsArray(AValue) then
  begin
    MyItem:=FListBox.AddItem(False);
    MyField:=TPSCListParam(MyItem.Params.ItemByName(SPSCValue));
    MyField.Value:=AValue;
    MyField.UpdateDisplayValue(LookupDef,PickList);
    exit;
  end;

  MyLowBound:=VarArrayLowBound(AValue,1);
  MyHighBound:=VarArrayHighBound(AValue,1);
  for i:=MyLowBound to MyHighBound do
    begin
      MyItem:=FListBox.AddItem(False);
      MyField:=TPSCListParam(MyItem.Params.ItemByName(SPSCValue));
      MyField.Value:=AValue[i];
      MyField.UpdateDisplayValue(LookupDef,PickList);
    end;
end;

{------------------------------------------------------------------------------}

procedure TPSCValueListPopup.ListBoxAdditem(Sender: TObject;AItem: TPSCListItem);
var
  MyField:TPSCListParam;
begin
  AItem.Caption:=' :'+SPSCValue;
  MyField:=TPSCListParam(AItem.Params.ItemByName(SPSCValue));
  MyField.Name:=SPSCValue;
  MyField.DataType:=FDataType;
  FListBox.ItemIndex:=AItem.Index;

  FPickList.AssignTo(PSCCreateStringsAdapter(MyField.PickList));
  MyField.PickType:=PickType;
  MyField.LookupKeyField:=LookupDef.KeyField;
  MyField.LookupDisplayField:=LookupDef.DisplayField;
  MyField.LookupGridFields:=LookupDef.GridFields;
  MyField.LookupDataSet:=LookupDef.DataSet;
end;

{------------------------------------------------------------------------------}

function TPSCValueListPopup.GetListBoxClass:TPSCListBoxClass;
begin
  Result:=CPSCUsedListBoxClass;
end;

{------------------------------------------------------------------------------}

constructor TPSCValueListPopup.CreateNew(AOwner: TComponent; Dummy: Integer=0);
var
  MyFooter:TPSCPanel;
begin
  inherited;
  BorderStyle := bsSizeToolWin;
  FListBox := GetListBoxClass.Create(Self);
  MyFooter := GetFooterPanel;
  FPickList:= PSCCreateStringList;

  With FListBox Do
    Begin
      Parent := Self;
      Self.ClientWidth := Width;
      Self.ClientHeight := Height+MyFooter.Height;
      Align := alClient;
      BorderStyle := bsNone;
      OnAddItem := ListBoxAddItem;
      Options := [fboCanAdd,fboCanDelete,fboAutoSizePopups,fboHideSortOrder];
      ShowAddItem:=True;
      AddItemText:=PSCConsts.ClickHereToAddValue;
      WantReturns:=True;
    End;
  Color := FListBox.Color;
  ActiveControl := FListBox;
  WantReturns:=False;
end;

{------------------------------------------------------------------------------}

end.
