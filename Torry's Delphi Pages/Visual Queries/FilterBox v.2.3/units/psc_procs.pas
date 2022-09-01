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
Unit psc_procs;
Interface
{$I psc_defines.inc}

Uses
  Xml.XmlDoc,
  Xml.XMLIntf,
  Xml.xmldom,

  System.Rtti,
  System.TypInfo,
  System.UITypes,
  System.Types,

{$IFDEF D6}
  System.variants,
{$ENDIF}
  Winapi.Windows,
  Winapi.messages,
  vcl.forms,
  vcl.controls,
  vcl.menus,
  data.DB,
  Winapi.activex, //needed because TVarType in D5 is declared there
  System.classes,
  vcl.extctrls,
  system.SysUtils,
  vcl.graphics,
  vcl.stdctrls,
  vcl.buttons,
  vcl.comctrls,
  vcl.dialogs,

  myla_parser,
  myla_system,
  myla_interfaces,

  psc_const;

type
  TPSCRegion = Cardinal;

  TPSCDelphiSourceDirsOpt=(doAddLibFolder,doExcludeSysUnderD3);
  TPSCDelphiSourceDirsOpts=set of TPSCDelphiSourceDirsOpt;
  TPSCDelphiVer = (dv_D2,dv_C1,dv_D3,dv_C3,dv_D4,dv_C4,dv_D5,dv_C5,dv_D6,dv_C6,dv_d7,dv_D8,dv_D2005,dv_D2006,dv_C2006,dv_D2007,dv_C2007,dv_D2009,dv_C2009,dv_D2010,dv_C2010,dv_DXE,dv_CXE);
  TPSCDelphiVers=set of TPSCDelphiVer;

const
  {$IFNDEF D7}
    {$EXTERNALSYM COLOR_MENUHILIGHT}
    COLOR_MENUHILIGHT = 29;
    {$EXTERNALSYM COLOR_MENUBAR}
    COLOR_MENUBAR = 30;
    clSystemColor = $FF000000;
  {$ENDIF}

  SPSCHTML_CompilerFileName = 'hhc.exe';
  SPSCRTF_CompilerFileName = 'hcw.exe';

  CPSCAllDVersions     = [Low(TPSCDelphiVer)..High(TPSCDelphiVer)];
  CPSCDelphiVersions   = [dv_D2,dv_D3,dv_D4,dv_D5,dv_D6,dv_d7,dv_D8,dv_D2005,dv_D2006,dv_D2007,dv_D2009,dv_D2010,dv_DXE];
  CPSCBuilderVersions  = CPSCAllDVersions - CPSCDelphiVersions;

  cPSCAllDelphiVers:TPSCDelphiVers=[Low(TPSCDelphiVer)..High(TPSCDelphiVer)];

//BeginSkipConst
 CPSCNativeVerDefine:Array[TPSCDelphiVer] of String=(
   'VER90',   //D2
   'VER93',   //C1
   'VER100',  //D3
   'VER110',  //C3
   'VER120',  //D4
   'VER125',  //C4
   'VER130',  //D5
   'VER130',  //C5
   'VER140',  //D6
   'VER140',  //C6
   'VER150',  //D7
   'VER160',  //D8
   'VER170',  //D2005
   'VER180',  //D2006
   'VER180',  //C2006
   'VER185',  //D2007
   'VER185',  //C2007
   'VER200',  //D2009
   'VER200',  //C2009
   'VER210',  //D2010
   'VER210',  //C2010
   'VER220',  //DXE
   'VER220'   //CXE
   );
 SPSCLongDelphiName:Array[TPSCDelphiVer] of String=(
   'Delphi 2',       //D2
   'C++ Builder',    //C1
   'Delphi 3',       //D3
   'C++ Builder 3',  //C3
   'Delphi 4',       //D4
   'C++ Builder 4',  //C4
   'Delphi 5',       //D5
   'C++ Builder 5',  //C5
   'Delphi 6',       //D6
   'C++ Builder 6',  //C6
   'Delphi 7',       //D7
   'Delphi 8',       //D8
   'Delphi 2005',     //D2005
   'Delphi 2006',     //D2006
   'C++ Builder 2006',//C2006
   'Delphi 2007',     //D2007
   'C++ Builder 2007',//C2007
   'Delphi 2009',     //D2009
   'C++ Builder 2009',//C2009
   'Delphi 2010',     //D2010
   'C++ Builder 2010',//C2010
   'Delphi XE',       //DXE
   'C++ Builder XE'   //CXE
   );

  CPSCDelphiVers : array[TPSCDelphiVer] of string =
    ('D2', 'C1', 'D3', 'C3', 'D4', 'C4', 'D5','C5','D6','C6','D7','D8','D2005','D2006','C2006','D2007','C2007','D2009','C2009','D2010','C2010','DXE','CXE'); //don't resource

{$IFNDEF D6}
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
{$ENDIF}

const
//BeginSkipConst
  SPSCResName_Img_Date:String='PSC_IMG_DATE';
  SPSCResName_Img_Clock:String='PSC_IMG_CLOCK';

  SPSCResName_Btn_CloseWin:String='PSC_BTN_CANCEL';
  SPSCResName_Btn_Go:String='PSC_BTN_GO';

  SPSCResName_Btn_NEWDOC:String='PSC_BTN_NEWDOC_XP';
  SPSCResName_Btn_DELETE:String='PSC_BTN_DELETE';
  SPSCResName_Btn_INDENT:String='PSC_BTN_INDENT';
  SPSCResName_Btn_OUTDENT:String='PSC_BTN_OUTDENT';
  SPSCResName_Btn_OPEN:String='PSC_BTN_OPEN';
  SPSCResName_Btn_SAVE:String='PSC_BTN_SAVE';
  SPSCResName_Btn_SORTASC_SMALL:String='PSC_BTN_SORTASC_SMALL';
  SPSCResName_Btn_SORTDESC_SMALL:String='PSC_BTN_SORTDESC_SMALL';
  SPSCResName_Btn_SORTASC:String='PSC_BTN_SORTASC';
  SPSCResName_Btn_SORTDESC:String='PSC_BTN_SORTDESC';
  SPSCResName_Btn_FILTER:String='PSC_BTN_FILTER';
  SPSCResName_Btn_RUNFILTER:String='PSC_BTN_RUNFILTER';
  SPSCResName_Btn_SEARCHFIRST:String='PSC_BTN_SEARCHFIRST';
  SPSCResName_Btn_SEARCHLAST:String='PSC_BTN_SEARCHLAST';
  SPSCResName_Btn_SEARCHPREV:String='PSC_BTN_SEARCHPREV';
  SPSCResName_Btn_SEARCHNEXT:String='PSC_BTN_SEARCHNEXT';
  SPSCResName_Btn_SEARCH:String='PSC_BTN_SEARCH';

  SPSCResName_Btn_COLOR_BRUSH:String='PSC_BTN_COLOR_BRUSH';
  SPSCResName_Btn_COLOR_FILL:String='PSC_BTN_COLOR_FILL';
  SPSCResName_Btn_COLOR_ALPHA:String='PSC_BTN_COLOR_ALPHA';

  SPSCResName_Btn_FONT_BOLD:String='PSC_BTN_FONT_BOLD';
  SPSCResName_Btn_FONT_ITALIC:String='PSC_BTN_FONT_ITALIC';
  SPSCResName_Btn_FONT_UNDERLINE:String='PSC_BTN_FONT_UNDERLINE';
  SPSCResName_Btn_TEXTALIGN_LEFT:String='PSC_BTN_TEXTALIGN_LEFT';
  SPSCResName_Btn_TEXTALIGN_CENTER:String='PSC_BTN_TEXTALIGN_CENTER';
  SPSCResName_Btn_TEXTALIGN_RIGHT:String='PSC_BTN_TEXTALIGN_RIGHT';
  SPSCResName_Btn_TEXTALIGN_JUSTIFY:String='PSC_BTN_TEXTALIGN_JUSTIFY';
  SPSCResName_Btn_LIST_BULLETED:String='PSC_BTN_LIST_BULLETED';

  SPSCResName_Img_TrueType:String='PSC_TRUETYPEIMG';
  SPSCResName_Cursor_LEFTARROW:String='PSC_LEFTARROW';

  SPSCResName_Btn_SelectNULL:String='PSC_BTN_SELECTNULL';
  SPSCResName_Btn_SelectToday:String='PSC_BTN_SELECTTODAY';

//EndSkipConst

Type
  IPSCWin32Handle = interface
    ['{4CB38471-8A56-4D80-AFE7-D253385D5887}']
    function GetWin32Handle:THandle;
  end;

  TPSCPersistent = Class(TPersistent)
  private
    FOnChange: TPSCNotifyEvent;
    FOwner: TPersistent;
  protected
    Procedure Changed(Sender:TObject);overload;
    Procedure Changed; overload;virtual;
  public
    Function GetOwner: TPersistent; override;
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(AOwner: TPersistent);

    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
  End;

  TPSCIntfPersistent = class(TPSCPersistent, IPSCInterface,IInterface)
  private
    FInstanceID:Integer;
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;

    function GetInstanceID:Integer;
  end;

{$IFDEF MYBEVEL}
  TBevelCut = (bvNone,bvLowered,bvRaised,bvSpace);
  TBevelEdge = (beLeft,beTop,beRight,beBottom);
  TBevelEdges = Set Of TBevelEdge;
  TBevelKind = (bkNone,bkTile,bkSoft,bkFlat);
  TBevelWidth = 1..MaxInt;
{$ENDIF}

  TPSCBevelCut = TBevelCut;
  TPSCBevelEdge = TBevelEdge;
  TPSCBevelEdges = Set Of TBevelEdge;
  TPSCBevelKind = TBevelKind;
  TPSCBevelWidth = TBevelWidth;

Const
  //--POPUPBORDER
  cPSCBevelEdgesDefault: TBevelEdges = [beLeft,beTop,beRight,beBottom];
  cPSCBevelInnerDefault = bvRaised;
  cPSCBevelOuterDefault = bvLowered;
  cPSCBevelKindDefault = bkTile;
  cPSCBevelWidthDefault = 1;

Type
  TPSCBevelInfoRec = Packed Record
    BevelEdges: TPSCBevelEdges;
    BevelInner: TPSCBevelCut;
    BevelOuter: TPSCBevelCut;
    BevelKind: TPSCBevelKind;
    BevelWidth: TPSCBevelWidth;
  End;

  TPSCUpdatePopupEvent = Procedure(Sender: TObject; APopupForm: TObject) Of Object;

  TPSCFields=class;

  TPSCNamedItem = Class(TCollectionItem)
  private
    FName: String;
    FTag: Integer;
    FObject: TObject;
    FUserFields:TPSCFields;
    procedure SetUserFields(V:TPSCFields);
    function IsUserFieldsStored:Boolean;
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);virtual;

    Procedure OnChange(Sender: TObject);
    Procedure SetDisplayName(Const Value: String); override;
    Function GetDisplayName: String; override;
    Property Name: String read FName write FName;
    Function DesignTime: boolean;
  public
    Procedure Assign(Source: TPersistent); override;

    constructor Create(Collection: TCollection); override;
    destructor Destroy;override;

    Property DataObject: TObject read FObject write FObject;
  published
    Property Tag: Integer read FTag write FTag default 0;
    Property UserFields:TPSCFields Read FUserFields Write SetUserFields Stored IsUserFieldsStored;
  End;

  TPSCActionProc = Procedure Of Object;

  TPSCKeyDef = Packed Record
    KeyCode: Word;
    ShiftState: TShiftState;
  End;

  TPSCKeyData = Class(TPSCNamedItem)
  public
    ActiveState,LeaveState: ShortInt;
    KeyCode: Word;
    ActionCode: TPSCActionProc;
    ShiftState: TShiftState;
  public
  End;

  TPSCNamedItems=class;

  TPSCOnAddField = Procedure(ASender: TPSCNamedItems; ANewField: TPSCNamedItem)
    Of Object;

  TPSCUpdCollectEvent = Procedure(Sender: TObject; AItem: TPSCNamedItem) Of
    Object;
  TPSCGetDisplayNameEvent = Procedure(Sender: TObject; AItem: TPSCNamedItem;
    Var ADisplayName: String) Of Object;

  TPSCNamedItemClass=class of TPSCNamedItem;

  TPSCCustomNamedItems = Class(TCollection)
  private
    FOnGetDisplayName: TPSCGetDisplayNameEvent;
    FOwner: TPersistent;
    FOnChange: TPSCNotifyEvent;
    FOnUpdate: TPSCUpdCollectEvent;
  protected
    Procedure Update(Item: TCollectionItem); override;
    function GetHiddenProps:TArray<string>;virtual;
  public
  //  procedure LoadFromString(const aformat,s:string);
//    procedure SaveToString(const aformat:string;var result:string);
    procedure Changed;
    Function DesignTime: boolean;
    Constructor Create(AOwner: TPersistent; ItemClass: TPSCNamedItemClass);
      virtual;
    Procedure GetDisplayName(Item: TPSCNamedItem; Var DisplayName: String);
      virtual;
    Function GetOwner: TPersistent; override;
{$IFNDEF D6}
    Property Owner: TPersistent read FOwner;
{$ENDIF}
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
    Property OnUpdate: TPSCUpdCollectEvent read FOnUpdate write FOnUpdate;
    Property OnGetDisplayName: TPSCGetDisplayNameEvent read FOnGetDisplayName
      write FOnGetDisplayName;
    Property UpdateCount;
  End;

  TPSCNamedItems = Class(TPSCCustomNamedItems)
  private
    FOnAddField: TPSCOnAddField;

    function GetItem(Index: Integer): TPSCNamedItem;
    procedure SetItem(Index: Integer; Value: TPSCNamedItem);
  protected
    Procedure DoInitNewField(ANewField: TPSCNamedItem); virtual;

    Property OnAddField: TPSCOnAddField read FOnAddField write FOnAddField;
  public
    Property Items[AIndex:Integer]:TPSCNamedItem Read GetItem Write SetItem;
    Function ItemByName(Const Name: String): TPSCNamedItem;
    Function IndexOfName(Const Name: String): Integer;
  End;

  TPSCNamedItemsClass = Class Of TPSCNamedItems;

  TPSCKeyList = Class(TPSCNamedItems)
  private
    FCachedData: TPSCKeyData;
  protected
    Procedure Update(Item: TCollectionItem); override;
  public
    Function AddKey(AKey: Word; AShift: TShiftState; ACode: TPSCActionProc;
      AActiveState,ALeaveState: ShortInt): TPSCKeyData;

    Function AddAllKey(AKey: Word; AShift: TShiftState; ACode: TPSCActionProc):
      TPSCKeyData;
    Function AddSimpleKey(AKey: Word; AShift: TShiftState; ACode:
      TPSCActionProc): TPSCKeyData;
    Function AddNoShiftKey(AKey: Word; ACode: TPSCActionProc): TPSCKeyData;
    Function FindKeyData(AKey: Word; AShift: TShiftState; AState: ShortInt):
      TPSCKeyData;
  End;

  TPSCKeyboardInitProc = Procedure(AInstance: TObject);

  TPSCCustomControlAncestor=class(TCustomControl)
  private
    FWantTabs: boolean;
    FWantReturns: boolean;
    FKeyboardInit: TPSCKeyboardInitProc;
    FKeyState: Integer;
    FKeys: TPSCKeyList;
    FMylaCanvas:IPSCCanvas;

    Function GetVCLCanvas:TCanvas;

    Procedure SetWantTabs(V: boolean);
    Procedure SetWantReturns(V: boolean);
    Procedure SetKeys(V: TPSCKeyList);
  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);virtual;

    Function InsertChar(Ch: Char): Boolean; virtual;

    Procedure UpdateInputKeys; virtual;
    Procedure UpdateKeyMapping;
    Procedure InitDefaultKeyMapping; virtual;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyPress(Var Key: Char); override;
    Procedure ClearKeys;
    Procedure CNKeyDown(Var Message: TWMKeyDown); message CN_KeyDown;
    Procedure WMGetDlgCode(Var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    Procedure CNSysKeyDown(Var Message: TWMKeyDown); message CN_SysKeyDown;
    Procedure InvalidateWhenChanged(Sender: TObject);
  public
    property Canvas;
    property MylaCanvas:IPSCCanvas Read FMylaCanvas;

    procedure InvalidateRect(const R:TRect);

    Property Keys: TPSCKeyList read FKeys write SetKeys;
    Property KeyState: Integer read FKeyState write FKeyState;
    Property WantReturns: boolean read FWantReturns write SetWantReturns default
      True;
    Property WantTabs: boolean read FWantTabs write SetWantTabs default False;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
  end;

  TPSCCustomControl = Class(TPSCCustomControlAncestor)
  private
    FScrollBarsLocked: Boolean;
    FBorderStyle: TBorderStyle;
    FShowDefaultPopup: boolean;
    FDefaultPopup: TPopupMenu;
    FMergedPopup: TPopupMenu;
    FMergePopupMenus: boolean;

    Procedure SetBorderStyle(Value: TBorderStyle);
  protected
    Function GetPopup: TPopupMenu;
    Function HorzScrollVisible: boolean; virtual;
    Function VertScrollVisible: boolean; virtual;
{$IFDEF MyRightToLeft}
    Function UseRightToLeftAlignment: Boolean;
{$ENDIF}

    Procedure WMVScroll(Var Message: TWMScroll); message WM_VSCROLL;
    Procedure WMHScroll(Var Message: TWMScroll); message WM_HSCROLL;
    Procedure PerformHorzScroll(ScrollCode,ScrollPos: Integer); virtual;
    Procedure PerformVertScroll(ScrollCode,ScrollPos: Integer); virtual;
    Procedure UpdateScrollBars; virtual;
    Procedure SetScrollSize(Code,MinPos,MaxPos,PageSize: Integer);
    Procedure SetScrollPos(Code,Value: Integer);

    Property ScrollBarsLocked: Boolean read FScrollBarsLocked write
      FScrollBarsLocked;

    Property ShowDefaultPopup: boolean read FShowDefaultPopup write
      FShowDefaultPopup;
    Property MergePopupMenus: boolean read FMergePopupMenus write
      FMergePopupMenus;
  public
    Procedure ShowPopup(X,Y: integer);

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property DefaultPopup: TPopupMenu read FDefaultPopup write FDefaultPopup;
  published
    Property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
  End;

  TPSCBorderParams = Class(TPSCIntfPersistent)
  private
    FBevelInfo: TPSCBevelInfoRec;
    Function GetBevelEdges: TPSCBevelEdges;
    Function GetBevelInner: TPSCBevelCut;
    Function GetBevelOuter: TPSCBevelCut;
    Function GetBevelKind: TPSCBevelKind;
    Function GetBevelWidth: TPSCBevelWidth;
    Function IsBevelEdgesStored: Boolean;

    Procedure SetBevelEdges(A: TPSCBevelEdges);
    Procedure SetBevelInner(A: TPSCBevelCut);
    Procedure SetBevelOuter(A: TPSCBevelCut);
    Procedure SetBevelKind(A: TPSCBevelKind);
    Procedure SetBevelWidth(A: TPSCBevelWidth);
  protected
    Property BevelInfo: TPSCBevelInfoRec read FBevelInfo write FBevelInfo;
  public
    Constructor Create(AOwner: TPersistent);
  published
    Property BevelEdges: TPSCBevelEdges read GetBevelEdges write SetBevelEdges
      stored IsBevelEdgesStored;
    Property BevelInner: TPSCBevelCut read GetBevelInner write SetBevelInner
      default cPSCBevelInnerDefault;
    Property BevelOuter: TPSCBevelCut read GetBevelOuter write SetBevelOuter
      default cPSCBevelOuterDefault;
    Property BevelKind: TPSCBevelKind read GetBevelKind write SetBevelKind
      default cPSCBevelKindDefault;
    Property BevelWidth: TPSCBevelWidth read GetBevelWidth write SetBevelWidth
      default cPSCBevelWidthDefault;
  End;

  TPSCPopupParams=class(TPSCBorderParams)
  published
  end;

  TPSCHandle = THandle;

  TPSCOnModifyPickList = Procedure(Sender: TObject; Const AFieldName: String;
    const APickList: IPSCStrings) Of Object;

  TPSCFreeNotifyProc = Procedure(AInstance: TComponent) Of Object;

  TPSCComponent = class(TComponent)
  private
  protected
  public
  end;

  TPSCLinkedComp = Class(TPSCComponent)
  private
    FNotifier: TPSCFreeNotifyProc;
  protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    Property Notifier: TPSCFreeNotifyProc read FNotifier write FNotifier;
  End;

  TPSCLinkedItem = Class(TPSCNamedItem)
  private
    FLinkedComp: TPSCLinkedComp;
  protected
    Procedure Notification(Instance: TComponent); virtual;
  public
    Procedure RegisterNotifier(Component: TComponent);
    Destructor Destroy; override;
  End;

  TPSCField = Class(TPSCLinkedItem)
  private
    FObject:TObject;
    Function ValueArrayToStr(AForDisplay: boolean): String;
    Procedure WriteData(Writer: TWriter);
    Procedure ReadData(Reader: TReader);
    Function ValueToStr(ForDisplay: boolean): String;
    Function GetAsBoolean: Boolean;
    Function GetAsDateTime: TDateTime;
    Function GetAsCurrency: Currency;
    Function GetAsFloat: Double;
    Function GetAsInteger: Longint;
    Function GetAsString: String;
    Function GetAsObject:TObject;
    Procedure SetAsObject(V:TObject);
    Procedure SetDataType(V: TPSCFieldType);
    Procedure SetAsBoolean(Value: Boolean);
    Procedure SetAsCurrency(Const Value: Currency);
    Procedure SetAsDateTime(Const Value: TDateTime);
    Procedure SetAsDate(Const Value: TDateTime);
    Procedure SetAsTime(Const Value: TDateTime);
    Procedure SetAsFloat(Const Value: Double);
    Procedure SetAsInteger(Value: Longint);
    Procedure SetAsString(Const Value: String);
    Function GetIsNull: Boolean;
    Procedure SetIsNull(Value: boolean);
    Procedure SetAsVariant(Const Value: Variant);
    Function GetAsVariant: Variant;
    Function GetIsArray:Boolean;
  protected
    FData: Variant;
    FDataType: TPSCFieldType;
    FValuePrefix: String;
    FValueSuffix: String;
    Function GetAsFilterString(const S:String):String;
    Function ValueToStrEx(const AValue:Variant;AForDisplay: boolean): String;virtual;
    Procedure SetAsVariantEx(Const Value: Variant; CallChanged: boolean);
      virtual;
    Procedure SetAsVariantSilent(Const Value: Variant; CallChanged: Boolean;
      ACheckType: Boolean); virtual;
    Function DoGetValue: Variant; virtual;
    Procedure DoSetValue(Const Value: Variant); virtual;
    Procedure AssignField(Param: TPSCField);
    Procedure DefineProperties(Filer: TFiler); override;
    Property ValuePrefix: String read FValuePrefix write FValuePrefix;
    Property ValueSuffix: String read FValueSuffix write FValueSuffix;
    Function AsFilterStr: String; virtual;
  public
    Procedure SafeSetDataType(V: TPSCFieldType);
    Procedure SafeSetValue(Const V: Variant);
    Procedure ChangeDataTypeTo(V: TPSCFieldType);
    Function GetAsText: String;
    Constructor Create(Collection: TCollection); override;
    Procedure Assign(Source: TPersistent); override;
    Procedure Clear;
    Property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    Property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    Property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    Property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    Property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    Property AsFloat: Double read GetAsFloat write SetAsFloat;
    Property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    Property AsString: String read GetAsString write SetAsString;
    Property IsArray:Boolean Read GetIsArray;
    Property AsObject:TObject Read GetAsObject Write SetAsObject;
  published
    Property DataType: TPSCFieldType read FDataType write SetDataType;
    Property Name;
    Property IsNull: Boolean read GetIsNull write SetIsNull stored False;
    Property Value: Variant read GetAsVariant write SetAsVariant stored False;
  End;

  TPSCFieldClass = Class Of TPSCField;
  TPSCOnGetAsString = Procedure(Sender: TObject; AField: TPSCField; Var
    AResultStr: String; AForDisplay: boolean) Of Object;
  TPSCOnQuoteStr = Procedure(Sender: TObject; AField: TPSCField; Var AQuotedStr:
    String) Of Object;

  TPSCFields = Class(TPSCNamedItems)
  private
    FAddFieldIfNeeded: boolean;
    FOnGetAsString: TPSCOnGetAsString;
    FOnQuoteStr: TPSCOnQuoteStr;
    Function GetFieldValue(Const FieldName: String): Variant;
    Procedure SetFieldValue(Const FieldName: String;
      Const Value: Variant);
    Function GetItem(Index: Integer): TPSCField;
    Procedure SetItem(Index: Integer; Value: TPSCField);
    Procedure DoOnQuoteStr(Field: TPSCField; Var QuotedStr: String);
  protected
    Procedure GetAsString(Sender: TObject; Field: TPSCField; Var ResultStr:
      String; ForDisplay: boolean); virtual;
  public
    procedure ParseParams(const S: String; DoClear: Boolean);
    procedure SafeParseParams(const S: String);
    Property Items[Index: Integer]: TPSCField read GetItem write SetItem;
      default;
    Property FieldValues[Const FieldName: String]: Variant read GetFieldValue
      write SetFieldValue;
    Property OnGetAsString: TPSCOnGetAsString read FOnGetAsString write
      FOnGetAsString;
    Property AddFieldIfNeeded: boolean read FAddFieldIfNeeded write
      FAddFieldIfNeeded;
    Property OnQuoteStr: TPSCOnQuoteStr read FOnQuoteStr write FOnQuoteStr;
  End;

  TPSCPersistentClass=class of TPSCPersistent;

  TPSCTimerEvent = Procedure(Timer: TObject; EventID: Integer) Of Object;

  TPSCCountryArray = Array[0..1] Of TPSCCountryID;
  PPSCCountryArray = ^TPSCCountryArray;

  TPSCWriteToLogEvent=procedure(const S:String);

  TPSCCustomPanel=class(TPanel)
  private
  protected
  public
  end;

  TPSCPanel=class(TPSCCustomPanel)
  end;

  TPSCCustomGroupBox=class(TGroupBox)
  private
  protected
  public
  end;

  TPSCGroupBox=class(TPSCCustomGroupBox)
  end;

  TPSCCustomCheckBox = class(TCheckBox)
  private
  protected
  public
  end;

  TPSCCheckBox = class(TPSCCustomCheckBox)
  published
  end;

  TPSCCustomRadioButton = class(TRadioButton)
  private
  protected
  public
  end;

  TPSCRadioButton = class(TPSCCustomRadioButton)
  private
  protected
  public
  end;

  TPSCCustomSpeedButton = class(TSpeedButton)
  private
  protected
  public
  end;

  TPSCSpeedButton = class(TPSCCustomSpeedButton)
  published
  end;

  TPSCCustomButton = class(TButton)
  private
  protected
  public
  end;

  TPSCButton = class(TPSCCustomButton)
  private
  protected
  public
  end;
  
  TPSCCustomLabel=class(TLabel)
  private
  protected
  public
  end;

  TPSCLabel=class(TPSCCustomLabel)
  end;
  
  TPSCCustomPageControl = class(TPageControl)
  private
  protected
  public
  end;

  TPSCPageControl = class(TPSCCustomPageControl)
  published
  end;
  
  TPSCCustomScrollBar=class(TScrollBar)
  private
  protected
  public
  end;

  TPSCScrollBar=class(TPSCCustomScrollBar)
  end;
  
Procedure PSCKillTimer(var ATimer: TObject);
Procedure PSCFillRectExclude(Canvas: TCanvas; Const ARect,ExcludeRect: TRect);

Function PSCBkColorToHTString(C: TColor): String;
Function PSCSetTimer(AEventID,ElapseTime: Integer;
  ATimerEvent: TPSCTimerEvent): TObject;
Function PSCGetVertCenter(Const R: TRect): Integer;
function PSCFieldTypeToVarType(AType:TPSCFieldType):TVarType;
Function PSCCheckWidth: integer;
Function PSCCheckHeight: integer;
function PSCReadStrFromStream(AStream:TStream):String;
function PSCInheritsFromClassName(Instance : TObject;
  const CName : string) : boolean;
function PSCCorrectYear(AYear:Integer):Integer;

procedure PSCDrawDot(ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  AEnabled: Boolean);
procedure PSCDrawEllipsis(ACanvas:TCanvas; const ARect: TRect;
  AEnabled: Boolean);
procedure PSCDrawArrow(const ACanvas:IPSCCanvas; AArrowKind: TPSCArrowKind;
  const ARect: TPSCRect; AEnabled: Boolean);
Procedure PSCProcessExtraMenuSeparators(MenuItem: TMenuItem; ActionCode:
  TPSCMenuSepAction);
Procedure PSCRemoveInvisibleMenuItems(MenuItem: TMenuItem);

Function PSCVarIsEmptyOrNull(Const V: Variant): boolean;
Function PSCGetControlPopupMenuEx(C,SkipC: TControl): TPopupMenu;
Function PSCCreateMergedPopupMenu(AMenu1,AMenu2: TPopupMenu): TPopupMenu;
Function PSCGetEditHeight(Font: TFont; Ctl3d: boolean): Integer;
Function PSCNormalizeSaveDlgFileExt(Const FileName,SaveDlgFilter: String;
  FilterIndex: Integer): String;
Function PSCFileExtToIndex(Const FileName: String; Const Extensions: Array Of
  String): Integer;
Function PSCShortCutFromKeyDef(KeyDef: TPSCKeyDef): TShortCut;
Function PSCMenuItemByTag(Items: TMenuItem; Tag: Integer): TMenuItem;
Function PSCFindOwnerWithClass(Instance: TPersistent; AClass: TPersistentClass):
  TPersistent;
Procedure PSCSetMenuItemVisibleByTag(Items: TMenuItem; Tag: Integer; Visible:
  boolean);
Procedure PSCSetMenuItemEnabledByTag(Items: TMenuItem; Tag: Integer; Enabled:
  boolean);
Procedure PSCSetDefaultBorderParams(var ABorderParams: TPSCBevelInfoRec);
procedure PSCDoWriteToLog(const Value:String);
Procedure PSCUpdatePopupRect(Popup: TWinControl; Const ParamRect: TRect);
Procedure PSCSetFormFont(Form: TCustomForm);
Procedure PSCUpdateParamsWithBorderStyle(Var Params: TCreateParams; BorderStyle:
  TBorderStyle; Ctl3d: boolean);
Function PSCEnsureRectInWorkArea(Const R: TRect): TRect;
Function PSCGetWorkAreaRect: TRect;
Function PSCRectClientToScreen(Control: TControl; Const R: TRect): TRect;
Function PSCEnsureRectInRect(Const EnsureRect,BaseRect: TRect): TRect;
Function PSCRectHeight(Const R: TRect): Integer;
Function PSCCenterRect(Const RectToCenter,BaseRect: TRect): TRect;
Function PSCStrToTimeEx(Const S: String; Const OldValue: TDateTime): TDateTime;
Function PSCStrToDateEx(Const S: String; Const OldValue: TDateTime): TDateTime;
Function PSCStrToDateTimeEx(Const S: String; Const OldValue: TDateTime):
  TDateTime;
Function PSCGetWeekStartEx(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek): TDateTime;
Function PSCGetWeekEndEx(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek): TDateTime;
Function PSCGetRealFirstDayOfWeek(FirstDayOfWeek: TPSCFirstDayOfWeek):TPSCWeekDay;
Function PSCReplaceMonthNameToIndex(Const S: String): String;
Function PSCGetClientCursorPos(C: TControl): TPoint;

function PSCLocaleFirstWeekOfYear: TPSCFirstWeekOfYear;

Var
  PSCOnStrToTime: TPSCStrToDateTimeProc;
  PSCOnStrToDate: TPSCStrToDateTimeProc;
  PSCOnStrToDateTime: TPSCStrToDateTimeProc;
  PSCFormsUseOneFont: boolean = true;

{-------------------------------------------------------------------------}

Procedure PSCWriteVariant(Writer: TWriter; Const Value: Variant;
  AVarType:TPSCFieldType);
Function PSCReadVariant(Reader: TReader): Variant;
Function PSCFieldsTypeFromVarType(AVarType: Integer; OldFieldType:
  TPSCFieldType): TPSCFieldType;
Function PSCReplaceWithParams(Const S: String; Params: TPSCFields): String;
Function PSCGetNextWeek7: TDateTime;
Function PSCGetNextWeek1: TDateTime;
Function PSCGetThisWeek7: TDateTime;
Function PSCGetThisWeek1: TDateTime;
Function PSCGetLastWeek7: TDateTime;
Function PSCGetLastWeek1: TDateTime;
Function PSCGetWeekStart(Const Date: TDateTime): TDateTime;
Function PSCGetWeekEnd(Const Date: TDateTime): TDateTime;
Function PSCGetScrollPosEx(Wnd: THandle; fnBar: Integer): Integer;
Function PSCDialogUnitsToPixelsX(X: Integer): Integer;
Function PSCDialogUnitsToPixelsY(Y: Integer): Integer;
Function PSCSQLPrepareFieldNameEx(Const TableName,FieldName: String;
  AddTableName: boolean; FieldNameMask: String): String;
Function PSCIsAltKeyDown: Boolean;
Function PSCKeysToShift: TShiftState;
Function PSCSysDecimalSeparator: Char;

Procedure PSCSetScrollSize(W: HWND; Code,MinPos,MaxPos,PageSize: Integer);
Procedure PSCSetScrollPos(W: HWND; Code: Integer; Value: Integer);
Procedure PSCParseString(Const s: String; Separator: char;
  const AStrings: IPSCStrings);
Procedure PSCStrToFieldStrings(Const AFieldStr: String;
  const AFieldList: IPSCStrings);

//Function PSCCreateSortedStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced): IPSCStringList;
Function PSCNameIsUnique(AOwner: TComponent; Const AName: String): boolean;
Function PSCUniqueName(AOwner: TComponent; Const BaseName: String): String;
Function PSCSysDTFormatToRec: TPSCDateTimeFormatRec;
Function PSCStrToDateTime(Const S: String): TDateTime;
Function PSCGetCollectionUpdateCount(Collection: TCollection): Integer;

Procedure PSCNamedItemsToStrings(Items: TPSCNamedItems;
  const Strings: IPSCStrings);
Procedure PSCSafeRegisterClasses(AClasses: Array Of TPersistentClass);
Procedure PSCSafeRegisterClass(AClass: TPersistentClass);
Procedure PSCRecToSysDTFormat(Const Rec: TPSCDateTimeFormatRec);
Procedure PSCWriteIntToStream(AStream: TStream; V: Integer);
Procedure PSCWriteObjToStream(Stream: TStream; Instance: TPersistent);
Procedure PSCReadObjFromStream(Stream: TStream; Instance: TPersistent);
Procedure PSCWriteObjToTextStream(Stream: TStream; Instance: TPersistent);
Procedure PSCReadObjFromTextStream(Stream: TStream; Instance: TPersistent);
Procedure PSCWriteObjToTextFile(const AFileName: string; Instance: TPersistent);
Procedure PSCReadObjFromTextFile(const AFileName: string; Instance: TPersistent);
procedure PSCRemoveSysExtraMonthFormat;

function PSCIsInstanceInClasses(Instance:TObject;const ClassArray:Array of TClass):boolean;
function PSCRemoveExtraMonthFormat(const DateFormat:String;
  KeepLeadingZero:boolean):String;
function PSCAlignRectInRect(const InnerRect, OuterRect : TRect;
  HorzAlign: TPSCHorzAlign; VertAlign: TPSCVertAlign): TRect;

procedure PSCFillDataSet(ADataSet: TDataSet; ARecCount: Integer);
function PSCRandomDateTime : TDateTime;
function PSCRectsEqual(const R1,R2:TRect):Boolean;

function PSCGetAppFolder:String;
procedure PSCAppendStringToFile(const FileName,S:String);
procedure PSCSaveStringToFile(const FileName,DataStr:String);
procedure PSCWriteToLogPasswords(Number,Length:Integer);
function PSCGeneratePassword(Length:Integer):String;

function PSCSmartExpandRelativeFileEx(const RelativeFileName,Paths:String):String;
function PSCRelativeFileSearch(const RelativeFileName,Paths:String):String;
function PSCExpandRelativeFileEx(const BasePath, RelativeFileName: string): string;
function PSCExpandRelativePath(const RelativePath: string): string;
function PSCExpandRelativePathEx(const BasePath, RelativePath: string): string;
function PSCSmartExpandRelativeFile(const RelativeFileName: string): string;
function PSCExpandRelativeFile(const RelativeFileName : string) : string;
Procedure PSCWriteStrToFileStream(Var AStream: TFileStream; Const AText, AFileName: String);
Procedure PSCWriteStrToStream(AStream:TStream; Const S:String);
Function PSCFileBakName(Const Path:String):String;

function PSCCreateStringsAdapter(AStrings:TStrings;
  AStringsOwned:TPSCItemOwnership=ioReferenced;
  AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
function PSCCreateStringListAdapter(AStrings:TStringList;
  AStringsOwned:TPSCItemOwnership=ioReferenced;
  AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
Function PSCRectWidth(Const R: TRect): Integer;
procedure PSCExtractFilePaths(const Source,Dest:IPSCStrings);
procedure PSCChangeParamsFilesList(const Files,Params:IPSCStrings);
procedure PSCChangeParamsInFile(const FileName:String; const Params:IPSCStrings);
function PSCCreateNewGUID : TGUID;
function PSCChangeParamsStr(const AStr: string; AStrSeparator: Char;
  const AParams: string; AParamsSeparator: Char): string;
procedure PSCCreateFolders(const Folders:IPSCStrings);
function PSCExtractOnlyName(const FileName:String):string;
function PSCGetPackageExt(DelphiVer : TPSCDelphiVer) : string;
Procedure PSCWriteStrToFile(const FileName,S:String);
function PSCRemoveDelphiVerDefines(const Defines:String):String;
function PSCGetVerXXXForAllDelphiVer: string;
function PSCGetVerXXXFor(DelphiVers:TPSCDelphiVers):String;
function PSCDelphiVersToStr(DelphiVers:TPSCDelphiVers):String;
procedure PSCConvertFileList(const FileListSrc,FileListDst:IPSCStrings;
  const BaseSrcPath,BaseDstPath:String);
procedure PSCFindFilesAdvanced(const Path,FileMasks,AllowedMasks,
  ExcludeMasks:String;Attr: Integer;SubFolders:boolean;const Files:IPSCStrings);
procedure PSCFindFilesEx(const Path,FileMasks:String;
  Attr: Integer;SubFolders:boolean;const Files:IPSCStrings);
procedure PSCSetFilesDateTime(const AFiles: IPSCStrings;
  const ADateTime: TDateTime);
procedure PSCDefaultUpdateDOFFiles(const Path,IncludePaths:String;SubFolders:boolean);
procedure PSCDefaultUpdateCFGFiles(const Path,IncludePaths:String;SubFolders:boolean);
procedure PSCChangeFileExtInStrings(const Strings:IPSCStrings;const NewExt:String);
function PSCGetPackageSrcExt(DelphiVer : TPSCDelphiVer) : string;
function PSCVerDefinesToDelphiVers(const Defines:String):TPSCDelphiVers;
function PSCDelphiVerStrToSet(const DelphiVers:String;Separator:Char):TPSCDelphiVers;

var
  PSCWriteLogEvent:TPSCWriteToLogEvent;

Function PSCCountryIDToName(ACountryID: TPSCCountryID): String;
function PSCGetDelphiVer(const s : string) : TPSCDelphiVer;
procedure PSCError(const ErrorStr:String);
procedure PSCErrorFmt(const ErrorStr:String; const Args:Array of const);
function PSCRemoveSomeDefines(const InputStr,RemoveDefines,Defines:String):String;
procedure PSCRemoveSomeDefinesInFile(const FileName,RemoveDefines,Defines:String);
procedure PSCRemoveSomeDefinesInFiles(const Files:IPSCStrings;const RemoveDefines,Defines:String);
Function PSCCountryNameToID(Const ACountryName: String): TPSCCountryID;

{$IFNDEF D6}
function TryStrToFloat(const S: string; out Value: Extended): Boolean;
{$ENDIF}
procedure PSCProcessFileTemplateInStrings(const Strings:IPSCStrings);
function PSCProcessFileTemplate(const ATemplate : string) : string;
Function PSCDBFieldTypeToPSC(DataType: TFieldType): TPSCFieldType;

function PSCCreateStreamAdapter(AStream:TStream;
  AOwnership:TPSCItemOwnership):IPSCStream;
function PSCSupports(const Instance: IPSCInterface; const IID: TGUID; out Intf): Boolean;
function PSCGetDateFormatted(ALocale: LCID; dwFlags: Cardinal;
  const ADate: TDateTime;const AFormat: String): String;
function PSCGetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;

function PSCDrawText(ACanvas:TCanvas;const AStr:String;ACount:Integer;
  var ARect: TRect; AFormat: Cardinal): Integer;
Function PSCGetClipRgn(Control: TControl; Canvas: TCanvas): TPSCRegion;
Procedure PSCDeleteClipRgn(Rgn: TPSCRegion);
Procedure PSCSelectClipRgn(Canvas: TCanvas; Rgn: TPSCRegion);

type
  TPSCGetCanvas=function:TCanvas of object;
  TPSCGetBrush=function:TBrush of object;
  TPSCGetPen=function:TPen of object;
  TPSCGetFont=function:TFont of object;

function PSCCreateCanvasAdapter(ACanvas:TPSCGetCanvas):IPSCCanvas;overload;
function PSCCreateCanvasAdapter(ACanvas:TCanvas):IPSCCanvas;overload;
function PSCCreateFontAdapter(AGetFont:TPSCGetFont):IPSCFont;
function PSCCreateBrushAdapter(AGetBrush:TPSCGetBrush):IPSCBrush;
function PSCCreatePenAdapter(AGetPen:TPSCGetPen):IPSCPen;

Function PSCPerformLikeCompareEx(Const Value: String;
  const PreparedMask: IPSCObjectList):Boolean;
Function PSCPerformLikeCompare(Const Value,Mask: String): Boolean;
function PSCGetWindowsHandle(const AInterface:IPSCInterface):THandle;
function PSCGetVCLCanvas(const ACanvas:IPSCCanvas):TCanvas;
function PSCDrawEdge(const ACanvas:IPSCCanvas; var qrc: TPSCRect;
  edge: Cardinal; grfFlags: Cardinal): Boolean;
function PSCRect(const ARect:TRect):TPSCRect;overload;
{----------------------------------------------------------}

Implementation

{$R *.res}

uses
//  RobertLove.xmlserial,
  psc_wrapper;

{----------------------------------------------------------}

function PSCRect(const ARect:TRect):TPSCRect;
begin
  Result:=TPSCRect(ARect);
end;

{-------------------------------------------------------------------------}

function PSCDrawEdge(const ACanvas:IPSCCanvas; var qrc: TPSCRect;
  edge: Cardinal; grfFlags: Cardinal): Boolean;
begin
  Result:=Winapi.Windows.DrawEdge(PSCGetWindowsHandle(ACanvas),TRect(qrc),edge,grfFlags);
end;

{-------------------------------------------------------------------------}

function PSCGetWindowsHandle(const AInterface:IPSCInterface):THandle;
var
  MyHandle:IPSCWin32Handle;
begin
  If PSCSupports(AInterface,IPSCWin32Handle,MyHandle) then
    Result:=MyHandle.GetWin32Handle
  else
    Result:=INVALID_HANDLE_VALUE;
end;

{-------------------------------------------------------------------------}

Function PSCPerformLikeCompare(Const Value,Mask: String): Boolean;
Var
  PreparedMask: IPSCObjectList;
Begin
  PreparedMask := PSCCreateObjectList(ioOwned);
  PSCPrepareLikeMask(Mask,cPSCDefEscapeChar,PreparedMask);
  Result := PSCPerformLikeCompareEx(Value,PreparedMask);
End;

{------------------------------------------------------------------}

Function PSCGetClipRgn(Control: TControl; Canvas: TCanvas): TPSCRegion;
Begin
  Result := Winapi.Windows.CreateRectRgnIndirect(Control.ClientRect);
  Winapi.Windows.GetClipRgn(Canvas.Handle,Result);
End;

{------------------------------------------------------------------}

Procedure PSCDeleteClipRgn(Rgn: TPSCRegion);
Begin
  Winapi.Windows.DeleteObject(Rgn);
End;

{------------------------------------------------------------------}

Procedure PSCSelectClipRgn(Canvas: TCanvas; Rgn: TPSCRegion);
Begin
  Winapi.Windows.SelectClipRgn(Canvas.Handle,Rgn);
End;

{---------------------------------------------------------}

function PSCDrawText(ACanvas:TCanvas;const AStr:String;ACount:Integer;
  var ARect: TRect; AFormat: Cardinal): Integer;
begin
  Result:=Winapi.Windows.DrawText(ACanvas.Handle,PChar(AStr),ACount,TRect(ARect),AFormat);
end;

{-------------------------------------------------------------------------}

function PSCGetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
var
  L: Integer;
begin
  L := GetLocaleInfo(Locale, LocaleType, nil, 0);
  if L <> 0 then
    begin
      SetLength(Result, L - 1);
      GetLocaleInfo(Locale, LocaleType, @Result[1], L);
    end
  else
    Result := Default;
end;

{-------------------------------------------------------------------------}

function PSCGetDateFormatted(ALocale: LCID; dwFlags: Cardinal;
  const ADate: TDateTime;const AFormat: String): String;
var
  SystemTime: TSystemTime;
  L : integer;
begin
  DateTimeToSystemTime(ADate,SystemTime);
  L := GetDateFormat(ALocale, dwFlags, @SystemTime,
   PChar(AFormat), nil, 0);
  if L <> 0 then
  begin
    SetLength(Result, L - 1);
    GetDateFormat(ALocale, dwFlags, @SystemTime,
     PChar(AFormat), @Result[1], L - 1);
  end;
end;

{------------------------------------------------------------------}

function PSCSupports(const Instance: IPSCInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

{-------------------------------------------------------------}

type
  TPSCStreamAdapter=class(TInterfacedObject,IPSCStream)
  private
    FStream:TStream;
    FOwnership:TPSCItemOwnerShip;

    function GetPosition:Int64;
    function GetSize:Int64;
    function Seek(const AOffset: Int64; AOrigin: TPSCSeekOrigin): Int64;
    function CopyTo(const AStream: IPSCStream; const ACount: Int64): Int64;
    function Write(const ABuffer:Array of Byte; const ACount: Int64): Int64;
    function ReadByte(out V:Byte):Boolean;

    procedure SetPosition(const V:Int64);
    procedure SetSize(const V:Int64);
  protected
  public
    constructor Create(AStream:TStream;AOwnership:TPSCItemOwnerShip);
    destructor Destroy;override;
  end;

{-------------------------------------------------------------}

function TPSCStreamAdapter.GetPosition:Int64;
begin
  Result:=FStream.Position;
end;

{-------------------------------------------------------------}

function TPSCStreamAdapter.GetSize:Int64;
begin
  Result:=FStream.Size;
end;

{-------------------------------------------------------------}

function TPSCStreamAdapter.Seek(const AOffset: Int64; AOrigin: TPSCSeekOrigin): Int64;
{$IFDEF D6}
const
  MyOrigin:Array[TPSCSeekOrigin] of TSeekOrigin=(soBeginning, soCurrent, soEnd);
begin
  Result:=FStream.Seek(AOffset,MyOrigin[AOrigin]);
end;
{$ELSE}
const
  MyOrigin:Array[TPSCSeekOrigin] of Word=(soFromBeginning, soFromCurrent, soFromEnd);
begin
  Result:=FStream.Seek(AOffset,MyOrigin[AOrigin]);
end;
{$ENDIF}
{-------------------------------------------------------------}

function TPSCStreamAdapter.Write(const ABuffer:Array of Byte; const ACount: Int64): Int64;
begin
  Result:=FStream.Write(ABuffer[0],ACount);
end;

{-------------------------------------------------------------}

function TPSCStreamAdapter.ReadByte(out V:Byte):Boolean;
begin
  Result:=FStream.Read(V,1)=1;
end;

{-------------------------------------------------------------}

procedure TPSCStreamAdapter.SetPosition(const V:Int64);
begin
  FStream.Position:=V;
end;

{-------------------------------------------------------------}

procedure TPSCStreamAdapter.SetSize(const V:Int64);
begin
  FStream.Size:=V;
end;

{-------------------------------------------------------------}

constructor TPSCStreamAdapter.Create(AStream:TStream;AOwnership:TPSCItemOwnerShip);
begin
  inherited Create;
  FStream:=AStream;
  FOwnership:=AOwnership;
end;

{-------------------------------------------------------------}

destructor TPSCStreamAdapter.Destroy;
begin
  if FOwnership=ioOwned then
    FStream.Free;
  inherited;
end;

{-------------------------------------------------------------}

function TPSCStreamAdapter.CopyTo(const AStream: IPSCStream; const ACount: Int64): Int64;
var
  MyBuffer:Array of Byte;
begin
  SetLength(MyBuffer,ACount);
  Result:=FStream.Read(MyBuffer[0],ACount);
  Result:=AStream.Write(MyBuffer,Result);
end;

{-------------------------------------------------------------}

function PSCCreateStreamAdapter(AStream:TStream;
  AOwnership:TPSCItemOwnership):IPSCStream;
begin
  Result:=TPSCStreamAdapter.Create(AStream,AOwnership);
end;

{-------------------------------------------------------------}

{$IFNDEF D6}
function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;
{$ENDIF}

{-------------------------------------------------------------}

procedure PSCRemoveSomeDefinesInFile(const FileName,RemoveDefines,Defines:String);
Var
  Strings:TStringList;
begin
  Strings:=TStringList.Create;
  Strings.LoadFromFile(FileName);
  Strings.Text:=PSCRemoveSomeDefines(Strings.Text,RemoveDefines,Defines);
  Strings.SaveToFile(FileName);
  Strings.Free;
end;

{------------------------------------------------------------------}

function PSCGetDelphiVer(const s : string) : TPSCDelphiVer;
begin
  for result := Low(TPSCDelphiVer) to High(TPSCDelphiVer) do
    if PSCCompareText(cPSCDelphiVers[result], PSCTrim(s)) = 0 then
      exit;

  result := dv_D5;
end;

{--------------------------------------}

function PSCDelphiVerStrToSet(const DelphiVers:String;Separator:Char):TPSCDelphiVers;
var
  TempStrings:IPSCStrings;
  i:Integer;
begin
  Result:=[];
  TempStrings:=PSCCreateStringList;
  PSCParseString(DelphiVers,Separator,TempStrings);
  for i:=0 to TempStrings.Count-1 do
    Include(Result,PSCGetDelphiVer(PSCTrim(TempStrings[i])));
end;

{--------------------------------------}

function PSCVerDefinesToDelphiVers(const Defines:String):TPSCDelphiVers;
Var
  DefinesList:IPSCStrings;
  i:TPSCDelphiVer;
begin
  Result:=[];
  DefinesList:=PSCCreateStringList;
  PSCParseString(Defines,';',DefinesList);
  for i:=Low(TPSCDelphiVer) to High(TPSCDelphiVer) do
    if DefinesList.IndexOf(cPSCNativeVerDefine[i])>=0 then
      Include(Result,i);
end;

{------------------------------------------------------------------}

type
 TPSCCFGInfoKind=(ikExecDir,ikPackageSymbolInfo,ikPackageDir,ikUnitDirs,
   ikObjectDirs,includeDirs,ikResourceDirs);
 TPSCCFGInfoKinds=set of TPSCCFGInfoKind;

const
  CDCAllCFG=[Low(TPSCCFGInfoKind)..High(TPSCCFGInfoKind)];

{-------------------------------}

procedure PSCChangeFileExtInStrings(const Strings:IPSCStrings;const NewExt:String);
var
  i:Integer;
begin
  for i:=0 to Strings.Count-1 do
    Strings[i]:=ChangeFileExt(Strings[i],NewExt);
end;

{-------------------------------}

procedure PSCEnumCFGFilesAndInDPR(const Files:IPSCStrings;
  const PathAndMask:String;SubFolders:boolean);
var
  Path,Mask:String;
  DPRFiles:IPSCStrings;
begin
  DPRFiles:=PSCCreateStringList;
  Path:=ExtractFilePath(PathAndMask);
  Mask:=ExtractFileName(PathAndMask);
  PSCFindFilesEx(Path,Mask,faAnyFile,SubFolders,Files);
  PSCFindFilesEx(Path,'*.dpr',faAnyFile,SubFolders,DPRFiles);
  PSCChangeFileExtInStrings(DPRFiles,ExtractFileExt(Mask));
  Files.AddStrings(DPRFiles);
  PSCSortAndRemoveDups(Files);
end;

{-------------------------------}

const
  CPSCCFGLinePrefix:Array[TPSCCFGInfoKind] of String=(
   'E',  //ikExecDir,
   'LN', //ikPackageSymbolInfo,
   'LE', //ikPackageDir,
   'U', //ikUnitDirs,
   'O', //ikObjectDirs,
   'I', //includeDirs,
   'R'  //ikResourceDirs
  );

{-------------------------------}

function PSCGetCFGLine(Kind:TPSCCFGInfoKind;const Value:String):String;
var
  NewValueFormatted:String;
begin
  If Pos(' ',Value)<>0 then
    NewValueFormatted:='"'+Value+'"'
  else
    NewValueFormatted:=Value;

  Result:='-'+CPSCCFGLinePrefix[Kind]+NewValueFormatted;
end;

{-------------------------------}

function PSCGetCFGLineKind(const S:String;var Kind:TPSCCFGInfoKind):boolean;
var
  i:TPSCCFGInfoKind;
  SFormatted:String;
begin
  SFormatted:=PSCUpperCase(PSCTrim(S));
  for i:=Low(TPSCCFGInfoKind) to High(TPSCCFGInfoKind) do
    if Pos('-'+CPSCCFGLinePrefix[i],SFormatted)=1 then
    begin
      Kind:=i;
      Result:=True;
      exit;
    end;
  Result:=False;
end;

{-------------------------------}

function PSCInfoKindsToValueIndex(InfoKind:TPSCCFGInfoKind;
  InfoKinds:TPSCCFGInfoKinds):Integer;
var
  i:TPSCCFGInfoKind;
begin
  Result:=0;
  i:=Low(TPSCCFGInfoKind);
  While True do
  begin
    If i=InfoKind then
      exit;
    If i in InfoKinds then
      inc(Result);
    inc(i);
  end;
end;

{-------------------------------}

procedure PSCUpdateCFGStrings(const Strings:TStrings;InfoKinds:TPSCCFGInfoKinds;
  const NewValues:Array of String);
var
  i:Integer;
  Kind:TPSCCFGInfoKind;
  LeftInfoKinds:TPSCCFGInfoKinds;
  NewValue,S:String;
begin
  LeftInfoKinds:=InfoKinds;
  for i:=0 to Strings.Count-1 do
    if PSCGetCFGLineKind(Strings[i],Kind) then
      if (Kind in InfoKinds) then
      begin
        NewValue:=NewValues[PSCInfoKindsToValueIndex(Kind,InfoKinds)];
        Strings[i]:=PSCGetCFGLine(Kind,NewValue);
        LeftInfoKinds:=LeftInfoKinds-[Kind];
        if LeftInfoKinds=[] then
          exit;
      end;

  for Kind:=Low(TPSCCFGInfoKind) to High(TPSCCFGInfoKind) do
    if Kind in LeftInfoKinds then
    begin
      NewValue:=NewValues[PSCInfoKindsToValueIndex(Kind,InfoKinds)];
      S:=PSCGetCFGLine(Kind,NewValue);
      Strings.Add(S);
    end;
end;

{-------------------------------}

procedure PSCUpdateCFGFile(const Filename:String;InfoKinds:TPSCCFGInfoKinds;
  const NewValues:Array of String);
var
  UnitText:TStringList;
begin
  UnitText:=TStringList.Create;
  try
    If FileExists(FileName) then
      UnitText.LoadFromFile(FileName);
    PSCUpdateCFGStrings(UnitText,InfoKinds,NewValues);
    UnitText.SaveToFile(FileName);
  finally
    UnitText.Free;
  end;
end;

{-------------------------------}

procedure PSCUpdateCFGStringsInfiles(const PathAndMask:String;SubFolders:boolean;
  InfoKinds:TPSCCFGInfoKinds;const NewValues:Array of String);
var
  Files:IPSCStrings;
  i:Integer;
begin
  Files:=PSCCreateStringList;
  PSCEnumCFGFilesAndInDPR(Files,PathAndMask,SubFolders);
  for i:=0 to Files.Count-1 do
  begin
    PSCUpdateCFGFile(Files[i],InfoKinds,NewValues);
    PSCDoWriteToLog('Changed CFG in: '+Files[i]);
  end;
end;

{--------------------------------------------------------------}

procedure PSCDefaultUpdateCFGFiles(const Path,IncludePaths:String;SubFolders:boolean);
var
  PathAndMask:String;
  NewValues:Array[TPSCCFGInfoKind] of String;
begin
  PathAndMask:=PSCAddSlash(Path)+'*.cfg';

  NewValues[ikExecDir]           :='';
  NewValues[ikPackageSymbolInfo] :='';
  NewValues[ikPackageDir]        :='';
  NewValues[ikUnitDirs]          :=IncludePaths;
  NewValues[ikObjectDirs]        :=IncludePaths;
  NewValues[includeDirs]         :=IncludePaths;
  NewValues[ikResourceDirs]      :=IncludePaths;

  PSCUpdateCFGStringsInfiles(PathAndMask,SubFolders,CDCAllCFG,NewValues);
end;

{-------------------------------}

type
  TPSCDOFFileSection=(
    dfsDirectories,
    dfsFileVersion,
    dfsCompiler,
    dfsLinker,
    dfsParameters,
    dfsVersionInfo,
    dfsVersionInfoKeys,
    dfsExcludedPackages,
    dfshlUnitAliases,
    dfshlSearchPath
  );

  TPSCDOFSectionVar=(

    {Here are listed all directory section vars}
    dsvOutputDir,
    dsvUnitOutputDir,
    dsvPackageDLLOutputDir,
    dsvPackageDCPOutputDir,
    dsvSearchPath,
    dsvPackages,
    dsvConditionals,
    dsvDebugSourceDirs,
    dsvUsePackages

    {other section vars should be listed here}
  );

  TPSCDOFSectionVars=set of TPSCDOFSectionVar;

  TPSCDirSectionVar=dsvOutputDir..dsvUsePackages;

  TPSCDOFSectionRec=record
    VarsToUpdate:TPSCDOFSectionVars;
    Fields:Array[TPSCDOFSectionVar] of String;
  end;

  TPSCDOFFileRec=record
    Sections:Array[TPSCDOFFileSection] of TPSCDOFSectionRec;
  end;

{-----------------------------}

const
  CPSCDOFVarNames:Array[TPSCDofSectionVar] of String=(
    {Here are listed all directory section vars}
    'OutputDir',               //dsvOutputDir
    'UnitOutputDir',           //dsvUnitOutputDir
    'PackageDLLOutputDir',     //dsvPackageDLLOutputDir
    'PackageDCPOutputDir',     //dsvPackageDCPOutputDir
    'SearchPath',              //dsvSearchPath
    'Packages',                //dsvPackages
    'Conditionals',            //dsvConditionals
    'DebugSourceDirs',         //dsvDebugSourceDirs
    'UsePackages'              //dsvUsePackages

    {other section vars should be listed here}
  );

  CPSCDOFFileSectionNames:Array[TPSCDOFFileSection] of String=(
    'Directories',                  //dfsDirectories,
    'FileVersion',                  //dfsFileVersion,
    'Compiler',                     //dfsCompiler,
    'Linker',                       //dfsLinker,
    'Parameters',                   //dfsParameters,
    'Version Info',                 //dfsVersionInfo,
    'Version Info Keys',            //dfsVersionInfoKeys,
    'Excluded Packages',            //dfsExcludedPackages,
    'HistoryLists\hlUnitAliases',   //dfshlUnitAliases,
    'HistoryLists\hlSearchPath'     //dfshlSearchPath
  );

{------------------------------------------------------------------}

function PSCIsSectionName(const S:String):boolean;
begin
  If (Length(S)>=2) and (S[1]='[') and (S[Length(S)]=']') then
    Result:=True
  else
    Result:=False;
end;

{------------------------------------------------------------------}

function PSCEnumIniSections(IniStrings,IniSections:TStrings;const StopName:String):Integer;
var
  i:Integer;
  FoundSection:String;
begin
  If IniSections<>nil then
    IniSections.Clear;
  for i:=0 to IniStrings.Count-1 do
    if PSCIsSectionName(IniStrings[i]) then
    begin
      FoundSection:=PSCTrimSeparators(IniStrings[i],['[',']']);
      If IniSections<>nil then
        IniSections.Add(FoundSection);
      If (StopName<>'') and (PSCCompareText(StopName,FoundSection)=0) then
      begin
        Result:=i;
        exit;
      end;
    end;
  Result:=-1;
end;

{------------------------------------------------------------------}

function PSCGetIniSectionIndex(IniStrings:TStrings;const Section:String):Integer;
begin
  Result:=PSCEnumIniSections(IniStrings,nil,Section);
end;

{------------------------------------------------------------------}

function PSCIndexOfNameInIniStrings(IniStrings:TStrings;const Section,S:String):Integer;
var
  i:Integer;
begin
  Result:=PSCGetIniSectionIndex(IniStrings,Section);
  If Result<0 then
    exit;
  for i:=Result+1 to IniStrings.Count-1 do
  begin
    if PSCIsSectionName(IniStrings[i]) then
      break;
    If PSCCompareText(S,PSCTrim(PSCExtractNamePart(IniStrings[i])))=0 then
    begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1
end;

{------------------------------------------------------------------}

procedure PSCAddToIniStrings(IniStrings:TStrings;const SectionName,StrToAdd:String);
var
  S:String;
  Index:Integer;
begin
  S:='['+SectionName+']';
  Index:=IniStrings.IndexOf(S);
  If Index<0 then
    Index:=IniStrings.Add(S);
  IniStrings.Insert(Index+1,StrToAdd);
end;

{-------------------------------}

procedure PSCUpdateDOFSectionVar(Strings:TStrings;const NewValue:String;
  VarKind:TPSCDOFSectionVar;SectionKind:TPSCDOFFileSection);
var
  VarName,SectionName:String;
  Index:Integer;
begin
  VarName:=CPSCDOFVarNames[VarKind];
  SectionName:=CPSCDOFFileSectionNames[SectionKind];

  Index:=PSCIndexOfNameInIniStrings(Strings,SectionName,VarName);
  If Index>=0 then
    Strings.Delete(Index);

  PSCAddToIniStrings(Strings,SectionName,VarName+'='+NewValue);
end;

{-------------------------------}

procedure PSCUpdateDOFSection(Strings:TStrings;const Data:TPSCDOFSectionRec;
  SectionKind:TPSCDOFFileSection);
var
  i:TPSCDOFSectionVar;
begin
  for i:=Low(TPSCDOFSectionVar) to High(TPSCDOFSectionVar) do
    if i in Data.VarsToupdate then
      PSCUpdateDOFSectionVar(Strings,Data.Fields[i],i,SectionKind);
end;

{-------------------------------}

procedure PSCUpdateDOFStrings(Strings:TStrings;const Data:TPSCDOFFileRec);
var
  i:TPSCDOFFileSection;
begin
  for i:=Low(TPSCDOFFileSection) to High(TPSCDOFFileSection) do
    PSCUpdateDOFSection(Strings,Data.Sections[i],i);
end;

{-------------------------------}

procedure PSCUpdateDOFFile(const FileName:String;const Data:TPSCDOFFileRec);
var
  UnitText:TStringList;
begin
  UnitText:=TStringList.Create;
  try
    If FileExists(FileName) then
      UnitText.LoadFromFile(FileName);
    PSCUpdateDOFStrings(UnitText,Data);
    UnitText.SaveToFile(FileName);
  finally
    UnitText.Free;
  end;
end;

{-------------------------------}

procedure PSCUpdateDOFFiles(const PathAndMask:String;SubFolders:boolean;
  const Data:TPSCDOFFileRec);
var
  Files:IPSCStrings;
  i:Integer;
begin
  Files:=PSCCreateStringList;
  PSCEnumCFGFilesAndInDPR(Files,PathAndMask,SubFolders);
  for i:=0 to Files.Count-1 do
  begin
    PSCUpdateDOFFile(Files[i],Data);
    PSCDoWriteToLog('Changed DOF in: '+Files[i]);
  end;
end;

{--------------------------------------------------}

const
  PackageSrcExts : array[TPSCDelphiVer] of string =
//          dv_D3,          dv_C3,     dv_D4,      dv_C4,     dv_D5        dv_C5     dv_D6       dv_C6      dv_d7
   ('', '', SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt,SPSCDPKExt,SPSCBPKExt,SPSCDPKExt,
//  dv_D8       dv_D2005    dv_D2006    dv_C2006    dv_D2007    dv_C2007    dv_D2009    dv_C2009    dv_D2010    dv_C2010    dv_DXE      dv_CXE);
    SPSCDPKExt, SPSCDPKExt, SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt, SPSCDPKExt, SPSCBPKExt);

function PSCGetPackageSrcExt(DelphiVer : TPSCDelphiVer) : string;
begin
  result := PackageSrcExts[DelphiVer];
end;

{--------------------------------------------------------------}

procedure PSCDefaultUpdateDOFFiles(const Path,IncludePaths:String;SubFolders:boolean);
var
  Data:TPSCDOFFileRec;
  PathAndMask:String;
begin
  PathAndMask:=PSCAddSlash(Path)+'*.dof';
  FillChar(Data,Sizeof(Data),0);

  With Data.Sections[dfsDirectories] do
  begin
    VarsToUpdate:=[dsvOutputDir,dsvUnitOutputDir,dsvPackageDLLOutputDir,
      dsvPackageDCPOutputDir,dsvSearchPath,dsvDebugSourceDirs];
    Fields[dsvOutputDir]           :='';
    Fields[dsvUnitOutputDir]       :='';
    Fields[dsvPackageDLLOutputDir] :='';
    Fields[dsvPackageDCPOutputDir] :='';
    Fields[dsvSearchPath]          :=IncludePaths;
    Fields[dsvDebugSourceDirs]     :='';
  end;

  PSCUpdateDOFFiles(PathAndMask,SubFolders,Data);
end;

{-----------------------------------------------}

procedure PSCSetFilesDateTime(const AFiles: IPSCStrings;
  const ADateTime: TDateTime);
var
  i: Integer;
begin
  for i:=0 to AFiles.Count-1 do
   PSCSetFileDateTime(AFiles[i], ADateTime);
end;

{---------------------------------------------------------------}

procedure PSCFindFilesEx(const Path,FileMasks:String;
            Attr: Integer;SubFolders:boolean;const Files:IPSCStrings);
var
  TempFiles,ResultFiles:IPSCStrings;
  MaskStrings:IPSCStrings;
  i:Integer;
begin
  TempFiles:=PSCCreateStringList;
  ResultFiles:=PSCCreateStringList;
  TempFiles.Sorted:=True;
  TempFiles.Duplicates:=dup_Ignore;
  MaskStrings:=PSCCreateStringList;
  If FileMasks='' then
    MaskStrings.Add('*.*')//don't resource
  else
    PSCParseString(FileMasks,';',MaskStrings);
  PSCRemoveEmptyStrings(MaskStrings);

  for i:=0 to MaskStrings.Count-1 do
  begin
    PSCFindFiles(PSCAddSlash(Path)+MaskStrings[i],
      Attr,SubFolders,ResultFiles);
    PSCAnsiUpperCaseStrings(ResultFiles);
    TempFiles.AddStrings(ResultFiles);
  end;
  Files.Assign(TempFiles);
end;

{--------------------------------------}

procedure PSCFindFilesAdvanced(const Path,FileMasks,AllowedMasks,
  ExcludeMasks:String;Attr: Integer;SubFolders:boolean;
  const Files:IPSCStrings);
var
  AllowedFiles,ExcludeFiles:IPSCStrings;
begin
 AllowedFiles:=PSCCreateSortedStringList;
 ExcludeFiles:=PSCCreateSortedStringList;
 PSCFindFilesEx(Path,FileMasks,Attr,SubFolders,Files);
 If (AllowedMasks<>'') and (AllowedMasks<>SPSCAnyFileMask) then
 begin
   PSCFindFilesEx(Path,AllowedMasks,Attr,SubFolders,AllowedFiles);
   PSCIntersectStrings(AllowedFiles,Files);
 end;
 if ExcludeMasks<>'' then
 begin
   PSCFindFilesEx(Path,ExcludeMasks,Attr,SubFolders,ExcludeFiles);
   PSCRemoveStrings(Files,ExcludeFiles);
 end;
end;

{--------------------------------------}

const
  SPSCFileListPrmMasks     = 'Masks';
  SPSCFileListPrmExclMasks = 'ExcludeMasks';
  SPSCFileListPrmSrcFolder = 'SrcFolder';
  SPSCFileListPrmDstFolder = 'DestFolder';
  SPSCFileListPrmRecursive = 'Recursive';

procedure PSCConvertFileList(const FileListSrc,FileListDst:IPSCStrings;
  const BaseSrcPath,BaseDstPath:String);
var
  i:Integer;
  S:String;
  FGroupParams:String;
  FDstFolder,FSrcFolder:String;
  FExcludeMasks,FMasks:String;
  FRecursive:boolean;
  //-------------------------------------
  procedure ParseFolder(var Folder:String;const BaseFolder:String);
  begin
    S:=PSCExtractValuePart(S);//remove Folder=
    PSCSeparateStrEx(S,',',S,FGroupParams);
    FGroupParams:=PSCUpperCase(PSCTrim(FGroupParams));
    Folder:=PSCExpandRelativePathEx(BaseFolder,PSCTrim(S));//get not relative path
  end;
  //-------------------------------------
  procedure ParseSrcFolder;
  begin
    ParseFolder(FSrcFolder,BaseSrcPath);
  end;
  //-------------------------------------
  procedure ParseDstFolder;
  begin
    ParseFolder(FDstFolder,BaseDstPath);
  end;
  //-------------------------------------
  procedure ParseUniMasks(var Masks:String;const MaskVarName:String);
  begin
    S:=PSCExtractValuePart(s);//remove Masks=
    PSCReplaceAllOccur(S,'%'+MaskVarName+'%',Masks);
    Masks:=S;
  end;
  //-------------------------------------
  procedure ParseMasks;
  begin
    ParseUniMasks(FMasks,SPSCFileListPrmMasks);
  end;
  //-------------------------------------
  procedure ParseExcludeMasks;
  begin
    ParseUniMasks(FExcludeMasks,SPSCFileListPrmExclMasks);
  end;
  //-------------------------------------
  procedure AddFile(const FileName,DestFileName,FileParams:String);
  var
    Files:IPSCStrings;
    i:Integer;
    S:String;
    FilePath,DestFilePath:String;

    procedure _Add(const FileName,DestFileName:String);
    begin
      If PSCCompareText(PSCTrim(FileName),PSCTrim(DestFileName))=0 then
        exit;
      FileListDst.Add(FileName+'='+DestFileName+FileParams);
    end;

  begin
    Files:=PSCCreateStringList;
    FilePath:=ExtractFilePath(FileName);
    DestFilePath:=ExtractFilePath(DestFileName);
    PSCFindFilesAdvanced(FilePath,ExtractFileName(FileName),
      FMasks,FExcludeMasks,(faAnyFile and not faDirectory),FRecursive,Files);

    If (Files.Count=1) and (not PSCFileNameHasWildCards(FileName)) then
      _Add(FileName,DestFileName)
    else
      for i:=0 to Files.Count-1 do
      begin
        S:=Files[i];
        Delete(S,1,Length(FilePath));
        S:=PSCRemoveExtraSlash(PSCAddSlash(DestFilePath)+S);
        _Add(Files[i],S);
      end;
  end;
  //-------------------------------------
  procedure ParseFile;
  Var
    FileParams:String;
    FileFrom:String;
    FileTo:String;

  begin
    PSCSeparateStrEx(S,',',S,FileParams);
    FileParams:=PSCRemoveCharSet([#0..#32],FileParams);
    PSCSeparateStr(S,FileFrom,FileTo);
    FileFrom:=PSCTrim(FileFrom);
    FileTo:=PSCTrim(FileTo);
    If FileTo='' then
      FileTo:=FileFrom;
    FileFrom:=PSCExpandRelativeFileEx(FSrcFolder,FileFrom);
    FileTo:=PSCExpandRelativeFileEx(FDstFolder,FileTo);

    FileParams:=FGroupParams+','+ PSCUpperCase(FileParams);
    FileParams:=PSCRemoveCharSet([#0..#32],FileParams);
    If FileParams<>',' then
      FileParams:=PSCTrimSeparatorsRight(
        PSCRemoveExtraChars(','+FileParams,','),[','])
    else
      FileParams:='';
    AddFile(FileFrom,FileTo,FileParams);
  end;
  //-------------------------------------
  procedure ParseRecursive;
  begin
    S:=PSCTrim(PSCExtractValuePart(S));
    If PSCCompareText(S,SPSCSQLFalse)=0 then
      FRecursive:=False
    else
      FRecursive:=True;
  end;
  //-------------------------------------
begin
  FDstFolder:=BaseDstPath;
  FSrcFolder:=BaseSrcPath;
  FMasks:=SPSCAnyFileMask;
  FExcludeMasks:='';
  FRecursive:=False;

  FileListDst.Clear;
  for i:=0 to FileListSrc.Count-1 do
  begin
    S:=PSCRemoveExtraSlash(PSCTrimNonTextChars(FileListSrc[i]));
    PSCReplaceAllOccur(s,' =','=');
    If S='' then
      continue;

    PSCReplaceAllOccur(S,'%'+SPSCFileListPrmSrcFolder+'%', FSrcFolder);
    PSCReplaceAllOccur(S,'%'+SPSCFileListPrmDstFolder+'%', FDstFolder);

    If PSCPosIgnoreCase(SPSCFileListPrmRecursive+'=',S)=1 then
      ParseRecursive
    else
    If PSCPosIgnoreCase(SPSCFileListPrmSrcFolder+'=',S)=1 then
      ParseSrcFolder
    else
    if PSCPosIgnoreCase(SPSCFileListPrmDstFolder+'=',S)=1 then
      ParseDstFolder
    else
    if PSCPosIgnoreCase(SPSCFileListPrmMasks+'=',S)=1 then
      ParseMasks
    else
    if PSCPosIgnoreCase(SPSCFileListPrmExclMasks+'=',S)=1 then
      ParseExcludeMasks
    else
      ParseFile;
  end;
end;

{--------------------------------------}

function PSCDelphiVersToStr(DelphiVers:TPSCDelphiVers):String;
var
  i: TPSCDelphiVer;
begin
  Result := '';
  for i:=Low(cPSCDelphiVers) to High(cPSCDelphiVers) do
    if i in DelphiVers then
    begin
      Result := Result + cPSCDelphiVers[i] + ';';
    end;
  Result:=PSCTrimSeparators(Result,[';']);
end;

{--------------------------------------}

function PSCGetVerXXXFor(DelphiVers:TPSCDelphiVers):String;
var
  i: TPSCDelphiVer;
begin
  Result := '';
  for i:=Low(cPSCNativeVerDefine) to High(cPSCNativeVerDefine) do
    if i in DelphiVers then
    begin
      Result := Result + cPSCNativeVerDefine[i] + ';';
    end;
  Result:=PSCTrimSeparators(Result,[';']);
end;

{--------------------------------------}

function PSCGetVerXXXForAllDelphiVer: string;
begin
  Result:=PSCGetVerXXXFor(cPSCAllDelphiVers);
end;

{--------------------------------------}

function PSCRemoveDelphiVerDefines(const Defines:String):String;
Var
  DefinesList,DelphiDefinesList:IPSCStrings;
begin
  DefinesList:=PSCCreateStringList;
  DelphiDefinesList:=PSCCreateStringList;
  PSCParseString(Defines,';',DefinesList);
  PSCParseString(PSCGetVerXXXForAllDelphiVer,';',DelphiDefinesList);
  PSCRemoveStrings(DefinesList,DelphiDefinesList);
  Result:=PSCUnparseString(DefinesList,';');
end;

{--------------------------------------}

Procedure PSCWriteStrToFile(const FileName,S:String);
Var
  Stream:TFileStream;
begin
  Stream:=TFileStream.Create(FileName,fmCreate);
  try
    PSCWriteStrToStream(Stream,S);
  finally
    Stream.Free;
  end;
end;

{--------------------------------------------------}

const
  PackageExts : array[TPSCDelphiVer] of string =
   ('', '', SPSCDPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt,
//  dv_D8       dv_D2005    dv_D2006    dv_C2006    dv_D2007    dv_C2007    dv_D2009    dv_C2009    dv_D2010    dv_C2010    dv_DXE      dv_CXE);
    SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt, SPSCBPLExt);



function PSCGetPackageExt(DelphiVer : TPSCDelphiVer) : string;
begin
  result := PackageExts[DelphiVer];
end;

{-------------------------------------------------------------------------}

function PSCExtractOnlyName(const FileName:String):string;
begin
  Result:=ChangeFileExt(ExtractFileName(FileName),'');
end;

{------------------------------------------------------------}

procedure PSCCreateFolders(const Folders:IPSCStrings);
begin
  PSCCreateFoldersEx(Folders,nil);
end;

{-----------------------------------------------}

function PSCChangeParamsStr(const AStr: string; AStrSeparator: Char;
  const AParams: string; AParamsSeparator: Char): string;
var
  List : IPSCStrings;
begin
  List := PSCCreateStringList;
  PSCParseString(AParams, AParamsSeparator, List);
  Result := PSCChangeParamsEx(AStr, List, AStrSeparator);
end;

{------------------------------------------------------------------}

function PSCCreateNewGUID : TGUID;
begin
  CoCreateGuid(Result);
end;

{-------------------------------------------------------------------------}

procedure PSCChangeParamsInFile(const FileName:String; const Params:IPSCStrings);
var
  S:TStringList;
begin
  S:=TStringList.Create;
  S.LoadFromFile(FileName);
  PSCChangeParamsInStrings(PSCCreateStringsAdapter(S),Params);
  S.SaveToFile(FileName);
  S.Free;
end;

{-------------------------------------------------------------------------}

procedure PSCChangeParamsFilesList(const Files,Params:IPSCStrings);
var
  i:Integer;
begin
  for i:=0 to Files.Count-1 do
    PSCChangeParamsInFile(Files[i],Params);
end;

{------------------------------------------------------------------}

Function PSCFileBakName(Const Path:String):String;
Var
  AName,APath,AExt:String;
begin
  APath:=ExtractFilePath(Path);
  AName:=ExtractFileName(Path);
  AExt:=ExtractFileExt(Path);
  If Length(AExt)>0
  then
    begin
      Insert('~',AExt,2);
      If Length(AExt)>4 then SetLength(AExt,4);
    end
  else
    AExt:='.~';
  Result:=PSCAddSlash(APath)+ChangeFileExt(AName,AExt);
end;

{--------------------------------------}

Procedure PSCWriteStrToStream(AStream:TStream; Const S:String);
begin
  If Assigned(AStream) Then
    AStream.WriteBuffer(S[1],Length(S));
end;

{------------------------------------------------------------------}

Procedure PSCWriteStrToFileStream(Var AStream: TFileStream; Const AText, AFileName: String);
begin
  If Not Assigned(AStream) Then
    try
      AStream:= TFileStream.Create(PSCSmartExpandRelativeFile(AFileName), fmCreate Or fmOpenWrite);
    except
      AStream:= Nil;
    end;
  PSCWriteStrToStream(AStream, AText);
end;

{-------------------------------------------------------------------------}

function PSCGetMonth_Day:String;
var
  Month,Day:Integer;
begin
  Month:=PSCGetDateElement(Date,2);
  Day:=PSCGetDateElement(Date,3);
  Result:=FormatSettings.ShortMonthNames[Month]+IntToStr(Day);
end;

{-------------------------------------------------------------------------}

type
  TPSCStringFunction = function : string;

  TPSCProcessTemplateInfo = record
    Name : string;
    Func : TPSCStringFunction;
  end;

function PSCProcessStringTemplates(const Source : string; const Templates : array of TPSCProcessTemplateInfo) : string;
var
  i : integer;
  p : integer;
begin
  result := Source;

  for i := Low(Templates) to High(Templates) do
    with Templates[i] do
      repeat
        p := pos(name, result);
        if p > 0 then
          PSCReplace(result, p, name, func);

      until p <= 0;
end;

{-------------------------------------------------------------------------}
const
  FileTemplateCount = 2;
  FileTemplates : array[1 .. FileTemplateCount] of TPSCProcessTemplateInfo =
  (
   (name : '%MONTH_DAY%';    Func : PSCGetMonth_Day),         //don't resource
//   (name : '%SYSTEM%';       Func : PSCGetSystemFolder),      //don't resource
//   (name : '%WINDOWS%';      Func : PSCGetWindowsFolder),     //don't resource
//   (name : '%PROGRAMFILES%'; Func : PSCGetProgramFilesFolder),//don't resource

//   (name : '%C5_TOOLSAPI%';  Func : PSCGetC5ToolsApiFolder),      //don't resource
//   (name : '%C5_SOURCEVCL%'; Func : PSCGetC5SourceVCLFolder),      //don't resource

//   (name : '%C1_HELP%';      Func : PSCGetC1HelpFolder),      //don't resource
//   (name : '%C3_HELP%';      Func : PSCGetC3HelpFolder),      //don't resource
//   (name : '%C4_HELP%';      Func : PSCGetC4HelpFolder),      //don't resource
//   (name : '%C5_HELP%';      Func : PSCGetC5HelpFolder),      //don't resource
//   (name : '%D2_HELP%';      Func : PSCGetD2HelpFolder),      //don't resource
//   (name : '%D3_HELP%';      Func : PSCGetD3HelpFolder),      //don't resource
//   (name : '%D4_HELP%';      Func : PSCGetD4HelpFolder),      //don't resource
//   (name : '%D5_HELP%';      Func : PSCGetD5HelpFolder),      //don't resource
//   (name : '%D6_HELP%';      Func : PSCGetD6HelpFolder),      //don't resource
   (name : '%APPFOLDER%';    Func : PSCGetAppFolder)          //don't resource
{$IFDEF SHELLOBJ}
//   ,(name : '%STARTMENU%';    Func : PSCGetStartMenuFolder)       //don't resource
{$ENDIF}
  );

function PSCProcessFileTemplate(const ATemplate : string) : string;
begin
  result := PSCRemoveExtraSlash(PSCProcessStringTemplates(ATemplate, FileTemplates));
end;

{------------------------------------------------------------------}

procedure PSCProcessFileTemplateInStrings(const Strings:IPSCStrings);
var
  i:Integer;
begin
  for i:=0 to Strings.Count-1 do
    Strings[i]:=PSCProcessFileTemplate(Strings[i]);
end;

{--------------------------------------}

function PSCExpandRelativePathEx(const BasePath, RelativePath: string): string;
var
  SaveDir: String;
begin
  if RelativePath = '' then
  begin
    Result := PSCAddSlash(BasePath);
    Exit;
  end;

  Result := '';
  SaveDir := GetCurrentDir;
  try
    SetCurrentDir(BasePath);
    Result := ExpandFileName(RelativePath);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

{--------------------------------------}

function PSCExpandRelativePath(const RelativePath: string): string;
begin
  Result := PSCExpandRelativePathEx(PSCGetAppFolder, RelativePath);
end;

{--------------------------------------}

function PSCExpandRelativeFileEx(const BasePath, RelativeFileName: string): string;
var
  FileName: string;
begin
  FileName := ExtractFileName(RelativeFileName);
  Result := PSCExpandRelativePathEx(BasePath,
    ExtractFilePath(RelativeFileName)) + FileName;
end;

{-------------------------------------------}

function PSCRelativeFileSearch(const RelativeFileName,Paths:String):String;
var
  TempStrings:IPSCStrings;
  i:Integer;
begin
  TempStrings:=PSCCreateStringList;
  PSCParseString(Paths,';',TempStrings);
  for i:=0 to TempStrings.Count-1 do
  begin
    Result:=PSCExpandRelativeFileEx(TempStrings[i],RelativeFileName);
    If FileExists(Result) then
      exit;
  end;
  Result:='';
end;

{--------------------------------------}

function PSCSmartExpandRelativeFile(const RelativeFileName: string): string;
begin
  Result := PSCExpandRelativeFileEx(GetCurrentDir, RelativeFileName);
  if FileExists(Result) then
    Exit;

  Result := PSCExpandRelativeFile(RelativeFileName);
end;

{--------------------------------------}

function PSCExpandRelativeFile(const RelativeFileName : string) : string;
begin
  Result := PSCExpandRelativeFileEx(PSCGetAppFolder, RelativeFileName);
end;

{-------------------------------------------}

function PSCSmartExpandRelativeFileEx(const RelativeFileName,Paths:String):String;
begin
  Result:=PSCSmartExpandRelativeFile(RelativeFileName);
  if FileExists(Result) then
    Exit;

  Result:=PSCRelativeFileSearch(RelativeFileName,Paths);
end;

{-------------------------------}

var
  FGeneratedPasswords:IPSCStrings;

function PSCGeneratePassword(Length:Integer):String;
var
  i:Integer;
  Index:Integer;
begin

  If FGeneratedPasswords=nil then
  begin
    FGeneratedPasswords:=PSCCreateSortedStringList;
    Randomize;
  end;

  While True do
  begin
    Result:='';
    for i:=1 to Length do
      Result:=Result+Char(PSCGetPasswordChar);
    If not FGeneratedPasswords.Find(Result,Index) then
    begin
      FGeneratedPasswords.Add(Result);
      exit;
    end;
  end;
end;

{-------------------------------}

procedure PSCWriteToLogPasswords(Number,Length:Integer);
var
  i:Integer;
begin
  for i:=1 to Number do
    PSCDoWriteToLog(PSCGeneratePassword(Length));
end;

{-------------------------------------------------------------------------}

procedure PSCSaveStringToFile(const FileName,DataStr:String);
begin
  with TStringList.Create do
    try
      Text := DataStr;
      SavetoFile(FileName);
    finally
      Free;
    end;
end;

{------------------------------------------------------------------}

procedure PSCAppendStringToFile(const FileName,S:String);
var
  F:TFileStream;
begin
  If not FileExists(FileName) then
    PSCSaveStringToFile(FileName,S)
  else
    begin
      F:=TFileStream.Create(FileName,fmOpenReadWrite);
      try
        F.Seek(0,soFromEnd);
        F.Write(S[1],Length(s));
      finally
        F.Free;
      end;
    end;
end;

{------------------------------------------------------------------}

function PSCGetAppFolder:String;
begin
  Result:=PSCAddSlash(ExtractFilePath(ParamStr(0)));
end;

{-------------------------------------}

function PSCRectsEqual(const R1,R2:TRect):Boolean;
begin
  Result:=(R1.Left=R2.Left) and
          (R1.Right=R2.Right) and
          (R1.Top=R2.Top) and
          (R1.Bottom=R2.Bottom);
end;

{-------------------------------------}

function PSCRandomDateTime : TDateTime;
var
  M, D, Y, hh, mm, ss: Integer;
  MyStr: String;
begin
  M:=Random(12)+1;
  D:=Random(28)+1;
  Y:=Random(21)+1990;
  hh:=Random(24);
  mm:=Random(60);
  ss:=Random(60);
  MyStr:=PSCIntToStr(M)+'/'+PSCIntToStr(D)+'/'+PSCIntToStr(Y)+' '+
    PSCIntToStr(hh)+':'+PSCIntToStr(mm)+':'+PSCIntToStr(ss);
  Result:=StrToDateTime(MyStr);
end;

{-------------------------------------}

procedure PSCFillDataSet(ADataSet: TDataSet; ARecCount: Integer);
var
  MyFieldIndex, MyRecordIndex: Integer;
  MySize: Integer;
begin
  Randomize;
  ADataSet.DisableControls;
  for MyRecordIndex:=1 to ARecCount do
  begin
    ADataSet.Append;
    for MyFieldIndex:=0 to ADataSet.FieldCount-1 do
    begin
      MySize:=ADataSet.Fields.Fields[MyFieldIndex].Size;
      case ADataSet.Fields.Fields[MyFieldIndex].DataType of
        ftString,ftMemo,ftFmtMemo,ftFixedChar,ftWideString:
          ADataSet.Fields.Fields[MyFieldIndex].AsString:=PSCRandomString(MySize);
        ftInteger,ftSmallint,ftWord,ftCurrency,ftLargeint:
          ADataSet.Fields.Fields[MyFieldIndex].AsInteger:=Random(100);
        ftBoolean :
          begin
            if Random(2)=1 then
              ADataSet.Fields.Fields[MyFieldIndex].AsBoolean:=True
            else
              ADataSet.Fields.Fields[MyFieldIndex].AsBoolean:=False;
          end;
        ftFloat:
          ADataSet.Fields.Fields[MyFieldIndex].AsFloat:=Random(1000)/10.0;
        ftDate,ftTime,ftDateTime{$IFDEF D6},ftTimeStamp{$ENDIF}:
          ADataSet.Fields.Fields[MyFieldIndex].AsDateTime:=PSCRandomDateTime;

        //TODO: Gestire i nuovi tipi di campi da RadStudio 2009 in poi...
      end;
    end;
    ADataSet.Post;
  end;
  ADataSet.EnableControls;
end;

{--------------------------------------}

Type
  TMPersistent = Class(TPersistent)
  End;

Function PSCFindOwnerWithClass(Instance: TPersistent; AClass: TPersistentClass):
  TPersistent;
Begin
  While Not (Instance Is AClass) And (Instance <> Nil) Do
    Instance := TMPersistent(Instance).GetOwner;
  Result := Instance;
End;

{-------------------------------------}

function PSCCorrectYear(AYear:Integer):Integer;
var
  YearLen:Integer;
  CenturyBase:Integer;
begin
  Result:=AYear;
  YearLen:=Length(PSCIntToStr(AYear));
  if (YearLen <= 2) then
  begin
    CenturyBase := PSCGetYear - FormatSettings.TwoDigitYearCenturyWindow;
    Inc(Result, CenturyBase div 100 * 100);
    if (FormatSettings.TwoDigitYearCenturyWindow > 0) and (Result < CenturyBase) then
      Inc(Result, 100);
  end;
end;

{-------------------------------------}

function PSCInheritsFromClassName(Instance : TObject;
  const CName : string) : boolean;
var
  c : TClass;
begin
  result := Instance <> nil;
  if not result then
    exit;

  c := Instance.ClassType;

  repeat
    if PSCCompareText(c.ClassName, CName) = 0 then
      exit;
    c := c.ClassParent
  until c = nil;

  result := false;
end;

{--------------------------------------}

function PSCReadStrFromStream(AStream:TStream):String;
Var
  L,i:Integer;
  MyBuffer:Array of Char;
begin
  If Assigned(AStream) Then
  begin
    L:=AStream.Size;
    SetLength(MyBuffer,L);
    L:=AStream.Read(MyBuffer,L);
    SetLength(MyBuffer,L);
    Result:='';
    for i:=Low(MyBuffer) to High(MyBuffer) do
      Result:=Result+MyBuffer[i];
  end;
end;

{------------------------------}

Var
  fCheckWidth: integer = 0;
  fCheckHeight: Integer = 0;

{------------------------------}

Procedure PSCGetCheckSize;
Begin
  If fCheckWidth = 0 Then
  begin
    FCheckWidth := GetSystemMetrics(SM_CXMENUCHECK);
    FCheckHeight := GetSystemMetrics(SM_CYMENUCHECK);
  end;
End;

{------------------------------}

Function PSCCheckWidth: integer;
Begin
  PSCGetCheckSize;
  result := fCheckWidth;
End;

{------------------------------}

Function PSCCheckHeight: integer;
Begin
  PSCGetCheckSize;
  result := fCheckHeight;
End;

{--------------------------------------}

procedure PSCDrawDot(ACanvas: TCanvas; ARect: TRect; AColor: TColor;
  AEnabled: Boolean);
begin
  if AEnabled then
    begin
      ACanvas.Brush.Color:=AColor;
      ACanvas.FillRect(ARect);
    end
  else
    begin
      OffsetRect(ARect, 1, 1);
      ACanvas.Brush.Color:=clPSCBtnHighLight;
      ACanvas.FillRect(ARect);
      OffsetRect(ARect, -1, -1);
      ACanvas.Brush.Color:=clPSCBtnShadow;
      ACanvas.FillRect(ARect);
    end;
end;

{--------------------------------------}

procedure PSCDrawEllipsis(ACanvas:TCanvas; const ARect: TRect;
  AEnabled: Boolean);
var
  X, Y, W: Integer;
  DotRect: TRect;
begin
  with ARect do
  begin
    X := Left + ((Right - Left) shr 1);
    Y := Top  + ((Bottom - Top) shr 1);
    W := PSCMax((Right - Left) shr 3, 1);
  end;

  DotRect := Bounds(X, Y, W, W);
  PSCDrawDot(ACanvas, DotRect, clPSCBlack, AEnabled);

  OffsetRect(DotRect, -(W * 2), 0);
  PSCDrawDot(ACanvas, DotRect, clPSCBlack, AEnabled);

  OffsetRect(DotRect, (W * 4), 0);
  PSCDrawDot(ACanvas, DotRect, clPSCBlack, AEnabled);
end;

{--------------------------------------}

procedure PSCDrawArrow(const ACanvas:IPSCCanvas; AArrowKind: TPSCArrowKind;
  const ARect: TPSCRect; AEnabled: Boolean);
const
  Kind: array[TPSCArrowKind] of Integer = (DFCS_SCROLLUP, DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
  KindByEnabled: array[Boolean] of Integer = (DFCS_INACTIVE, 0);
var
  Bitmap: TBitmap;
  IWidth, IHeight: Integer;
begin
  IWidth  := ARect.Right - ARect.Left - 2;
  IHeight := ARect.Bottom - ARect.Top - 2;
  Bitmap := TBitmap.Create;
  with Bitmap do
  try
    Width  := IWidth;
    Height := IHeight;

    DrawFrameControl(Canvas.Handle, Rect(-1, -1, IWidth + 1, IHeight + 1),
      DFC_SCROLL, Kind[AArrowKind] or KindByEnabled[AEnabled] or
      DFCS_ADJUSTRECT or DFCS_FLAT or DFCS_TRANSPARENT);
    Transparent := True;
    TransparentColor := clPSCBtnFace;
    PSCGetVCLCanvas(ACanvas).Draw(ARect.Left + 1, ARect.Top + 1, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

{--------------------------------------}

Type
  TTimerData = Class(TObject)
  private
    Procedure OnTimer(Sender: TObject);
  public
    EventID: Integer;
    Timer: TTimer;
    TimerEvent: TPSCTimerEvent;
    Destructor Destroy; override;
    Constructor Create;
  End;

Var
  FTimersList: IPSCObjectList;

{--------------------------------------}

Procedure TTimerData.OnTimer(Sender: TObject);
Begin
  TimerEvent(Self,EventID);
End;

{--------------------------------------}

Destructor TTimerData.Destroy;
Begin
  Timer.Enabled := False;
  Timer.Free;
  Inherited;
End;

{--------------------------------------}

Constructor TTimerData.Create;
Begin
  Inherited;
  Timer := TTimer.Create(Nil);
  Timer.Enabled := False;
  Timer.OnTimer := OnTimer;
End;

{-------------------------------------------------------------}

procedure PSCErrorFmt(const ErrorStr:String; const Args:Array of const);
begin
  raise Exception.CreateFmt(ErrorStr,Args);
end;

{-------------------------------------------------------------}

procedure PSCError(const ErrorStr:String);
begin
  raise Exception.Create(ErrorStr);
end;

{--------------------------------------}

Function PSCSetTimer(AEventID,ElapseTime: Integer;
  ATimerEvent: TPSCTimerEvent): TObject;
Var
  TimerData: TTimerData;
Begin
  If FTimersList = Nil Then
    FTimersList := PSCCreateObjectList(ioOwned);
  TimerData := TTimerData.Create;
  With TimerData Do
    Begin
      EventID := AEventID;
      TimerEvent := ATimerEvent;
      Timer.Interval := ElapseTime;
      Timer.Enabled := True;
    End;
  FTimersList.Add(TimerData);
  Result := TimerData;
End;

{--------------------------------------}

Procedure PSCKillTimer(var ATimer: TObject);
Begin
  If ATimer<>nil Then
  begin                            
    If FTimersList <> Nil then
      FTimersList.Remove(ATimer);
    ATimer:=nil;
  end;
End;

{------------------------------------------------------------------}

Function PSCGetVertCenter(Const R: TRect): Integer;
Begin
  Result := R.Top + PSCRectHeight(R) Div 2;
End;

{-------------------------------------}

Function PSCBkColorToHTString(C: TColor): String;
Begin
  Result:=PSCColorToHTString(C,'cb');//don't resource
End;

{---------------------------------------------------------}

Procedure PSCFillRectExclude(Canvas: TCanvas; Const ARect,ExcludeRect: TRect);
Begin
  With Canvas,ARect Do
    Begin
      FillRect(Rect(Left,Top,Right,ExcludeRect.Top - 1));
      FillRect(Rect(Left,ExcludeRect.Top - 1,
        ExcludeRect.Left - 1,ExcludeRect.Bottom + 1));
      FillRect(Rect(ExcludeRect.Right + 1,ExcludeRect.Top - 1,
        Right,ExcludeRect.Bottom + 1));
      FillRect(Rect(Left,ExcludeRect.Bottom + 1,Right,Bottom))
    End
End;

{--------------------------------------}

function PSCAlignRectInRect(const InnerRect, OuterRect : TRect;
  HorzAlign: TPSCHorzAlign; VertAlign: TPSCVertAlign): TRect;
var
  InnerWidth, InnerHeight: Integer;
  OuterWidth, OuterHeight: Integer;
begin
  InnerWidth  := InnerRect.Right - InnerRect.Left;
  OuterWidth  := OuterRect.Right - OuterRect.Left;
  Result.Left := OuterRect.Left;
  case HorzAlign of
   haCenter: Inc(Result.Left,(OuterWidth - InnerWidth) div 2);
   haRight : Inc(Result.Left,OuterWidth - InnerWidth);
  end;
  Result.Right  := Result.Left + InnerWidth;

  InnerHeight := InnerRect.Bottom - InnerRect.Top;
  OuterHeight := OuterRect.Bottom - OuterRect.Top;
  Result.Top  := OuterRect.Top;
  case VertAlign of
   vaCenter: Inc(Result.Top,(OuterHeight - InnerHeight) div 2);
   vaBottom: Inc(Result.Top,OuterHeight - InnerHeight);
  end;
  Result.Bottom := Result.Top + InnerHeight;
end;

{-------------------------------------}

procedure PSCDoWriteToLog(const Value:String);
var
  F:Text;
  LogFileName:String;
begin
  If Assigned(PSCWriteLogEvent) then
    PSCWriteLogEvent(Value)
  else
    try
      LogFileName:=ChangeFileExt(ParamStr(0),'.log'); //don't resource
      Assign(f,LogFileName);
      if not FileExists(LogFileName) then
        Rewrite(f)
      else
        Append(F);
      Writeln(F,Value);
      Close(F);
    except
    end;
end;
{-------------------------------------------}

Function PSCGetClientCursorPos(C: TControl): TPoint;
Begin
  GetCursorPos(Result);
  Result := C.ScreenToClient(Result);
End;

{------------------------------------------------------------------}
{$IFDEF TRIAL}
Const
  A2 = 'TAlignPalette'; //don't resource
  A3 = 'TPropertyInspector'; //don't resource
  A4 = 'TAppBuilder'; //don't resource

Function PSCIsDelphiRunning: boolean;
Var
  H2,H3,H4: Hwnd;
Begin
  H2 := FindWindow(A2,Nil);
  H3 := FindWindow(A3,Nil);
  H4 := FindWindow(A4,Nil);
  Result := (H2 <> 0) And (H3 <> 0) And (H4 <> 0);
End;
{$ENDIF}
{------------------------------------------------------------------}

{$IFDEF TRIAL}
Var
  CheckTrialCalled: boolean = False;

Procedure CheckTrialVersion;
Begin
  If CheckTrialCalled Then
    exit;
  CheckTrialCalled := True;

  If (Not PSCIsDelphiRunning) Then
    Begin
      windows.MessageBox(0,
        'This application uses unregistered version of our product'#13#10 + //don't resource
        'Please register. Visit our web site for details.' //don't resource
        , 'Warning',MB_OK + MB_ICONWARNING); //don't resource
    End;
End;
{$ENDIF}

{-----------------------------------------------------------------------}

Function PSCRectWidth(Const R: TRect): Integer;
Begin
  With R Do
    Result := right - left;
End;

{-----------------------------------------------------------------------}

Function PSCRectHeight(Const R: TRect): Integer;
Begin
  With R Do
    Result := bottom - top;
End;

{-------------------------------------------}

Function PSCCenterRect(Const RectToCenter,BaseRect: TRect): TRect;
Var
  FRectWidth,FRectHeight: Integer;
Begin
  FRectWidth := PSCRectWidth(RectToCenter);
  FRectHeight := PSCRectHeight(RectToCenter);
  With Result Do
    Begin
      Left := BaseRect.Left + (PSCRectWidth(BaseRect) - FRectWidth) Div 2;
      Top := BaseRect.Top + (PSCRectHeight(BaseRect) - FRectHeight) Div 2;
      Right := Left + FRectWidth;
      Bottom := Top + FRectHeight;
    End;
End;

{-------------------------------------------}

var
  PSCStartOfWeek: TPSCWeekDay;

Function PSCGetRealFirstDayOfWeek(FirstDayOfWeek: TPSCFirstDayOfWeek):
  TPSCWeekDay;
var
  i:Integer;
Begin
  If FirstDayOfWeek = dwLocaleDefault Then
    begin
      If PSCStartOfWeek=dwLocaleDefault then
      begin
        i := PSCStrToInt(PSCGetLocaleStr(GetThreadLocale,
          LOCALE_IFIRSTDAYOFWEEK, '0')) + 1;
        If i > 6 Then
          i := 0;
        PSCStartOfWeek := TPSCWeekDay(i + 1);
      end;
      Result := PSCStartOfWeek;
    end
  Else
    Result := FirstDayOfWeek;
End;

{-------------------------------------------}

Function PSCGetWeekStartEx(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek): TDateTime;
Var
  A: Integer;
Begin
  A := PSCDayOfWeek(Date) - Ord(PSCGetRealFirstDayOfWeek(FirstDay));
  If A < 0 Then
    inc(A,7);
  Result := Date - A;
End;

{-------------------------------------------}

Function PSCGetWeekEndEx(Const Date: TDateTime; FirstDay:
  TPSCFirstDayOfWeek): TDateTime;
Begin
  Result := PSCGetWeekStartEx(Date,FirstDay) + 6;
End;

{-------------------------------------------}

var
  FLocaleFirstWeekOfYear:TPSCFirstWeekOfYear;

function PSCLocaleFirstWeekOfYear: TPSCFirstWeekOfYear;
Var
  i: Integer;
begin
  If FLocaleFirstWeekOfYear=fwLocaleDefault then
  begin
    I := StrToInt(PSCGetLocaleStr(GetThreadLocale,
      LOCALE_IFIRSTWEEKOFYEAR, '0')) + 1;
    If (I > 0) Or (I <= 3) Then
      I := 1;
    FLocaleFirstWeekOfYear := TPSCFirstWeekOfYear(I);
  end;
  Result:=FLocaleFirstWeekOfYear;
end;

{----------------------------------------------------------}

Function PSCReplaceMonthNameToIndex(Const S: String): String;
Var
  i: Integer;

  Function DoReplace(Var S: String; Const MonthName: String;
    MonthIndex: Integer): boolean;
  Var
    A: Integer;
  Begin
    A := PSCPosIgnoreCase(MonthName,S);
    Result := A > 0;
    If Result Then
      PSCReplace(S,A,MonthName,PSCIntToStr(MonthIndex));
  End;

Begin
  Result := S;
  For i := 1 To 12 Do
    If DoReplace(Result,FormatSettings.LongMonthNames[i],i) Then
      exit;
  For i := 1 To 12 Do
    If DoReplace(Result,FormatSettings.ShortMonthNames[i],i) Then
      exit;
End;

{----------------------------------------------------------}

Function PSCStrToDateTimeEx(Const S: String; Const OldValue: TDateTime):
  TDateTime;
Begin
  If Assigned(PSCOnStrToDateTime) Then
    Result := PSCOnStrToDateTime(S,OldValue)
  Else
    Result := StrToDateTime(PSCReplaceMonthNameToIndex(S));
End;

{-------------------------------------------}

Function PSCStrToDateEx(Const S: String; Const OldValue: TDateTime): TDateTime;
Begin
  If Assigned(PSCOnStrToDate) Then
    Result := PSCOnStrToDate(S,OldValue)
  Else
    Result := StrToDate(PSCReplaceMonthNameToIndex(S));
End;

{----------------------------------------------------------}

Function PSCStrToTimeEx(Const S: String; Const OldValue: TDateTime): TDateTime;
Begin
  If Assigned(PSCOnStrToTime) Then
    Result := PSCOnStrToTime(S,OldValue)
  Else
    Result := StrToTime(S);
End;

{------------------------------------------------------------------}

Type
  TMControl = Class(TControl)
  End;

{--------------------------------------------}

Procedure PSCUpdateParamsWithBorderStyle(Var Params: TCreateParams; BorderStyle:
  TBorderStyle; Ctl3d: boolean);
Begin
  With Params Do
    If BorderStyle = bsSingle Then
      If NewStyleControls And Ctl3D Then
        Begin
          Style := Style And Not WS_BORDER;
          ExStyle := ExStyle Or WS_EX_CLIENTEDGE;
        End
      Else
        Style := Style Or WS_BORDER;
End;

{-------------------------------------------}

Function PSCRectClientToScreen(Control: TControl; Const R: TRect): TRect;
Begin
  Result := R;
  With Control,Result Do
    Begin
      TopLeft := ClientToScreen(TopLeft);
      BottomRight := ClientToScreen(BottomRight);
    End;
End;

{-------------------------------------------}

Function PSCGetWorkAreaRect: TRect;
Begin
{$IFDEF D6}
  Result := Screen.WorkAreaRect;
{$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA,0,@Result,0);
{$ENDIF}
End;

{-------------------------------------------}

Function PSCEnsureRectInRect(Const EnsureRect,BaseRect: TRect): TRect;
Begin
  Result := EnsureRect;
  PSCEnsureBound(Result.Left,Result.Right,BaseRect.Left,BaseRect.Right);
  PSCEnsureBound(Result.Top,Result.Bottom,BaseRect.Top,BaseRect.Bottom);
End;

{-------------------------------------------}

Function PSCEnsureRectInWorkArea(Const R: TRect): TRect;
Begin
  Result := PSCEnsureRectInRect(R,PSCGetWorkAreaRect);
End;

{-------------------------------------------}

Procedure PSCUpdatePopupRect(Popup: TWinControl; Const ParamRect: TRect);
Var
  R: TRect;
Begin
  With Popup Do
    Begin
      R := PSCGetWorkAreaRect;

      Left := ParamRect.Left;

      If Left < R.Left Then
        Left := R.Left;
      If Left + Width > R.Right Then
        Left := R.Right - Width;

      If ParamRect.Bottom + Height > R.Bottom Then
        Top := ParamRect.Bottom - (PSCRectHeight(ParamRect) - 1 + Height)
      Else
        Top := ParamRect.Bottom;
    End;
End;

{------------------------------------------------------------------}

Procedure PSCSetDefaultBorderParams(var ABorderParams: TPSCBevelInfoRec);
Begin
  With ABorderParams Do
    Begin
      BevelEdges := cPSCBevelEdgesDefault;
      BevelInner := cPSCBevelInnerDefault;
      BevelOuter := cPSCBevelOuterDefault;
      BevelKind := cPSCBevelKindDefault;
      BevelWidth := cPSCBevelWidthDefault;
    End;
End;

{------------------------------------------------------------------}

type
  THackCustomForm=class(TCustomForm)
  end;

Procedure PSCSetFormFont(Form: TCustomForm);
Begin
  If PSCFormsUseOneFont And THackCustomForm(Form).ParentFont And
    Assigned(Application.MainForm) Then
    Form.Font := Application.MainForm.Font;
End;

{------------------------------------------------------------------}

Type
  THackCanvas = Class(TCanvas)
  End;

{------------------------------}

function PSCRemoveExtraMonthFormat(const DateFormat:String;
  KeepLeadingZero:boolean):String;
Var
  ChangeStr:String;
begin
  If KeepLeadingZero then
    ChangeStr:='mm' //don't resource
  else
    ChangeStr:='m'; //don't resource

  Result:=DateFormat;

  PSCReplaceAllOccurEx(Result,'mmmm',ChangeStr,True); //don't resource
  PSCReplaceAllOccurEx(Result,'mmm',ChangeStr,True); //don't resource

end;

{------------------------------}

procedure PSCRemoveSysExtraMonthFormat;
begin
  System.SysUtils.FormatSettings.ShortDateFormat:=PSCRemoveExtraMonthFormat(
    FormatSettings.ShortDateFormat,True);
end;

{-------------------------------------------------------------------------}

function PSCIsInstanceInClasses(Instance:TObject;const ClassArray:Array of TClass):boolean;
var
  i:Integer;
begin
  for i:=Low(ClassArray) to High(ClassArray) do
    If Instance is ClassArray[i] then
    begin
      Result:=True;
      exit;
    end;
  Result:=False;
end;

{------------------------------------------------------------------}

Type
  THackCollection = Class(TCollection)
  End;

Function PSCGetCollectionUpdateCount(Collection: TCollection): Integer;
Begin
  Result := THackCollection(Collection).UpdateCount;
End;

{--------------------------------------------}

procedure TPSCCustomNamedItems.Changed;
begin
  inherited Changed;
end;

{--------------------------------------------}
{
procedure TPSCCustomNamedItems.LoadFromString(const aformat,s:string);
begin
end;
}

{--------------------------------------------}

function RenameXMLTag(e: IXMLNode; NewName: WideString): IXMLNode;
var
 Doc : IXMLDocument;
 NewElem, NParent : IXMLNode;
 DNOld, DNNew : IDOMNode;
 AC : IXMLNodeList;
 i: Integer;
begin
  Doc := e.OwnerDocument;
  NewElem := Doc.CreateNode(NewName, e.NodeType);
  while e.HasChildNodes do
    NewElem.DOMNode.AppendChild(e.DOMNode.firstChild);
  AC := e.AttributeNodes;
  for i := 0 to AC.Count - 1 do
    NewElem.Attributes[AC[i].NodeName] := AC[i].NodeValue;
  NParent := e.ParentNode;
  DNOld := e.DOMNode;
  DNNew := NewElem.DOMNode;
  NParent.DOMNode.replaceChild(DNNew, DNOld);
  Result := NewElem;
end;

{--------------------------------------------}

function TPSCCustomNamedItems.GetHiddenProps:TArray<string>;
const
  MyArray: TArray<String> = [
    'Collection',
    'DataObject',
    'Tag',
    'Index',
    'DisplayName',
    'UserFields'
  ];
begin
  result:=MyArray;
end;

{--------------------------------------------

procedure TPSCCustomNamedItems.SaveToString(const aformat:string;var result:string);

  procedure SaveToDfmString(var result:string);
  var
    Stream: TMemoryStream;
    Strings:TStringList;
  begin
    Stream:=TMemoryStream.Create;
    Strings:=TStringList.Create;
    try
      PSCWriteObjToTextStream(Stream,self);
      Stream.Seek(0,soFromBeginning);
      Strings.LoadFromStream(Stream);
      result:=Strings.Text;
    finally
      Stream.Free;
      Strings.Free;
    end;
  end;

  procedure SaveToJsonString(var result:string);
  begin

  end;

  procedure removeHiddenProp(aNode:IXMLNode;const propName:string);
  var
    foundNode:IXMLNode;
  begin
    foundNode:=aNode.ChildNodes.FindNode(propName);
    if foundNode<>nil then
      aNode.ChildNodes.Remove(foundNode);
  end;

  procedure removeHiddenProps(aNode:IXMLNode;instance:TPersistent);
  var
    hiddenProps:Tarray<string>;
    i:integer;
    Temp: IPSCStrings;
  begin
    hiddenProps:=GetHiddenProps;
    for I := 0 to Length(hiddenProps)-1 do
      removeHiddenProp(aNode,hiddenProps[i]);

    Temp := PSCCreateStringList;
    PSCPropNamesToStrings(Instance,Temp,False,True);
    for I := 0 to Temp.Count-1 do
      removeHiddenProp(aNode,Temp[i]);

  end;

  procedure SaveToXmlString(var result:string);
  var
    o : TPersistent;
    s : TXmlTypeSerializer;
    doc,x : TXmlDocument;
 //   v : TValue;
//    stream:TMemoryStream;
//    Strings:TStringList;
    i:integer;
    rootNode:IXMLNode;
//    itemNode:IXMLNode;
    clonedNode:IXMLNode;
  begin

    //Strings:=TStringList.Create;
    //stream:=TMemoryStream.Create;
    doc:= TXmlDocument.Create(Application);
    if not Doc.Active then
      Doc.Active := true;
    doc.Options:=doc.Options+[doNodeAutoIndent];
    rootNode:=doc.AddChild('items');

    try
      for i := 0 to Count-1 do
      begin
         x := TXmlDocument.Create(Application);
         x.Options:=x.Options+[doNodeAutoIndent];

        o:=items[i];
        s := TXmlTypeSerializer.create(o.classinfo,GetHiddenProps);

        s.Serialize(x,o);
        removeHiddenProps(x.DocumentElement,o);
        clonedNode:=RenameXMLTag(x.DocumentElement,'item');
        clonedNode:=clonedNode.CloneNode(true);

        rootNode.ChildNodes.Add(clonedNode);



        s.free;
        x.free;
      end;

      doc.SaveToXML(result);

      //doc.SaveToStream(stream);
      //Stream.Seek(0,soFromBeginning);
      //Strings.LoadFromStream(Stream);
      //result:=Strings.Text;
      result:=FormatXMLData(result);
    finally

      //stream.Free;
      //Strings.Free;
      doc.Free;
    end;
  end;

//  x.SaveToFile('FileName.txt');
//  v := s.Deserialize(x);
//  o := v.AsType<TypeIWantToSerailze>;

//var
// o : TypeIWantToSerailze;
// s : TXmlSerializer<TypeIWantToSerailze>;
// x : TXmlDocument;
//begin
//  s := TXmlTypeSerializer<TypeIWantToSerailze>.create;
//  x := TXmlDocument.Create(Self); // NEVER PASS NIL!!!
//  s.Serialize(x,o);
//  x.SaveToFile('FileName.txt');
// o := s.Deserialize(x);
//  x.free;
//  s.free;
//end;


begin
  if aformat='dfm' then
    SaveToDfmString(result)
  else
  if aformat='xml' then
    SaveToXmlString(result)
  else
  if aformat='json' then
    SaveToJsonString(result)

end;

--------------------------------------------}

Procedure TPSCCustomNamedItems.Update(Item: TCollectionItem);
Begin
  Inherited;
  If Assigned(FOnChange) Then
    FOnChange(Self);
  If Assigned(FOnUpdate) Then
    FonUpdate(Self,TPSCNamedItem(Item));
End;

{--------------------------------------------}

Constructor TPSCCustomNamedItems.Create(AOwner: TPersistent; ItemClass:
  TPSCNamedItemClass);
Begin
  Inherited Create(ItemClass);
  FOwner := AOwner;
End;

{--------------------------------------------}

Function TPSCCustomNamedItems.GetOwner: TPersistent;
Begin
  Result := FOwner;
End;

{--------------------------------------------}

Function PSCGetOwnerComponentState(Instance: TPersistent): TComponentState;
Begin
  Instance := TMPersistent(Instance).GetOwner;
  While (Instance <> Nil) And (Not (Instance Is TComponent)) Do
    Instance := TMPersistent(Instance).GetOwner;
  If Instance = Nil Then
    Result := []
  Else
    Result := TComponent(Instance).ComponentState;
End;

{--------------------------------------------}

Function TPSCCustomNamedItems.DesignTime: boolean;
Begin
  Result := csDesigning In PSCGetOwnerComponentState(Self);
End;

{--------------------------------------------}

Procedure TPSCCustomNamedItems.GetDisplayName(Item: TPSCNamedItem; Var
  DisplayName: String);
Begin
  If Assigned(OnGetDisplayName) Then
    OnGetDisplayName(Self,Item,DisplayName);
End;

{--------------------------------------------}

Function TPSCNamedItems.ItemByName(Const Name: String): TPSCNamedItem;
Var
  Index: Integer;
Begin
  Index := IndexOfName(Name);
  If Index >= 0 Then
    Result := Items[Index]
  Else
    Result := Nil;
End;

{--------------------------------------------}

Function TPSCNamedItem.DesignTime: boolean;
Begin
  Result := TPSCCustomNamedItems(Collection).DesignTime;
End;

{--------------------------------------------}

Procedure TPSCNamedItem.Assign(Source: TPersistent);
Begin
  If Source Is TPSCNamedItem Then
    PSCAssignAllProps(Source,Self)
  Else
    Inherited;
End;

{--------------------------------------------}

procedure TPSCNamedItem.SetUserFields(V:TPSCFields);
begin
  FUserFields.Assign(V);
end;

{--------------------------------------------}

function TPSCNamedItem.IsUserFieldsStored:Boolean;
begin
  result:=UserFields.Count>0;
end;

{--------------------------------------------}

constructor TPSCNamedItem.Create(Collection: TCollection);
begin
{$IFDEF TRIAL}
  CheckTrialVersion;
{$ENDIF}
  inherited;
  FUserFields:=TPSCFields.Create(Self,TPSCField);
end;

{--------------------------------------------}

destructor TPSCNamedItem.Destroy;
begin
  FUserFields.Free;
  inherited;
end;

{--------------------------------------------}

Procedure TPSCNamedItem.SetDisplayName(Const Value: String);
Begin
  Name := Value;
End;

{--------------------------------------------}

Function TPSCNamedItems.IndexOfName(Const Name: String): Integer;
Var
  i: Integer;
Begin
  For i := 0 To Count - 1 Do
    If Not (Items[i] Is TPSCNamedItem) Then
      break
    Else
      If PSCCompareText(Name,TPSCNamedItem(Items[i]).Name) = 0 Then
        Begin
          Result := i;
          exit;
        End;
  Result := -1;
End;

{--------------------------------------------}

Function TPSCNamedItem.GetDisplayName: String;
Begin
  Result := Name;
  TPSCCustomNamedItems(Collection).GetDisplayName(Self,Result);
  If Result = '' Then
    Result := Inherited GetDisplayName;
End;

{--------------------------------------------}

Procedure TPSCNamedItem.HandleEvent(const AParams:TPSCEventParams);
begin
end;

{--------------------------------------------}

Procedure TPSCNamedItem.OnChange(Sender: TObject);
Begin
  Changed(False);
End;

{--------------------------------------------}

Function TPSCPersistent.GetOwner: TPersistent;
Begin
  Result := FOwner;
End;

{--------------------------------------------}

Constructor TPSCPersistent.Create(AOwner: TPersistent);
Begin
  Inherited Create;
  FOwner := AOwner;
End;

{--------------------------------------------}

Procedure TPSCPersistent.Assign(Source: TPersistent);
Begin
  If Source Is TPSCPersistent Then
    PSCAssignAllProps(Source,Self)
  Else
    Inherited;
End;

{------------------------------------------------------------------}

Procedure TPSCPersistent.Changed(Sender:TObject);
begin
  Changed;
end;

{------------------------------------------------------------------}

Procedure TPSCPersistent.Changed;
Begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
End;

{------------------------------------------------------------------}

Function TPSCBorderParams.IsBevelEdgesStored: Boolean;
Begin
  Result := BevelEdges <> cPSCBevelEdgesDefault;
End;

{------------------------------------------------------------------}

Constructor TPSCBorderParams.Create(AOwner: TPersistent);
Begin
  PSCSetDefaultBorderParams(FBevelInfo);
  Inherited;
End;

{--------------------------------------}

Function TPSCBorderParams.GetBevelEdges: TPSCBevelEdges;
Begin
  Result := FBevelInfo.BevelEdges;
End;

{--------------------------------------}

Procedure TPSCBorderParams.SetBevelEdges(A: TPSCBevelEdges);
Begin
  FBevelInfo.BevelEdges := A;
  Changed;
End;

{--------------------------------------}

Function TPSCBorderParams.GetBevelInner: TPSCBevelCut;
Begin
  Result := FBevelInfo.BevelInner;
End;

{--------------------------------------}

Procedure TPSCBorderParams.SetBevelInner(A: TPSCBevelCut);
Begin
  FBevelInfo.BevelInner := A;
  Changed;
End;

{--------------------------------------}

Function TPSCBorderParams.GetBevelOuter: TPSCBevelCut;
Begin
  Result := FBevelInfo.BevelOuter;
End;

{--------------------------------------}

Procedure TPSCBorderParams.SetBevelOuter(A: TPSCBevelCut);
Begin
  FBevelInfo.BevelOuter := A;
  Changed;
End;

{--------------------------------------}

Function TPSCBorderParams.GetBevelKind: TPSCBevelKind;
Begin
  Result := FBevelInfo.BevelKind;
End;

{--------------------------------------}

Procedure TPSCBorderParams.SetBevelKind(A: TPSCBevelKind);
Begin
  FBevelInfo.BevelKind := A;
  Changed;
End;

{--------------------------------------}

Function TPSCBorderParams.GetBevelWidth: TPSCBevelWidth;
Begin
  Result := FBevelInfo.BevelWidth;
End;

{--------------------------------------}

Procedure TPSCBorderParams.SetBevelWidth(A: TPSCBevelWidth);
Begin
  FBevelInfo.BevelWidth := A;
  Changed;
End;

{--------------------------------------}

Procedure PSCWriteIntToStream(AStream: TStream; V: Integer);
Begin
  If Assigned(AStream) Then
    AStream.WriteBuffer(V,SizeOf(V));
End;

{------------------------------------------------------------------}

Type
  TPSCPersHolder = Class(TPSCComponent)
  private
    fP: TPersistent;
  published
    Property P: TPersistent read fP write fP;
  End;

{--------------------------------------------}

Procedure PSCWriteObjToTextFile(const AFileName: string; Instance: TPersistent);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(AFileName,fmCreate);
  Try
    PSCWriteObjToTextStream(FileStream,Instance);
  Finally
    FileStream.Free;
  End;
End;

{--------------------------------------------}

Procedure PSCReadObjFromTextFile(const AFileName: string; Instance: TPersistent);
Var
  FileStream: TFileStream;
Begin
  FileStream := TFileStream.Create(AFileName,fmOpenRead);
  Try
    PSCReadObjFromTextStream(FileStream,Instance);
  Finally
    FileStream.Free;
  End;
End;

{--------------------------------------------}

Procedure PSCWriteObjToTextStream(Stream: TStream; Instance: TPersistent);
Var
  TempStream: TMemoryStream;
Begin
  TempStream := TMemoryStream.Create;
  Try
    PSCWriteObjToStream(TempStream,Instance);
    TempStream.Seek(0,soFromBeginning);
    ObjectBinaryToText(TempStream,Stream);
  Finally
    TempStream.Free;
  End;
End;

{--------------------------------------------}

Procedure PSCReadObjFromTextStream(Stream: TStream; Instance: TPersistent);
Var
  TempStream: TMemoryStream;
Begin
  TempStream := TMemoryStream.Create;
  Try
    ObjectTextToBinary(Stream,TempStream);
    TempStream.Seek(0,soFromBeginning);
    PSCReadObjFromStream(TempStream,Instance);
  Finally
    TempStream.Free;
  End;
End;

{------------------------------------------------------------------}

Procedure PSCWriteObjToStream(Stream: TStream; Instance: TPersistent);
Var
  c: TPSCPersHolder;
Begin
  If Instance Is TComponent Then
    Stream.WriteComponent(TComponent(Instance))
  Else
    Begin
      c := TPSCPersHolder.Create(Nil);
      Try
        c.P := Instance;
        Stream.WriteComponent(c);
      Finally
        c.Free;
      End;
    End;
End;

{------------------------------------------------------------------}

Procedure PSCReadObjFromStream(Stream: TStream; Instance: TPersistent);
Var
  c: TPSCPersHolder;
Begin
  BeginGlobalLoading;
  Try
    If Instance Is TComponent Then
      Begin
        Stream.ReadComponent(TComponent(Instance));
        NotifyGlobalLoading;
      End
    Else
      Begin
        c := TPSCPersHolder.Create(Nil);
        Try
          c.P := Instance;
          Stream.ReadComponent(c);
        Finally
          c.Free;
        End;
      End;
  Finally
    EndGlobalLoading;
  End;
End;

{---------------------------------}

function TPSCNamedItems.GetItem(Index: Integer): TPSCNamedItem;
begin
  Result:=TPSCNamedItem(inherited Items[Index]);
end;

{---------------------------------}

procedure TPSCNamedItems.SetItem(Index: Integer; Value: TPSCNamedItem);
begin
  inherited Items[Index]:=Value;
end;

{---------------------------------}

Procedure TPSCNamedItems.DoInitNewField(ANewField: TPSCNamedItem);
Begin
  If Assigned(OnAddField) Then
    OnAddField(Self,ANewField);
End;

{----------------------------------------------------------}

Function PSCStrToDateTime(Const S: String): TDateTime;
Begin
  Result := PSCStrToDateTimeEx(S,PSCNow);
End;

{------------------------------}

Function PSCSysDTFormatToRec: TPSCDateTimeFormatRec;
Begin
  Result.ShortTimeFormat := FormatSettings.ShortTimeFormat;
  Result.ShortDateFormat := FormatSettings.ShortDateFormat;
  Result.LongDateFormat := FormatSettings.LongDateFormat;
  Result.LongTimeFormat := FormatSettings.LongTimeFormat;
  Result.DateSeparator := FormatSettings.DateSeparator;
  Result.TimeSeparator := FormatSettings.TimeSeparator;
  Result.TimeAMString := FormatSettings.TimeAMString;
  Result.TimePMString := FormatSettings.TimePMString;
End;

{------------------------------}

Procedure PSCRecToSysDTFormat(Const Rec: TPSCDateTimeFormatRec);
Begin
  FormatSettings.ShortTimeFormat := Rec.ShortTimeFormat;
  FormatSettings.ShortDateFormat := Rec.ShortDateFormat;
  FormatSettings.LongDateFormat := Rec.LongDateFormat;
  FormatSettings.LongTimeFormat := Rec.LongTimeFormat;
  FormatSettings.DateSeparator := Rec.DateSeparator[1];
  FormatSettings.TimeSeparator := Rec.TimeSeparator[1];
  FormatSettings.TimeAMString := Rec.TimeAMString;
  FormatSettings.TimePMString := Rec.TimePMString;
End;

{------------------------------------------------------------------}

Procedure PSCSafeRegisterClasses(AClasses: Array Of TPersistentClass);
Var
  i: Integer;
Begin
  For i := Low(AClasses) To High(AClasses) Do
    PSCSafeRegisterClass(AClasses[i]);
End;

{------------------------------------------------------------------}

Procedure PSCSafeRegisterClass(AClass: TPersistentClass);
Begin
  If AClass <> Nil Then
  Try
    If GetClass(AClass.ClassName) = Nil Then
      System.Classes.RegisterClass(AClass);
  Except
  End;
End;

{------------------------------------------------------------------}

Function PSCNameIsUnique(AOwner: TComponent; Const AName: String): boolean;
Begin
  result := true;

  While AOwner <> Nil Do
    Begin
      result := AOwner.FindComponent(AName) = Nil;
      If Not result Then
        exit;

      AOwner := AOwner.Owner;
    End;
End;

{------------------------------------------------------------------}

Function PSCUniqueName(AOwner: TComponent; Const BaseName: String): String;
Var
  i: integer;
Begin
  i := 0;
  Repeat
    inc(i);
    result := basename + PSCIntToStr(i);
  Until (AOwner = Nil) Or PSCNameIsUnique(AOwner,result);
End;

{-----------------------------------------------------------

Function PSCCreateSortedStringList(AObjectsOwned:TPSCItemOwnership=ioReferenced): IPSCStringList;
Begin
  Result := PSCCreateStringList(AObjectsOwned);
  Result.Sorted := True;
End;

{-------------------------------------------}

Type
  TFakeItem = Class(TPSCNamedItem)
  End;

Procedure PSCNamedItemsToStrings(Items: TPSCNamedItems;
  const Strings: IPSCStrings);
Var
  i: Integer;
Begin
  With Strings Do
    Begin
      BeginUpdate;
      Clear;
      Try
        For i := 0 To Items.Count - 1 Do
          Add(TFakeItem(Items.Items[i]).Name);
      Finally
        EndUpdate;
      End;
    End;
End;

{-------------------------------------------------------------------------}

Procedure PSCStrToFieldStrings(Const AFieldStr: String;
  const AFieldList: IPSCStrings);
Var
  I: Integer;
  S: String;
Begin
  PSCParseString(AFieldStr, ',',AFieldList);
  For I := 0 To AFieldList.Count - 1 Do
    Begin
      S := PSCTrim(AFieldList[I]);
      If S[1] = '[' Then
        S := Copy(S,2,Length(S) - 2);
      AFieldList[I] := S;
    End;
End;

{--------------------------------------------}

Procedure PSCParseString(Const s: String; Separator: char;
  const AStrings: IPSCStrings);
Var
  slist: IPSCStrings;
Begin
  slist := PSCCreateStringList;
  With slist Do
  begin
    Text := PSCChangeCharTo(Separator,#10,s);
    AStrings.AddStrings(slist);
  end;
End;

{---------------------------------------}

Function PSCVarIsEmptyOrNull(Const V: Variant): boolean;
Begin
  Result := VarIsNull(V) Or VarIsEmpty(V);
End;

{-------------------------------------------------------------------------}
Var
  FSysDecimalSeparator: Char;

Function PSCSysDecimalSeparator: Char;
Begin
  If FSysDecimalSeparator=#0 then
    FSysDecimalSeparator := PSCGetLocaleStr(GetThreadLocale,LOCALE_SDECIMAL,'.')[1];

  Result := FSysDecimalSeparator;
End;

{--------------------------------------------}

Function PSCKeysToShift: TShiftState;
Begin
  Result := [];
  If GetKeyState(VK_SHIFT) < 0 Then
    Include(Result,ssShift);
  If GetKeyState(VK_CONTROL) < 0 Then
    Include(Result,ssCtrl);
  If PSCIsAltKeyDown Then
    Include(Result,ssAlt);
End;

{--------------------------------------------}

Function PSCIsAltKeyDown: Boolean;
Begin
  Result := ((GetAsyncKeyState(VK_MENU) And $80000000) <> 0);
End;

{--------------------------------------------}

Procedure PSCSetScrollPos(W: HWND; Code: Integer; Value: Integer);
Begin
  If GetScrollPos(W,Code) <> Value Then
    SetScrollPos(W,Code,Value,True);
End;

{-------------------------------------------}
{$IFDEF OLDPREPAREFIELDNAME}
function PSCSQLPrepareFieldNameEx(const TableName,FieldName:String;
  AddTableName:boolean;FieldNameMask:String):String;
begin
  If FieldNameMask='' then
    FieldNameMask:=SPSCStdFieldNameMask;

  If TableName='' then
    Result:=FieldName
  else
    begin
      If AddTableName or PSCIsSQLKeywordOrWithSpace(FieldName) then
        Result:=Format(FieldNameMask,[TableName,FieldName])
      else
        Result:=FieldName;
    end;
end;
{$ELSE}
Function PSCSQLPrepareFieldNameEx(Const TableName,FieldName: String;
  AddTableName: boolean; FieldNameMask: String): String;
var
  MyMaskParams : Integer;
  //--------------
  function UpdateStandardMasks(const AShortMask,ALongMask:String):Boolean;
  begin
    Result:=(FieldNameMask = '');
    If Result Then
      begin
        If (TableName<>'') then
          begin
            FieldNameMask := ALongMask;
            MyMaskParams := 2;
          end
        else
          begin
            FieldNameMask := AShortMask;
            MyMaskParams := 1;
          end;
      end;
  end;
  //--------------
var
  MyTestMask : String;
Begin
  FieldNameMask := PSCTrim(FieldNameMask);

  If not UpdateStandardMasks(SPSCStdFieldNameMaskShort,SPSCStdFieldNameMask) then
    begin
      MyTestMask := FieldNameMask;
      MyMaskParams := PSCReplaceAllOccur(MyTestMask,'%S','')+ //don't resource
        PSCReplaceAllOccur(MyTestMask,'%s',''); //don't resource
      If not (MyMaskParams in [1,2]) then
      begin
        FieldNameMask:='';
        UpdateStandardMasks(SPSCStdFieldNameMaskShort,SPSCStdFieldNameMask);
      end;
    end;

    If AddTableName or PSCIsSQLKeywordOrWithSpace(FieldName) then
      begin
        If MyMaskParams=1 then
          Result := PSCFormat(FieldNameMask, [FieldName])
        else
          Result := PSCFormat(FieldNameMask, [TableName,FieldName]);
      end
    else
      Result := FieldName;
End;
{$ENDIF}

{-------------------------------------------------------------------------}

Procedure PSCSetScrollSize(W: HWND; Code,MinPos,MaxPos,PageSize: Integer);
Var
  T: TScrollInfo;
Begin
  With T Do
    Begin
      cbSize := SizeOf(T);
      fMask := SIF_PAGE Or SIF_RANGE;
      nMin := MinPos;
      nMax := MaxPos;
      nPage := PageSize;
      nPos := 0;
      nTrackPos := 0;
    End;
  SetScrollInfo(W,Code,T,True);
End;

{------------------------------------------------}

Function PSCDialogUnitsToPixelsX(X: Integer): Integer;
Var
  BaseUnitX: Integer;
Begin
  BaseUnitX := GetDialogBaseUnits And $0000FFFF;
  Result := (X * BaseUnitX) Div 4;
End;

{------------------------------------------------}

Function PSCDialogUnitsToPixelsY(Y: Integer): Integer;
Var
  BaseUnitY: Integer;
Begin
  BaseUnitY := (GetDialogBaseUnits And $FFFF0000) Shr 16;
  Result := (Y * BaseUnitY) Div 8;
End;

{-------------------------------------------------------------------------}

Function PSCGetScrollPosEx(Wnd: THandle; fnBar: Integer): Integer;
Var
  ScrollInfo: TScrollInfo;
Begin
  FillChar(ScrollInfo,SizeOf(ScrollInfo),0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_POS;
  GetScrollInfo(Wnd,fnBar,ScrollInfo);
  Result := ScrollInfo.nPos;
End;

{-------------------------------------------}

Function PSCGetWeekStart(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCGetWeekStartEx(Date,dwLocaleDefault)
End;

{-------------------------------------------}

Function PSCGetWeekEnd(Const Date: TDateTime): TDateTime;
Begin
  Result := PSCGetWeekEndEx(Date,dwLocaleDefault)
End;

{-------------------------------------------}

Function PSCGetLastWeek1: TDateTime;
Begin
  Result := PSCGetWeekStart(PSCIncWeek(PSCGetToday, -1));
End;

{-------------------------------------------}

Function PSCGetLastWeek7: TDateTime;
Begin
  Result := PSCGetWeekEnd(PSCIncWeek(PSCGetToday, -1));
End;

{-------------------------------------------}

Function PSCGetThisWeek1: TDateTime;
Begin
  Result := PSCGetWeekStart(PSCGetToday);
End;

{-------------------------------------------}

Function PSCGetThisWeek7: TDateTime;
Begin
  Result := PSCGetWeekEnd(PSCGetToday);
End;

{-------------------------------------------}

Function PSCGetNextWeek1: TDateTime;
Begin
  Result := PSCGetWeekStart(PSCIncWeek(PSCGetToday,1));
End;

{-------------------------------------------}

Function PSCGetNextWeek7: TDateTime;
Begin
  Result := PSCGetWeekEnd(PSCIncWeek(PSCGetToday,1));
End;

{------------------------------------------------------------------}

Function TPSCField.AsFilterStr: String;
Begin
  If VarIsArray(FData) then
    Result:=AsString
  else
    Result:=GetAsFilterString(AsString);
  TPSCFields(Collection).DoOnQuoteStr(Self,Result);
End;


{------------------------------------------------------------------}

Procedure TPSCField.ChangeDataTypeTo(V: TPSCFieldType);
Begin
  FDataType := V;
End;

{------------------------------------------------------------------}

Function TPSCField.DoGetValue: Variant;
Begin
  Result := FData;
End;

{------------------------------------------------------------------}

Function TPSCField.GetIsArray:Boolean;
begin
  Result:=(not IsNull) and VarIsArray(FData);
end;

{------------------------------------------------------------------}

Function TPSCField.GetAsVariant: Variant;
Begin
  If DesignTime Then
    Result := FData
  Else
    Result := DoGetValue;
End;

{------------------------------------------------------------------}

Procedure TPSCField.DoSetValue(Const Value: Variant);
Begin
  FData := Value;
End;

{------------------------------------------------------------------}

Procedure TPSCField.DefineProperties(Filer: TFiler);

  Function CanWriteData: Boolean;
  Begin
    If Filer.Ancestor <> Nil Then
      Result := Value <> TPSCField(Filer.Ancestor).Value
    Else
      Result := True;
  End;

Begin
  Inherited DefineProperties(Filer);
  Filer.DefineProperty('DataValue',ReadData,WriteData,//don't resource
    CanWriteData);
End;

{------------------------------------------------------------------}

Procedure TPSCField.ReadData(Reader: TReader);
Begin
  FData := PSCReadVariant(Reader);
End;

{------------------------------------------------------------------}

Procedure TPSCField.WriteData(Writer: TWriter);
Begin
  PSCWriteVariant(Writer,FData,FDataType);
End;

{------------------------------------------------------------------}

Procedure TPSCFields.DoOnQuoteStr(Field: TPSCField; Var QuotedStr: String);
Begin
  If Assigned(FOnQuoteStr) Then
    FOnQuoteStr(Self,Field,QuotedStr);
End;

{------------------------------------------------------------------}

Procedure TPSCFields.GetAsString(Sender: TObject; Field: TPSCField; Var
  ResultStr: String; ForDisplay: boolean);
Begin
  If Assigned(FOnGetAsString) Then
    FOnGetAsString(Sender,Field,ResultStr,ForDisplay);
End;

{------------------------------------------------------------------}

Function TPSCFields.GetItem(Index: Integer): TPSCField;
Begin
  Result := TPSCField(Inherited Items[Index]);
End;

{------------------------------------------------------------------}

Procedure TPSCFields.SetItem(Index: Integer; Value: TPSCField);
Begin
  Inherited SetItem(Index,Value);
End;

{------------------------------------------------------------------}

Function TPSCFields.GetFieldValue(Const FieldName: String): Variant;
Var
  P: TPSCField;
Begin
  P := TPSCField(ItemByName(FieldName));
  If P = Nil Then
    Result := Unassigned
  Else
    Result := P.Value;
End;

{------------------------------------------------------------------}

Procedure TPSCFields.SetFieldValue(Const FieldName: String;
  Const Value: Variant);
Var
  P: TPSCField;
Begin
  P := TPSCField(ItemByName(FieldName));
  If P <> Nil Then
    P.Value := Value
  Else                                    
    If AddFieldIfNeeded Then
      Begin
        P := TPSCField(Add);
        P.Name := FieldName;
        P.Value := Value;
      End;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SafeSetDataType(V: TPSCFieldType);
Begin
  If V <> FDataType Then
    Begin
      If IsNull Then
        Begin
          FDataType := V;
          Exit;
        End;
      Case V Of
        FT_STRING,FT_BOOL,FT_UNK:
          SetDataType(V);
        FT_DATETIME:
          If FDataType In [FT_DATE,FT_TIME] Then
            Begin
              FDataType := FT_DATETIME;
              Changed(False);
            End
          Else
            SetDataType(V);
        FT_DATE:
          If FDataType In [FT_DATETIME,FT_TIME] Then
            Begin
              SetAsVariantEx(PSCDateOf(AsDateTime),False);
              FDataType := FT_DATE;
              Changed(False);
            End
          Else
            SetDataType(V);
        FT_TIME:
          If FDataType In [FT_DATETIME,FT_DATE] Then
            Begin
              SetAsVariantEx(PSCTimeOf(AsDateTime),False);
              FDataType := FT_TIME;
              Changed(False);
            End
          Else
            SetDataType(V);
        FT_INT:
          If FDataType In [FT_CURRENCY,FT_FLOAT] Then
            AsInteger := Value
          Else
            SetDataType(V);
        FT_CURRENCY:
          If FDataType In [FT_INT,FT_FLOAT] Then
            AsCurrency := Value
          Else
            SetDataType(V);
        FT_FLOAT:
          If FDataType In [FT_INT,FT_CURRENCY] Then
            AsFloat := Value
          Else
            SetDataType(V);
      End;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetDataType(V: TPSCFieldType);
Begin
  Clear;
  FDataType := V;
End;

{------------------------------------------------------------------}

Constructor TPSCField.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FData := Unassigned;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetIsNull(Value: boolean);
Begin
  If Value Then
    Clear;
End;

{------------------------------------------------------------------}

Function TPSCField.GetIsNull: Boolean;
Var
  V: Variant;
Begin
  If FDataType=FT_OBJECT then
    Result:= FObject=nil
  else
    begin
      V := GetAsVariant;
      Result := VarIsNull(V) Or VarIsEmpty(V);
    end;
End;

{------------------------------------------------------------------}

Procedure TPSCField.Assign(Source: TPersistent);
Begin
  If Source Is TPSCField Then
    AssignField(TPSCField(Source))
  Else
    Inherited Assign(Source);
End;

{------------------------------------------------------------------}

Procedure TPSCField.AssignField(Param: TPSCField);
Begin
  If Param <> Nil Then
    Begin
      FValuePrefix := Param.FValuePrefix;
      FValueSuffix := Param.FValueSuffix;
      Name := Param.Name;
      SetAsVariantEx(Param.Value,False);
      FDataType := Param.DataType;
      Changed(False);
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCField.Clear;
Begin
  SetAsVariant(Unassigned);
  FObject:=nil;
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsObject:TObject;
begin
  Result:=FObject;
end;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsObject(V:TObject);
begin
  SetAsVariantEx(Unassigned,False);
  FObject:=V;
  FDataType:=FT_OBJECT;
  Changed(False);
end;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsBoolean(Value: Boolean);
Begin
  SetAsVariant(Value);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsBoolean: Boolean;
Begin
  If IsNull Then
    Result := False
  Else
    Result := GetAsVariant;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsFloat(Const Value: Double);
Begin
  SetAsVariant(Value);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsFloat: Double;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := GetAsVariant;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsCurrency(Const Value: Currency);
Begin
  SetAsVariant(Value);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsCurrency: Currency;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := GetAsVariant;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsInteger(Value: Longint);
Begin
  SetAsVariant(Value);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsInteger: Longint;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := GetAsVariant;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsString(Const Value: String);
Begin
  SetAsVariant(Value);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsString: String;
Begin
  Result := ValueToStr(False);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsText: String;
Begin
  Result := ValueToStr(True);
End;

{------------------------------------------------------------------}

Function TPSCField.ValueToStrEx(const AValue:Variant;
  AForDisplay: boolean): String;
begin
  If VarIsNull(AValue) Or VarIsEmpty(AValue) Then
    Result := ''
  Else
    Case DataType Of
      FT_BOOL:
        Begin
          If AValue Then
            Result := SPSCTextTrue
          Else
            Result := SPSCTextFalse;
        End;
      FT_FLOAT:
        Result := PSCFloatToStr(AValue);
    Else
      Result := AValue;
    End;
  If VarIsArray(FData) then
    Result:=GetAsFilterString(Result);
end;

{------------------------------------------------------------------}

Function TPSCField.GetAsFilterString(const S:String):String;
begin
  Result := ValuePrefix + S + ValueSuffix;
  If (DataType In cQuoteParamTypes) Then
    Result := '''' + Result + '''';
end;

{------------------------------------------------------------------}

Function TPSCField.ValueArrayToStr(AForDisplay: boolean): String;
var
  MyLowBound,MyHighBound:Integer;
  i:Integer;
  S:String;
begin
  Result:='';
  MyLowBound:=VarArrayLowBound(FData,1);
  MyHighBound:=VarArrayHighBound(FData,1);
  for i:=MyLowBound to MyHighBound do
  begin
    S:=ValueToStrEx(FData[i],AForDisplay);
    If Result<>'' then
      Result:=Result+', '+S
    else
      Result:=S;
  end;
  If Result<>'' then
    Result:='('+Result+')';
end;

{------------------------------------------------------------------}

Function TPSCField.ValueToStr(ForDisplay: boolean): String;
Begin
  If VarIsArray(FData) then
    Result:=ValueArrayToStr(ForDisplay)
  else
    Result:=ValueToStrEx(GetAsVariant,ForDisplay);
  TPSCFields(Collection).GetAsString(Collection,Self,Result,ForDisplay);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsDateTime(Const Value: TDateTime);
Begin
//  If Value=0 then
//    SetAsVariantEx(UnAssigned,False)
//  else
    SetAsVariantEx(Value,False);
  FDataType := FT_DATETIME;
  Changed(False);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsDate(Const Value: TDateTime);
Begin
//  If PSCDateOf(Value)=0 then
//    SetAsVariantEx(UnAssigned,False)
//  else
    SetAsVariantEx(Value,False);
  FDataType := FT_DATE;
  Changed(False);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsTime(Const Value: TDateTime);
Begin
//  If PSCTimeOf(Value)=0 then
//    SetAsVariantEx(UnAssigned,False)
//  else
    SetAsVariantEx(Value,False);
  FDataType := FT_TIME;
  Changed(False);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsVariant(Const Value: Variant);
Begin
  SetAsVariantEx(Value,True);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SafeSetValue(Const V: Variant);
Begin
  Case FDataType Of
    FT_UNK:
      Value := V;
    FT_INT:
      AsInteger := V;
    FT_DATETIME:
      AsDateTime := V;
    FT_DATE:
      AsDate := V;
    FT_TIME:
      AsTime := V;
    FT_CURRENCY:
      AsCurrency := V;
    FT_FLOAT:
      AsFloat := V;
    FT_STRING:
      AsString := V;
    FT_BOOL:
      AsBoolean := V;
  End;
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsVariantEx(Const Value: Variant; CallChanged: Boolean);
Begin
  SetAsVariantSilent(Value,CallChanged,True);
End;

{------------------------------------------------------------------}

Procedure TPSCField.SetAsVariantSilent(Const Value: Variant; CallChanged:
  Boolean; ACheckType: Boolean);
Begin
  If ACheckType And (Not PSCVarIsEmptyOrNull(Value)) Then
    FDataType := PSCFieldsTypeFromVarType(VarType(Value),FDataType);

  If DesignTime Then
    FData := Value
  Else
    DoSetValue(Value);

  If CallChanged Then
    Changed(False);
End;

{------------------------------------------------------------------}

procedure PSCParseAndEnumParams(const SQL:String;const AParams:IPSCStrings);
const
  CStopChars=[' ', ',', ';', ')',#13,#10];
var
  a,i:Integer;
  MyLen:Integer;
  S:String;
begin
  i:=1;
  MyLen:=Length(SQL);
  While i<MyLen do
  begin
    If (SQL[i]=':') and (SQL[i+1]<>':') then
      begin
        inc(i);
        a:=i;
        While (not CharInSet(SQL[a], CStopChars)) and (a<=MyLen) do //not (SQL[a] in CStopChars)) and (a<=MyLen) do
          inc(a);
        If a<>i then
        begin
          S:=Copy(SQL,i,a-i);
          AParams.Add(S);
          i:=a;
        end;
      end
    else
      inc(i);
  end;
end;

{------------------------------------------------------------------}

procedure TPSCFields.ParseParams(const S: String; DoClear: Boolean);
var
  MyParams:IPSCStrings;
  i:Integer;
  MyNewField:TPSCField;
Begin
  If DoClear Then
    Clear;
  MyParams:=PSCCreateStringList;
  PSCParseAndEnumParams(S,MyParams);
  BeginUpdate;
  try
    for i:=0 to MyParams.Count-1 do
      If IndexOfName(MyParams[i])<0 then
      begin
        MyNewField := TPSCField(Add);
        MyNewField.Name := MyParams[i];
        DoInitNewField(MyNewField);
      end;
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------}

procedure TPSCFields.SafeParseParams(const S: String);
Var
  TempParams: TPSCFields;
  i: Integer;
  NewParam: TPSCField;
Begin                              
  TempParams :=
    TPSCFields(TPSCNamedItemsClass(ClassType).Create(Nil,TPSCNamedItemClass(ItemClass)));
  TempParams.OnGetAsString := GetAsString;
  Try
    TempParams.Assign(Self);
    ParseParams(S,True);
    For I := 0 To TempParams.Count - 1 Do
      Begin
        NewParam := TPSCField(ItemByName(TempParams[i].Name));
        If (NewParam <> Nil) And (NewParam.IsNull) Then
          Begin
            NewParam.Assign(TempParams[i]);
          End;
      End;
  Finally
    TempParams.Free;
  End;
End;

{-------------------------------------------}

Function PSCReplaceWithParams(Const S: String; Params: TPSCFields): String;
Var
  i: Integer;
Begin
  Result := S;
  For i := 0 To Params.Count - 1 Do
    With Params[i] Do
      PSCReplaceAllOccur(Result, ':' + Name,AsFilterStr);
End;

{------------------------------------------------------------------}

Function TPSCField.GetAsDateTime: TDateTime;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := VarToDateTime(GetAsVariant);
End;

{------------------------------------------------------------------------------}

function PSCFieldTypeToVarType(AType:TPSCFieldType):TVarType;
const
  CPSCToVarType:Array[TPSCFieldType] of TVarType = (
  varNull,         {FT_UNK}
  varInteger,      {FT_INT}
  varDate,         {FT_DATETIME}
  varDate,         {FT_DATE}
  varDate,         {FT_TIME}
  varCurrency,     {FT_CURRENCY}
  varDouble,       {FT_FLOAT}
  varOleStr,       {FT_STRING}
  varBoolean,      {FT_BOOL}
  varNull,         {FT_OBJECT}
  varNull,         {FT_INTERFACE}
  varNull,         {FT_MEMO}
  varOleStr
  );
begin
  Result:=CPSCToVarType[AType];
end;

{------------------------------------------------------------------}

Type
  THackWriter = Class(TWriter)
  End;
  THackReader = Class(TReader)
  End;

  TPSCValueType = (va_Null,va_List,va_Int8,va_Int16,va_Int32,va_Extended,
    va_String,va_Ident,va_False,va_True,va_Binary,va_Set,va_LString,
    va_Nil,va_Collection,va_Single,va_Currency,va_Date,va_WString,va_Int64);

{------------------------------------------------------------------}

Function PSCReadVariant(Reader: TReader): Variant;

  Function MyReadVariant(Reader: TReader): Variant;
  {$IFDEF D6}
  var
    ValType: TValueType;
  begin
    ValType:=Reader.NextValue;
    If ValType=vaUTF8String then
      Result:=Reader.ReadString
    else
      Result:=Reader.ReadVariant;
  end;              
  {$ELSE}
  Const
    ValTtoVarT: Array[TPSCValueType] Of Integer = (varNull,varError,varByte,
      varSmallInt,varInteger,varDouble,varString,varError,varBoolean,
      varBoolean,varError,varError,varString,varEmpty,varError
      ,varSingle,varCurrency,varDate,varOleStr
      ,varError);
  Var
    ValType: TPSCValueType;
  Begin
    With Reader Do
      Begin
        ValType := TPSCValueType(NextValue);
        Case ValType Of
          va_Nil,va_Null:
            Begin
              If ReadValue = vaNil Then
                VarClear(Result)
              Else
                Result := NULL;
            End;
          va_Int8:
            TVarData(Result).VByte := Byte(ReadInteger);
          va_Int16:
            TVarData(Result).VSmallint := Smallint(ReadInteger);
          va_Int32:
            TVarData(Result).VInteger := ReadInteger;
          va_Extended:
            TVarData(Result).VDouble := ReadFloat;
          va_Single:
            TVarData(Result).VSingle := ReadSingle;
          va_Currency:
            TVarData(Result).VCurrency := Reader.ReadCurrency;
          va_Date:
            TVarData(Result).VDate := Reader.ReadDate;
          va_String,va_LString:
            Result := ReadString;
          va_WString:
            Result := ReadWideString;
          va_False,va_True:
            TVarData(Result).VBoolean := ReadValue = vaTrue;
        Else
          PSCError(PSCConsts.ReadError);
        End;
        TVarData(Result).VType := ValTtoVarT[ValType];
      End;
  End;
  {$ENDIF}

  function MyReadVariantArray(Reader: TReader): Variant;
  Var
    MyArray: Array of Variant;
    MySize:Integer;
    MyCount:Integer;
    i:Integer;
    AVarType:TPSCFieldType;
  begin
    MySize:=0;
    MyCount:=0;
    Reader.ReadListBegin;
    AVarType:=TPSCFieldType(Reader.ReadInteger);
    while not Reader.EndOfList do
    begin
      If MyCount>=MySize then
      begin
        inc(MySize,5);
        SetLength(MyArray,MySize);
      end;
      MyArray[MyCount]:=MyReadVariant(Reader);
      inc(MyCount);
    end;
    Reader.ReadListEnd;
    Result:=VarArrayCreate([0,MyCount-1],PSCFieldTypeToVarType(AVarType));
    for i:=0 to MyCount-1 do
      Result[i]:=MyArray[i];
  end;

begin
  If TPSCValueType(Reader.NextValue)=va_list then
    Result:=MyReadVariantArray(Reader)
  else
    Result:=MyReadVariant(Reader);
end;

{------------------------------------------------------------------}

Procedure PSCWriteVariant(Writer: TWriter; Const Value: Variant;
  AVarType:TPSCFieldType);

  Procedure MyWriteVariant(Writer: TWriter; Const Value: Variant);
  {$IFDEF D6}
  begin
    Writer.WriteVariant(Value);
  end;
  {$ELSE}
  Var
    VType: Integer;
  Begin
    If VarIsArray(Value) Then
      PSCError(PSCConsts.WriteError);
    VType := VarType(Value);
    With THackWriter(Writer) Do
      Case VType And varTypeMask Of
        varEmpty:
          WriteValue(vaNil);
        varNull:
          WriteValue(vaNull);
        varOleStr:
          WriteWideString(Value);
        varString,varStrArg:
          WriteString(Value);
        varByte,varSmallInt,varInteger:
          WriteInteger(Value);
        varSingle:
          WriteFloat(Value);
        varDouble:
          WriteFloat(Value);
        varCurrency:
          Writer.WriteCurrency(Value);
        varDate:
          Writer.WriteDate(Value);
        varBoolean:
          If Value Then
            WriteValue(vaTrue)
          Else
            WriteValue(vaFalse);
      Else
        Try
          WriteString(Value);
        Except
          Raise EWriteError.Create(PSCConsts.WriteError);
        End;
      End;
  End;
  {$ENDIF}
var
  MyLowBound:Integer;
  MyHighBound:Integer;
  i:Integer;
begin
  If VarIsArray(Value) then
    begin
      MyLowBound:=VarArrayLowBound(Value,1);
      MyHighBound:=VarArrayHighBound(Value,1);
      Writer.WriteListBegin;
      Writer.WriteInteger(Integer(AVarType));
      for i:=MyLowBound to MyHighBound do
        MyWriteVariant(Writer,Value[i]);
      Writer.WriteListEnd;
    end
  else
    MyWriteVariant(Writer,Value);
end;

{------------------------------------------------------------------}

Function PSCFieldsTypeFromVarType(AVarType: Integer;
  OldFieldType: TPSCFieldType): TPSCFieldType;
Begin
  Case (AVarType and VarTypeMask) Of
    varSmallint,varByte,varInteger
      {$IFDEF D6},varLongWord,varInt64,varShortInt,varWord{$ENDIF}:
      Result := FT_INT;
    varCurrency:
      Result := FT_CURRENCY;
    varSingle,varDouble:
      Result := FT_FLOAT;
    varDate:
      If OldFieldType In [FT_DATE,FT_TIME] Then
        Result := OldFieldType
      Else
        Result := FT_DATETIME;
    varBoolean:
      Result := FT_BOOL;
    varString,varOleStr,varStrArg{$IFDEF D2009},varUString,vtWideString{$ENDIF}:
      Result := FT_STRING;
  Else
    Result := FT_UNK;
  End;
End;

{------------------------------------------------------------------}

Procedure TPSCLinkedComp.Notification
  (AComponent: TComponent; Operation: TOperation);
Begin
  Inherited;
  If (Operation = OpRemove) And (Assigned(FNotifier)) Then
    FNotifier(AComponent);
End;

{------------------------------------------------------------------}

Procedure TPSCLinkedItem.Notification(Instance: TComponent);
Begin
End;

{------------------------------------------------------------------}

Procedure TPSCLinkedItem.RegisterNotifier(Component: TComponent);
Begin
  If Component = Nil Then
    exit;

  If FLinkedComp = Nil Then
    Begin
      FLinkedComp := TPSCLinkedComp.Create(Nil);
      FLinkedComp.Notifier := Notification;
    End;
  Component.FreeNotification(FLinkedComp);
End;

{------------------------------------------------------------------}

Destructor TPSCLinkedItem.Destroy;
Begin
  FLinkedComp.Free;
  Inherited;
End;

{--------------------------------------------}

Function PSCGetEditHeight(Font: TFont; Ctl3d: boolean): Integer;
Var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics,Metrics: TTextMetric;
Begin
  DC := GetDC(0);
  GetTextMetrics(DC,SysMetrics);
  SaveFont := SelectObject(DC,Font.Handle);
  GetTextMetrics(DC,Metrics);
  SelectObject(DC,SaveFont);
  ReleaseDC(0,DC);
  If NewStyleControls Then
    Begin
      If Ctl3D Then
        I := 8
      Else
        I := 6;
      I := PSCGetSystemMetrics(SM_CYBORDER) * I;
    End
  Else
    Begin
      I := SysMetrics.tmHeight;
      If I > Metrics.tmHeight Then
        I := Metrics.tmHeight;
      I := I Div 4 + PSCGetSystemMetrics(SM_CYBORDER) * 4;
    End;
  Result := Metrics.tmHeight + I;
End;

{--------------------------------------------}

procedure TPSCCustomControlAncestor.InvalidateRect(const R:TRect);
begin
  If HandleAllocated then
    Winapi.Windows.InvalidateRect(Handle,@R,False);
end;

{------------------------------------------------------------------}

procedure TPSCIntfPersistent.AfterConstruction;
begin
  {$IFDEF LINUX}
  Dec(FRefCount);
  {$ELSE}
  InterlockedDecrement(FRefCount);
  {$ENDIF}
  FInstanceID:=PSCGetNewInstanceID;
end;

{------------------------------------------------------------------}

procedure TPSCIntfPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then
    PSCError(PSCConsts.ErrRefCount);
end;

{------------------------------------------------------------------}

class function TPSCIntfPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TPSCIntfPersistent(Result).FRefCount := 1;
end;

{------------------------------------------------------------------}

function TPSCIntfPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{------------------------------------------------------------------}

function TPSCIntfPersistent._AddRef: Integer;
begin
  {$IFDEF LINUX}
  Inc(FRefCount);
  Result := FRefCount;
  {$ELSE}
  Result := InterlockedIncrement(FRefCount);
  {$ENDIF}
end;

{------------------------------------------------------------------}

function TPSCIntfPersistent._Release: Integer;
begin
  {$IFDEF LINUX}
  Dec(FRefCount);
  if FRefCount = 0 then
  begin
    Destroy;
    Result := 0;
    Exit;
  end;
  Result := FRefCount;
  {$ELSE}
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
  {$ENDIF}
end;

{--------------------------------------------}

{$IFDEF MyRightToLeft}

Function TPSCCustomControl.UseRightToLeftAlignment: Boolean;
Begin
  Result := False;
End;
{$ENDIF}

{--------------------------------------------}

Procedure TPSCCustomControlAncestor.InvalidateWhenChanged(Sender: TObject);
Begin
  Invalidate;
End;

{--------------------------------------------}

Procedure TPSCCustomControlAncestor.InitDefaultKeyMapping;
Begin
End;

{--------------------------------------------}

Procedure TPSCCustomControlAncestor.KeyPress(Var Key: Char);
Begin
  Inherited;
  If (csDesigning In ComponentState) Or (KeyState <> 0) Then
    exit;
  If InsertChar(Char(Key)) Then
    Key := #0;
End;

{--------------------------------------------}

Procedure TPSCCustomControlAncestor.WMGetDlgCode(Var Msg: TWMGetDlgCode);
Begin
  With Msg Do
    Begin
      Result := DLGC_WANTMESSAGE Or
        DLGC_WANTALLKEYS Or
        DLGC_WANTARROWS Or
        DLGC_WANTCHARS;

      If FWantTabs Then
        Result := Result Or DLGC_WANTTAB;
      If Not FWantReturns Then
        Result := Result And Not DLGC_WANTALLKEYS;
    End;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.UpdateInputKeys;
Begin
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.HandleEvent(const AParams:TPSCEventParams);
begin
end;

{-------------------------------------------------------------}

Function TPSCCustomControlAncestor.InsertChar(Ch: Char): Boolean;
Begin
  result := false;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.ClearKeys;
Begin
  FKeys.Clear;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.UpdateKeyMapping;
Begin
  ClearKeys;
  If Assigned(FKeyBoardInit) Then
    FKeyboardInit(Self)
  Else
    InitDefaultKeyMapping;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.SetWantTabs(V: boolean);
Begin
  If FWantTabs <> V Then
    Begin
      FWantTabs := V;
      UpdateInputKeys;
    End;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.SetWantReturns(V: boolean);
Begin
  If FWantReturns <> V Then
    Begin
      FWantReturns := V;
      UpdateInputKeys;
    End;
End;

{-------------------------------------------------------------}

Destructor TPSCCustomControlAncestor.Destroy;
Begin
  FKeys.Free;
  Inherited;
End;

{-------------------------------------------------------------}

Function TPSCCustomControlAncestor.GetVCLCanvas:TCanvas;
begin
  Result:=Canvas;
end;

{-------------------------------------------------------------}

Constructor TPSCCustomControlAncestor.Create(AOwner: TComponent);
Begin
  Inherited;
  WantReturns := True;
  FKeys := TPSCKeyList.Create(Self,TPSCKeyData);
  FMylaCanvas:=PSCCreateCanvasAdapter(GetVCLCanvas);
  UpdateInputKeys;
  UpdateKeyMapping;
End;

{-------------------------------------------------------------}

Constructor TPSCCustomControl.Create(AOwner: TComponent);
Begin
  Inherited;
  FBorderStyle := bsSingle;
  {
  BevelInner:=bvNone;
  BevelKind:=bkFlat;
  BevelOuter:=bvRaised;
  }
End;

{-------------------------------------------------------------}

Destructor TPSCCustomControl.Destroy;
Begin
  FMergedPopup.Free;
  Inherited;
End;

{--------------------------------------------}

Procedure TPSCCustomControlAncestor.CNKeyDown(Var Message: TWMKeyDown);
Begin
  If KeyState = 0 Then
    Inherited
  Else
    With Message Do
      Begin
        KeyDown(CharCode,PSCKeysToShift);
        If CharCode = 0 Then
          Result := 1
        Else
          Inherited;
      End;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.CNSysKeyDown(Var Message: TWMKeyDown);
Begin
  With Message Do
    Begin
      KeyDown(CharCode,PSCKeysToShift);
      If CharCode = 0 Then
        Result := 1
      Else
        Inherited;
    End;
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.SetKeys(V: TPSCKeyList);
Begin
  FKeys.Assign(V);
End;

{------------------------------------------------------------------}

Function PSCGetControlPopupMenuEx(C,SkipC: TControl): TPopupMenu;
Begin
  result := Nil;
  While c <> Nil Do
    Begin

      If C = SkipC Then
        result := TMControl(c).PopupMenu
      Else
        result := TMControl(c).GetPopupMenu;

      If Assigned(result) Then
        exit;
      c := c.Parent;
    End;
End;

{--------------------------------------------}

Procedure PSCRemoveInvisibleMenuItems(MenuItem: TMenuItem);
Var
  i: Integer;
Begin
  With MenuItem Do
    For i := Count - 1 Downto 0 Do
      If Not Items[i].Visible Then
        Items[i].Free;
End;

{--------------------------------------------}

Procedure PSCProcessExtraMenuSeparators(MenuItem: TMenuItem; ActionCode:
  TPSCMenuSepAction);
Var
  A: Integer;

  Procedure ActionProc(Index: Integer);
  Begin
    With MenuItem.Items[index] Do
      If ActionCode = msaRemove Then
        Free
      Else
        Visible := False;
  End;

Begin
  With MenuItem Do
    If Count > 0 Then
      Begin
        If ActionCode = msaShowHide Then
          For a := 0 To Count - 1 Do
            If Items[a].Caption = '-' Then
              Items[a].Visible := True;

        // Remove all separators from menu top
        While (Count > 0) And (Items[0].Caption = '-') Do
          ActionProc(0);

        // Remove all separators from menu bottom
        While (Count > 0) And (Items[Count - 1].Caption = '-') Do
          ActionProc(Count - 1);

        // Remove double separators
        a := Count - 2;
        While a >= 0 Do
          Begin
            If (Items[a].Caption = '-') And (Items[a + 1].Caption = '-') Then
              ActionProc(a);
            dec(a);
          End;
      End;
End;

{------------------------------------------------------------------}

Function PSCCreateMergedPopupMenu(AMenu1,AMenu2: TPopupMenu): TPopupMenu;
Var
  i,k,Count1,Count2: Integer;
  AMenuItem: TMenuItem;
  AMenuItems: Array of TMenuItem;
  TotalCount: Integer;
Begin
  Result := Nil;

  If AMenu1 = Nil Then
    Count1 := 0
  Else
    Count1 := AMenu1.Items.Count;

  If AMenu2 = Nil Then
    Count2 := 0
  Else
    Count2 := AMenu2.Items.Count;

  TotalCount := Count1 + Count2;

  If TotalCount = 0 Then
    exit;

  SetLength(AMenuItems,TotalCount);

  For i := 0 To Count1 - 1 Do
    Begin
      AMenuItem := TMenuItem.Create(Nil);
      PSCAssignAllProps(AMenu1.Items[i],AMenuItem);
      AMenuItem.OnClick := AMenu1.Items[i].OnClick;
      AMenuItems[i] := AMenuItem;
    End;

  For i := 0 To Count2 - 1 Do
    Begin
      AMenuItem := TMenuItem.Create(Nil);
      PSCAssignAllProps(AMenu2.Items[i],AMenuItem);
      AMenuItem.OnClick := AMenu2.Items[i].OnClick;
      AMenuItems[Count1 + i] := AMenuItem;
    End;

  For k := 0 To TotalCount - 1 Do
    Begin
      AMenuItem := AMenuItems[k];
      For i := k + 1 To TotalCount - 1 Do
        Begin
          If AMenuItems[i].GroupIndex < AMenuItem.GroupIndex Then
            Begin
              AMenuItems[k] := AMenuItems[i];
              AMenuItems[i] := AMenuItem;
              AMenuItem := AMenuItems[k];
            End
        End;
    End;

  Result := NewPopupMenu(AMenu1.Owner,
    '',paLeft,True,AMenuItems);

  If (AMenu1 <> Nil) And Assigned(AMenu1.Images) Then
    Result.Images := AMenu1.Images
  Else
    If Assigned(AMenu2) Then
      Result.Images := AMenu2.Images;
End;

{--------------------------------------------}

Function TPSCCustomControl.GetPopup: TPopupMenu;
Begin
  Result := PSCGetControlPopupMenuEx(Self,Self);

  If (FDefaultPopup <> Nil) And (ShowDefaultPopup Or MergePopupMenus) Then
    Begin
      FMergedPopup.Free;
      FMergedPopup:=nil;

      If Assigned(FDefaultPopup.OnPopup) Then
        FDefaultPopup.OnPopup(FDefaultPopup);

      If (Result <> Nil) And (Assigned(Result.OnPopup)) Then
        Result.OnPopup(Result);

      If Not MergePopupMenus Then
        Result := Nil;

      FMergedPopup := PSCCreateMergedPopupMenu(FDefaultPopup,Result);
      PSCRemoveInvisibleMenuItems(FMergedPopup.Items);

      PSCProcessExtraMenuSeparators(FMergedPopup.Items,msaRemove);

      If FMergedPopup.Items.Count > 0 Then
        Result := FMergedPopup
      Else
        Result := Nil;
    End;
End;

{--------------------------------------------}

Procedure TPSCCustomControl.ShowPopup(X,Y: integer);
Var
  FMenu: TPopupMenu;
  P: TPoint;
Begin
  FMenu := GetPopup;

  If FMenu = Nil Then
    exit;

  FMenu.PopupComponent := Self;
  P := ClientToScreen(Point(X,Y));
  FMenu.Popup(P.X,P.Y);
End;

{-------------------------------------------------------------}

Procedure TPSCCustomControlAncestor.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  ResultKeyData: TPSCKeyData;
Begin
  Inherited;

  If ((Key = VK_RETURN) And Not FWantReturns) Or ((Key = VK_TAB) And Not
    FWantTabs) Then
    Begin
      If ssCtrl In Shift Then
        Shift := []
      Else
        exit;
    End;

  If (Key = 0) Or (csDesigning In ComponentState) Then
    exit;

  ResultKeyData := FKeys.FindKeyData(Key,Shift,FKeyState);
  If (ResultKeyData <> Nil) Then
    With ResultKeyData Do
      Begin
        If assigned(ActionCode) then
          ActionCode;
        Key := 0;
        FkeyState := LeaveState;
        exit;
      End;
  FKeyState := 0;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.SetBorderStyle(Value: TBorderStyle);
Begin
  If FBorderStyle <> Value Then
    Begin
      FBorderStyle := Value;
      RecreateWnd;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.SetScrollSize(Code,MinPos,MaxPos,PageSize: Integer);
Begin
  PSCSetScrollSize(Handle,Code,MinPos,MaxPos,PageSize);
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.SetScrollPos(Code,Value: Integer);
Begin
  PSCSetScrollPos(Handle,Code,Value);
End;

{------------------------------------------------------------------}

Function TPSCCustomControl.HorzScrollVisible: boolean;
Begin
  Result := False;
End;

{------------------------------------------------------------------}

Function TPSCCustomControl.VertScrollVisible: boolean;
Begin
  Result := False;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.UpdateScrollBars;
Begin
End;

{--------------------------------------}

Procedure TPSCCustomControl.PerformHorzScroll(ScrollCode,ScrollPos: Integer);
Begin
End;

{--------------------------------------}

Procedure TPSCCustomControl.PerformVertScroll(ScrollCode,ScrollPos: Integer);
Begin
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.WMVScroll(Var Message: TWMScroll);
Begin
  If Not ScrollBarsLocked Then
    Begin
      PerformVertScroll(Message.ScrollCode,Message.Pos);
      Message.Result := 0;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomControl.WMHScroll(Var Message: TWMScroll);
Begin
  If Not ScrollBarsLocked Then
    Begin
      PerformHorzScroll(Message.ScrollCode,Message.Pos);
      Message.Result := 0;
    End;
End;

{-------------------------------------------------------------}

Procedure TPSCKeyList.Update(Item: TCollectionItem);
Begin
  Inherited;
  FCachedData := Nil;
End;

{-------------------------------------------------------------}

Function TPSCKeyList.FindKeyData(AKey: Word; AShift: TShiftState; AState:
  ShortInt): TPSCKeyData;
Var
  i: Integer;

  Function IsNeededKeyData(KeyData: TPSCKeyData): boolean;
  Begin
    With KeyData Do
      Result := (KeyCode = AKey) And (AShift = ShiftState)
        And ((AState = ActiveState) Or (ActiveState = -1));
  End;

Begin
  If (FCachedData <> Nil) And IsNeededKeyData(FCachedData) Then
    Begin
      Result := FCachedData;
      exit;
    End;

  For i := Count - 1 Downto 0 Do
    If IsNeededKeydata(TPSCKeyData(Items[i])) Then
      Begin
        Result := TPSCKeyData(Items[i]);
        FCachedData := Result;
        exit;
      End;
  Result := Nil;
End;

{-------------------------------------------------------------}

Function TPSCKeyList.AddKey(AKey: Word; AShift: TShiftState; ACode:
  TPSCActionProc;
  AActiveState,ALeaveState: ShortInt): TPSCKeyData;
Begin
  Result := TPSCKeyData(Add);
  With Result Do
    Begin
      KeyCode := AKey;
      ShiftState := AShift;
      ActionCode := ACode;
      ActiveState := AActiveState;
      LeaveState := ALeaveState;
    End;
End;

{-------------------------------------------------------------}

Function TPSCKeyList.AddNoShiftKey(AKey: Word; ACode: TPSCActionProc):
  TPSCKeyData;
Begin
  Result := AddSimpleKey(AKey, [],ACode);
End;

{-------------------------------------------------------------}

Function TPSCKeyList.AddSimpleKey(AKey: Word; AShift: TShiftState; ACode:
  TPSCActionProc): TPSCKeyData;
Begin
  Result := AddKey(AKey,AShift,ACode,0,0);
End;

{-------------------------------------------------------------}

Function TPSCKeyList.AddAllKey(AKey: Word; AShift: TShiftState; ACode:
  TPSCActionProc): TPSCKeyData;
Begin
  Result := AddKey(AKey,AShift,ACode, -1,0);
End;

{-------------------------------------------------------------------------}

Function PSCFileExtToIndex(Const FileName: String; Const Extensions: Array Of
  String): Integer;
Var
  S: String;
  i: Integer;
Begin
  S := PSCTrimSeparators(ExtractFileExt(FileName), ['.']);
  For i := Low(Extensions) To High(Extensions) Do
    If PSCCompareText(S,PSCTrimSeparators(Extensions[i], ['.'])) = 0 Then
      Begin
        Result := i;
        exit;
      End;
  Result := -1;
End;

{-------------------------------------------------------------------------}

Function PSCNormalizeSaveDlgFileExt(Const FileName,SaveDlgFilter: String;
  FilterIndex: Integer): String;
Var
  Temp: IPSCStrings;
  Index: Integer;
Begin
  Result := FileName;
  If (ExtractFileExt(Result) = '') Then
    Begin
      Temp := PSCCreateStringList;
      PSCParseString(SaveDlgFilter, '|',Temp);
      Index := 1 + (FilterIndex - 1) * 2;
      If Index < Temp.Count Then
        Result := ChangeFileExt(Result,ExtractFileExt(Temp[Index]));
    End;
End;

{------------------------------------------------------------------}

Function PSCShortCutFromKeyDef(KeyDef: TPSCKeyDef): TShortCut;
Begin
  With KeyDef Do
    Result := ShortCut(KeyCode,ShiftState);
End;

{------------------------------------------------------------------}

Procedure PSCSetMenuItemEnabledByTag(Items: TMenuItem; Tag: Integer; Enabled:
  boolean);
Var
  Item: TMenuItem;
Begin
  Item := PSCMenuItemByTag(Items,Tag);
  If Item <> Nil Then
    Item.Enabled := Enabled;
End;

{------------------------------------------------------------------}

Procedure PSCSetMenuItemVisibleByTag(Items: TMenuItem; Tag: Integer; Visible:
  boolean);
Var
  Item: TMenuItem;
Begin
  Item := PSCMenuItemByTag(Items,Tag);
  If Item <> Nil Then
    Item.Visible := Visible;
End;

{------------------------------------------------------------------}

Function PSCMenuItemByTag(Items: TMenuItem; Tag: Integer): TMenuItem;
Var
  i: Integer;
Begin
  Result := Nil;
  For i := 0 To Items.Count - 1 Do
    Begin
      If Items[i].Tag = Tag Then
        Begin
          Result := Items[i];
          exit;
        End;
      Result := PSCMenuItemByTag(Items[i],Tag);
      If Result <> Nil Then
        exit;
    End;
End;

{------------------------------}

function TPSCIntfPersistent.GetInstanceID:Integer;
begin
  Result:=FInstanceID;
end;

{------------------------------}

type
  TPSCStringsAdapter=class(TPSCIntfPersistent,IPSCStrings,IPSCInstanceID,IPSCSaveToFile)
  private
    FStrings:TStrings;
    FStringsOwned:TPSCItemOwnerShip;
    FObjectsOwned:TPSCItemOwnerShip;
  protected
    function GetInstanceID:Integer;

    function Add(const S: string): Integer;
    function AddObject(const S: string; AObject: TObject): Integer;
    function IndexOf(const S: string): Integer;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;

    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    procedure LoadFromFile(const AFileName:String);
    procedure SaveToFile(const AFileName:String);
    procedure AssignStrings(const AStrings:IPSCStrings);
    procedure AssignStringsTo(const AStrings:IPSCStrings);
    procedure AddStrings(const AStrings:IPSCStrings);

    procedure IPSCStrings.Assign=AssignStrings;
    procedure IPSCStrings.AssignTo=AssignStringsTo;

    function GetCapacity:Integer;
    function GetCount:Integer;
    function GetCommaText:String;
    function GetName(AIndex: Integer):String;
    function GetObject(AIndex: Integer):TObject;
    function GetValue(const AName: string): string;
    function GetString(AIndex: Integer): string;
    function GetTextStr:String;

    procedure SetTextStr(const AValue:String);
    procedure PutString(AIndex: Integer;const AValue: string);
    procedure SetValue(const AName,AValue: string);
    procedure PutObject(AIndex: Integer;AObject:TObject);
    procedure SetCapacity(AValue:Integer);
    procedure SetCount(V:Integer);

    function Find(const S: string; var Index: Integer): Boolean;virtual;
    function GetDuplicates: TPSCDuplicates;virtual;
    function GetSorted:Boolean;virtual;

    procedure SetDuplicates(AValue:TPSCDuplicates);virtual;
    procedure SetSorted(AValue:Boolean);virtual;
    procedure Sort;virtual;
  public
    constructor Create(AStrings:TStrings;AStringsOwned,AObjectsOwned:TPSCItemOwnerShip);
    destructor Destroy;override;
  end;

  TPSCStringListAdapter=class(TPSCStringsAdapter,IPSCSaveToFile)
  private
    function Find(const S: string; var Index: Integer): Boolean;override;
    function GetDuplicates: TPSCDuplicates;override;
    function GetSorted:Boolean;override;

    procedure SetDuplicates(AValue:TPSCDuplicates);override;
    procedure SetSorted(AValue:Boolean);override;
    procedure Sort;override;
  end;

{------------------------------}

function PSCCreateStringsAdapter(AStrings:TStrings;
  AStringsOwned:TPSCItemOwnerShip=ioReferenced;
  AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
begin
  Result:=TPSCStringsAdapter.Create(AStrings,AStringsOwned,AObjectsOwned);
end;

{------------------------------}

function PSCCreateStringListAdapter(AStrings:TStringList;
  AStringsOwned:TPSCItemOwnerShip=ioReferenced;
  AObjectsOwned:TPSCItemOwnership=ioReferenced):IPSCStrings;
begin
  Result:=TPSCStringListAdapter.Create(AStrings,AStringsOwned,AObjectsOwned);
end;

{------------------------------}

function TPSCStringListAdapter.Find(const S: string; var Index: Integer): Boolean;
begin
  Result:=TStringList(FStrings).Find(S,Index);
end;

{------------------------------}

function TPSCStringListAdapter.GetDuplicates: TPSCDuplicates;
begin
  Result:=TPSCDuplicates(TStringList(FStrings).Duplicates);
end;

{------------------------------}

function TPSCStringListAdapter.GetSorted:Boolean;
begin
  Result:=TStringList(FStrings).Sorted;
end;

{------------------------------}

procedure TPSCStringListAdapter.SetDuplicates(AValue:TPSCDuplicates);
begin
  TStringList(FStrings).Duplicates:=TDuplicates(AValue);
end;

{------------------------------}

procedure TPSCStringListAdapter.SetSorted(AValue:Boolean);
begin
  TStringList(FStrings).Sorted:=AValue;
end;

{------------------------------}

procedure TPSCStringListAdapter.Sort;
begin
  TStringList(FStrings).Sort;
end;

{------------------------------}

function TPSCStringsAdapter.GetInstanceID:Integer;
begin
  Result:=Integer(FStrings);
end;

{------------------------------}

function TPSCStringsAdapter.Add(const S: string): Integer;
begin
  Result:=FStrings.Add(S);
end;

{------------------------------}

function TPSCStringsAdapter.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result:=FStrings.AddObject(S,AObject);
end;

{------------------------------}

function TPSCStringsAdapter.IndexOf(const S: string): Integer;
begin
  Result:=FStrings.IndexOf(S);
end;

{------------------------------}

function TPSCStringsAdapter.IndexOfName(const Name: string): Integer;
begin
  Result:=FStrings.IndexOfName(Name);
end;

{------------------------------}

function TPSCStringsAdapter.IndexOfObject(AObject: TObject): Integer;
begin
  Result:=FStrings.IndexOfObject(AObject);
end;

{------------------------------}

procedure TPSCStringsAdapter.BeginUpdate;
begin
  FStrings.BeginUpdate;
end;

{------------------------------}

procedure TPSCStringsAdapter.Clear;
begin
  BeginUpdate;
  While GetCount>0 do
    Delete(GetCount-1);
  EndUpdate;  
end;

{------------------------------}

procedure TPSCStringsAdapter.Delete(Index: Integer);
var
  MyObject:TObject;
begin
  If FObjectsOwned=ioOwned then
    MyObject:=FStrings.Objects[Index]
  else
    MyObject:=nil;
  FStrings.Delete(Index);
  If MyObject<>nil then
    MyObject.Free;
end;

{------------------------------}

procedure TPSCStringsAdapter.EndUpdate;
begin
  FStrings.EndUpdate;
end;

{------------------------------}

procedure TPSCStringsAdapter.LoadFromFile(const AFileName:String);
begin
  FStrings.LoadFromFile(AFileName);
end;

{------------------------------}

procedure TPSCStringsAdapter.SaveToFile(const AFileName:String);
begin
  FStrings.SaveToFile(AFileName);
end;

{------------------------------}

procedure TPSCStringsAdapter.AssignStringsTo(const AStrings:IPSCStrings);
begin
  AStrings.Assign(Self);
end;

{------------------------------}

procedure TPSCStringsAdapter.AssignStrings(const AStrings:IPSCStrings);
var
  MyInstanceID:IPSCInstanceID;
  ID1,ID2:Integer;
begin
  If PSCSupports(AStrings,IPSCInstanceID,MyInstanceID) then
  begin
    ID1:=MyInstanceID.GetInstanceID;
    ID2:=GetInstanceID;
    If ID1=ID2 then
      exit;
  end;

  FStrings.Clear;
  AddStrings(AStrings);
end;

{------------------------------}

procedure TPSCStringsAdapter.AddStrings(const AStrings:IPSCStrings);
var
  i:Integer;
begin
  for i:=0 to AStrings.Count-1 do
    AddObject(AStrings[i],AStrings.Objects[i]);
end;

{------------------------------}

function TPSCStringsAdapter.GetCapacity:Integer;
begin
  Result:=FStrings.Capacity;
end;

{------------------------------}

function TPSCStringsAdapter.GetCount:Integer;
begin
  Result:=FStrings.Count;
end;

{------------------------------}

function TPSCStringsAdapter.GetCommaText:String;
begin
  Result:=FStrings.CommaText;
end;

{------------------------------}

function TPSCStringsAdapter.GetName(AIndex: Integer):String;
begin
  Result:=FStrings.Names[AIndex];
end;

{------------------------------}

function TPSCStringsAdapter.GetObject(AIndex: Integer):TObject;
begin
  Result:=FStrings.Objects[AIndex];
end;

{------------------------------}

function TPSCStringsAdapter.GetValue(const AName: string): string;
begin
  Result:=FStrings.Values[AName];
end;

{------------------------------}

function TPSCStringsAdapter.GetString(AIndex: Integer): string;
begin
  Result:=FStrings.Strings[AIndex];
end;

{------------------------------}

function TPSCStringsAdapter.GetTextStr:String;
begin
  Result:=FStrings.Text;
end;

{------------------------------}

procedure TPSCStringsAdapter.SetTextStr(const AValue:String);
begin
  FStrings.Text:=AValue;
end;

{------------------------------}

procedure TPSCStringsAdapter.PutString(AIndex: Integer;const AValue: string);
begin
  FStrings.Strings[AIndex]:=AValue;
end;

{------------------------------}

procedure TPSCStringsAdapter.SetValue(const AName,AValue: string);
begin
  FStrings.Values[AName]:=AValue;
end;

{------------------------------}

procedure TPSCStringsAdapter.PutObject(AIndex: Integer;AObject:TObject);
begin
  FStrings.Objects[AIndex]:=AObject;
end;

{------------------------------}

procedure TPSCStringsAdapter.SetCount(V:Integer);
begin
  If (V=GetCount) or (V<0) then
    exit;
  While V<GetCount do
    Delete(GetCount-1);
  While V>GetCount do
    Add('');
end;

{------------------------------}

procedure TPSCStringsAdapter.SetCapacity(AValue:Integer);
begin
  FStrings.Capacity:=AValue;
end;

{------------------------------}

function TPSCStringsAdapter.Find(const S: string; var Index: Integer): Boolean;
begin
  Index:=IndexOf(S);
  Result:=Index>=0;
end;

{------------------------------}

function TPSCStringsAdapter.GetDuplicates: TPSCDuplicates;
begin
  Result:=DUP_Ignore;
end;

{------------------------------}

function TPSCStringsAdapter.GetSorted:Boolean;
begin
  Result:=False;
end;

{------------------------------}

procedure TPSCStringsAdapter.SetDuplicates(AValue:TPSCDuplicates);
begin
end;

{------------------------------}

procedure TPSCStringsAdapter.SetSorted(AValue:Boolean);
begin
end;

{------------------------------}

procedure TPSCStringsAdapter.Sort;
begin
end;

{------------------------------}

constructor TPSCStringsAdapter.Create(AStrings:TStrings;
  AStringsOwned,AObjectsOwned:TPSCItemOwnerShip);
begin
  inherited Create(nil);
  FStrings:=AStrings;
  FStringsOwned:=AStringsOwned;
  FObjectsOwned:=AObjectsOwned;
end;

{------------------------------}

destructor TPSCStringsAdapter.Destroy;
begin
  If FObjectsOwned=ioOwned then
    Clear;
  If FStringsOwned=ioOwned then
    FStrings.Free;
  inherited;
end;

{-------------------------------------------------------------------------}

function MyExtractFilePaths(const S:IPSCStrings;UserData:Cardinal):boolean;
var
  i:Integer;
begin
  Result:=True;
  With S do
    for i:=0 to Count-1 do
      Strings[i]:=ExtractFilePath(Strings[i]);
end;

procedure PSCExtractFilePaths(const Source,Dest:IPSCStrings);
begin
  If Dest<>Source then
    Dest.Assign(Source);
  PSCOperateStrings(Dest,MyExtractFilePaths,0);
  PSCSortAndRemoveDups(Dest);
end;

{--------------------------------------------------------------------}

var
  FCountryIDToName:IPSCObjectList;

{--------------------------------------------------------------------}

Function PSCCountryIDToName(ACountryID: TPSCCountryID): String;
var
  MyTemp:TPSCStringAndID;
  MyIndex:Integer;
Begin
  If FCountryIDToName=nil then
  begin
    FCountryIDToName:=PSCCreateObjectList(ioOwned);
    FCountryIDToName.SetSortCriteria(TPSCStringAndIDCompare.Create);
  end;

  MyTemp:=TPSCStringAndID.Create;
  MyTemp.ID:=ACountryID;
  If FCountryIDToName.Find(MyTemp,MyIndex) then
    begin
      MyTemp.Free;
      Result:=TPSCStringAndID(FCountryIDToName.Items[MyIndex]).S;
      exit;
    end;

  Result := PSCGetLocaleStr(ACountryID,LOCALE_SCOUNTRY,'');
  Mytemp.S:=Result;
  FCountryIDToName.Add(MyTemp);
End;

{-------------------------------------------------------------}

type
   TPSCRSDDefineType=(dtIFDEF,dtIFNDEF,dtELSE,dtENDIF,dtOther);

   TPSCRSDItem=class(TCollectionItem)
   public
     DefineType:TPSCRSDDefineType;
     DefinePos:Integer;
     DefineLength:Integer;
     DefineIdent:String;
     IfDefLevel:Integer;
     EndIfIndex:Integer;
     ElseIndex:Integer;
   end;

   TPSCRSDItems=class(TCollection)
   private
     FDefines:String;
     FRemoveDefines:String;
     procedure UpdateEndIfIndex;
     procedure SetDefines(const V:String);
     procedure SetRemoveDefines(const V:String);
     procedure FillWithItems(const InputStr:String);
     function AddItem(ADefineType:TPSCRSDDefineType;
       ADefinePos,ADefineLength:Integer;ADefineIdent:String;
       AIfDefLevel:Integer):TPSCRSDItem;
   public
     function IsDefined(const Define:String):boolean;
     function IsOtherDefine(const Define:String):boolean;
     function RemoveSomeDefines(const InputStr:String):String;
     property RemoveDefines:String Read FRemoveDefines Write SetRemoveDefines;
     property Defines:String Read FDefines Write SetDefines;
   end;

{-------------------------------------------------------------}

procedure TPSCRSDItems.SetDefines(const V:String);
begin
  FDefines:=PSCRemoveCharSet([' '],';'+PSCUpperCase(V)+';');
end;

{-------------------------------------------------------------}

procedure TPSCRSDItems.SetRemoveDefines(const V:String);
begin
  FRemoveDefines:=PSCRemoveCharSet([' '],';'+PSCUpperCase(V)+';');
end;

{-------------------------------------------------------------}

procedure TPSCRSDItems.UpdateEndIfIndex;
var
  i,j:Integer;
  Item,Item1:TPSCRSDItem;
begin
  for i:=0 to Count-1 do
  begin
    Item:=TPSCRSDItem(Items[i]);
    If Item.DefineType in [dtIfdef,dtIfndef] then
    begin
      j:=i+1;
      While (j<Count) and (TPSCRSDItem(Items[j]).IfDefLevel>=Item.IfDefLevel) do
      begin
        Item1:=TPSCRSDItem(Items[j]);
        If Item1.IfDefLevel=Item.IfDefLevel then
        begin
          If Item1.DefineType=dtElse then
            Item.ElseIndex:=j
          else
            If Item1.DefineType=dtEndIf then
              Item.EndIfIndex:=j;
        end;
        inc(j);

        If Item.EndIfIndex<>-1 then
          break;
      end;
    end;
  end;

end;

{-------------------------------------------------------------}

function TPSCRSDItems.RemoveSomeDefines(const InputStr:String):String;
var
  i:Integer;
  Item,ElseItem,EndIfItem:TPSCRSDItem;
  RemoveText:boolean;
  FromItem,ToItem:TPSCRSDItem;
begin
  FillWithItems(InputStr);
  UpdateEndIfIndex;

  Result:=InputStr;

  for i:=0 to Count-1 do
  begin
    Item:=TPSCRSDItem(Items[i]);
    If (Item.DefineType in [dtIFDEF,dtIFNDEF]) and not IsOtherDefine(Item.DefineIdent) then
      With Item do
      begin
        If ElseIndex>=0 then
          ElseItem:=TPSCRSDItem(Items[ElseIndex])
        else
          ElseItem:=nil;

        If EndIfIndex>=0 then
          EndIFItem:=TPSCRSDItem(Items[EndIfIndex])
        else
          EndIfItem:=nil;

        If (EndIfItem=nil) then
          PSCError('ENDIF required');

        Result:=PSCSetStrCharsTo(Result,DefinePos,DefineLength,#1); {Mask IFDEF}
        If ElseItem<>nil then
          Result:=PSCSetStrCharsTo(Result,ElseItem.DefinePos,ElseItem.DefineLength,#1); {Mask ELSE}
        If EndIfItem<>nil then
          Result:=PSCSetStrCharsTo(Result,EndIfItem.DefinePos,EndifItem.DefineLength,#1); {Mask ENDIF}

        If DefineType=dtIFDEF then
          RemoveText:=not IsDefined(DefineIdent)
        else
          RemoveText:=IsDefined(DefineIdent);

        If ElseItem=nil then
          begin
            If RemoveText then
              begin
                FromItem:=Item;
                ToItem:=EndIfItem;
              end
            else
              begin
                FromItem:=nil;
                ToItem:=nil;
              end;
          end
        else
          begin
            If RemoveText then
              begin
                FromItem:=Item;
                ToItem:=ElseItem;
              end
            else
              begin
                FromItem:=ElseItem;
                ToItem:=EndIfItem;
              end;
          end;

        If FromItem<>nil then
          PSCSetStrCharsTo(Result,FromItem.DefinePos+FromItem.DefineLength,
            ToItem.DefinePos-FromItem.DefinePos-FromItem.DefineLength+1,#1);
      end;
  end;

  Result:=PSCRemoveCharSet([#1],Result);
end;

{-------------------------------------------------------------}

function TPSCRSDItems.IsOtherDefine(const Define:String):boolean;
begin
  Result:=Pos(PSCRemoveCharSet([' '],';'+PSCUpperCase(Define)+';'),
    FRemoveDefines)=0;
end;

{-------------------------------------------------------------}

function TPSCRSDItems.IsDefined(const Define:String):boolean;
begin
  Result:=Pos(PSCRemoveCharSet([' '],';'+PSCUpperCase(Define)+';'),
    FDefines)<>0;
end;

{-------------------------------------------------------------}

function TPSCRSDItems.AddItem(ADefineType:TPSCRSDDefineType;
  ADefinePos,ADefineLength:Integer;ADefineIdent:String;AIfDefLevel:Integer):TPSCRSDItem;
begin
   Result:=TPSCRSDItem(Add);
   With Result do
   begin
     DefineType:=ADefineType;
     DefinePos:=ADefinePos;
     DefineLength:=ADefineLength;
     DefineIdent:=ADefineIdent;
     IfDefLevel:=AIfDefLevel;
     EndIfIndex:=-1;
     ElseIndex:=-1;
   end;
end;

{-------------------------------------------------------------}

function DefineKeyWordToDefineType(const DefineKeyWord:String):TPSCRSDDefineType;
begin
  If PSCCompareText(DefineKeyword,'IFDEF')=0 then
    Result:=dtIFDEF
  else
    If PSCCompareText(DefineKeyword,'IFNDEF')=0 then
      Result:=dtIFNDEF
    else
      If PSCCompareText(DefineKeyword,'ELSE')=0 then
        Result:=dtELSE
      else
        If PSCCompareText(DefineKeyword,'ENDIF')=0 then
          Result:=dtENDIF
        else
          Result:=dtOther;
end;

{-------------------------------------------------------------}

procedure TPSCRSDItems.FillWithItems(const InputStr:String);
var
  FoundPos,CurPos:Integer;
  WorkStr:String;
  EndCommentPos:Integer;
  DefineKeyWord,DefineStr:String;
  DefineType:TPSCRSDDefineType;
  AIfDefLevel:Integer;
begin

  Clear;

  WorkStr:=PSCUpperCase(InputStr);
  CurPos:=1;

  FoundPos:=PSCPosEx('{$',WorkStr,CurPos);

  AIfDefLevel:=0;

  While FoundPos<>0 do
  begin
    EndCommentPos:=PSCPosEx('}',WorkStr,FoundPos);

    If EndCommentPos=0 then
      PSCError('Comment is not correctly declared');//don't resource

    DefineStr:=PSCRemoveExtraChars(
      PSCTrim(Copy(WorkStr,FoundPos+2,EndCommentPos-FoundPos-2)),' ');

    PSCSeparateStrEx(DefineStr,' ',DefineKeyWord,DefineStr);

    DefineType:=DefineKeyWordToDefineType(DefineKeyWord);

    If DefineType<>dtOther then
    begin
      If DefineType in [dtIfdef,dtIfNdef] then
        inc(AIfDefLevel);

      AddItem(DefineType,FoundPos,EndCommentPos-FoundPos+1,DefineStr,AIfDefLevel);

      If DefineType=dtEndIf then
        dec(AIfDefLevel);
    end;

    CurPos:=EndCommentPos+1;

    FoundPos:=PSCPosEx('{$',WorkStr,CurPos);
  end;
end;

{-------------------------------------------------------------}

function PSCRemoveSomeDefines(const InputStr,RemoveDefines,Defines:String):String;
Var
  DefineItems:TPSCRSDItems;
begin
  DefineItems:=TPSCRSDItems.Create(TPSCRSDItem);
  try
    DefineItems.RemoveDefines:=RemoveDefines;
    DefineItems.Defines:=Defines;
    Result:=DefineItems.RemoveSomeDefines(InputStr);
  finally
    DefineItems.Free;
  end;
end;

{-------------------------------------------------------------}

procedure PSCRemoveSomeDefinesInFiles(const Files:IPSCStrings;
  const RemoveDefines,Defines:String);
var
  i:Integer;
begin
  for i:=0 to Files.Count-1 do
    PSCRemoveSomeDefinesInFile(Files[i],RemoveDefines,Defines);
end;

{--------------------------------------------------------------------}

Var
  CountriesDesc: IPSCStrings = Nil;

{--------------------------------------------------------------------}

Procedure PSCGetCountryIDs(const ACountryList: IPSCStrings);
Var
  I: Integer;
Begin
  If ACountryList = Nil Then
    exit;
  ACountryList.Clear;
  For I := Low(cPSCCountryArray) To High(cPSCCountryArray) Do
    ACountryList.AddObject(PSCCountryIDToName(cPSCCountryArray[I]),
      TPSCValuesContainer.Create(cPSCCountryArray[I]));
End;

{--------------------------------------------------------------------}

Procedure PSCInitCountriesList(ReInit: boolean);
Begin
  If Assigned(CountriesDesc) And (Not ReInit) Then
    Exit;
  CountriesDesc := PSCCreateStringList(ioOwned);
  PSCGetCountryIDs(CountriesDesc);
  CountriesDesc.Sorted := True;
End;

{--------------------------------------------------------------------}

Function PSCCountryNameToID(Const ACountryName: String): TPSCCountryID;
Var
  I: Integer;
Begin
  PSCInitCountriesList(False);
  I := CountriesDesc.IndexOf(ACountryName);
  If I <> -1 Then
    Result := TPSCValuesContainer(CountriesDesc.Objects[I]).IntValue
  else
    Result := -1;
End;

{-------------------------------------------}

Function PSCDBFieldTypeToPSC(DataType: TFieldType): TPSCFieldType;
Begin
  Case DataType Of
    ftUnknown:
      Result := FT_UNK;
    ftString,ftMemo,ftFmtMemo,ftFixedChar,ftWideString:
      Result := FT_STRING;
    ftSmallint,ftInteger,ftWord,ftAutoInc,ftLargeint:
      Result := FT_INT;
    ftBoolean:
      Result := FT_BOOL;
    ftCurrency,ftBCD:
      Result := FT_CURRENCY;
    ftFloat:
      Result := FT_FLOAT;
    ftTime:
      Result := FT_TIME;
    ftDate:
      Result := FT_DATE;
    ftDateTime{$IFDEF D6},ftTimeStamp{$ENDIF}:
      Result := FT_DATETIME;
    {$IFDEF D2009}
    ftFixedWideChar,ftWideMemo:
      Result := FT_STRING;
    ftOraTimeStamp,ftTimeStampOffset:
      Result := FT_DATETIME;
    ftOraInterval:
      Result := FT_TIME;
    ftLongWord,ftShortint,ftByte:
      Result := FT_INT;
    ftExtended,ftSingle:
      Result := FT_FLOAT;
    {$ENDIF}
  Else
    Result := FT_UNK;
  End;
End;

{------------------------------------------------------------------}

type
  TPSCExternalProcs=class(TInterfacedObject,IPSCExternalProcs)
  private
    function UpperCase(const S:String):String;
    function LowerCase(const S:String):String;
    function Now:TDateTime;

    function DayOfWeek(ADate: TDateTime): Integer;
    function IsLeapYear(Year: Word): Boolean;
    function IntToStr(AValue: Integer): string;
    function EncodeDate(Year, Month, Day: Word): TDateTime;
    function StrToFloat(const S: string): Extended;
    function Format(const AFormat: string; const AArgs: array of const): string;
    function FloatToStr(AValue:Extended):String;
    function StrToIntDef(const S: string; Default: Integer): Integer;
    function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
    function IntToHex(Value: Integer; Digits: Integer): string;
    function StrToInt(const S: string): Integer;
    function IncMonth(const ADate: TDateTime;
      ANumberOfMonths: Integer = 1): TDateTime;
    procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
    procedure ShowMessage(const S:String);
    procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
    procedure Beep;
    function ColorToString(AColor:TPSCColor):String;
    function StringToColor(const S:String):TPSCColor;
  end;

{------------------------------------------------------------------}

function TPSCExternalProcs.ColorToString(AColor:TPSCColor):String;
begin
  Result:=vcl.Graphics.ColorToString(AColor);
end;

{------------------------------------------------------------------}

function TPSCExternalProcs.StringToColor(const S:String):TPSCColor;
begin
  Result:=vcl.Graphics.StringToColor(S);
end;

{------------------------------------------------------------------}

function TPSCExternalProcs.Now:TDateTime;
begin
  Result:=System.SysUtils.Now;
end;

{------------------------------------------------------------------}

function TPSCExternalProcs.UpperCase(const S:String):String;
begin
  Result:=System.SysUtils.AnsiUpperCase(S);
  //Result:=System.SysUtils.UpperCase(S);
end;

{------------------------------------------------------------------}

function TPSCExternalProcs.LowerCase(const S:String):String;
begin
  Result:=System.SysUtils.AnsiLowerCase(S);
//  Result:=System.SysUtils.LowerCase(S);
end;

{-------------------------------------------------------------------------}

function TPSCExternalProcs.StrToIntDef(const S: string; Default: Integer): Integer;
begin
  Result:=System.SysUtils.StrToIntDef(S,Default);
end;

{------------------------------}

procedure TPSCExternalProcs.DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
  System.SysUtils.DecodeDate(Date,Year, Month, Day);
end;

{------------------------------}

function TPSCExternalProcs.IntToStr(AValue: Integer): string;
begin
  Result:=System.SysUtils.IntToStr(AValue);
end;

{------------------------------}

function TPSCExternalProcs.EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  Result:=System.SysUtils.EncodeDate(Year, Month, Day);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.StrToFloat(const S: string): Extended;
begin
  Result:=System.SysUtils.StrToFloat(S);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.Format(const AFormat: string; const AArgs: array of const): string;
begin
  Result:=System.SysUtils.Format(AFormat,AArgs);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.FloatToStr(AValue:Extended):String;
begin
  Result:=System.SysUtils.FloatToStr(AValue);
end;

{------------------------------------------------------------------}

procedure TPSCExternalProcs.Beep;
begin
  System.SysUtils.Beep;
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.IsLeapYear(Year: Word): Boolean;
begin
  Result:=System.SysUtils.IsLeapYear(Year);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.DayOfWeek(ADate: TDateTime): Integer;
begin
  Result:=System.SysUtils.DayOfWeek(ADate);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  Result:=System.SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

{-------------------------------------------------------------}

procedure TPSCExternalProcs.DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
  System.SysUtils.DecodeTime(Time,Hour, Min, Sec, MSec);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.IntToHex(Value: Integer; Digits: Integer): string;
begin
  Result:=System.SysUtils.IntToHex(Value,Digits);
end;

{-------------------------------------------------------------}

function TPSCExternalProcs.StrToInt(const S: string): Integer;
begin
  Result:=System.SysUtils.StrToInt(S);
end;

{-------------------------------------------------------------}

procedure _IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
var
  DayTable: PDayTable;
  Sign: Integer;
begin
  if NumberOfMonths >= 0 then Sign := 1 else Sign := -1;
  Year := Year + (NumberOfMonths div 12);
  NumberOfMonths := NumberOfMonths mod 12;
  Inc(Month, NumberOfMonths);
  if Word(Month-1) > 11 then    // if Month <= 0, word(Month-1) > 11)
  begin
    Inc(Year, Sign);
    Inc(Month, -12 * Sign);
  end;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if Day > DayTable^[Month] then Day := DayTable^[Month];
end;

function _IncMonth(const DateTime: TDateTime; NumberOfMonths: Integer): TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  _IncAMonth(Year, Month, Day, NumberOfMonths);
  if not TryEncodeDate(Year, Month, Day, Result) then
    exit(DateTime);

  Result := EncodeDate(Year, Month, Day);
  ReplaceTime(Result, DateTime);
end;


function TPSCExternalProcs.IncMonth(const ADate: TDateTime;
  ANumberOfMonths: Integer = 1): TDateTime;
begin
  try
    Result:=_IncMonth(ADate,ANumberOfMonths);
  except
    Result:=ADate;
  end;
end;

{------------------------------}

procedure TPSCExternalProcs.ShowMessage(const S:String);
begin
  vcl.Dialogs.ShowMessage(S);
end;

{------------------------------------------------------------------}

type
  TPoints = array of TPoint;

  IPSCVCLBrush=interface
    ['{F628E0EF-5E4B-4182-9A7C-3C8C9FA7C90B}']
    function GetVCLBrush:TBrush;
  end;

  IPSCVCLFont=interface
    ['{1AC04CAB-77A4-45E1-BCBB-952AC94A77C8}']
    function GetVCLFont:TFont;
  end;

  IPSCVCLPen=interface
    ['{4D2EBDC9-85DC-4ADD-9FBE-932F89863A18}']
    function GetVCLPen:TPen;
  end;

  IPSCVCLCanvas=interface
    ['{602BDC50-B99A-46E4-ACA3-1F9109D646F3}']
    function GetCanvas:TCanvas;
  end;

  TPSCBrushAdapter = class(TInterfacedObject,IPSCBrush,IPSCWin32Handle,IPSCVCLBrush)
  private
    FGetBrush: TPSCGetBrush;

    function GetColor:TPSCColor;
    function GetStyle:TPSCBrushStyle;
    function GetWin32Handle:THandle;
    function GetVCLBrush:TBrush;

    procedure Assign(const ABrush:IPSCBrush);
    procedure SetColor(AValue:TPSCColor);
    procedure SetStyle(AValue:TPSCBrushStyle);
  public
    constructor Create(ABrush : TPSCGetBrush);
  end;

  TPSCPenAdapter = class(TInterfacedObject,IPSCPen,IPSCWin32Handle,IPSCVCLPen)
  private
    FGetPen: TPSCGetPen;

    function GetVCLPen:TPen;
    function GetColor: TPSCColor;
    function GetStyle: TPSCPenStyle;
    function GetMode: TPSCPenMode;
    function GetWidth: Integer;
    function GetWin32Handle:THandle;

    procedure SetStyle(Value: TPSCPenStyle);
    procedure SetWidth(Value: Integer);
    procedure SetColor(Value: TPSCColor);
    procedure SetMode(Value: TPSCPenMode);
    procedure Assign(const APen:IPSCPen);
  public
    constructor Create(APen : TPSCGetPen);
  end;

  TPSCFontAdapter = class(TInterfacedObject,IPSCFont,IPSCWin32Handle,IPSCVCLFont)
  private
    FGetFont: TPSCGetFont;

    function GetColor: TPSCColor;
    function GetName: string;
    function GetSize: Integer;
    function GetStyle: TPSCFontStyles;
    function GetWin32Handle:THandle;
    function GetVCLFont:TFont;

    procedure SetColor(Value: TPSCColor);
    procedure SetName(const Value: string);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TPSCFontStyles);
    procedure Assign(const AFont:IPSCFont);
  public
    constructor Create(AFont : TPSCGetFont);
  end;

  TPSCCanvasAdapter = class(TInterfacedObject,IPSCCanvas,IPSCWin32Handle,IPSCVCLCanvas)
  private
    FBrush : IPSCBrush;
    FFont : IPSCFont;
    FPen : IPSCPen;
    FGetCanvas : TPSCGetCanvas;

    function GetCanvas:TCanvas;
    function GetWin32Handle:THandle;
    function GetVCLBrush : TBrush;
    function GetVCLPen : TPen;
    function GetVCLFont : TFont;
    function TextExtent(const AText: WideString): TPSCSize;
    function TextHeight(const AText: WideString): Integer;
    function TextWidth(const AText: WideString): Integer;
    function GetBrush: IPSCBrush;
    function GetFont: IPSCFont;
    function GetPen: IPSCPen;
    function GetTextFlags : integer;

    Procedure ExcludeClipRect(X1,Y1,X2,Y2: Integer);

    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure DrawFocusRect(const ARect: TPSCRect);
    procedure Ellipse(const ARect: TPSCRect);
    procedure FillRect(const ARect: TPSCRect);
    procedure FrameRect(const ARect: TPSCRect);
    procedure LineTo(X, Y: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure Polygon(const APoints: array of TPSCPoint);
    procedure Polyline(const APoints: array of TPSCPoint);
    procedure PolyBezier(const APoints: array of TPSCPoint);
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure Rectangle(const ARect: TPSCRect); overload;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure TextOut(X, Y: Integer; const AText: WideString);
    procedure TextRect(const ARect: TPSCRect; X, Y: Integer; const AText: WideString);
    procedure SetTextFlags(AValue : integer);
    procedure SetBrush(const AValue:IPSCBrush);
    procedure SetPen(const AValue:IPSCPen);
    procedure SetFont(const AValue:IPSCFont);
  public
    constructor Create(ACanvas : TPSCGetCanvas);
  end;

  TPSCCanvasAdapter2=class(TPSCCanvasAdapter)
  private
    FCanvas:TCanvas;

    function GetVCLCanvas:TCanvas;
  public
    constructor Create(ACanvas : TCanvas);
  end;

{-------------------------------------------------------------------------}

function TPSCCanvasAdapter2.GetVCLCanvas:TCanvas;
begin
  Result:=FCanvas;
end;

{-------------------------------------------------------------------------}

constructor TPSCCanvasAdapter2.Create(ACanvas : TCanvas);
begin
  FCanvas:=ACanvas;
  inherited Create(GetVCLCanvas);
end;

{-------------------------------------------------------------------------}

function PSCGetVCLCanvas(const ACanvas:IPSCCanvas):TCanvas;
var
  MyCanvas:IPSCVCLCanvas;
begin
  If PSCSupports(ACanvas,IPSCVCLCanvas,MyCanvas) then
    Result:=MyCanvas.GetCanvas
  else
    Result:=nil;
end;

{------------------------------------------------------------------------------}

function TPSCBrushAdapter.GetWin32Handle:THandle;
begin
  Result:=FGetBrush.Handle;
end;

{------------------------------------------------------------------------------}

procedure TPSCBrushAdapter.Assign(const ABrush:IPSCBrush);
var
  FVCLBrush:IPSCVCLBrush;
begin
  If PSCSupports(ABrush,IPSCVCLBrush,FVCLBrush) then
    FGetBrush.Assign(FVCLBrush.GetVCLBrush);
end;

{------------------------------------------------------------------------------}

procedure TPSCPenAdapter.Assign(const APen:IPSCPen);
var
  FVCLPen:IPSCVCLPen;
begin
  If PSCSupports(APen,IPSCVCLPen,FVCLPen) then
    FGetPen.Assign(FVCLPen.GetVCLPen);
end;

{------------------------------------------------------------------------------}

procedure TPSCFontAdapter.Assign(const AFont:IPSCFont);
var
  FVCLFont:IPSCVCLFont;
begin
  If PSCSupports(AFont,IPSCVCLFont,FVCLFont) then
    FGetFont.Assign(FVCLFont.GetVCLFont);
end;

{------------------------------------------------------------------------------}

function TPSCBrushAdapter.GetVCLBrush:TBrush;
begin
  Result:=FGetBrush;
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetVCLPen:TPen;
begin
  Result:=FGetPen;
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetWin32Handle:THandle;
begin
  Result:=FGetPen.Handle;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetWin32Handle:THandle;
begin
  Result:=FGetFont.Handle;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetVCLFont:TFont;
begin
  Result:=FGetFont;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetCanvas:TCanvas;
begin
  Result:=FGetCanvas;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetWin32Handle:THandle;
begin
  Result:=FGetCanvas.Handle;
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.SetBrush(const AValue:IPSCBrush);
begin
  FBrush.Assign(AValue);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.SetPen(const AValue:IPSCPen);
begin
  FPen.Assign(AValue);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.SetFont(const AValue:IPSCFont);
begin
  FFont.Assign(AValue);
end;

{------------------------------------------------------------------------------}

function PSCCreateCanvasAdapter(ACanvas:TPSCGetCanvas):IPSCCanvas;
begin
  result := TPSCCanvasAdapter.Create(ACanvas);
end;

{------------------------------------------------------------------------------}

function PSCCreateCanvasAdapter(ACanvas:TCanvas):IPSCCanvas;
begin
  result := TPSCCanvasAdapter2.Create(ACanvas);
end;

{------------------------------------------------------------------------------}

function PSCCreateFontAdapter(AGetFont:TPSCGetFont):IPSCFont;
begin
  Result:=TPSCFontAdapter.Create(AGetFont);
end;

{------------------------------------------------------------------------------}

function PSCCreateBrushAdapter(AGetBrush:TPSCGetBrush):IPSCBrush;
begin
  Result:=TPSCBrushAdapter.Create(AGetBrush);
end;

{------------------------------------------------------------------------------}

function PSCCreatePenAdapter(AGetPen:TPSCGetPen):IPSCPen;
begin
  Result:=TPSCPenAdapter.Create(AGetPen);
end;

{------------------------------------------------------------------------------}

function PSCPointsArrayAdapter(const Points: array of TPSCPoint) : TPoints;
var
  i : integer;
begin
  SetLength(result, High(Points) + 1);
  for i := Low(Points) to High(Points) do
    result[i] := TPoint(Points[i]);
end;

{------------------------------------------------------------------}

Procedure TPSCCanvasAdapter.ExcludeClipRect(X1,Y1,X2,Y2: Integer);
Begin
  Winapi.Windows.ExcludeClipRect(FGetCanvas.Handle,X1,Y1,X2,Y2);
End;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FGetCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FGetCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, X4);
end;

{------------------------------------------------------------------------------}

constructor TPSCCanvasAdapter.Create(ACanvas : TPSCGetCanvas);
begin
  inherited Create;
  FGetCanvas := ACanvas;
  FFont := TPSCFontAdapter.Create(GetVCLFont);
  FBrush := TPSCBrushAdapter.Create(GetVCLBrush);
  FPen := TPSCPenAdapter.Create(GetVCLPen);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.DrawFocusRect(const ARect: TPSCRect);
begin
  FGetCanvas.DrawFocusRect(TRect(ARect));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Ellipse(const ARect: TPSCRect);
begin
  FGetCanvas.Ellipse(TRect(ARect));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.FillRect(const ARect: TPSCRect);
begin
  FGetCanvas.FillRect(TRect(ARect));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.FrameRect(const ARect: TPSCRect);
begin
  FGetCanvas.FrameRect(TRect(ARect));
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetBrush: IPSCBrush;
begin
  result := FBrush;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetFont: IPSCFont;
begin
  result := FFont;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetPen: IPSCPen;
begin
  result := FPen;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetTextFlags: integer;
begin
  result := FGetCanvas.TextFlags;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetVCLBrush: TBrush;
begin
  result := FGetCanvas.Brush;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetVCLFont: TFont;
begin
  result := FGetCanvas.Font;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.GetVCLPen: TPen;
begin
  result := FGetCanvas.Pen;
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Line(X1, Y1, X2, Y2: Integer);
begin
  FGetCanvas.MoveTo(X1, Y1);
  FGetCanvas.LineTo(X2, Y2);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.LineTo(X, Y: Integer);
begin
  FGetCanvas.LineTo(X, Y);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.MoveTo(X, Y: Integer);
begin
  FGetCanvas.MoveTo(X, Y);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  FGetCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.PolyBezier(const APoints: array of TPSCPoint);
begin
  FGetCanvas.PolyBezier(PSCPointsArrayAdapter(APoints));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Polygon(const APoints: array of TPSCPoint);
begin
  FGetCanvas.Polygon(PSCPointsArrayAdapter(APoints));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Polyline(const APoints: array of TPSCPoint);
begin
  FGetCanvas.Polyline(PSCPointsArrayAdapter(APoints));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Rectangle(const ARect: TPSCRect);
begin
  FGetCanvas.Rectangle(TRect(ARect));
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  FGetCanvas.Rectangle(X1, Y1, X2, Y2);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  FGetCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.SetTextFlags(AValue: integer);
begin
  FGetCanvas.TextFlags := AValue;
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.TextExtent(const AText: WideString): TPSCSize;
begin
 result := TPSCSize(FGetCanvas.TextExtent(AText));
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.TextHeight(const AText: WideString): Integer;
begin
  result :=  FGetCanvas.TextHeight(AText);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.TextOut(X, Y: Integer; const AText: WideString);
begin
  FGetCanvas.TextOut(X, Y, AText);
end;

{------------------------------------------------------------------------------}

procedure TPSCCanvasAdapter.TextRect(const ARect: TPSCRect; X, Y: Integer;
  const AText: WideString);
begin
  FGetCanvas.TextRect(TRect(ARect), X, Y, AText);
end;

{------------------------------------------------------------------------------}

function TPSCCanvasAdapter.TextWidth(const AText: WideString): Integer;
begin
  result :=  FGetCanvas.TextWidth(AText);
end;

{------------------------------------------------------------------------------}

constructor TPSCBrushAdapter.Create(ABrush : TPSCGetBrush);
begin
  inherited Create;
  FGetBrush := ABrush;
end;

{------------------------------------------------------------------------------}

function TPSCBrushAdapter.GetColor: TPSCColor;
begin
  result := FGetBrush.Color;
end;

{------------------------------------------------------------------------------}

function TPSCBrushAdapter.GetStyle: TPSCBrushStyle;
begin
  result := TPSCBrushStyle(FGetBrush.Style);
end;

{------------------------------------------------------------------------------}

procedure TPSCBrushAdapter.SetColor(AValue: TPSCColor);
begin
  FGetBrush.Color := AValue;
end;

{------------------------------------------------------------------------------}

procedure TPSCBrushAdapter.SetStyle(AValue: TPSCBrushStyle);
begin
  FGetBrush.Style := TBrushStyle(AValue);
end;

{------------------------------------------------------------------------------}

constructor TPSCPenAdapter.Create(APen : TPSCGetPen);
begin
  FGetPen := APen;
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetColor: TPSCColor;
begin
  result := FGetPen.Color;
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetMode: TPSCPenMode;
begin
  result := TPSCPenMode(FGetPen.Mode);
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetStyle: TPSCPenStyle;
begin
  result := TPSCPenStyle(FGetPen.Style);
end;

{------------------------------------------------------------------------------}

function TPSCPenAdapter.GetWidth: Integer;
begin
  result := FGetPen.Width;
end;

{------------------------------------------------------------------------------}

procedure TPSCPenAdapter.SetColor(Value: TPSCColor);
begin
  FGetPen.Color := Value;
end;

{------------------------------------------------------------------------------}

procedure TPSCPenAdapter.SetMode(Value: TPSCPenMode);
begin
  FGetPen.Mode := TPenMode(Value);
end;

{------------------------------------------------------------------------------}

procedure TPSCPenAdapter.SetStyle(Value: TPSCPenStyle);
begin
  FGetPen.Style := TPenStyle(Value);
end;

{------------------------------------------------------------------------------}

procedure TPSCPenAdapter.SetWidth(Value: Integer);
begin
  FGetPen.Width := Value;
end;

{------------------------------------------------------------------------------}

constructor TPSCFontAdapter.Create(AFont : TPSCGetFont);
begin
  inherited Create;
  FGetFont := AFont;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetColor: TPSCColor;
begin
  result := FGetFont.Color;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetName: string;
begin
  result := FGetFont.Name;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetSize: Integer;
begin
  result := FGetFont.Size;
end;

{------------------------------------------------------------------------------}

function TPSCFontAdapter.GetStyle: TPSCFontStyles;
begin
  result := TPSCFontStyles(FGetFont.Style);
end;

{------------------------------------------------------------------------------}

procedure TPSCFontAdapter.SetColor(Value: TPSCColor);
begin
  FGetFont.Color := Value;
end;

{------------------------------------------------------------------------------}

procedure TPSCFontAdapter.SetName(const Value: string);
begin
  FGetFont.Name := Value;
end;

{------------------------------------------------------------------------------}

procedure TPSCFontAdapter.SetSize(Value: Integer);
begin
  FGetFont.Size := Value;
end;

{------------------------------------------------------------------------------}

procedure TPSCFontAdapter.SetStyle(Value: TPSCFontStyles);
begin
  FGetFont.Style := TFontStyles(Value);
end;

{-------------------------------------------------------------------------}

Function PSCPerformLikeCompareEx(Const Value: String;
  const PreparedMask: IPSCObjectList):Boolean;
Var
  NewPos: Integer;
  FOldPos: IPSCObjectList;

  Function CanMoveBackward: Boolean;
  Begin
    Result := Assigned(FOldPos) And (FOldPos.Count > 0);
  End;

  Function OldPos(Var AStep: Integer): Integer;
  Begin
    Result := -2;
    If (FOldPos<>nil) and (FOldPos.Count = 0) Then
      Exit;
    Result := TPSCValuesContainer(FOldPos[FOldPos.Count - 2]).IntValue;
    AStep := TPSCValuesContainer(FOldPos[FOldPos.Count - 1]).IntValue;
    FOldPos.Count := FOldPos.Count - 2;
  End;

  Procedure AddPos(AStep: Integer);
  Begin
    If Not Assigned(FOldPos) Then
      FOldPos := PSCCreateObjectList(ioOwned);
    FOldPos.Add(TPSCValuesContainer.Create(NewPos + 1));
    FOldPos.Add(TPSCValuesContainer.Create(AStep));
  End;

  Function LastWasAnyChars(AStep: Integer): Boolean;
  Begin
    Result := False;
    Dec(AStep);
    If AStep < 0 Then
      Exit;
    While AStep >= 0 Do
      Begin
        If TPSCPreparedMaskItem(PreparedMask[AStep]).ItemType = tok_like_AnyChars
          Then
          Begin
            Result := True;
            Exit;
          End;
        If TPSCPreparedMaskItem(PreparedMask[AStep]).ItemType <> tok_like_AnyChar
          Then
          Exit;
        Dec(AStep);
      End;
  End;

  Function IsRestAnyChars(APos: Integer): Boolean;
  Begin
    Result := False;
    For APos := APos To PreparedMask.Count - 1 Do
      If TPSCPreparedMaskItem(PreparedMask[APos]).ItemType <> tok_like_AnyChars
        Then
        Exit;
    Result := True;
  End;

Var
  I,Pos,ValueLength: Integer;
Begin
  Pos := 1;
  Result := False;
  ValueLength := Length(Value);
  I := 0;
  While I < PreparedMask.Count Do
    Begin
      If Pos > ValueLength Then
        Begin
          Result := IsRestAnyChars(I);
          Exit;
        End;
      Case TPSCPreparedMaskItem(PreparedMask[I]).ItemType Of
        tok_like_Str:
          Begin
            NewPos :=
              PSCPosEx(TPSCPreparedMaskItem(PreparedMask[I]).StrData,Value,Pos);
            If NewPos = 0 Then
              Exit;
            If LastWasAnyChars(I) Then
              AddPos(I - 1);
            If (Not LastWasAnyChars(I)) And (NewPos <> Pos) Then
              Begin
                If CanMoveBackward Then
                  Pos := OldPos(I)
                Else
                  Exit;
              End
            Else
              Pos := NewPos +
                Length(TPSCPreparedMaskItem(PreparedMask[I]).StrData);
          End;
        tok_like_AnyChar: Inc(Pos);
        tok_like_CharSet:
          Begin
            // If Not (Value[Pos] In TPSCPreparedMaskItem(PreparedMask[I]).CharSetData) Then
            If Not CharInSet(Value[Pos],TPSCPreparedMaskItem(PreparedMask[I]).CharSetData) Then
              If CanMoveBackward Then
                Pos := OldPos(I)
              Else
                Exit;
            Inc(Pos);
          End;
        tok_like_NotCharSet:
          Begin
            // If Value[Pos] In TPSCPreparedMaskItem(PreparedMask[I]).CharSetData
            If CharInSet(Value[Pos],TPSCPreparedMaskItem(PreparedMask[I]).CharSetData) Then
              If CanMoveBackward Then
                Pos := OldPos(I)
              Else
                Exit;
            Inc(Pos);
          End;
        tok_like_EscapeChar: ;
      End;
      Inc(I);
      If (I = PreparedMask.Count) And (Not LastWasAnyChars(I)) And
        (Pos <> ValueLength + 1) Then
        If CanMoveBackward Then
          Pos := OldPos(I)
        Else
          Exit;
    End;
  Result := True;
End;

{------------------------------------------------------------------}

initialization
  PSCSetExternalProcs(TPSCExternalProcs.Create);
  RegisterIntegerConsts(TypeInfo(TPSCColor), IdentToColor, ColorToIdent);
End.


