{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Property Inspector and                          }
{       Standard property editors Unit                  }
{                                                       }
{       (c) 2002, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELPropInsp;

interface

uses
    Classes, Controls, Grids, Graphics{$IFDEF VER140}, Types{$ENDIF}, Windows,
    Messages, Forms, StdCtrls, ELControls, SysUtils, Dialogs, TypInfo
    {$IFDEF VER140}, Variants{$ENDIF}, ComCtrls, Menus, ELSConsts
    {$IFDEF VER130}, ELD5_Adds{$ENDIF}, ELStringsEdit;

{
  ToDo ( TELCustomPropsPage ):

  ? Когда SetDisplayValue возбуждает исключительную ситуацию в некоторых случаях
    (когда фокус уходит из компонента и когда нажимаешь Enter)
    содержимое InplaceEditor сначала меняется, а потом уже показывается Exception,
    хотя надо наоборот (как в Delphi Object inspector). Хотя при переходе на другую
    строку все работает как надо.
  ? При Expand и Collapse съезжает текущая строка, если AdjustTopRow меняет верхнюю
    строку.
}

  { TELPropsPage }

type
    EELPropsPage = class(Exception);

    TELCustomPropsPage = class;
    TELPropsPageItems = class;

    TELPropsPageInplaceEdit = class(TInplaceEditList)
    private
        FChangingBounds: Boolean;
        FReadOnlyStyle: Boolean;
        procedure PickListMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
        procedure PickListDrawItem(Control: TWinControl; Index: Integer;
            Rect: TRect; State: TOwnerDrawState);
        procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    protected
        procedure CreateParams(var Params: TCreateParams); override;
        procedure DropDown; override;
        procedure UpdateContents; override;
        procedure KeyDown(var Key: Word; Shift: TShiftState); override;
        procedure DoEditButtonClick; override;
        procedure DoGetPickListItems; override;
        procedure CloseUp(Accept: Boolean); override;
        procedure DblClick; override;
        procedure BoundsChanged; override;
    public
        constructor Create(AOwner: TComponent); override;
        property ReadOnlyStyle: Boolean read FReadOnlyStyle;
    end;

    TELPropsPageItemExpandable = (mieAuto, mieYes, mieNo);

    TELPropsPageItem = class(TELObjectList)
    private
        FParent: TELPropsPageItem;
        FOwner: TELCustomPropsPage;
        FExpandable: TELPropsPageItemExpandable;
        FCaption: string;
        FExpanded: Boolean;
        FDisplayValue: string;
        FEditStyle: TEditStyle;
        FRow: Integer;
        FReadOnly: Boolean;
        FAutoUpdate: Boolean;
        FOwnerDrawPickList: Boolean;
        function CanExpand: Boolean;
        function Ident: Integer;
        function IsOnExpandButton(AX: Integer): Boolean;
        function GetItems(AIndex: Integer): TELPropsPageItem;
        procedure SetExpandable(const Value: TELPropsPageItemExpandable);
        procedure SetCaption(const Value: string);
        function GetLevel: Integer;
        procedure SetEditStyle(const Value: TEditStyle);
        procedure SetReadOnly(const Value: Boolean);
        procedure SetAutoUpdate(const Value: Boolean);
        procedure SetOwnerDrawPickList(const Value: Boolean);
    protected
        function CreateItem: TObject; override;
        procedure Change; override;
        procedure Deleted; override;
        function GetDisplayValue: string; virtual;
        procedure SetDisplayValue(const Value: string); virtual;
        procedure EditButtonClick; dynamic;
        procedure EditDblClick; dynamic;
        procedure GetEditPickList(APickList: TStrings); virtual;
        procedure PickListMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); virtual;
        procedure PickListMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); virtual;
        procedure PickListDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
    public
        constructor Create(AOwner: TELCustomPropsPage; AParent: TELPropsPageItem); virtual;
        destructor Destroy; override;
        procedure Expand;
        procedure Collapse;
        property Owner: TELCustomPropsPage read FOwner;
        property Parent: TELPropsPageItem read FParent;
        property Expandable: TELPropsPageItemExpandable read FExpandable write SetExpandable;
        property Expanded: Boolean read FExpanded;
        property Level: Integer read GetLevel;
        property Caption: string read FCaption write SetCaption;
        property DisplayValue: string read GetDisplayValue write SetDisplayValue;
        property EditStyle: TEditStyle read FEditStyle write SetEditStyle;
        property ReadOnly: Boolean read FReadOnly write SetReadOnly;
        property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate;
        property OwnerDrawPickList: Boolean read FOwnerDrawPickList write SetOwnerDrawPickList;
        property Items[AIndex: Integer]: TELPropsPageItem read GetItems; default;
    end;

    TELPropsPageItems = class(TELObjectList)
    private
        FOwner: TELCustomPropsPage;
        function GetItems(AIndex: Integer): TELPropsPageItem;
    protected
        function CreateItem: TObject; override;
        procedure Change; override;
    public
        constructor Create(AOwner: TELCustomPropsPage);
        property Owner: TELCustomPropsPage read FOwner;
        property Items[AIndex: Integer]: TELPropsPageItem read GetItems; default;
    end;

    TELPropsPageState = set of (ppsMovingSplitter, ppsChanged, ppsDestroying,
        ppsUpdatingEditorContent);

    {$IFDEF VER140}
    TELCustomGrid = TCustomGrid;
    {$ELSE}
    TELCustomGrid = TD6CustomGrid;
    {$ENDIF}

    TELCustomPropsPage = class(TELCustomGrid)
    private
        FState: TELPropsPageState;
        FOldRow: Integer;
        FSplitterOffset: Integer;
        FEditText: string;
        FItems: TELPropsPageItems;
        FRows: array of TELPropsPageItem;
        FUpdateCount: Integer;
        FValuesColor: TColor;
        FBitmap: Graphics.TBitmap;
        FBitmapBkColor: TColor;
        FBrush: HBRUSH;
        FCellBitmap: Graphics.TBitmap;
        procedure ItemsChange;
        function IsOnSplitter(AX: Integer): Boolean;
        procedure UpdateColWidths;
        procedure UpdateScrollBar;
        procedure AdjustTopRow;
        function ItemByRow(ARow: Integer): TELPropsPageItem;
        procedure UpdateData(ARow: Integer);
        procedure UpdatePattern;
        procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
        procedure WMSize(var Message: TWMSize); message WM_SIZE;
        procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
        procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
        procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
        procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
        procedure CMExit(var Message: TMessage); message CM_EXIT;
        function GetActiveItem: TELPropsPageItem;
        function GetSplitter: Integer;
        procedure SetSplitter(const Value: Integer);
        procedure SetValuesColor(const Value: TColor);
    protected
        procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
            AState: TGridDrawState); override;
        function SelectCell(ACol, ARow: Longint): Boolean; override;
        function CreateEditor: TInplaceEdit; override;
        procedure Paint; override;
        function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
        function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
        function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
        function CanEditModify: Boolean; override;
        function GetEditText(ACol, ARow: Longint): string; override;
        procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
        procedure CreateHandle; override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer); override;
        procedure TopLeftChanged; override;
        procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
        function CreateItem(AParent: TELPropsPageItem): TELPropsPageItem; virtual;
        procedure ItemExpanded(AItem: TELPropsPageItem); virtual;
        procedure ItemCollapsed(AItem: TELPropsPageItem); virtual;
        function GetItemCaptionColor(AItem: TELPropsPageItem): TColor; virtual;
        property Items: TELPropsPageItems read FItems;
        property Splitter: Integer read GetSplitter write SetSplitter;
        property ValuesColor: TColor read FValuesColor write SetValuesColor default clNavy;
        property Color default clBtnFace;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure BeginUpdate;
        procedure EndUpdate;

		property ActiveItem: TELPropsPageItem read GetActiveItem;
    end;

    TELPropsPage = class(TELCustomPropsPage)
    public
        property Items;
        property ActiveItem;
    published
        property Splitter;
        property ValuesColor;
        property Align;
        property Anchors;
        property BiDiMode;
        property BorderStyle;
        property Color;
        property Constraints;
        property Ctl3D;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font;
        property ParentBiDiMode;
        property ParentColor;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property Visible;
        property OnClick;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnMouseWheelDown;
        property OnMouseWheelUp;
        property OnStartDock;
        property OnStartDrag;
    end;

    { TELPropEditor }

type
    TELPropEditor = class;
    TELPropEditorClass = class of TELPropEditor;

    EELPropEditor = class(Exception);

    TELPropAttr = (praValueList, praSubProperties, praDialog, praMultiSelect,
        praSortList, praReadOnly, praVolatileSubProperties, praNotNestable, praAutoUpdate,
        praOwnerDrawValues, praComponentRef);
    TELPropAttrs = set of TELPropAttr;

    TELGetEditorClassProc = function(AInstance: TPersistent; APropInfo: PPropInfo): TELPropEditorClass of object;

    TELOnGetComponent = procedure(Sender: TObject; const AComponentName: string; var AComponent: TComponent) of object;
    TELOnGetComponentNames = procedure(Sender: TObject; AClass: TComponentClass; AResult: TStrings) of object;
    TELOnGetComponentName = procedure(Sender: TObject; AComponent: TComponent; var AName: string) of object;

    TELPropEditorPropListItem = packed record
        Instance: TPersistent;
        PropInfo: PPropInfo;
    end;

    PELPropEditorPropList = ^TELPropEditorPropList;
    TELPropEditorPropList = array[0..1023 { Range not used }] of TELPropEditorPropListItem;

    TELPropEditor = class
    private
        FPropList: PELPropEditorPropList;
        FPropCount: Integer;
        FOnModified: TNotifyEvent;
        FOnGetComponent: TELOnGetComponent;
        FOnGetComponentNames: TELOnGetComponentNames;
        FOnGetComponentName: TELOnGetComponentName;
        FDesigner: Pointer;
        function GetPropTypeInfo: PTypeInfo;
        function DoGetValue: string;
    protected
        procedure SetPropEntry(AIndex: Integer; AInstance: TPersistent; APropInfo: PPropInfo);
        function GetComponent(const AComponentName: string): TComponent;
        procedure GetComponentNames(AClass: TComponentClass; AResult: TStrings);
        function GetComponentName(AComponent: TComponent): string;
        function GetValue: string; virtual;
        procedure SetValue(const Value: string); virtual;
        function GetAttrs: TELPropAttrs; virtual;
        procedure GetValues(AValues: TStrings); virtual;
        procedure GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList); virtual; // Returns a list of TELPropEditor
        function GetPropName: string; virtual;
        function AllEqual: Boolean; virtual;
        procedure Edit; virtual;
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); virtual;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); virtual;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
    protected
        function GetPropInfo(AIndex: Integer): PPropInfo;
        function GetInstance(AIndex: Integer): TPersistent;
        function GetFloatValue(AIndex: Integer): Extended;
        function GetInt64Value(AIndex: Integer): Int64;
        function GetOrdValue(AIndex: Integer): Longint;
        function GetStrValue(AIndex: Integer): string;
        function GetVarValue(AIndex: Integer): Variant;
        procedure SetFloatValue(Value: Extended);
        procedure SetInt64Value(Value: Int64);
        procedure SetOrdValue(Value: Longint);
        procedure SetStrValue(const Value: string);
        procedure SetVarValue(const Value: Variant);
    public
        constructor Create(ADesigner: Pointer; APropCount: Integer); virtual;
        destructor Destroy; override;
        procedure Modified;
        property PropName: string read GetPropName;
        property PropTypeInfo: PTypeInfo read GetPropTypeInfo;
        property PropCount: Integer read FPropCount;
        property Value: string read DoGetValue write SetValue;
        property Designer: Pointer read FDesigner;
        property OnModified: TNotifyEvent read FOnModified write FOnModified;
        property OnGetComponent: TELOnGetComponent read FOnGetComponent write FOnGetComponent;
        property OnGetComponentNames: TELOnGetComponentNames read FOnGetComponentNames write FOnGetComponentNames;
        property OnGetComponentName: TELOnGetComponentName read FOnGetComponentName write FOnGetComponentName;
    end;

    TELNestedPropEditor = class(TELPropEditor)
    protected
        function GetPropName: string; override;
    public
        constructor Create(AParent: TELPropEditor); reintroduce;
        destructor Destroy; override;
    end;

    { Standart property editors }

    TELOrdinalPropEditor = class(TELPropEditor)
    protected
        function AllEqual: Boolean; override;
    end;

    TELIntegerPropEditor = class(TELOrdinalPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
    end;

    TELCharPropEditor = class(TELOrdinalPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
    end;

    TELEnumPropEditor = class(TELOrdinalPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetValues(AValues: TStrings); override;
    end;

    TELFloatPropEditor = class(TELPropEditor)
    protected
        function AllEqual: Boolean; override;
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
    end;

    TELStringPropEditor = class(TELPropEditor)
    protected
        function AllEqual: Boolean; override;
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
    end;

    TELSetElemPropEditor = class(TELNestedPropEditor)
    private
        FElement: Integer;
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetValues(AValues: TStrings); override;
        function GetPropName: string; override;
        function AllEqual: Boolean; override;
    public
        constructor Create(AParent: TELPropEditor; AElement: Integer); reintroduce;
        property Element: Integer read FElement;
    end;

    TELSetPropEditor = class(TELOrdinalPropEditor)
    protected
        function GetValue: string; override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList); override;
    end;

    TELClassPropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList); override;
    end;

    TELComponentPropEditor = class(TELPropEditor)
    protected
        function AllEqual: Boolean; override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList); override;
        function GetValue: string; override;
        procedure GetValues(AValues: TStrings); override;
        procedure SetValue(const Value: string); override;
    end;

    TELVariantTypePropEditor = class(TELNestedPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        procedure GetValues(AValues: TStrings); override;
        function GetPropName: string; override;
        function GetAttrs: TELPropAttrs; override;
        function AllEqual: Boolean; override;
    end;

    TELVariantPropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
        procedure GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList); override;
    end;

    TELInt64PropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function AllEqual: Boolean; override;
    end;

    TELComponentNamePropEditor = class(TELStringPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
    end;

    TELDatePropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
    end;

    TELTimePropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
    end;

    TELDateTimePropEditor = class(TELPropEditor)
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        function GetAttrs: TELPropAttrs; override;
    end;

    { VCL property editors }

    TELCaptionPropEditor = class(TELStringPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
    end;

    TELColorPropEditor = class(TELIntegerPropEditor)
    private
        FValues: TStrings;
        procedure AddValue(const LS: string);
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        procedure GetValues(AValues: TStrings); override;
        function GetAttrs: TELPropAttrs; override;
        procedure Edit; override;
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    end;

    TELCursorPropEditor = class(TELIntegerPropEditor)
    private
        FValues: TStrings;
        procedure AddValue(const LS: string);
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        procedure GetValues(AValues: TStrings); override;
        function GetAttrs: TELPropAttrs; override;
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    end;

    TELFontCharsetPropEditor = class(TELIntegerPropEditor)
    private
        FValues: TStrings;
        procedure AddValue(const LS: string);
    protected
        function GetValue: string; override;
        procedure SetValue(const Value: string); override;
        procedure GetValues(AValues: TStrings); override;
        function GetAttrs: TELPropAttrs; override;
    end;

    TELFontNamePropEditor = class(TELStringPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure GetValues(AValues: TStrings); override;
    end;

    TELImeNamePropEditor = class(TELStringPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure GetValues(AValues: TStrings); override;
    end;

    TELFontPropEditor = class(TELClassPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure Edit; override;
    end;

    TELModalResultPropEditor = class(TELIntegerPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        function GetValue: string; override;
        procedure GetValues(AValues: TStrings); override;
        procedure SetValue(const Value: string); override;
    end;

    TELPenStylePropEditor = class(TELEnumPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    end;

    TELBrushStylePropEditor = class(TELEnumPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    end;

    TELTabOrderPropEditor = class(TELIntegerPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
    end;

    TELShortCutPropEditor = class(TELOrdinalPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        function GetValue: string; override;
        procedure GetValues(AValues: TStrings); override;
        procedure SetValue(const Value: string); override;
    end;

    TELStringsPropEditor = class(TELClassPropEditor)
    protected
        function GetAttrs: TELPropAttrs; override;
        procedure Edit; override;
    end;

procedure ELGetObjectsProps(ADesigner: Pointer; AObjList: TList; AKinds: TTypeKinds;
    AOnlyNestable: Boolean; AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);

{ TELPropertyInspector }

{
 ToDo ( TELPropertyInspector ):

 ? Нужно сделать, чтобы правильно отображались ReadOnly свойства, и чтобы они
   не редактировались. Проблема в том, что в массиве TELCustomPropertyInspector.Objects
   одновременно могут находится объекты со свойством, которое у одних объектов является
   ReadOnly, а у других нет.

}

type
    TELCustomPropertyInspector = class;

    TELPropertyInspectorItem = class(TELPropsPageItem)
    private
        FEditor: TELPropEditor;
        FDisplayValue: string;
        procedure EditorModified(Sender: TObject);
        procedure EditorGetComponent(Sender: TObject; const AComponentName: string; var AComponent: TComponent);
        procedure EditorGetComponentNames(Sender: TObject; AClass: TComponentClass; AResult: TStrings);
        procedure EditorGetComponentName(Sender: TObject; AComponent: TComponent; var AName: string);
        procedure DeGetPickList(AResult: TStrings);
        procedure SetEditor(const Value: TELPropEditor);
    protected
        function GetDisplayValue: string; override;
        procedure SetDisplayValue(const Value: string); override;
        procedure GetEditPickList(APickList: TStrings); override;
        procedure EditButtonClick; override;
        procedure EditDblClick; override;
        procedure PickListMeasureHeight(const AValue: string; ACanvas: TCanvas; var AHeight: Integer); override;
        procedure PickListMeasureWidth(const AValue: string; ACanvas: TCanvas; var AWidth: Integer); override;
        procedure PickListDrawValue(const AValue: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
    public
        destructor Destroy; override;
        procedure UpdateParams;
        property Editor: TELPropEditor read FEditor write SetEditor; // Editor is destroyed
        // with "Self"
    end;

    TELPropertyInspectorEditorDescr = record
        TypeInfo: PTypeInfo;
        ObjectClass: TClass;
        PropName: string;
        EditorClass: TELPropEditorClass;
    end;

    TELPropertyInspectorOnFilterProp = procedure(Sender: TObject;
        AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean) of object;
    TELPropertyInspectorOnGetCaptionColor = procedure(Sender: TObject; APropTypeInfo: PTypeInfo;
        const APropName: string; var AColor: TColor) of object;
    TELPropertyInspectorOnGetEditorClass = procedure(Sender: TObject;
        AInstance: TPersistent; APropInfo: PPropInfo; var AEditorClass: TELPropEditorClass) of object;

    TELPropertyInspectorPropKind = (pkProperties, pkEvents, pkReadOnly);
    TELPropertyInspectorPropKinds = set of TELPropertyInspectorPropKind;

    TELCustomPropertyInspector = class(TELCustomPropsPage)
    private
        FObjects: TList;
        FEditors: array of TELPropertyInspectorEditorDescr;
        FOnGetComponent: TELOnGetComponent;
        FOnGetComponentNames: TELOnGetComponentNames;
        FOnModified: TNotifyEvent;
        FOnGetComponentName: TELOnGetComponentName;
        FPropKinds: TELPropertyInspectorPropKinds;
        FOnFilterProp: TELPropertyInspectorOnFilterProp;
        FComponentRefColor: TColor;
        FComponentRefChildColor: TColor;
        FExpandComponentRefs: Boolean;
        FReadOnly: Boolean;
        FDesigner: Pointer;
        FOnGetCaptionColor: TELPropertyInspectorOnGetCaptionColor;
        FObjectsLocked: Boolean;
        FOnGetEditorClass: TELPropertyInspectorOnGetEditorClass;
        procedure Change;
        procedure InternalModified;
        function IndexOfEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
            const APropName: string; AEditorClass: TELPropEditorClass): Integer;
        procedure CheckObjectsLock;
        function GetObjects(AIndex: Integer): TPersistent;
        procedure SetObjects(AIndex: Integer; const Value: TPersistent);
        function GetObjectCount: Integer;
        procedure SetPropKinds(const Value: TELPropertyInspectorPropKinds);
        procedure SetComponentRefColor(const Value: TColor);
        procedure SetComponentRefChildColor(const Value: TColor);
        procedure SetExpandComponentRefs(const Value: Boolean);
        procedure SetReadOnly(const Value: Boolean);
        procedure SetDesigner(const Value: Pointer);
    protected
        function CreateItem(AParent: TELPropsPageItem): TELPropsPageItem; override;
        procedure ItemExpanded(AItem: TELPropsPageItem); override;
        procedure ItemCollapsed(AItem: TELPropsPageItem); override;
        function GetItemCaptionColor(AItem: TELPropsPageItem): TColor; override;
        function GetEditorClass(AInstance: TPersistent; APropInfo: PPropInfo): TELPropEditorClass; virtual;
        procedure GetComponent(const AComponentName: string; var AComponent: TComponent); virtual;
        procedure GetComponentNames(AClass: TComponentClass; AResult: TStrings); virtual;
        procedure GetComponentName(AComponent: TComponent; var AName: string); virtual;
        procedure FilterProp(AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean); virtual;
        procedure GetCaptionColor(APropTypeInfo: PTypeInfo; const APropName: string; var AColor: TColor); virtual;
        property Designer: Pointer read FDesigner write SetDesigner;
        property PropKinds: TELPropertyInspectorPropKinds read FPropKinds write SetPropKinds default [pkProperties];
        property ComponentRefColor: TColor read FComponentRefColor write SetComponentRefColor default clMaroon;
        property ComponentRefChildColor: TColor read FComponentRefChildColor write SetComponentRefChildColor default clGreen;
        property ExpandComponentRefs: Boolean read FExpandComponentRefs write SetExpandComponentRefs default True;
        property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
        property OnGetComponent: TELOnGetComponent read FOnGetComponent write FOnGetComponent;
        property OnGetComponentNames: TELOnGetComponentNames read FOnGetComponentNames write FOnGetComponentNames;
        property OnGetComponentName: TELOnGetComponentName read FOnGetComponentName write FOnGetComponentName;
        property OnFilterProp: TELPropertyInspectorOnFilterProp read FOnFilterProp write FOnFilterProp;
        property OnModified: TNotifyEvent read FOnModified write FOnModified;
        property OnGetCaptionColor: TELPropertyInspectorOnGetCaptionColor read FOnGetCaptionColor write FOnGetCaptionColor;
        property OnGetEditorClass: TELPropertyInspectorOnGetEditorClass read FOnGetEditorClass write FOnGetEditorClass;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Add(AObject: TPersistent);
        procedure Delete(AIndex: Integer);
        procedure Remove(AObject: TPersistent);
        procedure Clear;
        procedure UpdateItems;
        procedure AssignObjects(AObjects: TList);
        function IndexOf(AObject: TPersistent): Integer;
        procedure Modified;
        procedure RegisterPropEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
            const APropName: string; AEditorClass: TELPropEditorClass);
        procedure UnregisterPropEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
            const APropName: string; AEditorClass: TELPropEditorClass);
        property Objects[AIndex: Integer]: TPersistent read GetObjects write SetObjects;
        property ObjectCount: Integer read GetObjectCount;
    end;

    TELPropertyInspector = class(TELCustomPropertyInspector)
    public
        property Designer;
    published
        property PropKinds;
        property ComponentRefColor;
        property ComponentRefChildColor;
        property ExpandComponentRefs;
        property ReadOnly;
        property Splitter;
        property ValuesColor;
        property Align;
        property Anchors;
        property BiDiMode;
        property BorderStyle;
        property Color;
        property Constraints;
        property Ctl3D;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font;
        property ParentBiDiMode;
        property ParentColor;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property Visible;
        property OnGetComponent;
        property OnGetComponentNames;
        property OnGetComponentName;
        property OnFilterProp;
        property OnModified;
        property OnGetCaptionColor;
        property OnGetEditorClass;
        property OnClick;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnMouseWheelDown;
        property OnMouseWheelUp;
        property OnStartDock;
        property OnStartDrag;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents(SELComponentPage, [TELPropertyInspector]);
end;

const
    SNull = '(Null)';
    SString = 'String';
    SUnknown = '(Unknown)';
    SUnknownType = 'Unknown Type';
    srUnknown = '(Unknown)';
    srNone = '(None)';

    {$IFDEF VER140}
    VarTypeNames: array[varEmpty..varInt64] of string = (
        'Unassigned', // varEmpty
        'Null', // varNull
        'Smallint', // varSmallint
        'Integer', // varInteger
        'Single', // varSingle
        'Double', // varDouble
        'Currency', // varCurrency
        'Date', // varDate
        'OleStr', // varOleStr
        '', // varDispatch
        '', // varError
        'Boolean', // varBoolean
        '', // varVariant
        '', // varUnknown
        '', // [varDecimal]
        '', // [undefined]
        'Shortint', // varShortInt
        'Byte', // varByte
        'Word', // varWord
        'LongWord', // varLongWord
        'Int64'); // varInt64
    {$ELSE}
    VarTypeNames: array[0..varByte] of string = ('Unassigned', 'Null', 'Smallint',
        'Integer', 'Single', 'Double', 'Currency', 'Date', 'OleStr', '', '',
        'Boolean', '', '', '', '', '', 'Byte');
    {$ENDIF}

    {$IFDEF VER140}
    ModalResults: array[mrNone..mrYesToAll] of string = (
        'mrNone',
        'mrOk',
        'mrCancel',
        'mrAbort',
        'mrRetry',
        'mrIgnore',
        'mrYes',
        'mrNo',
        'mrAll',
        'mrNoToAll',
        'mrYesToAll');
    {$ELSE}
    ModalResults: array[mrNone..mrYesToAll] of string = (
        'mrNone',
        'mrOk',
        'mrCancel',
        'mrAbort',
        'mrRetry',
        'mrIgnore',
        'mrYes',
        'mrNo',
        'mrAll',
        'mrNoToAll',
        'mrYesToAll');
    {$ENDIF}

    {$IFDEF VER140}
    ShortCuts: array[0..108] of TShortCut = (
        scNone,
        Byte('A') or scCtrl,
        Byte('B') or scCtrl,
        Byte('C') or scCtrl,
        Byte('D') or scCtrl,
        Byte('E') or scCtrl,
        Byte('F') or scCtrl,
        Byte('G') or scCtrl,
        Byte('H') or scCtrl,
        Byte('I') or scCtrl,
        Byte('J') or scCtrl,
        Byte('K') or scCtrl,
        Byte('L') or scCtrl,
        Byte('M') or scCtrl,
        Byte('N') or scCtrl,
        Byte('O') or scCtrl,
        Byte('P') or scCtrl,
        Byte('Q') or scCtrl,
        Byte('R') or scCtrl,
        Byte('S') or scCtrl,
        Byte('T') or scCtrl,
        Byte('U') or scCtrl,
        Byte('V') or scCtrl,
        Byte('W') or scCtrl,
        Byte('X') or scCtrl,
        Byte('Y') or scCtrl,
        Byte('Z') or scCtrl,
        Byte('A') or scCtrl or scAlt,
        Byte('B') or scCtrl or scAlt,
        Byte('C') or scCtrl or scAlt,
        Byte('D') or scCtrl or scAlt,
        Byte('E') or scCtrl or scAlt,
        Byte('F') or scCtrl or scAlt,
        Byte('G') or scCtrl or scAlt,
        Byte('H') or scCtrl or scAlt,
        Byte('I') or scCtrl or scAlt,
        Byte('J') or scCtrl or scAlt,
        Byte('K') or scCtrl or scAlt,
        Byte('L') or scCtrl or scAlt,
        Byte('M') or scCtrl or scAlt,
        Byte('N') or scCtrl or scAlt,
        Byte('O') or scCtrl or scAlt,
        Byte('P') or scCtrl or scAlt,
        Byte('Q') or scCtrl or scAlt,
        Byte('R') or scCtrl or scAlt,
        Byte('S') or scCtrl or scAlt,
        Byte('T') or scCtrl or scAlt,
        Byte('U') or scCtrl or scAlt,
        Byte('V') or scCtrl or scAlt,
        Byte('W') or scCtrl or scAlt,
        Byte('X') or scCtrl or scAlt,
        Byte('Y') or scCtrl or scAlt,
        Byte('Z') or scCtrl or scAlt,
        VK_F1,
        VK_F2,
        VK_F3,
        VK_F4,
        VK_F5,
        VK_F6,
        VK_F7,
        VK_F8,
        VK_F9,
        VK_F10,
        VK_F11,
        VK_F12,
        VK_F1 or scCtrl,
        VK_F2 or scCtrl,
        VK_F3 or scCtrl,
        VK_F4 or scCtrl,
        VK_F5 or scCtrl,
        VK_F6 or scCtrl,
        VK_F7 or scCtrl,
        VK_F8 or scCtrl,
        VK_F9 or scCtrl,
        VK_F10 or scCtrl,
        VK_F11 or scCtrl,
        VK_F12 or scCtrl,
        VK_F1 or scShift,
        VK_F2 or scShift,
        VK_F3 or scShift,
        VK_F4 or scShift,
        VK_F5 or scShift,
        VK_F6 or scShift,
        VK_F7 or scShift,
        VK_F8 or scShift,
        VK_F9 or scShift,
        VK_F10 or scShift,
        VK_F11 or scShift,
        VK_F12 or scShift,
        VK_F1 or scShift or scCtrl,
        VK_F2 or scShift or scCtrl,
        VK_F3 or scShift or scCtrl,
        VK_F4 or scShift or scCtrl,
        VK_F5 or scShift or scCtrl,
        VK_F6 or scShift or scCtrl,
        VK_F7 or scShift or scCtrl,
        VK_F8 or scShift or scCtrl,
        VK_F9 or scShift or scCtrl,
        VK_F10 or scShift or scCtrl,
        VK_F11 or scShift or scCtrl,
        VK_F12 or scShift or scCtrl,
        VK_INSERT,
        VK_INSERT or scShift,
        VK_INSERT or scCtrl,
        VK_DELETE,
        VK_DELETE or scShift,
        VK_DELETE or scCtrl,
        VK_BACK or scAlt,
        VK_BACK or scShift or scAlt);
    {$ELSE}
    ShortCuts: array[0..108] of TShortCut = (
        scNone,
        Byte('A') or scCtrl,
        Byte('B') or scCtrl,
        Byte('C') or scCtrl,
        Byte('D') or scCtrl,
        Byte('E') or scCtrl,
        Byte('F') or scCtrl,
        Byte('G') or scCtrl,
        Byte('H') or scCtrl,
        Byte('I') or scCtrl,
        Byte('J') or scCtrl,
        Byte('K') or scCtrl,
        Byte('L') or scCtrl,
        Byte('M') or scCtrl,
        Byte('N') or scCtrl,
        Byte('O') or scCtrl,
        Byte('P') or scCtrl,
        Byte('Q') or scCtrl,
        Byte('R') or scCtrl,
        Byte('S') or scCtrl,
        Byte('T') or scCtrl,
        Byte('U') or scCtrl,
        Byte('V') or scCtrl,
        Byte('W') or scCtrl,
        Byte('X') or scCtrl,
        Byte('Y') or scCtrl,
        Byte('Z') or scCtrl,
        Byte('A') or scCtrl or scAlt,
        Byte('B') or scCtrl or scAlt,
        Byte('C') or scCtrl or scAlt,
        Byte('D') or scCtrl or scAlt,
        Byte('E') or scCtrl or scAlt,
        Byte('F') or scCtrl or scAlt,
        Byte('G') or scCtrl or scAlt,
        Byte('H') or scCtrl or scAlt,
        Byte('I') or scCtrl or scAlt,
        Byte('J') or scCtrl or scAlt,
        Byte('K') or scCtrl or scAlt,
        Byte('L') or scCtrl or scAlt,
        Byte('M') or scCtrl or scAlt,
        Byte('N') or scCtrl or scAlt,
        Byte('O') or scCtrl or scAlt,
        Byte('P') or scCtrl or scAlt,
        Byte('Q') or scCtrl or scAlt,
        Byte('R') or scCtrl or scAlt,
        Byte('S') or scCtrl or scAlt,
        Byte('T') or scCtrl or scAlt,
        Byte('U') or scCtrl or scAlt,
        Byte('V') or scCtrl or scAlt,
        Byte('W') or scCtrl or scAlt,
        Byte('X') or scCtrl or scAlt,
        Byte('Y') or scCtrl or scAlt,
        Byte('Z') or scCtrl or scAlt,
        VK_F1,
        VK_F2,
        VK_F3,
        VK_F4,
        VK_F5,
        VK_F6,
        VK_F7,
        VK_F8,
        VK_F9,
        VK_F10,
        VK_F11,
        VK_F12,
        VK_F1 or scCtrl,
        VK_F2 or scCtrl,
        VK_F3 or scCtrl,
        VK_F4 or scCtrl,
        VK_F5 or scCtrl,
        VK_F6 or scCtrl,
        VK_F7 or scCtrl,
        VK_F8 or scCtrl,
        VK_F9 or scCtrl,
        VK_F10 or scCtrl,
        VK_F11 or scCtrl,
        VK_F12 or scCtrl,
        VK_F1 or scShift,
        VK_F2 or scShift,
        VK_F3 or scShift,
        VK_F4 or scShift,
        VK_F5 or scShift,
        VK_F6 or scShift,
        VK_F7 or scShift,
        VK_F8 or scShift,
        VK_F9 or scShift,
        VK_F10 or scShift,
        VK_F11 or scShift,
        VK_F12 or scShift,
        VK_F1 or scShift or scCtrl,
        VK_F2 or scShift or scCtrl,
        VK_F3 or scShift or scCtrl,
        VK_F4 or scShift or scCtrl,
        VK_F5 or scShift or scCtrl,
        VK_F6 or scShift or scCtrl,
        VK_F7 or scShift or scCtrl,
        VK_F8 or scShift or scCtrl,
        VK_F9 or scShift or scCtrl,
        VK_F10 or scShift or scCtrl,
        VK_F11 or scShift or scCtrl,
        VK_F12 or scShift or scCtrl,
        VK_INSERT,
        VK_INSERT or scShift,
        VK_INSERT or scCtrl,
        VK_DELETE,
        VK_DELETE or scShift,
        VK_DELETE or scCtrl,
        VK_BACK or scAlt,
        VK_BACK or scShift or scAlt);
    {$ENDIF}

type
    TCustomListBoxAccess = class(TCustomListBox);

procedure ELGetObjectsProps(ADesigner: Pointer; AObjList: TList; AKinds: TTypeKinds;
    AOnlyNestable: Boolean; AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);

type
    TObjProps = record
        Props: PPropList;
        Count: Integer;
    end;

var
    LPropLists: array of TObjProps;
    LIntersection: array of array of PPropInfo;
    LI, LJ, LK, LIndex, LObjCount: Integer;
    LEditorClass: TELPropEditorClass;
    LEditor: TELPropEditor;
    LAttrs: TELPropAttrs;
    LObj: TPersistent;

begin
    LObjCount := AObjList.Count;
    { Create prop lists }
    SetLength(LPropLists, LObjCount);
    for LI := 0 to LObjCount - 1 do
        begin
            LObj := AObjList[LI];
            LPropLists[LI].Count := GetPropList(LObj.ClassInfo, AKinds, nil);
            GetMem(LPropLists[LI].Props, LPropLists[LI].Count * SizeOf(Pointer));
            try
                GetPropList(LObj.ClassInfo, AKinds, LPropLists[LI].Props);
            except
                FreeMem(LPropLists[LI].Props);
                raise;
            end;
        end;
    try
        { Initialize intersection }
        SetLength(LIntersection, LPropLists[0].Count);
        for LI := 0 to LPropLists[0].Count - 1 do
            begin
                SetLength(LIntersection[LI], LObjCount);
                LIntersection[LI][0] := LPropLists[0].Props[LI];
            end;
        { Intersect }
        for LI := 1 to LObjCount - 1 do
            for LJ := High(LIntersection) downto 0 do
                begin
                    LIndex := -1;
                    for LK := 0 to LPropLists[LI].Count - 1 do
                        if (LPropLists[LI].Props[LK].PropType^ = LIntersection[LJ][0].PropType^) and
                            SameText(LPropLists[LI].Props[LK].Name, LIntersection[LJ][0].Name) then
                            begin
                                LIndex := LK;
                                Break;
                            end;
                    if LIndex <> -1 then
                        LIntersection[LJ][LI] := LPropLists[LI].Props[LK]
                    else
                        begin
                            for LK := LJ + 1 to High(LIntersection) do
                                LIntersection[LK - 1] := LIntersection[LK];
                            SetLength(LIntersection, Length(LIntersection) - 1);
                        end;
                end;
        { Create property editors }
        for LI := 0 to High(LIntersection) do
            begin
                { Determine editor class }
                LEditorClass := AGetEditorClassProc(TPersistent(AObjList[0]), LIntersection[LI][0]);
                for LJ := 0 to LObjCount - 1 do
                    if AGetEditorClassProc(TPersistent(AObjList[LJ]), LIntersection[LI][LJ]) <> LEditorClass then
                        begin
                            LEditorClass := nil;
                            Break;
                        end;
                { Create editor }
                if LEditorClass <> nil then
                    begin
                        LEditor := LEditorClass.Create(ADesigner, AObjList.Count);
                        try
                            for LJ := 0 to AObjList.Count - 1 do
                                LEditor.SetPropEntry(LJ, TPersistent(AObjList[LJ]), LIntersection[LI][LJ]);
                            LAttrs := LEditor.GetAttrs;
                            if ((LObjCount = 1) or (praMultiSelect in LAttrs)) and
                                (not AOnlyNestable or not (praNotNestable in LAttrs)) then
                                AResult.Add(LEditor)
                            else
                                begin
                                    LEditor.Free;
                                    LEditor := nil;
                                end;
                        except
                            LEditor.Free;
                            raise;
                        end;
                    end;
            end;
    finally
        { Free prop lists }
        for LI := 0 to LObjCount - 1 do
            FreeMem(LPropLists[LI].Props);
    end;
end;

{ TELCustomPropsPage }

constructor TELCustomPropsPage.Create(AOwner: TComponent);
begin
    inherited;
    Width := 188;
    Height := 193;
    DefaultColWidth := 84;
    DefaultRowHeight := 16;
    ColCount := 2;
    RowCount := 0;
    FixedRows := 0;
    FixedCols := 1;
    Color := clBtnFace;
    Options := [goEditing, goAlwaysShowEditor, goThumbTracking];
    DesignOptionsBoost := [];
    FSaveCellExtents := False;
    ScrollBars := ssNone;
    DefaultDrawing := False;
    FItems := TELPropsPageItems.Create(Self);
    FValuesColor := clNavy;
    FBitmap := Graphics.TBitmap.Create;
    UpdatePattern;
    FCellBitmap := Graphics.TBitmap.Create;
end;

function TELCustomPropsPage.CreateEditor: TInplaceEdit;
begin
    Result := TELPropsPageInplaceEdit.Create(Self);
end;

(*procedure TELCustomPropsPage.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);

  procedure _DrawExpandButton(AX, AY: Integer; AExpanded: Boolean);
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(AX, AY, AX + 9, AY + 9);
    Canvas.Polyline([Point(AX + 2, AY + 4), Point(AX + 7, AY + 4)]);
    if not AExpanded then
      Canvas.Polyline([Point(AX + 4, AY + 2), Point(AX + 4, AY + 7)]);
  end;

var
  LS: string;
  LExpandButton, LExpanded: Boolean;
  LIdent: Integer;
  LItem: TELPropsPageItem;
  LCaptionColor: TColor;

begin
  if (ACol <> Col) or (ARow <> Row) or (InplaceEditor = nil) then
  begin
    { Fill }
    LItem := ItemByRow(ARow);
    if LItem <> nil then
    begin
      if ACol = 0 then LS := LItem.Caption else LS := LItem.DisplayValue;
      LExpandButton := LItem.CanExpand;
      LExpanded := LItem.Expanded;
      LIdent := LItem.Ident;
      LCaptionColor := GetItemCaptionColor(LItem);
    end
    else
    begin
      LS := '';
      LExpandButton := False;
      LExpanded := False;
      LIdent := 0;
      LCaptionColor := Font.Color;
    end;
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    if ACol = 0 then Canvas.Font.Color := LCaptionColor else Canvas.Font.Color := ValuesColor;
    Canvas.TextRect(
      Rect(
        ARect.Left,
        ARect.Top,
        ARect.Right - 2 * Ord(ACol = 0),
  ARect.Bottom - 1 - Ord((ARow = Row) or (ARow = Row - 1))
      ),
      ARect.Left + 1 + (12 + LIdent) * Ord(ACol = 0),
      ARect.Top + 1,
      LS
    );
    if LExpandButton and (ACol = 0) then
      _DrawExpandButton(ARect.Left + 2 + LIdent, ARect.Top + 3, LExpanded);
  end;
  if ACol = 0 then
  begin
    { Splitter }
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Polyline([Point(ARect.Right - 2, ARect.Top), Point(ARect.Right - 2, ARect.Bottom)]);
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.Polyline([Point(ARect.Right - 1, ARect.Top), Point(ARect.Right - 1, ARect.Bottom)]);
  end;
  if ARow = Row - 1 then
  begin
    { Selected row ages }
    Canvas.Pen.Color := cl3DDkShadow;
    Canvas.Polyline([Point(ARect.Left, ARect.Bottom - 2), Point(ARect.Right, ARect.Bottom - 2)]);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Polyline([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right, ARect.Bottom - 1)]);
  end
  else if ARow = Row then
  begin
 { Selected row ages }
    if ACol = 0 then
    begin
      Canvas.Pen.Color := cl3DDkShadow;
      Canvas.Polyline([Point(ARect.Left, ARect.Top), Point(ARect.Left, ARect.Bottom)]);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.Polyline([Point(ARect.Left + 1, ARect.Top), Point(ARect.Left + 1, ARect.Bottom)]);
    end;
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.Polyline([Point(ARect.Left, ARect.Bottom - 2), Point(ARect.Right, ARect.Bottom - 2)]);
    Canvas.Pen.Color := cl3DLight;
    Canvas.Polyline([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right, ARect.Bottom - 1)]);
  end
  else
  begin
    { Row line }
    if FBitmapBkColor <> Color then
      UpdatePattern;
    FillRect(Canvas.Handle, Rect(ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom), FBrush);
  end;
end;*)

procedure TELCustomPropsPage.DrawCell(ACol, ARow: Integer; ARect: TRect;
    AState: TGridDrawState);

    procedure _DrawExpandButton(AX, AY: Integer; AExpanded: Boolean);
    begin
        with FCellBitmap.Canvas do
            begin
                Pen.Color := clBlack;
                Brush.Color := clWhite;
                Rectangle(AX, AY, AX + 9, AY + 9);
                Polyline([Point(AX + 2, AY + 4), Point(AX + 7, AY + 4)]);
                if not AExpanded then
                    Polyline([Point(AX + 4, AY + 2), Point(AX + 4, AY + 7)]);
            end;
    end;

var
    LS: string;
    LExpandButton, LExpanded: Boolean;
    LIdent: Integer;
    LItem: TELPropsPageItem;
    LCaptionColor: TColor;

begin
    if (ACol <> Col) or (ARow <> Row) or (InplaceEditor = nil) then
        begin
            FCellBitmap.Width := ARect.Right - ARect.Left;
            FCellBitmap.Height := ARect.Bottom - ARect.Top;

            { Fill }
            with FCellBitmap.Canvas do
                begin
                    LItem := ItemByRow(ARow);
                    if LItem <> nil then
                        begin
                            if ACol = 0 then
                                LS := LItem.Caption
                            else
                                LS := LItem.DisplayValue;
                            LExpandButton := LItem.CanExpand;
                            LExpanded := LItem.Expanded;
                            LIdent := LItem.Ident;
                            LCaptionColor := GetItemCaptionColor(LItem);
                        end
                    else
                        begin
                            LS := '';
                            LExpandButton := False;
                            LExpanded := False;
                            LIdent := 0;
                            LCaptionColor := Font.Color;
                        end;
                    Brush.Color := Color;
                    FCellBitmap.Canvas.Font := self.Font;
                    if ACol = 0 then
                        Font.Color := LCaptionColor
                    else
                        Font.Color := ValuesColor;
                    TextRect(
                        Rect(0, 0, FCellBitmap.Width, FCellBitmap.Height),
                        1 + (12 + LIdent) * Ord(ACol = 0),
                        1,
                        LS
                        );
                    if LExpandButton and (ACol = 0) then
                        _DrawExpandButton(2 + LIdent, 3, LExpanded);

                    if ACol = 0 then
                        begin
                            { Splitter }
                            Pen.Color := clBtnShadow;
                            Polyline([Point(FCellBitmap.Width - 2, 0), Point(FCellBitmap.Width - 2, FCellBitmap.Height)]);
                            Pen.Color := clBtnHighlight;
                            Polyline([Point(FCellBitmap.Width - 1, 0), Point(FCellBitmap.Width - 1, FCellBitmap.Height)]);
                        end;
                    if ARow = Row - 1 then
                        begin
                            { Selected row ages }
                            Pen.Color := cl3DDkShadow;
                            Polyline([Point(0, FCellBitmap.Height - 2), Point(FCellBitmap.Width, FCellBitmap.Height - 2)]);
                            Pen.Color := clBtnShadow;
                            Polyline([Point(0, FCellBitmap.Height - 1), Point(FCellBitmap.Width, FCellBitmap.Height - 1)]);
                        end
                    else
                        if ARow = Row then
                            begin
                                { Selected row ages }
                                if ACol = 0 then
                                    begin
                                        Pen.Color := cl3DDkShadow;
                                        Polyline([Point(0, 0), Point(0, FCellBitmap.Height)]);
                                        Pen.Color := clBtnShadow;
                                        Polyline([Point(1, 0), Point(1, FCellBitmap.Height)]);
                                    end;
                                Pen.Color := clBtnHighlight;
                                Polyline([Point(0, FCellBitmap.Height - 2), Point(FCellBitmap.Width, FCellBitmap.Height - 2)]);
                                Pen.Color := cl3DLight;
                                Polyline([Point(0, FCellBitmap.Height - 1), Point(FCellBitmap.Width, FCellBitmap.Height - 1)]);
                            end
                        else
                            begin
                                { Row line }
                                if FBitmapBkColor <> Color then
                                    UpdatePattern;
                                Windows.FillRect(Handle, Rect(0, FCellBitmap.Height - 1, FCellBitmap.Width, FCellBitmap.Height), FBrush);
                            end;
                end;
            Canvas.Draw(ARect.Left, ARect.Top, FCellBitmap);
        end
    else
        with Canvas do
            begin
                Pen.Color := clBtnHighlight;
                Polyline([Point(ARect.Left, ARect.Bottom - 2), Point(ARect.Right, ARect.Bottom - 2)]);
                Pen.Color := cl3DLight;
                Polyline([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right, ARect.Bottom - 1)]);
            end;
end;

function TELCustomPropsPage.SelectCell(ACol, ARow: Integer): Boolean;
begin
    UpdateData(FOldRow);
    Result := inherited SelectCell(ACol, ARow);
    InvalidateRow(FOldRow - 1);
    InvalidateRow(FOldRow);
    InvalidateRow(FOldRow + 1);
    InvalidateRow(ARow - 1);
    InvalidateRow(ARow);
    InvalidateRow(ARow + 1);
    FOldRow := ARow;
end;

procedure TELCustomPropsPage.Paint;
begin
    inherited;
    DrawCell(Col, Row, CellRect(Col, Row), []);
end;

function TELCustomPropsPage.DoMouseWheelDown(Shift: TShiftState;
    MousePos: TPoint): Boolean;
begin
    Result := True;
    if TopRow < RowCount - VisibleRowCount then
        TopRow := TopRow + 1;
end;

function TELCustomPropsPage.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
    Result := True;
    if TopRow > FixedRows then
        TopRow := TopRow - 1;
end;

procedure TELCustomPropsPage.CreateHandle;
begin
    inherited;
    UpdateScrollBar;
    ShowEditor;
    UpdateColWidths;
end;

function TELCustomPropsPage.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
    LItem: TELPropsPageItem;
begin
    LItem := ItemByRow(ARow);
    if LItem <> nil then
        Result := LItem.EditStyle
    else
        Result := esSimple;
end;

procedure TELCustomPropsPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
    LGridCoord: TGridCoord;
begin
    if not (ppsMovingSplitter in FState) then
        begin
            inherited;
            if MouseCapture then
                begin
                    LGridCoord := MouseCoord(X, Y);
                    if (LGridCoord.Y <> -1) then
                        Row := LGridCoord.Y
                    else
                        if Y < 0 then
                            begin
                                if Row > 0 then
                                    Row := Row - 1;
                            end
                        else
                            begin
                                if Row < RowCount - 1 then
                                    Row := Row + 1;
                            end

                end;
        end;

    if ppsMovingSplitter in FState then
        Splitter := X - FSplitterOffset;
end;

function TELCustomPropsPage.IsOnSplitter(AX: Integer): Boolean;
begin
    Result := (AX >= ColWidths[0] - 4) and (AX <= ColWidths[0]);
end;

procedure TELCustomPropsPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
var
    LGridCoord: TGridCoord;
    LCellRect: TRect;
    LItem: TELPropsPageItem;
begin
    if ssLeft in Shift then
        begin
            if IsOnSplitter(X) then
                begin
                    Include(FState, ppsMovingSplitter);
                    FSplitterOffset := X - ColWidths[0];
                end
            else
                begin
                    inherited;
                    LGridCoord := MouseCoord(X, Y);
                    if (LGridCoord.X = 0) and (LGridCoord.Y <> -1) then
                        begin
                            LCellRect := CellRect(LGridCoord.X, LGridCoord.Y);
                            LItem := ItemByRow(LGridCoord.Y);
                            if (LItem <> nil) and LItem.CanExpand and LItem.IsOnExpandButton(X) then
                                begin
                                    Row := LGridCoord.Y;
                                    if LItem.Expanded then
                                        LItem.Collapse
                                    else
                                        LItem.Expand;
                                end
                            else
                                if (LGridCoord.Y < TopRow + VisibleRowCount) then
                                    Row := LGridCoord.Y;
                        end;
                end;
        end;
end;

procedure TELCustomPropsPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
var
    LForm: TCustomForm;
begin
    if ppsMovingSplitter in FState then
        if csDesigning in ComponentState then
            begin
                LForm := GetParentForm(Self);
                if (LForm <> nil) and (LForm.Designer <> nil) then
                    LForm.Designer.Modified;
            end
        else
            inherited;
    Exclude(FState, ppsMovingSplitter);
end;

procedure TELCustomPropsPage.UpdateColWidths;
begin
    ColWidths[1] := Width - ColWidths[0];
end;

procedure TELCustomPropsPage.UpdateScrollBar;
var
    LSI: TScrollInfo;
begin
    if HandleAllocated then
        begin
            LSI.cbSize := SizeOf(LSI);
            LSI.fMask := SIF_ALL;
            GetScrollInfo(Self.Handle, SB_VERT, LSI);
            LSI.nPage := VisibleRowCount;
            LSI.nMin := 0;
            LSI.nMax := RowCount - 1;
            LSI.nPos := TopRow;
            SetScrollInfo(Self.Handle, SB_VERT, LSI, True);
        end;
end;

procedure TELCustomPropsPage.WMVScroll(var Message: TWMVScroll);
var
    LTopRow: Integer;
    LSI: TScrollInfo;
begin
    LTopRow := TopRow;
    with Message do
        case ScrollCode of
            SB_LINEUP: LTopRow := LTopRow - 1;
            SB_LINEDOWN: LTopRow := LTopRow + 1;
            SB_PAGEUP: LTopRow := LTopRow - VisibleRowCount;
            SB_PAGEDOWN: LTopRow := LTopRow + VisibleRowCount;
            SB_THUMBPOSITION, SB_THUMBTRACK:
                begin
                    LSI.cbSize := SizeOf(LSI);
                    LSI.fMask := SIF_ALL;
                    GetScrollInfo(Self.Handle, SB_VERT, LSI);
                    LTopRow := LSI.nTrackPos;
                end;
            SB_BOTTOM: LTopRow := RowCount - 1;
            SB_TOP: LTopRow := 0;
        end;
    if LTopRow < 0 then
        LTopRow := 0;
    if LTopRow > RowCount - VisibleRowCount then
        LTopRow := RowCount - VisibleRowCount;
    TopRow := LTopRow;
    UpdateScrollBar;
    Message.Result := 0;
end;

procedure TELCustomPropsPage.TopLeftChanged;
begin
    inherited;
    UpdateScrollBar;
end;

procedure TELCustomPropsPage.SizeChanged(OldColCount, OldRowCount: Integer);
begin
    inherited;
    AdjustTopRow;
    UpdateScrollBar;
    ShowEditor;
end;

procedure TELCustomPropsPage.WMSize(var Message: TWMSize);
begin
    inherited;
    AdjustTopRow;
    UpdateScrollBar;
    Splitter := Splitter; // Include UpdateColWidths;
    ShowEditor;
end;

procedure TELCustomPropsPage.AdjustTopRow;
var
    LI: Integer;
begin
    if HandleAllocated then
        begin
            LI := ClientHeight div DefaultRowHeight;
            if RowCount - TopRow < LI then
                begin
                    LI := RowCount - LI;
                    if LI < 0 then
                        LI := 0;
                    TopRow := LI;
                end;
        end;
end;

destructor TELCustomPropsPage.Destroy;
begin
    Include(FState, ppsDestroying);
    FItems.Free;
    FBitmap.Free;
    FCellBitmap.Free;
    if FBrush <> 0 then
        DeleteObject(FBrush);
    inherited;
end;

procedure TELCustomPropsPage.ItemsChange;

    procedure _FillRows(AList: TELObjectList);
    var
        LI: Integer;
    begin
        for LI := 0 to AList.Count - 1 do
            begin
                SetLength(FRows, Length(FRows) + 1);
                FRows[High(FRows)] := TELPropsPageItem(AList[LI]);
                TELPropsPageItem(AList[LI]).FRow := High(FRows);
                if TELPropsPageItem(AList[LI]).Expanded then
                    _FillRows(TELObjectList(AList[LI]));
            end;
    end;

var
    LI: Integer;
    LActiveItem: TELPropsPageItem;

begin
    if (FUpdateCount <= 0) and not (ppsDestroying in FState) then
        begin
            LActiveItem := ActiveItem;
            for LI := 0 to High(FRows) do
                if FRows[LI] <> nil then
                    FRows[LI].FRow := -1;
            SetLength(FRows, 0);
            _FillRows(Items);
            RowCount := Length(FRows);
            while LActiveItem <> nil do
                begin
                    if LActiveItem.FRow <> -1 then
                        begin
                            if Row <> LActiveItem.FRow then
                                Row := LActiveItem.FRow;
                            Break;
                        end;
                    LActiveItem := LActiveItem.Parent;
                end;
            Invalidate;
            LActiveItem := ActiveItem;
            if InplaceEditor <> nil then
                begin
                    if LActiveItem <> nil then
                        begin
                            if (TELPropsPageInplaceEdit(InplaceEditor).ReadOnlyStyle <> LActiveItem.ReadOnly) or
                                (TELPropsPageInplaceEdit(InplaceEditor).EditStyle <> LActiveItem.EditStyle) then
                                InvalidateEditor;
                            InplaceEditor.Text := LActiveItem.DisplayValue;
                        end
                    else
                        begin
                            if not TELPropsPageInplaceEdit(InplaceEditor).ReadOnlyStyle or
                                (TELPropsPageInplaceEdit(InplaceEditor).EditStyle <> esSimple) then
                                InvalidateEditor;
                            InplaceEditor.Text := '';
                        end;
                    FEditText := InplaceEditor.Text;
                end;
            Update;
        end
    else
        Include(FState, ppsChanged);
end;

function TELCustomPropsPage.GetActiveItem: TELPropsPageItem;
var
    LItem: TELPropsPageItem;
begin
    LItem := ItemByRow(Row);
    if LItem <> nil then
        Result := LItem
    else
        Result := nil;
end;

procedure TELCustomPropsPage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
    LGridCoord: TGridCoord;
    LCellRect: TRect;
    LItem: TELPropsPageItem;
begin
    inherited;
    LGridCoord := MouseCoord(Message.XPos, Message.YPos);
    if (LGridCoord.X = 0) and (LGridCoord.Y <> -1) then
        begin
            LCellRect := CellRect(LGridCoord.X, LGridCoord.Y);
            LItem := ItemByRow(LGridCoord.Y);
            if (LItem <> nil) and not LItem.IsOnExpandButton(Message.XPos) then
                if LItem.Expanded then
                    LItem.Collapse
                else
                    LItem.Expand;
        end;
end;

function TELCustomPropsPage.CreateItem(AParent: TELPropsPageItem): TELPropsPageItem;
begin
    Result := TELPropsPageItem.Create(Self, AParent);
end;

function TELCustomPropsPage.CanEditModify: Boolean;
begin
    Result := (ActiveItem <> nil) and not ActiveItem.ReadOnly;
end;

function TELCustomPropsPage.GetEditText(ACol, ARow: Integer): string;
var
    LItem: TELPropsPageItem;
begin
    LItem := ItemByRow(ARow);
    if (ACol = 1) and (LItem <> nil) then
        Result := LItem.DisplayValue;
    FEditText := Result;
end;

procedure TELCustomPropsPage.SetEditText(ACol, ARow: Integer; const Value: string);
var
    LItem: TELPropsPageItem;
begin
    if not (ppsUpdatingEditorContent in FState) then
        begin
            LItem := ItemByRow(ARow);
            if (ACol = 1) and (LItem <> nil) and LItem.AutoUpdate then
                LItem.DisplayValue := Value;
            FEditText := Value;
        end;
end;

function TELCustomPropsPage.ItemByRow(ARow: Integer): TELPropsPageItem;
begin
    if (ARow >= 0) and (ARow <= High(FRows)) then
        Result := FRows[ARow]
    else
        Result := nil;
end;

procedure TELCustomPropsPage.UpdateData(ARow: Integer);
var
    LItem: TELPropsPageItem;
begin
    LItem := ItemByRow(ARow);
    if LItem <> nil then
        try
            LItem.DisplayValue := FEditText;
            FEditText := LItem.DisplayValue;
        finally
            if (InplaceEditor <> nil) then
                TELPropsPageInplaceEdit(InplaceEditor).UpdateContents;
        end;
end;

procedure TELCustomPropsPage.CMExit(var Message: TMessage);
begin
    UpdateData(Row);
    inherited;
end;

procedure TELCustomPropsPage.BeginUpdate;
begin
    Inc(FUpdateCount);
end;

procedure TELCustomPropsPage.EndUpdate;
begin
    Dec(FUpdateCount);
    if (FUpdateCount <= 0) and (ppsChanged in FState) then
        begin
            ItemsChange;
            Exclude(FState, ppsChanged);
        end;
end;

procedure TELCustomPropsPage.ItemCollapsed(AItem: TELPropsPageItem);
begin
    // Do nothing
end;

procedure TELCustomPropsPage.ItemExpanded(AItem: TELPropsPageItem);
begin
    // Do nothing
end;

procedure TELCustomPropsPage.WMSetCursor(var Message: TWMSetCursor);
var
    LP: TPoint;
begin
    GetCursorPos(LP);
    LP := ScreenToClient(LP);
    if IsOnSplitter(LP.X) then
        Windows.SetCursor(Screen.Cursors[crHSplit])
    else
        inherited;
end;

procedure TELCustomPropsPage.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
    Msg.Result := Ord(IsOnSplitter(Msg.XPos) or (ppsMovingSplitter in FState));
end;

procedure TELCustomPropsPage.SetSplitter(const Value: Integer);
var
    LNewVal: Integer;
begin
    LNewVal := Value;
    if LNewVal > Width - 40 then
        LNewVal := Width - 40;
    if LNewVal < 40 then
        LNewVal := 40;
    ColWidths[0] := LNewVal;
    UpdateColWidths;
end;

function TELCustomPropsPage.GetSplitter: Integer;
begin
    Result := ColWidths[0];
end;

procedure TELCustomPropsPage.SetValuesColor(const Value: TColor);
begin
    if FValuesColor <> Value then
        begin
            FValuesColor := Value;
            Invalidate;
        end;
end;

function TELCustomPropsPage.GetItemCaptionColor(
    AItem: TELPropsPageItem): TColor;
begin
    Result := Font.Color;
end;

procedure TELCustomPropsPage.CMFontChanged(var Message: TMessage);
begin
    inherited;
    Canvas.Font := Font;
    DefaultRowHeight := Canvas.TextHeight('Wg') + 3;
end;

procedure TELCustomPropsPage.UpdatePattern;
var
    LI: Integer;
begin
    FBitmapBkColor := Color;
    with FBitmap do
        begin
            Width := 8;
            Height := 1;
            Canvas.Brush.Color := FBitmapBkColor;
            Canvas.Brush.Style := bsSolid;
            Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
            LI := 0;
            while LI < Width do
                begin
                    Canvas.Pixels[LI, 0] := clBtnShadow;
                    Inc(LI, 2);
                end;
        end;
    if FBrush <> 0 then
        DeleteObject(FBrush);
    FBrush := CreatePatternBrush(FBitmap.Handle);
end;

{ TELPropsPageInplaceEdit }

procedure TELPropsPageInplaceEdit.BoundsChanged;
begin
    inherited;
    if not FChangingBounds then
        begin
            FChangingBounds := True;
            try
                UpdateLoc(Rect(Left, Top, Left + Width, Top + Height - 2));
                SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN,
                    MakeLong(0, ButtonWidth * Ord(EditStyle <> esSimple) + 2));
            finally
                FChangingBounds := False;
            end;
        end;
end;

procedure TELPropsPageInplaceEdit.CloseUp(Accept: Boolean);
begin
    inherited;
    if Accept then
        with TELCustomPropsPage(Owner) do
            begin
                UpdateData(Row);
                SelectAll;
            end;
end;

constructor TELPropsPageInplaceEdit.Create(AOwner: TComponent);
begin
    inherited;
    DropDownRows := 8;
    ButtonWidth := 16;
end;

procedure TELPropsPageInplaceEdit.CreateParams(var Params: TCreateParams);
begin
    inherited;
    Params.Style := Params.Style and not ES_MULTILINE;
end;

procedure TELPropsPageInplaceEdit.DblClick;
begin
    if TELCustomPropsPage(Grid).ActiveItem <> nil then
        TELCustomPropsPage(Grid).ActiveItem.EditDblClick;
end;

procedure TELPropsPageInplaceEdit.DoEditButtonClick;
begin
    if TELCustomPropsPage(Grid).ActiveItem <> nil then
        TELCustomPropsPage(Grid).ActiveItem.EditButtonClick;
end;

procedure TELPropsPageInplaceEdit.DoGetPickListItems;
begin
    if TELCustomPropsPage(Grid).ActiveItem <> nil then
        begin
            PickList.Items.Clear;
            TELCustomPropsPage(Grid).ActiveItem.GetEditPickList(PickList.Items);
            PickList.ItemIndex := PickList.Items.IndexOf(Text);
        end;
end;

procedure TELPropsPageInplaceEdit.DropDown;
var
    LP: TPoint;
    LI, LY, LVisItemCount, LItemHW, LHW: Integer;
    LItem: TELPropsPageItem;
begin
    LItem := TELCustomPropsPage(Grid).ActiveItem;
    if not ListVisible and (LItem <> nil) then
        begin
            if LItem.OwnerDrawPickList then
                begin
                    TCustomListBoxAccess(PickList).Style := lbOwnerDrawVariable;
                    TCustomListBoxAccess(PickList).OnMeasureItem := PickListMeasureItem;
                    TCustomListBoxAccess(PickList).OnDrawItem := PickListDrawItem;
                end
            else
                begin
                    TCustomListBoxAccess(PickList).Style := lbStandard;
                    TCustomListBoxAccess(PickList).OnMeasureItem := nil;
                    TCustomListBoxAccess(PickList).OnDrawItem := nil;
                end;

            ActiveList.Width := Width;
            if ActiveList = PickList then
                begin
                    { Get values }
                    DoGetPickListItems;
                    TCustomListboxAccess(PickList).Color := Color;
                    TCustomListboxAccess(PickList).Font := Font;
                    { Calc initial visible item count }
                    if (DropDownRows > 0) and (PickList.Items.Count >= DropDownRows) then
                        LVisItemCount := DropDownRows
                    else
                        LVisItemCount := PickList.Items.Count;
                    { Calc PickList height }
                    if LItem.OwnerDrawPickList then
                        begin
                            LHW := 4;
                            for LI := 0 to LVisItemCount - 1 do
                                begin
                                    LItemHW := TCustomListboxAccess(PickList).ItemHeight;
                                    LItem.PickListMeasureHeight(PickList.Items[LI], PickList.Canvas, LItemHW);
                                    Inc(LHW, LItemHW);
                                end;
                        end
                    else
                        LHW := LVisItemCount * TCustomListboxAccess(PickList).ItemHeight + 4;
                    if PickList.Items.Count > 0 then
                        PickList.Height := LHW
                    else
                        PickList.Height := 20;
                    { Set PickList selected item }
                    if Text = '' then
                        PickList.ItemIndex := -1
                    else
                        PickList.ItemIndex := PickList.Items.IndexOf(Text);
                    { Calc PickList width }
                    LHW := PickList.ClientWidth;
                    for LI := 0 to PickList.Items.Count - 1 do
                        begin
                            LItemHW := PickList.Canvas.TextWidth(PickList.Items[LI]);
                            if LItem.OwnerDrawPickList then
                                LItem.PickListMeasureWidth(PickList.Items[LI], PickList.Canvas, LItemHW);
                            if LItemHW > LHW then
                                LHW := LItemHW;
                        end;
                    PickList.ClientWidth := LHW;
                end;
            LP := Parent.ClientToScreen(Point(Left, Top));
            LY := LP.Y + Height;
            if LY + ActiveList.Height > Screen.Height then
                LY := LP.Y - ActiveList.Height;
            SetWindowPos(ActiveList.Handle, HWND_TOP, LP.X, LY, 0, 0,
                SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
            ListVisible := True;
            Invalidate;
            Windows.SetFocus(Handle);
        end;
end;

procedure _KillMessage(AWnd: HWnd; AMsg: Integer);
var
    LM: TMsg;
begin
    LM.Message := 0;
    if PeekMessage(LM, AWnd, AMsg, AMsg, pm_Remove) and (LM.Message = WM_QUIT) then
        PostQuitMessage(LM.wparam);
end;

procedure TELPropsPageInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
    case Key of
        VK_RETURN:
            begin
                TELCustomPropsPage(Grid).UpdateData(TELCustomPropsPage(Grid).Row);
                if Shift = [ssCtrl] then
                    begin
                        _KillMessage(Handle, WM_CHAR);
                        DblClick;
                        SelectAll;
                    end;
                Key := 0;
            end;
        VK_ESCAPE:
            if TELCustomPropsPage(Grid).ActiveItem <> nil then
                begin
                    Text := TELCustomPropsPage(Grid).ActiveItem.DisplayValue;
                    SelectAll;
                    Key := 0;
                end;
        VK_HOME:
            if (SelStart = 0) and (SelLength > 0) then
                SelLength := 0;
        VK_END:
            if (SelStart + SelLength = Length(Text)) and (SelLength > 0) then
                SelLength := 0;
    end;
    if Key <> 0 then
        inherited;
end;

procedure TELPropsPageInplaceEdit.PickListDrawItem(Control: TWinControl;
    Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
    LItem: TELPropsPageItem;
begin
    LItem := TELCustomPropsPage(Grid).ActiveItem;
    if (LItem <> nil) and LItem.OwnerDrawPickList then
        LItem.PickListDrawValue(PickList.Items[Index], PickList.Canvas, Rect, odSelected in State);
end;

procedure TELPropsPageInplaceEdit.PickListMeasureItem(Control: TWinControl;
    Index: Integer; var Height: Integer);
var
    LItem: TELPropsPageItem;
begin
    LItem := TELCustomPropsPage(Grid).ActiveItem;
    if (LItem <> nil) and LItem.OwnerDrawPickList then
        LItem.PickListMeasureHeight(PickList.Items[Index], PickList.Canvas, Height);
end;

procedure TELPropsPageInplaceEdit.UpdateContents;
begin
    Include(TELCustomPropsPage(Grid).FState, ppsUpdatingEditorContent);
    try
        inherited;
        if not EditCanModify then
            begin
                Color := TELCustomPropsPage(Owner).Color;
                Font.Color := TELCustomPropsPage(Owner).ValuesColor;
            end
        else
            begin
                Color := clWindow;
                Font.Color := clWindowText;
            end;
        FReadOnlyStyle := not EditCanModify;
    finally
        Exclude(TELCustomPropsPage(Grid).FState, ppsUpdatingEditorContent);
    end;
end;

procedure TELPropsPageInplaceEdit.WMLButtonDblClk(var Message: TWMMouse);
begin
    if OverButton(Point(Message.XPos, Message.YPos)) then
        PostMessage(Handle, WM_LBUTTONDOWN, TMessage(Message).WParam, TMessage(Message).LParam)
    else
        inherited;
    SelectAll;
end;

{ TELPropsPageItems }

procedure TELPropsPageItems.Change;
begin
    Owner.ItemsChange;
end;

constructor TELPropsPageItems.Create(AOwner: TELCustomPropsPage);
begin
    inherited Create;
    FOwner := AOwner;
end;

function TELPropsPageItems.CreateItem: TObject;
begin
    Result := FOwner.CreateItem(nil);
end;

function TELPropsPageItems.GetItems(AIndex: Integer): TELPropsPageItem;
begin
    Result := TELPropsPageItem(inherited Items[AIndex]);
end;

{ TELPropsPageItem }

function TELPropsPageItem.CanExpand: Boolean;
begin
    Result := (Expandable = mieYes) or ((Expandable = mieAuto) and (Count > 0));
end;

procedure TELPropsPageItem.Change;
begin
    Owner.ItemsChange;
end;

procedure TELPropsPageItem.Collapse;
begin
    if FExpanded then
        begin
            Owner.BeginUpdate;
            try
                FExpanded := False;
                Owner.ItemCollapsed(Self);
                Change;
            finally
                Owner.EndUpdate;
            end;
        end;
end;

constructor TELPropsPageItem.Create(AOwner: TELCustomPropsPage; AParent: TELPropsPageItem);
begin
    inherited Create;
    FOwner := AOwner;
    FParent := AParent;
    FRow := -1;
end;

function TELPropsPageItem.CreateItem: TObject;
begin
    Result := FOwner.CreateItem(Self);
end;

procedure TELPropsPageItem.Deleted;
begin
    FExpanded := FExpanded and CanExpand;
end;

procedure TELPropsPageItem.Expand;
begin
    if not FExpanded and CanExpand then
        begin
            Owner.BeginUpdate;
            try
                FExpanded := True;
                Owner.ItemExpanded(Self);
                Change;
            finally
                Owner.EndUpdate;
            end;
        end;
end;

function TELPropsPageItem.GetItems(AIndex: Integer): TELPropsPageItem;
begin
    Result := TELPropsPageItem(inherited Items[AIndex]);
end;

function TELPropsPageItem.GetLevel: Integer;
var
    LParent: TELPropsPageItem;
begin
    Result := 0;
    LParent := Parent;
    while LParent <> nil do
        begin
            Inc(Result);
            LParent := LParent.Parent;
        end;
end;

procedure TELPropsPageItem.SetCaption(const Value: string);
begin
    if FCaption <> Value then
        begin
            FCaption := Value;
            Change;
        end;
end;

procedure TELPropsPageItem.SetExpandable(const Value: TELPropsPageItemExpandable);
begin
    if FExpandable <> Value then
        begin
            FExpandable := Value;
            FExpanded := FExpanded and CanExpand;
            Change;
        end;
end;

procedure TELPropsPageItem.SetDisplayValue(const Value: string);
begin
    if FDisplayValue <> Value then
        begin
            FDisplayValue := Value;
            Change;
        end;
end;

function TELPropsPageItem.Ident: Integer;
begin
    Result := Level * 10;
end;

function TELPropsPageItem.IsOnExpandButton(AX: Integer): Boolean;
begin
    Result := (AX >= Ident) and (AX <= Ident + 13);
end;

procedure TELPropsPageItem.SetEditStyle(const Value: TEditStyle);
begin
    if FEditStyle <> Value then
        begin
            FEditStyle := Value;
            Change;
        end;
end;

destructor TELPropsPageItem.Destroy;
begin
    if FRow <> -1 then
        begin
            FOwner.FRows[FRow] := nil;
            FRow := -1;
        end;
    inherited;
end;

procedure TELPropsPageItem.SetReadOnly(const Value: Boolean);
begin
    if FReadOnly <> Value then
        begin
            FReadOnly := Value;
            Change;
        end;
end;

function TELPropsPageItem.GetDisplayValue: string;
begin
    Result := FDisplayValue;
end;

procedure TELPropsPageItem.EditButtonClick;
begin
    // Do nothing
end;

procedure TELPropsPageItem.GetEditPickList(APickList: TStrings);
begin
    // Do nothing
end;

procedure TELPropsPageItem.EditDblClick;
begin
    // Do nothing
end;

procedure TELPropsPageItem.SetAutoUpdate(const Value: Boolean);
begin
    if FAutoUpdate <> Value then
        begin
            FAutoUpdate := Value;
            Change;
        end;
end;

procedure TELPropsPageItem.SetOwnerDrawPickList(const Value: Boolean);
begin
    if FOwnerDrawPickList <> Value then
        begin
            FOwnerDrawPickList := Value;
            Change;
        end;
end;

procedure TELPropsPageItem.PickListDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
    // Do nothing
end;

procedure TELPropsPageItem.PickListMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    // Do nothing
end;

procedure TELPropsPageItem.PickListMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    // Do nothing
end;

{ TELPropertyInspectorItem }

procedure TELPropertyInspectorItem.DeGetPickList(AResult: TStrings);
var
    LList: TStringList;
begin
    if FEditor <> nil then
        begin
            LList := TStringList.Create;
            try
                FEditor.GetValues(LList);
                if praSortList in FEditor.GetAttrs then
                    LList.Sort;
                AResult.Assign(LList);
            finally
                LList.Free;
            end;
        end;
end;

destructor TELPropertyInspectorItem.Destroy;
begin
    if FEditor <> nil then
        FEditor.Free;
    inherited;
end;

procedure TELPropertyInspectorItem.EditButtonClick;
begin
    if (FEditor <> nil) and not TELCustomPropertyInspector(Owner).ReadOnly then
        FEditor.Edit;
end;

procedure TELPropertyInspectorItem.EditDblClick;
var
    LAttrs: TELPropAttrs;
    LValues: TStringList;
    LIndex: Integer;
begin
    if (FEditor <> nil) and not TELCustomPropertyInspector(Owner).ReadOnly then
        begin
            LAttrs := FEditor.GetAttrs;
            if (praValueList in LAttrs) and not (praDialog in LAttrs) then
                begin
                    LValues := TStringList.Create;
                    try
                        DeGetPickList(LValues);
                        if LValues.Count > 0 then
                            begin
                                LIndex := LValues.IndexOf(DisplayValue) + 1;
                                if LIndex > LValues.Count - 1 then
                                    LIndex := 0;
                                DisplayValue := LValues[LIndex];
                            end;
                    finally
                        LValues.Free;
                    end;
                end
            else
                if praDialog in LAttrs then
                    EditButtonClick;
        end;
end;

procedure TELPropertyInspectorItem.EditorGetComponent(Sender: TObject;
    const AComponentName: string; var AComponent: TComponent);
begin
    TELCustomPropertyInspector(Owner).GetComponent(AComponentName, AComponent);
end;

procedure TELPropertyInspectorItem.EditorGetComponentName(Sender: TObject;
    AComponent: TComponent; var AName: string);
begin
    TELCustomPropertyInspector(Owner).GetComponentName(AComponent, AName);
end;

procedure TELPropertyInspectorItem.EditorGetComponentNames(Sender: TObject;
    AClass: TComponentClass; AResult: TStrings);
begin
    TELCustomPropertyInspector(Owner).GetComponentNames(AClass, AResult);
end;

procedure TELPropertyInspectorItem.EditorModified(Sender: TObject);
begin
    TELCustomPropertyInspector(Owner).InternalModified;
end;

function TELPropertyInspectorItem.GetDisplayValue: string;
begin
    Result := FDisplayValue;
end;

procedure TELPropertyInspectorItem.GetEditPickList(APickList: TStrings);
begin
    DeGetPickList(APickList);
end;

procedure TELPropertyInspectorItem.PickListDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
    if FEditor <> nil then
        FEditor.ValuesDrawValue(AValue, ACanvas, ARect, ASelected);
end;

procedure TELPropertyInspectorItem.PickListMeasureHeight(
    const AValue: string; ACanvas: TCanvas; var AHeight: Integer);
begin
    if FEditor <> nil then
        FEditor.ValuesMeasureHeight(AValue, ACanvas, AHeight);
end;

procedure TELPropertyInspectorItem.PickListMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    if FEditor <> nil then
        FEditor.ValuesMeasureWidth(AValue, ACanvas, AWidth);
end;

procedure TELPropertyInspectorItem.SetDisplayValue(const Value: string);
var
    LOldValue: string;
begin
    if Value <> FDisplayValue then
        begin
            LOldValue := FDisplayValue;
            TELPropertyInspector(Owner).FObjectsLocked := True;
            try
                FEditor.Value := Value; // May raise an exception
            finally
                TELPropertyInspector(Owner).FObjectsLocked := False;
            end;
            FDisplayValue := FEditor.Value; // FEditor.Value may be not equal with Value
            if LOldValue <> FDisplayValue then
                begin
                    Owner.BeginUpdate;
                    try
                        Change;
                        if Expanded and (praVolatileSubProperties in FEditor.GetAttrs) then
                            begin
                                Collapse;
                                Expand;
                            end;
                    finally
                        Owner.EndUpdate;
                    end;
                end;
        end;
end;

procedure TELPropertyInspectorItem.SetEditor(const Value: TELPropEditor);
begin
    FEditor := Value;
    FEditor.OnModified := EditorModified;
    FEditor.OnGetComponent := EditorGetComponent;
    FEditor.OnGetComponentNames := EditorGetComponentNames;
    FEditor.OnGetComponentName := EditorGetComponentName;
    UpdateParams;
end;

procedure TELPropertyInspectorItem.UpdateParams;

const
    LExpandables: array[Boolean] of TELPropsPageItemExpandable = (mieNo, mieYes);

var
    LPropAttrs: TELPropAttrs;
    LStr: string;

begin
    if FEditor <> nil then
        begin
            Owner.BeginUpdate;
            try
                Caption := FEditor.PropName;
                LPropAttrs := FEditor.GetAttrs;
                if (praValueList in LPropAttrs) and not TELCustomPropertyInspector(Owner).ReadOnly then
                    EditStyle := esPickList
                else
                    if (praDialog in LPropAttrs) and not TELCustomPropertyInspector(Owner).ReadOnly then
                        EditStyle := esEllipsis
                    else
                        EditStyle := esSimple;
                Expandable := LExpandables[(praSubProperties in LPropAttrs) and
                    not ((praComponentRef in LPropAttrs) and not TELCustomPropertyInspector(Owner).ExpandComponentRefs)];
                ReadOnly := (praReadOnly in LPropAttrs) or TELCustomPropertyInspector(Owner).ReadOnly;
                AutoUpdate := praAutoUpdate in LPropAttrs;
                OwnerDrawPickList := praOwnerDrawValues in LPropAttrs;
                LStr := FEditor.Value;
                if FDisplayValue <> LStr then
                    begin
                        FDisplayValue := LStr;
                        Change;
                    end;
            finally
                Owner.EndUpdate;
            end;
        end;
end;

{ TELCustomPropertyInspector }

procedure TELCustomPropertyInspector.Add(AObject: TPersistent);
begin
    CheckObjectsLock;
    FObjects.Add(AObject);
    Change;
end;

constructor TELCustomPropertyInspector.Create(AOwner: TComponent);
begin
    inherited;
    FObjects := TList.Create;
    FPropKinds := [pkProperties];
    FComponentRefColor := clMaroon;
    FComponentRefChildColor := clGreen;
    FExpandComponentRefs := True;
end;

function TELCustomPropertyInspector.CreateItem(
    AParent: TELPropsPageItem): TELPropsPageItem;
begin
    Result := TELPropertyInspectorItem.Create(Self, AParent);
end;

procedure TELCustomPropertyInspector.Delete(AIndex: Integer);
begin
    CheckObjectsLock;
    FObjects.Delete(AIndex);
    Change;
end;

destructor TELCustomPropertyInspector.Destroy;
begin
    FObjects.Free;
    inherited;
end;

function TELCustomPropertyInspector.GetObjectCount: Integer;
begin
    Result := FObjects.Count;
end;

function TELCustomPropertyInspector.GetObjects(AIndex: Integer): TPersistent;
begin
    Result := FObjects[AIndex];
end;

function TELCustomPropertyInspector.IndexOf(AObject: TPersistent): Integer;
var
    LI: Integer;
begin
    Result := -1;
    for LI := 0 to FObjects.Count - 1 do
        if FObjects[LI] = AObject then
            begin
                Result := LI;
                Break;
            end;
end;

procedure TELCustomPropertyInspector.Change;
var
    LEditors: TList;
    LI: Integer;
begin
    BeginUpdate;
    try
        Items.Clear;
        if FObjects.Count > 0 then
            begin
                LEditors := TList.Create;
                try
                    ELGetObjectsProps(FDesigner, FObjects, tkAny, False, GetEditorClass, LEditors);
                    Items.Count := LEditors.Count;
                    for LI := 0 to LEditors.Count - 1 do
                        TELPropertyInspectorItem(Items[LI]).Editor := LEditors[LI];
                finally
                    LEditors.Free;
                end;
            end;
    finally
        EndUpdate;
    end;
end;

procedure TELCustomPropertyInspector.Remove(AObject: TPersistent);
var
    LI: Integer;
begin
    LI := IndexOf(AObject);
    if LI <> -1 then
        Delete(LI);
end;

procedure TELCustomPropertyInspector.SetObjects(AIndex: Integer;
    const Value: TPersistent);
begin
    FObjects[AIndex] := Value;
    Change;
end;

procedure TELCustomPropertyInspector.Modified;
begin
    UpdateItems;
end;

function TELCustomPropertyInspector.GetEditorClass(AInstance: TPersistent; APropInfo: PPropInfo): TELPropEditorClass;
var
    LI, LBest: Integer;
    LTypeInfo: PTypeInfo;
    LClass: TClass;
    LFilterRes: Boolean;
begin
    if Assigned(OnGetEditorClass) then
        begin
            Result := nil;
            OnGetEditorClass(Self, AInstance, APropInfo, Result);
            if Result <> nil then
                Exit;
        end;

    LTypeInfo := APropInfo.PropType^;
    LClass := AInstance.ClassType;

    if not (((pkProperties in PropKinds) and (LTypeInfo.Kind in tkProperties)) or
        ((pkEvents in PropKinds) and (LTypeInfo.Kind in tkMethods))) or
        not Assigned(APropInfo.GetProc) or
        (not (pkReadOnly in PropKinds) and not Assigned(APropInfo.SetProc) and (APropInfo.PropType^.Kind <> tkClass)) then
        begin
            Result := nil;
            Exit;
        end;

    LFilterRes := True;
    FilterProp(AInstance, APropInfo, LFilterRes);
    if not LFilterRes then
        begin
            Result := nil;
            Exit;
        end;

    LBest := -1;
    for LI := High(FEditors) downto 0 do
        if (LTypeInfo = FEditors[LI].TypeInfo) or
            ((LTypeInfo.Kind = tkClass) and (FEditors[LI].TypeInfo.Kind = tkClass) and
            GetTypeData(LTypeInfo).ClassType.InheritsFrom(GetTypeData(FEditors[LI].TypeInfo).ClassType)) then
            if ((FEditors[LI].ObjectClass = nil) or (LClass.InheritsFrom(FEditors[LI].ObjectClass))) and
                ((FEditors[LI].PropName = '') or SameText(FEditors[LI].PropName, APropInfo.Name)) then
                if (LBest = -1) or ((FEditors[LBest].ObjectClass = nil) and (FEditors[LI].ObjectClass <> nil)) or
                    ((FEditors[LBest].PropName = '') and (FEditors[LI].PropName <> '')) or
                    ((FEditors[LBest].TypeInfo <> LTypeInfo) and (FEditors[LI].TypeInfo = LTypeInfo)) or
                    ((FEditors[LBest].TypeInfo <> FEditors[LI].TypeInfo) and (FEditors[LBest].TypeInfo.Kind = tkClass) and
                    (FEditors[LI].TypeInfo.Kind = tkClass) and
                    (GetTypeData(FEditors[LI].TypeInfo).ClassType.InheritsFrom(GetTypeData(FEditors[LBest].TypeInfo).ClassType))) then
                    begin
                        LBest := LI;
                        Break;
                    end;
    if LBest <> -1 then
        Result := FEditors[LBest].EditorClass
    else
        Result := nil;

    if Result = nil then
        if LTypeInfo = TypeInfo(TComponentName) then
            Result := TELComponentNamePropEditor
        else
            if LTypeInfo = TypeInfo(TDate) then
                Result := TELDatePropEditor
            else
                if LTypeInfo = TypeInfo(TTime) then
                    Result := TELTimePropEditor
                else
                    if LTypeInfo = TypeInfo(TDateTime) then
                        Result := TELDateTimePropEditor
                    else
                        if LTypeInfo = TypeInfo(TCaption) then
                            Result := TELCaptionPropEditor
                        else
                            if LTypeInfo = TypeInfo(TColor) then
                                Result := TELColorPropEditor
                            else
                                if LTypeInfo = TypeInfo(TCursor) then
                                    Result := TELCursorPropEditor
                                else
                                    if LTypeInfo = TypeInfo(TFontCharset) then
                                        Result := TELFontCharsetPropEditor
                                    else
                                        if LTypeInfo = TypeInfo(TFontName) then
                                            Result := TELFontNamePropEditor
                                        else
                                            if LTypeInfo = TypeInfo(TImeName) then
                                                Result := TELImeNamePropEditor
                                            else
                                                if (LTypeInfo = TypeInfo(TFont)) or
                                                    ((LTypeInfo.Kind = tkClass) and
                                                    GetTypeData(LTypeInfo).ClassType.InheritsFrom(TFont)) then
                                                    Result := TELFontPropEditor
                                                else
                                                    if LTypeInfo = TypeInfo(TModalResult) then
                                                        Result := TELModalResultPropEditor
                                                    else
                                                        if LTypeInfo = TypeInfo(TPenStyle) then
                                                            Result := TELPenStylePropEditor
                                                        else
                                                            if LTypeInfo = TypeInfo(TBrushStyle) then
                                                                Result := TELBrushStylePropEditor
                                                            else
                                                                if LTypeInfo = TypeInfo(TTabOrder) then
                                                                    Result := TELTabOrderPropEditor
                                                                else
                                                                    if LTypeInfo = TypeInfo(TShortCut) then
                                                                        Result := TELShortCutPropEditor
                                                                    else
                                                                        if (LTypeInfo = TypeInfo(TStrings)) or
                                                                            ((LTypeInfo.Kind = tkClass) and
                                                                            GetTypeData(LTypeInfo).ClassType.InheritsFrom(TStrings)) then
                                                                            Result := TELStringsPropEditor;

    if Result = nil then
        case LTypeInfo.Kind of
            tkInteger: Result := TELIntegerPropEditor;
            tkChar: Result := TELCharPropEditor;
            tkEnumeration: Result := TELEnumPropEditor;
            tkFloat: Result := TELFloatPropEditor;
            tkString,
                tkLString,
                tkWString: Result := TELStringPropEditor;
            tkSet: Result := TELSetPropEditor;
            tkClass:
                if (LTypeInfo = TypeInfo(TComponent)) or
                    GetTypeData(LTypeInfo).ClassType.InheritsFrom(TComponent) then
                    Result := TELComponentPropEditor
                else
                    Result := TELClassPropEditor;
            tkVariant: Result := TELVariantPropEditor;
            tkInt64: Result := TELInt64PropEditor;
            else
                Result := TELPropEditor;
        end;
end;

procedure TELCustomPropertyInspector.ItemCollapsed(AItem: TELPropsPageItem);
begin
    BeginUpdate;
    try
        AItem.Clear;
    finally
        EndUpdate;
    end;
end;

procedure TELCustomPropertyInspector.ItemExpanded(AItem: TELPropsPageItem);
var
    LEditor: TELPropEditor;
    LSubProps: TList;
    LI: Integer;
begin
    LEditor := TELPropertyInspectorItem(AItem).Editor;
    if LEditor <> nil then
        begin
            LSubProps := TList.Create;
            try
                if not ((praComponentRef in LEditor.GetAttrs) and not ExpandComponentRefs) then
                    begin
                        LEditor.GetSubProps(GetEditorClass, LSubProps);
                        BeginUpdate;
                        try
                            for LI := 0 to LSubProps.Count - 1 do
                                TELPropertyInspectorItem(AItem[AItem.Add]).Editor := LSubProps[LI];
                        finally
                            EndUpdate;
                        end;
                    end;
            finally
                LSubProps.Free;
            end;
        end;
end;

procedure TELCustomPropertyInspector.Clear;
begin
    CheckObjectsLock;
    FObjects.Clear;
    Change;
end;

procedure TELCustomPropertyInspector.RegisterPropEditor(ATypeInfo: PTypeInfo;
    AObjectClass: TClass; const APropName: string;
    AEditorClass: TELPropEditorClass);
var
    LI: Integer;
begin
    LI := IndexOfEditor(ATypeInfo, AObjectClass, APropName, AEditorClass);
    if LI = -1 then
        begin
            SetLength(FEditors, Length(FEditors) + 1);
            LI := High(FEditors);
        end;
    with FEditors[LI] do
        begin
            TypeInfo := ATypeInfo;
            ObjectClass := AObjectClass;
            PropName := APropName;
            EditorClass := AEditorClass;
        end;
    Change;
end;

procedure TELCustomPropertyInspector.UnregisterPropEditor(ATypeInfo: PTypeInfo;
    AObjectClass: TClass; const APropName: string; AEditorClass: TELPropEditorClass);
var
    LI, LJ: Integer;
begin
    LI := IndexOfEditor(ATypeInfo, AObjectClass, APropName, AEditorClass);
    if LI <> -1 then
        begin
            for LJ := LI + 1 to High(FEditors) do
                FEditors[LJ - 1] := FEditors[LJ];
            SetLength(FEditors, Length(FEditors) - 1);
        end;
    Change;
end;

function TELCustomPropertyInspector.IndexOfEditor(ATypeInfo: PTypeInfo;
    AObjectClass: TClass; const APropName: string;
    AEditorClass: TELPropEditorClass): Integer;
var
    LI: Integer;
begin
    Result := -1;
    for LI := 0 to High(FEditors) do
        if (FEditors[LI].TypeInfo = ATypeInfo) and
            (FEditors[LI].ObjectClass = AObjectClass) and
            SameText(FEditors[LI].PropName, APropName) and
            (FEditors[LI].EditorClass = AEditorClass) then
            begin
                Result := LI;
                Break;
            end;
end;

procedure TELCustomPropertyInspector.GetComponent(const AComponentName: string;
    var AComponent: TComponent);
begin
    if Assigned(OnGetComponent) then
        OnGetComponent(Self, AComponentName, AComponent);
end;

procedure TELCustomPropertyInspector.GetComponentNames(AClass: TComponentClass;
    AResult: TStrings);
begin
    if Assigned(OnGetComponentNames) then
        OnGetComponentNames(Self, AClass, AResult);
end;

procedure TELCustomPropertyInspector.GetComponentName(AComponent: TComponent;
    var AName: string);
begin
    if Assigned(OnGetComponentName) then
        OnGetComponentName(Self, AComponent, AName);
end;

procedure TELCustomPropertyInspector.SetPropKinds(const Value: TELPropertyInspectorPropKinds);
begin
    if FPropKinds <> Value then
        begin
            FPropKinds := Value;
            Change;
        end;
end;

procedure TELCustomPropertyInspector.FilterProp(AInstance: TPersistent;
    APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
    if Assigned(OnFilterProp) then
        OnFilterProp(Self, AInstance, APropInfo, AIncludeProp);
end;

function TELCustomPropertyInspector.GetItemCaptionColor(
    AItem: TELPropsPageItem): TColor;
begin
    if (TELPropertyInspectorItem(AItem).FEditor <> nil) and
        (praComponentRef in TELPropertyInspectorItem(AItem).FEditor.GetAttrs) then
        Result := ComponentRefColor
    else
        if (AItem.Parent <> nil) and (TELPropertyInspectorItem(AItem.Parent).FEditor <> nil) and
            (praComponentRef in TELPropertyInspectorItem(AItem.Parent).FEditor.GetAttrs) then
            Result := ComponentRefChildColor
        else
            Result := inherited GetItemCaptionColor(AItem);

    if (TELPropertyInspectorItem(AItem).FEditor <> nil) then
        GetCaptionColor(TELPropertyInspectorItem(AItem).FEditor.PropTypeInfo,
            TELPropertyInspectorItem(AItem).FEditor.PropName, Result);
end;

procedure TELCustomPropertyInspector.SetComponentRefColor(
    const Value: TColor);
begin
    if FComponentRefColor <> Value then
        begin
            FComponentRefColor := Value;
            Invalidate;
        end;
end;

procedure TELCustomPropertyInspector.SetComponentRefChildColor(
    const Value: TColor);
begin
    if FComponentRefChildColor <> Value then
        begin
            FComponentRefChildColor := Value;
            Invalidate;
        end;
end;

procedure TELCustomPropertyInspector.SetExpandComponentRefs(
    const Value: Boolean);
begin
    if FExpandComponentRefs <> Value then
        begin
            FExpandComponentRefs := Value;
            Change;
        end;
end;

procedure TELCustomPropertyInspector.SetReadOnly(const Value: Boolean);
begin
    if FReadOnly <> Value then
        begin
            FReadOnly := Value;
            Change;
        end;
end;

procedure TELCustomPropertyInspector.AssignObjects(AObjects: TList);
var
    LI: Integer;
begin
    CheckObjectsLock;
    FObjects.Clear;
    for LI := 0 to AObjects.Count - 1 do
        FObjects.Add(AObjects[LI]);
    Change;
end;

procedure TELCustomPropertyInspector.SetDesigner(const Value: Pointer);
begin
    if FDesigner <> Value then
        begin
            FDesigner := Value;
            Change;
        end;
end;

procedure TELCustomPropertyInspector.GetCaptionColor(
    APropTypeInfo: PTypeInfo; const APropName: string; var AColor: TColor);
begin
    if Assigned(OnGetCaptionColor) then
        OnGetCaptionColor(Self, APropTypeInfo, APropName, AColor);
end;

procedure TELCustomPropertyInspector.InternalModified;
begin
    UpdateItems;
    if Assigned(OnModified) then
        OnModified(Self);
end;

procedure TELCustomPropertyInspector.UpdateItems;

    procedure _UpdateItems(AList: TELObjectList);
    var
        LI: Integer;
    begin
        for LI := 0 to AList.Count - 1 do
            begin
                TELPropertyInspectorItem(AList[LI]).UpdateParams;
                _UpdateItems(TELPropertyInspectorItem(AList[LI]));
            end;
    end;

begin
    BeginUpdate;
    try
        _UpdateItems(Items);
    finally
        EndUpdate;
    end;
end;

procedure TELCustomPropertyInspector.CheckObjectsLock;
begin
    if FObjectsLocked then
        raise EELPropsPage.Create('Property inspector is changing property value. ' +
            'Can not change objects');
end;

{ TELPropEditor }

function TELPropEditor.AllEqual: Boolean;
begin
    Result := FPropCount = 1;
end;

constructor TELPropEditor.Create(ADesigner: Pointer; APropCount: Integer);
begin
    GetMem(FPropList, APropCount * SizeOf(TELPropEditorPropListItem));
    FDesigner := ADesigner;
    FPropCount := APropCount;
end;

destructor TELPropEditor.Destroy;
begin
    if FPropList <> nil then
        FreeMem(FPropList, FPropCount * SizeOf(TELPropEditorPropListItem));
end;

function TELPropEditor.DoGetValue: string;
begin
    if AllEqual then
        Result := GetValue
    else
        Result := '';
end;

procedure TELPropEditor.Edit;
begin
    // Do nothing
end;

function TELPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect];
end;

function TELPropEditor.GetComponent(
    const AComponentName: string): TComponent;
begin
    Result := nil;
    if Assigned(OnGetComponent) then
        OnGetComponent(Self, AComponentName, Result);
end;

function TELPropEditor.GetComponentName(AComponent: TComponent): string;
begin
    Result := AComponent.Name;
    if Assigned(OnGetComponentName) then
        OnGetComponentName(Self, AComponent, Result);
end;

procedure TELPropEditor.GetComponentNames(AClass: TComponentClass;
    AResult: TStrings);
begin
    if Assigned(OnGetComponentNames) then
        OnGetComponentNames(Self, AClass, AResult);
end;

function TELPropEditor.GetFloatValue(AIndex: Integer): Extended;
begin
    with FPropList^[AIndex] do
        Result := GetFloatProp(Instance, PropInfo);
end;

function TELPropEditor.GetInstance(AIndex: Integer): TPersistent;
begin
    Result := FPropList[AIndex].Instance;
end;

function TELPropEditor.GetInt64Value(AIndex: Integer): Int64;
begin
    with FPropList^[AIndex] do
        Result := GetInt64Prop(Instance, PropInfo);
end;

function TELPropEditor.GetOrdValue(AIndex: Integer): Longint;
begin
    with FPropList^[AIndex] do
        Result := GetOrdProp(Instance, PropInfo);
end;

function TELPropEditor.GetPropInfo(AIndex: Integer): PPropInfo;
begin
    Result := FPropList[AIndex].PropInfo;
end;

function TELPropEditor.GetPropName: string;
begin
    Result := FPropList[0].PropInfo^.Name;
end;

function TELPropEditor.GetPropTypeInfo: PTypeInfo;
begin
    Result := FPropList[0].PropInfo^.PropType^;
end;

function TELPropEditor.GetStrValue(AIndex: Integer): string;
begin
    with FPropList^[AIndex] do
        Result := GetStrProp(Instance, PropInfo);
end;

procedure TELPropEditor.GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);
begin
    // Do nothing
end;

function TELPropEditor.GetValue: string;
begin
    Result := '(Unknown)';
end;

procedure TELPropEditor.GetValues(AValues: TStrings);
begin
    // Do nothing
end;

function TELPropEditor.GetVarValue(AIndex: Integer): Variant;
begin
    with FPropList^[AIndex] do
        Result := GetVariantProp(Instance, PropInfo);
end;

procedure TELPropEditor.Modified;
begin
    if Assigned(FOnModified) then
        FOnModified(Self);
end;

procedure TELPropEditor.SetFloatValue(Value: Extended);
var
    LI: Integer;
begin
    for LI := 0 to FPropCount - 1 do
        with FPropList^[LI] do
            SetFloatProp(Instance, PropInfo, Value);
    Modified;
end;

procedure TELPropEditor.SetInt64Value(Value: Int64);
var
    LI: Integer;
begin
    for LI := 0 to FPropCount - 1 do
        with FPropList^[LI] do
            SetInt64Prop(Instance, PropInfo, Value);
    Modified;
end;

procedure TELPropEditor.SetOrdValue(Value: Integer);
var
    LI: Integer;
begin
    for LI := 0 to FPropCount - 1 do
        with FPropList^[LI] do
            SetOrdProp(Instance, PropInfo, Value);
    Modified;
end;

procedure TELPropEditor.SetPropEntry(AIndex: Integer; AInstance: TPersistent;
    APropInfo: PPropInfo);
begin
    with FPropList[AIndex] do
        begin
            Instance := AInstance;
            PropInfo := APropInfo;
        end;
end;

procedure TELPropEditor.SetStrValue(const Value: string);
var
    LI: Integer;
begin
    for LI := 0 to FPropCount - 1 do
        with FPropList^[LI] do
            SetStrProp(Instance, PropInfo, Value);
    Modified;
end;

procedure TELPropEditor.SetValue(const Value: string);
begin
    // Do nothing
end;

procedure TELPropEditor.SetVarValue(const Value: Variant);
var
    LI: Integer;
begin
    for LI := 0 to FPropCount - 1 do
        with FPropList^[LI] do
            SetVariantProp(Instance, PropInfo, Value);
    Modified;
end;

procedure TELPropEditor.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
    // Do nothing
end;

procedure TELPropEditor.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    // Do nothing
end;

procedure TELPropEditor.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    // Do nothing
end;

{ TELNestedPropEditor }

constructor TELNestedPropEditor.Create(AParent: TELPropEditor);
begin
    FPropList := AParent.FPropList;
    FPropCount := AParent.FPropCount;
end;

destructor TELNestedPropEditor.Destroy;
begin
    // Do not execute inherited
end;

function TELNestedPropEditor.GetPropName: string;
begin
    Result := 'SubProp';
end;

{ TELOrdinalPropEditor }

function TELOrdinalPropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LV: Longint;
begin
    Result := True;
    if PropCount > 1 then
        begin
            LV := GetOrdValue(0);
            for LI := 1 to PropCount - 1 do
                if GetOrdValue(LI) <> LV then
                    begin
                        Result := False;
                        Break;
                    end;
        end;
end;

{ TELIntegerPropEditor }

function TELIntegerPropEditor.GetValue: string;
begin
    with GetTypeData(PropTypeInfo)^ do
        if OrdType = otULong then // Unsigned
            Result := IntToStr(Cardinal(GetOrdValue(0)))
        else
            Result := IntToStr(GetOrdValue(0));
end;

procedure TELIntegerPropEditor.SetValue(const Value: string);
var
    LI: Int64;
begin
    LI := StrToInt64(Value);
    with GetTypeData(PropTypeInfo)^ do
        if OrdType = otULong then
            begin // unsigned compare and reporting needed
                if (LI < Cardinal(MinValue)) or (LI > Cardinal(MaxValue)) then
                    // bump up to Int64 to get past the %d in the format string
                    raise EELPropEditor.CreateFmt('Value must be between %d and %d',
                        [Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
            end
        else
            if (LI < MinValue) or (LI > MaxValue) then
                raise EELPropEditor.CreateFmt('Value must be between %d and %d', [MinValue, MaxValue]);
    SetOrdValue(LI);
end;

{ TELCharPropEditor }

function TELCharPropEditor.GetValue: string;
var
    LCh: Char;
begin
    LCh := Chr(GetOrdValue(0));
    if LCh in [#33..#127] then
        Result := LCh
    else
        FmtStr(Result, '#%d', [Ord(LCh)]);
end;

procedure TELCharPropEditor.SetValue(const Value: string);
var
    LI: Longint;
begin
    if Length(Value) = 0 then
        LI := 0
    else
        if Length(Value) = 1 then
            LI := Ord(Value[1])
        else
            if Value[1] = '#' then
                LI := StrToInt(Copy(Value, 2, Maxint))
            else
                raise EELPropEditor.Create('Invalid property value');
    with GetTypeData(PropTypeInfo)^ do
        if (LI < MinValue) or (LI > MaxValue) then
            raise EELPropEditor.CreateFmt('Value must be between %d and %d', [MinValue, MaxValue]);
    SetOrdValue(LI);
end;

{ TELEnumPropEditor }

function TELEnumPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList];
end;

function TELEnumPropEditor.GetValue: string;
var
    LI: Longint;
begin
    LI := GetOrdValue(0);
    with GetTypeData(PropTypeInfo)^ do
        if (LI < MinValue) or (LI > MaxValue) then
            LI := MaxValue;
    Result := GetEnumName(PropTypeInfo, LI);
end;

procedure TELEnumPropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
    LEnumType: PTypeInfo;
begin
    LEnumType := PropTypeInfo;
    with GetTypeData(LEnumType)^ do
        begin
            if MinValue < 0 then // Longbool/Wordbool/Bytebool
                begin
                    AValues.Add(GetEnumName(LEnumType, 0));
                    AValues.Add(GetEnumName(LEnumType, 1));
                end
            else
                for LI := MinValue to MaxValue do
                    AValues.Add(GetEnumName(LEnumType, LI));
        end;
end;

procedure TELEnumPropEditor.SetValue(const Value: string);
var
    LI: Integer;
begin
    LI := GetEnumValue(PropTypeInfo, Value);
    with GetTypeData(PropTypeInfo)^ do
        if (LI < MinValue) or (LI > MaxValue) then
            raise EELPropEditor.Create('Invalid property value');
    SetOrdValue(LI);
end;

{ TELFloatPropEditor }

function TELFloatPropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LV: Extended;
begin
    Result := True;
    if PropCount > 1 then
        begin
            LV := GetFloatValue(0);
            for LI := 1 to PropCount - 1 do
                if GetFloatValue(LI) <> LV then
                    begin
                        Result := False;
                        Break;
                    end;
        end;
end;

function TELFloatPropEditor.GetValue: string;
const
    LPrecisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
    Result := FloatToStrF(GetFloatValue(0), ffGeneral,
        LPrecisions[GetTypeData(PropTypeInfo)^.FloatType], 0);
end;

procedure TELFloatPropEditor.SetValue(const Value: string);
begin
    SetFloatValue(StrToFloat(Value));
end;

{ TELStringPropEditor }

function TELStringPropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LV: string;
begin
    Result := True;
    if PropCount > 1 then
        begin
            LV := GetStrValue(0);
            for LI := 1 to PropCount - 1 do
                if GetStrValue(LI) <> LV then
                    begin
                        Result := False;
                        Break;
                    end;
        end;
end;

function TELStringPropEditor.GetValue: string;
begin
    Result := GetStrValue(0);
end;

procedure TELStringPropEditor.SetValue(const Value: string);
begin
    SetStrValue(Value);
end;

{ TELSetPropEditor }

function TELSetPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praSubProperties, praReadOnly];
end;

procedure TELSetPropEditor.GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);
var
    LI: Integer;
begin
    with GetTypeData(GetTypeData(PropTypeInfo)^.CompType^)^ do
        for LI := MinValue to MaxValue do
            AResult.Add(TELSetElemPropEditor.Create(Self, LI));
end;

function TELSetPropEditor.GetValue: string;
var
    LS: TIntegerSet;
    LTypeInfo: PTypeInfo;
    LI: Integer;
begin
    Integer(LS) := GetOrdValue(0);
    LTypeInfo := GetTypeData(PropTypeInfo)^.CompType^;
    Result := '[';
    for LI := 0 to SizeOf(Integer) * 8 - 1 do
        if LI in LS then
            begin
                if Length(Result) <> 1 then
                    Result := Result + ',';
                Result := Result + GetEnumName(LTypeInfo, LI);
            end;
    Result := Result + ']';
end;

{ TELSetElemPropEditor }

function TELSetElemPropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LS: TIntegerSet;
    LV: Boolean;
begin
    Result := True;
    if PropCount > 1 then
        begin
            Integer(LS) := GetOrdValue(0);
            LV := FElement in LS;
            for LI := 1 to PropCount - 1 do
                begin
                    Integer(LS) := GetOrdValue(LI);
                    if (FElement in LS) <> LV then
                        begin
                            Result := False;
                            Break;
                        end;
                end;
        end;
end;

constructor TELSetElemPropEditor.Create(AParent: TELPropEditor;
    AElement: Integer);
begin
    inherited Create(AParent);
    FElement := AElement;
end;

function TELSetElemPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList];
end;

function TELSetElemPropEditor.GetPropName: string;
begin
    Result := GetEnumName(GetTypeData(PropTypeInfo)^.CompType^, FElement);
end;

function TELSetElemPropEditor.GetValue: string;
var
    LS: TIntegerSet;
begin
    Integer(LS) := GetOrdValue(0);
    Result := BooleanIdents[FElement in LS];
end;

procedure TELSetElemPropEditor.GetValues(AValues: TStrings);
begin
    AValues.Add(BooleanIdents[False]);
    AValues.Add(BooleanIdents[True]);
end;

procedure TELSetElemPropEditor.SetValue(const Value: string);
var
    LS: TIntegerSet;
begin
    Integer(LS) := GetOrdValue(0);
    if CompareText(Value, BooleanIdents[True]) = 0 then
        Include(LS, FElement)
    else
        Exclude(LS, FElement);
    SetOrdValue(Integer(LS));
end;

{ TELClassPropEditor }

function TELClassPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praSubProperties, praReadOnly];
end;

procedure TELClassPropEditor.GetSubProps(AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);
var
    LI: Integer;
    LJ: Integer;
    LObjects: TList;
begin
    LObjects := TList.Create;
    try
        for LI := 0 to PropCount - 1 do
            begin
                LJ := GetOrdValue(LI);
                if LJ <> 0 then
                    LObjects.Add(TObject(LJ));
            end;
        if LObjects.Count > 0 then
            ELGetObjectsProps(Designer, LObjects, tkAny, False, AGetEditorClassProc, AResult);
    finally
        LObjects.Free;
    end;
end;

function TELClassPropEditor.GetValue: string;
begin
    FmtStr(Result, '(%s)', [PropTypeInfo^.Name]);
end;

{ TELInt64PropEditor }

function TELInt64PropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LV: Int64;
begin
    Result := True;
    if PropCount > 1 then
        begin
            LV := GetInt64Value(0);
            for LI := 1 to PropCount - 1 do
                if GetInt64Value(LI) <> LV then
                    begin
                        Result := False;
                        Break;
                    end;
        end;
end;

function TELInt64PropEditor.GetValue: string;
begin
    Result := IntToStr(GetInt64Value(0));
end;

procedure TELInt64PropEditor.SetValue(const Value: string);
begin
    SetInt64Value(StrToInt64(Value));
end;

{ TELVariantPropEditor }

function TELVariantPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praSubProperties];
end;

procedure TELVariantPropEditor.GetSubProps(
    AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);
begin
    AResult.Add(TELVariantTypePropEditor.Create(Self));
end;

function TELVariantPropEditor.GetValue: string;

    function _GetVariantStr(const AValue: Variant): string;
    begin
        case VarType(AValue) of
            varBoolean:
                Result := BooleanIdents[AValue = True];
            varCurrency:
                Result := CurrToStr(AValue);
            else
                if TVarData(AValue).VType <> varNull then
                    Result := AValue
                else
                    Result := SNull;
        end;
    end;

var
    LValue: Variant;

begin
    LValue := GetVarValue(0);
    if VarType(LValue) <> varDispatch then
        Result := _GetVariantStr(LValue)
    else
        Result := 'ERROR';
end;

procedure TELVariantPropEditor.SetValue(const Value: string);

    function _Cast(var AValue: Variant; ANewType: Integer): Boolean;
    var
        LV2: Variant;
    begin
        Result := True;
        if ANewType = varCurrency then
            Result := AnsiPos(CurrencyString, AValue) > 0;
        if Result then
            try
                VarCast(LV2, AValue, ANewType);
                Result := (ANewType = varDate) or (VarToStr(LV2) = VarToStr(AValue));
                if Result then
                    AValue := LV2;
            except
                Result := False;
            end;
    end;

var
    LV: Variant;
    LOldType: Integer;

begin
    LOldType := VarType(GetVarValue(0));
    LV := Value;
    if Value = '' then
        VarClear(LV)
    else
        if (CompareText(Value, SNull) = 0) then
            LV := NULL
        else
            if not _Cast(LV, LOldType) then
                LV := Value;
    SetVarValue(LV);
end;

{ TELVariantTypePropEditor }

function TELVariantTypePropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LV1, LV2: Variant;
begin
    Result := True;
    if PropCount > 1 then
        begin
            LV1 := GetVarValue(0);
            for LI := 1 to PropCount - 1 do
                begin
                    LV2 := GetVarValue(LI);
                    if VarType(LV1) <> VarType(LV2) then
                        begin
                            Result := False;
                            Break;
                        end;
                end;
        end;
end;

function TELVariantTypePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList];
end;

function TELVariantTypePropEditor.GetPropName: string;
begin
    Result := 'Type';
end;

function TELVariantTypePropEditor.GetValue: string;
begin
    case VarType(GetVarValue(0)) and varTypeMask of
        Low(VarTypeNames)..High(VarTypeNames):
            Result := VarTypeNames[VarType(GetVarValue(0))];
        varString:
            Result := SString;
        else
            Result := SUnknown;
    end;
end;

procedure TELVariantTypePropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    for LI := 0 to High(VarTypeNames) do
        if VarTypeNames[LI] <> '' then
            AValues.Add(VarTypeNames[LI]);
    AValues.Add(SString);
end;

procedure TELVariantTypePropEditor.SetValue(const Value: string);

    function _GetSelectedType: Integer;
    var
        LI: Integer;
    begin
        Result := -1;
        for LI := 0 to High(VarTypeNames) do
            if VarTypeNames[LI] = Value then
                begin
                    Result := LI;
                    Break;
                end;
        if (Result = -1) and (Value = SString) then
            Result := varString;
    end;

var
    LNewType: Integer;
    LV: Variant;

begin
    LV := GetVarValue(0);
    LNewType := _GetSelectedType;
    case LNewType of
        varEmpty: VarClear(LV);
        varNull: LV := NULL;
        -1: raise Exception.Create(SUnknownType);
        else
            try
                VarCast(LV, LV, LNewType);
            except
                { If it cannot cast, clear it and then cast again. }
                VarClear(LV);
                VarCast(LV, LV, LNewType);
            end;
    end;
    SetVarValue(LV);
end;

{ TELComponentNamePropEditor }

function TELComponentNamePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praNotNestable];
end;

{ TELDateTimePropEditor }

function TELDateTimePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect];
end;

function TELDateTimePropEditor.GetValue: string;
var
    LDT: TDateTime;
begin
    LDT := GetFloatValue(0);
    if LDT = 0.0 then
        Result := ''
    else
        Result := DateTimeToStr(LDT);
end;

procedure TELDateTimePropEditor.SetValue(const Value: string);
var
    LDT: TDateTime;
begin
    if Value = '' then
        LDT := 0.0
    else
        LDT := StrToDateTime(Value);
    SetFloatValue(LDT);
end;

{ TELDatePropEditor }

function TELDatePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect];
end;

function TELDatePropEditor.GetValue: string;
var
    LDT: TDateTime;
begin
    LDT := GetFloatValue(0);
    if LDT = 0.0 then
        Result := ''
    else
        Result := DateToStr(LDT);
end;

procedure TELDatePropEditor.SetValue(const Value: string);
var
    LDT: TDateTime;
begin
    if Value = '' then
        LDT := 0.0
    else
        LDT := StrToDate(Value);
    SetFloatValue(LDT);
end;

{ TELTimePropEditor }

function TELTimePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect];
end;

function TELTimePropEditor.GetValue: string;
var
    LDT: TDateTime;
begin
    LDT := GetFloatValue(0);
    if LDT = 0.0 then
        Result := ''
    else
        Result := TimeToStr(LDT);
end;

procedure TELTimePropEditor.SetValue(const Value: string);
var
    LDT: TDateTime;
begin
    if Value = '' then
        LDT := 0.0
    else
        LDT := StrToTime(Value);
    SetFloatValue(LDT);
end;

{ TELCaptionPropEditor }

function TELCaptionPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praAutoUpdate];
end;

{ TELColorPropEditor }

procedure TELColorPropEditor.AddValue(const LS: string);
begin
    FValues.Add(LS);
end;

procedure TELColorPropEditor.Edit;
var
    LColorDialog: TColorDialog;
begin
    LColorDialog := TColorDialog.Create(Application);
    try
        LColorDialog.Color := GetOrdValue(0);
        LColorDialog.Options := [];
        if LColorDialog.Execute then
            SetOrdValue(LColorDialog.Color);
    finally
        LColorDialog.Free;
    end;
end;

function TELColorPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praDialog, praValueList, praOwnerDrawValues];
end;

function TELColorPropEditor.GetValue: string;
begin
    Result := ColorToString(TColor(GetOrdValue(0)));
end;

procedure TELColorPropEditor.GetValues(AValues: TStrings);
begin
    FValues := AValues;
    GetColorValues(AddValue);
end;

procedure TELColorPropEditor.SetValue(const Value: string);
var
    LNewValue: Longint;
begin
    if IdentToColor(Value, LNewValue) then
        SetOrdValue(LNewValue)
    else
        inherited SetValue(Value);
end;

procedure TELColorPropEditor.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    function _ColorToBorderColor(AColor: TColor): TColor;
    type
        TColorQuad = record
            Red,
                Green,
                Blue,
                Alpha: Byte;
        end;
    begin
        if (TColorQuad(AColor).Red > 192) or
            (TColorQuad(AColor).Green > 192) or
            (TColorQuad(AColor).Blue > 192) then
            Result := clBlack
        else
            if ASelected then
                Result := clWhite
            else
                Result := AColor;
    end;

var
    LRight: Integer;
    LOldPenColor, LOldBrushColor: TColor;

begin
    LRight := (ARect.Bottom - ARect.Top) + ARect.Left;
    with ACanvas do
        begin
            LOldPenColor := Pen.Color;
            LOldBrushColor := Brush.Color;
            Pen.Color := Brush.Color;
            Rectangle(ARect.Left, ARect.Top, LRight, ARect.Bottom);
            Brush.Color := StringToColor(AValue);
            Pen.Color := _ColorToBorderColor(ColorToRGB(Brush.Color));
            Rectangle(ARect.Left + 1, ARect.Top + 1, LRight - 1, ARect.Bottom - 1);
            Brush.Color := LOldBrushColor;
            Pen.Color := LOldPenColor;
            ACanvas.TextRect(
                Rect(LRight, ARect.Top, ARect.Right, ARect.Bottom),
                LRight + 1,
                ARect.Top + 1,
                AValue
                );
        end;
end;

procedure TELColorPropEditor.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TELColorPropEditor.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    AWidth := AWidth + ACanvas.TextHeight('Wg');
end;

{ TELCursorPropEditor }

procedure TELCursorPropEditor.AddValue(const LS: string);
begin
    FValues.Add(LS);
end;

function TELCursorPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList, praOwnerDrawValues];
end;

function TELCursorPropEditor.GetValue: string;
begin
    Result := CursorToString(TCursor(GetOrdValue(0)));
end;

procedure TELCursorPropEditor.GetValues(AValues: TStrings);
begin
    FValues := AValues;
    GetCursorValues(AddValue);
end;

procedure TELCursorPropEditor.SetValue(const Value: string);
var
    LNewValue: Longint;
begin
    if IdentToCursor(Value, LNewValue) then
        SetOrdValue(LNewValue)
    else
        inherited SetValue(Value);
end;

procedure TELCursorPropEditor.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
    LRight: Integer;
    LCursorIndex: Integer;
    LCursorHandle: THandle;
begin
    LRight := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;
    with ACanvas do
        begin
            if not IdentToCursor(AValue, LCursorIndex) then
                LCursorIndex := StrToInt(AValue);
            ACanvas.FillRect(ARect);
            LCursorHandle := Screen.Cursors[LCursorIndex];
            if LCursorHandle <> 0 then
                DrawIconEx(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, LCursorHandle,
                    0, 0, 0, 0, DI_NORMAL or DI_DEFAULTSIZE);
            ACanvas.TextRect(
                Rect(LRight, ARect.Top, ARect.Right, ARect.Bottom),
                LRight + 1,
                ARect.Top + 1,
                AValue
                );
        end;
end;

procedure TELCursorPropEditor.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
var
    LTextHeight, LCursorHeight: Integer;
begin
    LTextHeight := ACanvas.TextHeight('Wg');
    LCursorHeight := GetSystemMetrics(SM_CYCURSOR) + 4;
    if LTextHeight >= LCursorHeight then
        AHeight := LTextHeight
    else
        AHeight := LCursorHeight;
end;

procedure TELCursorPropEditor.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

{ TELFontCharsetPropEditor }

procedure TELFontCharsetPropEditor.AddValue(const LS: string);
begin
    FValues.Add(LS);
end;

function TELFontCharsetPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praSortList, praValueList];
end;

function TELFontCharsetPropEditor.GetValue: string;
begin
    if not CharsetToIdent(TFontCharset(GetOrdValue(0)), Result) then
        FmtStr(Result, '%d', [GetOrdValue(0)]);
end;

procedure TELFontCharsetPropEditor.GetValues(AValues: TStrings);
begin
    FValues := AValues;
    GetCharsetValues(AddValue);
end;

procedure TELFontCharsetPropEditor.SetValue(const Value: string);
var
    LNewValue: Longint;
begin
    if IdentToCharset(Value, LNewValue) then
        SetOrdValue(LNewValue)
    else
        inherited SetValue(Value);
end;

{ TELFontNamePropEditor }

function TELFontNamePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList, praSortList];
end;

procedure TELFontNamePropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    for LI := 0 to Screen.Fonts.Count - 1 do
        AValues.Add(Screen.Fonts[LI]);
end;

{ TELImeNamePropEditor }

function TELImeNamePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praValueList, praSortList, praMultiSelect];
end;

procedure TELImeNamePropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    for LI := 0 to Screen.Imes.Count - 1 do
        AValues.Add(Screen.Imes[LI]);
end;

{ TELFontPropEditor }

procedure TELFontPropEditor.Edit;
var
    LFontDialog: TFontDialog;
begin
    LFontDialog := TFontDialog.Create(Application);
    try
        LFontDialog.Font := TFont(GetOrdValue(0));
        LFontDialog.Options := LFontDialog.Options + [fdForceFontExist];
        if LFontDialog.Execute then
            SetOrdValue(Longint(LFontDialog.Font));
    finally
        LFontDialog.Free;
    end;
end;

function TELFontPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praSubProperties, praDialog, praReadOnly];
end;

{ TELModalResultPropEditor }

function TELModalResultPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList];
end;

function TELModalResultPropEditor.GetValue: string;
var
    LCurValue: Longint;
begin
    LCurValue := GetOrdValue(0);
    case LCurValue of
        Low(ModalResults)..High(ModalResults):
            Result := ModalResults[LCurValue];
        else
            Result := IntToStr(LCurValue);
    end;
end;

procedure TELModalResultPropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    for LI := Low(ModalResults) to High(ModalResults) do
        AValues.Add(ModalResults[LI]);
end;

procedure TELModalResultPropEditor.SetValue(const Value: string);
var
    LI: Integer;
begin
    if Value = '' then
        begin
            SetOrdValue(0);
            Exit;
        end;
    for LI := Low(ModalResults) to High(ModalResults) do
        if CompareText(ModalResults[LI], Value) = 0 then
            begin
                SetOrdValue(LI);
                Exit;
            end;
    inherited SetValue(Value);
end;

{ TELPenStylePropEditor }

function TELPenStylePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := inherited GetAttrs + [praOwnerDrawValues];
end;

procedure TELPenStylePropEditor.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
    LRight, LTop: Integer;
    LOldPenColor, LOldBrushColor: TColor;
    LOldPenStyle: TPenStyle;
begin
    LRight := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
    LTop := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
    with ACanvas do
        begin
            LOldPenColor := Pen.Color;
            LOldBrushColor := Brush.Color;
            LOldPenStyle := Pen.Style;
            Pen.Color := Brush.Color;
            Rectangle(ARect.Left, ARect.Top, LRight, ARect.Bottom);
            Pen.Color := clWindowText;
            Brush.Color := clWindow;
            Rectangle(ARect.Left + 1, ARect.Top + 1, LRight - 1, ARect.Bottom - 1);
            Pen.Color := clWindowText;
            Pen.Style := TPenStyle(GetEnumValue(PropTypeInfo, AValue));
            MoveTo(ARect.Left + 1, LTop);
            LineTo(LRight - 1, LTop);
            MoveTo(ARect.Left + 1, LTop + 1);
            LineTo(LRight - 1, LTop + 1);
            Brush.Color := LOldBrushColor;
            Pen.Style := LOldPenStyle;
            Pen.Color := LOldPenColor;
            ACanvas.TextRect(
                Rect(LRight, ARect.Top, ARect.Right, ARect.Bottom),
                LRight + 1,
                ARect.Top + 1,
                AValue
                );
        end;
end;

procedure TELPenStylePropEditor.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TELPenStylePropEditor.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    AWidth := AWidth + ACanvas.TextHeight('Wg') * 2;
end;

{ TELBrushStylePropEditor }

function TELBrushStylePropEditor.GetAttrs: TELPropAttrs;
begin
    Result := inherited GetAttrs + [praOwnerDrawValues];
end;

procedure TELBrushStylePropEditor.ValuesDrawValue(const AValue: string;
    ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
    LRight: Integer;
    LOldPenColor, LOldBrushColor: TColor;
    LOldBrushStyle: TBrushStyle;
begin
    LRight := (ARect.Bottom - ARect.Top) + ARect.Left;
    with ACanvas do
        begin
            LOldPenColor := Pen.Color;
            LOldBrushColor := Brush.Color;
            LOldBrushStyle := Brush.Style;
            Pen.Color := Brush.Color;
            Brush.Color := clWindow;
            Rectangle(ARect.Left, ARect.Top, LRight, ARect.Bottom);
            Pen.Color := clWindowText;
            Brush.Style := TBrushStyle(GetEnumValue(PropTypeInfo, AValue));
            if Brush.Style = bsClear then
                begin
                    Brush.Color := clWindow;
                    Brush.Style := bsSolid;
                end
            else
                Brush.Color := clWindowText;
            Rectangle(ARect.Left + 1, ARect.Top + 1, LRight - 1, ARect.Bottom - 1);
            Brush.Color := LOldBrushColor;
            Brush.Style := LOldBrushStyle;
            Pen.Color := LOldPenColor;
            ACanvas.TextRect(
                Rect(LRight, ARect.Top, ARect.Right, ARect.Bottom),
                LRight + 1,
                ARect.Top + 1,
                AValue
                );
        end;
end;

procedure TELBrushStylePropEditor.ValuesMeasureHeight(const AValue: string;
    ACanvas: TCanvas; var AHeight: Integer);
begin
    AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TELBrushStylePropEditor.ValuesMeasureWidth(const AValue: string;
    ACanvas: TCanvas; var AWidth: Integer);
begin
    AWidth := AWidth + ACanvas.TextHeight('Wg') * 2;
end;

{ TELTabOrderPropEditor }

function TELTabOrderPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [];
end;

{ TELShortCutPropEditor }

function TELShortCutPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praValueList];
end;

function TELShortCutPropEditor.GetValue: string;
var
    LCurValue: TShortCut;
begin
    LCurValue := GetOrdValue(0);
    if LCurValue = scNone then
        Result := srNone
    else
        Result := ShortCutToText(LCurValue);
end;

procedure TELShortCutPropEditor.GetValues(AValues: TStrings);
var
    LI: Integer;
begin
    AValues.Add(srNone);
    for LI := 1 to High(ShortCuts) do
        AValues.Add(ShortCutToText(ShortCuts[LI]));
end;

procedure TELShortCutPropEditor.SetValue(const Value: string);
var
    LNewValue: TShortCut;
begin
    LNewValue := 0;
    if (Value <> '') and (AnsiCompareText(Value, srNone) <> 0) then
        begin
            LNewValue := TextToShortCut(Value);
            if LNewValue = 0 then
                raise EELPropEditor.Create('Invalid property value');
        end;
    SetOrdValue(LNewValue);
end;

{ TELComponentPropEditor }

function TELComponentPropEditor.AllEqual: Boolean;
var
    LI: Integer;
    LInstance: TComponent;
begin
    Result := True;
    LInstance := TComponent(GetOrdValue(0));
    if PropCount > 1 then
        for LI := 1 to PropCount - 1 do
            if TComponent(GetOrdValue(LI)) <> LInstance then
                begin
                    Result := False;
                    Break;
                end;
end;

function TELComponentPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praComponentRef];
    if Assigned(GetPropInfo(0).SetProc) then
        Result := Result + [praValueList, praSortList]
    else
        Result := Result + [praReadOnly];
    if (TComponent(GetOrdValue(0)) <> nil) and AllEqual then
        Result := Result + [praSubProperties, praVolatileSubProperties];
end;

procedure TELComponentPropEditor.GetSubProps(
    AGetEditorClassProc: TELGetEditorClassProc; AResult: TList);
var
    LI: Integer;
    LJ: Integer;
    LObjects: TList;
begin
    LObjects := TList.Create;
    try
        for LI := 0 to PropCount - 1 do
            begin
                LJ := GetOrdValue(LI);
                if LJ <> 0 then
                    LObjects.Add(TObject(LJ));
            end;
        if LObjects.Count > 0 then
            ELGetObjectsProps(Designer, LObjects, tkAny, True, AGetEditorClassProc, AResult);
    finally
        LObjects.Free;
    end;
end;

function TELComponentPropEditor.GetValue: string;
var
    LComponent: TComponent;
begin
    LComponent := TComponent(GetOrdValue(0));
    if LComponent <> nil then
        Result := GetComponentName(LComponent)
    else
        Result := '';
end;

procedure TELComponentPropEditor.GetValues(AValues: TStrings);
begin
    GetComponentNames(TComponentClass(GetTypeData(PropTypeInfo)^.ClassType), AValues);
end;

procedure TELComponentPropEditor.SetValue(const Value: string);
var
    LComponent: TComponent;
begin
    LComponent := nil;
    if Value <> '' then
        begin
            LComponent := GetComponent(Value);
            if not (LComponent is GetTypeData(PropTypeInfo)^.ClassType) then
                raise EPropertyError.Create('Invalid property value');
        end;
    SetOrdValue(LongInt(LComponent));
end;

{ TELStringsPropEditor }

procedure TELStringsPropEditor.Edit;
var
    LStringsEditorDlg: TELStringsEditorDlg;
begin
    LStringsEditorDlg := TELStringsEditorDlg.Create(Application);
    try
        LStringsEditorDlg.Lines := TStrings(GetOrdValue(0));
        if LStringsEditorDlg.Execute then
            SetOrdValue(Longint(LStringsEditorDlg.Lines));
    finally
        LStringsEditorDlg.Free;
    end;
end;

function TELStringsPropEditor.GetAttrs: TELPropAttrs;
begin
    Result := [praMultiSelect, praDialog, praReadOnly];
end;

end.

