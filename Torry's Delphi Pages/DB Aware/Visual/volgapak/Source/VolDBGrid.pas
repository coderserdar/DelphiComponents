//---------------------------------------------------------------------------
//  TVolgaDBGrid - inherited from TCustomGrid
//  Column Button Style may be cbsCombo,cbsLookup,cbsCalendar,cbsCheck,cbsEllipsis
//  cbsCalculator
//  Combo dropdown list supports combo values
//  Lookup dropdown list without adding calculated TField
//  Dropdown list width may be greater then width of column
//  Title columns as buttons, editable calculated fields
//  FixedCols, RowsHeight, TitleHeight, WrapText, Enter as Tab
//  Uses TVolgaCalendar and TVolgaCalculator for dropdown controls
//---------------------------------------------------------------------------
//  Copyright © 2000-2004, Olga Vlasova, Russia
//  http://www.volgadb.com
//  E-mail: info@volgadb.com
//---------------------------------------------------------------------------
// Some changes are added by Armando Novello
//---------------------------------------------------------------------------

unit VolDBGrid;

interface

uses Windows, SysUtils, Messages, Classes, Controls, Forms, StdCtrls,
{$IFDEF VER140}Variants, {$ENDIF}
{$IFDEF VER150}Variants, {$ENDIF}
  Graphics, Grids, DBCtrls, Db, Menus, ImgList, VolCalend, VolDBConst, VolCalc, ShellApi;

type
  TVolgaColumnValue = (cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly,
    cVolgaTitleColor,
    cVolgaTitleCaption, cVolgaTitleAlignment, cVolgaTitleFont, cvImeMode, cvImeName);
  TVolgaColumnValues = set of TVolgaColumnValue;
  TVolgaSortMark = (vsmNone, vsmDown, vsmUp);

const
  ColumnTitleValues = [cVolgaTitleColor..cVolgaTitleFont];
  cm_DeferLayout = WM_USER + 100;

type
  TVolgaColumn = class;
  TVolgaCustomDBGrid = class;

  TVolgaColumnTitle = class(TPersistent)
  private
    FColumn: TVolgaColumn;
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FAlignment: TAlignment;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetCaption: string;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string); virtual;
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TVolgaColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
    property Column: TVolgaColumn read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment
    stored IsAlignmentStored;
    property Caption: string
    read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  TVolgaColumnButtonStyle = (cbsCombo, cbsLookup, cbsCalendar, cbsCheck, cbsEllipsis,
    cbsCalculator, cbsNone);            //Volga

  TVolgaColumn = class(TCollectionItem)
  private
    FField: TField;
    FFieldName: string;
    FColor: TColor;
    FWidth: Integer;
    FTitle: TVolgaColumnTitle;
    FFont: TFont;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FPickList: TStrings;
    FPickValues: TStrings;              //Volga
    FPopupMenu: TPopupMenu;
    FDropDownRows: Cardinal;
    FDropDownWidth: Cardinal;           //Volga
    FCanClick: Boolean;                 //Volga
    FDown: Boolean;                     //Volga
    FWrap: Boolean;                     //Volga
    FButtonStyle: TVolgaColumnButtonStyle;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TVolgaColumnValues;
    FVisible: Boolean;
    FExpanded: Boolean;
    FStored: Boolean;
    FValueUnChecked: string;
    FValueChecked: string;
    FLookupDropDownFields: string;
    FLookupKeyField: string;
    FLookupLinkField: string;
    FLookupDataSet: TDataSet;
    FViewField: string;
    FAutoDrop: Boolean;
    FTextAsHint: Boolean;               //volga
    FSortMark: TVolgaSortMark;          //volga
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetExpanded: Boolean;
    function GetField: TField;
    function GetFont: TFont;
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    function GetParentColumn: TVolgaColumn;
    function GetPickList: TStrings;
    function GetPickValues: TStrings;   //Volga
    function GetReadOnly: Boolean;
    function GetShowing: Boolean;
    function GetWidth: Integer;
    function GetVisible: Boolean;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsImeModeStored: Boolean;
    function IsImeNameStored: Boolean;
    function IsReadOnlyStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetButtonStyle(Value: TVolgaColumnButtonStyle);
    procedure SetColor(Value: TColor);
    procedure SetExpanded(Value: Boolean);
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: string);
    procedure SetFont(Value: TFont);
    procedure SetImeMode(Value: TImeMode); virtual;
    procedure SetImeName(Value: TImeName); virtual;
    procedure SetPickList(Value: TStrings);
    procedure SetPickValues(Value: TStrings); //Volga
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetTitle(Value: TVolgaColumnTitle);
    procedure SetWidth(Value: Integer); virtual;
    procedure SetVisible(Value: Boolean);
    function GetExpandable: Boolean;
    procedure SetValueChecked(const Value: string);
    procedure SetValueUnChecked(const Value: string);
    procedure SetLookupDropDownFields(const Value: string);
    procedure SetLookupKeyField(const Value: string);
    procedure SetLookupLinkField(const Value: string);
    procedure SetLookupDataSet(const Value: TDataSet);
    procedure SetWrap(const Value: Boolean); //volga
  protected
    function CreateTitle: TVolgaColumnTitle; virtual;
    function GetGrid: TVolgaCustomDBGrid;
    function GetDisplayName: string; override;
    procedure RefreshDefaultFont;
    procedure SetIndex(Value: Integer); override;
    property IsStored: Boolean read FStored write FStored default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultImeMode: TImeMode;
    function DefaultImeName: TImeName;
    function DefaultReadOnly: Boolean;
    function DefaultWidth: Integer;
    function Depth: Integer;
    function IsLinkActive: Boolean;
    procedure RestoreDefaults; virtual;
    procedure AssignList(const AList: TStrings);
    property AssignedValues: TVolgaColumnValues read FAssignedValues;
    property Down: Boolean read FDown write FDown;
    property Expandable: Boolean read GetExpandable;
    property Grid: TVolgaCustomDBGrid read GetGrid;
    property Field: TField read GetField write SetField;
    property ImeMode: TImeMode read GetImeMode write SetImeMode stored IsImeModeStored;
    property ImeName: TImeName read GetImeName write SetImeName stored IsImeNameStored;
    property ParentColumn: TVolgaColumn read GetParentColumn;
    property Expanded: Boolean read GetExpanded write SetExpanded default False;
    property Showing: Boolean read GetShowing;
    property SortMark: TVolgaSortMark read FSortMark write FSortMark;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment
    stored IsAlignmentStored;
    property AutoDropDown: Boolean read FAutoDrop write FAutoDrop default false; //Volga
    property ButtonStyle: TVolgaColumnButtonStyle read FButtonStyle write SetButtonStyle
    default cbsNone;
    property CanClick: Boolean read FCanClick write FCanClick default false; //Volga
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 10;
    property DropDownWidth: Cardinal read FDropDownWidth write FDropDownWidth default 0;  //Volga
    property FieldName: string read FFieldName write SetFieldName;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property LongTextAsHint: Boolean read FTextAsHint write FTextAsHint default false;  //Volga
    property LookupDropDownFields: string read FLookupDropDownFields write
    SetLookupDropDownFields;            //Volga
    property LookupKeyField: string read FLookupKeyField write SetLookupKeyField; //Volga
    property LookupLinkField: string read FLookupLinkField write SetLookupLinkField;  //Volga
    property LookupDataSet: TDataSet read FLookupDataSet write SetLookupDataSet; //Volga
    property PickList: TStrings read GetPickList write SetPickList;
    property PickValues: TStrings read GetPickValues write SetPickValues; //Volga
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly
    stored IsReadOnlyStored;
    property Title: TVolgaColumnTitle read FTitle write SetTitle;
    property ValueChecked: string read FValueChecked write SetValueChecked; //Volga
    property ValueUnChecked: string read FValueUnChecked write SetValueUnChecked; //Volga
    property Visible: Boolean read GetVisible write SetVisible;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property WrapText: Boolean read FWrap write SetWrap default false; //Volga
  end;

  TVolgaColumnClass = class of TVolgaColumn;

  TVolgaDBGridColumnsState = (csDefault, csCustomized);

  TVolgaDBGridColumns = class(TOwnedCollection)
  private
    FGrid: TVolgaCustomDBGrid;
    function GetColumn(Index: Integer): TVolgaColumn;
    function InternalAdd: TVolgaColumn;
    procedure SetColumn(Index: Integer; Value: TVolgaColumn);
    procedure SetState(NewState: TVolgaDBGridColumnsState);
    function GetState: TVolgaDBGridColumnsState;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TVolgaCustomDBGrid; ColumnClass: TVolgaColumnClass);
//    function Add: TVolgaColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RestoreDefaults;
    procedure RebuildColumns;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property State: TVolgaDBGridColumnsState read GetState write SetState;
    property Grid: TVolgaCustomDBGrid read FGrid;
    property Items[Index: Integer]: TVolgaColumn read GetColumn write SetColumn; default;
  end;

  TGridDataLink = class(TDataLink)
  private
    FGrid: TVolgaCustomDBGrid;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FSparseMap: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
  protected
    procedure ActiveChanged; override;
    procedure BuildAggMap;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure EditingChanged; override;
    function IsAggRow(Value: Integer): Boolean; virtual;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    function GetMappedIndex(ColIndex: Integer): Integer;
  public
    constructor Create(AGrid: TVolgaCustomDBGrid);
    destructor Destroy; override;
    function AddMapping(const FieldName: string): Boolean;
    procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
  end;

  TBookmarkList = class
  private
    FList: TStringList;
    FGrid: TVolgaCustomDBGrid;
    FCache: TBookmarkStr;
    FCacheIndex: Integer;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    function GetCount: Integer;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Integer): TBookmarkStr;
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure StringsChanged(Sender: TObject);
  protected
    function CurrentRow: TBookmarkStr;
    function Compare(const Item1, Item2: TBookmarkStr): Integer;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create(AGrid: TVolgaCustomDBGrid);
    destructor Destroy; override;
    procedure Clear;                    // free all bookmarks
    procedure Delete;                   // delete all selected rows from dataset
    function Find(const Item: TBookmarkStr; var Index: Integer): Boolean;
    function IndexOf(const Item: TBookmarkStr): Integer;
    function Refresh: Boolean;          // drop orphaned bookmarks; True = orphans found
    property Count: Integer read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected
    write SetCurrentRowSelected;
    property Items[Index: Integer]: TBookmarkStr read GetItem; default;
  end;

  TVolgaDBGridOption = (dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator,
    dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect,
    dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit, dgMultiSelect,
    dgDragOutRows, dgEnterToTab);
  TVolgaDBGridOptions = set of TVolgaDBGridOption;
  TVolgaRowType = (rtTitle, rtFirstVisible, rtCurrent, rtLastVisible); //volga

  TCalcFieldEditedEvent = procedure(Sender: TObject; Field: TField; //Volga
    Text: string) of object;            //Volga
  TDrawDataCellEvent = procedure(Sender: TObject; const Rect: TRect; Field: TField;
    State: TGridDrawState) of object;
  TDrawTitleAttrEvent = procedure(Sender: TObject; Column: TVolgaColumn; AFont: TFont;
    var AColor: TColor) of object;
  TDrawCellAttrEvent = procedure(Sender: TObject; Column: TVolgaColumn; AFont: TFont;
    var AColor: TColor; State: TGridDrawState) of object;
  TDrawColumnCellEvent = procedure(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TVolgaColumn; State: TGridDrawState) of object;
  TVolgaDBGridClickEvent = procedure(Sender: TObject; Column: TVolgaColumn) of object;  //Volga
  TVolgaDBGridCloseUpEvent = procedure(Sender: TObject; Column: TVolgaColumn;
    Selected: Boolean) of object;       //Volga
  TVolgaSpecialKeyDownEvent = procedure(Sender: TObject; Column: TVolgaColumn;
    var Key: Word; AText: string) of object; //Volga

  TVolgaCustomDBGrid = class(TCustomGrid)
  private
    FColMoved: Boolean;                 //added by Armando Novello
    FClrSMarkOnClick: Boolean;          //added by Armando Novello
    DrawBitmap: TBitmap;
    UserCount: Integer;
    FIndicators: TImageList;
    FTitleFont: TFont;
    FReadOnly: Boolean;
    FOriginalImeName: TImeName;
    FOriginalImeMode: TImeMode;
    FUserChange: Boolean;
    FIsESCKey: Boolean;
    FLayoutFromDataset: Boolean;
    FOptions: TVolgaDBGridOptions;
    FTitleOffset, FIndicatorOffset: Byte;
    FUpdateLock: Byte;
    FLayoutLock: Byte;
    FInColExit: Boolean;
    FDefaultDrawing: Boolean;
    FSelfChangingTitleFont: Boolean;
    FSelecting: Boolean;
    FSelRow: Integer;
    FDataLink: TGridDataLink;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FOnDrawDataCell: TDrawDataCellEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnDrawTitleAttr: TDrawTitleAttrEvent;
    FOnDrawCellAttr: TDrawCellAttrEvent;
    FEditText: string;
    FColumns: TVolgaDBGridColumns;
    FVisibleColumns: TList;
    FBookmarks: TBookmarkList;
    FSelectionAnchor: TBookmarkStr;
    FOnEditButtonClick: TVolgaDBGridClickEvent;
    FOnColumnMoved: TMovedEvent;
    FOnCalcFieldEdited: TCalcFieldEditedEvent; //Volga
    FOnCellClick: TVolgaDBGridClickEvent;
    FOnTitleClick: TVolgaDBGridClickEvent;
    FDragCol: TVolgaColumn;
    FFixedCols: integer;                //Volga
    FTitleHeight: integer;              //Volga
    FRowsHeight: integer;               //Volga
    FAllowInsert: Boolean;              //Volga
    FAllowDelete: Boolean;              //Volga
    FBeforeDropDown: TVolgaDBGridClickEvent; //Volga
    FAfterCloseUp: TVolgaDBGridCloseUpEvent; //Volga
    FSpecialKey: TVolgaSpecialKeyDownEvent; //volga
    FOnMultiSelect: TNotifyEvent;       //Volga
    FOnColumnResized: TVolgaDBGridClickEvent; //Volga
    FHColor: TColor;                    //volga
    FHTColor: TColor;                   //Volga
    procedure UsesBitmap;
    procedure ReleaseBitmap;
    procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
      const Text: string; Alignment: TAlignment; Wrap: Boolean; ARightToLeft: Boolean);
    function AcquireFocus: Boolean;
    procedure DataChanged;
    procedure EditingChanged;
    function GetDataSource: TDataSource;
    function GetFieldCount: Integer;
    function GetFields(FieldIndex: Integer): TField;
    function GetSelectedField: TField;
    function GetSelectedIndex: Integer;
    procedure InternalLayout;
    procedure MoveCol(RawCol, Direction: Integer);
    function PtInExpandButton(X, Y: Integer; var MasterCol: TVolgaColumn): Boolean;
    procedure ReadColumns(Reader: TReader);
    procedure RecordChanged(Field: TField);
    procedure SetIme;
    procedure SetColumns(Value: TVolgaDBGridColumns);
    procedure SetDataSource(Value: TDataSource);
    procedure SetOptions(Value: TVolgaDBGridOptions);
    procedure SetSelectedField(Value: TField);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetFixedCols(Value: Integer); //Volga
    function GetFixedCols: Integer;     //Volga
    procedure SetTitleFont(Value: TFont);
    procedure TitleFontChanged(Sender: TObject);
    procedure ToggleCheckBox(Column: TVolgaColumn; col, row: integer);
    procedure UpdateData;
    procedure UpdateActive;
    procedure UpdateIme;
    procedure UpdateScrollBar;
    procedure UpdateRowCount;
    procedure WriteColumns(Writer: TWriter);
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMDeferLayout(var Message); message cm_DeferLayout;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SetFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure SetAllowDelete(const Value: Boolean);
    procedure SetAllowInsert(const Value: Boolean);
    procedure SetHColor(const Value: TColor); //volga
    procedure SetHTColor(const Value: TColor); //volga
    procedure SetTitleHeight(const Value: integer); //volga
    procedure SetRowsHeight(const Value: integer); //volga
  protected
    FUpdateFields: Boolean;
    FAcquireFocus: Boolean;
    function RawToDataColumn(ACol: Integer): Integer;
    function DataToRawColumn(ACol: Integer): Integer;
    function AcquireLayoutLock: Boolean;
    procedure BeginLayout;
    procedure BeginUpdate;
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;
    procedure CancelLayout;
    function CanEditAcceptKey(Key: Char): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    procedure CellClick(Column: TVolgaColumn); dynamic;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    function CalcTitleRect(Col: TVolgaColumn; ARow: Integer;
      var MasterCol: TVolgaColumn): TRect;
    function ColumnAtDepth(Col: TVolgaColumn; ADepth: Integer): TVolgaColumn;
    procedure ColEnter; dynamic;
    procedure ColExit; dynamic;
    procedure ColWidthsChanged; override;
    function CreateColumns: TVolgaDBGridColumns; dynamic;
    function CreateEditor: TInplaceEdit; override;
    procedure CreateWnd; override;
    procedure DeferLayout;
    procedure DefineFieldMap; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefaultDrawDataCell(const Rect: TRect; Field: TField;
      State: TGridDrawState);
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;  //volga
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;  //volga
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
      override;
    procedure DrawDataCell(const Rect: TRect; Field: TField;
      State: TGridDrawState);
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TVolgaColumn; State: TGridDrawState); dynamic;
    procedure EditButtonClick; dynamic;
    procedure EndLayout;
    procedure EndUpdate;
    function GetColField(DataCol: Integer): TField;
    function GetEditLimit: Integer; override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    function GetFieldValue(ACol: Integer): string;
    function HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure InvalidateTitles;
    procedure LayoutChanged; virtual;
    procedure LinkActive(Value: Boolean); virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(Distance: Integer); virtual;
    procedure SetColumnAttributes; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function StoreColumns: Boolean;
    procedure TimedScroll(Direction: TGridScrollDirection); override;
    procedure TitleClick(Column: TVolgaColumn); dynamic;
    procedure TopLeftChanged; override;
    function UseRightToLeftAlignmentForField(const AField: TField;
      Alignment: TAlignment): Boolean;
    function BeginColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function EndColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    property AllowDelete: Boolean read FAllowDelete write SetAllowDelete default True;
    property AllowInsert: Boolean read FAllowInsert write SetAllowInsert default True;
    property Columns: TVolgaDBGridColumns read FColumns write SetColumns;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default
    True;
    property DataLink: TGridDataLink read FDataLink;
    property IndicatorOffset: Byte read FIndicatorOffset;
    property LayoutLock: Byte read FLayoutLock;
    property Options: TVolgaDBGridOptions read FOptions write SetOptions
    default [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines,
      dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit];
    property ParentColor default False;
    property HighlightColor: TColor read FHColor write SetHColor default clHighlight;  //volga
    property HighlightTextColor: TColor read FHTColor write SetHTColor default
    clHighlightText;                    //volga
    property FixedCols: integer read GetFixedCols write SetFixedCols default 0; //Volga
    property TitleHeight: integer read FTitleHeight write SetTitleHeight default 0;  //Volga
    property RowsHeight: integer read FRowsHeight write SetRowsHeight default 0; //Volga
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property SelectedRows: TBookmarkList read FBookmarks;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property UpdateLock: Byte read FUpdateLock;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnDrawDataCell: TDrawDataCellEvent read FOnDrawDataCell
    write FOnDrawDataCell;              { obsolete }
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell
    write FOnDrawColumnCell;
    property OnDrawTitleAttr: TDrawTitleAttrEvent read FOnDrawTitleAttr
    write FOnDrawTitleAttr;
    property OnDrawCellAttr: TDrawCellAttrEvent read FOnDrawCellAttr
    write FOnDrawCellAttr;
    property OnEditButtonClick: TVolgaDBGridClickEvent read FOnEditButtonClick
    write FOnEditButtonClick;
    property OnCalcFieldEdited: TCalcFieldEditedEvent read FOnCalcFieldEdited //Volga
    write FOnCalcFieldEdited;           //Volga
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnCellClick: TVolgaDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnTitleClick: TVolgaDBGridClickEvent read FOnTitleClick write FOnTitleClick;
    property BeforeDropDown: TVolgaDBGridClickEvent read FBeforeDropDown write
    FBeforeDropDown;                    //Volga
    property AfterCloseUp: TVolgaDBGridCloseUpEvent read FAfterCloseUp write
    FAfterCloseUp;                      //Volga
    property OnSpecialKeyDown: TVolgaSpecialKeyDownEvent read FSpecialKey write
    FSpecialKey;                        //Volga
    property OnMultiSelectChanged: TNotifyEvent read FOnMultiSelect write FOnMultiSelect;  //Volga
    property OnColumnResized: TVolgaDBGridClickEvent read FOnColumnResized write
    FOnColumnResized;                   //Volga
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ColumnByName(const AName: string): TVolgaColumn;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TVolgaColumn; State: TGridDrawState);
    procedure DefaultHandler(var Msg); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ShowPopupEditor(Column: TVolgaColumn; X: Integer = Low(Integer);
      Y: Integer = Low(Integer)); dynamic;
    procedure InvalidateCurrentRow;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ValidFieldIndex(FieldIndex: Integer): Boolean;
    function ScreenCellRect(ARow: TVolgaRowType; AColName: string): TRect;
    procedure ExportToHTML(filename, title, header, footer: string;
      TotalFieldNames: string; ShowInBrowser: Boolean);
    property EditorMode;
    property FieldCount: Integer read GetFieldCount;
    property Fields[FieldIndex: Integer]: TField read GetFields;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    //added by Armando Novello
    property ClearSortMarkOnClick: Boolean read FClrSMarkOnClick write FClrSMarkOnClick
      default True;
  end;

  TVolgaDBGrid = class(TVolgaCustomDBGrid)
  public
    property Canvas;
    property SelectedRows;
  published
    property Align;
    property AllowDelete;
    property AllowInsert;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored False;      //StoreColumns;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;                 //Volga
    property Font;
    property HighlightColor;            //volga
    property HighlightTextColor;        //volga
    property ImeMode;
    property ImeName;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property BeforeDropDown;            //Volga
    property AfterCloseUp;              //Volga
    property OnCalcFieldEdited;         //Volga
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawColumnCell;
    property OnDrawCellAttr;            //Volga
    property OnDrawTitleAttr;           //Volga
    property OnColumnResized;           //Volga
    property OnMultiSelectChanged;      //Volga
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
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
    property OnSpecialKeyDown;          //Volga
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
    property RowsHeight;                //Volga
    property TitleHeight;               //Volga
  end;

const
  IndicatorWidth = 11;

implementation

uses Math, DBConsts, Dialogs;

{$R VolDBGRID.RES}

const
  bmArrow = 'VDBGARROW';
  bmEdit = 'VDBEDIT';
  bmInsert = 'VDBINSERT';
  bmMultiDot = 'VDBMULTIDOT';
  bmMultiArrow = 'VDBMULTIARROW';
  bmSortUp = 'VDBSMUP';
  bmSortDn = 'VDBSMDOWN';
  MaxMapSize = (MaxInt div 2) div SizeOf(Integer); { 250 million }

{ Error reporting }

procedure RaiseGridError(const S: string);
begin
  raise EInvalidGridOperation.Create(S);
end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

{ TVolgaDBGridInplaceEdit }

type
  TVolgaEditStyle = (esSimple, esEllipsis, esPickList, esDataList, esCalendar,
    esCalculator);                      //Volga
  TPopupListbox = class;
  TVolgaPopupCalendar = class;          //Volga

  TVolgaDBGridInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
    FDataList: TDBLookupListBox;
    FPickList: TPopupListbox;
    FCalendar: TVolgaPopupCalendar;     //Volga
    FCalculator: TVolgaCalculator;
    FActiveList: TWinControl;
    FEditStyle: TVolgaEditStyle;
    FListVisible: Boolean;
    FLookupSource: TDatasource;
    FTracking: Boolean;
    FPressed: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TVolgaEditStyle);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    function OverButton(const P: TPoint): Boolean;
    function ButtonRect: TRect;
    procedure CalSelectDate(Sender: TObject);
    procedure CalcExit(Sender: TObject; Selected: Boolean);
  protected
    procedure BoundsChanged; override;
    procedure CloseUp(Accept: Boolean);
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    property EditStyle: TVolgaEditStyle read FEditStyle write SetEditStyle;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property DataList: TDBLookupListBox read FDataList;
    property PickList: TPopupListbox read FPickList;
  public
    constructor Create(Owner: TComponent); override;
  end;

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

 { TVolgaPopupCalendar }//Volga

  TVolgaPopupCalendar = class(TVolgaCalendar)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;                                  //Volga

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 5000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TVolgaDBGridInPlaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
    (X < Width) and (Y < Height));
end;

//Volga

procedure TVolgaPopupCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
//  AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TVolgaPopupCalendar.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TVolgaPopupCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ((X >= 2) and (Y >= 41) and (X < Width - 2) and (Y < Height - 20)) then
    TVolgaDBGridInPlaceEdit(Owner).CloseUp(true)
  else if (X < 0) or (Y < 0) or (X > Width) or (Y > Height) then
    TVolgaDBGridInPlaceEdit(Owner).CloseUp(false)
end;
//Volga

{ TVolgaDBGridInplaceEdit }

constructor TVolgaDBGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FLookupSource := TDataSource.Create(Self);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) - 4; //Volga
  FEditStyle := esSimple;
end;

procedure TVolgaDBGridInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if (FEditStyle <> esSimple) then
    if not TVolgaCustomDBGrid(Owner).UseRightToLeftAlignment then
      Dec(R.Right, FButtonWidth)
    else
      Inc(R.Left, FButtonWidth - 2);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TVolgaDBGridInplaceEdit.CloseUp(Accept: Boolean);
var
  MasterField: TField;
  ListValue: Variant;
  ind: integer;                         //Volga
begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FActiveList = FDataList then
      ListValue := FDataList.KeyValue
    else if FActiveList = FPickList then
    begin                               //Volga
      ind := FPickList.ItemIndex;       //Volga
      if ind <> -1 then
        with TVolgaCustomDBGrid(Grid), Columns[SelectedIndex] do
          if (PickValues <> nil) and (PickValues.Count = PickList.Count) then //Volga
            ListValue := PickValues[ind] //Volga
          else
            ListValue := PickList[ind];
    end
    else if FActiveList = FCalendar then //Volga
      ListValue := DateToStr(FCalendar.Date) //Volga
    else if FActiveList = FCalculator then //Volga
      ListValue := FCalculator.Value;
    Windows.SetFocus(Handle);           //фокус остался на edit-контроле
    ShowCaret(Handle);
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    if Assigned(FDataList) then         //отцепили lookup-датасет
      FDataList.ListSource := nil;
    FLookupSource.Dataset := nil;
    Invalidate;
    if Accept then
    begin
      if FActiveList = FDataList then   //Volga
        with TVolgaCustomDBGrid(Grid), Columns[SelectedIndex] do
        begin
          MasterField := Field.DataSet.FieldByName(LookupKeyField); //Volga
          if MasterField.CanModify then
          begin
            Field.DataSet.Edit;
            MasterField.Value := ListValue;
          end;
        end
      else if (not VarIsNull(ListValue)) and EditCanModify then
        with TVolgaCustomDBGrid(Grid) do
          Columns[SelectedIndex].Field.Text := ListValue;
    end;
    with TVolgaCustomDBGrid(Grid) do
      if Assigned(AfterCloseUp) then
        AfterCloseUp(TVolgaCustomDBGrid(Grid), Columns[SelectedIndex], Accept);
  end;
end;

procedure TVolgaDBGridInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if EditStyle in [esPickList, esDataList, esCalendar, esCalculator] then //Volga
          if FListVisible then
            CloseUp(True)
          else
            DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      begin
        if FListVisible and not (ssAlt in Shift) then
        begin
          CloseUp(Key = VK_RETURN);
          Key := 0;
        end;
//        if Key=VK_RETURN then  //Volga
//          with TVolgaCustomDBGrid(Grid) do SelectedIndex := SelectedIndex+1;
      end;
    VK_SPACE, $30..$5A, VK_NUMPAD0..VK_NUMPAD9: ;  //буквенно-цифровые клавиши - ничего не делаем
  else //функциональные клавиши, end,home,insert,delete и т.п.
    with TVolgaCustomDBGrid(Grid) do    //Volga
      if Assigned(OnSpecialKeyDown) then
        OnSpecialKeyDown(TVolgaCustomDBGrid(Grid), Columns[SelectedIndex], Key,
          EditText);
  end;
end;

procedure TVolgaDBGridInplaceEdit.DropDown;
var
  P: TPoint;
  I, J, X, Y: Integer;
  Column: TVolgaColumn;
  FValue: Variant;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    with TVolgaCustomDBGrid(Grid) do
    begin
      Column := Columns[SelectedIndex];
      if Assigned(BeforeDropDown) then
        BeforeDropDown(TVolgaCustomDBGrid(Grid), Column);
      if Column.DropDownWidth > 0 then  //Volga
        FActiveList.Width := Column.DropDownWidth //Volga
      else
        FActiveList.Width := Column.Width;
    end;
    if FActiveList = FDataList then
      with Column do
      begin
        FDataList.Color := Color;
        FDataList.Font := Font;
        FDataList.RowCount := DropDownRows;
        FLookupSource.DataSet := LookupDataSet;
        FDataList.ListField := LookupDropDownFields; //Volga
        FDataList.KeyField := LookupLinkField; //Volga
        FDataList.ListFieldIndex := 0;  //в случае нескольких полей!!!
        FDataList.ListSource := FLookupSource;
        //пытаемся искать в списке нужное значение
        if IsLinkActive then
        begin
          FValue := Field.DataSet.FieldByName(LookupKeyField).Value;
          FDataList.KeyValue := FValue; //Volga
          LookupDataSet.Locate(LookupLinkField, FValue, []);
        end;
      end
    else if FActiveList = FPickList then //Volga
    begin
      FPickList.Color := Color;
      FPickList.Font := Font;
      FPickList.Items := Column.Picklist;
      if FPickList.Items.Count >= Integer(Column.DropDownRows) then
        FPickList.Height := Integer(Column.DropDownRows) * FPickList.ItemHeight + 4
      else
        FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;
      if Column.Field.IsNull then
        FPickList.ItemIndex := -1
      else if (Column.PickValues <> nil) and (Column.PickValues.Count = //Volga
        Column.PickList.Count) then     //Volga
        FPickList.ItemIndex := Column.PickValues.IndexOf(Column.Field.Text) //Volga
      else
        FPickList.ItemIndex := Column.PickList.IndexOf(Column.Field.Text);
      J := FPickList.ClientWidth;
      for I := 0 to FPickList.Items.Count - 1 do
      begin
        Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
        if Y > J then J := Y;
      end;
      FPickList.ClientWidth := J;
    end
    else if FActiveList = FCalendar then //Volga
    begin                               //календарь
      if Column.Field.IsNull then       //Volga
        FCalendar.Date := Date          //Volga
      else
        FCalendar.Date := Column.Field.AsDateTime; //Volga
    end
    else if FActiveList = FCalculator then //Volga
    begin
      HideCaret(Handle);
      //if DBLinked and FDataLink.Dataset.CanModify then
      //  FDataLink.Edit;
      try
        FCalculator.Value := Column.Field.AsFloat;
        FCalculator.OldValue := FCalculator.Value;
      except FCalculator.Value := 0;
      end;
      SelectAll;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    X := P.X;
    Y := P.Y + Height;
    if X + FActiveList.Width > Screen.Width then X := Screen.Width - FActiveList.Width;
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

type
  TWinControlCracker = class(TWinControl)
  end;

procedure TVolgaDBGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var ParentGrid: TVolgaCustomDBGrid;
begin
  if FListVisible and (EditStyle = esCalculator) then
  begin
    key := 0;
    Exit;
  end;

  ParentGrid := TVolgaCustomDBGrid(Grid);
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    ParentGrid.EditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
  begin
    case Key of
      VK_RETURN:
        if dgEnterToTab in ParentGrid.Options then
        begin
          ParentGrid.SetFocus;
          ParentGrid.KeyDown(Key, Shift);
          Key := 0;
          Update;
//    VK_DELETE:
//      if (Ctrl) then SendToParent;
        end;
    end;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TVolgaDBGridInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if dgEnterToTab in TVolgaCustomDBGrid(Grid).Options then
    if (Key = VK_RETURN) then Key := 9;
  inherited KeyUp(Key, Shift);
end;

procedure TVolgaDBGridInplaceEdit.KeyPress(var Key: Char);
begin
  if FListVisible and (EditStyle = esCalculator) then
  begin
    key := #0;
    Exit;
  end;
  if dgEnterToTab in TVolgaCustomDBGrid(Grid).Options then
    if (ord(Key) = VK_RETURN) then Key := #9;
  inherited KeyPress(Key);
end;

procedure TVolgaDBGridInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TVolgaDBGridInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> esSimple) and
    OverButton(Point(X, Y)) then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVolgaDBGridInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TVolgaDBGridInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = esEllipsis) and WasPressed then
    TVolgaCustomDBGrid(Grid).EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TVolgaDBGridInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TVolgaDBGridInplaceEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TVolgaDBGridInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
begin
  if (FEditStyle <> esSimple) then
  begin
    R := ButtonRect;
    Flags := 0;
    if FEditStyle in [esDataList, esPickList, esCalendar, esCalculator] then //Volga
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else                                { esEllipsis }
    begin
      if FPressed then Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);
      Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(FPressed);
      W := FButtonWidth shr 3;
      if W = 0 then W := 1;
      PatBlt(DC, X, Y, W, W, BLACKNESS);
      PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
      PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TVolgaDBGridInplaceEdit.SetEditStyle(Value: TVolgaEditStyle);
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    esPickList:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
    esDataList:
      begin
        if FDataList = nil then
        begin
          FDataList := TPopupDataList.Create(Self);
          FDataList.Visible := False;
          FDataList.Parent := Self;
          FDataList.OnMouseUp := ListMouseUp;
        end;
        FActiveList := FDataList;
      end;
    esCalendar:                         //Volga
      begin
        if FCalendar = nil then
        begin
          FCalendar := TVolgaPopupCalendar.Create(Self);
          FCalendar.Visible := False;
          FCalendar.Parent := Self;
          FCalendar.OnMouseUp := ListMouseUp;
          FCalendar.OnSelectDate := CalSelectDate;
        end;
        FActiveList := FCalendar;
      end;                              //Volga
    esCalculator:
      begin
        if FCalculator = nil then
        begin
          FCalculator := TVolgaCalculator.Create(Self);
          FCalculator.Visible := False;
          FCalculator.Parent := Self;
          FCalculator.OnExitClick := CalcExit;
        end;
        FActiveList := FCalculator;
      end;
  else                                  { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  with TVolgaCustomDBGrid(Grid) do
    Self.ReadOnly := Columns[SelectedIndex].ReadOnly;
  Repaint;
end;

procedure TVolgaDBGridInplaceEdit.UpdateContents;
var
  Column: TVolgaColumn;
  NewStyle: TVolgaEditStyle;
  MasterField: TField;
begin
  with TVolgaCustomDBGrid(Grid) do
    Column := Columns[SelectedIndex];
  NewStyle := esSimple;
  case Column.ButtonStyle of
    cbsEllipsis: NewStyle := esEllipsis;
    cbsCombo:
      if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and
        not Column.Readonly then
        NewStyle := esPickList;
    cbsLookup:
      if Assigned(Column.Field) and Column.IsLinkActive then
        with Column do
        begin
          {Show the dropdown button only if the field is editable }
          MasterField := Field.Dataset.FieldByName(LookupKeyField);
          {Column.DefaultReadonly will always be True for a lookup field.
          Test if Column.ReadOnly has been assigned a value of True }
          if Assigned(MasterField) and MasterField.CanModify and not Column.ReadOnly then
            with TVolgaCustomDBGrid(Grid) do
              if not ReadOnly and DataLink.Active and not Datalink.ReadOnly then
                NewStyle := esDataList
        end;
    cbsCalendar:
      if not Column.Readonly then       //Volga
        NewStyle := esCalendar;         //Volga
    cbsCalculator:
      if not Column.Readonly then       //Volga
        NewStyle := esCalculator;       //Volga
  end;
  EditStyle := NewStyle;
  inherited UpdateContents;             //берется текст из ячейки грида
  Font.Assign(Column.Font);
end;

procedure TVolgaDBGridInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> nil) and (Message.Sender <> Self) and
    (Message.Sender <> FActiveList) and (Message.Sender.Parent <> FActiveList) then
    CloseUp(False);
end;

procedure TVolgaDBGridInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TVolgaDBGridInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then
    inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.WParam) <> TVolgaCustomDBGrid(Grid).Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

function TVolgaDBGridInplaceEdit.ButtonRect: TRect;
begin
  if not TVolgaCustomDBGrid(Owner).UseRightToLeftAlignment then
    Result := Rect(Width - FButtonWidth, 0 {1}, Width {- 1}, Height {- 1}) //Volga
  else
    Result := Rect(0, 0, FButtonWidth, Height);
end;

function TVolgaDBGridInplaceEdit.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TVolgaDBGridInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
    if (FEditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
      Exit;
  inherited;
end;

procedure TVolgaDBGridInplaceEdit.WMPaint(var Message: TWMPaint);
begin
//  ControlState := ControlState + [csCustomPaint];
  PaintHandler(Message);
//  ControlState := ControlState - [csCustomPaint];
end;

procedure TVolgaDBGridInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (FEditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(Screen.Cursors[crHandPoint]) //Volga
  else
    inherited;
end;

procedure TVolgaDBGridInplaceEdit.WndProc(var Message: TMessage);
var
  Column: TVolgaColumn;
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
//    if EditStyle in [esPickList, esDataList, esCalendar] then //Volga
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (Message.Msg = wm_Char) and (CharCode >= 32) and not
          FListVisible and (EditStyle in [esPickList, esDataList]) then
        begin
          with TVolgaCustomDBGrid(Grid) do
            Column := Columns[SelectedIndex];
          if Column.AutoDropDown then
            DropDown; //выпадаем, если установлено AutoDropDown и нажата буква
        end;
        if (CharCode <> 0) and FListVisible then
        begin                           //перенаправляем сообщение выпавшему контролю
          with TMessage(Message) do
            SendMessage(FActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end;
  end;
  inherited;
end;

procedure TVolgaDBGridInplaceEdit.CalSelectDate(Sender: TObject);
begin                                   //в календаре выбрана дата
  CloseUp(true);
end;

procedure TVolgaDBGridInplaceEdit.CalcExit(Sender: TObject;
  Selected: Boolean);
begin
  CloseUp(Selected);
end;

{ TGridDataLink }

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TGridDataLink.Create(AGrid: TVolgaCustomDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  VisualControl := True;
end;

destructor TGridDataLink.Destroy;
begin
  ClearMapping;
  inherited Destroy;
end;

function TGridDataLink.GetDefaultFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  if DataSet <> nil then Result := DataSet.DefaultFields;
  if Result and SparseMap then
    for I := 0 to FFieldCount - 1 do
      if FFieldMap[I] < 0 then
      begin
        Result := False;
        Exit;
      end;
end;

function TGridDataLink.GetFields(I: Integer): TField;
begin
  if (0 <= I) and (I < FFieldCount) and (FFieldMap[I] >= 0) then
    Result := DataSet.FieldList[FFieldMap[I]]
  else
    Result := nil;
end;

function TGridDataLink.AddMapping(const FieldName: string): Boolean;
var
  Field: TField;
  NewSize: Integer;
begin
  Result := True;
  if FFieldCount >= MaxMapSize then RaiseGridError(STooManyColumns);
  if SparseMap then
    Field := DataSet.FindField(FieldName)
  else
    Field := DataSet.FieldByName(FieldName);

  if FFieldCount = Length(FFieldMap) then
  begin
    NewSize := Length(FFieldMap);
    if NewSize = 0 then
      NewSize := 8
    else
      Inc(NewSize, NewSize);
    if (NewSize < FFieldCount) then
      NewSize := FFieldCount + 1;
    if (NewSize > MaxMapSize) then
      NewSize := MaxMapSize;
    SetLength(FFieldMap, NewSize);
  end;
  if Assigned(Field) then
  begin
    FFieldMap[FFieldCount] := Dataset.FieldList.IndexOfObject(Field);
    Field.FreeNotification(FGrid);
  end
  else
    FFieldMap[FFieldCount] := -1;
  Inc(FFieldCount);
end;

procedure TGridDataLink.ActiveChanged;
begin
  FGrid.LinkActive(Active);
  FModified := False;
end;

procedure TGridDataLink.ClearMapping;
begin
  FFieldMap := nil;
  FFieldCount := 0;
end;

procedure TGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TGridDataLink.DataSetChanged;
begin
  FGrid.DataChanged;
  FModified := False;
end;

procedure TGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.Scroll(Distance);
end;

procedure TGridDataLink.LayoutChanged;
var
  SaveState: Boolean;
begin
  { FLayoutFromDataset determines whether default column width is forced to
    be at least wide enough for the column title.  }
  SaveState := FGrid.FLayoutFromDataset;
  FGrid.FLayoutFromDataset := True;
  try
    FGrid.LayoutChanged;
  finally
    FGrid.FLayoutFromDataset := SaveState;
  end;
  inherited LayoutChanged;
end;

procedure TGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    FGrid.SelectedField := Field^;
    if (FGrid.SelectedField = Field^) and FGrid.AcquireFocus then
    begin
      Field^ := nil;
      FGrid.ShowEditor;
    end;
  end;
end;

procedure TGridDataLink.EditingChanged;
begin
  FGrid.EditingChanged;
end;

procedure TGridDataLink.RecordChanged(Field: TField);
begin
  FGrid.RecordChanged(Field);
  FModified := False;
end;

procedure TGridDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified then FGrid.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

function TGridDataLink.GetMappedIndex(ColIndex: Integer): Integer;
begin
  if (0 <= ColIndex) and (ColIndex < FFieldCount) then
    Result := FFieldMap[ColIndex]
  else
    Result := -1;
end;

procedure TGridDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

function TGridDataLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TGridDataLink.BuildAggMap;
begin
end;

{ TVolgaColumnTitle }

constructor TVolgaColumnTitle.Create(Column: TVolgaColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TVolgaColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TVolgaColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TVolgaColumnTitle then
  begin
    if cVolgaTitleAlignment in TVolgaColumnTitle(Source).FColumn.FAssignedValues then
      Alignment := TVolgaColumnTitle(Source).Alignment;
    if cVolgaTitleColor in TVolgaColumnTitle(Source).FColumn.FAssignedValues then
      Color := TVolgaColumnTitle(Source).Color;
    if cVolgaTitleCaption in TVolgaColumnTitle(Source).FColumn.FAssignedValues then
      Caption := TVolgaColumnTitle(Source).Caption;
    if cVolgaTitleFont in TVolgaColumnTitle(Source).FColumn.FAssignedValues then
      Font := TVolgaColumnTitle(Source).Font;
  end
  else
    inherited Assign(Source);
end;

function TVolgaColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taCenter;
end;

function TVolgaColumnTitle.DefaultColor: TColor;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TVolgaColumnTitle.DefaultFont: TFont;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.TitleFont
  else
    Result := FColumn.Font;
end;

function TVolgaColumnTitle.DefaultCaption: string;
var
  Field: TField;
begin
  Field := FColumn.Field;
  if Assigned(Field) then
    Result := Field.DisplayName
  else
    Result := FColumn.FieldName;
end;

procedure TVolgaColumnTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, cVolgaTitleFont);
  FColumn.Changed(True);
end;

function TVolgaColumnTitle.GetAlignment: TAlignment;
begin
  if cVolgaTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TVolgaColumnTitle.GetColor: TColor;
begin
  if cVolgaTitleColor in FColumn.FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TVolgaColumnTitle.GetCaption: string;
begin
  if cVolgaTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TVolgaColumnTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (cVolgaTitleFont in FColumn.FAssignedValues) then
  begin
    Def := DefaultFont;
    if (FFont.Handle <> Def.Handle) or (FFont.Color <> Def.Color) then
    begin
      Save := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := Save;
    end;
  end;
  Result := FFont;
end;

function TVolgaColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (cVolgaTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TVolgaColumnTitle.IsColorStored: Boolean;
begin
  Result := (cVolgaTitleColor in FColumn.FAssignedValues) and
    (FColor <> DefaultColor);
end;

function TVolgaColumnTitle.IsFontStored: Boolean;
begin
  Result := (cVolgaTitleFont in FColumn.FAssignedValues);
end;

function TVolgaColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (cVolgaTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TVolgaColumnTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (cVolgaTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TVolgaColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cVolgaTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  { If font was assigned, changing it back to default may affect grid title
    height, and title height changes require layout and redraw of the grid. }
  FColumn.Changed(FontAssigned);
end;

procedure TVolgaColumnTitle.SetAlignment(Value: TAlignment);
begin
  if (cVolgaTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then
    Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, cVolgaTitleAlignment);
  FColumn.Changed(False);
end;

procedure TVolgaColumnTitle.SetColor(Value: TColor);
begin
  if (cVolgaTitleColor in FColumn.FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FColumn.FAssignedValues, cVolgaTitleColor);
  FColumn.Changed(False);
end;

procedure TVolgaColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TVolgaColumnTitle.SetCaption(const Value: string);
var
  Grid: TVolgaCustomDBGrid;
begin
  if Column.IsStored then
  begin
    if (cVolgaTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
    FCaption := Value;
    Include(Column.FAssignedValues, cVolgaTitleCaption);
    Column.Changed(False);
  end
  else
  begin
    Grid := Column.GetGrid;
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Column.Field) then
      Column.Field.DisplayLabel := Value;
  end;
end;

{ TVolgaColumn }

constructor TVolgaColumn.Create(Collection: TCollection);
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TVolgaDBGridColumns) then
    Grid := TVolgaDBGridColumns(Collection).Grid;
  if Assigned(Grid) then Grid.BeginLayout;
  try
    inherited Create(Collection);
    FDropDownRows := 10;                //Volga
    FDropDownWidth := 0;                //Volga
    FButtonStyle := cbsNone;
    FFont := TFont.Create;
    FFont.Assign(DefaultFont);
    FFont.OnChange := FontChanged;
    FImeMode := imDontCare;
    FImeName := Screen.DefaultIme;
    FTitle := CreateTitle;
    FVisible := True;
    FExpanded := False;
    FCanClick := False;                 //Volga
    FSortMark := vsmNone;               //Volga
    FAutoDrop := False;                 //Volga
    FStored := True;
    FDown := False;                     //Volga
    FTextAsHint := False;               //Volga
    FWrap := false;                     //Volga
//    FPickList := nil;
//    FPickValues := nil;     //Volga
  finally
    if Assigned(Grid) then Grid.EndLayout;
  end;
end;

destructor TVolgaColumn.Destroy;
begin
  FTitle.Free;
  FFont.Free;
  FPickList.Free;
  FPickValues.Free;                     //Volga
  inherited Destroy;
end;

procedure TVolgaColumn.Assign(Source: TPersistent);
begin
  if Source is TVolgaColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;
      FieldName := TVolgaColumn(Source).FieldName;
      if cvColor in TVolgaColumn(Source).AssignedValues then
        Color := TVolgaColumn(Source).Color;
      if cvWidth in TVolgaColumn(Source).AssignedValues then
        Width := TVolgaColumn(Source).Width;
      if cvFont in TVolgaColumn(Source).AssignedValues then
        Font := TVolgaColumn(Source).Font;
      if cvImeMode in TVolgaColumn(Source).AssignedValues then
        ImeMode := TVolgaColumn(Source).ImeMode;
      if cvImeName in TVolgaColumn(Source).AssignedValues then
        ImeName := TVolgaColumn(Source).ImeName;
      if cvAlignment in TVolgaColumn(Source).AssignedValues then
        Alignment := TVolgaColumn(Source).Alignment;
      if cvReadOnly in TVolgaColumn(Source).AssignedValues then
        ReadOnly := TVolgaColumn(Source).ReadOnly;
      Title := TVolgaColumn(Source).Title;
      DropDownRows := TVolgaColumn(Source).DropDownRows;
      DropDownWidth := TVolgaColumn(Source).DropDownWidth; //Volga
      CanClick := TVolgaColumn(Source).CanClick; //Volga
      LongTextAsHint := TVolgaColumn(Source).LongTextAsHint; //Volga
      ButtonStyle := TVolgaColumn(Source).ButtonStyle;
      PickList := TVolgaColumn(Source).PickList;
      PickValues := TVolgaColumn(Source).PickValues; //Volga
      LookupDropDownFields := TVolgaColumn(Source).LookupDropDownFields; //Volga
      LookupKeyField := TVolgaColumn(Source).LookupKeyField; //Volga
      LookupLinkField := TVolgaColumn(Source).LookupLinkField; //Volga
      LookupDataSet := TVolgaColumn(Source).LookupDataSet; //Volga
      PopupMenu := TVolgaColumn(Source).PopupMenu;
      FVisible := TVolgaColumn(Source).FVisible;
      FExpanded := TVolgaColumn(Source).FExpanded;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TVolgaColumn.CreateTitle: TVolgaColumnTitle;
begin
  Result := TVolgaColumnTitle.Create(Self);
end;

function TVolgaColumn.DefaultAlignment: TAlignment;
begin
  if Assigned(Field) then
    if (ButtonStyle = cbsCombo) or (ButtonStyle = cbsLookup) then
      Result := taLeftJustify
    else
      Result := FField.Alignment
  else
    Result := taLeftJustify;
end;

function TVolgaColumn.DefaultColor: TColor;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Color
  else
    Result := clWindow;
end;

function TVolgaColumn.DefaultFont: TFont;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TVolgaColumn.DefaultImeMode: TImeMode;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeMode
  else
    Result := FImeMode;
end;

function TVolgaColumn.DefaultImeName: TImeName;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeName
  else
    Result := FImeName;
end;

function TVolgaColumn.DefaultReadOnly: Boolean;
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  Result := (Assigned(Grid) and Grid.ReadOnly) or
    (Assigned(Field) and FField.ReadOnly);
end;

function TVolgaColumn.DefaultWidth: Integer;
var
  W: Integer;
  RestoreCanvas: Boolean;
  TM: TTextMetric;
begin
  if GetGrid = nil then
  begin
    Result := 64;
    Exit;
  end;
  with GetGrid do
  begin
    if Assigned(Field) then
    begin
      RestoreCanvas := not HandleAllocated;
      if RestoreCanvas then
        Canvas.Handle := GetDC(0);
      try
        Canvas.Font := Self.Font;
        GetTextMetrics(Canvas.Handle, TM);
        Result := Field.DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang)
          + TM.tmOverhang + 4;
        if dgTitles in Options then
        begin
          Canvas.Font := Title.Font;
          W := Canvas.TextWidth(Title.Caption) + 4;
          if Result < W then
            Result := W;
        end;
      finally
        if RestoreCanvas then
        begin
          ReleaseDC(0, Canvas.Handle);
          Canvas.Handle := 0;
        end;
      end;
    end
    else
      Result := DefaultColWidth;
  end;
end;

procedure TVolgaColumn.FontChanged;
begin
  Include(FAssignedValues, cvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

function TVolgaColumn.GetAlignment: TAlignment;
begin
  if cvAlignment in FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TVolgaColumn.GetColor: TColor;
begin
  if cvColor in FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TVolgaColumn.GetExpanded: Boolean;
begin
  Result := FExpanded and Expandable;
end;

function TVolgaColumn.GetField: TField;
var
  Grid: TVolgaCustomDBGrid;
begin { Returns Nil if FieldName can't be found in dataset }
  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid) and
    Assigned(Grid.DataLink.DataSet) then
    with Grid.Datalink.Dataset do
      if Active or (not DefaultFields) then
        SetField(FindField(FieldName));
  Result := FField;
end;

function TVolgaColumn.GetFont: TFont;
var
  Save: TNotifyEvent;
begin
  if not (cvFont in FAssignedValues) and (FFont.Handle <> DefaultFont.Handle) then
  begin
    Save := FFont.OnChange;
    FFont.OnChange := nil;
    FFont.Assign(DefaultFont);
    FFont.OnChange := Save;
  end;
  Result := FFont;
end;

function TVolgaColumn.GetGrid: TVolgaCustomDBGrid;
begin
  if Assigned(Collection) and (Collection is TVolgaDBGridColumns) then
    Result := TVolgaDBGridColumns(Collection).Grid
  else
    Result := nil;
end;

function TVolgaColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TVolgaColumn.GetImeMode: TImeMode;
begin
  if cvImeMode in FAssignedValues then
    Result := FImeMode
  else
    Result := DefaultImeMode;
end;

function TVolgaColumn.GetImeName: TImeName;
begin
  if cvImeName in FAssignedValues then
    Result := FImeName
  else
    Result := DefaultImeName;
end;

function TVolgaColumn.GetParentColumn: TVolgaColumn;
var
  Col: TVolgaColumn;
  Fld: TField;
  I: Integer;
begin
  Result := nil;
  Fld := Field;
  if (Fld <> nil) and (Fld.ParentField <> nil) and (Collection <> nil) then
    for I := Index - 1 downto 0 do
    begin
      Col := TVolgaColumn(Collection.Items[I]);
      if Fld.ParentField = Col.Field then
      begin
        Result := Col;
        Exit;
      end;
    end;
end;

function TVolgaColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TVolgaColumn.GetPickValues: TStrings; //Volga
begin
  if FPickValues = nil then
    FPickValues := TStringList.Create;
  Result := FPickValues;
end;                                    //Volga

function TVolgaColumn.GetReadOnly: Boolean;
begin
  if cvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TVolgaColumn.GetShowing: Boolean;
var
  Col: TVolgaColumn;
begin
  Result := not Expanded and Visible;
  if Result then
  begin
    Col := Self;
    repeat
      Col := Col.ParentColumn;
    until (Col = nil) or not Col.Expanded;
    Result := Col = nil;
  end;
end;

function TVolgaColumn.GetVisible: Boolean;
var
  Col: TVolgaColumn;
begin
  Result := FVisible;
  if Result then
  begin
    Col := ParentColumn;
    Result := Result and ((Col = nil) or Col.Visible);
  end;
end;

function TVolgaColumn.GetWidth: Integer;
begin
  if not Showing then
    Result := -1
  else if cvWidth in FAssignedValues then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TVolgaColumn.IsAlignmentStored: Boolean;
begin
  Result := (cvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TVolgaColumn.IsColorStored: Boolean;
begin
  Result := (cvColor in FAssignedValues) and (FColor <> DefaultColor);
end;

function TVolgaColumn.IsFontStored: Boolean;
begin
  Result := (cvFont in FAssignedValues);
end;

function TVolgaColumn.IsImeModeStored: Boolean;
begin
  Result := (cvImeMode in FAssignedValues) and (FImeMode <> DefaultImeMode);
end;

function TVolgaColumn.IsImeNameStored: Boolean;
begin
  Result := (cvImeName in FAssignedValues) and (FImeName <> DefaultImeName);
end;

function TVolgaColumn.IsReadOnlyStored: Boolean;
begin
  Result := (cvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

function TVolgaColumn.IsWidthStored: Boolean;
begin
  Result := (cvWidth in FAssignedValues) and (FWidth <> DefaultWidth);
end;

procedure TVolgaColumn.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if cvFont in FAssignedValues then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TVolgaColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;
  FPickList.Free;
  FPickList := nil;
  FPickValues.Free;                     //Volga
  FPickValues := nil;                   //Volga
  ButtonStyle := cbsNone;               //Volga
  CanClick := false;                    //Volga
  LongTextAsHint := false;              //Volga
  Changed(FontAssigned);
end;

procedure TVolgaColumn.SetAlignment(Value: TAlignment);
var
  Grid: TVolgaCustomDBGrid;
begin
  if IsStored then
  begin
    if (cvAlignment in FAssignedValues) and (Value = FAlignment) then Exit;
    FAlignment := Value;
    Include(FAssignedValues, cvAlignment);
    Changed(False);
  end
  else
  begin
    Grid := GetGrid;
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Field)
      and (ButtonStyle <> cbsCombo) and (ButtonStyle <> cbsLookup) then //Volga
      Field.Alignment := Value;
  end;
end;

procedure TVolgaColumn.SetButtonStyle(Value: TVolgaColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  Changed(False);
end;

procedure TVolgaColumn.SetColor(Value: TColor);
begin
  if (cvColor in FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FAssignedValues, cvColor);
  Changed(False);
end;

procedure TVolgaColumn.SetField(Value: TField);
begin
  if FField = Value then Exit;
  FField := Value;
  if Assigned(Value) then
    FFieldName := Value.FullName;
  if not IsStored then
  begin
    if Value = nil then
      FFieldName := '';
    RestoreDefaults;
  end;
  Changed(False);
end;

procedure TVolgaColumn.SetFieldName(const Value: string);
var
  AField: TField;
  Grid: TVolgaCustomDBGrid;
begin
  AField := nil;
  Grid := GetGrid;
  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
    not (csLoading in Grid.ComponentState) and (Length(Value) > 0) then
    AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
  FFieldName := Value;
  SetField(AField);
  Changed(False);
end;

procedure TVolgaColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TVolgaColumn.SetImeMode(Value: TImeMode);
begin
  if (cvImeMode in FAssignedValues) or (Value <> DefaultImeMode) then
  begin
    FImeMode := Value;
    Include(FAssignedValues, cvImeMode);
  end;
  Changed(False);
end;

procedure TVolgaColumn.SetImeName(Value: TImeName);
begin
  if (cvImeName in FAssignedValues) or (Value <> DefaultImeName) then
  begin
    FImeName := Value;
    Include(FAssignedValues, cvImeName);
  end;
  Changed(False);
end;

procedure TVolgaColumn.SetIndex(Value: Integer);
var
  Grid: TVolgaCustomDBGrid;
  Fld: TField;
  I, OldIndex: Integer;
  Col: TVolgaColumn;
begin
  OldIndex := Index;
  Grid := GetGrid;

  if IsStored then
  begin
    Grid.BeginLayout;
    try
      I := OldIndex + 1;                // move child columns along with parent
      while (I < Collection.Count) and (TVolgaColumn(Collection.Items[I]).ParentColumn =
        Self) do
        Inc(I);
      Dec(I);
      if OldIndex > Value then          // column moving left
      begin
        while I > OldIndex do
        begin
          Collection.Items[I].Index := Value;
          Inc(OldIndex);
        end;
        inherited SetIndex(Value);
      end
      else
      begin
        inherited SetIndex(Value);
        while I > OldIndex do
        begin
          Collection.Items[OldIndex].Index := Value;
          Dec(I);
        end;
      end;
    finally
      Grid.EndLayout;
    end;
  end
  else
  begin
    if (Grid <> nil) and Grid.Datalink.Active then
    begin
      if Grid.AcquireLayoutLock then
      try
        Col := Grid.ColumnAtDepth(Grid.Columns[Value], Depth);
        if (Col <> nil) then
        begin
          Fld := Col.Field;
          if Assigned(Fld) then
            Field.Index := Fld.Index;
        end;
      finally
        Grid.EndLayout;
      end;
    end;
    inherited SetIndex(Value);
  end;
end;

procedure TVolgaColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TVolgaColumn.SetPickValues(Value: TStrings); //Volga
begin
  if Value = nil then
  begin
    FPickValues.Free;
    FPickValues := nil;
    Exit;
  end;
  PickValues.Assign(Value);
end;                                    //Volga

procedure TVolgaColumn.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;

procedure TVolgaColumn.SetReadOnly(Value: Boolean);
var
  Grid: TVolgaCustomDBGrid;
begin
  Grid := GetGrid;
  if not IsStored and Assigned(Grid) and Grid.Datalink.Active and Assigned(Field) then
    Field.ReadOnly := Value
  else
  begin
    if (cvReadOnly in FAssignedValues) and (Value = FReadOnly) then Exit;
    FReadOnly := Value;
    Include(FAssignedValues, cvReadOnly);
    Changed(False);
  end;
end;

procedure TVolgaColumn.SetTitle(Value: TVolgaColumnTitle);
begin
  FTitle.Assign(Value);
end;

procedure TVolgaColumn.SetWidth(Value: Integer);
var
  Grid: TVolgaCustomDBGrid;
  TM: TTextMetric;
  DoSetWidth: Boolean;
begin
  DoSetWidth := IsStored;
  if not DoSetWidth then
  begin
    Grid := GetGrid;
    if Assigned(Grid) then
    begin
      if Grid.HandleAllocated and Assigned(Field) and Grid.FUpdateFields then
        with Grid do
        begin
          Canvas.Font := Self.Font;
          GetTextMetrics(Canvas.Handle, TM);
          Field.DisplayWidth := (Value + (TM.tmAveCharWidth div 2) - TM.tmOverhang - 3)
            div TM.tmAveCharWidth;
        end;
      if (not Grid.FLayoutFromDataset) or (cvWidth in FAssignedValues) then
        DoSetWidth := True;
    end
    else
      DoSetWidth := True;
  end;
  if DoSetWidth then
  begin
    if ((cvWidth in FAssignedValues) or (Value <> DefaultWidth))
      and (Value <> -1) then
    begin
      FWidth := Value;
      Include(FAssignedValues, cvWidth);
    end;
    Changed(False);
  end;
end;

procedure TVolgaColumn.SetWrap(const Value: Boolean);
begin
  if Value <> FWrap then
  begin
    FWrap := Value;
    Changed(False);
  end;
end;

procedure TVolgaColumn.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TVolgaColumn.SetExpanded(Value: Boolean);
const
  Direction: array[Boolean] of ShortInt = (-1, 1);
var
  Grid: TVolgaCustomDBGrid;
  WasShowing: Boolean;
begin
  if Value <> FExpanded then
  begin
    Grid := GetGrid;
    WasShowing := (Grid <> nil) and Grid.Columns[Grid.SelectedIndex].Showing;
    FExpanded := Value;
    Changed(True);
    if (Grid <> nil) and WasShowing then
    begin
      if not Grid.Columns[Grid.SelectedIndex].Showing then
        // The selected cell was hidden by this expand operation
        // Select 1st child (next col = 1) when parent is expanded
        // Select child's parent (prev col = -1) when parent is collapsed
        Grid.MoveCol(Grid.Col, Direction[FExpanded]);
    end;
  end;
end;

function TVolgaColumn.Depth: Integer;
var
  Col: TVolgaColumn;
begin
  Result := 0;
  Col := ParentColumn;
  if Col <> nil then Result := Col.Depth + 1;
end;

function TVolgaColumn.GetExpandable: Boolean;
var
  Fld: TField;
begin
  Fld := Field;
  Result := (Fld <> nil) and (Fld.DataType in [ftADT, ftArray]);
end;

procedure TVolgaColumn.SetValueChecked(const Value: string); //Volga
begin
  if Value <> FValueChecked then
  begin
    FValueChecked := Value;
    if ButtonStyle = cbsCheck then Changed(False);
  end;
end;                                    //Volga

procedure TVolgaColumn.SetValueUnChecked(const Value: string); //Volga
begin
  if Value <> FValueUnChecked then
  begin
    FValueUnChecked := Value;
    if ButtonStyle = cbsCheck then Changed(False);
  end;
end;                                    //Volga

function TVolgaColumn.IsLinkActive: Boolean;
begin
  try
    Result := (LookupDataSet <> nil)
      and LookupDataSet.Active and (LookupLinkField > '') and
      (LookupDropDownFields > '') and (LookupKeyField > '');
  except Result := false;
  end;
end;

procedure TVolgaColumn.SetLookupDropDownFields(const Value: string);
begin
  if Value <> FLookupDropDownFields then
  begin
    FLookupDropDownFields := Value;
    if Pos(';', FLookupDropDownFields) > 0 then //видимое поле всегда первое из списка!
      FViewField := Copy(FLookupDropDownFields, 1, Pos(';', FLookupDropDownFields) - 1)
    else
      FViewField := FLookupDropDownFields;
    if ButtonStyle = cbsLookup then Changed(False);
  end;
end;

procedure TVolgaColumn.SetLookupKeyField(const Value: string);
begin
  if Value <> FLookupKeyField then
  begin
    FLookupKeyField := Value;
    if ButtonStyle = cbsLookup then Changed(False);
  end;
end;

procedure TVolgaColumn.SetLookupLinkField(const Value: string);
begin
  if Value <> FLookupLinkField then
  begin
    FLookupLinkField := Value;
    if ButtonStyle = cbsLookup then Changed(False);
  end;
end;

procedure TVolgaColumn.SetLookupDataSet(const Value: TDataSet);
begin
//  CheckInactive;
  if (Value <> nil) and (Field <> nil) and (Value = Field.DataSet) then
    DatabaseError(V_LOOKUPSOURCEERROR, GetGrid);
  FLookupDataSet := Value;
end;

procedure TVolgaColumn.AssignList(const AList: TStrings); //Volga
var i: integer;
begin //присвоить сразу Items и Values из списка типа Name=Value
  if FPickList = nil then
    FPickList := TStringList.Create
  else
    FPickList.Clear;
  if FPickValues = nil then
    FPickValues := TStringList.Create
  else
    FPickValues.Clear;
  for i := 0 to AList.Count - 1 do
  begin
    FPickList.Add(AList.Names[i]);
    FPickValues.Add(AList.Values[AList.Names[i]]);
  end;
end;

{ TVolgaDBGridColumns }

constructor TVolgaDBGridColumns.Create(Grid: TVolgaCustomDBGrid; ColumnClass:
  TVolgaColumnClass);
begin
  inherited Create(Grid, ColumnClass);
  FGrid := Grid;
end;

//function TVolgaDBGridColumns.Add: TVolgaColumn;
//begin
//  Result := TVolgaColumn(inherited Add);
//end;

function TVolgaDBGridColumns.GetColumn(Index: Integer): TVolgaColumn;
begin
  Result := TVolgaColumn(inherited Items[Index]);
end;

function TVolgaDBGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TVolgaDBGridColumns.LoadFromFile(const Filename: string);
var
  S: TFileStream;
begin
  S := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

type
  TVolgaColumnsWrapper = class(TComponent)
  private
    FColumns: TVolgaDBGridColumns;
  published
    property Columns: TVolgaDBGridColumns read FColumns write FColumns;
  end;

procedure TVolgaDBGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TVolgaColumnsWrapper;
begin
  Wrapper := TVolgaColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TVolgaDBGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TVolgaDBGridColumns.RebuildColumns;

  procedure AddFields(Fields: TFields; Depth: Integer);
  var
    I: Integer;
  begin
    Inc(Depth);
    for I := 0 to Fields.Count - 1 do
    begin
      TVolgaColumn(Add).FieldName := Fields[I].FullName;
      if Fields[I].DataType in [ftADT, ftArray] then
        AddFields((Fields[I] as TObjectField).Fields, Depth);
    end;
  end;

begin
  if Assigned(FGrid) and Assigned(FGrid.DataSource) and
    Assigned(FGrid.Datasource.Dataset) then
  begin
    FGrid.BeginLayout;
    try
      Clear;
      AddFields(FGrid.Datasource.Dataset.Fields, 0);
    finally
      FGrid.EndLayout;
    end
  end
  else
    Clear;
end;

procedure TVolgaDBGridColumns.SaveToFile(const Filename: string);
var
  S: TStream;
begin
  S := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TVolgaDBGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TVolgaColumnsWrapper;
begin
  Wrapper := TVolgaColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TVolgaDBGridColumns.SetColumn(Index: Integer; Value: TVolgaColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TVolgaDBGridColumns.SetState(NewState: TVolgaDBGridColumnsState);
begin
  if NewState = State then Exit;
  if NewState = csDefault then
    Clear
  else
    RebuildColumns;
end;

procedure TVolgaDBGridColumns.Update(Item: TCollectionItem);
var
  Raw: Integer;
begin
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;
  if Item = nil then
  begin
    FGrid.LayoutChanged;
  end
  else
  begin
    Raw := FGrid.DataToRawColumn(Item.Index);
    FGrid.InvalidateCol(Raw);
    FGrid.ColWidths[Raw] := TVolgaColumn(Item).Width;
  end;
end;

function TVolgaDBGridColumns.InternalAdd: TVolgaColumn;
begin
  Result := TVolgaColumn(Add);
  Result.IsStored := False;
end;

function TVolgaDBGridColumns.GetState: TVolgaDBGridColumnsState;
begin
  Result := TVolgaDBGridColumnsState((Count > 0) and Items[0].IsStored);
end;

{ TBookmarkList }

constructor TBookmarkList.Create(AGrid: TVolgaCustomDBGrid);
begin
  inherited Create;
  FList := TStringList.Create;
  FList.OnChange := StringsChanged;
  FGrid := AGrid;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
begin
  if FList.Count = 0 then Exit;
  FList.Clear;
  FGrid.Invalidate;
  if Assigned(FGrid.FOnMultiSelect) then FGrid.FOnMultiSelect(FGrid);
end;

function TBookmarkList.Compare(const Item1, Item2: TBookmarkStr): Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

function TBookmarkList.CurrentRow: TBookmarkStr;
begin
  if not FLinkActive then RaiseGridError(sDataSetClosed);
  Result := FGrid.Datalink.Datasource.Dataset.Bookmark;
end;

function TBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := Find(CurrentRow, Index);
end;

function TBookmarkList.Find(const Item: TBookmarkStr; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(FList[I], Item);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TBookmarkList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBookmarkList.GetItem(Index: Integer): TBookmarkStr;
begin
  Result := FList[Index];
end;

function TBookmarkList.IndexOf(const Item: TBookmarkStr): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

procedure TBookmarkList.Delete;
var
  I: Integer;
begin
  with FGrid.Datalink.Datasource.Dataset do
  begin
    DisableControls;
    try
      for I := FList.Count - 1 downto 0 do
      begin
        Bookmark := FList[I];
        Delete;
        FList.Delete(I);
      end;
    finally
      EnableControls;
    end;
  end;
end;

function TBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with FGrid.DataLink.Datasource.Dataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if not BookmarkValid(TBookmark(FList[I])) then
      begin
        Result := True;
        FList.Delete(I);
      end;
  finally
    UpdateCursorPos;
    if Result then
    begin
      FGrid.Invalidate;
      if Assigned(FGrid.FOnMultiSelect) then FGrid.FOnMultiSelect(FGrid);
    end;
  end;
end;

procedure TBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: Integer;
  Current: TBookmarkStr;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then Exit;
  if Value then
    FList.Insert(Index, Current)
  else
    FList.Delete(Index);
  FGrid.InvalidateRow(FGrid.Row);
  if Assigned(FGrid.FOnMultiSelect) then FGrid.FOnMultiSelect(FGrid);
end;

procedure TBookmarkList.StringsChanged(Sender: TObject);
begin
  FCache := '';
  FCacheIndex := -1;
end;

{ TVolgaCustomDBGrid }

procedure TVolgaCustomDBGrid.UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure TVolgaCustomDBGrid.ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then DrawBitmap.Free;
end;

procedure TVolgaCustomDBGrid.WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string;
  Alignment: TAlignment; Wrap: Boolean; ARightToLeft: Boolean);
const
  AlignFlags: array[TAlignment] of Integer =
  (DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
    DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
    DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX);
  AlignFlagsNoWrap: array[TAlignment] of Integer =
  (DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
    DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
    DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX);
  RTL: array[Boolean] of Integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  Hold: Integer;
begin
  DrawBitmap.Canvas.Lock;
  try
    with DrawBitmap, ARect do           { Use offscreen bitmap to eliminate flicker and }
    begin                               { brush origin tics in painting / scrolling.    }
      Width := Max(Width, Right - Left);
      Height := Max(Height, Bottom - Top);
      R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
      B := Rect(0, 0, Right - Left, Bottom - Top);
    end;
    with DrawBitmap.Canvas do
    begin
      Font := ACanvas.Font;
      Font.Color := ACanvas.Font.Color;
      Brush := ACanvas.Brush;
      Brush.Style := bsSolid;
      FillRect(B);
      SetBkMode(Handle, TRANSPARENT);
      if (ACanvas.CanvasOrientation = coRightToLeft) then
        ChangeBiDiModeAlignment(Alignment);
      if Wrap then
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlags[Alignment] or RTL[ARightToLeft])
      else
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlagsNoWrap[Alignment] or RTL[ARightToLeft]);
    end;
    if (ACanvas.CanvasOrientation = coRightToLeft) then
    begin
      Hold := ARect.Left;
      ARect.Left := ARect.Right;
      ARect.Right := Hold;
    end;
    ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
  finally
    DrawBitmap.Canvas.Unlock;
  end;
end;

constructor TVolgaCustomDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  FAcquireFocus := True;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, bmArrow);
    FIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmEdit);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmInsert);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiDot);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmMultiArrow);
    FIndicators.AddMasked(Bmp, clWhite);
    Bmp.LoadFromResourceName(HInstance, bmSortDn); //Volga
    FIndicators.AddMasked(Bmp, clSilver); //Volga
    Bmp.LoadFromResourceName(HInstance, bmSortUp); //Volga
    FIndicators.AddMasked(Bmp, clSilver); //Volga
  finally
    Bmp.Free;
  end;
  FTitleOffset := 1;
  FIndicatorOffset := 1;
  FUpdateFields := True;
  FAllowDelete := True;                 //Volga
  FAllowInsert := True;                 //Volga
  FFixedCols := 0;                      //Volga
  FTitleHeight := 0;                    //Volga
  FRowsHeight := 0;                     //Volga
  FHColor := clHighlight;               //Volga
  FHTColor := clHighlightText;          //Volga
  FColMoved := False;                   //added by Armando Novello
  FOptions := [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit];
  if SysLocale.PriLangID = LANG_KOREAN then
    Include(FOptions, dgAlwaysShowEditor);
  DesignOptionsBoost := [goColSizing];
  VirtualView := True;
  UsesBitmap;
  ScrollBars := ssHorizontal;
  inherited Options := [goFixedHorzLine, goFixedVertLine, goHorzLine,
    goVertLine, goColSizing, goColMoving, goTabs, goEditing];
  FColumns := CreateColumns;
  FVisibleColumns := TList.Create;
  inherited RowCount := 2;
  inherited ColCount := 2;
  FDataLink := TGridDataLink.Create(Self);
  Color := clWindow;
  ParentColor := False;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := TitleFontChanged;
  FSaveCellExtents := False;
  FUserChange := True;
  FDefaultDrawing := True;
  FBookmarks := TBookmarkList.Create(Self);
  HideEditor;
end;

destructor TVolgaCustomDBGrid.Destroy;
begin
  FColumns.Free;
  FColumns := nil;
  FVisibleColumns.Free;
  FVisibleColumns := nil;
  FDataLink.Free;
  FDataLink := nil;
  FIndicators.Free;
  FTitleFont.Free;
  FTitleFont := nil;
  FBookmarks.Free;
  FBookmarks := nil;
  inherited Destroy;
  ReleaseBitmap;
end;

function TVolgaCustomDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TVolgaCustomDBGrid.RawToDataColumn(ACol: Integer): Integer;
begin
  Result := ACol - FIndicatorOffset;
end;

function TVolgaCustomDBGrid.DataToRawColumn(ACol: Integer): Integer;
begin
  Result := ACol + FIndicatorOffset;
end;

function TVolgaCustomDBGrid.AcquireLayoutLock: Boolean;
begin
  Result := (FUpdateLock = 0) and (FLayoutLock = 0);
  if Result then BeginLayout;
end;

procedure TVolgaCustomDBGrid.BeginLayout;
begin
  BeginUpdate;
  if FLayoutLock = 0 then Columns.BeginUpdate;
  Inc(FLayoutLock);
end;

procedure TVolgaCustomDBGrid.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TVolgaCustomDBGrid.CancelLayout;
begin
  if FLayoutLock > 0 then
  begin
    if FLayoutLock = 1 then
      Columns.EndUpdate;
    Dec(FLayoutLock);
    EndUpdate;
  end;
end;

function TVolgaCustomDBGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  with Columns[SelectedIndex] do
    Result := FDatalink.Active and Assigned(Field) and Field.IsValidChar(Key);
end;

function TVolgaCustomDBGrid.CanEditModify: Boolean;
begin
  Result := False;
  if not ReadOnly and FDatalink.Active and not FDatalink.Readonly then
    with Columns[SelectedIndex] do
      if (not ReadOnly) and Assigned(Field) and
        //Volga - даем редактировать вычисляемые поля
      (Field.CanModify or (FDatalink.DataSet.CanModify and (Field.FieldKind =
        fkCalculated)))
        and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
      begin
        FDatalink.Edit;
        Result := FDatalink.Editing;
        if Result then FDatalink.Modified;
      end;
end;

function TVolgaCustomDBGrid.CanEditShow: Boolean;
begin
  Result := (LayoutLock = 0) and inherited CanEditShow
    and (Columns[SelectedIndex].ButtonStyle <> cbsCheck); //volga
end;

procedure TVolgaCustomDBGrid.CellClick(Column: TVolgaColumn);
begin
  if not (csDesigning in ComponentState) and Column.LongTextAsHint //Volga
    and Assigned(Column.Field) then
  begin                                 //Volga
    //колонка должна показывать хинт?                              //Volga
    Application.CancelHint;             //Volga
    Hint := Column.Field.Text;          //Volga
  end;                                  //Volga
  if Assigned(FOnCellClick) then FOnCellClick(Self, Column);
end;

procedure TVolgaCustomDBGrid.ColEnter;
begin
  UpdateIme;
  if Assigned(FOnColEnter) then FOnColEnter(Self);
end;

procedure TVolgaCustomDBGrid.ColExit;
begin
  if Assigned(FOnColExit) then FOnColExit(Self);
end;

procedure TVolgaCustomDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
var sm: TVolgaSortMark;
begin
  //changes are added by Armando Novello
  FromIndex := RawToDataColumn(FromIndex);
  ToIndex := RawToDataColumn(ToIndex);
  sm := vsmNone;
  if (Columns[FromIndex].CanClick) then //save
    sm := Columns[FromIndex].SortMark;
  Columns[FromIndex].Index := ToIndex;
  if (Columns[ToIndex].CanClick) then   //and restore sort mark at new position
    Columns[ToIndex].SortMark := sm;
  FColMoved := True;
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

procedure TVolgaCustomDBGrid.ColWidthsChanged;
var
  I: Integer;
begin
  inherited ColWidthsChanged;
  if (FDatalink.Active or (FColumns.State = csCustomized)) and
    AcquireLayoutLock then
  try
    for I := FIndicatorOffset to ColCount - 1 do
      FColumns[I - FIndicatorOffset].Width := ColWidths[I];
  finally
    EndLayout;
  end;
end;

function TVolgaCustomDBGrid.CreateColumns: TVolgaDBGridColumns;
begin
  Result := TVolgaDBGridColumns.Create(Self, TVolgaColumn);
end;

function TVolgaCustomDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := TVolgaDBGridInplaceEdit.Create(Self);
end;

procedure TVolgaCustomDBGrid.CreateWnd;
begin
  BeginUpdate; { prevent updates in WMSize message that follows WMCreate }
  try
    inherited CreateWnd;
  finally
    EndUpdate;
  end;
  UpdateRowCount;
  UpdateActive;
  UpdateScrollBar;
  FOriginalImeName := ImeName;
  FOriginalImeMode := ImeMode;
end;

procedure TVolgaCustomDBGrid.DataChanged;
begin
  if not HandleAllocated then Exit;
  UpdateRowCount;
  UpdateScrollBar;
  UpdateActive;
  InvalidateEditor;
  ValidateRect(Handle, nil);
  Invalidate;
end;

procedure TVolgaCustomDBGrid.DefaultHandler(var Msg);
var
  P: TPopupMenu;
  Cell: TGridCoord;
begin
  inherited DefaultHandler(Msg);
  if TMessage(Msg).Msg = wm_RButtonUp then
    with TWMRButtonUp(Msg) do
    begin
      Cell := MouseCoord(XPos, YPos);
      if (Cell.X < FIndicatorOffset) or (Cell.Y < 0) then Exit;
      P := Columns[RawToDataColumn(Cell.X)].PopupMenu;
      if (P <> nil) and P.AutoPopup then
      begin
        SendCancelMode(nil);
        P.PopupComponent := Self;
        with ClientToScreen(SmallPointToPoint(Pos)) do
          P.Popup(X, Y);
        Result := 1;
      end;
    end;
end;

procedure TVolgaCustomDBGrid.DeferLayout;
var
  M: TMsg;
begin
  if HandleAllocated and
    not PeekMessage(M, Handle, cm_DeferLayout, cm_DeferLayout, pm_NoRemove) then
    PostMessage(Handle, cm_DeferLayout, 0, 0);
  CancelLayout;
end;

procedure TVolgaCustomDBGrid.DefineFieldMap;
var
  I: Integer;
begin
  if FColumns.State = csCustomized then
  begin { Build the column/field map from the column attributes }
    DataLink.SparseMap := True;
    for I := 0 to FColumns.Count - 1 do
      FDataLink.AddMapping(FColumns[I].FieldName);
  end
  else { Build the column/field map from the field list order }
  begin
    FDataLink.SparseMap := False;
    with Datalink.Dataset do
      for I := 0 to FieldList.Count - 1 do
        with FieldList[I] do
          if Visible then Datalink.AddMapping(FullName);
  end;
end;

function TVolgaCustomDBGrid.UseRightToLeftAlignmentForField(const AField: TField;
  Alignment: TAlignment): Boolean;
begin
  Result := False;
  if IsRightToLeft then
    Result := OkToChangeFieldAlignment(AField, Alignment);
end;

procedure TVolgaCustomDBGrid.DefaultDrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
var
  Alignment: TAlignment;
  Value: string;
begin
  Alignment := taLeftJustify;
  Value := '';
  if Assigned(Field) then
  begin
    Alignment := Field.Alignment;
    Value := Field.DisplayText;
  end;
  WriteText(Canvas, Rect, 2, 2, Value, Alignment, false,
    UseRightToLeftAlignmentForField(Field, Alignment));
end;

procedure TVolgaCustomDBGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TVolgaColumn; State: TGridDrawState);
var
  Value: string;
begin
  Value := '';
  if Assigned(Column.Field) then
    Value := Column.Field.DisplayText;
  WriteText(Canvas, Rect, 2, 2, Value, Column.Alignment, Column.WrapText,
    UseRightToLeftAlignmentForField(Column.Field, Column.Alignment));
end;

procedure TVolgaCustomDBGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TVolgaCustomDBGrid.WriteColumns(Writer: TWriter);
begin
  Writer.WriteCollection(Columns);
end;

procedure TVolgaCustomDBGrid.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Columns', ReadColumns, WriteColumns,
    ((Columns.State = csCustomized) and (Filer.Ancestor = nil)) or
    ((Filer.Ancestor <> nil) and
    ((Columns.State <> TVolgaCustomDBGrid(Filer.Ancestor).Columns.State) or
{$IFDEF VER140}
    (not CollectionsEqual(Columns, TVolgaCustomDBGrid(Filer.Ancestor).Columns,
    Self.Owner, TVolgaCustomDBGrid(Filer.Ancestor).Columns.FGrid.Owner)))));
{$ELSE}
{$IFDEF VER150}
    (not CollectionsEqual(Columns, TVolgaCustomDBGrid(Filer.Ancestor).Columns,
    Self.Owner, TVolgaCustomDBGrid(Filer.Ancestor).Columns.FGrid.Owner)))));
{$ELSE}
    (not CollectionsEqual(Columns, TVolgaCustomDBGrid(Filer.Ancestor).Columns)))));
{$ENDIF}
{$ENDIF}
end;

function TVolgaCustomDBGrid.ColumnAtDepth(Col: TVolgaColumn; ADepth: Integer):
TVolgaColumn;
begin
  Result := Col;
  while (Result <> nil) and (Result.Depth > ADepth) do
    Result := Result.ParentColumn;
end;

function TVolgaCustomDBGrid.CalcTitleRect(Col: TVolgaColumn; ARow: Integer;
  var MasterCol: TVolgaColumn): TRect;
var
  I, J: Integer;
  InBiDiMode: Boolean;
  DrawInfo: TGridDrawInfo;
begin
  MasterCol := ColumnAtDepth(Col, ARow);
  if MasterCol = nil then Exit;

  I := DataToRawColumn(MasterCol.Index);
  if I >= LeftCol then
    J := MasterCol.Depth
  else
  begin
    //I := LeftCol;
    if Col.Depth > ARow then
      J := ARow
    else
      J := Col.Depth;
  end;

  Result := CellRect(I, J);

  InBiDiMode := UseRightToLeftAlignment and
    (Canvas.CanvasOrientation = coLeftToRight);

  for I := Col.Index to Columns.Count - 1 do
  begin
    if ColumnAtDepth(Columns[I], ARow) <> MasterCol then Break;
    if not InBiDiMode then
    begin
      J := CellRect(DataToRawColumn(I), ARow).Right;
      if J = 0 then Break;
      Result.Right := Max(Result.Right, J);
    end
    else
    begin
      J := CellRect(DataToRawColumn(I), ARow).Left;
      if J >= ClientWidth then Break;
      Result.Left := J;
    end;
  end;
  J := Col.Depth;
  if (J <= ARow) and (J < FixedRows - 1) then
  begin
    CalcFixedInfo(DrawInfo);
    Result.Bottom := DrawInfo.Vert.FixedBoundary - DrawInfo.Vert.EffectiveLineWidth;
  end;
end;

procedure TVolgaCustomDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState:
  TGridDrawState);
var
  FrameOffs: Byte;
  MultiSelected: Boolean;

  //function RowIsMultiSelected: Boolean;
  //var Index: Integer;
  //begin
  //  Result := (dgMultiSelect in Options) and Datalink.Active and
  //    FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  //end;

  procedure DrawTitleCell(ACol, ARow: Integer; Column: TVolgaColumn; var AState:
    TGridDrawState);
  const
    ScrollArrows: array[Boolean, Boolean] of Integer =
    ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    MasterCol: TVolgaColumn;
    TitleRect, TextRect, ButtonRect: TRect;
    I: Integer;
    InBiDiMode: Boolean;
    AFont: TFont;                       //Volga
    AColor: TColor;                     //Volga
  begin
    TitleRect := CalcTitleRect(Column, ARow, MasterCol);

    if MasterCol = nil then
    begin
      Canvas.FillRect(ARect);
      Exit;
    end;

    AFont := MasterCol.Title.Font;      //Volga
    AColor := MasterCol.Title.Color;    //Volga
    if Assigned(FOnDrawTitleAttr) then FOnDrawTitleAttr(Self, MasterCol, AFont, AColor);  //Volga
    Canvas.Font := AFont;               //Volga
    Canvas.Brush.Color := AColor;       //Volga
    if [dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines] then
      InflateRect(TitleRect, -1, -1);
    TextRect := TitleRect;
    I := GetSystemMetrics(SM_CXHSCROLL);
    if ((TextRect.Right - TextRect.Left) > I) and MasterCol.Expandable then
    begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;
      I := SaveDC(Canvas.Handle);
      try
        Canvas.FillRect(ButtonRect);
        InflateRect(ButtonRect, -1, -1);
        IntersectClipRect(Canvas.Handle, ButtonRect.Left,
          ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
        InflateRect(ButtonRect, 1, 1);
        { DrawFrameControl doesn't draw properly when orienatation has changed.
          It draws as ExtTextOut does. }
        InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
        if InBiDiMode then              { stretch the arrows box }
          Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
        DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
          ScrollArrows[InBiDiMode, MasterCol.Expanded] or DFCS_FLAT);
      finally
        RestoreDC(Canvas.Handle, I);
      end;
    end;
    with MasterCol.Title do
      if MasterCol.Down then            //Volga
        WriteText(Canvas, TextRect, FrameOffs + 1, FrameOffs + 1, Caption, Alignment,  //Volga
          true, IsRightToLeft)          //Volga
      else
      begin
        WriteText(Canvas, TextRect, FrameOffs, FrameOffs, Caption, Alignment,
          true, IsRightToLeft);         //volga
        FIndicators.BkColor := AColor;
        I := TextRect.Right - FIndicators.Width - FrameOffs;
        if Canvas.CanvasOrientation = coRightToLeft then Inc(I);
        case MasterCol.SortMark of
          vsmDown:
            FIndicators.Draw(Canvas, I,
              (TextRect.Top + TextRect.Bottom - FIndicators.Height) shr 1, 5, True);
          vsmUp:
            FIndicators.Draw(Canvas, I,
              (TextRect.Top + TextRect.Bottom - FIndicators.Height) shr 1, 6, True);
        end;
      end;
    if [dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines] then
    begin
      InflateRect(TitleRect, 1, 1);
      if MasterCol.Down then
      begin                             //Volga заголовок нажат!!
        DrawEdge(Canvas.Handle, TitleRect, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, BDR_SUNKENINNER, BF_TOPLEFT);
      end
      else
      begin
        DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_TOPLEFT);
      end;
    end;
    AState := AState - [gdFixed];       // prevent box drawing later
  end;

  procedure DrawCheckBoxColumn(ARect: TRect; Column: TVolgaColumn;
    AState: TGridDrawState; Value: string); //Volga
  var CRect: TRect;
    h, l, t: integer;
  begin
    Canvas.FillRect(ARect); //заполняем фон, если клетка выделена, то выделенным цветом
    h := ARect.Bottom - ARect.Top - 4;  //высота чекбокса
    if h > (ARect.Right - ARect.Left - 2) then h := ARect.Right - ARect.Left - 4;
    l := (ARect.Right - ARect.Left - h) div 2; //отступ от левого края
    t := (ARect.Bottom - ARect.Top - h) div 2; //отступ от верхнего края
    CRect := Rect(ARect.Left + l, ARect.Top + t, ARect.Right - l, ARect.Bottom - t);
    if (MultiSelected and (Column.FFieldName = 'Selected'))
      or (Value = Column.ValueChecked) then //рисуем крестик
      DrawFrameControl(Canvas.Handle, CRect, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED)
    else
      DrawFrameControl(Canvas.Handle, CRect, DFC_BUTTON, DFCS_BUTTONCHECK);
  end;                                  //Volga

var
  OldActive: Integer;
  Indicator: Integer;
  Highlight: Boolean;
  Index: Integer;
  Value: string;
  VarValue: Variant;
  DrawColumn: TVolgaColumn;
  ALeft: Integer;
  ind: integer;                         //Volga
  AFont: TFont;                         //Volga
  AColor: TColor;                       //Volga
begin
  if csLoading in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
    Exit;
  end;

  Dec(ARow, FTitleOffset);
  Dec(ACol, FIndicatorOffset);

  if (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines]) then
  begin
    InflateRect(ARect, -1, -1);
    FrameOffs := 2;
  end
  else
    FrameOffs := 3;

  //проверить запись на выделенную при мультиселекте
  MultiSelected := False;
  if (dgMultiSelect in Options) and Assigned(DataLink) and Datalink.Active and (ARow >= 0)
    then
  begin
    OldActive := FDataLink.ActiveRecord;
    try
      FDatalink.ActiveRecord := ARow;
      MultiSelected := FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
    finally
      FDatalink.ActiveRecord := OldActive;
    end;
  end;

  if (gdFixed in AState) and (ACol < 0) then
  begin
    Canvas.Brush.Color := FixedColor;
    Canvas.FillRect(ARect);
    if Assigned(DataLink) and DataLink.Active then
    begin
      if (ARow = FDataLink.ActiveRecord) or MultiSelected then
      begin
        Indicator := 0;
        if FDataLink.DataSet <> nil then
          case FDataLink.DataSet.State of
            dsEdit: Indicator := 1;
            dsInsert: Indicator := 2;
            dsBrowse:
              if MultiSelected then
                if (ARow <> FDatalink.ActiveRecord) then
                  Indicator := 3
                else
                  Indicator := 4;       // multiselected and current row
          end;
        FIndicators.BkColor := FixedColor;
        ALeft := ARect.Right - FIndicators.Width - FrameOffs;
        if Canvas.CanvasOrientation = coRightToLeft then Inc(ALeft);
        FIndicators.Draw(Canvas, ALeft,
          (ARect.Top + ARect.Bottom - FIndicators.Height) shr 1, Indicator, True);
        if ARow = FDatalink.ActiveRecord then
          FSelRow := ARow + FTitleOffset;
      end;
    end;
  end
  else
    with Canvas do
    begin
      DrawColumn := Columns[ACol];
      if not DrawColumn.Showing then Exit;
      Font := DrawColumn.Font;          //Volga
      if not (gdFixed in AState) then
      begin
        //Font := DrawColumn.Font;
        Brush.Color := DrawColumn.Color;
      end
      else                              //Volga
        Brush.Color := FixedColor;      //Volga
      if ARow < 0 then
        DrawTitleCell(ACol, ARow + FTitleOffset, DrawColumn, AState)
      else if (FDataLink = nil) or not FDataLink.Active then
        FillRect(ARect)
      else
      begin
        Value := '';
        OldActive := FDataLink.ActiveRecord;
        try
          if FDataLink.ActiveRecord <> ARow then
            FDataLink.ActiveRecord := ARow;
          if (FDataLink.Dataset.RecordCount > 0) or (FDataLink.Dataset.State = dsInsert)
            then
          begin
            if Assigned(DrawColumn.Field) then
            begin
              if DrawColumn.Field is TMemoField then
              begin                     //volga
                Value := DrawColumn.Field.AsString; //volga
                ind := Pos(#13#10, Value);
                if ind > 0 then Value := Copy(Value, 1, ind - 1);
              end
              else
                Value := DrawColumn.Field.DisplayText;
              case DrawColumn.ButtonStyle of
                cbsCombo:
                  if (DrawColumn.PickValues <> nil) and (DrawColumn.PickList.Count > 0)
                    and                 //volga
                    (DrawColumn.PickValues.Count = DrawColumn.PickList.Count) then
                  begin                 //volga
                    ind := DrawColumn.PickValues.IndexOf(DrawColumn.Field.Text); //volga
                    if ind >= 0 then    //volga
                      Value := DrawColumn.PickList[ind] //volga
                    else
                      Value := '';      //volga
                  end;
                cbsLookup:
                  with DrawColumn do
                    if IsLinkActive then
                    begin               //напрямую ищем в Lookup-таблице!!!!!!!
                      if LookupLinkField = FViewField then
                        //для бал.счетов например (иначе буква Д не показывается)
                        VarValue := FDataLink.Dataset.FieldValues[FieldName]
                      else
                        VarValue := LookupDataSet.Lookup(LookupLinkField,
                          FDataLink.Dataset.FieldValues[LookupKeyField], FViewField);
                      if not VarIsNull(VarValue) then
                        Value := VarValue
                      else
                        Value := '';
                    end;
              end;
            end;
          end;
          Highlight := HighlightCell(ACol, ARow, Value, AState);
          if Highlight then
          begin
            Brush.Color := FHColor;     //volga
            Font.Color := FHTColor;     //volga
          end;
          if not Enabled then
            Font.Color := clGrayText;
          AColor := Brush.Color;        //volga
          AFont := Font;                //volga
          if Assigned(FOnDrawCellAttr) then //volga
            FOnDrawCellAttr(Self, DrawColumn, AFont, AColor, AState);
          Brush.Color := AColor;        //volga
          Font := AFont;                //volga
          if FDefaultDrawing then
            if DrawColumn.ButtonStyle <> cbsCheck then
              WriteText(Canvas, ARect, 2, 2, Value, DrawColumn.Alignment,
                DrawColumn.WrapText,
                UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment))
            else
              DrawCheckBoxColumn(ARect, DrawColumn, AState, Value); //volga
          if Columns.State = csDefault then
            DrawDataCell(ARect, DrawColumn.Field, AState);
          DrawColumnCell(ARect, ACol, DrawColumn, AState);  //вызов юзерского рисования,если назначено
        finally
          if FDataLink.ActiveRecord <> OldActive then
            FDataLink.ActiveRecord := OldActive;
        end;
        if FDefaultDrawing and (gdSelected in AState)
          and ((dgAlwaysShowSelection in Options) or Focused)
          and not (csDesigning in ComponentState)
          and not (dgRowSelect in Options) and (UpdateLock = 0)
          and (ValidParentForm(Self).ActiveControl = Self) then
          //здесь можно нарисовать кнопку, если она нужна
          Windows.DrawFocusRect(Handle, ARect);
      end;
    end;
  if (gdFixed in AState) and ([dgRowLines, dgColLines] * Options =
    [dgRowLines, dgColLines]) then
  begin
    InflateRect(ARect, 1, 1);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
  end;
end;

procedure TVolgaCustomDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  if Assigned(FOnDrawDataCell) then FOnDrawDataCell(Self, Rect, Field, State);
end;

procedure TVolgaCustomDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TVolgaColumn; State: TGridDrawState);
begin
  if Assigned(OnDrawColumnCell) then
    OnDrawColumnCell(Self, Rect, DataCol, Column, State);
end;

procedure TVolgaCustomDBGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self, Columns[SelectedIndex])
  else
    ShowPopupEditor(Columns[SelectedIndex]);
end;

procedure TVolgaCustomDBGrid.EditingChanged;
begin
  if dgIndicator in Options then InvalidateCell(0, FSelRow);
end;

procedure TVolgaCustomDBGrid.EndLayout;
begin
  if FLayoutLock > 0 then
  begin
    try
      try
        if FLayoutLock = 1 then
          InternalLayout;
      finally
        if FLayoutLock = 1 then
          FColumns.EndUpdate;
      end;
    finally
      Dec(FLayoutLock);
      EndUpdate;
    end;
  end;
end;

procedure TVolgaCustomDBGrid.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TVolgaCustomDBGrid.GetColField(DataCol: Integer): TField;
begin
  Result := nil;
  if (DataCol >= 0) and FDatalink.Active and (DataCol < Columns.Count) then
    Result := Columns[DataCol].Field;
end;

function TVolgaCustomDBGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TVolgaCustomDBGrid.GetEditLimit: Integer;
begin
  Result := 0;
  if Assigned(SelectedField) and (SelectedField.DataType = ftString) then
    Result := SelectedField.Size;
end;

function TVolgaCustomDBGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if FDatalink.Active then
    with Columns[RawToDataColumn(ACol)] do
      if Assigned(Field) then
        Result := Field.EditMask;
end;

function TVolgaCustomDBGrid.GetEditText(ACol, ARow: Longint): string;
var ind: integer;                       //Volga
  VarValue: Variant;
begin
  Result := '';
  if FDatalink.Active then
    with Columns[RawToDataColumn(ACol)] do
      if Assigned(Field) then
      begin
        Result := Field.Text;           //Volga  всегда из поля БД!!!
        case ButtonStyle of
          cbsCombo:
            if (PickValues <> nil) and (PickList.Count > 0) and
              (PickValues.Count = PickList.Count) then
            begin                       //Volga
              ind := PickValues.IndexOf(Field.Text); //Volga
              if ind >= 0 then          //Volga
                Result := PickList[ind] //Volga
              else
                Result := '';           //Volga
            end;
          cbsLookup:
            if IsLinkActive then
            begin                       //напрямую ищем в Lookup-таблице!!!!!!!
              if LookupLinkField = FViewField then
                //для бал.счетов например (иначе буква Д не показывается)
                VarValue := FDataLink.Dataset.FieldValues[FieldName]
              else
                VarValue := LookupDataSet.Lookup(LookupLinkField,
                  FDataLink.Dataset.FieldValues[LookupKeyField], FViewField);
              if not VarIsNull(VarValue) then
                Result := VarValue
              else
                Result := '';
            end;
        end;
      end;
  FEditText := Result;
end;

function TVolgaCustomDBGrid.GetFieldCount: Integer;
begin
  Result := FDatalink.FieldCount;
end;

function TVolgaCustomDBGrid.GetFields(FieldIndex: Integer): TField;
begin
  Result := FDatalink.Fields[FieldIndex];
end;

function TVolgaCustomDBGrid.GetFieldValue(ACol: Integer): string;
var
  Field: TField;
begin
  Result := '';
  Field := GetColField(ACol);
  if Field <> nil then Result := Field.DisplayText;
end;

function TVolgaCustomDBGrid.GetSelectedField: TField;
var
  Index: Integer;
begin
  Index := SelectedIndex;
  if Index <> -1 then
    Result := Columns[Index].Field
  else
    Result := nil;
end;

function TVolgaCustomDBGrid.GetSelectedIndex: Integer;
begin
  Result := RawToDataColumn(Col);
end;

function TVolgaCustomDBGrid.HighlightCell(DataCol, DataRow: Integer;
  const Value: string; AState: TGridDrawState): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (dgMultiSelect in Options) and Datalink.Active then
    Result := FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  if not Result then
    Result := (gdSelected in AState)
      and ((dgAlwaysShowSelection in Options) or Focused)
        { updatelock eliminates flicker when tabbing between rows }
    and ((UpdateLock = 0) or (dgRowSelect in Options));
end;

procedure TVolgaCustomDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var KeyDownEvent: TKeyEvent;
  AColumn: TVolgaColumn;

  procedure ClearSelection;
  begin
    if (dgMultiSelect in Options) then
    begin
      FBookmarks.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
  begin
    AddAfter := False;
    BeginUpdate;
    try
      if (dgMultiSelect in Options) and FDatalink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            FSelectionAnchor := FBookmarks.CurrentRow;
            FBookmarks.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
            with FBookmarks do
            begin
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              if not AddAfter then
                CurrentRowSelected := False;
            end
        end
        else
          ClearSelection;
      FDatalink.MoveBy(Direction);
      if AddAfter then FBookmarks.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;
  end;

  procedure NextRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
    begin
      if (State = dsInsert) and not Modified and not FDatalink.FModified then
        if FDataLink.EOF then
          Exit
        else
          Cancel
      else
        DoSelection(Select, 1);
      if FDataLink.EOF and CanModify and (not ReadOnly) and (dgEditing in Options)
        and AllowInsert then            //Volga
        Append;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
      if (State = dsInsert) and not Modified and FDataLink.EOF and
        not FDatalink.FModified then
        Cancel
      else
        DoSelection(Select, -1);
  end;

  procedure Tab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    BeginUpdate; { Prevent highlight flicker on tab to next/prior row }
    try
      while True do
      begin
        if GoForward then
          Inc(ACol)
        else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          NextRow(False);
          ACol := FIndicatorOffset;
        end
        else if ACol < FIndicatorOffset then
        begin
          PriorRow(False);
          ACol := ColCount - FIndicatorOffset;
        end;
        if ACol = Original then Exit;
        if TabStops[ACol] then
        begin
          MoveCol(ACol, 0);
          Exit;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;

  function DeletePrompt: Boolean;
  var
    Msg: string;
  begin
    if (FBookmarks.Count > 1) then
      Msg := V_DELETEROWS               //Volga
    else
      Msg := V_DELETEONEROW;            //Volga
    Result := AllowDelete and (not (dgConfirmDelete in Options) or //Volga
      (MessageDlg(Msg, mtConfirmation, mbOKCancel, 0) <> idCancel));
  end;

const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END];

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then KeyDownEvent(Self, Key, Shift);
  if not FDatalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;
  if UseRightToLeftAlignment then
    if Key = VK_LEFT then
      Key := VK_RIGHT
    else if Key = VK_RIGHT then
      Key := VK_LEFT;
  AColumn := Columns[SelectedIndex];    //volga
  if AColumn.ButtonStyle = cbsCheck then
  begin                                 //volga
    if Key = VK_SPACE then ToggleCheckBox(AColumn, Col, Row); //volga
  end;
  with FDatalink.DataSet do
    if ssCtrl in Shift then
    begin
      if (Key in RowMovementKeys) then ClearSelection;
      case Key of
        VK_UP, VK_PRIOR: FDataLink.MoveBy(-FDatalink.ActiveRecord);
        VK_DOWN, VK_NEXT:
          FDataLink.MoveBy(FDatalink.BufferCount - FDatalink.ActiveRecord
            - 1);
        VK_LEFT: MoveCol(FIndicatorOffset, 1);
        VK_RIGHT: MoveCol(ColCount - 1, -1);
        VK_HOME: First;
        VK_END: Last;
        VK_DELETE:
          if (not ReadOnly) and not IsEmpty
            and CanModify and DeletePrompt then
            if FBookmarks.Count > 0 then
              FBookmarks.Delete
            else
              Delete;
      end
    end
    else
      case Key of
        VK_UP: PriorRow(True);
        VK_DOWN: NextRow(True);
        VK_LEFT:
          if dgRowSelect in Options then
            PriorRow(False)
          else
            MoveCol(Col - 1, -1);
        VK_RIGHT:
          if dgRowSelect in Options then
            NextRow(False)
          else
            MoveCol(Col + 1, 1);
        VK_HOME:
          if (ColCount = FIndicatorOffset + 1)
            or (dgRowSelect in Options) then
          begin
            ClearSelection;
            First;
          end
          else
            MoveCol(FIndicatorOffset, 1);
        VK_END:
          if (ColCount = FIndicatorOffset + 1)
            or (dgRowSelect in Options) then
          begin
            ClearSelection;
            Last;
          end
          else
            MoveCol(ColCount - 1, -1);
        VK_NEXT:
          begin
            ClearSelection;
            FDataLink.MoveBy(VisibleRowCount);
          end;
        VK_PRIOR:
          begin
            ClearSelection;
            FDataLink.MoveBy(-VisibleRowCount);
          end;
        VK_INSERT:
          if CanModify and (not ReadOnly) and (dgEditing in Options)
            and AllowInsert then        //Volga
          begin
            ClearSelection;
            Insert;
          end;
        VK_TAB:
          if not (ssAlt in Shift) then Tab(not (ssShift in Shift));
        VK_RETURN:
          if dgEnterToTab in Options then
            if not (ssAlt in Shift) then Tab(not (ssShift in Shift)); //Volga
        VK_ESCAPE:
          begin
            if SysLocale.PriLangID = LANG_KOREAN then
              FIsESCKey := True;
            FDatalink.Reset;
            ClearSelection;
            if not (dgAlwaysShowEditor in Options) then HideEditor;
          end;
        VK_F2: EditorMode := True;
      end;
end;

procedure TVolgaCustomDBGrid.KeyPress(var Key: Char);
begin
  FIsESCKey := False;
  if (dgEnterToTab in Options) and (ord(Key) = VK_RETURN) then Key := #9;
  if not (dgAlwaysShowEditor in Options) and (Key = #13) then
    FDatalink.UpdateData;
  inherited KeyPress(Key);
end;

{ InternalLayout is called with layout locks and column locks in effect }

procedure TVolgaCustomDBGrid.InternalLayout;

  function FieldIsMapped(F: TField): Boolean;
  var
    X: Integer;
  begin
    Result := False;
    if F = nil then Exit;
    for X := 0 to FDatalink.FieldCount - 1 do
      if FDatalink.Fields[X] = F then
      begin
        Result := True;
        Exit;
      end;
  end;

  procedure CheckForPassthroughs;       // check for Columns.State flip-flop
  var
    SeenPassthrough: Boolean;
    I, J: Integer;
    Column: TVolgaColumn;
  begin
    SeenPassthrough := False;
    for I := 0 to FColumns.Count - 1 do
      if not FColumns[I].IsStored then
        SeenPassthrough := True
      else if SeenPassthrough then
      begin // we have both persistent and non-persistent columns.  Kill the latter
        for J := FColumns.Count - 1 downto 0 do
        begin
          Column := FColumns[J];
          if not Column.IsStored then
            Column.Free;
        end;
        Exit;
      end;
  end;

  procedure ReSetColumnFieldBindings;
  var
    I, J, K: Integer;
    Fld: TField;
    Column: TVolgaColumn;
  begin
    if FColumns.State = csDefault then
    begin
       { Destroy columns whose fields have been destroyed or are no longer
         in field map }
      if (not FDataLink.Active) and (FDatalink.DefaultFields) then
        FColumns.Clear
      else
        for J := FColumns.Count - 1 downto 0 do
          with FColumns[J] do
            if not Assigned(Field)
              or not FieldIsMapped(Field) then Free;
      I := FDataLink.FieldCount;
      if (I = 0) and (FColumns.Count = 0) then Inc(I);
      for J := 0 to I - 1 do
      begin
        Fld := FDatalink.Fields[J];
        if Assigned(Fld) then
        begin
          K := J;
           { Pointer compare is valid here because the grid sets matching
             column.field properties to nil in response to field object
             free notifications.  Closing a dataset that has only default
             field objects will destroy all the fields and set associated
             column.field props to nil. }
          while (K < FColumns.Count) and (FColumns[K].Field <> Fld) do
            Inc(K);
          if K < FColumns.Count then
            Column := FColumns[K]
          else
          begin
            Column := FColumns.InternalAdd;
            Column.Field := Fld;
          end;
        end
        else
          Column := FColumns.InternalAdd;
        Column.Index := J;
      end;
    end
    else
    begin
      { Force columns to reaquire fields (in case dataset has changed) }
      for I := 0 to FColumns.Count - 1 do
        FColumns[I].Field := nil;
    end;
  end;

  procedure MeasureTitleHeights;
  var
    I, J, K, D, B: Integer;
    RestoreCanvas: Boolean;
    Heights: array of Integer;
  begin
    RestoreCanvas := not HandleAllocated;
    if RestoreCanvas then
      Canvas.Handle := GetDC(0);
    try
      Canvas.Font := Font;
      K := Canvas.TextHeight('Wg') + 3;
      if dgRowLines in Options then
        Inc(K, GridLineWidth);
      if (FRowsHeight > 0) and (K < FRowsHeight) then //Volga
        DefaultRowHeight := FRowsHeight //Volga
      else                              //Volga
        DefaultRowHeight := K;
      B := GetSystemMetrics(SM_CYHSCROLL);
      if dgTitles in Options then
      begin
        SetLength(Heights, FTitleOffset + 1);
        for I := 0 to FColumns.Count - 1 do
        begin
          Canvas.Font := FColumns[I].Title.Font;
          D := FColumns[I].Depth;
          if D <= High(Heights) then
          begin
            J := Canvas.TextHeight('Wg') + 4;
            if FColumns[I].Expandable and (B > J) then
              J := B;
            Heights[D] := Max(J, Heights[D]);
          end;
        end;
        if Heights[0] = 0 then
        begin
          Canvas.Font := FTitleFont;
          Heights[0] := Canvas.TextHeight('Wg') + 4;
        end;
        if (FTitleHeight > 0) and (Heights[0] < FTitleHeight) then //Volga
          Heights[0] := FTitleHeight;   //Volga
        for I := 0 to High(Heights) - 1 do
          RowHeights[I] := Heights[I];
      end;
    finally
      if RestoreCanvas then
      begin
        ReleaseDC(0, Canvas.Handle);
        Canvas.Handle := 0;
      end;
    end;
  end;

var
  I, J: Integer;
begin
  if (csLoading in ComponentState) then Exit;

  if HandleAllocated then KillMessage(Handle, cm_DeferLayout);

  CheckForPassthroughs;
  FIndicatorOffset := 0;
  if dgIndicator in Options then
    Inc(FIndicatorOffset);
  FDatalink.ClearMapping;
  if FDatalink.Active then DefineFieldMap;
  DoubleBuffered := (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView;
  ReSetColumnFieldBindings;
  FVisibleColumns.Clear;
  for I := 0 to FColumns.Count - 1 do
    if FColumns[I].Showing then FVisibleColumns.Add(FColumns[I]);
  ColCount := FColumns.Count + FIndicatorOffset;
  //inherited FixedCols := FIndicatorOffset;
  FTitleOffset := 0;
  if dgTitles in Options then
  begin
    FTitleOffset := 1;
    if (FDatalink <> nil) and (FDatalink.Dataset <> nil)
      and FDatalink.Dataset.ObjectView then
    begin
      for I := 0 to FColumns.Count - 1 do
      begin
        if FColumns[I].Showing then
        begin
          J := FColumns[I].Depth;
          if J >= FTitleOffset then FTitleOffset := J + 1;
        end;
      end;
    end;
  end;
  UpdateRowCount;
  MeasureTitleHeights;
  SetColumnAttributes;
  UpdateActive;
  Invalidate;
end;

procedure TVolgaCustomDBGrid.LayoutChanged;
begin
  if AcquireLayoutLock then
    EndLayout;
end;

procedure TVolgaCustomDBGrid.LinkActive(Value: Boolean);
var
  Comp: TComponent;
  I: Integer;
begin
  if not Value then HideEditor;
  FBookmarks.LinkActive(Value);
  try
    LayoutChanged;
  finally
    for I := ComponentCount - 1 downto 0 do
    begin
      Comp := Components[I];            // Free all the popped-up subgrids
      if (Comp is TVolgaCustomDBGrid)
        and (TVolgaCustomDBGrid(Comp).DragKind = dkDock) then
        Comp.Free;
    end;
    UpdateScrollBar;
    if Value and (dgAlwaysShowEditor in Options) then ShowEditor;
  end;
end;

procedure TVolgaCustomDBGrid.Loaded;
begin
  inherited Loaded;
  if FColumns.Count > 0 then
    ColCount := FColumns.Count;
  LayoutChanged;
end;

function TVolgaCustomDBGrid.PtInExpandButton(X, Y: Integer; var MasterCol: TVolgaColumn):
Boolean;
var
  Cell: TGridCoord;
  R: TRect;
begin
  MasterCol := nil;
  Result := False;
  Cell := MouseCoord(X, Y);
  if (Cell.Y < FTitleOffset) and FDatalink.Active
    and (Cell.X >= FIndicatorOffset)
    and (RawToDataColumn(Cell.X) < Columns.Count) then
  begin
    R := CalcTitleRect(Columns[RawToDataColumn(Cell.X)], Cell.Y, MasterCol);
    if not UseRightToLeftAlignment then
      R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL)
    else
      R.Right := R.Left + GetSystemMetrics(SM_CXHSCROLL);
    Result := MasterCol.Expandable and PtInRect(R, Point(X, Y));
  end;
end;

procedure TVolgaCustomDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  OldCol, OldRow: Integer;
  MasterCol: TVolgaColumn;
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;

  if Sizing(X, Y) then
  begin
    FDatalink.UpdateData;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  Cell := MouseCoord(X, Y);
  if (Cell.X < 0) and (Cell.Y < 0) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  if (DragKind = dkDock) and (Cell.X < FIndicatorOffset) and
    (Cell.Y < FTitleOffset) and (not (csDesigning in ComponentState)) then
  begin
    BeginDrag(false);
    Exit;
  end;

  if PtInExpandButton(X, Y, MasterCol) then
  begin
    MasterCol.Expanded := not MasterCol.Expanded;
    ReleaseCapture;
    UpdateDesigner;
    Exit;
  end;

  if ((csDesigning in ComponentState) {or (dgColumnResize in Options)}) and //Volga
    (Cell.Y < FTitleOffset) then
  begin
    FDataLink.UpdateData;
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  //Volga попали на заголовочную ячейку во время выполнения
  if ((Cell.Y < FTitleOffset) and (Cell.X >= FIndicatorOffset)) then
  begin
    FDataLink.UpdateData;
    MasterCol := Columns[RawToDataColumn(Cell.X)];
    FColMoved := False;
    if (MasterCol <> nil) and MasterCol.CanClick then
    begin
      MasterCol.Down := True;
      InvalidateCell(Cell.X, Cell.Y);   //перерисовываем ячейку нажатой
    end;
    inherited MouseDown(Button, Shift, X, Y); //added by Armando Novello
    Exit;
  end;                                  //Volga

  if FDatalink.Active then
    with Cell do
    begin
      BeginUpdate; { eliminates highlight flicker when selection moves }
      try
        FDatalink.UpdateData;           // validate before moving
        HideEditor;
        OldCol := Col;
        OldRow := Row;
        if (Y >= FTitleOffset) and (Y - Row <> 0) then
          FDatalink.MoveBy(Y - Row);
        if X >= FIndicatorOffset then
        begin
          MoveCol(X, 0);
          MasterCol := Columns[RawToDataColumn(X)]; //с какой колонкой работаем? volga
          if (dgMultiSelect in Options) and (MasterCol.FFieldName <> 'Selected') then  //volga
            with FBookmarks do
            begin
              FSelecting := False;
              if ssCtrl in Shift then
                CurrentRowSelected := not CurrentRowSelected
              else
              begin
                Clear;
                CurrentRowSelected := True;
              end;
            end;
          if (Button = mbLeft) and (X >= FIndicatorOffset) then
          begin
            if MasterCol.ButtonStyle = cbsCheck then
            begin                       //переключить чекбокс
              ToggleCheckBox(MasterCol, X, Y); //volga
              ReleaseCapture;           //volga
            end
            else if (((X = OldCol) and (Y = OldRow)) or (dgAlwaysShowEditor in Options))
              then
              ShowEditor                { put grid in edit mode }
            else
              InvalidateEditor;         { draw editor, if needed }
          end;
        end;
      finally
        EndUpdate;
      end;
      if (Button = mbLeft) and (DragKind = dkDrag) and (dgDragOutRows in Options) then
        //начинаем операцию drag
        BeginDrag(false, 5);
    end
  else                                  //кликнули на индикаторную ячейку
    inherited;
end;

procedure TVolgaCustomDBGrid.ToggleCheckBox(Column: TVolgaColumn; col, row: integer);
var value: string;                      //Volga
  tempField: TField;
begin
  if (dgMultiSelect in Options) and FDatalink.Active and (Column.FFieldName = 'Selected')
    then
    FBookmarks.CurrentRowSelected := not FBookmarks.CurrentRowSelected
  else
  begin
    tempField := Column.Field;
    if not Assigned(tempField) then exit;
    if Column.ReadOnly or not (dgEditing in Options) then Exit;
    if not DataSource.AutoEdit and
      not (DataSource.state in [dsEdit, dsInsert]) then Exit;
    value := tempField.Text;
    if value = Column.ValueChecked then
      value := Column.ValueUnChecked
    else
      value := Column.ValueChecked;
    if (tempField.FieldKind <> fkCalculated) then //Volga
    begin
      DataLink.Edit;
      tempField.Text := value;
    end
    else if Assigned(FOnCalcFieldEdited) then //Volga
      FOnCalcFieldEdited(Self, tempField, value); //Volga
    DrawCell(col, row, CellRect(col, row), [gdSelected]);
  end;
end;                                    //Volga

procedure TVolgaCustomDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  SaveState: TGridState;
  i, isel, idown: integer;              //Volga
begin
  SaveState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);
  if (SaveState = gsRowSizing) or       //(SaveState = gsColSizing)
    ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
    (PtInRect(InplaceEditor.BoundsRect, Point(X, Y)))) then Exit;
  if (SaveState = gsColSizing) and Assigned(FOnColumnResized) then //Volga
  begin
    Cell := MouseCoord(X - 5, Y);       //чтобы не оказаться на соседней колонке
    isel := RawToDataColumn(Cell.X);    //Volga
    FOnColumnResized(self, Columns[isel]); //Volga
    Exit;
  end;
  Cell := MouseCoord(X, Y);
  isel := RawToDataColumn(Cell.X);      //Volga
  //перебираем все колонки и отжимаем их все
  idown := -1;
  for i := 0 to Columns.Count - 1 do    //Volga
  begin
    if Columns[i].Down then
    begin                               //Volga
      Columns[i].Down := False;         //Volga
      idown := i;
    end;                                //Volga
    InvalidateCell(DataToRawColumn(i), 0);  //Volga перерисовать колонку отжатой или без sortmark
  end;
  if (Button = mbLeft) and (Cell.X >= FIndicatorOffset) and (Cell.Y >= 0) then
    if Cell.Y < FTitleOffset then
      if (Columns[isel].CanClick and (idown = isel)) then //Volga
        TitleClick(Columns[isel]) //Volga отжата именно та колонка, кот.была ранее нажата
      else
    else
      CellClick(Columns[isel]);         //Volga
  if idown >= 0 then
    InvalidateCell(DataToRawColumn(idown), 0); //Volga перерисовать колонку отжатой
end;

procedure TVolgaCustomDBGrid.MoveCol(RawCol, Direction: Integer);
var
  OldCol: Integer;
begin
  FDatalink.UpdateData;
  if RawCol >= ColCount then
    RawCol := ColCount - 1;
  if RawCol < FFixedCols + FIndicatorOffset then //Volga
    RawCol := FFixedCols + FIndicatorOffset; //Volga
  if Direction <> 0 then
  begin
    while (RawCol < ColCount) and (RawCol >= FFixedCols + FIndicatorOffset) and //Volga
      (ColWidths[RawCol] <= 0) do
      Inc(RawCol, Direction);
    if (RawCol >= ColCount) or (RawCol < FFixedCols + FIndicatorOffset) then Exit; //Volga
  end;
  OldCol := Col;
  if RawCol <> OldCol then
  begin
    if not FInColExit then
    begin
      FInColExit := True;
      try
        ColExit;
      finally
        FInColExit := False;
      end;
      if Col <> OldCol then Exit;
    end;
    if not (dgAlwaysShowEditor in Options) then HideEditor;
    Col := RawCol;
    ColEnter;
  end;
end;

procedure TVolgaCustomDBGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
  NeedLayout: Boolean;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent is TPopupMenu) then
    begin
      for I := 0 to Columns.Count - 1 do
        if Columns[I].PopupMenu = AComponent then
          Columns[I].PopupMenu := nil;
    end
    else if (AComponent is TDataSet) then
    begin
      NeedLayout := False;
      BeginLayout;
      try
        for I := 0 to Columns.Count - 1 do
          with Columns[I] do
            if LookupDataSet = AComponent then
            begin
              LookupDataSet := nil;
              NeedLayout := True;
            end;
      finally
        if NeedLayout and Assigned(FDatalink.Dataset)
          and not FDatalink.Dataset.ControlsDisabled then
          EndLayout
        else
          DeferLayout;
      end;
    end
    else if (FDataLink <> nil) then
      if (AComponent = DataSource) then
        DataSource := nil
      else if (AComponent is TField) then
      begin
        NeedLayout := False;
        BeginLayout;
        try
          for I := 0 to Columns.Count - 1 do
            with Columns[I] do
              if Field = AComponent then
              begin
                Field := nil;
                NeedLayout := True;
              end;
        finally
          if NeedLayout and Assigned(FDatalink.Dataset)
            and not FDatalink.Dataset.ControlsDisabled then
            EndLayout
          else
            DeferLayout;
        end;
      end;
  end;
end;

procedure TVolgaCustomDBGrid.RecordChanged(Field: TField);
var
  I: Integer;
  CField: TField;
begin
  if not HandleAllocated then Exit;
  if Field = nil then
    Invalidate
  else
  begin
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Field = Field then
        InvalidateCol(DataToRawColumn(I));
  end;
  CField := SelectedField;
  if ((Field = nil) or (CField = Field)) and
    (Assigned(CField) and (CField.Text <> FEditText) and
    ((SysLocale.PriLangID <> LANG_KOREAN) or FIsESCKey)) then
  begin
    InvalidateEditor;
    if InplaceEditor <> nil then InplaceEditor.Deselect;
  end;
end;

procedure TVolgaCustomDBGrid.Scroll(Distance: Integer);
var
  OldRect, NewRect: TRect;
  RowHeight: Integer;
begin
  if not HandleAllocated then Exit;
  OldRect := BoxRect(0, Row, ColCount - 1, Row);
  if (FDataLink.ActiveRecord >= RowCount - FTitleOffset) then UpdateRowCount;
  UpdateScrollBar;
  UpdateActive;
  NewRect := BoxRect(0, Row, ColCount - 1, Row);
  ValidateRect(Handle, @OldRect);
  InvalidateRect(Handle, @OldRect, False);
  InvalidateRect(Handle, @NewRect, False);
  if Distance <> 0 then
  begin
    HideEditor;
    try
      if Abs(Distance) > VisibleRowCount then
      begin
        Invalidate;
        Exit;
      end
      else
      begin
        RowHeight := DefaultRowHeight;
        if dgRowLines in Options then Inc(RowHeight, GridLineWidth);
        if dgIndicator in Options then
        begin
          OldRect := BoxRect(0, FSelRow, ColCount - 1, FSelRow);
          InvalidateRect(Handle, @OldRect, False);
        end;
        NewRect := BoxRect(0, FTitleOffset, ColCount - 1, 1000);
        ScrollWindowEx(Handle, 0, -RowHeight * Distance, @NewRect, @NewRect,
          0, nil, SW_Invalidate);
        if dgIndicator in Options then
        begin
          NewRect := BoxRect(0, Row, ColCount - 1, Row);
          InvalidateRect(Handle, @NewRect, False);
        end;
      end;
    finally
      if dgAlwaysShowEditor in Options then ShowEditor;
    end;
  end;
  if UpdateLock = 0 then Update;
end;

procedure TVolgaCustomDBGrid.SetColumns(Value: TVolgaDBGridColumns);
begin
  Columns.Assign(Value);
end;

function ReadOnlyField(Field: TField): Boolean;
var
  MasterField: TField;
begin
  Result := Field.ReadOnly;
  if not Result and (Field.FieldKind = fkLookup) then
  begin
    Result := True;
    if Field.DataSet = nil then Exit;
    MasterField := Field.Dataset.FindField(Field.KeyFields);
    if MasterField = nil then Exit;
    Result := MasterField.ReadOnly;
  end;
end;

procedure TVolgaCustomDBGrid.SetColumnAttributes;
var
  I: Integer;
begin                                   //volga
  if (FFixedCols + FIndicatorOffset < ColCount) and (FFixedCols >= 0) then
  begin
    inherited FixedCols := FFixedCols + FIndicatorOffset;
    for I := 0 to FFixedCols + FIndicatorOffset - 1 do
    begin
      ColWidths[I + FIndicatorOffset] := FColumns[I].Width;
      TabStops[I + FIndicatorOffset] := False;
    end;
  end;

  for I := FFixedCols to FColumns.Count - 1 do //Volga
    with FColumns[I] do
    begin                               //даем редактировать Calculated-поля!!!!!!!!
      TabStops[I + FIndicatorOffset] := Showing and not ReadOnly and DataLink.Active and
        Assigned(Field) and {not (Field.FieldKind = fkCalculated) and } not //Volga
        ReadOnlyField(Field);
      ColWidths[I + FIndicatorOffset] := Width;
    end;
  if (dgIndicator in Options) then
    ColWidths[0] := IndicatorWidth;
end;

procedure TVolgaCustomDBGrid.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then Exit;
  FBookmarks.Clear;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  LinkActive(FDataLink.Active);
end;

procedure TVolgaCustomDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var Column: TVolgaColumn;               //Volga
  ind: integer;                         //Volga
begin
  Column := Columns[RawToDataColumn(ACol)]; //Volga
  if (Column.PickList.Count > 0) and (Column.PickValues <> nil) //Volga
    and (Column.PickList.Count = Column.PickValues.Count) then
  begin                                 //Volga
    ind := Column.PickList.IndexOf(Value); //Volga
    if ind >= 0 then                    //Volga
      FEditText := Column.PickValues[ind] //Volga
    else                                //Volga
      FEditText := '';                  //не нашли такое значение!!
  end
  else
    FEditText := Value;                 //??????????
end;

procedure TVolgaCustomDBGrid.SetFixedCols(Value: integer);
begin
  if (csDesigning in ComponentState) then
  begin
    if ((dataSource <> nil) and (dataSource.dataSet <> nil) and
      (dataSource.dataSet.active) and (Value + FIndicatorOffset >= ColCount)) or
      (Value < 0) then
    begin
      Value := 0;
    end
  end;
  FFixedCols := Value;
  LayoutChanged;
end;

function TVolgaCustomDBGrid.GetFixedCols: integer;
begin
  Result := FFixedCols;
end;

procedure TVolgaCustomDBGrid.SetOptions(Value: TVolgaDBGridOptions);
const
  LayoutOptions = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator,
    dgColLines, dgRowLines, dgRowSelect, dgAlwaysShowSelection];
var
  NewGridOptions: TGridOptions;
  ChangedOptions: TVolgaDBGridOptions;
begin
  if FOptions <> Value then
  begin
    NewGridOptions := [];
    if dgColLines in Value then
      NewGridOptions := NewGridOptions + [goFixedVertLine, goVertLine];
    if dgRowLines in Value then
      NewGridOptions := NewGridOptions + [goFixedHorzLine, goHorzLine];
    if dgColumnResize in Value then
      NewGridOptions := NewGridOptions + [goColSizing, goColMoving];
    if dgTabs in Value then Include(NewGridOptions, goTabs);
    if dgDragOutRows in Value then Include(Value, dgRowSelect);
    if dgRowSelect in Value then
    begin
      Include(NewGridOptions, goRowSelect);
      Exclude(Value, dgAlwaysShowEditor);
      Exclude(Value, dgEditing);
      Exclude(Value, dgEnterToTab);
    end;
    if dgEditing in Value then Include(NewGridOptions, goEditing);
    if dgAlwaysShowEditor in Value then Include(NewGridOptions, goAlwaysShowEditor);
    inherited Options := NewGridOptions;
    if dgMultiSelect in (FOptions - Value) then FBookmarks.Clear;
    ChangedOptions := (FOptions + Value) - (FOptions * Value);
    FOptions := Value;
    FAllowInsert := FAllowInsert and (dgEditing in FOptions); //Volga
    FAllowDelete := FAllowDelete and (dgEditing in FOptions); //Volga
    if ChangedOptions * LayoutOptions <> [] then LayoutChanged;
  end;
end;

procedure TVolgaCustomDBGrid.SetRowsHeight(const Value: integer);
begin
  FRowsHeight := Value;
  LayoutChanged;
end;

procedure TVolgaCustomDBGrid.SetSelectedField(Value: TField);
var
  I: Integer;
begin
  if Value = nil then Exit;
  for I := 0 to Columns.Count - 1 do
    if Columns[I].Field = Value then
      MoveCol(DataToRawColumn(I), 0);
end;

procedure TVolgaCustomDBGrid.SetSelectedIndex(Value: Integer);
begin
  MoveCol(DataToRawColumn(Value), 0);
end;

procedure TVolgaCustomDBGrid.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
  if dgTitles in Options then LayoutChanged;
end;

procedure TVolgaCustomDBGrid.SetTitleHeight(const Value: integer);
begin
  FTitleHeight := Value;
  if dgTitles in Options then LayoutChanged;
end;

function TVolgaCustomDBGrid.StoreColumns: Boolean;
begin
  Result := Columns.State = csCustomized;
end;

procedure TVolgaCustomDBGrid.TimedScroll(Direction: TGridScrollDirection);
begin
  if FDatalink.Active then
  begin
    with FDatalink do
    begin
      if sdUp in Direction then
      begin
        FDataLink.MoveBy(-ActiveRecord - 1);
        Exclude(Direction, sdUp);
      end;
      if sdDown in Direction then
      begin
        FDataLink.MoveBy(RecordCount - ActiveRecord);
        Exclude(Direction, sdDown);
      end;
    end;
    if Direction <> [] then inherited TimedScroll(Direction);
  end;
end;

procedure TVolgaCustomDBGrid.TitleClick(Column: TVolgaColumn);
var i: integer;
begin
  //changed are added by Armando Novello
  if not FColMoved then                 // FColMoved is set to True in OnMoveColumn
  begin
    if (Column.SortMark = vsmNone) or FClrSMarkOnClick then
    //if not FClrSMarkOnClick then
    begin
      for i := 0 to Columns.Count - 1 do //Volga
        Columns[i].SortMark := vsmNone; //Volga
    end;
    if Assigned(FOnTitleClick) then FOnTitleClick(Self, Column);
  end
  else
    FColMoved := False;
end;

procedure TVolgaCustomDBGrid.TitleFontChanged(Sender: TObject);
begin
  if (not FSelfChangingTitleFont) and not (csLoading in ComponentState) then
    ParentFont := False;
  if dgTitles in Options then LayoutChanged;
end;

procedure TVolgaCustomDBGrid.UpdateActive;
var
  NewRow: Integer;
  Field: TField;
begin
  if FDatalink.Active and HandleAllocated and not (csLoading in ComponentState) then
  begin
    NewRow := FDatalink.ActiveRecord + FTitleOffset;
    if Row <> NewRow then
    begin
      if not (dgAlwaysShowEditor in Options) then HideEditor;
      MoveColRow(Col, NewRow, False, False);
      InvalidateEditor;
    end;
    Field := SelectedField;
    if Assigned(Field) and (Field.Text <> FEditText) then
      InvalidateEditor;
  end;
end;

procedure TVolgaCustomDBGrid.UpdateData;
var Field: TField;
  i: integer;
  FET: string;
begin
  Field := SelectedField;
  if Assigned(Field) then
    if (Field.FieldKind <> fkCalculated) then //Volga
    begin
      if (Field.DataType <> ftString) and (Field.EditMask <> '') then
      begin
        FET := '';  //support for numeric Field.EditMask
        for i := 0 to Length(FEditText) - 1 do
          if ((ord(FEditText[i]) >= 44) and (ord(FEditText[i]) < 58)) or (FEditText[i] =
            DecimalSeparator) then
            FET := FET + FEditText[i];
        FEditText := FET;
      end;
      Field.Text := FEditText;
    end
    else if Assigned(FOnCalcFieldEdited) then //Volga
      FOnCalcFieldEdited(Self, Field, FEditText); //Volga
end;

procedure TVolgaCustomDBGrid.UpdateRowCount;
var
  OldRowCount: Integer;
begin
  OldRowCount := RowCount;
  if RowCount <= FTitleOffset then RowCount := FTitleOffset + 1;
  FixedRows := FTitleOffset;
  with FDataLink do
    if not Active or (RecordCount = 0) or not HandleAllocated then
      RowCount := 1 + FTitleOffset
    else
    begin
      RowCount := 1000;
      FDataLink.BufferCount := VisibleRowCount;
      RowCount := RecordCount + FTitleOffset;
      if dgRowSelect in Options then TopRow := FixedRows;
      UpdateActive;
    end;
  if OldRowCount <> RowCount then Invalidate;
end;

procedure TVolgaCustomDBGrid.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
begin
  if FDatalink.Active and HandleAllocated then
    with FDatalink.DataSet do
    begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      if IsSequenced then
      begin
        SINew.nMin := 1;
        SINew.nPage := Self.VisibleRowCount;
        SINew.nMax := Integer(DWORD(RecordCount) + SINew.nPage - 1);
        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := RecNo;          // else keep old pos
      end
      else
      begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        SINew.nMax := 4;
        if FDataLink.BOF then
          SINew.nPos := 0
        else if FDataLink.EOF then
          SINew.nPos := 4
        else
          SINew.nPos := 2;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
end;

function TVolgaCustomDBGrid.ValidFieldIndex(FieldIndex: Integer): Boolean;
begin
  Result := DataLink.GetMappedIndex(FieldIndex) >= 0;
end;

procedure TVolgaCustomDBGrid.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    FSelfChangingTitleFont := True;
    try
      TitleFont := Font;
    finally
      FSelfChangingTitleFont := False;
    end;
    LayoutChanged;
  end;
end;

procedure TVolgaCustomDBGrid.CMBiDiModeChanged(var Message: TMessage);
var
  Loop: Integer;
begin
  inherited;
  for Loop := 0 to ComponentCount - 1 do
    if Components[Loop] is TVolgaCustomDBGrid then
      with Components[Loop] as TVolgaCustomDBGrid do
        { Changing the window, echos down to the subgrid }
        if Parent <> nil then
          Parent.BiDiMode := Self.BiDiMode;
end;

procedure TVolgaCustomDBGrid.CMExit(var Message: TMessage);
begin
  try
    if FDatalink.Active then
      with FDatalink.Dataset do
        if (dgEditing in Options) and (dgCancelOnExit in Options) and
          (State = dsInsert) and not Modified and not FDatalink.FModified then
          Cancel
        else
          FDataLink.UpdateData;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TVolgaCustomDBGrid.CMFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  BeginLayout;
  try
    for I := 0 to Columns.Count - 1 do
      Columns[I].RefreshDefaultFont;
  finally
    EndLayout;
  end;
end;

procedure TVolgaCustomDBGrid.CMDeferLayout(var Message);
begin
  if AcquireLayoutLock then
    EndLayout
  else
    DeferLayout;
end;

procedure TVolgaCustomDBGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  MasterCol: TVolgaColumn;
begin
  inherited;
  if (Msg.Result = 1) and ((FDataLink = nil) or
    ((Columns.State = csDefault) and
    (FDataLink.DefaultFields or (not FDataLink.Active)))) then
    Msg.Result := 0
  else if (Msg.Result = 0) and (FDataLink <> nil) and (FDataLink.Active)
    and (Columns.State = csCustomized)
    and PtInExpandButton(Msg.XPos, Msg.YPos, MasterCol) then
    Msg.Result := 1;
end;

procedure TVolgaCustomDBGrid.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (csDesigning in ComponentState) and
    ((FDataLink = nil) or
    ((Columns.State = csDefault) and
    (FDataLink.DefaultFields or not FDataLink.Active))) then
    Windows.SetCursor(LoadCursor(0, IDC_ARROW))
  else
    inherited;
end;

procedure TVolgaCustomDBGrid.WMSize(var Message: TWMSize);
begin
  inherited;
  if UpdateLock = 0 then UpdateRowCount;
  InvalidateTitles;
end;

procedure TVolgaCustomDBGrid.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  if not AcquireFocus then Exit;
  if FDatalink.Active then
    with Message, FDataLink.DataSet do
      case ScrollCode of
        SB_LINEUP: FDataLink.MoveBy(-FDatalink.ActiveRecord - 1);
        SB_LINEDOWN: FDataLink.MoveBy(FDatalink.RecordCount - FDatalink.ActiveRecord);
        SB_PAGEUP: FDataLink.MoveBy(-VisibleRowCount);
        SB_PAGEDOWN: FDataLink.MoveBy(VisibleRowCount);
        SB_THUMBPOSITION:
          begin
            if IsSequenced then
            begin
              SI.cbSize := sizeof(SI);
              SI.fMask := SIF_ALL;
              GetScrollInfo(Self.Handle, SB_VERT, SI);
              if SI.nTrackPos <= 1 then
                First
              else if SI.nTrackPos >= RecordCount then
                Last
              else
                RecNo := SI.nTrackPos;
            end
            else
              case Pos of
                0: First;
                1: FDataLink.MoveBy(-VisibleRowCount);
                2: Exit;
                3: FDataLink.MoveBy(VisibleRowCount);
                4: Last;
              end;
          end;
        SB_BOTTOM: Last;
        SB_TOP: First;
      end;
end;

procedure TVolgaCustomDBGrid.SetIme;
var
  Column: TVolgaColumn;
begin
  if not SysLocale.FarEast then Exit;
  if Columns.Count = 0 then Exit;

  ImeName := FOriginalImeName;
  ImeMode := FOriginalImeMode;
  Column := Columns[SelectedIndex];
  if Column.IsImeNameStored then ImeName := Column.ImeName;
  if Column.IsImeModeStored then ImeMode := Column.ImeMode;

  if InplaceEditor <> nil then
  begin
    TVolgaDBGridInplaceEdit(Self).ImeName := ImeName;
    TVolgaDBGridInplaceEdit(Self).ImeMode := ImeMode;
  end;
end;

procedure TVolgaCustomDBGrid.UpdateIme;
begin
  if not SysLocale.FarEast then Exit;
  SetIme;
  SetImeName(ImeName);
  SetImeMode(Handle, ImeMode);
end;

procedure TVolgaCustomDBGrid.WMIMEStartComp(var Message: TMessage);
begin
  inherited;
  ShowEditor;
end;

procedure TVolgaCustomDBGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  if not ((InplaceEditor <> nil) and
    (Message.FocusedWnd = InplaceEditor.Handle)) then SetIme;
  inherited;
end;

procedure TVolgaCustomDBGrid.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then
    inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if not ((InplaceEditor <> nil) and
      (HWND(Message.WParam) = InplaceEditor.Handle)) then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
end;

{ Defer action processing to datalink }

function TVolgaCustomDBGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.ExecuteAction(Action);
end;

function TVolgaCustomDBGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.UpdateAction(Action);
end;

procedure TVolgaCustomDBGrid.ShowPopupEditor(Column: TVolgaColumn; X, Y: Integer);
var
  SubGrid: TVolgaCustomDBGrid;
  DS: TDataSource;
  I: Integer;
  FloatRect: TRect;
  Cmp: TControl;
begin
  if not ((Column.Field <> nil) and (Column.Field is TDataSetField)) then Exit;

  // find existing popup for this column field, if any, and show it
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TVolgaCustomDBGrid then
    begin
      SubGrid := TVolgaCustomDBGrid(Components[I]);
      if (SubGrid.DataSource <> nil) and
        (SubGrid.DataSource.DataSet = (Column.Field as TDatasetField).NestedDataset) then
      begin
        SubGrid.Parent.Show;
        SubGrid.SetFocus;
        Exit;
      end;
    end;

  // create another instance of this kind of grid
  SubGrid := TVolgaCustomDBGrid(TComponentClass(Self.ClassType).Create(Self));
  try
    DS := TDataSource.Create(SubGrid);  // incestuous, but easy cleanup
    DS.Dataset := (Column.Field as TDatasetField).NestedDataset;
    SubGrid.DataSource := DS;
    SubGrid.Columns.State := Columns.State;
    SubGrid.Columns[0].Expanded := True;
    SubGrid.Visible := False;
    SubGrid.FloatingDockSiteClass := TCustomDockForm;
    FloatRect.TopLeft := ClientToScreen(CellRect(Col, Row).BottomRight);
    if X > Low(Integer) then FloatRect.Left := X;
    if Y > Low(Integer) then FloatRect.Top := Y;
    FloatRect.Right := FloatRect.Left + Width;
    FloatRect.Bottom := FloatRect.Top + Height;
    SubGrid.ManualFloat(FloatRect);
//    SubGrid.ManualDock(nil,nil,alClient);
    SubGrid.Parent.BiDiMode := Self.BiDiMode; { This carries the BiDi setting }
    I := SubGrid.CellRect(SubGrid.ColCount - 1, 0).Right;
    if (I > 0) and (I < Screen.Width div 2) then
      SubGrid.Parent.ClientWidth := I
    else
      SubGrid.Parent.Width := Screen.Width div 4;
    SubGrid.Parent.Height := Screen.Height div 4;
    SubGrid.Align := alClient;
    SubGrid.DragKind := dkDock;
    SubGrid.Color := Color;
    SubGrid.Ctl3D := Ctl3D;
    SubGrid.Cursor := Cursor;
    SubGrid.Enabled := Enabled;
    SubGrid.FixedColor := FixedColor;
    SubGrid.Font := Font;
    SubGrid.HelpContext := HelpContext;
    SubGrid.IMEMode := IMEMode;
    SubGrid.IMEName := IMEName;
    SubGrid.Options := Options;
    Cmp := Self;
    while (Cmp <> nil) and (TVolgaCustomDBGrid(Cmp).PopupMenu = nil) do
      Cmp := Cmp.Parent;
    if Cmp <> nil then
      SubGrid.PopupMenu := TVolgaCustomDBGrid(Cmp).PopupMenu;
    SubGrid.TitleFont := TitleFont;
    SubGrid.Visible := True;
    SubGrid.Parent.Show;
  except
    SubGrid.Free;
    raise;
  end;
end;

procedure TVolgaCustomDBGrid.CalcSizingState(X, Y: Integer;
  var State: TGridState; var Index, SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);
var
  R: TGridCoord;
begin
  inherited CalcSizingState(X, Y, State, Index, SizingPos, SizingOfs, FixedInfo);
  if (State = gsColSizing) and (FDataLink <> nil)
    and (FDatalink.Dataset <> nil) and FDataLink.Dataset.ObjectView then
  begin
    R := MouseCoord(X, Y);
    R.X := RawToDataColumn(R.X);
    if (R.X >= 0) and (R.X < Columns.Count) and (Columns[R.X].Depth > R.Y) then
      State := gsNormal;
  end;
end;

function TVolgaCustomDBGrid.CheckColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
  DestCol: TVolgaColumn;
begin
  Result := inherited CheckColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    assert(FDragCol <> nil);
    ARow := FDragCol.Depth;
    if Destination <> Origin then
    begin
      DestCol := ColumnAtDepth(Columns[RawToDataColumn(Destination)], ARow);
      if DestCol.ParentColumn <> FDragCol.ParentColumn then
        if Destination < Origin then
          DestCol := Columns[FDragCol.ParentColumn.Index + 1]
        else
        begin
          I := DestCol.Index;
          while DestCol.ParentColumn <> FDragCol.ParentColumn do
          begin
            Dec(I);
            DestCol := Columns[I];
          end;
        end;
      if (DestCol.Index > FDragCol.Index) then
      begin
        I := DestCol.Index + 1;
        while (I < Columns.Count) and (ColumnAtDepth(Columns[I], ARow) = DestCol) do
          Inc(I);
        DestCol := Columns[I - 1];
      end;
      Destination := DataToRawColumn(DestCol.Index);
    end;
  end;
end;

function TVolgaCustomDBGrid.BeginColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
begin
  Result := inherited BeginColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    ARow := MouseCoord(MousePt.X, MousePt.Y).Y;
    FDragCol := ColumnAtDepth(Columns[RawToDataColumn(Origin)], ARow);
    if FDragCol = nil then Exit;
    I := DataToRawColumn(FDragCol.Index);
    if Origin <> I then Origin := I;
    Destination := Origin;
  end;
end;

function TVolgaCustomDBGrid.EndColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := inherited EndColumnDrag(Origin, Destination, MousePt);
  FDragCol := nil;
end;

procedure TVolgaCustomDBGrid.InvalidateTitles;
var
  R: TRect;
  DrawInfo: TGridDrawInfo;
begin
  if HandleAllocated and (dgTitles in Options) and (FDatalink <> nil) and
    (FDatalink.Dataset <> nil) then
  begin
    CalcFixedInfo(DrawInfo);
    R := Rect(0, 0, Width, DrawInfo.Vert.FixedBoundary);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TVolgaCustomDBGrid.TopLeftChanged;
begin
  InvalidateTitles;
  inherited TopLeftChanged;
end;

procedure TVolgaCustomDBGrid.InvalidateCurrentRow; //Volga
var i: integer;
begin
  for i := 0 to colCount - 1 do
    InvalidateCell(i, row);
end;

procedure TVolgaCustomDBGrid.SetAllowDelete(const Value: Boolean);
begin                                   //Volga
  FAllowDelete := Value and (dgEditing in Options);
end;

procedure TVolgaCustomDBGrid.SetAllowInsert(const Value: Boolean);
begin                                   //Volga
  FAllowInsert := Value and (dgEditing in Options);
end;

function TVolgaCustomDBGrid.ColumnByName(const AName: string): TVolgaColumn;
var i: integer;
begin
  Result := nil;
  for i := 0 to Columns.Count - 1 do
    if AnsiCompareText(Columns[i].FieldName, AName) = 0 then
    begin
      Result := Columns[i];
      Exit;
    end;
end;

procedure TVolgaCustomDBGrid.SetHColor(const Value: TColor);
begin
  FHColor := Value;
  Invalidate;
end;

procedure TVolgaCustomDBGrid.SetHTColor(const Value: TColor);
begin
  FHTColor := Value;
  Invalidate;
end;

function TVolgaCustomDBGrid.ScreenCellRect(ARow: TVolgaRowType;
  AColName: string): TRect;
var AColumn: TVolgaColumn;
  nrow, ncol: integer;
  p1, p2: TPoint;
begin
  Result := Rect(0, 0, 0, 0);
  AColumn := ColumnByName(AColName);
  if (AColumn = nil) or not FDatalink.Active then Exit;
  ncol := DataToRawColumn(AColumn.Index);
  nrow := 0;
  if (ARow = rtTitle) and not (dgTitles in Options) then
    ARow := rtFirstVisible;
  case ARow of
    rtTitle: nrow := 0;
    rtFirstVisible: nrow := FTitleOffset;
    rtCurrent: nrow := FDatalink.ActiveRecord + FTitleOffset;
    rtLastVisible: nrow := RowCount;
  end;
  Result := CellRect(ncol, nrow);
  p1 := ClientToScreen(Result.TopLeft);
  p2 := ClientToScreen(Result.BottomRight);
  Result := Rect(p1.x, p1.y, p2.x, p2.y);
end;

function TVolgaCustomDBGrid.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if Datalink.Active then
    FDataLink.MoveBy(1)
  else
    inherited DoMouseWheelDown(Shift, MousePos);
  Result := True;
end;

function TVolgaCustomDBGrid.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if Datalink.Active then
    FDataLink.MoveBy(-1)
  else
    inherited DoMouseWheelUp(Shift, MousePos);
  Result := True;
end;

procedure TVolgaCustomDBGrid.ExportToHTML(filename, title, header, footer: string;
  TotalFieldNames: string; ShowInBrowser: Boolean); //volga
var A: TStrings;
  sval, align: string;
  i, k: integer;
  LF: TList;
  totals: array of double;
  Bmk: TBookmark;
  pfil: array[0..400] of char;

  function GetDisplayText(Col: TVolgaColumn): string;
  var Value: string;
    VarValue: Variant;
    ind: integer;
  begin
    Value := '';
    if Assigned(Col.Field) then
    begin
      if Col.Field is TMemoField then
        Value := Col.Field.AsString
      else
        Value := Col.Field.DisplayText;
      case Col.ButtonStyle of
        cbsCombo:
          if (Col.PickValues <> nil) and (Col.PickList.Count > 0) and
            (Col.PickValues.Count = Col.PickList.Count) then
          begin
            ind := Col.PickValues.IndexOf(Col.Field.Text);
            if ind >= 0 then
              Value := Col.PickList[ind]
            else
              Value := '';
          end;
        cbsLookup:
          with Col do
            if IsLinkActive then
            begin
              if LookupLinkField = FViewField then
                VarValue := FDataLink.Dataset[FieldName]
              else
                VarValue := LookupDataSet.Lookup(LookupLinkField,
                  FDataLink.Dataset[LookupKeyField], FViewField);
              if not VarIsNull(VarValue) then
                Value := VarValue
              else
                Value := '';
            end;
      end;
    end;
    Result := Value;
  end;

begin
  if not FDataLink.Active then Exit;
  Screen.Cursor := crHourGlass;
  A := TStringList.Create;
  LF := TList.Create;
  if TotalFieldNames <> '' then
  begin
    FDataLink.Dataset.GetFieldList(LF, TotalFieldNames);
    SetLength(totals, LF.Count);
    for i := 0 to High(totals) do
      totals[i] := 0;
  end;
  {заголовок файла}
  A.Add('<html>');
  A.Add('<head>');
  A.Add('<title>' + title + '</title>');
  A.Add('</head>');
  A.Add('<body>');
  A.Add(header);
  A.Add('<table border="1" cellpadding="3" cellspacing="0" width=100%>');
  A.Add('<tr>');
  for i := 0 to Columns.Count - 1 do
    if Columns[i].Visible then
      A.Add('<th bgcolor="#C0C0C0">' + Columns[i].Title.Caption + '</th>');
  A.Add('</tr>');
  FDataLink.DataSet.DisableControls;
  Bmk := FDataLink.DataSet.GetBookmark;
  FDataLink.Dataset.First;
  while not FDataLink.Dataset.Eof do
  begin
    A.Add('<tr>');
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Visible then
      begin
        sval := GetDisplayText(Columns[i]);
        case Columns[i].Alignment of
          taLeftJustify: align := '';
          taRightJustify: align := ' align=right';
          taCenter: align := ' align=center';
        end;
        if sval = '' then
          A.Add('<td>&nbsp;</td>')
        else
          A.Add('<td' + align + '>' + sval + '</td>');
      end;
    A.Add('</tr>');
    for i := 0 to LF.Count - 1 do
    try
      totals[i] := totals[i] + TField(LF[i]).AsFloat;
    except
    end;
    DataLink.Dataset.Next;
  end;
  if LF.Count > 0 then
  begin
    A.Add('<tr>');
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Visible and Assigned(Columns[i].Field) then
      begin
        k := LF.IndexOf(Columns[i].Field);
        if k >= 0 then
        begin
          sval := '';
          if Columns[i].Field is TNumericField then
            sval := TNumericField(Columns[i].Field).DisplayFormat;
          if sval = '' then sval := '0.##';
          A.Add('<td align=right><b>' + FormatFloat(sval, totals[k]) + '</b></td>');
        end
        else
          A.Add('<td>&nbsp;</td>')
      end;
    A.Add('</tr>');
  end;
  A.Add('</table>');
  A.Add(footer);
  A.Add('</body>');
  A.Add('</html>');
  A.SaveToFile(filename);
  A.Free;
  LF.Clear;
  LF.Free;
  FDataLink.DataSet.GotoBookmark(Bmk);
  FDataLink.DataSet.FreeBookmark(Bmk);
  FDataLink.DataSet.EnableControls;
  Screen.Cursor := crDefault;
  if ShowInBrowser then
  try
    StrPCopy(pfil, filename);
    ShellExecute(GetDesktopWindow, 'open', pfil, nil, nil, SW_SHOWNORMAL);
  except
  end;
end;

end.

