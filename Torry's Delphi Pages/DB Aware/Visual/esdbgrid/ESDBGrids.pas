
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1995,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit ESDBGrids;

{$R-,T-,H+,X+}

interface

uses Windows, SysUtils, Messages, Classes, Controls, Forms, StdCtrls, StrUtils,
   TypInfo, 
{$IFDEF VER140}Variants, {$ENDIF}
{$IFDEF VER150}Variants, {$ENDIF}
  Graphics, Grids, DBCtrls, Db, Menus, ImgList, ShellApi, Mask, Consts;

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

type
  TColumnValue = (cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly, cvTitleColor,
    cvTitleCaption, cvTitleAlignment, cvTitleFont, cvImeMode, cvImeName);
  TColumnValues = set of TColumnValue;

type
  TTextAngle = (taHorizontal, taVertical);

const
  ColumnTitleValues = [cvTitleColor..cvTitleFont];
  cm_DeferLayout = WM_USER + 100;

{ TColumn defines internal storage for column attributes.  If IsStored is
  True, values assigned to properties are stored in this object, the grid-
  or field-based default sources are not modified.  Values read from
  properties are the previously assigned value, if any, or the grid- or
  field-based default values if nothing has been assigned to that property.
  This class also publishes the column attribute properties for persistent
  storage.

  If IsStored is True, the column does not maintain local storage of
  property values.  Assignments to column properties are passed through to
  the underlying grid- or field-based default sources.  }
type

  TCustomESDBGrid = class;

  TInplaceEditMS = class(TCustomMaskEdit)
  private
    FGrid: TCustomESDBGrid;
    FClickTime: Longint;
    FWordWrap: Boolean;         //Добавлено
    FLines: TStrings;           //Добавлено
    FAlignment: TAlignment;     //Добавлено
    FPreventSelection: Boolean; //Добавлено для реализации AutoSelect
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message); message WM_PASTE;
    procedure WMCut(var Message); message WM_CUT;
    procedure WMClear(var Message); message WM_CLEAR;
    procedure SetWordWrap(const Value: Boolean);         //Добавлено
    procedure SetLines(const Value: TStrings);           //Добавлено
    function  GetCaretPos: TPoint;                       //Добавлено
    procedure SetCaretPos(const Value: TPoint);          //Добавлено
    procedure SetAlignment(const Value: TAlignment);     //Добавлено
    procedure SetCaretPosition;                          //Добавлено для обработки not AutoSelect;
  protected
    IsScrolled: Boolean;                                 //Добавлено
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    procedure Loaded; override;                          //Добавлено
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function  EditCanModify: Boolean; override;
    function  NeedScrollBars: Integer;
    function  WantRecreateWnd: Boolean;                 //Добавлено
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; virtual;
    procedure UpdateContents; virtual;
    procedure WndProc(var Message: TMessage); override;   //Добавлено EM_SETSEL
    property  Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property  Grid: TCustomESDBGrid read FGrid;
    property  WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property AutoSelect;                //Добавлено
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Deselect;
    procedure Hide;
    procedure Invalidate; reintroduce;
    procedure Move(const Loc: TRect);
    function PosEqual(const Rect: TRect): Boolean;
    procedure SetFocus; reintroduce;
    procedure UpdateLoc(const Loc: TRect);
    function Visible: Boolean;
    property EditText;
    property Lines: TStrings read FLines write SetLines;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property Text;
  end;

  TColumn = class;

  TColumnTitle = class(TPersistent)
  private
    FColumn: TColumn;
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FAlignment: TAlignment;
    FTextAngle: TTextAngle;
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
    procedure SetTextAngle(const Value: TTextAngle);
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
    property Column: TColumn read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property TextAngle: TTextAngle read FTextAngle write SetTextAngle default taHorizontal;
  end;

  TColumnButtonStyle = (cbsAuto, cbsEllipsis, cbsNone);

  TMemoOption = (moOnlyTabMove, moScrollBar,  moWantArrows, moWantReturns);
  TMemoOptions = set of TMemoOption;

  TColumn = class(TCollectionItem)
  private
    FField: TField;
    FFieldName: string;
    FColor: TColor;
    FWidth: Integer;
    FTitle: TColumnTitle;
    FFont: TFont;
    FImeMode: TImeMode;
    FImeName: TImeName;
    FPickList: TStrings;
    FPopupMenu: TPopupMenu;
    FDropDownRows: Cardinal;
    FButtonStyle: TColumnButtonStyle;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TColumnValues;
    FVisible: Boolean;
    FExpanded: Boolean;
    FStored: Boolean;
    FMemo: Boolean;
    procedure FontChanged(Sender: TObject);
    function  GetAlignment: TAlignment;
    function  GetColor: TColor;
    function  GetExpanded: Boolean;
    function  GetField: TField;
    function  GetFont: TFont;
    function  GetImeMode: TImeMode;
    function  GetImeName: TImeName;
    function  GetParentColumn: TColumn;
    function  GetPickList: TStrings;
    function  GetReadOnly: Boolean;
    function  GetShowing: Boolean;
    function  GetWidth: Integer;
    function  GetVisible: Boolean;
    function  IsAlignmentStored: Boolean;
    function  IsColorStored: Boolean;
    function  IsFontStored: Boolean;
    function  IsImeModeStored: Boolean;
    function  IsImeNameStored: Boolean;
    function  IsReadOnlyStored: Boolean;
    function  IsWidthStored: Boolean;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetButtonStyle(Value: TColumnButtonStyle);
    procedure SetColor(Value: TColor);
    procedure SetExpanded(Value: Boolean);
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: String);
    procedure SetFont(Value: TFont);
    procedure SetImeMode(Value: TImeMode); virtual;
    procedure SetImeName(Value: TImeName); virtual;
    procedure SetPickList(Value: TStrings);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetTitle(Value: TColumnTitle);
    procedure SetWidth(Value: Integer); virtual;
    procedure SetVisible(Value: Boolean);
    function GetExpandable: Boolean;
    procedure SetFMemo(const Value: Boolean);
  protected
    function  CreateTitle: TColumnTitle; virtual;
    function  GetGrid: TCustomESDBGrid;
    function GetDisplayName: string; override;
    procedure RefreshDefaultFont;
    procedure SetIndex(Value: Integer); override;
    property IsStored: Boolean read FStored write FStored default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function  DefaultAlignment: TAlignment;
    function  DefaultColor: TColor;
    function  DefaultFont: TFont;
    function  DefaultImeMode: TImeMode;
    function  DefaultImeName: TImeName;
    function  DefaultReadOnly: Boolean;
    function  DefaultWidth: Integer;
    function  Depth: Integer;
    procedure RestoreDefaults; virtual;
    property  Grid: TCustomESDBGrid read GetGrid;
    property  AssignedValues: TColumnValues read FAssignedValues;
    property  Expandable: Boolean read GetExpandable;
    property  Field: TField read GetField write SetField;
    property  ParentColumn: TColumn read GetParentColumn;
    property  Showing: Boolean read GetShowing;
  published
    property  Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
    property  ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle
      default cbsAuto;
    property  Color: TColor read GetColor write SetColor stored IsColorStored;
    property  DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
    property  Expanded: Boolean read GetExpanded write SetExpanded default True;
    property  FieldName: String read FFieldName write SetFieldName;
    property  Font: TFont read GetFont write SetFont stored IsFontStored;
    property  ImeMode: TImeMode read GetImeMode write SetImeMode stored IsImeModeStored;
    property  ImeName: TImeName read GetImeName write SetImeName stored IsImeNameStored;
    property  PickList: TStrings read GetPickList write SetPickList;
    property  PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly
      stored IsReadOnlyStored;
    property  Title: TColumnTitle read FTitle write SetTitle;
    property  Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property  Visible: Boolean read GetVisible write SetVisible;
    property Memo: Boolean read FMemo write SetFMemo default False;     //Добавлено
  end;

  TColumnClass = class of TColumn;

  TDBGridColumnsState = (csDefault, csCustomized);

  TDBGridColumns = class(TCollection)
  private
    FGrid: TCustomESDBGrid;
    function GetColumn(Index: Integer): TColumn;
    function InternalAdd: TColumn;
    procedure SetColumn(Index: Integer; Value: TColumn);
    procedure SetState(NewState: TDBGridColumnsState);
    function GetState: TDBGridColumnsState;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Grid: TCustomESDBGrid; ColumnClass: TColumnClass);
    function  Add: TColumn;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(S: TStream);
    procedure RestoreDefaults;
    procedure RebuildColumns;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(S: TStream);
    property State: TDBGridColumnsState read GetState write SetState;
    property Grid: TCustomESDBGrid read FGrid;
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
  end;

  TGridDataLink = class(TDataLink)
  private
    FScrollPocessed: Boolean;                   //
    FFirstRecNo: Integer;                       //
    FIsAppended: Integer;                       //Режимы Browse и Insert =0, Append =1, AfterAppend =-1
    FEdited: Boolean; 
    FGrid: TCustomESDBGrid;
    FFieldCount: Integer;
    FFieldMap: array of Integer;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FSparseMap: Boolean;
    function GetDefaultFields: Boolean;
    function GetFields(I: Integer): TField;
    procedure LastPosition;
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
    function  GetMappedIndex(ColIndex: Integer): Integer;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override; //Добавлено
  public
    constructor Create(AGrid: TCustomESDBGrid);
    destructor Destroy; override;
    function AddMapping(const FieldName: string): Boolean;
    procedure ClearMapping;
    procedure Modified;
    procedure Reset;
    property DefaultFields: Boolean read GetDefaultFields;
    property FieldCount: Integer read FFieldCount;
    property Fields[I: Integer]: TField read GetFields;
    property SparseMap: Boolean read FSparseMap write FSparseMap;
    property Grid: TCustomESDBGrid read FGrid;
  end;

  TBookmarkList = class
  private
    FList: TStringList;
    FGrid: TCustomESDBGrid;
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
    constructor Create(AGrid: TCustomESDBGrid);
    destructor Destroy; override;
    procedure Clear;           // free all bookmarks
    procedure Delete;          // delete all selected rows from dataset
    function  Find(const Item: TBookmarkStr; var Index: Integer): Boolean;
    function  IndexOf(const Item: TBookmarkStr): Integer;
    function  Refresh: Boolean;// drop orphaned bookmarks; True = orphans found
    property Count: Integer read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected
      write SetCurrentRowSelected;
    property Items[Index: Integer]: TBookmarkStr read GetItem; default;
  end;


  TDBGridOption = (dgEditing, dgAlwaysShowEditor, dgTitles,
    dgIndicator, dgColumnResize, dgColLines, dgColMoving,
    dgRowLines, dgRowSelect, dgTabs, dgAlwaysShowSelection,
    dgDrawFocusSelected, dgConfirmDelete, dgCancelOnExit,
    dgMultiSelect);
  TDBGridOptions = set of TDBGridOption;


  { The DBGrid's DrawDataCell virtual method and OnDrawDataCell event are only
    called when the grid's Columns.State is csDefault.  This is for compatibility
    with existing code. These routines don't provide sufficient information to
    determine which column is being drawn, so the column attributes aren't
    easily accessible in these routines.  Column attributes also introduce the
    possibility that a column's field may be nil, which would break existing
    DrawDataCell code.   DrawDataCell, OnDrawDataCell, and DefaultDrawDataCell
    are obsolete, retained for compatibility purposes. }
  TDrawDataCellEvent = procedure (Sender: TObject; const Rect: TRect; Field: TField;
    State: TGridDrawState) of object;

  { The DBGrid's DrawColumnCell virtual method and OnDrawColumnCell event are
    always called, when the grid has defined column attributes as well as when
    it is in default mode.  These new routines provide the additional
    information needed to access the column attributes for the cell being
    drawn, and must support nil fields.  }

  TDrawColumnCellEvent = procedure (Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState) of object;
  TDBGridClickEvent = procedure (Column: TColumn) of object;

  TCustomESDBGrid = class(TCustomGrid)
  private
    FColumns: TDBGridColumns;
    FBotPartVisRow: Integer;    //Номер нижней частично видимой строки или -1, если такой нет
    FLastRowFull: Boolean;   //True, если последняя строка полностью видима, False, если не видна или видна частично
    FLastVisRow: Integer;       //Номер полностью видимой последней строки или -1, если она не видна или видна частично
    FActiveColor: TColor;       //Добавлено
    FMousePoint: TPoint;        //Для обработки not InPlaceEditor.АutoSelect
    FOneRowHeight: Integer;     //
    FIndicators: TImageList;
    FTitleFont: TFont;
    FReadOnly: Boolean;
    FOriginalImeName: TImeName;
    FOriginalImeMode: TImeMode;
    FUserChange: Boolean;
    FIsESCKey: Boolean;
    FLayoutFromDataset: Boolean;
    FOptions: TDBGridOptions;
    FTitleOffset, FIndicatorOffset: Byte;
    FUpdateLock: Byte;
    FLayoutLock: Byte;
    FInColExit: Boolean;
    FDefaultDrawing: Boolean;
    FSelfChangingTitleFont: Boolean;
    FSelectedColor: TColor;             //Добавлено
    FSelecting: Boolean;
    FSelRow: Integer;
    FDataLink: TGridDataLink;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FOnDrawDataCell: TDrawDataCellEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FEditText: string;
    FVisibleColumns: TList;
    FBookmarks: TBookmarkList;
    FSelectionAnchor: TBookmarkStr;
    FOnEditButtonClick: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FOnCellClick: TDBGridClickEvent;
    FOnTitleClick:TDBGridClickEvent;
    FDragCol: TColumn;
    FMemoAllowed: Boolean;              //Добавлено
    FMemoMaxRows: Integer;              //
    FMemoOptions: TMemoOptions;
    FAutoSelect: Boolean;
    FTitleWordWrap: Boolean;
    FActiveFontColor: TColor;         //
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
    function PtInExpandButton(X,Y: Integer; var MasterCol: TColumn): Boolean;
    procedure ReadColumns(Reader: TReader);
    procedure RecordChanged(Field: TField);
    procedure SetIme;
    procedure SetColumns(Value: TDBGridColumns);
    procedure SetDataSource(Value: TDataSource);
    procedure SetOptions(Value: TDBGridOptions);
    procedure SetSelectedField(Value: TField);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetTitleFont(Value: TFont);
    procedure ShowEditorMS;
    procedure TitleFontChanged(Sender: TObject);
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
    procedure WMComand(var Message: TMessage); message WM_COMMAND;    //Добавлено
    procedure SetMemoAllowed(const Value: Boolean);                   //
    procedure SetMemoMaxRows(const Value: Integer);                   //
    procedure SetMemoOptions(const Value: TMemoOptions);              //
    procedure SetAutoSelect(const Value: Boolean);                    //Добавлено
    procedure CalcBuffer(DefRowHeight: Integer=-1);    //если вызывается отдельно нужно вызвать BeginUpdate и FScrollProceesed должно быть True
    function CalcRowHeight(ARow: Integer=-1): Integer;
    function GetDefaultRowHeight: integer;
    procedure SetDefaultRowHeight(const Value: integer);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetGridLineWidth: Integer;
    procedure SetGridLineWidth(const Value: Integer);
    procedure CalcOneRowHeight;           //
    procedure NextPage;                 //
    procedure PrevPage;
    procedure SetTitleWordWrap(const Value: Boolean);                 //
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure SetSctiveFontColor(const Value: TColor);
    procedure SetActiveColor(const Value: TColor);
  protected
    FUpdateFields: Boolean;
    FAcquireFocus: Boolean;
    function  RawToDataColumn(ACol: Integer): Integer;
    function  DataToRawColumn(ACol: Integer): Integer;
    function  AcquireLayoutLock: Boolean;
    procedure BeginLayout;
    procedure BeginUpdate;
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: Longint; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;
    procedure CancelLayout;
    function  CanEditAcceptKey(Key: Char): Boolean; override;
    function  CanEditModify: Boolean; override;
    function  CanEditShow: Boolean; override;
    procedure CellClick(Column: TColumn); dynamic;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    function CalcTitleRect(Col: TColumn; ARow: Integer;
      var MasterCol: TColumn): TRect;
    function ColumnAtDepth(Col: TColumn; ADepth: Integer): TColumn;
    procedure ColEnter; dynamic;
    procedure ColExit; dynamic;
    procedure ColWidthsChanged; override;
    function  CreateColumns: TDBGridColumns; dynamic;
    function  CreateEditor: TInplaceEdit; override;
    function  CreateDataLink: TGridDataLink; dynamic;
    procedure CreateWnd; override;
    procedure DeferLayout;
    procedure DefineFieldMap; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawDataCell(const Rect: TRect; Field: TField;
      State: TGridDrawState); dynamic; { obsolete }
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); dynamic;
    procedure EditButtonClick; dynamic;
    procedure EndLayout;
    procedure EndUpdate;
    function  GetColField(DataCol: Integer): TField;
    function  GetEditLimit: Integer; override;
    function  GetEditMask(ACol, ARow: Longint): string; override;
    function  GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function  GetEditText(ACol, ARow: Longint): string; override;
    function  GetFieldValue(ACol: Integer): string;
    function  HighlightCell(DataCol, DataRow: Integer; const Value: string;
      AState: TGridDrawState): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure InvalidateTitles;
    procedure LayoutChanged; virtual;
    procedure LinkActive(Value: Boolean); virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      Xc, Yc: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(Distance: Integer); virtual;               //Добавлена проверка FScrollProcessed
    procedure SetColumnAttributes; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function  StoreColumns: Boolean;
    procedure TimedScroll(Direction: TGridScrollDirection); override;
    procedure TitleClick(Column: TColumn); dynamic;
    procedure TopLeftChanged; override;
    function UseRightToLeftAlignmentForField(const AField: TField;
      Alignment: TAlignment): Boolean;
    function BeginColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function CheckColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function EndColumnDrag(var Origin, Destination: Integer;
      const MousePt: TPoint): Boolean; override;
    function GetVisibleRowCount: Integer;
    property  Columns: TDBGridColumns read FColumns write SetColumns;
    property DefaultRowHeight: integer read GetDefaultRowHeight write SetDefaultRowHeight;
    property  DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property  DataLink: TGridDataLink read FDataLink;
    property Font: TFont read GetFont write SetFont;
    property GridLineWidth: Integer read GetGridLineWidth write SetGridLineWidth;
    property  IndicatorOffset: Byte read FIndicatorOffset;
    property  LayoutLock: Byte read FLayoutLock;
    property  MemoAllowed: Boolean read FMemoAllowed write SetMemoAllowed default False;
    property  MemoMaxRows: Integer read FMemoMaxRows write SetMemoMaxRows default 3;
    property  MemoOptions: TMemoOptions read FMemoOptions write SetMemoOptions default [];
    property ParentColor default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Options: TDBGridOptions read FOptions write SetOptions
      default [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines,
      dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit];
    property SelectedRows: TBookmarkList read FBookmarks;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property UpdateLock: Byte read FUpdateLock;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnDrawDataCell: TDrawDataCellEvent read FOnDrawDataCell
      write FOnDrawDataCell; { obsolete }
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell
      write FOnDrawColumnCell;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnCellClick: TDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnTitleClick: TDBGridClickEvent read FOnTitleClick write FOnTitleClick;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor; //Добавлено
    property ActiveColor: TColor read FActiveColor write SetActiveColor;       //Добавлено
    property ActiveFontColor: TColor read FActiveFontColor write SetSctiveFontColor; //Добавлено
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property AutoSelect: Boolean read FAutoSelect write SetAutoSelect default True;  //Добавлено
    property TitleWordWrap: Boolean read FTitleWordWrap write SetTitleWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawDataCell(const Rect: TRect; Field: TField;
      State: TGridDrawState); { obsolete }
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState);
    procedure DefaultHandler(var Msg); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ShowPopupEditor(Column: TColumn; X: Integer = Low(Integer);
      Y: Integer = Low(Integer)); dynamic;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ValidFieldIndex(FieldIndex: Integer): Boolean;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EditorMode;
    property FieldCount: Integer read GetFieldCount;
    property Fields[FieldIndex: Integer]: TField read GetFields;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

  TESDBGrid = class(TCustomESDBGrid)
  private
    function GetScrollBars(): TScrollStyle;
    procedure SetScrollBars(NewVal: TScrollStyle);
    function GetVisibleRowCount: Integer;
  public
    property Canvas;
    property SelectedRows;
    property Col;
    property ColCount; //Longint read GetColCount;
    property ColWidths;
    property EditorMode;
    property FixedCols;
    property FixedRows;
    property LeftCol;
    property Row;
    property RowHeights;
    property RowCount; //Longint read GetRowCount;
    property Selection;
    property TabStops;
    property TopRow;
    property VisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;    //Добавлено
  published
    property ActiveColor;               //Добавлено
    property ActiveFontColor;           //Добавлено
    property Align;
    property Anchors;
    property AutoSelect;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored False; //StoreColumns;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property GridLineWidth;
    property ImeMode;
    property ImeName;
    property MemoAllowed;
    property MemoMaxRows;
    property MemoOptions;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars  read GetScrollBars write SetScrollBars;
    property SelectedColor;             //Добавлено
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleWordWrap;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;  { obsolete }
    property OnDrawColumnCell;
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
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

  TOnGetPickListItemsMS = procedure(ACol, ARow: Integer; Items: TStrings) of Object;

  TInplaceEditListMS = class(TInPlaceEditMS)
  private
    FButtonWidth: Integer;
    FPickList: TCustomListBox;
    FActiveList: TWinControl;
    FEditStyle: TEditStyle;
    FDropDownRows: Integer;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FPickListLoaded: Boolean;
    FOnGetPickListitems: TOnGetPickListItemsMS;
    FOnEditButtonClick: TNotifyEvent;
    function GetPickList: TCustomListbox;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    procedure BoundsChanged; override;
    function ButtonRect: TRect;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DblClick; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditButtonClick; virtual;
    procedure DoGetPickListItems; dynamic;
    procedure DropDown; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function OverButton(const P: TPoint): Boolean;
    procedure PaintWindow(DC: HDC); override;
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure RestoreContents;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows;
    property EditStyle: TEditStyle read FEditStyle;
    property ListVisible: Boolean read FListVisible write FListVisible;
    property PickList: TCustomListbox read GetPickList;
    property PickListLoaded: Boolean read FPickListLoaded write FPickListLoaded;
    property Pressed: Boolean read FPressed;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnGetPickListitems: TOnGetPickListItemsMS read FOnGetPickListitems
      write FOnGetPickListitems;
  end;

  TDBGridInplaceEdit = class(TInplaceEditListMS)
  private
    FDataList: TDBLookupListBox;
    FUseDataList: Boolean;
    FLookupSource: TDatasource;
  protected
    procedure CloseUp(Accept: Boolean); override;
    procedure DoEditButtonClick; override;
    procedure DropDown; override;
    procedure UpdateContents; override;
  public
    constructor Create(Owner: TComponent); override;
    property  DataList: TDBLookupListBox read FDataList;
  end;

const
  IndicatorWidth = 11;

///procedure Register;


implementation

uses Math, DBConsts, VDBConsts, Dialogs, Types;
{$R ESDBGrids.DCR}
{$R ESDBGrids.RES}

const
  bmArrow = 'DBGARROWMS';               //Изменено
  bmEdit = 'DBEDITMS';                  //Изменено
  bmInsert = 'DBINSERTMS';              //Изменено
  bmMultiDot = 'DBMULTIDOTMS';          //Изменено
  bmMultiArrow = 'DBMULTIARROWMS';      //Изменено

  MaxMapSize = (MaxInt div 2) div SizeOf(Integer);  { 250 million }

{ Error reporting }

///procedure Register;
///begin
///  RegisterComponents('Data Controls', [TESDBGrid]);
{  RegisterComponentEditor(TESDBGrid, TDBGridEditor);
{  RegisterPropertyEditor(TypeInfo(TCollection), TESDBGrid, 'Columns',
    TESDBGridColumnsProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'FieldName', TColumnMSDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'LookupKeyField', TColumnMSDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'LookupLinkField', TColumnMSLookupKeyProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'LookupDropDownFields', TColumnMSLookupKeyProperty);}
///end;


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

{ TESDBGrid }

function TESDBGrid.GetScrollBars(): TScrollStyle;
begin
  case inherited ScrollBars of
    ssNone: result:=ssVertical;
    ssBoth: result:=ssHorizontal;
    ssVertical: result:=ssNone;
    else result:=ssBoth;
  end;
end;

function TESDBGrid.GetVisibleRowCount: Integer;      //Добавлено override если
begin                                                //уберется из TCustomESDBGrid
  Result:=TCustomESDBGrid(Self).GetVisibleRowCount;  //то убрать и здесь
end;

procedure TESDBGrid.SetScrollBars(NewVal: TScrollStyle);
begin
  case NewVal of
    ssNone: inherited ScrollBars:=ssVertical;
    ssBoth: inherited ScrollBars:=ssHorizontal;
    ssVertical: inherited ScrollBars:=ssNone;
    else inherited ScrollBars:=ssBoth;
  end;
end;

{ TDBGridInplaceEdit }

{ TDBGridInplaceEdit adds support for a button on the in-place editor,
  which can be used to drop down a table-based lookup list, a stringlist-based
  pick list, or (if button style is esEllipsis) fire the grid event
  OnEditButtonClick.  }

constructor TDBGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FLookupSource := TDataSource.Create(Self);
end;

procedure TDBGridInplaceEdit.CloseUp(Accept: Boolean);
var
  MasterField: TField;
  ListValue: Variant;
begin
  if ListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if ActiveList = DataList then
      ListValue := DataList.KeyValue
    else
      if TCustomListBox(PickList).ItemIndex <> -1 then
        ListValue := TCustomListBox(PickList).Items[TCustomListBox(Picklist).ItemIndex];
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    ListVisible := False;
    if Assigned(FDataList) then
      FDataList.ListSource := nil;
    FLookupSource.Dataset := nil;
    Invalidate;
    if Accept then
      if ActiveList = DataList then
        with TCustomESDBGrid(FGrid), FColumns[SelectedIndex].Field do
        begin
          MasterField := DataSet.FieldByName(KeyFields);
          if MasterField.CanModify and FDataLink.Edit then
            MasterField.Value := ListValue;
        end
      else
        if (not VarIsNull(ListValue)) and EditCanModify then
          with TCustomESDBGrid(FGrid), FColumns[SelectedIndex].Field do
            Text := ListValue;
  end;
end;

procedure TDBGridInplaceEdit.DoEditButtonClick;
begin
  TCustomESDBGrid(FGrid).EditButtonClick;
end;

procedure TDBGridInplaceEdit.DropDown;
var
  Column: TColumn;
begin
  if not ListVisible then
  begin
    with TCustomESDBGrid(FGrid) do
      Column := FColumns[SelectedIndex];
    if ActiveList = FDataList then
      with Column.Field do
      begin
        FDataList.Color := Color;
        FDataList.Font := Font;
        FDataList.RowCount := Column.DropDownRows;
        if FDataList.RowCount>LookupDataSet.RecordCount then
           FDataList.RowCount:=LookupDataSet.RecordCount;
        FLookupSource.DataSet := LookupDataSet;
        FDataList.KeyField := LookupKeyFields;
        FDataList.ListField := LookupResultField;
        FDataList.ListSource := FLookupSource;
        FDataList.KeyValue := DataSet.FieldByName(KeyFields).Value;
      end
    else if ActiveList = TCustomListBox(PickList) then
    begin
      TCustomListBox(PickList).Items.Assign(Column.PickList);
      if Column.DropDownRows>TCustomListBox(PickList).Count then
        DropDownRows:=TCustomListBox(PickList).Count
      else
        DropDownRows := Column.DropDownRows;
    end;
  end;
  inherited DropDown;
end;

procedure TDBGridInplaceEdit.UpdateContents;
var
  Column: TColumn;
begin
if FGrid.Columns.Count>0 then
begin
  inherited UpdateContents;
  if FUseDataList then
  begin
    if FDataList = nil then
    begin
      FDataList := TPopupDataList.Create(Self);
      FDataList.Visible := False;
      FDataList.Parent := Self;
      FDataList.OnMouseUp := ListMouseUp;
    end;
    ActiveList := FDataList;
  end;
  with TCustomESDBGrid(FGrid) do
    Column := FColumns[SelectedIndex];
  Self.ReadOnly := Column.ReadOnly;
  Font.Assign(Column.Font);
  ImeMode := Column.ImeMode;
  ImeName := Column.ImeName;
end;
end;


{ TGridDataLink }

type
  TIntArray = array[0..MaxMapSize] of Integer;
  PIntArray = ^TIntArray;

constructor TGridDataLink.Create(AGrid: TCustomESDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  VisualControl := True;
  FFirstRecNo:=FGrid.FTitleOffset;
  FScrollPocessed:=False;
  FIsAppended:=0;
  FEdited:=False;
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
  for I := 0 to FFieldCount-1 do
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
  if Active then
  begin
    if Assigned(DataSource) then
      if Assigned(DataSource.DataSet) then
        if DataSource.DataSet.IsUnidirectional then
          DatabaseError(SDataSetUnidirectional);
  end
  else
    BufferCount:=0;
  with FGrid do
  begin
    LinkActive(Active);
    FModified := False;
    if Active then
    begin
      BeginUpdate;
      try
        FScrollPocessed:=True;
        CalcBuffer;
        DataSet.UpdateCursorPos;          //Обязятельно после
        DataSet.Resync([]);       //CalcBuffer
        UpdateRowCount;
        FScrollPocessed:=False;
        DataSet.First;
      finally
        EndUpdate;
      end;
    end
    else
      RowHeights[FTitleOffset]:=DefaultRowHeight;
  end;
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
  if not FScrollPocessed then
  begin
    if (Distance<>0)  then
    begin
      with FGrid do
      begin
        BeginUpdate;
        try
          FScrollPocessed:=True;
          BufferCount:=1;
          RowCount:=1+FTitleOffset;
          CalcBuffer;
          DataSet.UpdateCursorPos;          //Обязятельно после
          DataSet.Resync([]);       //CalcBuffer
          UpdateRowCount;
          FScrollPocessed:=False;
        finally
          EndUpdate;
        end;
      end;
    end;
    FGrid.Scroll(Distance);                //Переход через верхнюю или нижнюю границы
  end;
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
      FGrid.ShowEditorMS;
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
  if FModified then RecordChanged(nil) else Dataset.Cancel;
end;

function TGridDataLink.IsAggRow(Value: Integer): Boolean;
begin
  Result := False;
end;

procedure TGridDataLink.BuildAggMap;
begin
end;

procedure TGridDataLink.DataEvent(Event: TDataEvent; Info: Integer);
var
  ABof: Boolean;
  AEof: Boolean;
  i, f: Integer;
begin
  inherited;
  if not FScrollPocessed then
    if Event=deUpdateState then
      FEdited:=True;
  if (Event=deDataSetScroll) or (Event=deDataSetChange) then
  begin
    if not FScrollPocessed then
    begin
      with FGrid do
      begin
        BeginUpdate;
        try
          FScrollPocessed:=True;
          if DataSet.State<>dsInsert then
          begin
            ABof:=Bof;
            AEof:=Eof;
            if Event=deDataSetChange then        //и не после вставки FIsAppend
            begin
              if FIsAppended>=0 then
              begin
                if (not FEdited) or AEof or ABof then
                begin
                  if AEof then
                    LastPosition
                  else
                  begin
                    BufferCount:=1;
                    RowCount:=1+FTitleOffset;
                    CalcBuffer;
                  end;
                  DataSet.UpdateCursorPos;
                  DataSet.Resync([]);
                  UpdateRowCount;
                  if ABof then
                    MoveBy(-MoveBy(-1));
                  if AEof then
                    MoveBy(-MoveBy(1));
                end;
                FEdited:=False;
              end
              else
              begin
                CalcBuffer;
                FIsAppended:=FIsAppended+1;  //??Очень слабенько
                DataSet.UpdateCursorPos;
                DataSet.Resync([]);
                if FIsAppended<0 then       //Это чтобы был перехлд в режиме Insert
                  FIsAppended:=FIsAppended+1;
                UpdateRowCount;
              end;
            end;
            if Row+1-FTitleOffset=FBotPartVisRow then   //в последней частично видимой строке
            begin
              while Row+1-FTitleOffset=FBotPartVisRow do
              begin
                BufferCount:=BufferCount-1;
                RowCount:=RowCount-1;
                CalcBuffer;
                UpdateRowCount;
              end;
              DataSet.UpdateCursorPos;
              DataSet.Resync([]);
            end;
          end
          else                              //Режим Insert
          begin
            if Event=deDataSetChange then
            begin
              if FIsAppended=0 then           //Insert
              begin
                if not (DataSet.Bof and DataSet.Eof) then
                begin
                  f:=RowHeights[RowCount-1];
                  for i:=RowCount-1 downto Row do
                    RowHeights[i]:=RowHeights[i-1];
                  RowHeights[Row]:=DefaultRowHeight;
                  DataSet.Insert;
                  RowCount:=RowCount+1;
                  RowHeights[RowCount-1]:=f;
                end;
                FIsAppended:=-1;
              end
              else                              //Append
              begin
                UpdateRowCount;
                DataSet.Cancel;
                i:=DefaultRowHeight;
                if (dgRowLines in FOptions) then
                  i:=i+GridLineWidth;
                if GridHeight+i<=ClientHeight then
                begin
                  RowCount:=RowCount+1;
                  FDataLink.BufferCount:=FDataLink.BufferCount+1;
                  DataSet.UpdateCursorPos;
                  DataSet.Resync([]);
                end
                else
                begin
                  for i:=FTitleOffset to RowCount-1 do
                    RowHeights[i]:=RowHeights[i+1];
                end;
                DataSet.Append;
                RowHeights[Row]:=DefaultRowHeight;
                end;
                FIsAppended:=-1;
            end;
          end;
          FScrollPocessed:=False;
        finally
          EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TGridDataLink.LastPosition;
begin
  With FGrid do
  begin
    DataSet.Last;
    repeat
      MoveBy(-1);
      if Bof then
        Break;
      BufferCount:=1;
      RowCount:=1+FTitleOffset;
      CalcBuffer;
    until not FLastRowFull;
    if not Bof then
      MoveBy(1);
    BufferCount:=1;
    RowCount:=1+FTitleOffset;
    CalcBuffer;
    DataSet.Last;
  end;
end;

{ TColumnTitle }
constructor TColumnTitle.Create(Column: TColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TColumnTitle.Assign(Source: TPersistent);
begin
  if Source is TColumnTitle then
  begin
    if cvTitleAlignment in TColumnTitle(Source).FColumn.FAssignedValues then
      Alignment := TColumnTitle(Source).Alignment;
    if cvTitleColor in TColumnTitle(Source).FColumn.FAssignedValues then
      Color := TColumnTitle(Source).Color;
    if cvTitleCaption in TColumnTitle(Source).FColumn.FAssignedValues then
      Caption := TColumnTitle(Source).Caption;
    if cvTitleFont in TColumnTitle(Source).FColumn.FAssignedValues then
      Font := TColumnTitle(Source).Font;
  end
  else
    inherited Assign(Source);
end;

function TColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TColumnTitle.DefaultColor: TColor;
var
  Grid: TCustomESDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TColumnTitle.DefaultFont: TFont;
var
  Grid: TCustomESDBGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.TitleFont
  else
    Result := FColumn.Font;
end;

function TColumnTitle.DefaultCaption: string;
var
  Field: TField;
begin
  Field := FColumn.Field;
  if Assigned(Field) then
    Result := Field.DisplayName
  else
    Result := FColumn.FieldName;
end;

procedure TColumnTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, cvTitleFont);
  FColumn.Changed(True);
end;

function TColumnTitle.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TColumnTitle.GetColor: TColor;
begin
  if cvTitleColor in FColumn.FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TColumnTitle.GetCaption: string;
begin
  if cvTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TColumnTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (cvTitleFont in FColumn.FAssignedValues) then
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

function TColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TColumnTitle.IsColorStored: Boolean;
begin
  Result := (cvTitleColor in FColumn.FAssignedValues) and
    (FColor <> DefaultColor);
end;

function TColumnTitle.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in FColumn.FAssignedValues);
end;

function TColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TColumnTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (cvTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  { If font was assigned, changing it back to default may affect grid title
    height, and title height changes require layout and redraw of the grid. }
  FColumn.Changed(FontAssigned);
end;

procedure TColumnTitle.SetAlignment(Value: TAlignment);
begin
  if (cvTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, cvTitleAlignment);
  FColumn.Changed(False);
end;

procedure TColumnTitle.SetColor(Value: TColor);
begin
  if (cvTitleColor in FColumn.FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FColumn.FAssignedValues, cvTitleColor);
  FColumn.Changed(False);
end;

procedure TColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TColumnTitle.SetCaption(const Value: string);
var
  Grid: TCustomESDBGrid;
begin
  Grid := Column.GetGrid;
  if Column.IsStored then
  begin
    if (cvTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
    FCaption := Value;
    Include(Column.FAssignedValues, cvTitleCaption);
    if Assigned(Grid) then
      if Grid.FTitleWordWrap then
        Column.Changed(True)
      else
        Column.Changed(False)
    else
      Column.Changed(False)
  end
  else
  begin
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Column.Field) then
      Column.Field.DisplayLabel := Value;
  end;
end;

procedure TColumnTitle.SetTextAngle(const Value: TTextAngle);
begin
  FTextAngle := Value;
end;

{ TColumn }

constructor TColumn.Create(Collection: TCollection);
var
  Grid: TCustomESDBGrid;
begin
  Grid := nil;
  FMemo:=False;
  if Assigned(Collection) and (Collection is TDBGridColumns) then
    Grid := TDBGridColumns(Collection).Grid;
  if Assigned(Grid) then Grid.BeginLayout;
  try
    inherited Create(Collection);
    FDropDownRows := 7;
    FButtonStyle := cbsAuto;
    FFont := TFont.Create;
    FFont.Assign(DefaultFont);
    FFont.OnChange := FontChanged;
    FImeMode := imDontCare;
    FImeName := Screen.DefaultIme;
    FTitle := CreateTitle;
    FVisible := True;
    FExpanded := True;
    FStored := True;
  finally
    if Assigned(Grid) then Grid.EndLayout;
  end;
end;

destructor TColumn.Destroy;
begin
  FTitle.Free;
  FFont.Free;
  FPickList.Free;
  inherited Destroy;
end;

procedure TColumn.Assign(Source: TPersistent);
begin
  if Source is TColumn then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;
      FieldName := TColumn(Source).FieldName;
      if cvColor in TColumn(Source).AssignedValues then
        Color := TColumn(Source).Color;
      if cvWidth in TColumn(Source).AssignedValues then
        Width := TColumn(Source).Width;
      if cvFont in TColumn(Source).AssignedValues then
        Font := TColumn(Source).Font;
      if cvImeMode in TColumn(Source).AssignedValues then
        ImeMode := TColumn(Source).ImeMode;
      if cvImeName in TColumn(Source).AssignedValues then
        ImeName := TColumn(Source).ImeName;
      if cvAlignment in TColumn(Source).AssignedValues then
        Alignment := TColumn(Source).Alignment;
      if cvReadOnly in TColumn(Source).AssignedValues then
        ReadOnly := TColumn(Source).ReadOnly;
      Title := TColumn(Source).Title;
      DropDownRows := TColumn(Source).DropDownRows;
      ButtonStyle := TColumn(Source).ButtonStyle;
      PickList := TColumn(Source).PickList;
      PopupMenu := TColumn(Source).PopupMenu;
      FVisible := TColumn(Source).FVisible;
      FExpanded := TColumn(Source).FExpanded;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TColumn.CreateTitle: TColumnTitle;
begin
  Result := TColumnTitle.Create(Self);
end;

function TColumn.DefaultAlignment: TAlignment;
begin
  if Assigned(Field) then
    Result := FField.Alignment
  else
    Result := taLeftJustify;
end;

function TColumn.DefaultColor: TColor;
var
  Grid: TCustomESDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Color
  else
    Result := clWindow;
end;

function TColumn.DefaultFont: TFont;
var
  Grid: TCustomESDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TColumn.DefaultImeMode: TImeMode;
var
  Grid: TCustomESDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeMode
  else
    Result := FImeMode;
end;

function TColumn.DefaultImeName: TImeName;
var
  Grid: TCustomESDBGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeName
  else
    Result := FImeName;
end;

function TColumn.DefaultReadOnly: Boolean;
var
  Grid: TCustomESDBGrid;
begin
  Grid := GetGrid;
  Result := (Assigned(Grid) and Grid.ReadOnly) or
    (Assigned(Field) and FField.ReadOnly);
end;

function TColumn.DefaultWidth: Integer;
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
          ReleaseDC(0,Canvas.Handle);
          Canvas.Handle := 0;
        end;
      end;
    end
    else
      Result := DefaultColWidth;
  end;
end;

procedure TColumn.FontChanged;
begin
  Include(FAssignedValues, cvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

function TColumn.GetAlignment: TAlignment;
begin
  if cvAlignment in FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TColumn.GetColor: TColor;
begin
  if cvColor in FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TColumn.GetExpanded: Boolean;
begin
  Result := FExpanded and Expandable;
end;

function TColumn.GetField: TField;
var
  Grid: TCustomESDBGrid;
begin    { Returns Nil if FieldName can't be found in dataset }
  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid) and
    Assigned(Grid.DataLink.DataSet) then
  with Grid.Datalink.Dataset do
    if Active or (not DefaultFields) then
      SetField(FindField(FieldName));
  Result := FField;
end;

function TColumn.GetFont: TFont;
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

function TColumn.GetGrid: TCustomESDBGrid;
begin
  if Assigned(Collection) and (Collection is TDBGridColumns) then
    Result := TDBGridColumns(Collection).FGrid
  else
    Result := nil;
end;

function TColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TColumn.GetImeMode: TImeMode;
begin
  if cvImeMode in FAssignedValues then
    Result := FImeMode
  else
    Result := DefaultImeMode;
end;

function TColumn.GetImeName: TImeName;
begin
  if cvImeName in FAssignedValues then
    Result := FImeName
  else
    Result := DefaultImeName;
end;

function TColumn.GetParentColumn: TColumn;
var
  Col: TColumn;
  Fld: TField;
  I: Integer;
begin
  Result := nil;
  Fld := Field;
  if (Fld <> nil) and (Fld.ParentField <> nil) and (Collection <> nil) then
    for I := Index - 1 downto 0 do
    begin
      Col := TColumn(Collection.Items[I]);
      if Fld.ParentField = Col.Field then
      begin
        Result := Col;
        Exit;
      end;
    end;
end;

function TColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TColumn.GetReadOnly: Boolean;
begin
  if cvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TColumn.GetShowing: Boolean;
var
  Col: TColumn;
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

function TColumn.GetVisible: Boolean;
var
  Col: TColumn;
begin
  Result := FVisible;
  if Result then
  begin
    Col := ParentColumn;
    Result := Result and ((Col = nil) or Col.Visible);
  end;
end;

function TColumn.GetWidth: Integer;
begin
  if not Showing then
    Result := -1
  else if cvWidth in FAssignedValues then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TColumn.IsAlignmentStored: Boolean;
begin
  Result := (cvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TColumn.IsColorStored: Boolean;
begin
  Result := (cvColor in FAssignedValues) and (FColor <> DefaultColor);
end;

function TColumn.IsFontStored: Boolean;
begin
  Result := (cvFont in FAssignedValues);
end;

function TColumn.IsImeModeStored: Boolean;
begin
  Result := (cvImeMode in FAssignedValues) and (FImeMode <> DefaultImeMode);
end;

function TColumn.IsImeNameStored: Boolean;
begin
  Result := (cvImeName in FAssignedValues) and (FImeName <> DefaultImeName);
end;

function TColumn.IsReadOnlyStored: Boolean;
begin
  Result := (cvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

function TColumn.IsWidthStored: Boolean;
begin
  Result := (cvWidth in FAssignedValues) and (FWidth <> DefaultWidth);
end;

procedure TColumn.RefreshDefaultFont;
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

procedure TColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;
  FPickList.Free;
  FPickList := nil;
  ButtonStyle := cbsAuto;
  Changed(FontAssigned);
end;

procedure TColumn.SetAlignment(Value: TAlignment);
var
  Grid: TCustomESDBGrid;
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
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Field) then
      Field.Alignment := Value;
  end;
  if Self.Grid<>nil then
    if TDBGridInplaceEdit(Self.Grid.InplaceEditor)<>nil then
      TDBGridInplaceEdit(Self.Grid.InplaceEditor).WantRecreateWnd;
end;

procedure TColumn.SetButtonStyle(Value: TColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  Changed(False);
end;

procedure TColumn.SetColor(Value: TColor);
begin
  if (cvColor in FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FAssignedValues, cvColor);
  Changed(False);
end;

procedure TColumn.SetField(Value: TField);
begin
  if FField = Value then Exit;
  if Assigned(FField) and (GetGrid <> nil) then
    FField.RemoveFreeNotification(GetGrid);
  if Assigned(Value) and (csDestroying in Value.ComponentState) then
    Value := nil;    // don't acquire references to fields being destroyed
  FField := Value;
  if Assigned(Value) then
  begin
    if GetGrid <> nil then
      FField.FreeNotification(GetGrid);
    FFieldName := Value.FullName;
  end;
  if not IsStored then
  begin
    if Value = nil then
      FFieldName := '';
    RestoreDefaults;
  end;
  Changed(False);
end;

procedure TColumn.SetFieldName(const Value: String);
var
  AField: TField;
  Grid: TCustomESDBGrid;
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

procedure TColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
  if FMemo and Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Field) then
  begin
    Grid.BeginUpdate;
    try
      Grid.FDataLink.FScrollPocessed:=True;
      Grid.CalcBuffer;
      Grid.FDataLink.FScrollPocessed:=False;
      Grid.UpdateRowCount;
    finally
      Grid.EndUpdate;
    end;
    Grid.InvalidateGrid;
  end;
end;

procedure TColumn.SetImeMode(Value: TImeMode);
begin
  if (cvImeMode in FAssignedValues) or (Value <> DefaultImeMode) then
  begin
    FImeMode := Value;
    Include(FAssignedValues, cvImeMode);
  end;
  Changed(False);
end;

procedure TColumn.SetImeName(Value: TImeName);
begin
  if (cvImeName in FAssignedValues) or (Value <> DefaultImeName) then
  begin
    FImeName := Value;
    Include(FAssignedValues, cvImeName);
  end;
  Changed(False);
end;

procedure TColumn.SetIndex(Value: Integer);
var
  Grid: TCustomESDBGrid;
  Fld: TField;
  I, OldIndex: Integer;
  Col: TColumn;
begin
  OldIndex := Index;
  Grid := GetGrid;

  if IsStored then
  begin
    Grid.BeginLayout;
    try
      I := OldIndex + 1;  // move child columns along with parent
      while (I < Collection.Count) and (TColumn(Collection.Items[I]).ParentColumn = Self) do
        Inc(I);
      Dec(I);
      if OldIndex > Value then   // column moving left
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
        Col := Grid.ColumnAtDepth(Grid.FColumns[Value], Depth);
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

procedure TColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TColumn.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;

procedure TColumn.SetReadOnly(Value: Boolean);
var
  Grid: TCustomESDBGrid;
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

procedure TColumn.SetTitle(Value: TColumnTitle);
begin
  FTitle.Assign(Value);
end;

procedure TColumn.SetWidth(Value: Integer);
var
  Grid: TCustomESDBGrid;
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

procedure TColumn.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TColumn.SetExpanded(Value: Boolean);
const
  Direction: array [Boolean] of ShortInt = (-1,1);
var
  Grid: TCustomESDBGrid;
  WasShowing: Boolean;
begin
  if Value <> FExpanded then
  begin
    Grid := GetGrid;
    WasShowing := (Grid <> nil) and Grid.FColumns[Grid.SelectedIndex].Showing;
    FExpanded := Value;
    Changed(True);
    if (Grid <> nil) and WasShowing then
    begin
      if not Grid.FColumns[Grid.SelectedIndex].Showing then
        // The selected cell was hidden by this expand operation
        // Select 1st child (next col = 1) when parent is expanded
        // Select child's parent (prev col = -1) when parent is collapsed
        Grid.MoveCol(Grid.Col, Direction[FExpanded]);
    end;
  end;
end;

function TColumn.Depth: Integer;
var
  Col: TColumn;
begin
  Result := 0;
  Col := ParentColumn;
  if Col <> nil then Result := Col.Depth + 1;
end;

function TColumn.GetExpandable: Boolean;
var
  Fld: TField;
begin
  Fld := Field;
  Result := (Fld <> nil) and (Fld.DataType in [ftADT, ftArray]);
end;

procedure TColumn.SetFMemo(const Value: Boolean);
begin
  if Value <> FMemo then
  begin
    FMemo := Value;
    Changed(False);
  end;
end;

{ TDBGridColumns }

constructor TDBGridColumns.Create(Grid: TCustomESDBGrid; ColumnClass: TColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

function TDBGridColumns.Add: TColumn;
begin
  Result := TColumn(inherited Add);
end;

function TDBGridColumns.GetColumn(Index: Integer): TColumn;
begin
  Result := TColumn(inherited Items[Index]);
end;

function TDBGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TDBGridColumns.LoadFromFile(const Filename: string);
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
  TColumnsWrapper = class(TComponent)
  private
    FColumns: TDBGridColumns;
  published
    property Columns: TDBGridColumns read FColumns write FColumns;
  end;

procedure TDBGridColumns.LoadFromStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := FGrid.CreateColumns;
    S.ReadComponent(Wrapper);
    Assign(Wrapper.Columns);
  finally
    Wrapper.Columns.Free;
    Wrapper.Free;
  end;
end;

procedure TDBGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count-1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TDBGridColumns.RebuildColumns;

  procedure AddFields(Fields: TFields; Depth: Integer);
  var
    I: Integer;
  begin
    Inc(Depth);
    for I := 0 to Fields.Count-1 do
    begin
      Add.FieldName := Fields[I].FullName;
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

procedure TDBGridColumns.SaveToFile(const Filename: string);
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

procedure TDBGridColumns.SaveToStream(S: TStream);
var
  Wrapper: TColumnsWrapper;
begin
  Wrapper := TColumnsWrapper.Create(nil);
  try
    Wrapper.Columns := Self;
    S.WriteComponent(Wrapper);
  finally
    Wrapper.Free;
  end;
end;

procedure TDBGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign(Value);
end;

procedure TDBGridColumns.SetState(NewState: TDBGridColumnsState);
begin
  if NewState = State then Exit;
  if NewState = csDefault then
    Clear
  else
    RebuildColumns;
end;

procedure TDBGridColumns.Update(Item: TCollectionItem);
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
    FGrid.ColWidths[Raw] := TColumn(Item).Width;
  end;
end;

function TDBGridColumns.InternalAdd: TColumn;
begin
  Result := Add;
  Result.IsStored := False;
end;

function TDBGridColumns.GetState: TDBGridColumnsState;
begin
  Result := TDBGridColumnsState((Count > 0) and Items[0].IsStored);
end;

{ TBookmarkList }

constructor TBookmarkList.Create(AGrid: TCustomESDBGrid);
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
    if C < 0 then L := I + 1 else
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
      for I := FList.Count-1 downto 0 do
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
    if Result then FGrid.Invalidate;
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
end;

procedure TBookmarkList.StringsChanged(Sender: TObject);
begin
  FCache := '';
  FCacheIndex := -1;
end;


{ TCustomESDBGrid }

var
  DrawBitmap: TBitmap;
  UserCount: Integer;

procedure UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then DrawBitmap.Free;
end;

procedure DrawCellText(ACanvas: TCanvas; ARect: TRect; const Text: string;
     WordWrap: Boolean; Alignment: TAlignment; ARightToLeft: Boolean);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX );
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  I: TColorRef;
  uFormat: Integer;
  R,B: TRect;
  dc, bmdc: HDC;
  Hold: Integer;
  bmsize: SIZE;
  br: HBRUSH;
begin
  dc:=ACanvas.Handle;
  uFormat:=AlignFlags[Alignment] or RTL[ARightToLeft];
  if Alignment=taLeftJustify then
    uFormat:=uFormat or DT_WORD_ELLIPSIS;
  if WordWrap then
    uFormat:=uFormat or DT_WORDBREAK;
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(dc, I) = I then
  begin                       { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
    R.Left:=ARect.Left+2;
    R.Top:=ARect.Top+2;
    if Alignment=taRightJustify then
      R.Right:=ARect.Right-3
    else
      R.Right:=ARect.Right-2;
    R.Bottom:=ARect.Bottom-2;
    FillRect(dc,ARect,ACanvas.Brush.Handle);
    DrawText(dc,PChar(Text),-1,R, uFormat);
  end
  else
  begin                  { Use FillRect and Drawtext for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      bmdc:=DrawBitmap.Canvas.Handle;
      GetBitmapDimensionEx(bmdc, bmsize);
      with ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        bmsize.cx := Max(bmsize.cx, Right - Left);
        bmsize.cy := Max(bmsize.cy, Bottom - Top);
        SetBitmapDimensionEx(bmdc, bmsize.cx, bmsize.cy, nil);
        R := Rect(2, 2, Right - Left - 2, Bottom - Top - 3);
        if Alignment=taRightJustify then
          R.Right:=Right-1;
        B := Rect(0, 0, Right - Left, Bottom - Top);
        SelectObject(bmdc,ACanvas.Font.Handle);
        SetTextColor(bmdc, ACanvas.Font.Color);
        br:=CreateSolidBrush(ACanvas.Font.Color);
        FillRect(bmdc, B, br);
        DeleteObject(br);
        SetBkMode(bmdc, TRANSPARENT);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(bmdc,PChar(Text),-1,R, uFormat);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
        begin
          Hold := Left;
          Left := Right;
          Right := Hold;
        end;
        BitBlt(dc, Left, Top, Right - Left, Bottom - Top, bmdc, 0, 0, SRCCOPY);
      end;
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

constructor TCustomESDBGrid.Create(AOwner: TComponent);
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
  finally
    Bmp.Free;
  end;
  FTitleOffset := 1;
  FIndicatorOffset := 1;
  FUpdateFields := True;
  FOptions := [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit];
  if SysLocale.PriLangID = LANG_KOREAN then
    Include(FOptions, dgAlwaysShowEditor);
  DesignOptionsBoost := [goColSizing];
  VirtualView := True;
  UsesBitmap;
  ScrollBars := ssHorizontal;
  inherited Options := [goFixedHorzLine, goFixedVertLine, goHorzLine,
    goVertLine, goColSizing, goTabs, goEditing];
  FColumns := CreateColumns;
  FVisibleColumns := TList.Create;
  inherited RowCount := 2;
  inherited ColCount := 2;
  FDataLink := CreateDataLink;
  Color := clWindow;
  ParentColor := False;
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := TitleFontChanged;
  FSaveCellExtents := False;
  FUserChange := True;
  FDefaultDrawing := True;
  FBookmarks := TBookmarkList.Create(Self);
  HideEditor;
  FTitleWordWrap:=False;
  FActiveColor:=clMenuHighlight;
  FActiveFontColor:=clHighlightText;
  FSelectedColor:=clHotLight;
  FMemoMaxRows:=3;
  FBotPartVisRow:=-1;
  FLastRowFull:=False;
  FLastVisRow:=-1;
  FAutoSelect:=True;
  FMemoOptions:=[moOnlyTabMove, moScrollBar,  moWantArrows];
end;

destructor TCustomESDBGrid.Destroy;
var
  I,J: Integer;

begin
  if DataSource<> nil then
  begin
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I] is TCustomForm then
        with Screen.Forms[I] as TCustomForm do
          for J := 0 to ComponentCount - 1 do
            if Components[J].ClassName='TDBMultiNav' then
            begin
              if GetObjectProp(Components[J], 'DBGrid' ) as TESDBGrid = Self then
                SetObjectProp(Components[J],'DBGrid',nil);
            end;
  end;
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

function TCustomESDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TCustomESDBGrid.RawToDataColumn(ACol: Integer): Integer;
begin
  Result := ACol - FIndicatorOffset;
end;

function TCustomESDBGrid.DataToRawColumn(ACol: Integer): Integer;
begin
  Result := ACol + FIndicatorOffset;
end;

function TCustomESDBGrid.AcquireLayoutLock: Boolean;
begin
  Result := (FUpdateLock = 0) and (FLayoutLock = 0);
  if Result then BeginLayout;
end;

procedure TCustomESDBGrid.BeginLayout;
begin
  BeginUpdate;
  if FLayoutLock = 0 then FColumns.BeginUpdate;
  Inc(FLayoutLock);
end;

procedure TCustomESDBGrid.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomESDBGrid.CancelLayout;
begin
  if FLayoutLock > 0 then
  begin
    if FLayoutLock = 1 then
      FColumns.EndUpdate;
    Dec(FLayoutLock);
    EndUpdate;
  end;
end;

function TCustomESDBGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  with FColumns[SelectedIndex] do
    Result := FDatalink.Active and Assigned(Field) and Field.IsValidChar(Key);
end;

function TCustomESDBGrid.CanEditModify: Boolean;
begin
  Result := False;
  if not ReadOnly and FDatalink.Active and not FDatalink.Readonly then
  with FColumns[SelectedIndex] do
    if (not ReadOnly) and Assigned(Field) and Field.CanModify
      and (not (Field.DataType in ftNonTextTypes) or Assigned(Field.OnSetText)) then
    begin
      FDatalink.Edit;
      Result := FDatalink.Editing;
      if Result then FDatalink.Modified;
    end;
end;

function TCustomESDBGrid.CanEditShow: Boolean;
begin
  Result := (LayoutLock = 0) and inherited CanEditShow;
end;

procedure TCustomESDBGrid.CellClick(Column: TColumn);
begin
  if Assigned(FOnCellClick) then FOnCellClick(Column);
end;

procedure TCustomESDBGrid.ColEnter;
begin
  UpdateIme;
  if Assigned(FOnColEnter) then FOnColEnter(Self);
end;

procedure TCustomESDBGrid.ColExit;
begin
  if Assigned(FOnColExit) then FOnColExit(Self);
end;

procedure TCustomESDBGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  FromIndex := RawToDataColumn(FromIndex);
  ToIndex := RawToDataColumn(ToIndex);
  FColumns[FromIndex].Index := ToIndex;
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;

procedure TCustomESDBGrid.ColWidthsChanged;
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

function TCustomESDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TDBGridColumns.Create(Self, TColumn);
end;

function TCustomESDBGrid.CreateDataLink: TGridDataLink;
begin
  Result := TGridDataLink.Create(Self);
end;

function TCustomESDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEdit(TDBGridInplaceEdit.Create(Self));
end;

procedure TCustomESDBGrid.CreateWnd;
begin
  BeginUpdate;   { prevent updates in WMSize message that follows WMCreate }
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

procedure TCustomESDBGrid.DataChanged;
begin
  if not HandleAllocated then Exit;
  UpdateRowCount;
  UpdateScrollBar;
  UpdateActive;
  InvalidateEditor;
  ValidateRect(Handle, nil);
  Invalidate;
end;

procedure TCustomESDBGrid.DefaultHandler(var Msg);
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
      P := FColumns[RawToDataColumn(Cell.X)].PopupMenu;
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

procedure TCustomESDBGrid.DeferLayout;
var
  M: TMsg;
begin
  if HandleAllocated and
    not PeekMessage(M, Handle, cm_DeferLayout, cm_DeferLayout, pm_NoRemove) then
    PostMessage(Handle, cm_DeferLayout, 0, 0);
  CancelLayout;
end;

procedure TCustomESDBGrid.DefineFieldMap;
var
  I: Integer;
begin
  if FColumns.State = csCustomized then
  begin   { Build the column/field map from the column attributes }
    DataLink.SparseMap := True;
    for I := 0 to FColumns.Count-1 do
      FDataLink.AddMapping(FColumns[I].FieldName);
  end
  else   { Build the column/field map from the field list order }
  begin
    FDataLink.SparseMap := False;
    with Datalink.Dataset do
      for I := 0 to FieldList.Count - 1 do
        with FieldList[I] do if Visible then Datalink.AddMapping(FullName);
  end;
end;

function TCustomESDBGrid.UseRightToLeftAlignmentForField(const AField: TField;
  Alignment: TAlignment): Boolean;
begin
  Result := False;
  if IsRightToLeft then
    Result := OkToChangeFieldAlignment(AField, Alignment);
end;

procedure TCustomESDBGrid.DefaultDrawDataCell(const Rect: TRect; Field: TField;
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
  DrawCellText(Canvas, Rect, Value, FMemoAllowed and TColumn(Field.GetParentComponent).FMemo, Alignment,
    UseRightToLeftAlignmentForField(Field, Alignment));
end;

procedure TCustomESDBGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  Value: string;
begin
  Value := '';
  if Assigned(Column.Field) then
    Value := Column.Field.DisplayText;
  DrawCellText(Canvas, Rect, Value, FMemoAllowed and Column.FMemo, Column.Alignment,
     UseRightToLeftAlignmentForField(Column.Field, Column.Alignment));
end;

procedure TCustomESDBGrid.ReadColumns(Reader: TReader);
begin
  FColumns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(FColumns);
end;

procedure TCustomESDBGrid.WriteColumns(Writer: TWriter);
begin
  if FColumns.State = csCustomized then
    Writer.WriteCollection(FColumns)
  else  // ancestor state is customized, ours is not
    Writer.WriteCollection(nil);
end;

procedure TCustomESDBGrid.DefineProperties(Filer: TFiler);
var
  StoreIt: Boolean;
  vState: TDBGridColumnsState;
begin
  vState := FColumns.State;
  if Filer.Ancestor = nil then
    StoreIt := vState = csCustomized
  else
    if vState <> TCustomESDBGrid(Filer.Ancestor).FColumns.State then
      StoreIt := True
    else
      StoreIt := (vState = csCustomized) and
        (not CollectionsEqual(FColumns, TCustomESDBGrid(Filer.Ancestor).FColumns, Self, TCustomESDBGrid(Filer.Ancestor)));

  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, StoreIt);
end;

function TCustomESDBGrid.ColumnAtDepth(Col: TColumn; ADepth: Integer): TColumn;
begin
  Result := Col;
  while (Result <> nil) and (Result.Depth > ADepth) do
    Result := Result.ParentColumn;
end;

function TCustomESDBGrid.CalcTitleRect(Col: TColumn; ARow: Integer;
  var MasterCol: TColumn): TRect;
var
  I,J: Integer;
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
    I := LeftCol;
    if Col.Depth > ARow then
      J := ARow
    else
      J := Col.Depth;
  end;

  Result := CellRect(I, J);

  InBiDiMode := UseRightToLeftAlignment and
                (Canvas.CanvasOrientation = coLeftToRight);

  for I := Col.Index to FColumns.Count-1 do
  begin
    if ColumnAtDepth(FColumns[I], ARow) <> MasterCol then Break;
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
  if (J <= ARow) and (J < FixedRows-1) then
  begin
    CalcFixedInfo(DrawInfo);
    Result.Bottom := DrawInfo.Vert.FixedBoundary - DrawInfo.Vert.EffectiveLineWidth;
  end;
end;

procedure TCustomESDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  FrameOffs: Byte;

  function RowIsMultiSelected: Boolean;
  var
    Index: Integer;
  begin
    Result := (dgMultiSelect in Options) and Datalink.Active and
      FBookmarks.Find(Datalink.Datasource.Dataset.Bookmark, Index);
  end;

  procedure DrawTitleCell(ACol, ARow: Integer; Column: TColumn; var AState: TGridDrawState);
  const
    ScrollArrows: array [Boolean, Boolean] of Integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    MasterCol: TColumn;
    TitleRect, TextRect, ButtonRect: TRect;
    I: Integer;
    InBiDiMode: Boolean;
  begin
    TitleRect := CalcTitleRect(Column, ARow, MasterCol);

    if MasterCol = nil then
    begin
      Canvas.FillRect(ARect);
      Exit;
    end;

    Canvas.Font := MasterCol.Title.Font;
    Canvas.Brush.Color := MasterCol.Title.Color;
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
        if InBiDiMode then { stretch the arrows box }
          Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
        DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
          ScrollArrows[InBiDiMode, MasterCol.Expanded] or DFCS_FLAT);
      finally
        RestoreDC(Canvas.Handle, I);
      end;
    end;
    with MasterCol.Title do
    begin
      OffsetRect(TextRect,-1,-1);  //Грязный заголовок
//      TextRect.Left:=TextRect.Left-1;
      TextRect.Bottom:=TextRect.Bottom+2;
      TextRect.Right:=TextRect.Right+1;
      DrawCellText(Canvas, TextRect, Caption, FTitleWordWrap, Alignment, IsRightToLeft);
    end;
    if [dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines] then
    begin
      InflateRect(TitleRect, 1, 1);
      DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, TitleRect, BDR_RAISEDINNER, BF_TOPLEFT);
    end;
    AState := AState - [gdFixed];  // prevent box drawing later
  end;

var
  OldActive: Integer;
  Indicator: Integer;
  Highlight: Boolean;
  Value: string;
  DrawColumn: TColumn;
  MultiSelected: Boolean;
  ALeft: Integer;
//  Red, Green, Blue, High: Integer;        //Добавлено
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
    FrameOffs := 1;
  end
  else
    FrameOffs := 2;

  if (gdFixed in AState) and (ACol < 0) then
  begin
    Canvas.Brush.Color := FixedColor;
    Canvas.FillRect(ARect);
    if Assigned(DataLink) and DataLink.Active  then
    begin
      MultiSelected := False;
      if ARow >= 0 then
      begin
        OldActive := FDataLink.ActiveRecord;
        try
          FDatalink.ActiveRecord := ARow;
          MultiSelected := RowIsMultiselected;
        finally
          FDatalink.ActiveRecord := OldActive;
        end;
      end;
      if (ARow = FDataLink.ActiveRecord) or MultiSelected then
      begin
        Indicator := 0;
        if FDataLink.DataSet <> nil then
          case FDataLink.DataSet.State of
            dsEdit:
              if ARow=FDataLink.ActiveRecord then       //Добавлено
                Indicator := 1
              else                                      //
                Indicator:=3;                           //
            dsInsert:
              if ARow=FDataLink.ActiveRecord then       //
                Indicator := 2
              else                                      //
                Indicator:=3;                           //что помечено
            dsBrowse:
              if MultiSelected then
                if (ARow <> FDatalink.ActiveRecord) then
                  Indicator := 3
                else
                  Indicator := 4;  // multiselected and current row
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
  else with Canvas do
  begin
    DrawColumn := FColumns[ACol];
    if not DrawColumn.Showing then Exit;
    if not (gdFixed in AState) then
    begin
      Font := DrawColumn.Font;
      Brush.Color := DrawColumn.Color;
    end;
    if ARow < 0 then
      DrawTitleCell(ACol, ARow + FTitleOffset, DrawColumn, AState)
    else if (FDataLink = nil) or not FDataLink.Active then
      FillRect(ARect)
    else
    begin
      Value := '';
      OldActive := FDataLink.ActiveRecord;
      try
        FDataLink.ActiveRecord := ARow;
        if Assigned(DrawColumn.Field) then
          Value := DrawColumn.Field.DisplayText;
        Highlight := HighlightCell(ACol, ARow, Value, AState);
        if Highlight then
        begin
          Font.Color := FActiveFontColor;
          if FBookmarks.Count>0 then                    //Добавлено
            if FBookmarks.CurrentRowSelected then
            begin
{*              Indicator:=ColorToRGB(clHighlight);
              Red:=Indicator and $000000FF;
              Green:=(Indicator and $0000FF00) shr 8;
              Blue:=(Indicator and $00FF0000) shr 16;
              High:=Max(Red,Green);
              High:=Max(High,Blue);
              If High=Red then
                Indicator:=Indicator xor $007F7F00;
              If High=Green then
                 Indicator:=Indicator xor $007F007F;
              If High=Blue then
                 Indicator:=Indicator xor $00007F7F;
              Brush.Color:=TColor(Indicator);*}
              Brush.Color:=FSelectedColor;
            end
            else
            begin          //Цвет текущей при наличии помеченных
//              Indicator:=ColorToRGB(clHighlight);       //Вариант 1
//              Red:=Indicator and $000000FF;
//              Green:=(Indicator and $0000FF00) shr 8;
//              Blue:=(Indicator and $00FF0000) shr 16;
//              High:=Max(Red,Green);
//              High:=Max(High,Blue);
//              If High=Red then
//                Indicator:=Indicator or $007F7F00;
//              If High=Green then
//                 Indicator:=Indicator or $007F007F;
//              If High=Blue then
//                 Indicator:=Indicator or $00007F7F;
//              Brush.Color:=TColor(Indicator)
//              Brush.Color:=clHighlight                  //Вариант 2
//              Brush.Color:=Color;                       //Вариант 3
//              Font.Color:=FTitleFont.Color
              Brush.Color:=FActiveColor;
            end
          else                                          //до сих пор
            Brush.Color:=FActiveColor;
        end;
        if not Enabled then
          Font.Color := clGrayText;
        if FDefaultDrawing then
          DrawCellText(Canvas,ARect,Value,FMemoAllowed and DrawColumn.FMemo,DrawColumn.Alignment,
            UseRightToLeftAlignmentForField(DrawColumn.Field, DrawColumn.Alignment));
        if FColumns.State = csDefault then
          DrawDataCell(ARect, DrawColumn.Field, AState);
        DrawColumnCell(ARect, ACol, DrawColumn, AState);
      finally
        FDataLink.ActiveRecord := OldActive;
      end;
      if FDefaultDrawing and (gdSelected in AState)
        and ((dgAlwaysShowSelection in Options) or Focused)
        and not (csDesigning in ComponentState)
        and not (dgRowSelect in Options)
        and (UpdateLock = 0)
        and (ValidParentForm(Self).ActiveControl = Self) then
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

procedure TCustomESDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  if Assigned(FOnDrawDataCell) then FOnDrawDataCell(Self, Rect, Field, State);
end;

procedure TCustomESDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin
  if Assigned(OnDrawColumnCell) then
    OnDrawColumnCell(Self, Rect, DataCol, Column, State);
end;

procedure TCustomESDBGrid.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Self)
  else
    ShowPopupEditor(FColumns[SelectedIndex]);
end;

procedure TCustomESDBGrid.EditingChanged;
begin
  if dgIndicator in Options then InvalidateCell(0, FSelRow);
end;

procedure TCustomESDBGrid.EndLayout;
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

procedure TCustomESDBGrid.EndUpdate;
begin
  if FUpdateLock > 0 then
    Dec(FUpdateLock);
end;

function TCustomESDBGrid.GetColField(DataCol: Integer): TField;
begin
  Result := nil;
  if (DataCol >= 0) and FDatalink.Active and (DataCol < FColumns.Count) then
    Result := FColumns[DataCol].Field;
end;

function TCustomESDBGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomESDBGrid.GetEditLimit: Integer;
begin
  Result := 0;
  if Assigned(SelectedField) and (SelectedField.DataType in [ftString, ftWideString]) then
    Result := SelectedField.Size;
end;

function TCustomESDBGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
  if FDatalink.Active then
  with FColumns[RawToDataColumn(ACol)] do
    if Assigned(Field) then
      Result := Field.EditMask;
end;

function TCustomESDBGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
  Column: TColumn;
  MasterField: TField;
begin
  TDBGridInplaceEdit(InplaceEditor).FUseDataList := False;
  Column := FColumns[SelectedIndex];
  Result := esSimple;
  case Column.ButtonStyle of
   cbsEllipsis:
     Result := esEllipsis;
   cbsAuto:
     if Assigned(Column.Field) then
     with Column.Field do
     begin
       { Show the dropdown button only if the field is editable }
       if FieldKind = fkLookup then
       begin
         MasterField := Dataset.FieldByName(KeyFields);
         { Column.DefaultReadonly will always be True for a lookup field.
           Test if Column.ReadOnly has been assigned a value of True }
         if Assigned(MasterField) and MasterField.CanModify and
           not ((cvReadOnly in Column.AssignedValues) and Column.ReadOnly) then
           if not FReadOnly and DataLink.Active and not Datalink.ReadOnly then
             Result := esPickList;
             TDBGridInplaceEdit(InplaceEditor).FUseDataList := True;
       end
       else
       if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and
         not Column.Readonly then
         Result := esPickList
       else if DataType in [ftDataset, ftReference] then
         Result := esEllipsis;
     end;
  end;
end;

function TCustomESDBGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
  if FDatalink.Active then
  with FColumns[RawToDataColumn(ACol)] do
    if Assigned(Field) then
      Result := Field.Text;
  FEditText := Result;
end;

function TCustomESDBGrid.GetFieldCount: Integer;
begin
  Result := FDatalink.FieldCount;
end;

function TCustomESDBGrid.GetFields(FieldIndex: Integer): TField;
begin
  Result := FDatalink.Fields[FieldIndex];
end;

function TCustomESDBGrid.GetFieldValue(ACol: Integer): string;
var
  Field: TField;
begin
  Result := '';
  Field := GetColField(ACol);
  if Field <> nil then Result := Field.DisplayText;
end;

function TCustomESDBGrid.GetSelectedField: TField;
var
  Index: Integer;
begin
  Index := SelectedIndex;
  if Index <> -1 then
    Result := FColumns[Index].Field
  else
    Result := nil;
end;

function TCustomESDBGrid.GetSelectedIndex: Integer;
begin
  Result := RawToDataColumn(Col);
end;

function TCustomESDBGrid.HighlightCell(DataCol, DataRow: Integer;
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

procedure TCustomESDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

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
      if AddAfter then
        FBookmarks.CurrentRowSelected := True;
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
      begin
        DoSelection(Select, 1);
        if (dgAlwaysShowEditor in Options) then
        begin
          InvalidateEditor;
          TDBGridInplaceEdit(InplaceEditor).NeedScrollBars;
          TDBGridInplaceEdit(InplaceEditor).Invalidate;
        end;
      end;
      if FDataLink.EOF and CanModify and (not ReadOnly) and (dgEditing in Options) then
      begin
        FDataLink.FIsAppended:=1;
        Append;
      end;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    with FDatalink.Dataset do
      if (State = dsInsert) and not Modified and FDataLink.EOF and
        not FDatalink.FModified then
        Cancel
      else
      begin
        DoSelection(Select, -1);
        if (dgAlwaysShowEditor in Options) then
          TDBGridInplaceEdit(InplaceEditor).NeedScrollBars;
      end;
  end;

  procedure Tab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    BeginUpdate;    { Prevent highlight flicker on tab to next/prior row }
    try
      while True do
      begin
        if GoForward then
          Inc(ACol) else
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
      Msg := SDeleteMultipleRecordsQuestion
    else
      Msg := SDeleteRecordQuestion;
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(Msg, mtConfirmation, mbOKCancel, 0) <> idCancel);
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
  with FDatalink.DataSet do
    if ssCtrl in Shift then
    begin
      if (Key in RowMovementKeys) then ClearSelection;
      case Key of
        VK_UP, VK_PRIOR: FDataLink.MoveBy(-FDatalink.ActiveRecord);
        VK_DOWN, VK_NEXT: FDataLink.MoveBy(FDatalink.BufferCount - FDatalink.ActiveRecord - 1);
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
            PriorRow(False) else
            MoveCol(Col - 1, -1);
        VK_RIGHT:
          if dgRowSelect in Options then
            NextRow(False) else
            MoveCol(Col + 1, 1);
        VK_HOME:
          if (ColCount = FIndicatorOffset+1)
            or (dgRowSelect in Options) then
          begin
            ClearSelection;
            First;
          end
          else
            MoveCol(FIndicatorOffset, 1);
        VK_END:
          if (ColCount = FIndicatorOffset+1)
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
          NextPage;
        end;
        VK_PRIOR:
          begin
            ClearSelection;
            PrevPage;
//            FDataLink.MoveBy(-VisibleRowCount);
          end;
        VK_INSERT:
          if CanModify and (not ReadOnly) and (dgEditing in Options) then
          begin
            ClearSelection;
            Insert;
          end;
        VK_TAB: if not (ssAlt in Shift) then Tab(not (ssShift in Shift));
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

procedure TCustomESDBGrid.KeyPress(var Key: Char);
begin
  FIsESCKey := False;
  if not (dgAlwaysShowEditor in Options) and (Key = #13) then
    FDatalink.UpdateData;
  inherited KeyPress(Key);
end;

{ InternalLayout is called with layout locks and column locks in effect }
procedure TCustomESDBGrid.InternalLayout;

  function FieldIsMapped(F: TField): Boolean;
  var
    X: Integer;
  begin
    Result := False;
    if F = nil then Exit;
    for X := 0 to FDatalink.FieldCount-1 do
      if FDatalink.Fields[X] = F then
      begin
        Result := True;
        Exit;
      end;
  end;

  procedure CheckForPassthroughs;  // check for Columns.State flip-flop
  var
    SeenPassthrough: Boolean;
    I, J: Integer;
    Column: TColumn;
  begin
    SeenPassthrough := False;
    for I := 0 to FColumns.Count-1 do
      if not FColumns[I].IsStored then
        SeenPassthrough := True
      else if SeenPassthrough then
      begin  // we have both persistent and non-persistent columns.  Kill the latter
        for J := FColumns.Count-1 downto 0 do
        begin
          Column := FColumns[J];
          if not Column.IsStored then
            Column.Free;
        end;
        Exit;
      end;
  end;

  procedure ResetColumnFieldBindings;
  var
    I, J, K: Integer;
    Fld: TField;
    Column: TColumn;
  begin
    if FColumns.State = csDefault then
    begin
       { Destroy columns whose fields have been destroyed or are no longer
         in field map }
      if (not FDataLink.Active) and (FDatalink.DefaultFields) then
        FColumns.Clear
      else
        for J := FColumns.Count-1 downto 0 do
          with FColumns[J] do
          if not Assigned(Field)
            or not FieldIsMapped(Field) then Free;
      I := FDataLink.FieldCount;
      if (I = 0) and (FColumns.Count = 0) then Inc(I);
      for J := 0 to I-1 do
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
      for I := 0 to FColumns.Count-1 do
        FColumns[I].Field := nil;
    end;
  end;

  procedure MeasureTitleHeights;
  var
    I, J, K, D, B: Integer;
    RestoreCanvas: Boolean;
    Heights: array of Integer;
//Добавлено
    LinesWidth: Integer;
    RowH: Integer;
    Rect: TRect;
  begin
    RestoreCanvas := not HandleAllocated;
    if RestoreCanvas then
      Canvas.Handle := GetDC(0);
    try
      Canvas.Font := Font;
      K := Canvas.TextHeight('Wg') + 3;
      if dgRowLines in Options then
        Inc(K, GridLineWidth);
//      DefaultRowHeight := K;
      B := GetSystemMetrics(SM_CYHSCROLL);
      if dgTitles in Options then
      begin
        SetLength(Heights, FTitleOffset+1);

//Вставка
    if dgRowLines in Options then
      LinesWidth:=GridLineWidth
    else
      LinesWidth:=0;
    RowH:=DefaultRowHeight;
//До сих пор

        for I := 0 to FColumns.Count-1 do
        begin
          Canvas.Font := FColumns[I].Title.Font;
          D := FColumns[I].Depth;
          if D <= High(Heights) then
          begin
//Вставка


            if FTitleWordWrap then
            begin
              Rect.Top:=0;
              Rect.Left:=0;
              Rect.Right:=FColumns[I].FWidth-2*LinesWidth-2-1;
              J:=Max(DrawText(Canvas.Handle,PChar(FColumns[I].Title.FCaption),-1,Rect,DT_WORDBREAK or DT_CALCRECT)+3+LinesWidth,RowH);
              if (J+LinesWidth+DefaultRowHeight>=ClientHeight) and (J>=2*DefaultRowHeight) then
                J:=J-DefaultRowHeight;
            end

            else
//До сих пор, нижняя закоментирована
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
        for I := 0 to High(Heights)-1 do
          RowHeights[I] := Heights[I];
      end;
    finally
      if RestoreCanvas then
      begin
        ReleaseDC(0,Canvas.Handle);
        Canvas.Handle := 0;
      end;
    end;
  end;

var
  I, J: Integer;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then Exit;

  if HandleAllocated then KillMessage(Handle, cm_DeferLayout);

  CheckForPassthroughs;
  FIndicatorOffset := 0;
  if dgIndicator in Options then
    Inc(FIndicatorOffset);
  FDatalink.ClearMapping;
  if FDatalink.Active then DefineFieldMap;
  DoubleBuffered := (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView;
  ResetColumnFieldBindings;
  FVisibleColumns.Clear;
  for I := 0 to FColumns.Count-1 do
    if FColumns[I].Showing then FVisibleColumns.Add(FColumns[I]);
  ColCount := FColumns.Count + FIndicatorOffset;
  inherited FixedCols := FIndicatorOffset;
  FTitleOffset := 0;
  if dgTitles in Options then
  begin
    FTitleOffset := 1;
    if (FDatalink <> nil) and (FDatalink.Dataset <> nil)
      and FDatalink.Dataset.ObjectView then
    begin
      for I := 0 to FColumns.Count-1 do
      begin
        if FColumns[I].Showing then
        begin
          J := FColumns[I].Depth;
          if J >= FTitleOffset then FTitleOffset := J+1;
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

procedure TCustomESDBGrid.LayoutChanged;
begin
  if AcquireLayoutLock then
    EndLayout;
end;

procedure TCustomESDBGrid.LinkActive(Value: Boolean);
var
  Comp: TComponent;
  I: Integer;
begin
  if not Value then HideEditor;
  FBookmarks.LinkActive(Value);
  try
    LayoutChanged;
  finally
    for I := ComponentCount-1 downto 0 do
    begin
      Comp := Components[I];   // Free all the popped-up subgrids
      if (Comp is TCustomESDBGrid)
        and (TCustomESDBGrid(Comp).DragKind = dkDock) then
        Comp.Free;
    end;
    UpdateScrollBar;
    if Value and (dgAlwaysShowEditor in Options) then ShowEditorMS;
  end;
end;

procedure TCustomESDBGrid.Loaded;
begin
  inherited Loaded;
  if FColumns.Count > 0 then
    ColCount := FColumns.Count;
  LayoutChanged;
  CalcOneRowHeight;
end;

function TCustomESDBGrid.PtInExpandButton(X,Y: Integer; var MasterCol: TColumn): Boolean;
var
  Cell: TGridCoord;
  R: TRect;
begin
  MasterCol := nil;
  Result := False;
  Cell := MouseCoord(X,Y);
  if (Cell.Y < FTitleOffset) and FDatalink.Active
    and (Cell.X >= FIndicatorOffset)
    and (RawToDataColumn(Cell.X) < FColumns.Count) then
  begin
    R := CalcTitleRect(FColumns[RawToDataColumn(Cell.X)], Cell.Y, MasterCol);
    if not UseRightToLeftAlignment then
      R.Left := R.Right - GetSystemMetrics(SM_CXHSCROLL)
    else
      R.Right := R.Left + GetSystemMetrics(SM_CXHSCROLL);
    Result := MasterCol.Expandable and PtInRect(R, Point(X,Y));
  end;
end;

procedure TCustomESDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  Xc, Yc: Integer);
var
  Cell: TGridCoord;
  OldCol,OldRow: Integer;
  MasterCol: TColumn;
  lBookMark: TBookmark;      //Добавлено
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;

  if Sizing(Xc, Yc) then
  begin
    FDatalink.UpdateData;
    inherited MouseDown(Button, Shift, Xc, Yc);
    Exit;
  end;

  Cell := MouseCoord(Xc, Yc);
  if (Cell.X < 0) and (Cell.Y < 0) then
  begin
    inherited MouseDown(Button, Shift, Xc, Yc);
    Exit;
  end;

  if (DragKind = dkDock) and (Cell.X < FIndicatorOffset) and
    (Cell.Y < FTitleOffset) and (not (csDesigning in ComponentState)) then
  begin
    BeginDrag(false);
    Exit;
  end;

  if PtInExpandButton(Xc,Yc, MasterCol) then
  begin
    MasterCol.Expanded := not MasterCol.Expanded;
    ReleaseCapture;
    UpdateDesigner;
    Exit;
  end;

  if ((csDesigning in ComponentState) or (dgColumnResize in Options)) and
    (Cell.Y < FTitleOffset) then
  begin
    FDataLink.UpdateData;
    inherited MouseDown(Button, Shift, Xc, Yc);
    Exit;
  end;

  if FDatalink.Active then
    with Cell do
    begin
       BeginUpdate;   { eliminates highlight flicker when selection moves }
      try
        FDatalink.UpdateData; // validate before moving
        HideEditor;
        OldCol := Col;
        OldRow := Row;
        if (Button = mbLeft) and (dgMultiSelect in Options) and (ssShift in Shift) and (Cell.Y=FBotPartVisRow) then //Добавлено
           FBookmarks.CurrentRowSelected := not FBookmarks.CurrentRowSelected;
        if (Y >= FTitleOffset) and (Y - Row <> 0) then
            FDatalink.MoveBy(Y - Row);
        if X >= FIndicatorOffset then
          MoveCol(X, 0);
        if (Button = mbLeft) and (dgMultiSelect in Options) {and FDatalink.Active} then
          with FBookmarks do
          begin
            FSelecting := False;
            if ssCtrl in Shift then
              CurrentRowSelected := not CurrentRowSelected
            else
              if ssShift in Shift then          //Добавлено
              begin
                lBookMark:=FDataLink.DataSet.GetBookmark;  //Запоминаем запись
                if FDataLink.ActiveRecord+1 > OldRow then
                begin
                  while FDataLink.ActiveRecord+1>=OldRow do
                  begin
                    CurrentRowSelected := True;
                    FDataLink.DataSet.Prior;
                    if (FDataLink.ActiveRecord=0) and (OldRow=1) then
                    begin
                      CurrentRowSelected := True;
                      exit;
                    end;
                  end;
                end
                else
                begin
                  while FDataLink.ActiveRecord+1<OldRow do
                  begin
                    CurrentRowSelected := True;
                    FDataLink.DataSet.Next;
                  end;
                  CurrentRowSelected := True;
                end;
                FDataLink.DataSet.GotoBookmark(lBookMark);
                FDataLink.DataSet.FreeBookmark(lBookMark);
              end
              else                              //до сих пор
              begin
                Clear;
//                CurrentRowSelected := True;
              end;
          end;
        if (Button = mbLeft) and
          (((X = OldCol) and (Y = OldRow)) or (dgAlwaysShowEditor in Options)) then
          begin
            FMousePoint.X:=Xc;
            FMousePoint.Y:=Yc;
            ShowEditorMS;         { put grid in edit mode }
            FMousePoint.X:=-1;
            FMousePoint.Y:=-1;
          end
        else
          InvalidateEditor;  { draw editor, if needed }
      finally
        EndUpdate;
      end;
    end;
end;

procedure TCustomESDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  SaveState: TGridState;
begin
  SaveState := FGridState;
  inherited MouseUp(Button, Shift, X, Y);
  if (SaveState = gsRowSizing) or (SaveState = gsColSizing) or
    ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
     (PtInRect(InplaceEditor.BoundsRect, Point(X,Y)))) then Exit;
  Cell := MouseCoord(X,Y);
  if (Button = mbLeft) and (Cell.X >= FIndicatorOffset) and (Cell.Y >= 0) then
    if Cell.Y < FTitleOffset then
      TitleClick(FColumns[RawToDataColumn(Cell.X)])
    else
      CellClick(FColumns[SelectedIndex]);
end;

procedure TCustomESDBGrid.MoveCol(RawCol, Direction: Integer);
var
  OldCol: Integer;
begin
  FDatalink.UpdateData;
  if RawCol >= ColCount then
    RawCol := ColCount - 1;
  if RawCol < FIndicatorOffset then RawCol := FIndicatorOffset;
  if Direction <> 0 then
  begin
    while (RawCol < ColCount) and (RawCol >= FIndicatorOffset) and
      (ColWidths[RawCol] <= 0) do
      Inc(RawCol, Direction);
    if (RawCol >= ColCount) or (RawCol < FIndicatorOffset) then Exit;
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
    if not (dgAlwaysShowEditor in Options) then
      HideEditor;
    Col := RawCol;
    if (dgAlwaysShowEditor in Options) then
    begin
      if TDBGridInplaceEdit(InplaceEditor).WantRecreateWnd then
      begin
        TDBGridInplaceEdit(InplaceEditor).RecreateWnd;
        InvalidateEditor;
      end;
      TDBGridInplaceEdit(InplaceEditor).NeedScrollBars;
    end;
    ColEnter;
  end;
end;

procedure TCustomESDBGrid.Notification(AComponent: TComponent;
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
      for I := 0 to FColumns.Count-1 do
        if FColumns[I].PopupMenu = AComponent then
          FColumns[I].PopupMenu := nil;
    end
    else if (FDataLink <> nil) then
      if (AComponent = DataSource)  then
        DataSource := nil
      else if (AComponent is TField) then
      begin
        NeedLayout := False;
        BeginLayout;
        try
          for I := 0 to FColumns.Count-1 do
            with FColumns[I] do
              if Field = AComponent then
              begin
                Field := nil;
                NeedLayout := True;
              end;
        finally
          if NeedLayout and Assigned(FDatalink.Dataset)
            and not (csDestroying in FDatalink.DataSet.ComponentState)
            and not FDatalink.Dataset.ControlsDisabled then
            EndLayout
          else
            DeferLayout;
        end;
      end;
  end;
end;

procedure TCustomESDBGrid.RecordChanged(Field: TField);
var
  I: Integer;
  CField: TField;
begin
  if not HandleAllocated then Exit;
  if Field = nil then
    Invalidate
  else
  begin
    for I := 0 to FColumns.Count - 1 do
      if FColumns[I].Field = Field then
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

procedure TCustomESDBGrid.Scroll(Distance: Integer);
var
  OldRect, NewRect: TRect;
  RowHeight: Integer;
begin
  if not HandleAllocated then Exit;
  if not FDataLink.FScrollPocessed then                   //??
  begin                                                   //
    OldRect := BoxRect(0, Row, ColCount - 1, Row);
    if (FDataLink.ActiveRecord >= RowCount - FTitleOffset) then
      UpdateRowCount;
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
          RowHeight :=RowHeights[Row];                //??? было DefaultRowHeight;
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
        if dgAlwaysShowEditor in Options then ShowEditorMS;
      end;
    end;
    if UpdateLock = 0 then Update;
  end;                                                    //??
end;

procedure TCustomESDBGrid.SetColumns(Value: TDBGridColumns);
begin
  FColumns.Assign(Value);
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

procedure TCustomESDBGrid.SetColumnAttributes;
var
  I: Integer;
begin
  for I := 0 to FColumns.Count-1 do
  with FColumns[I] do
  begin
    TabStops[I + FIndicatorOffset] := Showing and not ReadOnly and DataLink.Active and
      Assigned(Field) and not (Field.FieldKind = fkCalculated) and not ReadOnlyField(Field);
    ColWidths[I + FIndicatorOffset] := Width;
  end;
  if (dgIndicator in Options) then
    ColWidths[0] := IndicatorWidth;
end;

procedure TCustomESDBGrid.SetDataSource(Value: TDataSource);
begin
  if Value = FDatalink.Datasource then Exit;
  if Assigned(Value) then
  begin
    if Assigned(Value.DataSet) then
      if Value.DataSet.IsUnidirectional then
        DatabaseError(SDataSetUnidirectional);
  end;
  FBookmarks.Clear;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TCustomESDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  FEditText := Value;
end;

procedure TCustomESDBGrid.SetOptions(Value: TDBGridOptions);
const
  LayoutOptions = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator,
    dgColLines, dgRowLines, dgRowSelect, dgAlwaysShowSelection];
//  LayoutOptions =[ dgEditing, dgAlwaysShowEditor, dgTitles,
//    dgIndicator, dgColumnResize, dgColLines, dgColMoving,
//    dgRowLines, dgRowSelect, dgTabs, dgAlwaysShowSelection,
//    dgDrawFocusSelected, dgConfirmDelete, dgCancelOnExit,
//    dgMultiSelect ]

var
  NewGridOptions: TGridOptions;
  ChangedOptions: TDBGridOptions;
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
    if dgRowSelect in Value then
    begin
      Include(NewGridOptions, goRowSelect);
      Exclude(Value, dgAlwaysShowEditor);
      Exclude(Value, dgEditing);
    end;
    if dgEditing in Value then Include(NewGridOptions, goEditing);
    if dgAlwaysShowEditor in Value then Include(NewGridOptions, goAlwaysShowEditor);
    if dgColMoving in Value then
      NewGridOptions:=NewGridOptions + [goColMoving]
    else
      NewGridOptions:=NewGridOptions - [goColMoving];
    inherited Options := NewGridOptions;
    if dgMultiSelect in (FOptions - Value) then FBookmarks.Clear;
    ChangedOptions := (FOptions + Value) - (FOptions * Value);
    FOptions := Value;
    if ChangedOptions * LayoutOptions <> [] then LayoutChanged;
    if (dgRowLines in ChangedOptions) then
      CalcOneRowHeight;
    if (dgTitles in ChangedOptions) then
    begin
      FDataLink.FScrollPocessed:=True;
      if FDataLink.Active then
        if Assigned(DataSource) then
          if Assigned(DataSource.DataSet) then
          begin
            BeginUpdate;
            try
              FDataLink.FScrollPocessed:=True;
              CalcBuffer;
              FDataLink.FScrollPocessed:=False;
            finally
              EndUpdate;
            end;
          end;
      UpdateRowCount;
      FDataLink.FScrollPocessed:=False;
      UpdateScrollBar;
    end;
  end;
end;

procedure TCustomESDBGrid.SetSelectedField(Value: TField);
var
  I: Integer;
begin
  if Value = nil then Exit;
  for I := 0 to FColumns.Count - 1 do
    if FColumns[I].Field = Value then
      MoveCol(DataToRawColumn(I), 0);
end;

procedure TCustomESDBGrid.SetSelectedIndex(Value: Integer);
begin
  MoveCol(DataToRawColumn(Value), 0);
end;

procedure TCustomESDBGrid.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
  if dgTitles in Options then LayoutChanged;
end;

function TCustomESDBGrid.StoreColumns: Boolean;
begin
  Result := FColumns.State = csCustomized;
end;

procedure TCustomESDBGrid.TimedScroll(Direction: TGridScrollDirection);
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

procedure TCustomESDBGrid.TitleClick(Column: TColumn);
begin
  if Assigned(FOnTitleClick) then FOnTitleClick(Column);
end;

procedure TCustomESDBGrid.TitleFontChanged(Sender: TObject);
begin
  if (not FSelfChangingTitleFont) and not (csLoading in ComponentState) then
    ParentFont := False;
  if dgTitles in Options then LayoutChanged;
end;

procedure TCustomESDBGrid.UpdateActive;
var
  NewRow: Integer;
  Field: TField;
begin
  if FDatalink.Active and HandleAllocated and not (csLoading in ComponentState) then
  begin
    NewRow := FDatalink.ActiveRecord + FTitleOffset;
    if Row <> NewRow then
    begin
      if not (dgAlwaysShowEditor in Options) then
        HideEditor;
      MoveColRow(Col, NewRow, False, False);
      InvalidateEditor;
    end;
    Field := SelectedField;
    if Assigned(Field) and (Field.Text <> FEditText) then
      InvalidateEditor;
  end;
end;

procedure TCustomESDBGrid.UpdateData;
var
  Field: TField;
begin
  Field := SelectedField;
  if Assigned(Field) then
    Field.Text := FEditText;
end;

procedure TCustomESDBGrid.UpdateRowCount;
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
      FDataLink.BufferCount := GetVisibleRowCount;        //Переписать загянув сюда
      RowCount := RecordCount + FTitleOffset;
      UpdateActive;
    end;
  if OldRowCount <> RowCount then Invalidate;
end;

procedure TCustomESDBGrid.UpdateScrollBar;
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
          SINew.nPos := RecNo;  // else keep old pos
      end
      else
      begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        SINew.nMax := 4;
        if FDataLink.BOF then SINew.nPos := 0
        else if FDataLink.EOF then SINew.nPos := 4
        else SINew.nPos := 2;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
end;

function TCustomESDBGrid.ValidFieldIndex(FieldIndex: Integer): Boolean;
begin
  Result := DataLink.GetMappedIndex(FieldIndex) >= 0;
end;

procedure TCustomESDBGrid.CMParentFontChanged(var Message: TMessage);
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

procedure TCustomESDBGrid.CMBiDiModeChanged(var Message: TMessage);
var
  Loop: Integer;
begin
  inherited;
  for Loop := 0 to ComponentCount - 1 do
    if Components[Loop] is TCustomESDBGrid then
      with Components[Loop] as TCustomESDBGrid do
        { Changing the window, echos down to the subgrid }
        if Parent <> nil then
          Parent.BiDiMode := Self.BiDiMode;
end;

procedure TCustomESDBGrid.CMExit(var Message: TMessage);
begin
  try
    if FDatalink.Active then
      with FDatalink.Dataset do
        if (dgCancelOnExit in Options) and (State = dsInsert) and
          not Modified and not FDatalink.FModified then
          Cancel else
          FDataLink.UpdateData;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomESDBGrid.CMFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  BeginLayout;
  try
    for I := 0 to FColumns.Count-1 do
      FColumns[I].RefreshDefaultFont;
  finally
    EndLayout;
  end;
end;

procedure TCustomESDBGrid.CMDeferLayout(var Message);
begin
  if AcquireLayoutLock then
    EndLayout
  else
    DeferLayout;
end;

procedure TCustomESDBGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  MasterCol: TColumn;
begin
  inherited;
  if (Msg.Result = 1) and ((FDataLink = nil) or
    ((FColumns.State = csDefault) and
     (FDataLink.DefaultFields or (not FDataLink.Active)))) then
    Msg.Result := 0
  else if (Msg.Result = 0) and (FDataLink <> nil) and (FDataLink.Active)
    and (FColumns.State = csCustomized)
    and PtInExpandButton(Msg.XPos, Msg.YPos, MasterCol) then
    Msg.Result := 1;
end;

procedure TCustomESDBGrid.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (csDesigning in ComponentState) and
      ((FDataLink = nil) or
       ((FColumns.State = csDefault) and
        (FDataLink.DefaultFields or not FDataLink.Active))) then
    Windows.SetCursor(LoadCursor(0, IDC_ARROW))
  else inherited;
end;

procedure TCustomESDBGrid.WMSize(var Message: TWMSize);
var
  LinesWidth: Integer;
begin
  inherited;
  if dgRowLines in Options then
    LinesWidth:=GridLineWidth
  else
    LinesWidth:=0;
  if dgTitles in FOptions then
  begin
    if Height<RowHeights[0]+DefaultRowHeight+2*LinesWidth then
      Height:=RowHeights[0]+DefaultRowHeight+2*LinesWidth;
  end
  else
    if Height<(DefaultRowHeight+LinesWidth)*2 then
      Height:=(DefaultRowHeight+LinesWidth)*2;
  if UpdateLock = 0 then
  begin
    FDataLink.FScrollPocessed:=True;
    if FDataLink.Active then
      if Assigned(DataSource) then
        if Assigned(DataSource.DataSet) then
        begin
          BeginUpdate;
          try
            FDataLink.ActiveRecord:=0;
            FDataLink.FScrollPocessed:=True;
            CalcBuffer;
       FDataLink.DataSet.UpdateCursorPos;          //Обязятельно после
       FDataLink.DataSet.Resync([]);       //CalcBuffer
            UpdateRowCount;
            FDataLink.ActiveRecord:=0;
            FDataLink.FScrollPocessed:=False;
          finally
            EndUpdate;
          end;
        end;
    FDataLink.FScrollPocessed:=False;
    UpdateScrollBar;
  end;
  InvalidateTitles;
end;

procedure TCustomESDBGrid.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  if not AcquireFocus then Exit;
  if FDatalink.Active then
    with Message, FDataLink.DataSet do
      case ScrollCode of
        SB_LINEUP:                    //Изменено
          MoveBy(-1);
        SB_LINEDOWN:                            //Изменено
          MoveBy(1);
        SB_PAGEUP: PrevPage;                             //Изменено
        SB_PAGEDOWN: NextPage;                           //Изменено
        SB_THUMBPOSITION:
          begin
            if IsSequenced then
            begin
              SI.cbSize := sizeof(SI);
              SI.fMask := SIF_ALL;
              GetScrollInfo(Self.Handle, SB_VERT, SI);
              if SI.nTrackPos <= 1 then
                First
              else
                if SI.nTrackPos >= RecordCount then
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

procedure TCustomESDBGrid.SetIme;
var
  Column: TColumn;
begin
  if not SysLocale.FarEast then Exit;
  if FColumns.Count = 0 then Exit;

  ImeName := FOriginalImeName;
  ImeMode := FOriginalImeMode;
  Column := FColumns[SelectedIndex];
  if Column.IsImeNameStored then ImeName := Column.ImeName;
  if Column.IsImeModeStored then ImeMode := Column.ImeMode;

  if InplaceEditor <> nil then
  begin
    TDBGridInplaceEdit(InplaceEditor).ImeName := ImeName;
    TDBGridInplaceEdit(InplaceEditor).ImeMode := ImeMode;
  end;
end;

procedure TCustomESDBGrid.UpdateIme;
begin
  if not SysLocale.FarEast then Exit;
  SetIme;
  SetImeName(ImeName);
  SetImeMode(Handle, ImeMode);
end;

procedure TCustomESDBGrid.WMIMEStartComp(var Message: TMessage);
begin
  inherited;
  ShowEditorMS;
end;

procedure TCustomESDBGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  if not ((InplaceEditor <> nil) and
    (Message.FocusedWnd = InplaceEditor.Handle)) then SetIme;
  inherited;
end;

procedure TCustomESDBGrid.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then inherited
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

function TCustomESDBGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.ExecuteAction(Action);
end;

function TCustomESDBGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil) and DataLink.UpdateAction(Action);
end;

procedure TCustomESDBGrid.ShowPopupEditor(Column: TColumn; X, Y: Integer);
var
  SubGrid: TCustomESDBGrid;
  DS: TDataSource;
  I: Integer;
  FloatRect: TRect;
  Cmp: TControl;
begin
  if not ((Column.Field <> nil) and (Column.Field is TDataSetField)) then  Exit;

  // find existing popup for this column field, if any, and show it
  for I := 0 to ComponentCount-1 do
    if Components[I] is TCustomESDBGrid then
    begin
      SubGrid := TCustomESDBGrid(Components[I]);
      if (SubGrid.DataSource <> nil) and
        (SubGrid.DataSource.DataSet = (Column.Field as TDatasetField).NestedDataset) and
        SubGrid.CanFocus then
      begin
        SubGrid.Parent.Show;
        SubGrid.SetFocus;
        Exit;
      end;
    end;

  // create another instance of this kind of grid
  SubGrid := TCustomESDBGrid(TComponentClass(Self.ClassType).Create(Self));
  try
    DS := TDataSource.Create(SubGrid); // incestuous, but easy cleanup
    DS.Dataset := (Column.Field as TDatasetField).NestedDataset;
    DS.DataSet.CheckBrowseMode;
    SubGrid.DataSource := DS;
    SubGrid.FColumns.State := FColumns.State;
    SubGrid.FColumns[0].Expanded := True;
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
    I := SubGrid.CellRect(SubGrid.ColCount-1, 0).Right;
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
    while (Cmp <> nil) and (TCustomESDBGrid(Cmp).PopupMenu = nil) do
      Cmp := Cmp.Parent;
    if Cmp <> nil then
      SubGrid.PopupMenu := TCustomESDBGrid(Cmp).PopupMenu;
    SubGrid.TitleFont := TitleFont;
    SubGrid.Visible := True;
    SubGrid.Parent.Show;
  except
    SubGrid.Free;
    raise;
  end;
end;

procedure TCustomESDBGrid.CalcSizingState(X, Y: Integer;
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
    if (R.X >= 0) and (R.X < FColumns.Count) and (FColumns[R.X].Depth > R.Y) then
      State := gsNormal;
  end;
end;

function TCustomESDBGrid.CheckColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
  DestCol: TColumn;
begin
  Result := inherited CheckColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    assert(FDragCol <> nil);
    ARow := FDragCol.Depth;
    if Destination <> Origin then
    begin
      DestCol := ColumnAtDepth(FColumns[RawToDataColumn(Destination)], ARow);
      if DestCol.ParentColumn <> FDragCol.ParentColumn then
        if Destination < Origin then
          DestCol := FColumns[FDragCol.ParentColumn.Index+1]
        else
        begin
          I := DestCol.Index;
          while DestCol.ParentColumn <> FDragCol.ParentColumn do
          begin
            Dec(I);
            DestCol := FColumns[I];
          end;
        end;
      if (DestCol.Index > FDragCol.Index) then
      begin
        I := DestCol.Index + 1;
        while (I < FColumns.Count) and (ColumnAtDepth(FColumns[I],ARow) = DestCol) do
          Inc(I);
        DestCol := FColumns[I-1];
      end;
      Destination := DataToRawColumn(DestCol.Index);
    end;
  end;
end;

function TCustomESDBGrid.BeginColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
var
  I, ARow: Integer;
begin
  Result := inherited BeginColumnDrag(Origin, Destination, MousePt);
  if Result and (FDatalink.Dataset <> nil) and FDatalink.Dataset.ObjectView then
  begin
    ARow := MouseCoord(MousePt.X, MousePt.Y).Y;
    FDragCol := ColumnAtDepth(FColumns[RawToDataColumn(Origin)], ARow);
    if FDragCol = nil then Exit;
    I := DataToRawColumn(FDragCol.Index);
    if Origin <> I then Origin := I;
    Destination := Origin;
  end;
end;

function TCustomESDBGrid.EndColumnDrag(var Origin, Destination: Integer;
  const MousePt: TPoint): Boolean;
begin
  Result := inherited EndColumnDrag(Origin, Destination, MousePt);
  FDragCol := nil;
end;

procedure TCustomESDBGrid.InvalidateTitles;
var
  R: TRect;
  DrawInfo: TGridDrawInfo;
begin
  if HandleAllocated and (dgTitles in Options) then
  begin
    CalcFixedInfo(DrawInfo);
    R := Rect(0, 0, Width, DrawInfo.Vert.FixedBoundary);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCustomESDBGrid.TopLeftChanged;
begin
  InvalidateTitles;
  inherited TopLeftChanged;
end;

procedure TCustomESDBGrid.ShowEditorMS;
begin
//  FDataLink.FEdited:=True;
  ShowEditor;
  if (InplaceEditor<>nil) then     //Обязательно проверять
  begin
    with TDBGridInplaceEdit(InplaceEditor) do
    begin
      if WantRecreateWnd then
      begin
        RecreateWnd;
        FGrid.InvalidateEditor;
        if (FMousePoint.X<>-1) and (EditStyle=esSimple) and not FAutoSelect then
          SetCaretPosition
        else
          if EditStyle<>esSimple then
            SelectAll;
      end;
      FMousePoint.X:=-1;
      FMousePoint.Y:=-1;
      NeedScrollBars;
    end;
  end;
end;

procedure TCustomESDBGrid.WMComand(var Message: TMessage);
var
  NeedLineHeight: Integer;
  sz: SIZE;
  WndDC: HDC;
  NumLines: Integer;
begin
  inherited;
  if FMemoAllowed and FColumns[SelectedIndex].Memo then
    if Message.WParamHi=EN_CHANGE then
      if InplaceEditor<>nil then
        if Integer(Message.LParam)=Integer(InplaceEditor.Handle) then
        begin
          with TDBGridInplaceEdit(InplaceEditor) do
          begin
            WndDC:=GetDC(Handle);
            SelectObject(WndDC, Font.Handle);
            GetTextExtentPoint32(WndDC,'Ж',1,sz);
            Windows.ReleaseDC(Handle,WndDC);
            NumLines:=NeedScrollBars;
          end;
          NeedLineHeight:=sz.cy*NumLines+4;
          if (NumLines=1) and (NeedLineHeight<DefaultRowHeight) then
            NeedLineHeight:=DefaultRowHeight;
          if NeedLineHeight<>RowHeights[Row] then
          begin
            RowHeights[Row]:=NeedLineHeight;
            Invalidate;
          end;
        end;
end;

procedure TCustomESDBGrid.SetMemoAllowed(const Value: Boolean);
begin
  FMemoAllowed := Value;
end;

procedure TCustomESDBGrid.SetMemoMaxRows(const Value: Integer);
begin
  FMemoMaxRows := Value;
end;

procedure TCustomESDBGrid.SetMemoOptions(const Value: TMemoOptions);
begin
  if FMemoOptions<>Value then
  begin
    FMemoOptions:=Value;
    if InplaceEditor<>nil then
      if TDBGridInplaceEdit(InplaceEditor).WantRecreateWnd then
      begin
         TDBGridInplaceEdit(InplaceEditor).RecreateWnd;
         TDBGridInplaceEdit(InplaceEditor).NeedScrollBars;
      end;
  end;
end;


{ TMemoStrings }

type

  TMemoStrings = class(TStrings)
  private
    Memo: TCustomMemo;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetTextStr: string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;


function TMemoStrings.GetCount: Integer;
begin
  Result := 0;
  if Memo.HandleAllocated or (TInplaceEditMS(Memo).WindowText <> nil) then
  begin
    Result := SendMessage(Memo.Handle, EM_GETLINECOUNT, 0, 0);
    if SendMessage(Memo.Handle, EM_LINELENGTH, SendMessage(Memo.Handle,
      EM_LINEINDEX, Result - 1, 0), 0) = 0 then Dec(Result);
  end;
end;

function TMemoStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
begin
  Word((@Text)^) := SizeOf(Text);
  SetString(Result, Text, SendMessage(Memo.Handle, EM_GETLINE, Index,
    Longint(@Text)));
end;

procedure TMemoStrings.Put(Index: Integer; const S: string);
var
  SelStart: Integer;
begin
  SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
  if SelStart >= 0 then
  begin
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelStart +
      SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0));
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(PChar(S)));
  end;
end;

procedure TMemoStrings.Insert(Index: Integer; const S: string);
var
  SelStart, LineLen: Integer;
  Line: string;
begin
  if Index >= 0 then
  begin
    SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
    if SelStart >= 0 then Line := S + #13#10 else
    begin
      SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index - 1, 0);
      if SelStart < 0 then Exit;
      LineLen := SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0);
      if LineLen = 0 then Exit;
      Inc(SelStart, LineLen);
      Line := #13#10 + s;
    end;
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelStart);
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(PChar(Line)));
  end;
end;

procedure TMemoStrings.Delete(Index: Integer);
const
  Empty: PChar = '';
var
  SelStart, SelEnd: Integer;
begin
  SelStart := SendMessage(Memo.Handle, EM_LINEINDEX, Index, 0);
  if SelStart >= 0 then
  begin
    SelEnd := SendMessage(Memo.Handle, EM_LINEINDEX, Index + 1, 0);
    if SelEnd < 0 then SelEnd := SelStart +
      SendMessage(Memo.Handle, EM_LINELENGTH, SelStart, 0);
    SendMessage(Memo.Handle, EM_SETSEL, SelStart, SelEnd);
    SendMessage(Memo.Handle, EM_REPLACESEL, 0, Longint(Empty));
  end;
end;

procedure TMemoStrings.Clear;
begin
  Memo.Clear;
end;

procedure TMemoStrings.SetUpdateState(Updating: Boolean);
begin
  if Memo.HandleAllocated then
  begin
    SendMessage(Memo.Handle, WM_SETREDRAW, Ord(not Updating), 0);
    if not Updating then
    begin   // WM_SETREDRAW causes visibility side effects in memo controls
      Memo.Perform(CM_SHOWINGCHANGED,0,0); // This reasserts the visibility we want
      Memo.Refresh;
    end;
  end;
end;

function TMemoStrings.GetTextStr: string;
begin
  Result := Memo.Text;
end;

procedure TMemoStrings.SetTextStr(const Value: string);
var
  NewText: string;
begin
  NewText := AdjustLineBreaks(Value);
  if (Length(NewText) <> Memo.GetTextLen) or (NewText <> Memo.Text) then
  begin
    if SendMessage(Memo.Handle, WM_SETTEXT, 0, Longint(NewText)) = 0 then
      raise EInvalidOperation.Create(SInvalidMemoSize);
    Memo.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

function TCustomESDBGrid.GetVisibleRowCount: Integer;   //Добавлено override
var
//  DrawInfo: TGridDrawInfo;
  i: Integer;
  RowsHeight: Integer;
  OneHeight: Integer;
  ClientH: Integer;
begin
{  CalcDrawInfo(DrawInfo);
  With DrawInfo.Vert do
  begin
    Result := LastFullVisibleCell - TopRow + 1;
    if (GridExtent>FullVisBoundary) and (FullVisBoundary<>FixedBoundary)
       and not (FLastRowFull) then
    begin
      Result:=Result+1;
      FBotPartVisRow:=Result;
    end
    else
    begin
//      if FLastRowFull then
//        Result:=Result+FTitleOffset-1;
      if (FixedBoundary=FullVisBoundary) and not (FLastRowFull) then
        FBotPartVisRow:=Result
      else
        FBotPartVisRow:=-1;
    end;
  end;}
  ClientH:=ClientHeight;
  if FDataLink<>nil then
  begin
    i:=-1;
    RowsHeight:=0;
    with FDataLink do
      while i<BufferCount+FTitleOffset-1 do
      begin
        i:=i+1;
        OneHeight:=RowHeights[i];
        if (dgRowLines in FOptions) then
          OneHeight:=OneHeight+GridLineWidth;
        RowsHeight:=RowsHeight+OneHeight;
        if (RowsHeight>=ClientH) then
          Break;
      end;
    Result:=i; //-TopRow+1;
    if RowsHeight>ClientH then
      FBotPartVisRow:=Result
    else
      FBotPartVisRow:=-1;
  end
  else
  begin
    Result:=FTitleOffset+1;
    if GridHeight>ClientH then
      FBotPartVisRow:=Result
    else
      FBotPartVisRow:=-1;
  end;
end;

procedure TCustomESDBGrid.SetAutoSelect(const Value: Boolean);
begin
  if FAutoSelect<>Value then
  begin
    FAutoSelect:=Value;
    if InplaceEditor<>nil then
    begin
      with TInplaceEditMS(InplaceEditor) do
      begin
        AutoSelect:=Value;
        if Value then
          SelectAll
        else
          SelLength:=0;
      end;
    end;
  end;
end;


function TCustomESDBGrid.CalcRowHeight(ARow: Integer): Integer;
var
  i, j: Integer;
  OldRecord: Integer;
  dc: HDC;
  Rect: TRect;
  uFormat: Integer;
  LinesWidth: Integer;
  TitleHeight: Integer;
  s: string;
begin
  if FMemoAllowed and (FMemoMaxRows<>1) then
  begin
    Result:=DefaultRowHeight;
    Rect.Top:=0;
    Rect.Left:=0;
    if ARow>-1 then
    begin
      OldRecord:=FDataLink.ActiveRecord;
      FDataLink.ActiveRecord:=ARow;
    end;
    dc:=Canvas.Handle;
    if dgRowLines in Options then
      LinesWidth:=GridLineWidth
    else
      LinesWidth:=0;
    for i:=0 to FColumns.Count-1 do
    begin
      with FColumns[i] do
      begin
        if FMemo then
        begin
          if FField.AsString<>'' then
          begin
            uFormat:=DT_WORDBREAK or DT_CALCRECT;
            Rect.Right:=FWidth-2*LinesWidth-2;
            SelectObject(dc, FFont.Handle);
            Result:=Max(DrawText(dc,PChar(FField.AsString),-1,Rect,uFormat)+3+LinesWidth,Result);
            if FMemoMaxRows>0 then
            begin
              s:='';
              for j:=1 to FMemoMaxRows do
                s:=s+'Жр'+#13;
              s:=LeftStr(s,StrLen(PChar(s))-1);
              j:=DrawText(dc,PChar(s),-1,Rect,uFormat)+3+LinesWidth;
              if j<Result then
                Result:=j;
            end;
          end;
        end;
      end;
    end;
    if Result=FOneRowHeight then
      Result:=DefaultRowHeight;
    if FTitleOffset>0 then
      TitleHeight:=RowHeights[0]+LinesWidth
    else
      TitleHeight:=0;
    TitleHeight:=ClientHeight-TitleHeight-LinesWidth;
    if Result+LinesWidth>=TitleHeight then
      Result:=TitleHeight-1;
    if ARow>-1 then
      FDataLink.ActiveRecord:=OldRecord;
  end
  else
    Result:=DefaultRowHeight;
end;

procedure TCustomESDBGrid.CalcBuffer(DefRowHeight: Integer);
var
  OneHeight: Integer;
  RowsHeight: Integer;
  i: Integer;
  OldRecord: Integer;
  ClientH: Integer;
begin
  with FDataLink do
  begin
    DataSet.DisableControls;
    OldRecord:=ActiveRecord;
    RowsHeight:=0;
    if FTitleOffset>0 then
      RowsHeight:=RowHeights[0];
    if (dgRowLines in FOptions) then
      RowsHeight:=RowsHeight+GridLineWidth;
    i:=FTitleOffset;
    ActiveRecord:=0;
    BufferCount:=1;
    RowCount:=FTitleOffset+1;
    ClientH:=ClientHeight;
    while (RowsHeight<ClientH) do
    begin
      if i<>DefRowHeight then             //Эта примочка для Insert, возможно уберется
        OneHeight:=CalcRowHeight
      else
        OneHeight:=DefaultRowHeight;
      RowHeights[i]:=OneHeight;
      if (dgRowLines in FOptions) then
        OneHeight:=OneHeight+GridLineWidth;
      RowsHeight:=RowsHeight+OneHeight;
      if RowsHeight>=ClientH then
        break
      else
      begin
        BufferCount:=BufferCount+1;
        RowCount:=RowCount+1;
        if (i=DefRowHeight) and (ActiveRecord<>OldRecord) then
          OldRecord:=ActiveRecord;
        if i<>DefRowHeight then
          MoveBy(1);
        if Eof then
          Break;
        i:=i+1;
      end;
    end;
    if Eof then
    begin
      MoveBy(-MoveBy(-1));
      BufferCount:=BufferCount-1;
      RowCount:=RowCount-1;
      if RowsHeight<=ClientHeight then
      begin
        FLastRowFull:=True;
        FBotPartVisRow:=-1;
      end
      else
        FBotPartVisRow:=i-FTitleOffset+1;
    end
    else
    begin
      FLastRowFull:=False;
      if RowsHeight=ClientHeight then
        FBotPartVisRow:=-1
      else
        FBotPartVisRow:=i-FTitleOffset+1;
    end;
    DataSet.EnableControls;
    ActiveRecord:=OldRecord;
  end;
end;


function TCustomESDBGrid.GetDefaultRowHeight: integer;
begin
  Result:=inherited DefaultRowHeight;
end;

procedure TCustomESDBGrid.SetDefaultRowHeight(const Value: integer);
begin
  BeginUpdate;
  try
    inherited DefaultRowHeight:=Value;
    if Assigned(FDataLink) and FDataLink.Active then
    begin
      FDataLink.FScrollPocessed:=True;
      CalcBuffer;
      FDataLink.FScrollPocessed:=False;
      UpdateRowCount;
//      InvalidateGrid;
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomESDBGrid.GetFont: TFont;
begin
  Result:=inherited Font;
end;

procedure TCustomESDBGrid.SetFont(const Value: TFont);
begin
  BeginUpdate;
  try
    inherited Font:=Value;
    if Assigned(FDataLink) and FDataLink.Active then
    begin
      FDataLink.FScrollPocessed:=True;
      CalcBuffer;
      FDataLink.FScrollPocessed:=False;
      UpdateRowCount;
    end;
    CalcOneRowHeight;
  finally
    EndUpdate;
  end;
end;

function TCustomESDBGrid.GetGridLineWidth: Integer;
begin
  Result:=inherited GridLineWidth;
end;

procedure TCustomESDBGrid.SetGridLineWidth(const Value: Integer);
begin
  inherited GridLineWidth:=Value;
  CalcOneRowHeight;
end;

procedure TCustomESDBGrid.CalcOneRowHeight;
var
  R: TRect;
  LinesWidth: Integer;
begin
  R.Top:=0;
  R.Left:=0;
  R.Bottom:=0;
  R.Right:=1024;
  if dgRowLines in Options then
    LinesWidth:=GridLineWidth
  else
    LinesWidth:=0;
  FOneRowHeight:=DrawText(Canvas.Handle,'Др',-1,R,DT_CALCRECT)+3+LinesWidth;
end;

procedure TCustomESDBGrid.NextPage;
var
  Distance: Integer;
begin
  if (FBotPartVisRow>=0) then
    Distance:=FBotPartVisRow-Row-1+FTitleOffset
  else
    if not FLastRowFull then
      Distance:=GetVisibleRowCount-Row+FTitleOffset
    else
    if Row+1<>RowCount then
      Distance:=GetVisibleRowCount-Row-1+FTitleOffset
    else
      Distance:=1;
  BeginUpdate;
  try
    with FDataLink do
    begin
      FScrollPocessed:=True;
      if FIsAppended<>0 then
      begin
        DataSet.UpdateCursorPos;
        DataSet.Resync([]);
        FIsAppended:=0;
        CalcBuffer;
        UpdateRowCount;
      end;
      MoveBy(Distance);
      if not FLastRowFull then
      begin
        MoveBy(-MoveBy(1));
        BufferCount:=1;
        RowCount:=1+FTitleOffset;
        if Eof then
        begin
          LastPosition;
          ActiveRecord:=0;
        end
        else
          CalcBuffer;
        DataSet.UpdateCursorPos;
        DataSet.Resync([]);
      end;
      UpdateRowCount;
      FScrollPocessed:=False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomESDBGrid.PrevPage;
var
  ClientH: Integer;
  RowsHeight: Integer;
  OneHeight: Integer;
begin
  with FDataLink do
  begin
    BeginUpdate;
    try
      FScrollPocessed:=True;
      if not Bof then
      begin
        if FIsAppended<>0 then
        begin
          DataSet.UpdateCursorPos;
          DataSet.Resync([]);
          FIsAppended:=0;
          CalcBuffer;
          UpdateRowCount;
        end;
        MoveBy(-1);
        if not Bof then
        begin
          ClientH:=ClientHeight;
          RowsHeight:=0;
          if FTitleOffset>0 then
          begin
            RowsHeight:=RowHeights[0];
            if (dgRowLines in FOptions) then
              RowsHeight:=RowsHeight+GridLineWidth;
          end;
          repeat
            OneHeight:=CalcRowHeight;
            if (dgRowLines in FOptions) then
              OneHeight:=OneHeight+GridLineWidth;
            RowsHeight:=RowsHeight+OneHeight;
            if RowsHeight>=ClientH then
            begin
              MoveBy(1);
              Break;
            end
            else
            begin
              MoveBy(-1);
              if Bof then
                Break;
            end;
          until Bof;
          ActiveRecord:=0;
          BufferCount:=1;
          RowCount:=1+FTitleOffset;
          CalcBuffer;
          DataSet.UpdateCursorPos;
          DataSet.Resync([]);
          UpdateRowCount;
        end;
      end;
      FScrollPocessed:=False;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomESDBGrid.SetTitleWordWrap(const Value: Boolean);
begin
  FTitleWordWrap := Value;
  FColumns.Changed;
end;

procedure TCustomESDBGrid.WMChar(var Msg: TWMChar);
begin
  if (goEditing in inherited Options) and (Char(Msg.CharCode) in [^H, #32..#255]) then
    if InplaceEditor<>nil then
      if TInplaceEditMS(InplaceEditor).WantRecreateWnd then
        TInplaceEditMS(InplaceEditor).RecreateWnd;
  inherited;
end;

procedure TCustomESDBGrid.SetSctiveFontColor(const Value: TColor);
begin
  FActiveFontColor := Value;
  InvalidateRow(Row);
end;

procedure TCustomESDBGrid.SetActiveColor(const Value: TColor);
begin
  FActiveColor := Value;
  InvalidateRow(Row);
end;

{ TInplaceEditMS }

constructor TInplaceEditMS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := False;
  FLines := TMemoStrings.Create;
  TMemoStrings(FLines).Memo := TCustomMemo(Self);
  FPreventSelection:=False;
end;

destructor TInplaceEditMS.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TInplaceEditMS.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD =
    ((ES_LEFT, ES_RIGHT, ES_CENTER),(ES_RIGHT, ES_LEFT, ES_CENTER));
//  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
//    WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
begin
  if (FGrid<>nil) and (FGrid.FColumns.Count>0) then   //
    WantRecreateWnd;
  inherited CreateParams(Params);
  if FWordWrap then
    Params.Style:=Params.Style and not WordWraps[FWordWrap] or ES_MULTILINE
       or Alignments[UseRightToLeftAlignment, FAlignment]
  else
    Params.Style := Params.Style or ES_MULTILINE
      or Alignments[UseRightToLeftAlignment, FAlignment];
end;


procedure TInplaceEditMS.CMShowingChanged(var Message: TMessage);
begin
  // Ignore showing using the Visible property
end;

procedure TInplaceEditMS.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;                    //Изменена для обработки клавиши Tab
  Message.Result := Message.Result or DLGC_WANTTAB and not DLGC_WANTARROWS;
end;

procedure TInplaceEditMS.WMPaste(var Message);
begin
  if not EditCanModify then Exit;
  inherited
end;

procedure TInplaceEditMS.WMClear(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEditMS.WMCut(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEditMS.DblClick;
begin
  FGrid.DblClick;
end;

function TInplaceEditMS.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := FGrid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TInplaceEditMS.EditCanModify: Boolean;
begin
  Result := FGrid.CanEditModify;
end;

procedure TInplaceEditMS.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    FGrid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := FGrid.OnKeyDown;
    if Assigned(GridKeyDown) then GridKeyDown(FGrid, Key, Shift);
  end;

  function ForwardMovement: Boolean;
  begin
    Result := TDBGridOption(goAlwaysShowEditor) in FGrid.Options;
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, Longint(@Result.StartPos), Longint(@Result.EndPos));
  end;

  function CaretPos: Integer;
  var
    P: TPoint;
  begin
    Windows.GetCaretPos(P);
    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
  end;

  function RightSide: Boolean;           //Изменено
  begin
    if FWordWrap then
      if moOnlyTabMove in FGrid.FMemoOptions then
        Result:=False
      else
        with Selection do
          Result := (Self.CaretPos.X = StrLen(PChar(Lines[Self.CaretPos.Y]))) and
            ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen)
    else
      with Selection do
        Result := (CaretPos = GetTextLen) and
          ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
  end;

  function LeftSide: Boolean;
  begin
    if FWordWrap then   //Изменено
    begin
      if moOnlyTabMove in FGrid.FMemoOptions then
        Result:=False
      else
        with Selection do
          Result := (CaretPos = 0) and (StartPos = 0) and
            ((EndPos = 0) or (EndPos = GetTextLen));
    end
    else
      with Selection do
        Result := (CaretPos = 0) and (StartPos = 0) and
          ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  case Key of
    VK_UP, VK_PRIOR:                    //Изменено
      with FGrid do
      begin
        if Self.FWordWrap then
          if moWantArrows in FMemoOptions then
          begin
            if (Self.CaretPos.Y=0) and not (moOnlyTabMove in FMemoOptions) then
              SendToParent;
          end
          else
            SendToParent
        else
          SendToParent;
      end;
    VK_DOWN, VK_NEXT:                    //Изменено
      with FGrid do
      begin
        if Self.FWordWrap then
          if moWantArrows in FMemoOptions then
          begin
            if (Self.CaretPos.Y=Lines.Count-1) and not (moOnlyTabMove in FMemoOptions) then
              SendToParent;
          end
          else
            SendToParent
        else
          SendToParent;
      end;
    VK_ESCAPE: SendToParent;
    VK_INSERT:
      if Shift = [] then SendToParent
      else if (Shift = [ssShift]) and not FGrid.CanEditModify then Key := 0;
    VK_LEFT:
      if ForwardMovement and (Ctrl or LeftSide) then
        SendToParent;
    VK_RIGHT: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_HOME: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_END: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_F2:
      begin
        ParentEvent;
        if Key = VK_F2 then
        begin
          Deselect;
          Exit;
        end;
      end;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
    VK_DELETE:
      if Ctrl then
        SendToParent
      else
        if not FGrid.CanEditModify then Key := 0;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TInplaceEditMS.KeyPress(var Key: Char);
var
  Selection: TSelection;
begin
  FGrid.KeyPress(Key);
  if (Key in [#32..#255]) and not FGrid.CanEditAcceptKey(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        if FGrid.FAutoSelect then
        begin
          SendMessage(Handle, EM_GETSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
          if (Selection.StartPos = 0) and (Selection.EndPos = GetTextLen) then
            Deselect
          else
            SelectAll;
        end;
        Key := #0;
      end;
    ^H, ^V, ^X, #32..#255:
      if not FGrid.CanEditModify then
        Key := #0
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TInplaceEditMS.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FGrid.KeyUp(Key, Shift);
end;

procedure TInplaceEditMS.WndProc(var Message: TMessage);
var
  R: TRect;
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(FGrid) then
          Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
    WM_KILLFOCUS:
      begin
        SetRect(R, 2, 2, Width-2, Height);
        SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
        ShowScrollBar(Handle,SB_VERT,False);
        IsScrolled:=False;
      end;
    EM_SETSEL:                  //Добавлено для реализации AutoSelect
      if not FGrid.FAutoSelect and
        (TDBGridInplaceEdit(Self).EditStyle=esSimple)
        and not FPreventSelection and (FGrid.FColumns.Count>0) then
      begin
        FPreventSelection:=True;
        SetCaretPosition;
        FPreventSelection:=False;
        Exit;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TInplaceEditMS.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Longint($FFFFFFFF));
end;

procedure TInplaceEditMS.Invalidate;
var
  Cur: TRect;
begin
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, FGrid.Handle, Cur, 2);
  ValidateRect(FGrid.Handle, @Cur);
  InvalidateRect(FGrid.Handle, @Cur, False);
end;

procedure TInplaceEditMS.Hide;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
      SWP_NOREDRAW);
    if Focused then Windows.SetFocus(FGrid.Handle);
  end;
end;

function TInplaceEditMS.PosEqual(const Rect: TRect): Boolean;
var
  Cur: TRect;
begin
  GetWindowRect(Handle, Cur);
  MapWindowPoints(HWND_DESKTOP, FGrid.Handle, Cur, 2);
  Result := EqualRect(Rect, Cur);
end;

procedure TInplaceEditMS.InternalMove(const Loc: TRect; Redraw: Boolean);
begin
  if IsRectEmpty(Loc) then Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
    with Loc do
      SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
        SWP_SHOWWINDOW or SWP_NOREDRAW);
    BoundsChanged;
    if Redraw then Invalidate;
    if FGrid.Focused then
      Windows.SetFocus(Handle);
  end;
end;

procedure TInplaceEditMS.BoundsChanged;
var
  R: TRect;
begin
  R := Rect(2, 2, Width - 2, Height);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TInplaceEditMS.UpdateLoc(const Loc: TRect);
begin
  InternalMove(Loc, False);
end;

function TInplaceEditMS.Visible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TInplaceEditMS.Move(const Loc: TRect);
begin
  InternalMove(Loc, True);
end;

procedure TInplaceEditMS.SetFocus;
begin
  if IsWindowVisible(Handle) then
begin
    Windows.SetFocus(Handle);
end;
end;

procedure TInplaceEditMS.UpdateContents;
begin
  Text := '';
  EditMask := FGrid.GetEditMask(FGrid.Col, FGrid.Row);
  Text := FGrid.GetEditText(FGrid.Col, FGrid.Row);
  MaxLength := FGrid.GetEditLimit;
end;

procedure TInplaceEditMS.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    if Visible then
      RecreateWnd;
  end;
end;

procedure TInplaceEditMS.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

function TInplaceEditMS.GetCaretPos: TPoint;
begin
  Result.X := LongRec(SendMessage(Handle, EM_GETSEL, 0, 0)).Hi;
  Result.Y := SendMessage(Handle, EM_LINEFROMCHAR, Result.X, 0);
  Result.X := Result.X - SendMessage(Handle, EM_LINEINDEX, -1, 0);
end;

procedure TInplaceEditMS.SetCaretPos(const Value: TPoint);
var
  CharIdx: Integer;
begin
  CharIdx := SendMessage(Handle, EM_LINEINDEX, Value.y, 0) + Value.x;
  SendMessage(Handle, EM_SETSEL, CharIdx, CharIdx);
end;

procedure TInplaceEditMS.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Visible then
      RecreateWnd;
  end;
end;

function TInplaceEditMS.WantRecreateWnd: Boolean;
begin
  result:=False;
  with FGrid do
  begin
    if (FMemoAllowed and FColumns[SelectedIndex].Memo) then
    begin
      if not FWordWrap then
      begin
        FWordWrap:=True;
        result:=True;
      end;
    end
    else
      if FWordWrap then
      begin
        FWordWrap:=False;
        result:=True;
      end;
    if FAlignment<>FColumns[SelectedIndex].Alignment then
    begin
      FAlignment:=FColumns[SelectedIndex].Alignment;
      result:=True;
    end;
  end;
end;

function TInplaceEditMS.NeedScrollBars: Integer;
var
  NumLines: Integer;
  MemoMaxRows: Integer;
  R: TRect;
  RH: Integer;
  dc: HDC;
  LinesWidth: Integer;
begin
  NumLines:=Lines.Count;
  MemoMaxRows:=FGrid.FMemoMaxRows;
  if NumLines<1 then
    NumLines:=1;
  if dgRowLines in FGrid.FOptions then
    LinesWidth:=FGrid.GridLineWidth
  else
    LinesWidth:=0;
  SetRect(R,0,0,Width-2*LinesWidth-2,17);
  if FGrid.FMemoAllowed then
  begin
    dc:=GetDC(Handle);
    SelectObject(dc,Font.Handle);
    RH:=DrawText(dc, PChar(Text), -1, R, DT_WORDBREAK or DT_CALCRECT)+3+LinesWidth;
    ReleaseDC(Handle,dc);
  end
  else
    RH:=0;
  if (FGrid.FMemoAllowed) and ((MemoMaxRows>0)or (RH>Height)) then
  begin
    if (NumLines>MemoMaxRows) or (RH>Height) then
    begin
      NumLines:=MemoMaxRows;
      if (not IsScrolled) and
         (FGrid.FColumns[FGrid.SelectedIndex].ButtonStyle<>cbsEllipsis) and
         (moScrollBar in FGrid.FMemoOptions) then
      begin
        SetRect(R, 2, 2, Width-2- GetSystemMetrics(SM_CXVSCROLL), Height);
        ShowScrollBar(Handle,SB_VERT,True);
        SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
        IsScrolled:=True;
      end;
    end
    else
      if IsScrolled then
      begin
        SetRect(R, 2, 2, Width - 2 , Height);
        ShowScrollBar(Handle,SB_VERT,False);
        SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
        IsScrolled:=False;
      end;
  end;
  result:=NumLines;
end;

procedure TInplaceEditMS.SetCaretPosition;
var
  Coord: Integer;
  Rect: TRect;
  p: TPoint;
begin
  with FGrid do
  begin
    if (FMousePoint.X<>-1) {and not (dgAlwaysShowEditor in Options)} then
    begin
      Self.Width:=FColumns[SelectedIndex].Width;
      Self.Height:=RowHeights[Row];
      SetRect(Rect, 2, 2, Self.Width - 2 , Self.Height-2);
      SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Rect));
      Rect:=CellRect(Col,Row);
      p.X:=FMousePoint.X-Rect.Left;
      p.Y:=FMousePoint.Y-Rect.Top;
      Coord:=SendMessage(Self.Handle,EM_CHARFROMPOS,0,MAKELPARAM(p.X,p.Y));
      p.X:=LO(Coord);
      p.Y:=HI(Coord);
      SetCaretPos(p);
    end;
  end;
end;

{ TPopupListbox }

type


  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

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
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
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
  TInplaceEditListMS(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;


procedure TInplaceEditMS.Loaded;
begin
  inherited;
  AutoSelect:=FGrid.FAutoSelect;
end;

{ TInplaceEditListMS }

constructor TInplaceEditListMS.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
end;

procedure TInplaceEditListMS.BoundsChanged;
var
  R: TRect;
  ScrollWidth: Integer;
  WndStyle: Integer;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if EditStyle <> esSimple then
  begin
    if not FGrid.UseRightToLeftAlignment then
      Dec(R.Right, ButtonWidth)
    else
      Inc(R.Left, ButtonWidth - 2);
  end
  else
  WndStyle:=GetWindowLong(Handle,GWL_STYLE);
  if (WndStyle and WS_VSCROLL)<>0 then
  begin
    ScrollWidth:=Windows.GetSystemMetrics(SM_CXVSCROLL);
    Dec(R.Right, ScrollWidth);
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TInplaceEditListMS.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if ListVisible and (ActiveList = TWinControl(FPickList)) then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if TCustomListBox(PickList).ItemIndex <> -1 then
      ListValue := TCustomListBox(PickList).Items[TCustomListBox(PickList).ItemIndex];
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then
      if (not VarIsEmpty(ListValue) or VarIsNull(ListValue))
         and (ListValue <> Text) then
      begin
//          Here we store the new value directly in the edit control so that
//          we bypass the CMTextChanged method on TCustomMaskedEdit.  This
//          preserves the old value so that we can restore it later by calling
//          the Reset method.
        Perform(WM_SETTEXT, 0, Longint(string(ListValue)));
        Modified := True;
        with FGrid do
          SetEditText(Col, Row, ListValue);
      end;
  end;
end;

procedure TInplaceEditListMS.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if ListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if ListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TInplaceEditListMS.DoEditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(FGrid);
end;

procedure TInplaceEditListMS.DoGetPickListItems;
begin
  if not PickListLoaded then
  begin
    if Assigned(OnGetPickListItems) then
      OnGetPickListItems(FGrid.Col, FGrid.Row, TCustomListBox(PickList).Items);
    PickListLoaded := (TCustomListBox(PickList).Items.Count > 0);
  end;
end;

function TInplaceEditListMS.GetPickList: TCustomListbox;
var
  PopupListbox: TPopupListbox;
begin
  if not Assigned(FPickList) then
  begin
    PopupListbox := TPopupListbox.Create(Self);
    PopupListbox.Visible := False;
    PopupListbox.Parent := Self;
    PopupListbox.OnMouseUp := ListMouseUp;
    PopupListbox.IntegralHeight := True;
    PopupListbox.ItemHeight := 11;
    FPickList := PopupListBox;
  end;
  Result := FPickList;
end;

procedure TInplaceEditListMS.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
begin
  if not ListVisible then
  begin
    ActiveList.Width := Width;
    if ActiveList = FPickList then
    begin
      DoGetPickListItems;
      TPopupListBox(PickList).Color := Color;
      TPopupListBox(PickList).Font := Font;
      if (DropDownRows > 0) and (PickList.Items.Count >= DropDownRows) then
        PickList.Height := DropDownRows * TPopupListBox(PickList).ItemHeight + 4
      else
        PickList.Height := PickList.Items.Count * TPopupListBox(PickList).ItemHeight + 4;
      if Text = '' then
        PickList.ItemIndex := -1
      else
        PickList.ItemIndex := PickList.Items.IndexOf(Text);
      J := PickList.ClientWidth;
      for I := 0 to PickList.Items.Count - 1 do
      begin
        Y := PickList.Canvas.TextWidth(PickList.Items[I]);
        if Y > J then J := Y;
      end;
      PickList.ClientWidth := J;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + ActiveList.Height > Screen.Height then Y := P.Y - ActiveList.Height;
    if TDBGridInplaceEdit(Self).FDataList<>nil then
      if TDBGridInplaceEdit(Self).FDataList.RowCount<=FGrid.FColumns[FGrid.SelectedIndex].DropDownRows then
        ShowScrollBar(ActiveList.Handle,SB_VERT,False)
    else
      ShowScrollBar(ActiveList.Handle,SB_VERT,True);
    SetWindowPos(ActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

procedure TInplaceEditListMS.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    DoEditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TInplaceEditListMS.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(ActiveList.ClientRect, Point(X, Y)));
end;

procedure TInplaceEditListMS.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if ListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(ActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TInplaceEditListMS.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if ListVisible then
    begin
      ListPos := ActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(ActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(ActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TInplaceEditListMS.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := Pressed;
  StopTracking;
  if (Button = mbLeft) and (EditStyle = esEllipsis) and WasPressed then
    DoEditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TInplaceEditListMS.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
begin
  if EditStyle <> esSimple then
  begin
    R := ButtonRect;
    Flags := 0;
    case EditStyle of
      esPickList:
        begin
          if ActiveList = nil then
            Flags := DFCS_INACTIVE
          else if Pressed then
            Flags := DFCS_FLAT or DFCS_PUSHED;
          DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
        end;
      esEllipsis:
        begin
          if Pressed then Flags := BF_FLAT;
          DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
          X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(Pressed);
          Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(Pressed);
          W := ButtonWidth shr 3;
          if W = 0 then W := 1;
          PatBlt(DC, X, Y, W, W, BLACKNESS);
          PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
          PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
        end;
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TInplaceEditListMS.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TInplaceEditListMS.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TInplaceEditListMS.UpdateContents;
begin
  ActiveList := nil;
  PickListLoaded := False;
  FEditStyle := FGrid.GetEditStyle(FGrid.Col, FGrid.Row);
  if EditStyle = esPickList then
    ActiveList := PickList;
  inherited UpdateContents;
end;

procedure TInplaceEditListMS.RestoreContents;
begin
  Reset;
  with FGrid do
  begin
    if (Col<>-1) and (Row<>-1) then
      SetEditText(Col,Row,Text);
  end;
end;

procedure TInplaceEditListMS.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> ActiveList) then
    CloseUp(False);
end;

procedure TInplaceEditListMS.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TInplaceEditListMS.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.WParam) <> FGrid.Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

function TInplaceEditListMS.ButtonRect: TRect;
begin
  if not FGrid.UseRightToLeftAlignment then
    Result := Rect(Width - ButtonWidth, 0, Width, Height)
  else
    Result := Rect(0, 0, ButtonWidth, Height);
end;

function TInplaceEditListMS.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TInplaceEditListMS.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (EditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TInplaceEditListMS.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TInplaceEditListMS.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (EditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TInplaceEditListMS.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle = esPickList then
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and ListVisible then
        begin
          with TMessage(Message) do
            SendMessage(ActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

procedure TInplaceEditListMS.DblClick;
var
  Index: Integer;
  ListValue: string;
begin
  if (EditStyle = esSimple) or Assigned(FGrid.OnDblClick) then
    inherited
  else if (EditStyle = esPickList) and (ActiveList = PickList) then
  begin
    DoGetPickListItems;
    if PickList.Items.Count > 0 then
    begin
      Index := PickList.ItemIndex + 1;
      if Index >= PickList.Items.Count then
        Index := 0;
      PickList.ItemIndex := Index;
      ListValue := PickList.Items[PickList.ItemIndex];
      Perform(WM_SETTEXT, 0, Longint(ListValue));
      Modified := True;
      with FGrid do
        SetEditText(Col, Row, ListValue);
      SelectAll;
    end;
  end
  else if EditStyle = esEllipsis then
    DoEditButtonClick;
end;

end.
