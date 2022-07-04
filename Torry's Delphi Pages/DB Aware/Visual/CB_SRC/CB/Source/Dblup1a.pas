unit dblup1a;

{************************************************************}
{                                                            }
{      TDBComboBoxPlus & TComboBoxPlus Component             }
{                                                            }
{  Copyright (c) 1995,1996,1997,1998 Out & About Production  }
{Portions Copyright (c) 1995,96,97,98 Inprise International  }
{                                                            }
{                     Version 4.00                           }
{                                                            }
{************************************************************}

interface

{$IFDEF VER100}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D3OR4OR5}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D3OR4OR5}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D4OR5}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D4OR5}
{$ENDIF}

uses
  SysUtils,
{$IFDEF Win32}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
   Messages, Classes, Graphics, Controls, Menus,
  Forms, Dialogs, StdCtrls, grids, Cbxbase;

type
  TComboPlusStyle = (csIncSearch, csIncSrchEdit);
  TNewLookUpRecEvent = procedure(Sender: TObject; var Cancelled: Boolean) of object;
  TStringGridPOptions = (loColLines, loRowLines, loThumbTracking);
  TStringGridPOptionss = set of TStringGridPOptions;
  TLookupRecChangedEvent = procedure(Sender: TObject; byIncSearch: Boolean) of object;

  TComboBoxPlus = class(TCBXBase)
  private
    { Private declarations }
    fStartNonEditHighlight : Integer;
    FLastChar : char;
    FLastKey : Word;
    FAutoFill : Boolean;
    FStyle: TComboPlusStyle;
    FAutoDropDown : Boolean;
    FSearchValue : String; {Holds the current search value}
    FOnDropDown: TNotifyEvent;
    FOnLookupRecChanged: TLookupRecChangedEvent;
    FOnBeforeSearch: TNotifyEvent;
    FOnAfterSearch: TNotifyEvent;
    function GetLookupActive : Boolean;
    procedure SetLookupActive(const Value : Boolean);
    function GetSearchValue : string;
    procedure SetSearchValue(const Value: string);
    function GetValue: string;
    procedure SetValue(const NewValue: string);
    function GetDisplayValue: string; virtual;
    procedure SetDisplayValue(const NewValue: string); virtual;
    function GetObjects(ACol, ARow: Integer): TObject;
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetStyle(Value: TComboPlusStyle);
    function GetFixedRows : Integer;
    procedure SetFixedRows(const Value: Integer);
    function GetRowCount : LongInt;
    procedure SetRowCount(Value: Longint);
    function GetColCount : LongInt;
    procedure SetColCount(Value: Longint);
    function GetCols(Index: Integer): TStrings;
    procedure SetCols(Index: Integer; Value: TStrings);
    function GetCol : LongInt;
    function GetRow : LongInt;
    function GetRows(Index: Integer): TStrings;
    procedure SetRows(Index: Integer; Value: TStrings);
    function GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    function GetColWidths(Index: LongInt): Integer;
    procedure SetColWidths(Index: LongInt; Value: Integer);
    function GetRowHeights(Index: Longint): Integer;
    procedure SetRowHeights(Index: Longint; Value: Integer);
    function GetDropDownCount : Integer;
    procedure SetDropDownCount(Value : Integer);
    function GetOptions: TStringGridPOptionss;
    procedure SetOptions(Value: TStringGridPOptionss);
    function GetDefaultRowHeight: Integer;
    procedure SetDefaultRowHeight(Value: Integer);
    function GetDropDownWidth: Integer;
    procedure SetDropDownWidth(Value: Integer);
    function GetListCursor: TCursor;
    procedure SetListCursor(Value: TCursor);
    function GetListColor: TColor;
    procedure SetListColor(Value: TColor);
    function GetTitleColor: TColor;
    procedure SetTitleColor(Value: TColor);
    function GetListParentColor: Boolean;
    procedure SetListParentColor(Value: Boolean);
    function IsDropDownColorStored: Boolean;
    function GetSorted : Boolean;
    procedure SetSorted(const Value : Boolean);
    function GetListVisible : Boolean;
    function GetListDefaultDrawing : Boolean;
    procedure SetListDefaultDrawing(value : Boolean);
    function GetOnDrawCell : TDrawCellEvent;
    procedure SetOnDrawCell(value : TDrawCellEvent);
    procedure ReadCells(Reader: TReader);
    procedure WriteCells(Writer: TWriter);
    procedure NonEditMouseDown(var Message: TWMLButtonDown);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMCut (var Message: TMessage); message WM_CUT;
    procedure WMPaste (var Message: TMessage); message WM_PASTE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
  protected
    FOnNewLookupRec: TNewLookUpRecEvent;
    procedure Loaded; override;
    function CanModify : Boolean; virtual;
    procedure edit; virtual;
    function Editable: Boolean; override;
    function CanEdit: Boolean; override;
    procedure DrawNonEditFocusRect(var Message: TWMPaint); override;
    Procedure SelectText(Const StartPos, EndPos : Integer);
    function editing : boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
//    procedure DoSearch;
    procedure DoSelectSome;
    procedure DataModified; virtual;
    procedure LookupRecChanged(byIncSearch: Boolean); virtual;
    procedure EnterLookupRec; dynamic;
    procedure UpdateRecord; virtual;
    procedure Change; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure DoSearch;
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    function AddRow(Const Values: array of String) : Integer;
    procedure ClearGridData;
    procedure SizeGridToData;
{   procedure SortList; }  {REMOVED INFAVOR OF SORTED PROPERTY}
{    procedure DropDown; } {Implemented in CBXBase}
{    procedure CloseUp; }  {Implemented in CBXBase}
    property SearchValue: string read GetSearchValue write SetSearchValue;
    property Value: string read GetValue write SetValue;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Col : Longint read GetCol;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
{    property ListCanvas : TCanvas read GetListCanvas; }
    property ListVisible: Boolean read GetListVisible;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
{    property IsSorted : boolean read fIsSorted write fIsSorted; }
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
    property Row : LongInt read getRow;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
{    property LeftTextMargin : Integer read FLeftTextMargin; }
{    property TopTextMargin : Integer read FTopTextMargin; }
  published
    property AutoDropDown : Boolean read FAutoDropDown Write FAutoDropDown default True;
    property AutoSelect;
    property BorderStyle;
    property ColCount : LongInt read GetColCount write SetColCount default 1;
    property Color;
    property Ctl3D;
    property DefaultRowHeight: Integer read GetDefaultRowHeight write SetDefaultRowHeight;
    property DragCursor;
{$IFDEF D4OR5}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property DragMode;
    property DropDownCount : Integer read GetDropDownCount write SetDropDownCount;
    property DropDownAlign;
    property DropDownTop;
    property DropDownWidth : Integer read getDropDownWidth write SetDropDownWidth;
    property Enabled;
    property FixedRows : Integer read GetFixedRows write SetFixedRows default 0;
    property Font;
    property ListColor : TColor read getListColor write SetListColor stored IsDropDownColorStored;
    property ListDefaultDrawing : Boolean read GetListDefaultDrawing write SetListDefaultDrawing;
    property ListFont;
    property ListCursor : TCursor read GetListCursor write SetListCursor;
    property ButtonCursor;
    property LookupActive : Boolean read GetLookupActive write SetLookupActive default False;
    property MaxLength;
    property Options : TStringGridPOptionss read GetOptions write SetOptions;
    property ReadOnly;
    property RowCount : LongInt read GetRowCount write SetRowCount default 10;
    property ShowSpeedButton;
    property Style: TComboPlusStyle read FStyle write SetStyle default csIncSearch;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleColor : TColor read getTitleColor write SetTitleColor;
    property Visible;
    {New Properties}
    property ListParentColor : Boolean read getListParentColor write SetListParentColor;
    property Sorted : boolean read GetSorted write SetSorted default True;
    property ListParentFont;
    property UserDraw;
    property ButtonGlyph;
    property ButtonNumGlyphs;
    {End New Properties}
    property OnAfterSearch: TNotifyEvent read FOnAfterSearch write FOnAfterSearch;
    property OnBeforeSearch: TNotifyEvent read FOnBeforeSearch write FOnBeforeSearch;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell : TDrawCellEvent read GetOnDrawCell write SetOnDrawCell;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnLookupRecChanged: TLookupRecChangedEvent read FOnLookupRecChanged write FOnLookupRecChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewLookupRec: TNewLookUpRecEvent read FOnNewLookupRec write FOnNewLookupRec;
  end;

  TDDLinkPlus = class;
  
  TStringGridPStrings = class(TStrings)
  private
    FLink: TDDLinkPlus; 
    FIndex: Integer;
    procedure CalcXY(Index: Integer; var X, Y: Integer);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(ALink: TDDLinkPlus; AIndex: Longint);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Add(const S: string): Integer; override;
  end;

  TDDLinkPlus = class(TDDLinkBase)
  private
    FData: Pointer;
    FRows: Pointer;
    FCols: Pointer;
    FTotalW : Integer;
    fRow : LongInt;      {Stores the current row in the grid}
    fCol : LongInt;      {Stores the current col in the grid}

    FDisplayValue: String; {Contains the value that goes into the edit box if Lookup active = True}
    FValue: String;      {The is the value used to lookup the display value, this is what true data source returns}

    FFoundValue: Boolean;  {Flag that indicates if DoLookUp was successful}
    FSorted : Boolean;     {Is the list supposed to be sorted (this is the property field)}
    FIsSorted : Boolean;   {If its supposed to be sorted has it been sorted yet}
    fLookupActive : Boolean;
    FColWidths: Pointer;
    FRowHeights: Pointer;
    FDefaultRowHeight: Integer;
    fFixedRows : Integer;
    fRowCount : LongInt;
    fColCount : LongInt;
    fDropDownCount : Integer;
    fOptions : TStringGridPOptionss;
    fDropDownWidth: Integer;
    fListCursor: TCursor;
    fListColor: TColor;
    fListParentColor : Boolean;
    fTitleColor: TColor;
    FListDefaultDrawing : Boolean;
    FOnDrawCell: TDrawCellEvent;

    procedure ChangeSize(NewColCount, NewRowCount: Longint);
    procedure Initialize;
    procedure SetLookupActive(const Value : Boolean);
    procedure SetFixedRows(const Value: Integer);
    procedure SetRowCount(Value: Longint);
    procedure SetColCount(Value: Longint);
    procedure SetDropDownWidth(value : Integer);

    function GetColWidths(Index: Longint): Integer;
    procedure SetColWidths(Index: Longint; Value: Integer);
    function GetRowHeights(Index: Longint): Integer;
    procedure SetRowHeights(Index: Longint; Value: Integer);
    procedure SetDefaultRowHeight(Value: Integer);
    function EnsureColRow(Index: Integer; IsCol: Boolean): TStringGridPStrings;
    function EnsureDataRow(ARow: Integer): Pointer;
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetCol : LongInt;
    function GetRow : LongInt;

    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCols(Index: Integer; Value: TStrings);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetRows(Index: Integer; Value: TStrings);
    procedure DoLookup;
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetDisplayValue: string;
    procedure SetDisplayValue(const Value: string);
  protected
    procedure GridClick (Sender: TObject);
  public
    constructor Create(AOwner: TCBXBase); override;
    destructor Destroy; override;
    function AddRow(Values: array of string) : Integer;
    procedure ClearGridData;
    procedure SortList(SortCol : Integer); dynamic;
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure SetColumnAttributes; virtual;
    property LookupActive : Boolean read fLookupActive write setLookupActive;
    property FixedRows : Integer read fFixedRows write SetFixedRows default 0;
    property RowCount : LongInt read fRowCount write SetRowCount default 1;
    property ColCount : LongInt read fColCount write SetColCount default 2;

    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 24;
    property DropDownCount : Integer read fDropDownCount write fDropDownCount default 8;
    property Options: TStringGridPOptionss read fOptions write fOptions default [loThumbTracking];
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth;
    property ListCursor: TCursor read fListCursor write fListCursor;
    property ListColor: TColor read fListColor write fListColor default clWindow;
    property TitleColor: TColor read fTitleColor write fTitleColor default clBtnFace;
    property ListParentColor : Boolean read fListParentColor write fListParentColor default false;
    property ListDefaultDrawing : Boolean read FListDefaultDrawing write FListDefaultDrawing default true;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Col : LongInt read GetCol;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
    property Row : LongInt read GetRow;

    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property Value: string read GetValue write SetValue;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property OnDrawCell : TDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;


 {TDrawGridP}
  TDrawGridP = class(TCustomGrid)
  private
    FOnDrawCell: TDrawCellEvent;
    FOnTopLeftChanged: TNotifyEvent;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure TopLeftChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CellRect(ACol, ARow: Longint): TRect;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
  end;


  TStringGridP = class(TDrawGridP)
  private
    FDDLink : TDDLinkPlus;
    FUpdating: Boolean;
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FOptions: TStringGridPOptionss;

    FOnListClick: TNotifyEvent;
    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure xUpdate(ACol, ARow: Integer);
    function GetCells(ACol, ARow: Integer): string;
    function GetCols(Index: Integer): TStrings;
    function GetObjects(ACol, ARow: Integer): TObject;
    function GetRows(Index: Integer): TStrings;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCols(Index: Integer; Value: TStrings);
    procedure SetObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetRows(Index: Integer; Value: TStrings);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetOptions(Value: TStringGridPOptionss);

    function GetValue: string;
    procedure SetValue(const Value: string);
    procedure ListClick; dynamic;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(msButton: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(msButton: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetUpdateState(Updating: Boolean);
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property ColWidths;
    property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property Options: TStringGridPOptionss read FOptions write SetOptions default [];
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;
    property Value: string read GetValue write SetValue;
    property OnClick: TNotifyEvent read FOnListClick write FOnListClick;
  end;

{ TPopupGrid }

  TPopupGrid = class(TStringGridP)
  private
    FCombo: TComboBoxPlus;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure MouseDown(msButton: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses
 Consts;

 Const
{$IFDEF Win32}
   MaxSel = MaxShort;
{$ELSE}
   MaxSel = MaxInt;
{$ENDIF}

{$IFDEF Demo}
  Function DelphiLoaded(theState : TComponentState) : Boolean;
{$IFNDEF Win32}
  var
    aHandle : THandle;
    buff : ARRAY[0..60] OF Char;
    WndClass : TWndClass;
{$ENDIF}
  begin
    Result := False;
{$IFDEF Win32}
    if (FindWindow('TAppBuilder',nil) <> 0) then  Result := True
{$ELSE}
    aHandle := GetModuleHandle('delphi.exe');
    if aHandle = 0 then exit;
    strCopy(buff, 'TAppBuilder');
    If not(GetClassInfo(aHandle, buff, WndClass)) then exit;
    Result := True;
{$ENDIF}
  end;

Procedure KillApp(Comp : String);
begin
  ShowMessage(
  Format('The %S component you used in this application is a demo version ',  [Comp]) + #13#10 +
         'and requires that Delphi be running. To fix this problem you need to ' +  #13#10 +
         'contact O && A Productions at ' +  #13#10 +
         '          Fax (619)839-3834 or ' + #13#10 +
         '          E-mail sales@o2a.com or /n/r' +  #13#10 +
         'Visit our web site at www.o2a.com');
  Halt;
end;
{$ENDIF} {Demo}


{---------------- TComboBoxPlus ---------------------------------}

Constructor TComboBoxPlus.Create(aOwner: TComponent);
begin
{$IFDEF Demo}
  if Not DelphiLoaded(ComponentState) then KillApp('TComboBoxPlus');
{$ENDIF} {Demo}
  inherited Create(aOwner);
  AutoSize := False;
  DDLink := TDDLinkPlus.Create(Self);
  FStyle := csIncSearch;
  FSearchValue := '';
  FAutoDropDown := True;
end;

Destructor TComboBoxPlus.Destroy;
begin
  if (DDLink <> nil) then DDLink.free;
  inherited Destroy;
end;

procedure TComboBoxPlus.Loaded;
begin
  inherited Loaded;
  FButton.OnClick := OnButtonClick;
end;

procedure TComboBoxPlus.SizeGridToData;
begin
  TDDLinkPlus(DDLink).FIsSorted := False;      {Since data is changing make sure it gets sorted again}
  TDDLinkPlus(DDLink).SetColumnAttributes;
end;

function TComboBoxPlus.AddRow(const Values: array of String) : Integer;
begin
  TDDLinkPlus(DDLink).FIsSorted := False;      {Since data is changing make sure it gets sorted again}
  result := TDDLinkPlus(DDLink).AddRow(Values);
end;

procedure TComboBoxPlus.ClearGridData;
begin
  TDDLinkPlus(DDLink).FIsSorted := False;      {Since data is changing make sure it gets sorted again}
  TDDLinkPlus(DDLink).ClearGridData;
end;

procedure TComboBoxPlus.SetStyle(Value: TComboPlusStyle);
begin
  if FStyle <> Value then FStyle := Value;
end;

procedure TComboBoxPlus.SetSearchValue(const Value: string);
begin
  if (Value <> SearchValue) then
    FSearchValue := Value;
end;

function TComboBoxPlus.GetSearchValue : string;
begin
  Result := FSearchValue;
end;

function TComboBoxPlus.GetValue: String;
begin
  if Editable then
    Result := Text
  else
    Result := TDDLinkPlus(DDLink).Value;
end;

procedure TComboBoxPlus.SetValue(const NewValue: String);
begin
  if (TDDLinkPlus(DDLink).Rows[TDDLinkPlus(DDLink).Row].Strings[0] <> NewValue) or
    (Text <> NewValue) then
  begin
    TDDLinkPlus(DDLink).Value := NewValue;      {this moves to the right place in grid}
    Text := TDDLinkPlus(DDLink).DisplayValue;   {this returns the correct text}
  end;
end;

function TComboBoxPlus.GetDisplayValue: String;
begin
  Result := Text;
end;

procedure TComboBoxPlus.SetDisplayValue(const NewValue: String);
begin
  if DDLink = nil then exit;
  if (TDDLinkPlus(DDLink).DisplayValue <> NewValue) or
     (Text <> TDDLinkPlus(DDLink).DisplayValue)  then
  begin
    TDDLinkPlus(DDLink).DisplayValue := NewValue;
    Text := TDDLinkPlus(DDLink).DisplayValue;
    LookupRecChanged(True);
  end;
end;

function TComboBoxPlus.GetObjects(ACol, ARow: Integer): TObject;
begin
  Result := TDDLinkPlus(DDLink).GetObjects(ACol, ARow);
end;

procedure TComboBoxPlus.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  TDDLinkPlus(DDLink).SetObjects(ACol, ARow, Value);
end;

function TComboBoxPlus.GetLookupActive : Boolean;
begin
  Result := TDDLinkPlus(DDLink).LookupActive;
end;

procedure TComboBoxPlus.SetLookupActive(const Value: Boolean);
begin
  TDDLinkPlus(DDLink).LookupActive := Value;
end;

function TComboBoxPlus.GetOnDrawCell : TDrawCellEvent;
begin
  Result := TDDLinkPlus(DDLink).OnDrawCell;
end;

procedure TComboBoxPlus.SetOnDrawCell(value : TDrawCellEvent);
begin
  TDDLinkPlus(DDLink).OnDrawCell := value;
end;

function TComboBoxPlus.GetFixedRows : Integer;
begin
  Result := TDDLinkPlus(DDLink).FixedRows;
end;

procedure TComboBoxPlus.SetFixedRows(Const Value: Integer);
begin
  TDDLinkPlus(DDLink).FixedRows := Value;
end;

function TComboBoxPlus.GetRowCount : LongInt;
begin
  Result := TDDLinkPlus(DDLink).RowCount;
end;

procedure TComboBoxPlus.SetRowCount(Value: Longint);
begin
  TDDLinkPlus(DDLink).RowCount := Value;
end;

function TComboBoxPlus.GetColCount : LongInt;
begin
  Result := TDDLinkPlus(DDLink).ColCount;
end;

procedure TComboBoxPlus.SetColCount(Value: Longint);
begin
  TDDLinkPlus(DDLink).ColCount := Value;
end;

function TComboBoxPlus.GetCols(Index: Integer): TStrings;
begin
  Result := TDDLinkPlus(DDLink).Cols[Index];
end;

procedure TComboBoxPlus.SetCols(Index: Integer; Value: TStrings);
begin
  TDDLinkPlus(DDLink).Cols[Index] := Value;
end;

function TComboBoxPlus.GetCol : LongInt;
begin
  result := TDDLinkPlus(DDLink).Col;
end;

function TComboBoxPlus.GetRow : LongInt;
begin
  result := TDDLinkPlus(DDLink).Row;
end;

function TComboBoxPlus.GetRows(Index: Integer): TStrings;
begin
  Result := TDDLinkPlus(DDLink).Rows[Index];
end;

procedure TComboBoxPlus.SetRows(Index: Integer; Value: TStrings);
begin
  TDDLinkPlus(DDLink).Rows[Index] := Value;
end;

function TComboBoxPlus.GetCells(ACol, ARow: Integer): string;
begin
  Result := TDDLinkPlus(DDLink).Cells[ACol, ARow];
end;

procedure TComboBoxPlus.SetCells(ACol, ARow: Integer; const Value: string);
begin
  TDDLinkPlus(DDLink).Cells[ACol, ARow] := Value;
end;


function TComboBoxPlus.GetColWidths(Index: LongInt): Integer;
begin
  if Index = 0 then
    result := 0
  else
    result := TDDLinkPlus(DDLink).ColWidths[Index];
end;

procedure TComboBoxPlus.SetColWidths(Index: LongInt; Value: Integer);
begin
  If Index = 0 then
    ShowMessage('Can not assign a width to column 0 which is invisible')
  else
    TDDLinkPlus(DDLink).ColWidths[Index] := Value;
end;

function TComboBoxPlus.GetRowHeights(Index: Longint): Integer;
begin
  result := TDDLinkPlus(DDLink).RowHeights[Index];
end;

procedure TComboBoxPlus.SetRowHeights(Index: Longint; Value: Integer);
begin
  TDDLinkPlus(DDLink).RowHeights[Index] := Value;
end;

function TComboBoxPlus.GetDropDownCount : Integer;
begin
  result := TDDLinkPlus(DDLink).DropDownCount;
end;

procedure TComboBoxPlus.SetDropDownCount(Value : Integer);
begin
  TDDLinkPlus(DDLink).DropDownCount := Value;
end;

function TComboBoxPlus.GetOptions: TStringGridPOptionss;
begin
  result := TDDLinkPlus(DDLink).Options;
end;

procedure TComboBoxPlus.SetOptions(Value: TStringGridPOptionss);
begin
  TDDLinkPlus(DDLink).Options := Value;
end;

function TComboBoxPlus.GetDefaultRowHeight: Integer;
begin
  result := TDDLinkPlus(DDLink).DefaultRowHeight;
end;

procedure TComboBoxPlus.SetDefaultRowHeight(Value: Integer);
begin
  TDDLinkPlus(DDLink).DefaultRowHeight := Value;
end;

function TComboBoxPlus.GetDropDownWidth: Integer;
begin
  result := TDDLinkPlus(DDLink).DropDownWidth;
end;

procedure TComboBoxPlus.SetDropDownWidth(Value: Integer);
begin
  TDDLinkPlus(DDLink).DropDownWidth := Value;
end;

function TComboBoxPlus.GetListCursor: TCursor;
begin
  result := TDDLinkPlus(DDLInk).ListCursor;
end;

procedure TComboBoxPlus.SetListCursor(Value: TCursor);
begin
  TDDLinkPlus(DDLink).ListCursor := Value;
end;

function TComboBoxPlus.GetListColor: TColor;
begin
  result := TDDLinkPlus(DDLInk).ListColor;
end;

procedure TComboBoxPlus.SetListColor(Value: TColor);
begin
  if TDDLinkPlus(DDLink).ListColor <> Value then
  begin
    TDDLinkPlus(DDLink).ListColor := Value;
    ListParentColor := False;
    Perform(CM_COLORCHANGED, 0, 0);
  end;
end;

function TComboBoxPlus.GetListDefaultDrawing : Boolean;
begin
  result := TDDLinkPlus(DDLink).ListDefaultDrawing;
end;

procedure TComboBoxPlus.SetListDefaultDrawing(value : Boolean);
begin
  TDDLinkPlus(DDLink).ListDefaultDrawing := value;
end;

procedure TComboBoxPlus.NonEditMouseDown(var Message: TWMLButtonDown);
var
  CtrlState: TControlState;
begin
  SetFocus;
  HideCaret (Handle);
  if DDLink.DropDownObj <> nil then
    CloseUp
  else
    DropDown;
  MouseCapture := True;
  if csClickEvents in ControlStyle then
  begin
    CtrlState := ControlState;
    Include(CtrlState, csClicked);
    ControlState := CtrlState;
  end;
  with Message do
    MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TComboBoxPlus.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if Editable then
    inherited
  else
    NonEditMouseDown (Message);
end;

procedure TComboBoxPlus.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if not Editable then MouseCapture := False;
  inherited;
end;

procedure TComboBoxPlus.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if Editable then
    inherited
  else
    NonEditMouseDown (Message);
end;

procedure MouseDragToGrid (Ctrl: TControl; Grid: TPopupGrid; X, Y: Integer);
var
  pt, clientPt: TPoint;
begin
  if (Grid.Visible) then
  begin
    pt.X := X;
    pt.Y := Y;
    pt := Ctrl.ClientToScreen (pt);
    clientPt := Grid.ClientOrigin;
    if (pt.X >= clientPt.X) and (pt.Y >= clientPt.Y) and
       (pt.X <= clientPt.X + Grid.ClientWidth) and
       (pt.Y <= clientPt.Y + Grid.ClientHeight) then
    begin
      Ctrl.Perform(WM_LBUTTONUP, 0, MakeLong (abs(X), abs(Y)));
      pt := Grid.ScreenToClient(pt);
      Grid.Perform(WM_LBUTTONDOWN, 0, MakeLong (abs(pt.x), abs(pt.y)));
    end;
  end;
end;

procedure TComboBoxPlus.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);
  if (DDLink <> nil) and (TDDLinkPlus(DDLink).DropDownObj <> nil) and
   ((ssLeft in Shift) and not Editable and (GetCapture = Handle)) then
    MouseDragToGrid (Self, TPopupGrid(DDLink.DropDownObj), X, Y);
end;

procedure TComboBoxPlus.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not Editable then
    HideCaret (Handle);
end;

procedure TComboBoxPlus.CMExit(var Message: TCMExit);
begin
  try
    UpdateRecord;
  except
    if Editable then SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
  if not Editable then Invalidate;
end;

procedure TComboBoxPlus.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  if not Editable then Invalidate;
end;


function TComboBoxPlus.GetTitleColor: TColor;
begin
  result := TDDLinkPlus(DDLInk).TitleColor;
end;

procedure TComboBoxPlus.SetTitleColor(Value: TColor);
begin
  TDDLinkPlus(DDLink).TitleColor := Value;
end;

function TComboBoxPlus.GetListParentColor: Boolean;
begin
  result := TDDLinkPlus(DDLInk).ListParentColor;
end;

procedure TComboBoxPlus.SetListParentColor(Value: Boolean);
begin
  if TDDLinkPlus(DDLink).ListParentColor <> Value then
  begin
    TDDLinkPlus(DDLink).ListParentColor := Value;
    if Parent <> nil then Perform(CM_PARENTCOLORCHANGED, 0, 0);
  end;
end;

function TComboBoxPlus.IsDropDownColorStored: Boolean;
begin
  Result := not ListParentColor;
end;

procedure TComboBoxPlus.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if ListParentColor then
    TDDLinkPlus(DDLInk).ListColor := Color;
end;

procedure TComboBoxPlus.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ListParentColor then
  begin
    TDDLinkPlus(DDLInk).ListColor := Color;
    TDDLinkPlus(DDLInk).ListParentColor := True;
  end;
end;


procedure TComboBoxPlus.ReadCells(Reader: TReader);
var
  R,C : Integer;
  Garbage : String;
begin
  Reader.ReadListBegin;
  for R := 0 to (RowCount - 1) do
    Rows[R].Clear;
  R := 0;
  C := 0;

  While not Reader.EndOfList do
  begin

{}    If R < RowCount then
{}      Cells[C,R] := Reader.ReadString
{}    else
      Garbage := Reader.ReadString;  {make sure even bogus data is read}
    inc(C);
{}    if C = ColCount then
{}    begin
{}      C := 0;
{}      Inc(R);
{}    end;
  end;

  Reader.ReadListEnd;
end;

procedure TComboBoxPlus.WriteCells(Writer: TWriter);
var
  R,C : Integer;
begin
  Writer.WriteListBegin;
  for R := 0 to (RowCount - 1) do
    for C := 0 to (ColCount - 1) do
      Writer.WriteString(Cells[C,R]);
  Writer.WriteListEnd;
end;

procedure TComboBoxPlus.DefineProperties(Filer: TFiler);
  function DoWrite : Boolean;
  var I : Integer;
  begin
{$IFDEF Win32}
  { This special Win32 code is to handle the form inheritance introduced in D2}
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TComboBoxPlus then
      begin
        Result := False;
        For I := 0 to (ColCount - 1) do
          If not Cols[I].Equals(TComboBoxPlus(Filer.Ancestor).Cols[I]) then
          begin
            Result := True;
            break;
          end;
      end;
    end
    else
{$ELSE}
      Result := (ColCount > 0) or (RowCount > 0);
{$ENDIF}
    Result := (ColCount > 0) or (RowCount > 0);
  end;
begin
  inherited DefineProperties(Filer);
  with Filer do
  begin
    DefineProperty('Cells', ReadCells, WriteCells, DoWrite);
  end;
end;


function TComboBoxPlus.Editable: Boolean;
{ Does the controls style and properties allow it to be editable }
{ Basically its editable if the value returned can be something other than whats
  in the dropdown }
{ If its editable then it should have (goal) a highlight bar across the whole client area }
{ If the Lookup is not active then let it edit even if OnNewLURec is not assigned }
begin
  Result := (FStyle = csIncSrchEdit) and 
            (not(LookupActive) or Assigned(FOnNewLookupRec)); 
end;

Procedure TComboBoxPlus.SelectText(Const StartPos, EndPos : Integer);
{Note that these positions are inclusive with the first character in position 0
 also if EndPos < StartPos then the caret is to the left of the selection}
{$IFNDEF Win32}
type
  TSelection = record
    StartPos, EndPos: Integer;
  end;
var
  Selection: TSelection;
{$ENDIF}
begin
 If editable then
 begin
  {$IFDEF Win32}
    SendMessage(Handle, EM_SETSEL, StartPos, EndPos);
  {$ELSE}
    Selection.StartPos := StartPos;
    Selection.EndPos := EndPos;
    Perform(EM_SETSEL, 0, LongInt(Selection));
  {$ENDIF}
 end
 else
 begin
   { let paint handle it }
   if StartPos <= EndPos then
     fStartNonEditHighlight := StartPos
   else
     fStartNonEditHighlight := EndPos;
   invalidate;
 end;
end;

Procedure TComboBoxPlus.DrawNonEditFocusRect(var Message: TWMPaint);
{ called by WMPaint, this needs to be able to handle any special painting
  of the inside of the edit when editable is false }
var
  ARect: TRect;
  tLeft, tTop : Integer;
{$IFNDEF Win32}
  S: array[0..255] of Char;
{$ENDIF}

begin
  Perform(EM_GETRECT, 0, LongInt(@ARect));
  ARect := ClientRect;
  tLeft := ARect.Left;
  tTop :=  ARect.Top;
  Canvas.Font := Font;
  Canvas.Brush.Color := clHighlight;
  Canvas.Font.Color := clHighlightText;
  inc(ARect.Left,Canvas.textwidth(Copy(text, 0, fStartNonEditHighlight)));
  inc(ARect.Top,1);
  if (Button <> nil) then
  begin
    dec(ARect.Bottom,1);
    dec(ARect.Right, Button.WIdth);
  end;
{$IFDEF Win32}
  ExtTextOut(Canvas.Handle, tLeft, tTop, ETO_OPAQUE or ETO_CLIPPED, @ARect,
    PChar(Text), Length(Text), nil);
{$ELSE}
  StrPCopy (S, Text);
  ExtTextOut(Canvas.Handle, tLeft, tTop, ETO_OPAQUE or ETO_CLIPPED, @ARect,
    S, StrLen(S), nil);
{$ENDIF}
 Canvas.DrawFocusRect (ARect);
end;


function TComboBoxPlus.CanEdit: Boolean;
{CanEdit should return true if we are in edit mode and false otherwise. This is
most applicable in dataaware controls}
begin
  result := FStyle = csIncSrchEdit;
end;

procedure TComboBoxPlus.edit;
{Puts the control into edit mode}
begin
  {abstract - hear for Dataaware decendent}
end;

function TComboBoxPlus.editing : boolean;
{True if control is in edit mode}
begin
  {non dataawre version is always true}
  Result := True;
end;

function TComboBoxPlus.CanModify : Boolean;
{ In a data aware control this sould check the Datalink to see if the field data
  can be changed }
begin
  Result := True;
end;

procedure TComboBoxPlus.DataModified;
begin
end;

procedure TComboBoxPlus.EnterLookupRec;
var
  cancelled : Boolean;
begin
  Cancelled := True;
  if Assigned(FOnNewLookupRec) then FOnNewLookupRec(Self, Cancelled);
  If Cancelled then Reset;
end;

procedure TComboBoxPlus.UpdateRecord;
begin
   if (FStyle in [csIncSrchEdit]) and
    (AnsiCompareText(Text,
     TDDLinkPlus(DDLInk).Cols[1].Strings[TDDLinkPlus(DDLInk).Row]) <> 0) then
     EnterLookupRec;
end;

procedure TComboBoxPlus.DoSearch;
Var
 I : LongInt;
begin
  if Assigned(FOnBeforeSearch) then FOnBeforeSearch(Self);
  For I := FixedRows to RowCount - 1 do
  begin
    If AnsiCompareText(SearchValue, Cols[1].Strings[I])<=0 then
    begin
      TDDLinkPlus(DDLInk).DisplayValue := Cols[1].Strings[I];
      Break;
    end;
  end;
  if Assigned(FOnAfterSearch) then FOnAfterSearch(Self);
end;

procedure TComboBoxPlus.DoSelectSome;
var
  Str : String;
  I,X : Word;
begin
  Str := SearchValue;
  X := 0;
  For I := 1 to Length(Str) do
  begin
    X := I;
    If UpCase(Str[I]) <> UpCase(Text[I]) then break;
  end;
  SelectText(MaxSel, X);
end;

procedure TComboBoxPlus.LookupRecChanged(byIncSearch: Boolean);
{ This is the caller for OnLookupRecChanged event }
begin
  if Assigned(FOnLookupRecChanged) then
    FOnLookupRecChanged(Self, byIncSearch);
end;

procedure TComboBoxPlus.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  FLastKey := key;
  FLastChar := #0;
  if Key in [VK_BACK, VK_DELETE, VK_INSERT] then
  begin
    if Editable then edit;
    if not CanEdit then
      Key := 0;
  end
  else if not Editable and (Key in [VK_LEFT, VK_RIGHT]) then
    Key := 0;
  if (Editable and (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR])) or
     (not Editable and (Key in [VK_HOME, VK_END, VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR])) then
  begin
    if DDLink.DropDownObj = nil then
      DropDown
    else
    begin
      if (FStyle in [csIncSearch]) then searchvalue := '';
      Edit;
      if editing then
         TPopupGrid(DDLink.DropDownObj).KeyDown (Key, Shift);
    end;
    Key := 0;
  end;
end;

procedure TComboBoxPlus.KeyPress(var Key: Char);
var
  SchResult,Str : String;
  X, I : word;
  FGrid : TPopupGrid;
begin
  inherited KeyPress(Key);
  FGrid := TPopupGrid(DDLink.DropDownObj);
  case Key of
    ^H, ^V, ^X, #32..#255:     {BS, Insert, Down}
      begin
        if Editable or (FStyle in [csIncSearch]) then Edit;
        if TDDLinkPlus(DDLink).fSorted then
        begin
          if not CanModify then          {this section protects the}
            Key := #0;               {Displayed data from changing when the}
                                     {DataSource is read only}
          if (DDLink <> nil) and not TDDLinkPlus(DDLink).fIsSorted then
          begin
            TDDLinkPlus(DDLink).SortList(1);
            searchvalue := '';
          end;
          if (FGrid = nil) and AutoDropDown then DropDown;
          FLastChar := Key;
          If (FStyle = csIncSearch) then
          begin
            Str := SearchValue;
            if (FLastChar = chr(8)) then   {BackSpace}
              delete(Str,length(SearchValue) ,1)
            else if FLastChar <> #0 then
              Str := Str + FLastChar;
            Searchvalue := Str;
            If (Searchvalue > '') then
            begin
              While True do
              begin
                DoSearch;
                SchResult := TDDLinkPlus(DDLink).Cols[1].Strings[TDDLinkPlus(DDLink).Row];
                if (length(SchResult) = 0) then  //case where there is nothing in the dropdown list
                begin
                  Key := #0;
                  exit;
                end;
                  
                I := 0;
                For X := 1 to Length(Str) do
                begin
                  I := X;
                  If (UpCase(Str[I]) <> UpCase(SchResult[I])) then break;
                end;
                If (I < Length(Str)) or
                  ((SchResult[I] = ' ') and     {where there are spaces between words - Del Mar Heights}
                  (UpCase(Str[I]) <> UpCase(SchResult[I]))) then  {case of last char different}
                begin
                  Delete(str, Length(str), 1);
                  Searchvalue := Str;
                  If Length(Str) < 1 then break;
                end
                else BREAK;
              end;
            end
            else {if they backspaced into null, Ok}
              if FLastChar = chr(8) then
              begin
                DoSearch;
                SchResult := '';   {needed to suport numeric entries via before/aftersearch event}
              end;
            DisplayValue := SchResult{or FGrid.FDisplayFld.asString};
            DoSelectSome;
          end;
        end; {if FStyle in ....}
        if not CanEdit then
          Key := #0;
      end;
    Char (VK_RETURN):
    begin
      if (DDLink.DropDownObj <> nil) then CloseUp;
      Key := #0;  { must catch and remove this, since is actually multi-line }
    end;
    Char (VK_ESCAPE):
      begin
        if DDLink.DropDownObj = nil then
          Reset
        else
          CloseUp;
        if editable then SelectAll; 
        if (FStyle in [csIncSearch, csIncSrchEdit]) then SelectText(MaxSel, 0);
        Key := #0;
      end;
  end;
end;

procedure TComboBoxPlus.Change;
var
  SchResult,Str : String;
  I,X : Word;
  OldStart : Integer;
  StrNul : Array[0..255] of char;

  function IsSearchStrInResult(SString, SResult : String) : Word;
  {returns 0 if SSTring is the first characters of SResult otherwise
   returns the index of the character in SResult where they are different}
  Var
    I,X : Word;
    OK : Boolean;
  begin
    Ok := True;
    Result := 0;
    X := 0;
    if SResult = '' then exit;
    For I := 1 To Length(SString) do
    begin
      X := I;
      If UpCase(SString[I]) <> UpCase(SResult[I]) then
      begin
        OK := False;
        break;
      end;
    end;
    If Not OK then Result := X;
  end;

begin
  inherited Change;
  If (FStyle = csIncSrchEdit) and TDDLinkPlus(DDLink).fIsSorted and
    not(FLastChar in [#0]) then
  begin
    Str := Text;
    If Length(Str) < Length(SearchValue) then
      SearchValue := Str;
    if FLastKey = VK_BACK then
      delete(Str,length(SearchValue) ,1)
    else If (Str > '') then
    begin
      if (UpperCase(Str)<>UpperCase(SearchValue)) Then
      begin
        SearchValue := Str;
        DoSearch;
        SchResult := TDDLinkPlus(DDLink).Cols[1].Strings[TDDLinkPlus(DDLink).Row];
        If length(SchResult) = 0 then exit; //case where there is nothing in the dropdown list
        If (IsSearchStrInResult(Str, SchResult) = 0) then                                        {New 3/12/96}
        begin {Search String is in search result so auto fill}
          FAutoFill := True;
          Perform(WM_SETTEXT, 0, Longint(StrPCopy(StrNul, SchResult)));
          X := 0;
          For I := 1 to Length(Str) do
          begin
            X := I;
            If UpCase(Str[I]) <> UpCase(SchResult[I]) then break;
          end;
          SelectText(MaxSel, X);
          LookupRecChanged(True);
        end
        else {Search String not in Search Result so stop autofill}
        begin
          OldStart := SelStart;
          If FAutoFill then
          begin
            Perform(WM_SETTEXT, 0, Longint(StrPCopy(StrNul, Str)));
            SelStart := OldStart;
            FAutoFill := False;
          end;
        end;
      end;
    end;
    TDDLinkPlus(DDLink).DisplayValue :=
      TDDLinkPlus(DDLink).Cols[1].Strings[TDDLinkPlus(DDLink).Row];
  end;
end;

procedure TComboBoxPlus.SetSorted(const Value: Boolean);
begin
  if Value <> TDDLinkPlus(DDLink).FSorted then 
  begin
    TDDLinkPlus(DDLink).FSorted := Value;
    TDDLinkPlus(DDLink).FIsSorted := False;  {either way make sure it resorts if needed}
    if TDDLinkPlus(DDLink).FSorted then TDDLinkPlus(DDLink).SortList(1);
  end;  
end;

function TComboBoxPlus.GetSorted : Boolean;
begin
  Result := TDDLinkPlus(DDLink).FSorted;
end;

function TComboBoxPlus.GetListVisible : boolean;
begin
  result := TDDLinkPlus(DDLink).Visible;
end;

procedure TComboBoxPlus.WMPaste (var Message: TMessage);
begin
  if Editable then Edit;
  inherited;
end;

procedure TComboBoxPlus.WMCut (var Message: TMessage);
begin
  if Editable then Edit;
  inherited;
end;


{------------------------------------------------------------}
             { THIS next is all the dropdown grid }
{------------------------------------------------------------}
                        { TDrawGridP }
{------------------------------------------------------------}
constructor TDrawGridP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Options := [goRowSelect];
  FixedCols := 0;
end;

function TDrawGridP.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TDrawGridP.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

function TDrawGridP.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
end;

function TDrawGridP.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
end;

function TDrawGridP.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
end;

procedure TDrawGridP.SetEditText(ACol, ARow: Longint; const Value: string);
begin
end;

procedure TDrawGridP.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
  if Assigned(FOnDrawCell) then FOnDrawCell(Self, ACol, ARow, ARect, AState);
end;

procedure TDrawGridP.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

{$IFDEF Win32}

{ StrItem management, shared by TStringList and TStringSparseList }

type
  PStrItem = ^TStrItem;
  TStrItem = record
    FObject: TObject;
    FString: string;
  end;

function NewStrItem(const AString: string; AObject: TObject): PStrItem;
begin
  New(Result);
  Result^.FObject := AObject;
  Result^.FString := AString;
end;

procedure DisposeStrItem(P: PStrItem);
begin
  Dispose(P);
end;

{$ENDIF}

{ Sparse array classes for TStringGridP }

type

  PPointer = ^Pointer;

{ Exception classes }

  EStringSparseListError = class(Exception);

{ TSparsePointerArray class}

{ Used by TSparseList.  Based on Sparse1Array, but has Pointer elements
  and Integer index, just like TPointerList/TList, and less indirection }

  { Apply function for the applicator:
        TheIndex        Index of item in array
        TheItem         Value of item (i.e pointer element) in section
        Returns: 0 if success, else error code. }
  TSPAApply = function(TheIndex: Integer; TheItem: Pointer): Integer;

  TSecDir = array[0..4095] of Pointer;  { Enough for up to 12 bits of sec }
  PSecDir = ^TSecDir;
  TSPAQuantum = (SPASmall, SPALarge);   { Section size }

  TSparsePointerArray = class(TObject)
  private
    secDir: PSecDir;
    slotsInDir: Word;
    indexMask, secShift: Word;
    FHighBound: Integer;
    FSectionSize: Word;
    cachedIndex: Integer;
    cachedPointer: Pointer;
    { Return item[i], nil if slot outside defined section. }
    function  GetAt(Index: Integer): Pointer;
    { Return address of item[i], creating slot if necessary. }
    function  MakeAt(Index: Integer): PPointer;
    { Store item at item[i], creating slot if necessary. }
    procedure PutAt(Index: Integer; Item: Pointer);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;

    { Traverse SPA, calling apply function for each defined non-nil
      item.  The traversal terminates if the apply function returns
      a value other than 0. }
    { NOTE: must be static method so that we can take its address in
      TSparseList.ForAll }
    function  ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;

    { Ratchet down HighBound after a deletion }
    procedure ResetHighBound;

    property HighBound: Integer read FHighBound;
    property SectionSize: Word read FSectionSize;
    property Items[Index: Integer]: Pointer read GetAt write PutAt; default;
  end;

{ TSparseList class }

  TSparseList = class(TObject)
  private
    FList: TSparsePointerArray;
    FCount: Integer;    { 1 + HighBound, adjusted for Insert/Delete }
    FQuantum: TSPAQuantum;
    procedure NewList(Quantum: TSPAQuantum);
  protected
    procedure Error; virtual;
    function  Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
{    function  Add(Item: Pointer): Integer; }
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
{    function  First: Pointer; }
    function  ForAll(ApplyFunction: Pointer {TSPAApply}): Integer;
{    function  IndexOf(Item: Pointer): Integer; }
    procedure Insert(Index: Integer; Item: Pointer);
{    function  Last: Pointer; }
{    procedure Move(CurIndex, NewIndex: Integer); }
{    procedure Pack; }
{    function  Remove(Item: Pointer): Integer; }
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

{ TStringSparseList class }

  TStringSparseList = class(TStrings)
  private
    FList: TSparseList;                 { of StrItems }
    FOnChange: TNotifyEvent;
  protected
    function  Get(Index: Integer): String; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure Changed; virtual;
    procedure Error; virtual;
  public
    constructor Create(Quantum: TSPAQuantum);
    destructor  Destroy; override;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Clear; override;
    property List: TSparseList read FList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$IFNDEF Win32}
type
  TFarPtr = record
    case Boolean of
      False: (Ptr: Pointer);
      True:  (Ofs, Seg: Word)
  end;
{$ENDIF}

{ TSparsePointerArray }

const
  SPAIndexMask: array[TSPAQuantum] of Byte = (15, 255);
  SPASecShift: array[TSPAQuantum] of Byte = (4, 8);

{ Expand Section Directory to cover at least `newSlots' slots. Returns: Possibly
  updated pointer to the Section Directory. }
function  ExpandDir(secDir: PSecDir; var slotsInDir: Word;
  newSlots: Word): PSecDir;
{$IFDEF Win32}
begin
  Result := secDir;
  ReallocMem(Result, newSlots * SizeOf(Pointer));
  FillChar(Result^[slotsInDir], (newSlots - slotsInDir) * SizeOf(Pointer), 0);
  slotsInDir := newSlots;
end;
{$ELSE}
var
  dirP: PSecDir;
  oldSlots: Word;
begin
  GetMem(dirP, newSlots * SizeOf(Pointer));
  oldSlots := slotsInDir;
  slotsInDir := newSlots;
  if oldSlots > 0 then begin
    Move(secDir^, dirP^, oldSlots * SizeOf(Pointer));
    FreeMem(secDir, oldSlots * SizeOf(Pointer))
  end;
  FillChar(dirP^[oldSlots], (newSlots - oldSlots) * SizeOf(Pointer), 0);
  expandDir := dirP
end;
{$ENDIF}

{ Allocate a section and set all its items to nil. Returns: Pointer to start of
  section. }
function  MakeSec(SecIndex: Integer; SectionSize: Word): Pointer;
var
  SecP: Pointer;
  Size: Word;
begin
  Size := SectionSize * SizeOf(Pointer);
  GetMem(secP, size);
  FillChar(secP^, size, 0);
  MakeSec := SecP
end;

constructor TSparsePointerArray.Create(Quantum: TSPAQuantum);
begin
  SecDir := nil;
  SlotsInDir := 0;
  FHighBound := -1;
  FSectionSize := Word(SPAIndexMask[Quantum]) + 1;
  IndexMask := Word(SPAIndexMask[Quantum]);
  SecShift := Word(SPASecShift[Quantum]);
  CachedIndex := -1
end;

destructor TSparsePointerArray.Destroy;
var
  i:  Integer;
  size: Word;
begin
  { Scan section directory and free each section that exists. }
  i := 0;
  size := FSectionSize * SizeOf(Pointer);
  while i < slotsInDir do begin
    if secDir^[i] <> nil then
      FreeMem(secDir^[i], size);
    Inc(i)
  end;

  { Free section directory. }
  if secDir <> nil then
    FreeMem(secDir, slotsInDir * SizeOf(Pointer));
end;

function  TSparsePointerArray.GetAt(Index: Integer): Pointer;
{$IFDEF Win32}
var
  byteP: PChar;
  secIndex: Cardinal;
begin
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If not null, index into
    Section using low order part of index. }
  if Index = cachedIndex then
    Result := cachedPointer
  else begin
    secIndex := Index shr secShift;
    if secIndex >= slotsInDir then
      byteP := nil
    else begin
      byteP := secDir^[secIndex];
      if byteP <> nil then begin
        Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
      end
    end;
    if byteP = nil then Result := nil else Result := PPointer(byteP)^;
    cachedIndex := Index;
    cachedPointer := Result
  end
end;
{$ELSE}
var
  byteP: PChar;
  secIndex, shift: Cardinal;
begin
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If not null, index into
    Section using low order part of index. }
  if Index = cachedIndex then
    Result := cachedPointer
  else begin
    shift := secShift;
    asm
      mov   ax,Index
      mov   cx,shift
      shr   ax,cl
      mov   secIndex,ax
    end;
    if secIndex >= slotsInDir then
      byteP := nil
    else begin
      byteP := secDir^[secIndex];
      if byteP <> nil then begin
        shift := indexMask;
        asm
          mov   ax,Index
          and   ax,shift
          shl   ax,2
          mov   shift,ax
        end;
        Inc(byteP, shift)
      end
    end;
    if byteP = nil then Result := nil else Result := PPointer(byteP)^;
    cachedIndex := Index;
    cachedPointer := Result
  end
end;
{$ENDIF}

function  TSparsePointerArray.MakeAt(Index: Integer): PPointer;
var
  dirP: PSecDir;
  p: Pointer;
  byteP: PChar;
  secIndex: Word;
begin
{  byteP := nil; }
  { Expand Section Directory if necessary. }
  secIndex := Index shr secShift;       { Unsigned shift }
  if secIndex >= slotsInDir then
    dirP := expandDir(secDir, slotsInDir, secIndex + 1)
  else
    dirP := secDir;
  { Index into Section Directory using high order part of
    index.  Get pointer to Section. If null, create new
    Section.  Index into Section using low order part of index. }
  secDir := dirP;
  p := dirP^[secIndex];
  if p = nil then begin
    p := makeSec(secIndex, FSectionSize);
    dirP^[secIndex] := p
  end;
  byteP := p;
  Inc(byteP, (Index and indexMask) * SizeOf(Pointer));
  if Index > FHighBound then
    FHighBound := Index;
  Result := PPointer(byteP);
  cachedIndex := -1
end;

procedure TSparsePointerArray.PutAt(Index: Integer; Item: Pointer);
begin
  if (Item <> nil) or (GetAt(Index) <> nil) then
  begin
    MakeAt(Index)^ := Item;
    if Item = nil then
      ResetHighBound
  end
end;

function  TSparsePointerArray.ForAll(ApplyFunction: Pointer):
  Integer;
var
  itemP: PChar;                         { Pointer to item in section }
{$IFDEF Win32}
  item: Pointer;
{$ELSE}
  item: TFarPtr;
{$ENDIF}
  i, callerBP: Cardinal;
  j, index: Integer;
begin
  { Scan section directory and scan each section that exists,
    calling the apply function for each non-nil item.
    The apply function must be a far local function in the scope of
    the procedure P calling ForAll.  The trick of setting up the stack
    frame (taken from TurboVision's TCollection.ForEach) allows the
    apply function access to P's arguments and local variables and,
    if P is a method, the instance variables and methods of P's class }
  Result := 0;
  i := 0;
{$IFDEF Win32}
  asm
    mov   eax,[ebp]                     { Set up stack frame for local }
    mov   callerBP,eax
  end;
{$ELSE}
  asm
    mov   ax,[bp]                       { Set up stack frame for local }
    and   al,$FE
    mov   callerBP,ax
  end;
{$ENDIF}
  while (i < slotsInDir) and (Result = 0) do begin
    itemP := secDir^[i];
    if itemP <> nil then begin
      j := 0;
      index := i shl SecShift;
      while (j < FSectionSize) and (Result = 0) do begin
{$IFDEF Win32}
        item := PPointer(itemP)^;
        if item <> nil then
          { ret := ApplyFunction(index, item.Ptr); }
          asm
            mov   eax,index
            mov   edx,item
            push  callerBP
            call  ApplyFunction
            pop   ecx
            mov   @Result,eax
          end;
        Inc(itemP, SizeOf(Pointer));
        Inc(j);
        Inc(index)
{$ELSE}
        item.Ptr := PPointer(itemP)^;
        if item.Ptr <> nil then
          { ret := ApplyFunction(index, item.Ptr); }
          asm
            push  index
            push  word ptr item+2       { .Seg }
            push  word ptr item         { .Ofs }
            push  callerBP              { Outer proc's BP for nested proc }
            call  ApplyFunction
            mov   @Result,ax
          end;
        Inc(itemP, SizeOf(Pointer));
        Inc(j);
        Inc(index)
{$ENDIF}
      end
    end;
    Inc(i)
  end;
end;

procedure TSparsePointerArray.ResetHighBound;
var
  NewHighBound: Integer;

  function  Detector(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    if TheIndex > FHighBound then
      Result := 1
    else
    begin
      Result := 0;
      if TheItem <> nil then NewHighBound := TheIndex
    end
  end;

begin
  NewHighBound := -1;
  ForAll(@Detector);
  FHighBound := NewHighBound
end;

{ TSparseList }

constructor TSparseList.Create(Quantum: TSPAQuantum);
begin
  NewList(Quantum)
end;

destructor TSparseList.Destroy;
begin
  if FList <> nil then FList.Destroy
end;
(*
function  TSparseList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  FList[Result] := Item;
  Inc(FCount)
end;
*)
procedure TSparseList.Clear;
begin
  FList.Destroy;
  NewList(FQuantum);
  FCount := 0
end;

procedure TSparseList.Delete(Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then Exit;
  for I := Index to FCount - 1 do
    FList[I] := FList[I + 1];
  FList[FCount] := nil;
  Dec(FCount);
end;

procedure TSparseList.Error;
begin
{$IFDEF Win32}
  {$IFDEF VER90} //Delphi 2.0
  raise EListError.CreateRes(SListIndexError);
  {$ENDIF}
  {$IFDEF VER100}  //Delphi 3.0
  raise EListError.Create(SListIndexError);
  {$ENDIF}
{$ELSE}
  raise EListError.Create(LoadStr(SListIndexError))
{$ENDIF}
end;

procedure TSparseList.Exchange(Index1, Index2: Integer);
var
  temp: Pointer;
begin
  temp := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, temp);
end;
(*
function  TSparseList.First: Pointer;
begin
  Result := Get(0)
end;
*)
{$IFDEF Win32}
function TSparseList.ForAll(ApplyFunction: Pointer {TSPAApply}): Integer; assembler;
{ Jump to TSparsePointerArray.ForAll so that it looks like it was called
  from our caller, so that the BP trick works. }
asm
        MOV     EAX,[EAX].TSparseList.FList
        JMP     TSparsePointerArray.ForAll
end;
{$ELSE}
{ This is pretty gross, but what we want to do is to call
  TSparsePointerArray.ForAll as though it had been called
  directly by our caller, so that the BP trick works. }
var
  ForAllPtr: Pointer;                   { Scratch, but cannot be on stack }

function TSparseList.ForAll(ApplyFunction: Pointer {TSPAApply}): Integer; assembler;
asm
        les     di,Self                     { Replace Self with ... }
        mov     ax,es:[di].TSparseList.Flist.Word[0]
        mov     Self.Word[0],ax                   { TSparseList.FList }
        mov     ax,es:[di].TSparseList.Flist.Word[2]
        mov     Self.Word[2],ax
        mov     sp,bp                       { Remove our frame }
        pop     bp
        and     bp,$FFFE
        jmp     TSparsePointerArray.ForAll
end;
{$ENDIF}

function  TSparseList.Get(Index: Integer): Pointer;
begin
  if Index < 0 then Error;
  Result := FList[Index]
end;
(*
function  TSparseList.IndexOf(Item: Pointer): Integer;
var
  MaxIndex, Index: Integer;
  function  IsTheItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    if TheIndex > MaxIndex then
      Result := -1                      { Bail out }
    else if TheItem <> Item then
      Result := 0
    else begin
      Result := 1;                      { Found it, stop traversal }
      Index := TheIndex
    end
  end;
begin
  Index := -1;
  MaxIndex := FList.HighBound;
  FList.ForAll(@IsTheItem);
  Result := Index
end;
*)
procedure TSparseList.Insert(Index: Integer; Item: Pointer);
var
  i: Integer;
begin
  if Index < 0 then Error;
  I := FCount;
  while I > Index do
  begin
    FList[i] := FList[i - 1];
    Dec(i)
  end;
  FList[Index] := Item;
  if Index > FCount then FCount := Index;
  Inc(FCount)
end;
(*
function  TSparseList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TSparseList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;
*)
procedure TSparseList.NewList(Quantum: TSPAQuantum);
begin
  FQuantum := Quantum;
  FList := TSparsePointerArray.Create(Quantum)
end;
(*
procedure TSparseList.Pack;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do if Items[i] = nil then Delete(i)
end;
*)
procedure TSparseList.Put(Index: Integer; Item: Pointer);
begin
  if Index < 0 then Error;
  FList[Index] := Item;
  FCount := FList.HighBound + 1
end;
(*
function  TSparseList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result)
end;
*)

{ TStringSparseList }

constructor TStringSparseList.Create(Quantum: TSPAQuantum);
begin
  inherited Create; //new D6
  FList := TSparseList.Create(Quantum)
end;

destructor  TStringSparseList.Destroy;
begin
  if FList <> nil then begin
    Clear;
    FList.Destroy
  end
end;

procedure TStringSparseList.ReadData(Reader: TReader);
var
  i: Integer;
begin
  with Reader do begin
    i := Integer(ReadInteger);
    while i > 0 do begin
      InsertObject(Integer(ReadInteger), ReadString, nil);
      Dec(i)
    end
  end
end;

procedure TStringSparseList.WriteData(Writer: TWriter);
var
  itemCount: Integer;

  function  CountItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Inc(itemCount);
    Result := 0
  end;

  function  StoreItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    with Writer do
    begin
      WriteInteger(TheIndex);           { Item index }
      WriteString(PStrItem(TheItem)^.FString);
    end;
    Result := 0
  end;

begin
  with Writer do
  begin
    itemCount := 0;
    FList.ForAll(@CountItem);
    WriteInteger(itemCount);
    FList.ForAll(@StoreItem);
  end
end;

procedure TStringSparseList.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('List', ReadData, WriteData, True);
end;

function  TStringSparseList.Get(Index: Integer): String;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then Result := '' else Result := p^.FString
end;

function  TStringSparseList.GetCount: Integer;
begin
  Result := FList.Count
end;

function  TStringSparseList.GetObject(Index: Integer): TObject;
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p = nil then Result := nil else Result := p^.FObject
end;

procedure TStringSparseList.Put(Index: Integer; const S: String);
var
  p: PStrItem;
  obj: TObject;
begin
  p := PStrItem(FList[Index]);
  if p = nil then obj := nil else obj := p^.FObject;
  if S = '' then                        { Null string blanks data and object }
    FList[Index] := nil
  else
    FList[Index] := NewStrItem(S, obj);
  if p <> nil then DisposeStrItem(p);
  Changed
end;

procedure TStringSparseList.PutObject(Index: Integer; AObject: TObject);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then p^.FObject := AObject else if AObject <> nil then Error;
  Changed
end;

procedure TStringSparseList.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self)
end;

procedure TStringSparseList.Error;
begin
{$IFDEF Win32}
  {$IFDEF VER90} //Delphi 2.0
  raise EStringSparseListError.CreateRes(SPutObjectError);
  {$ENDIF}
  {$IFDEF VER100}  //Delphi 3.0
  raise EStringSparseListError.Create(SPutObjectError);
  {$ENDIF}
{$ELSE}
  raise EStringSparseListError.Create(LoadStr(SPutObjectError))
{$ENDIF}
end;

procedure TStringSparseList.Delete(Index: Integer);
var
  p: PStrItem;
begin
  p := PStrItem(FList[Index]);
  if p <> nil then DisposeStrItem(p);
  FList.Delete(Index);
  Changed
end;

procedure TStringSparseList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TStringSparseList.Insert(Index: Integer; const S: String);
begin
  FList.Insert(Index, NewStrItem(S, nil));
  Changed
end;

procedure TStringSparseList.Clear;

  function  ClearItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    DisposeStrItem(PStrItem(TheItem));    { Item guaranteed non-nil }
    Result := 0
  end;

begin
  FList.ForAll(@ClearItem);
  FList.Clear;
  Changed
end;

{ TStringGridPStrings }

{ AIndex < 0 is a column (for column -AIndex - 1)
  AIndex > 0 is a row (for row AIndex - 1)
  AIndex = 0 denotes an empty row or column }

constructor TStringGridPStrings.Create(ALink: TDDLinkPlus; AIndex: Longint);
begin
  inherited Create;
  FLink := ALink;
  FIndex := AIndex;
end;

procedure TStringGridPStrings.Assign(Source: TPersistent);
var
  I, Max: Integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    Max := TStrings(Source).Count - 1;
    if Max >= Count then Max := Count - 1;
    try
      for I := 0 to Max do
      begin
        Put(I, TStrings(Source).Strings[I]);
        PutObject(I, TStrings(Source).Objects[I]);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStringGridPStrings.CalcXY(Index: Integer; var X, Y: Integer);
begin
  if FIndex = 0 then
  begin
    X := -1; Y := -1;
  end else if FIndex > 0 then
  begin
    X := Index;
    Y := FIndex - 1;
  end else
  begin
    X := -FIndex - 1;
    Y := Index;
  end;
end;

{ Changes the meaning of Add to mean copy to the first empty string }
function TStringGridPStrings.Add(const S: string): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Strings[I] = '' then
    begin
      Strings[I] := S;
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TStringGridPStrings.Clear;
var
  SSList: TStringSparseList;
  I: Integer;

  function BlankStr(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    Strings[TheIndex] := '';
    Result := 0;
  end;

begin
  if FIndex > 0 then
  begin
    SSList := TStringSparseList(TSparseList(FLink.FData)[FIndex - 1]);
    if SSList <> nil then SSList.List.ForAll(@BlankStr);
  end
  else if FIndex < 0 then
    for I := Count - 1 downto 0 do Strings[I] := '';
end;

function TStringGridPStrings.Get(Index: Integer): string;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := '' else Result := FLink.Cells[X, Y];
end;

function TStringGridPStrings.GetCount: Integer;
begin
  { AIndex < 0 is a column (for column -AIndex - 1)
    AIndex > 0 is a row (for row AIndex - 1)
    AIndex = 0 denotes an empty row or column }
  { Count of a row is the column count, and vice versa }
  if FIndex = 0 then Result := 0
  else if FIndex > 0 then Result := Integer(FLink.ColCount) {**}
  else Result := Integer(FLink.RowCount);
end;

function TStringGridPStrings.GetObject(Index: Integer): TObject;
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  if X < 0 then Result := nil else Result := FLink.Objects[X, Y];
end;

procedure TStringGridPStrings.Put(Index: Integer; const S: string);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FLink.Cells[X, Y] := S;
end;

procedure TStringGridPStrings.PutObject(Index: Integer; AObject: TObject);
var
  X, Y: Integer;
begin
  CalcXY(Index, X, Y);
  FLink.Objects[X, Y] := AObject;
end;

procedure TStringGridPStrings.SetUpdateState(Updating: Boolean);
begin
  if FLink.DropDownObj <> nil then
  TPopupGrid(FLink.DropDownObj).SetUpdateState(Updating);
end;

{ TStringGridP }

constructor TStringGridP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TStringGridP.Destroy;
begin
  inherited Destroy;
end;

procedure TStringGridP.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TStringGridP.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;

function TStringGridP.GetEditText(ACol, ARow: Longint): string;
begin
  Result := Cells[ACol, ARow];
end;

procedure TStringGridP.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  DisableEditUpdate;
  try
    if Value <> Cells[ACol, ARow] then Cells[ACol, ARow] := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TStringGridP.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);

  procedure DrawCellText;
  var
    Text: array[0..255] of Char;
  begin
    StrPCopy(Text, Cells[ACol+1, ARow]); {The +1 Hides column 0 }
    ExtTextOut(Canvas.Handle, ARect.Left + 2, ARect.Top + 2, ETO_CLIPPED or
      ETO_OPAQUE, @ARect, Text, StrLen(Text), nil);
  end;
begin
  if DefaultDrawing then DrawCellText;
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TStringGridP.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TStringGridP.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

procedure TStringGridP.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
    Invalidate;
    FNeedsUpdating := False;
    fDDLink.SetColumnAttributes;
{***}    Invalidate; {See what happens when you eliminate the first one}
  end;
end;

procedure TStringGridP.xUpdate(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

function TStringGridP.GetCells(ACol, ARow: Integer): string;
begin
  result := fDDLink.Cells[ACol, ARow];
end;

function TStringGridP.GetCols(Index: Integer): TStrings;
begin
  result := fDDLink.Cols[Index];
end;

function TStringGridP.GetObjects(ACol, ARow: Integer): TObject;
begin
  result := fDDLink.Objects[ACol, ARow];
end;

function TStringGridP.GetRows(Index: Integer): TStrings;
begin
  result := fDDLink.Rows[Index];
end;

procedure TStringGridP.SetCells(ACol, ARow: Integer; const Value: string);
begin
  fDDLink.Cells[ACol, ARow] := value;
end;

procedure TStringGridP.SetCols(Index: Integer; Value: TStrings);
begin
  fDDLink.Cols[Index] := value;
end;

procedure TStringGridP.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  fDDLink.Objects[ACol, ARow] := Value;
end;

procedure TStringGridP.SetRows(Index: Integer; Value: TStrings);
begin
  fDDLink.Rows[Index] := value;
end;


procedure TStringGridP.SetOptions(Value: TStringGridPOptionss);
var
  NewGridOptions: TGridOptions;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    NewGridOptions := [goRowSelect];
    if loColLines in Value then
      NewGridOptions := NewGridOptions + [goFixedVertLine, goVertLine];
    if loRowLines in Value then
      NewGridOptions := NewGridOptions + [goFixedHorzLine, goHorzLine];
    if loThumbTracking in Value then
      NewGridOptions := NewGridOptions + [goThumbTracking];
    inherited Options := NewGridOptions;
  end;
end;

function TStringGridP.GetValue: string;
begin
  result := fDDLink.Value;
end;

procedure TStringGridP.SetValue(const Value: string);
begin
  fDDLink.value := value;
end;


procedure TStringGridP.MouseDown(msButton: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CellHit: TGridCoord;
  MyOnMouseDown: TMouseEvent;
begin
  if not (csDesigning in ComponentState) and CanFocus and TabStop then
  begin
    SetFocus;
    if ValidParentForm(Self).ActiveControl <> Self then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if ssDouble in Shift then
  begin
    DblClick;
    Exit;
  end;
  if (msButton = mbLeft) then
  begin
    CellHit := MouseCoord (X, Y);
    if (CellHit.Y >= FixedRows) then
    begin
      FGridState := gsSelecting;
      SetTimer(Handle, 1, 60, nil);
      if CellHit.Y <> row then
      begin
        InvalidateRow(row);
        InvalidateRow(CellHit.Y);
      end;
      Row := CellHit.Y;
    end;
  end;
  MyOnMouseDown := OnMouseDown;
  if Assigned(MyOnMouseDown) then MyOnMouseDown(Self, msButton, Shift, X, Y);
end;

procedure TStringGridP.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);
end;

procedure TStringGridP.MouseUp(msButton: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldState: TGridState;
begin
  OldState := FGridState;
  inherited MouseUp(msButton, Shift, X, Y);
  if OldState = gsSelecting then
    ListClick;
end;

procedure TStringGridP.ListClick;
{ If LookupActive then value is column 0, if not LookupActive then there really is no lookup
  so return whats in col 1}
begin
  fDDLink.fDisplayValue := Cols[1].Strings[row];
  if fDDLink.FLookupActive then
    fDDLink.FValue := Cols[0].Strings[row]
  else
    fDDLink.FValue := Cols[1].Strings[row];
  if Assigned (FOnListClick) then FOnListClick(Self);
end;

procedure TStringGridP.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  if (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR, VK_HOME, VK_END])  then
     ListClick;
end;

{-----------------------------------------------------------}
{-----------------------------------------------------------}
{-----------------------------------------------------------}
{ TPopupGrid }

constructor TPopupGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF Win32}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
{$ENDIF}
  TabStop := False;
end;

procedure TPopupGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{$IFDEF Win32}
  {this is so the dropdown window will not be added to the task bar }
  with Params do
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
{$ENDIF}
  Params.WindowClass.Style := CS_SAVEBITS;
end;

procedure TPopupGrid.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
{$IFDEF Win32}
    Windows.SetParent(Handle, 0);
{$ELSE}
    WinProcs.SetParent(Handle, 0);
{$ENDIF}
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TPopupGrid.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  with Message do
    FCombo.CloseUp;
end;

procedure TPopupGrid.MouseDown(msButton: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FCombo.Edit;
  inherited MouseDown(msButton, Shift, X, Y);
end;

{----------------------------TDDLinkPlus -----------------------}
type
 { PIntArray is a dynamically allocated array that is used to hold the widths and
   heights of the cell in the dropdown grid note that the array is never declared
   only used as a typecast on a pointer type that has been dynamically allocated. }
  PIntArray = ^TIntArray;
  TIntArray = array[0..16000] of Integer;


{$IFDEF D3OR4OR5} {Delphi 3 or 4 or 5}
procedure InvalidOp(ErrStr: String);
begin
  raise EInvalidOperation.Create(ErrStr);
end;
{$ELSE}  {Not Delphi 3 or 4}
procedure InvalidOp(id: Word);
begin
  raise Exception.Create(LoadStr(id));
end;
{$ENDIF}

Constructor TDDLinkPlus.Create(aOwner: TCBXBase);
begin
  inherited Create(AOwner);
  Initialize;
  FLookupActive := False;
  FSorted := True;
  FIsSorted := False;
  FDefaultRowHeight := 24;
  fColWidths := nil;
  fRowHeights := nil;
  FColCount := 2;
  FRowCount := 1;
  FFixedRows := 0;
  FDropDownCount := 8;
  Options := [loThumbTracking];
  fListColor := clWindow;
  fTitleColor := clBtnFace;
  fListParentColor := false;
  FListDefaultDrawing := true;
end;

destructor TDDLinkPlus.Destroy;
  function FreeItem(TheIndex: Integer; TheItem: Pointer): Integer; far;
  begin
    TObject(TheItem).Free;
    Result := 0;
  end;
begin
  if FRows <> nil then
  begin
    TSparseList(FRows).ForAll(@FreeItem);
    TSparseList(FRows).Free;
  end;
  if FCols <> nil then
  begin
    TSparseList(FCols).ForAll(@FreeItem);
    TSparseList(FCols).Free;
  end;
  if FData <> nil then
  begin
    TSparseList(FData).ForAll(@FreeItem);
    TSparseList(FData).Free;
  end;
  if assigned(FColWidths) then FreeMem(FColWidths, ColCount * SizeOf(Integer));
  if assigned(FRowHeights) then FreeMem(FColWidths, RowCount * SizeOf(Integer));
  inherited Destroy;
end;

procedure TDDLinkPlus.Initialize;
var
  quantum: TSPAQuantum;
begin
  if FCols = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    FCols := TSparseList.Create(quantum);
  end;
  if RowCount > 256 then quantum := SPALarge else quantum := SPASmall;
  if FRows = nil then FRows := TSparseList.Create(quantum);
  if FData = nil then FData := TSparseList.Create(quantum);
end;

procedure TDDLinkPlus.SortList(SortCol : Integer);
var
  TmpStr : String;
  TmpObj : TObject; { wd 5/8/1998 }
  I,  S,  Dest : Integer;
  SelectRow : Integer;
begin
  SelectRow := fRow;
  Dest := Cols[1].Count - 1;
  While Dest > FixedRows do
  begin
    S := FixedRows;
    { Find the highest value in column 1 that hasn't been sorted yet}
    For I := FixedRows to Dest do
    begin
      if AnsiCompareText(Cols[SortCol].Strings[S],
                         Cols[SortCol].Strings[I])< 0 then
         S := I;
    end;
    { Now move the highest value into the destination, destination goes where source was}
    If S <> Dest then
      For I := 0 to Rows[S].Count-1 do
      begin
        TmpStr := Rows[S].Strings[I];
        TmpObj := Rows[S].Objects[I];                                 { !!!! wd 8/5/98 }
        Rows[S].Strings[I] := Rows[Dest].Strings[I];
        Rows[S].Objects[I] := Rows[Dest].Objects[I];                  { !!!! wd 8/5/98 }
        Rows[Dest].Strings[I] := TmpStr;
        Rows[Dest].Objects[I] := TmpObj;                              { !!!! wd 8/5/98 }
      end;
      { Make sure the selected row moves with the sort }
      If S = SelectRow then
        SelectRow := Dest
      else if Dest = SelectRow then
        SelectRow := S;
    Dec(Dest);
  end;
  fRow := SelectRow;
  FIsSorted := True;
end;

function TDDLinkPlus.AddRow(Values: array of String): Integer;
var
  I : Integer;
begin
  result := 0;
  for I := 0 to High(Values) do
  begin
    if Values[I] = '' then values[I] := ' ';
    result := Cols[I].Add(Values[I]);
  end;
  fIsSorted := False;
end;


procedure TDDLinkPlus.GridClick (Sender: TObject);
begin
  TComboBoxPlus(Owner).Edit;
  if TComboBoxPlus(Owner).Editing then
  begin
    TComboBoxPlus(Owner).DataModified;
    TComboBoxPlus(Owner).Text := DisplayValue;
    TComboBoxPlus(Owner).LookupRecChanged(False);
  end;
end;

procedure TDDLinkPlus.SetDropDownWidth(value : Integer);
begin
  if value <> fDropDownWidth then
  begin
    fDropDownWidth := value;
    if DropDownObj <> nil then
      TPopupGrid(DropDownObj).width := fDropDownWidth;
  end;
end;


procedure ModifyExtents(var Extents: Pointer; Index, Amount: Longint;
  Default: Integer);
var
  LongSize: LongInt;
  NewSize: Word;
  OldSize: Word;
  I: Word;
begin
  if Amount <> 0 then
  begin
    if not Assigned(Extents) then OldSize := 0
    else OldSize := PIntArray(Extents)^[0];
    if (Index < 0) or (OldSize < Index) then InvalidOp(SIndexOutOfRange);
    LongSize := OldSize + Amount;
    if LongSize < 0 then  InvalidOp(STooManyDeleted)
    else if LongSize >= MaxListSize - 1 then InvalidOp(SGridTooLarge);
    NewSize := Cardinal(LongSize);
{$IFNDEF Win32}
    if Assigned(Extents) then Inc (OldSize);
{$ENDIF}
    if NewSize > 0 then Inc(NewSize);
{$IFDEF Win32}
    ReallocMem(Extents, NewSize * SizeOf(Integer));
{$ELSE}
    Extents := ReallocMem(Extents, OldSize * SizeOf(Integer),
      NewSize * SizeOf(Integer));
{$ENDIF}
    if Assigned(Extents) then
    begin
      I := Index;
      while I < NewSize do
      begin
        PIntArray(Extents)^[I] := Default;
        Inc(I);
      end;
      PIntArray(Extents)^[0] := NewSize-1;
    end;
  end;
end;

procedure UpdateExtents(var Extents: Pointer; NewSize: Longint;
  Default: Integer);
var
  OldSize: Integer;
begin
  OldSize := 0;
  if Assigned(Extents) then OldSize := PIntArray(Extents)^[0];
  ModifyExtents(Extents, OldSize, NewSize - OldSize, Default);
end;

procedure TDDLinkPlus.ChangeSize(NewColCount, NewRowCount: Longint);
var
  OldColCount, OldRowCount: Longint;
  procedure DoChange;
  begin
    if FColWidths <> nil then
      UpdateExtents(FColWidths, ColCount, 64);
    if FRowHeights <> nil then
      UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
  end;
begin
  OldColCount := FColCount;
  OldRowCount := FRowCount;
  FColCount := NewColCount;
  FRowCount := NewRowCount;
 {here is the row implementation of the above still needs to go in dropdown}
  if FixedRows > NewRowCount then
  begin
    FixedRows := NewRowCount - 1;
    if DropDownObj <> nil then
      TPopupGrid(DropDownObj).FixedRows := FixedRows;
  end;
  try
    DoChange;
  except
    { Could not change size so try to clean up by setting the size back }
    FColCount := OldColCount;
    FRowCount := OldRowCount;
    DoChange;
    raise;
  end;
end;

function TDDLinkPlus.GetColWidths(Index: Longint): Integer;
begin
  if (FColWidths = nil) or (Index >= ColCount) then
    Result := 64
  else
    Result := PIntArray(FColWidths)^[Index + 1];
end;

procedure TDDLinkPlus.SetColWidths(Index: Longint; Value: Integer);
begin
  if FColWidths = nil then
    UpdateExtents(FColWidths, ColCount, 64);
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  PIntArray(FColWidths)^[Index + 1] := Value;
  if DropDownObj <> nil then
    TPopupGrid(DropDownObj).ColWidths[Index] := value;
end;

procedure TDDLinkPlus.SetDefaultRowHeight(Value: Integer);
begin
  if FRowHeights <> nil then UpdateExtents(FRowHeights, 0, 0);
  FDefaultRowHeight := Value;
end;

function TDDLinkPlus.GetRowHeights(Index: Longint): Integer;
begin
  if (FRowHeights = nil) or (Index >= RowCount) then
    Result := DefaultRowHeight
  else
    Result := PIntArray(FRowHeights)^[Index + 1];
end;

procedure TDDLinkPlus.SetRowHeights(Index: Longint; Value: Integer);
begin
  if FRowHeights = nil then
    UpdateExtents(FRowHeights, RowCount, DefaultRowHeight);
  if Index >= RowCount then InvalidOp(SIndexOutOfRange);
  PIntArray(FRowHeights)^[Index + 1] := Value;
end;

procedure TDDLinkPlus.ClearGridData;
var
  R,C : Integer;
begin
  for R := 0 to RowCount-1 do
    for C := 0 to ColCount-1 do
      Cells[C, R]:= '';
  fRow := 0;    // New 5/99 fixes bug where current row is out of range after list is cleared
end;

procedure TDDLinkPlus.SetColumnAttributes;
{ Sets the column widths and also the row height }
var
  I : Integer;

  Function GetColWidth(ColNum : Integer) : Integer;
  var
    DC : HDC;
    SaveFont : hFont;
    Extent: TSize;
    J : Integer;
    s : String;
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, ListFont.Handle);
    Result := 0;
    For J := 0 to RowCount-1 do
    begin
      s := Rows[J].Strings[ColNum];
      if GetTextExtentPoint(DC, @s[1], Length(s), Extent)
        and (Extent.cX > Result) then
          Result := Extent.cX;
    end;
    { now add a little space to the end of the column }
    if GetTextExtentPoint(DC, 'O', Length('O'), Extent) then
      Result := Result + Extent.cX;
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
  end;

  function AdjustRowHeight : Integer;
  { makes sure the row height is big enough }
  var
    DC : HDC;
    SaveFont : hFont;
    Extent: TSize;
    s : String;
  begin
    result := DefaultRowHeight;
    DC := GetDC(0);
    SaveFont := SelectObject(DC, ListFont.Handle);
    s := 'W';
    if GetTextExtentPoint(DC, @s[1], Length(s), Extent) and
       (DefaultRowHeight < (Extent.cY - 2)) then
      result := Extent.cY;
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
  end;

begin
  FTotalW := 0;
  { set the grid's RowCount and ColCount}
  I := 0;
  While Cols[1].Strings[I] <> '' do Inc(I);
  RowCount := I;
  I := 0;
  While Rows[0].Strings[I] <> '' do Inc(I);
  ColCount := I;
  { Now set the individual Column widths }
  For I := 1 to ColCount-1 do {minus one because the grid has one less column then the data}
  begin
    ColWidths[I-1] := GetColWidth(I);
    FTotalW := FTotalW + ColWidths[I-1] + 1;  {1 is grid line width}
  end;
  FTotalW := FTotalW + 1;  {1 is grid line width}
{$IFDEF Win32}
  FTotalW := FTotalW + 2;
{$ENDIF}
  DefaultRowHeight := AdjustRowHeight;
end;

procedure TDDLinkPlus.DropDown;
var
  I : Integer;
  P: TPoint;
  ItemCount: Integer;
  GridWidth, OldGridWidth, GridHeight, BorderWidth: Integer;
  DownOK, UpOK, LeftOK, RightOK : Boolean;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (TPopupGrid(DropDownObj).ScrollBars = ssBoth) or
      ((Code = SB_HORZ) and (TPopupGrid(DropDownObj).ScrollBars = ssHorizontal)) or
      ((Code = SB_VERT) and (TPopupGrid(DropDownObj).ScrollBars = ssVertical)) then
    begin
      GetScrollRange(TPopupGrid(DropDownObj).Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

begin
  {Make sure list is sorted if it should be}
  if fSorted and not fIsSorted then  SortList(1);
  { First Create the DropDown Object}
  DropDownObj := TPopupGrid.Create(Owner);
  { Set visible to false so setting the parent doesn't try updating the screen}
  DropDownObj.visible := false;
  { Set the Dropdowns parent to be the edit controls owner (the form & not the edit)}
{  DropDownObj.Parent := TComboBoxPlus(Owner).Parent; }
  DropDownObj.Parent := TComboBoxPlus(Owner);
  { Set the pointer to this link object in the dropdown }
  TPopupGrid(DropDownObj).FDDLink := Self;
  { Set the pointer to the edit in the dropdown }
  TPopupGrid(DropDownObj).FCombo := TComboBoxPlus(Owner);
  { Set the needed values in the new dropdown }
  TPopupGrid(DropDownObj).OnClick := GridClick;
  { Set owner draw support properties }
  TPopupGrid(DropDownObj).OnDrawCell := fOnDrawCell;
  TPopupGrid(DropDownObj).DefaultDrawing := fListDefaultDrawing;
  { execute the users onDropDown if defined. Do this prior to the dd properties
    being actually assigned to the DropDownObj so the component user has a
    chance to update the ddproperties just prior to their use.}
  if Assigned(TComboBoxPlus(Owner).FOnDropDown) then
    TComboBoxPlus(Owner).FOnDropDown(TComboBoxPlus(Owner));
  { Now set the DropDownObj properties }
  TPopupGrid(DropDownObj).OnClick := GridClick;
  TPopupGrid(DropDownObj).Options := fOptions;
  TPopupGrid(DropDownObj).DefaultRowHeight := fDefaultRowHeight;
  TPopupGrid(DropDownObj).RowCount := fRowCount;
  TPopupGrid(DropDownObj).ColCount := fColCount - 1;  {because there is one less col in data then in grid }
  TPopupGrid(DropDownObj).FixedRows := fFixedRows;
  TPopupGrid(DropDownObj).Font.Assign(ListFont);
  if FColWidths <> nil then
    For I := 0 to TPopupGrid(DropDownObj).ColCount-1 do
      TPopupGrid(DropDownObj).ColWidths[I] := ColWIdths[I];
  if FRowHeights <> nil then
    For I := 0 to fRowCount-1 do
      TPopupGrid(DropDownObj).RowHeights[I] := RowHeights[I];
  if fRow < fFixedRows then fRow := fFixedRows;
  if TPopupGrid(DropDownObj).Rowcount = 1 then // new 3/24/99
    TPopupGrid(DropDownObj).row := 0           // new 3/24/99
  else                                         // new 3/24/99
    TPopupGrid(DropDownObj).row := fRow;
  TPopupGrid(DropDownObj).col := fCol;
  TPopupGrid(DropDownObj).ParentFont := ListParentFont;
  TPopupGrid(DropDownObj).Cursor := FListCursor;
  TPopupGrid(DropDownObj).Color := FListColor;
  TPopupGrid(DropDownObj).FixedColor := FTitleColor;
  { Calc actual ItemCount of the dropdown (case of RowCount < DropDownCount)}
  IF fRowCount < fDropDownCount then     { for cases when # of records is}
    ItemCount := fRowCount               { less then dropdowncount property}
  else
    ItemCount := fDropDownCount;
  if ItemCount = 0 then ItemCount := 1;
  { Get the top left corner of the edit }
  P := TComboBoxPlus(Owner).ClientOrigin;   { top left corner of TEdit, TPoint}
  { Calc the BorderWidth }
  BorderWidth := 0;
  if loRowLines in Options then
    BorderWidth := TPopupGrid(DropDownObj).GridLineWidth;
  { Calc the real GridHeight }
  GridHeight := ((fDefaultRowHeight + BorderWidth)*ItemCount) + 1;
{$IFDEF Win32}
  inc(GridHeight, 2);
{$ENDIF}
  if not(loRowLines in fOptions) then
    inc(GridHeight, TPopupGrid(DropDownObj).GridLineWidth);
  { Calc the real GridWidth }
  GridWidth := DropDownWidth;  { DropDownWidth defaults to zero. It means component calcs width }
  if GridWidth = 0 then
  begin { this is the typical case where the component calcs the width }
    if FTotalW = 0 then SetColumnAttributes;
    GridWidth := FTotalW + 2;

    { make the dropdown width at least as big as the Edit }
    OldGridWidth := GridWidth;
    if GridWidth <  TComboBoxPlus(Owner).width then
      GridWidth := TComboBoxPlus(Owner).width;
    If TPopupGrid(DropDownObj).GridWidth < GridWidth then
      TPopupGrid(DropDownObj).ColWidths[TPopupGrid(DropDownObj).ColCount-1] :=
        TPopupGrid(DropDownObj).ColWidths[TPopupGrid(DropDownObj).ColCount-1] +
        (GridWidth - TPopupGrid(DropDownObj).GridWidth) - 4;

    { Make sure its visible but very small}
    DropDownObj.SetBounds(0,0,0,0);
    TPopupGrid(DropDownObj).Visible := True;
    { First time draw it like it has scroll bars then make it smaller}
    SetWindowPos (DropDownObj.Handle, 0,
         5000,
         5000,
         GridWidth + GetSystemMetrics(SM_CXVSCROLL) {FTotalW},                   {Width of grid window}
         GridHeight + GetSystemMetrics(SM_CYHSCROLL) - 1,                {Height of Grid window}
         SWP_NOACTIVATE);
    { Test grid to see if we need width for the vertical scrollbar}
    SetWindowPos (DropDownObj.Handle, 0,
         5000,
         5000,
         GridWidth {FTotalW},                   {Width of grid window}
         GridHeight,                {Height of Grid window}
         SWP_NOACTIVATE);
    { We increase the width of dropdown if it has a scroll bar ... but don't if we already have enough padding}
    If ScrollbarVisible(SB_VERT) and (RowCount > DropDownCount) then
    begin
      If GridWidth - OldGridWidth > GetSystemMetrics(SM_CXVSCROLL) then
      begin
        TPopupGrid(DropDownObj).ColWidths[TPopupGrid(DropDownObj).ColCount-1] :=
        TPopupGrid(DropDownObj).ColWidths[TPopupGrid(DropDownObj).ColCount-1] -
        GetSystemMetrics(SM_CXVSCROLL);
      end
      else
        inc(GridWidth, GetSystemMetrics(SM_CXVSCROLL));
    end;



  end;
  { Now test to see if there needs to be room for a horizontal Scrollbar}

  SetWindowPos (DropDownObj.Handle, 0,
         {P.X +(TComboBoxPlus(Owner).Width - GridWidth),} 5000,
         {P.Y + TComboBoxPlus(Owner).Height - 1,}  5000,                         {Top of Grid window}
         GridWidth,                    {Width of grid window}
         GridHeight,                   {Height of Grid window}
         SWP_NOACTIVATE);
  if ScrollBarVisible(SB_HORZ) then
    inc(GridHeight, GetSystemMetrics(SM_CYHSCROLL) - 1);
  TPopupGrid(DropDownObj).Height := GridHeight;

  { Adjust the dropdowns position based on alignment }
  DownOk := (P.Y + TComboBoxPlus(Owner).Height - 1 + GridHeight) < Screen.height;
  UpOk := (P.Y - GridHeight + 1) > 0;
  If ((TComboBoxPlus(Owner).DropDownTop = Below) and DownOK) or
     ((TComboBoxPlus(Owner).DropDownTop = Below) and not(DownOK) and not(UpOK)) or
     ((TComboBoxPlus(Owner).DropDownTop = Above) and not(UpOK)) then
    Inc(P.Y, TComboBoxPlus(Owner).Height - 1)
  else
    Dec(P.Y, GridHeight - 1);
  LeftOK :=  (P.X + GridWidth) < Screen.Width;
  RightOK := (P.X + TComboBoxPlus(Owner).Width - GridWidth) > 0;
  if ((TComboBoxPlus(Owner).DropDownAlign = Right) and RightOK) or
     ((TComboBoxPlus(Owner).DropDownAlign = Right) and not(RightOK) and not(LeftOK)) or
     ((TComboBoxPlus(Owner).DropDownAlign <> Right) and not(LeftOK)) then
    inc(P.X, (TComboBoxPlus(Owner).Width - GridWidth));
  { Put the dropdown in its corrected possition}
  SetWindowPos (DropDownObj.Handle, 0,
     P.X,
     P.Y,                         {Top of Grid window}
     GridWidth,                 {Width of grid window}
     GridHeight,                {Height of Grid window}
     SWP_NOACTIVATE);
  { This does the stay on top fix }
  if (Owner.Owner is TForm) and (TForm(Owner.Owner).FormStyle = fsStayOnTop) then
    SetWindowPos(DropDownObj.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
        SWP_NOSIZE or SWP_NOACTIVATE);

  { Make sure its visible }
  TPopupGrid(DropDownObj).Visible := True;

  { Call win api setfocus to focus the dropdown }
{$IFDEF Win32}
  Windows.SetFocus(TComboBoxPlus(Owner).Handle);
{$ELSE}
  WinProcs.SetFocus(TComboBoxPlus(Owner).Handle);
{$ENDIF}
end;

procedure TDDLinkPlus.CloseUp;
begin
  if DropDownObj <> nil then
  begin
    fRow := TPopupGrid(DropDownObj).Row;
    TComboBoxPlus(Owner).searchvalue := '';
    TComboBoxPlus(Owner).SelectText(MaxSel, 0);
    DropDownObj.free;
  end;
  DropDownObj := nil;
end;


procedure TDDLinkPlus.SetLookupActive(const Value: Boolean);
begin
  If Value <> FLookupActive then
  begin
    fLookupActive := Value;
  end;
end;

procedure TDDLinkPlus.SetFixedRows(Const Value: Integer);
begin
  fFixedRows := Value;
  if DropDownObj <> nil then TPopupGrid(DropDownObj).FixedRows := Value;
end;

procedure TDDLinkPlus.SetRowCount(Value: Longint);
begin
  if FRowCount <> Value then
  begin
    if Value < 1 then Value := 1;
    ChangeSize(ColCount, Value);
  end;
  if DropDownObj <> nil then TPopupGrid(DropDownObj).RowCount := Value;
end;

procedure TDDLinkPlus.SetColCount(Value: Longint);
begin
  if FColCount <> Value then
  begin
    if Value < 1 then Value := 1;
    ChangeSize(Value, RowCount);
  end;
  {THIS NEXT IS FOR CASE WHEN DROPED DOWN}
  if DropDownObj <> nil then TPopupGrid(DropDownObj).ColCount := Value-1;
end;

function  TDDLinkPlus.EnsureColRow(Index: Integer; IsCol: Boolean):
  TStringGridPStrings;
{retuns the specified row or column fancy string list. If the string list doesn't
exist then it creates it. Is used by all the row and col sets and gets}
var
  RCIndex: Integer;
  PList: ^TSparseList;
begin
  if IsCol then PList := @FCols else PList := @FRows;
  Result := TStringGridPStrings(PList^[Index]);
  if Result = nil then
  begin
    if IsCol then RCIndex := -Index - 1 else RCIndex := Index + 1;
    Result := TStringGridPStrings.Create(Self, RCIndex);
    PList^[Index] := Result;
  end;
end;

function  TDDLinkPlus.EnsureDataRow(ARow: Integer): Pointer;
{retuns the row data if the row doesn't exist then it creats it. Used for setting
and getting cells and objects}
var
  quantum: TSPAQuantum;
begin
  Result := TStringSparseList(TSparseList(FData)[ARow]);
  if Result = nil then
  begin
    if ColCount > 512 then quantum := SPALarge else quantum := SPASmall;
    Result := TStringSparseList.Create(quantum);
    TSparseList(FData)[ARow] := Result;
  end;
end;

function TDDLinkPlus.GetCells(ACol, ARow: Integer): string;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then Result := '' else Result := ssl[ACol];
end;

function TDDLinkPlus.GetCols(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, True);
end;

function TDDLinkPlus.GetObjects(ACol, ARow: Integer): TObject;
var
  ssl: TStringSparseList;
begin
  ssl := TStringSparseList(TSparseList(FData)[ARow]);
  if ssl = nil then Result := nil else Result := ssl.Objects[ACol];
end;

function TDDLinkPlus.GetCol : LongInt;
begin
  if DropDownObj <> nil then
    fCol := TPopupGrid(DropDownObj).Col;
  result := fCol;
end;

function TDDLinkPlus.GetRow : LongInt;
begin
  if DropDownObj <> nil then
    fRow := TPopupGrid(DropDownObj).Row;
  result := fRow;
end;

function TDDLinkPlus.GetRows(Index: Integer): TStrings;
begin
  Result := EnsureColRow(Index, False);
end;

procedure TDDLinkPlus.SetCells(ACol, ARow: Integer; const Value: string);
begin
  TStringGridPStrings(EnsureDataRow(ARow))[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  if DropDownObj <> nil then
    TStringGridP(DropDownObj).xUpdate(ACol, ARow);
end;

procedure TDDLinkPlus.SetCols(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, True).Assign(Value);
end;

procedure TDDLinkPlus.SetObjects(ACol, ARow: Integer; Value: TObject);
begin
  TStringGridPStrings(EnsureDataRow(ARow)).Objects[ACol] := Value;
  EnsureColRow(ACol, True);
  EnsureColRow(ARow, False);
  if DropDownObj <> nil then
    TStringGridP(DropDownObj).xUpdate(ACol, ARow);
end;

procedure TDDLinkPlus.SetRows(Index: Integer; Value: TStrings);
begin
  EnsureColRow(Index, False).Assign(Value);
end;

procedure TDDLinkPlus.DoLookup;
var
  I : LongInt;
begin
  FFoundValue := False;

  if (Value = '') or visible then Exit;
  {Do a quick sequential search}
  {note the following trick with ord(FLookupActive) not good style but it works ans saves a field}
  { if the dropdown exists then only do the lookup if needed }
  if (AnsiCompareText(Value, Cols[ord(not FLookupActive)].Strings[fRow])=0) then
    FFoundValue := True
  else
  begin
    For I := FixedRows to RowCount-1 do
    begin
      If AnsiCompareText(Value, Cols[ord(not FLookupActive)].Strings[I])=0 then
      begin
        FFoundValue := True;
        fRow := I;
        if (DropDownObj <> nil) then
          TPopupGrid(DropDownObj).Row := I;   {Move to the row found}
        Break;
      end;
    end;
  end;
  if not FFoundValue then
  begin
    fRow := FixedRows;
    if (DropDownObj <> nil) then
       TPopupGrid(DropDownObj).Row := fRow;   {Move to the row found}
  end
end;

function TDDLinkPlus.GetValue: string;
begin
  result := FValue;
end;

procedure TDDLinkPlus.SetValue(const Value: string);
{This method is a driving force. This not only sets the
current value of the grid it also moves to that part of the grid}
begin
  if (FValue <> Value) or (fRow < FixedRows) then
  begin
    FValue := Value;
    DoLookup;             { this is a really key step }
    if FFoundValue then
      FDisplayValue := Rows[fRow].Strings[1]
    else
    begin
      FDisplayValue := '';
      FValue := '';
    end;
  end;
end;

function TDDLinkPlus.GetDisplayValue: string;
begin
  Result := FDisplayValue;
end;

procedure TDDLinkPlus.SetDisplayValue(const Value: string);
var
  I : LongInt;
begin
  if (FDisplayValue <> Value) or (fRow < FixedRows) then
  begin
    FFoundValue := False;
    { Find the row where the value in Col 1 = value }
    For I := FixedRows to RowCount-1 do
    begin
      If AnsiCompareText(Value, Cols[1].Strings[I])=0 then
      begin
        FFoundValue := True;
        fRow := I;   {Move to the row found}
        if (DropDownObj <> nil) then
          TPopupGrid(DropDownObj).Row := I;   {Move to the row found}
        Break;
      end;
    end;
    FDisplayValue := Value;
    if not LookupActive then
      FValue := FDisplayValue
    else if not FFoundValue then
    begin
      FDisplayValue := '';
      FValue := '';
    end
    else { if lookupActive then }
      FValue := Cols[0].Strings[frow];
  end;
end;

end.

