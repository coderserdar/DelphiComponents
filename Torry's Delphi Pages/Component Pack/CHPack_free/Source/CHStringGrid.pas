unit CHStringGrid;

{ ##############################################################################
  TCHStringgrid

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 04.04.2004    - First Release
  1.0.1 - 31.12.2004    - BUG: Fix some Error


  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  Grids, StdCtrls, Menus, Types;

type
  TStringGridRowOptions = set of (roAllowAppend, roAllowDelete, roAllowInsert);

	TStringGridRowsChangedEvent = procedure (Sender: TObject; ARow: Integer) of object;
	TStringGridRowAppendEvent = procedure (Sender: TObject; ARow: Integer; var CanAppend: Boolean) of object;
	TStringGridRowInsertEvent = procedure (Sender: TObject; ARow: Integer; var CanInsert: Boolean) of object;
	TStringGridRowDeleteEvent = procedure (Sender: TObject; ARow: Integer; var CanDelete: Boolean) of object;

  //TCHGridClickEvent = procedure (Sender: TObject; ACol, ARow: Integer) of object;
//  TCHGridDrawCellButtonEvent = procedure (Sender: TObject; ACol, ARow: Integer; VAR ButtonGlyph: string) of object;
//  TCHGridExitCellEvent = procedure (Sender: TObject; ACol, ARow: Integer; VAR AllowExit: Boolean) of object;
//  TCHGridGetCellColorEvent = procedure (Sender: TObject; ACol, ARow: Integer; VAR CellColor: TColor) of object;
//  TCHGridIsCellButtonEnabledEvent = procedure (Sender: TObject; ACol, ARow: Integer; VAR AEnabled: Boolean) of object;
//  TCHGridShowCellHintEvent = procedure (Sender: TObject; ACol, ARow: Integer; IsOverButton: Boolean; VAR AHint: string) of object;
//  TCHGridBeforeInsertEvent = procedure(Sender: TObject; ARow: Integer; VAR AllowInsert: Boolean) of object;

  //TCellMode = (cmString, cmNumber, cmDate, cmCheckbox);
//  ToSortType  = (stCharacter, stNumeric, stDate);
//  SortDirType = (sdAscending, sdDescending);

  TCellStyle = (csText, csCheckbox, csAutoInc);
  TRowMode = (rmNone, rmInsert, rmUpdate, rmDelete);
  TCellMode = (cmString, cmNumber, cmDate);
  ToSortType  = (stCharacter, stNumeric, stDate);
  SortDirType = (sdAscending, sdDescending);


  TWinControlCracker = class(TWinControl)end;
  TCHGridColumn = class;
  TCHStringGrid = class;

  TSortGrid = class(TPersistent)
    private
      FOwner : TCHStringGrid;
      FGridToSort : TCHStringGrid;
      FStartIndex : Integer;
      FEndIndex : Integer;
      FSortIndex : Integer;
      FSortType : ToSortType;
      FCaseSensitiv : Boolean;
      FSortDir : SortDirType;
      FShowMsg : Boolean;
      FArrowColor: TColor;
      FEnabled: Boolean;

      procedure QuickSortGrid(sGrid : TStringGrid; StartIdx, EndIdx, SortIdx : Integer);
      procedure BubbleSortGrid(sGrid : TStringGrid; StartIdx, EndIdx, SortIdx : Integer);
      procedure qsortGrid(sGrid : TStringGrid; StartIdx, EndIdx, SortIdx : Integer);
      procedure qsortGridNumeric(sGrid : TStringGrid; StartIdx, EndIdx, SortIdx : Integer);
      procedure qsortGridDate(sGrid : TStringGrid; StartIdx, EndIdx, SortIdx : Integer);
      function  GetDateSQL(a_s: string): string;
    protected

    public
      ErrorCode : Integer;
      ErrorText : String;

      constructor Create(AOwner: TCHStringGrid); virtual;
      function Execute : Boolean;

    published

      property Enabled : Boolean read FEnabled Write FEnabled;
      property ArrowColor : TColor read FArrowColor Write FArrowColor;
      property SortColumn : Integer read FSortIndex write FSortIndex default 0;
      property SortType : ToSortType read FSortType write FSortType default stCharacter;
      property CaseSensitiv : Boolean read FCaseSensitiv write FCaseSensitiv default False;
      property SortDirection : SortDirType read FSortDir write FSortDir default sdAscending;
      property ShowMessageOnError : Boolean read FShowMsg write FShowMsg default False;
   end;


// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TCHStringGridInplaceEdit = class(TInplaceEdit)
  private
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TCHColumnTitle = class(TPersistent)
  private
    FColumn: TCHGridColumn;
    FCaption: string;
    FFont: TFont;
    FAlignment: TAlignment;
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetFont: TFont;
    procedure SetTitelAlignment(const Value: TAlignment);
    procedure SetTitelCaption(const Value: string);
    procedure SetTitelFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);

  protected
  public
    constructor Create(Column: TCHGridColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read GetAlignment write SetTitelAlignment;
    property Caption: string read GetCaption write SetTitelCaption;
    property Font: TFont read GetFont write SetTitelFont;
  end;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TCHGridColumn = class(TCollectionItem)
  private
    FName: string;
    FWidth: Integer;
    FTag: Integer;
    FColor: TColor;
    FAlignment: TAlignment;
    FMarginY: Integer;
    FMarginX: Integer;
    FActiveDrawing: Boolean;
    FMask: string;
    FMode: TCellMode;
    FMasking: Boolean;
    FTitle: TCHColumnTitle;
    FFont : TFont;
    FVisible: Boolean;
    FStyle: TCellStyle;
    FEnableHighlight: Boolean;
    procedure SetColColor(const Value: TColor);
    procedure SetColAlignment(const Value: TAlignment);
    procedure SetColWidth(const Value: Integer);
    procedure SetColMarginX(const Value: Integer);
    procedure SetColMarginY(const Value: Integer);
    procedure SetColActiveDrawing(const Value: Boolean);
    procedure SetColMask(const Value: string);
    procedure SetColMode(const Value: TCellMode);
    procedure SetColMasking(const Value: Boolean);
    procedure SetColTitle(const Value: TCHColumnTitle);
    function GetFont: TFont;
    procedure SetColFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    function GetColWidth: Integer;
    procedure SetVisible(const Value: Boolean);
    procedure SetColStyle(const Value: TCellStyle);
    procedure SetEnableHighlight(const Value: Boolean);

  protected
    function  GetGrid: TCHStringGrid;
    function  CreateTitle: TCHColumnTitle; virtual;
  public
    FOldWidth : Integer;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property  Grid: TCHStringGrid read GetGrid;

  published
    property ActiveDrawing : Boolean read FActiveDrawing Write SetColActiveDrawing;
    property Alignment : TAlignment read FAlignment Write SetColAlignment default taLeftJustify;
    property EnableHighlight : Boolean read FEnableHighlight Write SetEnableHighlight;
    property MarginX : Integer read FMarginX Write SetColMarginX;
    property MarginY : Integer read FMarginY Write SetColMarginY;
    property Color: TColor read FColor write SetColColor default clWindow;
    property Name : string read FName Write FName;
    property Width : Integer read GetColWidth Write SetColWidth default 64;
    property Tag: Integer read FTag write FTag;
    property Masking : Boolean read FMasking Write SetColMasking;
    property Mask : string read FMask Write SetColMask;
    property Mode : TCellMode read FMode Write SetColMode;
    property Title: TCHColumnTitle read FTitle write SetColTitle;
    property Font: TFont read GetFont write SetColFont;
    property Visible : Boolean read FVisible Write SetVisible;
    property Style : TCellStyle read FStyle Write SetColStyle;
  end;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TColumnClass = class of TCHGridColumn;

  TCHGridColumns = class(TCollection)
  private
    FGrid: TCHStringGrid;
    function GetColumn(Index: Integer): TCHGridColumn;
    procedure SetColumn(Index: Integer; Value: TCHGridColumn);
    function GetColumnN(Name: string): TCHGridColumn;
    procedure SetColumnN(Name: string; const Value: TCHGridColumn);
  protected
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(Grid: TCHStringGrid; ColumnClass: TColumnClass); virtual;
    function  Add: TCHGridColumn;
    property Grid: TCHStringGrid read FGrid;
    property Items[Index: Integer]: TCHGridColumn read GetColumn write SetColumn; default;
    property ItemsName[Name : string]: TCHGridColumn read GetColumnN write SetColumnN;
  end;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TRowSetting = record
    Row : Integer;
    Backcolor : TColor;
    Textcolor : TColor;
    TextStyle : TFontStyles;
    FixRowDraw : Boolean;
  end;

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TCHStringGrid = class(TStringGrid)
  private
    FColumns: TCHGridColumns;
    FLayoutFlag : Integer;
    FFontActive: Boolean;
    FFixedLineColor: TColor;
    FGridLineColor: TColor;
    FMultiline: Boolean;
    FSelectedTextColor: TColor;
    FSelectedColor: TColor;
    FBuffered: Boolean;
    FRowAppend: TStringGridRowAppendEvent;
    FRowDelete: TStringGridRowDeleteEvent;
    FRowInsert: TStringGridRowInsertEvent;
    FRowOptions: TStringGridRowOptions;
    FRowsChanged: TStringGridRowsChangedEvent;
    FSortgrid: TSortGrid;
    FHighlightColor: TColor;
    FHighlightTextColor: TColor;
    FOnAfterSort: TNotifyEvent;
    FOnBeforeSort: TNotifyEvent;
    FMultilineMax: Word;
    FCellHint: Boolean;
    FLastHintRow : Integer;
    FLastHintCol : Integer;
    FAcceptControls: Boolean;
    procedure SetColumnCount(NewCount: LongInt);
    procedure SetColumns(Value: TCHGridColumns);
    procedure SetCellColor(ACol, ARow : Integer; ARect: TRect; AColor : TColor);
    procedure SetCellAlignment(AText : string; ARect: TRect;  ACol, ARow : Integer; AAlignment: TAlignment);
    procedure SetFontActive(const Value: Boolean);
    procedure SetFixedLineColor(const Value: TColor);
    procedure SetGridLineColor(const Value: TColor);
    procedure SetMultiline(const Value: Boolean);
    procedure SetBuffered(const Value: Boolean);

    procedure SetColCell(AColName: string; ARow: Integer; const Value: string);
    function GetColCell(AColName: string; ARow: Integer): string;
    function GetTitelCell(ATitelName: string; ARow: Integer): string;
    procedure SetTitelCell(ATitelName: string; ARow: Integer; const Value: string);
    procedure SetHighlightColor(const Value: TColor);
    procedure SetHighlightTextColor(const Value: TColor);
    procedure SetMultilineMax(const Value: Word);
    Procedure WMCommand( var msg: TWMCommand ); message WM_COMMAND;
  protected
    procedure DataChanged(Col, Row: Integer; Value: string); virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    function  CreateEditor: TInplaceEdit; override;
    function  CreateColumns: TCHGridColumns; dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure OwnerDrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); virtual;
    function FormatToNumber(AText : string; ACol : Integer) : string;
    function FormatToDate(AText : string) : string;
    function GetDouble(const Value: string): Double;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
  public
    FOnColumnResize: TNotifyEvent;
    FSelectedRow : Integer;
    FSelectedCol : Integer;
    FLastRow : Integer;
    FLastCol : Integer;
    FSortedCol : Integer;
    FRowColorArray : array of TRowSetting;
    FCheck, FNoCheck: TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function IsCellSelected( X, Y : LONGINT): Boolean;
    function IsCellChecked( X, Y : LONGINT): Boolean;
    function CheckCell( X, Y : LONGINT): Boolean;
    function UnCheckCell( X, Y : LONGINT): Boolean;
    function IsScrollBar(nFlag: Cardinal ): Boolean;
    function IsEmpty : Boolean;
    function IsRowEmpty(ARow : Integer) : Boolean;
    procedure OptimalColWidth(MinWidth : Integer; WithTitle : Boolean);
    procedure OptimalTableWidth;
    procedure Clear;
    procedure RowAppend;
    procedure RowDelete; overload;
		procedure RowDelete(RowNr : Integer); overload;
		procedure RowInsert; overload;
    procedure RowInsert(RowNr : Integer); overload;
    procedure AddRowColor(ARow : Integer; cBackground, cText : TColor; fFontStyle : TFontStyles; FixRow : Boolean);
    procedure DelRowColor(ARow : Integer);
    procedure ClearRowColors;
    //procedure BitmapToCell(ARow, ACol : Integer; ABitmap : TBitmap);

    property CellC[AColName : string; ARow: Integer]: string read GetColCell write SetColCell;
    property CellT[ATitelName : string; ARow: Integer]: string read GetTitelCell write SetTitelCell;
  published
    property FontActive : Boolean read FFontActive Write SetFontActive;
    property Columns : TCHGridColumns read FColumns write SetColumns;
    property FixedLineColor : TColor read FFixedLineColor Write SetFixedLineColor;
    property GridLineColor : TColor read FGridLineColor Write SetGridLineColor;
    property HighlightColor : TColor read FHighlightColor Write SetHighlightColor;
    property HighlightTextColor : TColor read FHighlightTextColor Write SetHighlightTextColor;
    property Multiline : Boolean read FMultiline Write SetMultiline;
    property MultilineMax : Word read FMultilineMax Write SetMultilineMax;
    property Buffered : Boolean read FBuffered Write SetBuffered;
    property AcceptControls : Boolean read FAcceptControls Write FAcceptControls;
    property Sort : TSortGrid read FSortgrid Write FSortGrid;
    property CellAsHint : Boolean read FCellHint Write FCellHint;

    property OnColumnResize : TNotifyEvent read FOnColumnResize write FOnColumnResize;
    property RowOptions: TStringGridRowOptions read FRowOptions write FRowOptions;
		property OnRowsChanged: TStringGridRowsChangedEvent read FRowsChanged write FRowsChanged;
		property OnRowAppend: TStringGridRowAppendEvent read FRowAppend write FRowAppend;
		property OnRowDelete: TStringGridRowDeleteEvent read FRowDelete write FRowDelete;
		property OnRowInsert: TStringGridRowInsertEvent read FRowInsert write FRowInsert;
    property OnBeforeSort : TNotifyEvent read FOnBeforeSort write FOnBeforeSort;
    property OnAfterSort : TNotifyEvent read FOnAfterSort write FOnAfterSort;
  end;


procedure Register;

implementation

{$R CHStringgrid.res}

uses StrUtils, math;

const
   MinErrCode = -4;
   MaxErrCode = 7;
   ErrorTextConst : Array [MinErrCode..MaxErrCode] of string = (
{-4}   'Case Sensitivity ignored for Date Sort.'
{-3}  ,'Case Sensitivity ignored for Numeical Sort.'
{-2}  ,'Column/Row contains non-date values for date sort. Sorted as Character.'
{-1}  ,'Column/Row contains non-numerical values for numeric sort. Sorted as Character.'
{ 0}  ,'Ok'
{ 1}  ,'No StringGrid given'
{ 2}  ,'StartIndex is greater or equal to EndIndex'
{ 3}  ,'StartIndex is less then 0'
{ 4}  ,'StartIndex is greater then number of rows/columns in StringGrid'
{ 5}  ,'EndIndex is greater then number of rows/columns in StringGrid'
{ 6}  ,'Sort Index is less then 0'
{ 7}  ,'Sort Index is greater then number of rows/columns in StringGrid'
   );


procedure Register;
begin
  RegisterComponents('CH Pack', [TCHStringGrid]);
end;


{ ############################################################################ }
{ ############################################################################ }
{ TCHGridColumn }
{ ############################################################################ }
{ ############################################################################ }
constructor TCHGridColumn.Create(Collection: TCollection);
var
  Grid: TCHStringGrid;
begin
  Grid := nil;
  if Assigned(Collection) and (Collection is TCHGridColumns) then
    Grid := TCHGridColumns(Collection).Grid;
  try
    inherited Create(Collection);
    FColor := clWindow;
    FActiveDrawing := True;
    FMarginX := 3;
    FWidth := 64;
    FOldWidth := 64;
    FMask := '#,##0.00';
    FTitle := CreateTitle;
    FFont := TFont.Create;
    FFont.Assign(Grid.Font);
    FFont.OnChange := FontChanged;
    FVisible := True;
    FEnableHighlight := True;
  finally
    if (Grid <> nil) then
    begin
      grid.setcolumncount(Grid.columns.count);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHGridColumn.Destroy;
begin
  with TCHGridColumns(Collection).Grid do
    if FLayoutFlag = 0 then
      setcolumncount(Grid.colcount-1);
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.Assign(Source: TPersistent);
begin
  if Source is TCHGridColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try

    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumn.GetGrid: TCHStringGrid;
begin
  if Assigned(Collection) and (Collection is TCHGridColumns) then
    Result := TCHGridColumns(Collection).Grid
  else
    Result := nil;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    if Value = -1 then
    begin
      FOldWidth := FWidth;
      FVisible := False;
    end;
    FWidth := Value;
    Grid.ColWidths[Self.Index] := Value;

    if Assigned(Grid.FOnColumnResize) then
      Grid.FOnColumnResize(Self);

    //Grid.SetSortBmp;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColMarginX(const Value: Integer);
begin
  if FMarginX <> Value then
  begin
    FMarginX := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColMarginY(const Value: Integer);
begin
  if FMarginY <> Value then
  begin
    FMarginY := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.FontChanged(Sender: TObject);
begin
  Changed(True);
end;



{ ############################################################################ }
{ ############################################################################ }
{ TCHColumnTitle }
{ ############################################################################ }
{ ############################################################################ }
constructor TCHColumnTitle.Create(Column: TCHGridColumn);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(FColumn.Grid.Font);
  FFont.OnChange := FontChanged;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColumnTitle.FontChanged(Sender: TObject);
begin
  FColumn.Changed(True);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColumnTitle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHColumnTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColumnTitle.GetAlignment: TAlignment;
begin
  Result := FColumn.FTitle.FAlignment;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColumnTitle.GetCaption: string;
begin
  Result := FColumn.FTitle.FCaption;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHColumnTitle.GetFont: TFont;
begin
  Result := FColumn.FTitle.FFont;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColumnTitle.SetTitelAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    FColumn.Changed(False);
    FColumn.Grid.InvalidateRow(FColumn.Grid.FixedRows);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColumnTitle.SetTitelCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FColumn.Changed(False);
    FColumn.Grid.InvalidateRow(FColumn.Grid.FixedRows);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHColumnTitle.SetTitelFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FColumn.Changed(False);
  FColumn.Grid.InvalidateRow(FColumn.Grid.FixedRows);
end;


{ ############################################################################ }
{ ############################################################################ }
{ TCHGridColumns }
{ ############################################################################ }
{ ############################################################################ }
constructor TCHGridColumns.Create(Grid: TCHStringGrid; ColumnClass: TColumnClass);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumns.Add: TCHGridColumn;
begin
  Result := TCHGridColumn(inherited Add);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumns.GetColumn(Index: Integer): TCHGridColumn;
begin
  Result := TCHGridColumn(inherited Items[Index]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumns.SetColumn(Index: Integer; Value: TCHGridColumn);
begin
  Items[Index].Assign(Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumns.GetColumnN(Name: string): TCHGridColumn;
var
  I : Integer;
begin
  Result := nil;
  if Trim(Name) <> '' then
  begin
    for I := 0 to Grid.ColCount -1 do
    begin
      if UpperCase(Grid.Columns[I].Name) = UpperCase(Name) then
      begin
        Result := TCHGridColumn(inherited Items[I]);
        Break;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumns.SetColumnN(Name: string; const Value: TCHGridColumn);
var
  I : Integer;
begin
  if Trim(Name) <> '' then
  begin
    for I := 0 to Grid.ColCount -1 do
    begin
      if UpperCase(Grid.Columns[I].Name) = UpperCase(Name) then
      begin
        Items[I].Assign(Value);
        Break;
      end;
    end;
  end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumns.Update(Item: TCollectionItem);
VAR
  IPE: TCHStringGridInplaceEdit;
begin
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then
    Exit;

  if (csDesigning in FGrid.ComponentState) then
    FGrid.invalidate
  else if Assigned(Item) then
  begin
    FGrid.invalidatecol(Item.Index);
    IPE := FGrid.InplaceEditor as TCHStringGridInplaceEdit;
    if Assigned(IPE) then
      IPE.UpdateContents;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColTitle(const Value: TCHColumnTitle);
begin
  FTitle.Assign(Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;

    if FVisible = False then
    begin
//      if Width > -1 then
//        FOldWidth := Width;
      Width := -1;
    end
    else
    begin
      Width := FOldWidth;
//      FOldWidth := -1;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColMask(const Value: string);
begin
  if FMask <> Value then
  begin
    FMask := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColMode(const Value: TCellMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColMasking(const Value: Boolean);
begin
  if FMasking <> Value then
  begin
    FMasking := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumn.CreateTitle: TCHColumnTitle;
begin
  Result := TCHColumnTitle.Create(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumn.GetFont: TFont;
begin
  Result := FFont;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHGridColumn.GetColWidth: Integer;
begin
  Result := Grid.ColWidths[Index];
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed(False);
  Grid.InvalidateCol(Index);
end;


{ ############################################################################ }
{ ############################################################################ }
{ StringgridInplaceEdit }
{ ############################################################################ }
{ ############################################################################ }
type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

procedure TCHStringGridInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHStringGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  ShowHint := FALSE;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHStringGridInplaceEdit.Destroy();
begin
  inherited Destroy;
end;


{ ############################################################################ }
{ ############################################################################ }
{ TCHStringGrid }
{ ############################################################################ }
{ ############################################################################ }
constructor TCHStringGrid.Create(AOwner: TComponent);
var
  Bmp : TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultRowHeight := 20;
  FLayoutFlag := 2;
  FColumns := CreateColumns;
  HideEditor;
  FLayoutFlag := 0;
  FFontActive := False;
  FGridLineColor := clSilver;
  FFixedLineColor := clBlack;
  FSelectedColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FHighlightColor := clHighlight;
  FHighlightTextColor := clHighlightText;
  FSelectedRow := -1;
  FSelectedCol := -1;
  FLastRow := -1;
  FLastCol := -1;
  FSortedCol := -1;
  FMultilineMax := 0;
  SetLength(FRowColorArray, 0);

  FRowOptions := [roAllowAppend, roAllowDelete, roAllowInsert];

  sizechanged(ColCount, RowCount);

  FSortgrid := TSortGrid.Create(Self);

  // für Checkbox
  FCheck := TBitmap.Create;
  FNoCheck := TBitmap.Create;
  Bmp := TBitmap.create;
  try
    bmp.handle := LoadBitmap( 0, PChar(OBM_CHECKBOXES ));
    {bmp now has a 4x3 bitmap of divers state images used by checkboxes and radiobuttons}
    with FNoCheck do
    begin
      {the first subimage is the unchecked box}
      width := bmp.width div 4;
      height := bmp.height div 3;
      canvas.copyrect( canvas.cliprect, bmp.canvas, canvas.cliprect );
    end;
    with FCheck do
    begin
      {the second subimage is the checked box}
      width := bmp.width div 4;
      height := bmp.height div 3;
      canvas.copyrect(canvas.cliprect, bmp.canvas, rect( width, 0, 2 * width, height ));
    end;
  finally
    Bmp.free
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHStringGrid.Destroy;
begin
  FLayoutFlag := 2;
  FColumns.Free;
  FNoCheck.Free;
  FCheck.Free;
  FColumns := nil;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.DataChanged(Col, Row: Integer; Value: string);
begin
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
  if not (csLoading in ComponentState) and (FLayoutFlag = 0) then
  begin
    inc(FLayoutFlag);
    while Columns.count > ColCount do
      Columns[ColCount].destroy;
    while Columns.count < ColCount do
      Columns.add;
    dec(FLayoutFlag);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetColumnCount(NewCount: LongInt);
begin
  if (FLayoutFlag > 0) or (csLoading in ComponentState) then
    exit;
  inc(FLayoutFlag);
  ColCount := NewCount;
  dec(FLayoutFlag);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TCHStringGridInplaceEdit.Create(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.CreateColumns: TCHGridColumns;
begin
  Result := TCHGridColumns.Create(Self, TCHGridColumn);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetColumns(Value: TCHGridColumns);
begin
  Columns.Assign(Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGridColumn.SetColActiveDrawing(const Value: Boolean);
begin
  if FActiveDrawing <> Value then
  begin
    FActiveDrawing := Value;
    Grid.InvalidateCol(Index);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.Paint;
begin
  inherited;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  if Columns[ACol].ActiveDrawing then
    OwnerDrawCell(ACol, ARow, ARect, AState)
  else
    inherited DrawCell(ACol, ARow, ARect, AState);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.OwnerDrawCell(ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var
  AColor : TColor;
  AAlignment : TAlignment;
  AText : string;
  AFont : TFont;
  I, nRow : Integer;
  ArrowRect : TRect;
begin
  // 1. Titel
  if (ARow = 0) and (ARow <= FixedRows) then
  begin
    AText := Columns.Items[ACol].Title.Caption;
    AAlignment := Columns.Items[ACol].Title.Alignment;
    AFont := Columns.Items[ACol].Title.Font;
    Canvas.Font := AFont;
    if Cells[ACol, ARow] <> AText then
      Cells[ACol, ARow] := AText;
    SetCellAlignment(AText, ARect, ACol, ARow, AAlignment);

    if ACol = FSortedCol then
    begin
      ArrowRect.Left := ARect.Right - 12;
      ArrowRect.Right := ARect.Right - 3;
      ArrowRect.Top := ARect.Top;
      ArrowRect.Bottom := ARect.Bottom;
      Canvas.Pen.Color := FixedColor;
      Canvas.FillRect(ArrowRect);

      Canvas.Pen.Color := FSortgrid.FArrowColor;
      if FSortgrid.SortDirection = sdAscending then
      begin
        Canvas.MoveTo(ARect.Right - 3, ARect.Bottom div 2 -3);
        Canvas.LineTo(ARect.Right - 12, ARect.Bottom div 2 -3);

        Canvas.MoveTo(ARect.Right - 4, ARect.Bottom div 2 -2);
        Canvas.LineTo(ARect.Right - 11, ARect.Bottom div 2 -2);

        Canvas.MoveTo(ARect.Right - 5, ARect.Bottom div 2 -1);
        Canvas.LineTo(ARect.Right - 10, ARect.Bottom div 2 -1);

        Canvas.MoveTo(ARect.Right - 6, ARect.Bottom div 2);
        Canvas.LineTo(ARect.Right - 9, ARect.Bottom div 2);

        Canvas.MoveTo(ARect.Right - 7, ARect.Bottom div 2 +1);
        Canvas.LineTo(ARect.Right - 8, ARect.Bottom div 2 +1);
      end
      else
      begin
        Canvas.MoveTo(ARect.Right - 3, ARect.Bottom div 2 +1);
        Canvas.LineTo(ARect.Right - 12, ARect.Bottom div 2 +1);

        Canvas.MoveTo(ARect.Right - 4, ARect.Bottom div 2);
        Canvas.LineTo(ARect.Right - 11, ARect.Bottom div 2);

        Canvas.MoveTo(ARect.Right - 5, ARect.Bottom div 2 -1);
        Canvas.LineTo(ARect.Right - 10, ARect.Bottom div 2 -1);

        Canvas.MoveTo(ARect.Right - 6, ARect.Bottom div 2 -2);
        Canvas.LineTo(ARect.Right - 9, ARect.Bottom div 2 -2);

        Canvas.MoveTo(ARect.Right - 7, ARect.Bottom div 2 -3);
        Canvas.LineTo(ARect.Right - 8, ARect.Bottom div 2 -3);
      end;
    end;

  end
  // 2. Style
  else
  begin
    // TEXT

    AText := Cells[ACol, ARow];


    AAlignment := Columns.Items[ACol].Alignment;

    // Format prüfen + maskieren
    if Columns.Items[ACol].Mode = cmString then
      AText := AText
    else if Columns.Items[ACol].Mode = cmNumber then
      AText := FormatToNumber(AText, ACol)
    else if Columns.Items[ACol].Mode = cmDate then
      AText := FormatToDate(AText);

    // Font
    if FFontActive then
      AFont := Self.Font
    else
      AFont := Columns.Items[ACol].Font;

    Canvas.Font := AFont;

    if Length(FRowColorArray) > 0 then
    begin
      for I := 0 to Length(FRowColorArray) -1 do
      begin
        nRow := FRowColorArray[I].Row;
        if nRow = ARow then
        begin
          if (gdFixed in AState) then
          begin
            if (FRowColorArray[I].FixRowDraw) then
            begin
              Canvas.Font.Color := FRowColorArray[I].Textcolor;
              if FRowColorArray[I].TextStyle <> [] then
                Canvas.Font.Style := FRowColorArray[I].TextStyle;
            end;
          end
          else
          begin
            Canvas.Font.Color := FRowColorArray[I].Textcolor;
            if FRowColorArray[I].TextStyle <> [] then
              Canvas.Font.Style := FRowColorArray[I].TextStyle;
          end;
        end
      end;
    end;

    AColor := Columns.Items[ACol].Color;

    if (gdSelected in AState) then
    begin
      if (Columns.Items[ACol].EnableHighlight) then
      begin
        Canvas.Font.Color := FHighlightTextColor;
        Canvas.Brush.Color := FHighlightColor;
      end
      else
      begin
        Canvas.Font.Color := AFont.Color;
        Canvas.Brush.Color := AColor;
      end;
    end;


    if Length(FRowColorArray) > 0 then
    begin
      for I := 0 to Length(FRowColorArray) -1 do
      begin
        nRow := FRowColorArray[I].Row;
        if nRow = ARow then
        begin
          AColor := FRowColorArray[I].Backcolor;
        end;
      end;
    end;
    SetCellColor(ACol, ARow, ARect, AColor);


    DefaultDrawing := False;

    // Alignment setzen und Text zeichnen
    SetCellAlignment(AText, ARect, ACol, ARow, AAlignment);
    if Cells[ACol, ARow] <> AText then
      Cells[ACol, ARow] := AText;
    inherited DrawCell(ACol, ARow, ARect, AState);

    if gdFixed in AState then
      Canvas.Brush.Color := FFixedLineColor
    else
      Canvas.Brush.Color := FGridLineColor;

    for I := 1 to GridLineWidth do
    begin
      InflateRect(ARect,1,1);
      Canvas.FrameRect(ARect);

      if not (goVertLine in Options) then
      begin
        if gdSelected in AState then
          Canvas.Brush.Color := FHighlightColor
        else
          Canvas.Brush.Color := Color;
          
        ARect.Right := ARect.Left+GridLineWidth;
        ARect.Bottom := ARect.Bottom-GridLineWidth;
        ARect.Top := ARect.Top+GridLineWidth;
        Canvas.FrameRect(ARect);
      end;

    end;

    // Checkbox zeichnen
    if Columns.Items[ACol].Style = csCheckbox then
    begin
      if not IsRowEmpty(ARow) then
      begin
        with Canvas do
        begin
          if (Assigned(Objects[ACol, ARow])) or (AText = '1') then
            Draw( (Arect.Right+Arect.Left-FCheck.width) div 2, (Arect.bottom + Arect.top - FCheck.height) div 2, FCheck )
          else
            Draw( (Arect.Right+Arect.Left-FNoCheck.width) div 2, (Arect.bottom + Arect.top - FNoCheck.height) div 2, FNoCheck )
        end;
      end;
    end;

    DefaultDrawing := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetCellAlignment(AText: string; ARect: TRect; ACol, ARow : Integer;
  AAlignment: TAlignment);
var
  FontHeight, Format : Integer;
  Rec, CalcRec: TRect;
  Str : string;
  H, HF: Integer;
begin
  FontHeight := Canvas.TextHeight(AText);
  ARect.Left := ARect.Left + Columns[ACol].MarginX;
  ARect.Right := ARect.Right - Columns[ACol].MarginX;
  Str := AText;
  Str := StringReplace(Str, '&', '&&', [rfReplaceAll]);

  IF FontHeight > 0 THEN
  BEGIN
    Rec := ARect;
    dec(Rec.Right, 2);
    H := Rec.Bottom - Rec.Top;
    HF := (H DIV FontHeight) * FontHeight;
    Rec.Top := (Rec.Top + (H - HF) DIV 2) + Columns[ACol].MarginY;
    Rec.Bottom := (Rec.Top + HF) - Columns[ACol].MarginY;

    // Multiline or Singleline
    if FMultiline then
    begin
      CalcRec := ARect;
      Format := DT_WORDBREAK;
      DrawText(Canvas.handle, PAnsiChar(Str), - 1, CalcRec, DT_CALCRECT or DT_WORDBREAK or DT_LEFT );
      if (CalcRec.Bottom - CalcRec.Top) > RowHeights[ARow] then
      begin
        if (FMultilineMax > 0) then
        begin
          if ((CalcRec.Bottom - CalcRec.Top) > (FMultilineMax * DefaultRowHeight)) then
            RowHeights[ARow] := FMultilineMax * DefaultRowHeight
          else
            RowHeights[ARow] := (CalcRec.Bottom - CalcRec.Top);
        end
        else
        begin
          RowHeights[ARow] := (CalcRec.Bottom - CalcRec.Top);
        end;
      end;
    end
    else
    begin
      Format := DT_SINGLELINE;
      if RowHeights[ARow] > DefaultRowHeight then
        RowHeights[ARow] := DefaultRowHeight;
    end;

    CASE AAlignment OF
      taLeftJustify : DrawText(Canvas.Handle, PAnsiChar(Str), -1, Rec, Format OR DT_Left);
      taRightJustify: DrawText(Canvas.Handle, PAnsiChar(Str), -1, Rec, Format OR DT_Right);
      taCenter      : DrawText(Canvas.Handle, PAnsiChar(Str), -1, Rec, Format OR DT_Center);
    END;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetCellColor(ACol, ARow: Integer;
  ARect: TRect; AColor: TColor);
var
  oldColor : TColor;
  oldBrush : TBrush;
begin
  with Canvas do
  begin
    oldColor := Font.Color;
    oldBrush := Brush;
    if (ARow >= FixedRows) and (ACol >= FixedCols) then
    begin
      if not IsCellSelected(ACol, ARow) then
      begin
        Brush.Color := AColor;
      end
      else
      begin
        Pen.Color := clHighlightText;
      end;
    end;
    FillRect(ARect);
    Font.Color := oldColor;
    Brush := oldBrush;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.AddRowColor(ARow: Integer; cBackground, cText: TColor;
  fFontStyle : TFontStyles; FixRow : Boolean);
begin
  SetLength(FRowColorArray, Length(FRowColorArray) +1);
  if ARow > -1 then
  begin
    FRowColorArray[Length(FRowColorArray) -1].Row := ARow;
    FRowColorArray[Length(FRowColorArray) -1].Backcolor := cBackground;
    FRowColorArray[Length(FRowColorArray) -1].Textcolor := cText;
    FRowColorArray[Length(FRowColorArray) -1].TextStyle := fFontStyle;
    FRowColorArray[Length(FRowColorArray) -1].FixRowDraw := FixRow;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.ClearRowColors;
begin
  SetLength(FRowColorArray, 0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.DelRowColor(ARow: Integer);
//var
//  I, X, nRow : Integer;
begin
  //for I := 0 to Length(FRowColorArray[0]) -1 do
//  begin
//    nRow := strtoint(FRowColorArray[0, I]);
//    if nRow = ARow then
//    begin
//      // Farbe aus Array löschen
//      for X := I to Length(FRowColorArray[0]) -1 do
//      begin
//        FRowColorArray[0, X] := FRowColorArray[0, X+1];
//        FRowColorArray[1, X] := FRowColorArray[1, X+1];
//        FRowColorArray[2, X] := FRowColorArray[2, X+1];
//      end;
//      SetLength(FRowColorArray, 3, Length(FRowColorArray[0]) -1);
//    end;
//  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.IsCellSelected(X, Y: Integer): Boolean;
begin
  Result := false;
  try
    if (X >= Selection.Left) and (X <= Selection.Right) and
    (Y >= Selection.Top) and (Y <= Selection.Bottom)
    then
      Result := true;
  except
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.FormatToDate(AText : string): string;
var
  i1, i2  : integer;
begin
  if AText <> '' then
  begin
    i1:= Pos('.', AText);
    if i1 <> 3 then
    begin
      i2 := PosEx('.', AText, i1+1);
      if (i1 = 0) or (i2 = 0) then
        Result := 'No Date'
      else
        Result := Copy(AText, i2 + 1, Length(AText)- i2) +
                  Copy(AText, i1, i2 - i1 + 1) +
                  Copy(AText, 1, i1 - 1);
    end
    else
      Result := AText;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.FormatToNumber(AText : string; ACol : Integer): string;
var
  StrPos : Word;
  bNumeric : Boolean;
  nNumber : Double;
begin
  Result := AText;
  // prüfen, ob AText ein numerischer Wert ist
  StrPos := 1;
  bNumeric := True;
  if Length(AText) > 0 then
  begin
    while (StrPos <= Length(AText)) and (bNumeric = True) do
    begin
      if (AText[StrPos] in ['0'..'9']) or (AText[StrPos] in ['.',',','+','-']) then
        bNumeric := True
      else
        bNumeric := False;
      Inc(StrPos);
    end;

    // falls numerischer Wert, Text ggf. maskieren
    if bNumeric then
    begin
      if FColumns.Items[ACol].Masking then
      begin
        nNumber := GetDouble(AText);
        Result := FormatFloat(FColumns.Items[ACol].Mask, nNumber);
      end;
    end
    else
      Result := 'NaN';
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.GetDouble(const Value: string): Double;
var
  nChar : Integer;
  sDouble : string;
begin
  Result := 0;
  sDouble := '';
  for nChar := 1 to Length(Value) do
  begin
    case Value[nChar] of
      '0'..'9' : sDouble := sDouble + Value[nChar];
      '+','-'  : if nChar = 1 then
                   sDouble := sDouble + Value[nChar];
      '.'      : if nChar = Length(Value) -2 then
                   sDouble := sDouble + ',';
    end;
    if Value[nChar] = DecimalSeparator then
      sDouble := sDouble + Value[nChar];
  end;

  if sDouble <> '' then
    Result := StrToFloat(sDouble);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetFontActive(const Value: Boolean);
begin
  if FFontActive <> Value then
  begin
    FFontActive := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetFixedLineColor(const Value: TColor);
begin
  if FFixedLineColor <> Value then
  begin
    FFixedLineColor := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetGridLineColor(const Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetMultiline(const Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Coord: TGridCoord;
begin
  inherited;
  Coord := MouseCoord(X, Y);

  FSelectedRow := Coord.Y;
  FSelectedCol := Coord.X;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt : TPoint;
  aCol, aRow: Integer;
begin
  GetCursorPos(pt);
  pt := Self.ScreenToClient( pt );
  Self.MouseToCell( pt.x, pt.y, aCol, aRow );
  if (aCol >= FixedCols) and (aRow >= fixedRows) then
  begin
    {click landed in a checkbox cell}
    if Assigned( Objects[aCol, aRow] ) then
      Objects[aCol, aRow] := Nil
    else
      Objects[aCol, aRow] := Pointer(1);
    InvalidateCell( aCol, aRow );
  end;

  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.DblClick;
begin
  // click auf Spaltenüberschrift
  if (FSelectedRow = 0) and (FSelectedCol > FixedCols -1) then
  begin
    if FSortgrid.FEnabled then
    begin
      if FSelectedCol <> FLastCol then
        FSortgrid.SortDirection := sdAscending
      else
      begin
        if FSortgrid.SortDirection = sdAscending then
          FSortgrid.SortDirection := sdDescending
        else
          FSortgrid.SortDirection := sdAscending;
      end;
      FSortedCol := FSelectedCol;
      FSortgrid.Execute;
    end;
  end;

  FLastCol := FSelectedCol;
  inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  FLastRow := ARow;
  FLastCol := ACol;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetBuffered(const Value: Boolean);
begin
  if FBuffered <> Value then
  begin
    FBuffered := Value;
    if (FBuffered = True) then
      Self.DoubleBuffered := True
    else
      Self.DoubleBuffered := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.RowAppend;
var
	IsOK: Boolean;
	ColIndex: Integer;
begin
	{ append row if allowed }
	if roAllowAppend in FRowOptions then
	begin
		IsOK := True;

		{ raise OnRowAppend event }
		if Assigned(FRowAppend) then
			FRowAppend(Self, Row, IsOK);

		{ action Append if IsOK }
		if IsOK then
		begin
			{ append new row to bottom of grid }
			RowCount := RowCount + 1;

			{ set current row to new row }
			Row := RowCount - 1;

			{ blank new row - some interesting effects if you don't}
			for ColIndex := 0 to ColCount-1 do
				Cells[ColIndex, Row] := '';

			{ raise OnRowsChanged event - return current row }
			if Assigned(FRowsChanged) then
				FRowsChanged(Self, Row);
		end;
	end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.RowDelete;
begin
  RowDelete(Row);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.RowDelete(RowNr : Integer);
var
	IsOK: Boolean;
	ColIndex,
	RowIndex: Integer;
begin
	{ delete row if allowed }
	{ don't allow deletion of 1st row when only one row }
	if (roAllowDelete in FRowOptions) and (RowCount > FixedRows +1) then
	begin
		IsOK := True;

		{ raise OnRowDelete event }
		if Assigned(FRowDelete) then
			FRowDelete(Self, Row, IsOK);

		{ action Delete if IsOK }
		if IsOK then
		begin
			{ move cells data from next to last rows up one - overwriting current row}
			for RowIndex := RowNr to RowCount-2 do
				for ColIndex := 0 to ColCount-1 do
					Cells[ColIndex, RowIndex] := Cells[ColIndex, RowIndex+1];

			{ delete last row }
   	  RowCount := RowCount - 1;
      Rows[RowCount].Clear();
//      if Row > RowCount-1 then
//        Row:= RowCount-1;

			{ raise OnRowsChanged event - return current row}
			if Assigned(FRowsChanged) then
				FRowsChanged(Self, Row);
		end;
	end
  else
  begin
    Rows[Row].Clear();
  end;

end;

procedure TCHStringGrid.RowInsert;
begin
  RowInsert(Row);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.RowInsert(RowNr: Integer);
var
	IsOK: Boolean;
	ColIndex,
	RowIndex: Integer;
begin
	{ insert row if allowed }
	if roAllowInsert in FRowOptions then
	begin
		IsOK := True;

		{ raise OnRowInsert event }
		if Assigned(FRowInsert) then
			FRowInsert(Self, RowNr, IsOK);

		{ action Insert if IsOK }
		if IsOK then
		begin
			{ append new row to bottom of grid }
			RowCount := RowCount + 1;

			{ move cells data from current to old last rows down one }
			for RowIndex := RowCount-1 downto RowNr+1 do
				for ColIndex := 0 to ColCount-1 do
					Cells[ColIndex, RowIndex] := Cells[ColIndex, RowIndex-1];

			{ blank current row - effectively the newly inserted row}
			for ColIndex := 0 to ColCount-1 do
				Cells[ColIndex, RowNr] := '';

			{ raise OnRowsChanged event - return current row}
			if Assigned(FRowsChanged) then
				FRowsChanged(Self, RowNr);
		end;
	end;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetColCell(AColName: string; ARow: Integer;
  const Value: string);
var
  I, ACol : Integer;
  sColName : string;
begin
  ACol := -1;
  for I := 0 to ColCount -1 do
  begin
    sColName := Columns[I].Name;
    if AnsiSameText(sColName, AColName) then
    begin
      ACol := I;
      Break;
    end;
  end;
  if (ACol > -1) and (ARow > -1) then
    Cells[ACol, ARow] := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.GetColCell(AColName: string; ARow: Integer): string;
var
  I, ACol : Integer;
begin
  ACol := -1;
  for I := 0 to ColCount -1 do
  begin
    if UpperCase(Columns[I].Name) = UpperCase(AColName) then
    begin
      ACol := I;
      Break;
    end;
  end;
  if (ACol > -1) and (ARow > -1) then
    Result := Cells[ACol, ARow]
  else
    Result := '';
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.GetTitelCell(ATitelName: string;
  ARow: Integer): string;
var
  I, ACol : Integer;
begin
  ACol := -1;
  for I := 0 to ColCount -1 do
  begin
    if UpperCase(Columns[I].Title.Caption) = UpperCase(ATitelName) then
    begin
      ACol := I;
      Break;
    end;
  end;
  if (ACol > -1) and (ARow > -1) then
    Result := Cells[ACol, ARow]
  else
    Result := '';
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.SetTitelCell(ATitelName: string; ARow: Integer;
  const Value: string);
var
  I, ACol : Integer;
begin
  ACol := -1;
  for I := 0 to ColCount -1 do
  begin
    if UpperCase(Columns[I].Title.Caption) = UpperCase(ATitelName) then
    begin
      ACol := I;
      Break;
    end;
  end;
  if (ACol > -1) and (ARow > -1) then
    Cells[ACol, ARow] := Value;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.IsScrollBar(nFlag: Cardinal): Boolean;
begin
  Result := (GetWindowLong(Handle, GWL_STYLE) and nFlag) <> 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.OptimalColWidth(MinWidth : Integer; WithTitle : Boolean);
var
  nRow, nCol, nNewLen, nColLen, A, I : Integer;
  sText : string;
begin
  nRow := RowCount;
  nCol := ColCount;
  for I := FixedCols to nCol -1 do
  begin
    if ColWidths[I] >= 0 then
    begin
      nNewLen := 0;
      for A := 0 to nRow do
      begin
        if WithTitle and (A = FixedCols) then
          sText := Columns[I].Title.Caption
        else
          sText := Cells[I, A];

        nColLen := Canvas.TextWidth(sText);

        if nColLen > nNewLen then
          nNewLen := nColLen
      end;
      if nNewLen < MinWidth then
        nNewLen := MinWidth;
      ColWidths[I] := nNewLen + (Columns[I].MarginX * 2) + (GridLineWidth * 2) + 2;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.OptimalTableWidth;
var
  I, GrdWidth, nRest, nCols, nActColW: Integer;
  FinalWidth: Double;
  //GoOn: Boolean;
begin
  nCols := 0;
  nActColW := 0;
  //Perform(WM_SETREDRAW, 0, 0);
  try
    GrdWidth := ClientWidth;
    // alle sichtbaren Spalten auf Breite 5 setzen
    for I := FixedCols to ColCount - 1 do
    begin
      if ColWidths[I] > 0 then
        ColWidths[I] := 5;
    end;

    // Gesamtbreite aller sichtbaren Spalten berechnen
    for I := 0 to ColCount - 1 do
    begin
      if ColWidths[I] > 0 then
        nActColW := nActColW + ColWidths[I];
    end;
    // Anzahl der sichtbaren Spalten zählen
    for I := FixedCols To ColCount - 1 do
    begin
      if ColWidths[I] >= 0 then
        inc(nCols);
    end;
    nRest := GrdWidth - nActColW - (nCols * GridLineWidth);

    if IsScrollBar(WS_VSCROLL) then
      Dec(nRest, GetSystemMetrics(SM_CXVSCROLL));

    FinalWidth := ( nRest / nCols );

    for I := FixedCols To ColCount - 1 do
    begin
      if ColWidths[I] >= 0 then
      begin
        Application.ProcessMessages;
        ColWidths[I] := ColWidths[I] + Trunc(FinalWidth);
      end;
    end;


    //GoOn := True;
//    if IsScrollBar(WS_HSCROLL) then
//    begin
//      while GoOn do
//      begin
//        Application.ProcessMessages;
//        for I := ColCount - 1 downto FixedCols do
//        begin
//          if ColWidths[I] >= 0 then
//          begin
//            Application.ProcessMessages;
//            ColWidths[I] := ColWidths[I] -1;
//            if not IsScrollBar(WS_HSCROLL) then
//            begin
//              GoOn := False;
//              Break;
//            end;
//          end;
//        end;
//      end;
//    end
//    else
//    begin
//      while GoOn do
//      begin
//        Application.ProcessMessages;
//        for I := ColCount - 1 downto FixedCols do
//        begin
//          if ColWidths[I] >= 0 then
//          begin
//            Application.ProcessMessages;
//            ColWidths[I] := ColWidths[I] +1;
//            if IsScrollBar(WS_HSCROLL) then
//            begin
//              ColWidths[I] := ColWidths[I] -1;
//              GoOn := False;
//              Break;
//            end;
//          end;
//        end;
//      end;
//    end;
  finally
    //Perform(WM_SETREDRAW, 1, 0);
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringGrid.Clear;
var
  nRow : Integer;
begin
  for nRow := FixedRows to RowCount - 1 do
  begin
    Rows[nRow].Clear();
    RowHeights[nRow] := DefaultRowHeight;
  end;

  RowCount := 2;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringGrid.IsEmpty: Boolean;
var
  nRowCount, nColCount, nFirstRow, nFirstCol, I, J : Integer;
begin
  Result := True;
  nRowCount := RowCount - FixedRows;
  nColCount := ColCount - FixedCols;
  nFirstRow := FixedRows;
  nFirstCol := FixedCols;

  for I := nFirstRow to nRowCount do
  begin
    for J := nFirstCol to nColCount do
    begin
      if Trim(Cells[J, I]) <> '' then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TCHStringGrid.IsRowEmpty(ARow : Integer): Boolean;
var
  nColCount, nFirstCol, J : Integer;
begin
  Result := True;
  nColCount := ColCount - FixedCols;
  nFirstCol := FixedCols;

  for J := nFirstCol to nColCount do
  begin
    if Trim(Cells[J, ARow]) <> '' then
    begin
      Result := False;
      Exit;
    end;
  end;

end;


{ TSortGrid }

constructor TSortGrid.Create(AOwner: TCHStringGrid);
begin
  inherited Create;
  FOwner := AOwner;

  FGridToSort := FOwner;
  FStartIndex := 0;
  FEndIndex := 0;
  FSortIndex := 0;
  FSortType := stCharacter;
  FCaseSensitiv := False;
  FSortDir := sdAscending;
  FShowMsg := False;
  FArrowColor := clNavy;
  FEnabled := True;
end;

procedure TSortGrid.QuickSortGrid(sGrid: TStringGrid; StartIdx, EndIdx,
  SortIdx: Integer);
var
   j : Word;
   sortGrid, tempGrid : TStringGrid;

Function UpString(Instring : String) : String;
var
   tel : byte;
   outstring : string;
begin
   OutString := InString;
   FOR tel := 1 TO length(Instring) DO
      OutString[tel] := upcase(OutString[tel]);
   UpString := OutString;
end;

begin
   sortGrid := TStringGrid.Create(Nil);
   sortGrid.RowCount := sGrid.RowCount;
   sortGrid.ColCount := 2;
   for j := StartIdx to EndIdx do
   begin
      sortGrid.Cells[0, j] := IntToStr(j);
      sortGrid.Cells[1, j] := sGrid.Cells[SortIdx, j]
   end;

   If SortType = stCharacter Then
   begin
      If Not(CaseSensitiv) Then
         For j := StartIdx to EndIdx do
            SortGrid.Cells[1, j] := UpString(SortGrid.Cells[1, j]);
      qsortGrid(sortGrid, StartIdx, EndIdx, 1)
   end
   else if SortType = stNumeric Then
      qsortGridNumeric(sortGrid, StartIdx, EndIdx, 1)
   else if SortType = stDate Then
      qsortGridDate(sortGrid, StartIdx, EndIdx, 1);

   tempGrid := TStringGrid.Create(Nil);
   tempGrid.RowCount := sGrid.RowCount;
   tempGrid.ColCount := sGrid.ColCount;

  for j := StartIdx to EndIdx do
     tempGrid.rows[j] :=sGrid.rows[StrToInt(sortGrid.Cells[0,j])];
  for j := StartIdx to EndIdx do
     sGrid.rows[j] := tempGrid.rows[j];

   sortGrid.Free;
   If SortDirection = sdDescending THEN
   begin
      FOR j := EndIdx DOWNTO StartIdx DO
        sGrid.rows[EndIdx-j+StartIdx] := tempGrid.rows[j]
   end;
   tempGrid.Free

end;

procedure TSortGrid.BubbleSortGrid(sGrid: TStringGrid; StartIdx, EndIdx,
  SortIdx: Integer);
Var
   Idx : Word;
   Changed : Boolean;
   tempRow : TStringList;
   fields, i : Word;

begin
   tempRow :=TStringList.Create;
   fields := sGrid.ColCount;
   repeat
      Changed := False;
      for Idx := StartIdx to EndIdx-1 do
      begin
         if sGrid.Cells[SortIdx, Idx] > sGrid.Cells[SortIdx, Idx+1] then
         begin
            tempRow.Clear;
            for i := 0 to fields - 1 do
               tempRow.Add(sGrid.cells[i, Idx+1]);
            sGrid.rows[Idx+1] := sGrid.rows[Idx];
            for i := 0 to fields - 1 do
               sGrid.cells[i, Idx] := tempRow.Strings[i];
            Changed := True;
         end;
      end;
   until Changed = False;
   tempRow.Free;
end;



function TSortGrid.Execute: Boolean;
Var
   CheckForNum, NumErr : Integer;
   NumCheck : Double;
   StrToChk, sFloatStr : string;
begin
  Result := True;
  if FEnabled then
  begin
    if Assigned(FOwner.FOnBeforeSort) then
      FOwner.FOnBeforeSort(self);
    FStartIndex := FOwner.FixedRows;
    FEndIndex := FOwner.RowCount -1;
    FSortIndex := FOwner.FSelectedCol;

    if FOwner.FColumns[FSortIndex].Mode = cmNumber then
      FSortType := stNumeric
    else if FOwner.FColumns[FSortIndex].Mode = cmDate then
      FSortType := stDate
    else
      FSortType := stCharacter;

    ErrorCode := 0;
    If FGridToSort = NIL THEN
      ErrorCode := 1
    Else If FStartIndex >= FEndIndex THEN
      ErrorCode := 2
    Else If FStartIndex < 0 THEN
      ErrorCode := 3
    Else If FStartIndex > (FGridToSort.RowCount-1) THEN
      ErrorCode := 4
    Else If FEndIndex > (FGridToSort.RowCount-1) THEN
      ErrorCode := 5
    Else If FSortIndex < 0 THEN
      ErrorCode := 6
    Else If FSortIndex > (FGridToSort.ColCount-1) THEN
      ErrorCode := 7;

    If FSortType = stNumeric THEN
    begin
      for CheckForNum := FStartIndex TO FEndIndex DO
      begin
        sFloatStr := FGridToSort.Cols[FSortIndex].Strings[CheckForNum];
        sFloatStr := StringReplace(sFloatStr, '.', '', [rfReplaceAll]);
        sFloatStr := StringReplace(sFloatStr, ',', '.', [rfReplaceAll]);
        Val(sFloatStr,NumCheck,NumErr);
        if (NumErr <> 0) and (NumCheck <> NaN) then
        begin
          ErrorCode := -1;
          SortType := stCharacter
        end
        else
          If CaseSensitiv Then
            ErrorCode := -3;
      end
    end
    Else If FSortType = stDate THEN
    begin
      If CaseSensitiv Then
        ErrorCode := -4;
      For CheckForNum := FStartIndex TO FEndIndex DO
      Begin
        StrToChk := FGridToSort.Cols[FSortIndex].Strings[CheckForNum];

        SortType := stDate;
      end;
    end;
    ErrorText := ErrorTextConst[ErrorCode];

    If ErrorCode <= 0 Then
    begin
      QuickSortGrid(FGridToSort, FStartIndex, FEndIndex, FSortIndex);
      If (ErrorCode < 0) And FShowMsg Then
        MessageDlg(ErrorText,mtWarning,[mbOK],0)
    end
    else
    begin
      Result := False;
      If FShowMsg Then
        MessageDlg(ErrorText,mtError,[mbOK],0)
    end;
    if Assigned(FOwner.FOnAfterSort) then
      FOwner.FOnAfterSort(self);
  end;
end;

procedure TSortGrid.qsortGrid(sGrid: TStringGrid; StartIdx, EndIdx,
  SortIdx: Integer);
Var
   x, y : Word;
   temp: String;
   tempRow : TStringList;
   ind : Word;
   fields, i : Word;

begin
   if (EndIdx-StartIdx) < 5 then
      BubbleSortGrid(sGrid, StartIdx, EndIdx, SortIdx)
   else
   begin
      tempRow :=TStringList.Create;
      fields := sGrid.ColCount;
      if StartIdx < EndIdx then
      begin
         x:= StartIdx;
         y:= EndIdx;
         ind := (StartIdx+EndIdx) div 2;
         temp := sGrid.cells[SortIdx, ind];
         while x <= y do
         begin
            while sGrid.cells[SortIdx, x] < temp do
               Inc(x);
            while sGrid.cells[SortIdx, y] > temp do
               Dec(y);
            if x <= y then
            begin
               tempRow.Clear;
               for i := 0 to fields - 1 do
                  tempRow.Add(sGrid.cells[i, x]);
               sGrid.rows[x] := sGrid.rows[y];
               for i := 0 to fields - 1 do
                  sGrid.cells[i, y] := tempRow.Strings[i];
               Inc(x);
               Dec(y);
            end;
         end;
         tempRow.Free;
         qsortGrid(sGrid, StartIdx, y, SortIdx);
         qsortGrid(sGrid, x, EndIdx, SortIdx);
      end;
   end;

end;

procedure TSortGrid.qsortGridDate(sGrid: TStringGrid; StartIdx, EndIdx,
  SortIdx: Integer);
Var
   x, y : Word;
   temp: string;
   tempRow : TStringList;
   ind : Word;
   fields, i : Word;
begin
   tempRow :=TStringList.Create;
   fields := sGrid.ColCount;
   if StartIdx < EndIdx then
   begin
      x:= StartIdx;
      y:= EndIdx;
      ind := (StartIdx+EndIdx) div 2;
      temp := GetDateSQL(sGrid.cells[SortIdx, ind]);
      while x <= y do
      begin
         while GetDateSQL(sGrid.cells[SortIdx, x]) < temp do
               Inc(x);
         while GetDateSQL(sGrid.cells[SortIdx, y]) > temp do
               Dec(y);
         if x <= y then
         begin
            tempRow.Clear;
            for i := 0 to fields - 1 do
               tempRow.Add(sGrid.cells[i, x]);
            sGrid.rows[x] := sGrid.rows[y];
            for i := 0 to fields - 1 do
               sGrid.cells[i, y] := tempRow.Strings[i];
            Inc(x);
            Dec(y);
         end;
      end;
      tempRow.Free;
      qsortGridDate(sGrid, StartIdx, y, SortIdx);
      qsortGridDate(sGrid, x, EndIdx, SortIdx);
   end;
end;

procedure TSortGrid.qsortGridNumeric(sGrid: TStringGrid; StartIdx, EndIdx,
  SortIdx: Integer);
Var
   x, y, ind, fields, i : Word;
   temp: Extended;
   tempRow : TStringList;
begin
   tempRow :=TStringList.Create;
   fields := sGrid.ColCount;
   if StartIdx < EndIdx then
   begin
      x:= StartIdx;
      y:= EndIdx;
      ind := (StartIdx+EndIdx) div 2;
      temp := StrToFloat(StringReplace(sGrid.cells[SortIdx, ind],'.', '', [rfReplaceAll]));
      while x <= y do
      begin
         while StrToFloat(StringReplace(sGrid.cells[SortIdx, x],'.', '', [rfReplaceAll])) < temp do
            Inc(x);
         while StrToFloat(StringReplace(sGrid.cells[SortIdx, y],'.', '', [rfReplaceAll])) > temp do
            Dec(y);
         if x <= y then
         begin
            tempRow.Clear;
            for i := 0 to fields - 1 do
               tempRow.Add(sGrid.cells[i, x]);
            sGrid.rows[x] := sGrid.rows[y];
            for i := 0 to fields - 1 do
               sGrid.cells[i, y] := tempRow.Strings[i];
            Inc(x);
            Dec(y);
         end;
      end;
      tempRow.Free;
      qsortGridNumeric(sGrid, StartIdx, y, SortIdx);
      qsortGridNumeric(sGrid, x, EndIdx, SortIdx);
   end;

end;


procedure TCHStringGrid.SetHighlightColor(const Value: TColor);
begin
  FHighlightColor := Value;
end;

procedure TCHStringGrid.SetHighlightTextColor(const Value: TColor);
begin
  FHighlightTextColor := Value;
end;

procedure TCHStringGrid.SetMultilineMax(const Value: Word);
begin
  if FMultilineMax <> Value then
  begin
    FMultilineMax := Value;
    Invalidate;
  end;
end;

procedure TCHStringGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow : Integer;
begin
  inherited;
  if CellAsHint then
  begin
    if ShowHint = False Then
      ShowHint := True;

    MouseToCell(X,Y, ACol, ARow);
    if (FLastHintRow <> ARow) or (FLastHintCol <> ACol) then
    begin
      Application.CancelHint;
      FLastHintRow := ARow;
      FLastHintCol := ACol;
    end;

    if (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
      Hint := Cells[ACol,ARow];
    end
    else
      Hint := '';
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TSortGrid.GetDateSQL(a_s: string): string;
var
  i: integer;

  function FindStrPos(a_Sub, a_S: string; a_Count: integer): integer;
  var i,
      loc_Count: LongInt;
  begin
    result:= pos(a_Sub, a_S);

    if result = 0 then
       exit;

    loc_Count:= 1;
    i:= result;

    while (i > 0) and (loc_Count < a_Count) do
    begin
      i:= pos(a_Sub, copy(a_S, result+1, length(a_S)-result+1));

      result:= result + i;
      loc_Count:= loc_Count + 1;
    end;

    if loc_Count < a_Count then
       result:= 0;

    if i = 0 then
       result:= 0;
  end;

  function StrDateReverse(a_s: string): string;
  var i1,
      i2  : integer;
  begin
    i1:= pos('.', a_S);
    i2:= FindStrPos('.', a_S, 2);

    result:= RightStr(a_S, length(a_S)- i2) +
             copy(a_S, i1, i2 - i1 + 1) +
             LeftStr(a_S, i1 - 1);
  end;
begin
  i:= pos('.', a_s);
  if i = 3 then
    result:= StrDateReverse(a_s)
  else
    result:= a_s;
end;


procedure TCHStringGrid.WMCommand(var msg: TWMCommand);
begin
  if FAcceptControls then
  begin
    if EditorMode and ( msg.Ctl = InplaceEditor.Handle ) then
      inherited
    else
      if msg.Ctl <> 0 then
        msg.result := SendMessage(msg.ctl, CN_COMMAND,
                                 TMessage(msg).wparam, TMessage(msg).lparam);
  end;
end;

function TCHStringGrid.IsCellChecked(X, Y: Integer): Boolean;
begin
  if Objects[X, Y] = nil then
    Result := False
  else
    Result := True;
end;


procedure TCHGridColumn.SetColStyle(const Value: TCellStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Grid.InvalidateCol(Index);
  end;
end;

function TCHStringGrid.CheckCell(X, Y: Integer): Boolean;
begin
  Objects[X, Y] := Pointer(1);
  InvalidateCell( X, Y );
  Result := True;
end;

function TCHStringGrid.UnCheckCell(X, Y: Integer): Boolean;
begin
  Objects[X, Y] := nil;
  InvalidateCell( X, Y );
  Result := True;
end;

procedure TCHGridColumn.SetEnableHighlight(const Value: Boolean);
begin
  if FEnableHighlight <> Value then
  begin
    FEnableHighlight := Value;
    Grid.InvalidateCol(Index);
  end;
end;

end.
