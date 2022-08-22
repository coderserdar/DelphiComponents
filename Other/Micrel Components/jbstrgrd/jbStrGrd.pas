{*******************************************************}
{                                                       }
{       StringGrid extenders                            }
{                                                       }
{       Copyright (C) 1997-2013 Jaro.Benes              }
{       All right reserved                              }
{                                                       }
{       E-mail: micrel@micrel.cz                        }
{       WWW home: http://www.micrel.cz/delphi/          }
{       Target platform: D1..2009 ?                     }
{                                                       }
{       based by AdvStringGrid of CoolDev.Com           }
{       and by kjStrGrid of Kendall Jackman             }
{                                                       }
{                                                       }
{*******************************************************}
unit jbStrGrd;
{
 Updates:
  03.10.2013 Update events OnCellExit/OnCellEnter
  06.11.2012 Update component for editors and converters
  06.04.2011 Update file
  11.03.2003 SaveToFile fixup and enhanced by Xander Lutz, E-mail:Xlutz@home.nl
}
{$I jb.inc}

{English Messages}
{$DEFINE msgEnglish}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Grids, Graphics, Menus,
{$IFDEF VER7UP}Types, {$ENDIF}
{$IFDEF VER16UP}System.UITypes, {$ENDIF}
  Dialogs;

type
  TColumnValidate = procedure(Sender: TObject; ARow, ACol: Longint;
    AState: TGridDrawState; ABrush: TBrush; AFont: TFont) of object;

  TTypeOfColumn = (tocString, tocNumber, tocFloat, tocDateTime, tocBool, tocFormattedString);

  {  TColumnParams  }

  TColumnParam = class(TPersistent)
  private
    FName: string;
    FTypeOfColumn: TTypeOfColumn;
    FDisplayName: string;
    FAlignnment: TAlignment;
    FMask: string;
    FColor: TColor;
    FFont: TFont;
    FHeaderFont: TFont;
    FPickList: TStringList;
    FOnValidate: TColumnValidate;
  public
    constructor Create(const AColumnName: string);
    destructor Destroy; override;
    property Alignment: TAlignment read FAlignnment write FAlignnment;
    property Color: TColor read FColor write FColor;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Font: TFont read FFont;
    property HeaderFont: TFont read FHeaderFont write FHeaderFont;
    property Mask: string read FMask write FMask;
    property Name: string read FName write FName;
    property OnValidate: TColumnValidate read FOnValidate write FOnValidate;
    property PickList: TStringList read FPickList;
    property TypeOfColumn: TTypeOfColumn read FTypeOfColumn write FTypeOfColumn;
  end;

  TColumnParams = class(TPersistent)
  private
    FColumnParams: array of TColumnParam;
    function GetItems(Idx: Integer): TColumnParam;
    procedure SetItems(Idx: Integer; const Value: TColumnParam);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AName: string; const ATypeOfColumn: TTypeOfColumn = tocString): TColumnParam;
    function Insert(Idx: Integer; AName: string; const ATypeOfColumn: TTypeOfColumn = tocString): TColumnParam;
    procedure Delete(Idx: Integer);
    function IsValid(Idx: Integer): Boolean;
    function IsEmpty(): Boolean;
    function IndexOf(const AName: string): Integer;
    property Items[Idx: Integer]: TColumnParam read GetItems write SetItems; default;
  published
  end;


  {  TGridColorEvent  }

  TGridColorEvent = procedure(Sender: TObject; ARow, ACol: Longint;
    AState: TGridDrawState; ABrush: TBrush; AFont: TFont) of object;

  {  TGridAlignEvent  }

  TGridAlignEvent = procedure(Sender: TObject; ARow, ACol: Longint;
    var AAlignment: TAlignment) of object;

  {  TCaption2Placement  }

  TCaption2Placement = (cpTop, cpBottom);

  {  EPubStrGridError  }

  EPubStrGridError = class(Exception);

  {  TGetEditStyleEvent  }

  TGetEditStyleEvent = procedure(TSender: TObject; ACol, ARow: Integer;
    var EditStyle: TEditStyle) of object;

  {  TPubStrGrid  }

  TPubStrGrid = class(TStringGrid)
  private
    FCellDelimiter: Char; {zakladni oddelovac muze byt ; nebo | - jak se hodi}
    FAutoBackup: Boolean;
    FAutoSave: Boolean; { kdyz je TRUE, ulozi obsah mrize pred zrusenim}
    FCaption1: string; { titulek pouzity pro tabulku HTML}
    FCaption2: string; { druhy titulek pro HTML tabulku}
    FPlacement: TCaption2Placement; {kde ten druhy titulek bude}
    FInitialFile: string; {pokud je komponent vytvoren, pokusi se nacist tenhle ini, kdyz je prazdny, bude mrizka vycistena}
    FAlign: TAlignment;
    FGrid3D: Boolean; {jen pro vzhled}
    FHCol: TStrings; {header sloupcu}
    FHRow: TStrings; {a radku}
    FOnGetCellColor: TGridColorEvent; {pro individualni obarveni bunek}
    FOnGetAlignment: TGridAlignEvent; {a pro individualni zarovnani}
    FDropdownRowCount: Integer;
    FOnEditButtonClick: TNotifyEvent;
    FOnGetEditStyle: TGetEditStyleEvent;
    FOnGetPickListItems: TOnGetPickListItems;
    FOnUserChangedCell: TSetEditEvent;
    FDefTrue: string;
    FDefFalse: string;
    FInplaceEdit: TInplaceEdit;
    FColumns: TColumnParams;
    FOnNavigate: TNotifyEvent;
    procedure SetDropdownRowCount(Value: Integer);
    procedure SetOnEditButtonClick(Value: TNotifyEvent);
    procedure SetOnGetPicklistItems(Value: TOnGetPickListItems);
    function GetAlign: TAlignment;
    procedure SetAlign(const Value: TAlignment);
    procedure SetGrid3D(Value: Boolean);
    procedure InitHCol;
    procedure InitHRow;
    procedure SetHCol(Value: TStrings);
    procedure SetHRow(Value: TStrings);
{$IFNDEF VER9UP}
    procedure SetCells(ACol, ARow: Integer; const Value: string);
{$ENDIF}
    procedure SetColumnTitle(ACol: Integer; Title: string);
    procedure SetFileName(F: string);
    function GetAsBoolean(ACol, ARow: Integer): Boolean;
    function GetAsDate(ACol, ARow: Integer): TDateTime;
    function GetAsDateTime(ACol, ARow: Integer): TDateTime;
    function GetAsFloat(ACol, ARow: Integer): Double;
    function GetAsInteger(ACol, ARow: Integer): Int64;
    function GetAsTime(ACol, ARow: Integer): TDateTime;
    procedure SetAsBoolean(ACol, ARow: Integer; const Value: Boolean);
    procedure SetAsDate(ACol, ARow: Integer; const Value: TDateTime);
    procedure SetAsDateTime(ACol, ARow: Integer; const Value: TDateTime);
    procedure SetAsFloat(ACol, ARow: Integer; const Value: Double);
    procedure SetAsInteger(ACol, ARow: Integer; const Value: Int64);
    procedure SetAsTime(ACol, ARow: Integer; const Value: TDateTime);
    function GetIsNotNull(ACol, ARow: Integer): Boolean;
    function GetIsNull(ACol, ARow: Integer): Boolean;
    function GetColumnParam(ACol: Integer): TColumnParam;
  protected
    FLayoutFlag: Integer;
    FCallOnAfterEdit: Boolean;
    FGotEditText: string;
    function CreateEditor: TInplaceEdit; override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditStyle(ACol, ARow: Integer): TEditStyle; override;
    function GetEditText(Acol: Integer; Arow: Integer): string; override;
    function SelectCell(Col, Row: Longint): Boolean; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    procedure Click; override;
    procedure DblClick; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MoveTo(ACol, ARow: longint);
    procedure Deselect;
    property InplaceEdit: TInplaceEdit read FInplaceEdit;
{$IFNDEF VER9UP}
    property Cells write SetCells;
{$ENDIF}
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure Load;
    procedure Save;
    procedure WriteHTML(SList: TStrings);
    procedure InsertRow(ToIndex: LongInt);
    procedure DeleteRow(DIndex: LongInt); {$IFDEF VER5UP} reintroduce; {$ENDIF}
    function GetColumnTitle(ACol: Integer): string;
    function FindColumn(const AColName: string): Integer;
    procedure RemoveRows(RowIndex, RCount: LongInt);
    procedure InsertRows(RowIndex, RCount: LongInt);
    procedure RemoveCols(ColIndex, CCount: LongInt);
    procedure InsertCols(ColIndex, CCount: LongInt; const Title: string = '');
    procedure Clear;
    function IsCell(SubStr: string; var ACol, ARow: LongInt): Boolean;
    procedure SimpleSaveToFile(FileName: string);
    procedure SimpleLoadFromFile(FileName: string);
    function CellToReal(ACol, ARow: LongInt): Real;
    function IsInteger(ACol, ARow: LongInt): Boolean;
    function IsFloat(ACol, ARow: LongInt): Boolean;
    function IsDate(ACol, ARow: LongInt): Boolean;
    function IsDateTime(ACol, ARow: LongInt): Boolean;
    function IsTime(ACol, ARow: LongInt): Boolean;
    function IsBoolean(ACol, ARow: LongInt): Boolean;
    procedure ForceSychnronizeColumn; virtual;
    property Column[ACol: Integer]: TColumnParam read GetColumnParam; default;
    property AsBoolean[ACol, ARow: LongInt]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime[ACol, ARow: LongInt]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate[ACol, ARow: LongInt]: TDateTime read GetAsDate write SetAsDate;
    property AsFloat[ACol, ARow: LongInt]: Double read GetAsFloat write SetAsFloat;
    property AsInteger[ACol, ARow: LongInt]: Int64 read GetAsInteger write SetAsInteger;
    property AsTime[ACol, ARow: LongInt]: TDateTime read GetAsTime write SetAsTime;
    property DefFalse: string read FDefFalse write FDefFalse;
    property DefTrue: string read FDefTrue write FDefTrue;
    property IsNotNull[ACol, ARow: LongInt]: Boolean read GetIsNotNull;
    property IsNull[ACol, ARow: LongInt]: Boolean read GetIsNull;
    property ColumnTitle[ACol: Integer]: string read GetColumnTitle write SetColumnTitle;
    property Columns: TColumnParams read FColumns;
  published
    { Published declarations }
    property Alignment: TAlignment read GetAlign write SetAlign default taLeftJustify;
    property AutoBackup: Boolean read FAutoBackup write FAutoBackup default True;
    property AutoSave: Boolean read FAutoSave write FAutoSave default True;
    property Caption1: string read FCaption1 write FCaption1;
    property Caption2: string read FCaption2 write FCaption2;
    property Caption2Placement: TCaption2Placement read FPlacement write FPlacement default cpTop;
    property CellDelimiter: Char read FCellDelimiter write FCellDelimiter default '|';
    property DropdownRowCount: Integer read FDropDownRowCount write SetDropdownRowCount default 8;
    property FileName: string read FInitialFile write SetFileName;
    property Grid3D: Boolean read FGrid3D write SetGrid3D;
    property HCol: TStrings read FHCol write SetHCol;
    property HRow: TStrings read FHRow write SetHRow;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write SetOnEditButtonClick;
    property OnGetAlignment: TGridAlignEvent read FOnGetAlignment write FOnGetAlignment;
    property OnGetCellColor: TGridColorEvent read FOnGetCellColor write FOnGetCellColor;
    property OnGetEditStyle: TGetEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnGetPickListItems: TOnGetPickListItems read FOnGetPickListItems write SetOnGetPickListItems;
    property OnUserChangedCell: TSetEditEvent read FOnUserChangedCell write FOnUserChangedCell;
    property OnNavigate: TNotifyEvent read FOnNavigate write FOnNavigate;
  end;

  {** sort string grids ----------------------------------------------------}

  {  TSortTypeOf  }

  TSortTypeOf = (Character, Numeric);

  {  TSortTypeDirection  }

  TSortTypeDirection = (Ascending, Descending);

  {  TSortByCellLine  }

  TSortByCellLine = (Column, Row);

  {  TPubSortStrGrid  }

  TPubSortStrGrid = class(TComponent)
  private
    FGridToSort: TStringGrid;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FSortIndex: Integer;
    FSortType: TSortTypeOf;
    FCaseSensitiv: Boolean;
    FSortDir: TSortTypeDirection;
    FShowMsg: Boolean;
    FHowToSort: TSortByCellLine;
    procedure AutoInitialize;
    procedure QuickSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
    procedure BubbleSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
    procedure QSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
    procedure QSortGridNumeric(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    ErrorCode: Integer;
    ErrorText: string;
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property GridToSort: TStringGrid read FGridToSort write FGridToSort;
    property StartIndex: Integer read FStartIndex write FStartIndex default 0;
    property EndIndex: Integer read FEndIndex write FEndIndex default 0;
    property SortIndex: Integer read FSortIndex write FSortIndex default 0;
    property SortType: TSortTypeOf read FSortType write FSortType default Character;
    property CaseSensitiv: Boolean read FCaseSensitiv write FCaseSensitiv default False;
    property SortDirection: TSortTypeDirection read FSortDir write FSortDir default Ascending;
    property HowToSort: TSortByCellLine read FHowToSort write FHowToSort default Row;
    property ShowMessageOnError: Boolean read FShowMsg write FShowMsg default False;
  end;

procedure Register;

implementation

{$IFDEF VER5UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  {hlasky}
{$IFDEF msgEnglish}
  msgDeleteThisRow = 'Are you sure you want to delete this row?';
  msgFileNotFound = 'File %s not found';
  msgErrorGridAt = 'Error %x at position in Cell [%d, %d].';
  msgSortedAsChar = 'Column/Row contains non-numerical values for numeric sort. Sorted as Character.';
  msgOK = 'Ok';
  msgNoStrGrid = 'No StringGrid given';
  msgBoundsErr = 'StartIndex is greater or equal to EndIndex';
  msgUnderflowStartIndex = 'StartIndex is less then 0';
  msgOverflowStartIndex = 'StartIndex is greater then number of rows/columns in StringGrid';
  msgOverflowEndIndex = 'EndIndex is greater then number of rows/columns in StringGrid';
  msgUnderflowSortIndex = 'Sort Index is less then 0';
  msgOverflowSortIndex = 'Sort Index is greater then number of rows/columns in StringGrid';
  msgUnknownError = 'Any error occurred';
  msgInvalidFileName = '%s is not a valid file name.';
{$ELSE}
  msgDeleteThisRow = 'Jste si jist vymazaním tohoto øádku ?';
  msgFileNotFound = 'Soubor %s nebyl nalezen';
  msgErrorGridAt = 'Chyba %x na pozici v buòce [%d, %d].';
  msgSortedAsChar = 'Sloupec nebo øádek obsahuje neèíselné hodnoty. Tøídím jako znaky.';
  msgOK = 'Ok';
  msgNoStrGrid = 'StringGrid není';
  msgBoundsErr = 'StartIndex je vìtší èi roven EndIndex';
  msgUnderflowStartIndex = 'StartIndex je menší než 0';
  msgOverflowStartIndex = 'StartIndex je vìtší než poèet øádkù/sloupcù';
  msgOverflowEndIndex = 'EndIndex je vìtší než poèet øádkù/sloupcù';
  msgUnderflowSortIndex = 'Sort Index je menší než 0';
  msgOverflowSortIndex = 'Sort Index je vìtší než poèet øádkù/sloupcù';
  msgUnknownError = 'Nastala nìjaká chyba';
  msgInvalidFileName = '"%s" není platné jméno souboru.';
{$ENDIF}

{ TColumnParam }

constructor TColumnParam.Create(const AColumnName: string);
begin
  inherited Create;
  FName := AColumnName;
  FDisplayName := AColumnName;
  FAlignnment := taLeftJustify;
  FFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FPickList := TStringList.Create;
end;

destructor TColumnParam.Destroy;
begin
  FPickList.Free;
  FHeaderFont.Free;
  FFont.Free;
  inherited;
end;

{ TColumnParams }

function TColumnParams.Add(AName: string; const ATypeOfColumn: TTypeOfColumn): TColumnParam;
begin
  Result := TColumnParam.Create(AName);
  Result.TypeOfColumn := ATypeOfColumn;
  SetLength(FColumnParams, Length(FColumnParams) + 1);
  FColumnParams[High(FColumnParams)] := Result;
end;

procedure TColumnParams.Clear;
var
  i: Integer;
begin
  for i := Low(FColumnParams) to High(FColumnParams) do
    FColumnParams[i].Free;
  SetLength(FColumnParams, 0);
end;

constructor TColumnParams.Create;
begin
  inherited Create;
  SetLength(FColumnParams, 0);
end;

procedure TColumnParams.Delete(Idx: Integer);
var
  i: Integer;
begin
  if IsValid(Idx) then
  begin
    FColumnParams[Idx].Free;
    if Idx < High(FColumnParams) then
      for i := Idx + 1 to High(FColumnParams) do
        FColumnParams[i - 1] := FColumnParams[i];
    SetLength(FColumnParams, Length(FColumnParams) - 1);
  end;
end;

destructor TColumnParams.Destroy;
begin
  Clear;
  inherited;
end;

function TColumnParams.GetItems(Idx: Integer): TColumnParam;
begin
  if IsValid(Idx) then
    Result := FColumnParams[Idx]
  else
    Result := nil;
end;

function TColumnParams.IndexOf(const AName: string): Integer;
var
  i: Integer;
begin
  for i := Low(FColumnParams) to High(FColumnParams) do
    if AnsiSameText(AName, FColumnParams[i].Name) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TColumnParams.Insert(Idx: Integer; AName: string;
  const ATypeOfColumn: TTypeOfColumn): TColumnParam;
var
  i: Integer;
begin
  if IsValid(Idx) then
  begin
    if Idx = High(FColumnParams) then Result := Add(AName, ATypeOfColumn)
    else
    begin
      Result := TColumnParam.Create(AName);
      Result.TypeOfColumn := ATypeOfColumn;
      SetLength(FColumnParams, Length(FColumnParams) + 1);
      for i := High(FColumnParams) - 1 downto Idx do
        FColumnParams[i + 1] := FColumnParams[i];
      FColumnParams[Idx] := Result;
    end;
  end
  else
    Result := nil
end;

procedure TColumnParams.SetItems(Idx: Integer; const Value: TColumnParam);
begin
  if IsValid(Idx) then
  begin
    FColumnParams[Idx] := Value;
  end;
end;

function TColumnParams.IsEmpty: Boolean;
begin
  Result := Length(FColumnParams) = 0;
end;

function TColumnParams.IsValid(Idx: Integer): Boolean;
begin
  Result := (Idx >= Low(FColumnParams)) and (Idx <= High(FColumnParams));
end;

{  TPubStrGrid  }

procedure TPubStrGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
  procedure DrawCellText(const Value: string);
  var
    AlignValue: TAlignment;
    FontHeight: Integer;
    Rect: TRect;
  const
    Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  begin
      {determine text alignment}
    AlignValue := Alignment;
    if Assigned(FOnGetAlignment) then
      FOnGetAlignment(Self, ARow, ACol, AlignValue);
      {using of Brush for background color}
    Rect := ARect;
    Canvas.FillRect(Rect);
      {centering text in cell}
    FontHeight := Canvas.TextHeight('W');
    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) shr 1;
      Bottom := Top + FontHeight;
      Dec(Right, 2);
      Inc(Left, 2);
    end;
    {drawing of text}
    {$IFDEF CLR}
    DrawText(Canvas.Handle, Value, Length(Value), Rect, (DT_EXPANDTABS or DT_VCENTER) or Alignments[AlignValue]);
    {$ELSE}
    DrawText(Canvas.Handle, PChar(Value), Length(Value), Rect, (DT_EXPANDTABS or DT_VCENTER) or Alignments[AlignValue]);
    {$ENDIF}
  end;
  procedure DrawCellTextByHeader(const AValue: string);
  var
    c: TColumnParam;
    oldfont: TFont;
    tmpColor: TColor;
    value: string;
  begin
    c := FColumns.Items[ACol];
    oldfont := Canvas.Font;
    if c = nil then DrawCellText(AValue)
    else
    try
      value := AValue;
      if ARow > 0 then //data
      begin
        Alignment := c.Alignment;
        if (gdFocused in AState) or ((goRowSelect in Options) and (gdSelected in AState)) then
        begin
          TmpColor := Canvas.Font.Color;
          Canvas.Font := c.Font;
          Canvas.Font.Color := TmpColor;
        end
        else
        begin
          Canvas.Font := c.Font;
          //Canvas.Brush.Color := c.Color;
        end;
        if c.Mask <> '' then
        try
          case c.TypeOfColumn of
            tocString: ;
            tocNumber: Value := FormatFloat(c.Mask, StrToFloat(Value));
            tocFloat: Value := FormatFloat(c.Mask, StrToFloat(Value));
            tocDateTime: Value := FormatDateTime(c.Mask, StrToDateTime(Value));
            tocFormattedString: ;
          end;
        except
        end;
      end
      else
      begin //Title
        if c.Name = '' then Value := ColumnTitle[ACol] else Value := c.Name;
        Alignment := c.Alignment;
        Canvas.Font := c.HeaderFont;
      end;
      if ACol < FixedCols then Canvas.Brush.Color := FixedColor;
      if ARow < FixedRows then Canvas.Brush.Color := FixedColor;
      DrawCellText(value)
    finally
      Canvas.Font := oldfont;
    end;
  end;
var
  Value: string;
begin
  Value := Cells[ACol, ARow];
  {prepare color and font selection}
  if Assigned(FOnGetCellColor) then
    FOnGetCellColor(Self, ARow, ACol, AState, Canvas.Brush, Canvas.Font);

  {text draw with alignment}
  if DefaultDrawing then DrawCellTextByHeader(Value)
  else
    inherited DrawCell(ACol, ARow, ARect, AState);
  {3D look cells}
  if FGrid3D and ([goHorzLine, goVertLine] * Options = [goHorzLine, goVertLine]) then
    with ARect do
    begin
      Canvas.Pen.Color := clHighLightText;
      Canvas.PolyLine([Point(Left, Bottom - 1), Point(Left, Top),
        Point(Right, Top)]);
    end;
end;

function TPubStrGrid.FindColumn(const AColName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ColCount - 1 do
    if AnsiSameText(AColName, Cells[I, 0]) then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TPubStrGrid.ForceSychnronizeColumn;
var
  i: Integer;
  c: TColumnParam;
begin
  //nejprve se zbav vsech zaznamu o sloupcich
  FColumns.Clear;
  //a znovu je z dat rekonstuuj
  for i := 0 to ColCount - 1 do
  begin
    c := FColumns.Add(Cells[i, 0]);
    c.Font.Assign(Self.Font);
    c.HeaderFont.Assign(Self.Font);
    c.HeaderFont.Style := [];
    c.Color := Self.FixedColor;
    c.Alignment := taLeftJustify;
  end;
end;

procedure TPubStrGrid.RemoveRows(RowIndex, RCount: LongInt);
var
  i: LongInt;
begin
  for i := RowIndex to RowCount - 1 do Rows[i] := Rows[i + RCount];
  RowCount := RowCount - RCount;
end;

procedure TPubStrGrid.InsertRows(RowIndex, RCount: LongInt);
var
  i: LongInt;
begin
  RowCount := RowCount + RCount;
  for i := RowCount - 1 downto RowIndex do Rows[i] := Rows[i - RCount];
end;

procedure TPubStrGrid.RemoveCols(ColIndex, CCount: LongInt);
var
  i: LongInt;
begin
  for i := ColIndex to ColCount - 1 do Cols[i] := Cols[i + CCount];
  ColCount := ColCount - CCount;
  FColumns.Delete(ColIndex);
  if FColumns.IsEmpty then
    ForceSychnronizeColumn;
end;

procedure TPubStrGrid.InsertCols(ColIndex, CCount: LongInt; const Title: string);
var
  i: LongInt;
begin
  ColCount := ColCount + CCount;
  for i := ColCount - 1 downto ColIndex do Cols[i] := Cols[i - CCount];
  if FColumns.IsEmpty then
    ForceSychnronizeColumn;
  ColumnTitle[ColIndex] := Title;
end;

procedure TPubStrGrid.Clear;
var
  i: LongInt;
begin
  for i := 0 to ColCount - 1 do Cols[i].Clear;
end;

procedure TPubStrGrid.Click;
begin
  if Assigned(FOnNavigate) then
    FOnNavigate(Self);
  inherited;
end;

function TPubStrGrid.IsBoolean(ACol, ARow: Integer): Boolean;
begin
  Result := AnsiSameText(Cells[ACol, ARow], FDefTrue) or AnsiSameText(Cells[ACol, ARow], FDefFalse);
end;

function TPubStrGrid.IsCell(SubStr: string; var ACol, ARow: LongInt): Boolean;
var
  i, j: LongInt;
begin
  for i := 0 to RowCount - 1 do
  begin
    for j := 0 to ColCount - 1 do
    begin
      if Rows[i].Strings[j] = SubStr then
      begin
        ARow := i;
        ACol := j;
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TPubStrGrid.IsDate(ACol, ARow: Integer): Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToDate(Cells[ACol, ARow], D);
end;

function TPubStrGrid.IsDateTime(ACol, ARow: Integer): Boolean;
var
  DT: TDateTime;
begin
  Result := TryStrToDateTime(Cells[ACol, ARow], DT);
end;

function TPubStrGrid.IsFloat(ACol, ARow: Integer): Boolean;
var
  F: Double;
begin
  Result := TryStrToFloat(Cells[ACol, ARow], F);
end;

function TPubStrGrid.IsInteger(ACol, ARow: Integer): Boolean;
var
  I: Int64;
begin
  Result := TryStrToInt64(Cells[ACol, ARow], I);
end;

function TPubStrGrid.IsTime(ACol, ARow: Integer): Boolean;
var
  T: TDateTime;
begin
  Result := TryStrToTime(Cells[ACol, ARow], T);
end;

procedure TPubStrGrid.SimpleSaveToFile(FileName: string);
var
  i, j: LongInt;
  t: TStringList;
begin
  t := TStringList.Create;
  try
    t.Add(IntToStr(ColCount) + ',' + IntToStr(RowCount));
    for i := 0 to RowCount - 1 do
      for j := 0 to ColCount - 1 do
        if Cells[j, i] <> '' then
          t.Add(IntToStr(j) + ',' + IntToStr(i) + ',' + Cells[j, i]);
    t.SaveToFile(FileName);
  finally
    t.Free;
  end;
end;

procedure TPubStrGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited
end;

procedure TPubStrGrid.SimpleLoadFromFile(FileName: string);
var
  I, X, Y: Integer;
  ss, ss1: string;
  t: TStringList;
begin
  if not FileExists(FileName) then
    raise EPubStrGridError.Create(Format(msgFileNotFound, [FileName]));
  t := TStringList.Create;
  try
    t.LoadFromFile(FileName);

    I := 0;
    ss := t[I];
    if ss <> '' then
    begin
      ss1 := Copy(ss, 1, Pos(',', ss) - 1);
      ColCount := StrToInt(ss1);
      ss1 := Copy(ss, Pos(',', ss) + 1, Length(ss));
      RowCount := StrToInt(ss1);
    end;

    for I := 1 to t.Count - 1 do
    begin
      ss := t[I];

      ss1 := Copy(ss, 1, Pos(',', ss) - 1);
      ss := Copy(ss, Pos(',', ss) + 1, Length(ss));
      X := StrToInt(ss1);

      ss1 := Copy(ss, 1, Pos(',', ss) - 1);
      ss := Copy(ss, Pos(',', ss) + 1, Length(ss));
      Y := StrToInt(ss1);

      Cells[X, Y] := ss;
    end;
  finally
    t.Free;
  end;
end;

function TPubStrGrid.CellToReal(ACol, ARow: LongInt): Real;
var
  i: Real;
  Code: Integer;
begin
  Result := 0.0;
  if Cells[ACol, ARow] <> '' then
  begin
    Val(Cells[ACol, ARow], i, Code);
    if Code <> 0 then raise
      EPubStrGridError.Create(
        Format(msgErrorGridAt, [Code, ACol, ARow])
        )
    else
      Result := i;
  end;
end;

{** advanced grids ----------------------------------------------------}

constructor TPubStrGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlacement := cpTop;
  FAutoSave := True;
  FAutoBackup := True;
  Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goRowSizing,goColSizing,goEditing,goTabs, goRowMoving];
  DefaultRowHeight := 21;
  FCellDelimiter := '|';
  ColWidths[0] := 21;
  FHCol := TStringList.Create;
  FHRow := TStringList.Create;
  FAlign := taLeftJustify;
  FDropdownRowCount := 8;
  FDefTrue := 'true';
  FDefFalse := 'false';
  FCallOnAfterEdit := True;
  FGotEditText := ''; //emtpy
  FColumns := TColumnParams.Create;
end;

function TPubStrGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(Self);
  with TInplaceEditList(Result) do
  begin
    DropdownRows := FDropdownRowCount;
    OnGetPickListItems := FOnGetPickListItems;
    OnEditButtonClick := FOnEditButtonClick;
  end;
  FInplaceEdit := Result;
end;

destructor TPubStrGrid.Destroy;
begin
  if FAutoSave then
    Save;
  FHCol.Free;
  FHRow.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TPubStrGrid.Loaded;
begin
  inherited Loaded;
  if FInitialFile > '' then
    Load;
  initHCol;
  initHRow;
end;

procedure TPubStrGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
   { at the end of the Grid extend it automatically }
    VK_TAB:
      begin
        if (not (ssShift in Shift)) then
          if ((Row = RowCount - 1) and (Col = ColCount - 1)) then
            RowCount := RowCount + 1;
      end;
    VK_DOWN:
      begin
        if (Row = RowCount - 1) then
          RowCount := RowCount + 1;
      end;
    VK_INSERT: InsertRow(Row);
    VK_DELETE:
      begin
        if ssCtrl in Shift then
          if RowCount > 2 then
            if MessageDlg(msgDeleteThisRow, mtConfirmation, mbOKCancel, 0) = mrOk then
              DeleteRow(Row)
      end;
  end; { case }
  if Assigned(FOnNavigate) then
    FOnNavigate(Self);
  inherited KeyDown(Key, Shift);
end;

procedure TPubStrGrid.SetColumnTitle(ACol: Integer; Title: string);
begin
  Cells[ACol, 0] := Title;
  if FColumns.IsValid(ACol) then
    FColumns.Items[ACol].Name := Title;
end;

procedure TPubStrGrid.SetDropdownRowCount(Value: Integer);
begin
  FDropdownRowCount := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).DropdownRows := Value;
end;

procedure TPubStrGrid.SetEditText(ACol, ARow: Integer; const Value: string);
var
  AValue: string;
begin
  AValue := Value;
  if (not EditorMode) and FCallOnAfterEdit then
  begin
    if Assigned(FOnUserChangedCell) and (FGotEditText <> AValue) then
      FOnUserChangedCell(Self, ACol, ARow, AValue);
  end;
  inherited SetEditText(ACol, ARow, AValue);
  FCallOnAfterEdit := True;
end;

function TPubStrGrid.GetColumnParam(ACol: Integer): TColumnParam;
begin
  Result := FColumns.Items[ACol];
end;

function TPubStrGrid.GetColumnTitle(ACol: Integer): string;
begin
  Result := Cells[ACol, 0];
end;

function TPubStrGrid.GetEditMask(ACol, ARow: Integer): string;
begin
  Result := '';
end;

function TPubStrGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  Result := esSimple;
  if Assigned(FOnGetEditStyle) then
    FOnGetEditStyle(Self, ACol, ARow, Result);
end;

function TPubStrGrid.GetEditText(Acol, Arow: Integer): string;
begin
  //zachyceni textu pred editaci
  FGotEditText := inherited GetEditText(Acol, Arow);
  Result := FGotEditText;
end;

function TPubStrGrid.GetIsNotNull(ACol, ARow: Integer): Boolean;
begin
  Result := Cells[ACol, ARow] <> '';
end;

function TPubStrGrid.GetIsNull(ACol, ARow: Integer): Boolean;
begin
  Result := Cells[ACol, ARow] = '';
end;

procedure TPubStrGrid.SaveToFile(FileName: string);
var
  t: TStringList;
  r, c, rc, cc: Integer;
  dr, dc: Integer;
begin
  rc := RowCount;
  cc := ColCount;
  dr := DefaultRowHeight;
  dc := DefaultColWidth;
  if FAutoBackup then
    if FileExists(FileName) then
    begin
      //enhanced by Xander Lutz [Xlutz@home.nl]
      DeleteFile(ChangeFileExt(Filename, '.bak'));
      RenameFile(Filename, ChangeFileExt(Filename, '.bak'));
    end; { if }
  t := TStringList.Create;
  try
    t.Add(FCellDelimiter);
    t.Add(Format('%d'+FCellDelimiter+'%d', [rc,cc]));
    t.Add(IntToStr(DefaultRowHeight));
    t.Add(FCaption1);
    t.Add(FCaption2);
    if FPlacement = cpTop then
      t.Add('Top')
    else
      t.Add('Bottom');
    for r := 0 to rc - 1 do
      if RowHeights[r] <> dr then
        t.Add(Format('Row %d Height: %d', [r, RowHeights[r]]));
    for c := 0 to cc - 1 do
      if ColWidths[c] <> dc then
        t.Add(Format('Column %d Width: %d', [c, ColWidths[c]]));
    for r := 0 to rc - 1 do
      for c := 0 to cc - 1 do //from 0, fixed by Xander Lutz [Xlutz@home.nl]
        if Cells[c, r] > '' then
          t.Add(Format('%d'+FCellDelimiter+'%d'+FCellDelimiter+'%s', [c, r, Cells[c, r]]));
    t.Add('');
    t.SaveToFile(FileName);
  finally
    t.Free;
  end;
end;

procedure TPubStrGrid.LoadFromFile(FileName: string);
var
  u: TStringList;
  R: string;
  P: Byte;
  RecNo: LongInt;
  t, v, i, c: Integer;
begin
  if not FileExists(FileName) then
  begin
{ nehlasime nic, skryte vystoupit}
{    MessageDlg(Format(msgFileNotFound,[FileName]), mtError, [mbOK], 0); }
    Exit; {sillent exit}
  end;
  u := TStringList.Create;
  try
    u.LoadFromFile(FileName);
    if Trim(u.Text) = '' then Exit;

    RecNo := 0;

    for c := 0 to u.Count - 1 do
    begin
      R := u[c];
      Inc(RecNo);
      case RecNo of
        1: FCellDelimiter := R[1];
        2:
        begin
          P := Pos('|', R);
          RowCount := StrToInt(Copy(R, 1, P - 1));
          ColCount := StrToInt(Copy(R, P + 1, Length(R) - P));
        end;
        3: DefaultRowHeight := StrToInt(R);
        4: FCaption1 := R;
        5: FCaption2 := R;
        6:
        begin
          if R = 'Top' then
            FPlacement := cpTop
          else
            FPlacement := cpBottom

        end
      else

        if Pos('Row ', R) = 1 then //def row
        begin
          i := Pos('Height: ', R);
          t := StrToInt(Copy(R, 5, i - 6));
          v := StrToInt(Copy(R, i + 8, Length(R) - (i + 5)));
          RowHeights[t] := v;
        end
        else
          if Pos('Column ', R) = 1 then //def column
          begin
            i := Pos('Width: ', R);
            t := StrToInt(Copy(R, 8, i - 9));
            v := StrToInt(Copy(R, i + 7, Length(R) - (i + 6)));
            ColWidths[t] := v;
          end
          else
          begin //data here
            i := Pos('|', R);
            if i = 0 then
              Continue;
            t := StrToInt(Copy(R, 1, i - 1)); { cislo sloupce }
            R := Copy(R, i + 1, Length(R) - i);
            i := Pos('|', R);
            v := StrToInt(Copy(R, 1, i - 1)); { cislo radku }
            R := Copy(R, i + 1, Length(R) - i); {abych se nesplet}
            Cells[t, v] := R;
          end; { if-else-if-else-if -bla-bla-bla-bla}

      end;
    end;
  finally
    u.Free;
  end;
end;

procedure TPubStrGrid.Load;
begin
  if FInitialFile > '' then
    LoadFromFile(FInitialFile);
end;

procedure TPubStrGrid.Save;
begin
  if FInitialFile > '' then
    SaveToFile(FInitialFile);
end;

procedure TPubStrGrid.SetFileName(F: string);
begin
  FInitialFile := F;
  Load;
end;

procedure TPubStrGrid.WriteHTML(SList: TStrings);

  procedure AddSecondCaption;
  {generuje druhou hlavicku hi-hi-hi-hiiii}
  begin
    if FCaption2 > '' then
      with SList do
      begin
        Add('<tr><th nowrap colspan=' + IntToStr(ColCount - 1) + '>' + FCaption2 +
          '</th></tr>');
      end;
  end;

var
  i, j: Integer;
  s: string;
begin
  with SList do
  begin
    Add('<table border=1>'); { begin the table }
    if FCaption1 > '' then
    begin { Add the first (top) caption, if exists }
      Add('<tr><th nowrap colspan=' + IntToStr(ColCount - 1) + '>' + FCaption1 +
        '</th></tr>');
    end; { if }
    if FPlacement = cpTop then { Add the second caption if it goes on top }
      AddSecondCaption;

    Add('<tr>'); { Add titles as HTML table column headers }
    for i := 1 to ColCount - 1 do
    begin
      s := Cells[i, 0];
      if s = '' then
        s := '<br>';
      Add('<th nowrap>' + s + '</th>');
    end;
    Add('</tr>');

    for i := 1 to RowCount - 1 do
    begin
      Add('<tr>');
      for j := 1 to ColCount - 1 do
      begin
        s := Cells[j, i];
        if s = '' then
          s := '<br>';
        Add('<td nowrap>' + s + '</td>');
      end; { for }
      Add('</tr>');
    end;
    if FPlacement = cpBottom then
      AddSecondCaption;

    Add('</table><p>');
  end; { with SList }
end;

procedure TPubStrGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
  inherited RowMoved(FromIndex, ToIndex);
end;

procedure TPubStrGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TPubStrGrid.InsertRow(ToIndex: LongInt);
begin
  HideEditor;
  RowCount := RowCount + 1;
  RowMoved(RowCount, ToIndex);
end;

procedure TPubStrGrid.DblClick;
begin
  if Assigned(FOnNavigate) then
    FOnNavigate(Self);
  inherited;
end;

procedure TPubStrGrid.DeleteRow(DIndex: LongInt);
begin
  HideEditor;
  RowMoved(DIndex, RowCount);
  Rows[RowCount].Clear;
  RowCount := RowCount - 1;
end;

{** extended string grid ----------------------------------------------------}

  {uz nevim odkud jsem to opsal :-))) }

procedure TPubStrGrid.MoveTo(ACol, ARow: longint);
begin
  MoveColRow(ACol, ARow, True, True);
end;

procedure TPubStrGrid.InitHCol;
var
  I: Integer;
begin
  if (FHCol <> nil) then
    for I := 0 to pred(ColCount) do
      if I < FHCol.Count then Cells[I, 0] := FHCol[I]
      else Cells[I, 0] := '';
end;

procedure TPubStrGrid.InitHRow;
var
  I: Integer;
begin
  if (FHRow <> nil) then
    for I := 0 to RowCount - 2 do
      if I < FHRow.Count then Cells[0, I + 1] := FHRow[I]
      else Cells[0, I + 1] := '';
end;

procedure TPubStrGrid.SetHCol(Value: TStrings);
begin
  FHCol.Assign(Value);
  InitHCol;
  Refresh;
end;

procedure TPubStrGrid.SetHRow(Value: TStrings);
begin
  FHRow.Assign(Value);
  InitHRow;
  Refresh;
end;

procedure TPubStrGrid.SetOnEditButtonClick(Value: TNotifyEvent);
begin
  FOnEditButtonClick := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).OnEditButtonClick := Value;
end;

procedure TPubStrGrid.SetOnGetPicklistItems(Value: TOnGetPickListItems);
begin
  FOnGetPicklistItems := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).OnGetPickListitems := Value;
end;

function TPubStrGrid.GetAlign: TAlignment;
begin
  Result := FAlign;
end;

function TPubStrGrid.GetAsBoolean(ACol, ARow: Integer): Boolean;
begin
  Result := AnsiSameText(Cells[ACol, ARow], DefTrue);
end;

function TPubStrGrid.GetAsDate(ACol, ARow: Integer): TDateTime;
begin
  if not TryStrToDate(Cells[ACol, ARow], Result) then
    Result := 0.0;
end;

function TPubStrGrid.GetAsDateTime(ACol, ARow: Integer): TDateTime;
begin
  if not TryStrToDateTime(Cells[ACol, ARow], Result) then
    Result := 0.0;
end;

function TPubStrGrid.GetAsFloat(ACol, ARow: Integer): Double;
begin
  if not TryStrToFloat(Cells[ACol, ARow], Result) then
    Result := 0.0;
end;

function TPubStrGrid.GetAsInteger(ACol, ARow: Integer): Int64;
begin
  if not TryStrToInt64(Cells[ACol, ARow], Result) then
    Result := 0;
end;

function TPubStrGrid.GetAsTime(ACol, ARow: Integer): TDateTime;
begin
  if not TryStrToTime(Cells[ACol, ARow], Result) then
    Result := 0.0;
end;

function TPubStrGrid.SelectCell(Col, Row: Integer): Boolean;
var
  cellstring: string;
begin
  cellstring := Self.Cells[Self.col, Self.row];
  if EditorMode then
  begin
    FCallOnAfterEdit := False;

    if (Assigned(FOnUserChangedCell)) and (FGotEditText <> cellstring) then
      FOnUserChangedCell(Self, Self.Col, Self.Row, cellstring);
  end;
  Result := inherited SelectCell(Col, Row);
end;

procedure TPubStrGrid.SetAlign(const Value: TAlignment);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    Invalidate;
  end;
end;

procedure TPubStrGrid.SetAsBoolean(ACol, ARow: Integer; const Value: Boolean);
begin
  case Value of
    True: Cells[ACol, ARow] := FDefTrue;
    False: Cells[ACol, ARow] := FDefFalse;
  end;
end;

procedure TPubStrGrid.SetAsDate(ACol, ARow: Integer; const Value: TDateTime);
begin
  Cells[ACol, ARow] := DateToStr(Value);
end;

procedure TPubStrGrid.SetAsDateTime(ACol, ARow: Integer;
  const Value: TDateTime);
begin
  Cells[ACol, ARow] := DateTimeToStr(Value);
end;

procedure TPubStrGrid.SetAsFloat(ACol, ARow: Integer; const Value: Double);
begin
  Cells[ACol, ARow] := FloatToStr(Value);
end;

procedure TPubStrGrid.SetAsInteger(ACol, ARow: Integer; const Value: Int64);
begin
  Cells[ACol, ARow] := IntToStr(Value);
end;

procedure TPubStrGrid.SetAsTime(ACol, ARow: Integer; const Value: TDateTime);
begin
  Cells[ACol, ARow] := TimeToStr(Value);
end;

procedure TPubStrGrid.SetGrid3D(Value: Boolean);
begin
  if FGrid3D <> Value then
  begin
    FGrid3D := Value;
    Invalidate;
  end;
end;

{$IFNDEF VER9UP}
procedure TPubStrGrid.SetCells(ACol, ARow: Integer; const Value: string);
begin
  if Value <> inherited Cells[ACol, ARow] then
  begin
    inherited Cells[ACol, ARow] := Value;
  end;
end;
{$ENDIF}

procedure TPubStrGrid.Deselect;
var
  SRect: TGridRect;
begin
  with SRect do
  begin
    Top := 0;
    Left := ColCount;
    Bottom := 0;
    Right := Left;
  end;
  Selection := SRect;
end;

{** sort string grids ----------------------------------------------------}

const
  MinErrCode = -1;
  MaxErrCode = 7;

function ErrorTextConst(A: ShortInt): string;
begin
  case A of
    -1: Result := msgSortedAsChar;
    0: Result := msgOK;
    1: Result := msgNoStrGrid;
    2: Result := msgBoundsErr;
    3: Result := msgUnderflowStartIndex;
    4: Result := msgOverflowStartIndex;
    5: Result := msgOverflowEndIndex;
    6: Result := msgUnderflowSortIndex;
    7: Result := msgOverflowSortIndex;
  else
    Result := msgUnknownError;
  end;
end;

{  TPubSortStrGrid  }

procedure TPubSortStrGrid.AutoInitialize;
begin
  FGridToSort := nil;
  FStartIndex := 0;
  FEndIndex := 0;
  FSortIndex := 0;
  FSortType := Character;
  FCaseSensitiv := False;
  FSortDir := Ascending;
  FShowMsg := False;
  FHowToSort := Row;
end;

procedure TPubSortStrGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then
    Exit;
  if AComponent = FGridToSort then
    FGridToSort := nil;
end;

constructor TPubSortStrGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoInitialize;
end;

function TPubSortStrGrid.Execute: Boolean;
var
  CheckForNum, NumCheck{$IFNDEF VER6UP}, NumErr{$ENDIF}: Integer;
  s: string;
begin
  ErrorCode := 0;
  if FGridToSort = nil then
    ErrorCode := 1
  else
    if FStartIndex >= FEndIndex then
      ErrorCode := 2
    else
      if FStartIndex < 0 then
        ErrorCode := 3
      else
        if FStartIndex > (FGridToSort.RowCount - 1) then
          ErrorCode := 4
        else
          if FEndIndex > (FGridToSort.RowCount - 1) then
            ErrorCode := 5
          else
            if FSortIndex < 0 then
              ErrorCode := 6
            else
              if SortIndex > (FGridToSort.ColCount - 1) then
                ErrorCode := 7
              else
                if SortType = Numeric then
                  for CheckForNum := FStartIndex to FEndIndex do
                  begin
                    if FHowToSort = Row then
                      s := FGridToSort.Cols[FSortIndex].Strings[CheckForNum]
                    else
                      s := FGridToSort.Rows[FSortIndex].Strings[CheckForNum];
                   {$IFNDEF VER6UP}
                    Val(s, NumCheck, NumErr);
                    if NumErr <> 0 then
                   {$ELSE}
                    if not TryStrToInt(s, NumCheck) then
                   {$ENDIF}
                    begin
                      ErrorCode := -1;
                      SortType := Character
                    end;
                  end;

  ErrorText := ErrorTextConst(ErrorCode);
  Result := True;
  if ErrorCode <= 0 then
  begin
    QuickSortGrid(FGridToSort, FStartIndex, FEndIndex, FSortIndex);
    if (ErrorCode < 0) and FShowMsg then
      MessageDlg(ErrorText, mtWarning, [mbOK], 0)
  end
  else
  begin
    Result := False;
    if FShowMsg then
      MessageDlg(ErrorText, mtError, [mbOK], 0)
  end
end;

procedure TPubSortStrGrid.QuickSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
var
  j: Word;
  sortGrid, tempGrid: TStringGrid;

  function UpString(Instring: string): string;
  var
    tel: byte;
    outstring: string;
  begin
    OutString := InString;
    for tel := 1 to Length(Instring) do
      OutString[tel] := upcase(OutString[tel]);
    UpString := OutString;
  end;

begin
  sortGrid := TStringGrid.Create(nil);
  sortGrid.RowCount := sGrid.RowCount;
  sortGrid.ColCount := 2;
  for j := StartIdx to EndIdx do
  begin
    sortGrid.Cells[0, j] := IntToStr(j);
    if HowToSort = Row then
      sortGrid.Cells[1, j] := sGrid.Cells[SortIdx, j]
    else
      sortGrid.Cells[1, j] := sGrid.Cells[j, SortIdx]
  end;
  if SortType = Character then
  begin
    if not (CaseSensitiv) then
      for j := StartIdx to EndIdx do
        SortGrid.Cells[1, j] := UpString(SortGrid.Cells[1, j]);
    QSortGrid(sortGrid, StartIdx, EndIdx, 1)
  end
  else
    QSortGridNumeric(sortGrid, StartIdx, EndIdx, 1);
  tempGrid := TStringGrid.Create(nil);
  tempGrid.RowCount := sGrid.RowCount;
  tempGrid.ColCount := sGrid.ColCount;
  if HowToSort = Row then
  begin
    for j := StartIdx to EndIdx do
      tempGrid.rows[j] := sGrid.rows[StrToInt(sortGrid.Cells[0, j])];
    for j := StartIdx to EndIdx do
      sGrid.rows[j] := tempGrid.rows[j]
  end
  else
  begin
    for j := StartIdx to EndIdx do
      tempGrid.cols[j] := sGrid.cols[StrToInt(sortGrid.Cells[0, j])];
    for j := StartIdx to EndIdx do
      sGrid.cols[j] := tempGrid.cols[j]
  end;
  sortGrid.Free;
  if SortDirection = Descending then
  begin
    for j := EndIdx downto StartIdx do
      if HowToSort = Row then
        sGrid.rows[EndIdx - j + StartIdx] := tempGrid.rows[j]
      else
        sGrid.cols[EndIdx - j + StartIdx] := tempGrid.cols[j];
  end;
  tempGrid.Free
end;

procedure TPubSortStrGrid.BubbleSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
var
  Idx: Word;
  Changed: Boolean;
  tempRow: TStringList;
  fields, i: Word;
begin
  tempRow := TStringList.Create;
  fields := sGrid.ColCount;
  repeat
    Changed := False;
    for Idx := StartIdx to EndIdx - 1 do
    begin
      if sGrid.Cells[SortIdx, Idx] > sGrid.Cells[SortIdx, Idx + 1] then
      begin
        tempRow.Clear;
        for i := 0 to fields - 1 do
          tempRow.Add(sGrid.cells[i, Idx + 1]);
        sGrid.rows[Idx + 1] := sGrid.rows[Idx];
        for i := 0 to fields - 1 do
          sGrid.cells[i, Idx] := tempRow.Strings[i];
        Changed := True;
      end;
    end;
  until Changed = False;
  tempRow.Free;
end;

procedure TPubSortStrGrid.QSortGridNumeric(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
var
  x, y: Word;
  temp: Extended;
  tempRow: TStringList;
  ind: Word;
  fields, i: Word;
begin
  tempRow := TStringList.Create;
  fields := sGrid.ColCount;
  if StartIdx < EndIdx then
  begin
    x := StartIdx;
    y := EndIdx;
    ind := (StartIdx + EndIdx) div 2;
    temp := StrToFloat(sGrid.cells[SortIdx, ind]);
    while x <= y do
    begin
      while StrToFloat(sGrid.cells[SortIdx, x]) < temp do
        Inc(x);
      while StrToFloat(sGrid.cells[SortIdx, y]) > temp do
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
    QSortGridNumeric(sGrid, StartIdx, y, SortIdx);
    QSortGridNumeric(sGrid, x, EndIdx, SortIdx);
  end;
end;

procedure TPubSortStrGrid.QSortGrid(sGrid: TStringGrid; StartIdx, EndIdx, SortIdx: Integer);
var
  x, y: Word;
  temp: string;
  tempRow: TStringList;
  ind: Word;
  fields, i: Word;
begin
  if (EndIdx - StartIdx) < 5 then
    BubbleSortGrid(sGrid, StartIdx, EndIdx, SortIdx)
  else
  begin
    tempRow := TStringList.Create;
    fields := sGrid.ColCount;
    if StartIdx < EndIdx then
    begin
      x := StartIdx;
      y := EndIdx;
      ind := (StartIdx + EndIdx) div 2;
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
      QSortGrid(sGrid, StartIdx, y, SortIdx);
      QSortGrid(sGrid, x, EndIdx, SortIdx);
    end;
  end;
end;

{  Register  }

procedure Register;
begin
  RegisterComponents('Library', [TPubStrGrid, TPubSortStrGrid]);
end;

end.