unit Main;

{$include kcontrols.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LResources, MaskEdit,
{$ELSE}
  Windows, Messages, Mask,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, ExtCtrls, ComCtrls,
  KGrids, KControls, KDialogs, KGraphics;

type
  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    ACAddColBefore: TAction;
    ACAddColAfter: TAction;
    ACDeleteCol: TAction;
    ACAdd5000Rows: TAction;
    ACDeleteRows: TAction;
    ACInsertSortedRow: TAction;
    ACMerge: TAction;
    ACSplit: TAction;
    BUAutosizeRow: TButton;
    CBAutosizeGrid: TCheckBox;
    PPDMain: TKPrintPreviewDialog;
    PSDMain: TKPrintSetupDialog;
    PCMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GBSpecial: TGroupBox;
    CBEnabled: TCheckBox;
    CBAlignLastCol: TCheckBox;
    GBSelection: TGroupBox;
    CBRowSelect: TCheckBox;
    CBRangeSelect: TCheckBox;
    GBEditing: TGroupBox;
    CBEditing: TCheckBox;
    CBNoSelEditedText: TCheckBox;
    GBColsRows: TGroupBox;
    CBColMoving: TCheckBox;
    CBRowMoving: TCheckBox;
    CBColSizing: TCheckBox;
    CBRowSizing: TCheckBox;
    CBColSorting: TCheckBox;
    CBRowSorting: TCheckBox;
    Button2: TButton;
    BUMerge: TButton;
    BUSortRows: TButton;
    BUInsertSortedRow: TButton;
    BUModifyCell: TButton;
    BUDeleteCol: TButton;
    BUAddColAfter: TButton;
    BUDeleteRows: TButton;
    BUAdd5000Rows: TButton;
    BUAddColBefore: TButton;
    GBAppearance: TGroupBox;
    CBThemedCells: TCheckBox;
    CBMouseOverCells: TCheckBox;
    CBClippedCells: TCheckBox;
    CBDoubleBufferedCells: TCheckBox;
    KGrid1: TKGrid;
    KGrid2: TKGrid;
    KGrid3: TKGrid;
    BUPreview: TButton;
    BUPrint: TButton;
    BUAutoSizeCol: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KGrid1EditorDataFromGrid(Sender: TObject;
      AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean);
    procedure KGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      R: TRect; State: TKGridDrawState);
    procedure KGrid1EditorResize(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var ARect: TRect);
    procedure KGrid1EditorKeyPreview(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var Key: Word; Shift: TShiftState;
      var IsGridKey: Boolean);
    procedure CBAlignLastColCLick(Sender: TObject);
    procedure CBColMovingClick(Sender: TObject);
    procedure CBRowMovingClick(Sender: TObject);
    procedure CBColSizingClick(Sender: TObject);
    procedure CBRowSizingClick(Sender: TObject);
    procedure CBRowSelectClick(Sender: TObject);
    procedure CBRangeSelectClick(Sender: TObject);
    procedure CBEditingClick(Sender: TObject);
    procedure CBThemedCellsClick(Sender: TObject);
    procedure ACAddColBeforeExecute(Sender: TObject);
    procedure ACDeleteColExecute(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure KGrid1EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean);
    procedure CBMouseOverCellsClick(Sender: TObject);
    procedure ACDeleteColUpdate(Sender: TObject);
    procedure ACAdd5000RowsExecute(Sender: TObject);
    procedure ACDeleteRowsExecute(Sender: TObject);
    procedure ACDeleteRowsUpdate(Sender: TObject);
    procedure KGrid1EditorCreate(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TWinControl);
    procedure BUModifyCellClick(Sender: TObject);
    procedure CBColSortingClick(Sender: TObject);
    procedure CBRowSortingClick(Sender: TObject);
    procedure CBNoSelEditedTextClick(Sender: TObject);
    procedure CBClippedCellsClick(Sender: TObject);
    procedure CBDoubleBufferedCellsClick(Sender: TObject);
    function KGrid1CompareCells(Sender: TObject; Col1, Row1, Col2,
      Row2: Integer): Integer;
    procedure BUSortRowsClick(Sender: TObject);
    procedure ACInsertSortedRowExecute(Sender: TObject);
    procedure ACInsertSortedRowUpdate(Sender: TObject);
    procedure KGrid1BeginColSizing(Sender: TObject; var Index, Pos: Integer;
      var CanBeginSizing: Boolean);
    procedure KGrid1EditorSelect(Sender: TObject; AEditor: TWinControl; ACol,
      ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean);
    procedure ACMergeExecute(Sender: TObject);
    procedure ACMergeUpdate(Sender: TObject);
    procedure ACSplitExecute(Sender: TObject);
    procedure ACSplitUpdate(Sender: TObject);
    procedure KGrid2EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean);
    procedure KGrid3ExchangeRows(Sender: TObject; Index1, Index2: Integer);
    function KGrid3CompareCells(Sender: TObject; Col1, Row1, Col2,
      Row2: Integer): Integer;
    procedure KGrid3DrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
      State: TKGridDrawState);
    procedure KGrid3EditorCreate(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TWinControl);
    procedure KGrid3EditorDataFromGrid(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean);
    procedure KGrid3EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
      ACol, ARow: Integer; var AssignText: Boolean);
    procedure KGrid3ExchangeCols(Sender: TObject; Index1, Index2: Integer);
    procedure KGrid2DrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
      State: TKGridDrawState);
    procedure BUPrintClick(Sender: TObject);
    procedure BUPreviewClick(Sender: TObject);
    procedure KGrid1PrintPaint(Sender: TObject);
    procedure BUAutoSizeColClick(Sender: TObject);
    procedure KGrid1MeasureCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
      State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
    procedure BUAutosizeRowClick(Sender: TObject);
    procedure KGrid1ColWidthsChanged(Sender: TObject);
    procedure CBAutosizeGridClick(Sender: TObject);
    procedure KGrid1MouseDblClickCell(Sender: TObject; ACol, ARow: Integer);
  private
    { Private declarations }
    FTextToInsert: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
    FThumbnail: TKAlphaBitmap;
    procedure FillRows(At, BaseIndex, NumRows: Integer);
    procedure SetOption(Option: TKGridOption; State: Boolean);
    procedure PrepareCellPainter(ACol, ARow: Integer;
      R: TRect; State: TKGridDrawState);
  public
    { Public declarations }
    A: array of array of WideString;
  end;

var
  Form1: TForm1;

implementation

uses
{$IFDEF USE_THEMES}
  Themes,
{$ENDIF}
  KFunctions, Input;

const
  AutosizeRows: Boolean = False;

procedure ParseText(const S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  out TextPart: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  out NumberPart: Integer);
var
  I, Len, Code: Integer;
begin
  Len := Length(S);
  I := Len;
  // search for a number from the right
  while (I > 0) and (AnsiChar(S[I]) in ['0'..'9']) do Dec(I);
  // check if a number is present
  if (I > 0) and (I < Len) then
  begin
    Val(Copy(S, I, Len), NumberPart, Code);
    TextPart := Copy(S, 1, I - 1);
  end else
  begin
    TextPart := S;
    NumberPart := 0;
  end;
end;

type
  { custom cell class - contains Checked property for column with check boxes
    and Number property for all cell strings with number beside the text }
  TMyTextCell = class(TKGridAttrTextCell)
  private
    FCheckBoxState: TCheckBoxState;
    FNumber: Integer;
  protected
    procedure Initialize; override;
  public
    procedure Assign(Source: TKGridCell); override;
    // we don't need to override other methods
    property CheckBoxState: TCheckBoxState read FCheckBoxState write FCheckBoxState;
    property Number: Integer read FNumber write FNumber;
  end;

{ TMyTextCell }

procedure TMyTextCell.Assign(Source: TKGridCell);
begin
  inherited;
  if Source is TMyTextCell then
  begin
    FCheckBoxState := TMyTextCell(Source).CheckBoxState;
    FNumber := TMyTextCell(Source).Number;
  end;
end;

procedure TMyTextCell.Initialize;
begin
  inherited;
  FCheckBoxState := cbChecked;
  FNumber := 0;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J: Integer;
  Cell: TMyTextCell;
begin
  // because of BUModifyCellClick
  Randomize;
  // load thumbnail from our resources
  FThumbnail := TKAlphaBitmap.CreateFromRes('_CUBE');
  // Initialize KGrid1
{$IFNDEF MSWINDOWS}
  KGrid1.DefaultRowHeight := 34;
{$ENDIF}
    { goDoubleBufferedCells is less memory intensive than DoubleBuffered.
    But DoubleBuffered is faster, so especially suitable for big grids.
    goDoubleBufferedCells is slower and doesn't include the focus rectangle
    in goRowSelect and goRangeSelect mode - it is suitable for smaller grids.
    But you can freely decide what is the best for a particular grid in your app. }
//  KGrid1.DoubleBuffered := True;
  KGrid1.Options := KGrid1.Options + [goDoubleBufferedCells];
  // In Windows Vista and 7 this is much more nicer
  if Win32MajorVersion >= 6 then
    KGrid1.OptionsEx := KGrid1.OptionsEx + [gxFixedThemedCells];
  // register custom cell class
  KGrid1.CellClass := TMyTextCell;
  KGrid1.RealizeCellClass; // in fact this is not necessary here because all cells are still nil
  // fill cell texts
  KGrid1.Cells[0, 0] := 'My table';
  KGrid1.Cells[1, 0] := 'TEdit';
  KGrid1.Cells[2, 0] := 'TComboBox';
  KGrid1.Cells[3, 0] := 'TButton';
  KGrid1.Cells[4, 0] := 'TCheckBox';
  KGrid1.Cells[5, 0] := 'TScrollBar';
  KGrid1.Cells[6, 0] := {$IFDEF FPC}'TMemo'{$ELSE}'TRichEdit'{$ENDIF};
  KGrid1.Cells[7, 0] := 'TMaskEdit';
  KGrid1.ColWidths[0] := 60;
  KGrid1.Cells[0, 1] := 'Initial pos.';
  KGrid1.RowCount := 30;
  FillRows(2, 2, KGrid1.RowCount - 2);

  // setting custom column constraints (can be done for rows either)
  // KGrid1.MinColWidth:= 20; // this can be done for all columns
  KGrid1.Cols[4].MaxExtent := 100; // does not work in goAlignLastCol mode!
  KGrid1.Cols[6].MinExtent := 40;

  // Cell property demo - you won't probably need this but it is possible:
  Cell := TMyTextCell.Create(nil);
  try
    Cell.Text := 'hello!';
    Cell.CheckBoxState := cbUnChecked;
    KGrid1.Cell[4, 5] := Cell; // Cell is copied, we must destroy it
  finally
    Cell.Free;
  end;
  // or, more effectively, but we assume KGrid1.Cell[4, 7] is TMyTextCell:
  with TMyTextCell(KGrid1.Cell[4, 7]) do
  begin
    Text := 'hi!';
    Number := 0;
    CheckBoxState := cbUnchecked;
  end;
  // Printing settings
  KGrid1.PageSetup.Title := 'KGridDemo table';
  KGrid1.PageSetup.HeaderSpace := 1; //we want to print a custom header, cm is default

  // Initialize KGrid2 - a design-time tool would be nice here
  KGrid2.DoubleBuffered := True;
  // Header layout:
  KGrid2.CellSpan[0, 0] := MakeCellSpan(1, 3);
  KGrid2.Cells[0, 0] := 'Index';

  KGrid2.CellSpan[1, 0] := MakeCellSpan(3, 1);
  KGrid2.Cells[1, 0] := 'Top group 1';
  KGrid2.CellSpan[1, 1] := MakeCellSpan(2, 1);
  KGrid2.Cells[1, 1] := 'Center group 1';
  KGrid2.Cells[1, 2] := 'Item 1A';
  KGrid2.Cells[2, 2] := 'Item 1B';
  KGrid2.CellSpan[3, 1] := MakeCellSpan(1, 2);
  KGrid2.Cells[3, 1] := 'Item 1C';

  KGrid2.CellSpan[4, 0] := MakeCellSpan(2, 1);
  KGrid2.Cells[4, 0] := 'Top group 2';
  KGrid2.CellSpan[4, 1] := MakeCellSpan(1, 2);
  KGrid2.Cells[4, 1] := 'Item 2A';
  KGrid2.CellSpan[5, 1] := MakeCellSpan(1, 2);
  KGrid2.Cells[5, 1] := 'Item 2B';

  KGrid2.CellSpan[6, 0] := MakeCellSpan(3, 2);
  KGrid2.Cells[6, 0] := 'Top group 3';
  KGrid2.Cells[6, 2] := 'Item 3A';
  KGrid2.Cells[7, 2] := 'Item 3B';
  KGrid2.Cells[8, 2] := 'Item 3C';

  KGrid2.CellSpan[9, 0] := MakeCellSpan(1, 3);
  KGrid2.Cells[9, 0] := 'Item 4';

  KGrid2.CellSpan[10, 0] := MakeCellSpan(2, 2);
  KGrid2.Cells[10, 0] := 'Top group 4';
  KGrid2.Cells[10, 2] := 'Item 4A';
  KGrid2.Cells[11, 2] := 'Item 4B';
  // Sorting indicator positions:
  KGrid2.Cols[1].SortArrowIndex := 2;
  KGrid2.Cols[2].SortArrowIndex := 2;
  KGrid2.Cols[3].SortArrowIndex := 1;
  KGrid2.Cols[4].SortArrowIndex := 1;
  KGrid2.Cols[5].SortArrowIndex := 1;
  KGrid2.Cols[6].SortArrowIndex := 2;
  KGrid2.Cols[7].SortArrowIndex := 2;
  KGrid2.Cols[8].SortArrowIndex := 2;
  KGrid2.Cols[9].SortArrowIndex := 0; // not needed, default
  KGrid2.Cols[10].SortArrowIndex := 2;
  KGrid2.Cols[11].SortArrowIndex := 2;
  // And fill the grid with some data
  for I := 0 to KGrid2.ColCount - 1 do
    for J := 0 to KGrid2.RowCount - KGrid2.FixedRows - 1 do
    begin
      if I = 0 then
        KGrid2.Cells[I, J + KGrid2.FixedRows] := IntToStr(J + 1)
      else
        KGrid2.Cells[I, J + KGrid2.FixedRows] := Format('Item %s:%s', [Chr(Ord('A') + I - 1), Chr(Ord('A') + J)]);
    end;

  // Initialize KGrid3
  KGrid3.DoubleBuffered := True;
  SetLength(A, KGrid3.ColCount);
  for I := 0 to KGrid3.ColCount - 1 do
  begin
    SetLength(A[I], KGrid3.RowCount);
    for J := 0 to KGrid3.RowCount - 1 do
      A[I, J] := Format('Text %s:%s', [Chr(Ord('A') + I), Chr(Ord('A') + J)]);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // hiding inplace editors before destroying form is more secure in Lazarus
  KGrid1.EditorMode := False;
  KGrid2.EditorMode := False;
  KGrid3.EditorMode := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThumbNail.Free;
end;

procedure TForm1.ACAdd5000RowsExecute(Sender: TObject);
var
  At, BaseIndex, NumRows: Integer;
begin
  // index where to add new rows
  At := KGrid1.RowCount;
  // base index for cell text indexes
  BaseIndex := KGrid1.MaxRow + 1;
  // number of rows to be inserted
  NumRows := 5000;
  KGrid1.RowCount := KGrid1.RowCount + NumRows;
  FillRows(At, BaseIndex, NumRows);
end;

procedure TForm1.ACAddColBeforeExecute(Sender: TObject);
var
  ACol: Integer;
begin
  InputForm.Edit1.Text := Format('Column %d', [KGrid1.MaxCol + 1]);
  if InputForm.ShowModal = mrOk then
  begin
    ACol := KGrid1.Col;
    if Sender = ACAddColAfter then
      Inc(ACol);
    // insert new column
    KGrid1.InsertCol(ACol);
    // set column name
    KGrid1.Cells[ACol, 0] := InputForm.Edit1.Text;
    // select the cell in the same row but in the new column
    KGrid1.Col := ACol;
  end;
end;

procedure TForm1.ACDeleteColExecute(Sender: TObject);
begin
  KGrid1.DeleteCol(KGrid1.Col);
end;

procedure TForm1.ACDeleteColUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := KGrid1.ColCount > KGrid1.FixedCols + 1;
end;

procedure TForm1.ACDeleteRowsExecute(Sender: TObject);
var
  Pos: Integer;
  Sel: TKGridRect;
begin
  Sel := KGrid1.Selection;
  NormalizeGridRect(Sel); // makes Cell1 always top-left cell
  // find the 'Initial pos.'-row
  Pos := 1;
  while KGrid1.InitialRow(Pos) <> 1 do Inc(Pos);
  // delete selection except Pos
  if (Pos > Sel.Row1) and (Pos < Sel.Row2) then
  begin
    // deletion must be called twice so that only Pos remains
    // first, delete range from including Sel.Row1 to Pos
    KGrid1.DeleteRows(Sel.Row1, Pos - Sel.Row1);
    // second, delete range Pos to including Sel.Row2
    // - must start at Sel.Row1 + 1 because the first rows are already deleted
    KGrid1.DeleteRows(Sel.Row1 + 1, Sel.Row2 - Pos);
  end else
  begin
    // update deletion range so that Pos is excluded
    if Pos = Sel.Row1 then
      Inc(Sel.Row1)
    else if Pos = Sel.Row2 then
      Dec(Sel.Row2);
    // delete range from including Sel.Row1 to including Sel.Row2
    KGrid1.DeleteRows(Sel.Row1, Sel.Row2 - Sel.Row1 + 1);
  end;
  // select one row (in row mode)
  KGrid1.Row := Sel.Row1;
end;

procedure TForm1.ACDeleteRowsUpdate(Sender: TObject);
var
  Sel: TKGridRect;
begin
  Sel := KGrid1.Selection;
  TAction(Sender).Enabled := (Sel.Row1 <> Sel.Row2) or
    (KGrid1.InitialRow(Sel.Row1) <> 1);
end;

procedure TForm1.ACInsertSortedRowExecute(Sender: TObject);
var
  ByCol, ARow: Integer;
  Cell: TMyTextCell;
begin
  { Inserts a new row into the grid at a position determined
    by the value of FTextToInsert in the previously sorted column. }
  FTextToInsert := Chr(Ord('A') + Random(Ord('Z') - Ord('A') + 1)) + '_text';
  { Inserts an empty row at the corresponding position. If rows are not sorted
    at this point, InsertSortedRow does nothing and returns False. During
    InsertSortedRow, a non recursive binary tree search is performed and
    the KGrid1CompareCells event handler is called several times with slightly
    different parameters than during SortCols or SortRows. }
  if KGrid1.InsertSortedRow(ByCol, ARow) then
  begin
    { Normally, if you modify a cell that belongs to the sorted column or row,
      the SortMode property of that column or row is cleared, i.e. assigned to
      smNone. LockSortMode prevents this. Here we want to preserve the grid's
      sort status. }
    KGrid1.LockSortMode;
    try
      // fill the newly inserted row
      FillRows(ARow, KGrid1.MaxRow + 1, 1);
      Cell := TMyTextCell(KGrid1.Cell[ByCol, ARow]);
      Cell.Number := 0;
      Cell.Text := FTextToInsert;
    finally
      KGrid1.UnlockSortMode;
    end;
    { focus the cell that lies on the newly inserted row and belongs to
      the sorted column. }
    KGrid1.FocusCell(ByCol, ARow);
  end;
end;

procedure TForm1.ACInsertSortedRowUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := KGrid1.SortCol >= KGrid1.FixedCols;
end;

procedure TForm1.ACMergeExecute(Sender: TObject);
var
  R: TKGridRect;
begin
  R := KGrid1.Selection;
  NormalizeGridRect(R);
  { merging an area of cells is simple, just assign corresponding cell span
    parameters to the top left cell (base cell) of the planned merged area. }
  KGrid1.CellSpan[R.Col1, R.Row1] := MakeCellSpan(R.Col2 - R.Col1 + 1, R.Row2 - R.Row1 + 1);
  KGrid1.Selection := R;
end;

procedure TForm1.ACMergeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not (goRowSelect in KGrid1.Options) and
   ((KGrid1.Selection.Col1 <> KGrid1.Selection.Col2) or
   (KGrid1.Selection.Row1 <> KGrid1.Selection.Row2));
end;

procedure TForm1.ACSplitExecute(Sender: TObject);
begin
  { splitting a previously merged area of cells can be done by assigning the
    default cell span parameters to the top left cell (base cell) of the merged area. }
  KGrid1.CellSpan[KGrid1.Col, KGrid1.Row] := MakeCellSpan(1, 1);
end;

procedure TForm1.ACSplitUpdate(Sender: TObject);
var
  Span: TKGridCellSpan;
begin
  Span := KGrid1.CellSpan[KGrid1.Col, KGrid1.Row];
  TAction(Sender).Enabled := (Span.ColSpan > 1) or (Span.RowSpan > 1);
end;

procedure TForm1.BUAutoSizeColClick(Sender: TObject);
var
  RandomCol: Integer;
begin
  RandomCol := Random(KGrid1.ColCount - KGrid1.FixedCols) + KGrid1.FixedCols;
  KGrid1.AutoSizeCol(RandomCol);
  KGrid1.FocusCell(RandomCol, KGrid1.Row);
end;

procedure TForm1.BUAutosizeRowClick(Sender: TObject);
var
  RandomRow: Integer;
begin
  RandomRow := Random(KGrid1.RowCount - KGrid1.FixedRows) + KGrid1.FixedRows;
  KGrid1.AutoSizeRow(RandomRow);
  KGrid1.FocusCell(KGrid1.Col, RandomRow);
end;

procedure TForm1.BUModifyCellClick(Sender: TObject);
var
  RandomCol, RandomRow: Integer;
begin
  KGrid1.EditorMode := False;
  RandomCol := Random(KGrid1.ColCount - KGrid1.FixedCols) + KGrid1.FixedCols;
  RandomRow := Random(KGrid1.RowCount - KGrid1.FixedRows) + KGrid1.FixedRows;
  KGrid1.FindBaseCell(RandomCol, RandomRow, RandomCol, RandomRow);
  with TMyTextCell(KGrid1.Cell[RandomCol, RandomRow]) do
  begin
    Text := 'changed!';
    Number := 0;
    Font.Assign(KGrid1.Font);
    Font.Style := [fsBold];
    Font.Color := RGB(Random(64), Random(64), Random(64));
    Brush.Style := TBrushStyle(Random(Integer(High(TBrushStyle)) - 1));
    if Brush.Style = bsClear then Brush.Style := bsSolid;
    Brush.Color := RGB(128 + Random(127), 128 + Random(127), 128 + Random(127));
    BackColor := clWhite;
    case CheckBoxState of
      cbUnchecked: CheckBoxState := cbGrayed;
      cbChecked: CheckBoxState := cbUnchecked;
      cbGrayed: CheckBoxState := cbChecked;
    end;
  end;
  KGrid1.UpdateSortMode(RandomCol, RandomRow);
  KGrid1.FocusCell(RandomCol, RandomRow);
end;

procedure TForm1.BUSortRowsClick(Sender: TObject);
var
  RandomCol: Integer;
begin
  RandomCol := Random(KGrid1.ColCount);
  KGrid1.SortRows(RandomCol, TKGridSortMode(Random(Integer(High(TKGridSortMode))) + 1));
end;

procedure TForm1.BUPreviewClick(Sender: TObject);
begin
  PPDMain.Show;
end;

procedure TForm1.BUPrintClick(Sender: TObject);
begin
  PSDMain.Execute;
end;

procedure TForm1.CBAlignLastColCLick(Sender: TObject);
begin
  SetOption(goAlignLastCol, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBAutosizeGridClick(Sender: TObject);
begin
  AutosizeRows := TCheckBox(Sender).Checked;
end;

procedure TForm1.CBClippedCellsClick(Sender: TObject);
begin
  SetOption(goClippedCells, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBColMovingClick(Sender: TObject);
begin
  SetOption(goColMoving, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBColSizingClick(Sender: TObject);
begin
  SetOption(goColSizing, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBColSortingClick(Sender: TObject);
begin
  SetOption(goColSorting, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBDoubleBufferedCellsClick(Sender: TObject);
begin
  SetOption(goDoubleBufferedCells, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBEditingClick(Sender: TObject);
begin
  SetOption(goEditing, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBEnabledClick(Sender: TObject);
begin
  KGrid1.Enabled := CBEnabled.Checked;
end;

procedure TForm1.CBMouseOverCellsClick(Sender: TObject);
begin
  SetOption(goMouseOverCells, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBNoSelEditedTextClick(Sender: TObject);
begin
  SetOption(goNoSelEditText, not TCheckBox(Sender).Checked);
end;

procedure TForm1.CBRangeSelectClick(Sender: TObject);
begin
  SetOption(goRangeSelect, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBRowMovingClick(Sender: TObject);
begin
  SetOption(goRowMoving, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBRowSelectClick(Sender: TObject);
begin
  SetOption(goRowSelect, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBRowSizingClick(Sender: TObject);
begin
  SetOption(goRowSizing, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBRowSortingClick(Sender: TObject);
begin
  SetOption(goRowSorting, TCheckBox(Sender).Checked);
end;

procedure TForm1.CBThemedCellsClick(Sender: TObject);
begin
  SetOption(goThemedCells, TCheckBox(Sender).Checked);
end;

procedure TForm1.KGrid1EditorCreate(Sender: TObject; ACol, ARow: Integer;
  var AEditor: TWinControl);
var
  InitialCol, InitialRow: Integer;
begin
  { It is possible to map current column or row indexes to
    their initial values for easier event handling.
    If the user moves, adds or deletes a column or row,
    InitialCol and InitialRow hold the initial
    positions of the column or row. You need initial positions
    only if you test column or row indexes absolutely by numbers
    like here in EditorCreate event handler. }
  InitialCol := KGrid1.InitialCol(ACol); // map column indexes
  InitialRow := KGrid1.InitialRow(ARow); // map row indexes
  // do not create any editor in the 1.row
  if InitialRow = KGrid1.FixedRows then Exit
  // new feature: create TEdit in the fixed rows!
  else if InitialRow < KGrid1.FixedRows then
  begin
    if gxEditFixedRows in KGrid1.OptionsEx then
      AEditor := TEdit.Create(nil);
    Exit;
  end;
  // create custom editors
  case InitialCol of
    1:
    begin
      AEditor := TEdit.Create(nil);
    end;
    2:
    begin
      AEditor := TComboBox.Create(nil);
      TComboBox(AEditor).Style := csDropDown; // cannot set height on Win!
    end;
    3:
    begin
      AEditor := TButton.Create(nil);
    end;
    4:
    begin
      AEditor := TCheckBox.Create(nil);
      TCheckBox(AEditor).Font.Color := clRed; // applies only without OS themes (in Delphi)
    end;
    5:
    begin
      AEditor := TScrollBar.Create(nil);
      TScrollBar(AEditor).Max := 10;
    end;
    6:
    begin
      AEditor := {$IFDEF FPC}TMemo{$ELSE}TRichEdit{$ENDIF}.Create(nil);
      AEditor.Cursor:= crIBeam;
    end;
    7:
    begin
      AEditor := TMaskEdit.Create(nil);
    end
  else
    if gxEditFixedCols in KGrid1.OptionsEx then
      AEditor := TEdit.Create(nil);
  end;
end;

procedure TForm1.KGrid1EditorDataFromGrid(Sender: TObject;
  AEditor: TWinControl; ACol, ARow: Integer; var AssignText: Boolean);
var
  I, InitialCol, InitialRow: Integer;
  Cell: TMyTextCell;
  S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  InitialCol := KGrid1.InitialCol(ACol); // map column indexes
  InitialRow := KGrid1.InitialRow(ARow); // map row indexes
  Cell := TMyTextCell(KGrid1.Cell[ACol, ARow]);
  { Set data from the grid to the editors in an user defined way.
    Data can be set in EditorCreate but some assignments need
    that the inplace editor has a parent control and this is
    the fact here (grid is always parent of inplace editor). }
  if InitialRow < KGrid1.FixedRows then Exit;
  S := Cell.Text + ' ' + IntToStr(Cell.Number);
  case InitialCol of
    2: with TComboBox(AEditor) do
    begin
      for I := 0 to 9 do
        Items.Add('item ' + IntToStr(InitialRow + I));
      ItemIndex := Items.IndexOf(Cell.Text)
    end;
    4:
    begin
      TCheckBox(AEditor).Checked := Cell.CheckBoxState = cbChecked;
    end;
    7:
    begin
      // does not work good in some versions of Lazarus
      TMaskEdit(AEditor).EditMask := Copy('00:00_9999999', 1, Length(S)) + ';1;_';
      TMaskEdit(AEditor).EditText := S;
      AssignText := False;
    end;
  end;
  { We must combine the text and number and assign it to the editor: }
  if AssignText and (Cell.Number <> 0) then
  begin
    SetControlText(AEditor, S);
    AssignText := False;
  end;
end;

procedure TForm1.KGrid1EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
var
  InitialCol, InitialRow: Integer;
  Cell: TMyTextCell;
  S, S1: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
  Value: Integer;
begin
  InitialRow := KGrid1.InitialRow(ARow); // map row indexes
  InitialCol := KGrid1.InitialCol(ACol); // map column indexes
  Cell := TMyTextCell(KGrid1.Cell[ACol, ARow]);
  if InitialRow < KGrid1.FixedRows then Exit;
  // set data from the editors to grid in a user defined way
  case InitialCol of
    4: if  TCheckBox(AEditor).Checked then Cell.CheckBoxState := cbChecked else Cell.CheckBoxState := cbUnchecked;
  end;
  if InitialCol <> 7 then
    S := GetControlText(AEditor)
  else
    S := TMaskEdit(AEditor).EditText;
  { We must parse text coming from AEditor here to identify if there is
    a number index available in the text or not: }
  ParseText(S, S1, Value);
  if (Cell.Text <> S1) or (Cell.Number <> Value) then
  begin
    Cell.Text := S1;
    Cell.Number := Value;
    { Call this method to quickly update KGrid's sorting interface after data has
      been modified in a single cell. Prior KGrid 1.3 changing cell data enforced
      any sort modes to be cleared. }
    KGrid1.UpdateSortMode(ACol, ARow);
  end;
  AssignText := False;
end;

procedure TForm1.KGrid1EditorKeyPreview(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var Key: Word; Shift: TShiftState;
  var IsGridKey: Boolean);
begin
  // preview the key that is sent to the editor
  // sometimes this key needs to be handled by the grid instead
  // the e.g. EditKeyPreview decides whether the key "belongs" to the grid
  // based on these routines you can write other decision functions for your editors
  KGrid1.DefaultEditorKeyPreview(AEditor, ACol, ARow, Key, Shift, IsGridKey);
end;

procedure TForm1.KGrid1EditorResize(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var ARect: TRect);
{$IFNDEF LCLGTK2}
var
  InitialCol: Integer;
{$ENDIF}
begin
{$IFNDEF LCLGTK2}
  InitialCol := KGrid1.InitialCol(ACol); // map column indexes
  // you can change the position and size of your editor within the cell here
  case InitialCol of
    4: Inc(ARect.Left, 2);
  end;
{$ENDIF}
end;

procedure TForm1.KGrid1EditorSelect(Sender: TObject; AEditor: TWinControl; ACol,
  ARow: Integer; SelectAll, CaretToLeft, SelectedByMouse: Boolean);
begin
  if SelectedByMouse and (KGrid1.InitialCol(ACol) in [4]) then
    KGrid1.ThroughClick := True;
  KGrid1.DefaultEditorSelect(AEditor, ACol, ARow, SelectAll, CaretToLeft, SelectedByMouse);
end;

procedure TForm1.KGrid1PrintPaint(Sender: TObject);
begin
  // print custom header (here almost the same as poTitle)
  // there are always printer device context units here, even for previewing
  with KGrid1.PageSetup do
  begin
    Canvas.Font.Name := 'Arial';
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;
    Canvas.Font.Height := VMap(14);
    Canvas.TextOut(PrinterMarginLeftMirrored, PrinterMarginTop, Title);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(Rect(PrinterMarginLeftMirrored, PrinterMarginTop + VMap(20), PageWidth - PrinterMarginRightMirrored, PrinterMarginTop + VMap(22)));
  end;
end;

procedure TForm1.KGrid1BeginColSizing(Sender: TObject; var Index, Pos: Integer;
  var CanBeginSizing: Boolean);
begin
  if Index = 0 then
    CanBeginSizing := False; // simulate old KGrid 1.2 behavior
end;

procedure TForm1.KGrid1ColWidthsChanged(Sender: TObject);
begin
  if AutosizeRows then
    KGrid1.AutosizeGrid(mpRowHeight, False);
end;

function TForm1.KGrid1CompareCells(Sender: TObject; Col1, Row1, Col2,
  Row2: Integer): Integer;
var
  InsertNew: Boolean;
  Cell1, Cell2: TMyTextCell;
{$IFDEF STRING_IS_UNICODE}
  S1, S2: string;
{$ELSE}
  W1, W2: PWideChar;
  DelW1, DelW2: Boolean;
{$ENDIF}
begin
  { This event handler should be VERY FAST to optimize sorting if there is
    a large amount of sorted rows or columns. That is why direct access
    to cell array without index range checks is used here. In addition,
    as TKGridTextCell uses PWideChar internally instead of WideString
    because of speed of course, we access PWideChar buffers directly through
    TextPtr property. In Delphi 2009 or Lazarus this approach is not needed and you can
    access the Text property without affecting the performance.
    Stack consumption is not critical because a non recursive quick sort algorithm
    is used to sort the cells. }
  { InsertNew indicates that this event handler has been called
    from InsertSortedCol or InsertSortedRow. }
  InsertNew := (Col1 = cInvalidIndex) or (Row1 = cInvalidIndex);
{$IFDEF STRING_IS_UNICODE}
  if InsertNew then
  begin
    // get string that is about to be inserted
    Cell1 := nil;
    S1 := FTextToInsert;
  end else
  begin
    // get string beeing displayed in Cell1 (except a number displayed beneath text)
    Cell1 := TMyTextCell(KGrid1.ArrayOfCells[Row1, Col1]);
    if KGrid1.InitialRow(Row1) = 1 then
      S1 := IntToStr(KGrid1.Cols[Col1].InitialPos)
    else
      S1 := Cell1.Text;
  end;
  // get string beeing displayed in Cell2 (except a number displayed beneath text)
  Cell2 := TMyTextCell(KGrid1.ArrayOfCells[Row2, Col2]);
  if KGrid1.InitialRow(Row2) = 1 then
    S2 := IntToStr(KGrid1.Cols[Col1].InitialPos)
  else
    S2 := Cell2.TextPtr;
  Result := CompareStrings(S1, S2);
  if (Result = 0) and (Cell1 <> nil) then
    // try to compare numbers if strings are equal
    Result := CompareIntegers(Cell1.Number, Cell2.Number);
{$ELSE}
  DelW1 := False;
  if InsertNew then
  begin
    // get string that is about to be inserted
    Cell1 := nil;
    W1 := PWideChar(FTextToInsert);
  end else
  begin
    // get string beeing displayed in Cell1 (except a number displayed beneath text)
    Cell1 := TMyTextCell(KGrid1.ArrayOfCells[Row1, Col1]);
    if KGrid1.InitialRow(Row1) = 1 then
    begin
      { Get initial column positions in the (initially) first row. This is not
        very fast but, generally, we have to convert Integer to PWideChar. }
      W1 := AnsiStringToWideChar(IntToStr(KGrid1.Cols[Col1].InitialPos));
      DelW1 := True;
    end else
      W1 := Cell1.TextPtr;
  end;
  DelW2 := False;
  // get string beeing displayed in Cell2 (except a number displayed beneath text)
  Cell2 := TMyTextCell(KGrid1.ArrayOfCells[Row2, Col2]);
  if KGrid1.InitialRow(Row2) = 1 then
  begin
    { Get initial column positions in the (initially) first row. This is not
      very fast but we have to convert Integer to PWideChar somehow. }
    W2 := AnsiStringToWideChar(IntToStr(KGrid1.Cols[Col2].InitialPos));
    DelW2 := True;
  end else
    W2 := Cell2.TextPtr;
  try
    Result := CompareWideChars(W1, W2);
    if (Result = 0) and (Cell1 <> nil) then
      // try to compare numbers if strings are equal
      Result := CompareIntegers(Cell1.Number, Cell2.Number);
  finally
    if DelW1 then FreeMem(W1);
    if DelW2 then FreeMem(W2);
  end;
{$ENDIF}
end;

procedure TForm1.KGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
begin
  { You can still paint cells here as in TStringGrid or TDrawGrid, i.e.
    use the KGrid1.Canvas to paint cell contents. However, there is a versatile
    CellPainter instance available that belongs to the grid and can perform
    many common cell painting tasks related to a grid control.
    Painting is performed via CellPainter.Canvas that is either equal to
    KGrid1.Canvas or is a memory buffer if goDoubleBufferedCells is active.
    You can use many Draw... methods from TKGridCellPainter or override
    the TKGridCellPainter class to adapt behavior. You must paint to
    CellPainter.Canvas if you wish to use goDoubleBufferedCells
    painting mode. Accessing CellPainter is also meaningfull only
    in OnDrawCell or cell aware DrawCell/ApplyDrawProperties method. }

  // To enable full autosize feature, following function is needed
  PrepareCellPainter(ACol, ARow, R, State);
  { Calling CellPainter.DefaultDraw ensures standard cell painting with
    modified attributes like Brush or Font. You can still use
    DefaultDrawCell as in KGrid 1.0. but it is deprecated. }
  KGrid1.CellPainter.DefaultDraw;
end;

procedure TForm1.KGrid1MeasureCell(Sender: TObject; ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState; Priority: TKGridMeasureCellPriority; var Extent: TPoint);
begin
  { To enable full autosize feature, we must call exactly the same code
    affecting the cellpainter settings as in OnDrawCell. This code cannot paint anything! }
  PrepareCellPainter(ACol, ARow, R, State);
  { Calling CellPainter.DefaultMeasure obtains the cell contents from default
    implementation }
  Extent := KGrid1.CellPainter.DefaultMeasure(Priority);
end;

procedure TForm1.KGrid1MouseDblClickCell(Sender: TObject; ACol, ARow: Integer);
begin
  ;
end;

procedure TForm1.FillRows(At, BaseIndex, NumRows: Integer);
var
  I, J, ACol: Integer;
  S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  // switch to fast operation mode
  KGrid1.LockUpdate;
  try
    for J := 0 to NumRows - 1 do
      for I := 0 to KGrid1.ColCount - 1 do
      begin
        ACol := KGrid1.InitialCol(I);
        case ACol of
          0: S := 'Row';
          1: S := 'edited text';
          2: S := 'item';
          3: S := 'button';
          4: S := 'option';
          5: S := 'scroll';
          6: S := {$IFDEF FPC}'memo'{$ELSE}'richedit'{$ENDIF};
          7: S := '12:00';
        else
          S := '';
        end;
        if S <> '' then with TMyTextCell(KGrid1.Cell[I, At + J]) do
        begin
          Text := S;
          Number := BaseIndex + J;
        end;
      end;
  finally
    // switch back to normal operation mode and force grid updating
    KGrid1.UnlockUpdate;
  end;
end;

procedure TForm1.PrepareCellPainter(ACol, ARow: Integer;
  R: TRect; State: TKGridDrawState);
var
  InitialCol, InitialRow: Integer;
  Cell: TMyTextCell;
begin
  InitialCol := KGrid1.InitialCol(ACol); // map column indexes
  InitialRow := KGrid1.InitialRow(ARow); // map row indexes
  Cell := TMyTextCell(KGrid1.Cell[ACol, ARow]); // we know it is always the case
  { apply specific paint properties first - applies the cell class specific
    settings to the CellPainter. If you write your own cell class make any
    specific drawing adjustments to be accessible through ApplyDrawProperties. }
  Cell.ApplyDrawProperties;
  // get cell text
  if (InitialRow = 1) and (InitialCol > 0) then
    // display initial column positions in the (initially) first row
    KGrid1.CellPainter.Text := IntToStr(KGrid1.Cols[ACol].InitialPos)
  else if Cell.Number <> 0 then
    { Display cell text and cell number. The Text property of CellPainter
      is already filled with the Text property of the corresponding TKGridTextCell
      cell instance. }
    KGrid1.CellPainter.Text := KGrid1.CellPainter.Text + ' ' + IntToStr(Cell.Number);
  // mouse "hover" simulation for default grid colors
  with KGrid1.CellPainter.Canvas do
  begin
    if gdMouseOver in State then
      if gdFocused in State then
        Brush.Color := BrightColor(Brush.Color, 0.2, bsOfTop)
      else if ColorToRGB(Brush.Color) <> clWhite then
        Brush.Color := BrightColor(Brush.Color, 0.4, bsOfTop)
      else
        Brush.Color := $E0F8F8;
  end;
  if InitialRow > 1 then
  begin
    if InitialCol = 3 then
    begin
      { Painting a graphic in a cell. Reworked in version 1.6. Incorporated into DefaultDraw. }
      KGrid1.CellPainter.GraphicHAlign := halLeft;
      KGrid1.CellPainter.GraphicVAlign := valCenter;
      KGrid1.CellPainter.GraphicVPadding := 1;
      KGrid1.CellPainter.GraphicDrawText := True;
      KGrid1.CellPainter.Graphic := FThumbNail;
      KGrid1.CellPainter.GraphicStretchMode := stmZoomOutOnly;
    end
    else if InitialCol = 4 then
    begin
      { Painting a check box frame in the cell is very likely a common programming
        task. That's why I encapsulated this task into TKGridCellPainter. }
      KGrid1.CellPainter.CheckBox := True;
      KGrid1.CellPainter.CheckBoxState := TMyTextCell(KGrid1.Cell[ACol, ARow]).CheckBoxState;
//      KGrid1.CellPainter.CheckBoxHAlign := halRight;
    {$IFDEF LCLQT}
      // QT workaround: QT cannot draw checkbox transparent here
      if gdEdited in State then
        KGrid1.CellPainter.Canvas.Brush.Color := KGrid1.Color;
    {$ENDIF}
    end;
  end;
  // apply custom text output attributes
  if (InitialRow > 1) and (InitialCol = 6) then
    KGrid1.CellPainter.Attributes := [taEndEllipsis, taLineBreak, taWordBreak]
  else
    KGrid1.CellPainter.Attributes := [taEndEllipsis, taWordBreak]; // this is the default
end;

procedure TForm1.SetOption(Option: TKGridOption; State: Boolean);
begin
  if State then
    KGrid1.Options := KGrid1.Options + [Option]
  else
    KGrid1.Options := KGrid1.Options - [Option];
end;

procedure TForm1.KGrid2DrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
  State: TKGridDrawState);
begin
  KGrid2.Cell[ACol, ARow].ApplyDrawProperties;
  if State * [gdFixed, gdSelected] = [] then
  begin
    if ARow mod 2 = 0 then
      KGrid2.CellPainter.Canvas.Brush.Color := KGrid2.Color
    else
      KGrid2.CellPainter.Canvas.Brush.Color := RGB(240,240,255);
  end;
  KGrid2.CellPainter.DefaultDraw;
end;

procedure TForm1.KGrid2EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
begin
  KGrid2.Cells[ACol, ARow] := GetControlText(AEditor);
  KGrid2.UpdateSortMode(ACol, ARow);
  AssignText := False;
end;

function TForm1.KGrid3CompareCells(Sender: TObject; Col1, Row1, Col2,
  Row2: Integer): Integer;
begin
  Result := {$IFDEF STRING_IS_UNICODE}CompareStrings{$ELSE}CompareWideStrings{$ENDIF}(A[Col1, Row1], A[Col2, Row2]);
end;

procedure TForm1.KGrid3DrawCell(Sender: TObject; ACol, ARow: Integer; R: TRect;
  State: TKGridDrawState);
begin
  KGrid3.CellPainter.Text := A[ACol, ARow];
  KGrid3.CellPainter.Attributes := [taEndEllipsis];
  KGrid3.CellPainter.DefaultDraw;
end;

procedure TForm1.KGrid3EditorCreate(Sender: TObject; ACol, ARow: Integer;
  var AEditor: TWinControl);
begin
  AEditor := TEdit.Create(nil);
end;

procedure TForm1.KGrid3EditorDataFromGrid(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
begin
  (AEditor as TEdit).Text := A[ACol, ARow];
  AssignText := False;
end;

procedure TForm1.KGrid3EditorDataToGrid(Sender: TObject; AEditor: TWinControl;
  ACol, ARow: Integer; var AssignText: Boolean);
begin
  A[ACol, ARow] := (AEditor as TEdit).Text;
  KGrid3.UpdateSortMode(ACol, ARow);
  AssignText := False;
end;

procedure TForm1.KGrid3ExchangeCols(Sender: TObject; Index1, Index2: Integer);
var
  I: Integer;
  S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  // called both from sorting interface and MoveCol
  for I := 0 to KGrid3.RowCount - 1 do
  begin
    S := A[Index1, I];
    A[Index1, I] := A[Index2, I];
    A[Index2, I] := S;
  end;
end;

procedure TForm1.KGrid3ExchangeRows(Sender: TObject; Index1, Index2: Integer);
var
  I: Integer;
  S: {$IFDEF STRING_IS_UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  // called both from sorting interface and MoveRow
  for I := 0 to KGrid3.ColCount - 1 do
  begin
    S := A[I, Index1];
    A[I, Index1] := A[I, Index2];
    A[I, Index2] := S;
  end;
end;

{$IFDEF FPC}

initialization
  {$i main.lrs}
  {$i kgriddemolaz_rsrc.lrs}
{$ELSE}
  {$R *.dfm}
  {$R kgriddemo_rsrc.res}
{$ENDIF}
end.
