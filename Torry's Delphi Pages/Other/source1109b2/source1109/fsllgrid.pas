{$I fsdefine.inc}

Unit fsllgrid;

Interface

Uses
  Classes,
  Controls,
  Grids,
  SysUtils, {!!.07}
  Messages;

Type
  TFSGrid = Class; { forward declaration }

  TffCellFocusEvent = Procedure(Sender: TFSGrid;
    aCol, aRow: Integer;
    Const text: String) Of Object;
  { This event is raised when a TFSGrid cell gains or loses focus. }

  TffColumnSortEvent = Procedure(Sender: TFSGrid;
    aCol: Integer) Of Object;
  { This event is raised when the user clicks on a fixed cell (header) of
    the grid. }

{ This string grid has the following extra features:
  1. Sort (one direction) on any column.
  2. OnEnterCell and OnExitCell events.
  3. Misc utility functions.
}
  TFSGrid = Class(TStringGrid)
  Protected
    FOnEnterCell: TffCellFocusEvent;
    FOnExitCell: TffCellFocusEvent;
    FOnSortColumn: TffColumnSortEvent;

    sgSavedRow: TStringList;

    Function CreateEditor: TInPlaceEdit; Override;

    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); Override;

    Function SelectCell(ACol, ARow: Longint): Boolean; Override; {!!.02}

    Procedure sgEnterCell(Const text: String; aCol, aRow: Integer); Virtual;
    { Called by custom inplace editor when the cell has gained focus.
      Raises the OnEnterCell event. }

    Procedure sgExitCell(Const text: String; aCol, aRow: Integer); Virtual;
    { Called by custom inplace editor when the cell has lost focus.
      Raises the OnExitCell event. }

    Function sgGetVersion: String; {!!.07}
    Procedure sgSetVersion(Const aValue: String); {!!.07}
  Public

    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Function AnyCellIsEmpty(Const aRow: Integer): boolean;
    { Returns True if any cell in the specified row is
      empty. }

    Procedure BeginUpdate;
    { Use this method to prevent the grid from redrawing itself
      while it is being modified. }

    Procedure BlankRow(Const aRow: Integer);
    { Blank out each cell in the specified row. }

    Procedure CopyRow(Const srcRow, destRow: Integer);
    { Copies all cells in srcRow to the corresponding cells in destRow. }

    Procedure EndUpdate;
    { After calling BeginUpdate and modifying the grid's contents,
      use this method to have the grid redraw itself. }

    Function LastRowIsEmpty: boolean;
    { Returns True if each cell of the last row is empty. }

    Procedure RestoreToRow(Const aRow: Integer);
    { If the cells of a row have been preserved using the SaveRow method,
      use this method to write the cells back to the specified row. }

    Function RowIsEmpty(Const aRow: Integer): boolean;
    { Returns True if each cell of the specified row is empty. }

    Function RowIsFilled(Const aRow: Integer): boolean;
    { Returns True if each cell of the specified row has a non-blank value. }

    Procedure SaveRow(Const aRow: Integer);
  Published

    Property Version: String {!!.07}
    Read sgGetVersion
      Write sgSetVersion
      Stored False;

    Property OnEnterCell: TffCellFocusEvent Read FOnEnterCell Write FOnEnterCell;
    { This event is raised when a TFSGrid cell gains focus. }

    Property OnExitCell: TffCellFocusEvent Read FOnExitCell Write FOnExitCell;
    { This event is raised when a TFSGrid cell loses focus. }

    Property OnSortColumn: TffColumnSortEvent Read FOnSortColumn
      Write FOnSortColumn;
    { This event is raised when the user clicks on a fixed cell (header) of
      the grid. }

  End;

  { This class is an extension of the TInPlaceEdit used by the grid.  It detects
    when the user enters and leaves a cell.  When the user leaves a cell,
    this class invokes the TFSGrid's sgExitCell method. }
  TffInPlaceEdit = Class(TInPlaceEdit)
  Protected
    FLastCol: Integer;
    FLastRow: Integer;

    Procedure WMKillFocus(Var msg: TMessage); Message WM_KILLFOCUS;

    Procedure WMSetFocus(Var msg: TMessage); Message WM_SETFOCUS;

  Public
  End;

Implementation
Uses
  fsllbase; {!!.07}

{===TffInPlaceEdit===================================================}

Procedure TffInPlaceEdit.WMKillFocus(Var msg: TMessage);
Begin
  TFSGrid(Grid).sgExitCell(Text, FLastCol, FLastRow);
  Inherited;
End;
{--------}

Procedure TffInPlaceEdit.WMSetFocus(Var msg: TMessage);
Begin
  FLastCol := TFSGrid(Grid).Col;
  FLastRow := TFSGrid(Grid).Row;
  //TFSGrid(Grid).sgEnterCell(Text, FLastCol, FLastRow);         {Deleted !!.02}
  Inherited;
End;
{====================================================================}

{===TFSGrid====================================================}

Constructor TFSGrid.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  sgSavedRow := Nil;
End;
{--------}

Destructor TFSGrid.Destroy;
Begin
  If assigned(sgSavedRow) Then
    sgSavedRow.Free;
  Inherited Destroy;
End;
{--------}

Function TFSGrid.AnyCellIsEmpty(Const aRow: Integer): boolean;
Var
  Inx: Integer;
Begin
  Result := False;
  For Inx := FixedCols To pred(ColCount) Do
    If Cells[Inx, aRow] = '' Then
      Begin
        Result := True;
        break;
      End;
End;
{--------}

Procedure TFSGrid.BeginUpdate;
Begin
  Perform(WM_SETREDRAW, 0, 0);
End;
{--------}

Procedure TFSGrid.BlankRow(Const aRow: Integer);
Var
  Inx: Integer;
Begin
  For Inx := FixedCols To pred(ColCount) Do
    Begin
      Cells[Inx, aRow] := '';
      Objects[Inx, aRow] := Nil;
    End;
End;
{--------}

Procedure TFSGrid.CopyRow(Const srcRow, destRow: Integer);
Var
  Inx: Integer;
Begin
  For Inx := FixedCols To pred(ColCount) Do
    Begin
      Cells[Inx, destRow] := Cells[Inx, srcRow];
      Objects[Inx, destRow] := Objects[Inx, srcRow];
    End;
End;
{--------}

Function TFSGrid.CreateEditor: TInplaceEdit;
Begin
  Result := TfFInPlaceEdit.Create(Self);
End;
{--------}

Procedure TFSGrid.EndUpdate;
Begin
  Perform(WM_SETREDRAW, 1, 0);
  Invalidate;
End;
{--------}

Function TFSGrid.LastRowIsEmpty: boolean;
Begin
  Result := RowIsEmpty(pred(RowCount));
End;
{--------}

Procedure TFSGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Var
  Column, Row: Longint;
Begin
  If (FixedRows > 0) And (FGridState <> gsColSizing) Then
    Begin
      MouseToCell(X, Y, Column, Row);
      If (Row = 0) And assigned(FOnSortColumn) Then
        FOnSortColumn(Self, Column);
    End;
  Inherited MouseUp(Button, Shift, X, Y);
End;
{--------}

Procedure TFSGrid.RestoreToRow(Const aRow: Integer);
Var
  Inx: Integer;
Begin
  If assigned(sgSavedRow) Then
    Begin
      For Inx := 0 To pred(sgSavedRow.Count) Do
        Begin
          Cells[FixedCols + Inx, aRow] := sgSavedRow[Inx];
          Objects[FixedCols + Inx, aRow] := sgSavedRow.Objects[Inx];
        End;
      sgSavedRow.Free;
      sgSavedRow := Nil;
    End;
End;
{--------}

Function TFSGrid.RowIsEmpty(Const aRow: Integer): boolean;
Var
  Inx: Integer;
Begin
  Result := True;
  For Inx := FixedCols To pred(ColCount) Do
    If (Cells[Inx, aRow] <> '') Then
      Begin
        Result := False;
        break;
      End;
End;
{--------}

Function TFSGrid.RowIsFilled(Const aRow: Integer): boolean;
Var
  Inx: Integer;
Begin
  Result := True;
  For Inx := FixedCols To pred(ColCount) Do
    If (Cells[Inx, aRow] = '') Then
      Begin
        Result := False;
        break;
      End;
End;
{--------}

Procedure TFSGrid.SaveRow(Const aRow: Integer);
Var
  Inx: Integer;
Begin

  If assigned(sgSavedRow) Then
    sgSavedRow.Free;

  sgSavedRow := TStringList.Create;
  For Inx := FixedCols To pred(ColCount) Do
    sgSavedRow.AddObject(Cells[Inx, aRow], Objects[Inx, ARow]);
End;
{Begin !!.02}
{--------}

Function TFSGrid.SelectCell(ACol, ARow: Longint): Boolean;
Begin
  Result := Inherited SelectCell(aCol, aRow);
  If Result Then
    sgEnterCell(Cells[aCol, aRow], aCol, aRow);
End;
{End !!.02}
{--------}

Procedure TFSGrid.sgEnterCell(Const text: String; aCol, aRow: Integer);
Begin
  If assigned(FOnEnterCell) Then
    FOnEnterCell(Self, aCol, aRow, text);
End;
{--------}

Procedure TFSGrid.sgExitCell(Const text: String; aCol, aRow: Integer);
Begin
  If assigned(FOnExitCell) Then
    FOnExitCell(Self, aCol, aRow, text);
End;
{--------}

Function TFSGrid.sgGetVersion: String; {new !!.07}
Begin
  Result := Format('%5.3f', [fsVersionNumber / 1000.0]);
End;
{--------}

Procedure TFSGrid.sgSetVersion(Const aValue: String); {new !!.07}
Begin
  {do nothing}
End;
{====================================================================}

End.

