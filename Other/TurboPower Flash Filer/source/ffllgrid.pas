{*********************************************************}
{* Custom string grid for server config forms            *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllgrid;

interface

uses
  Classes,
  Controls,
  Grids,
  SysUtils,                                                           {!!.07}
  Messages;

type
  TffStringGrid = class;    { forward declaration }

  TffCellFocusEvent = procedure(Sender : TffStringGrid;
                                aCol, aRow : integer;
                                const text : string) of object;
    { This event is raised when a TffStringGrid cell gains or loses focus. }

  TffColumnSortEvent = procedure(Sender : TffStringGrid;
                                 aCol : integer) of object;
    { This event is raised when the user clicks on a fixed cell (header) of
      the grid. }

  { This string grid has the following extra features:
    1. Sort (one direction) on any column.
    2. OnEnterCell and OnExitCell events.
    3. Misc utility functions.
  }
  TffStringGrid = class(TStringGrid)
  protected
    FOnEnterCell : TffCellFocusEvent;
    FOnExitCell : TffCellFocusEvent;
    FOnSortColumn : TffColumnSortEvent;

    sgSavedRow : TStringList;

    function CreateEditor : TInPlaceEdit; override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer); override;

    function SelectCell(ACol, ARow: Longint): Boolean; override;       {!!.02}

    procedure sgEnterCell(const text : string; aCol, aRow : integer); virtual;
      { Called by custom inplace editor when the cell has gained focus.
        Raises the OnEnterCell event. }

    procedure sgExitCell(const text : string; aCol, aRow : integer); virtual;
      { Called by custom inplace editor when the cell has lost focus.
        Raises the OnExitCell event. }

    function sgGetVersion : string;                                   {!!.07}
    procedure sgSetVersion(const aValue : string);                    {!!.07}
  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    function AnyCellIsEmpty(const aRow : integer) : boolean;
      { Returns True if any cell in the specified row is
        empty. }

    procedure BeginUpdate;
      { Use this method to prevent the grid from redrawing itself
        while it is being modified. }

    procedure BlankRow(const aRow : integer);
      { Blank out each cell in the specified row. }

    procedure CopyRow(const srcRow, destRow : integer);
      { Copies all cells in srcRow to the corresponding cells in destRow. }

    procedure EndUpdate;
      { After calling BeginUpdate and modifying the grid's contents,
        use this method to have the grid redraw itself. }

    function LastRowIsEmpty : boolean;
      { Returns True if each cell of the last row is empty. }

    procedure RestoreToRow(const aRow : integer);
      { If the cells of a row have been preserved using the SaveRow method,
        use this method to write the cells back to the specified row. }

    function RowIsEmpty(const aRow : integer) : boolean;
      { Returns True if each cell of the specified row is empty. }

    function RowIsFilled(const aRow : integer) : boolean;
      { Returns True if each cell of the specified row has a non-blank value. }

    procedure SaveRow(const aRow : integer);
  published

    property Version : string                                         {!!.07}
       read sgGetVersion
       write sgSetVersion
       stored False;

    property OnEnterCell: TffCellFocusEvent read FOnEnterCell write FOnEnterCell;
      { This event is raised when a TffStringGrid cell gains focus. }

    property OnExitCell: TffCellFocusEvent read FOnExitCell write FOnExitCell;
      { This event is raised when a TffStringGrid cell loses focus. }

    property OnSortColumn : TffColumnSortEvent read FOnSortColumn
                                               write FOnSortColumn;
    { This event is raised when the user clicks on a fixed cell (header) of
      the grid. }

  end;

  { This class is an extension of the TInPlaceEdit used by the grid.  It detects
    when the user enters and leaves a cell.  When the user leaves a cell,
    this class invokes the TffStringGrid's sgExitCell method. }
  TffInPlaceEdit = class(TInPlaceEdit)
  protected
    FLastCol : integer;
    FLastRow : integer;

    procedure WMKillFocus(var msg : TMessage); message WM_KILLFOCUS;

    procedure WMSetFocus(var msg : TMessage); message WM_SETFOCUS;

  public
  end;

implementation
uses
  ffllbase;                                                           {!!.07}

{===TffInPlaceEdit===================================================}
procedure TffInPlaceEdit.WMKillFocus(var msg : TMessage);
begin
  TffStringGrid(Grid).sgExitCell(Text, FLastCol, FLastRow);
  inherited;
end;
{--------}
procedure TffInPlaceEdit.WMSetFocus(var msg : TMessage);
begin
  FLastCol := TffStringGrid(Grid).Col;
  FLastRow := TffStringGrid(Grid).Row;
  //TffStringGrid(Grid).sgEnterCell(Text, FLastCol, FLastRow);         {Deleted !!.02}
  inherited;
end;
{====================================================================}

{===TffStringGrid====================================================}
constructor TffStringGrid.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  sgSavedRow := nil;
end;
{--------}
destructor TffStringGrid.Destroy;
begin
  if assigned(sgSavedRow) then
    sgSavedRow.Free;
  inherited Destroy;
end;
{--------}
function TffStringGrid.AnyCellIsEmpty(const aRow : integer) : boolean;
var
  Inx : integer;
begin
  Result := False;
  for Inx := FixedCols to pred(ColCount) do
    if Cells[Inx, aRow] = '' then begin
      Result := True;
      break;
    end;
end;
{--------}
procedure TffStringGrid.BeginUpdate;
begin
  Perform(WM_SETREDRAW, 0, 0);
end;
{--------}
procedure TffStringGrid.BlankRow(const aRow : integer);
var
  Inx : integer;
begin
  for Inx := FixedCols to pred(ColCount) do begin
    Cells[Inx, aRow] := '';
    Objects[Inx, aRow] := nil;
  end;
end;
{--------}
procedure TffStringGrid.CopyRow(const srcRow, destRow : integer);
var
  Inx : integer;
begin
  for Inx := FixedCols to pred(ColCount) do begin
    Cells[Inx, destRow] := Cells[Inx, srcRow];
    Objects[Inx, destRow] := Objects[Inx, srcRow];
  end;
end;
{--------}
function TffStringGrid.CreateEditor : TInplaceEdit;
begin
  Result := TfFInPlaceEdit.Create(self);
end;
{--------}
procedure TffStringGrid.EndUpdate;
begin
  Perform(WM_SETREDRAW, 1, 0);
  Invalidate;
end;
{--------}
function TffStringGrid.LastRowIsEmpty : boolean;
begin
  Result := RowIsEmpty(pred(RowCount));
end;
{--------}
procedure TffStringGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer);
var
  Column, Row: Longint;
begin
  if (FixedRows > 0) and (FGridState <> gsColSizing) then begin
    MouseToCell(X, Y, Column, Row);
    if (Row = 0) and assigned(FOnSortColumn) then
      FOnSortColumn(Self, Column);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;
{--------}
procedure TffStringGrid.RestoreToRow(const aRow : integer);
var
  Inx : integer;
begin
  if assigned(sgSavedRow) then begin
    for Inx := 0 to pred(sgSavedRow.Count) do begin
      Cells[FixedCols + Inx, aRow] := sgSavedRow[Inx];
      Objects[FixedCols + Inx, aRow] := sgSavedRow.Objects[Inx];
    end;
    sgSavedRow.Free;
    sgSavedRow := nil;
  end;
end;
{--------}
function TffStringGrid.RowIsEmpty(const aRow : integer) : boolean;
var
  Inx : integer;
begin
  Result := True;
  for Inx := FixedCols to pred(ColCount) do
    if (Cells[Inx, aRow] <> '') then begin
      Result := False;
      break;
    end;
end;
{--------}
function TffStringGrid.RowIsFilled(const aRow : integer) : boolean;
var
  Inx : integer;
begin
  Result := True;
  for Inx := FixedCols to pred(ColCount) do
    if (Cells[Inx, aRow] = '') then begin
      Result := False;
      break;
    end;
end;
{--------}
procedure TffStringGrid.SaveRow(const aRow : integer);
var
  Inx : integer;
begin

  if assigned(sgSavedRow) then
    sgSavedRow.Free;

  sgSavedRow := TStringList.Create;
  for Inx := FixedCols to pred(ColCount) do
    sgSavedRow.AddObject(Cells[Inx, aRow], Objects[Inx, ARow]);
end;
{Begin !!.02}
{--------}
function TffStringGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited SelectCell(aCol, aRow);
  if Result then
    sgEnterCell(Cells[aCol, aRow], aCol, aRow);
end;
{End !!.02}
{--------}
procedure TffStringGrid.sgEnterCell(const text : string; aCol, aRow : integer);
begin
  if assigned(FOnEnterCell) then
    FOnEnterCell(self, aCol, aRow, text);
end;
{--------}
procedure TffStringGrid.sgExitCell(const text : string; aCol, aRow : integer);
begin
  if assigned(FOnExitCell) then
    FOnExitCell(self, aCol, aRow, text);
end;
{--------}
function TffStringGrid.sgGetVersion : string;                    {new !!.07}
begin
  Result := Format('%5.4f', [ffVersionNumber / 10000.0]);
end;
{--------}
procedure TffStringGrid.sgSetVersion(const aValue : string);     {new !!.07}
begin
  {do nothing}
end;
{====================================================================}

end.
