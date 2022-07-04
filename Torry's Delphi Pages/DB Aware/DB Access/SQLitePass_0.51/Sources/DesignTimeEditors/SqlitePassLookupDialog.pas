{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2007 - 2010
    Last update : 02/01/2010

  --------------------------------------------------------------------------- }
unit SqlitePassLookupDialog;

{$i SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources, Variants,
 {$ELSE}
  Windows, Messages,
  {$IFDEF Delphi6}
   Variants,
  {$ENDIF}
  {$IFDEF Delphi2009}
   Variants,
  {$ENDIF}
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SqlitePassFiltersDialogTemplate, StdCtrls, CheckLst, ExtCtrls, ComCtrls,
  Buttons, SqlitePassDbo, Grids, Db;

type

  { TSqlitePassLookupDlg }

  TSqlitePassLookupDlg = class(TSqlitePassFilterDialogTemplate)
    Bevel2: TBevel;
    Label1: TLabel;
    Panel2: TPanel;
    CheckListBoxResultFields: TCheckListBox;
    Panel3: TPanel;
    Image1: TImage;
    tsLookupResults: TTabSheet;
    PanelLookupResults: TPanel;
    PanelDatasetLookup: TPanel;
    LabelLookupResultsCount: TLabel;
    LookupResultsGrid: TStringGrid;
    SbSetLookup: TSpeedButton;
    SbSelectAll: TSpeedButton;
    sbUnselectAll: TSpeedButton;
    procedure SbCancelClick(Sender: TObject);
    procedure SbOkClick(Sender: TObject);
    procedure tsCustomFiltersShow(Sender: TObject);
    procedure tsLookupResultsShow(Sender: TObject);
    procedure SbSetLookupClick(Sender: TObject);
    procedure sbUnselectAllClick(Sender: TObject);
    procedure SbSelectAllClick(Sender: TObject);
  private
    FResultFields: String;
    procedure RefreshResultFieldsList;
  public
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
    Property ResultFields: String Read FResultFields;
  end;

var
  SqlitePassLookupDlg: TSqlitePassLookupDlg;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

{ TSqlitePassLookupDlg }

constructor TSqlitePassLookupDlg.Create(AOwner: TComponent;
  Dataset: TSqlitePassDataset);
begin
  inherited Create(AOwner, Dataset, Dataset.LookupFilters);
  Caption := 'Lookup Fields Editor for ' + MyDataset.DatasetName;
  tsCustomFilters.TabVisible := False;
  tsLookupResults.TabVisible := False;
  PageControl.ActivePage := tsCustomFilters;
  RefreshResultFieldsList;
end;


procedure TSqlitePassLookupDlg.RefreshResultFieldsList;
var
i: Integer;

begin
  inherited;
  { Listbox }
  CheckListBoxResultFields.Clear;
  For i := 0 to Pred(MyDataset.Fields.count)
    do CheckListBoxResultFields.items.Add(MyDataset.Fields[i].FieldName);
end;

procedure TSqlitePassLookupDlg.SbOkClick(Sender: TObject);
var
i, j: Integer;
LookupResult: Variant;
MyResultFields: TStringList;
FilterResultFields: String;

  procedure DisplayLookupResult(LookupResult: Variant; ColIndex, RowIndex: Integer);
  var
  Str: String;
  begin
   If Not VarIsNull(LookupResult) or VarIsEmpty(LookupResult)
     then Str := LookupResult
     else Str := '';
  LookupResultsGrid.Cells[ColIndex, RowIndex] := Str;
  end;

begin
FFilterText := GetFilterText;
MyResultFields := TStringList.Create;
Try
  for i := 0 to Pred(CheckListBoxResultFields.Items.Count) do
     if CheckListBoxResultFields.Checked[i] then
        MyResultFields.Add(CheckListBoxResultFields.Items[i]);

  if (FFilterText = '') or (MyResultFields.Count = 0) then
     begin
     ShowMessage('Lookup filter is not defined or result fields are not selected');
     Exit;
     end;

  LookupResultsGrid.ColCount := MyResultFields.Count;

  FilterResultFields := MyResultFields.CommaText;
  FilterResultFields := StringReplace(FilterResultFields, ',', ';', [rfReplaceAll]);

  LookupResult := MyDataset.LookupEx(FilterText, FilterResultFields);
  LookupResultsGrid.RowCount := MyDataset.LookupFilters.Results.Count + 1;
  for i := 0 to Pred(MyResultFields.Count) do
    LookupResultsGrid.Cells[i,0] := CheckListBoxResultFields.Items[i];

  i := 1;
  While (MyDataset.LookupMoveState <> grError) do
    begin
    if VarIsArray(LookupResult)
        then begin
             for j := 0 to VarArrayHighBound(LookupResult, 1)
                 do DisplayLookupResult(LookupResult[j], j, i);
             end
        else DisplayLookupResult(LookupResult, 0, i);
    Inc(i);
    MyDataset.LookupNext(LookupResult);
    end;
  LabelLookupResultsCount.Caption := IntToStr(MyDataset.LookupFilters.Results.Count) + ' Lookup Result(s) found';
finally
 MyResultFields.Free;
end;
 PageControl.ActivePage := tsLookupResults;
end;

procedure TSqlitePassLookupDlg.SbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSqlitePassLookupDlg.tsCustomFiltersShow(Sender: TObject);
begin
  SbOk.Enabled := True;
  SbSetLookup.Enabled := False;
end;

procedure TSqlitePassLookupDlg.tsLookupResultsShow(Sender: TObject);
begin
  SbOk.Enabled := False;
  SbSetLookup.Enabled := True;
end;

procedure TSqlitePassLookupDlg.SbSetLookupClick(Sender: TObject);
begin
  PageControl.ActivePage := tsCustomFilters;
end;

procedure TSqlitePassLookupDlg.sbUnselectAllClick(Sender: TObject);
var
i: Integer;
begin
  for i := 0 to Pred(CheckListBoxResultFields.Items.Count)
    do CheckListBoxResultFields.Checked[i] := False;
end;

procedure TSqlitePassLookupDlg.SbSelectAllClick(Sender: TObject);
var
i: Integer;
begin
  for i := 0 to Pred(CheckListBoxResultFields.Items.Count)
    do CheckListBoxResultFields.Checked[i] := True;
end;

initialization

{$IFDEF FPC}
 {$I SqlitePassLookupDialog.lrs}
{$ENDIF}
end.
