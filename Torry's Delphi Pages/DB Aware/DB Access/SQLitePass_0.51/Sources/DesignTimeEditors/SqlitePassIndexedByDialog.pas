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
    2007-2010
    Last update : 26-01-2010

  --------------------------------------------------------------------------- }

unit SqlitePassIndexedByDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ELSE}
  Windows,
  Messages,
  Math,
  Db,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, SqlitePassConst, SqlitePassDbo,
  CheckLst;

type

  { TSqlitePassSortedByDialog }

  TSqlitePassIndexedByDialog = class(TForm)
    PanelBottom: TPanel;
    BtOk: TButton;
    BtCancel: TButton;
    BtnReset: TButton;
    Panel1: TPanel;
    Label2: TLabel;
    LbIndexedFields: TCheckListBox;
    sbSelectAll: TSpeedButton;
    sbUnselectAll: TSpeedButton;
    procedure BtOkClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure sbUnselectAllClick(Sender: TObject);
    procedure sbSelectAllClick(Sender: TObject);
  private
    FDataset: TSqlitePassDataset;
    Procedure DisplayFieldsIndexes;
  public
    OriginalSQL: String;
    SQL: String;
    constructor Create(AOwner: TComponent; Dataset: TSqlitePassDataset); reintroduce;
    destructor Destroy; override;
  end;

var
  SqlitePassIndexedByDlg: TSqlitePassIndexedByDialog;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ Constructor and Destructor }

constructor TSqlitePassIndexedByDialog.Create(AOwner: TComponent; Dataset: TSqlitePassDataset);
begin
inherited Create(AOwner);
FDataset := Dataset;
OriginalSQL := FDataset.IndexedBy;
BtnResetClick(Self);
end;

destructor TSqlitePassIndexedByDialog.Destroy;
begin
inherited Destroy;
FDataset := Nil;
end;

{ Create the SQL result string }

procedure TSqlitePassIndexedByDialog.BtOkClick(Sender: TObject);
var
i: Integer;
begin
SQL := '';
for i := 0 to Pred(LbIndexedFields.Items.Count) do
    begin
    if LbIndexedFields.Checked[i]
       then SQL := SQL + LbIndexedFields.Items[i] + '; ';
    end;
SQL := Trim(SQL);    
end;

procedure TSqlitePassIndexedByDialog.BtnResetClick(Sender: TObject);
var
i: Integer;
begin
LbIndexedFields.Items.Clear;
for i := 0 to Pred(FDataset.Fields.count)
    do LbIndexedFields.Items.Add(FDataset.Fields[i].FieldName);
if FDataset.Fields.Count > 0
   then LbIndexedFields.ItemIndex := 0;
DisplayFieldsIndexes;
end;

procedure TSqlitePassIndexedByDialog.DisplayFieldsIndexes;
var
i: Integer;
TempIndex: TSqlitePassInMemoryIndex;
begin
Try
for i := 0 to Pred(LbIndexedFields.Items.Count) do
    begin
    TempIndex := FDataset.Indexes.IndexByFieldName(LbIndexedFields.Items[i]);
    if Assigned(TempIndex)
       then LbIndexedFields.Checked[i] := TempIndex.Indexed;
    end;
Except
 On E: Exception do;
end;
end;


procedure TSqlitePassIndexedByDialog.sbUnselectAllClick(Sender: TObject);
var
i: Integer;
begin
for i := 0 to Pred(lbIndexedFields.Items.Count)
    do lbIndexedFields.Checked[i] := False;
end;

procedure TSqlitePassIndexedByDialog.sbSelectAllClick(Sender: TObject);
var
i: Integer;
begin
for i := 0 to Pred(lbIndexedFields.Items.Count)
    do lbIndexedFields.Checked[i] := True;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassIndexedByDialog.lrs}
 {$ENDIF}
end.
