{*********************************************************}
{* Dialog to copy records to another table               *}
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
 * Eivind Bakkestuen
 * Used with permission.
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit dgcpytbl;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ubase,
  uentity,
  ffdb;

type
  TdlgCopyToTable = class(TForm)
    lstTables: TListBox;
    lblImport: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbCopyBlobs: TCheckBox;
    btnNewTable: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure lstTablesDblClick(Sender: TObject);
    procedure btnNewTableClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    FDatabase : TffeDatabaseItem;
    FTableIndex: LongInt;
    FSourceDataset: TffDataset;
    FExludeTableName: String;
  end;

var
  dlgCopyToTable: TdlgCopyToTable;

function ShowCopyTableDlg(aDatabase : TffeDatabaseItem;
                          aExcludeTableIndex: LongInt;
                          aSourceDataset: TffDataset;
                      var aTableIndex: LongInt;
                      var aCopyBlobs: Boolean;
                      var aTableItem: TffeTableItem): TModalResult;     {!!.11}


implementation

{$R *.DFM}

uses
  uconfig,                                                              {!!.11}
  fmmain;   { to refresh tablelist if we create new }


function ShowCopyTableDlg(aDatabase : TffeDatabaseItem;
                          aExcludeTableIndex: LongInt;
                          aSourceDataset: TffDataset;
                      var aTableIndex: LongInt;
                      var aCopyBlobs: Boolean;
                      var aTableItem: TffeTableItem): TModalResult;     {!!.11}
var
  T: LongInt;
  TableName : String;                                                   {!!.11}
  { we must save the tablename and use it to return the
    possibly changed TableItem. Creating new tables
    changes the tablelist structure and invalidates
    the passed-in aTableItem. }
begin
  with TdlgCopyToTable.Create(nil) do
  try
    TableName := aTableItem.TableName;                                  {!!.11}
    FDatabase := aDatabase;
    FSourceDataset := aSourceDataset;
    FDatabase := aDatabase;
    lstTables.Clear;
    for T := 0 to pred(FDatabase.TableCount) do
      with FDatabase.Tables[T] do
        if T <> aExcludeTableIndex then
          lstTables.Items.AddObject(TableName, Pointer(T))
        else
          FExludeTableName := FDatabase.Tables[T].TableName;
    lstTables.ItemIndex := 0;
    Result := ShowModal;
    aTableIndex := -1;
    if Result = mrOK then begin
      aTableIndex := FTableIndex;
      aCopyBlobs := cbCopyBlobs.Checked;
    end;
    { ensure we reset aTableName; it could have
      changed in the underlying structure }
    {Begin !!.11}
    if Assigned(aTableItem) then
    for T := 0 to Pred(aDatabase.TableCount) do
      if aDatabase.Tables[T].TableName=TableName then begin
        aTableItem := aDatabase.Tables[T];
        break;
      end;
    {End !!.11}
  finally
    Free;
  end;
end;


procedure TdlgCopyToTable.lstTablesDblClick(Sender: TObject);
begin
  btnOk.Click;
end;

procedure TdlgCopyToTable.btnOKClick(Sender: TObject);
begin
  with lstTables do
    FTableIndex := LongInt(Items.Objects[ItemIndex]);
end;

procedure TdlgCopyToTable.btnNewTableClick(Sender: TObject);
var
  T : Integer;
  NewTableName : String;
begin
  if InputQuery('New Table', 'Tablename:', NewTableName) then begin
    FDatabase.CreateTable(NewTableName, FSourceDataset.Dictionary);
    { refresh mainwindow treeview }
    frmMain.outServers.Selected := frmMain.GetEntityNode(etDatabase, FDatabase);
    frmMain.RefreshTables(Self);
    { refresh listbox }
    lstTables.Clear;
    for T := 0 to pred(FDatabase.TableCount) do
      with FDatabase.Tables[T] do
        if TableName<>FExludeTableName then
          lstTables.Items.AddObject(TableName, Pointer(T));
    lstTables.ItemIndex := lstTables.Items.IndexOf(NewTableName);
    btnOk.SetFocus;
  end;
end;

{Begin !!.11}
procedure TdlgCopyToTable.FormShow(Sender: TObject);
var
  BaseSection : string;
begin
  BaseSection := ClassName + '.' + Self.Caption;
  cbCopyBlobs.Checked := FFEConfigGetBoolean(BaseSection, 'Copy BLOBs', True);
end;

procedure TdlgCopyToTable.FormClose(Sender: TObject; var Action: TCloseAction);
var
  BaseSection : string;
begin
  BaseSection := ClassName + '.' + Self.Caption;
  FFEConfigSaveBoolean(BaseSection, 'Copy BLOBs', cbCopyBlobs.Checked);
end;
{End !!.11}


end.
