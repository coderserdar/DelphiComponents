{*********************************************************}
{* Dialog to import structure from another table         *}
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

unit dgimpdef;

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
  uentity;

type
  TdlgImportDefinition = class(TForm)
    lstTables: TListBox;
    lblImport: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    cbDatabases: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure lstTablesDblClick(Sender: TObject);
    procedure cbDatabasesChange(Sender: TObject);
  private
  public
    TableInDatabase,
    CurrentDatabase : TffeDatabaseItem;
    ExcludeTableIndex,
    FTableIndex: LongInt;
  end;

var
  dlgImportDefinition: TdlgImportDefinition;

function ShowImportTableDefDlg(aDatabase : TffeDatabaseItem;
                               aExcludeTableIndex: LongInt;
                           var aImportFromDatabase: TffeDatabaseItem;
                           var aTableIndex: LongInt): TModalResult;

implementation

{$R *.DFM}

function ShowImportTableDefDlg(aDatabase : TffeDatabaseItem;
                               aExcludeTableIndex: LongInt;
                           var aImportFromDatabase: TffeDatabaseItem;
                           var aTableIndex: LongInt): TModalResult;
var
  CurrentIdx,
  i: Integer;
begin
  with TdlgImportDefinition.Create(nil) do
  try
    TableInDatabase := aDatabase;
    CurrentDatabase := aDatabase;
    ExcludeTableIndex := aExcludeTableIndex;
    { load databaselist }
    CurrentIdx := -1;
    cbDatabases.Clear;
    for i := 0 to CurrentDatabase.Server.DatabaseCount-1 do begin
      cbDatabases.Items.AddObject(CurrentDatabase.Server.Databases[i].DatabaseName,
                          CurrentDatabase.Server.Databases[i]);
      if CurrentDatabase.Server.Databases[i]=CurrentDatabase then
        CurrentIdx := i;
    end;
    cbDatabases.ItemIndex := CurrentIdx;
    cbDatabasesChange(NIL);
    Result := ShowModal;
    aTableIndex := -1;
    if Result = mrOK then begin
      aTableIndex := FTableIndex;
      aImportFromDatabase := CurrentDatabase;
    end;
  finally
    Free;
  end;
end;

procedure TdlgImportDefinition.lstTablesDblClick(Sender: TObject);
begin
  btnOk.Click;
end;

procedure TdlgImportDefinition.btnOKClick(Sender: TObject);
begin
  with lstTables do
    FTableIndex := LongInt(Items.Objects[ItemIndex]);
end;

procedure TdlgImportDefinition.cbDatabasesChange(Sender: TObject);
var
  T: LongInt;
begin
  lstTables.Clear;
  CurrentDatabase := TffeDatabaseItem(cbDatabases.Items.Objects[cbDatabases.ItemIndex]);
  { make sure tablelist exists }
  if CurrentDatabase.TableCount=0 then
    CurrentDatabase.LoadTables;
  for T := 0 to pred(CurrentDatabase.TableCount) do
    with CurrentDatabase.Tables[T] do
      if (CurrentDatabase<>TableInDatabase) or (T <> ExcludeTableIndex) then
        lstTables.Items.AddObject(TableName, Pointer(T));
  if lstTables.Items.Count>0 then
    lstTables.ItemIndex := 0;    
end;

end.
