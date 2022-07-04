{   This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ---------------------------------------------------------------------------

    SqlitePass Database Objects Package
    Registration unit
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006-2008

  ---------------------------------------------------------------------------

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 30.04.2009

  --------------------------------------------------------------------------- }


unit RegisterSqlitePassDbo;
{$i ..\..\Sources\SqlitePassDbo.inc}

{$IFNDEF FPC}
{$R ../Dx_SqlitePassDbo_Designtime.dcr}
{$ENDIF}
interface

uses

{$IFDEF FPC}
 LResources, componenteditors, PropEdits, LazarusPackageIntf, Buttons,
{$ELSE}
  {$IFDEF Delphi2009}DesignIntf, DesignEditors, {$ENDIF}
  {$IFDEF Delphi6}   DesignIntf, DesignEditors, {$ENDIF}
  {$IFDEF Delphi4}   DsgnIntf,                  {$ENDIF}
  StdCtrls,
  Db,
  FileCtrl,
{$ENDIF}
 Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Dialogs, ActnList,
 SqlitePassDbo, SqlitePassDesignErrorLang,
 SqlitePassSortByDialog, SqlitePassChooseDatasetDialog, SqlitePassIndexesDialog,
 SqlitePassMasterDetailFieldsDialog, SqlitePassFieldDefsDialog, SqlitePassDataTypesDialog,
 SqlitePassCustomFieldDefsDialog, SqlitePassFiltersDialog, SqlitePassIndexedByDialog,
 SqlitePassCreateNewDatabaseDialog, SqlitePassErrorLogDialog, SqlitePassVisualTools;

Type

{ Database Editors }

TDatabaseFileNameEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatabaseSQLiteLibraryEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatabaseDatatypesEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatabaseCustomFieldDefsEditor = class(TStringProperty)
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  end;

TDatabaseComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ DatabaseOptions Editors }

TDatabaseOptionsTempStorageDirEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

{ Dataset Editors }

TDatasetNameEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatasetSortedByEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatasetIndexedByEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatasetFilterEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatasetIndexesEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

TDatasetMasterDetailFieldsEditor = class(TStringProperty)
  Public
    Procedure Edit;override;
    Function  GetAttributes: TPropertyAttributes; override;
  End;

var
LastDatabaseDir: String;

procedure Register;
function CheckDatabaseConnected(Database: TSqlitePassDatabase; Msg: String = ''): Boolean;

implementation
procedure Register;
begin
 { TSqlitePassDatabase }
 RegisterComponents('SqlitePassDbo', [TSqlitePassDatabase]);
 RegisterComponentEditor(TSqlitePassDatabase, TDatabaseComponentEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDatabase,'Database',TDatabaseFileNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDatabase,'SQLiteLibrary',TDatabaseSQLiteLibraryEditor);
 RegisterPropertyEditor(TypeInfo(TSqlitePassFieldTypesTranslationRules),TSqlitePassDatabaseDataTypeOptions,'TranslationRules',TDatabaseDatatypesEditor);
 RegisterPropertyEditor(TypeInfo(TSqlitePassCustomFieldDefs),TSqlitePassDatabaseDataTypeOptions,'CustomFieldDefs',TDatabaseCustomFieldDefsEditor);

 { TSqlitePassDatabaseOptions }
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDatabaseOptions,'TemporaryStorageDir',TDatabaseOptionsTempStorageDirEditor);

 { TSqlitePassDataset }
 RegisterComponents('SqlitePassDbo', [TSqlitePassDataset]);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDataset,'DatasetName',TDatasetNameEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDataset,'MasterFields',TDatasetMasterDetailFieldsEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDataset,'SortedBy',TDatasetSortedByEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDataset,'IndexedBy',TDatasetIndexedByEditor);
 RegisterPropertyEditor(TypeInfo(TSqlitePassDatasetIndexDefs),TSqlitePassDataset,'IndexDefs',TDatasetIndexesEditor);
 RegisterPropertyEditor(TypeInfo(String),TSqlitePassDataset,'Filter',TDatasetFilterEditor);

 { Visual Tools }
 RegisterComponents('SqlitePassDbo', [TSqlitePassDBActionList]);
 RegisterActions('TSqlitePassDataset',
                  [TSqlitePassDatasetSort, TSqlitePassDatasetSortAsc, TSqlitePassDatasetSortDesc,
                   TSqlitePassDatasetFilterOnSelection, TSqlitePassDatasetFilter, TSqlitePassDatasetFilterOnOff,
                   TSqlitePassDatasetLocate, TSqlitePassDatasetLocateFirst, TSqlitePassDatasetLocatePrior, TSqlitePassDatasetLocateNext, TSqlitePassDatasetLocateLast],
                   nil);
end;


{ TDatasetNameEditor }

Function TDatasetNameEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;

procedure TDatasetNameEditor.Edit;
var
F: TSqlitePassChooseDatasetDialog;
Dataset: TSqlitePassDataset;
IsOpen: Boolean;
begin
Dataset := TSqlitePassDataset(GetComponent(0));
if Assigned(Dataset.Database) then
   begin
   if CheckDatabaseconnected(Dataset.Database, Msg1027) then
      begin
      F := TSqlitePassChooseDatasetDialog.Create(Nil, Dataset.Database);
      F.DBTreeView.VisibleItems :=  [itTable, itQuery, itView, itTableSystem,
                                     itSql, itSqlSelect, itSqlCreate, itSqlCreateTable, itSqlUpdate,
                                     itSqlInsert, itSqlDelete, itSqlOthers];
      if (F.ShowModal = mrOk)
         then begin
              IsOpen := Dataset.Active;
              Dataset.Close;
              SetStrValue(F.DBTreeView.SelectedItem.Caption);
              If IsOpen then Dataset.Active := True;
              end;
      F.Free;
      end;
   end
   else MessageDlg(Msg1010, mtInformation, [mbOk], 0);
End;

{ TDatasetSortedByEditor }

procedure TDatasetSortedByEditor.Edit;
var
Dataset: TSqlitePassDataset;
F: TSqlitePassSortedByDialog;
begin
Dataset:=TSqlitePassDataset(GetComponent(0));
if Dataset.Active then
   begin
   F := TSqlitePassSortedByDialog.Create(Nil, Dataset.SortedBy, Dataset.Fields);
   if (F.ShowModal = mrOk)
      then Dataset.SortedBy := F.SQL;
   F.Free;
   end
   else MessageDlg('SortedBy : ' + Msg1015, mtInformation, [mbOk], 0);
end;

function TDatasetSortedByEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;

{ TDatasetIndexedByEditor }

procedure TDatasetIndexedByEditor.Edit;
var
Dataset: TSqlitePassDataset;
F: TSqlitePassIndexedByDialog;
begin
Dataset:=TSqlitePassDataset(GetComponent(0));
if Dataset.Active then
   begin
   F := TSqlitePassIndexedByDialog.Create(Nil, Dataset);
   if (F.ShowModal = mrOk)
      then Dataset.IndexedBy := F.SQL;
   F.Free;
   end
   else MessageDlg('IndexedBy : ' + Msg1015, mtInformation, [mbOk], 0);
end;

function TDatasetIndexedByEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;

{ TDatasetFilterEditor }

procedure TDatasetFilterEditor.Edit;
var
Dataset: TSqlitePassDataset;
F: TSqlitePassFilterDialog;
begin
Dataset:=TSqlitePassDataset(GetComponent(0));
if Dataset.Active then
   begin
   F := TSqlitePassFilterDialog.Create(Nil, Dataset);
   F.ShowModal;
   F.Free;
   end
   else MessageDlg('Filter : ' + Msg1015, mtInformation, [mbOk], 0);
end;

function TDatasetFilterEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;



{ TDatasetIndexesEditor }

procedure TDatasetIndexesEditor.Edit;
var
Dataset: TSqlitePassDataset;
F: TSqlitePassIndexDefsDialog;
begin
Dataset:=TSqlitePassDataset(GetComponent(0));
F := TSqlitePassIndexDefsDialog.Create(Nil);
F.Dataset := Dataset;
F.ShowModal;
F.Free;
end;

function TDatasetIndexesEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;


{ TDatasetMasterDetailFieldsEditor }

procedure TDatasetMasterDetailFieldsEditor.Edit;
var
Dataset: TSqlitePassDataset;
F: TSqlitePassMasterDetailDialog;
begin
Dataset:=TSqlitePassDataset(GetComponent(0));
if Not Assigned(Dataset.MasterSource) then
   begin
   MessageDlg(Msg1000, mtInformation, [mbOk], 0);
   Exit;
   end;

F := TSqlitePassMasterDetailDialog.Create(Nil, Dataset);
F.ShowModal;
F.Free;
end;

function TDatasetMasterDetailFieldsEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog];
end;


{ ----------  TSQLiteDatabase Editors ---------- }

function CheckDatabaseConnected(Database: TSqlitePassDatabase; Msg: String = ''): Boolean;
begin
Result := Assigned(Database) and Database.Connected;
If Not Result
   then MessageDlg(Msg1020 + CRLF + Msg, mtInformation, [mbOk], 0);
end;


{ TDatabaseFileNameEditor }

procedure TDatabaseFileNameEditor.Edit;
var
D: TOpenDialog;
begin
D := TOpenDialog.Create(Nil);
D.InitialDir := LastDatabaseDir;
D.Filter :=  'Kexi Database files (*.kexi)|*.kexi|'
           + 'Sqlite Administrator Database files (*.s3db)|*.s3db|'
           + 'Sqlite Expert Database files (*.db3)|*.db3|'
           + 'Sqlite4Fpc  Database files (*.s3fpc)|*.s3fpc|'
           + 'SqliteToolbox Database files (*.sp3)|*.sp3|'
           + 'All files (*.*)|*.*';
if D.Execute then
   begin
   SetStrValue(D.FileName);
   LastDatabaseDir := ExtractFilePath(D.FileName);
   end;
D.Free;
end;

function TDatabaseFileNameEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;

{ TDatabaseOptionsTempStorageDirEditor }
procedure TDatabaseOptionsTempStorageDirEditor.Edit;
var
ChosenDir: String;
begin
 if SelectDirectory('Select a Temporary Storage Directory', 'C:\', chosenDir)
    then SetStrValue(ChosenDir);
end;

function TDatabaseOptionsTempStorageDirEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;

{ TDatabaseSQLiteLibraryEditor }

procedure TDatabaseSQLiteLibraryEditor.Edit;
var
D: TOpenDialog;
begin
D := TOpenDialog.Create(Nil);
{$IFDEF LINUX}
D.Filter := 'SQLite library files (*.so)|*.so|All files (*.*)|*.*';
{$ELSE}
D.Filter := 'SQLite windows library files (*.dll)|*.dll|All files (*.*)|*.*';
{$ENDIF}

if D.Execute
   then SetStrValue(D.FileName);

D.Free;
end;

function TDatabaseSQLiteLibraryEditor.GetAttributes: TPropertyAttributes;
begin
   Result:= [paDialog];
end;


{ TDatabaseDataTypesEditor }

procedure TDatabaseDataTypesEditor.Edit;
var
Database: TSqlitePassDatabase;
F: TSqlitePassDataTypesDlg;
begin
Database:= TSqlitePassDatabaseDataTypeOptions(GetComponent(0)).Database;
If CheckDatabaseconnected(Database, Msg1025) then
   begin
   F := TSqlitePassDataTypesDlg.Create(Database);
   F.ShowModal;
   F.Free;
   end;
end;

function TDatabaseDataTypesEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog];
end;

{ TDatabaseCustomFieldDefsEditor }

procedure TDatabaseCustomFieldDefsEditor.Edit;
var
Database: TSqlitePassDatabase;
F: TSqlitePassCustomFieldDefsDlg;
begin
Database:= TSqlitePassDatabaseDataTypeOptions(GetComponent(0)).Database;
if CheckDatabaseconnected(Database) then
   begin
   F := TSqlitePassCustomFieldDefsDlg.Create(Database);
   F.ShowModal;
   F.Free;
   end;
end;

function TDatabaseCustomFieldDefsEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog];
end;


{ TDatabaseComponentEditor }

procedure TDatabaseComponentEditor.ExecuteVerb(Index: Integer);
var
Database: TSqlitePassDatabase;
F: TSqlitePassCreateNewDatabaseDlg;
E: TSqlitePassErrorLogDlg;
begin
Database:=TSqlitePassDatabase(Component);
Case Index of
    0: Database.Connected := Not Database.Connected;
    1: begin
       F := TSqlitePassCreateNewDatabaseDlg.Create(Nil);
       F.ShowModal;
       F.Free;
       end;
    3: Database.DatatypeOptions.LoadFromDatabase(Database.DatatypeOptions.LoadOptions);
    5: Database.DatatypeOptions.SaveToDatabase(Database.DatatypeOptions.SaveOptions);
    6: Database.DatatypeOptions.SetDefaultPropertiesValues;
    8: begin
       E := TSqlitePassErrorLogDlg.Create(Nil, Database);
       E.ShowModal;
       E.Free;
       end;
    end;
end;

function TDatabaseComponentEditor.GetVerb(Index: Integer): string;
var
Database: TSqlitePassDatabase;
begin
Database:=TSqlitePassDatabase(Component);
  Case Index of
   0: begin
      if Database.Connected
         then Result := '&Close the database'
         else Result := '&Open the database';
      end;
   1: Result := '&Create a new database';
   2: Result := '-';
   3: Result := '&Load DataTypesOptions from database';
   4: Result := '&Save DataTypesOptions to database';
   5: Result := '-';
   6: Result := 'Load &Default translation rules';
   7: Result := '-';
   8: Result := 'Show Log &Errors Dialog';
   end;
end;

function TDatabaseComponentEditor.GetVerbCount: Integer;
begin
  Result := 9;
end;

initialization
{$IFDEF FPC}
 {$i SqlitePassDbo.lrs}
{$ENDIF}
end.


