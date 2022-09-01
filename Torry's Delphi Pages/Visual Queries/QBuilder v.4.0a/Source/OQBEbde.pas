{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for BDE Sources            }
{                                                       }
{       Copyright (c) 1996-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit OQBEbde;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, DBTables, QBuilder;

type
  TOQBEngineBDE = class(TOQBEngine)
  private
    FResultQuery: TQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDatabaseName(const Value: string); override;
    function  SelectDatabase: boolean; override;
    procedure ReadTableList; override;
    procedure ReadFieldList(ATableName: string); override;
    procedure ClearQuerySQL; override;
    procedure SetQuerySQL(Value: string); override;
    function  ResultQuery : TDataSet; override;
    procedure OpenResultQuery; override;
    procedure CloseResultQuery; override;
    procedure SaveResultQueryData; override;
  end;

resourcestring
  resSaveResFilter='Paradox files (*.db)|*.db|dBase files (*.dbf)|*.dbf|ASCII files (*.txt)|*.txt';

implementation

uses
  QBDBFrm;

{ TOQBEngineBDE }

constructor TOQBEngineBDE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResultQuery:=TQuery.Create(Self);
end;

destructor TOQBEngineBDE.Destroy;
begin
  FResultQuery.Free;
  inherited Destroy;
end;

procedure TOQBEngineBDE.SetDatabaseName(const Value: string);
begin
  inherited SetDatabaseName(Value);
  FResultQuery.DatabaseName:=Value;
end;

function TOQBEngineBDE.SelectDatabase: boolean;
var
  QBDBForm : TOQBDBForm;
  TempDatabaseName : string;
  ShowSystemTables : boolean;
begin
  QBDBForm:=TOQBDBForm.Create(Application);
  Session.GetAliasNames(QBDBForm.ComboDB.Items);
  if QBDBForm.ComboDB.Items.Count<>0 then
    QBDBForm.ComboDB.ItemIndex:=0
  else
    begin
      QBDBForm.ComboDB.Enabled:=false;
      QBDBForm.CheckDB.Enabled:=false;
    end;
  if QBDBForm.ShowModal=mrOk then
    begin
      if QBDBForm.EdtDir.Text<>EmptyStr then
        TempDatabaseName:=QBDBForm.EdtDir.Text
      else
        TempDatabaseName:=QBDBForm.ComboDB.Items[QBDBFOrm.ComboDB.ItemIndex];
      DatabaseName:=TempDatabaseName;
      ShowSystemTables:=QBDBForm.CheckDB.Checked;
      Result:=true;
    end
  else
    Result:=false;
  QBDBForm.Free;  
end;

procedure TOQBEngineBDE.ReadTableList;
var
  TempDatabase : TDatabase;
begin
  TableList.Clear;
  TempDatabase:=Session.OpenDatabase(DatabaseName);
  try
    Session.GetTableNames(DatabaseName, '', not TempDatabase.IsSQLBased,
                          ShowSystemTables, TableList);
  finally
    Session.CloseDatabase(TempDatabase);
  end;
end;

procedure TOQBEngineBDE.ReadFieldList(ATableName: string);
var
  TempTable : TTable;
  Fields : TFieldDefs;
  i : integer;
begin
  FieldList.Clear;
  TempTable:=TTable.Create(Self);
  TempTable.DatabaseName:=DatabaseName;
  TableName:=ATableName;
  TempTable.TableName:=ATableName;
  Fields:=TempTable.FieldDefs;
  try
    try
      TempTable.Active:=true;
      FieldList.Add('*');
      for i:=0 to Fields.Count-1 do
      begin
        FieldList.Add(Fields.Items[i].Name);
      end;
    except
      on E:EDBEngineError do
        begin
          ShowMessage(E.Message);
          Exit;
        end;
    end;
  finally
    TempTable.Free;
  end;
end;

procedure TOQBEngineBDE.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineBDE.SetQuerySQL(Value: string);
begin
  FResultQuery.SQL.Text:=Value;
end;

function TOQBEngineBDE.ResultQuery: TDataSet;
begin
  Result:=FResultQuery;
end;

procedure TOQBEngineBDE.OpenResultQuery;
begin
  FResultQuery.Open;
end;

procedure TOQBEngineBDE.CloseResultQuery;
begin
  FResultQuery.Close;
end;

procedure TOQBEngineBDE.SaveResultQueryData;
var
  DlgSaveRes : TSaveDialog;
  ResBatchMove : TBatchMove;
  ResTable : TTable;
begin
  if ResultQuery.State=dsInactive then
  begin
    ShowMessage('Data is not selected. Please, run query.');
    Exit;
  end;
  DlgSaveRes:=TSaveDialog.Create(Self);
  DlgSaveRes.Filter:=resSaveResFilter;
  DlgSaveRes.Options:=[ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
  DlgSaveRes.Title:='Save query results data to external table';

  if DlgSaveRes.Execute then
  try
    DlgSaveRes.DefaultExt:=EmptyStr;
    ResTable:=TTable.Create(Self);
    ResBatchMove:=TBatchMove.Create(Self);
    ResBatchMove.Destination:=ResTable;
    ResBatchMove.Source:=FResultQuery;
    ResBatchMove.Mode:=batCopy;

    if DlgSaveRes.FilterIndex=1 then
      ResTable.TableType:=ttParadox
    else
      if DlgSaveRes.FilterIndex=2 then
        ResTable.TableType:=ttDBase
      else
        ResTable.TableType:=ttASCII;

    ResTable.DatabaseName:=ExtractFilePath(DlgSaveRes.FileName);
    ResTable.TableName:=DlgSaveRes.FileName;
    ResBatchMove.Execute;
    ResTable.CreateTable;
    ResBatchMove.Mode:=batAppend;
    ResBatchMove.Execute;
  finally
    ResBatchMove.Free;
    ResTable.Free;
    DlgSaveRes.Free;
  end;
end;

end.

