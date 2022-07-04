{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for ADO Sources            }
{                                                       }
{       Copyright (c) 2003 Fast Reports, Inc.           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEADO;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, ADODB, ADOInt, QBuilder;

type
  TOQBEngineADO = class(TOQBEngine)
  private
    FResultQuery: TADOQuery;
    FADOConnection : TADOConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearQuerySQL; override;
    procedure CloseResultQuery; override;
    procedure OpenResultQuery; override;
    procedure ReadFieldList(const ATableName: string); override;
    procedure ReadTableList; override;
    procedure SaveResultQueryData; override;
    procedure SetConnection(Value: TADOConnection);
    procedure SetQuerySQL(const Value: string); override;
    function ResultQuery: TDataSet; override;
    function SelectDatabase: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DatabaseName:  TADOConnection read FADOConnection write SetConnection;
  end;

resourcestring
  resSaveResFilter = 'XML files (*.xml)|*.xml';

implementation

{ TOQBEngineADO }

constructor TOQBEngineADO.Create(AOwner: TComponent);
begin
  inherited;
  FResultQuery := TADOQuery.Create(Self);
end;

destructor TOQBEngineADO.Destroy;
begin
  FResultQuery.Free;
  inherited;
end;

procedure TOQBEngineADO.SetConnection(Value: TADOConnection);
begin
  FADOConnection := Value;
  FResultQuery.Connection := Value;
end;

function TOQBEngineADO.SelectDatabase: Boolean;
begin
  Result := True;
end;

procedure TOQBEngineADO.ReadTableList;
begin
  TableList.Clear;
  FResultQuery.Connection.GetTableNames(TableList, ShowSystemTables);
end;

procedure TOQBEngineADO.ReadFieldList(const ATableName: string);
begin
  FieldList.Clear;
  FResultQuery.Connection.GetFieldNames(ATableName, FieldList);
  FieldList.Insert(0, '*');
end;

procedure TOQBEngineADO.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineADO.SetQuerySQL(const Value: string);
begin
  FResultQuery.SQL.Text := Value;
end;

function TOQBEngineADO.ResultQuery: TDataSet;
begin
  Result := FResultQuery;
end;

procedure TOQBEngineADO.OpenResultQuery;
begin
  try
    FResultQuery.Open;
  finally
  end;
end;

procedure TOQBEngineADO.CloseResultQuery;
begin
  FResultQuery.Close;
end;

{$WARNINGS OFF}
procedure TOQBEngineADO.SaveResultQueryData;
var
  DlgSaveRes: TSaveDialog;
begin
  if ResultQuery.State = dsInactive then
  begin
    ShowMessage('Data is not selected. Please, run query.');
    Exit;
  end;
  DlgSaveRes := TSaveDialog.Create(Self);
  DlgSaveRes.Filter := resSaveResFilter;
  DlgSaveRes.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
  DlgSaveRes.Title := 'Save query results data to external table';
  if DlgSaveRes.Execute then
  try
    FResultQuery.SaveToFile(DlgSaveRes.FileName, pfXML);
  finally
    DlgSaveRes.Free;
  end;
end;
{$WARNINGS ON}

procedure TOQBEngineADO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FADOConnection) and (Operation = opRemove) then
  begin
    FADOCOnnection := nil;
    FResultQuery.Connection := nil;
  end;
end;


end.

