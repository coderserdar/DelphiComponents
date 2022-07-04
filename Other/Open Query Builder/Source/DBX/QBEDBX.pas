{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for DBX Sources            }
{                                                       }
{       Copyright (c) 2003 Fast Reports, Inc.           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEDBX;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, DBXpress, DBClient, Provider, SqlExpr, QBuilder;

type
  TOQBEngineDBX = class(TOQBEngine)
  private
    FResultQuery: TSQLQuery;
    FDBXConnection : TSQLConnection;
    FClientDataSet : TClientDataSet;
    FProvider : TDataSetProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearQuerySQL; override;
    procedure CloseResultQuery; override;
    procedure OpenResultQuery; override;
    procedure ReadFieldList(const ATableName: string); override;
    procedure ReadTableList; override;
    procedure SaveResultQueryData; override;
    procedure SetConnection(Value: TSQLConnection);
    procedure SetQuerySQL(const Value: string); override;
    function ResultQuery: TDataSet; override;
    function SelectDatabase: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DatabaseName:  TSQLConnection read FDBXConnection write SetConnection;
  end;

implementation

{ TOQBEngineDBX }

constructor TOQBEngineDBX.Create(AOwner: TComponent);
begin
  inherited;
  FResultQuery := TSQLQuery.Create(Self);
  FClientDataSet := TClientDataSet.Create(Self);
  FProvider := TDataSetProvider.Create(Self);
end;

destructor TOQBEngineDBX.Destroy;
begin
  FResultQuery.Free;
  FClientDataSet.Free;
  FProvider.Free;
  inherited;
end;

procedure TOQBEngineDBX.SetConnection(Value: TSQLConnection);
begin
  FDBXConnection := Value;
  FResultQuery.SQLConnection := Value;
end;

function TOQBEngineDBX.SelectDatabase: Boolean;
begin
  Result := True;
end;

procedure TOQBEngineDBX.ReadTableList;
begin
  TableList.Clear;
  FResultQuery.SQLConnection.GetTableNames(TableList, ShowSystemTables);
end;

procedure TOQBEngineDBX.ReadFieldList(const ATableName: string);
begin
  FieldList.Clear;
  FResultQuery.SQLConnection.GetFieldNames(ATableName, FieldList);
  FieldList.Insert(0, '*');
end;

procedure TOQBEngineDBX.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineDBX.SetQuerySQL(const Value: string);
begin
  FResultQuery.SQL.Text := Value;
end;

function TOQBEngineDBX.ResultQuery: TDataSet;
begin
  Result := FClientDataSet;
end;

procedure TOQBEngineDBX.OpenResultQuery;
begin
  FResultQuery.Open;
  FProvider.DataSet := FResultQuery;
  FClientDataSet.SetProvider(FProvider);
  FClientDataSet.Open;
end;

procedure TOQBEngineDBX.CloseResultQuery;
begin
  FClientDataSet.Close;
  FResultQuery.Close;
end;

{$WARNINGS OFF}
procedure TOQBEngineDBX.SaveResultQueryData;
begin
//
end;
{$WARNINGS ON}

procedure TOQBEngineDBX.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDBXConnection) and (Operation = opRemove) then
  begin
    FDBXCOnnection := nil;
    FResultQuery.SQLConnection := nil;
  end;
end;


end.

