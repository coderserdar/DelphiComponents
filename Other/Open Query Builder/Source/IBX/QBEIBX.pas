{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for IBX Sources            }
{                                                       }
{       Copyright (c) 2003 Fast Reports, Inc.           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEIBX;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, IBDatabase, IBQuery, QBuilder;

type
  TOQBEngineIBX = class(TOQBEngine)
  private
    FResultQuery: TIBQuery;
    FIBXConnection : TIBDatabase;
    FTransaction : TIBTransaction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearQuerySQL; override;
    procedure CloseResultQuery; override;
    procedure OpenResultQuery; override;
    procedure ReadFieldList(const ATableName: string); override;
    procedure ReadTableList; override;
    procedure SaveResultQueryData; override;
    procedure SetConnection(Value: TIBDatabase);
    procedure SetQuerySQL(const Value: string); override;
    function ResultQuery: TDataSet; override;
    function SelectDatabase: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property DatabaseName:  TIBDatabase read FIBXConnection write SetConnection;
  end;

implementation

{ TOQBEngineIBX }

constructor TOQBEngineIBX.Create(AOwner: TComponent);
begin
  inherited;
  FResultQuery := TIBQuery.Create(Self);
  FTransaction := TIBTransaction.Create(Self);
  FResultQuery.Transaction := FTransaction;
end;

destructor TOQBEngineIBX.Destroy;
begin
  FResultQuery.Free;
  FTransaction.Free;
  inherited;
end;

procedure TOQBEngineIBX.SetConnection(Value: TIBDatabase);
begin
  FIBXConnection := Value;
  FResultQuery.Database := Value;
end;

function TOQBEngineIBX.SelectDatabase: Boolean;
begin
  Result := True;
end;

procedure TOQBEngineIBX.ReadTableList;
begin
  TableList.Clear;
  FResultQuery.Database.GetTableNames(TableList, ShowSystemTables);
end;

procedure TOQBEngineIBX.ReadFieldList(const ATableName: string);
begin
  FieldList.Clear;
  FResultQuery.Database.GetFieldNames(ATableName, FieldList);
  FieldList.Insert(0, '*');
end;

procedure TOQBEngineIBX.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineIBX.SetQuerySQL(const Value: string);
begin
  FResultQuery.SQL.Text := Value;
end;

function TOQBEngineIBX.ResultQuery: TDataSet;
begin
  Result := FResultQuery;
end;

procedure TOQBEngineIBX.OpenResultQuery;
begin
  FTransaction.DefaultDatabase := FIBXConnection;
  FTransaction.Active := True;
  FResultQuery.Open;
end;

procedure TOQBEngineIBX.CloseResultQuery;
begin
  FResultQuery.Close;
end;

{$WARNINGS OFF}
procedure TOQBEngineIBX.SaveResultQueryData;
begin
//
end;
{$WARNINGS ON}

procedure TOQBEngineIBX.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FIBXConnection) and (Operation = opRemove) then
  begin
    FIBXCOnnection := nil;
    FResultQuery.Database := nil;
  end;
end;


end.

