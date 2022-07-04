{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for BDE Sources            }
{                                                       }
{       Copyright (c) 1996-2003 Sergey Orlik            }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Borland Moscow office                           }
{       Internet:  support@fast-report.com,             }
{                  sorlik@borland.com                   }
{                  http://www.fast-report.com           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEIBO;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, IB_Components, IBDataset, QBuilder;

type
  TOQBEngineIBO = class(TOQBEngine)
  private
    FDatabase: TIB_Database;
    FResultQuery: TIBQuery;
    FShowViews: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetDatabaseName(const Value: string); override;
    function  SelectDatabase: Boolean; override;
    procedure ReadTableList; override;
    procedure ReadFieldList(const ATableName: string); override;
    procedure ClearQuerySQL; override;
    procedure SetQuerySQL(const Value: string); override;
    function  ResultQuery: TDataSet; override;
    procedure OpenResultQuery; override;
    procedure CloseResultQuery; override;
    procedure SaveResultQueryData; override;
  published
    property UserName;
    property Password;
    property ShowViews: Boolean read FShowViews write FShowViews default True;
  end;


implementation

uses
  IB_Schema, QBDBFrm2;

{ TOQBEngineIBO }

constructor TOQBEngineIBO.Create(AOwner: TComponent);
begin
  inherited;
  FShowViews := True;
  FDatabase := TIB_Database.Create(Self);
  FResultQuery := TIBQuery.Create(Self);
  FResultQuery.KeyLinksAutoDefine := False;
end;

destructor TOQBEngineIBO.Destroy;
begin
  CloseResultQuery;
  FResultQuery.Free;
  FDatabase.Connected := False;
  FDatabase.Free;
  inherited;
end;

procedure TOQBEngineIBO.Loaded;
begin
  inherited;
  FDatabase.UserName := UserName;
  FDatabase.Password := Password;
  try
    if FDatabase.DatabaseName <> EmptyStr then
      FDatabase.Connected := True;
  except
    FDatabase.LoginPrompt := True;
  end;  
end;

procedure TOQBEngineIBO.SetDatabaseName(const Value: string);
begin
  if FDatabase.Connected then
    FDatabase.Connected := False;
  inherited SetDatabaseName(Value);
  FDatabase.DatabaseName := Value;
  FResultQuery.DatabaseName := Value;
end;

function TOQBEngineIBO.SelectDatabase: Boolean;
var
  QBDBForm: TOQBDBForm2;
begin
  Result := False;
  //  Check and set UserName
  if (UserName <> EmptyStr) and (FDatabase.UserName <> UserName) then
    FDatabase.UserName := UserName 
  else if FDatabase.UserName <> EmptyStr then
    UserName := FDatabase.UserName;
  //  Check and set Password
  if (Password <> EmptyStr) and (FDatabase.Password <> Password) then
    FDatabase.Password := Password
  else if FDatabase.Password <> EmptyStr then
    Password := FDatabase.Password;

  // Select new database ...
  QBDBForm := TOQBDBForm2.Create(Application);
  QBDBForm.EdtDb.Text := DatabaseName;
  if QBDBForm.ShowModal = mrOk then
  begin
    if QBDBForm.EdtDb.Text <> EmptyStr then
    try
      DatabaseName := QBDBForm.EdtDb.Text;
      ShowSystemTables := QBDBForm.CheckDB.Checked;
      ShowViews := QBDBForm.CheckView.Checked;
      FDatabase.LoginPrompt := not ((UserName <> EmptyStr) and 
        (Password <> EmptyStr) and (DatabaseName <> EmptyStr));
      //  Trying to connect ...
      FDatabase.Connected := True;
      Result := True;
    finally
      QBDBForm.Free;
    end;
  end
  else
    QBDBForm.Free;
end;

procedure TOQBEngineIBO.ReadTableList;
begin
//  TableList.Clear;
  try
    SchemaRelationNames(FDatabase , FDatabase.IB_Transaction, ShowSystemTables, 
      True, ShowViews, TableList);
  except
  end;
end;

procedure TOQBEngineIBO.ReadFieldList(const ATableName: string);
begin
//  FieldList.Clear;
  try
    SchemaFieldNames(FDatabase, FDatabase.IB_Transaction, FieldList, ATableName);
    FieldList.Insert(0, '*');
  except
  end;
end;

procedure TOQBEngineIBO.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineIBO.SetQuerySQL(const Value: string);
begin
  ClearQuerySQL;
  FResultQuery.SQL.Text := Value;
end;

function TOQBEngineIBO.ResultQuery: TDataSet;
begin
  Result := FResultQuery;
end;

procedure TOQBEngineIBO.OpenResultQuery;
begin
  if not FResultQuery.Prepared then
    FResultQuery.Prepare;
  FResultQuery.Active := True;
end;

procedure TOQBEngineIBO.CloseResultQuery;
begin
  FResultQuery.Active := False;
  FResultQuery.Prepared := False;
end;

procedure TOQBEngineIBO.SaveResultQueryData;
begin
  ShowMessage('Operation is not supported.');
end;

end.

