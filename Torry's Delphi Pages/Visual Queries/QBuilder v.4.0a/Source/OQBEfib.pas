{*********************************************************}
{                                                         }
{       Delphi Visual Component Library                   }
{       Open QBuilder Engine for FreeIBComponents Sources }
{                                                         }
{       Copyright (c) 1996-99 Sergey Orlik                }
{                                                         }
{     Written by:                                         }
{       Sergey Orlik                                      }
{       product manager                                   }
{       Russia, C.I.S. and Baltic States (former USSR)    }
{       Inprise Moscow office                             }
{       Internet:  sorlik@inprise.ru                      }
{       www.geocities.com/SiliconValley/Way/9006/         }
{                                                         }
{*********************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit OQBEfib;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, FIB, FIBDatabase, FIBDataset, QBuilder;

type
  TOQBEngineFIB = class(TOQBEngine)
  private
    FDatabase  : TFIBDatabase;
    FTransaction : TFIBTransaction;
    FResultQuery : TFIBDataSet;
    FShowViews: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
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
  published
    property UserName;
    property Password;
    property ShowViews : boolean read FShowViews write FShowViews default true;
  end;

implementation

uses
  ibase, QBDBFrm2;

{ TOQBEngineFIB }

constructor TOQBEngineFIB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowViews:=true;
  FDatabase:=TFIBDatabase.Create(Self);
  FTransaction:=TFIBTransaction.Create(Self);
  FDatabase.DefaultTransaction:=FTransaction;
  FResultQuery:=TFIBDataSet.Create(Self);
  FResultQuery.Database:=FDatabase;
end;

destructor TOQBEngineFIB.Destroy;
begin
  CloseResultQuery;
  FResultQuery.Free;
  FTRansaction.Free;
  if FDatabase.Connected then
    FDatabase.Connected:=false;
  FDatabase.Free;
  inherited Destroy;
end;

procedure TOQBEngineFIB.Loaded;
begin
  inherited;
  FDatabase.DBParams.Values[DPBConstantNames[isc_dpb_user_name]]:=UserName;
  FDatabase.DBParams.Values[DPBConstantNames[isc_dpb_password]]:=Password;
  try
    if FDatabase.DBName<>EmptyStr then
      FDatabase.Connected:=true;
  except
    FDatabase.UseLoginPrompt:=true;
  end;
end;

procedure TOQBEngineFIB.SetDatabaseName(const Value: string);
begin
  if FDatabase.Connected then
    FDatabase.Connected:=false;
  inherited SetDatabaseName(Value);
  FDatabase.DBName:=Value;
end;

function TOQBEngineFIB.SelectDatabase: boolean;
var
  QBDBForm : TOQBDBForm2;
begin
  Result:=false;
  // Select new database ...
  QBDBForm:=TOQBDBForm2.Create(Application);
  QBDBForm.EdtDb.Text:=DatabaseName;
  if QBDBForm.ShowModal=mrOk then
    begin
      if QBDBForm.EdtDb.Text<>EmptyStr then
      try
        DatabaseName:=QBDBForm.EdtDb.Text;
        ShowSystemTables:=QBDBForm.CheckDB.Checked;
        ShowViews:=QBDBForm.CheckView.Checked;
        if (UserName<>EmptyStr) and (Password<>EmptyStr) and (DatabaseName<>EmptyStr) then
          FDatabase.UseLoginPrompt:=false
        else
          FDatabase.UseLoginPrompt:=true;
        //  Try to connect ...
        FDatabase.Connected:=true;
        Result:=true;
      finally
        QBDBForm.Free;
      end;
    end
  else
    QBDBForm.Free;
end;

procedure TOQBEngineFIB.ReadTableList;
var
  TempDataSet : TFIBDataSet;
begin
  TableList.Clear;
  TempDataSet:=TFIBDataset.Create(Self);
  try
    if not FDatabase.Connected then
      FDatabase.Connected:=true;
    TempDataSet.Database:=FDatabase;
    TempDataSet.SelectSQL.Add('SELECT R.RDB$RELATION_NAME');
    TempDataSet.SelectSQL.Add('FROM RDB$RELATIONS R');
    if ShowSystemTables then
      begin
        TempDataSet.SelectSQL.Add('WHERE 1=1');
      end
    else
      begin
        TempDataSet.SelectSQL.Add('WHERE ( R.RDB$SYSTEM_FLAG <> 1 OR ' +
                                '  R.RDB$SYSTEM_FLAG IS NULL )');
      end;
    if not ShowViews then
    begin
      TempDataSet.SelectSQL.Add('  AND NOT EXISTS ( SELECT V.RDB$VIEW_CONTEXT');
      TempDataSet.SelectSQL.Add('                   FROM RDB$VIEW_RELATIONS V');
      TempDataSet.SelectSQL.Add('                   WHERE R.RDB$RELATION_NAME=V.RDB$VIEW_NAME )');
    end;
    TempDataSet.SelectSQL.Add('ORDER BY 1');
    FTransaction.StartTransaction;
    TempDataSet.Prepare;
    TempDataSet.Open;
    TempDataSet.First;
    while not TempDataSet.Eof do
    begin
      TableList.Add(Trim(TempDataSet.Fields[0].AsString));
      TempDataSet.Next;
    end;
  finally
    TempDataSet.Close;
    FTransaction.Commit;
    TempDataSet.Free;
  end;
end;

procedure TOQBEngineFIB.ReadFieldList(ATableName: string);
var
  TempTransaction : TFIBTransaction;
  TempDataSet : TFIBDataSet;
  TempTableName : string;
begin
  FieldList.Clear;
  TempTransaction:=TFIBTransaction.Create(Self);
  TempTransaction.DefaultDatabase:=FDatabase;
  FDatabase.AddTransaction(TempTransaction);
  TempTableName:=UpperCase(ATableName);
  TempDataSet:=TFIBDataset.Create(Self);
  try
    TempDataSet.Database:=FDatabase;
    TempDataSet.Transaction:=TempTransaction;
    TempDataSet.SelectSQL.Add('SELECT RDB$FIELD_NAME');
    TempDataSet.SelectSQL.Add('FROM RDB$RELATION_FIELDS R');
    TempDataSet.SelectSQL.Add('WHERE R.RDB$RELATION_NAME ='''+TempTableName+'''');
    TempDataSet.SelectSQL.Add('ORDER BY R.RDB$FIELD_POSITION');
    TempTransaction.StartTransaction;
    TempDataSet.Prepare;
    TempDataSet.Open;
    TempDataSet.First;
    FieldList.Add('*');
    while not TempDataSet.Eof do
    begin
      FieldList.Add(Trim(TempDataSet.Fields[0].AsString));
      TempDataSet.Next;
    end;
  finally
    If TempTransaction.Active then
      TempTransaction.Commit;
    TempDataSet.Close;
    TempDataSet.Free;
    FDatabase.RemoveTransaction(FDatabase.FindTransaction(TempTransaction));    
    TempTransaction.Free;
  end;
end;

procedure TOQBEngineFIB.ClearQuerySQL;
begin
  FResultQuery.SelectSQL.Clear;
end;

procedure TOQBEngineFIB.SetQuerySQL(Value: string);
begin
  ClearQuerySQL;
  FResultQuery.SelectSQL.Text:=Value;
end;

function TOQBEngineFIB.ResultQuery: TDataSet;
begin
  Result:=FResultQuery;
end;

procedure TOQBEngineFIB.OpenResultQuery;
begin
  FTRansaction.StartTransaction;
  if not FResultQuery.Prepared then
    FResultQuery.Prepare;
  FResultQuery.Active:=True;
end;

procedure TOQBEngineFIB.CloseResultQuery;
begin
  FResultQuery.Active:=false;
  If FTransaction.Active then
    FTRansaction.Commit;
end;

procedure TOQBEngineFIB.SaveResultQueryData;
begin
  ShowMessage('Operation is not supported.');
end;

end.

