unit OQBExqry;

{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for TCustomxQuery sources        }
{                                                       }
{       Adapted by Alfonso moreno                       }
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Db, QBuilder, xquery;

type
  TOQBEnginexQry = class(TOQBEngine)
  private
    FxQuery: TCustomxQuery;
  public
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
    property  xQuery : TCustomxQuery read FxQuery write FxQuery;
  end;

implementation


{ TOQBEnginexQry }
procedure TOQBEnginexQry.SetDatabaseName(const Value: string);
begin
  inherited SetDatabaseName(Value);
end;

function TOQBEnginexQry.SelectDatabase: boolean;
begin
   Result:=true;
end;

procedure TOQBEnginexQry.ReadTableList;
var
  i: Integer;
begin
  TableList.Clear;
  with FxQuery do
     for i:= 0 to DataSets.Count - 1 do
         TableList.Add(DataSets[i].Alias);
end;

procedure TOQBEnginexQry.ReadFieldList(ATableName: string);
var
  i, j   : Integer;
  TmpFields : TFieldDefs;
  DataSet : TDataSet;
begin
  FieldList.Clear;
  for i := 0 to FxQuery.DataSets.Count - 1 do
     if AnsiCompareText(ATableName, FxQuery.DataSets[i].Alias) = 0 then
     begin
        DataSet := FxQuery.DataSets[i].DataSet;
        TmpFields  := DataSet.FieldDefs;
        try
          DataSet.Active:=true;
          FieldList.Add('*');
          for j:= 0 to TmpFields.Count-1 do
          begin
            FieldList.Add(TmpFields.Items[j].Name);
          end;
        except
          on E:Exception do
            begin
              ShowMessage(E.Message);
              Exit;
            end;
        end;
        Exit;
     end;
end;

procedure TOQBEnginexQry.ClearQuerySQL;
begin
  FxQuery.SQL.Clear;
end;

procedure TOQBEnginexQry.SetQuerySQL(Value: string);
begin
  FxQuery.SQL.Text:=Value;
end;

function TOQBEnginexQry.ResultQuery: TDataSet;
begin
  Result:=FxQuery;
end;

procedure TOQBEnginexQry.OpenResultQuery;
begin
  FxQuery.Open;
end;

procedure TOQBEnginexQry.CloseResultQuery;
begin
  FxQuery.Close;
end;

procedure TOQBEnginexQry.SaveResultQueryData;
begin
  ShowMessage('Operation is not supported.');
end;

end.
 