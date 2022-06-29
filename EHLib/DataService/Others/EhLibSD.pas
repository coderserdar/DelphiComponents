{*******************************************************}
{                                                       }
{                        EhLib                          }
{    Copyright (c) 2002 - 2004 by Dmitry V. Bolshakov   }
{                                                       }
{  Register object that sort and filtering data in      }
{             TSDQuery from SQLDirect                   }
{   Copyright (c) 2003-2004 by Andrew Holubovski        }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TSDQuery automatically after sorting markers          }
{ will be changed.                                      }
{ TSQLDatasetFeaturesEh will try to find line in        }
{ TSDQuery.SQL string that begin from 'ORDER BY' phrase }
{ and replace line by 'ORDER BY FieldNo1 [DESC],....'   }
{ using SortMarkedColumns.                              }
{*******************************************************}

unit EhLibSD;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, DBGridEh, SDEngine, Db, SysUtils;

type
  TSDDataSetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation

uses TypInfo;

function SDDataSetDriverName(DataSet: TSDDataSet): String;
begin
  Result := GetEnumName(TypeInfo(TSDServerType), Ord(DataSet.Database.ServerType));
  Result := Copy(Result, 3, Length(Result)-2)
end;

function DateValueToSDSQLStringProc(DataSet: TDataSet; Value: Variant): String;
begin
  Result := DateValueToDataBaseSQLString(SDDataSetDriverName(TSDDataSet(DataSet)), Value)
end;

{ TSDDataSetFeaturesEh }

procedure TSDDataSetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
begin
  if TDBGridEh(Sender).STFilter.Local then
    TDBGridEh(Sender).DataSource.DataSet.Filter :=
      GetExpressionAsFilterString(TDBGridEh(Sender), GetOneExpressionAsLocalFilterString, nil)
  else
    ApplyFilterSQLBasedDataSet(TDBGridEh(Sender), DateValueToSDSQLStringProc, IsReopen, 'SQL');
end;

initialization
  RegisterDatasetFeaturesEh(TSDDataSetFeaturesEh, TSDQuery);
end.
