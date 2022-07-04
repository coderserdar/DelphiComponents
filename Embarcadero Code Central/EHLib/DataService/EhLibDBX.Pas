{*******************************************************}
{                                                       }
{                       EhLib v3.2                      }
{         Register object that sort data in             }
{               TSQLQuery, TSQLDataSet                  }
{                                                       }
{   Copyright (c) 2002, 2003 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TSQLQuery automatically after sorting markers         }
{ will be changed.                                      }
{ TSQLDatasetFeaturesEh will try to find line in        }
{ TSQLQuery.SQL string that begin from 'ORDER BY' phrase}
{ and replace line by 'ORDER BY FieldNo1 [DESC],....'   }
{ using SortMarkedColumns.                              }
{*******************************************************}

unit EhLibDBX;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, SqlExpr, Db;

implementation

function DBXDataSetDriverName(DataSet: TCustomSQLDataSet): String;
begin
  Result := DataSet.SQLConnection.DriverName;
end;

function DateValueToDBXSQLStringProc(DataSet: TDataSet; Value: Variant): String;
begin
  Result := DateValueToDataBaseSQLString(DBXDataSetDriverName(TCustomSQLDataSet(DataSet)), Value)
end;

type

  TDBXSQLDatasetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    constructor Create; override;
  end;

  TDBXCommandTextDatasetFeaturesEh = class(TCommandTextDatasetFeaturesEh)
  public
    constructor Create; override;
  end;

//implementation

{ TDBXSQLDatasetFeaturesEh }

constructor TDBXSQLDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToDBXSQLStringProc;
end;

{ TDBXCommandTextDatasetFeaturesEh }

constructor TDBXCommandTextDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToDBXSQLStringProc;
end;

initialization
  RegisterDatasetFeaturesEh(TDBXSQLDatasetFeaturesEh, TSQLQuery);
  RegisterDatasetFeaturesEh(TDBXCommandTextDatasetFeaturesEh, TSQLDataSet);
end.
