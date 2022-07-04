{*******************************************************}
{                                                       }
{                       EhLib v3.2                      }
{      Register object that sort data in TIBQuery       }
{                                                       }
{   Copyright (c) 2002, 2003 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TIBQuery automatically after sorting markers          }
{ will be changed.                                      }
{ TSQLDatasetFeaturesEh will try to find line in        }
{ TIBQuery.SQL string that begin from 'ORDER BY' phrase }
{ and replace line by 'ORDER BY FieldNo1 [DESC],....'   }
{ using SortMarkedColumns.                              }
{*******************************************************}

unit EhLibIBX;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, IBQuery, IBCustomDataSet, Db;

implementation

function IBDataSetDriverName(DataSet: TDataSet): String;
begin
  Result := 'INTRBASE';
end;

function DateValueToIBSQLStringProc(DataSet: TDataSet; Value: Variant): String;
begin
  Result := DateValueToDataBaseSQLString(IBDataSetDriverName(DataSet), Value)
end;

type

  TIBSQLDatasetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    constructor Create; override;
  end;

  TIBSelectSQLDatasetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    constructor Create; override;
  end;

//implementation

{ TIBSQLDatasetFeaturesEh }

constructor TIBSQLDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToIBSQLStringProc;
  SQLPropName := 'SQL';
end;

{ TIBSelectSQLDatasetFeaturesEh }

constructor TIBSelectSQLDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToIBSQLStringProc;
  SQLPropName := 'SelectSQL';
end;

initialization
  RegisterDatasetFeaturesEh(TIBSQLDatasetFeaturesEh, TIBQuery);
  RegisterDatasetFeaturesEh(TIBSelectSQLDatasetFeaturesEh, TIBDataSet);
end.
