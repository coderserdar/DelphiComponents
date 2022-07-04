{*******************************************************}
{                                                       }
{                       EhLib v3.2                      }
{          Register object that sort data in            }
{                TADODataSet, TADOQuery                 }
{                                                       }
{   Copyright (c) 2002, 2003 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TADOQuery automatically after sorting markers         }
{ will be changed.                                      }
{ TSQLDatasetFeaturesEh will try to find line in        }
{ TADOQuery.SQL string that begin from 'ORDER BY' phrase}
{ and replace line by 'ORDER BY FieldNo1 [DESC],....'   }
{ using SortMarkedColumns.                              }
{*******************************************************}

unit EhLibADO;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, ADODB, DB, DBGridEh;

implementation

uses Classes;

//type
//  TADODataSetCr ack = class(TADODataSet);

function ADODataSetDriverName(DataSet: TADODataSet): String;
begin
  Result := 'MSACCESS';
end;

function DateValueToADOSQLStringProc(DataSet: TDataSet; Value: Variant): String;
begin
  Result := DateValueToDataBaseSQLString(ADODataSetDriverName(TADODataSet(DataSet)), Value)
end;

procedure SortDataInADODataSet(Grid: TCustomDBGridEh; DataSet: TCustomADODataSet);
var
  s: String;
  i: Integer;
begin
  s := '';
  for i := 0 to Grid.SortMarkedColumns.Count - 1 do
  begin
    s := s + Grid.SortMarkedColumns[i].FieldName;
    if Grid.SortMarkedColumns[i].Title.SortMarker = smUpEh
      then s := s + ' DESC, '
      else s := s + ', ';
  end;
  DataSet.Sort := Copy(s, 1, Length(s) - 2);
end;

type

  TADOSQLDatasetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    constructor Create; override;
  end;

  TADOCommandTextDatasetFeaturesEh = class(TCommandTextDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    constructor Create; override;
  end;

//implementation

{ TADOSQLDatasetFeaturesEh }

procedure TADOSQLDatasetFeaturesEh.ApplySorting(Sender: TObject;
  DataSet: TDataSet; IsReopen: Boolean);
begin
  if Sender is TCustomDBGridEh then
    if TCustomDBGridEh(Sender).SortLocal then
      SortDataInADODataSet(TCustomDBGridEh(Sender), TCustomADODataSet(DataSet))
    else
      inherited ApplySorting(Sender, DataSet, IsReopen);
end;

constructor TADOSQLDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToADOSQLStringProc;
  SupportsLocalLike := True;
end;

{ TADOCommandTextDatasetFeaturesEh }

procedure TADOCommandTextDatasetFeaturesEh.ApplySorting(Sender: TObject;
  DataSet: TDataSet; IsReopen: Boolean);
begin
  if Sender is TCustomDBGridEh then
    if TCustomDBGridEh(Sender).SortLocal then
      SortDataInADODataSet(TCustomDBGridEh(Sender), TCustomADODataSet(DataSet))
    else
      inherited ApplySorting(Sender, DataSet, IsReopen);
end;

constructor TADOCommandTextDatasetFeaturesEh.Create;
begin
  inherited Create;
  DateValueToSQLString := DateValueToADOSQLStringProc;
  SupportsLocalLike := True;
end;

initialization
  RegisterDatasetFeaturesEh(TADOSQLDatasetFeaturesEh, TADOQuery);
  RegisterDatasetFeaturesEh(TADOCommandTextDatasetFeaturesEh, TCustomADODataSet);
end.
