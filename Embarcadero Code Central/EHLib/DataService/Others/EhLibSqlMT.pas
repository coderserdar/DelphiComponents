{*******************************************************}
{                                                       }
{                         EhLib                         }
{    Copyright (c) 2002 - 2004 by Dmitry V. Bolshakov   }
{                                                       }
{   Register object that sort and filtering data in     }
{  TSQLMemTable & TSQLMemQuery from AidAim SQLMemTable  }
{       Copyright (c) 2004 by Andrew Holubovski         }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TSQLMemTable and TSQLMemQuery automatically after     }
{ sorting markers will be changed.                      }
{ TSQLMemTableDatasetFeaturesEh determine it will       }
{ create Index with name 'SortIndexEh' using            }
{ SortMarkedColumns and set IndexName to 'SortIndexEh'  }
{ TSQLMemQueryDataSetFeaturesEh will try to find line   }
{ in TSQLMemQuery.SQL string that begin from 'ORDER BY' }
{ phrase and replace line by 'ORDER BY FieldNo1         }
{ [DESC],....' using SortMarkedColumns.                 }
{*******************************************************}

unit EhLibSqlMT;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, DBGridEh, Db, SQLMemMain;

type

  TSQLMemTableDatasetFeaturesEh = class(TDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure SortTable(Grid: TCustomDBGridEh; DataSet: TSQLMemTable);
  end;

  TSQLMemQueryDataSetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
    constructor Create; override;
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation


procedure SQLMemApplyFilter(Grid: TDBGridEh; DataSet: TDataSet);
begin
  DataSet.Filter := GetExpressionAsFilterString(Grid, GetOneExpressionAsLocalFilterString, nil);
  DataSet.Filtered := False;
  DataSet.Filtered := True;
end;

{ TSQLMemTableDatasetFeaturesEh }

procedure TSQLMemTableDatasetFeaturesEh.SortTable(Grid: TCustomDBGridEh; DataSet: TSQLMemTable);
var
  i: Integer;
  sIndexFields, sIndexDescFields: String;
begin
 If DataSet.Active then
  begin
   If Grid.SortMarkedColumns.Count>0
    then begin
     sIndexFields := '';
     sIndexDescFields := '';
     For i := 0 to Grid.SortMarkedColumns.Count - 1 do
      begin
       sIndexFields := sIndexFields + Grid.SortMarkedColumns[i].FieldName + ';';
       If Grid.SortMarkedColumns[i].Title.SortMarker = smUpEh then
        sIndexDescFields := sIndexDescFields + Grid.SortMarkedColumns[i].FieldName + ';'
      end;
     Delete(sIndexFields,Length(sIndexFields),1);
     Delete(sIndexDescFields,Length(sIndexDescFields),1);

     DataSet.IndexName := '';
     If DataSet.IndexDefs.IndexOf('SortIndexEh')>-1
      then DataSet.DeleteIndex('SortIndexEh');
     DataSet.AddIndex('SortIndexEh', sIndexFields, [ixCaseInsensitive], sIndexDescFields);
     DataSet.IndexName := 'SortIndexEh';
    end
    else DataSet.IndexName := '';
  end;
end;

procedure TSQLMemTableDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
begin
  If Sender is TCustomDBGridEh then
   If DataSet is TSQLMemTable
      then SortTable(Sender as TCustomDBGridEh, DataSet as TSQLMemTable);
end;

procedure TSQLMemTableDatasetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
begin
  SQLMemApplyFilter(Sender as TDBGridEh, DataSet);
end;

{ TSQLMemQueryDataSetFeaturesEh }

constructor TSQLMemQueryDataSetFeaturesEh.Create;
begin
  inherited Create;
  SortUsingFieldName := True;
end;

procedure TSQLMemQueryDataSetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
begin
 SQLMemApplyFilter(Sender as TDBGridEh, DataSet);
end;

initialization
  RegisterDatasetFeaturesEh(TSQLMemTableDatasetFeaturesEh, TSQLMemTable);
  RegisterDatasetFeaturesEh(TSQLMemQueryDataSetFeaturesEh, TSQLMemQuery);
end.
