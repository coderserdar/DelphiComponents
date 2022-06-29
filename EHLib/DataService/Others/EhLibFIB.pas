{*******************************************************}
{                                                       }
{                       EhLib v2.5                      }
{    Register object that sort data in TpFIBDataset     }
{                                                       }
{      Copyright (c) 2002 by Konstantin Beliaev         }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TpFIBDataset automatically after sorting markers      }
{ will be changed.                                      }
{ TFIBDatasetFeaturesEh will sort data locally          }
{ using DoSort procedure of TpFIBDataset                }
{*******************************************************}

unit EhLibFIB;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, DBGridEh, DB, pFIBDataSet;

type
  TFIBDatasetFeaturesEh = class(TDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation
uses Classes;

procedure TFIBDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
var FLD  : array of TVarRec ;
    sort : array of boolean;
    I,J  : integer;
    Grid : TCustomDBGridEh;
begin
  if Sender is TCustomDBGridEh then begin
    Grid:=TCustomDBGridEh(Sender);
    J:=Grid.SortMarkedColumns.Count;
    setlength(fld,J);setlength(sort,J);
    for i:=0 to pred(j) do
      begin
       fld[i].VType:=vtAnsiString;
       string(fld[i].VString):=Grid.SortMarkedColumns[i].fieldname;
       sort[i]:=Grid.SortMarkedColumns[i].Title.SortMarker=smDownEh;
      end;
      TpFibDataset(Dataset).DoSort(fld,sort);
    end;
end;

initialization
  RegisterDatasetFeaturesEh(TFIBDatasetFeaturesEh, TpFIBDataSet);
end.
