{*******************************************************}
{                                                       }
{                       EhLib v2.5                      }
{    Register object that sort data in FBDataset        }
{                                                       }
{      Copyright (c) 2004 by Lagunov Aleksey            }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ DBDataset automatically after sorting markers         }
{ will be changed.                                      }
{ TFBUDatasetFeaturesEh will sort data locally          }
{ using SortOnField procedure of FBDataset              }
{*******************************************************}

unit EhLibFBU;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, DBGridEh, DB, FBCustomDataSet;

type
  TFBUDatasetFeaturesEh = class(TDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation
uses Classes;

procedure TFBUDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
var FLD  : array of TVarRec ;
    sort : array of boolean;
    I,J  : integer;
    Grid : TCustomDBGridEh;
begin
  if Sender is TCustomDBGridEh then
  begin
    Grid:=TCustomDBGridEh(Sender);
    if Grid.SortMarkedColumns.Count=1 then
    begin
//      J:=Grid.SortMarkedColumns.Count;
      TFBCustomDataSet(Dataset).SortOnField(Grid.SortMarkedColumns[0].fieldname, Grid.SortMarkedColumns[0].Title.SortMarker=smDownEh);
    end;
  end;
end;

initialization
  RegisterDatasetFeaturesEh(TFBUDatasetFeaturesEh, TFBCustomDataSet);
end.
