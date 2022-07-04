{*******************************************************}
{                                                       }
{                       EhLib v3.2                      }
{          Register object that sort data in            }
{                TCustomClientDataSet                   }
{                                                       }
{   Copyright (c) 2002, 2003 by Dmitry V. Bolshakov     }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TClientDataSet automatically  after sorting markers   }
{ will be changed.                                      }
{ TCDSDatasetFeaturesEh determine if                    }
{ TDBGridEh.SortLocal = True then it will create index  }
{ with name 'SortIndexEh' using SortMarkedColumns       }
{ else if SortLocal = False and CDS connected to other  }
{ DataSet via DataSetProvider it will try to sord data  }
{ in this DataSet using GetDatasetFeaturesForDataSet    }
{ function                                              }
{*******************************************************}

unit EhLibCDS;

{$I EhLib.Inc}

interface

uses
{$IFDEF EH_LIB_6} Variants, {$ENDIF}
  DbUtilsEh, DBGridEh, Db, DBClient, Provider, SysUtils;

type

{$IFNDEF EH_LIB_6}
  TCustomClientDataSet = class(TClientDataSet);
{$ENDIF}

  TCDSDatasetFeaturesEh = class(TDatasetFeaturesEh)
  public
    function GetProviderDataSet(DataSet: TDataSet): TDataSet; virtual;
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation

uses Classes;

type
{$IFDEF EH_LIB_6}
  TCustomClientDataSetCrack = class(TCustomClientDataSet);
{$ELSE}
  TCustomClientDataSetCrack = class(TClientDataSet);
{$ENDIF}

{ TCDSDatasetFeaturesEh }

function TCDSDatasetFeaturesEh.GetProviderDataSet(DataSet: TDataSet): TDataSet;
var
{$IFDEF CIL}
  CDS: TClientDataSet;
{$ELSE}
  CDS: TCustomClientDataSetCrack;
{$ENDIF}
  ProvComp: TComponent;
  DS: TObject;
begin
  Result := nil;
{$IFDEF EH_LIB_6}
  if not (DataSet is TCustomClientDataSet) then Exit;
{$ELSE}
  if not (DataSet is TClientDataSet) then Exit;
{$ENDIF}
{$IFDEF CIL}
{ TODO : How to get ProviderName for TCustomDataSet under CIL }
  if DataSet is TClientDataSet
    then CDS := TClientDataSet(DataSet)
    else Exit;
{$ELSE}
  CDS := TCustomClientDataSetCrack(DataSet);
{$ENDIF}
  if (CDS.ProviderName <> '') and Assigned(CDS.Owner) then
  begin
    ProvComp := CDS.Owner.FindComponent(CDS.ProviderName);
    if Assigned(ProvComp) and (ProvComp is TCustomProvider) then
    begin
      DS := GetObjectProperty(ProvComp, 'DataSet');
      if Assigned(DS) and (DS is TDataSet) then
        Result := TDataSet(DS);
    end;
  end;
end;

procedure TCDSDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
var
  DS: TDataSet;
  CDS: TCustomClientDataSetCrack;
  DatasetFeatures: TDatasetFeaturesEh;
  i: Integer;
  IndexFields, IndexDescFields: String;
begin
  if Sender is TCustomDBGridEh then
    if TCustomDBGridEh(Sender).SortLocal then
      with TCustomDBGridEh(Sender) do
      begin
        IndexFields := '';
        IndexDescFields := '';
        for i := 0 to SortMarkedColumns.Count - 1 do
        begin
          IndexFields := IndexFields + SortMarkedColumns[i].FieldName + ';';
          if SortMarkedColumns[i].Title.SortMarker = smUpEh then
            IndexDescFields := IndexDescFields + SortMarkedColumns[i].FieldName + ';'
        end;
        Delete(IndexFields,Length(IndexFields),1);
        Delete(IndexDescFields,Length(IndexDescFields),1);
{$IFDEF EH_LIB_6}
        if (DataSet is TCustomClientDataSet) then
{$ELSE}
        if (DataSet is TClientDataSet) then
{$ENDIF}
        begin
          CDS := TCustomClientDataSetCrack(DataSet);
          CDS.IndexDefs.Update;
          if CDS.IndexDefs.IndexOf('SortIndexEh') >= 0 then
            CDS.DeleteIndex('SortIndexEh');
          if IndexFields > '' then
          begin
            CDS.AddIndex('SortIndexEh', IndexFields, [], IndexDescFields);
            CDS.IndexDefs.Update;
            CDS.IndexName := 'SortIndexEh';
          end else
            CDS.IndexName := '';
          CDS.First;
        end;
      end
    else
    begin
      DS := GetProviderDataSet(DataSet);
      if DS <> nil then
      begin
        DatasetFeatures := GetDatasetFeaturesForDataSet(DS);
        if DatasetFeatures <> nil then
          DatasetFeatures.ApplySorting(Sender, DS, False);
        DataSet.Close;
        DataSet.Open;
      end;
    end;
end;

procedure TCDSDatasetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
var
  DS: TDataSet;
  DatasetFeatures: TDatasetFeaturesEh;
begin
  if TDBGridEh(Sender).STFilter.Local then
  begin
    TDBGridEh(Sender).DataSource.DataSet.Filter :=
      GetExpressionAsFilterString(TDBGridEh(Sender),
        GetOneExpressionAsLocalFilterString, nil, False, True);
    // CDS bug when empty filter and aggrigated fields
    if TDBGridEh(Sender).DataSource.DataSet.Filter = '' then
      TDBGridEh(Sender).DataSource.DataSet.Filter := '1=1';
  end else if (DataSet is {$IFDEF EH_LIB_6}TCustomClientDataSet{$ELSE}TClientDataSet{$ENDIF}) then
  begin
    DS := GetProviderDataSet(TCustomClientDataSet(DataSet));
    if DS <> nil then
    begin
      DatasetFeatures := GetDatasetFeaturesForDataSet(TDataSet(DS));
      if (DatasetFeatures <> nil) then
        DatasetFeatures.ApplyFilter(TDBGridEh(Sender), TDataSet(DS), False);
      DataSet.Close;
      DataSet.Open;
    end;
  end;
end;

initialization
{$IFDEF EH_LIB_6}
  RegisterDatasetFeaturesEh(TCDSDatasetFeaturesEh, TCustomClientDataSet);
{$ELSE}
  RegisterDatasetFeaturesEh(TCDSDatasetFeaturesEh, TClientDataSet);
{$ENDIF}
end.
