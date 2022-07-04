{*******************************************************}
{                                                       }
{                         EhLib                         }
{    Copyright (c) 2002, 2003 by Dmitry V. Bolshakov    }
{                                                       }
{  Register object that sort and filtering data in      }
{                   TkbmMemTable                        }
{       Copyright (c) 2004 by Andrew Holubovski         }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Add this unit to 'uses' clause of any unit of your    }
{ project to allow TDBGridEh to sort data in            }
{ TkbmMemTable automatically after sorting markers will }
{ be changed.                                           }
{ TkbmDatasetFeaturesEh determine it will create        }
{ IndexDefs with name 'SortIndexEh' using               }
{ SortMarkedColumns and set new index to                }
{ kbmMemTable.FSortIndex                                }
{*******************************************************}

unit EhLibKbmMT;

{$I EhLib.Inc}

interface

uses
  DbUtilsEh, DBGridEh, Db, kbmMemTable;

type

  TkbmDatasetFeaturesEh = class(TDatasetFeaturesEh)
  public
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
  end;

implementation

Type
 TkbmMemTableCrack = Class(TkbmMemTable);

 { TkbmDatasetFeaturesEh }

procedure Sort(DataSet: TkbmMemTableCrack; sIndexFields, sIndexDescFields: String);
var
   OldRange:boolean;
   IndexDef: TIndexDef;
begin
  With DataSet do
   begin
    If not Active then
     Exit;
    CheckBrowseMode;

    OldRange:= FRangeActive;
    FRangeActive:=false;
    Try
     // Check if old sort index defined, remove it.
     If FSortIndex<>nil then
      begin
       Indexes.DeleteIndex(FSortIndex);
       FSortIndex.free;
       FSortIndex:=nil;
      end;
     // setup IndexDef
     If IndexDefs.IndexOf('SortIndexEh')=-1
      then IndexDef := TIndexDef.Create(IndexDefs, 'SortIndexEh', sIndexFields, [])
      else IndexDef := IndexDefs[IndexDefs.IndexOf('SortIndexEh')];
     IndexDef.Fields := sIndexFields;
     IndexDef.DescFields := sIndexDescFields;
     // Now add a new index.
     FSortIndex := TkbmIndex.Create(IndexDef, DataSet);
     Indexes.AddIndex(FSortIndex);
     FSortIndex.Rebuild;

     SwitchToIndex(FSortIndex);
    Finally
     FRangeActive:=OldRange;
    End;
  end;
end;

procedure TkbmDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
var
  i: Integer;
  IndexFields, IndexDescFields: String;
begin
  If Sender is TCustomDBGridEh then
   With TCustomDBGridEh(Sender) do
    begin
     If SortMarkedColumns.Count=0 then
      begin
       TkbmMemTableCrack(DataSet).IndexName := '';
       Exit;
      end;
     IndexFields := '';
     IndexDescFields := '';
     For i := 0 to SortMarkedColumns.Count - 1 do
      begin
       IndexFields := IndexFields + SortMarkedColumns[i].FieldName + ';';
       If SortMarkedColumns[i].Title.SortMarker = smUpEh then
        IndexDescFields := IndexDescFields + SortMarkedColumns[i].FieldName + ';'
      end;
     Delete(IndexFields,Length(IndexFields),1);
     Delete(IndexDescFields,Length(IndexDescFields),1);
     Sort(TkbmMemTableCrack(DataSet), IndexFields, IndexDescFields);
    End
end;

procedure TkbmDatasetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean);
begin
  TDBGridEh(Sender).DataSource.DataSet.Filter :=
      GetExpressionAsFilterString(TDBGridEh(Sender),
        GetOneExpressionAsLocalFilterString, nil, False, True);
end;

initialization
  RegisterDatasetFeaturesEh(TkbmDatasetFeaturesEh, TkbmMemTable);
end.
