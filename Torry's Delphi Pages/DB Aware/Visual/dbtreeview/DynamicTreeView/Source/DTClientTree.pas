unit DTClientTree;

interface

{$I DTDBTree.Inc}

uses SysUtils, DB, DTDBTreeView;

type
  TDTClientTree = class(TDTDBTreeView)
  protected
    procedure CreateCloneDataSet; override;
  public
  end;

implementation

uses DBClient, DTCommon;

{ TDTClientTree }

type
{$IFNDEF TR_DELPHI5}
  TCustomClientDataSetClass = class of TCustomClientDataSet;
{$ELSE}
  TClientDataSetClass = class of TClientDataSet;
{$ENDIF}

procedure TDTClientTree.CreateCloneDataSet;
var
{$IFNDEF TR_DELPHI5}
  DataSetClass: TCustomClientDataSetClass;
{$ELSE}
  DataSetClass: TClientDataSetClass;
{$ENDIF}
begin
{$IFNDEF TR_DELPHI5}
  if DataSource.DataSet is TCustomClientDataSet then
  begin
    DataSetClass := TCustomClientDataSetClass(DataSource.DataSet.ClassType);
    FCloneDataSet := DataSetClass.Create(nil);
    with FCloneDataSet as TCustomClientDataSet do
    begin
      CloneCursor(Self.DataSource.DataSet as TCustomClientDataSet, False, False);
    end;
  end
  else
    raise Exception.Create(sDatasetIsNotCustomClientDataSet);
{$ELSE}
  if DataSource.DataSet is TClientDataSet then
  begin
    DataSetClass := TClientDataSetClass(DataSource.DataSet.ClassType);
    FCloneDataSet := DataSetClass.Create(nil);
    with FCloneDataSet as TClientDataSet do
    begin
      CloneCursor(Self.DataSource.DataSet as TClientDataSet, False, False);
      Active := True;
    end;
  end
  else
    raise Exception.Create(sDatasetIsNotClientDataSet);
{$ENDIF}
end;

end.
