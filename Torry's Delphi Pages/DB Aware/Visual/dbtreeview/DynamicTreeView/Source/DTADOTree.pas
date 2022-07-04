unit DTADOTree;

interface


uses SysUtils, DB, DTDBTreeView;

type
  TDTADOTree = class(TDTDBTreeView)
  protected
    procedure CreateCloneDataSet; override;
  public
  end;

implementation

{ TDTADOTree }

uses ADOdb, DTCommon;

type
  TCustomADODataSetClass = class of TCustomADODataSet;

procedure TDTADOTree.CreateCloneDataSet;
var
  DataSetClass: TCustomADODataSetClass;
begin
  if DataSource.DataSet is TCustomADODataSetClass then
  begin
    DataSetClass := TCustomADODataSetClass(DataSource.DataSet.ClassType);
    FCloneDataSet := DataSetClass.Create(nil);
    with FCloneDataSet as TCustomADODataSet do
    begin
      Clone(Self.DataSource.DataSet as TCustomADODataSet, ltReadOnly);
      Active := True;
    end;
  end
  else
    raise Exception.Create(sDatasetIsNotCustomADODataSet);  
end;

end.
