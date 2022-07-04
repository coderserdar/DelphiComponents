unit DTTableTree;

interface

uses SysUtils, DB, DTDBTreeView;

type
  TDTTableTree = class(TDTDBTreeView)
  protected
    procedure CreateCloneDataSet; override;
  public
  end;

implementation

uses DbTables, DTCommon;

{ TVirtualDBTableTree }

procedure TDTTableTree.CreateCloneDataSet;
begin
  if DataSource.DataSet is TTable then
  begin
    FCloneDataSet := TTable.Create(nil);
    with FCloneDataSet as TTable do
    begin
      DatabaseName := (Self.DataSource.DataSet as TTable).DatabaseName;
      TableType := (Self.DataSource.DataSet as TTable).TableType;
      TableName := (Self.DataSource.DataSet as TTable).TableName;
      Active := True;
    end;
  end
  else
    raise Exception.Create(sDatasetIsNotTable);  
end;

end.
