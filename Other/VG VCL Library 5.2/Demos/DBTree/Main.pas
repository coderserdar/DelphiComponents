unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, vgdbtree, DB, DBTables, Mask, ToolEdit, vgTreeCm,
  Explorer, ExplrDB, vgCtrls, ExplCtrl, vgTools, Grids, DBGrids;

type
  TMainForm = class(TForm)
    dsTree: TDataSource;
    ExplorerRootNode1: TExplorerRootNode;
    ExplorerSource1: TExplorerSource;
    ExplorerDBTreeRootNode1: TExplorerDBTreeRootNode;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    dbTree: TvgDBTreeView;
    etv: TExplorerTreeView;
    ExplorerTreeCombo1: TExplorerTreeCombo;
    DBGrid1: TDBGrid;
    procedure cmCloseClick(Sender: TObject);
    procedure dbTreeSetRange(Sender: TObject; DataSet: TDataSet;
      ParentID: string);
    procedure FormShow(Sender: TObject);
    procedure dbTreeProcessBranches(Sender: TObject; Node: TTreeNode;
      var Process: Boolean);
    procedure ExplorerDBTreeRootNode1SetRange(
      ExplorerNodes: TExplorerNodes; DataSet: TDataSet; ParentID: String);
    procedure dbTreeCancelRange(Sender: TObject; DataSet: TDataSet);
    procedure ExplorerDBTreeRootNode1HasBranches(Sender: TObject;
      ExplorerNodes: TExplorerDBTreeNode; var HasBranches: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses BDE, vgUtils, ADm;

{$R *.DFM}

procedure TMainForm.cmCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.dbTreeSetRange(Sender: TObject; DataSet: TDataSet;
  ParentID: string);
begin
  if ParentID = NullValue then ParentID := '';
  with TTable(dsTree.DataSet) do
  begin
    SetRangeStart;
    dm.tbTree.FieldByName('Parent_ID').AsString := ParentID;
    SetRangeEnd;
    dm.tbTree.FieldByName('Parent_ID').AsString := ParentID;
    ApplyRange;
  end;
end;

procedure TMainForm.dbTreeCancelRange(Sender: TObject; DataSet: TDataSet);
begin
  with TTable(DataSet) do CancelRange;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not dm.db.Connected then
    dm.db.Params.Values[szPATH] := ExtractFilePath(AppFileName);
  dm.tbTree.Open;
end;

procedure TMainForm.dbTreeProcessBranches(Sender: TObject; Node: TTreeNode;
  var Process: Boolean);
begin
  if Assigned(Node) and (Node.Level = 2) then Process := False;
end;

procedure TMainForm.ExplorerDBTreeRootNode1SetRange(
  ExplorerNodes: TExplorerNodes; DataSet: TDataSet; ParentID: String);
begin
  dbTreeSetRange(nil, DataSet, ParentID);
end;

procedure TMainForm.ExplorerDBTreeRootNode1HasBranches(Sender: TObject;
  ExplorerNodes: TExplorerDBTreeNode; var HasBranches: Boolean);
var
  Node: TTreeNode;
begin
  Node := etv.FindNode(ExplorerNodes);
  HasBranches := not Assigned(Node) or (Node.Level < 3);
end;

end.
