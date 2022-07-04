{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library demo                         }
{                                                       }
{         Copyright (c) 1997, 1998                      }
{                                                       }
{*******************************************************}

{$I VG.INC}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, vgCtrls, CommCtrl, ComCtrls, vgTools, StdCtrls, Menus,
  Buttons, Explorer, ExplCtrl, ImgList, ExplShl;

type
  TMainForm = class(TForm)
    paExplorer: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    vgSplitter1: TvgSplitter;
    vgSplitter2: TvgSplitter;
    paAdd: TPanel;
    Panel6: TPanel;
    enRoot: TExplorerRootNode;
    esDrives: TExplorerSource;
    esDrives2: TExplorerSource;
    esCommands: TExplorerSource;
    esMenuItems: TExplorerSource;
    lvDrives: TExplorerListView;
    Panel7: TPanel;
    Panel8: TPanel;
    tvDrives: TExplorerTreeView;
    Panel10: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    enCommands: TExplorerRootNode;
    ExplorerFolderNode1: TExplorerFolderNode;
    ExplorerFolderNode2: TExplorerFolderNode;
    ExplorerFolderNode3: TExplorerFolderNode;
    ExplorerFolderNode4: TExplorerFolderNode;
    ExplorerFolderNode5: TExplorerFolderNode;
    vgSplitter3: TvgSplitter;
    lbCommands: TExplorerListBox;
    ExplorerFolderNode6: TExplorerFolderNode;
    ExplorerFolderNode7: TExplorerFolderNode;
    ExplorerFolderNode8: TExplorerFolderNode;
    ExplorerFolderNode9: TExplorerFolderNode;
    ExplorerFolderNode10: TExplorerFolderNode;
    ExplorerFolderNode11: TExplorerFolderNode;
    ExplorerActionNode1: TExplorerActionNode;
    ExplorerActionNode2: TExplorerActionNode;
    ExplorerActionNode3: TExplorerActionNode;
    ExplorerActionNode4: TExplorerActionNode;
    ExplorerActionNode5: TExplorerActionNode;
    ExplorerActionNode6: TExplorerActionNode;
    ExplorerActionNode7: TExplorerActionNode;
    ExplorerActionNode8: TExplorerActionNode;
    ExplorerActionNode9: TExplorerActionNode;
    ExplorerActionNode10: TExplorerActionNode;
    ExplorerActionNode11: TExplorerActionNode;
    ExplorerActionNode12: TExplorerActionNode;
    ExplorerActionNode13: TExplorerActionNode;
    ExplorerActionNode14: TExplorerActionNode;
    ExplorerActionNode15: TExplorerActionNode;
    ExplorerActionNode16: TExplorerActionNode;
    ExplorerActionNode17: TExplorerActionNode;
    ExplorerActionNode18: TExplorerActionNode;
    ExplorerActionNode19: TExplorerActionNode;
    ExplorerActionNode20: TExplorerActionNode;
    ExplorerActionNode21: TExplorerActionNode;
    ExplorerActionNode22: TExplorerActionNode;
    ExplorerActionNode23: TExplorerActionNode;
    ExplorerActionNode24: TExplorerActionNode;
    ExplorerActionNode25: TExplorerActionNode;
    ExplorerActionNode26: TExplorerActionNode;
    ExplorerActionNode27: TExplorerActionNode;
    ExplorerActionNode28: TExplorerActionNode;
    ExplorerActionNode29: TExplorerActionNode;
    ExplorerActionNode30: TExplorerActionNode;
    ExplorerActionNode31: TExplorerActionNode;
    ilCommands: TImageList;
    Panel1: TPanel;
    lvMenuItems: TExplorerListView;
    Panel4: TPanel;
    vgSplitter4: TvgSplitter;
    lbList1: TExplorerListBox;
    vgSplitter5: TvgSplitter;
    lbList2: TExplorerListBox;
    enListRoot: TExplorerRootNode;
    esList1: TExplorerSource;
    esList2: TExplorerSource;
    enList1: TExplorerStringsNode;
    enList2: TExplorerStringsNode;
    ExplorerSeparatorNode1: TExplorerSeparatorNode;
    ExplorerFormNode1: TExplorerFormNode;
    tmDrives: TTimer;
    enShelFlolder: TExplorerShellFolderNode;
    procedure tvDrivesChange(Sender: TObject; Node: TTreeNode);
    procedure esDrives2DblClick(Sender: TObject; ExplorerNodes: TExplorerNodes);
    procedure FormShow(Sender: TObject);
    procedure tvDrivesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure ViewStyleClick(Sender: TObject);
    procedure lbCommandsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmDrivesTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation
uses vgUtils, ComObj, ShellAPI, SHlObj;

{$R *.DFM}
{$R COMMANDS.RES}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ilCommands.ResourceLoad(rtBitmap, 'IDB_COMMANDS', clFuchsia);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (tvDrives.Items.GetFirstNode <> nil) then
    tvDrives.Items.GetFirstNode.Expanded := True;
end;

procedure TMainForm.tmDrivesTimer(Sender: TObject);
var
  Node: TTreeNode;
  Nodes: TExplorerNodes;
begin
  Node := tvDrives.Selected;
  if Assigned(Node) then
    Nodes := (Node as TExplorerTreeNode).ExplorerNodes else
    Nodes := enShelFlolder;
  esDrives2.ExplorerRoot := Nodes;
end;

procedure TMainForm.tvDrivesChange(Sender: TObject; Node: TTreeNode);
begin
  tmDrives.Enabled := False;
  tmDrives.Enabled := True;
end;

procedure TMainForm.lbCommandsClick(Sender: TObject);
begin
  esMenuItems.ExplorerRoot := lbCommands.SelectedExplorerNodes;
end;

procedure TMainForm.esDrives2DblClick(Sender: TObject;
  ExplorerNodes: TExplorerNodes);
var
  Node: TTreeNode;
begin
  Node := tvDrives.Selected;
  if Assigned(Node) then Node.Expanded := True;
  if ExplorerNodes.CanExpand(ntNodeTypesAll) then
    tvDrives.SelectedExplorerNodes := ExplorerNodes
  else
    ExplorerNodes.DblClick;
end;

procedure TMainForm.tvDrivesCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := Node <> (Sender as TExplorerTreeView).Items.GetFirstNode;
end;

procedure TMainForm.ViewStyleClick(Sender: TObject);
begin
  lvDrives.ViewStyle := TViewStyle(TComponent(Sender).Tag);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Tmp: TExplorerShellFolderNode;
begin
  Tmp := TExplorerShellFolderNode.Create(nil);
  Tmp.Parent := enRoot;
  esDrives.ExplorerRoot := Tmp;
//  Tmp.Folder := sfControls;
end;

end.

