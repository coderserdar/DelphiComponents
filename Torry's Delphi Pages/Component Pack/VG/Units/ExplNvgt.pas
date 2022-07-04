{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: explorer navigator          }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplNvgt;

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Forms, DesignIntf,
  DesignWindows, Explorer, ComCtrls, ExplCtrl, Menus, vgCtrls;

type
{$IFDEF _D4_}
  TFormDesigner = IDesigner;
{$ENDIF}

{ TExplorerNodesNavigatorForm }
  TExplorerNodesNavigatorForm = class(TDesignWindow)
    etTree: TExplorerTreeView;
    es: TExplorerSource;
    puPopup: TPopupMenu;
    miNew: TMenuItem;
    miDel: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure etTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure miDelClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    function GetExplorerNodes: TExplorerNodes;
    procedure SetExplorerNodes(Value: TExplorerNodes);
    procedure UpdateData;
  public
    { Public declarations }
    procedure ItemDeleted(const ADesigner: IDesigner; AComponent: TPersistent); override;
//    procedure ComponentDeleted(AComponent: TComponent); override;
    property ExplorerNodes: TExplorerNodes read GetExplorerNodes write SetExplorerNodes;
    procedure DesignerClosed(const Form: IDesigner; AGoingDormant: Boolean); override;
//    procedure DesignerClosed(Form: TCustomForm); override;
    procedure ItemsModified(const Designer: IDesigner); override;
//    procedure DesignerModified; override;
  end;

implementation
uses vgUtils, ExplEdit;

{$R *.DFM}

procedure TExplorerNodesNavigatorForm.ItemDeleted(const ADesigner: IDesigner; AComponent: TPersistent);
//procedure TExplorerNodesNavigatorForm.ComponentDeleted(AComponent: TComponent);
begin
  if (ExplorerNodes = nil) or (AComponent = ExplorerNodes) then
  begin
    Close;
  end;
end;

procedure TExplorerNodesNavigatorForm.ItemsModified(const Designer: IDesigner);
//procedure TExplorerNodesNavigatorForm.FormModified;
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TExplorerNodesNavigatorForm.UpdateData;
begin
  if Assigned(ExplorerNodes) then
    Caption := Format('%s.%s', [Designer.Root.Name, ExplorerNodes.Name]) else
    Caption := Format('%s.none', [Designer.Root.Name]);
end;

function TExplorerNodesNavigatorForm.GetExplorerNodes: TExplorerNodes;
begin
  Result := es.ExplorerRoot;
end;

procedure TExplorerNodesNavigatorForm.SetExplorerNodes(Value: TExplorerNodes);
begin
  es.ExplorerRoot := Value;
  UpdateData;
end;

procedure TExplorerNodesNavigatorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TExplorerNodesNavigatorForm.DesignerClosed(const Form: IDesigner; AGoingDormant: Boolean);
//procedure TExplorerNodesNavigatorForm.FormClosed(Form: TCustomForm);
begin
  if (Form = Designer) then
  begin
    Free;
  end;
end;

procedure TExplorerNodesNavigatorForm.FormDestroy(Sender: TObject);
begin
  ExplorerNodes := nil;
end;

procedure TExplorerNodesNavigatorForm.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then ActivateInspector(#0);
end;

procedure TExplorerNodesNavigatorForm.miNewClick(Sender: TObject);
begin
  CreateExplorerNodes(etTree.SelectedExplorerNodes, Designer);
end;

procedure TExplorerNodesNavigatorForm.miDelClick(Sender: TObject);
begin
  etTree.SelectedExplorerNodes.Free;
end;

procedure TExplorerNodesNavigatorForm.etTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  List: IDesignerSelections;
  Nodes: TExplorerNodes;
begin
  if Active then
  begin
    List := TDesignerSelections.Create;
    try
      Nodes := etTree.SelectedExplorerNodes;
      if Assigned(Nodes) and not Nodes.IsRunTime then List.Add(Nodes);
      miDel.Enabled := (es.ExplorerRoot <> Nodes) and (List.Count > 0);
      Designer.SetSelections(List)
    except
//      List.Free;
      raise;
    end;
  end;
end;

end.
