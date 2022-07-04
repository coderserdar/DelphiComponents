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
  Windows, Messages, Classes, SysUtils, Controls, Forms, DsgnIntf,
  DsgnWnds, LibIntf, Explorer, ComCtrls, ExplCtrl, Menus, vgCtrls;

type
{$IFDEF _D4_}
  TFormDesigner = IFormDesigner;
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
{$IFDEF _D4_}
    procedure ComponentDeleted(AComponent: IPersistent); override;
{$ELSE}
    procedure ComponentDeleted(AComponent: TComponent); override;
{$ENDIF}
    property ExplorerNodes: TExplorerNodes read GetExplorerNodes write SetExplorerNodes;
    procedure FormClosed(Form: TCustomForm); override;
    procedure FormModified; override;
  end;

implementation
uses vgUtils, ExplEdit;

{$R *.DFM}

{$IFDEF _D4_}
procedure TExplorerNodesNavigatorForm.ComponentDeleted(AComponent: IPersistent);
begin
  if (ExplorerNodes = nil) or (ExtractPersistent(AComponent) = ExplorerNodes) then
{$ELSE}
procedure TExplorerNodesNavigatorForm.ComponentDeleted(AComponent: TComponent);
begin
  if (ExplorerNodes = nil) or (AComponent = ExplorerNodes) then
{$ENDIF}
  begin
    Close;
  end;
end;

procedure TExplorerNodesNavigatorForm.FormModified;
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TExplorerNodesNavigatorForm.UpdateData;
begin
  if Assigned(ExplorerNodes) then
    Caption := Format('%s.%s', [Designer.Form.Name, ExplorerNodes.Name]) else
    Caption := Format('%s.none', [Designer.Form.Name]);
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

procedure TExplorerNodesNavigatorForm.FormClosed(Form: TCustomForm);
begin
  if (Form = Designer.Form) then
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
  List: {$IFDEF _D5_}TDesignerSelectionList{$ELSE}TComponentList{$ENDIF};
  Nodes: TExplorerNodes;
begin
  if Active then
  begin
    List := {$IFDEF _D5_}TDesignerSelectionList{$ELSE}TComponentList{$ENDIF}.Create;
    try
      Nodes := etTree.SelectedExplorerNodes;
      if Assigned(Nodes) and not Nodes.IsRunTime then List.Add(Nodes);
      miDel.Enabled := (es.ExplorerRoot <> Nodes) and (List.Count > 0);
      Designer.SetSelections(List)
    except
      List.Free;
      raise;
    end;
  end;
end;

end.
