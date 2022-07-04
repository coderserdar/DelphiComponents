{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: component editor            }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}
{$WARNINGS OFF}

unit ExplEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  vgItems, Menus, StdCtrls, vgTools, Explorer, ComCtrls, ExplCtrl, DsgnIntf;

type
{ TExplorerNodesEditorForm }
  TExplorerNodesEditorForm = class(TItemsEditorForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut); override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

{ TExplorerNodesEditor }
  TExplorerNodesEditor = class(TItemsEditor)
  public
    function GetFormClass: TItemsEditorFormClass; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure CreateExplorerNodes(ExplorerRoot: TExplorerNodes; Designer: TFormDesigner);

implementation

uses vgUtils, ExplAdd, ExplNvgt;

function FindExplorerForm(Component: TComponent): TExplorerNodesNavigatorForm;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[I] is TExplorerNodesNavigatorForm) and
      ((Screen.Forms[I] as TExplorerNodesNavigatorForm).ExplorerNodes = Component) then
    begin
      Result := TExplorerNodesNavigatorForm(Screen.Forms[I]);
      Exit;
    end;
  Result := nil;
end;

procedure ShowExplorerForm(Designer: TFormDesigner; Component: TComponent);
var
  Form: TExplorerNodesNavigatorForm;
begin
  Form := FindExplorerForm(Component);

  if not Assigned(Form) then
  begin
    Form := TExplorerNodesNavigatorForm.Create(Application);
    try
      Form.Designer := Designer;
      Form.ExplorerNodes := Component as TExplorerNodes;
    except
      Form.Free;
      raise;
    end;
  end;
  Form.Show;
  if Form.WindowState = wsMinimized then
    Form.WindowState := wsNormal;
end;

procedure CreateExplorerNodes(ExplorerRoot: TExplorerNodes; Designer: TFormDesigner);
var
  Nodes: TExplorerNodes;
  NodesClass: TExplorerNodesClass;
begin
  NodesClass := GetExplorerNodesClass;
  if Assigned(NodesClass) then
  begin
    Nodes := TExplorerNodes(Designer.CreateComponent(NodesClass, Designer.Form, 0, 0, 0, 0));
    try
      Nodes.DesignInfo := 0;
      Nodes.Parent := TExplorerNodes(ExplorerRoot);
      Designer.Modified; 
    except
      Nodes.Free;
      raise;
    end;
  end;
end;

procedure TExplorerNodesEditorForm.GetVerb(Index: Integer; var Caption: TCaption; var ShortCut: TShortCut);
begin
  case Index of
    0:
      begin
        Caption := '&New...';
        ShortCut := Menus.ShortCut(ord('N'), [ssCtrl]);
      end;
  else
    inherited GetVerb(Index - 1, Caption, ShortCut);
  end;
end;

function TExplorerNodesEditorForm.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

procedure TExplorerNodesEditorForm.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: CreateExplorerNodes(ItemList as TExplorerNodes, Designer);
  else
    inherited ExecuteVerb(Index - 1);
  end;
end;

{ TExplorerNodesEditor }
function TExplorerNodesEditor.GetVerb(Index: Integer): string;
begin
  if Index > 0 then
    Result := inherited GetVerb(Index - 1) else
    Result := 'Explore...';
end;

function TExplorerNodesEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

procedure TExplorerNodesEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowExplorerForm(Self.Designer, Component);
  else
    inherited ExecuteVerb(Index - 1);
  end;
end;

function TExplorerNodesEditor.GetFormClass: TItemsEditorFormClass;
begin
  Result := TExplorerNodesEditorForm;
end;

end.
