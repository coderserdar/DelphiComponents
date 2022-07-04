{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtMenuTreeView                                 }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.tk                }
{                                                       }
{                                                       }
{                                                       }
{*******************************************************}
unit o_MenuTreeView;

interface

uses
  SysUtils, Classes, Controls, ComCtrls,Menus;

type
  TgtMenuTreeView = class(TTreeView)
  private
    { Private declarations }
    FMapEvents         : Boolean;
    FSelectedMenuItem  : TMenuItem;
    FHideDividers      : Boolean;
    FMenu              : TMenu;
    FImageIndexForRootItems: Integer;
    procedure SetMapEvents   (const Value: Boolean);
    procedure SetHideDividers(const Value: Boolean);
  protected
    { Protected declarations }
    function  GetMenuItemByCaption(ACaption : String):TMenuItem;
    procedure InternalExpanding(Sender:TObject ; Node : TTreeNode ;  var AllowExpansion: Boolean);
    procedure InternalCollapsing(Sender:TObject ; Node : TTreeNode ; var AllowCollapse: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    procedure PopulateMenu(AMenu  : TMenu );
    procedure Click;override;
    property  SelectedMenuItem : TMenuItem read FSelectedMenuItem;
  published
    { Published declarations }
    property MapEvents        : Boolean read FMapEvents        write SetMapEvents    default True;
    property HideDividers     : Boolean read FHideDividers     write SetHideDividers default True;
    property ImageIndexForRootItems : Integer read FImageIndexForRootItems write FImageIndexForRootItems;
  end;


implementation



{ TgtMenuTreeView }
{------------------------------------------------------------------------------}
constructor TgtMenuTreeView.Create(AOwner: TComponent);
begin
  inherited;
  ReadOnly         := True;
  FHideDividers    := True;
  OnExpanding      := InternalExpanding;
  OnCollapsing     := InternalCollapsing;
  RowSelect        := True;
  HideSelection    := False;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.Click;
begin
  inherited;
  if FMapEvents then
    begin
      if Assigned(Selected) then
        begin
          if Assigned(Selected.Data) then
            TMenuItem(Selected.Data).Click;
        end;
    end;
end;
{------------------------------------------------------------------------------}
function TgtMenuTreeView.GetMenuItemByCaption(ACaption: String): TMenuItem;
var i : Integer;
begin
  Result := nil;
  for i:=0 to Pred(FMenu.ComponentCount) do
    begin
      if FMenu.Components[i] is TMenuItem then
        begin
          if TMenuItem(FMenu.Components[i]).Caption = ACaption then
            begin
              Result :=  TMenuItem(FMenu.Components[i]);
              Break;
            end;
        end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.PopulateMenu(AMenu  : TMenu );
var i    : Integer;
    Node : TTreeNode;
procedure PopulateItems(ParentNode : TTreeNode ; Item : TMenuItem);
var i    : Integer;
    Node : TTreeNode;
begin
  Node := nil;
  for i:=0 to Pred(Item.Count) do
    begin
      if FHideDividers then
        begin
          if ((Item.Items[i].Caption <> '-') and (Item.Items[i].Visible)) then
          begin
            Node           := Items.AddChild(ParentNode,Item.Items[i].Caption);
            Node.Data      := Item.Items[i];
            if  Item.Items[i].ImageIndex <> -1 then
            begin
              Node.ImageIndex    := Item.Items[i].ImageIndex;
              Node.StateIndex    := Node.ImageIndex;
              Node.SelectedIndex := Node.ImageIndex;
            end
            else
            begin
              Node.ImageIndex := FImageIndexForRootItems;
              Node.StateIndex := FImageIndexForRootItems;
              Node.SelectedIndex := FImageIndexForRootItems;
            end;            
          end;
        end
      else
        begin
            Node           := Items.AddChild(ParentNode,Item.Items[i].Caption);
            Node.Data      := Item.Items[i];
            if  Item.Items[i].ImageIndex <> -1 then
            begin
              Node.ImageIndex    := Item.Items[i].ImageIndex;
              Node.StateIndex    := Node.ImageIndex;
              Node.SelectedIndex := Node.ImageIndex;
            end
            else
            begin
              Node.ImageIndex := FImageIndexForRootItems;
              Node.StateIndex := FImageIndexForRootItems;
              Node.SelectedIndex :=FImageIndexForRootItems;
            end;
        end;
      PopulateItems(Node,Item.Items[i]);
    end;
end;
begin
  FMenu := AMenu;

  Images := FMenu.Images;
  Items.Clear;
  Node := nil;
  for i:=0 to Pred(AMenu.Items.Count) do
    begin
      if FHideDividers then
        begin
          if ((AMenu.Items[i].Caption<>'-') and (AMenu.Items[i].Visible))  then
          begin
            Node               := Items.Add(nil,AMenu.Items[i].Caption);
            Node.Data          := AMenu.Items[i];
            if  AMenu.Items[i].ImageIndex <> -1 then
            begin
              Node.ImageIndex    := AMenu.Items[i].ImageIndex;
              Node.StateIndex    := Node.ImageIndex;
              Node.SelectedIndex := Node.ImageIndex;
            end
            else
            begin
              Node.ImageIndex :=FImageIndexForRootItems;
              Node.StateIndex :=FImageIndexForRootItems;
              Node.SelectedIndex :=FImageIndexForRootItems;
            end;
          end;
        end
      else
        begin
            Node            := Items.Add(nil,AMenu.Items[i].Caption);
            Node.Data       := AMenu.Items[i];
            if  AMenu.Items[i].ImageIndex <> -1 then
            begin
              Node.ImageIndex    := AMenu.Items[i].ImageIndex;
              Node.StateIndex    := Node.ImageIndex;
              Node.SelectedIndex := Node.ImageIndex;
            end
            else
            begin
              Node.ImageIndex :=FImageIndexForRootItems;
              Node.StateIndex :=FImageIndexForRootItems;
              Node.SelectedIndex :=FImageIndexForRootItems;
            end;
        end;
      PopulateItems(Node,AMenu.Items[i]);
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.SetMapEvents(const Value: Boolean);
begin
  FMapEvents := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.SetHideDividers(const Value: Boolean);
begin
  FHideDividers := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.InternalExpanding(Sender: TObject; Node: TTreeNode;var AllowExpansion: Boolean);
begin
  Selected := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtMenuTreeView.InternalCollapsing(Sender: TObject;Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Selected := nil;
  inherited;
end;
{------------------------------------------------------------------------------}
end.
