{ ***** Registered Components Declaration ***** }
{ if You have already installed components of such names }
{ or just You don't like names given by me }
{ You can change these classnames of registered components }
{ if so then You must make appropriate changes in AdvTreeViews_Reg }
{ ********************************************* }

unit AdvTreeViews_RegDeclar;

interface

uses
  gmTreeView;

type
  TAdvTreeView = class(TAdvCustomTreeView)
  public
  { Public declarations }
  published
  { Published declarations }
    property PopupMenu;
    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
    property Items;
  end;

  TAdvDBTreeView = class(TAdvDBCustomTreeView)
  public
  { Public declarations }
    property KeyValue;
  published
  { Published declarations }
    property DataLinkParams;
    property OnPostInsertedNode;
    property OnDragDropForeignObject;
  end;

implementation


end.
