
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2002 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit ActnPopupCtrl;

interface

uses Classes, ActnMan, Menus, ActnMenus, ActnList;

type
{ TCustomActionPopupMenuEx }

  TCustomActionPopupMenuEx = class(TCustomActionPopupMenu)
  protected
    procedure ExecAction(Action: TContainedAction); override;
  end;

{ TCustomPopupActionBarEx

  The menu will get its style from the ActionManager property if it is assigned,
  otherwise it will use the default (first registered) ActionBand style.  This
  component uses an internal ActionManager so that it can be used within
  form inheritance.

  *** NOTE ***
  For this menu to work correctly you MUST manually add one of the following
  units to your uses clause StdStyleActnCtrlsEx, XPStyleActnCtrlsEx or a
  similar third party style unit.

}

  TCustomPopupActionBarEx = class(TPopupMenu)
  private
    FPopupMenu: TCustomActionPopupMenu;
    FInternalActionManager: TCustomActionManager;
    FActionManager: TCustomActionManager;
    FShadows: Boolean;
    function GetMenuActive: Boolean;
    procedure SetActionManager(const Value: TCustomActionManager);
  protected
    procedure LoadMenu(Clients: TActionClients; AMenu: TMenuItem);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup(X: Integer; Y: Integer); override;
    procedure PopupHandler(Sender: TObject; Item: TCustomActionControl);
    property MenuActive: Boolean read GetMenuActive;
    property ActionManager: TCustomActionManager read FActionManager write SetActionManager;
    property PopupMenu: TCustomActionPopupMenu read FPopupMenu write FPopupMenu;
    property Shadows: Boolean read FShadows write FShadows default True;
  end;

  TPopupActionBarEx = class(TCustomPopupActionBarEx)
  published
    property ActionManager;
    property PopupMenu;
    property Shadows;
  end;

function NewPopupMenu(Owner: TComponent; const AName: string;
  Alignment: TPopupAlignment; AutoPopup: Boolean; const Items: array of TMenuItem): TPopupMenu;

implementation

uses SysUtils, Windows, Messages, Controls, Consts;

resourcestring
  sActionManagerNotAssigned = '%s ActionManager property has not been assigned';

procedure InitMenuItems(AMenu: TMenu; const Items: array of TMenuItem);
var
  I: Integer;

  procedure SetOwner(Item: TMenuItem);
  var
    I: Integer;
  begin
    if Item.Owner = nil then AMenu.Owner.InsertComponent(Item);
    for I := 0 to Item.Count - 1 do
      SetOwner(Item[I]);
  end;

begin
  for I := Low(Items) to High(Items) do
  begin
    SetOwner(Items[I]);
    AMenu.Items.Add(Items[I]);
  end;
end;

function NewPopupMenu(Owner: TComponent; const AName: string;
  Alignment: TPopupAlignment; AutoPopup: Boolean; const Items: array of TMenuItem): TPopupMenu;
begin
  Result := TPopupActionBarEx.Create(Owner);
  Result.Name := AName;
  Result.AutoPopup := AutoPopup;
  Result.Alignment := Alignment;
  InitMenuItems(Result, Items);
end;

type
  TMenuAction = class(TCustomAction)
  private
    FMenuItem: TMenuItem;
  public
    function Execute: Boolean; override;
    constructor Create(AOwner: TComponent; AMenuItem: TMenuItem); reintroduce;
  end;

{ TMenuAction }

constructor TMenuAction.Create(AOwner: TComponent; AMenuItem: TMenuItem);
begin
  inherited Create(AOwner);
  FMenuItem := AMenuItem;
end;

function TMenuAction.Execute: Boolean;
begin
  Result := True;
  if (FMenuItem <> nil) and Assigned(FMenuItem.OnClick) then
    FMenuItem.OnClick(FMenuItem);
end;

{ TCustomActionPopupMenuEx }

procedure TCustomActionPopupMenuEx.ExecAction(Action: TContainedAction);
begin
  PostMessage(PopupList.Window, WM_COMMAND, TMenuItem(FSelectedItem.Tag).Command, 0);
end;

{ TCustomPopupActionBarEx }

constructor TCustomPopupActionBarEx.Create(AOwner: TComponent);
begin
  inherited;
  FInternalActionManager := TActionManager.Create(Self);
  FShadows := True;
end;

function TCustomPopupActionBarEx.GetMenuActive: Boolean;
begin
  Result := Assigned(FPopupMenu);
end;

procedure TCustomPopupActionBarEx.LoadMenu(Clients: TActionClients;
  AMenu: TMenuItem);
var
  I: Integer;
  AC: TActionClientItem;
begin
  Clients.AutoHotKeys := False;
  for I := 0 to AMenu.Count - 1 do
  begin
    AC := Clients.Add;
    AC.Caption := AMenu.Items[I].Caption;
    AC.Tag := Integer(AMenu.Items[I]);
    AC.Action := TContainedAction(AMenu.Items[I].Action);
    if (AMenu.Items[I].Count > 0) and (AMenu.Items[I].Visible) then
      AC.Items.Add
    else
      if ((AMenu.Items[I].Caption <> '') and (AMenu.Items[I].Action = nil) and
          (AMenu.Items[I].Caption <> '-')) then
      begin
        AC.Action := TMenuAction.Create(Self, AMenu.Items[I]);
        AC.Action.ActionList := FInternalActionManager;
        AC.Action.Tag := AMenu.Items[I].Tag;
        TCustomAction(AC.Action).ImageIndex := AMenu.Items[I].ImageIndex;
        TCustomAction(AC.Action).Visible := AMenu.Items[I].Visible;
        TCustomAction(AC.Action).Checked := AMenu.Items[I].Checked;
        TCustomAction(AC.Action).Caption := AMenu.Items[I].Caption;
        TCustomAction(AC.Action).ShortCut := AMenu.Items[I].ShortCut;
        TCustomAction(AC.Action).Enabled := AMenu.Items[I].Enabled;
        AC.ImageIndex := AMenu.Items[I].ImageIndex;
        AC.ShortCut := AMenu.Items[I].ShortCut;
      end;
    AC.Caption := AMenu.Items[I].Caption;
    AC.ShortCut := AMenu.Items[I].ShortCut;
    AC.HelpContext := AMenu.Items[I].HelpContext;
    AC.ImageIndex := AMenu.Items[I].ImageIndex;
    AC.Visible := AMenu.Items[I].Visible;
  end;
end;

type
  TControlClass = class(TControl);

procedure TCustomPopupActionBarEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FActionManager) then
    FActionManager := nil;
end;

procedure TCustomPopupActionBarEx.Popup(X, Y: Integer);
var
  Style: TActionBarStyle;
begin
  // Dynamically build the ActionBand popup menu from the TPopupMenu
  if Assigned(FPopupMenu) then exit;
  DoPopup(Self);
  Style := ActionBarStyles.Style[0];
  if Assigned(FActionManager) then
    Style := FActionManager.Style;
  FPopupMenu := TActionBarStyleEx(Style).GetPopupClass(nil).Create(nil);
  try
    FPopupMenu.OnPopup := PopupHandler;
    FInternalActionManager.ActionBars.Clear;
    FPopupMenu.Designable := False;
    FPopupMenu.AnimationStyle := asNone;
    FInternalActionManager.Images := Images;
    FInternalActionManager.Style := Style;
    with FInternalActionManager.ActionBars.Add do
    begin
       ActionBar := FPopupMenu;
       ActionBar.Orientation := boTopToBottom;
       AutoSize := False;
    end;
    FPopupMenu.ColorMap := nil;
    TControlClass(FPopupMenu).AutoSize := True;
    if not Assigned(FPopupMenu.ActionClient) then
      raise Exception.CreateFmt(sActionManagerNotAssigned, [Name]);
    FPopupMenu.ActionClient.Items.Clear;
    FPopupMenu.Shadow := FShadows;
    LoadMenu(FPopupMenu.ActionClient.Items, Self.Items);
    FPopupMenu.RecreateControls;
    FPopupMenu.Popup(X, Y);
  finally
    FreeAndNil(FPopupMenu);
  end;
end;

procedure TCustomPopupActionBarEx.PopupHandler(Sender: TObject;
  Item: TCustomActionControl);
begin
  // Check to see if we've already built this submenu this time around
  if Item.ActionClient.Items[0].Tag <> 0 then exit;
  // If we haven't then call the actual TMenuItem's OnClick event (if assigned)
  // which causing any dynamically created menus to populate
  if Assigned(TMenuItem(Item.ActionClient.Tag).OnClick) then
    TMenuItem(Item.ActionClient.Tag).OnClick(TMenuItem(Item.ActionClient.Tag));
  // Since submenus are always recreated clear any old items...
  Item.ActionClient.Items.Clear;
  // ...and reload the actual TMenuItem
  LoadMenu(Item.ActionClient.Items, TMenuItem(Item.ActionClient.Tag));
end;

procedure TCustomPopupActionBarEx.SetActionManager(
  const Value: TCustomActionManager);
begin
  if FActionManager <> Value then
  begin
    if Assigned(FActionManager) then
      FActionManager.RemoveFreeNotification(Self);
    FActionManager := Value;
    if Assigned(FActionManager) then
      FActionManager.FreeNotification(Self);
  end;
end;

end.
