////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Unit Name   : acSkinMenu
//  * Purpose     : Automatic creation of menus for a skin selection
//  * Author      : Vasiliy (VahaC) Chepil
//  * Copyright   : © VahaC 2011.
//  * Version     : 1.04
//  * Home Page   : http://vahac.com.ua
//  ****************************************************************************

unit aceSkinMenu;
{$I sDefs.inc}

interface

uses
  SysUtils, Classes, Forms, Menus, acntUtils, sSkinManager, sSkinProps, Dialogs;

const
  Ver = '1.05';
  Aut = 'VahaC';
  Web = 'http://www.vahac.com';

  sSkinsMenuItem = 'acsmMenuItem_';
  sSkinsItnMenuItem = 'acsmIntMenuItem_';
  sSkinsExtMenuItem = 'acsmExtMenuItem_';

  sSkinsPopupMenu = 'acsmPopupMenu_';
  sSkinsItnPopupMenu = 'acsmIntPopupMenu_';
  sSkinsExtPopupMenu = 'acsmExtPopupMenu_';


type
  TItmInCol = 1..100;

  TOnMenuItemClickEvent = procedure (Sender: TMenuItem; const SkinName: string) of object;
  TOnRefreshSkinMenus = TNotifyEvent;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TacSkinMenu = class(TComponent)
  private
    { Private declarations }
    FAuthor: string;
    FVersion: String;
    FExtSkinMenuItem: TMenuItem;
    FIntSkinMenuItem: TMenuItem;
    FSkinMenuItem: TMenuItem;
    FSkinManager: TsSkinManager;
    FItemsInColumn: TItmInCol;
    FOnMenuItemClick: TOnMenuItemClickEvent;
    FOnRefreshSkinMenus: TOnRefreshSkinMenus;
    FWebSite: String;
    FDesignTimePreview: Boolean;
    FSkinMenu: TPopupMenu;
    FExtSkinMenu: TPopupMenu;
    FIntSkinMenu: TPopupMenu;
    procedure SetExtSkinMenuItem(const Value: TMenuItem);
    procedure SetIntSkinMenuItem(const Value: TMenuItem);
    procedure SetSkinMenuItem(const Value: TMenuItem);
    procedure SetExtSkinMenu(const Value: TPopupMenu);
    procedure SetIntSkinMenu(const Value: TPopupMenu);
    procedure SetSkinMenu(const Value: TPopupMenu);
    procedure SetSkinManager(const Value: TsSkinManager);
    procedure FillExtSkinMenuItem(parentMI: TMenuItem);
    procedure FillIntSkinMenuItem(parentMI: TMenuItem);
    procedure FillSkinMenuItem(parentMI: TMenuItem);
    procedure FillExtSkinPopupMenu(pm: TPopupMenu);
    procedure FillIntSkinPopupMenu(pm: TPopupMenu);
    procedure FillSkinPopupMenu(pm: TPopupMenu);
    procedure SetItemsInColumn(const Value: TItmInCol);
    procedure SetDesignTimePreview(const Value: Boolean);
  protected
    { Protected declarations }
    FOriginal_SkinManagerAfterChange: TNotifyEvent;
    FOriginal_SkinManagerSkinbListChanged: TNotifyEvent;
    procedure MenuItemClick(Sender: TObject);
    procedure SkinManagerAfterChange(Sender: TObject);
    procedure SkinManagerSkinbListChanged(Sender: TObject);
    procedure DeleteMenuItems(menuitem: TMenuItem; const prefix: String); overload;
    procedure DeleteMenuItems(pm: TPopupMenu; const prefix: String); overload;
    procedure DoDivideByColumns(parentMI: TMenuItem); overload;
    procedure DoDivideByColumns(pm: TPopupMenu); overload;
    procedure DoReloadAllSkinMenus;
    procedure DoRefreshSkinMenus;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Author: String read FAuthor;
    property Version: String read FVersion;
    property WebSite: String read FWebSite;
    property DesignTimePreview: Boolean read FDesignTimePreview write SetDesignTimePreview default False;
    property ExtSkinMenuItem: TMenuItem read FExtSkinMenuItem write SetExtSkinMenuItem;
    property IntSkinMenuItem: TMenuItem read FIntSkinMenuItem write SetIntSkinMenuItem;
    property SkinMenuItem: TMenuItem read FSkinMenuItem write SetSkinMenuItem;
    property ExtSkinMenu: TPopupMenu read FExtSkinMenu write SetExtSkinMenu;
    property IntSkinMenu: TPopupMenu read FIntSkinMenu write SetIntSkinMenu;
    property SkinMenu: TPopupMenu read FSkinMenu write SetSkinMenu;
    property SkinManager: TsSkinManager read FSkinManager write SetSkinManager;
    property ItemsInColumn: TItmInCol read FItemsInColumn write SetItemsInColumn;
    property OnMenuItemClick: TOnMenuItemClickEvent read FOnMenuItemClick write FOnMenuItemClick;
    property OnRefreshSkinMenus: TOnRefreshSkinMenus read FOnRefreshSkinMenus write FOnRefreshSkinMenus;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('acExtra', [TacSkinMenu]);
end;

{ TacSkinMenu }

constructor TacSkinMenu.Create(AOwner: TComponent);
begin
  inherited;
  FVersion := Ver;
  FAuthor := Aut;
  FWebSite := Web;
  FDesignTimePreview := False;
  if FItemsInColumn = 0 then
    FItemsInColumn := 15;
end;

procedure TacSkinMenu.DeleteMenuItems(menuitem: TMenuItem;
  const prefix: String);
var
  I: Integer;
begin
  for I := menuitem.Count - 1 downto 0 do
    if Copy(menuitem.Items[i].Name, 1, Length(prefix)) = prefix then
    begin
      menuitem.Items[i].Free;
    end;
end;

procedure TacSkinMenu.DeleteMenuItems(pm: TPopupMenu; const prefix: String);
var
  I: Integer;
begin
  for I := pm.Items.Count - 1 downto 0 do
    if Copy(pm.Items.Items[i].Name, 1, Length(prefix)) = prefix then
    begin
      pm.Items.Items[i].Free;
    end;
end;

procedure TacSkinMenu.DoDivideByColumns(parentMI: TMenuItem);
var
  I: Integer;
begin
  for I := 0 to parentMI.Count - 1 do
  begin
    if parentMI.Items[I].Break = mbBreak then
      parentMI.Items[I].Break := mbNone;
    if (parentMI.Count <> 0) and (I mod FItemsInColumn = 0) then
      parentMI.Items[I].Break := mbBreak;
  end;
end;

procedure TacSkinMenu.DoDivideByColumns(pm: TPopupMenu);
var
  I: Integer;
begin
  for I := 0 to pm.Items.Count - 1 do
  begin
    if pm.Items.Items[I].Break = mbBreak then
      pm.Items.Items[I].Break := mbNone;
    if (pm.Items.Count <> 0) and (I mod FItemsInColumn = 0) then
      pm.Items.Items[I].Break := mbBreak;
  end;
end;

procedure TacSkinMenu.DoRefreshSkinMenus;
begin
  DoReloadAllSkinMenus;

  if Assigned(FOnRefreshSkinMenus) then
    FOnRefreshSkinMenus(Self);
end;

procedure TacSkinMenu.DoReloadAllSkinMenus;
begin
  FillExtSkinMenuItem(FExtSkinMenuItem);
  FillIntSkinMenuItem(FIntSkinMenuItem);
  FillSkinMenuItem(FSkinMenuItem);
  FillExtSkinPopupMenu(FExtSkinMenu);
  FillIntSkinPopupMenu(FIntSkinMenu);
  FillSkinPopupMenu(FSkinMenu);
end;

procedure TacSkinMenu.FillExtSkinPopupMenu(pm: TPopupMenu);
var
  i: Integer;
  mi: TMenuItem;
  sl: TStringList;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(pm) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(pm, sSkinsExtPopupMenu);
    sl := TStringList.Create;
    try
      FSkinManager.GetExternalSkinNames(sl);
      for i := 0 to sl.Count - 1 do
      begin
        mi := TMenuItem.Create(pm);
        mi.Name := sSkinsExtPopupMenu + IntToStr(i);
        mi.Caption := sl[i];
        mi.Checked := mi.Caption = FSkinManager.SkinName;
        mi.OnClick := MenuItemClick;
        mi.RadioItem := True;
        pm.Items.Add(mi);
      end;
    finally
      FreeAndNil(sl);
    end;
    DoDivideByColumns(pm);
  end;
end;

procedure TacSkinMenu.FillExtSkinMenuItem(parentMI: TMenuItem);
var
  i: Integer;
  mi: TMenuItem;
  sl: TStringList;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(parentMI) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(parentMI, sSkinsExtMenuItem);
    sl := TStringList.Create;
    try
      FSkinManager.GetExternalSkinNames(sl);
      parentMI.Enabled := sl.Count > 0;
      for i := 0 to sl.Count - 1 do
      begin
        mi := TMenuItem.Create(parentMI);
        mi.Name := sSkinsExtMenuItem + IntToStr(i);
        mi.Caption := sl[i];
        mi.Checked := mi.Caption = FSkinManager.SkinName;
        mi.OnClick := MenuItemClick;
        mi.RadioItem := True;
        parentMI.Add(mi);
      end;
    finally
      FreeAndNil(sl);
    end;
    DoDivideByColumns(parentMI);
  end;
end;

procedure TacSkinMenu.FillIntSkinPopupMenu(pm: TPopupMenu);
var
  i: Integer;
  mi: TMenuItem;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(pm) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(pm, sSkinsItnPopupMenu);
    for i := 0 to FSkinManager.InternalSkins.Count - 1 do
    begin
      mi := TMenuItem.Create(pm);
      mi.Name := sSkinsItnPopupMenu + IntToStr(i);
      mi.Caption := FSkinManager.InternalSkins[i].Name;
      mi.Checked := mi.Caption = FSkinManager.SkinName;
      mi.OnClick := MenuItemClick;
      mi.RadioItem := True;
      pm.Items.Add(mi);
    end;
    DoDivideByColumns(pm);
  end;
end;

procedure TacSkinMenu.FillIntSkinMenuItem(parentMI: TMenuItem);
var
  i: Integer;
  mi: TMenuItem;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(parentMI) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(parentMI, sSkinsItnMenuItem);
    parentMI.Enabled := FSkinManager.InternalSkins.Count > 0;
    for i := 0 to FSkinManager.InternalSkins.Count - 1 do
    begin
      mi := TMenuItem.Create(parentMI);
      mi.Name := sSkinsItnMenuItem + IntToStr(i);
      mi.Caption := FSkinManager.InternalSkins[i].Name;
      mi.Checked := mi.Caption = FSkinManager.SkinName;
      mi.OnClick := MenuItemClick;
      mi.RadioItem := True;
      parentMI.Add(mi);
    end;
    DoDivideByColumns(parentMI);
  end;
end;

procedure TacSkinMenu.FillSkinPopupMenu(pm: TPopupMenu);
var
  i: Integer;
  mi: TMenuItem;
  sl: TStringList;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(pm) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(pm, sSkinsPopupMenu);
    sl := TStringList.Create;
    try
      FSkinManager.GetSkinNames(sl, False, stAllSkins);
      for i := 0 to sl.Count - 1 do
      begin
        mi := TMenuItem.Create(pm);
        mi.Name := sSkinsPopupMenu + IntToStr(i);
        mi.Caption := sl[i];
        mi.Checked := mi.Caption = FSkinManager.SkinName;
        mi.OnClick := MenuItemClick;
        mi.RadioItem := True;
        pm.Items.Add(mi);
      end;
    finally
      FreeAndNil(sl);
    end;
    DoDivideByColumns(pm);
  end;
end;

procedure TacSkinMenu.FillSkinMenuItem(parentMI: TMenuItem);
var
  i: Integer;
  mi: TMenuItem;
  sl: TStringList;
begin
  if (csDesigning in ComponentState) and (not FDesignTimePreview) then
    Exit;
  if Assigned(parentMI) and Assigned(FSkinManager) then
  begin
    DeleteMenuItems(parentMI, sSkinsMenuItem);
    sl := TStringList.Create;
    try
      FSkinManager.GetSkinNames(sl, False, stAllSkins);
      parentMI.Enabled := sl.Count > 0;
      for i := 0 to sl.Count - 1 do
      begin
        mi := TMenuItem.Create(parentMI);
        mi.Name := sSkinsMenuItem + IntToStr(i);
        mi.Caption := sl[i];
        mi.Checked := mi.Caption = FSkinManager.SkinName;
        mi.OnClick := MenuItemClick;
        mi.RadioItem := True;
        parentMI.Add(mi);
      end;
    finally
      FreeAndNil(sl);
    end;
    DoDivideByColumns(parentMI);
  end;
end;

procedure TacSkinMenu.MenuItemClick(Sender: TObject);
begin
  FSkinManager.SkinName := DelChars(TMenuItem(Sender).Caption, '&');
  TMenuItem(Sender).Checked := true;
  if not FSkinManager.Active then
    FSkinManager.Active := True;


  if Assigned(FOnMenuItemClick) then
    FOnMenuItemClick(TMenuItem(Sender), DelChars(TMenuItem(Sender).Caption, '&'));
end;

procedure TacSkinMenu.SetDesignTimePreview(const Value: Boolean);
begin
  FDesignTimePreview := Value;
  SetExtSkinMenuItem(FExtSkinMenuItem);
  SetIntSkinMenuItem(FIntSkinMenuItem);
  SetSkinMenuItem(FSkinMenuItem);
end;

procedure TacSkinMenu.SetExtSkinMenu(const Value: TPopupMenu);
begin
  if FExtSkinMenu <> nil then
  begin
    DeleteMenuItems(FExtSkinMenu, sSkinsExtPopupMenu);
    if FExtSkinMenu.Items.Count > 0 then
      DoDivideByColumns(FExtSkinMenu);
  end;
  FExtSkinMenu := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillExtSkinPopupMenu(Value);


    DoDivideByColumns(FExtSkinMenu);
  end;
end;

procedure TacSkinMenu.SetExtSkinMenuItem(const Value: TMenuItem);
begin
  if FExtSkinMenuItem <> nil then
  begin
    DeleteMenuItems(FExtSkinMenuItem, sSkinsExtMenuItem);
    FExtSkinMenuItem.Enabled := True;
    if FExtSkinMenuItem.Count > 0 then
      DoDivideByColumns(FExtSkinMenuItem);
  end;
  FExtSkinMenuItem := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillExtSkinMenuItem(value);


    DoDivideByColumns(FExtSkinMenuItem);
  end;
end;

procedure TacSkinMenu.SetIntSkinMenu(const Value: TPopupMenu);
begin
  if FIntSkinMenu <> nil then
  begin
    DeleteMenuItems(FIntSkinMenu, sSkinsItnPopupMenu);
    if FIntSkinMenu.Items.Count > 0 then
      DoDivideByColumns(FIntSkinMenu);
  end;
  FIntSkinMenu := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillIntSkinPopupMenu(value);


    DoDivideByColumns(FIntSkinMenu);
  end;
end;

procedure TacSkinMenu.SetIntSkinMenuItem(const Value: TMenuItem);
begin
  if FIntSkinMenuItem <> nil then
  begin
    DeleteMenuItems(FIntSkinMenuItem, sSkinsItnMenuItem);
    FIntSkinMenuItem.Enabled := True;
    if FIntSkinMenuItem.Count > 0 then
      DoDivideByColumns(FIntSkinMenuItem);
  end;
  FIntSkinMenuItem := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillIntSkinMenuItem(value);


    DoDivideByColumns(FIntSkinMenuItem);
  end;
end;

procedure TacSkinMenu.SetSkinMenu(const Value: TPopupMenu);
begin
  if FSkinMenu <> nil then
  begin
    DeleteMenuItems(FSkinMenu, sSkinsPopupMenu);
    if FSkinMenu.Items.Count > 0 then
      DoDivideByColumns(FSkinMenu);
  end;
  FSkinMenu := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillSkinPopupMenu(Value);
  end;
end;

procedure TacSkinMenu.SetSkinMenuItem(const Value: TMenuItem);
begin
  if FSkinMenuItem <> nil then
  begin
    DeleteMenuItems(FSkinMenuItem, sSkinsMenuItem);
    FSkinMenuItem.Enabled := True;
    if FSkinMenuItem.Count > 0 then
      DoDivideByColumns(FSkinMenuItem);
  end;
  FSkinMenuItem := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(Self);
    FillSkinMenuItem(Value);
  end;
end;

procedure TacSkinMenu.SetItemsInColumn(const Value: TItmInCol);
begin
  if FItemsInColumn <> Value then
  begin
    FItemsInColumn := Value;
    DoRefreshSkinMenus;
  end;
end;

procedure TacSkinMenu.SetSkinManager(const Value: TsSkinManager);
begin
  if Assigned(FSkinManager) then
    FSkinManager.OnAfterChange := FOriginal_SkinManagerAfterChange;
  FSkinManager := Value;
  if Value <> nil then
  begin
    FOriginal_SkinManagerAfterChange := FSkinManager.OnAfterChange;
    FSkinManager.OnAfterChange := SkinManagerAfterChange;
    FOriginal_SkinManagerSkinbListChanged := FSkinManager.OnSkinListChanged;
    FSkinManager.OnSkinListChanged := SkinManagerSkinbListChanged;
  end;
  DoRefreshSkinMenus;
end;

procedure TacSkinMenu.SkinManagerAfterChange(Sender: TObject);
var
  i : integer;
  b : boolean;
begin
  i := FSkinManager.GetSkinIndex(s_Form);
  if FSkinManager.IsValidSkinIndex(i) then
  begin
    b := True;
    if FIntSkinMenuItem <> nil then
      for i := 0 to FIntSkinMenuItem.Count - 1 do
        if b and (DelChars(FIntSkinMenuItem.Items[i].Caption, '&') = FSkinManager.SkinName) then
        begin
          FIntSkinMenuItem.Items[i].Checked := True;
          b := False;
        end
        else
          FIntSkinMenuItem.Items[i].Checked := False;
    if FExtSkinMenuItem <> nil then
      for i := 0 to FExtSkinMenuItem.Count - 1 do
        if b and (DelChars(FExtSkinMenuItem.Items[i].Caption, '&') = FSkinManager.SkinName) then
        begin
          FExtSkinMenuItem.Items[i].Checked := True;
          b := False;
        end
        else
          FExtSkinMenuItem.Items[i].Checked := False;
    if FSkinMenuItem <> nil then
      for i := 0 to FSkinMenuItem.Count - 1 do
        FSkinMenuItem.Items[i].Checked := DelChars(FSkinMenuItem.Items[i].Caption, '&') = FSkinManager.SkinName;
  end;
  if Assigned(FOriginal_SkinManagerAfterChange) then
    FOriginal_SkinManagerAfterChange(FSkinManager);
end;

procedure TacSkinMenu.SkinManagerSkinbListChanged(Sender: TObject);
begin
  DoRefreshSkinMenus;

  if Assigned(FOriginal_SkinManagerSkinbListChanged) then
    FOriginal_SkinManagerSkinbListChanged(FSkinManager);
end;

end.

