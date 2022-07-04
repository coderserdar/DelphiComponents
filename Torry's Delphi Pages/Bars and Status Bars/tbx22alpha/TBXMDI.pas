unit TBXMDI;


// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// Parts of the code are converted from original Toolbar2000 sources.
//    Original Copyright:
//    Copyright (C) 1998-2004 by Jordan Russell. All rights reserved.
//
// $Id: TBXMDI.pas 99 2005-09-23 19:34:50Z Alex $


interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, TB2Item, TB2Toolbar, TBX;

type
  TTBXMDIButtonsItem = class;
  TTBXMDISystemMenuItem = class;
  TTBXMDIWindowItem = class;

  TTBXMDIHandler = class(TComponent)
  private
    FButtonsItem: TTBXMDIButtonsItem;
    FSystemMenuItem: TTBXMDISystemMenuItem;
    FToolbar: TTBCustomToolbar;
    FStretchButtons: Boolean;
    procedure SetToolbar(Value: TTBCustomToolbar);
    procedure SetStretchButtons(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Toolbar: TTBCustomToolbar read FToolbar write SetToolbar;
    property StretchButtons: Boolean read FStretchButtons write SetStretchButtons default False;
  end;

  TTBXMDIWindowItem = class(TTBXCustomItem)
  private
    FForm: TForm;
    FWindowMenu: TMenuItem;
    FOnUpdate: TNotifyEvent;
    procedure ItemClick(Sender: TObject);
    procedure SetForm(AForm: TForm);
  protected
    procedure EnabledChanged; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
  published
    property Enabled;
    property FontSettings;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

  TTBXMDISystemMenuItem = class(TTBXCustomItem)
  private
    FImageList: TImageList;
    procedure CommandClick(Sender: TObject);
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TTBXMDISystemMenuItemViewer = class(TTBItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  end;

  TTBXMDIButtonType = (tbmbMinimize, tbmbRestore, tbmbClose);

  TTBXMDIButtonItem = class(TTBCustomItem)
  private
    FButtonType: TTBXMDIButtonType;
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TTBXMDIButtonItemViewer = class(TTBItemViewer)
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
  end;

  TTBXMDISepItem = class(TTBSeparatorItem)
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TTBXMDISepItemViewer = class(TTBSeparatorItemViewer)
  protected
    procedure CalcSize (const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
  end;

  TTBXMDIButtonsItem = class(TTBCustomItem)
  private
    FMinimizeItem: TTBXMDIButtonItem;
    FRestoreItem: TTBXMDIButtonItem;
    FCloseItem: TTBXMDIButtonItem;
    FSep1, FSep2: TTBXMDISepItem;
    procedure InvalidateSystemMenuItem;
    procedure ItemClick(Sender: TObject);
    procedure UpdateState(W: HWND; Maximized: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R TBX_MDI.res}

uses
  TBXThemes, TB2Common, TB2Consts, CommCtrl;

type
  TTBViewAccess = class(TTBView);
  TTBCustomToolbarAccess = class(TTBCustomToolbar);


//----------------------------------------------------------------------------//

{ TTBXMDIHandler }

constructor TTBXMDIHandler.Create (AOwner: TComponent);
begin
  inherited;
  FSystemMenuItem := TTBXMDISystemMenuItem.Create(Self);
  FButtonsItem := TTBXMDIButtonsItem.Create(Self);
end;

destructor TTBXMDIHandler.Destroy;
begin
  Toolbar := nil;
  inherited;
end;

procedure TTBXMDIHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FToolbar) and (Operation = opRemove) then Toolbar := nil;
end;

procedure TTBXMDIHandler.SetStretchButtons(Value: Boolean);
begin
  if Value <> FStretchButtons then
  begin
    FStretchButtons := Value;
    with FButtonsItem do
      if FStretchButtons then
      begin
        FMinimizeItem.ItemStyle := FMinimizeItem.ItemStyle + [tbisStretch];
        FRestoreItem.ItemStyle := FRestoreItem.ItemStyle + [tbisStretch];
        FCloseItem.ItemStyle := FCloseItem.ItemStyle + [tbisStretch];
      end
      else
      begin
        FMinimizeItem.ItemStyle := FMinimizeItem.ItemStyle - [tbisStretch];
        FRestoreItem.ItemStyle := FRestoreItem.ItemStyle - [tbisStretch];
        FCloseItem.ItemStyle := FCloseItem.ItemStyle - [tbisStretch];
      end;
  end;
end;

procedure TTBXMDIHandler.SetToolbar(Value: TTBCustomToolbar);
var
  Rebuild: Boolean;
begin
  if FToolbar <> Value then
  begin
    if Assigned(FToolbar) then with TTBCustomToolbarAccess(FToolbar) do
    begin
      Rebuild := False;
      if FMDIButtonsItem = FButtonsItem then
      begin
        FMDIButtonsItem := nil;
        Rebuild := True;
      end;
      if FMDISystemMenuItem = FSystemMenuItem then
      begin
        FMDISystemMenuItem := nil;
        Rebuild := True;
      end;
      if Rebuild and Assigned(View) then View.RecreateAllViewers;
    end;
    FToolbar := Value;
    if Assigned(Value) then with TTBCustomToolbarAccess(Value) do
    begin
      FreeNotification(Self);
      FMDIButtonsItem := FButtonsItem;
      FMDISystemMenuItem := FSystemMenuItem;
      View.RecreateAllViewers;
    end;
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXMDISystemMenuItem }

constructor TTBXMDISystemMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisDontSelectFirst] -
    [tbisRedrawOnSelChange, tbisRedrawOnMouseOverChange];
  Caption := '&-';

  FImageList := TImageList.Create(Self);
  FImageList.Handle := ImageList_LoadBitmap(HInstance, 'TBXSYSMENUIMAGES',
    16, 0, $C0C0C0);
  SubMenuImages := FImageList;
end;

function TTBXMDISystemMenuItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXMDISystemMenuItemViewer;
end;

procedure TTBXMDISystemMenuItem.Click;
var
  I: Integer;
  Form: TForm;
  M: HMENU;
  State, ID: UINT;
  Item: TTBCustomItem;
  Buf: array[0..1024] of WideChar;
begin
  inherited;
  Clear;
  if Application.MainForm = nil then Exit;
  Form := Application.MainForm.ActiveMDIChild;
  if Form = nil then Exit;
  M := GetSystemMenu(Form.Handle, False);
  for I := 0 to GetMenuItemCount(M) - 1 do
  begin
    State := GetMenuState(M, I, MF_BYPOSITION);
    if State and MF_SEPARATOR <> 0 then Add(TTBXSeparatorItem.Create(Self))
    else
    begin
      Item := TTBXCustomItem.Create(Self);
      if State and MF_GRAYED <> 0 then Item.Enabled := False;
      if GetMenuStringW(M, I, Buf, 1024, MF_BYPOSITION) = 0 then Buf[0] := #0;
      Item.Caption := Buf;
      ID := GetMenuItemID(M, I);
      Item.Tag := ID;
      case ID and $FFF0 of
        SC_RESTORE: Item.ImageIndex := 3;
        SC_MINIMIZE: Item.ImageIndex := 2;
        SC_MAXIMIZE: Item.ImageIndex := 1;
        SC_CLOSE: begin
            Item.ImageIndex := 0;
            Item.Options := Item.Options + [tboDefault];
          end;
      end;
      Item.OnClick := CommandClick;
      Add(Item);
    end;
  end;
end;

procedure TTBXMDISystemMenuItem.CommandClick (Sender: TObject);
var
  Form: TForm;
begin
  if Assigned(Application.MainForm) then
  begin
    Form := Application.MainForm.ActiveMDIChild;
    if Assigned(Form) then
      SendMessage(Form.Handle, WM_SYSCOMMAND, TTBXCustomItem(Sender).Tag, GetMessagePos);
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXMDISystemMenuItemViewer }

procedure TTBXMDISystemMenuItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
begin
  AWidth := GetSystemMetrics(SM_CXSMICON) + 2;
  AHeight := GetSystemMetrics(SM_CYSMICON) + 2;
end;

procedure TTBXMDISystemMenuItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);

  function GetIconHandle: HICON;
  var
    Form: TForm;
  begin
    Result := 0;
    if Assigned(Application.MainForm) then
    begin
      Form := Application.MainForm.ActiveMDIChild;
      if Assigned(Form) then Result := Form.Icon.Handle;
    end;
    if Result = 0 then Result := Application.Icon.Handle;
    if Result = 0 then Result := LoadIcon(0, IDI_APPLICATION);
  end;

var
  R: TRect;
  IconHandle, TempIcon: HICON;
begin
  R := ClientAreaRect;
  InflateRect(R, -1, -1);
  IconHandle := GetIconHandle;
  TempIcon := CopyImage(IconHandle, IMAGE_ICON, R.Right - R.Left,
    R.Bottom - R.Top, LR_COPYFROMRESOURCE);
  if TempIcon <> 0 then
  begin
    DrawIconEx(Canvas.Handle, R.Left, R.Top, TempIcon, 0, 0, 0, 0, DI_NORMAL);
    DestroyIcon(TempIcon);
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXMDIButtonItem }

constructor TTBXMDIButtonItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle - [tbisSelectable] + [tbisRightAlign];
end;

function TTBXMDIButtonItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXMDIButtonItemViewer;
end;


//----------------------------------------------------------------------------//

{ TTBXMDIButtonItemViewer }

procedure TTBXMDIButtonItemViewer.CalcSize (const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
begin
  if NewStyleControls then
  begin
    AWidth := GetSystemMetrics(SM_CXMENUSIZE) - CurrentTheme.MenuMDIDW;
    if AWidth < 0 then AWidth := 0;
    AHeight := GetSystemMetrics(SM_CYMENUSIZE) - CurrentTheme.MenuMDIDH;
    if AHeight < 0 then AHeight := 0;
  end
  else
  begin
    AWidth := 16;
    AHeight := 14;
  end;
end;

procedure TTBXMDIButtonItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
const
  ButtonTypeFlags: array [TTBXMDIButtonType] of UINT = (DFCS_CAPTIONMIN,
    DFCS_CAPTIONRESTORE, DFCS_CAPTIONCLOSE);
  CDesigning: array [Boolean] of Integer = (0, IO_DESIGNING);
var
  ItemInfo: TTBXItemInfo;
begin
  FillChar(ItemInfo, SizeOf(ItemInfo), 0);
  ItemInfo.ViewType := VT_NORMALTOOLBAR; //GetViewType(View);
  ItemInfo.ItemOptions := IO_TOOLBARSTYLE or CDesigning[csDesigning in Item.ComponentState];
  ItemInfo.Enabled := Item.Enabled{ or View.Customizing};
  ItemInfo.Pushed := IsPushed;
  ItemInfo.Selected := False;
  ItemInfo.ImageShown := False;
  ItemInfo.ImageWidth := 0;
  ItemInfo.ImageHeight := 0;
  if IsSelected then
  begin
    if not ItemInfo.Enabled and not TTBViewAccess(View).MouseOverSelected then ItemInfo.HoverKind := hkKeyboardHover
    else if ItemInfo.Enabled then ItemInfo.HoverKind := hkMouseHover;
  end
  else ItemInfo.HoverKind := hkNone;
  ItemInfo.IsVertical := View.Orientation = tbvoVertical;

  CurrentTheme.PaintMDIButton(Canvas.Handle, ClientAreaRect, ItemInfo,
    ButtonTypeFlags[TTBXMDIButtonItem(Item).FButtonType]);
end;


//----------------------------------------------------------------------------//

{ TTBXMDISepItem }

function TTBXMDISepItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXMDISepItemViewer;
end;


//----------------------------------------------------------------------------//

{ TTBXMDISepItemViewer }

procedure TTBXMDISepItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
begin
  if View.Orientation <> tbvoVertical then
  begin
    AWidth := 2;
    AHeight := 6;
  end
  else
  begin
    AWidth := 6;
    AHeight := 2;
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXMDIButtonsItem }

var
  CBTHookHandle: HHOOK;
  MDIButtonsItems: TList;

function WindowIsMDIChild(W: HWND): Boolean;
var
  I: Integer;
  MainForm, ChildForm: TForm;
begin
  MainForm := Application.MainForm;
  if Assigned(MainForm) then
    for I := 0 to MainForm.MDIChildCount-1 do
    begin
      ChildForm := MainForm.MDIChildren[I];
      if ChildForm.HandleAllocated and (ChildForm.Handle = W) then
      begin
        Result := True;
        Exit;
      end;
    end;
  Result := False;
end;

function CBTHook(Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  Maximizing: Boolean;
  WindowPlacement: TWindowPlacement;
  I: Integer;
begin
  case Code of
    HCBT_SETFOCUS:
      if WindowIsMDIChild(HWND(WParam)) and Assigned(MDIButtonsItems) then
        for I := 0 to MDIButtonsItems.Count-1 do
          TTBXMDIButtonsItem(MDIButtonsItems[I]).InvalidateSystemMenuItem;
    HCBT_MINMAX:
      if WindowIsMDIChild(HWND(WParam)) and Assigned(MDIButtonsItems) and
         (LParam in [SW_SHOWNORMAL, SW_SHOWMAXIMIZED, SW_MINIMIZE, SW_RESTORE]) then
      begin
        Maximizing := (LParam = SW_MAXIMIZE);
        if (LParam = SW_RESTORE) and not IsZoomed(HWND(WParam)) then
        begin
          WindowPlacement.length := SizeOf(WindowPlacement);
          GetWindowPlacement (HWND(WParam), @WindowPlacement);
          Maximizing := (WindowPlacement.flags and WPF_RESTORETOMAXIMIZED <> 0);
        end;
        for I := 0 to MDIButtonsItems.Count-1 do
          TTBXMDIButtonsItem(MDIButtonsItems[I]).UpdateState(HWND(WParam), Maximizing);
      end;
    HCBT_DESTROYWND:
      if WindowIsMDIChild(HWND(WParam)) and Assigned(MDIButtonsItems) then
      begin
        for I := 0 to MDIButtonsItems.Count-1 do
          TTBXMDIButtonsItem(MDIButtonsItems[I]).UpdateState(HWND(WParam), False);
      end;
  end;
  Result := CallNextHookEx(CBTHookHandle, Code, WParam, LParam);
end;

constructor TTBXMDIButtonsItem.Create(AOwner: TComponent);

  function CreateItem(const AType: TTBXMDIButtonType): TTBXMDIButtonItem;
  begin
    Result := TTBXMDIButtonItem.Create(Self);
    Result.FButtonType := AType;
    Result.OnClick := ItemClick;
  end;

begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  FMinimizeItem := CreateItem(tbmbMinimize);
  FRestoreItem := CreateItem(tbmbRestore);
  FCloseItem := CreateItem(tbmbClose);
  FSep1 := TTBXMDISepItem.Create(Self);
  FSep1.Blank := True;
  FSep1.ItemStyle := FSep1.ItemStyle + [tbisRightAlign, tbisNoLineBreak];
  FSep2 := TTBXMDISepItem.Create(Self);
  FSep2.Blank := True;
  FSep2.ItemStyle := FSep2.ItemStyle + [tbisRightAlign, tbisNoLineBreak];
  Add(FSep1);
  Add(FMinimizeItem);
  Add(FRestoreItem);
  Add(FSep2);
  Add(FCloseItem);
  UpdateState(0, False);
  AddToList(MDIButtonsItems, Self);
  if CBTHookHandle = 0 then CBTHookHandle := SetWindowsHookEx(WH_CBT, CBTHook, 0, GetCurrentThreadId);
end;

destructor TTBXMDIButtonsItem.Destroy;
begin
  RemoveFromList(MDIButtonsItems, Self);
  if (MDIButtonsItems = nil) and (CBTHookHandle <> 0) then
  begin
    UnhookWindowsHookEx(CBTHookHandle);
    CBTHookHandle := 0;
  end;
  inherited;
end;

procedure TTBXMDIButtonsItem.UpdateState(W: HWND; Maximized: Boolean);
var
  HasMaxChild, VisibilityChanged: Boolean;

  procedure UpdateVisibleEnabled(const Item: TTBCustomItem; const AEnabled: Boolean);
  begin
    if (Item.Visible <> HasMaxChild) or (Item.Enabled <> AEnabled) then
    begin
      Item.Visible := HasMaxChild;
      Item.Enabled := AEnabled;
      VisibilityChanged := True;
    end;
  end;

var
  MainForm, ActiveMDIChild, ChildForm: TForm;
  I: Integer;
begin
  HasMaxChild := False;
  MainForm := Application.MainForm;
  ActiveMDIChild := nil;
  if Assigned(MainForm) then
  begin
    for I := 0 to MainForm.MDIChildCount-1 do
    begin
      ChildForm := MainForm.MDIChildren[I];
      if ChildForm.HandleAllocated and
         (((ChildForm.Handle = W) and Maximized) or
          ((ChildForm.Handle <> W) and IsZoomed(ChildForm.Handle))) then
      begin
        HasMaxChild := True;
        Break;
      end;
    end;
    ActiveMDIChild := MainForm.ActiveMDIChild;
  end;

  VisibilityChanged := False;
  UpdateVisibleEnabled(TTBXMDIHandler(Owner).FSystemMenuItem, True);
  UpdateVisibleEnabled(FSep1, True);
  UpdateVisibleEnabled(FMinimizeItem, (ActiveMDIChild = nil) or
    (GetWindowLong(ActiveMDIChild.Handle, GWL_STYLE) and WS_MINIMIZEBOX <> 0));
  UpdateVisibleEnabled(FRestoreItem, True);
  UpdateVisibleEnabled(FSep2, True);
  UpdateVisibleEnabled(FCloseItem, True);

  if VisibilityChanged and Assigned((Owner as TTBXMDIHandler).FToolbar) then
  begin
    TTBXMDIHandler(Owner).FToolbar.View.InvalidatePositions;
    TTBXMDIHandler(Owner).FToolbar.View.TryValidatePositions;
  end;
end;

procedure TTBXMDIButtonsItem.ItemClick (Sender: TObject);
var
  MainForm, ChildForm: TForm;
  Cmd: WPARAM;
begin
  MainForm := Application.MainForm;
  if Assigned(MainForm) then begin
    ChildForm := MainForm.ActiveMDIChild;
    if Assigned(ChildForm) then begin
      { Send WM_SYSCOMMAND messages so that we get sounds }
      if Sender = FRestoreItem then
        Cmd := SC_RESTORE
      else if Sender = FCloseItem then
        Cmd := SC_CLOSE
      else
        Cmd := SC_MINIMIZE;
      SendMessage(ChildForm.Handle, WM_SYSCOMMAND, Cmd, GetMessagePos);
    end;
  end;
end;

procedure TTBXMDIButtonsItem.InvalidateSystemMenuItem;
var
  View: TTBView;
begin
  if Assigned((Owner as TTBXMDIHandler).FToolbar) then
  begin
    View := TTBXMDIHandler(Owner).FToolbar.View;
    View.Invalidate (View.Find(TTBXMDIHandler(Owner).FSystemMenuItem));
  end;
end;


//----------------------------------------------------------------------------//

{ TTBXMDIWindowItem }

constructor TTBXMDIWindowItem.Create(AOwner: TComponent);
var
  Form: TForm;
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := STBMDIWindowItemDefCaption;
  FWindowMenu := TMenuItem.Create(Self);

  if not(csDesigning in ComponentState) then
  begin
    { Need to set WindowMenu before MDI children are created. Otherwise the
      list incorrectly shows the first 9 child windows, even if window 10+ is
      active. }
    Form := Application.MainForm;
    if (Form = nil) and (Screen.FormCount > 0) then Form := Screen.Forms[0];
    SetForm (Form);
  end;
end;

procedure TTBXMDIWindowItem.GetChildren (Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TTBXMDIWindowItem.Notification (AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FForm) then SetForm (nil);
end;

procedure TTBXMDIWindowItem.SetForm (AForm: TForm);
begin
  if FForm <> AForm then
  begin
    if Assigned(FForm) and (FForm.WindowMenu = FWindowMenu) then FForm.WindowMenu := nil;
    FForm := AForm;
    if Assigned(FForm) then FForm.FreeNotification (Self);
  end;
  if Assigned(FForm) then FForm.WindowMenu := FWindowMenu;
end;

procedure TTBXMDIWindowItem.EnabledChanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count-1 do Items[I].Enabled := Enabled;
end;

procedure TTBXMDIWindowItem.InitiateAction;
var
  MainForm: TForm;
  I: Integer;
  M: HMENU;
  Item: TTBXCustomItem;
  ItemCount: Integer;
  Buf: array[0..1024] of WideChar;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  MainForm := Application.MainForm;
  if Assigned(MainForm) then SetForm(MainForm);
  if FForm = nil then Exit;
  if FForm.ClientHandle <> 0 then
    { This is needed, otherwise windows selected on the More Windows dialog
      don't move back into the list }
    SendMessage (FForm.ClientHandle, WM_MDIREFRESHMENU, 0, 0);
  M := FWindowMenu.Handle;
  ItemCount := GetMenuItemCount(M) - 1;
  if ItemCount < 0 then ItemCount := 0;
  while Count < ItemCount do
  begin
    Item := TTBXCustomItem.Create(Self);
    Item.Enabled := Enabled;
    Item.OnClick := ItemClick;
    Item.FontSettings := FontSettings;
    Add(Item);
  end;
  while Count > ItemCount do Items[Count - 1].Free;
  for I := 0 to ItemCount - 1 do
  begin
    Item := TTBXCustomItem(Items[I]);
    Item.Tag := GetMenuItemID(M, I + 1);
    if GetMenuStringW(M, I + 1, Buf, 1024, MF_BYPOSITION) = 0 then Buf[0] := #0;
    Item.Caption := Buf;
    Item.Checked := GetMenuState(M, I + 1, MF_BYPOSITION) and MF_CHECKED <> 0;
    Item.FontSettings := FontSettings;
  end;
  if Assigned(FOnUpdate) then FOnUpdate(Self);
end;

procedure TTBXMDIWindowItem.ItemClick (Sender: TObject);
var
  Form: TForm;
begin
  Form := Application.MainForm;
  if Assigned(Form) then PostMessage(Form.Handle, WM_COMMAND, TTBXCustomItem(Sender).Tag, 0);
end;

end.
