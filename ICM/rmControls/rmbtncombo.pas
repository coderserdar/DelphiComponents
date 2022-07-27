{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmBtnCombo
Purpose  : An edit control with a Elipsis button and combo button combination.
Date     : 01-26-01
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmBtnCombo;

interface

{$I CompilerDefines.inc}

uses Windows, Classes, StdCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, rmBtnEdit, rmSpeedBtns, rmScrnCtrls{$IFDEF rmDebug}, rmMsgList{$ENDIF};

type
{ TrmCustomBtnCombo }
  TrmCustomBtnCombo = class(TrmCustomBtnEdit)
  private
    FScreenListBox: TrmCustomScreenListBox;
    fDropDownWidth: integer;
    FDropDownHeight: integer;
    FEditorEnabled: Boolean;
    FOnDropDown: TNotifyEvent;
    FOnChanged: TNotifyEvent;
{$IFDEF rmDebug}
    fMsg: TrmMsgEvent;
{$ENDIF}
    FOnBtnClick: TNotifyEvent;
    fOnCloseUp: TNotifyEvent;

    procedure DoLBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DoLBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoLBExit(Sender: Tobject);

    procedure ToggleListBox(Sender: TObject);
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;

    procedure CMFontchanged(var Message: TMessage); message CM_FontChanged;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure wmKillFocus(var Message: TMessage); message wm_killfocus;

{$IFDEF D4_OR_HIGHER}
    procedure SetEnabled(value: Boolean); reintroduce; (* reintroduce is D4 Modification *)
    function GetEnabled: Boolean; reintroduce; (* reintroduce is D4 Modification *)
{$ELSE}
    procedure SetEnabled(value: Boolean);
{$ENDIF}
    procedure SetComboItems(const Value: TStrings);
    function GetComboItems: TStrings;
    function GetEllipsisBtnVisible: boolean;
    procedure SetEllipsisBtnVisible(const Value: boolean);
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    procedure SetEditorEnabled(const Value: Boolean);
    function GetDroppedDown: boolean;
  protected
    procedure DownClick(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure keyPress(var key: char); override;

    property DropDownHeight: integer read FDropDownHeight write fDropDownHeight default 0;
    property DropDownWidth: integer read fDropDownWidth write fDropDownWidth default 0;

    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Items: TStrings read GetComboItems write SetComboItems;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;

    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
    property OnDropDown: TNotifyEvent read FOnDropDown write fOnDropDown;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
    property OnCloseUp: TNotifyEvent read fOnCloseUp write fOnCloseUp;
    property EllipsisBtnVisible: boolean read GetEllipsisBtnVisible write SetEllipsisBtnVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
{$IFDEF rmDebug}
    property OnMessage: TrmMsgEvent read fMsg write fMsg;
{$ENDIF}
    procedure CloseUp; virtual;
    property DroppedDown : boolean read GetDroppedDown;
  end;

  TrmBtnCombo = class(TrmCustomBtnCombo)
  published
    property EditorEnabled;
    property Enabled;
    property DropDownHeight;
    property DropDownWidth;
    property Items;
    property ItemIndex;
    property Text;
    property EllipsisBtnVisible;

{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnBtnClick;
    property OnDropDown;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses rmLibrary;

{$R rmBtnCombo.RES}

{ TrmCustomBtnCombo }

constructor TrmCustomBtnCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetButton(1) do
  begin
    Font.name := 'Marlett';
    font.size := 10;
    Font.color := clBtnText;
    Caption := '6';
    Glyph := nil;
  end;
  OnBtn1Click := ToggleListBox;

  Btn2Visible := true;
  OnBtn2Click := DownClick;

  FScreenListBox := TrmCustomScreenListBox.create(nil);
  with FScreenListBox do
  begin
    width := self.width;
    height := self.height * 8;
    visible := false;
    Parent := self;
    OnKeyDown := DoLBKeyDown;
    OnMousedown := DoLBMouseDown;
  end;
  FScreenListBox.hide;

  OnExit := doLBExit;

  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
end;

destructor TrmCustomBtnCombo.Destroy;
begin
  FScreenListBox.free;
  inherited Destroy;
end;

procedure TrmCustomBtnCombo.KeyDown(var Key: Word; Shift: TShiftState);
var
  wIndex: integer;
begin
  if not FEditorEnabled then
  begin
    if (key in [vk_delete]) then
      key := 0;
  end;
  if (((Key = VK_DOWN) or (key = VK_UP)) and (shift = [ssALT])) or
    ((key = vk_f4) and (shift = [])) then
  begin
    if not FScreenListbox.visible then
      ToggleListBox(self)
    else
    begin
      FScreenListbox.hide;
      CloseUp;
    end;
  end
  else if (key in [vk_Down, VK_up, vk_left, vk_right,
    vk_home, vk_end, vk_prior, vk_next]) and
    (shift = []) then
  begin
    if not FScreenListbox.visible then
    begin
      try
        wIndex := fScreenListBox.items.Indexof(Text);
        case key of
          vk_down, vk_right: inc(wIndex);
          vk_up, vk_left: dec(wIndex);
          vk_home: wIndex := 0;
          vk_end: wIndex := FScreenListBox.Items.Count - 1;
          vk_next: inc(wIndex, (DropDownHeight div FScreenListBox.ItemHeight) - 1);
          vk_prior: dec(wIndex, (DropDownHeight div FScreenListBox.ItemHeight) - 1);
        end;
        if wIndex < 0 then
          wIndex := 0
        else if wIndex >= FScreenListBox.Items.Count then
          wIndex := FScreenListBox.Items.Count - 1;
        FScreenListBox.ItemIndex := wIndex;
        Text := FScreenListBox.Items[wIndex];
      except
          //do nothing
      end;
      setfocus;
      selectall;
      key := 0;
    end
    else
    begin
      setfocus;
      selectall;
      key := 0;
    end;
  end
  else if ((key = VK_RETURN) and (shift = [ssCTRL]) and not FScreenListbox.visible) then
  begin
    DownClick(self);
    key := 0
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TrmCustomBtnCombo.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomBtnCombo.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomBtnCombo.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TrmCustomBtnCombo.SetEnabled(value: Boolean);
begin
  inherited enabled := value;
  Btn1Enabled := Value;
  Btn2Enabled := value;
end;

function TrmCustomBtnCombo.GetEnabled: Boolean;
begin
  result := inherited Enabled;
end;

procedure TrmCustomBtnCombo.DownClick(Sender: TObject);
begin
  if assigned(fonbtnClick) then
    fonbtnClick(self);
end;

procedure TrmCustomBtnCombo.SetComboItems(const Value: TStrings);
begin
  FScreenListBox.Items.assign(Value);
end;

procedure TrmCustomBtnCombo.ToggleListBox(Sender: TObject);
var
  CP, SP: TPoint;
begin
  CP.X := Left;
  CP.Y := Top + Height;
  SP := parent.ClientToScreen(CP);

  if assigned(fonDropdown) then
    fOnDropDown(self);

  SetFocus;
  SelectAll;

  FScreenListBox.Font := Font;

  if fDropDownWidth = 0 then
    FScreenListBox.Width := self.width
  else
    FScreenListBox.width := fDropDownWidth;

  if fDropDownHeight = 0 then
    FScreenListBox.Height := self.Height * 8
  else
    FScreenListBox.Height := fDropDownHeight;

  try
    fScreenListBox.itemindex := fScreenListBox.Items.indexof(text);
  except
    fScreenListBox.itemindex := -1;
  end;

  FScreenListBox.Left := SP.X;

  if assigned(screen.ActiveForm) then
  begin
    if (SP.Y + FScreenListBox.height < screen.activeForm.Monitor.Height) then
      FScreenListBox.Top := SP.Y
    else
      FScreenListBox.Top := (SP.Y - self.height) - FScreenListBox.height;
  end
  else
  begin
    if (SP.Y + FScreenListBox.height < screen.Height) then
      FScreenListBox.Top := SP.Y
    else
      FScreenListBox.Top := (SP.Y - self.height) - FScreenListBox.height;
  end;

  FScreenListBox.Show;
  SetWindowPos(FScreenListBox.handle, hwnd_topMost, 0, 0, 0, 0, swp_nosize or swp_NoMove);
end;

procedure TrmCustomBtnCombo.DoLBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FScreenListBox.hide;
  if FScreenListBox.ItemIndex <> -1 then
    Text := FScreenListBox.items[fscreenlistbox.itemindex];
  closeup;
  self.setfocus;
  self.SelectAll;
  if (FScreenListBox.ItemIndex <> -1) and assigned(fonchanged) then
    fOnchanged(self);
end;

procedure TrmCustomBtnCombo.DoLBExit(Sender: Tobject);
begin
  if FScreenListBox.visible then
  begin
    FScreenListBox.visible := false;
    CloseUp;
  end;
end;

procedure TrmCustomBtnCombo.DoLBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = vk_escape) then
  begin
    FScreenListBox.hide;
    CloseUp;
    self.setfocus;
    self.SelectAll;
    key := 0;
  end
  else if (key = vk_Return) then
  begin
    FScreenListBox.hide;
    if FScreenListBox.ItemIndex <> -1 then
      Text := FScreenListBox.items[fscreenlistbox.itemindex];
    CloseUp;
    self.setfocus;
    self.SelectAll;
    key := 0;
    if assigned(fonchanged) then
      fOnchanged(self);
  end
end;

procedure TrmCustomBtnCombo.WndProc(var Message: TMessage);
begin
{$IFDEF rmDebug}
  if assigned(OnMessage) then
  try
    OnMessage(Message);
  except
  end;
{$ENDIF}

  case Message.Msg of
    WM_CHAR,
      WM_KEYDOWN,
      WM_KEYUP:
      if (FScreenListBox.visible) then
      begin
        if GetCaptureControl = nil then
        begin
          Message.result := SendMessage(FScreenListBox.Handle, message.msg, message.wParam, message.LParam);
          if message.result = 0 then exit;
        end;
      end;
  end;
  inherited WndProc(message);
end;

procedure TrmCustomBtnCombo.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  if Message.Sender = FScreenListBox then
    exit;
  if FScreenListBox.visible then
  begin
    FScreenListBox.Hide;
    CloseUp;
  end;
end;

function TrmCustomBtnCombo.GetComboItems: TStrings;
begin
  Result := fscreenlistbox.Items;
end;

procedure TrmCustomBtnCombo.keyPress(var key: char);
begin
  if not FEditorEnabled then
    key := #0
  else if key = #13 then
    key := #0;

  inherited;
end;

procedure TrmCustomBtnCombo.wmKillFocus(var Message: TMessage);
begin
  inherited;
  if FScreenListBox.visible then
  begin
    FScreenListBox.Hide;
    CloseUp;
  end;
end;

procedure TrmCustomBtnCombo.CMFontchanged(var Message: TMessage);
begin
  inherited;
  FScreenListBox.Font.Assign(self.font);
end;

function TrmCustomBtnCombo.GetEllipsisBtnVisible: boolean;
begin
  Result := Btn2Visible;
end;

procedure TrmCustomBtnCombo.SetEllipsisBtnVisible(const Value: boolean);
begin
  Btn2Visible := value;
end;

procedure TrmCustomBtnCombo.CloseUp;
begin
  try
    if assigned(fOnCloseUp) then
      fOnCloseUp(self);
  except
      //Do Nothing...
  end;
end;

function TrmCustomBtnCombo.GetItemIndex: integer;
begin
  result := items.IndexOf(Text);
end;

procedure TrmCustomBtnCombo.SetItemIndex(const Value: integer);
begin
  if value = -1 then
    text := ''
  else
    text := items[value];
end;

procedure TrmCustomBtnCombo.SetEditorEnabled(const Value: Boolean);
var
   styles : TControlStyle;
begin
  FEditorEnabled := Value;
  styles := ControlStyle;
  if FEditorEnabled then
     include(Styles, csMenuEvents)
  else
     exclude(Styles, csMenuEvents);

  ControlStyle := Styles;
end;

procedure TrmCustomBtnCombo.WMContextMenu(var Message: TWMContextMenu);
begin
   if not EditorEnabled and not assigned(popupmenu) then
      message.result := 1;
   inherited;
end;

function TrmCustomBtnCombo.GetDroppedDown: boolean;
begin
   result := FScreenListBox.Visible;  
end;

end.
                        
