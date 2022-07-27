{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmSpinCombo
Purpose  : An edit control with a Spin and combo button combination.
Date     : 07-20-00
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmSpinCombo;


interface

{$I CompilerDefines.INC}

uses Windows, Classes, StdCtrls, Controls, Messages, SysUtils,
  Forms, Graphics, Buttons, rmBaseEdit, rmSpeedBtns, rmScrnCtrls {$ifDef rmDebug}, rmMsgList{$endif};

type
{ TrmCustomSpinCombo }

  TrmCustomSpinCombo = class(TrmCustomEdit)
  private
    FScreenListBox: TrmCustomScreenListBox;
    fDropDownWidth: integer;
    FDropDownHeight: integer;
    FSpinBtn: TrmSpinButton;
    FComboBtn: TrmSpeedButton;
    FEditorEnabled: Boolean;
    fMaxValue: integer;
    fMinValue: integer;
    fRanges: boolean;
    FOnDropDown: TNotifyEvent;
    FOnChanged : TNotifyEvent;
{$ifdef rmDebug}
    fMsg: TrmMsgEvent;
{$endif}

    procedure DoLBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DoLBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoLBExit(Sender: Tobject);

    procedure ToggleListBox(Sender: TObject);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;

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
    procedure SetMaxValue(const Value: integer);
    procedure SetMinValue(const Value: integer);
    procedure SetRanges(const Value: boolean);
    procedure CheckRanges;
    function GetComboItems: TStrings;
  protected
    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure keyPress(var key:char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    property DropDownHeight: integer read FDropDownHeight write fDropDownHeight default 0;
    property DropDownWidth: integer read fDropDownWidth write fDropDownWidth default 0;

    property MaxValue: integer read fMaxValue write SetMaxValue default 0;
    property MinValue: integer read fMinValue write SetMinValue default 0;
    property UseRanges: boolean read fRanges write SetRanges default false;

    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Items: TStrings read GetComboItems write SetComboItems;

    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
    property OnDropDown: TNotifyEvent read FOnDropDown write fOnDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;
{$ifdef rmDebug}
    property OnMessage:TrmMsgEvent read fMsg write fMsg;
{$endif}
  end;

  TrmSpinCombo = class(TrmCustomSpinCombo)
  published
    property EditorEnabled;
    property Enabled;
    property DropDownHeight;
    property DropDownWidth;
    property Items;
    property UseRanges;
    property MaxValue;
    property MinValue;
    property Text;

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

    property OnChanged;
    property OnDropDown;
  end;

implementation

uses rmLibrary;

{ TrmCustomSpinCombo }

constructor TrmCustomSpinCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FComboBtn := TrmSpeedButton.Create(Self);
  with FComboBtn do
  begin
    Height := 17;
    Width := 16;
    Style := sbsComboButton;
    Cursor := crArrow;
    Parent := Self;
    OnClick := ToggleListBox;
    Layout := blGlyphTop;
    enabled := true;

    Font.name := 'Marlett';
    font.size := 10;
    Font.color := clBtnText;
    Caption := '6';
    Glyph := nil;
  end;

  FSpinBtn := TrmSpinButton.Create(Self);
  with FSpinBtn do
  begin
    Width := 15;
    Height := 16;
    Visible := True;
    Parent := Self;
    FocusControl := Self;
    OnUpClick := UpClick;
    OnDownClick := DownClick;
    enabled := true;
  end;

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

  fMaxValue := 0;
  fMinValue := 0;
  fRanges := false;
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
end;

destructor TrmCustomSpinCombo.Destroy;
begin
  FSpinBtn.free;
  FComboBtn.free;
  FScreenListBox.free;
  inherited Destroy;
end;

procedure TrmCustomSpinCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FEditorEnabled then
  begin
     if (key in [vk_delete]) then
        key := 0;
  end;
  if (((Key = VK_DOWN) or (key = VK_UP)) and (ssAlt in Shift)) or
      ((key = vk_f4) and (shift = [])) then
  begin
    if not FScreenListbox.visible then
      ToggleListBox(self)
    else
      FScreenListbox.hide;
  end
  else if ((Key = VK_DOWN) or (key = VK_UP)) and (shift = []) then
  begin
    if not fScreenListbox.visible then
    begin
      if key = vk_up then
        UpClick(self);

      if key = vk_down then
        DownClick(self);
    end;
  end
  else
  inherited KeyDown(Key, Shift);
end;

procedure TrmCustomSpinCombo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or ES_MULTILINE;
end;

procedure TrmCustomSpinCombo.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TrmCustomSpinCombo.SetEditRect;
var
  R: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));
  R.Right := clientwidth - FSpinBtn.Width - FComboBtn.width - 1;
  R.Top := 0;
  R.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@R)); {debug}
end;

procedure TrmCustomSpinCombo.WMSize(var Message: TWMSize);
begin
  inherited;
  if NewStyleControls and Ctl3D then
  begin
    FSpinBtn.SetBounds((width - (FSpinBtn.width + FComboBtn.width)) - 4, 0, FSpinBtn.width, Height - 4);
    FComboBtn.SetBounds((width - (FComboBtn.width)) - 4, 0, FComboBtn.width, Height - 4);
  end
  else
  begin
    FSpinBtn.SetBounds((width - (FSpinBtn.width + FComboBtn.width)), 1, FSpinBtn.width, Height - 2);
    FComboBtn.SetBounds((width - (FComboBtn.width)), 1, FComboBtn.width, Height - 2);
  end;
  SetEditRect;
end;

procedure TrmCustomSpinCombo.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomSpinCombo.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomSpinCombo.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TrmCustomSpinCombo.SetEnabled(value: Boolean);
begin
  inherited enabled := value;
  FSpinBtn.enabled := value;
  FComboBtn.Enabled := value;
end;

function TrmCustomSpinCombo.GetEnabled: Boolean;
begin
  result := inherited Enabled;
end;

procedure TrmCustomSpinCombo.DownClick(Sender: TObject);
var
  wInt: integer;
begin
  try
    wInt := StrToInt(text);
    dec(wInt);
  except
    wInt := 0;
  end;
  text := inttostr(wInt);
  CheckRanges;
  if assigned(fonchanged) then
     fOnchanged(self);
end;

procedure TrmCustomSpinCombo.UpClick(Sender: TObject);
var
  wInt: integer;
begin
  try
    wInt := StrToInt(text);
    inc(wInt);
  except
    wInt := 0;
  end;
  text := inttostr(wInt);
  CheckRanges;
  if assigned(fonchanged) then
     fOnchanged(self);
end;

procedure TrmCustomSpinCombo.SetComboItems(const Value: TStrings);
begin
  FScreenListBox.Items.assign(Value);
end;

procedure TrmCustomSpinCombo.SetMaxValue(const Value: integer);
begin
  fMaxValue := Value;
  if (maxvalue < fMinvalue) then
    fMinValue := fMaxValue;
  CheckRanges;
end;

procedure TrmCustomSpinCombo.SetMinValue(const Value: integer);
begin
  fMinValue := value;
  if (fMinvalue > fMaxValue) then
    fMaxValue := Value;
  CheckRanges;
end;

procedure TrmCustomSpinCombo.SetRanges(const Value: boolean);
begin
  if franges <> value then
  begin
    fRanges := Value;
    CheckRanges;
  end;
end;

procedure TrmCustomSpinCombo.CheckRanges;
var
  wInt: integer;
begin
  if UseRanges then
  begin
    try
      wInt := strtoint(text);
      if (wInt <= fMinValue) then
      begin
        wInt := fMinValue;
        FSpinBtn.DownEnabled := false;
      end
      else
        fSpinBtn.DownEnabled := true;

      if (wInt >= fMaxValue) then
      begin
        wInt := fMaxValue;
        fSpinBtn.UpEnabled := false;
      end
      else
        fSpinBtn.UpEnabled := true;
      text := inttostr(wInt);
    except
    end;
  end;
end;

procedure TrmCustomSpinCombo.ToggleListBox(Sender: TObject);
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

  with FScreenListBox do
  begin
    if fDropDownWidth = 0 then
      Width := self.width
    else
      width := fDropDownWidth;

    if fDropDownHeight = 0 then
      Height := self.Height * 8
    else
      Height := fDropDownHeight;

    Left := SP.X;

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

    Show;
    SetWindowPos(handle, hwnd_topMost, 0, 0, 0, 0, swp_nosize or swp_NoMove);
  end;
end;

procedure TrmCustomSpinCombo.DoLBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FScreenListBox.hide;
  if FScreenListBox.ItemIndex <> -1 then
    Text := FScreenListBox.items[fscreenlistbox.itemindex];
  self.setfocus;
  self.SelectAll;
  if (FScreenListBox.ItemIndex <> -1) and assigned(fonchanged) then
     fOnchanged(self);
end;

procedure TrmCustomSpinCombo.DoLBExit(Sender: Tobject);
begin
  if FScreenListBox.visible then
    FScreenListBox.visible := false;
end;

procedure TrmCustomSpinCombo.DoLBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = vk_escape) then
  begin
    FScreenListBox.hide;
    self.setfocus;
    self.SelectAll;
    key := 0;
  end
  else
    if (key = vk_Return) then
    begin
      key := 0;
      FScreenListBox.hide;
      if FScreenListBox.ItemIndex <> -1 then
        Text := FScreenListBox.items[fscreenlistbox.itemindex];
      self.setfocus;
      self.SelectAll;
      if assigned(fonchanged) then
         fOnchanged(self);
    end
end;

procedure TrmCustomSpinCombo.WndProc(var Message: TMessage);
begin
{$ifdef rmDebug}
  if assigned(OnMessage) then
  try
     OnMessage(Message);
  except
  end;
{$endif}

  case Message.Msg of
    WM_CHAR,
      WM_KEYDOWN,
      WM_KEYUP: if (FScreenListBox.visible) then
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

procedure TrmCustomSpinCombo.CMCancelMode(var Message: TCMCancelMode);
begin
   inherited;
   if Message.Sender = FScreenListBox then
      exit;
   if FScreenListBox.visible then
      FScreenListBox.Hide;
end;

function TrmCustomSpinCombo.GetComboItems: TStrings;
begin
   Result := fscreenlistbox.Items;
end;

procedure TrmCustomSpinCombo.keyPress(var key: char);
begin
  if not FEditorEnabled then
     key := #0
  else if key = #13 then
     key := #0;

  inherited;
end;

procedure TrmCustomSpinCombo.wmKillFocus(var Message: TMessage);
begin
   inherited;
   if FScreenListBox.visible then
      FScreenListBox.Hide;
end;

procedure TrmCustomSpinCombo.CMFontchanged(var Message: TMessage);
begin
   inherited;
   FScreenListBox.Font.Assign(self.font);
end;

end.

