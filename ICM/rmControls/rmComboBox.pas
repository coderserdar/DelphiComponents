{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmComboBox
Purpose  : Standard Combobox or MRU with registry save.
Date     : 01-01-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmComboBox;

interface

{$I CompilerDefines.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, buttons, rmBtnEdit, rmSpeedBtns, rmScrnCtrls{$IFDEF rmDebug}, rmMsgList{$ENDIF};

type
  TRegKey = (rkHKEY_CLASSES_ROOT, rkHKEY_CURRENT_USER, rkHKEY_LOCAL_MACHINE, rkHKEY_USERS);

  TrmCustomComboBox = class(TrmCustomBtnEdit)
  private
    FScreenListBox: TrmCustomScreenListBox;
    fDropDownIndex : integer;
    fDropDownWidth: integer;
    fDropDownCount: integer;
    FEditorEnabled: Boolean;
    FOnDropDown: TNotifyEvent;
    fOnCloseUp: TNotifyEvent;
    FOnChanged: TNotifyEvent;
{$IFDEF rmDebug}
    fMsg: TrmMsgEvent;
{$ENDIF}

    procedure DoLBKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DoLBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoLBExit(Sender: Tobject);

    procedure ToggleListBox(Sender: TObject);
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMFontchanged(var Message: TMessage); message CM_FontChanged;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;

    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure wmKillFocus(var Message: TMessage); message wm_killfocus;
    procedure SetComboItems(const Value: TStrings);
    function GetComboItems: TStrings;
    function GetItemHeight: integer;

    function GetItemIndex: integer;

    procedure SetItemIndex(const Value: integer);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure keyPress(var key: char); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure InternalChanged; virtual;

    function GetEnabled: Boolean; override;
    procedure SetEnabled(Value: Boolean); override;

    property DropDownCount: integer read FDropDownCount write fDropDownCount default 8;
    property DropDownWidth: integer read fDropDownWidth write fDropDownWidth default 0;

    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled default True;
    property Items: TStrings read GetComboItems write SetComboItems;
    property ItemHeight: integer read GetItemHeight;

    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
    property OnDropDown: TNotifyEvent read FOnDropDown write fOnDropDown;
    property OnCloseUp: TNotifyEvent read fOnCloseUp write fOnCloseUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message: TMessage); override;

    function DroppedDown: boolean;
{$IFDEF rmDebug}
    property OnMessage: TrmMsgEvent read fMsg write fMsg;
{$ENDIF}
    property ItemIndex : integer read GetItemIndex write SetItemIndex;
  end;

  TrmNewComboBox = class(TrmCustomComboBox)
  published
{$IFDEF D4_OR_HIGHER}
    property Anchors;
    property Constraints;
{$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property EditorEnabled;
    property DropDownCount;
    property DropDownWidth;
    property Items;
    property ItemHeight;

    property OnChanged;
    property OnDropDown;
    property OnCloseUp;

    property OnChange;
    property OnClick;
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

  TrmComboBox = class(TCustomComboBox)
  private
    { Private declarations }
    FTextCompletion: Boolean;
    fHistory: TStringList;
    fEditText: string;
    fKeyBuffer: char;
    fMaxHistory: integer;
    fRegKey: TRegKey;
    fRegPath: string;
    fRegName: string;
    fMRUCombo: boolean;
    fCompletionInProgress: boolean;
    fAutoUpdateHistory: boolean;
    fLastCharIndex: integer;
    fLastCompletionIndex: integer;
    fDroppedDown: boolean;
    FOnCloseUp: TNotifyEvent;
    procedure FixRegPath;
    procedure SetMaxHistory(const Value: integer);
    procedure SetHistory(const Value: TStringList);
    procedure SetRegPath(const value: string);
    procedure SetRegName(const Value: string);
    procedure SetText;
    function GetTextCompletion: Boolean;
    procedure SetTextCompletion(const Value: Boolean);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    { Protected declarations }
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure loaded; override;
    procedure DropDown; override;
    procedure InsertText(NewText: string);
    procedure LoadHistory;
    procedure SaveHistory;
    procedure UpdateHistory;
    procedure ClearHistory;
    {$IFNDEF D6_or_higher}
    procedure CloseUp; dynamic;
    {$ENDIF}
  published
    { Published declarations }
    property Style; {Must be published before Items}
    property Anchors;
    property AutoUpdateHistory: boolean read fAutoUpdateHistory write fAutoUpdateHistory default true;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property MRUCombo: boolean read fMRUCombo write fMRUCombo default False;
    property MaxHistory: integer read fMaxHistory write SetMaxHistory default 20;
    property History: TStringList read fHistory write SetHistory;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RegKey: TRegKey read fRegKey write fRegKey;
    property RegPath: string read fRegPath write SetRegPath;
    property RegName: string read fRegName write SetRegName;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property TextCompletion: Boolean read GetTextCompletion write SetTextCompletion default false;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Registry;

{ TrmComboBox }

procedure TrmComboBox.Change;
begin
  case fKeyBuffer of
    #13:
      begin
        if fAutoUpdateHistory then
          SetText;
      end;
    #27:
      begin
        Text := fEditText;
        SelectAll;
      end;
  end;

  inherited;
end;

procedure TrmComboBox.ClearHistory;
begin
  fHistory.Clear;
end;

{$IFNDEF D6_or_higher}
procedure TrmComboBox.CloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;
{$ENDIF}

procedure TrmComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  if Message.NotifyCode = CBN_CLOSEUP then
  begin
    fDroppedDown := false;
    CloseUp;
  end;
end;

constructor TrmComboBox.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fHistory := TStringList.create;
  fMaxHistory := 20;
  fMRUCombo := false;
  FTextCompletion := false;
  fCompletionInProgress := false;
  fAutoUpdateHistory := true;
  fLastCharIndex := 0;
  fLastCompletionIndex := -1;
  fDroppedDown := false;
end;

destructor TrmComboBox.destroy;
begin
  fHistory.free;
  inherited;
end;

procedure TrmComboBox.DoEnter;
begin
  fEditText := Text;
  inherited;
end;

procedure TrmComboBox.DoExit;
begin
  if fAutoUpdateHistory and (Text <> fEditText) then
    UpdateHistory;

  inherited;
end;

procedure TrmComboBox.DropDown;
begin
  if fMRUCombo then LoadHistory;
  fDroppedDown := true;
  inherited;
end;

procedure TrmComboBox.FixRegPath;
begin
  if (fRegName <> '') then
  begin
    if length(RegPath) > 0 then
    begin
      if fRegPath[length(fRegPath)] <> '\' then
        fRegPath := fRegPath + '\';
    end;
  end
  else
  begin
    if length(RegPath) > 0 then
    begin
      if fRegPath[length(fRegPath)] = '\' then
        Delete(fRegPath, length(fRegPath), 1);
    end;
  end
end;

function TrmComboBox.GetTextCompletion: Boolean;
begin
  Result := fTextCompletion;
end;

procedure TrmComboBox.InsertText(NewText: string);
begin
  text := NewText;
  SetText;
end;

procedure TrmComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  Index, TextLength: Integer;
  wText: string;
begin
  inherited;
  if FTextCompletion then
  begin
    if (key = vk_left) then
    begin
      SelLength := 0;
      key := 0;
      selstart := fLastCharIndex;
      fCompletionInProgress := false;
    end
    else if (key = vk_right) then
    begin
      selLength := 0;
      selstart := length(text);
      flastcharindex := selstart;
      fCompletionInProgress := false;
      key := 0;
    end
    else if (key = vk_down) and not (ssalt in shift) and not (fDroppedDown) then
    begin
      fCompletionInProgress := true;
      wText := copy(text, 0, selstart);
      Index := Perform(CB_FINDSTRING, fLastCompletionIndex, Integer(PChar(wText)));
      if Index <> CB_ERR then
      begin
        fLastCompletionIndex := index;
        TextLength := Length(wText);
        ItemIndex := Index;
        SelStart := TextLength;
        SelLength := Length(Text) - TextLength;
        fLastCharIndex := selstart;
      end;
      key := 0;
    end;
  end;
end;

procedure TrmComboBox.KeyPress(var Key: Char);
begin
  inherited;
  fKeyBuffer := key;
  if ((fKeyBuffer = #13) or (fKeyBuffer = #27)) and (text <> fEditText) then
    Change;
end;

procedure TrmComboBox.KeyUp(var Key: Word; Shift: TShiftState);
var
  Index, TextLength: Integer;
begin
  inherited;
  fCompletionInProgress := false;
  if FTextCompletion then
  begin
    if (key <> vk_delete) and (key <> VK_BACK) then
    begin
      if (SelStart = Length(Text)) then
      begin
        fCompletionInProgress := true;
        Index := Perform(CB_FINDSTRING, -1, Integer(PChar(Text)));
        if Index <> CB_ERR then
        begin
          fLastCompletionIndex := index;
          TextLength := Length(Text);
          ItemIndex := Index;
          SelStart := TextLength;
          SelLength := Length(Text) - TextLength;
          fLastCharIndex := selstart;
        end
      end
    end;
  end;
end;

procedure TrmComboBox.loaded;
begin
  inherited;
  if not (csdesigning in componentstate) then
  begin
    if fMRUCombo then LoadHistory;
  end;
end;

procedure TrmComboBox.LoadHistory;
const
  Keys: array[TRegKey] of HKEY = (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS);
var
  Reg: TRegistry;
  loop: integer;
  wStr : string;
begin
  if fHistory.Text = '' then
  begin
    if (fRegPath <> '') and (fRegName <> '') then
    begin
      Reg := TRegistry.create;
      try
        Reg.RootKey := Keys[fRegKey];
        if Reg.OpenKeyReadOnly(fRegPath + fRegName) then
        begin
          try
            loop := 0;
            while loop < fMaxHistory do
            begin
              if Reg.ValueExists('Item_' + inttostr(loop)) then
              begin
                wstr := Reg.ReadString('Item_' + inttostr(loop));
                if fHistory.IndexOf(wstr) = -1 then
                   fHistory.Add(wstr);
              end;
              inc(loop);
            end;
          finally
            Reg.CloseKey;
          end;
        end;
      finally
        Reg.free;
      end;
    end;
  end;
  Items.Text := fHistory.Text;
end;

procedure TrmComboBox.SaveHistory;
const
  Keys: array[TRegKey] of HKEY = (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE, HKEY_USERS);
var
  Reg: TRegistry;
  loop: integer;
begin
  Reg := TRegistry.create;
  try
    Reg.RootKey := Keys[fRegKey];
    if Reg.OpenKey(fRegPath + fRegName, True) then
    begin
      try
        loop := 0;
        while (loop < fMaxHistory) and (loop < fHistory.Count) do
        begin
          Reg.WriteString('Item_' + inttostr(loop), fHistory.Strings[loop]);
          inc(loop);
        end;
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.free;
  end;
end;

procedure TrmComboBox.SetHistory(const Value: TStringList);
begin
  fHistory.assign(Value);
end;

procedure TrmComboBox.SetMaxHistory(const Value: integer);
begin
  fMaxHistory := Value;
end;

procedure TrmComboBox.SetRegName(const Value: string);
begin
  fRegName := Value;
  FixRegPath;
end;

procedure TrmComboBox.SetRegPath(const value: string);
begin
  fRegPath := value;
  FixRegPath;
end;

procedure TrmComboBox.SetText;
var
  index: integer;
begin
  fEditText := Text;
  SelectAll;
  try
    if fMRUCombo then
    begin
      index := fHistory.IndexOf(fEditText);
      if index = -1 then
        fHistory.Insert(0, fEditText)
      else
        fHistory.Move(index, 0);
      SaveHistory;
    end;
  finally
    SelLength := 0;
    SelStart := length(text);
  end;
end;

procedure TrmComboBox.SetTextCompletion(const Value: Boolean);
begin
  fTextCompletion := Value;
end;

procedure TrmComboBox.UpdateHistory;
begin
  SetText;
end;

{ TrmCustomComboBox }

constructor TrmCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fDropDownCount := 8;

  OnBtn1Click := ToggleListBox;
  UseDefaultGlyphs := false;
  Btn1Glyph := nil;
  with GetButton(1) do
  begin
     font.name := 'Marlett';
     font.size := 10;
     caption := '6';
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
    Font.assign(self.font);
  end;
  FScreenListBox.hide;

  OnExit := doLBExit;

  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
end;

destructor TrmCustomComboBox.Destroy;
begin
  FScreenListBox.free;
  inherited Destroy;
end;

procedure TrmCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  wIndex: integer;
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
    begin
      Try
        if assigned(fOnCloseUp) then
          fOnCloseUp(self);
      except
         //Do Nothing...
      end;
      FScreenListbox.hide;
    end;
    key := 0;
  end
  else if ((Key = VK_DOWN) or (key = VK_UP)) and (shift = []) then
  begin
    if not fScreenListbox.visible then
    begin
      wIndex := FScreenListBox.ItemIndex;
      if (key = vk_up) and (wIndex > 0) then
      begin
        fScreenListBox.itemIndex := wIndex - 1;
      end;

      if (key = vk_down) and (wIndex < FScreenListBox.Items.Count) then
      begin
        fScreenListBox.itemIndex := wIndex + 1;
      end;

      if fScreenListBox.itemIndex <> wIndex then
      begin
        Self.Text := fScreenListBox.items[fScreenListBox.itemIndex];
        InternalChanged;
        Self.SelectAll;
      end;
    end;
    key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TrmCustomComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN or ES_MULTILINE;
end;

procedure TrmCustomComboBox.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomComboBox.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TrmCustomComboBox.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
end;

procedure TrmCustomComboBox.SetComboItems(const Value: TStrings);
begin
  FScreenListBox.Items.assign(Value);
end;

procedure TrmCustomComboBox.ToggleListBox(Sender: TObject);
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

  with FScreenListBox do
  begin
    if fDropDownWidth = 0 then
      Width := self.width
    else
      width := fDropDownWidth;

    if Items.Count > fDropDownCount then
      Height := itemheight * fDropDownCount
    else
    begin
      Height := itemheight * Items.Count;
      if height = 0 then
        height := itemheight;
    end;

    height := height + 2;

    fDropDownIndex := ItemIndex;
    FScreenListBox.itemindex := fDropDownIndex;

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

procedure TrmCustomComboBox.DoLBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FScreenListBox.ItemIndex <> -1) then
  begin
    FScreenListBox.hide;
    Text := FScreenListBox.items[fscreenlistbox.itemindex];
    self.setfocus;
    self.SelectAll;
  end;
  if (FScreenListBox.ItemIndex <> fDropDownIndex) then
    InternalChanged;
end;

procedure TrmCustomComboBox.DoLBExit(Sender: Tobject);
begin
  if FScreenListBox.visible then
    FScreenListBox.visible := false;
end;

procedure TrmCustomComboBox.DoLBKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = vk_escape) then
  begin
    FScreenListBox.hide;
    self.setfocus;
    self.SelectAll;
    key := 0;
  end
  else if (key = vk_Return) then
  begin
    key := 0;
    FScreenListBox.hide;
    if FScreenListBox.ItemIndex <> fDropDownIndex then
       Text := FScreenListBox.items[fscreenlistbox.itemindex];
    if Self.CanFocus then
       self.setfocus;
    self.SelectAll;
    InternalChanged;
  end
end;

procedure TrmCustomComboBox.WndProc(var Message: TMessage);
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

procedure TrmCustomComboBox.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  if message.sender = fScreenListbox then
    exit;
  if (FScreenListBox.visible) then
  begin
    try
      if assigned(fOnCloseUp) then
        fOnCloseUp(self);
    except
         //Do Nothing.
    end;
    FScreenListBox.Hide;
  end;
end;

function TrmCustomComboBox.GetComboItems: TStrings;
begin
  Result := fscreenlistbox.Items;
end;

procedure TrmCustomComboBox.keyPress(var key: char);
begin
  if not FEditorEnabled then
    key := #0
  else if key = #13 then
    key := #0;

  inherited;
end;

function TrmCustomComboBox.DroppedDown: boolean;
begin
  Result := FScreenListBox.Visible;
end;

procedure TrmCustomComboBox.CMFontchanged(var Message: TMessage);
begin
  inherited;
  fScreenListBox.Font.assign(self.Font);
end;

function TrmCustomComboBox.GetItemHeight: integer;
begin
  Result := FScreenListBox.ItemHeight;
end;

procedure TrmCustomComboBox.wmKillFocus(var Message: TMessage);
begin
  inherited;
  if (FScreenListBox.visible) then
  begin
    try
      if assigned(fOnCloseUp) then
        fOnCloseUp(self);
    except
         //Do Nothing.
    end;
    FScreenListBox.Hide;
  end;
end;

function TrmCustomComboBox.GetEnabled: Boolean;
begin
   result := inherited GetEnabled;
end;

function TrmCustomComboBox.GetItemIndex: integer;
begin
   result := FScreenListBox.Items.indexof(text);
end;

procedure TrmCustomComboBox.SetEnabled(Value: Boolean);
begin
   inherited Setenabled(value);
   Btn1Enabled := value;
end;

procedure TrmCustomComboBox.SetItemIndex(const Value: integer);
begin
   text := FScreenListBox.Items[Value];
end;

procedure TrmCustomComboBox.InternalChanged;
begin
   try
     modified := true;
     if assigned(fOnchanged) then
       fOnchanged(self);
   except
       //do nothing
   end;
end;

end.

