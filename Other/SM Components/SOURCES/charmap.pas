{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29
}
unit CharMap;

interface
uses Windows, Classes, Messages, StdCtrls, Graphics, Buttons, Controls;

type
  TSMPopupListbox = class(TWinControl)
  private
    FCharacter: Char;
    procedure BtnClick(Sender: TObject);
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCharMapCombo = class(TCustomEdit)
  private
    { Private declarations }
    FButton: TSpeedButton;
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;

    FPickList: TSMPopupListbox;

    FAlignment: TAlignment;

    procedure SetAlignment(Value: TAlignment);

    function GetCharacter: Char;
    procedure SetCharacter(Value: Char);

    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;

    procedure EditButtonClick(Sender: TObject);
    procedure SetEditRect;

    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    { Protected declarations }
    procedure ButtonClick; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure CloseUp(Accept: Boolean);
    procedure DropDown;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Button: TSpeedButton read FButton;
  published
    { Published declarations }
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Character: Char read GetCharacter write SetCharacter;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;

    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
uses SysUtils, menus, forms, extctrls;

procedure Register;
begin
  RegisterComponents('SMComponents', [TCharMapCombo]);
end;


{ TSMPopupListBox }
constructor TSMPopupListBox.Create(AOwner: TComponent);
var
  i, j: Integer;
  BackPanel: TPanel;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
  Color := clBtnFace;
  Parent := TWinControl(AOWner);

  BackPanel := TPanel.Create(Self);
  with BackPanel do
  begin
    Parent := Self;
    Align := alClient;
    ParentColor := True;
  end;

  for i := 1 to 14 do
    for j := 1 to 16 do
      with TSpeedButton.Create(Self) do
        try
          Caption := Chr(i*16+j+15);
          if Caption = '&' then
            Caption := Caption + '&';
          Hint := Format('ASCII dec value: %d'#13#10'ASCII hex value: $%x'#13#10'Symbol: %s', [i*16+j+15, i*16+j+15, Chr(i*16+j+15)]);
          Tag := i*16+j+15;
          Flat := True;
          GroupIndex := 1;
          Height := 18;
          Width := 18;
          Left := j*20 - 15;
          Top := i*20 - 15;
          Parent := BackPanel; //Self;
          OnClick := BtnClick
        finally
        end;
end;

procedure TSMPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    Style := Style or WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW {or WS_EX_TOPMOST};
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TSMPopupListbox.BtnClick(Sender: TObject);
begin
  with TCharMapCombo(Owner) do
  begin
    if not ReadOnly then
      Character := Chr(TSpeedButton(Sender).Tag);
    CloseUp(True);
  end;
end;

procedure TSMPopupListbox.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;


{ TCharMapCombo }
constructor TCharMapCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAlignment := taLeftJustify;

  FBtnControl := TWinControl.Create(Self);
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
  FBtnControl.Width := GetSystemMetrics(SM_CXVSCROLL);
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;

  FButton := TSpeedButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
  FButton.OnClick := EditButtonClick;
  Height := 21;

  FPickList := TSMPopupListbox.Create(Self);
  FPickList.Visible := False;
  FPickList.Width := 330;
  FPickList.Height := 290;
  FPickList.Parent := Self;
  MaxLength := 1;
end;

destructor TCharMapCombo.Destroy;
begin
  FButton.OnClick := nil;
  FPickList.Free;

  inherited Destroy;
end;

procedure TCharMapCombo.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of dWord =
              (ES_LEFT, ES_RIGHT, ES_CENTER);
  Multilines: array[Boolean] of dWord = (0, ES_MULTILINE);
begin
  inherited CreateParams(Params);

  Params.Style := Params.Style or
                  Multilines[PasswordChar = #0] or
                  Alignments[FAlignment];
end;

procedure TCharMapCombo.CreateWnd;
begin
  inherited CreateWnd;

  SetEditRect;
end;

procedure TCharMapCombo.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TCharMapCombo.Change;
begin
  inherited Change;

  if Length(Text) > 0 then
    Character := Text[1]
end;

procedure TCharMapCombo.SetEditRect;
var Loc: TRect;
begin
  if NewStyleControls then
  begin
    if Ctl3D then
      FBtnControl.SetBounds(Width - FButton.Width - 4, 0, FButton.Width, Height - 4)
    else
      FBtnControl.SetBounds(Width - FButton.Width - 2, 2, FButton.Width, Height - 4);
  end
  else
    FBtnControl.SetBounds(Width - FButton.Width - 2, 1, FButton.Width, ClientHeight - 2);
  FButton.Height := FBtnControl.Height;

  SetRect(Loc, 0, 0, ClientWidth - FBtnControl.Width - 2, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TCharMapCombo.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;

  SetEditRect;
end;

procedure TCharMapCombo.WMSize(var Message: TWMSize);
begin
  inherited;

  SetEditRect;
end;

function TCharMapCombo.GetCharacter: Char;
begin
  Result := FPickList.FCharacter;
end;

procedure TCharMapCombo.SetCharacter(Value: Char);
var i: Integer;
begin
  if FPickList.FCharacter <> Value then
  begin
    FPickList.FCharacter := Value;
    Text := Value;

    for i := 0 to FPickList.ComponentCount-1 do
      if FPickList.Components[i] is TSpeedButton then
        with TSpeedButton(FPickList.Components[i]) do
          if Chr(Tag) = Character then
            Down := True;
  end;
end;

procedure TCharMapCombo.EditButtonClick(Sender: TObject);
begin
  if Enabled then
    ButtonClick;
end;

procedure TCharMapCombo.ButtonClick;
begin
  DropDown;

  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

procedure TCharMapCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (scAlt + vk_Down = ShortCut(Key, Shift)) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

procedure TCharMapCombo.CloseUp(Accept: Boolean);
begin
  if Assigned(FPickList) and FPickList.Visible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetWindowPos(FPickList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);

    FPickList.Visible := False;
    Invalidate;
    SetFocus;
  end;
end;

procedure TCharMapCombo.DropDown;
var P: TPoint;
  i: Integer;
begin
  if Assigned(FPickList) and (not FPickList.Visible) then
  begin
    FPickList.ShowHint := ShowHint;
//    FPickList.Width := Width;
//    FPickList.Color := Color;
    FPickList.Font := Font;
    P := Parent.ClientToScreen(Point(Left, Top));
    i := P.Y + Height;
    if i + FPickList.Height > Screen.Height then
      i := P.Y - FPickList.Height;
    SetWindowPos(FPickList.Handle, HWND_TOP, P.X, i, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);

    FPickList.Visible := True;
    Invalidate;
    SetFocus;
  end;
end;

procedure TCharMapCombo.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPickList) then
    CloseUp(False);
end;

procedure TCharMapCombo.WMKillFocus(var Message: TMessage);
begin
  inherited;

  CloseUp(False);
end;

procedure TCharMapCombo.WndProc(var Message: TMessage);

  procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
  begin
    case Key of
      VK_UP, VK_DOWN:
        if ssAlt in Shift then
        begin
          if FPickList.Visible then
            CloseUp(True)
          else
            DropDown;
          Key := 0;
        end;
      VK_RETURN, VK_ESCAPE:
        if FPickList.Visible and not (ssAlt in Shift) then
        begin
          CloseUp(Key = VK_RETURN);
          Key := 0;
        end;
    end;
  end;

begin
  case Message.Msg of
    WM_KeyDown, WM_SysKeyDown, WM_Char:
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and FPickList.Visible then
        begin
          with TMessage(Message) do
            SendMessage(FPickList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;

  inherited;
end;

end.
