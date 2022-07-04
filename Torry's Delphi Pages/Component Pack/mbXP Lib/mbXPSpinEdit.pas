unit mbXPSpinEdit;

interface

uses
 Windows, Classes, StdCtrls, Controls, Messages, SysUtils, mbXPSpin, dialogs;

type
  TmbXPCustomSpinEdit = class(TCustomEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FEditorEnabled: Boolean;
    FDefaultValue: integer;
    FEmpty, FAllowEmpty: boolean;

    procedure SetEmpty(e: boolean);
    procedure SetAllowEmpty(a: boolean);
    function GetMinHeight: Integer;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
  protected
    FButton: TmbXPSpinButton;

    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetDefaultValue(v: integer);
    procedure SetValue (NewValue: LongInt);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SpinChanged(Sender: TObject);
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Empty: boolean read FEmpty write SetEmpty default false;
    property AllowEmpty: boolean read FAllowEmpty write SetAllowEmpty default false;
    property Button: TmbXPSpinButton read FButton;
    property Value: LongInt read GetValue write SetValue;
    property DefaultValue: integer read FDefaultValue write SetDefaultValue default 0;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
  end;

  TmbXPSpinEdit = class(TmbXPCustomSpinEdit)
  published
    property Text;
    property Button;
    property Value;
    property AllowEmpty;
    property Empty;
    property DefaultValue;
    property EditorEnabled;
    property Increment;
    property MaxValue;
    property MinValue;
    property Enabled;
    property Font;
    property MaxLength default 9;
    property AutoSelect;
    property AutoSize;
    property Color;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
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
    property OnStartDrag;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{$DEFINE mbXP_Lib}

constructor TmbXPCustomSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TmbXPSpinButton.Create(Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
  OnChange := SpinChanged;
  FDefaultValue := 0;
  MaxLength := 9;
  FAllowEmpty := false;
  FEmpty := false;
end;

destructor TmbXPCustomSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TmbXPCustomSpinEdit.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 if FButton <> nil then
  FButton.Enabled := Enabled;
end;

procedure TmbXPCustomSpinEdit.SpinChanged(Sender: TObject);
begin
 Modified := true;
end;

procedure TmbXPCustomSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TmbXPCustomSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then UpClick (Self)
  else if Key = VK_DOWN then DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TmbXPCustomSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TmbXPCustomSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (Key in [DecimalSeparator, '+', '-', '0'..'9']) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TmbXPCustomSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TmbXPCustomSpinEdit.SetAllowEmpty(a: boolean);
begin
 FAllowEmpty := a;
 SetEmpty(FEmpty);
end;

procedure TmbXPCustomSpinEdit.SetEmpty(e: boolean);
begin
 FEmpty := e;
 if FAllowEmpty then
  begin
   if FEmpty then
    Text := ''
   else
    if Text = '' then
     Text := IntToStr(FDefaultValue);
  end
 else
  if Text = '' then
   Text := IntToStr(FDefaultValue);
end;

procedure TmbXPCustomSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  if FEmpty and FAllowEmpty then Text := '';
end;

procedure TmbXPCustomSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc := Rect(0, 0, ClientWidth - FButton.Width, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
end;

procedure TmbXPCustomSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 3, 0, FButton.Width, Height - 3)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TmbXPCustomSpinEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TmbXPCustomSpinEdit.UpClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else
   begin
    if Text = '' then Text := IntToStr(FDefaultValue);
    Value := Value + FIncrement;
   end;
end;

procedure TmbXPCustomSpinEdit.DownClick (Sender: TObject);
begin
  if ReadOnly then MessageBeep(0)
  else
   begin
    if Text = '' then Text := IntToStr(FDefaultValue);
    Value := Value - FIncrement;
   end;
end;

procedure TmbXPCustomSpinEdit.WMPaste(var Message: TWMPaste);   
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TmbXPCustomSpinEdit.WMCut(var Message: TWMPaste);   
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TmbXPCustomSpinEdit.CMExit(var Message: TCMExit);
var
 FWasEmpty: boolean;
begin
  inherited;
  FWasEmpty := false;
  if Text = '' then
   begin
    Text := IntToStr(FDefaultValue);
    FWasEmpty := true;
   end;
  if CheckValue(Value) <> Value then
   SetValue(Value);
  if FEmpty and FAllowEmpty and FWasEmpty then
   Text := '';
end;

function TmbXPCustomSpinEdit.GetValue: LongInt;
begin
  try
   if FAllowEmpty then
    begin
     if Text <> '' then
      Result := StrToInt(Text)
     else
      Result := FDefaultValue;
    end
   else
    Result := StrToInt(Text);
  except
    Result := FMinValue;
  end;
end;

procedure TmbXPCustomSpinEdit.SetValue (NewValue: LongInt);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

function TmbXPCustomSpinEdit.CheckValue(NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TmbXPCustomSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

procedure TmbXPCustomSpinEdit.SetDefaultValue(v: integer);
begin
 FDefaultValue := v;
 if FDefaultValue < FMinValue then FDefaultValue := FMinValue;
 if FDefaultValue > FMaxValue then FDefaultValue := FMaxValue;
 SetValue(v);
end;

end.
