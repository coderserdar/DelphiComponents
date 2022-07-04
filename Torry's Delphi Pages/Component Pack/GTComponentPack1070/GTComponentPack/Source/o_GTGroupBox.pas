{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       GT GroupBox with CheckBox                       }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}

unit o_GTGroupBox;

interface
uses
   Classes
  ,Controls
  ,StdCtrls
  ,Messages
  ;
{$I ../../GTDIRECTIVES.inc}

type
  TgtGroupBox = class(TCustomControl)
  private
    { Private declarations }
    FCheckBox : TCheckBox;
    FOnCheckBoxClick: TNotifyEvent;
    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    { Protected declarations }
    procedure InternalOnCheckBoxClick(Sender : TObject);
    procedure SetParent(AParent: TWinControl);override;
    procedure EnableControls(Enable : Boolean);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    function  GetProperLeftForText:Integer;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  published
    property Checked : Boolean read GetChecked write SetChecked default True;
    property Caption : string  read GetCaption write SetCaption;
  published
    property OnCheckBoxClick : TNotifyEvent read FOnCheckBoxClick write FOnCheckBoxClick;
  published
    { Published declarations}
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {$IFNDEF DELPHI6}
      property ParentBackground default True;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
uses
   Windows
  {$IFNDEF DELPHI6}
   ,Themes
  {$ENDIF}
  ,Graphics
  ;


{ TgtGroupBox }
{------------------------------------------------------------------------------}
constructor TgtGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF DELPHI6}
    ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
      csSetCaption, csDoubleClicks, csReplicatable, csParentBackground];
  {$ELSE}
    ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
      csSetCaption, csDoubleClicks, csReplicatable];
  {$ENDIF}
  Width  := 185;
  Height := 105;
  FCheckBox         := TCheckBox.Create(Self);
  FCheckBox.Caption := '';
  FCheckBox.Width   := 17;
  FCheckBox.Left    := 5;
  FCheckBox.Top     := 0;
  FCheckBox.OnClick := InternalOnCheckBoxClick;
  FCheckBox.Checked := True;
  Self.BevelInner   := bvLowered;
end;
{------------------------------------------------------------------------------}
destructor TgtGroupBox.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.CreateParams(var Params: TCreateParams);
begin
  //Taken from CreateParams StdCtrls.pas TCustomGroupBox CreateParams.
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.InternalOnCheckBoxClick(Sender: TObject);
begin
  EnableControls(TCheckBox(Sender).Checked);
  if Assigned(FOnCheckBoxClick) then
    FOnCheckBoxClick(Self);
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.EnableControls(Enable: Boolean);
var
  i : Integer;
begin
  for i:=0 to Pred(Self.ControlCount) do
  begin
    if Self.Controls[i] <> FCheckBox then
      Self.Controls[i].Enabled := Enable;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF DELPHI6}
procedure TgtGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  CaptionRect,
  OuterRect: TRect;
  Size: TSize;
  Box: TThemedButton;
  Details: TThemedElementDetails;
begin
  //Taken from StdCtrls.pas TCustomGroupBox paint.
  //Modified by George Trifidis.
  with Canvas do
  begin
    Font := Self.Font;
    if ThemeServices.ThemesEnabled then
    begin
      if Text <> '' then
      begin
        GetTextExtentPoint32(Handle, PChar(Text), Length(Text), Size);
        CaptionRect := Rect(GetProperLeftForText, 0, Size.cx + GetProperLeftForText , Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect,8, 0)
        else
          OffsetRect(CaptionRect,(Width - 8 - CaptionRect.Right), 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(Handle, Left, Top, Right, Bottom);
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := ThemeServices.GetElementDetails(Box);
      ThemeServices.DrawElement(Handle, Details, OuterRect);

      SelectClipRgn(Handle, 0);
      if Text <> '' then
        ThemeServices.DrawText(Handle, Details, Text, CaptionRect, DT_LEFT, 0);
    end
    else
    begin
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end
      else
        Brush.Color := clWindowFrame;
      FrameRect(R);
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(GetProperLeftForText, 0, 0, H)
        else
          R := Rect(GetProperLeftForText, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
        Brush.Color := Color;
        DrawText(Handle, PChar(Text), Length(Text), R, Flags);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
{$ELSE}
procedure TgtGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
begin
  //Taken from StdCtrls.pas TCustomGroupBox paint.
  //Modified by George Trifidis.
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);
    if Ctl3D then
    begin
      Inc(R.Left);
      Inc(R.Top);
      Brush.Color := clBtnHighlight;
      FrameRect(R);
      OffsetRect(R, -1, -1);
      Brush.Color := clBtnShadow;
    end else
      Brush.Color := clWindowFrame;
    FrameRect(R);
    if Text <> '' then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(GetProperLeftForText, 0, 0, H)
      else                         
        R := Rect(GetProperLeftForText, 0,0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      DrawText(Handle, PChar(Text), Length(Text), R, Flags);
    end;
  end;
end;
{$ENDIF}
{-----------------------------------------------------------------------------}
function TgtGroupBox.GetProperLeftForText: Integer;
begin
  Result := FCheckBox.Left + FCheckBox.Width;
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
{------------------------------------------------------------------------------}




//Getters - Setters\\
{------------------------------------------------------------------------------}
function TgtGroupBox.GetChecked: Boolean;
begin
  Result := FCheckBox.Checked;
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.SetChecked(const Value: Boolean);
begin
  FCheckBox.Checked := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(AParent) then
  begin
    FCheckBox.Parent  := Self;
  end;
end;
{------------------------------------------------------------------------------}
function TgtGroupBox.GetCaption: string;
begin
  Result := Self.Text;
end;
{------------------------------------------------------------------------------}
procedure TgtGroupBox.SetCaption(const Value: string);
begin
  Self.Text := Value;
  Invalidate;
end;
{------------------------------------------------------------------------------}
















end.
