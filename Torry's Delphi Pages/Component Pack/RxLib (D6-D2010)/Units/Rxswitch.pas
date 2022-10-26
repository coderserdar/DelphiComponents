{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit RXSwitch;

interface

{$I RX.INC}

uses
  SysUtils, Windows,
  Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Menus;

type

{ TRxSwitch }

  TTextPos = (tpNone, tpLeft, tpRight, tpAbove, tpBelow);
  TSwitchBitmaps = set of Boolean;

  TRxSwitch = class(TCustomControl)
  private
    FActive: Boolean;
    FBitmaps: array[Boolean] of TBitmap;
    FDisableBitmaps: array[Boolean] of TBitmap;
    FOnOn: TNotifyEvent;
    FOnOff: TNotifyEvent;
    FStateOn: Boolean;
    FTextPosition: TTextPos;
    FBorderStyle: TBorderStyle;
    FToggleKey: TShortCut;
    FShowFocus: Boolean;
    FUserBitmaps: TSwitchBitmaps;
    procedure GlyphChanged(Sender: TObject);
    procedure SetStateOn(Value: Boolean);
    procedure SetTextPosition(Value: TTextPos);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetSwitchGlyph(Index: Integer): TBitmap;
    procedure SetSwitchGlyph(Index: Integer; Value: TBitmap);
    function StoreBitmap(Index: Integer): Boolean;
    procedure SetShowFocus(Value: Boolean);
    procedure CreateDisabled(Index: Integer);
    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure DoOn; dynamic;
    procedure DoOff; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ToggleSwitch;
  published
    property Align;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsNone;
    property Caption;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property GlyphOff: TBitmap index 0 read GetSwitchGlyph write SetSwitchGlyph
      stored StoreBitmap;
    property GlyphOn: TBitmap index 1 read GetSwitchGlyph write SetSwitchGlyph
      stored StoreBitmap;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property ToggleKey: TShortCut read FToggleKey write FToggleKey
      default VK_SPACE;
    property ShowHint;
    property StateOn: Boolean read FStateOn write SetStateOn default False;
    property TabOrder;
    property TabStop default True;
    property TextPosition: TTextPos read FTextPosition write SetTextPosition
      default tpNone;
{$IFDEF RX_D4}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
    property OnOn: TNotifyEvent read FOnOn write FOnOn;
    property OnOff: TNotifyEvent read FOnOff write FOnOff;
  end;

implementation

uses
  rxVCLUtils;

{$R *.R32}

const
  ResName: array [Boolean] of PChar = ('SWITCH_OFF', 'SWITCH_ON');
  BorderStyles: array[TBorderStyle] of Longint = (0, WS_BORDER);

{ TRxSwitch component }

constructor TRxSwitch.Create(AOwner: TComponent);
var
  I: Byte;
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  Width := 50;
  Height := 60;
  for I := 0 to 1 do
  begin
    FBitmaps[Boolean(I)] := TBitmap.Create;
    SetSwitchGlyph(I, nil);
    FBitmaps[Boolean(I)].OnChange := GlyphChanged;
  end;
  FUserBitmaps := [];
  FShowFocus := True;
  FStateOn := False;
  FTextPosition := tpNone;
  FBorderStyle := bsNone;
  FToggleKey := VK_SPACE;
  TabStop := True;
end;

destructor TRxSwitch.Destroy;
var
  I: Byte;
begin
  for I := 0 to 1 do
  begin
    FBitmaps[Boolean(I)].OnChange := nil;
    FDisableBitmaps[Boolean(I)].Free;
    FBitmaps[Boolean(I)].Free;
  end;
  inherited Destroy;
end;

procedure TRxSwitch.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style or CS_HREDRAW or CS_VREDRAW;
    Style := Style or Longword(BorderStyles[FBorderStyle]);
  end;
end;

procedure TRxSwitch.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := FUserBitmaps <> TRxSwitch(Filer.Ancestor).FUserBitmaps
    else
      Result := FUserBitmaps <> [];
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData, DoWrite);
end;

function TRxSwitch.GetPalette: HPALETTE;
begin
  if Enabled then
    Result := FBitmaps[FStateOn].Palette else Result := 0;
end;

procedure TRxSwitch.ReadBinaryData(Stream: TStream);
begin
  Stream.ReadBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

procedure TRxSwitch.WriteBinaryData(Stream: TStream);
begin
  Stream.WriteBuffer(FUserBitmaps, SizeOf(FUserBitmaps));
end;

function TRxSwitch.StoreBitmap(Index: Integer): Boolean;
begin
  Result := Boolean(Index) in FUserBitmaps;
end;

function TRxSwitch.GetSwitchGlyph(Index: Integer): TBitmap;
begin
  if csLoading in ComponentState then
    Include(FUserBitmaps, Boolean(Index));
  Result := FBitmaps[Boolean(Index)]
end;

procedure TRxSwitch.CreateDisabled(Index: Integer);
begin
  if FDisableBitmaps[Boolean(Index)] <> nil then
    FDisableBitmaps[Boolean(Index)].Free;
  try
    FDisableBitmaps[Boolean(Index)] :=
      CreateDisabledBitmap(FBitmaps[Boolean(Index)], clBlack);
  except
    FDisableBitmaps[Boolean(Index)] := nil;
    raise;
  end;
end;

procedure TRxSwitch.GlyphChanged(Sender: TObject);
var
  I: Boolean;
begin
  for I := False to True do
    if Sender = FBitmaps[I] then
      CreateDisabled(Ord(I));
  Invalidate;
end;

procedure TRxSwitch.SetSwitchGlyph(Index: Integer; Value: TBitmap);
begin
  if Value <> nil then
  begin
    FBitmaps[Boolean(Index)].Assign(Value);
    Include(FUserBitmaps, Boolean(Index));
  end
  else
  begin
    FBitmaps[Boolean(Index)].Handle := LoadBitmap(HInstance,
      ResName[Boolean(Index)]);
    Exclude(FUserBitmaps, Boolean(Index));
  end;
end;

procedure TRxSwitch.CMFocusChanged(var Message: TCMFocusChanged);
var
  Active: Boolean;
begin
  with Message do
    Active := (Sender = Self);
  if Active <> FActive then
  begin
    FActive := Active;
    if FShowFocus then
      Invalidate;
  end;
  inherited;
end;

procedure TRxSwitch.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TRxSwitch.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TRxSwitch.CMDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    Message.Result := 1;
  end;
end;

procedure TRxSwitch.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if TabStop and CanFocus then
      SetFocus;
    ToggleSwitch;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if FToggleKey = ShortCut(Key, Shift) then
  begin
    ToggleSwitch;
    Key := 0;
  end;
end;

procedure TRxSwitch.Paint;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  FontHeight: Integer;

  procedure DrawBitmap(Bmp: TBitmap);
  var
    TmpImage: TBitmap;
    IWidth, IHeight, X, Y: Integer;
    IRect: TRect;
  begin
    IWidth := Bmp.Width;
    IHeight := Bmp.Height;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage := TBitmap.Create;
    try
      TmpImage.Width := IWidth;
      TmpImage.Height := IHeight;
      TmpImage.Canvas.Brush.Color := Self.Brush.Color;
      TmpImage.Canvas.BrushCopy(IRect, Bmp, IRect, Bmp.TransparentColor);
      X := 0; Y := 0;
      case FTextPosition of
        tpNone:
          begin
            X := ((Width - IWidth) div 2);
            Y := ((Height - IHeight) div 2);
          end;
        tpLeft:
          begin
            X := Width - IWidth;
            Y := ((Height - IHeight) div 2);
            Dec(ARect.Right, IWidth);
          end;
        tpRight:
          begin
            X := 0;
            Y := ((Height - IHeight) div 2);
            Inc(ARect.Left, IWidth);
          end;
        tpAbove:
          begin
            X := ((Width - IWidth) div 2);
            Y := Height - IHeight;
            Dec(ARect.Bottom, IHeight);
          end;
        tpBelow:
          begin
            X := ((Width - IWidth) div 2);
            Y := 0;
            Inc(ARect.Top, IHeight);
          end;
      end;
      Canvas.Draw(X, Y, TmpImage);
      if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
        Canvas.DrawFocusRect(Rect(X, Y, X + IWidth, Y + IHeight));
    finally
      TmpImage.Free;
    end;
  end;

begin
  ARect := GetClientRect;
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Color := Self.Color;
    FillRect(ARect);
    if not Enabled and (FDisableBitmaps[FStateOn] <> nil) then
      DrawBitmap(FDisableBitmaps[FStateOn])
    else
      DrawBitmap(FBitmaps[FStateOn]);
    if FTextPosition <> tpNone then
    begin
      FontHeight := TextHeight('W');
      with ARect do
      begin
        Top := ((Bottom + Top) - FontHeight) shr 1;
        Bottom := Top + FontHeight;
      end;
      StrPCopy(Text, Caption);
      Windows.DrawText(Handle, Text, StrLen(Text), ARect, DT_EXPANDTABS or
        DT_VCENTER or DT_CENTER);
    end;
  end;
end;

procedure TRxSwitch.DoOn;
begin
  if Assigned(FOnOn) then
    FOnOn(Self);
end;

procedure TRxSwitch.DoOff;
begin
  if Assigned(FOnOff) then
    FOnOff(Self);
end;

procedure TRxSwitch.ToggleSwitch;
begin
  StateOn := not StateOn;
end;

procedure TRxSwitch.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRxSwitch.SetStateOn(Value: Boolean);
begin
  if FStateOn <> Value then
  begin
    FStateOn := Value;
    Invalidate;
    if Value then
      DoOn
    else
      DoOff;
  end;
end;

procedure TRxSwitch.SetTextPosition(Value: TTextPos);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TRxSwitch.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if not (csDesigning in ComponentState) then
      Invalidate;
  end;
end;

end.