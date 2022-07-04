{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  Эта компонента - это аналог TLabel, но с возможностью
  привязать к нему картинку и при наведении курсора на
  Label или Image будет производиться их подсветка.
}
unit SMHLLbl;

interface

uses
  Windows, Messages, Classes, Graphics, Controls;

type
  TState = (stEnter, stLeave);
  TSMAlignment = (taLeftJustify, taRightJustify);
  TTextLayout = (tlTop, tlCenter, tlBottom);

  THighLightLabel = class(TGraphicControl)
  private
    { Private declarations }
    FState: TState;
    FPicActive: TPicture;
    FPicPassive: TPicture;
    FFontActive: TFont;
    FFontPassive: TFont;

    FFocusControl: TWinControl;
    FAlignment: TSMAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    procedure AdjustBounds;
    procedure DoDrawText(var ARect: TRect; Flags: Word);
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TSMAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);
    procedure SetWordWrap(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;

    procedure SetPictureActive(Value: TPicture);
    procedure SetPicturePassive(Value: TPicture);
    procedure SetFontActive(Value: TFont);
    procedure SetFontPassive(Value: TFont);
    procedure SetState(Value: TState);
  protected
    { Protected declarations }
    function GetLabelText: string; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetHLAutoSize(Value: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

    property Canvas;
    property State: TState read FState write SetState;
  published
    { Published declarations }
    property FontActive: TFont read FFontActive write SetFontActive;
    property FontPassive: TFont read FFontPassive write SetFontPassive;
    property PicActive: TPicture read FPicActive write SetPictureActive;
    property PicPassive: TPicture read FPicPassive write SetPicturePassive;

    property Alignment: TSMAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetHLAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property Align;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
{$R *.RES}
uses SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [THighLightLabel]);
end;

{ THighLightLabel }

constructor THighLightLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;

  FFontActive := TFont.Create;
  FFontActive.Assign(Font);
  FFontPassive := TFont.Create;
  FFontPassive.Assign(Font);

  FPicActive := TPicture.Create;
  FPicActive.Bitmap.TransparentColor := clSilver;
  FPicActive.Bitmap.Transparent := True;
  FPicActive.Bitmap.Handle := LoadBitmap(hInstance, 'PICENTER');

  FPicPassive := TPicture.Create;
  FPicPassive.Bitmap.TransparentColor := clSilver;
  FPicPassive.Bitmap.Transparent := True;
  FPicPassive.Bitmap.Handle := LoadBitmap(hInstance, 'PICLEAVE');

  FState := stLeave;
end;

destructor THighLightLabel.Destroy;
begin
  FFontActive.Free;
  FFontPassive.Free;

  FPicActive.Free;
  FPicPassive.Free;

  inherited Destroy;
end;

function THighLightLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure THighLightLabel.DoDrawText(var ARect: TRect; Flags: Word);
var
  Text: string;
  BRect: TRect;
  intLeft, intHeight: Integer;
  FPicture: TPicture;
begin
  if FState = stEnter then
  begin
    FPicture := FPicActive;
    Canvas.Font := FFontActive;
  end
  else
  begin
    FPicture := FPicPassive;
    Canvas.Font := FFontPassive;
  end;

  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  if not Enabled then
  begin
    OffsetRect(ARect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
    OffsetRect(ARect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
  end
  else
  begin
    intLeft := 0;
    intHeight := 0;
    case FAlignment of
      taLeftJustify: begin
                       intLeft := 0;
                       ARect.Left := ARect.Left + FPicture.Width + 5;
                       intHeight := DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
                       ARect.Left := ARect.Left - FPicture.Width;
                     end;
      taRightJustify: begin
                        intLeft := Width - FPicture.Width;
                        ARect.Right := ARect.Right - FPicture.Width - 5;
                        intHeight := DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags);
                        ARect.Right := ARect.Right + FPicture.Width;
                      end;
    end;

    if Flags and DT_CALCRECT = 0 then
    begin
      BRect := Bounds(intLeft, ARect.Top + (intHeight - FPicture.Height) div 2, FPicture.Width, FPicture.Height);
      Canvas.StretchDraw(BRect, FPicture.Graphic);
    end;
  end;
end;

procedure THighLightLabel.Paint;
const
  Alignments: array[TSMAlignment] of Word = (DT_LEFT, DT_RIGHT);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, CalcRect: TRect;
  DrawStyle: Integer;
begin
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> tlTop then
    begin
      CalcRect := Rect;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = tlBottom then
        OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
  end;
end;

procedure THighLightLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure THighLightLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if FAlignment = taRightJustify then
      Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure THighLightLabel.SetAlignment(Value: TSMAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure THighLightLabel.SetHLAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

function THighLightLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure THighLightLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure THighLightLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure THighLightLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure THighLightLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure THighLightLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure THighLightLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure THighLightLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure THighLightLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

function IsAccel(VK: Word; const Str: string): Boolean;
var
  P: Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (AnsiCompareText(Str[P + 1], Char(VK)) = 0);
end;

procedure THighLightLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure THighLightLabel.SetFontActive(Value: TFont);
begin
  if FFontActive <> nil then
  begin
    FFontActive.Assign(Value);
    Invalidate;
  end;
end;

procedure THighLightLabel.SetFontPassive(Value: TFont);
begin
  if FFontPassive <> nil then
  begin
    FFontPassive.Assign(Value);
    Invalidate;
  end;
end;

procedure THighLightLabel.SetPictureActive(Value: TPicture);
begin
  FPicActive.Assign(FPicActive.Graphic);
  if Assigned(FPicActive.Graphic) then
    FPicActive.Graphic.Transparent := True;
end;

procedure THighLightLabel.SetPicturePassive(Value: TPicture);
begin
  FPicPassive.Assign(Value);
  if Assigned(FPicPassive.Graphic) then
    FPicPassive.Graphic.Transparent := True;
end;

procedure THighLightLabel.SetState(Value: TState);
begin
  if (FState <> Value) then
  begin
    FState := Value;
    Invalidate;
  end;
end;

procedure THighLightLabel.CMMouseEnter(var Msg: TMessage);
begin
  //Change color when mouse is over label
  if (FState = stLeave) then
    State := stEnter;
end;

procedure THighLightLabel.CMMouseLeave(var Msg: TMessage);
begin
  //Change color when mouse leaves label
  if (FState = stEnter) then
    State := stLeave;
end;

end.
