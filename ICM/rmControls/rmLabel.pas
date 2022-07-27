{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmLabel
Purpose  : This label has extra functionality for UI eye-candy.
Date     : 07-09-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmLabel;

interface

{$I CompilerDefines.INC}

uses Messages, Windows, SysUtils, Classes, Controls, forms, Graphics, extctrls, dialogs;

type
  TrmBorderStyle = (rmbsNone, rmbsSingle, rmbsSunken, rmbsRaised, rmbsSunkenEdge, rmbsRaisedEdge);
  TrmTextStyle = (rmtsNormal, rmtsRaised, rmtsLowered, rmtsShadow);
  TrmTextLayout = (rmtlTop, rmtlCenter, rmtlBottom);
  TrmGradientLayout = (rmglTopDown, rmglLeftRight);

  TrmCustomLabel = class;

  TrmLMouseOptions = class(TPersistent)
  private
    { Private declarations }
    fenabled: boolean;
    FOwner: TrmCustomLabel;
    FEnterColor: TColor;
    FEnterBorderStyle: TrmBorderStyle;
    FEnterTextStyle: TrmTextStyle;
  protected
    { Protected declarations }
    procedure SetEnterColor(Value: TColor);
    procedure SetEnterBorderStyle(Value: TrmBorderStyle);
    procedure SetEnterTextStyle(Value: TrmTextStyle);
  public
    { Public declarations }
    constructor Create(AOwner: TrmCustomLabel); virtual;
  published
    { Published declarations }
    property EnterColor: TColor read FEnterColor write SetEnterColor default clWindowText;
    property EnterBorder: TrmBorderStyle read fEnterBorderStyle write SetEnterBorderStyle default rmbsNone;
    property EnterTextStyle: TrmTextStyle read FEnterTextStyle write SetEnterTextStyle default rmtsNormal;
    property Enabled: boolean read fEnabled write fenabled default false;
  end; { TCompanyText }

  TrmCustomLabel = class(TGraphicControl)
  private
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;
    fMouseOptions: TrmLMouseOptions;
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    fdoublebuffered: boolean;
    fbuffer: TBitmap;
    fThickBorder: boolean;
    FLayout: TrmTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    fGradTLColor, fGradBRColor: TColor;
    fShadowColor: TColor;
    fShadowdepth: integer;
    fGradDir: TrmGradientLayout;
    fBorderStyle: TrmBorderStyle;
    fUseGradient: Boolean;
    ftextstyle: TrmTextStyle;
    fnormalcolor: tcolor;
    fnormaltext: TrmTextStyle;
    fnormalborder: TrmBorderstyle;
    {$IFNDEF D6_or_higher}
    procedure AdjustBounds;
    {$ENDIF}
    function DrawBorder(canvas: TCanvas): trect;
    procedure DoDrawText(canvas: TCanvas; var Rect: TRect; Flags: Word);
    function GetTransparent: Boolean;
    function GetBorderWidth: integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TrmTextLayout);
    procedure SetWordWrap(Value: Boolean);
    procedure SetBorderStyle(Value: TrmBorderStyle);
    procedure SetThickBorder(Value: Boolean);
    procedure SetTextStyle(Value: TrmTextStyle);
    procedure SetGradient(Value: Boolean);
    procedure SetTLColor(value: TColor);
    procedure SetBRColor(value: TColor);
    procedure SetGradientDirection(value: TrmGradientLayout);
    procedure GradientFill(canvas: TCanvas; R: TRect);
    procedure setshadowcolor(value: tcolor);
    procedure setshadowdepth(value: integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseEnter(var Message: TCMEnter); message CM_MouseEnter;
    procedure CMMouseLeave(var Message: TCMExit); message CM_MouseLeave;
  protected
    procedure Draw3DText(canvas: TCanvas; var Rect: TRect; Flags: Word; Raised: boolean);
    function GetLabelText: string; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    {$IFNDEF D6_or_higher}
    procedure SetAutoSize(Value: Boolean); virtual;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    {$ENDIF}
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property DoubleBuffered: Boolean read fdoublebuffered write fdoublebuffered default true;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TrmTextLayout read FLayout write SetLayout default rmtlTop;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property BorderStyle: TrmBorderStyle read fBorderStyle write SetBorderStyle default rmbsNone;
    property UseGradient: Boolean read fuseGradient write SetGradient default false;
    property TextStyle: TrmTextStyle read ftextstyle write settextstyle default rmtsNormal;
    property GradientTLColor: TColor read fGradTLColor write SetTLColor default clbtnshadow;
    property GradientBRColor: TColor read fGradBRColor write SetBRColor default clbtnface;
    property GradientDirection: TrmGradientLayout read fGradDir write SetGradientDirection default rmglLeftRight;
    property MouseOptions: TrmLMouseOptions read fmouseoptions write fmouseoptions stored true;
    property ThickBorder: Boolean read fthickborder write setthickborder default false;
    property Borderwidth: integer read GetBorderWidth;
    property ShadowColor: tcolor read fshadowcolor write setShadowColor default clbtnshadow;
    property ShadowDepth: integer read fshadowdepth write setshadowdepth default 4;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    property Canvas;
  end;

  TrmLabel = class(TrmCustomLabel)
  public
    property Borderwidth;
  published
    property Align;
    property Alignment;
    property AutoSize;
    property DoubleBuffered;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property BorderStyle;
    property UseGradient;
    property TextStyle;
    property ThickBorder;
    property GradientTLColor;
    property GradientBRColor;
    property GradientDirection;
    property shadowcolor;
    property shadowdepth;
    property MouseOptions;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses rmLibrary;

{ TrmLMouseOptions }

constructor TrmLMouseOptions.Create(AOwner: TrmCustomLabel);
begin
  inherited create;
  FOwner := aowner;
  fenabled := false;
  fEnterColor := clWindowText;
  fEnterBorderStyle := rmbsNone;
  fEnterTextStyle := rmtsNormal;
end;

procedure TrmLMouseOptions.SetEnterColor(Value: TColor);
begin
  fEnterColor := value;
  fowner.Invalidate;
end;

procedure TrmLMouseOptions.SetEnterBorderStyle(Value: TrmBorderStyle);
begin
  fEnterBorderStyle := value;
  fowner.Invalidate;
end;

procedure TrmLMouseOptions.SetEnterTextStyle(Value: TrmTextStyle);
begin
  fEnterTextStyle := value;
  fowner.Invalidate;
end;

{ TrmCustomLabel }

constructor TrmCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  fMouseOptions := TrmLMouseOptions.create(self);
  fdoublebuffered := true;
  fbuffer := tbitmap.create;
  Width := 65;
  Height := 17;
  fshadowdepth := 4;
  fshadowcolor := clbtnshadow;
  FAutoSize := True;
  FShowAccelChar := True;
  fGradTLColor := clbtnshadow;
  fGradBRColor := clbtnface;
  fGradDir := rmglLeftRight;
  fBorderStyle := rmbsNone;
  fUseGradient := false;
  ftextstyle := rmtsNormal;
end;

destructor TrmCustomLabel.destroy;
begin
  fmouseoptions.free;
  fbuffer.free;
  inherited;
end;

function TrmCustomLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure TrmCustomLabel.Draw3DText(canvas: TCanvas; var Rect: TRect; Flags: Word; Raised: boolean);
var
  top, bottom: tcolor;
begin
  if raised then
  begin
    top := clBtnShadow;
    bottom := clBtnHighlight;
  end
  else
  begin
    top := clBtnHighlight;
    bottom := clBtnShadow;
  end;
  OffsetRect(Rect, 1, 1);

  Canvas.Font.Color := top;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, -2, -2);
  Canvas.Font.Color := bottom;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, 1, 1);
  Canvas.Font.color := font.color;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  inflaterect(rect, 1, 1);
  rect.right := rect.right + 1;
  rect.bottom := rect.bottom + 1;
end;

procedure TrmCustomLabel.DoDrawText(canvas: TCanvas; var Rect: TRect; Flags: Word);
var
  Text: string;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Canvas.Font := Font;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  end
  else
    case TextStyle of
      rmtsNormal: DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
      rmtsRaised: Draw3DText(canvas, rect, flags, true);
      rmtsLowered: Draw3DText(canvas, rect, flags, false);
      rmtsShadow:
        begin
          OffsetRect(Rect, fshadowdepth, fshadowdepth);
          Canvas.Font.Color := fshadowcolor;
          DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
          OffsetRect(Rect, -fshadowdepth, -fshadowdepth);
          Canvas.Font.Color := font.color;
          DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
          inflaterect(rect, fshadowdepth, fshadowdepth);
        end;
    end;
end;

procedure TrmCustomLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  WR, CalcRect: TRect;
  DrawStyle: Integer;
  bmp: TBitmap;
  Workcanvas: TCanvas;
begin
  bmp := nil;
  if fdoublebuffered then
  begin
    bmp := tbitmap.create;
    bmp.width := width;
    bmp.height := height;
    workcanvas := bmp.canvas
  end
  else
    workcanvas := canvas;
  with WorkCanvas do
  begin
    WR := DrawBorder(WorkCanvas);
    if not Transparent then
    begin
      if UseGradient then GradientFill(WorkCanvas, wr)
      else
      begin
        Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(WR);
      end;
    end
    else
    begin
      if fdoublebuffered then
      begin
        if font.color = clblack then
          bmp.transparentcolor := clred
        else
          font.color := clblue;
        bmp.transparent := true;
        Brush.color := bmp.transparentcolor;
        Brush.Style := bsSolid;
        fillRect(WR);
      end;
    end;
    Brush.Style := bsClear;
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> rmtlTop then
        begin
      CalcRect := WR;
      DoDrawText(WorkCanvas, CalcRect, DrawStyle or DT_CALCRECT);
      if FLayout = rmtlBottom then OffsetRect(WR, 0, Height - CalcRect.Bottom)
      else OffsetRect(WR, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawText(WorkCanvas, WR, DrawStyle);
  end;
  if (fdoublebuffered) and assigned(bmp) then
  begin
    canvas.Draw(0, 0, bmp);
    bmp.free;
  end;
end;

procedure TrmCustomLabel.Loaded;
begin
  inherited Loaded;
  {$ifndef D6_or_higher}
  AdjustBounds;
  {$endif}
end;

{$IFNDEF D6_or_higher}
procedure TrmCustomLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  WR: TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    wr := clientrect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(canvas, wr, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    if FAlignment = taRightJustify then Inc(X, Width - (wr.Right + BorderWidth));
    if (align in [altop, albottom, alclient]) then wr.right := width;
    if (align in [alRight, alLeft, alclient]) then wr.bottom := height;
    if (height <= wr.Bottom + borderwidth) or (width <= wr.Right + borderwidth) then
      SetBounds(X, Top, wr.Right + borderwidth, wr.Bottom + borderwidth);
  end;
end;
{$ENDIF}

procedure TrmCustomLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

{$IFNDEF D6_or_higher}
procedure TrmCustomLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;
{$ENDIF}

function TrmCustomLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TrmCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TrmCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TrmCustomLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TrmCustomLabel.SetLayout(Value: TrmTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TrmCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    {$ifndef D6_or_higher}
    AdjustBounds;
    {$endif}
    Invalidate;
  end;
end;

procedure TrmCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TrmCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  {$ifndef D6_or_higher}
  AdjustBounds;
  {$endif}
end;

procedure TrmCustomLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  {$ifndef D6_or_higher}
  AdjustBounds;
  {$endif}
end;

procedure TrmCustomLabel.CMDialogChar(var Message: TCMDialogChar);
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

procedure TrmCustomLabel.SetBorderStyle(Value: TrmBorderStyle);
begin
  if fBorderStyle <> value then fBorderStyle := value;
  {$ifndef D6_or_higher}
  Adjustbounds;
  {$endif}
  invalidate;
end;

procedure TrmCustomLabel.SetTextStyle(Value: TrmTextStyle);
begin
  if ftextstyle <> value then ftextstyle := value;
  {$ifndef D6_or_higher}
  AdjustBounds;
  {$endif}
  invalidate;
end;

procedure TrmCustomLabel.SetGradient(Value: Boolean);
begin
  if fUseGradient <> value then fUseGradient := value;
  invalidate;
end;

procedure TrmCustomLabel.SetTLColor(value: TColor);
begin
  if fGradTLColor <> value then fGradTLColor := value;
  invalidate;
end;

procedure TrmCustomLabel.SetBRColor(value: TColor);
begin
  if fGradBRColor <> value then fGradBRColor := value;
  invalidate;
end;

procedure TrmCustomLabel.SetGradientDirection(value: TrmGradientLayout);
begin
  if fGradDir <> value then fGradDir := value;
  invalidate;
end;

function TrmCustomLabel.DrawBorder(canvas: TCanvas): trect;
var
  Innertopcolor, Innerbottomcolor, Outertopcolor, Outerbottomcolor: TColor;
  wr: TRect;
begin
  wr := GetClientRect;
  result := wr;
  if BorderStyle = rmbsnone then exit;
  InnerTopColor := clBlack;
  InnerBottomColor := clblack;
  OuterTopColor := clblack;
  OuterBottomColor := clblack;
  case borderstyle of
    rmbsSunken:
      begin
        InnerTopColor := cl3ddkshadow;
        InnerBottomColor := cl3dlight;
        OuterTopColor := clBtnShadow;
        OuterBottomColor := clBtnhighlight;
      end;
    rmbsRaised:
      begin
        InnerTopColor := clBtnhighlight;
        InnerBottomColor := clBtnShadow;
        if thickborder then
        begin
          OuterTopColor := cl3dlight;
          OuterBottomColor := cl3ddkshadow;
        end
        else
        begin
          OuterTopColor := InnerTopColor;
          OuterBottomColor := InnerBottomColor;
        end
      end;
    rmbsRaisedEdge:
      begin
        InnerTopColor := clBtnShadow;
        InnerBottomColor := clBtnhighlight;
        OuterTopColor := clBtnhighlight;
        OuterBottomColor := clBtnShadow;
      end;
    rmbsSunkenEdge:
      begin
        InnerTopColor := clBtnhighlight;
        InnerBottomColor := clBtnShadow;
        OuterTopColor := clBtnShadow;
        OuterBottomColor := clBtnhighlight;
      end;
  end;
  frame3d(canvas, wr, Outertopcolor, Outerbottomcolor, 1);
  if (ThickBorder) or (borderstyle in [rmbsSunkenEdge, rmbsRaisedEdge]) then
    frame3d(canvas, wr, Innertopcolor, Innerbottomcolor, 1);
  result := wr;
end;

procedure TrmCustomLabel.GradientFill(canvas: TCanvas; R: TRect);
const
  fNumColors = 63;
var
  BeginRGBValue: array[0..2] of Byte;
  RGBDifference: array[0..2] of integer;
  ColorBand: TRect;
  I: Integer;
  Red: Byte;
  Green: Byte;
  Blue: Byte;
  Brush, OldBrush: HBrush;
begin
  BeginRGBValue[0] := GetRValue(ColorToRGB(fGradTLColor));
  BeginRGBValue[1] := GetGValue(ColorToRGB(fGradTLColor));
  BeginRGBValue[2] := GetBValue(ColorToRGB(fGradTLColor));

  RGBDifference[0] := GetRValue(ColorToRGB(fGradBRColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue(ColorToRGB(fGradBRColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue(ColorToRGB(fGradBRColor)) - BeginRGBValue[2];

  { Calculate the color band's top and bottom coordinates }
  { for Left To Right fills }
  if Gradientdirection = rmglLeftRight then
  begin
    ColorBand.Top := R.Top;
    ColorBand.Bottom := R.Bottom;
  end
  else
  begin
    ColorBand.Left := R.Left;
    ColorBand.Right := R.Right;
  end;
  { Perform the fill }
  for I := 0 to FNumColors - 1 do
  begin { iterate through the color bands }
    if Gradientdirection = rmglLeftRight then
    begin
      { Calculate the color band's left and right coordinates }
      ColorBand.Left := R.Left + MulDiv(I, R.Right - R.Left, FNumColors);
      ColorBand.Right := R.Left + MulDiv(I + 1, R.Right - R.Left, FNumColors);
    end
    else
    begin
      ColorBand.Top := R.Top + MulDiv(I, R.Bottom - R.Top, FNumColors);
      ColorBand.Bottom := R.Top + MulDiv(I + 1, R.Bottom - R.Top, FNumColors);
    end;

    { Calculate the color band's color }
    Red := BeginRGBValue[0] + MulDiv(I, RGBDifference[0], FNumColors - 1);
    Green := BeginRGBValue[1] + MulDiv(I, RGBDifference[1], FNumColors - 1);
    Blue := BeginRGBValue[2] + MulDiv(I, RGBDifference[2], FNumColors - 1);

    { Create a brush with the appropriate color for this band }
    Brush := CreateSolidBrush(RGB(Red, Green, Blue));
    { Select that brush into the temporary DC. }
    OldBrush := SelectObject(Canvas.handle, Brush);
    try
      { Fill the rectangle using the selected brush -- PatBlt is faster than FillRect }
      PatBlt(Canvas.handle, ColorBand.Left, ColorBand.Top, ColorBand.Right - ColorBand.Left, ColorBand.Bottom - ColorBand.Top, PATCOPY);
    finally
      { Clean up the brush }
      SelectObject(Canvas.handle, OldBrush);
      DeleteObject(Brush);
    end;
  end; { iterate through the color bands }
end; { GradientFill }

procedure TrmCustomLabel.CMMouseEnter(var Message: TCMEnter);
begin
  inherited;
  if fmouseoptions.enabled then
  begin
    fnormalborder := borderstyle;
    fnormalcolor := font.color;
    fnormaltext := textstyle;
    fborderstyle := fmouseoptions.EnterBorder;
    ftextstyle := fmouseoptions.EnterTextStyle;
    font.color := fmouseoptions.EnterColor;
  end;
  if assigned(fOnMouseEnter) then fOnMouseEnter(self);
end;

procedure TrmCustomLabel.CMMouseLeave(var Message: TCMExit);
begin
  inherited;
  if fmouseoptions.enabled then
  begin
    fborderstyle := fnormalborder;
    ftextstyle := fnormaltext;
    font.color := fNormalColor;
  end;
  if assigned(fOnMouseLeave) then fOnMouseLeave(self);
end;

procedure TrmCustomLabel.SetThickBorder(Value: Boolean);
begin
  if fthickborder <> value then fthickborder := value;
  invalidate;
end;

function TrmCustomLabel.GetBorderWidth: integer;
begin
  result := 0;
  case borderstyle of
    rmbsRaisedEdge,
      rmbsSunkenEdge: result := 2;
    rmbsSingle,
      rmbsRaised,
      rmbsSunken: result := 1;
  end;
  if (ThickBorder) and (result = 1) then result := 2;
end;

procedure TrmCustomLabel.SetShadowColor(value: tcolor);
begin
  fshadowcolor := value;
  invalidate;
end;

procedure TrmCustomLabel.SetShadowDepth(value: integer);
begin
  fshadowdepth := value;
  invalidate;
end;

end.

