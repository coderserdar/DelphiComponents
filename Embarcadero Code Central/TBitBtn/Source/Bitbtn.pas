{*******************************************************************************
 * Name       : BitBtn Component
 * Version    : 2.50
 * Author     : Oliver Killguss
 * Copyright  : ©2000-2005 by Oliver Killguss
 *              All rights reserved
 *
 *
 * RELEASE NOTICE
 * ==============
 * Sorry, Kylix are currently not supported!
 *
 * DESCRIPTION
 * ===========
 * This control is a flat styled button for all Delphi versions which
 * Includes automatically grayscale convert to get hot image effect.
 * This component saves time on designing button bars, because
 * you don't need to handle different images (colorized and
 * grayed) to get the hot image effect, this will be done by
 * this component.
 *
 * If you're handling large imagelists for designing button bar's you
 * can use GrayImageList component. For more details see GrayImageList
 * also available at www.vclcomponents.com !
 *
 * KYLIX SUPPORT IS IN PREPARATION
 *
 * HISTORY TBitBtn
 * ===============
 * 06.01.2006 - 2.50 - BDS2006 - Delphi 2005 support some minor changes
 *                     Property Version has removed
 *                     Improvements to include file DDM.inc
 * 08.01.2003 - 2.30 - CustomDDMBitBtn introduced, Property Version added,
 *                     bug fixes: mouse move on disabled containers
 * 27.10.2002 - 2.20 - Delphi 7 support. Resource bug fixed some minor changes
 * 26.03.2002 - 2.11 - If enabled is set to false caption will displayed grayed
 * 03.03.2002 - 2.10 - Anchors and Action properties added. Resource files reoganized
 *                     OnChangeCaption removed. New improved Win-Messaging support.
 *                     Some changes on handling resources.
 * 11.02.2002 - 2.01 - Bugfix: Now Delphi 1 compliant. New Delphi 1 resource included
 * 09.02.2002 - 2.00 - FINAL RELEASE! Some displaying bugs fixed, new
 *                     properties and events added. New improved demo included.
 * 04.02.2002 - 2.00 - Now TBitBtn inherits now from TGraphicControl
 *             (BETA)  Transparent mode bug fixed, NumGlyph and
 *                     transparent Glyph also supported. Now supports fonts.
 *                     Notice that this version currently supports only D1-D6.
 *                     Complete revised sources
 * 12.01.2002 - 1.30 - Bug fixed when layout is set to blGlyphRight
 *                     Timer based mouse event replaced
 *                     Demo is now also included
 * 13.12.2001 - 1.20 - Now support's Kylix 1/2
 * 04.10.2001 - 1.10 - Now also useable for Delphi 2-6
 * 27.08.2000 - 1.00 - Initial release for Delphi 1 with integrated grayscale
 *                     convert to generate hot image effect
 *
 *
 * HISTORY TBitButton
 * ==================
 * 10.01.1998 - 0.01 - Flat styled button for Delphi 2 (never released)
 *
 *
 * TODO (Known limitations)
 * ========================
 * GroupIndex and AllowAllUp properties are published but not supported now
 * (implementation is in progress now)
 *
 *
 * LICENSE
 * =======
 * This Product is Freeware and you can use "as is" and for
 * non commercial use is royalty free. Please remember to
 * give me some credits in your application.
 * For further details read license.txt shipped with this file.
 *
 *******************************************************************************}
unit bitbtn;
{-------------------------------------------------------------------------------}
interface
{-------------------------------------------------------------------------------}
{$ifdef win32}
  {$R *.D32}
{$else}
  {$R *.D16}
{$endif}
{-------------------------------------------------------------------------------}
{$I DDM.inc}
{-------------------------------------------------------------------------------}
uses
  {$ifdef LINUX}
    SysUtils, QMessages, Classes, QGraphics, QControls, QForms, QDialogs,
    QButtons, StdCtrls, ActnList, ImgList;
  {$else}
    {$ifdef D2_up}
      Wintypes, Winprocs, Messages, Classes, Graphics, Controls, Dialogs;
    {$else}
      Windows, Messages, Classes, Graphics, Controls, ActnList, ImgList
      , Forms, SysUtils;
    {$endif}
  {$endif}
{-------------------------------------------------------------------------------}
type
{-------------------------------------------------------------------------------}
  TGrayImageListStyle = (gisGray,gisSystemDeactive,gisBlue);
  TButtonLayout       = (blGlyphBottom, blGlyphLeft, blGlyphRight, blGlyphTop);
  TBevelWidth         = 1..10;
  TNumGlyphs          = 0..4;
{-------------------------------------------------------------------------------}
  TCustomDDMBitBtn = class(TGraphicControl)
  private
    FFont: TFont;
    FLayout: TButtonLayout;
    FStyle: TGrayImageListStyle;
    FNumGlyphs: TNumGlyphs;
    FTransparentColor, FGITransparentColor, FDisabledTransparentColor, FColor: TColor;
    FDisabledBitmap, FGrayBitmap, FGlyph: TBitmap;
    FConvert, FDown, FMove, FTransparent, FAutoSize, FShowCaption: Boolean;
    FSpacing, FGroupIndex,
    FGrayPortion: Integer;
    FOnChangeGlyph: TNotifyEvent;
    procedure CMMouseEnter(var Msg:TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg:TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure UpdateTracking;
    procedure CalcLayout;
    procedure DrawEmptyRect;
    procedure SetSpacing(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure ChangeImage(Sender: TObject);
    procedure DrawUpBtn;
    procedure SetDown(Value: Boolean);
    procedure SetShowCaption(Value: Boolean);
    procedure SetGrayPortion(Value : integer);
    procedure SetGlyph(newGlyph: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetStyle(Value : TGrayImageListStyle);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetTransparent(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetTransparentColor(Value: TColor);
    procedure SetGITransparentColor;
    procedure SetDisabledTransparentColor;
    procedure calcButtonSize;
    function CreateGrayBitmap(Bitmap : TBitmap): TBitmap;
    function IsMouseInside(x, y: integer): Boolean;
    function GetDest: TRect;
    function GetSource: TRect;
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X,y: Integer); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X,y: Integer); override;
    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property Color: TColor read FColor write SetColor;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Down: Boolean read FDown write SetDown ;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property GrayPortion: Integer read FGrayPortion write SetGrayPortion;
    property Style: TGrayImageListStyle read FStyle write SetStyle;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 0;
    property Spacing: Integer read FSpacing write SetSpacing;
    property Font: TFont read FFont write SetFont;
    property GroupIndex: integer read FGroupIndex write FGroupIndex;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property OnChangeGlyph: TNotifyEvent read FonChangeGlyph write FOnChangeGlyph;
  end;
{-------------------------------------------------------------------------------}
  TDDMBitBtn = class(TCustomDDMBitBtn)
  published
    property TransparentColor;
    property Color;
    property Layout;
    property Caption;
    property Transparent;
    property Down;
    property ShowCaption;
    property Enabled;
    property GrayPortion;
    property Style;
    property Glyph;
    property NumGlyphs;
    property Spacing;
    property Font;
    property GroupIndex;
    property AutoSize;
    {$ifdef D4_up}
      property Action;
      property Anchors;
    {$endif}
    property Hint;
    property DragCursor;
    property DragMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnChangeGlyph;
    property OnMouseMove;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
  end;
{-------------------------------------------------------------------------------}
procedure Register;
{-------------------------------------------------------------------------------}
implementation
{-------------------------------------------------------------------------------}
var
  TX, TY, PX, PY : Integer;
{-------------------------------------------------------------------------------}
function IsAccellerator(VK: Word; const Str: string): Boolean;
var
  P : Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (Upcase(Str[P + 1])=Upcase(Char(VK)));
end;
{-------------------------------------------------------------------------------}
constructor TCustomDDMBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisabledBitmap  := TBitmap.Create;
  FGrayBitmap      := TBitmap.Create;
  FGlyph           := TBitmap.Create;
  FFont            := TFont.Create;
  FLayout          := blGlyphTop;
  Color            := clBtnFace;
  TransparentColor := clWhite;
  Glyph.onChange   := changeImage;
  FMove            := false;
  FDown            := false;
  FNumGlyphs       := 1;
  FGrayPortion     := 100;
  Width            := 25;
  Height           := 25;
  Spacing          := 4;
end;
{-------------------------------------------------------------------------------}
destructor TCustomDDMBitBtn.Destroy;
begin
  FDisabledBitmap.Free;
  FGrayBitmap.Free;
  FGlyph.Free;
  FFont.Free;
  inherited Destroy;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetGITransparentColor;
begin
  FGITransparentColor := FGrayBitmap.Canvas.Pixels[0, FGrayBitmap.Height - 1];
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetDisabledTransparentColor;
begin
  FDisabledTransparentColor := FDisabledBitmap.Canvas.Pixels[0, FDisabledBitmap.Height - 1];
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMMouseEnter(var Msg:TMessage);
begin
  FMove := True;
  drawUpBtn;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMMouseLeave(var Msg:TMessage);
begin
  FMove := False;
  invalidate;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CalcLayout;
begin
  with Canvas do
  begin
    drawEmptyRect;
    { Layout calculates position of text and glyph }
    PX     := (Width  - Glyph.Width div NumGlyphs) div 2;
    PY     := (Height - Glyph.Height) div 2;
    if (ShowCaption = true) and (caption <> '') then
    begin
      case Layout of
      blGlyphTop :    begin
                        PY := (Height - glyph.Height) div 2 - Spacing;
                        TX := (Width  - Canvas.Textwidth(Caption)) div 2;
                        TY := (Height - Canvas.Textheight(Caption)) div 2 + (glyph.Height div 2) + Spacing;
                      end;
      blGlyphLeft :   begin
                        PX := (Width - glyph.Width div NumGlyphs - Canvas.Textwidth(Caption) - Spacing)  div 2;
                        TX := PX + Glyph.Width div NumGlyphs + Spacing;
                        TY := (Height - Canvas.Textheight(Caption)) div 2;
                      end;
      blGlyphRight :  begin
                        PX := (Width + Canvas.Textwidth(Caption) - Glyph.Width + Spacing)  div 2;
                        TX := PX - Canvas.Textwidth(Caption) - Spacing;
                        TY := (Height - Canvas.Textheight(Caption)) div 2;
                      end;
      blGlyphBottom : begin
                        PY := (Height - glyph.Height) div 2 + Spacing;
                        TX := (Width  - Canvas.Textwidth(Caption)) div 2;
                        TY := (Height - Canvas.Textheight(Caption)) div 2 - (glyph.Height div 2) - Spacing;
                      end;
      end;
    end;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.DrawEmptyRect;
begin
  with Canvas do
  begin
    if not Transparent then
    begin
      brush.color := Color;
      pen.Color   := Color;
      brush.Style := bsSolid;
      pen.Style   := psSolid;
    end else begin
      brush.color := Color;
      pen.Color   := Color;
      Brush.style := bsClear;
      pen.Style   := psClear;
    end;
    FillRect(Rect(0, 0, width, height));
  end;
end;
{-------------------------------------------------------------------------------}
function TCustomDDMBitBtn.GetDest: TRect;
begin  result := Rect(PX, PY, PX + FGlyph.width div NumGlyphs, PY + FGlyph.height);  end;
{-------------------------------------------------------------------------------}
function TCustomDDMBitBtn.GetSource: TRect;
begin  result := Rect(0, 0, FGlyph.width div NumGlyphs, FGlyph.height);  end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then
    begin
      if Enabled then
        Click;
      Result:=1;
    end
    else
      inherited;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMEnabledChanged(var Message: TMessage);
begin
  UpdateTracking;
  Repaint;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    if isMouseInside(P.x, P.y) then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.Paint;
var
 RectT: TRect;
begin
  inherited paint;
  with Canvas do
  begin
    calcLayout;
{    if (FDrawFocusRect) and (FHasFocus) then
      Canvas.DrawFocusRect(Rect(4, 4, width -4, height - 4));}
    if FDown = False then { Button is not pressed }
    begin
      if ShowCaption = True then
      begin
        Font.Assign(FFont);

        if not Enabled then
        begin
          brush.Style := bsClear;
          Font.Color := clWhite;
          RectT := Rect(TX + 1, TY + 1, TX + Textwidth(Caption) + 1, TY + TextHeight(Caption) + 1);
          DrawText(Handle,
                   PChar(Caption),
                   length(Caption),
                   RectT,
                   DT_SingleLine);
          Font.Color := clGray;
        end;

        RectT := Rect(TX, TY, TX + Textwidth(Caption), TY + TextHeight(Caption));
        DrawText(Handle,
                 PChar(Caption),
                 length(Caption),
                 RectT,
                 DT_SingleLine);
      end;
      if not Enabled then { Button enabled }
      begin
        copymode := cmSrcCopy;
        BrushCopy(getDest, FDisabledBitmap, getSource, FDisabledTransparentColor);
      end
      else
      begin
        copymode := cmSrcCopy;
        BrushCopy(getDest, FGrayBitmap, getSource, FGITransparentColor);
        if FMove then drawUpBtn;
      end;
    end
    else { Button is pressed }
    begin
      Pen.Style := psSolid;
      Pen.Color := clwhite;
      MoveTo(width - 1, 0);
      LineTo(Width - 1, Height - 1);
      MoveTo(        0, Height - 1);
      LineTo(    Width, Height - 1);
      Pen.Color := clbtnshadow;
      MoveTo(        0, 0);
      LineTo(        0, Height - 1);
      MoveTo(        0, 0);
      LineTo(Width - 1, 0);
      if ShowCaption = True then
      begin
        Font.Assign(FFont);

        if not Enabled then
        begin
          brush.Style := bsClear;
          Font.Color := clWhite;
          RectT := Rect(TX + 2, TY + 2, TX + Textwidth(Caption) + 2, TY + TextHeight(Caption) + 2);
          DrawText(Handle,
                   PChar(Caption),
                   length(Caption),
                   RectT,
                   DT_SingleLine);
          Font.Color := clGray;
        end;
        RectT := Rect(TX + 1, TY + 1, TX + Textwidth(Caption) + 1, TY + TextHeight(Caption) + 1);
        DrawText(Handle,
                 PChar(Caption),
                 length(Caption),
                 RectT,
                 DT_SingleLine);
      end;
      BrushCopy(Rect(getDest.Left + 1, getdest.Top + 1, getdest.Right + 1, getDest.Bottom + 1),
                     Glyph, getSource, FTransparentColor);
      exit;
    end;
  end;
  { check for designing mode }
  If (csDesigning in ComponentState) and (FDown = False) then
  begin
    with canvas do
    begin
      Pen.Color := clbtnshadow;
      pen.style := psDot;
      MoveTo(width - 1, 0);
      LineTo(Width - 1, Height - 1);
      MoveTo(0, Height - 1);
      LineTo(Width, Height - 1);
      Pen.Color := clwhite;
      MoveTo(0, 0);
      LineTo(0, Height - 1);
      MoveTo(0, 0);
      LineTo(Width - 1, 0);
      BrushCopy(getDest, FGrayBitmap, getSource, FGITransparentColor);
    end;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.DrawUpBtn;
var
  RectT: TRect;
begin
  if (not Down) and (Enabled) and (Parent.Enabled) and (TForm(Owner).Enabled)
     and not (ComponentState = [csDesigning]) then
  begin
    with Canvas do
    begin
      calcLayout;
      drawEmptyRect;
      Pen.Color := clWhite;
      Pen.Style := psSolid;
      MoveTo(0,0);
      LineTo(0,Height);
      MoveTo(0,0);
      LineTo(Width,0);
      Pen.Color := clbtnshadow;
      MoveTo(width-1,0);
      LineTo(Width-1,Height-1);
      MoveTo(0,Height-1);
      LineTo(Width,Height-1);
      if ShowCaption = True then
      begin
        Font.Assign(FFont);
        if transparent = true then
        begin
          copymode := cmSrcErase;
          RectT := Rect(TX, TY, TX + Textwidth(Caption), TY + TextHeight(Caption));
          DrawText(Handle,
                   PChar(Caption),
                   length(Caption),
                   RectT,
                   DT_SingleLine);
        end;
        RectT := Rect(TX, TY, TX + Textwidth(Caption), TY + TextHeight(Caption));
        DrawText(Handle,
                 PChar(Caption),
                 length(Caption),
                 RectT,
                 DT_SingleLine);
      end;
      BrushCopy(getDest, FGlyph, getSource, FTransparentColor);
    end;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.MouseDown(Button: TMouseButton;Shift: TShiftState; X,Y: Integer);
begin
  if enabled = false then exit;
  Down      := true;
  inherited MouseDown(Button,Shift, x, y);
  invalidate;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.MouseUP(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if enabled = false then exit;
  FDown := False;
  inherited MouseUp(Button, Shift, X, Y);
  if isMouseInside(x, y) then
  begin
    FMove := true;
    drawEmptyRect;
    invalidate;
  end;
end;
{-------------------------------------------------------------------------------}
function TCustomDDMBitBtn.IsMouseInside(x, y: integer): Boolean;
var
  BitBtn : HRgn;
begin
  BitBtn := CreateRectRgn(0, 0, Width, Height);
  Result := PtInRegion(BitBtn, X, Y);
  DeleteObject(BitBtn);
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetShowCaption(Value: Boolean);
begin
  FShowCaption := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  if not enabled then exit;
  inherited;
  if FDown then DblClick;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMButtonPressed(var Message: TMessage);
var
  Sender: TCustomDDMBitBtn;
begin
  if not Enabled then exit;
  if Message.WParam = FGroupIndex then
  begin
    Sender := TCustomDDMBitBtn(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        Invalidate;
      end;
    end;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CMTextChanged(var msg: TMessage);
begin
  calcButtonSize;
  Invalidate;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetDown(Value:Boolean);
begin
  FDown := value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetGrayPortion(Value : integer);
begin
  if (Value<>FGrayPortion) and (Value>0) and (Value<256) then
  begin
    FGrayPortion := Value;
    changeImage(self);
    calcButtonSize;
  end;
end;
{-------------------------------------------------------------------------------}
function TCustomDDMBitBtn.CreateGrayBitmap(Bitmap : TBitmap): TBitmap;
var
  PixelColor      : TColor;
  x,y,r,g,b,sum,z : integer;
  rs              : real;
begin
  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      PixelColor := ColorToRGB(Bitmap.Canvas.Pixels[x,y]);
      if PixelColor <> FTransparentColor then
      begin
        FConvert := true;
        r        := PixelColor shr 16;  {Red}
        g        := (PixelColor shr 8) and $00FF; {Green}
        b        := PixelColor and $0000FF;  {Blue}
        sum      := (r+g+b) div 3;  {Sum}
        rs       := sum * FGrayPortion * 0.01;
        sum      := round(rs);
        if sum > 255 then sum := 255;
        if FStyle = gisSystemDeactive then
        begin
          { Systemcolors (0-255) }
          if sum > 180 then Bitmap.Canvas.Pixels[x,y] := clBtnHighLight
          else if sum > 80 then Bitmap.Canvas.Pixels[x,y] := clBtnFace
          else if sum > 30 then Bitmap.Canvas.Pixels[x,y] := clBtnShadow
          else Bitmap.Canvas.Pixels[x,y] := clBtnText;
        end
        else if FStyle = gisBlue then
        begin
          rs := sum / 1.1;
          z  := round(rs) ;
          Bitmap.Canvas.Pixels[x, y] := RGB(z, z, sum);
        end
        else
          { make grayvalue }
          Bitmap.Canvas.Pixels[x,y]:=RGB(sum,sum,sum);
      end;
    end;
  end;
  result := Bitmap; { result }
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetStyle(Value : TGrayImageListStyle);
begin
  if Value <> FStyle then
  begin
    FStyle:=Value;
    changeImage(self);
    calcButtonSize;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetGlyph(newGlyph: TBitmap);
begin
  if(not Assigned(FGlyph)) then exit;

  FGlyph.Assign(newGlyph);

  if Assigned(FOnChangeGlyph) then
    FOnChangeGlyph(Self);

  if newGlyph.Empty then exit;
  changeImage(self);

  FTransparentColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  if (csDesigning in ComponentState) then
  begin
    if (newGlyph.width mod newGlyph.height = 0) then
      FNumGlyphs := newGlyph.width div newGlyph.height
    else
      FNumGlyphs := 1;
  end;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.ChangeImage(Sender: TObject);
var
  merge: integer;
begin
  merge := FGrayPortion;
  FGrayBitmap.Assign(Glyph);
  FGrayBitmap  := CreateGrayBitmap(FGrayBitmap);
  SetGITransparentColor;
  FGrayPortion := 255;
  FDisabledBitmap.Monochrome := true;
  FDisabledBitmap.Assign(FGrayBitmap);
  FDisabledBitmap := CreateGrayBitmap(FDisabledBitmap);
  SetDisabledTransparentColor;
  FGrayPortion := merge;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    calcButtonSize;
  end;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (FNumGlyphs = Value) or (Value < 1) or (Value > 4) then Exit;
  FNumGlyphs := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor = Value then Exit;
  FTransparentColor := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetFont(Value: TFont);
begin
  if FFont = Value then Exit;
  FFont.Assign(Value);
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetSpacing(Value: integer);
begin
  if FSpacing = Value then Exit;
  FSpacing := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.SetAutoSize(Value: Boolean);
begin
  if FAutoSize = Value then exit;
  FAutoSize := Value;
  calcButtonSize;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.CalcButtonSize;
begin
  if FAutosize = true then
  begin
    with Canvas do
    begin
      if ShowCaption = true then
      begin
        if (Layout = blGlyphTop) or (Layout = blGlyphBottom) then
        begin
          if TextWidth(Caption) + Spacing * 2 + 4 > Width then
            Width := TextWidth(Caption) + Spacing * 2 + 4;
          if (Glyph.Width < Width) and (TextWidth(Caption) + Spacing * 2 + 4 < Width) then
            Width := TextWidth(Caption) + Spacing * 2 + 4;
          if Glyph.Width > Width then
            Width := Glyph.Width + Spacing *2 + 4;
          Height := Glyph.Height + Spacing * 2 + TextHeight(Caption) + Spacing * 2;
        end
        else
        begin
          if Glyph.Width + TextWidth(Caption) + 4 * Spacing + 8 > Width then
            Width := TextWidth(Caption) + 4 * Spacing + 8 + Glyph.Width;
          if (Glyph.Width + TextWidth(Caption) + 4 * Spacing + 8 < Width) then
            Width := TextWidth(Caption) + 4 * Spacing + 8 + Glyph.Width;
          Height := Glyph.Height + Spacing * 2;
        end;
      end
      else
      begin
        Width  := Glyph.Width + Spacing * 2;
        Height := Glyph.Height + Spacing * 2;
      end;
    end;
  end;
  Invalidate;
end;
{-------------------------------------------------------------------------------}
procedure TCustomDDMBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
  {-------------------------------------------------------------------------------}
  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  var
    dummy: TBitmap;
  begin
    dummy := TBitmap.Create;
    try
      with dummy do
      begin
        Width := ImageList.Width;
        Height := ImageList.Height;
        Canvas.Brush.Color := clFuchsia;
        Canvas.FillRect(Rect(0,0, Width, Height));
        ImageList.Draw(Canvas, 0, 0, Index);
      end;
      setGlyph(dummy);
      TForm(Owner).Caption := 'OK';
      Invalidate;
    finally
      dummy.free;
    end;
  end;
  {-------------------------------------------------------------------------------}
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TAction then
    with TCustomAction(Sender) do
    begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
      begin
        TForm(Owner).Caption := 'OK'+timetostr(now);
        CopyImage(ActionList.Images, ImageIndex);
      end;
    end;
end;
{-------------------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('DDM', [TDDMBitBtn]);
end;
{-------------------------------------------------------------------------------}
end.