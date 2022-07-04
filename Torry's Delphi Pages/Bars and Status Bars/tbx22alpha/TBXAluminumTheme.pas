unit TBXAluminumTheme;

// TBX Package
// Copyright 2001-2005 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXAluminumTheme.pas 133 2005-11-08 20:00:43Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Graphics, TBXThemes, TBXDefaultTheme, ImgList, CommCtrl;

type
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  TWinFrameState = (wfsActive, wfsInactive);

  TTBXAluminumTheme = class(TTBXDefaultTheme)
  protected
    DockColor: TColor;
    MenuBarColor: TColor;
    ToolbarColor: TColor;
    StatusbarColor: TColor;
    PopupColor: TColor;
    DockPanelColor: TColor;
    PopupFrameColor: TColor;
    WinFrameColors: array [TWinFrameState, TWinFramePart] of TColor;
    PnlFrameColors: array [TWinFrameState, TWinFramePart] of TColor;
    MenuItemColors: array [TMenuItemState, TItemPart] of TColor;
    BtnItemColors: array [TBtnItemState, TItemPart] of TColor;
    SeparatorColor: TColor;
    DefaultRoughness: Integer;

    procedure SetupColorCache; override;
  protected
    { Internal Methods }
    function GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
    function GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics access, etc. }
    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function  GetImageOffset(DC: HDC; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint; override;
    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(ViewType: Integer): TColor; override;

    { Painting routines }
    procedure PaintBackgnd(DC: HDC; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; AViewType: Integer); override;
    procedure PaintButton(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(DC: HDC; const Rect: TRect; const ItemInfo: TTBXItemInfo; const Caption: WideString; Format: Cardinal; Color: TColor); override;
    procedure PaintCheckMark(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintChevron(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintDock(DC: HDC; const ClientRect, DockRect: TRect; DockPosition: Integer); override;
    procedure PaintDockPanelNCArea(DC: HDC; R: TRect; const DockPanelInfo: TTBXDockPanelInfo); override;
    procedure PaintDropDownArrow(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintEditButton(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo); override;
    procedure PaintEditFrame(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo); override;
    procedure PaintFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintFloatingBorder(DC: HDC; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintImage(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMenuItem(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPopupNCArea(DC: HDC; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintPageScrollButton(DC: HDC; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintMDIButton(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintSeparator(DC: HDC; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(DC: HDC; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintFrameControl(DC: HDC; R: TRect; Kind, State: Integer; Params: Pointer); override;
    procedure PaintStatusBar(DC: HDC; R: TRect; Part: Integer); override;
  end;

implementation

uses TBXUtils, TB2Item, TB2Common, TBXGraphics, Classes, Controls, Forms;

const
  BaseColor = $DFDFDF;
  BtnText = $000000;
  BtnShadow = $9F9F9F;
  BtnFace = $C7C7C7;
  BtnHighlight = $DFDFDF;

var
  StockImgList: TImageList;
//  StockPatternBitmap: TBitmap;
  CounterLock: Integer = 0;

procedure InitializeStock;
begin
{  StockPatternBitmap := TBitmap.Create;
  StockPatternBitmap.Width := 8;
  StockPatternBitmap.Height := 8;   }
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
end;

procedure FinalizeStock;
begin
//  StockPatternBitmap.Free;
  StockImgList.Free;
end;

procedure DotFill(DC: HDC; const Rect: TRect; Color: TColor; Border: Integer);
const
  Step = 4;
  Step50 = STEP div 2;
  Pattern: array [0..15] of Word =
    ($8888, 0, 0, 0, $8888, 0, 0, 0, $8888, 0, 0, 0, $8888, 0, 0, 0);
var
  W, H: Integer;
  R: TRect;

  function GetShiftedRect(DX, DY: Integer): TRect;
  begin
    Result := R;
    OffsetRect(Result, DX, DY);
  end;

begin
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;
  W := ((W - STEP50) div STEP) * STEP + STEP50;
  H := ((H - STEP50) div STEP) * STEP + STEP50;
  R.Left := (Rect.Right + Rect.Left - W + 1) div 2;
  R.Right := R.Left + W;
  R.Top := (Rect.Top + Rect.Bottom - H + 1) div 2;
  R.Bottom := R.Top + H;

  with TMonoGlyph.Create(16, 16, Pattern) do
  try
    DrawTiled(DC, GetShiftedRect(0, 0), ColorToRGB(NearestLighten(Color, -96)));
    DrawTiled(DC, GetShiftedRect(-1, -1), ColorToRGB(NearestLighten(Color, -24)));
    DrawTiled(DC, GetShiftedRect(-1, 0), ColorToRGB(NearestLighten(Color, -32)));
    DrawTiled(DC, GetShiftedRect(0, -1), ColorToRGB(NearestLighten(Color, -32)));
    DrawTiled(DC, GetShiftedRect(1, 0), ColorToRGB(NearestLighten(Color, 24)));
    DrawTiled(DC, GetShiftedRect(0, 1), ColorToRGB(NearestLighten(Color, 24)));
    DrawTiled(DC, GetShiftedRect(1, 1), ColorToRGB(NearestLighten(Color, 18)));
  finally
    Free;
  end;
end;

procedure DotFill2(DC: HDC; R: TRect; Color: TColor; Border: Integer);
const
  STEP = 4;
  STEP50 = STEP div 2;
  Pattern: array [0..15] of Word =
    ($8888, 0, 0, 0, $8888, 0, 0, 0, $8888, 0, 0, 0, $8888, 0, 0, 0);
var
  W, H: Integer;
  NX, NY: Integer;
  B: TBitmap;
  BitmapDC: HDC;
  OldTextColor, OldBkColor: Longword;
  OldBrush, Brush: HBrush;

  procedure PaintDots(X, Y: Integer; C: TColorRef);
  var
    I, J: Integer;
  begin
    Brush := CreateSolidBrush(C);
    OldBrush := SelectObject(DC, Brush);
    for J := 0 to NY - 1 do
      for I := 0 to NX - 1 do
      begin
        BitBlt(DC, X + I shl 4, Y + J shl 4, 16, 16, BitmapDC, 0, 0, ROP_DSPDxax);
      end;
    SelectObject(DC, OldBrush);
    DeleteObject(Brush);
  end;

begin
  B := TBitmap.Create;
  B.Handle := CreateBitmap(16, 16, 1, 1, @Pattern);
  BitmapDC := B.Canvas.Handle;
  OldTextColor := SetTextColor(DC, clBlack);
  OldBkColor := SetBkColor(DC, clWhite);

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  W := ((W - STEP50) div STEP) * STEP + STEP50;
  H := ((H - STEP50) div STEP) * STEP + STEP50;
  R.Left := (R.Right + R.Left - W - 1) div 2;
  R.Right := R.Left + W;
  R.Top := (R.Top + R.Bottom - H - 1) div 2;
  R.Bottom := R.Top + H;
  NX := (R.Right - R.Left + 15) div 16;
  NY := (R.Bottom - R.Top + 15) div 16;

  SaveDC(DC);
  with R do IntersectClipRect(DC, Left, Top, Right, Bottom);

  PaintDots(R.Left + 1, R.Top + 1, ColorToRGB(NearestLighten(Color, -96)));
  PaintDots(R.Left, R.Top, ColorToRGB(NearestLighten(Color, -24)));
  PaintDots(R.Left, R.Top + 1, ColorToRGB(NearestLighten(Color, -32)));
  PaintDots(R.Left + 1, R.Top, ColorToRGB(NearestLighten(Color, -32)));
  PaintDots(R.Left + 2, R.Top + 1, ColorToRGB(NearestLighten(Color, 24)));
  PaintDots(R.Left + 1, R.Top + 2, ColorToRGB(NearestLighten(Color, 24)));
  PaintDots(R.Left + 2, R.Top + 2, ColorToRGB(NearestLighten(Color, 18)));

  RestoreDC(DC, -1);

  SetTextColor(DC, OldTextColor);
  SetBkColor(DC, OldBkColor);
  B.Free;
end;


procedure DotFillOld(DC: HDC; R: TRect; Color: TColor; Border: Integer);
const
  STEP = 4;
  STEP50 = STEP div 2;
var
  W, H, I, J, X, Y: Integer;
  C, CLo1, CLo2, CHi1, CHi2: TColorRef;
begin
  InflateRect(R, -Border, -Border);
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  W := ((W - STEP50) div STEP) * STEP + STEP50;
  H := ((H - STEP50) div STEP) * STEP + STEP50;
  R.Left := (R.Right + R.Left - W - 1) div 2;
  R.Right := R.Left + W;
  R.Top := (R.Top + R.Bottom - H - 1) div 2;
  R.Bottom := R.Top + H;

  C := ColorToRGB(NearestLighten(Color, -96));
  CHi1 := ColorToRGB(NearestLighten(Color, 18));
  CHi2 := ColorToRGB(NearestLighten(Color, 24));
  CLo1 := ColorToRGB(NearestLighten(Color, -24));
  CLo2 := ColorToRGB(NearestLighten(Color, -32));

  for J := 0 to (R.Bottom - R.Top - 1) div STEP do
    for I := 0 to (R.Right - R.Left - 1) div STEP do
      begin
        X := R.Left + I * STEP + 1;
        Y := R.Top + J * STEP + 1;
        SetPixelV(DC, X, Y, C);
        SetPixelV(DC, X - 1, Y - 1, CLo1);
        SetPixelV(DC, X - 1, Y, CLo2);
        SetPixelV(DC, X, Y - 1, CLo2);
        SetPixelV(DC, X + 1, Y, CHi2);
        SetPixelV(DC, X, Y + 1, CHi2);
        SetPixelV(DC, X + 1, Y + 1, CHi1);
      end;
end;

procedure RoundFrame(DC: HDC; R: TRect; TL, TR, BL, BR: Integer; Color: TColor); overload;
var
  Radius: Integer;
  CornerID: Integer;

  procedure PutPixel(X, Y: Integer; Alpha: Integer);
  begin
    with R do
      case CornerID of
        0: SetPixelEx(DC, Left + X,  Top + Y,    Color, Alpha);
        1: SetPixelEx(DC, Right - X, Top + Y,    Color, Alpha);
        2: SetPixelEx(DC, Left + X,  Bottom - Y, Color, Alpha);
        3: SetPixelEx(DC, Right - X, Bottom - Y, Color, Alpha);
      end;
  end;

begin
  if Color = clNone then Exit;
  with R do
  begin
    Dec(Right); Dec(Bottom);
    if Color < 0 then Color := GetSysColor(Color and $FF);

    { Edges }
    DrawLineEx(DC, Left, Bottom - BL, Left, Top + TL - 1, Color);
    DrawLineEx(DC, Left + TL, Top, Right - TR + 1, Top, Color);
    DrawLineEx(DC, Left + BL, Bottom, Right - BR + 1, Bottom, Color);
    DrawLineEx(DC, Right, Top + TR, Right, Bottom - BR + 1, Color);

    { Corners }
    for CornerID := 0 to 3 do
    begin
      case CornerID of
        0: Radius := TL;
        1: Radius := TR;
        2: Radius := BL;
      else
        Radius := BR;
      end;
      case Radius of
        1: begin PutPixel(0, 0, $3F); PutPixel(1, 1, $3F); end;
        2: begin PutPixel(0, 0, $1F); PutPixel(1, 0, $7F); PutPixel(0, 1, $7F); PutPixel(1, 1, $9F); end;
        3: begin PutPixel(0, 0, $00); PutPixel(1, 0, $2F); PutPixel(0, 1, $2F);
                 PutPixel(2, 0, $8F); PutPixel(0, 2, $8F); PutPixel(1, 1, $AF);
                 PutPixel(2, 1, $3F); PutPixel(1, 2, $3F); end;
      end;
    end;
  end;
end;

procedure RoundFrame(DC: HDC; R: TRect; Rounding: Integer; Color: TColor; BgColor: TColor); overload;
begin
  if Color <> clNone then RoundFrame(DC, R, Rounding, Rounding, Rounding, Rounding, Color);
end;

procedure RoundFill(DC: HDC; R: TRect; TL, TR, BL, BR: Integer; Color: TColor);
begin
  if Color <> clNone then with R do
  begin
    Dec(Right); Dec(Bottom);
    PolygonEx(DC, [
      Point(Left + TL, Top),
      Point(Right - TR, Top),
      Point(Right, Top + TR),
      Point(Right, Bottom - BR),
      Point(Right - BR, Bottom),
      Point(Left + BL, Bottom),
      Point(Left, Bottom - BL),
      Point(Left, Top + TL),
      Point(Left + TL, Top)
    ], Color, Color);
    Inc(Right); Inc(Bottom);
  end;
end;

procedure RoundBevel(
  DC: HDC; ARect: TRect;
  TL, TR, BL, BR: Integer;
  ColorTL, ColorBR: TColor; CornerColor: TColor);
var
  Mix25, Mix50, Mix75: TColor;
begin
  if not RectVisible(DC, ARect) then Exit;

  Dec(ARect.Right); Dec(ARect.Bottom);
  ColorTL := ColorToRGB(ColorTL);
  ColorBR := ColorToRGB(ColorBR);
  if CornerColor <> clNone then CornerColor := ColorToRGB(CornerColor);

  with ARect do
  begin
    { Top and left edges }
    DrawLineEx(DC, Left, Bottom - BL, Left, Top + TL - 1, ColorTL);
    DrawLineEx(DC, Left + TL, Top, Right - TR + 1, Top, ColorTL);

    { Bottom and right edges }
    DrawLineEx(DC, Left + BL, Bottom, Right - BR + 1, Bottom, ColorBR);
    DrawLineEx(DC, Right, Top + TR, Right, Bottom - BR + 1, ColorBR);

    Mix25 := MixColors(ColorTL, ColorBR, 63);
    Mix50 := MixColors(ColorTL, ColorBR, 127);
    Mix75 := MixColors(ColorTL, ColorBR, 191);

    { Top Left corner }
    case TL of
      1:
        begin
          if CornerColor <> clNone then SetPixelV(DC, Left, Top, MixColors(ColorTL, CornerColor, $7F))
          else SetPixelEx(DC, Left, Top, ColorTL, $3F);
          SetPixelEx(DC, Left + 1, Top + 1, ColorTL, $3F);
        end;
      2:
        begin
          if CornerColor <> clNone then
          begin
            SetPixelV(DC, Left, Top, MixColors(ColorTL, CornerColor, $1F));
            SetPixelV(DC, Left + 1, Top, MixColors(ColorTL, CornerColor, $AF));
            SetPixelV(DC, Left, Top + 1, MixColors(ColorTL, CornerColor, $AF));
          end
          else
          begin
            SetPixelEx(DC, Left, Top, ColorTL, $1F);
            SetPixelEx(DC, Left + 1, Top, ColorTL, $8F);
            SetPixelEx(DC, Left, Top + 1, ColorTL, $8F);
          end;
          SetPixelEx(DC, Left + 1, Top + 1, ColorTL, $CF);
        end;
    end;

    { Top Right corner }
    case TR of
      1:
        begin
          if CornerColor <> clNone then SetPixelV(DC, Right, Top, MixColors(Mix50, CornerColor, $3F))
          else SetPixelEx(DC, Right, Top, ColorBR, $3F);
          SetPixelEx(DC, Right, Top, Mix50, $3F);
        end;
      2:
        begin
          if CornerColor <> clNone then
          begin
            SetPixelV(DC, Right, Top, CornerColor);
            SetPixelV(DC, Right, Top + 1, MixColors(Mix25, CornerColor, $7F));
            SetPixelV(DC, Right - 1, Top, MixColors(Mix75, CornerColor, $7F));
          end
          else
          begin
            // not implemented
          end;
        end;
    end;

    { Bottom Left corner }
    case BL of
      1:
        begin
          if CornerColor <> clNone then SetPixelV(DC, Left, Bottom, MixColors(Mix50, CornerColor, $3F))
          else SetPixelEx(DC, Left, Bottom, ColorBR, $3F);
          SetPixelEx(DC, Left, Bottom, Mix50, $3F);
        end;
      2:
        begin
          if CornerColor <> clNone then
          begin
            SetPixelV(DC, Left, Bottom, CornerColor);
            SetPixelV(DC, Left, Bottom - 1, MixColors(Mix75, CornerColor, $7F));
            SetPixelV(DC, Left + 1, Bottom, MixColors(Mix25, CornerColor, $7F));
          end
          else
          begin
            // not implemented
          end;
        end;
    end;

    { Bottom Right corner }
    case BR of
      1:
        begin
          if CornerColor <> clNone then SetPixelV(DC, Right, Bottom, MixColors(ColorBR, CornerColor, $7F))
          else SetPixelEx(DC, Right, Bottom, ColorBR, $3F);
          SetPixelEx(DC, Right - 1, Bottom - 1, ColorBR, $3F);
        end;
      2:
        begin
          if CornerColor <> clNone then
          begin
            SetPixelV(DC, Right, Bottom, MixColors(ColorBR, CornerColor, $1F));
            SetPixelV(DC, Right - 1, Bottom, MixColors(ColorBR, CornerColor, $AF));
            SetPixelV(DC, Right, Bottom - 1, MixColors(ColorBR, CornerColor, $AF));
          end
          else
          begin
            SetPixelEx(DC, Right, Bottom, ColorBR, $1F);
            SetPixelEx(DC, Right - 1, Bottom, ColorBR, $8F);
            SetPixelEx(DC, Right, Bottom - 1, ColorBR, $8F);
          end;
          SetPixelEx(DC, Right - 1, Bottom - 1, ColorBR, $CF);
        end;
    end;
  end;
end;

procedure DrawCaptionArea(DC: HDC; R: TRect; Color: TColor; Vertical: Boolean);
var
  T: Integer;
begin
  if Vertical then
  begin
    T := (2 * R.Top + R.Bottom) div 3;
    GradFill(DC, Rect(R.Left, R.Top, R.Right, T), Lighten(Color, 24), Color, gkVert);
    GradFill(DC, Rect(R.Left, T, R.Right, R.Bottom), Color, Lighten(Color, -8), gkVert);
  end
  else
  begin
    T := (2 * R.Left + R.Right) div 3;
    GradFill(DC, Rect(R.Left, R.Top, T, R.Bottom), Lighten(Color, 24), Color, gkHorz);
    GradFill(DC, Rect(T, R.Top, R.Right, R.Bottom), Color, Lighten(Color, -8), gkHorz);
  end;
end;

procedure DrawButtonBitmap(DC: HDC; R: TRect; BgColor: TColor; Hot: Boolean);
const
  Pattern: array [0..15] of Byte =
    ($C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0, 0, 0);
var
  C: TColor;
begin
  OffsetRect(R, 1, 1);
  DrawGlyph(DC, R, 7, 6, Pattern[0], Lighten(BgColor, 24));
  OffsetRect(R, -2, -2);
  DrawGlyph(DC, R, 7, 6, Pattern[0], Lighten(BgColor, -32));
  OffsetRect(R, 1, 1);
  if Hot then C := $000000 else C := $3F3F3F;
  DrawGlyph(DC, R, 7, 6, Pattern[0], C);
end;

procedure DrawBtnFill(DC: HDC; const R: TRect);
var
  C: TColor;
  T: Integer;
begin
  C := MixColors(BtnHighlight, BtnFace, 192);
  T := (R.Top + R.Bottom) div 2;
  GradFill(DC, Rect(R.Left, R.Top, R.Right, T), BtnHighlight, C, gkVert);
  GradFill(DC, Rect(R.Left, T, R.Right, R.Bottom), C, Lighten(BtnFace, -16), gkVert);
end;

{ TTBXAluminumTheme }

function TTBXAluminumTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT:    Result := True;
    TMB_EDITMENUFULLSELECT:        Result := True;
    TMB_EDITHEIGHTEVEN:            Result := False;
    TMB_PAINTDOCKBACKGROUND:       Result := True;
    TMB_SOLIDTOOLBARNCAREA:        Result := True;
    TMB_SOLIDTOOLBARCLIENTAREA:    Result := True;
  else
    Result := inherited GetBooleanMetrics(Index);
  end;
end;

function TTBXAluminumTheme.GetViewColor(ViewType: Integer): TColor;
begin
  Result := clBtnFace;
  case ViewType and VT_CATEGORYMASK of
    VT_BARS:
      if ViewType and VT_TYPEMASK = VT_MENUBAR then Result := MenubarColor
      else Result := ToolbarColor;
    VT_POPUPS:
      if ViewType and VT_TYPEMASK = VT_LISTBOX then Result := clWindow
      else Result := PopupColor;
    VT_STATUSBAR: Result := StatusBarColor;
    VT_DOCKPANEL: Result := DockPanelColor;
    VT_TOOLWINDOW: Result := ToolbarColor;
    VT_DOCK: Result := DockColor;
  end;
end;

function TTBXAluminumTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXAluminumTheme.GetIntegerMetrics(Index: Integer): Integer;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:         Result := 12;
    TMI_MENU_MDI_DW:                 Result := 1;
    TMI_MENU_MDI_DH:                 Result := 2;
    TMI_EDIT_FRAMEWIDTH:             Result := 2;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 1;
    TMI_EDIT_BTNWIDTH:               Result := 14;
    TMI_EDIT_MENURIGHTINDENT:        Result := 0;
  else
    Result := inherited GetIntegerMetrics(Index);
  end;
end;

function TTBXAluminumTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipText);
end;

procedure TTBXAluminumTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array [Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array [Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;
  Sz: Integer;
begin
  Sz := 0;
  case ViewType and VT_CATEGORYMASK of
    VT_BARS, VT_TOOLWINDOW, VT_DOCKPANEL:
      if ViewType and VT_FLOATING <> 0 then
      begin
        Resizable := ViewType and VT_RESIZABLE <> 0;
        Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
        Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
        Exit;
      end
      else Sz := 2;
    VT_POPUPS: Sz := 2;
  end;
  Border.X := Sz;
  Border.Y := Sz;
end;

procedure TTBXAluminumTheme.PaintBackgnd(DC: HDC; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; AViewType: Integer);
var
  R: TRect;
begin
  if TBXLoColor then inherited
  else if AColor <> clNone then
  begin
    if AColor = clDefault then AColor := GetViewColor(AViewType);
    IntersectRect(R, ARect, AClipRect);
    if not RectVisible(DC, R) then Exit;

    AColor := ColorToRGB(AColor);
    if ((AViewType and VT_CATEGORYMASK = VT_BARS) and (AViewType and VT_EMBEDDED <> 0)) or
      (AViewType and VT_CATEGORYMASK = VT_POPUPS) then
    begin
      FillRectEx(DC, R, AColor);
    end
    else
    begin
      BrushedFill(DC, nil, R, AColor, DefaultRoughness);
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintCaption(DC: HDC;
  const Rect: TRect; const ItemInfo: TTBXItemInfo;
  const Caption: WideString; Format: Cardinal; Color: TColor);
var
  R: TRect;
  C2: TColor;
begin
  if not ItemInfo.Enabled or (Color = clDefault) then Color := GetItemTextColor(ItemInfo)
  else if Color = clNone then Color := clDefault; // use existing DC color
  with ItemInfo do
  begin
    R := Rect;
    if ItemOptions and IO_TOOLBARSTYLE <> 0 then
    begin
      OffsetRect(R, 1, 1);
      C2 := GetBtnColor(ItemInfo, ipBody);
      if C2 = clNone then C2 := ToolbarColor;
      DrawTextRW(DC, Caption, R, Format, Lighten(C2, 16));
      OffsetRect(R, -1, -1);
    end
    else if ItemInfo.ViewType and VT_STATUSBAR = VT_STATUSBAR then
    begin
      OffsetRect(R, 1, 1);
      DrawTextRW(DC, Caption, R, Format, Lighten(StatusBarColor, 16));
      OffsetRect(R, -1, -1);
    end;
    DrawTextRW(DC, Caption, R, Format, Color);
  end;
end;

procedure TTBXAluminumTheme.PaintCheckMark(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
  C: TColor;
begin
  X := (ARect.Left + ARect.Right) div 2;
  Y := (ARect.Top + ARect.Bottom) div 2;
  C := GetBtnColor(ItemInfo, ipText);
  if ItemInfo.ItemOptions and IO_RADIO <> 0 then
  begin
    Dec(X, 2); Inc(Y);
    PolyLineEx(DC, [Point(X-2, Y-2), Point(X, Y), Point(X+4, Y-4),
      Point(X+4, Y-3), Point(X, Y+1), Point(X-2, Y-1), Point(X-2, Y-2)], C);
  end
  else
  begin
    Inc(X); Inc(Y);
    RoundRectEx(DC, Rect(X - 3, Y - 3, X + 2, Y + 2), C, C, 2, 2);
  end;
end;

procedure TTBXAluminumTheme.PaintDropDownArrow(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
  C: TColor;
begin
  with ItemInfo, ARect do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;
    C := GetPartColor(ItemInfo, ipText);
    if ItemInfo.IsVertical then PolygonEx(DC, [Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)], C, C)
    else PolygonEx(DC, [Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)], C, C);
  end;
end;

procedure TTBXAluminumTheme.PaintButton(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  RL, RR: Integer;
begin
  R := ARect;
  RL := 2; RR := 2;
  with ItemInfo do
  begin
    R := ARect;
    if IsPopupParent then
    begin
      RL := 1; RR := 1;
    end;

    if ((ItemOptions and IO_DESIGNING) <> 0) and not Selected then
    begin
      if ComboPart = cpSplitRight then Dec(R.Left);
      RoundFrame(DC, R, 1, GetNearestColor(DC, MixColors(BtnShadow, BtnFace, 100)), clNone);
    end
    else
    begin
      if ComboPart = cpSplitLeft then
      begin
        RR := 0;
      end;
      if ComboPart = cpSplitRight then
      begin
        Dec(R.Left);
        RL := 0;
      end;

      RoundFill(DC, R, RL, RR, RL, RR, GetBtnColor(ItemInfo, ipBody));
      RoundFrame(DC, R, RL, RR, RL, RR, GetBtnColor(ItemInfo, ipFrame));
      if (ComboPart = cpSplitRight) and IsPopupParent then
        DrawLineEx(DC, R.Left, R.Top + 1, R.Left, R.Bottom - 1, GetBtnColor(ItemInfo, ipBody));
    end;
    if ComboPart = cpSplitRight then PaintDropDownArrow(DC, R, ItemInfo);
  end;
end;

procedure TTBXAluminumTheme.PaintChevron(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  Pattern: array[Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  R2: TRect;
  X, Y: Integer;
  C: TColor;
  W, H: Integer;
begin
  PaintButton(DC, ARect, ItemInfo);
  R2 := ARect;
  if not ItemInfo.IsVertical then
  begin
    Inc(R2.Top, 4);
    R2.Bottom := R2.Top + 5;
    W := 8;
    H := 5;
  end
  else
  begin
    R2.Left := R2.Right - 9;
    R2.Right := R2.Left + 5;
    W := 5;
    H := 8;
  end;
  C := GetBtnColor(ItemInfo, ipBody);
  if C = clNone then C := ToolbarColor;

  X := (R2.Left + R2.Right - W) div 2;
  Y := (R2.Top + R2.Bottom - H) div 2;
  with TMonoGlyph.Create(W, H, Pattern[ItemInfo.IsVertical][0]) do
  try
    Draw(DC, X + 1, Y + 1, Lighten(C, 24));
    Draw(DC, X - 1, Y - 1, Lighten(C, -32));
    if ItemInfo.HoverKind <> hkNone then C := $000000 else C := $3F3F3F;
    Draw(DC, X, Y, C);
  finally
    Free
  end;
end;

procedure TTBXAluminumTheme.PaintFloatingBorder(DC: HDC; const ARect: TRect; const WindowInfo: TTBXWindowInfo);
var
  BorderColor, C: TColor;
  I: Integer;
  Sz: TPoint;
  R: TRect;
  IsPushed, IsHovered: Boolean;
  BtnItemState: TBtnItemState;
  OldFont: HFONT;

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if not WindowInfo.Active then Result := bisDisabled
    else if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

begin
  BorderColor := NearestMixedColor(BtnShadow, clBlack, 220);

  if (WRP_BORDER and WindowInfo.RedrawPart) <> 0  then
  begin
    R := ARect;
    Sz := WindowInfo.FloatingBorderSize;
    for I := 0 to Sz.X - 2 do FrameRectEx(DC, R, BorderColor, True);
    FrameRectEx(DC, R, WindowInfo.Color, False);
    C := NearestMixedColor(BorderColor, WindowInfo.Color, $7F);
    SetPixelV(DC, R.Left, R.Top, C);
    SetPixelV(DC, R.Right - 1, R.Top, C);
    SetPixelV(DC, R.Left, R.Bottom - 1, C);
    SetPixelV(DC, R.Right - 1, R.Bottom - 1, C);
  end;

  if not WindowInfo.ShowCaption then Exit;

  { Caption }
  if (WRP_CAPTION and WindowInfo.RedrawPart) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION));
    with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
    Dec(R.Bottom, 2);
    DrawCaptionArea(DC, R, ToolbarColor, True);
    InflateRect(R, 1, 1);
    RoundBevel(DC, R, 2, 2, 0, 0, Lighten(ToolbarColor, 32), Lighten(ToolbarColor, -16), BorderColor);
    InflateRect(R, -1, -1);
    Inc(R.Bottom);
    DrawLineEx(DC, R.Left - 1, R.Bottom - 1, R.Right + 1, R.Bottom - 1, BorderColor);
    DrawLineEx(DC, R.Left - 1, R.Bottom, R.Right + 1, R.Bottom, WindowInfo.Color);

    if ((CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0) and
      ((WRP_CLOSEBTN and WindowInfo.RedrawPart) <> 0) then
      Dec(R.Right, GetSystemMetrics(SM_CYSMCAPTION));

    InflateRect(R, -2, 0);
    Dec(R.Top, 2);
    OldFont := SelectObject(DC, SmCaptionFont.Handle);
    DrawTextRW(DC, WindowInfo.Caption, R,
      DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX, BtnText);
    SelectObject(DC, OldFont);
  end;

  { Close button }
  if (CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION));
    with Windowinfo.FloatingBorderSize do OffsetRect(R, X, Y);

    R.Left := R.Right - (R.Bottom - R.Top) - 1;      
    Dec(R.Bottom, 2);
    DrawCaptionArea(DC, R, ToolbarColor, True);
    Inc(R.Left, 3); Dec(R.Right);

    IsPushed := (CDBS_PRESSED and WindowInfo.CloseButtonState) <> 0;
    IsHovered := (CDBS_HOT and WindowInfo.CloseButtonState) <> 0;
    BtnItemState := GetBtnItemState(WindowInfo.CloseButtonState);
    C := BtnItemColors[BtnItemState, ipBody];
    BorderColor := BtnItemColors[BtnItemState, ipFrame];
    Inc(R.Top);
    Dec(R.Bottom);
    RoundFill(DC, R, 2, 2, 2, 2, C);
    RoundFrame(DC, R, 2, 2, 2, 2, BorderColor);
    DrawButtonBitmap(DC, R, C, IsPushed or IsHovered);
  end;
end;

procedure TTBXAluminumTheme.PaintFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  E, Embedded, InPopup: Boolean;
  CHi, CLo: TColor;
begin
  R := ARect;
  with ItemInfo do
  begin
    E := Enabled or (not Enabled and (HoverKind = hkKeyboardHover));
    Embedded := (ViewType and VT_CATEGORYMASK = VT_BARS) and (ViewType and VT_EMBEDDED <> 0);
    InPopup := ViewType and VT_CATEGORYMASK = VT_POPUPS;
    CHi := Lighten(ToolbarColor, 16);
    CLo := Lighten(ToolbarColor, -16);
    if not E then
    begin
      RoundBevel(DC, R, 1, 1, 1, 1, CLo, CHi, clNone);
      InflateRect(R, -1, -1);
      RoundBevel(DC, R, 1, 1, 1, 1, CHi, CLo, clNone);
    end
    else if Pushed or Selected or (HoverKind <> hkNone) or ((ItemOptions and IO_DESIGNING) <> 0) then
    begin
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, clWindow, False);
      InflateRect(R, 1, 1);
      RoundFrame(DC, R, 2, GetBtnColor(ItemInfo, ipFrame), clNone);
    end
    else
    begin
      if not (Embedded or InPopup) then RoundBevel(DC, R, 1, 1, 1, 1, CLo, CHi, clNone);
      InflateRect(R, -1, -1);
      RoundFrame(DC, R, 1, 1, 1, 1, BtnShadow);
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintEditButton(DC: HDC; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
const
  ArrowColor: array [Boolean] of TColor = (BtnText, clMenuText);
var
  BtnDisabled, BtnHot, BtnPressed: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  FrameColor, C: TColor;
  SaveItemInfoPushed: Boolean;

  procedure PaintEnabled(R: TRect; Pressed: Boolean; AAUpper, AALower: Boolean);
  begin
    C := GetPartColor(ItemInfo, ipBody);
    if Pressed or BtnHot then
    begin
      FillRectEx(DC, R, C);
      C := MixColors(FrameColor, C, 128);
      if AAUpper then SetPixelV(DC, R.Right - 1, R.Top, C);
      if AALower then SetPixelV(DC, R.Right - 1, R.Bottom - 1, C);
    end
    else
    begin
      GradFill(DC, R, Lighten(BaseColor, 16), Lighten(BaseColor, -16), gkVert);
      RoundBevel(DC, R, 0, 1, 0, 1, Lighten(BaseColor, 24), Lighten(BaseColor, -24), clNone);
    end;
  end;

begin
  with ItemInfo do
  begin
    R := ARect;
    Dec(R.Left);
    BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
    BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;

    FrameColor := GetPartColor(ItemInfo, ipFrame);
    if FrameColor = clNone then FrameColor := BtnShadow;

    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
      if BtnHot or BtnPressed then InflateRect(R, -1, -1)
      else InflateRect(R, -2, -2);

      if not BtnDisabled then
      begin
        DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, FrameColor);
        PaintEnabled(R, BtnPressed, True, True);
      end;
      PaintDropDownArrow(DC, R, ItemInfo);
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      BtnPressed := (ButtonInfo.ButtonState and (EBSS_UP or EBSS_DOWN)) <> 0;
      if BtnHot or BtnPressed then InflateRect(R, -1, -1)
      else InflateRect(R, -2, -2);

      if not BtnDisabled then
      begin
        SaveItemInfoPushed := ItemInfo.Pushed;

        { Upper button }
        BR := R; BR.Bottom := (R.Top + R.Bottom) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
        ItemInfo.Pushed := BtnPressed;
        PaintEnabled(BR, BtnPressed, True, False);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom - 1) div 2;
        C := GetPartColor(ItemInfo, ipText);
        PolygonEx(DC, [Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)], C, C);


        { Lower button }
        BR := R; BR.Top := (R.Top + R.Bottom + 1) div 2;
        BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
        ItemInfo.Pushed := BtnPressed;
        PaintEnabled(BR, BtnPressed, False, True);
        X := (BR.Left + BR.Right) div 2;
        Y := (BR.Top + BR.Bottom) div 2;
        C := GetPartColor(ItemInfo, ipText);
        PolygonEx(DC, [Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)], C, C);

        ItemInfo.Pushed := SaveItemInfoPushed;

        DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, FrameColor);
        Y := (R.Top + R.Bottom - 1) div 2;
        DrawLineEx(DC, R.Left, Y, R.Right, Y, FrameColor);
        Y := (R.Top + R.Bottom) div 2;
        DrawLineEx(DC, R.Left, Y, R.Right, Y, FrameColor);
      end;
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintEditFrame(DC: HDC;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
//  Embedded: Boolean;
begin
  R := ARect;
  PaintFrame(DC, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
{  Embedded := ((ItemInfo.ViewType and VT_TOOLBAR) = VT_TOOLBAR) and
    ((ItemInfo.ViewType and TVT_EMBEDDED) = TVT_EMBEDDED); }

  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - W);

  with ItemInfo do if Enabled then
  begin
    if not (Pushed or Selected or (HoverKind <> hkNone)) then
    begin
      RoundFrame(DC, R, 1, 1, 1, 1, clWindow);
      InflateRect(R, -1, -1);
    end;
    FillRectEx(DC, R, clWindow);
  end;

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(DC, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXAluminumTheme.PaintImage(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  HiContrast: Boolean;
  C: TCanvas;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      SaveDC(DC);
      try
        C := TCanvas.Create;
        C.Handle := DC;
        TTBCustomImageList(ImageList).DrawState(C, ARect.Left, ARect.Top,
          ImageIndex, Enabled, (HoverKind <> hkNone), Selected);
        C.Handle := 0;
        C.Free;
      finally
        RestoreDC(DC, -1);
      end;
      Exit;
    end;

    HiContrast := ColorIntensity(GetItemImageBackground(ItemInfo)) < 80;
    if not Enabled then
    begin
      if not HiContrast then
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 0)
      else
        DrawTBXIconFlatShadow(DC, ARect, ImageList, ImageIndex, BtnShadow);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if (HoverKind <> hkNone) and not Selected and not IsPopupParent then
      begin
        if Pushed then
        begin
          OffsetRect(ARect, -1, -1);
          DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
          OffsetRect(ARect, 2, 2);
          DrawTBXIconFullShadow(DC, ARect, ImageList, ImageIndex, Lighten(BtnItemColors[bisPressed, ipBody], 24));
          OffsetRect(ARect, -1, -1);
        end
        else
        begin
          OffsetRect(ARect, -1, -1);
          DrawTBXIconFullShadow(DC, ARect, ImageList, ImageIndex, Lighten(BtnItemColors[bisHot, ipBody], 16));
          OffsetRect(ARect, 2, 2);
          DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
          OffsetRect(ARect, -1, -1);
        end;
      end;
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast then
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
    else
      BlendTBXIcon(DC, ARect, ImageList, ImageIndex, 200);
  end;
end;

procedure TTBXAluminumTheme.PaintMenuItemFrame(DC: HDC;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
begin
  with ItemInfo do if (Enabled and (HoverKind <> hkNone)) or
    (not Enabled and (HoverKind = hkKeyboardHover)) then
  begin
    RoundFill(DC, ARect, 1, 1, 1, 1, GetPartColor(ItemInfo, ipBody));
    RoundFrame(DC, ARect, 1, 1, 1, 1, GetPartColor(ItemInfo, ipFrame));
  end;  
end;

procedure TTBXAluminumTheme.PaintMenuItem(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  R: TRect;
  ShowImageOrCheck: Boolean;
  IsCombo: Boolean;
  X, Y: Integer;
  ArrowWidth: Integer;
  C, ClrText: TColor;

  procedure DrawArrow(AColor: TColor);
  begin
    PolygonEx(DC, [Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)], AColor, AColor);
  end;

begin
  with ItemInfo do
  begin
    ShowImageOrCheck := (ImageWidth > 0) or Selected;
    IsCombo := ((ItemOptions and IO_COMBO) <> 0);
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);
    ClrText := GetPartColor(ItemInfo, ipText);

    R := ARect;
    if ShowImageOrCheck then Inc(R.Left, ItemInfo.PopupMargin + MenuImageTextSpace);

    PaintMenuItemFrame(DC, R, ItemInfo);

    if IsCombo then
    begin
      X := R.Right - ArrowWidth - 1;
      if not ItemInfo.Enabled then C := ClrText
      else if HoverKind = hkMouseHover then C := GetPartColor(ItemInfo, ipFrame)
      else C := BtnShadow;
      DrawLineEx(DC, X, R.Top + 1, X, R.Bottom + 1, C);
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3;
      DrawArrow(ClrText);
    end;

    if ShowImageOrCheck and ((HoverKind <> hkNone) or Selected) then
    begin
      R.Left := ARect.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      PaintButton(DC, R, ItemInfo);
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintPopupNCArea(DC: HDC; R: TRect; const PopupInfo: TTBXPopupInfo);
var
  PR: TRect;
  C: TColor;
begin
  FrameRectEx(DC, R, PopupFrameColor, True);
  FillRectEx(DC, R, PopupColor);
  if not IsRectEmpty(PopupInfo.ParentRect) then
  begin
    PR := PopupInfo.ParentRect;
    if not IsRectEmpty(PR) then with PR do
    begin
      C := BtnItemColors[bisPopupParent, ipFrame];
      if Bottom = R.Top then
      begin
        if Left <= R.Left then Left := R.Left - 1;
        if Right >= R.Right then Right := R.Right + 1;
        DitherRect(DC, Rect(Left + 1, Bottom - 1, Right - 1, Bottom), PopupColor, C);
      end
      else if Top = R.Bottom then
      begin
        if Left <= R.Left then Left := R.Left - 1;
        if Right >= R.Right then Right := R.Right + 1;
        DitherRect(DC, Rect(Left + 1, Top, Right - 1, Top + 1), PopupColor, C);
      end;
      if Right = R.Left then
      begin
        if Top <= R.Top then Top := R.Top - 1;
        if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
        DitherRect(DC, Rect(Right - 1, Top + 1, Right, Bottom - 1), PopupColor, C);
      end
      else if Left = R.Right then
      begin
        if Top <= R.Top then Top := R.Top - 1;
        if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
        DitherRect(DC, Rect(Left, Top + 1, Left + 1, Bottom - 1), PopupColor, C);
      end;
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintSeparator(DC: HDC; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
begin
  { Note: for blank separators, Enabled = False }
  with ItemInfo, ARect do if Enabled then
  begin
    if Horizontal then
    begin
      Top := (Top + Bottom) div 2;
      Inc(Left, 2); Dec(Right, 2);
      DrawLineEx(DC, Left, Top, Right, Top, SeparatorColor);
    end
    else
    begin
      Left := (Left + Right) div 2;
      Inc(Top, 2); Dec(Bottom, 2);
      DrawLineEx(DC, Left, Top, Left, Bottom, SeparatorColor);
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintToolbarNCArea(DC: HDC; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
{const
  Pattern: array [0..15] of Byte = ($C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0, 0, 0);  }
var
  Sz: Integer;
  R2: TRect;
  ToolbarColor, C, CHi, CLo, BorderColor: TColor;
  BtnVisible, Horz, CloseButtondown, CloseButtonHover: Boolean;
  BtnItemState: TBtnItemState;

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

begin
  ToolbarColor := ToolbarInfo.Color;
  if ToolbarColor = clDefault then ToolbarColor := GetViewColor(ToolbarInfo.ViewType);

  { Border }
  CHi := Lighten(ToolbarColor, 32);
  CLo := Lighten(ToolbarColor, -96);
  if ToolbarInfo.BorderStyle = bsNone then
  begin
    BrushedFill(DC, nil, R, ToolbarColor, 12);
    InflateRect(R, -2, -2);
  end
  else
  begin
    InflateRect(R, -1, -1);
    BrushedFill(DC, nil, R, ToolbarColor, 12);
    InflateRect(R, 1, 1);
    RoundBevel(DC, R, 2, 2, 2, 2, CHi, CLo, DockColor);
    InflateRect(R, -2, -2);
  end;

  if not ToolbarInfo.AllowDrag then Exit;

  BtnVisible := (ToolbarInfo.CloseButtonState and CDBS_VISIBLE) <> 0;
  Sz := GetTBXDragHandleSize(ToolbarInfo);
  Horz := not ToolbarInfo.IsVertical;
  if Horz then R.Right := R.Left + Sz
  else R.Bottom := R.Top + Sz;

  { Drag Handle }
  if ToolbarInfo.DragHandleStyle <> DHS_NONE then
  begin
    R2 := R;
    if Horz then
    begin
      Dec(R2.Right, 2);
      if BtnVisible then Inc(R2.Top, Sz - 2);
    end
    else
    begin
      Dec(R2.Bottom, 2);
      if BtnVisible then Dec(R2.Right, Sz - 2);
    end;
    InflateRect(R2, -1, -1);
    DotFill(DC, R2, ToolbarColor, 1);
  end;

  { Close button }
  if BtnVisible then
  begin
    CloseButtonDown := (ToolbarInfo.CloseButtonState and CDBS_PRESSED) <> 0;
    CloseButtonHover := (ToolbarInfo.CloseButtonState and CDBS_HOT) <> 0;
    R2 := GetTBXDockedCloseButtonRect(ToolbarInfo);
    if Horz then OffsetRect(R2, -1, 0)
    else Offsetrect(R2, 0, -1);
    BtnItemState := GetBtnItemState(ToolbarInfo.CloseButtonState);
    C := BtnItemColors[BtnItemState, ipBody];
    BorderColor := BtnItemColors[BtnItemState, ipFrame];
    RoundFill(DC, R2, 1, 1, 1, 1, C);
    RoundFrame(DC, R2, 1, 1, 1, 1, BorderColor);
    DrawButtonBitmap(DC, R2, C, CloseButtonDown or CloseButtonHover);
  end;
end;

procedure TTBXAluminumTheme.PaintMDIButton(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
  X, Y: Integer;
  C: TColor;
begin
  PaintButton(DC, ARect, ItemInfo);
  with ARect do
  begin
    X := (Left + Right - StockImgList.Width) div 2;
    Y := (Top + Bottom - StockImgList.Height - 1) div 2;
  end;
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  C := GetBtnColor(ItemInfo, ipBody);
  if C = clNone then C := ToolbarColor;
  DrawGlyph(DC, X + 1, Y + 1, StockImgList, Index, Lighten(C, 24));
  DrawGlyph(DC, X - 1, Y - 1, StockImgList, Index, Lighten(C, -32));
  if ItemInfo.HoverKind <> hkNone then C := $000000 else C := $3F3F3F;
  DrawGlyph(DC, X, Y, StockImgList, Index, C);
end;

function TTBXAluminumTheme.GetPopupShadowType: Integer;
begin
  Result := PST_OFFICEXP;
end;

constructor TTBXAluminumTheme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then InitializeStock;
  Inc(CounterLock);
end;

destructor TTBXAluminumTheme.Destroy;
begin
  Dec(CounterLock);
  if CounterLock = 0 then FinalizeStock;
  inherited;
end;

procedure TTBXAluminumTheme.PaintDock(DC: HDC; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
begin
  BrushedFill(DC, @(ClientRect.TopLeft), DockRect, DockColor, 8);
end;

procedure TTBXAluminumTheme.PaintDockPanelNCArea(DC: HDC; R: TRect;
  const DockPanelInfo: TTBXDockPanelInfo);
var
  Sz: Integer;
  R2: TRect;
  Flags: Integer;
  CloseButtonDown, CloseButtonHover: Boolean;
  PanelColor, C, BorderColor: TColor;
  BtnItemState: TBtnItemState;
  OldFont: HFont;
  OldTextColor: TColorRef;
  OldBkMode: Cardinal;

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

begin
  with DockPanelInfo do
  begin
    PanelColor := Color;
    if PanelColor = clDefault then PanelColor := GetViewColor(ViewType);
    Sz := GetSystemMetrics(SM_CYSMCAPTION);

    { Border }
    FrameRectEx(DC, R, DockColor, True);
    FrameRectEx(DC, R, PanelColor, False);
    C := ColorToRGB(DockColor);
    SetPixelV(DC, R.Left, R.Top, C);
    SetPixelV(DC, R.Right - 1, R.Top, C);
    SetPixelV(DC, R.Left, R.Bottom - 1, C);
    SetPixelV(DC, R.Right - 1, R.Bottom - 1, C);

    if ShowCaption then
    begin
      if IsVertical then
      begin
        R.Bottom := R.Top + Sz;
        DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, PanelColor);
      end
      else
      begin
        R.Right := R.Left + Sz;
        DrawLineEx(DC, R.Right, R.Top, R.Right, R.Bottom, PanelColor);
      end;

      InflateRect(R, -1, -1);
      DrawCaptionArea(DC, R, ToolbarColor, IsVertical);
      InflateRect(R, 1, 1);

      RoundBevel(DC, R, 2, 2 * Integer(IsVertical), 2 * Integer(not IsVertical), 0,
        Lighten(ToolbarColor, 32), Lighten(ToolbarColor, -64), DockColor);
      InflateRect(R, -1, -1);

      { Close button }
      if (CDBS_VISIBLE and CloseButtonState) <> 0 then
      begin
        CloseButtonDown := (CloseButtonState and CDBS_PRESSED) <> 0;
        CloseButtonHover := (CloseButtonState and CDBS_HOT) <> 0;
        R2 := R;

        if IsVertical then
        begin
          Dec(R2.Right);
          R2.Left := R2.Right - Sz + 1;
          R.Right := R2.Left;
        end
        else
        begin
          Dec(R2.Bottom);
          R2.Top := R2.Bottom - Sz + 2;
          R.Bottom := R2.Top;
        end;

        InflateRect(R2, -1, -1);
        BtnItemState := GetBtnItemState(CloseButtonState);
        C := BtnItemColors[BtnItemState, ipBody];
        BorderColor := BtnItemColors[BtnItemState, ipFrame];
        RoundFill(DC, R2, 1, 1, 1, 1, C);
        RoundFrame(DC, R2, 1, 1, 1, 1, BorderColor);
        DrawButtonBitmap(DC, R2, C, CloseButtonDown or CloseButtonHover);
      end;

      { Caption }
      if IsVertical then InflateRect(R, -1, 0)
      else Inflaterect(R, 0, -1);
      OldFont := SelectObject(DC, SmCaptionFont.Handle);
      OldTextColor := SetTextColor(DC, ColorToRGB(BtnText));
      OldBkMode := SetBkMode(DC, TRANSPARENT);
      Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
      if IsVertical then _DrawTextW(DC, Caption, -1, R, Flags)
      else DrawRotatedTextW(DC, Caption, R, Flags);
      SetBkMode(DC, OldBkMode);
      SetTextColor(DC, OldTextColor);
      SelectObject(DC, OldFont);
    end;
  end;
end;

procedure TTBXAluminumTheme.PaintPageScrollButton(DC: HDC;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
const
  GRAD_KINDS: array [PSBT_UP..PSBT_RIGHT] of TGradientKind = (gkVert, gkVert, gkHorz, gkHorz);
var
  R: TRect;
  X, Y, Sz: Integer;
  C: TColor;
begin
  R := ARect;
  InflateRect(R, -1, -1);

  if Hot then FillRectEx(DC, R, BtnItemColors[bisHot, ipBody])
  else GradFill(DC, R, Lighten(ToolbarColor, 12), Lighten(ToolbarColor, -8), GRAD_KINDS[ButtonType]);

  InflateRect(R, 1, 1);
  C := GetSysColor(COLOR_BTNFACE);
  with R do
  begin
    SetPixelV(DC, Left, Top, C);
    SetPixelV(DC, Right - 1, Top, C);
    SetPixelV(DC, Right - 1, Bottom - 1, C);
    SetPixelV(DC, Left, Bottom - 1, C);
  end;

  if Hot then C := BtnItemColors[bisHot, ipFrame] else C := MixColors(BtnShadow, clWindow, 192);
  RoundFrame(DC, R, 1, 1, 1, 1, C);

  { Arrow }
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
  case ButtonType of
    PSBT_UP:
      begin
        Inc(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)], BtnText, BtnText);
      end;
    PSBT_DOWN:
      begin
        Y := (R.Top + R.Bottom - 1) div 2;
        Dec(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)], BtnText, BtnText);
      end;
    PSBT_LEFT:
      begin
        Inc(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)], BtnText, BtnText);
      end;
    PSBT_RIGHT:
      begin
        X := (R.Left + R.Right - 1) div 2;
        Dec(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)], BtnText, BtnText);
      end;
  end;
end;

procedure TTBXAluminumTheme.PaintFrameControl(DC: HDC; R: TRect; Kind, State: Integer; Params: Pointer);
var
  X, Y: Integer;
  C: TColor;
  Pen, OldPen: HPEN;
  Brush, OldBrush: HBRUSH;

  procedure SetupPen;
  begin
    if Boolean(State and PFS_DISABLED) then C := BtnShadow
    else if Boolean(State and PFS_PUSHED) then C := BtnItemColors[bisPressed, ipFrame]
    else if Boolean(State and PFS_HOT) then C := BtnItemColors[bisHot, ipFrame]
    else C := BtnShadow;
    Pen := CreatePenEx(C);
  end;

  procedure SetupBrush;
  begin
    if Boolean(State and PFS_DISABLED) then Brush := CreateBrushEx(clNone)
    else if Boolean(State and PFS_PUSHED) then Brush := CreateBrushEx(BtnItemColors[bisPressed, ipBody])
    else if Boolean(State and PFS_HOT) then Brush := CreateBrushEx(BtnItemColors[bisHot, ipBody])
    else if Boolean(State and PFS_MIXED) then Brush := CreateDitheredBrush(clWindow, clBtnFace)
    else Brush := CreateBrushEx(clWindow);
  end;

  function TextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := BtnItemColors[bisDisabled, ipText]
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipText]
    else if Boolean(State and PFS_MIXED) then Result := BtnShadow
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipText]
    else Result := BtnItemColors[bisNormal, ipText];
  end;

begin
  case Kind of
    PFC_CHECKBOX:
      begin
        InflateRect(R, -1, -1);
        SetupPen;
        SetupBrush;
        OldPen := SelectObject(DC, Pen);
        OldBrush := SelectObject(DC, Brush);
        with R do Rectangle(DC, Left, Top, Right, Bottom);
        SelectObject(DC, OldPen);
        SelectObject(DC, OldBrush);
        DeleteObject(Pen);
        DeleteObject(Brush);

        if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
        begin
          X := (R.Left + R.Right) div 2 - 1;
          Y := (R.Top + R.Bottom) div 2 + 1;
          C := TextColor;
          PolygonEx(DC, [Point(X-2, Y), Point(X, Y+2), Point(X+4, Y-2),
            Point(X+4, Y-4), Point(X, Y), Point(X-2, Y-2), Point(X-2, Y)], C, C);
        end;
      end;
    PFC_RADIOBUTTON:
      begin
        SetupPen;
        SetupBrush;
        InflateRect(R, -1, -1);
        OldPen := SelectObject(DC, Pen);
        OldBrush := SelectObject(DC, Brush);
        with R do Ellipse(DC, Left, Top, Right, Bottom);
        SelectObject(DC, OldPen);
        SelectObject(DC, OldBrush);
        DeleteObject(Pen);
        DeleteObject(Brush);
        if Boolean(State and PFS_CHECKED) then
        begin
          InflateRect(R, -3, -3);
          C := TextColor;
          Brush := CreateBrushEx(C);
          OldBrush := SelectObject(DC, Brush);
          Pen := CreatePenEx(C);
          OldPen := SelectObject(DC, Pen);
          with R do Windows.Ellipse(DC, Left, Top, Right, Bottom);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
          SelectObject(DC, OldBrush);
          DeleteObject(Brush);
        end;
      end;
  else
    inherited;
  end;
end;

procedure TTBXAluminumTheme.PaintStatusBar(DC: HDC; R: TRect; Part: Integer);
var
  D, Sz, I: Integer;
  Lo, Hi: TColor;

  procedure DiagLine(C: TColor);
  begin
    with R do DrawLineEx(DC, Right - 2 - D, Bottom - 2, Right - 1, Bottom - D - 3, C);
    Inc(D);
  end;

begin
  case Part of
    SBP_BODY:
      begin
        GradFill(DC, Rect(R.Left, R.Top, R.Right, R.Top + 5), Lighten(StatusBarColor, -16), StatusBarColor, gkVert);
        BrushedFill(DC, nil, Rect(R.Left, R.Top + 5, R.Right, R.Bottom), StatusBarColor, DefaultRoughness);
      end;
    SBP_PANE, SBP_LASTPANE:
      begin
        if Part = SBP_PANE then Dec(R.Right, 3);
        DrawLineEx(DC, R.Right, R.Top + 4, R.Right, R.Bottom - 3, SeparatorColor);
      end;
    SBP_GRIPPER:
      begin
        Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
        Hi := NearestLighten(StatusBarColor, 64);
        Lo := BtnText;

        D := 2;
        for I := 1 to 3 do
        begin
          case Sz of
          0..8:
            begin
              DiagLine(Hi);
              DiagLine(Lo);
            end;
          9..12:
            begin
              DiagLine(Hi);
              DiagLine(Lo);
              Inc(D);
            end;
          else
            DiagLine(Hi);
            DiagLine(Lo);
            Inc(D, 2);
          end;
        end;
      end;
  end;
end;

procedure TTBXAluminumTheme.SetupColorCache;
var
  DC: HDC;
  HotBtnFace, DisabledText, Highlight: TColor;
  H, L, S: Single;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then C := GetNearestColor(DC, ColorToRGB(C));
  end;

begin
  DC := StockCompatibleBitmap.Canvas.Handle;

  DefaultRoughness := 12;

  RGBtoHSL(clHighlight, H, L, S);
  if S < 0.4 then S := 0.4;
  Highlight := HSLtoRGB(H, L, S);

  DockColor := Lighten(BaseColor, -24);
  MenuBarColor := BaseColor;
  ToolbarColor := BaseColor;
  StatusBarColor := Lighten(BaseColor, -16);
  PopupColor := Blend(DockColor, clWindow, 100);
  DockPanelColor := PopupColor;
  PopupFrameColor := Blend(BtnText, BtnShadow, 20);
  SetContrast(PopupFrameColor, PopupColor, 100);

  HotBtnFace := Blend(Highlight, clWindow, 30);
  DisabledText := Blend(BtnShadow, clWindow, 90);

  WinFrameColors[wfsActive, wfpBorder]        := Blend(BtnText, BtnShadow, 15);
  SetContrast(WinFrameColors[wfsActive, wfpBorder], ToolbarColor, 120);
  WinFrameColors[wfsActive, wfpCaption]       := BtnShadow;
  WinFrameColors[wfsActive, wfpCaptionText]   := BtnHighlight;
  SetContrast(WinFrameColors[wfsActive, wfpCaptionText], BtnShadow, 180);
  WinFrameColors[wfsInactive, wfpBorder]      := WinFrameColors[wfsActive, wfpBorder];
  WinFrameColors[wfsInactive, wfpCaption]     := BtnFace;
  WinFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
  SetContrast(WinFrameColors[wfsInactive, wfpCaptionText], BtnFace, 120);

  PnlFrameColors[wfsActive, wfpBorder]        := BtnShadow;
  PnlFrameColors[wfsActive, wfpCaption]       := BtnFace;
  PnlFrameColors[wfsActive, wfpCaptionText]   := BtnText;
  PnlFrameColors[wfsInactive, wfpBorder]      := BtnShadow;
  PnlFrameColors[wfsInactive, wfpCaption]     := BtnFace;
  PnlFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
  SetContrast(PnlFrameColors[wfsInactive, wfpCaptionText], BtnFace, 120);

  BtnItemColors[bisNormal, ipBody]           := clNone;
  BtnItemColors[bisNormal, ipText]           := BtnText;
  SetContrast(BtnItemColors[bisNormal, ipText], ToolbarColor, 180);
  BtnItemColors[bisNormal, ipFrame]          := clNone;
  BtnItemColors[bisDisabled, ipBody]         := clNone;
  BtnItemColors[bisDisabled, ipText]         := BtnShadow;
  SetContrast(BtnItemColors[bisDisabled, ipText], ToolbarColor, 80);
  BtnItemColors[bisDisabled, ipFrame]        := clNone;
  BtnItemColors[bisSelected, ipBody]         := Blend(Highlight, Blend(BtnFace, clWindow, 50), 10);
  SetContrast(BtnItemColors[bisSelected, ipBody], ToolbarColor, 5);
  BtnItemColors[bisSelected, ipText]         := BtnItemColors[bisNormal, ipText];
  BtnItemColors[bisSelected, ipFrame]        := clHighlight;
  BtnItemColors[bisPressed, ipBody]          := Blend(Highlight, clWindow, 50);
  BtnItemColors[bisPressed, ipText]          := clHighlightText;
  BtnItemColors[bisPressed, ipFrame]         := clHighlight;
  BtnItemColors[bisHot, ipBody]              := HotBtnFace;
  BtnItemColors[bisHot, ipText]              := clMenuText;
  SetContrast(BtnItemColors[bisHot, ipText], BtnItemColors[bisHot, ipBody], 180);
  BtnItemColors[bisHot, ipFrame]             := clHighlight;
  SetContrast(BtnItemColors[bisHot, ipFrame], ToolbarColor, 100);
  BtnItemColors[bisDisabledHot, ipBody]      := HotBtnFace;
  BtnItemColors[bisDisabledHot, ipText]      := DisabledText;
  BtnItemColors[bisDisabledHot, ipFrame]     := clHighlight;
  BtnItemColors[bisSelectedHot, ipBody]      := Blend(Highlight, clWindow, 50);
  SetContrast(BtnItemColors[bisSelectedHot, ipBody], ToolbarColor, 30);
  BtnItemColors[bisSelectedHot, ipText]      := clHighlightText;
  SetContrast(BtnItemColors[bisSelectedHot, ipText], BtnItemColors[bisSelectedHot, ipBody], 180);
  BtnItemColors[bisSelectedHot, ipFrame]     := clHighlight;
  SetContrast(BtnItemColors[bisSelectedHot, ipFrame], BtnItemColors[bisSelectedHot, ipBody], 100);
  BtnItemColors[bisPopupParent, ipBody]      := Blend(ToolbarColor, PopupColor, 50);
  BtnItemColors[bisPopupParent, ipText]      := BtnItemColors[bisNormal, ipText];
  BtnItemColors[bisPopupParent, ipFrame]     := PopupFrameColor;

  MenuItemColors[misNormal, ipBody]          := clNone;
  MenuItemColors[misNormal, ipText]          := clWindowText;
  SetContrast(MenuItemColors[misNormal, ipText], PopupColor, 180);
  MenuItemColors[misNormal, ipFrame]         := clNone;
  MenuItemColors[misDisabled, ipBody]        := clNone;
  MenuItemColors[misDisabled, ipText]        := Blend(clGrayText, clWindow, 70);
  SetContrast(MenuItemColors[misDisabled, ipText], PopupColor, 80); 
  MenuItemColors[misDisabled, ipFrame]       := clNone;
  MenuItemColors[misHot, ipBody]             := BtnItemColors[bisHot, ipBody];
  MenuItemColors[misHot, ipText]             := BtnItemColors[bisHot, ipText];
  MenuItemColors[misHot, ipFrame]            := BtnItemColors[bisHot, ipFrame];
  MenuItemColors[misDisabledHot, ipBody]     := PopupColor;
  MenuItemColors[misDisabledHot, ipText]     := Blend(clGrayText, clWindow, 70);
  MenuItemColors[misDisabledHot, ipFrame]    := clHighlight;

  { Other Colors }
  SeparatorColor := BtnShadow;
  SetContrast(SeparatorColor, ToolbarColor, 50);

  Undither(MenubarColor);
  Undither(ToolbarColor);
  Undither(PopupColor);
  Undither(DockPanelColor);
  Undither(PopupFrameColor);
  Undither(WinFrameColors[wfsActive, wfpBorder]);
  Undither(WinFrameColors[wfsActive, wfpCaption]);
  Undither(WinFrameColors[wfsActive, wfpCaptionText]);
  Undither(WinFrameColors[wfsInactive, wfpBorder]);
  Undither(WinFrameColors[wfsInactive, wfpCaption]);
  Undither(WinFrameColors[wfsInactive, wfpCaptionText]);
  Undither(PnlFrameColors[wfsActive, wfpBorder]);
  Undither(PnlFrameColors[wfsActive, wfpCaption]);
  Undither(PnlFrameColors[wfsActive, wfpCaptionText]);
  Undither(PnlFrameColors[wfsInactive, wfpBorder]);
  Undither(PnlFrameColors[wfsInactive, wfpCaption]);
  Undither(PnlFrameColors[wfsInactive, wfpCaptionText]);
  Undither(BtnItemColors[bisNormal, ipBody]);
  Undither(BtnItemColors[bisNormal, ipText]);
  Undither(BtnItemColors[bisNormal, ipFrame]);
  Undither(BtnItemColors[bisDisabled, ipBody]);
  Undither(BtnItemColors[bisDisabled, ipText]);
  Undither(BtnItemColors[bisDisabled, ipFrame]);
  Undither(BtnItemColors[bisSelected, ipBody]);
  Undither(BtnItemColors[bisSelected, ipText]);
  Undither(BtnItemColors[bisSelected, ipFrame]);
  Undither(BtnItemColors[bisPressed, ipBody]);
  Undither(BtnItemColors[bisPressed, ipText]);
  Undither(BtnItemColors[bisPressed, ipFrame]);
  Undither(BtnItemColors[bisHot, ipBody]);
  Undither(BtnItemColors[bisHot, ipText]);
  Undither(BtnItemColors[bisHot, ipFrame]);
  Undither(BtnItemColors[bisDisabledHot, ipBody]);
  Undither(BtnItemColors[bisDisabledHot, ipText]);
  Undither(BtnItemColors[bisDisabledHot, ipFrame]);
  Undither(BtnItemColors[bisSelectedHot, ipBody]);
  Undither(BtnItemColors[bisSelectedHot, ipText]);
  Undither(BtnItemColors[bisSelectedHot, ipFrame]);
  Undither(BtnItemColors[bisPopupParent, ipBody]);
  Undither(BtnItemColors[bisPopupParent, ipText]);
  Undither(BtnItemColors[bisPopupParent, ipFrame]);
  Undither(MenuItemColors[misNormal, ipBody]);
  Undither(MenuItemColors[misNormal, ipText]);
  Undither(MenuItemColors[misNormal, ipFrame]);
  Undither(MenuItemColors[misDisabled, ipBody]);
  Undither(MenuItemColors[misDisabled, ipText]);
  Undither(MenuItemColors[misDisabled, ipFrame]);
  Undither(MenuItemColors[misHot, ipBody]);
  Undither(MenuItemColors[misHot, ipText]);
  Undither(MenuItemColors[misHot, ipFrame]);
  Undither(MenuItemColors[misDisabledHot, ipBody]);
  Undither(MenuItemColors[misDisabledHot, ipText]);
  Undither(MenuItemColors[misDisabledHot, ipFrame]);
  Undither(SeparatorColor);
end;

function TTBXAluminumTheme.GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
const
  BFlags1: array [Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array [Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array [Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  B: TBtnItemState;
  Embedded: Boolean;
begin
  with ItemInfo do
  begin
    Embedded := (ViewType and VT_CATEGORYMASK = VT_BARS) and (ViewType and VT_EMBEDDED <> 0);
    if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
    else if ItemInfo.IsPopupParent then B := bisPopupParent
    else if Pushed then B := bisPressed
    else if Selected then B := BFlags2[HoverKind <> hkNone]
    else B := BFlags3[HoverKind <> hkNone];
    Result := BtnItemColors[B, ItemPart];
    if Embedded then
    begin
      if (ItemPart = ipBody) and (Result = clNone) then Result := ToolbarColor;
      if ItemPart = ipFrame then
      begin
        if Selected then Result := clWindowFrame
        else if (Result = clNone) then Result := BtnShadow;
      end;
    end;
  end;
end;

function TTBXAluminumTheme.GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
const
  MFlags1: array [Boolean] of TMenuItemState = (misDisabled, misDisabledHot);
  MFlags2: array [Boolean] of TMenuItemState = (misNormal, misHot);
  BFlags1: array [Boolean] of TBtnItemState = (bisDisabled, bisDisabledHot);
  BFlags2: array [Boolean] of TBtnItemState = (bisSelected, bisSelectedHot);
  BFlags3: array [Boolean] of TBtnItemState = (bisNormal, bisHot);
var
  IsMenuItem, Embedded: Boolean;
  M: TMenuItemState;
  B: TBtnItemState;
begin
  with ItemInfo do
  begin
    IsMenuItem := (ViewType and VT_TYPEMASK = VT_POPUPMENU) and (ItemOptions and IO_TOOLBARSTYLE = 0);
    Embedded := (ViewType and VT_CATEGORYMASK = VT_BARS) and (ViewType and VT_EMBEDDED <> 0);
    if IsMenuItem then
    begin
      if not Enabled then M := MFlags1[HoverKind = hkKeyboardHover]
      else M := MFlags2[HoverKind <> hkNone];
      Result := MenuItemColors[M, ItemPart];
    end
    else
    begin
      if not Enabled then B := BFlags1[HoverKind = hkKeyboardHover]
      else if ItemInfo.IsPopupParent then B := bisPopupParent
      else if Pushed then B := bisPressed
      else if Selected then B := BFlags2[HoverKind <> hkNone]
      else B := BFlags3[HoverKind <> hkNone];
      Result := BtnItemColors[B, ItemPart];
      if Embedded and (Result = clNone) then
      begin
        if ItemPart = ipBody then Result := ToolbarColor;
        if ItemPart = ipFrame then Result := BtnShadow;
      end;
    end;
  end;
end;

function TTBXAluminumTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXAluminumTheme.GetImageOffset(DC: HDC;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

procedure TTBXAluminumTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
begin
  inherited;
  if MarginID = MID_MENUITEM then
  begin
    Margins.TopHeight := 2; Margins.BottomHeight := 2;
  end;
end;

initialization

RegisterTBXTheme('Aluminum', TTBXAluminumTheme);

end.
