unit TBXStripesTheme;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXStripesTheme.pas 133 2005-11-08 20:00:43Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Graphics, TBXThemes, TBXDefaultTheme, ImgList;

type
  TTBXStripesTheme = class(TTBXDefaultTheme)
  protected
    DockColor: TColor;
    DockPanelColor: TColor;
    DisabledColor: TColor;
    procedure SetupColorCache; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics access, etc. }
    function  GetBooleanMetrics(Index: Integer): Boolean; override;
    function  GetIntegerMetrics(Index: Integer): Integer; override;
    procedure GetMargins(MarginID: Integer; out Margins: TTBXMargins); override;
    function  GetItemColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor; override;
    function  GetPopupShadowType: Integer; override;
    procedure GetViewBorder(ViewType: Integer; out Border: TPoint); override;
    function  GetViewColor(ViewType: Integer): TColor; override;

    { Painting routines }
    procedure PaintBackgnd(DC: HDC; const ADockRect, ARect, AClipRect: TRect; AColor: TColor; AViewType: Integer); override;
    procedure PaintButton(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintCaption(DC: HDC; const Rect: TRect; const ItemInfo: TTBXItemInfo; const Caption: WideString; Format: Cardinal; Color: TColor); override;
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

uses TBXUtils, TB2Item, TB2Common, Classes, Controls, Forms, Commctrl;

var
  StockImgList: TImageList;
  StockPatternBitmap: TBitmap;
  CounterLock: Integer = 0;

procedure InitializeStock;
begin
  StockPatternBitmap := TBitmap.Create;
  StockPatternBitmap.Width := 8;
  StockPatternBitmap.Height := 8;
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
end;

procedure FinalizeStock;
begin
  StockPatternBitmap.Free;
  StockImgList.Free;
end;

procedure CreateDottedPattern(Color: TColor);
var
  Hi, Lo: TColor;
begin
  with StockPatternBitmap.Canvas do
  begin
    Brush.Color := Color;
    Hi := GetNearestColor(Handle, MixColors(Color, clBtnHighlight, 32));
    Lo := GetNearestColor(Handle, MixColors(Color, clBtnShadow, 64));
    FillRect(Rect(0, 0, 8, 8));
    Pixels[0, 0] := Hi;  Pixels[1, 1] := Lo;
    Pixels[4, 0] := Hi;  Pixels[5, 1] := Lo;
    Pixels[0, 4] := Hi;  Pixels[1, 5] := Lo;
    Pixels[4, 4] := Hi;  Pixels[5, 5] := Lo;
  end;
end;

procedure DotFill(DC: HDC; R: TRect; Color: TColor; Border: Integer);
var
  Pt: TPoint;
  W, H, I, J: Integer;
  Brush: HBRUSH;
  CHi, CLo: TColorRef;
begin
  if Color <> clNone then
  begin
    CreateDottedPattern(Color);
    FillRectEx(DC, R, Color);
  end;
  InflateRect(R, -Border, -Border);
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  W := ((W - 2) div 4) * 4 + 2;
  H := ((H - 2) div 4) * 4 + 2;
  R.Left := (R.Right + R.Left - W - 1) div 2;
  R.Right := R.Left + W;
  R.Top := (R.Top + R.Bottom - H - 1) div 2;
  R.Bottom := R.Top + H;

  if Color <> clNone then
  begin
    Brush := CreatePatternBrush(StockPatternBitmap.Handle);
    GetWindowOrgEx(DC, Pt);
    SetBrushOrgEx(DC, R.Left - Pt.X, R.Top - Pt.Y, nil);
    Windows.FillRect(DC, R, Brush);
    DeleteObject(Brush);
  end
  else
  begin
    CHi := GetSysColor(COLOR_BTNHIGHLIGHT);
    CLo := GetSysColor(COLOR_BTNSHADOW);
    for J := 0 to (R.Bottom - R.Top - 1) div 4 do
      for I := 0 to (R.Right - R.Left - 1) div 4 do
        begin
          Windows.SetPixelV(DC, R.Left + I * 4 + 1, R.Top + J * 4 + 1, CHi);
          Windows.SetPixelV(DC, R.Left + I * 4 + 2, R.Top + J * 4 + 2, CLo);
        end;
  end;
end;

procedure DrawButtonBitmap(DC: HDC; R: TRect);
const
  Pattern: array [0..15] of Byte = ($C6, 0, $6C, 0, $38, 0, $38, 0, $6C, 0, $C6, 0, 0, 0, 0, 0);
begin
  DrawGlyph(DC, R, 7, 6, Pattern[0], clBtnText);
end;

procedure RoundFrame(DC: HDC; R: TRect; RL, RR: Integer; C: TColor);
var
  P: array [0..8] of TPoint;
  OldPen, Pen: HPen;
begin
  if C < 0 then C := GetSysColor(C and $FF);
  with R do
  begin
    Dec(Right); Dec(Bottom);
    with P[0] do begin X := Left + RL; Y := Top; end;
    with P[1] do begin X := Right - RR; Y := Top; end;
    with P[2] do begin X := Right; Y := Top + RR; end;
    with P[3] do begin X := Right; Y := Bottom - RR; end;
    with P[4] do begin X := Right - RR; Y := Bottom; end;
    with P[5] do begin X := Left + RL; Y := Bottom; end;
    with P[6] do begin X := Left; Y := Bottom - RL; end;
    with P[7] do begin X := Left; Y := Top + RL; end;
    with P[8] do begin X := Left + RL; Y := Top; end;
    Pen := CreatePen(PS_SOLID, 1, C);
    OldPen := SelectObject(DC, Pen);
    Windows.Polyline(DC, P[0], 9);
    SelectObject(DC, OldPen);
    DeleteObject(Pen);
    Inc(Right); Inc(Bottom);
  end;
end;

{ TTBXStripesTheme }

function TTBXStripesTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_EDITHEIGHTEVEN:            Result := False;
    TMB_PAINTDOCKBACKGROUND:       Result := False;
    TMB_SOLIDTOOLBARNCAREA:        Result := True;
    TMB_SOLIDTOOLBARCLIENTAREA:    Result := True;
  else
    Result := inherited GetBooleanMetrics(Index);
  end;
end;

function TTBXStripesTheme.GetViewColor(ViewType: Integer): TColor;
begin
  Result := ToolbarColor;
  case ViewType and VT_CATEGORYMASK of
    VT_BARS: Result := ToolbarColor;
    VT_POPUPS:
      case ViewType and VT_TYPEMASK of
        VT_POPUPMENU: Result := clPopup;
        VT_LISTBOX: Result := clWindow;
        VT_TOOLBOX: Result := ToolbarColor;
        VT_CHEVRONMENU: Result := clPopup;
      end;
    VT_STATUSBAR: Result := StatusBarColor;
    VT_DOCKPANEL: Result := DockPanelColor;
    VT_TOOLWINDOW: Result := ToolbarColor;
    VT_DOCK: Result := DockColor;
  end;
end;

function TTBXStripesTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := inherited GetItemColor(ItemInfo);
end;

function TTBXStripesTheme.GetIntegerMetrics(Index: Integer): Integer;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:         Result := 12;
    TMI_MENU_MDI_DW:                 Result := 1;
    TMI_MENU_MDI_DH:                 Result := 2;
    TMI_EDIT_FRAMEWIDTH:             Result := 2;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 1;
    TMI_EDIT_BTNWIDTH:               Result := 14;
  else
    Result := inherited GetIntegerMetrics(Index);
  end;
end;

function TTBXStripesTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
var
  InMenuBar, ToolbarStyle, ShowInactive: Boolean;
begin
  with ItemInfo do
  begin
    InMenuBar := ItemInfo.ViewType and VT_TYPEMASK = VT_MENUBAR;
    ToolbarStyle := Boolean(ItemOptions and IO_TOOLBARSTYLE);
    ShowInactive := InMenubar and not Boolean(ItemOptions and IO_APPACTIVE);

    if not ToolbarStyle and not Enabled and (HoverKind = hkKeyboardHover) then Result := clBtnShadow
    else if not Enabled then Result := DisabledColor
    else if not ToolbarStyle or InMenuBar then
    begin
      if HoverKind <> hkNone then Result := clHighlightText
      else if ShowInactive then Result := clGrayText
      else Result := clMenuText
    end
    else Result := clBtnText;
  end;
end;

procedure TTBXStripesTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
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
        Border.X := GetSystemMetrics(XMetrics[Resizable]);
        Border.Y := GetSystemMetrics(YMetrics[Resizable]);
        Exit;
      end
      else Sz := 2;

    VT_POPUPS:
      if ViewType and VT_TYPEMASK = VT_LISTBOX then Sz := 2 else Sz := 3;
  end;

  Border.X := Sz;
  Border.Y := Sz;
end;

procedure TTBXStripesTheme.PaintBackgnd(DC: HDC; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; AViewType: Integer);
const
  STRIPE_STEP = 5;
var
  HighlightColor: TColor;
  ShadowColor: TColor;
  Y, I: Integer;
  R: TRect;
begin
  if TBXLoColor then inherited
  else if AColor <> clNone then
  begin
    IntersectRect(R, ARect, AClipRect);
    if not RectVisible(DC, R) then Exit;

    if AColor = clDefault then AColor := GetViewColor(AViewType);

    if (AViewType and VT_TYPEMASK = VT_MENUBAR) or
      ((AViewType and VT_CATEGORYMASK = VT_BARS) and (AViewType and VT_EMBEDDED <> 0)) then
    begin
      FillRectEx(DC, R, AColor);
    end
    else
    begin
      I := ColorIntensity(AColor);
      if I < 200 then I := (200 - I) div 20
      else I := 0;
      HighlightColor := GetNearestColor(DC, Lighten(AColor, 16 + I));
      ShadowColor := GetNearestColor(DC, Lighten(AColor, -16));

      FillRectEx(DC, R, AColor);
      Y := (R.Top - ARect.Top) mod STRIPE_STEP;
      Y := R.Top - Y;
      while Y < ARect.Bottom do
      begin
        DrawLineEx(DC, R.Left, Y, R.Right, Y, ShadowColor);
        Inc(Y);
        DrawLineEx(DC, R.Left, Y, R.Right, Y, HighlightColor);
        Inc(Y, STRIPE_STEP - 1);
      end;        

    end;
  end;
end;

procedure TTBXStripesTheme.PaintCaption(DC: HDC;
  const Rect: TRect; const ItemInfo: TTBXItemInfo;
  const Caption: WideString; Format: Cardinal; Color: TColor);
var
  R: TRect;
  InMenuBar: Boolean;
begin
  if not ItemInfo.Enabled or (Color = clDefault) then Color := GetItemTextColor(ItemInfo)
  else if Color = clNone then Color := clDefault; // use existing DC color
  with ItemInfo do
  begin
    R := Rect;
    InMenuBar := ItemInfo.ViewType and VT_TYPEMASK = VT_MENUBAR;
    if (Pushed or Selected) and not InMenuBar then OffsetRect(R, 1, 1);
    DrawTextRW(DC, Caption, R, Format, Color);
  end;
end;

procedure TTBXStripesTheme.PaintDropDownArrow(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ArrowColor: array [Boolean] of TColor = (clBtnText, clMenuText);
var
  X, Y: Integer;
  C: TColor;
begin
  with ItemInfo, ARect do
  begin
    X := (Left + Right) div 2 + Ord(Pushed or Selected);
    Y := (Top + Bottom) div 2 - 1 + Ord(Pushed or Selected);
    if Enabled then C := ArrowColor[not Boolean(ItemOptions and IO_TOOLBARSTYLE)]
    else C := DisabledColor;
    if ItemInfo.IsVertical then
      PolygonEx(DC, [Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)], C, C)
    else
      PolygonEx(DC, [Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)], C, C);
  end;
end;

procedure TTBXStripesTheme.PaintButton(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  C: TColor;
  ShowHover, Embedded: Boolean;
  RL, RR: Integer;
begin
  R := ARect;
  with ItemInfo do
  begin
    ShowHover := (Enabled and (HoverKind <> hkNone)) or
      (not Enabled and (HoverKind = hkKeyboardHover));
    Embedded := (ViewType and VT_CATEGORYMASK = VT_BARS) and (ViewType and VT_EMBEDDED <> 0);
    RL := 2;
    RR := 2;

    if ComboPart = cpSplitRight then
    begin
      Dec(R.Left);
      Dec(RL);
    end;
    if ComboPart = cpSplitLeft then Dec(RR);
    if (ItemInfo.ItemOptions and IO_TOOLBARSTYLE) = 0 then
    begin
      RR := 1; RL := 1;
    end;

    if Embedded and not ShowHover then
    begin
      if Enabled then
      begin
        InflateRect(R, -1, -1);
        FillRectEx(DC, R, NearestMixedColor(clWindow, clBtnFace, 16));
        InflateRect(R, 1, 1);
        C := NearestMixedColor(clWindow, clBtnShadow, 64);
      end
      else
        C := clBtnFace;
      RoundFrame(DC, R, RL, RR, C);
    end;

    if ViewType and VT_TYPEMASK = VT_MENUBAR then
    begin
      if ((Pushed or Selected) and Enabled) or ShowHover then
        FillRectEx(DC, R, clHighlight);
      Exit;
    end;

    if (Pushed or Selected) and Enabled then
    begin
      InflateRect(R, -1, -1);
      if not Pushed and (HoverKind = hkNone) then
        DitherRect(DC, R, clWindow, clBtnHighlight)
      else
      begin
        if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 128)
        else C := clBtnHighlight;
        DitherRect(DC, R, C, clBtnHighlight);
      end;
      with R do
        PolyLineEx(DC, [Point(Left, Bottom - 2), Point(Left, Top), Point(Right - 1, Top)], clBtnFace);
      InflateRect(R, 1, 1);
      RoundFrame(DC, R, RL, RR, clBtnShadow);
    end
    else if ShowHover or ((ItemOptions and IO_DESIGNING) <> 0) then
    begin
      if HoverKind <> hkNone then
      begin
        if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 192)
        else C := clBtnFace;
        InflateRect(R, -1, -1);
        DitherRect(DC, R, clBtnFace, C);
        InflateRect(R, 1, 1);
      end;
      if not TBXLoColor then C := MixColors(clBtnShadow, clBtnFace, 192)
      else C := clBtnShadow;
      RoundFrame(DC, R, RL, RR, C);
    end;
    if ComboPart = cpSplitRight then PaintDropDownArrow(DC, R, ItemInfo);
  end;
end;

procedure TTBXStripesTheme.PaintFloatingBorder(DC: HDC; const ARect: TRect;
  const WindowInfo: TTBXWindowInfo);
var
  BorderColor, C: TColor;
  Sz: TPoint;
  R, R2: TRect;
  TextSize: TPoint;
  CaptionString: WideString;
  IsPushed, IsHovered: Boolean;
  OldFont: HFONT;
begin
  BorderColor := NearestMixedColor(clBtnShadow, clBlack, 127);
  if (WRP_BORDER and WindowInfo.RedrawPart) <> 0  then
  begin
    R := ARect;
    FrameRectEx(DC, R, BorderColor, True);
    Windows.DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);
    FrameRectEx(DC, R, clBtnFace, True);
    FrameRectEx(DC, R, clBtnFace, True);

    SaveDC(DC);
    Sz := WindowInfo.FloatingBorderSize;
    with ARect, Sz do R2 := Rect(Left + X, Top + Y, Right - X, Bottom - Y);
    with R2 do ExcludeClipRect(DC, Left, Top, Right, Bottom);
    FillRectEx(DC, R, GetViewColor(WindowInfo.ViewType));
    RestoreDC(DC, -1);
  end;

  if not WindowInfo.ShowCaption then Exit;

  { Caption }
  if (WRP_CAPTION and WindowInfo.RedrawPart) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION));
    with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);

    Dec(R.Bottom);
    DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, clBtnFace);
    Dec(R.Bottom);
    DrawLineEx(DC, R.Left - 1, R.Bottom, R.Right + 1, R.Bottom, DisabledColor);

    if ((CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0) and
      ((WRP_CLOSEBTN and WindowInfo.RedrawPart) <> 0) then
      Dec(R.Right, GetSystemMetrics(SM_CYSMCAPTION));

    OldFont := SelectObject(DC, SmCaptionFont.Handle);
    try
      CaptionString := WideString(WindowInfo.Caption);
      TextSize.X := GetTextWidthW(DC, CaptionString, False);
      TextSize.Y := GetTextHeight(DC);
      if TextSize.X > 0 then Inc(TextSize.X, 16);

      if WindowInfo.Active and (TextSize.X < R.Right - R.Left) then
      begin
        if TextSize.X = 0 then DotFill(DC, R, clBtnFace, 2)
        else
        begin
          R2 := R;
          R2.Right := (R.Left + R.Right - TextSize.X) div 2;
          DotFill(DC, R2, clBtnFace, 2);
          R2.Right := R.Right;
          R2.Left := (R.Left + R.Right + TextSize.X) div 2;
          DotFill(DC, R2, clBtnFace, 2);
          R2.Right := R2.Left;
          R2.Left := (R.Left + R.Right - TextSize.X) div 2;
          FillRectEx(DC, R2, clBtnFace);
        end;
      end
      else FillRectEx(DC, R, clBtnFace);

      InflateRect(R, -2, 0);
      Dec(R.Top, 2);

      DrawTextRW(DC, CaptionString, R,
        DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_HIDEPREFIX, clBtnText);
    finally
      SelectObject(DC, OldFont);
    end;
  end;

  { Close button }
  if (CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION));
    with Windowinfo.FloatingBorderSize do OffsetRect(R, X, Y);

    R.Left := R.Right - (R.Bottom - R.Top) - 1;      

    FillRectEx(DC, R, clBtnFace);

    Dec(R.Bottom, 2);
    DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, DisabledColor);
    Dec(R.Bottom, 2);
    Inc(R.Left, 4);

    IsPushed := (CDBS_PRESSED and WindowInfo.CloseButtonState) <> 0;
    IsHovered := (CDBS_HOT and WindowInfo.CloseButtonState) <> 0;

    if IsPushed or IsHovered then
    begin
      RoundFrame(DC, R, 1, 1, clBtnShadow);
      InflateRect(R, -1, -1);
      if IsPushed then FillRectEx(DC, R, clBtnHighlight)
      else
      begin
        if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 192)
        else C := clBtnFace;
        DitherRect(DC, R, clBtnFace, C);
      end;
      if IsPushed then OffsetRect(R, 1, 1);
    end;

    DrawButtonBitmap(DC, R);
  end;
end;

procedure TTBXStripesTheme.PaintFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
  E: Boolean;
begin
  R := ARect;
  with ItemInfo do
  begin
    E := Enabled or (not Enabled and (HoverKind = hkKeyboardHover));
    if not E then
    begin
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, DisabledColor, False);
    end
    else if Pushed or Selected {or Embedded} or (HoverKind <> hkNone) or
      ((ItemOptions and IO_DESIGNING) <> 0) then
    begin
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, clWindow, False);
      InflateRect(R, 1, 1);
      RoundFrame(DC, R, 1, 1, DisabledColor);
    end
    else
    begin
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, DisabledColor, False);
    end;
  end;
end;

procedure TTBXStripesTheme.PaintEditButton(DC: HDC; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
const
  ArrowColor: array [Boolean] of TColor = (clBtnText, clMenuText);
var
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  C: TColor;

  procedure PaintEnabled(R: TRect; Pressed: Boolean);
  begin
    if Pressed then
    begin
      if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 126)
      else C := clBtnHighlight;
      DitherRect(DC, R, C, clBtnHighlight);
    end
    else if BtnHot then
    begin
      if not TBXLoColor then C := NearestMixedColor(clBtnHighlight, clBtnFace, 192)
      else C := clBtnFace;
      DitherRect(DC, R, clBtnFace, C)
    end
    else if Embedded then FillRectEx(DC, R, clBtnFace);
  end;

begin
  with ItemInfo do
  begin
    R := ARect;
//    W := EditFrameWidth;
    Inc(R.Left);
    Embedded := (ViewType and VT_CATEGORYMASK = VT_BARS) and (ViewType and VT_EMBEDDED <> 0);
    BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;

    if ButtonInfo.ButtonType = EBT_DROPDOWN then
    begin
      BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
      if BtnHot or BtnPressed then InflateRect(R, -1, -1)
      else InflateRect(R, -2, -2);
      if not BtnDisabled then
      begin
        PaintEnabled(R, BtnPressed);
        DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, DisabledColor);
      end;
      PaintDropDownArrow(DC, R, ItemInfo);
    end
    else if ButtonInfo.ButtonType = EBT_SPIN then
    begin
      BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;
      BtnPressed := (ButtonInfo.ButtonState and (EBSS_UP or EBSS_DOWN)) <> 0;
      if BtnHot or BtnPressed then InflateRect(R, -1, -1)
      else InflateRect(R, -2, -2);

      if not BtnDisabled then
      begin
        { Upper button }
        BR := R; BR.Bottom := (R.Top + R.Bottom + 1) div 2;

        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_UP) <> 0);

        { Lower button }
        BR := R; BR.Top := (R.Top + R.Bottom) div 2;
        PaintEnabled(BR, (ButtonInfo.ButtonState and EBSS_DOWN) <> 0);

        DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, DisabledColor);
        Y := (R.Top + R.Bottom - 1) div 2;
        DrawLineEx(DC, R.Left, Y, R.Right, Y, DisabledColor);
        Y := (R.Top + R.Bottom) div 2;
        DrawLineEx(DC, R.Left, Y, R.Right, Y, DisabledColor);
      end;

      { Arrows }
      if not BtnDisabled then C := ArrowColor[not Boolean(ItemOptions and IO_TOOLBARSTYLE)]
      else C := DisabledColor;

      BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
      X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
      Y := (3 * R.Top + R.Bottom) div 4 + Ord(BtnPressed);
      PolygonEx(DC, [Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)], C, C);

      BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
      X := (R.Left + R.Right) div 2 + Ord(BtnPressed);
      Y := (R.Top + 3 * R.Bottom) div 4 + Ord(BtnPressed);
      PolygonEx(DC, [Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)], C, C);
    end;
  end;
end;

procedure TTBXStripesTheme.PaintEditFrame(DC: HDC;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
begin
  R := ARect;
  PaintFrame(DC, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
  
  with EditInfo do
    if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);
  
  if ItemInfo.Enabled then FillRectEx(DC, R, clWindow);
  InflateRect(R, -1, -1);

  with EditInfo do if LeftBtnWidth > 0 then Inc(R.Left, LeftBtnWidth - 2);

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    R.Left := R.Right - EditInfo.RightBtnWidth - W;
    PaintEditButton(DC, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXStripesTheme.PaintImage(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer);
var
  HiContrast: Boolean;
  C: TCanvas;
begin
  with ItemInfo do
  begin
    if ImageList is TTBCustomImageList then
    begin
      if Selected or Pushed then OffsetRect(ARect, 1, 1);
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
        DrawTBXIconFlatShadow(DC, ARect, ImageList, ImageIndex, clBtnShadow);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if Selected or Pushed then OffsetRect(ARect, 1, 1)
      else
      begin
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, -2, -2);
      end;
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast then
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
    else
      BlendTBXIcon(DC, ARect, ImageList, ImageIndex, 200);
  end;
end;

procedure TTBXStripesTheme.PaintMenuItemFrame(DC: HDC;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
begin
  with ItemInfo do if (Enabled and (HoverKind <> hkNone)) or
    (not Enabled and (HoverKind = hkKeyboardHover)) then
  begin
    PaintBackgnd(DC, ZERO_RECT, ARect, ARect, clHighlight, VT_UNKNOWN);
  end;
end;

procedure TTBXStripesTheme.PaintMenuItem(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo);
const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  R: TRect;
  ShowImageOrCheck: Boolean;
  IsCombo: Boolean;
  X, Y: Integer;
  ArrowWidth: Integer;

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

    R := ARect;
    if ShowImageOrCheck then Inc(R.Left, ItemInfo.PopupMargin + MenuImageTextSpace);
    if IsCombo and Enabled then Dec(R.Right, ArrowWidth + 1);

    PaintMenuItemFrame(DC, R, ItemInfo);

    if IsCombo then
    begin
      R.Left := ARect.Right - ArrowWidth;
      R.Right := ARect.Right;
      if Enabled and (HoverKind <> hkNone) then
        PaintBackgnd(DC, ZERO_RECT, R, R, clHighlight, VT_POPUPMENU)
      else
      begin
        Dec(R.Left);
        DrawLineEx(DC, R.Left, R.Top, R.Left, R.Bottom, DisabledColor);
      end;
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then 
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3;
      if not Enabled then
      begin
        if HoverKind = hkKeyboardHover then DrawArrow(clBtnShadow)
        else DrawArrow(DisabledColor);
      end
      else if (HoverKind <> hkNone) {and not IsCombo} then DrawArrow(clHighlightText)
      else DrawArrow(clMenuText);
    end;

    if ShowImageOrCheck and ((HoverKind <> hkNone) or Selected) then
    begin
      R.Left := ARect.Left;
      R.Right := R.Left + ItemInfo.PopupMargin;
      PaintButton(DC, R, ItemInfo);
    end;
  end;
end;

procedure TTBXStripesTheme.PaintPopupNCArea(DC: HDC; R: TRect; const PopupInfo: TTBXPopupInfo);
begin
  FrameRectEx(DC, R, GetNearestColor(DC, MixColors(clBtnShadow, clBlack, 127)), True);
  if PopupInfo.ViewType and VT_TYPEMASK = VT_LISTBOX then
  begin
    FillRectEx(DC, R, clWindow);
  end
  else if not USE_FLATMENUS or (PopupInfo.ViewType and VT_TYPEMASK = VT_TOOLBOX) then
  begin
    Windows.DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);
    FillRectEx(DC, R, ToolbarColor);
  end
  else
  begin
    FillRectEx(DC, R, clPopup);
  end;
end;

procedure TTBXStripesTheme.PaintSeparator(DC: HDC; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
begin
  with ItemInfo, ARect do if Enabled then
  begin
    if Horizontal then
    begin
      Top := (Top + Bottom) div 2;
      DrawLineEx(DC, Left + 1, Top, Right - 1, Top, DisabledColor)
    end
    else
    begin
      Left := (Left + Right) div 2;
      DrawLineEx(DC, Left, Top + 1, Left, Bottom - 1, DisabledColor);
    end;
  end;
end;

procedure TTBXStripesTheme.PaintToolbarNCArea(DC: HDC; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
var
  ToolbarColor: TColor;
  Sz: Integer;
  R2: TRect;
  C: TColor;
  BtnVisible, Horz, CloseButtondown, CloseButtonHover: Boolean;
begin
  ToolbarColor := ToolbarInfo.Color;
  if ToolbarColor = clDefault then ToolbarColor := GetViewColor(ToolbarInfo.ViewType);

  { Border }
  if ToolbarInfo.BorderStyle = bsSingle then
    Windows.DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT or BF_ADJUST);

  FillRectEx(DC, R, ToolbarColor);
  InflateRect(R, -1, -1);

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
      DrawLineEx(DC, R2.Right - 2, R2.Top, R2.Right - 2, R2.Bottom,
        NearestMixedColor(clBtnShadow, clBtnFace,  96));
      Dec(R2.Right, 2);
      FillRectEx(DC, R2, ToolbarColor);
      if BtnVisible then Inc(R2.Top, Sz - 2);
    end
    else
    begin
      DrawLineEx(DC, R2.Left, R2.Bottom - 2, R2.Right, R2.Bottom - 2,
        NearestMixedColor(clBtnShadow, clBtnFace,  96));
      Dec(R2.Bottom, 2);
      FillRectEx(DC, R2, ToolbarColor);
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

    InflateRect(R2, -1, -1);
    if CloseButtonDown or CloseButtonHover then
    begin
      if CloseButtonDown then
      begin
        if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 126)
        else C := clBtnHighlight;
        DitherRect(DC, R2, clBtnHighlight, C);
      end
      else
      begin
        if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 192)
        else C := clBtnFace;
        DitherRect(DC, R2, clBtnFace, C);
      end;

      InflateRect(R2, 1, 1);
      if not TBXLoColor or not CloseButtonDown then
        C := NearestMixedColor(clBtnShadow, clBtnFace, 191)
      else
        C := clBtnShadow;
      RoundFrame(DC, R2, 1, 1, C);
    end;

    if CloseButtonDown then OffsetRect(R2, 1, 1);
    DrawButtonBitmap(DC, R2);
  end;
end;

procedure TTBXStripesTheme.PaintMDIButton(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
  X, Y: Integer;
begin
  PaintButton(DC, ARect, ItemInfo);
  with ARect do
  begin
    X := (Left + Right - StockImgList.Width) div 2 + Ord(ItemInfo.Pushed);
    Y := (Top + Bottom - StockImgList.Height - 1) div 2 + Ord(ItemInfo.Pushed);
  end;
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  DrawGlyph(DC, X, Y, StockImgList, Index, clBtnText);
end;

function TTBXStripesTheme.GetPopupShadowType: Integer;
begin
  Result := PST_WINDOWS2K;
end;

constructor TTBXStripesTheme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then InitializeStock;
  Inc(CounterLock);
end;

destructor TTBXStripesTheme.Destroy;
begin
  Dec(CounterLock);
  if CounterLock = 0 then FinalizeStock;
  inherited;
end;

procedure TTBXStripesTheme.PaintDock(DC: HDC; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
begin
  // this theme does not support dock painting
end;

procedure TTBXStripesTheme.PaintDockPanelNCArea(DC: HDC; R: TRect;
  const DockPanelInfo: TTBXDockPanelInfo);
var
  Sz: Integer;
  R2: TRect;
  PanelColor, C: TColor;
  Flags: Integer;
  CloseButtonDown, CloseButtonHover: Boolean;
  TextSz: Integer;
  OldFont: HFONT;
  OldTextColor: TColorRef;
  OldBkMode: Integer;
begin
  with DockPanelInfo do
  begin
    PanelColor := DockPanelInfo.Color;
    if PanelColor = clDefault then PanelColor := GetViewColor(ViewType);
    Sz := GetSystemMetrics(SM_CYSMCAPTION);

    { Border }
    FrameRectEx(DC, R, ToolbarColor, True);
    R2 := R;
    if ShowCaption then
      if IsVertical then Inc(R2.Top, Sz)
      else Inc(R2.Left, Sz);
    FrameRectEx(DC, R2, PanelColor, False);

    if not ShowCaption then Exit;

    R2 := R;
    if IsVertical then R.Bottom := R.Top + Sz
    else R.Right := R.Left + Sz;
    Windows.DrawEdge(DC, R, BDR_RAISEDINNER, BF_RECT or BF_MIDDLE or BF_ADJUST);

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

      InflateRect(R2, -2, -2);
      if CloseButtonDown or CloseButtonHover then
      begin
        if CloseButtonDown then
        begin
          if not TBXLoColor then C := NearestMixedColor(clBtnHighlight, clBtnFace, 126)
          else C := clBtnHighlight;
          DitherRect(DC, R2, clBtnHighlight, C);
        end
        else
        begin
          if not TBXLoColor then C := NearestMixedColor(clBtnHighlight, clBtnFace, 192)
          else C := clBtnFace;
          DitherRect(DC, R2, clBtnFace, C);
        end;

        InflateRect(R2, 1, 1);
        if not TBXLoColor or not CloseButtonDown then
          C := NearestMixedColor(clBtnShadow, clBtnFace, 191)
        else
          C := clBtnShadow;

        RoundFrame(DC, R2, 1, 1, C);
      end;

      if CloseButtonDown then OffsetRect(R2, 1, 1);
      DrawButtonBitmap(DC, R2);
    end;

    { Caption }
    if IsVertical then InflateRect(R, -1, 0)
    else Inflaterect(R, 0, -1);

    OldFont := SelectObject(DC, SmCaptionFont.Handle);

    if IsVertical then
    begin
      TextSz := GetTextWidthW(DC, Caption, False);
      if TextSz > 0 then Inc(TextSz, 12);
      if TextSz < R.Right - R.Left then
      begin
        R2 := R;
        Inc(R2.Top);
        if TextSz = 0 then DotFill(DC, R2, clBtnFace, 1)
        else
        begin
          R2.Right := (R.Left + R.Right - TextSz) div 2;
          Inc(R2.Left);
          DotFill(DC, R2, clBtnFace, 1);
          R2.Right := R.Right;
          R2.Left := (R.Left + R.Right + TextSz) div 2;
          DotFill(DC, R2, clBtnFace, 1);
          R2.Right := R2.Left;
          R2.Left := (R.Left + R.Right - TextSz) div 2;
          FillRectEx(DC, R2, clBtnFace);
        end;
      end;
    end
    else
    begin
      TextSz := GetTextWidthW(DC, Caption, False);
      if TextSz > 0 then Inc(TextSz, 12);
      if TextSz < R.Bottom - R.Top then
      begin
        R2 := R;
        Inc(R2.Left);
        if TextSz = 0 then DotFill(DC, R2, clBtnFace, 1)
        else
        begin
          R2.Bottom := (R.Top + R.Bottom - TextSz) div 2;
          Inc(R2.Top);
          DotFill(DC, R2, clBtnFace, 1);
          R2.Bottom := R.Bottom;
          R2.Top := (R.Top + R.Bottom + TextSz) div 2;
          DotFill(DC, R2, clBtnFace, 1);
          R2.Bottom := R2.Top;
          R2.Top := (R.Top + R.Bottom - TextSz) div 2;
          FillRectEx(DC, R2, clBtnFace);
        end;
      end;
    end;

    OldTextColor := SetTextColor(DC, GetSysColor(COLOR_BTNTEXT));
    OldBkMode := SetBkMode(DC, TRANSPARENT);
    Flags := DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then _DrawTextW(DC, Caption, -1, R, Flags)
    else DrawRotatedTextW(DC, Caption, R, Flags);
    SelectObject(DC, OldFont);
    SetBkMode(DC, OldBkMode);
    SetTextColor(DC, OldTextColor);

  end;
end;

procedure TTBXStripesTheme.PaintPageScrollButton(DC: HDC;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  X, Y, Sz: Integer;
  C: TColorRef;
begin
  R := ARect;
  InflateRect(R, -1, -1);
  if Hot then DitherRect(DC, R, clBtnFace, clBtnHighlight)
  else FillRectEx(DC, R, clBtnFace);
  InflateRect(R, 1, 1);
  RoundFrame(DC, R, 1, 1, NearestMixedColor(clWindow, clBtnShadow, 64));
  C := GetSysColor(COLOR_BTNFACE);
  with R do
  begin
    SetPixelV(DC, Left, Top, C);
    SetPixelV(DC, Left, Top, C);
    SetPixelV(DC, Right + 1, Top, C);
    SetPixelV(DC, Right + 1, Bottom + 1, C);
    SetPixelV(DC, Left, Bottom + 1, C);
  end;

  { Arrow }
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;
  case ButtonType of
    PSBT_UP:
      begin
        Inc(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)], clBtnText, clBtnText);
      end;
    PSBT_DOWN:
      begin
        Y := (R.Top + R.Bottom - 1) div 2;
        Dec(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)], clBtnText, clBtnText);
      end;
    PSBT_LEFT:
      begin
        Inc(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)], clBtnText, clBtnText);
      end;
    PSBT_RIGHT:
      begin
        X := (R.Left + R.Right - 1) div 2;
        Dec(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)], clBtnText, clBtnText);
      end;
  end;
end;

procedure TTBXStripesTheme.PaintFrameControl(DC: HDC; R: TRect; Kind, State: Integer; Params: Pointer);
var
  X, Y: Integer;
  PenColor: TColor;
  C: TColor;
  Pen, OldPen: HPEN;
  Brush, OldBrush: HBRUSH;

  procedure SetupPen;
  begin
    if Boolean(State and PFS_DISABLED) then PenColor := clBtnShadow
    else if Boolean(State and PFS_PUSHED) then PenColor := clBtnText
    else if Boolean(State and PFS_HOT) then PenColor := clBtnText
    else PenColor := clBtnShadow;
    Pen := CreatePenEx(PenColor);
  end;

  procedure SetupBrush;
  begin
    if Boolean(State and PFS_DISABLED) then Brush := CreateBrushEx(clNone)
    else if Boolean(State and PFS_PUSHED) then
    begin
      if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 126)
      else C := clBtnHighlight;
      Brush := CreateDitheredBrush(C, clBtnHighlight);
    end
    else if Boolean(State and PFS_HOT) then
    begin
      if not TBXLoColor then C := MixColors(clBtnHighlight, clBtnFace, 192)
      else C := clBtnFace;
      Brush := CreateDitheredBrush(clBtnFace, C);
    end
    else if Boolean(State and PFS_MIXED) then Brush := CreateDitheredBrush(clWindow, clBtnFace)
    else Brush := CreateBrushEx(clNone);
  end;

  function TextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := clBtnFace
    else if Boolean(State and PFS_MIXED) then Result := clBtnShadow
    else Result := clBtnText;
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
        with R do Windows.Rectangle(DC, Left, Top, Right, Bottom);
        SelectObject(DC, OldBrush);
        SelectObject(DC, OldPen);
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
        InflateRect(R, -1, -1);
        SetupPen;
        SetupBrush;
        OldPen := SelectObject(DC, Pen);
        OldBrush := SelectObject(DC, Brush);
        with R do Windows.Ellipse(DC, Left, Top, Right, Bottom);
        SelectObject(DC, OldBrush);
        SelectObject(DC, OldPen);
        DeleteObject(Pen);
        DeleteObject(Brush);

        if Boolean(State and PFS_CHECKED) then
        begin
          InflateRect(R, -3, -3);

          C := TextColor;
          Pen := CreatePenEx(C);
          Brush := CreateBrushEx(C);
          OldPen := SelectObject(DC, Pen);
          OldBrush := SelectObject(DC, Brush);
          with R do Windows.Ellipse(DC, Left, Top, Right, Bottom);
          SelectObject(DC, OldBrush);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
          DeleteObject(Brush);
        end;
      end;
  else
    inherited;
  end;
end;

procedure TTBXStripesTheme.PaintStatusBar(DC: HDC; R: TRect; Part: Integer);
var
  D, Sz, I: Integer;
  Lo, Hi: TColor;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(DC, Right - 2 - D, Bottom - 2, Right - 1, Bottom - D - 3, C);
    Inc(D);
  end;

begin
  case Part of
    SBP_BODY:
      begin
        FillRectEx(DC, R, clBtnFace);
        DrawLineEx(DC, R.Left, R.Top + 2, R.Right, R.Top + 2,
          NearestMixedColor(clBtnShadow, clBtnFace,  96));
      end;
    SBP_PANE, SBP_LASTPANE:
      begin
        if Part = SBP_PANE then Dec(R.Right, 3);
        DrawLineEx(DC, R.Right, R.Top + 4, R.Right, R.Bottom - 3, DisabledColor);
      end;
    SBP_GRIPPER:
      begin
        Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
        Hi := NearestMixedColor(clBtnFace, clBtnHighlight, 64);
        Lo := NearestMixedColor(clBtnFace, clBtnShadow, 64);

        D := 2;
        for I := 1 to 3 do
        begin
          case Sz of
          0..8:
            begin
            DiagLine(Lo);
            DiagLine(Hi);
            end;
          9..12:
            begin
              DiagLine(Lo);
              DiagLine(Hi);
              Inc(D);
            end;
          else
            DiagLine(Lo);
            Inc(D, 1);
            DiagLine(Hi);
            Inc(D, 1);
          end;
        end;
      end;
  end;
end;

procedure TTBXStripesTheme.SetupColorCache;
begin
  DockPanelColor := NearestMixedColor(clBtnFace, clWindow, 64);
  if not TBXLoColor then DisabledColor := MixColors(clBtnShadow, clBtnFace, 210)
  else DisabledColor := clBtnShadow;
  ToolbarColor := clBtnFace;
  StatusBarColor := clBtnFace;
  DockColor := clBtnFace;
end;

procedure TTBXStripesTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
begin
  inherited;
  if MarginID = MID_MENUITEM then
  begin
    Margins.TopHeight := 2;
    Margins.BottomHeight := 2;
  end;
end;

initialization

RegisterTBXTheme('Stripes', TTBXStripesTheme);

end.
