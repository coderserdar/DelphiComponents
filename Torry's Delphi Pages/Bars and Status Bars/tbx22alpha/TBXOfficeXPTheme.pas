unit TBXOfficeXPTheme;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXOfficeXPTheme.pas 133 2005-11-08 20:00:43Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Graphics, TBXThemes, TBXDefaultTheme, ImgList;

type
  TItemPart = (ipBody, ipText, ipFrame);
  TBtnItemState = (bisNormal, bisDisabled, bisSelected, bisPressed, bisHot,
    bisDisabledHot, bisSelectedHot, bisPopupParent);
  TMenuItemState = (misNormal, misDisabled, misHot, misDisabledHot);
  TWinFramePart = (wfpBorder, wfpCaption, wfpCaptionText);
  TWinFrameState = (wfsActive, wfsInactive);

  TTBXOfficeXPTheme = class(TTBXTheme)
  private
    procedure TBXSysCommand(var Message: TMessage); message TBX_SYSCOMMAND;
  protected
    { View/Window Colors }
    MenubarColor: TColor;
    ToolbarColor: TColor;
    PopupColor: TColor;
    DockColor: TColor;
    DockPanelColor: TColor;
    PopupFrameColor: TColor;
    StatusBarColor: TColor;
    WinFrameColors: array [TWinFrameState, TWinFramePart] of TColor;
    PnlFrameColors: array [TWinFrameState, TWinFramePart] of TColor;
    MenuItemColors: array [TMenuItemState, TItemPart] of TColor;
    BtnItemColors: array [TBtnItemState, TItemPart] of TColor;

    { Other Colors }
    DragHandleColor: TColor;
    PopupSeparatorColor: TColor;
    ToolbarSeparatorColor: TColor;
    IconShadowColor: TColor;
    StatusPanelFrameColor: TColor;

    procedure SetupColorCache; virtual;
  protected
    { Internal Methods }
    function GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
    function GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { Metrics access}
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
    procedure GetViewMargins(ViewType: Integer; out Margins: TTBXMargins); override;

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
    procedure PaintFloatingBorder(DC: HDC; const ARect: TRect; const WindowInfo: TTBXWindowInfo); override;
    procedure PaintFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintImage(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList; ImageIndex: Integer); override;
    procedure PaintMDIButton(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal); override;
    procedure PaintMenuItem(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo); override;
    procedure PaintMenuItemFrame(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo); override;
    procedure PaintPageScrollButton(DC: HDC; const ARect: TRect; ButtonType: Integer; Hot: Boolean); override;
    procedure PaintPopupNCArea(DC: HDC; R: TRect; const PopupInfo: TTBXPopupInfo); override;
    procedure PaintSeparator(DC: HDC; ARect: TRect; ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean); override;
    procedure PaintToolbarNCArea(DC: HDC; R: TRect; const ToolbarInfo: TTBXToolbarInfo); override;
    procedure PaintFrameControl(DC: HDC; R: TRect; Kind, State: Integer; Params: Pointer); override;
    procedure PaintStatusBar(DC: HDC; R: TRect; Part: Integer); override;
  end;

implementation

uses TBXUtils, TBXGraphics, TB2Common, TB2Item, Classes, Controls, Commctrl, Forms;

var
  StockImgList: TImageList;
  CounterLock: Integer;

procedure InitializeStock;
begin
  StockImgList := TImageList.Create(nil);
  StockImgList.Handle := ImageList_LoadBitmap(HInstance, 'TBXGLYPHS', 16, 0, clWhite);
end;

procedure FinalizeStock;
begin
  StockImgList.Free;
end;

{ TTBXOfficeXPTheme }

function TTBXOfficeXPTheme.GetBooleanMetrics(Index: Integer): Boolean;
begin
  case Index of
    TMB_OFFICEXPPOPUPALIGNMENT:    Result := True;
    TMB_EDITMENUFULLSELECT:        Result := True;
    TMB_EDITHEIGHTEVEN:            Result := False;
    TMB_SOLIDTOOLBARNCAREA:        Result := False;
    TMB_SOLIDTOOLBARCLIENTAREA:    Result := True;
  else
    Result := False;
  end;
end;

function TTBXOfficeXPTheme.GetIntegerMetrics(Index: Integer): Integer;
const
  DEFAULT = -1;
begin
  case Index of
    TMI_SPLITBTN_ARROWWIDTH:         Result := 12;

    TMI_DROPDOWN_ARROWWIDTH:         Result := 8;
    TMI_DROPDOWN_ARROWMARGIN:        Result := 3;

    TMI_MENU_IMGTEXTSPACE:           Result := 5;
    TMI_MENU_LCAPTIONMARGIN:         Result := 3;
    TMI_MENU_RCAPTIONMARGIN:         Result := 3;
    TMI_MENU_SEPARATORSIZE:          Result := 3;
    TMI_MENU_MDI_DW:                 Result := 2;
    TMI_MENU_MDI_DH:                 Result := 2;

    TMI_TLBR_SEPARATORSIZE:          Result := 6;

    TMI_EDIT_FRAMEWIDTH:             Result := 1;
    TMI_EDIT_TEXTMARGINHORZ:         Result := 2;
    TMI_EDIT_TEXTMARGINVERT:         Result := 2;
    TMI_EDIT_BTNWIDTH:               Result := 14;
    TMI_EDIT_MENURIGHTINDENT:        Result := 1;
  else
    Result := DEFAULT;
  end;
end;

function TTBXOfficeXPTheme.GetViewColor(ViewType: Integer): TColor;
begin
  Result := clBtnFace;
  case ViewType and VT_CATEGORYMASK of
    VT_BARS:
      begin
        if ViewType and VT_TYPEMASK = VT_MENUBAR then Result := MenubarColor
        else Result := ToolbarColor;
      end;
    VT_POPUPS:
      if ViewType and VT_TYPEMASK = VT_LISTBOX then Result := clWindow
      else Result := PopupColor;
    VT_STATUSBAR: Result := StatusBarColor;
    VT_DOCKPANEL: Result := DockPanelColor;
    VT_TOOLWINDOW: Result := ToolbarColor;
    VT_DOCK: Result := DockColor;
  end;
end;

function TTBXOfficeXPTheme.GetBtnColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
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
        else if (Result = clNone) then Result := clBtnShadow;
      end;
    end;
  end;
end;

function TTBXOfficeXPTheme.GetPartColor(const ItemInfo: TTBXItemInfo; ItemPart: TItemPart): TColor;
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
      if ItemPart = ipFrame then Result := clBtnShadow;
      end;
    end;
  end;
end;

function TTBXOfficeXPTheme.GetItemColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

function TTBXOfficeXPTheme.GetItemTextColor(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetPartColor(ItemInfo, ipText);
end;

function TTBXOfficeXPTheme.GetItemImageBackground(const ItemInfo: TTBXItemInfo): TColor;
begin
  Result := GetBtnColor(ItemInfo, ipBody);
  if Result = clNone then Result := GetViewColor(ItemInfo.ViewType);
end;

procedure TTBXOfficeXPTheme.GetViewBorder(ViewType: Integer; out Border: TPoint);
const
  XMetrics: array [Boolean] of Integer = (SM_CXDLGFRAME, SM_CXFRAME);
  YMetrics: array [Boolean] of Integer = (SM_CYDLGFRAME, SM_CYFRAME);
var
  Resizable: Boolean;

  procedure SetBorder(X, Y: Integer);
  begin
    Border.X := X;
    Border.Y := Y;
  end;

begin
  case ViewType and VT_CATEGORYMASK of
    VT_BARS, VT_TOOLWINDOW, VT_DOCKPANEL:
      if ViewType and VT_FLOATING <> 0 then
      begin
        Resizable := ViewType and VT_RESIZABLE <> 0;
        Border.X := GetSystemMetrics(XMetrics[Resizable]) - 1;
        Border.Y := GetSystemMetrics(YMetrics[Resizable]) - 1;
      end
      else SetBorder(2, 2);
    VT_POPUPS:
      if ViewType and VT_TYPEMASK = VT_POPUPMENU then SetBorder(1, 2)
      else SetBorder(2, 2);
  else
    SetBorder(0, 0);
  end;
end;

procedure TTBXOfficeXPTheme.GetMargins(MarginID: Integer; out Margins: TTBXMargins);
begin
  with Margins do
    case MarginID of
      MID_TOOLBARITEM:
        begin
          LeftWidth := 2; RightWidth := 2;
          TopHeight := 2; BottomHeight := 2;
        end;

      MID_MENUITEM:
        begin
          LeftWidth := 1; RightWidth := 1;
          TopHeight := 3; BottomHeight := 3;
        end;

      MID_STATUSPANE:
        begin
          LeftWidth := 1; RightWidth := 3;
          TopHeight := 1; BottomHeight := 1;
        end;
    else
      LeftWidth := 0;
      RightWidth := 0;
      TopHeight := 0;
      BottomHeight := 0;
    end;
end;

procedure TTBXOfficeXPTheme.PaintBackgnd(DC: HDC; const ADockRect, ARect, AClipRect: TRect;
  AColor: TColor; AViewType: Integer);
var
  R: TRect;
begin
  if AColor = clDefault then AColor := GetViewColor(AViewType);
  IntersectRect(R, ARect, AClipRect);
  FillRectEx(DC, R, AColor);
end;

procedure TTBXOfficeXPTheme.PaintCaption(DC: HDC;
  const Rect: TRect; const ItemInfo: TTBXItemInfo;
  const Caption: WideString; Format: Cardinal; Color: TColor);
var
  R: TRect;
begin
  if not ItemInfo.Enabled or (Color = clDefault) then Color := GetPartColor(ItemInfo, ipText)
  else if Color = clNone then Color := clDefault; // use existing DC color
  with ItemInfo do
  begin
    R := Rect;
    DrawTextRW(DC, Caption, R, Format, Color);
  end;
end;

procedure TTBXOfficeXPTheme.PaintCheckMark(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo);
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

procedure TTBXOfficeXPTheme.PaintChevron(DC: HDC; ARect: TRect; const ItemInfo: TTBXItemInfo);
const
  Pattern: array [Boolean, 0..15] of Byte = (
    ($CC, 0, $66, 0, $33, 0, $66, 0, $CC, 0, 0, 0, 0, 0, 0, 0),
    ($88, 0, $D8, 0, $70, 0, $20, 0, $88, 0, $D8, 0, $70, 0, $20, 0));
var
  R2: TRect;
  W, H: Integer;
begin
  R2 := ARect;
  PaintButton(DC, ARect, ItemInfo);
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
  DrawGlyph(DC, R2, W, H, Pattern[ItemInfo.IsVertical][0], GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOfficeXPTheme.PaintEditButton(DC: HDC; const ARect: TRect;
  var ItemInfo: TTBXItemInfo; ButtonInfo: TTBXEditBtnInfo);
var
  BtnDisabled, BtnHot, BtnPressed, Embedded: Boolean;
  R, BR: TRect;
  X, Y: Integer;
  SaveItemInfoPushed: Boolean;
  C: TColor;
begin
  R := ARect;
  Embedded := (ItemInfo.ViewType and VT_CATEGORYMASK = VT_BARS) and (ItemInfo.ViewType and VT_EMBEDDED <> 0);

  InflateRect(R, 1, 1);
  Inc(R.Left);
  if ButtonInfo.ButtonType = EBT_DROPDOWN then
  begin
    BtnDisabled := (ButtonInfo.ButtonState and EBDS_DISABLED) <> 0;
    BtnHot := (ButtonInfo.ButtonState and EBDS_HOT) <> 0;
    BtnPressed := (ButtonInfo.ButtonState and EBDS_PRESSED) <> 0;
    if not BtnDisabled then
    begin
      if BtnPressed or BtnHot or Embedded then PaintButton(DC, R, ItemInfo)
      else if ItemInfo.ViewType and VT_CATEGORYMASK = VT_BARS then
      begin
        R := ARect;
        if not Embedded then
        begin
          FrameRectEx(DC, R, clWindow, False);
          C := clWindow;
        end
        else C := GetBtnColor(ItemInfo, ipFrame);
        DrawLineEx(DC, R.Left - 1, R.Top, R.Left - 1, R.Bottom, C);
      end;
    end;
    PaintDropDownArrow(DC, R, ItemInfo);
  end
  else if ButtonInfo.ButtonType = EBT_SPIN then
  begin
    BtnDisabled := (ButtonInfo.ButtonState and EBSS_DISABLED) <> 0;
    BtnHot := (ButtonInfo.ButtonState and EBSS_HOT) <> 0;

    { Upper button }
    BR := R;
    BR.Bottom := (R.Top + R.Bottom + 1) div 2;
    BtnPressed := (ButtonInfo.ButtonState and EBSS_UP) <> 0;
    SaveItemInfoPushed := ItemInfo.Pushed;
    ItemInfo.Pushed := BtnPressed;
    if not BtnDisabled then
    begin
      if BtnPressed or BtnHot or Embedded then PaintButton(DC, BR, ItemInfo)
      else if ItemInfo.ViewType and VT_CATEGORYMASK = VT_BARS then
      begin
        BR.Left := ARect.Left; BR.Top := ARect.Top; BR.Right := ARect.Right;
        if not Embedded then
        begin
          FrameRectEx(DC, BR, clWindow, False);
          C := clWindow;
        end
        else C := GetBtnColor(ItemInfo, ipFrame);
        DrawLineEx(DC, BR.Left - 1, BR.Top, BR.Left - 1, BR.Bottom, C);
      end;
    end;
    X := (BR.Left + BR.Right) div 2;
    Y := (BR.Top + BR.Bottom - 1) div 2;
    C := GetPartColor(ItemInfo, ipText);
    PolygonEx(DC, [Point(X - 2, Y + 1), Point(X + 2, Y + 1), Point(X, Y - 1)], C, C);

    { Lower button }
    BR := R;
    BR.Top := (R.Top + R.Bottom) div 2;
    BtnPressed := (ButtonInfo.ButtonState and EBSS_DOWN) <> 0;
    ItemInfo.Pushed := BtnPressed;
    if not BtnDisabled then
    begin
      if BtnPressed or BtnHot or Embedded then PaintButton(DC, BR, ItemInfo)
      else if ItemInfo.ViewType and VT_CATEGORYMASK = VT_BARS then
      begin
        BR.Left := ARect.Left; BR.Bottom := ARect.Bottom; BR.Right := ARect.Right;
        if not Embedded then
        begin
          FrameRectEx(DC, BR, clWindow, False);
          C := clWindow;
        end
        else C := GetBtnColor(ItemInfo, ipFrame);
        DrawLineEx(DC, BR.Left - 1, BR.Top, BR.Left - 1, BR.Bottom, C);
      end;
    end;
    X := (BR.Left + BR.Right) div 2;
    Y := (BR.Top + BR.Bottom) div 2;
    C := GetPartColor(ItemInfo, ipText);
    PolygonEx(DC, [Point(X - 2, Y - 1), Point(X + 2, Y - 1), Point(X, Y + 1)], C, C);

    ItemInfo.Pushed := SaveItemInfoPushed;
  end;
end;

procedure TTBXOfficeXPTheme.PaintEditFrame(DC: HDC;
  const ARect: TRect; var ItemInfo: TTBXItemInfo; const EditInfo: TTBXEditInfo);
var
  R: TRect;
  W: Integer;
  Embedded: Boolean;
begin
  R := ARect;
  PaintFrame(DC, R, ItemInfo);
  W := EditFrameWidth;
  InflateRect(R, -W, -W);
  Embedded := (ItemInfo.ViewType and VT_CATEGORYMASK = VT_BARS) and (ItemInfo.ViewType and VT_EMBEDDED <> 0);
  if not (ItemInfo.Enabled or Embedded) then
    FrameRectEx(DC, R, BtnItemColors[bisDisabled, ipText], False);

  with EditInfo do if RightBtnWidth > 0 then Dec(R.Right, RightBtnWidth - 2);

  if ItemInfo.Enabled then
  begin
    if (ItemInfo.ViewType and VT_CATEGORYMASK <> VT_BARS) and (GetPartColor(ItemInfo, ipFrame) = clNone) then
      FrameRectEx(DC, R, ToolbarColor, False)
    else
      FrameRectEx(DC, R, clWindow, False);

    InflateRect(R, -1, -1);
    FillRectEx(DC, R, clWindow);
    if (ItemInfo.ViewType and VT_CATEGORYMASK <> VT_BARS) and (GetPartColor(ItemInfo, ipFrame) = clNone) then
    begin
      R := ARect;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, ToolbarColor, False);
    end;
  end
  else InflateRect(R, -1, -1);

  with EditInfo do if LeftBtnWidth > 0 then Inc(R.Left, LeftBtnWidth - 2);

  if EditInfo.RightBtnWidth > 0 then
  begin
    R := ARect;
    InflateRect(R, -W, -W);
    R.Left := R.Right - EditInfo.RightBtnWidth;
    PaintEditButton(DC, R, ItemInfo, EditInfo.RightBtnInfo);
  end;
end;

procedure TTBXOfficeXPTheme.PaintDropDownArrow(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  X, Y: Integer;
  C: TColor;
begin
  with ARect do
  begin
    X := (Left + Right) div 2;
    Y := (Top + Bottom) div 2 - 1;
    C := GetPartColor(ItemInfo, ipText);
    if ItemInfo.IsVertical then PolygonEx(DC, [Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)], C, C)
    else PolygonEx(DC, [Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)], C, C);
  end;
end;

procedure TTBXOfficeXPTheme.PaintButton(DC: HDC; const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  with ItemInfo do
  begin
    R := ARect;
    if ((ItemOptions and IO_DESIGNING) <> 0) and not Selected then
    begin
      if ComboPart = cpSplitRight then Dec(R.Left);
      FrameRectEx(DC, R, GetNearestColor(DC, MixColors(clBtnShadow, clBtnFace, 100)), False);
    end
    else
    begin
      FrameRectEx(DC, R, GetBtnColor(ItemInfo, ipFrame), True);
      if (ComboPart = cpSplitLeft) and IsPopupParent then Inc(R.Right);
      if ComboPart = cpSplitRight then Dec(R.Left);
      FillRectEx(DC, R, GetBtnColor(ItemInfo, ipBody));
    end;
    if ComboPart = cpSplitRight then PaintDropDownArrow(DC, R, ItemInfo);
  end;
end;

procedure TTBXOfficeXPTheme.PaintFloatingBorder(DC: HDC; const ARect: TRect;
  const WindowInfo: TTBXWindowInfo);
const
  WinStates: array [Boolean] of TWinFramestate = (wfsInactive, wfsActive);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if not WindowInfo.Active then Result := bisDisabled
    else if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  WinState: TWinFrameState;
  BtnItemState: TBtnItemState;
  X, Y: Integer;
  Sz: TPoint;
  R: TRect;
  BodyColor, CaptionColor, CaptionText, C: TColor;
  IsDockPanel: Boolean;
  OldFont: HFONT;
begin
  WinState := WinStates[WindowInfo.Active];
  IsDockPanel := (WindowInfo.ViewType and VT_DOCKPANEL) = VT_DOCKPANEL;
  BodyColor := WindowInfo.Color;

  if (WRP_BORDER and WindowInfo.RedrawPart) <> 0 then
  begin
    R := ARect;

    if not IsDockPanel then C := WinFrameColors[WinState, wfpBorder]
    else C := PnlFrameColors[WinState, wfpBorder];
      
    SaveDC(DC);
    Sz := WindowInfo.FloatingBorderSize;
    with R, Sz do ExcludeClipRect(DC, Left + X, Top + Y, Right - X, Bottom - Y);
    FillRectEx(DC, R, C);
    RestoreDC(DC, -1);
    InflateRect(R, -Sz.X, -Sz.Y);
    with R do
      if not IsDockPanel then
        PolylineEx(DC, [
          Point(Left, Top - 1), Point(Right - 1, Top - 1),
          Point(Right, Top), Point(Right, Bottom - 1),
          Point(Right - 1, Bottom),
          Point(Left, Bottom), Point(Left - 1, Bottom - 1),
          Point(Left - 1, Top), Point(Left, Top - 1)
          ], BodyColor)
      else
        PolylineEx(DC, [
          Point(Left, Top - 1), Point(Right - 1, Top - 1),
          Point(Right, Top), Point(Right, Bottom),
          Point(Left - 1, Bottom),
          Point(Left - 1, Top), Point(Left, Top - 1)
          ], BodyColor);
  end;

  if not WindowInfo.ShowCaption then Exit;

  if WindowInfo.ViewType and VT_CATEGORYMASK = VT_BARS then
  begin
    CaptionColor := WinFrameColors[WinState, wfpCaption];
    CaptionText := WinFrameColors[WinState, wfpCaptionText];
  end
  else
  begin
    CaptionColor := PnlFrameColors[WinState, wfpCaption];
    CaptionText := PnlFrameColors[WinState, wfpCaptionText];
  end;

  { Caption }
  if (WRP_CAPTION and WindowInfo.RedrawPart) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
    with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
    DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, BodyColor);

    if ((CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0) and
      ((WRP_CLOSEBTN and WindowInfo.RedrawPart) <> 0) then
      Dec(R.Right, GetSystemMetrics(SM_CYSMCAPTION) - 1);

    FillRectEx(DC, R, CaptionColor);
    InflateRect(R, -2, 0);

    OldFont := SelectObject(DC, SmCaptionFont.Handle);
    DrawTextRW(DC, WindowInfo.Caption, R,
      DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX, CaptionText);
    SelectObject(DC, OldFont);
  end;

  { Close button }
  if (CDBS_VISIBLE and WindowInfo.CloseButtonState) <> 0 then
  begin
    R := Rect(0, 0, WindowInfo.ClientWidth, GetSystemMetrics(SM_CYSMCAPTION) - 1);
    with WindowInfo.FloatingBorderSize do OffsetRect(R, X, Y);
    R.Left := R.Right - (R.Bottom - R.Top);
    DrawLineEx(DC, R.Left - 1, R.Bottom, R.Right, R.Bottom, BodyColor);
    FillRectEx(DC, R, CaptionColor);
    with R do
    begin
      X := (Left + Right - StockImgList.Width + 1) div 2;
      Y := (Top + Bottom - StockImgList.Height) div 2;
    end;
    BtnItemState := GetBtnItemState(WindowInfo.CloseButtonState);
    FrameRectEx(DC, R, BtnItemColors[BtnItemState, ipFrame], True);
    if FillRectEx(DC, R, BtnItemColors[BtnItemState, ipBody]) then
      DrawGlyph(DC, X, Y, StockImgList, 0, BtnItemColors[BtnItemState, ipText])
    else
      DrawGlyph(DC, X, Y, StockImgList, 0, CaptionText);
  end;
end;

procedure TTBXOfficeXPTheme.PaintFrame(DC: HDC; const ARect: TRect;
  const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  R := ARect;
  FrameRectEx(DC, R, GetPartColor(ItemInfo, ipFrame), True);
  FillRectEx(DC, R, GetPartColor(ItemInfo, ipBody));
end;

function TTBXOfficeXPTheme.GetImageOffset(DC: HDC;
  const ItemInfo: TTBXItemInfo; ImageList: TCustomImageList): TPoint;
begin
  Result.X := 0;
  if not (ImageList is TTBCustomImageList) then
    with ItemInfo do
      if Enabled and (HoverKind <> hkNone) and
      not (Selected or Pushed and not IsPopupParent) then
      Result.X := -1;
  Result.Y := Result.X
end;

procedure TTBXOfficeXPTheme.PaintImage(DC: HDC; ARect: TRect;
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

{$IFNDEF OFFICEXP_ALTERNATIVE_DISABLED_STYLE}
    HiContrast := IsDarkColor(GetItemImageBackground(ItemInfo), 64);

    if not Enabled then
    begin
      DrawTBXIconFlatShadow(DC, ARect, ImageList, ImageIndex,
        BtnItemColors[bisDisabled, ipText]);
    end
    else if Selected or Pushed or (HoverKind <> hkNone) then
    begin
      if not (Selected or Pushed and not IsPopupParent) then
      begin
        OffsetRect(ARect, 1, 1);
        DrawTBXIconFullShadow(DC, ARect, ImageList, ImageIndex, IconShadowColor);
        OffsetRect(ARect, -2, -2);
      end;
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast or TBXLoColor then
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
    else
      HighlightTBXIcon(DC, ARect, ImageList, ImageIndex, clWindow, 200);
{$ELSE}
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
      if not (Selected or Pushed and not IsPopupParent) then
      begin
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, 1, 1);
        DrawTBXIconShadow(DC, ARect, ImageList, ImageIndex, 1);
        OffsetRect(ARect, -2, -2);
      end;
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast);
    end
    else if HiContrast or TBXHiContrast or TBXLoColor then
      DrawTBXIcon(DC, ARect, ImageList, ImageIndex, HiContrast)
    else
      HighlightTBXIcon(DC, ARect, ImageList, ImageIndex, clWindow, 200);
{$ENDIF}
  end;
end;

procedure TTBXOfficeXPTheme.PaintMDIButton(DC: HDC; ARect: TRect;
  const ItemInfo: TTBXItemInfo; ButtonKind: Cardinal);
var
  Index: Integer;
begin
  PaintButton(DC, ARect, ItemInfo);
  Dec(ARect.Bottom);
  case ButtonKind of
    DFCS_CAPTIONMIN: Index := 2;
    DFCS_CAPTIONRESTORE: Index := 3;
    DFCS_CAPTIONCLOSE: Index := 0;
  else
    Exit;
  end;
  DrawGlyph(DC, ARect, StockImgList, Index, GetPartColor(ItemInfo, ipText));
end;

procedure TTBXOfficeXPTheme.PaintMenuItemFrame(DC: HDC;
  const ARect: TRect; const ItemInfo: TTBXItemInfo);
var
  R: TRect;
begin
  R := ARect;
  if ItemInfo.ViewType and VT_TYPEMASK <> VT_TOOLBOX then
  begin
    R.Right := R.Left + ItemInfo.PopupMargin + 2;
    FillRectEx(DC, R, ToolbarColor);
    Inc(R.Left);
    R.Right := ARect.Right - 1;
  end;
  PaintFrame(DC, R, ItemInfo);
end;

procedure TTBXOfficeXPTheme.PaintMenuItem(DC: HDC; const ARect: TRect; var ItemInfo: TTBXItemInfo);
var
  R: TRect;
  X, Y: Integer;
  ArrowWidth: Integer;
  C, ClrText: TColor;
begin
  with ItemInfo do
  begin
    ArrowWidth := GetSystemMetrics(SM_CXMENUCHECK);
    PaintMenuItemFrame(DC, ARect, ItemInfo);
    ClrText := GetPartColor(ItemInfo, ipText);
    R := ARect;

    if (ItemOptions and IO_COMBO) <> 0 then
    begin
      X := R.Right - ArrowWidth - 1;
      if not ItemInfo.Enabled then C := ClrText
      else if HoverKind = hkMouseHover then C := GetPartColor(ItemInfo, ipFrame)
      else C := PopupSeparatorColor;
      DrawLineEx(DC, X, R.Top + 1, X, R.Bottom - 1, C);
    end;

    if (ItemOptions and IO_SUBMENUITEM) <> 0 then
    begin
      Y := ARect.Bottom div 2;
      X := ARect.Right - ArrowWidth * 2 div 3 - 1;
      PolygonEx(DC, [Point(X, Y - 3), Point(X, Y + 3), Point(X + 3, Y)], ClrText, ClrText);
    end;

    if Selected and Enabled then
    begin
      R := ARect;
      R.Left := ARect.Left + 1;
      R.Right := R.Left + ItemInfo.PopupMargin;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, GetBtnColor(ItemInfo, ipFrame), True);
      FillRectEx(DC, R, GetBtnColor(ItemInfo, ipBody));
    end;
  end;
end;

procedure TTBXOfficeXPTheme.PaintPopupNCArea(DC: HDC; R: TRect; const PopupInfo: TTBXPopupInfo);
var
  PR: TRect;
begin
  FrameRectEx(DC, R, PopupFrameColor, True);
  FillRectEx(DC, R, PopupColor);

  if not IsRectEmpty(PopupInfo.ParentRect) then
  begin
    PR := PopupInfo.ParentRect;
    if not IsRectEmpty(PR) then with PR do
    begin
      if Bottom = R.Top then
      begin
        if Left <= R.Left then Left := R.Left - 1;
        if Right >= R.Right then Right := R.Right + 1;
        DrawLineEx(DC, Left + 1, Bottom - 1, Right - 1, Bottom - 1, ToolbarColor);
      end
      else if Top = R.Bottom then
      begin
        if Left <= R.Left then Left := R.Left - 1;
        if Right >= R.Right then Right := R.Right + 1;
        DrawLineEx(DC, Left + 1, Top, Right - 1, Top, ToolbarColor);
      end;
      if Right = R.Left then
      begin
        if Top <= R.Top then Top := R.Top - 1;
        if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
        DrawLineEx(DC, Right - 1, Top + 1, Right - 1, Bottom - 1, ToolbarColor);
      end
      else if Left = R.Right then
      begin
        if Top <= R.Top then Top := R.Top - 1;
        if Bottom >= R.Bottom then Bottom := R.Bottom + 1;
        DrawLineEx(DC, Left, Top + 1, Left, Bottom - 1, ToolbarColor);
      end;
    end;
  end;
end;

procedure TTBXOfficeXPTheme.PaintSeparator(DC: HDC; ARect: TRect;
  ItemInfo: TTBXItemInfo; Horizontal, LineSeparator: Boolean);
var
  IsToolbox: Boolean;
  R: TRect;
  C: TColor;
begin
  { Note: for blank separators, Enabled = False }
  with ItemInfo, ARect do
  begin
    if Horizontal then
    begin
      IsToolbox := ViewType and VT_TYPEMASK = VT_TOOLBOX;
      if ((ItemOptions and IO_TOOLBARSTYLE) = 0) and not IsToolBox then
      begin
        R := ARect;
        R.Right := ItemInfo.PopupMargin + 2;
        FillRectEx(DC, R, ToolbarColor);
        Inc(Left, ItemInfo.PopupMargin + 9);
        C := PopupSeparatorColor;
      end
      else
        C := ToolbarSeparatorColor;
      Top := (Top + Bottom) div 2;
      if Enabled then DrawLineEx(DC, Left, Top, Right, Top, C);
    end
    else if Enabled then
    begin
      Left := (Left + Right) div 2;
      DrawLineEx(DC, Left, Top, Left, Bottom, ToolbarSeparatorColor);
    end;
  end;
end;

procedure DrawButtonBitmap(DC: HDC; R: TRect; Color: TColor);
const
{$IFNDEF SMALL_CLOSE_BUTTON}
  Pattern: array [0..15] of Byte =
    ($C3, 0, $66, 0, $3C, 0, $18, 0, $3C, 0, $66, 0, $C3, 0, 0, 0);
{$ELSE}
  Pattern: array [0..15] of Byte =
    (0, 0, $63, 0, $36, 0, $1C, 0, $1C, 0, $36, 0, $63, 0, 0, 0);
{$ENDIF}
begin
  DrawGlyph(DC, R, 8, 7, Pattern[0], Color);
end;

procedure TTBXOfficeXPTheme.PaintToolbarNCArea(DC: HDC; R: TRect; const ToolbarInfo: TTBXToolbarInfo);
const
  DragHandleOffsets: array [Boolean, DHS_DOUBLE..DHS_SINGLE] of Integer = ((2, 0, 1), (5, 0, 5));

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  ToolbarColor: TColor;
  Sz: Integer;
  R2: TRect;
  I: Integer;
  BtnVisible, Horz: Boolean;
  BtnItemState: TBtnItemState;
  PatternBrush: HBrush;
begin
  ToolbarColor := ToolbarInfo.Color;
  if ToolbarColor = clDefault then ToolbarColor := GetViewColor(ToolbarInfo.ViewType);

  if ToolbarInfo.BorderStyle = bsSingle then
  begin
    I := ColorIntensity(clBtnFace);
    if not (TBXLoColor or not (I in [50..254])) or
      (ToolbarInfo.ViewType and VT_TYPEMASK = VT_MENUBAR) then
    begin
      InflateRect(R, -1, -1);
      Dec(R.Right); Dec(R.Bottom);
      with R do
        PolygonEx(DC, [Point(Left + 1, Top), Point(Right - 1, Top), Point(Right, Top + 1),
          Point(Right, Bottom - 1), Point(Right - 1, Bottom), Point(Left + 1, Bottom),
          Point(Left, Bottom - 1), Point(Left, Top + 1)], ToolbarColor, ToolbarColor);
      Inc(R.Left);
      Inc(R.Top);
    end
    else
    begin
      with R do
      begin
        PatternBrush := CreateDitheredBrush(ToolbarColor, BtnItemColors[bisDisabled, ipText]);
        Windows.FillRect(DC, Rect(Left + 1, Top, Right - 1, Top + 1), PatternBrush);
        Windows.FillRect(DC, Rect(Left + 1, Top, Right - 1, Top + 1), PatternBrush);
        Windows.FillRect(DC, Rect(Left + 1, Bottom - 1, Right - 1, Bottom), PatternBrush);
        Windows.FillRect(DC, Rect(Left, Top + 1, Left + 1, Bottom - 1), PatternBrush);
        Windows.FillRect(DC, Rect(Right - 1, Top + 1, Right, Bottom - 1), PatternBrush);
      end;
      FillRectEx(DC, R, ToolbarColor);
    end;
  end
  else
    InflateRect(R, -1, -1);
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
      Inc(R2.Left, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
      if BtnVisible then Inc(R2.Top, Sz - 2);
      R2.Right := R2.Left + 3;
    end
    else
    begin
      Inc(R2.Top, DragHandleOffsets[BtnVisible, ToolbarInfo.DragHandleStyle]);
      if BtnVisible then Dec(R2.Right, Sz - 2);
      R2.Bottom := R2.Top + 3;
    end;

    if Horz then
    begin
      I := R2.Top + 3;
      while I < R2.Bottom - 3 do
      begin
        DrawLineEx(DC, R2.Left, I, R2.Right, I, DragHandleColor);
        Inc(I, 2);
      end;
    end
    else
    begin
      I := R2.Left + 3;
      while I < R2.Right - 3 do
      begin
        DrawLineEx(DC, I, R2.Top, I, R2.Bottom, DragHandleColor);
        Inc(I, 2);
      end;
    end;
  end;

  { Close button }
  if BtnVisible then
  begin
    R2 := R;
    if Horz then
    begin
      Dec(R2.Right);
      R2.Bottom := R2.Top + R2.Right - R2.Left;
    end
    else
    begin
      Dec(R2.Bottom);
      R2.Left := R2.Right - R2.Bottom + R2.Top;
    end;

    BtnItemState := GetBtnItemState(ToolbarInfo.CloseButtonState);
    FrameRectEx(DC, R2, BtnItemColors[BtnItemState, ipFrame], True);
    FillRectEx(DC, R2, BtnItemColors[BtnItemState, ipBody]);
    DrawButtonBitmap(DC, R2, BtnItemColors[BtnItemState, ipText]);
  end;
end;

procedure TTBXOfficeXPTheme.PaintDock(DC: HDC; const ClientRect,
  DockRect: TRect; DockPosition: Integer);
begin
  // this theme does not support dock painting
end;

procedure TTBXOfficeXPTheme.PaintDockPanelNCArea(DC: HDC; R: TRect; const DockPanelInfo: TTBXDockPanelInfo);

  function GetBtnItemState(BtnState: Integer): TBtnItemState;
  begin
    if (BtnState and CDBS_PRESSED) <> 0 then Result := bisPressed
    else if (BtnState and CDBS_HOT) <> 0 then Result := bisHot
    else Result := bisNormal;
  end;

var
  PanelColor, C, HeaderColor: TColor;
  I, Sz, Flags: Integer;
  R2: TRect;
  BtnItemState: TBtnItemState;
  B: HBrush;
  OldBkMode: Cardinal;
  OldFont: HFont;
  OldTextColor: TColorRef;
begin
  with DockPanelInfo do
  begin
    PanelColor := Color;
    if PanelColor = clDefault then PanelColor := GetViewColor(ViewType);
    I := ColorIntensity(ColorToRGB(clBtnFace));
    R2 := R;
    if not TBXLoColor and (I in [64..250]) then
    begin
      FrameRectEx(DC, R, clBtnFace, True);
      FrameRectEx(DC, R, PanelColor, False);
      with R do
      begin
        C := GetSysColor(COLOR_BTNFACE);
        SetPixelV(DC, Left, Top, C);
        if IsVertical then SetPixelV(DC, Right -  1, Top, C)
        else SetPixelV(DC, Left, Bottom - 1, C);
      end;
    end
    else
    begin
      FrameRectEx(DC, R, PanelColor, True);

      if I < 64 then B := CreateDitheredBrush(PanelColor, clWhite)
      else B := CreateDitheredBrush(PanelColor, clBtnShadow);
      Windows.FrameRect(DC, R, B);
      DeleteObject(B);

      with R do
      begin
        SetPixelV(DC, Left, Top, PanelColor);
        if IsVertical then SetPixelV(DC, Right -  1, Top, PanelColor)
        else SetPixelV(DC, Left, Bottom - 1, PanelColor);
      end;
      InflateRect(R, -1, -1);
      FrameRectEx(DC, R, PanelColor, False);
    end;
    R := R2;
    InflateRect(R, -BorderSize.X, -BorderSize.Y);
    Sz := GetSystemMetrics(SM_CYSMCAPTION);
    if IsVertical then
    begin
      R.Bottom := R.Top + Sz - 1;
      DrawLineEx(DC, R.Left, R.Bottom, R.Right, R.Bottom, PanelColor);
    end
    else
    begin
      R.Right := R.Left + Sz - 1;
      DrawLineEx(DC, R.Right, R.Top, R.Right, R.Bottom, PanelColor);
    end;
    HeaderColor := clBtnFace;
    FillRectEx(DC, R, HeaderColor);

    if (CDBS_VISIBLE and CloseButtonState) <> 0 then
    begin
      R2 := R;
      if IsVertical then
      begin
        R2.Left := R2.Right - Sz + 1;
        R.Right := R2.Left;
      end
      else
      begin
        R2.Top := R2.Bottom - Sz + 1;
        R.Bottom := R2.Top;
      end;

      BtnItemState := GetBtnItemState(CloseButtonState);
      FrameRectEx(DC, R2, BtnItemColors[BtnItemState, ipFrame], True);
      FillRectEx(DC, R2, BtnItemColors[BtnItemState, ipBody]);
      DrawButtonBitmap(DC, R2, BtnItemColors[BtnItemState, ipText]);
    end;


    if IsVertical then InflateRect(R, -4, 0)
    else InflateRect(R, 0, -4);

    OldFont := SelectObject(DC, SmCaptionFont.Handle);
    OldBkMode := SetBkMode(DC, TRANSPARENT);
    OldTextColor := SetTextColor(DC, ColorToRGB(SmCaptionFont.Color));
    Flags := DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
    if IsVertical then _DrawTextW(DC, Caption, -1, R, Flags)
    else DrawRotatedTextW(DC, Caption, R, Flags);
    SetTextColor(DC, OldTextColor);
    SetBkMode(DC, OldBkMode);
    SelectObject(DC, OldFont);
  end;
end;

procedure TTBXOfficeXPTheme.SetupColorCache;
var
  DC: HDC;
  HotBtnFace, DisabledText: TColor;

  procedure Undither(var C: TColor);
  begin
    if C <> clNone then C := GetNearestColor(DC, ColorToRGB(C));
  end;

begin
  DC := StockCompatibleBitmap.Canvas.Handle;

  if TBXLoColor then
  begin
    { View/Window Colors }
    MenubarColor := clBtnFace;
    ToolbarColor := clBtnFace;
    PopupColor := clWindow;
    DockPanelColor := clWindow;
    StatusPanelFrameColor := clBtnShadow;

    PopupFrameColor := clBtnText;
    WinFrameColors[wfsActive, wfpBorder]        := clBtnShadow;
    WinFrameColors[wfsActive, wfpCaption]       := clBtnShadow;
    WinFrameColors[wfsActive, wfpCaptionText]   := clBtnHighlight;
    WinFrameColors[wfsInactive, wfpBorder]      := clBtnShadow;
    WinFrameColors[wfsInactive, wfpCaption]     := clBtnShadow;
    WinFrameColors[wfsInactive, wfpCaptionText] := clBtnHighlight;

    PnlFrameColors[wfsActive, wfpBorder]        := clBtnShadow;
    PnlFrameColors[wfsActive, wfpCaption]       := clBtnFace;
    PnlFrameColors[wfsActive, wfpCaptionText]   := clBtnText;
    PnlFrameColors[wfsInactive, wfpBorder]      := clBtnShadow;
    PnlFrameColors[wfsInactive, wfpCaption]     := clBtnFace;
    PnlFrameColors[wfsInactive, wfpCaptionText] := clBtnText;

    MenuItemColors[misNormal, ipBody]          := clNone;
    MenuItemColors[misNormal, ipText]          := clWindowText;
    MenuItemColors[misNormal, ipFrame]         := clNone;
    MenuItemColors[misDisabled, ipBody]        := clNone;
    MenuItemColors[misDisabled, ipText]        := clGrayText;
    MenuItemColors[misDisabled, ipFrame]       := clNone;
    MenuItemColors[misHot, ipBody]             := clWindow;
    MenuItemColors[misHot, ipText]             := clWindowtext;
    MenuItemColors[misHot, ipFrame]            := clHighlight;
    MenuItemColors[misDisabledHot, ipBody]     := clWindow;
    MenuItemColors[misDisabledHot, ipText]     := clGrayText;
    MenuItemColors[misDisabledHot, ipFrame]    := clHighlight;

    BtnItemColors[bisNormal, ipBody]           := clNone;
    BtnItemColors[bisNormal, ipText]           := clBtnText;
    BtnItemColors[bisNormal, ipFrame]          := clNone;
    BtnItemColors[bisDisabled, ipBody]         := clNone;
    BtnItemColors[bisDisabled, ipText]         := clBtnShadow;
    BtnItemColors[bisDisabled, ipFrame]        := clNone;
    BtnItemColors[bisSelected, ipBody]         := clWindow;
    BtnItemColors[bisSelected, ipText]         := clWindowText;
    BtnItemColors[bisSelected, ipFrame]        := clHighlight;
    BtnItemColors[bisPressed, ipBody]          := clHighlight;
    BtnItemColors[bisPressed, ipText]          := clHighlightText;
    BtnItemColors[bisPressed, ipFrame]         := clHighlight;
    BtnItemColors[bisHot, ipBody]              := clWindow;
    BtnItemColors[bisHot, ipText]              := clWindowText;
    BtnItemColors[bisHot, ipFrame]             := clHighlight;
    BtnItemColors[bisDisabledHot, ipBody]      := clWindow;
    BtnItemColors[bisDisabledHot, ipText]      := clBtnShadow;
    BtnItemColors[bisDisabledHot, ipFrame]     := clHighlight;
    BtnItemColors[bisSelectedHot, ipBody]      := clHighlight;
    BtnItemColors[bisSelectedHot, ipText]      := clHighlightText;
    BtnItemColors[bisSelectedHot, ipFrame]     := clHighlight;
    BtnItemColors[bisPopupParent, ipBody]      := clBtnFace;
    BtnItemColors[bisPopupParent, ipText]      := clBtnText;
    BtnItemColors[bisPopupParent, ipFrame]     := PopupFrameColor;

    { Other Colors }
    DragHandleColor := clBtnText;
    IconShadowColor := clBtnFace;
    PopupSeparatorColor := clBtnShadow;
    ToolbarSeparatorColor := clBtnShadow;
  end
 else
  begin
    { View/Window Colors }
    MenubarColor := clBtnFace;
    ToolbarColor := Blend(clWindow, clBtnFace, 165);
    PopupColor := Blend(clBtnFace, clWindow, 143);
    DockPanelColor := PopupColor;
    PopupFrameColor := Blend(clBtnText, clBtnShadow, 20);
    SetContrast(PopupFrameColor, PopupColor, 100);

    HotBtnFace := Blend(clHighlight, clWindow, 30);
    SetContrast(HotBtnFace, ToolbarColor, 50);
    DisabledText := Blend(clBtnshadow, clWindow, 90);

    WinFrameColors[wfsActive, wfpBorder]        := Blend(clBtnText, clBtnShadow, 15);
    SetContrast(WinFrameColors[wfsActive, wfpBorder], ToolbarColor, 120);
    WinFrameColors[wfsActive, wfpCaption]       := clBtnShadow;
    WinFrameColors[wfsActive, wfpCaptionText]   := clBtnHighlight;
    SetContrast(WinFrameColors[wfsActive, wfpCaptionText], clBtnShadow, 180);
    WinFrameColors[wfsInactive, wfpBorder]      := WinFrameColors[wfsActive, wfpBorder];
    WinFrameColors[wfsInactive, wfpCaption]     := clBtnFace;
    WinFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
    SetContrast(WinFrameColors[wfsInactive, wfpCaptionText], clBtnFace, 120);

    PnlFrameColors[wfsActive, wfpBorder]        := clBtnShadow;
    PnlFrameColors[wfsActive, wfpCaption]       := clBtnFace;
    PnlFrameColors[wfsActive, wfpCaptionText]   := clBtnText;
    PnlFrameColors[wfsInactive, wfpBorder]      := clBtnShadow;
    PnlFrameColors[wfsInactive, wfpCaption]     := clBtnFace;
    PnlFrameColors[wfsInactive, wfpCaptionText] := DisabledText;
    SetContrast(PnlFrameColors[wfsInactive, wfpCaptionText], clBtnFace, 120);

    BtnItemColors[bisNormal, ipBody]           := clNone;
    BtnItemColors[bisNormal, ipText]           := clBtnText;
    SetContrast(BtnItemColors[bisNormal, ipText], ToolbarColor, 180);
    BtnItemColors[bisNormal, ipFrame]          := clNone;
    BtnItemColors[bisDisabled, ipBody]         := clNone;
    BtnItemColors[bisDisabled, ipText]         := DisabledText;
    SetContrast(BtnItemColors[bisDisabled, ipText], ToolbarColor, 80);
    BtnItemColors[bisDisabled, ipFrame]        := clNone;
    BtnItemColors[bisSelected, ipBody]         := Blend(clHighlight, Blend(clBtnFace, clWindow, 50), 10);
    SetContrast(BtnItemColors[bisSelected, ipBody], ToolbarColor, 5);
    BtnItemColors[bisSelected, ipText]         := BtnItemColors[bisNormal, ipText];
    BtnItemColors[bisSelected, ipFrame]        := clHighlight;
    BtnItemColors[bisPressed, ipBody]          := Blend(clHighlight, clWindow, 50);
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
    BtnItemColors[bisSelectedHot, ipBody]      := Blend(clHighlight, clWindow, 50);
    SetContrast(BtnItemColors[bisSelectedHot, ipBody], ToolbarColor, 30);
    BtnItemColors[bisSelectedHot, ipText]      := clHighlightText;
    SetContrast(BtnItemColors[bisSelectedHot, ipText], BtnItemColors[bisSelectedHot, ipBody], 180);
    BtnItemColors[bisSelectedHot, ipFrame]     := clHighlight;
    SetContrast(BtnItemColors[bisSelectedHot, ipFrame], BtnItemColors[bisSelectedHot, ipBody], 100);
    BtnItemColors[bisPopupParent, ipBody]      := ToolbarColor;
    BtnItemColors[bisPopupParent, ipText]      := BtnItemColors[bisNormal, ipText];
    BtnItemColors[bisPopupParent, ipFrame]     := PopupFrameColor;

    MenuItemColors[misNormal, ipBody]          := clNone;
    MenuItemColors[misNormal, ipText]          := clWindowText;
    SetContrast(MenuItemColors[misNormal, ipText], PopupColor, 180);
    MenuItemColors[misNormal, ipFrame]         := clNone;
    MenuItemColors[misDisabled, ipBody]        := clNone;
    MenuItemColors[misDisabled, ipText]        := Blend(clGrayText, clWindow, 70);
    SetContrast(MenuItemColors[misDisabled, ipText], PopupColor, 80); // 145?
    MenuItemColors[misDisabled, ipFrame]       := clNone;
    MenuItemColors[misHot, ipBody]             := BtnItemColors[bisHot, ipBody];
    MenuItemColors[misHot, ipText]             := BtnItemColors[bisHot, ipText];
    MenuItemColors[misHot, ipFrame]            := BtnItemColors[bisHot, ipFrame];
    MenuItemColors[misDisabledHot, ipBody]     := PopupColor;
    MenuItemColors[misDisabledHot, ipText]     := Blend(clGrayText, clWindow, 70);
    MenuItemColors[misDisabledHot, ipFrame]    := clHighlight;

    { Other Colors }
    DragHandleColor := Blend(clBtnShadow, clWindow, 75);
    SetContrast(DragHandleColor, ToolbarColor, 85);
    IconShadowColor := Blend(clBlack, HotBtnFace, 25);
    ToolbarSeparatorColor := Blend(clBtnShadow, clWindow, 70);
    SetContrast(ToolbarSeparatorColor, ToolbarColor, 50);
    PopupSeparatorColor := ToolbarSeparatorColor;
    StatusPanelFrameColor := Blend(clWindow, clBtnFace, 30);
    SetContrast(StatusPanelFrameColor, clBtnFace, 30);

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
    Undither(DragHandleColor);
    Undither(IconShadowColor);
    Undither(ToolbarSeparatorColor);
    Undither(PopupSeparatorColor);
    Undither(StatusPanelFrameColor);
  end;

  DockColor := clBtnFace;
  StatusBarColor := clBtnFace;
end;

function TTBXOfficeXPTheme.GetPopupShadowType: Integer;
begin
  Result := PST_OFFICEXP;
end;

constructor TTBXOfficeXPTheme.Create(const AName: string);
begin
  inherited;
  if CounterLock = 0 then InitializeStock;
  Inc(CounterLock);
  AddTBXSysChangeNotification(Self);
  SetupColorCache;
end;

destructor TTBXOfficeXPTheme.Destroy;
begin
  RemoveTBXSysChangeNotification(Self);
  Dec(CounterLock);
  if CounterLock = 0 then FinalizeStock;
  inherited;
end;

procedure TTBXOfficeXPTheme.GetViewMargins(ViewType: Integer; out Margins: TTBXMargins);
begin
  Margins.LeftWidth := 0;
  Margins.TopHeight := 0;
  Margins.RightWidth := 0;
  Margins.BottomHeight := 0;
end;

procedure TTBXOfficeXPTheme.PaintPageScrollButton(DC: HDC;
  const ARect: TRect; ButtonType: Integer; Hot: Boolean);
var
  R: TRect;
  X, Y, Sz: Integer;
  C: TColor;
begin
  R := ARect;
  if Hot then C := BtnItemColors[bisHot, ipFrame]
  else C := clBtnShadow;
  FrameRectEx(DC, R, C, False);
  InflateRect(R, -1, -1);
  if Hot then C := BtnItemColors[bisHot, ipBody]
  else C := clBtnFace;
  FillRectEx(DC, R, C);
  X := (R.Left + R.Right) div 2;
  Y := (R.Top + R.Bottom) div 2;
  Sz := Min(X - R.Left, Y - R.Top) * 3 div 4;

  if Hot then C := BtnItemColors[bisHot, ipText]
  else C := BtnItemColors[bisNormal, ipText];

  case ButtonType of
    PSBT_UP:
      begin
        Inc(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)], C, C);
      end;
    PSBT_DOWN:
      begin
        Y := (R.Top + R.Bottom - 1) div 2;
        Dec(Y, Sz div 2);
        PolygonEx(DC, [Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)], C, C);
      end;
    PSBT_LEFT:
      begin
        Inc(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)], C, C);
      end;
    PSBT_RIGHT:
      begin
        X := (R.Left + R.Right - 1) div 2;
        Dec(X, Sz div 2);
        PolygonEx(DC, [Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)], C, C);
      end;
  end;
end;

procedure TTBXOfficeXPTheme.PaintFrameControl(DC: HDC; R: TRect; Kind, State: Integer; Params: Pointer);
var
  X, Y: Integer;
  Pen, OldPen: HPen;
  Brush, OldBrush: HBrush;
  C: TColor;

  function GetPenColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := clBtnShadow
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipFrame]
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipFrame]
    else Result := clBtnShadow;
  end;

  function GetBrush: HBrush;
  begin
    if Boolean(State and PFS_DISABLED) then Result := CreateBrushEx(clNone)
    else if Boolean(State and PFS_PUSHED) then Result := CreateBrushEx(BtnItemColors[bisPressed, ipBody])
    else if Boolean(State and PFS_HOT) then Result := CreateBrushEx(BtnItemColors[bisHot, ipBody])
    else if Boolean(State and PFS_MIXED) then Result := CreateDitheredBrush(clWindow, clBtnFace)
    else Result := CreateBrushEx(clNone);
  end;

  function GetTextColor: TColor;
  begin
    if Boolean(State and PFS_DISABLED) then Result := BtnItemColors[bisDisabled, ipText]
    else if Boolean(State and PFS_PUSHED) then Result := BtnItemColors[bisPressed, ipText]
    else if Boolean(State and PFS_MIXED) then Result := clBtnShadow
    else if Boolean(State and PFS_HOT) then Result := BtnItemColors[bisHot, ipText]
    else Result := BtnItemColors[bisNormal, ipText];
  end;

begin
  case Kind of
    PFC_CHECKBOX:
      begin
        InflateRect(R, -1, -1);
        FrameRectEx(DC, R, GetPenColor, True);
        Brush := GetBrush;
        Windows.FillRect(DC, R, Brush);
        DeleteObject(Brush);
        InflateRect(R, 1, 1);

        if Boolean(State and (PFS_CHECKED or PFS_MIXED)) then
        begin
          X := (R.Left + R.Right) div 2 - 1;
          Y := (R.Top + R.Bottom) div 2 + 1;
          C := GetTextColor;
          PolygonEx(DC, [Point(X-2, Y), Point(X, Y+2), Point(X+4, Y-2),
            Point(X+4, Y-4), Point(X, Y), Point(X-2, Y-2), Point(X-2, Y)], C, C);
        end;
      end;
    PFC_RADIOBUTTON:
      begin
        InflateRect(R, -1, -1);

        with R do
        begin
          Brush := GetBrush;
          OldBrush := SelectObject(DC, Brush);
          Pen := CreatePenEx(GetPenColor);
          OldPen := SelectObject(DC, Pen);
          Windows.Ellipse(DC, Left, Top, Right, Bottom);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
          SelectObject(DC, OldBrush);
          DeleteObject(Brush);
        end;

        if Boolean(State and PFS_CHECKED) then
        begin
          InflateRect(R, -3, -3);
          C := GetTextColor;
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
  end;
end;

procedure TTBXOfficeXPTheme.PaintStatusBar(DC: HDC; R: TRect; Part: Integer);
var
  D, Sz, I: Integer;

  procedure DiagLine(C: TColor);
  begin
    with R do
      DrawLineEx(DC, Right - 1 - D, Bottom - 1, Right, Bottom - D - 2, C);
    Inc(D);
  end;

begin
  case Part of
    SBP_BODY:
      begin
        FillRectEx(DC, R, clBtnFace);
      end;
    SBP_PANE, SBP_LASTPANE:
      begin
        if Part = SBP_PANE then Dec(R.Right, 2);
        FrameRectEx(DC, R, StatusPanelFrameColor, False);
      end;
    SBP_GRIPPER:
      begin
        D := 0;
        Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
        for I := 1 to 3 do
          case Sz of
            0..8:
              begin
                DiagLine(clBtnShadow);
                DiagLine(clBtnHighlight);
              end;
            9..11:
              begin
                DiagLine(clBtnFace);
                DiagLine(clBtnShadow);
                DiagLine(clBtnHighlight);
              end;
            12..14:
              begin
                DiagLine(clBtnShadow);
                DiagLine(clBtnShadow);
                DiagLine(clBtnHighlight);
              end;
          else
            DiagLine(clBtnFace);
            DiagLine(clBtnShadow);
            DiagLine(clBtnShadow);
            DiagLine(clBtnHighlight);
          end;
      end;
  end;
end;

procedure TTBXOfficeXPTheme.TBXSysCommand(var Message: TMessage);
begin
  if Message.WParam = TSC_VIEWCHANGE then SetupColorCache;
end;

initialization
  RegisterTBXTheme('OfficeXP', TTBXOfficeXPTheme);

end.
