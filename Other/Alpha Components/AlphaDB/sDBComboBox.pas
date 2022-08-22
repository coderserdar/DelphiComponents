unit sDBComboBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DBCtrls, math,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  acSBUtils, sConst, acntUtils, sGraphUtils, sCommonData, sDefaults;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBComboBox = class(TDBComboBox)
  private
    FAlignment: TAlignment;
    FButtonMargin: integer;
    FBoundLabel: TsBoundLabel;
    FCommonData: TsCommonData;
    FDisabledKind: TsDisabledKind;
    procedure SetAlignment   (const Value: TAlignment);
    procedure SetButtonMargin(const Value: integer);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure WMPrintClient(var Message: TWMPaint); message WM_PRINTCLIENT;
    function FontStored: boolean;
  protected
    actM,
    actD: integer;

    lboxhandle: hwnd;
    ListSW: TacScrollWnd;

    procedure DropDown; override;
    procedure PaintBorder(ADC: HDC);
    procedure PrepareCache;
    procedure PaintText;
    procedure PaintButton;
    procedure OurPaintHandler(iDC: hdc);

    procedure ChangeScale(M, D: Integer); override;
{$IFNDEF DELPHI5}
    function GetItemHt: Integer; override;
{$ENDIF}
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem   (var Message: TWMDrawItem);    message CN_DRAWITEM;
    procedure WMMouseWheel (var Message: TMessage);       message WM_MOUSEWHEEL;
    procedure WndProc      (var Message: TMessage); override;
    procedure ComboWndProc (var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function EditText: string;
  public
    FDropDown: boolean;

    procedure AfterConstruction; override;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    property Color;
    procedure Invalidate; override;
    procedure InvalidateSelection;
    function ButtonRect: TRect;
    function ButtonHeight: integer;

    procedure WriteText;
    function Focused: Boolean; override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ButtonMargin: integer read FButtonMargin write SetButtonMargin default 2;
  published
    property Font stored FontStored;
    property ParentFont stored FontStored;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property DropDownCount default 16;
    property SkinData: TsCommonData read FCommonData write FCommonData;
  end;


implementation

uses sMessages, sMaskData, sStyleSimply, sSkinProps, sVCLUtils, sAlphaGraph, acGlow
{$IFDEF DELPHI7UP}{$IFNDEF D2009}, Themes{$ENDIF}{$ENDIF};


const
  BordWidth = 3;


procedure TsDBComboBox.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


function TsDBComboBox.ButtonHeight: integer;
begin
  with SkinData, CommonSkinData.ComboBtn do
    if Skinned and (GlyphIndex >= 0) then
      Result := SkinData.CommonSkinData.ma[GlyphIndex].Height
    else
      Result := 16;
end;


function TsDBComboBox.ButtonRect: TRect;
var
  w: integer;
begin
  w := iff(Style <> csSimple, GetSystemMetrics(SM_CXVSCROLL), 0);
  Result.Left := iff(UseRightToLeftAlignment, 3, Width - w - 3);        
  Result.Top := 3;
  Result.Right := Result.Left + w;
  Result.Bottom := Height - 3;
end;


procedure TsDBComboBox.ChangeScale(M, D: Integer);
var
  UpdateNeeded: boolean;
begin
  UpdateNeeded := (actM <> M) or (actD <> D);
  actM := M;
  actD := D;
  inherited;
  if UpdateNeeded then
    RecreateWnd;
end;


procedure TsDBComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    if itemState and ODS_COMBOBOXEDIT <> 0 then
      Include(State, odComboBoxEdit);

    if itemState and ODS_DEFAULT <> 0 then
      Include(State, odDefault);

    Canvas.Handle := hDC;
    DrawItem(integer(itemID), rcItem, State);
    Canvas.Handle := 0;
  end;
end;


procedure TsDBComboBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  if not (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then
    Message.MeasureItemStruct^.itemHeight := GetFontHeight(Font.Handle) - 4
  else
    inherited;
end;


procedure TsDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
var
  ps: TPaintStruct;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  if not (csDestroying in ComponentState) then begin
    if ReadOnly then
      case Message.Msg of
        WM_KEYDOWN, WM_CHAR, WM_KEYUP, WM_SYSKEYUP, CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN, CN_SYSCHAR, WM_PASTE, WM_CUT, WM_CLEAR, WM_UNDO:
          Exit;
      end;

    if FCommonData.Skinned then
      case Message.Msg of
        WM_ERASEBKGND, WM_NCPAINT:
          if (Style <> csSimple) and (not (Focused or FCommonData.FFocused) or not Enabled or ReadOnly) then begin
            Message.Result := 1;
            Exit;
          end;

        WM_PAINT:
          if (Style <> csSimple) and (not (Focused or FCommonData.FFocused) or not Enabled or ReadOnly) then begin
            BeginPaint(ComboWnd, PS);
            EndPaint(ComboWnd, PS);
            Exit;
          end;
      end;
  end;
  inherited;
end;


constructor TsDBComboBox.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsComboBox;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csFixedHeight, csFramed, csOpaque];
  TControlCanvas(Canvas).Control := self;
  FDropDown := False;

  ParentColor := False;
  actM := 1;
  actD := 1;

  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  FButtonMargin := 2;
  DropDownCount := 16;
  DoubleBuffered := True;
end;


procedure TsDBComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Style in [csDropDown, csDropDownList] then
    Params.Style := Params.Style or CBS_OWNERDRAWFIXED and not CBS_OWNERDRAWVARIABLE;
end;


procedure TsDBComboBox.CreateWnd;
begin
  inherited;
{$IFDEF DELPHI7UP}
{$IFNDEF D2009}
  if CheckWin32Version(5, 1) and ThemeServices.ThemesEnabled then
    SendMessage(Handle, $1701{CB_SETMINVISIBLE}, WPARAM(DropDownCount), 0);
{$ENDIF}
{$ENDIF}
  FCommonData.Loaded;
end;


destructor TsDBComboBox.Destroy;
begin
  if lBoxHandle <> 0 then begin
    SetWindowLong(lBoxHandle, GWL_STYLE, GetWindowLong(lBoxHandle, GWL_STYLE) and not WS_THICKFRAME or WS_BORDER);
    UninitializeACScroll(lBoxHandle, True, False, ListSW);
    lBoxHandle := 0;
  end;
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


procedure TsDBComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  s: acString;
  Bmp: TBitmap;
  aRect: TRect;
  sNdx: integer;
  CI: TCacheInfo;
  DrawStyle: longint;
  OldDC, SavedDC: hdc;
  C, TmpColor: TColor;
begin
  aRect := MkRect(WidthOf(Rect), HeightOf(Rect));
  DrawStyle := DT_NOPREFIX or DT_EXPANDTABS or DT_SINGLELINE or DT_TOP or DT_NOCLIP;
  if SkinData.Skinned then begin
    Bmp := CreateBmp32(WidthOf(Rect, True), HeightOf(Rect, True));
    Bmp.Canvas.Font.Assign(Font);
    if odComboBoxEdit in State then
      CI := MakeCacheInfo(FCommonData.FCacheBmp, Rect.Left, Rect.Top)
    else begin
      CI.Bmp := nil;
      CI.Ready := False;
      CI.FillColor := Color;
    end;
    if odSelected in State then begin
      sNdx := SkinData.SkinManager.GetSkinIndex(s_Selection);
      C := SkinData.SkinManager.GetHighLightColor(True);
      Canvas.Brush.Color := C;
      if sNdx < 0 then
        FillDC(Bmp.Canvas.Handle, MkRect(Bmp), C)
      else
        PaintItem(sNdx, CI, True, 1, MkRect(Bmp), Point(0, 0), Bmp, SkinData.CommonSkinData);

      Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(True);
    end
    else begin
      sNdx := -1;
      if odComboBoxEdit in State then
        C := Color
      else
        C := SkinData.SkinManager.GetActiveEditColor;

      if CI.Ready then
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, CI.Bmp.Canvas.Handle, CI.X, CI.Y, SRCCOPY)
      else
        FillDC(Bmp.Canvas.Handle, MkRect(Bmp), C);

      Canvas.Brush.Color := C;
    end;
    if Assigned(OnDrawItem) and (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then begin
      if IsValidIndex(Index, Items.Count) then begin
        OldDC := Canvas.Handle;
        Canvas.Handle := Bmp.Canvas.Handle;
        Bmp.Canvas.Lock;
        SavedDC := SaveDC(Canvas.Handle);
        try
          MovewindowOrg(Canvas.Handle, -Rect.Left, -Rect.Top);
          OnDrawItem(Self, Index, Rect, State);
        finally
          RestoreDC(Canvas.Handle, SavedDC);
          Bmp.Canvas.UnLock;
        end;
        Canvas.Handle := OldDC;
      end
    end
    else begin
      if UseRightToLeftAlignment then
        DrawStyle := DrawStyle or DT_RIGHT;

      if UseRightToLeftReading then
        DrawStyle := DrawStyle or DT_RTLREADING;

      if (csDropDown = Style) and (odComboBoxEdit in State) then begin
        Bmp.Canvas.Brush.Style := bsClear;
        AcDrawText(Bmp.Canvas.Handle, EditText, aRect, DrawStyle);
      end
      else begin
        InflateRect(aRect, -2, 0);
        if sNdx < 0 then begin
          if odSelected in State then
            Bmp.Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(True)
          else
            if odComboBoxEdit in State then
              Bmp.Canvas.Font.Color := Font.Color
            else
              Bmp.Canvas.Font.Color := SkinData.SkinManager.GetActiveEditFontColor;

          Bmp.Canvas.Brush.Style := bsClear;
          s := Items[Index];
          AcDrawText(Bmp.Canvas.Handle, s, aRect, DrawStyle);
        end
        else
          acWriteTextEx(Bmp.Canvas, PacChar(Items[Index]), True, aRect, DrawStyle, sNdx, True, SkinData.SkinManager);

        if (odFocused in State) and (sNdx < 0) then begin
          Bmp.Canvas.Brush.Style := bsSolid;
          InflateRect(aRect, 2, 0);
          DrawFocusRect(Bmp.Canvas.Handle, aRect);
        end;
      end;
    end;
    BitBlt(Canvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end
  else begin
    Canvas.Font.Assign(Font);
    if odSelected in State then begin
      TmpColor           := ColorToRGB(clHighLight);
      Canvas.Font.Color  := ColorToRGB(clHighLightText);
      Canvas.Brush.Color := ColorToRGB(clHighLight);
    end
    else begin
      TmpColor := ColorToRGB(Color);
      Canvas.Font.Color := ColorToRGB(Font.Color);
      Canvas.Brush.Color := Color;
    end;
    FillDC(Canvas.Handle, Rect, TmpColor);
    if IsValidIndex(Index, Items.Count) then begin
      InflateRect(Rect, -2, 0);
      if Assigned(OnDrawItem) and (Style in [csOwnerDrawFixed, csOwnerDrawVariable]) then
        OnDrawItem(Self, Index, Rect, State)
      else begin
        Canvas.Brush.Style := bsClear;
        AcDrawText(Canvas.Handle, Items[Index], Rect, DrawStyle);
      end;
      if odFocused in State then begin
        InflateRect(Rect, 2, 0);
        DrawFocusRect(Canvas.Handle, Rect);
      end;
    end;
  end;
end;


procedure TsDBComboBox.DropDown;
begin
  FDropDown := True;
  inherited;
end;


function TsDBComboBox.Focused: Boolean;
var
  FocusedWnd: HWND;
begin
  Result := False;
  if HandleAllocated then begin
    FocusedWnd := GetFocus;
    if (FocusedWnd <> 0) and ((FocusedWnd = EditHandle) or (FocusedWnd = ListHandle)) then
      Result := True
    else
      Result := Assigned(FCommonData) and FCommonData.FFocused;
  end;
end;


function TsDBComboBox.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


{$IFNDEF DELPHI5}
function TsDBComboBox.GetItemHt: Integer;
begin
  Result := SendMessage(Handle, CB_GETITEMHEIGHT, 0, 0);
end;
{$ENDIF}


procedure TsDBComboBox.Invalidate;
begin
  if Focused then
    FCommonData.FFocused := True;

  inherited Invalidate;
end;


procedure TsDBComboBox.InvalidateSelection;
begin
//
end;


procedure TsDBComboBox.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if FCommonData.Skinned then begin
    if not FCommonData.CustomColor then
      Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Color;

    if not FCommonData.CustomFont then
      Font.Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].FontColor.Color;
  end;
end;


function IsOwnerDraw(Ctrl: TsDBComboBox): boolean;
begin
  Result := (Ctrl.Style in [csOwnerDrawFixed, csOwnerDrawVariable]) and Assigned(Ctrl.OnDrawItem)
end;


procedure TsDBComboBox.OurPaintHandler(iDC: hdc);
const
  BordWidth = 3;
var
  DC: hdc;
  R: TRect;
begin
  if Showing and HandleAllocated then begin
    if iDC = 0 then
      DC := GetDC(Handle)
    else
      DC := iDC;

    R := ButtonRect;
    try
      if not InUpdating(FCommonData) and not (InAnimationProcess and not ((SkinData.PrintDC = 0) or (SkinData.PrintDC = DC))) then begin
        FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE) or IsOwnerDraw(Self);
        FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);

        if FCommonData.BGChanged then
          PrepareCache;

        UpdateCorners(FCommonData, 0);
        case Style of
          csSimple:
            BitBltBorder(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);

          csDropDown:
            if Focused then begin
              BitBltBorder(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);
              R := ButtonRect;
              BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
            end
            else
              BitBlt(DC, 0, 0, Width, FCommonData.FCacheBmp.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

          csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable: begin
            BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
            Canvas.Handle := 0;
          end;
        end;

{$IFDEF DYNAMICCACHE}
        if Assigned(FCommonData.FCacheBmp) then
          FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
      end;
    finally
      if iDC = 0 then
        ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsDBComboBox.PaintBorder(ADC: HDC);
var
  DC, SavedDC: HDC;
  R: TRect;
begin
  SavedDC := 0;
  if ADC = 0 then begin
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
  end
  else begin
    DC := ADC;
    MoveWindowOrg(DC, -BordWidth, -BordWidth);
    SelectClipRgn(DC,0);
  end;
  try
    PrepareCache;
    UpdateCorners(FCommonData, 0);
    BitBlt(DC, 0, 0, Width, BordWidth, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(DC, 0, BordWidth, BordWidth, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, BordWidth, SRCCOPY);
    BitBlt(DC, BordWidth, Height - BordWidth, Width - BordWidth, BordWidth, FCommonData.FCacheBmp.Canvas.Handle, BordWidth, Height - BordWidth, SRCCOPY);
    BitBlt(DC, Width - BordWidth, BordWidth, BordWidth, Height - BordWidth, FCommonData.FCacheBmp.Canvas.Handle, Width - BordWidth, BordWidth, SRCCOPY);
    R := ButtonRect;
    BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCommonData.FCacheBmp.Canvas.Handle, R.Left, R.TOp, SRCCOPY);
{$IFDEF DYNAMICCACHE}
    if Assigned(FCommonData.FCacheBmp) then
      FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
  finally
    if SavedDC > 0 then begin
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end
    else
      MoveWindowOrg(DC, BordWidth, BordWidth);
  end;
end;


procedure TsDBComboBox.PaintButton;
var
  R: TRect;
  Mode: integer;
  TmpBtn: TBitmap;
  C: TColor;
begin
  if csDesigning in ComponentState then
    Mode := 0
  else
    if FDropDown then
      Mode := 2
    else
      Mode := min(integer(ControlIsActive(FCommonData) or ({AllowBtnStyle and} (FCommonData.FMouseAbove or FCommonData.FFocused))), ac_MaxPropsIndex);
{
  if FDropDown then
    Mode := 2
  else
    Mode := integer(ControlIsActive(FCommonData));
}
  R := ButtonRect;
  with SkinData.SkinManager, CommonSkinData.ComboBtn do begin
    if SkinIndex >= 0 then begin
      TmpBtn := CreateBmpLike(FCommonData.FCacheBmp);
      BitBlt(TmpBtn.Canvas.Handle, 0, 0, TmpBtn.Width, TmpBtn.Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      PaintItem(SkinIndex, MakeCacheInfo(FCommonData.FCacheBmp),
                True, Mode, R, Point(0, 0), FCommonData.FCacheBmp, FCommonData.CommonSkinData, BGIndex[0], BGIndex[1]);

      FreeAndNil(TmpBtn);
    end;
    if IsValidImgIndex(GlyphIndex) then
      DrawSkinGlyph(FCommonData.FCacheBmp, Point(R.Left + (WidthOf(R) - ma[GlyphIndex].Width) div 2,
                    (Height - ButtonHeight) div 2), Mode, 1, ma[GlyphIndex], MakeCacheInfo(SkinData.FCacheBmp))
    else begin // Paint without glyph
      if SkinIndex >= 0 then
        C := gd[SkinIndex].Props[mini(Mode, ac_MaxPropsIndex)].FontColor.Color
      else
        C := ColorToRGB(clWindowText);

      DrawArrow(FCommonData.FCacheBmp, C, clNone, R, asBottom, 0, 0, 0, Options.ActualArrowStyle, GetPPI(SkinData))
    end;
  end;
end;


procedure TsDBComboBox.PaintText;
var
  R: TRect;
begin
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  R := Rect(3, 3, Width - 3, Height - 3);
  WriteTextEx(FCommonData.FCacheBMP.Canvas, PChar(EditText), True, R, DT_NOPREFIX or DT_TOP or DT_SINGLELINE or DT_WORDBREAK or GetStringFlags(Self, Alignment),
              FCommonData, ControlIsActive(FCommonData));
end;


procedure TsDBComboBox.PrepareCache;
const
  BordWidth = 3;
var
  R, bRect: TRect;
  State: TOwnerDrawState;
begin
  InitCacheBmp(SkinData);
  if Style <> csSimple then
    PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(ControlIsActive(FCommonData)), MkRect(Width, Height), Point(Left, top), FCommonData.FCacheBmp, False)
  else begin
    FCommonData.FCacheBmp.Height := ItemHeight + 8;
    PaintItem(FCommonData, GetParentCache(FCommonData), True, integer(ControlIsActive(FCommonData)), MkRect(Width, FCommonData.FCacheBmp.Height), Point(Left, top), FCommonData.FCacheBmp, False);
  end;
  case Style of
    csDropDown, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable: begin
      bRect := ButtonRect;
      if UseRightToLeftAlignment then
        R := Rect(bRect.Right + 1, BordWidth, Width - BordWidth, FCommonData.FCacheBmp.Height - BordWidth)
      else
        R := Rect(BordWidth, BordWidth, bRect.Left - 1, FCommonData.FCacheBmp.Height - BordWidth);

      State := [odComboBoxEdit];
      if (Focused or SkinData.FFocused) and not (Style in [csDropDown, csSimple]) then
        State := State + [odFocused, odSelected];
        
      Canvas.Handle := FCommonData.FCacheBmp.Canvas.Handle;
      FCommonData.FCacheBmp.Canvas.Lock;
      DrawItem(ItemIndex, R, State);
      FCommonData.FCacheBmp.Canvas.Unlock;
      Canvas.Handle := 0;
      PaintButton;
    end;
  end;
  if not Enabled and not IsOwnerDraw(Self) then
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));

  FCommonData.BGChanged := False;
end;


procedure TsDBComboBox.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBComboBox.SetButtonMargin(const Value: integer);
begin
  if FButtonMargin <> Value then begin
    FButtonMargin := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsDBComboBox.WMPrintClient(var Message: TWMPaint);
begin
  if not SkinData.Skinned then
    inherited;
end;


procedure TsDBComboBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


var
  bFlag: boolean = False;


procedure TsDBComboBox.WMMouseWheel(var Message: TMessage);
begin
  if not FDropDown then // If not dropped down
    case Message.Msg of
      WM_MOUSEWHEEL: begin
        Message.Msg := WM_KEYDOWN;
        Message.lParam := 0;
        Message.wParam := iff(SmallInt(Message.wParamHi) > 0, VK_UP, VK_DOWN);
        MainWndProc(Message);
      end;
    end
  else
    inherited;
end;


procedure TsDBComboBox.WndProc(var Message: TMessage);
var
  DC: hdc;
  R: TRect;
  P: TPoint;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end;

      AC_REMOVESKIN: begin
        CommonWndProc(Message, FCommonData);
        Color := clWindow;
        RecreateWnd;
        Exit;
      end;

      AC_REFRESH: begin
        CommonWndProc(Message, FCommonData);
        if FCommonData.Skinned then begin
          if not FCommonData.CustomColor then
            Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Color;

          if not FCommonData.CustomFont then
            Font.Color := FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].FontColor.Color;
        end;
        Repaint;
        Exit;
      end;

      AC_SETNEWSKIN: begin
        CommonWndProc(Message, FCommonData);
        Exit;
      end;

      AC_DROPPEDDOWN:
        Message.WParamLo := integer(DroppedDown);

      AC_MOUSELEAVE:
        SendMessage(Handle, CM_MOUSELEAVE, 0, 0);

      AC_ENDPARENTUPDATE:
        if FCommonData.Updating then begin
          FCommonData.FUpdating := False;
          Repaint;
          Exit;
        end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssComboBox] + 1;

        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    case Message.Msg of
      WM_SETFOCUS, CM_ENTER:
        if CanFocus and not bFlag then begin
          bFlag := True;
          FCommonData.FFocused := True;
          FCommonData.FMouseAbove := False;
          FCommonData.BGChanged := True;
          UpdateControlColors(SkinData);
          inherited;
          bFlag := False;
          Exit;
        end;

      WM_KILLFOCUS, CM_EXIT:
        if not bFlag then begin
          bFlag := True;
          DroppedDown := False;
          FCommonData.FFocused := False;
          FCommonData.FMouseAbove := False;
          FCommonData.BGChanged := True;
          UpdateControlColors(SkinData);
          inherited;
          bFlag := False;
          Exit;
        end;

      CM_INVALIDATE, CM_FOCUSCHANGED:
        if not bFlag then begin
          inherited;
          Exit;
        end;

      WM_PRINT: begin
        SkinData.FUpdating := False;
        OurPaintHandler(TWMPaint(Message).DC);
        if (Style = csDropDown) and SkinData.FFocused then
          BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

        Exit;
      end;

      WM_NCPAINT: begin
        if InanimationProcess then
          OurPaintHandler(0);

        Exit;
      end;

      WM_PAINT: begin
        BeginPaint(Handle, PS);
        if not InAnimationProcess then begin
          if TWMPaint(Message).DC = 0 then
            DC := GetDC(Handle)
          else
            DC := TWMPaint(Message).DC;

          OurPaintHandler(DC);
          if TWMPaint(Message).DC = 0 then
            ReleaseDC(Handle, DC);
        end;
        EndPaint(Handle, PS);
        Exit;
      end;

      CM_MOUSEENTER, CM_MOUSELEAVE:
        if not (csDesigning in ComponentState) then begin
          if not DroppedDown then begin
            if Message.Msg = CM_MOUSELEAVE then begin
              P := acMousePos;
              GetWindowRect(Handle, R);
              if PtInRect(R, P) then
                Exit;
            end
            else begin
              if FCommonData.FMouseAbove then
                Exit;

              SkinData.SkinManager.ActiveControl := Handle;
            end;
            FCommonData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
            FCommonData.BGChanged := True;
            Repaint;
            inherited;
            if FCommonData.FMouseAbove then
              ShowGlowingIfNeeded(SkinData, False, Handle, 0, True)
            else
              HideGlow(SkinData.GlowID, True);
          end
          else
            inherited;

          Exit;
        end;
{$IFNDEF TNTUNICODE}
      WM_CTLCOLORLISTBOX:
        if not (csLoading in ComponentState) and (lBoxHandle = 0) then begin
          lBoxHandle := hwnd(Message.LParam);
          ListSW := TacComboListWnd.CreateEx(lboxhandle, nil, SkinData.SkinManager, s_Edit, True, Style = csSimple);
        end;
{$ENDIF}
      CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT:
        FCommonData.BGChanged := True;

      CN_COMMAND: begin
        FDropDown := False;
        Repaint;
      end;

      CM_CHANGED, CM_TEXTCHANGED: begin
        FCommonData.BGChanged := True;
        Repaint;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      CB_SETCURSEL, CM_VISIBLECHANGED, CM_ENABLEDCHANGED, WM_SETFONT:
        FCommonData.Invalidate;

      WM_SIZE, WM_WINDOWPOSCHANGED:
        if FDropDown then
          Exit; // Prevent of handling by BoundLabel
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


function TsDBComboBox.EditText: string;
var
  dLink: TFieldDataLink;
begin
  dLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  if (csPaintCopy in ControlState) and (dLink <> nil) and (dLink.Field <> nil) then begin
    Result := dLink.Field.DisplayText
  end
  else
    Result := Text;
end;


procedure TsDBComboBox.WriteText;
var
  Flags: Longint;
  R: TRect;
begin
  if EditText <> '' then begin
    Flags := 0;
    Canvas.Font.Assign(Font);
    R := ClientRect;
    dec(R.Left);
    dec(R.Top);
    dec(R.Right, ButtonRect.Left);
    case Alignment of
      taLeftJustify:  Flags := DT_LEFT;
      taRightJustify: Flags := DT_RIGHT;
      taCenter:       Flags := DT_CENTER;
    end;
    Flags := Flags or DT_EXPANDTABS or DT_VCENTER or DT_SINGLELINE;
    Flags := DrawTextBiDiModeFlags(Flags);

    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(EditText), Length(Text), R, Flags);
  end;
end;

end.
