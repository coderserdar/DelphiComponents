unit acDBCtrlGrid;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DBCtrls, DB, dbcgrids, math,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  sConst, acntUtils, sGraphUtils, sCommonData, sDefaults, acSBUtils;


type
  TsDBCtrlGrid = class;

  TacPanelPaintData = record
    Color: TColor;
  end;

  TOnGridPaintPanel = procedure(DBCtrlGrid: TsDBCtrlGrid; Index: Integer; var PaintData: TacPanelPaintData) of object;

  TsDBCtrlPanel = class(TDBCtrlPanel)
  private
    FDisabledKind: TsDisabledKind;
    DrawIndex: integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure acPaintControls(DC: HDC; First: TControl);
    function GetDBCtrlGrid: TsDBCtrlGrid;
  protected
    function Selected: boolean;
    procedure PaintWindow(DC: HDC); override;
    procedure WndProc(var Message: TMessage); override;
    procedure PrepareCache(pR: TPoint; Selected: boolean);
  public
    FCommonData: TsCommonData;
    constructor CreateLinked2(DBCtrlGrid: TDBCtrlGrid);
    destructor Destroy; override;
    procedure Loaded; override;
    property DBCtrlGrid: TsDBCtrlGrid read GetDBCtrlGrid;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBCtrlGrid = class(TDBCtrlGrid)
  private
    BoundsChanging,
    FAutoMouseWheel: boolean;

    SaveActive,
    FBitmapCount: Integer;

    FSaveBitmap,
    FPanelBitmap: HBitmap;

    FPanelSkin,
    FSelectionSkin: TsSkinSection;

    FPanelDC: HDC;
    FCommonData: TsScrollWndData;
    FOnGridPaintPanel: TOnGridPaintPanel;
    procedure PrepareCache;
    procedure CreatePanelBitmap;
    procedure DestroyPanelBitmap;
    function GetDataLink: TDataLink;
    function GetDisabledKind: TsDisabledKind;
    procedure DrawPanel(DC: HDC; Index: Integer);
    function GetPanelBounds(Index: Integer): TRect;
    procedure SetPanelSkin(const Value: TsSkinSection);
    procedure SetDisabledKind(const Value: TsDisabledKind);
    procedure DrawPanelBackground(DC: HDC; const R: TRect; Erase, Selected: Boolean);
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure PaintPanel(Index: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  public
    ListSW: TacScrollWnd;
    ScrollsUpdating: boolean;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure AdjustSize; reintroduce;
    procedure AfterConstruction; override;
    property DataLink: TDataLink read GetDataLink;
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Panel;
  published
    property AutoMouseWheel: boolean read FAutoMouseWheel write FAutoMouseWheel default False;
    property DisabledKind:  TsDisabledKind read GetDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData:      TsScrollWndData read FCommonData write FCommonData;
    property PanelSkin:     TsSkinSection read FPanelSkin     write SetPanelSkin;
    property SelectionSkin: TsSkinSection read FSelectionSkin write FSelectionSkin;
    property OnGridPaintPanel: TOnGridPaintPanel read FOnGridPaintPanel write FOnGridPaintPanel;
  end;


implementation

uses sMessages, sStyleSimply, sSkinProps, sVCLUtils, sAlphaGraph{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};


type
  TDBCtrlGrid_ = class(TWinControl)
  private
    FDataLink: TDBCtrlGridLink;
    FPanel: TDBCtrlPanel;
  end;

  TDBCtrlPanel_ = class(TWinControl)
  private
    FDBCtrlGrid: TDBCtrlGrid;
  end;


constructor TsDBCtrlPanel.CreateLinked2(DBCtrlGrid: TDBCtrlGrid);
begin
  inherited CreateLinked(DBCtrlGrid);
  DrawIndex := -1;
  ParentColor := False;
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsPanel;
  FDisabledKind := DefDisabledKind;
end;


destructor TsDBCtrlPanel.Destroy;
begin
  FreeAndNil(FCommonData);
  inherited;
end;


function TsDBCtrlPanel.GetDBCtrlGrid: TsDBCtrlGrid;
begin
  Result := TDBCtrlPanel_(Self).FDBCtrlGrid as TsDBCtrlGrid;
end;


procedure TsDBCtrlPanel.Loaded;
begin
  inherited Loaded;
  if FCommonData.SkinSection = '' then
    FCommonData.SkinSection := TsDBCtrlGrid(DBCtrlGrid).PanelSkin;

  try
    FCommonData.Loaded;
  except
    Application.HandleException(Self);
  end;
end;


procedure TsDBCtrlPanel.acPaintControls(DC: HDC; First: TControl);

  procedure CopyControl(Ctrl: TWinControl);
  var
    I, Count, SaveIndex: Integer;
    cCtrl: TControl;
  begin
    I := 0;
    Count := Ctrl.ControlCount;
    while I < Count do begin
      cCtrl := Ctrl.Controls[I];
      if (cCtrl.Visible or (csDesigning in cCtrl.ComponentState) and not (csNoDesignVisible in cCtrl.ControlStyle)) and RectVisible(DC, cCtrl.BoundsRect {Rect(Left, Top, Left + Width, Top + Height)}) then begin
        if csPaintCopy in Ctrl.ControlState then
          cCtrl.ControlState := cCtrl.ControlState + [csPaintCopy];

        SaveIndex := SaveDC(DC);
        MoveWindowOrg(DC, cCtrl.Left, cCtrl.Top);
        IntersectClipRect(DC, 0, 0, cCtrl.Width, cCtrl.Height);

        if cCtrl is TWinControl then begin
          cCtrl.ControlState := cCtrl.ControlState + [csPaintCopy];
          cCtrl.Perform(WM_PAINT, WParam(DC), 0);
          cCtrl.Perform(WM_PRINT, WParam(DC), 0);
          cCtrl.ControlState := cCtrl.ControlState - [csPaintCopy];
          CopyControl(TWinControl(cCtrl));
        end
        else
          cCtrl.Perform(WM_PAINT, WParam(DC), 0);

        RestoreDC(DC, SaveIndex);
        cCtrl.ControlState := cCtrl.ControlState - [csPaintCopy];
      end;
      inc(I);
    end;
  end;

begin
  if DockSite and UseDockManager and (DockManager <> nil) then
    DockManager.PaintSite(DC);

  CopyControl(Self);
end;


procedure TsDBCtrlPanel.PaintWindow(DC: HDC);
var
  R: TRect;
  Ndx, sIndex: integer;
  PPaintData: TacPanelPaintData;
begin
  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    if DrawIndex < 0 then
      Ndx := DBCtrlGrid.PanelIndex
    else
      Ndx := DrawIndex;

    R := DBCtrlGrid.GetPanelBounds(Ndx);

    FCommonData.FCacheBmp.Canvas.Font := Font;
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    FCommonData.FCacheBmp.Canvas.Brush.Color := Color;

    PPaintData.Color := clNone;
    if Assigned(DBCtrlGrid.OnGridPaintPanel) then
      DBCtrlGrid.OnGridPaintPanel(DBCtrlGrid, Ndx, PPaintData);

    if PPaintData.Color <> clNone then begin
      InitCacheBmp(FCommonData);
      FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Width, Height), PPaintData.Color);
      if FCommonData.Skinned then begin
        sIndex := FCommonData.SkinManager.GetSkinIndex(DBCtrlGrid.PanelSkin);
        if FCommonData.SkinManager.gd[sIndex].BorderIndex >= 0 then
          DrawSkinRect(FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), EmptyCI, FCommonData.SkinManager.ma[FCommonData.SkinManager.gd[sIndex].BorderIndex], 0, True);
      end;
    end
    else
      PrepareCache(Point(R.Left, R.Top), Selected);

    if FCommonData.Skinned then
      UpdateCorners(FCommonData, 0);

    BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

    if DBCtrlGrid.ShowFocus and DBCtrlGrid.Focused and Selected then
      DrawFocusRect(DC, Rect(2, 2, Width - 2, Height - 2));

    SetParentUpdated(Self);
  end;
end;


procedure TsDBCtrlPanel.PrepareCache(pR: TPoint; Selected: boolean);
begin
  DBCtrlGrid.PrepareCache;
  InitCacheBmp(FCommonData);
  if Selected then
    if DBCtrlGrid.SelectionSkin <> '' then
      PaintItem(FCommonData.SkinManager.GetSkinIndex(DBCtrlGrid.SelectionSkin), GetParentCache(FCommonData), True, integer(DBCtrlGrid.Focused), MkRect(Self), pR, FCommonData.FCacheBmp, FCommonData.CommonSkinData)
    else
      FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), acColorToRGB(DBCtrlGrid.SelectedColor))
  else
    if '' <> DBCtrlGrid.PanelSkin then
      PaintItem(FCommonData.SkinManager.GetSkinIndex(DBCtrlGrid.PanelSkin), GetParentCache(FCommonData), False, 0, MkRect(Self), pR, FCommonData.FCacheBmp, FCommonData.CommonSkinData)
    else
      BitBlt(FCommonData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, DBCtrlGrid.SkinData.FCacheBmp.Canvas.Handle, pR.X, pR.Y, SRCCOPY);

  FCommonData.BGChanged := False;
end;


function TsDBCtrlPanel.Selected: boolean;
begin
  with DBCtrlGrid do
    if DataLink.Active then
      Result := DBCtrlGrid.SaveActive = DataLink.ActiveRecord
    else
      Result := False;
end;


procedure TsDBCtrlPanel.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  i: integer;
  PS: TPaintStruct;
{$IFDEF DELPHI7UP}
  OldDC, SavedDC: hdc;
{$ENDIF}
begin
  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else
    if not (csDestroying in ComponentState) then
      if Message.DC = 0 then begin
        for i := 0 to ControlCount - 1 do
          Controls[i].ControlState := Controls[i].ControlState + [csPaintCopy];

        DBCtrlGrid.CreatePanelBitmap;
        try
          Message.DC := DBCtrlGrid.FPanelDC;
          PaintHandler(Message);
          Message.DC := 0;
          DC := BeginPaint(Handle, PS);
          BitBlt(DC, 0, 0, Width, Height, DBCtrlGrid.FPanelDC, 0, 0, SRCCOPY);
{$IFDEF DELPHI7UP}
          if Assigned(DBCtrlGrid.OnPaintPanel) then begin
            if DBCtrlGrid.Canvas.HandleAllocated then
              OldDC := DBCtrlGrid.Canvas.Handle
            else
              OldDC := 0;

            DBCtrlGrid.Canvas.Handle := DC;
            SavedDC := SaveDC(DC);

            DBCtrlGrid.OnPaintPanel(DBCtrlGrid, DBCtrlGrid.PanelIndex);
            RestoreDC(DC, SavedDC);
            DBCtrlGrid.Canvas.Handle := OldDC;
          end;
{$ENDIF}
          EndPaint(Handle, PS);
        finally
          DBCtrlGrid.DestroyPanelBitmap;
        end;
        for i := 0 to ControlCount - 1 do
          Controls[i].ControlState := Controls[i].ControlState - [csPaintCopy];
      end
      else
        PaintHandler(Message);
end;


procedure TsDBCtrlPanel.WndProc(var Message: TMessage);
var
  b: boolean;
begin
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins supported

      AC_REMOVESKIN:
        if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          AlphaBroadCast(Self,Message);
          Exit;
        end;

      AC_SETNEWSKIN:
        if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          AlphaBroadCast(Self,Message);
          Exit;
        end;

      AC_REFRESH:
        if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          AlphaBroadCast(Self,Message);
          if FCommonData.Skinned then
            Perform(WM_NCPAINT, 0, 0);

          Exit;
        end;

      AC_ENDPARENTUPDATE:
        if FCommonData.Updating then begin
          FCommonData.Updating := False;
          Perform(WM_NCPAINT, 0, 0);
        end;

      AC_GETBG: begin
        if DBCtrlGrid.FCommonData.BGChanged then
          DBCtrlGrid.PrepareCache;

        with PacBGInfo(Message.LParam)^ do begin
          Offset := MkPoint;
          Bmp := FCommonData.FCacheBmp;
          BgType := btCache;
        end;
        Exit;
      end;

      AC_GETFONTINDEX:
        if FCommonData.Skinned then begin
          b := Selected and (DBCtrlGrid.SelectionSkin <> '');
          if b then begin
            PacPaintInfo(Message.LParam)^.FontIndex := FCommonData.SkinManager.GetSkinIndex(DBCtrlGrid.SelectionSkin);
            PacPaintInfo(Message.LParam)^.State := integer(DBCtrlGrid.Focused);
          end
          else
            if NeedParentFont(FCommonData.SkinManager, FCommonData.SkinIndex, integer(b), Self) then begin
              inc(PacPaintInfo(Message.LParam)^.R.Left, Left);
              inc(PacPaintInfo(Message.LParam)^.R.Top,  Top);
              Message.Result := GetFontIndex(Parent, PacPaintInfo(Message.LParam))
            end
            else
              PacPaintInfo(Message.LParam)^.FontIndex := FCommonData.SkinIndex;

          Message.Result := 1;
          Exit;
        end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    case Message.Msg of
      WM_PRINT:
        if ControlIsReady(Self) and FCommonData.Skinned then begin
          Perform(WM_PAINT, Message.WParam, Message.LParam);
          SendMessage(Handle, WM_NCPAINT, Message.WParam, Message.LParam);
          Exit;
        end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
  end;
end;


constructor TsDBCtrlGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsScrollBox;
  FAutoMouseWheel := False;
  BoundsChanging := False;
  FreeAndNil(TDBCtrlGrid_(Self).FPanel);
  TDBCtrlGrid_(Self).FPanel := TsDBCtrlPanel.CreateLinked2(Self);
  AdjustSize;
end;


procedure TsDBCtrlGrid.Loaded;
begin
  inherited Loaded;
  if FPanelSkin = '' then
    FPanelSkin := s_GroupBox;

  try
    FCommonData.Loaded;
    TsDBCtrlPanel(Panel).Loaded;
  except
    Application.HandleException(Self);
  end;
  RefreshScrolls(FCommonData, ListSW);
end;


destructor TsDBCtrlGrid.Destroy;
begin
  FreeAndNil(ListSW);
  FreeAndNil(FCommonData);
  inherited;
end;


procedure TsDBCtrlGrid.DrawPanel(DC: HDC; Index: Integer);
var
  R: TRect;
{$IFDEF DELPHI7UP}
  OldDC, SavedDC: hdc;
{$ENDIF}
begin
  R := GetPanelBounds(Index);
  if (Index < PanelCount) then begin
//    if Index <> PanelIndex then begin
      SaveActive := DataLink.ActiveRecord;
      DataLink.ActiveRecord := Index;
      TsDBCtrlPanel(Panel).DrawIndex := Index;
      Panel.ControlState := Panel.ControlState + [csPaintCopy];
      TsDBCtrlPanel(Panel).PaintWindow(FPanelDC);
      TsDBCtrlPanel(Panel).acPaintControls(FPanelDC, nil);
      Panel.ControlState := Panel.ControlState - [csPaintCopy];
{$IFDEF DELPHI7UP}
      if Assigned(OnPaintPanel) then begin
        if Canvas.HandleAllocated then
          OldDC := Canvas.Handle
        else
          OldDC := 0;

        Canvas.Handle := FPanelDC;
        SavedDC := SaveDC(FPanelDC);

        OnPaintPanel(Self, Index);
        RestoreDC(FPanelDC, SavedDC);
        Canvas.Handle := OldDC;
      end;
{$ENDIF}
      DataLink.ActiveRecord := SaveActive;
      BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, FPanelDC, 0, 0, SRCCOPY);
//    end;
  end
  else begin
    if SkinData.Skinned then
      PaintItem(SkinData.SkinManager.SkinCommonInfo.Sections[ssTransparent], GetParentCache(FCommonData), False, 0,
        Panel.ClientRect, Point(Left + R.Left, Top + R.Top), TsDBCtrlPanel(Panel).FCommonData.FCacheBmp, SkinData.SkinManager)//, True)
    else
      PaintItem(TsDBCtrlPanel(Panel).FCommonData, GetParentCache(FCommonData), False, 0, Panel.ClientRect, R.TopLeft, TsDBCtrlPanel(Panel).FCommonData.FCacheBmp, True);

    BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, TsDBCtrlPanel(Panel).FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;


procedure TsDBCtrlGrid.DrawPanelBackground(DC: HDC; const R: TRect; Erase, Selected: Boolean);
begin
  TsDBCtrlPanel(Panel).PrepareCache(R.TopLeft, Selected);
  PaintItem(TsDBCtrlPanel(Panel).FCommonData, GetParentCache(FCommonData), False, 0, Panel.ClientRect, R.TopLeft, TsDBCtrlPanel(Panel).FCommonData.FCacheBmp, True);
  BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, TsDBCtrlPanel(Panel).FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


function TsDBCtrlGrid.GetPanelBounds(Index: Integer): TRect;
var
  Col, Row: Integer;
begin
  if Orientation = goVertical then begin
    Col := Index mod ColCount;
    Row := Index div ColCount;
  end
  else begin
    Col := Index div RowCount;
    Row := Index mod RowCount;
  end;
  Result.Left := PanelWidth * Col;
  Result.Top := PanelHeight * Row;
  Result.Right := Result.Left + PanelWidth;
  Result.Bottom := Result.Top + PanelHeight;
end;


procedure TsDBCtrlGrid.PaintPanel(Index: Integer);
var
  R, pR: TRect;
  PPaintData: TacPanelPaintData;
begin
  R := GetPanelBounds(Index);
  pR := MkRect(WidthOf(R), HeightOf(R));
  PPaintData.Color := clNone;
  if Assigned(FOnGridPaintPanel) then
    FOnGridPaintPanel(Self, Index, PPaintData);

  if PPaintData.Color <> clNone then begin
    FillDC(Canvas.Handle, pR, PPaintData.Color);
    FillDCBorder(Canvas.Handle, pR, 1, 1, 0, 0, clBtnHighlight);
    FillDCBorder(Canvas.Handle, pR, 0, 0, 1, 1, clBtnShadow);
  end;

  inherited;
end;


procedure TsDBCtrlGrid.PaintWindow(DC: HDC);
var
  I: Integer;
  Brush: HBrush;
  M : TMessage;
begin
  if (Panel is TsDBCtrlPanel) and TsDBCtrlPanel(Panel).FCommonData.Skinned then begin
    if csDesigning in ComponentState then begin
      Panel.Update;
      Brush := CreateHatchBrush(HS_BDIAGONAL, ColorToRGB(clBtnShadow));
      SetBkColor(DC, ColorToRGB(Color));
      FillRect(DC, ClientRect, Brush);
      DeleteObject(Brush);
      for I := 1 to ColCount * RowCount - 1 do
        DrawPanelBackground(DC, GetPanelBounds(I), False, DataLink.Active and (I = DataLink.ActiveRecord));
    end
    else begin
      CreatePanelBitmap;
      try
        for I := 0 to ColCount * RowCount - 1 do begin
          m := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
          Panel.Broadcast(m);
          DrawPanel(DC, I);
        end;
      finally
        TsDBCtrlPanel(Panel).DrawIndex := -1;
        DestroyPanelBitmap;
      end;
    end;
    { When width or height are not evenly divisible by panel size, fill the gaps }
    if HandleAllocated then begin
      if Height <> Panel.Height * RowCount then
        BitBlt(DC, 0, Panel.Height * RowCount, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

      if Width <> Panel.Width * ColCount then
        BitBlt(DC, Panel.Width * ColCount, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end
  else
    inherited;
end;


procedure TsDBCtrlGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ScrollWidth, ScrollHeight, NewPanelWidth, NewPanelHeight: Integer;
begin
  if not BoundsChanging then begin
    BoundsChanging := True;
    inherited;
    if (SkinData <> nil) and SkinData.Skinned and ((SkinData.VertScrollData.ScrollWidth >= 0) or (SkinData.SkinManager.ScrollsOptions.ScrollSize >= 0)) then begin
      ScrollWidth := 0;
      ScrollHeight := 0;
      if ListSW <> nil then
        if Orientation = goVertical then
          ScrollWidth := GetScrollMetric(ListSW.sbarVert, SM_SCROLLWIDTH)
        else
          ScrollHeight := GetScrollMetric(ListSW.sbarHorz, SM_SCROLLWIDTH);

      NewPanelWidth := (AWidth - ScrollWidth) div ColCount;
      NewPanelHeight := (AHeight - ScrollHeight) div RowCount;
      if NewPanelWidth < 1 then
        NewPanelWidth := 1;

      if NewPanelHeight < 1 then
        NewPanelHeight := 1;

      if (PanelWidth <> NewPanelWidth) or (PanelHeight <> NewPanelHeight) then begin
        PanelWidth := NewPanelWidth;
        PanelHeight := NewPanelHeight;
  //      Reset;
  {
        if csDesigning in ComponentState then
          DataLink.BufferCount := 1
        else
          DataLink.BufferCount := ColCount * RowCount;}
  //      DataSetChanged(True);
      end;
    end;
    BoundsChanging := False;
  end;
end;


procedure TsDBCtrlGrid.SetDisabledKind(const Value: TsDisabledKind);
begin
  with TsDBCtrlPanel(Panel) do
    if FDisabledKind <> Value then begin
      FDisabledKind := Value;
      FCommonData.Invalidate;
    end;
end;


function TsDBCtrlGrid.GetDisabledKind: TsDisabledKind;
begin
  with TsDBCtrlPanel(Panel) do
    Result := FDisabledKind;
end;


procedure TsDBCtrlGrid.PrepareCache;
begin
  InitCacheBmp(FCommonData);
  PaintItem(FCommonData, GetParentCache(FCommonData), False, 0, MkRect(Self), MkPoint(Self), FCommonData.FCacheBmp, False);
  FCommonData.BGChanged := False;
end;


//var
//  bacWheelFlag: boolean = False;

procedure TsDBCtrlGrid.WndProc(var Message: TMessage);
var
  VertSize, HorzSize: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_REMOVESKIN:
          if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
            FreeAndNil(ListSW);
            CommonMessage(Message, FCommonData);
            AlphaBroadCast(Self,Message);
            RecreateWnd;
            Exit;
          end;

        AC_SETNEWSKIN:
          if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            AlphaBroadCast(Self,Message);
            Exit;
          end;

        AC_REFRESH:
          if LongWord(Message.LParam) = LongWord(FCommonData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            RefreshScrolls(FCommonData, ListSW);
            AlphaBroadCast(Self,Message);
            RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
            Exit;
          end;

        AC_ENDPARENTUPDATE:
          if InUpdating(FCommonData) then begin
            FCommonData.Updating := False;
            Perform(WM_NCPAINT, 0, 0);
          end;

        AC_GETBG: begin
          if (SkinData.BGChanged or SkinData.HalfVisible) and not SkinData.Updating then
            PrepareCache;

          with PacBGInfo(Message.LParam)^ do begin
            Offset := Point(0, 0);
            Bmp := FCommonData.FCacheBmp;
            BgType := btCache;
          end;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := 1 + FCommonData.SkinManager.SkinCommonInfo.Sections[ssTransparent];

          Exit;
        end;

        AC_GETFONTINDEX:
          if FCommonData.Skinned then begin
            inc(PacPaintInfo(Message.LParam)^.R.Left, Left);
            inc(PacPaintInfo(Message.LParam)^.R.Top,  Top);
            Message.Result := GetFontIndex(Parent, PacPaintInfo(Message.LParam));
            Exit;
          end;
      end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    case Message.Msg of
      WM_PRINT: begin
        Ac_NCDraw(ListSW, Handle, -1, hdc(Message.WParam));
        VertSize := integer(ListSW.sBarVert.fScrollVisible) * GetScrollMetric(ListSW.sBarVert, SM_SCROLLWIDTH);
        HorzSize := integer(ListSW.sBarHorz.fScrollVisible) * GetScrollMetric(ListSW.sBarHorz, SM_SCROLLWIDTH);

        if IsRightToLeft and ListSW.sBarVert.fScrollVisible then
          MoveWindowOrg(hdc(Message.WParam), VertSize, 0);

        IntersectClipRect(hdc(Message.WParam), 0, 0, Width - VertSize, Height - HorzSize);
        Perform(WM_PAINT, Message.WParam, Message.LParam);
        Exit;
      end;
    end;
    CommonWndProc(Message, FCommonData);
    inherited;
    case Message.Msg of
      WM_KILLFOCUS, WM_SETFOCUS:
        RedrawWindow(TDBCtrlGrid_(Self).FPanel.Handle, nil, 0, RDWA_ALLNOW);

      WM_SETFONT, CM_ENABLEDCHANGED, CM_VISIBLECHANGED:
        FCommonData.Invalidate;

      WM_SIZE, WM_MOVE:
        UpdateScrolls(ListSW, True);

      WM_PASTE, WM_CUT, WM_CLEAR, WM_UNDO, WM_SETTEXT:
        UpdateScrolls(ListSW, True);

      WM_HSCROLL, WM_VSCROLL:
        UpdateScrolls(ListSW, True);

      CM_CANCELMODE, CM_CHANGED, CN_KEYDOWN, CN_KEYUP:
        UpdateScrolls(ListSW, True);

      WM_PARENTNOTIFY:
        if Message.WParam and $FFFF in [WM_CREATE, WM_DESTROY] then
          RefreshScrolls(FCommonData, ListSW);

      CM_CONTROLLISTCHANGE, CM_CONTROLCHANGE:
        if not SkinData.Updating then
          UpdateScrolls(ListSW, True);

      CM_INVALIDATE:
        UpdateScrolls(ListSW, True);

      WM_MOUSEWHEEL:
        if AutoMouseWheel and (DataLink <> nil) and (DataLink.DataSet <> nil) then
          if TCMMouseWheel(Message).WheelDelta < 0 then
            GetDataLink.DataSet.Next
          else
            GetDataLink.DataSet.Prior;
    end;
  end;
end;


function TsDBCtrlGrid.GetDataLink: TDataLink;
begin
  Result := TDBCtrlGrid_(Self).FDataLink;
end;


procedure TsDBCtrlGrid.CreatePanelBitmap;
var
  DC: HDC;
begin
  if FBitmapCount = 0 then begin
    DC := GetDC(0);
    FPanelBitmap := CreateCompatibleBitmap(DC, Panel.Width, Panel.Height);
    ReleaseDC(0, DC);
    FPanelDC := CreateCompatibleDC(0);
    FSaveBitmap := SelectObject(FPanelDC, FPanelBitmap);
  end;
  Inc(FBitmapCount);
end;


procedure TsDBCtrlGrid.DestroyPanelBitmap;
begin
  Dec(FBitmapCount);
  if FBitmapCount = 0 then begin
    SelectObject(FPanelDC, FSaveBitmap);
    DeleteDC(FPanelDC);
    DeleteObject(FPanelBitmap);
  end;
end;


procedure TsDBCtrlGrid.AdjustSize;
var
  W, H: Integer;
begin
  if ListSW <> nil then begin
    W := PanelWidth * ColCount;
    H := PanelHeight * RowCount;
    if Orientation = goVertical then
      Inc(W, GetScrollMetric(ListSW.sBarVert, SM_SCROLLWIDTH))//GetSystemMetrics(SM_CXVSCROLL))
    else
      Inc(H, GetScrollMetric(ListSW.sBarHorz, SM_SCROLLWIDTH));//GetSystemMetrics(SM_CYHSCROLL));
  end
  else begin
    W := Width;
    H := Height;
  end;

  SetBounds(Left, Top, W, H);
end;


procedure TsDBCtrlGrid.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
  if HandleAllocated then
    RefreshScrolls(FCommonData, ListSW);
end;


procedure TsDBCtrlGrid.SetPanelSkin(const Value: TsSkinSection);
begin
  if FPanelSkin <> Value then begin
    FPanelSkin := Value;
    TsDBCtrlPanel(TDBCtrlGrid_(Self).FPanel).FCommonData.SkinSection := Value;
    FCommonData.BGChanged := True;
    RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
  end;
end;


function acGetDBFieldCheckState(AObj: TObject): TCheckBoxState;
begin
  if AObj is TFieldDataLink then begin
    with TFieldDataLink(AObj) do begin
      Result := cbGrayed;
      if not Field.IsNull then
        if Field.DataType = ftBoolean then
          Result := CheckBoxStates[integer(Field.AsBoolean)]
        else
          Result := cbGrayed
    end;
  end
  else
    Result := cbGrayed;
end;


initialization
  GetDBFieldCheckState := acGetDBFieldCheckState;

end.
