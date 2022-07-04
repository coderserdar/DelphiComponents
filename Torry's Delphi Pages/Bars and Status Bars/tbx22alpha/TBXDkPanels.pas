unit TBXDkPanels;

// TBX Package
// Copyright 2001-2004 Alex A. Denisov. All Rights Reserved
// See TBX.chm for license and installation instructions
//
// $Id: TBXDkPanels.pas 85 2005-09-21 06:30:57Z Alex $

interface

{$I TB2Ver.inc}
{$I TBX.inc}

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  TB2Dock, TB2Item, TBX, TBXThemes, ImgList, Menus;

type
  { TTBXMultiDock }

  TTBXMultiDock = class(TTBDock)
  private
    FMoving: Boolean;
    FResizing: Boolean;
    FUpdatingColor: Boolean;
    FUseParentBackground: Boolean;
    FUseThemeColor: Boolean;
    function  IsColorStored: Boolean;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure SetUseThemeColor(Value: Boolean);
    procedure TBMGetViewType(var Message: TMessage); message TBM_GETVIEWTYPE;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    LastValidRowSize: Integer;
    function  Accepts(ADockableWindow: TTBCustomDockableWindow): Boolean; override;
    procedure ValidateInsert(AComponent: TComponent); override;
    function  ThemedBackground: Boolean; virtual;
    procedure DrawBackground(DC: HDC; const DrawRect: TRect); override;
    procedure Resize; override;
    procedure SetUseParentBackground(Value: Boolean);
    procedure UpdateColor;
    function  UsingBackground: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ResizeVisiblePanels(NewSize: Integer);
    procedure ArrangeToolbars; override;
  published
    property Color stored IsColorStored;
    property UseParentBackground: Boolean read FUseParentBackground write SetUseParentBackground default False;
    property UseThemeColor: Boolean read FUseThemeColor write SetUseThemeColor default True;
  end;


  { TTBXDockablePanel }

  TDPCaptionRotation = (dpcrAuto, dpcrAlwaysHorz, dpcrAlwaysVert);
  TTBXResizingStage = (rsBeginResizing, rsResizing, rsEndResizing);
  TTBXDockedResizing = procedure(Sender: TObject; Vertical: Boolean;
    var NewSize: Integer; Stage: TTBXResizingStage; var AllowResize: Boolean) of object;
  TDockKinds = set of (dkStandardDock, dkMultiDock);

  TTBXDockablePanel = class(TTBCustomDockableWindow)
  private
    FBorderSize: Integer;
    FCaptionRotation: TDPCaptionRotation;
    FDockedWidth: Integer;
    FDockedHeight: Integer;
    FFloatingWidth: Integer;
    FFloatingHeight: Integer;
    FIsResizing: Boolean;
    FIsSplitting: Boolean;
    FMinClientWidth: Integer;
    FMinClientHeight: Integer;
    FMaxClientWidth: Integer;
    FMaxClientHeight: Integer;
    FSmoothDockedResize: Boolean;
    FSnapDistance: Integer;
    FShowCaptionWhenDocked: Boolean;
    FSplitHeight: Integer;
    FSplitWidth: Integer;
    FSupportedDocks: TDockKinds;
    FUpdatingColor: Boolean;
    FOnDockedResizing: TTBXDockedResizing;
    FUseThemeColor: Boolean;
    function  CalcSize(ADock: TTBDock): TPoint;
    function  IsColorStored: Boolean;
    procedure SetBorderSize(Value: Integer);
    procedure SetCaptionRotation(Value: TDPCaptionRotation);
    procedure SetDockedHeight(Value: Integer);
    procedure SetDockedWidth(Value: Integer);
    procedure SetFloatingHeight(Value: Integer);
    procedure SetFloatingWidth(Value: Integer);
    procedure SetMinClientHeight(Value: Integer);
    procedure SetMinClientWidth(Value: Integer);
    procedure SetShowCaptionWhenDocked(Value: Boolean);
    procedure SetSnapDistance(Value: Integer);
    procedure SetUseThemeColor(Value: Boolean);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure TBMGetViewType(var Message: TMessage); message TBM_GETVIEWTYPE;
    procedure TBMThemeChange(var Message: TMessage); message TBM_THEMECHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure SetSplitHeight(Value: Integer);
    procedure SetSplitWidth(Value: Integer);
  protected
    BlockSizeUpdate: Boolean;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure BeginDockedSizing(HitTest: Integer);
    procedure BeginSplitResizing(HitTest: Integer);
    function  CalcNCSizes: TPoint; override;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function  CanDockTo(ADock: TTBDock): Boolean; override;
    function  CanSplitResize(EdgePosition: TTBDockPosition): Boolean;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    function  DoArrange(CanMoveControls: Boolean; PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint; override;
    function  DoBeginDockedResizing(Vertical: Boolean): Boolean; virtual;
    function  DoDockedResizing(Vertical: Boolean; var NewSize: Integer): Boolean; virtual;
    function  DoEndDockedResizing(Vertical: Boolean): Boolean; virtual;
    procedure DrawNCArea(const DrawToDC: Boolean; const ADC: HDC; const Clip: HRGN); override;
    procedure GetBaseSize(var ASize: TPoint); override;
    function  GetDockedCloseButtonRect(LeftRight: Boolean): TRect; override;
    procedure GetDockPanelInfo(out DockPanelInfo: TTBXDockPanelInfo); virtual;
    function  GetFloatingWindowParentClass: TTBFloatingWindowParentClass; override;
    procedure GetMinMaxSize(var AMinClientWidth, AMinClientHeight, AMaxClientWidth, AMaxClientHeight: Integer); override;
    function  GetViewType: Integer;
    function  IsVertCaption: Boolean; virtual;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SizeChanging(const AWidth, AHeight: Integer); override;
    procedure UpdateColor;
    property ParentColor default False;
    property IsResizing: Boolean read FIsResizing;
    property IsSplitting: Boolean read FIsSplitting;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetFloatingBorderSize: TPoint; override;
    procedure ReadPositionData(const Data: TTBReadPositionData); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure WritePositionData(const Data: TTBWritePositionData); override;
  published
    { client size constraints should be restored before other size related properties }
    property MaxClientHeight: Integer read FMaxClientHeight write FMaxClientHeight default 0;
    property MaxClientWidth: Integer read FMaxClientWidth write FMaxClientWidth default 0;
    property MinClientHeight: Integer read FMinClientHeight write SetMinClientHeight default 32;
    property MinClientWidth: Integer read FMinClientWidth write SetMinClientWidth default 32;

    property ActivateParent;
    property Align;
    property Anchors;
    property BorderSize: Integer read FBorderSize write SetBorderSize default 0;
    property BorderStyle;
    property Caption;
    property CaptionRotation: TDPCaptionRotation read FCaptionRotation write SetCaptionRotation default dpcrAuto;
    property Color stored IsColorStored;
    property CloseButton;
    property CloseButtonWhenDocked default True;
    property CurrentDock;
    property DblClickUndock default False;
    property DefaultDock;
    property DockableTo;
    property DockedWidth: Integer read FDockedWidth write SetDockedWidth default 128;
    property DockedHeight: Integer read FDockedHeight write SetDockedHeight default 128;
    property DockMode;
    property DockPos;
    property DockRow;
    property FloatingWidth: Integer read FFloatingWidth write SetFloatingWidth default 0;
    property FloatingHeight: Integer read FFloatingHeight write SetFloatingHeight default 0;
    property FloatingMode;
    property Font;
    property Height stored False;
    property HideWhenInactive;
    property LastDock;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Resizable;
    property ShowCaption;
    property ShowCaptionWhenDocked: Boolean read FShowCaptionWhenDocked write SetShowCaptionWhenDocked default True;
    property ShowHint;
    property SplitHeight: Integer read FSplitHeight write SetSplitHeight default 0;
    property SplitWidth: Integer read FSplitWidth write SetSplitWidth default 0;
    property SupportedDocks: TDockKinds read FSupportedDocks write FSupportedDocks default [dkStandardDock, dkMultiDock];
    property SmoothDrag;
    property SmoothDockedResize: Boolean read FSmoothDockedResize write FSmoothDockedResize default True;
    property SnapDistance: Integer read FSnapDistance write SetSnapDistance default 0;
    property TabOrder;
    property UseLastDock;
    property UseThemeColor: Boolean read FUseThemeColor write SetUseThemeColor default True;
    property Visible;
    property Width stored False;

    property OnClose;
    property OnCloseQuery;
    {$IFDEF JR_D5}
    property OnContextPopup;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnDockChanged;
    property OnDockChanging;
    property OnDockChangingHidden;
    property OnDockedResizing: TTBXDockedResizing read FOnDockedResizing write FOnDockedResizing;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMove;
    property OnRecreated;
    property OnRecreating;
    property OnResize;
    property OnVisibleChanged;
  end;

implementation

uses
  TB2Common, TBXUtils, TBXControls, SysUtils;

type
  TWinControlAccess = class(TWinControl);
  TDockAccess = class(TTBXMultiDock);
  TTBDockableWindowAccess = class(TTBCustomDockableWindow);

const
  { Constants for TTBXDockablePanel-specific registry values. Do not localize! }
  rvDockedWidth = 'DPDockedWidth';
  rvDockedHeight = 'DPDockedHeight';
  rvFloatingWidth = 'DPFloatingWidth';
  rvFloatingHeight = 'DPFloatingHeight';
  rvSplitWidth = 'DPSplitWidth';
  rvSplitHeight = 'DPSplitHeight';

  HT_TB2k_Border = 2000;
  HT_TB2k_Close = 2001;
  HT_TB2k_Caption = 2002;
  HT_TBX_SPLITRESIZELEFT = 86;
  HT_TBX_SPLITRESIZERIGHT = 87;
  HT_TBX_SPLITRESIZETOP = 88;
  HT_TBX_SPLITRESIZEBOTTOM = 89;

  DockedBorderSize = 2;
  
procedure UpdateNCArea(Control: TWinControl; ViewType: Integer);
var
  W, H: Integer;
begin
  with Control do
  begin
    { Keep the client rect at the same position relative to screen }
    W := ClientWidth;
    H := ClientHeight;
    SetWindowPos(Handle, 0, 0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOREDRAW or SWP_NOMOVE or SWP_NOSIZE);
    W := W - ClientWidth;
    H := H - ClientHeight;
    if (W <> 0) or (H <> 0) then
      SetWindowPos(Handle, 0, Left - W div 2, Top - H div 2, Width + W, Height + H,
       SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOZORDER);
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
      RDW_ERASE or RDW_UPDATENOW or RDW_ALLCHILDREN);
  end;
end;

function CompareEffectiveDockPos(const Item1, Item2, ExtraData: Pointer): Integer; far;
begin
  Result := TTBCustomDockableWindow(Item1).EffectiveDockPos - TTBCustomDockableWindow(Item2).EffectiveDockPos;
end;

function CompareDockPos(const Item1, Item2, ExtraData: Pointer): Integer; far;
var
  P1, P2: Integer;
begin
  P1 := TTBCustomDockableWindow(Item1).DockPos;
  P2 := TTBCustomDockableWindow(Item2).DockPos;
  if csLoading in TTBCustomDockableWindow(Item1).ComponentState then
  begin
    if P1 < 0 then P1 := MaxInt;
    if P2 < 0 then P2 := MaxInt;
  end;
  Result := P1 - P2;
end;


//----------------------------------------------------------------------------//

{ TTBXMultiDock }

function TTBXMultiDock.Accepts(ADockableWindow: TTBCustomDockableWindow): Boolean;
begin
  Result := ADockableWindow is TTBXDockablePanel;
end;

procedure TTBXMultiDock.ArrangeToolbars;
const
  DSGN_DROPZONESIZE = 16;
type
  TPosRec = record
    Panel: TTBXDockablePanel;
    MinSize, MaxSize, Size, Pos: Integer;
    CanStretch: Boolean;
  end;
var
  NewDockList: TList;
  PosData: array of TPosRec;
  LeftRight: Boolean;
  I, J, K, L, DragIndex, ResizeIndex, ForcedWidth: Integer;
  EmptySize, ClientW, ClientH, DockSize, TotalSize, TotalMinimumSize, TotalMaximumSize: Integer;
  DragIndexPos: Integer;
  T: TTBXDockablePanel;
  S: TPoint;
  CurRowPixel, CurRowSize: Integer;
  StretchPanelCount: Integer;
  Stretching: Boolean;
  AccDelta, Acc: Extended;
  Delta, IntAcc: Integer;
  MinWidth, MaxWidth, EffectiveMinWidth, EffectiveMaxWidth: Integer;
  R: TRect;

  function IndexOfDraggingToolbar(const List: TList): Integer;
  { Returns index of toolbar in List that's currently being dragged, or -1 }
  var
    I: Integer;
  begin
    for I := 0 to List.Count - 1 do
      if TTBCustomDockableWindow(List[I]).DragMode then
      begin
        Result := I;
        Exit;
      end;
    Result := -1;
  end;

  procedure GetSizes(Panel: TTBXDockablePanel; out Size, MinSize, MaxSize: Integer);
  var
    Sz: TPoint;
    MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
  begin
    Panel.GetBaseSize(Sz);
    if LeftRight then
    begin
      Size := Panel.SplitHeight;
    end
    else
    begin
      Size := Panel.SplitWidth;
    end;
    MinWidth := 0; MaxWidth := 0; MinHeight := 0; MaxHeight := 0;
    Panel.ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
    if not LeftRight then begin MinSize := MinWidth; MaxSize := MaxWidth end
    else begin MinSize := MinHeight; MaxSize := MaxHeight end;
    if MaxSize < MinSize then
    begin
      MaxSize := DockSize;
      if MaxSize < MinSize then MaxSize := MinSize;
    end;
    if Size < MinSize then Size := MinSize
    else if Size > MaxSize then Size := MaxSize;
  end;

  procedure GetMinMaxWidth(Panel: TTBXDockablePanel; out Min, Max: Integer);
  var
    MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
  begin
    MinWidth := 0; MaxWidth := 0; MinHeight := 0; MaxHeight := 0;
    Panel.ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
    if LeftRight then begin Min := MinWidth; Max := MaxWidth end
    else begin Min := MinHeight; Max := MaxHeight end;
  end;

begin
  if (DisableArrangeToolbars > 0) or (csLoading in ComponentState) then
  begin
    ArrangeToolbarsNeeded := True;
    Exit;
  end;
  NewDockList := nil;
  PosData := nil;
  DisableArrangeToolbars := DisableArrangeToolbars + 1;
  try
    LeftRight := Position in [dpLeft, dpRight];

    if not HasVisibleToolbars then
    begin
      EmptySize := Ord(FixAlign);
      if csDesigning in ComponentState then EmptySize := 7;
      if not LeftRight then ChangeWidthHeight(Width, EmptySize)
      else ChangeWidthHeight(EmptySize, Height);
      Exit;
    end;

    ClientW := Width - NonClientWidth;
    if ClientW < 0 then ClientW := 0;
    ClientH := Height - NonClientHeight;
    if ClientH < 0 then ClientH := 0;
    if not LeftRight then DockSize := ClientW
    else DockSize := ClientH;

    { Leave some space for dropping other panels in design time }
    if csDesigning in ComponentState then Dec(DockSize, DSGN_DROPZONESIZE);
    if DockSize < 0 then DockSize := 0;


    for I := DockList.Count - 1 downto 0 do
    begin
      T := DockList[I];
      if csDestroying in T.ComponentState then
      begin
        DockList.Delete(I);
        DockVisibleList.Remove(T);
      end;
    end;

    { always limit to one row }
    for I := 0 to DockList.Count - 1 do
      with TTBCustomDockableWindow(DockList[I]) do DockRow := 0;

    { Copy DockList to NewDockList, and ensure it is in correct ordering
      according to DockRow/DockPos }
    NewDockList := TList.Create;
    NewDockList.Count := DockList.Count;
    for I := 0 to NewDockList.Count - 1 do NewDockList[I] := DockList[I];
    I := IndexOfDraggingToolbar(NewDockList);
    ListSortEx(NewDockList, CompareDockPos, nil);
    DragIndex := IndexOfDraggingToolbar(NewDockList);
    if (I <> -1) and TTBCustomDockableWindow(NewDockList[DragIndex]).DragSplitting then 
    begin
      { When splitting, don't allow the toolbar being dragged to change
        positions in the dock list }
      NewDockList.Move(DragIndex, I);
      DragIndex := I;
    end;
    ListSortEx(DockVisibleList, CompareDockPos, nil);

    { Create a temporary array that holds new position data for the toolbars
      and get size info }
    SetLength(PosData, 0);
    for I := 0 to NewDockList.Count - 1 do
    begin
      T := NewDockList[I];
      if ToolbarVisibleOnDock(T) then
      begin
        SetLength(PosData, Length(PosData) + 1);
        with PosData[Length(PosData) - 1] do
        begin
          Panel := T as TTBXDockablePanel;
          Pos := Panel.DockPos;
          GetSizes(Panel, Size, MinSize, MaxSize{, OrigWidth});
        end;
      end;
    end;

    { Update drag index... }
    if DragIndex >= 0 then
      for I := 0 to Length(PosData) - 1 do
        if NewDockList.IndexOf(PosData[I].Panel) = DragIndex then
        begin
          DragIndex := I;
          Break;
        end;

    { Count total sizes and set initial positions }
    DragIndexPos := 0;
    TotalSize := 0; TotalMinimumSize := 0; TotalMaximumSize := 0;
    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do
      begin
        if I = DragIndex then DragIndexPos := Panel.DockPos;
        Pos := TotalSize;
        Inc(TotalSize, Size);
        Inc(TotalMinimumSize, MinSize);
        Inc(TotalMaximumSize, MaxSize);
      end;

    if DockSize <> TotalSize then
    begin
      begin
        { Proportionally stretch and shrink toolbars }

        if TotalMinimumSize >= DockSize then
          for I := 0 to Length(PosData) - 1 do PosData[I].Size := PosData[I].MinSize
        else if TotalMaximumSize <= DockSize then
          for I := 0 to Length(PosData) - 1 do PosData[I].Size := PosData[I].MaxSize
        else
        begin
          Delta := DockSize - TotalSize;
          StretchPanelCount := 0;
          Stretching := TotalSize < DockSize; // otherwise, shrinking

          for I := 0 to Length(PosData) - 1 do
            with PosData[I] do
            begin
              if Stretching then CanStretch := Size < MaxSize
              else CanStretch := Size > MinSize;
              if CanStretch then Inc(StretchPanelCount);
            end;
          Assert(StretchPanelCount > 0);

          while Delta <> 0 do
          begin
            Assert(StretchPanelCount <> 0);
            AccDelta := Delta / StretchPanelCount;
            Acc := 0; IntAcc := 0;
            for I := 0 to Length(PosData) - 1 do
              with PosData[I] do if CanStretch then
              begin
                Acc := Acc + AccDelta;
                Inc(Size, Round(Acc) - IntAcc);
                IntAcc := Round(Acc);
              end;

            TotalSize := 0;
            for I := 0 to Length(PosData) - 1 do
              with PosData[I] do
              begin
                if CanStretch then
                  if Stretching then
                  begin
                    if Size > MaxSize then
                    begin
                      Size := MaxSize;
                      CanStretch := False;
                      Dec(StretchPanelCount);
                    end;
                  end
                  else
                  begin
                    if Size < MinSize then
                    begin
                      Size := MinSize;
                      CanStretch := False;
                      Dec(StretchPanelCount);
                    end;
                  end;
                Inc(TotalSize, Size);
              end;
            Delta := DockSize - TotalSize;
          end;
        end;

        TotalSize := 0;
        for I := 0 to Length(PosData) - 1 do
          with PosData[I] do
          begin
            Pos := TotalSize;
            Inc(TotalSize, Size);
          end;
      end
    end;

    for I := 0 to NewDockList.Count - 1 do
    begin
      for J := 0 to Length(PosData) - 1 do
        with PosData[J] do
        begin
          if Panel = NewDockList[I] then
          begin
            Panel.EffectiveDockRowAccess := 0;
            Panel.EffectiveDockPosAccess := PosData[J].Pos;
          end;
        end;
      if CommitNewPositions then
      begin
        T := NewDockList[I];
        T.DockRow := T.EffectiveDockRow;
        T.DockPos := T.EffectiveDockPos;
        DockList[I] := NewDockList[I];
      end;
    end;

    ResizeIndex := -1;
    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do
        if Panel is TTBXDockablePanel and Panel.IsResizing then
        begin
          ResizeIndex := I;
          Break;
        end;

    { Calculate the size of the dock }
    if ResizeIndex < 0 then
    begin
      CurRowSize := 0;
      for I := 0 to Length(PosData) - 1 do
        with PosData[I] do
        begin
          Panel.CurrentSize := Size;
          Panel.GetBaseSize(S);
          if LeftRight then K := S.X + Panel.CalcNCSizes.X else K := S.Y + Panel.CalcNCSizes.Y;
          if (DragIndex = I) and (Length(PosData) > 1) and (LastValidRowSize > 0) then K := 0;
          if K > CurRowSize then CurRowSize := K;
        end;
    end
    else
    begin
      EffectiveMinWidth := 0;
      EffectiveMaxWidth := 0;
      for I := 0 to Length(PosData) - 1 do
      begin
        GetMinMaxWidth(PosData[I].Panel, MinWidth, MaxWidth);
        if MinWidth > EffectiveMinWidth then EffectiveMinWidth := MinWidth;
        if (MaxWidth >= MinWidth) and (MaxWidth < EffectiveMaxWidth) then EffectiveMaxWidth := MaxWidth;
      end;
      if LeftRight then CurRowSize := PosData[ResizeIndex].Panel.Width
      else CurRowSize := PosData[ResizeIndex].Panel.Height;
      if (EffectiveMaxWidth > EffectiveMinWidth) and (CurRowSize > EffectiveMaxWidth) then CurRowSize := EffectiveMaxWidth;
      if CurRowSize < EffectiveMinWidth then CurRowSize := EffectiveMinWidth;
    end;
    if CurRowSize > 0 then LastValidRowSize := CurRowSize;

    { Now actually move the toolbars }
    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do
      begin
        if LeftRight then R := Bounds(0, Pos, CurRowSize, Size)
        else R := Bounds(Pos, 0, Size, CurRowSize);
        Panel.BoundsRect := R;
        { This is to fix some weird behavior in design time }
        if csDesigning in ComponentState then
          with R do MoveWindow(Panel.Handle, Left, Top, Right - Left, Bottom - Top, True);
      end;

    { Set the size of the dock }
    if not LeftRight then ChangeWidthHeight(Width, CurRowSize + NonClientHeight)
    else ChangeWidthHeight(CurRowSize + NonClientWidth, Height);

  finally
    DisableArrangeToolbars := DisableArrangeToolbars - 1;
    ArrangeToolbarsNeeded := False;
    CommitNewPositions := False;
    SetLength(PosData, 0);
    NewDockList.Free;
  end;
end;

procedure TTBXMultiDock.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not FUpdatingColor then FUseThemeColor := False;
end;

constructor TTBXMultiDock.Create(AOwner: TComponent);
begin
  inherited;
  AddThemeNotification(Self);
  ParentColor := False;
  FUseThemeColor := True;
  UpdateColor;
end;

destructor TTBXMultiDock.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

procedure TTBXMultiDock.DrawBackground(DC: HDC; const DrawRect: TRect);
const
  DOCK_POSITIONS: array [TTBDockPosition] of Integer = (DP_TOP, DP_BOTTOM, DP_LEFT, DP_RIGHT);
begin
  if UseParentBackground then DrawParentBackground(Self, DC, ClientRect)
  else if ThemedBackground then
  begin
    CurrentTheme.PaintDock(DC, ClientRect, DrawRect, DOCK_POSITIONS[Position]);
  end
  else inherited;
end;

function TTBXMultiDock.IsColorStored: Boolean;
begin
  Result := not (ParentColor or UseThemeColor);
end;

procedure TTBXMultiDock.Paint;
var
  R: TRect;
begin
  { Draw dotted border in design mode }
  if csDesigning in ComponentState then
  begin
    R := ClientRect;
    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Color := clBtnHighlight;
      Brush.Color := clBtnHighlight;
      Brush.Style := bsFDiagonal;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      Pen.Style := psSolid;
    end;
  end;
end;

procedure TTBXMultiDock.Resize;
var
  I, J: Integer;
  V: TTBItemViewer;
  R: TRect;
begin
  inherited Resize;
  if UsingBackground then
  begin
    for J := 0 to ToolbarCount - 1 do
    begin
      Invalidate;
      if Toolbars[J] is TTBXToolbar then with TTBXToolbar(Toolbars[J]) do
      begin
        for I := 0 to View.ViewerCount - 1 do
        begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem)
          then View.Invalidate(V);
        end;
        Update;
        if HandleAllocated then
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
        for I := 0 to View.ViewerCount - 1 do
        begin
          V := View.Viewers[I];
          if V.Show and not IsRectEmpty(V.BoundsRect) and not (V.Item is TTBControlItem)
          then
          begin
            R := V.BoundsRect;
            ValidateRect(Handle, @R);
          end;
        end;
      end
      else if Toolbars[J] is TTBXToolWindow then with TTBXToolWindow(Toolbars[J]) do
      begin
        if HandleAllocated then
          RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
      end;
    end;
  end;
end;

procedure TTBXMultiDock.ResizeVisiblePanels(NewSize: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to DockVisibleList.Count - 1 do
      if Position in [dpLeft, dpRight] then
        TTBXDockablePanel(DockVisibleList[I]).DockedWidth := NewSize
      else
        TTBXDockablePanel(DockVisibleList[I]).DockedHeight := NewSize;
  finally
    EndUpdate;
  end;
end;

procedure TTBXMultiDock.SetUseParentBackground(Value: Boolean);
begin
  if Value <> FUseParentBackground then
  begin
    FUseParentBackground := Value;
    if HandleAllocated then
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
        RDW_ERASE or RDW_ALLCHILDREN);
  end;
end;

procedure TTBXMultiDock.SetUseThemeColor(Value: Boolean);
begin
  if FUseThemeColor <> Value then
  begin
    FUseThemeColor := Value;
    UpdateColor;
  end;
end;

procedure TTBXMultiDock.TBMGetViewType(var Message: TMessage);
begin
  Message.Result := VT_MULTIDOCK;
end;

procedure TTBXMultiDock.TBMThemeChange(var Message: TMessage);
begin
  if Message.WParam = TSC_AFTERVIEWCHANGE then Invalidate;
end;

function TTBXMultiDock.ThemedBackground: Boolean;
begin
  Result := (Background = nil) and UseThemeColor and
    CurrentTheme.GetBooleanMetrics(TMB_PAINTDOCKBACKGROUND);
end;

procedure TTBXMultiDock.UpdateColor;
var
  C: TColor;
begin
  if UseThemeColor then
  begin
    C := CurrentTheme.GetViewColor(VT_MULTIDOCK);
    if C <> Color then
    try
      FUpdatingColor := True;
      Color := C;
    finally
      FUpdatingColor := False;
    end;
  end;
end;

function TTBXMultiDock.UsingBackground: Boolean;
begin
  Result := UseParentBackground or (ThemedBackground and not FMoving) or inherited UsingBackground;
end;

procedure TTBXMultiDock.ValidateInsert(AComponent: TComponent);
begin
  if not (AComponent is TTBXDockablePanel) then
    raise EInvalidOperation.CreateFmt('Cannot insert %s into TTBXMultiDock', [AComponent.ClassName]);
end;

procedure TTBXMultiDock.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if UsingBackground then
  begin
    DrawBackground(Message.DC, ClientRect);
    Message.Result := 1;
  end
  else
  begin
    FillRectEx(Message.DC, ClientRect, Color);
    Message.Result := 1;
  end;
end;

procedure TTBXMultiDock.WMMove(var Message: TWMMove);
begin
  FMoving := True;
  inherited;
  FMoving := False;
end;

procedure TTBXMultiDock.WMSize(var Message: TWMSize);
begin
  FResizing := True;
  inherited;
  FResizing := False;
end;

//----------------------------------------------------------------------------//

{ TTBXDockablePanel }

procedure TTBXDockablePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderSize <> 0 then InflateRect(Rect, -BorderSize, -BorderSize);
end;

procedure TTBXDockablePanel.BeginDockedSizing(HitTest: Integer);
var
  OrigPos, OldPos: TPoint;
  Msg: TMsg;
  DockRect, DragRect, OrigDragRect, OldDragRect: TRect;
  NCSizes: TPoint;
  EdgeRect, OldEdgeRect: TRect;
  ScreenDC: HDC;
  EraseEdgeRect, CommitResizing: Boolean;
  Form: TCustomForm;
  LeftRight: Boolean;

  function RectToScreen(const R: TRect): TRect;
  begin
    Result := R;
    Result.TopLeft := Parent.ClientToScreen(Result.TopLeft);
    Result.BottomRight := Parent.ClientToScreen(Result.BottomRight);
  end;

  function RectToClient(const R: TRect): TRect;
  begin
    Result := R;
    Result.TopLeft := Parent.ScreenToClient(Result.TopLeft);
    Result.BottomRight := Parent.ScreenToClient(Result.BottomRight);
  end;

  function GetEdgeRect(const R: TRect): TRect;
  begin
    Result := DockRect;
    case HitTest of
      HTLEFT: begin Result.Left := R.Left - 1; Result.Right := R.Left + 1 end;
      HTRIGHT: begin Result.Left := R.Right - 1; Result.Right := R.Right + 1 end;
      HTTOP: begin Result.Top := R.Top - 1; Result.Bottom := R.Top + 1 end;
      HTBOTTOM: begin Result.Top := R.Bottom - 1; Result.Bottom := R.Bottom + 1 end;
    end;
  end;

  procedure MouseMoved;
  var
    Pos: TPoint;
    NewWidth: Integer;
    NewHeight: Integer;
  begin
    GetCursorPos(Pos);
    if (Pos.X = OldPos.X) and (Pos.Y = OldPos.Y) then Exit;
    DragRect := OrigDragRect;
    case HitTest of
      HTLEFT:
        begin
          NewWidth := DragRect.Right - (DragRect.Left + Pos.X - OrigPos.X  - 1);
          if DoDockedResizing(False, NewWidth) then
            DragRect.Left := DragRect.Right - NewWidth;
        end;
      HTRIGHT:
        begin
          NewWidth := (DragRect.Right + Pos.X - OrigPos.X) - DragRect.Left;
          if DoDockedResizing(False, NewWidth) then
            DragRect.Right := DragRect.Left + NewWidth;
        end;
      HTTOP:
        begin
          NewHeight := DragRect.Bottom - (DragRect.Top + Pos.Y - OrigPos.Y - 1);
          if DoDockedResizing(True, NewHeight) then
            DragRect.Top := DragRect.Bottom - NewHeight;
        end;
      HTBOTTOM:
        begin
          NewHeight := (DragRect.Bottom + Pos.Y - OrigPos.Y) - DragRect.Top;
          if DoDockedResizing(True, NewHeight) then
            DragRect.Bottom := DragRect.Top + NewHeight;
        end;
    end;               
    if not CompareMem(@OldDragRect, @DragRect, SizeOf(TRect)) then
    begin
      if SmoothDockedResize then
      begin
        CurrentDock.BeginUpdate;
        if HitTest in [HTLEFT, HTRIGHT] then
        begin
          BlockSizeUpdate := True;
          DockedWidth := DragRect.Right - DragRect.Left - NCSizes.X;
        end
        else
        begin
          BlockSizeUpdate := True;
          DockedHeight := DragRect.Bottom - DragRect.Top - NCSizes.Y;
        end;
        BlockSizeUpdate := False;
        CurrentDock.EndUpdate;
      end
      else
      begin
        EdgeRect := GetEdgeRect(DragRect);
        DrawDraggingOutline(ScreenDC, EdgeRect, OldEdgeRect);
        OldEdgeRect := EdgeRect;
        EraseEdgeRect := True;
      end;
      OldPos := Pos;
      OldDragRect := DragRect;
    end;
  end;

begin
  LeftRight := HitTest in [HTLEFT, HTRIGHT];
  if DoBeginDockedResizing(HitTest in [HTTOP, HTBOTTOM]) then
  try
    SetCapture(Handle);
    ScreenDC := GetDC(0);
    OrigDragRect := RectToScreen(BoundsRect);
    DockRect := RectToScreen(CurrentDock.ClientRect);
    OldDragRect := Rect(0, 0, 0, 0);
    NCSizes := CalcNCSizes;
    DragRect := OrigDragRect;
    GetCursorPos(OrigPos);
    OldPos := OrigPos;
    FIsResizing := True;

    if not SmoothDockedResize then
    begin
      EdgeRect := GetEdgeRect(DragRect);
      DrawDraggingOutline(ScreenDC, EdgeRect, Rect(0, 0, 0, 0));
      OldEdgeRect := EdgeRect;
      EraseEdgeRect := True;
    end
    else EraseEdgeRect := False;

    while GetCapture = Handle do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP: if Msg.WParam = VK_ESCAPE then Break;
        WM_MOUSEMOVE: MouseMoved;
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:Break;
        WM_LBUTTONUP: Break;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = Handle then ReleaseCapture;
    CommitResizing := DoEndDockedResizing(HitTest in [HTTOP, HTBOTTOM]);
    if EraseEdgeRect then
    begin
      DrawDraggingOutline(ScreenDC, Rect(0, 0, 0, 0), OldEdgeRect);
      if CommitResizing and not IsRectEmpty(OldDragRect) then
        with OldDragRect do
        begin
          BlockSizeUpdate := True;
          if LeftRight then DockedWidth := Right - Left - NCSizes.X
          else DockedHeight := Bottom - Top - NCSizes.Y;
          BlockSizeUpdate := False;
        end;
    end
    else if not CommitResizing then
    begin
      BlockSizeUpdate := True;
      BoundsRect := RectToClient(OrigDragRect);
      BlockSizeUpdate := False;
    end;
    ReleaseDC(0, ScreenDC);
    FIsResizing := False;
    if csDesigning in ComponentState then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
    end;
  end;
end;

procedure TTBXDockablePanel.BeginSplitResizing(HitTest: Integer);
type
  TPosRec = record
    Panel: TTBXDockablePanel;
    OrigPos, OrigSize, OrigWidth, Pos, Size, MinSize, MaxSize: Integer;
  end;
var
  Dock: TDockAccess;
  PosData: array of TPosRec;
  I: Integer;
  LeftRight, Smooth, CommitResizing: Boolean;
  DockSize, TotalSize, TotalMinSize, TotalMaxSize: Integer;
  OrigCursorPos, OldCursorPos: TPoint;
  Msg: TMsg;
  EffectiveIndex: Integer;
  EffectivePanel: TTBXDockablePanel;
  PanelRect, DockRect, EdgeRect, OrigEdgeRect, OldEdgeRect: TRect;
  EdgePosition: TTBDockPosition;
  ScreenDC: HDC;
  EraseEdgeRect: Boolean;
  Form: TCustomForm;
  Delta: Integer;

  procedure GetSizes(Panel: TTBXDockablePanel; out Size, MinSize, MaxSize, W: Integer);
  var
    Sz: TPoint;
    MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
  begin
    Panel.GetBaseSize(Sz);
    if not LeftRight then
    begin
      Size := Panel.Width;
      W := Panel.Height;
    end
    else
    begin
      Size := Panel.Height;
      W := Panel.Width;
    end;
    MinWidth := 0; MaxWidth := 0; MinHeight := 0; MaxHeight := 0;
    Panel.ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
    if not LeftRight then begin MinSize := MinWidth; MaxSize := MaxWidth; end
    else begin MinSize := MinHeight; MaxSize := MaxHeight end;
    if MaxSize < MinSize then
    begin
      MaxSize := DockSize;
      if MaxSize < MinSize then MaxSize := MinSize;
    end;
    if Size < MinSize then Size := MinSize
    else if Size > MaxSize then Size := MaxSize;
  end;

  procedure BlockSizeUpdates(DoBlock: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to Length(PosData) - 1 do
      with PosData[I].Panel do BlockSizeUpdate := DoBlock;
  end;

  procedure SetSizes(RestoreOriginal: Boolean = False);
  var
    I: Integer;
    R: TRect;
  begin
    Dock.BeginUpdate;
    BlockSizeUpdates(True);
    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do
      begin
        if LeftRight then
        begin
          if RestoreOriginal then R := Bounds(0, OrigPos, OrigWidth, OrigSize)
          else R := Bounds(0, Pos, OrigWidth, Size);
        end
        else
        begin
          if RestoreOriginal then R := Bounds(OrigPos, 0, OrigSize, OrigWidth)
          else R := Bounds(Pos, 0, Size, OrigWidth);
        end;
        if LeftRight then Panel.SplitHeight := Size
        else Panel.SplitWidth := Size;
        Panel.BoundsRect := R;

        { This is to fix some weird behavior in design time }
        if csDesigning in ComponentState then
          with R do MoveWindow(Panel.Handle, Left, Top, Right - Left, Bottom - Top, True);
      end;
    BlockSizeUpdates(False);
    Dock.EndUpdate;
  end;

  function GetEdgeRect(R: TRect): TRect;
  begin
    Result := R;
    case EdgePosition of
      dpRight: begin Result.Left := Result.Right - 1; Inc(Result.Right); end;
      dpBottom: begin Result.Top := Result.Bottom - 1; Inc(Result.Bottom); end;
    end;
  end;

  function RectToScreen(const R: TRect): TRect;
  begin
    Result := R;
    Result.TopLeft := Parent.ClientToScreen(Result.TopLeft);
    Result.BottomRight := Parent.ClientToScreen(Result.BottomRight);
  end;

  function RectToClient(const R: TRect): TRect;
  begin
    Result := R;
    Result.TopLeft := Parent.ScreenToClient(Result.TopLeft);
    Result.BottomRight := Parent.ScreenToClient(Result.BottomRight);
  end;

  procedure MouseMoved;
  var
    CursorPos: TPoint;
    I, P, Acc: Integer;
  begin
    GetCursorPos(CursorPos);
    if (CursorPos.X = OldCursorPos.X) and (CursorPos.Y = OldCursorPos.Y) then Exit;
    case EdgePosition of
      dpRight: Delta := CursorPos.X - OrigCursorPos.X;
      dpBottom: Delta := CursorPos.Y - OrigCursorPos.Y;
    end;
    if Delta = 0 then Exit;

    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do
      begin
        Pos := OrigPos;
        Size := OrigSize;
      end;

    Acc := Delta;
    for I := EffectiveIndex downto 0 do
      with PosData[I] do
      begin
        Inc(Size, Acc); Acc := 0;
        if Size > MaxSize then
        begin
          Acc := Size - MaxSize;
          Size := MaxSize;
        end
        else if Size < MinSize then
        begin
          Acc := Size - MinSize;
          Size := MinSize;
        end;
        if Acc = 0 then Break;
      end;

    if Acc <> 0 then Dec(Delta, Acc);

    Acc := Delta;
    for I := EffectiveIndex + 1 to Length(PosData) - 1 do
      with PosData[I] do
      begin
        Dec(Size, Acc); Acc := 0;
        if Size > MaxSize then
        begin
          Acc := MaxSize - Size;
          Size := MaxSize;
        end
        else if Size < MinSize then
        begin
          Acc := MinSize - Size;
          Size := MinSize;
        end;
        if Acc = 0 then Break;
      end;

    if Acc <> 0 then
    begin
      Dec(Delta, Acc);
      for I := 0 to EffectiveIndex do with PosData[I] do Size := OrigSize;
      Acc := Delta;
      for I := EffectiveIndex downto 0 do
        with PosData[I] do
        begin
          Inc(Size, Acc); Acc := 0;
          if Size > MaxSize then
          begin
            Acc := Size - MaxSize;
            Size := MaxSize;
          end
          else if Size < MinSize then
          begin
            Acc := Size - MinSize;
            Size := MinSize;
          end;
          if Acc = 0 then Break;
        end;
    end;

    P := 0;
    for I := 0 to Length(PosData) - 1 do
      with PosData[I] do begin Pos := P; Inc(P, Size); end;

    if Smooth then SetSizes
    else
    begin
      EdgeRect := DockRect;
      if LeftRight then
      begin
        Inc(EdgeRect.Top, PosData[EffectiveIndex + 1].Pos - 1);
        EdgeRect.Bottom := EdgeRect.Top + 2;
      end
      else
      begin
        Inc(EdgeRect.Left, PosData[EffectiveIndex + 1].Pos - 1);
        EdgeRect.Right := EdgeRect.Left + 2;
      end;
      DrawDraggingOutline(ScreenDC, EdgeRect, OldEdgeRect);
      EraseEdgeRect := True;
    end;

    OldCursorPos := CursorPos;
    OldEdgeRect := EdgeRect;
  end;

begin
  if not (CurrentDock is TTBXMultiDock) then Exit;
  Dock := TDockAccess(CurrentDock);

  SetLength(PosData, Dock.DockVisibleList.Count);
  for I := 0 to Dock.DockVisibleList.Count - 1 do
    with PosData[I] do
    begin
      { only docks with TTBXDockablePanels can be resized }
      if not (TTBCustomDockableWindow(Dock.DockVisibleList[I]) is TTBXDockablePanel) then Exit;
      Panel := TTBXDockablePanel(Dock.DockVisibleList[I]);
    end;

  LeftRight := Dock.Position in [dpLeft, dpRight];
  if not LeftRight then DockSize := Dock.Width - Dock.NonClientWidth
  else DockSize := Dock.Height - Dock.NonClientHeight;
  if DockSize < 0 then DockSize := 0;

  { See if we can actually resize anything }
  TotalSize := 0; TotalMinSize := 0; TotalMaxSize := 0;
  for I := 0 to Length(PosData) - 1 do
    with PosData[I] do
    begin
      GetSizes(Panel, Size, MinSize, MaxSize, OrigWidth);
      OrigSize := Size;
      OrigPos := TotalSize;
      Pos := OrigPos;
      Inc(TotalSize, Size);
      Inc(TotalMinSize, MinSize);
      Inc(TotalMaxSize, MaxSize);
    end;
  if (TotalMinSize > DockSize) or (TotalMaxSize < DockSize) then Exit;

  { Get effective edge and panel }
  case HitTest of
    HT_TBX_SPLITRESIZETOP: EdgePosition := dpTop;
    HT_TBX_SPLITRESIZEBOTTOM: EdgePosition := dpBottom;
    HT_TBX_SPLITRESIZELEFT: EdgePosition := dpLeft;
  else
    EdgePosition := dpRight;
  end;
  Smooth := True;
  EffectivePanel := Self;
  for I := 0 to Length(PosData) - 1 do
    with PosData[I] do
    begin
      if not Panel.SmoothDockedResize then Smooth := False;
      if Panel = Self then
      begin
        EffectiveIndex := I;
        if EdgePosition in [dpLeft, dpTop] then
        begin
          Assert(I > 0);
          EffectivePanel := PosData[I - 1].Panel;
          if EdgePosition = dpLeft then EdgePosition := dpRight
          else EdgePosition := dpBottom;
          Dec(EffectiveIndex);
        end;
      end;
    end;

  try
    SetCapture(Handle);
    ScreenDC := GetDC(0);
    with EffectivePanel do PanelRect := RectToScreen(BoundsRect);
    DockRect := RectToScreen(Dock.ClientRect);
    GetCursorPos(OrigCursorPos);
    OldCursorPos := OrigCursorPos;
    OrigEdgeRect := GetEdgeRect(PanelRect);
    OldEdgeRect := Rect(0, 0, 0, 0);
    EdgeRect := OrigEdgeRect;
    FIsSplitting := True;

    if not Smooth then
    begin
      DrawDraggingOutline(ScreenDC, EdgeRect, Rect(0, 0, 0, 0));
      OldEdgeRect := EdgeRect;
      EraseEdgeRect := True;
    end
    else EraseEdgeRect := False;

    while GetCapture = Handle do
    begin
      case Integer(GetMessage(Msg, 0, 0, 0)) of
        -1: Break;
        0: begin
             PostQuitMessage(Msg.WParam);
             Break;
           end;
      end;
      case Msg.Message of
        WM_KEYDOWN, WM_KEYUP: if Msg.WParam = VK_ESCAPE then Break;
        WM_MOUSEMOVE: MouseMoved;
        WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:Break;
        WM_LBUTTONUP: Break;
        WM_RBUTTONDOWN..WM_MBUTTONDBLCLK:;
      else
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  finally
    if GetCapture = Handle then ReleaseCapture;
    CommitResizing := True;
    if EraseEdgeRect then
    begin
      DrawDraggingOutline(ScreenDC, Rect(0, 0, 0, 0), OldEdgeRect);
      if CommitResizing then SetSizes;
    end
    else if not CommitResizing then SetSizes(True);
    ReleaseDC(0, ScreenDC);
    FIsSplitting := False;
    if csDesigning in ComponentState then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
    end;
  end;
end;

function TTBXDockablePanel.CalcNCSizes: TPoint;
begin
  if not Docked then
  begin
    Result.X := 0;
    Result.Y := 0;
  end
  else
  begin
    Result.X := DockedBorderSize * 2;
    Result.Y := DockedBorderSize * 2;
    if ShowCaptionWhenDocked then
      if IsVertCaption then Inc(Result.X, GetSystemMetrics(SM_CYSMCAPTION))
      else Inc(Result.Y, GetSystemMetrics(SM_CYSMCAPTION));
  end;
end;

function TTBXDockablePanel.CalcSize(ADock: TTBDock): TPoint;
begin
  if Assigned(ADock) then
  begin
    if ADock.Position in [dpLeft, dpRight] then
    begin
      Result.X := FDockedWidth;
      Result.Y := ADock.ClientHeight - CalcNCSizes.Y;
    end
    else
    begin
      Result.X := ADock.ClientWidth - CalcNCSizes.X;
      Result.Y := FDockedHeight;
    end;
  end
  else
  begin
    { if floating width and height are yet undefined, copy them from docked width and height }
    if FFloatingWidth = 0 then
    begin
      if Docked and (CurrentDock.Position in [dpTop, dpBottom]) then
        FFloatingWidth := Width  {CurrentDock.ClientWidth} - CalcNCSizes.X
      else
        FFloatingWidth := FDockedWidth;
    end;

    if FFloatingHeight = 0 then
    begin
      if Docked and (CurrentDock.Position in [dpLeft, dpRight]) then
        FFloatingHeight := {CurrentDock.ClientHeight} Height - CalcNCSizes.Y
      else
        FFloatingHeight := FDockedHeight;
    end;

    Result.X := FFloatingWidth;
    Result.Y := FFloatingHeight;
  end;
end;

function TTBXDockablePanel.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
end;

function TTBXDockablePanel.CanDockTo(ADock: TTBDock): Boolean;
begin
  Result := inherited CanDockTo(ADock);
  if Result then
  begin
    if ADock is TTBXMultiDock then
    begin
      Result := dkMultiDock in SupportedDocks;
    end
    else
    begin
      Result := dkStandardDock in SupportedDocks;;
    end;
  end;
end;

function TTBXDockablePanel.CanSplitResize(EdgePosition: TTBDockPosition): Boolean;
var
  Dock: TDockAccess;
begin
  Result := Docked and (CurrentDock is TTBXMultiDock) and HandleAllocated;
  if not Result then Exit;
  Dock := TDockAccess(CurrentDock);
  ListSortEx(Dock.DockVisibleList, CompareEffectiveDockPos, nil);
  if Dock.Position in [dpLeft, dpRight] then
  begin
    case EdgePosition of
      dpTop: Result := EffectiveDockPos > 0;
      dpBottom: Result := Dock.DockVisibleList.Last <> Self;
    else
      Result := False;
    end;
  end
  else
  begin
    case EdgePosition of
      dpLeft: Result := EffectiveDockPos > 0;
      dpRight: Result := Dock.DockVisibleList.Last <> Self;
    else
      Result := False;
    end;
  end;
end;

procedure TTBXDockablePanel.CMColorChanged(var Message: TMessage);
begin
  if not FUpdatingColor then FUseThemeColor := False;
  if Docked and HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or
      RDW_ERASE {or RDW_UPDATENOW} or RDW_ALLCHILDREN);
end;

procedure TTBXDockablePanel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    if Docked then RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE)
    else RedrawWindow(TTBXFloatingWindowParent(Parent).Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE);
  end;
end;

procedure TTBXDockablePanel.ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
var
  Sz: TPoint;
begin
  Sz := CalcNCSizes;
  if MinClientWidth > 0 then MinWidth := MinClientWidth + Sz.X;
  if MinClientHeight > 0 then MinHeight := MinClientHeight + Sz.Y;
  if MaxClientWidth > 0 then MaxWidth := MaxClientWidth + Sz.X;
  if MaxClientHeight > 0 then MaxHeight := MaxClientHeight + Sz.Y;
end;

constructor TTBXDockablePanel.Create(AOwner: TComponent);
begin
  inherited;
  FMinClientWidth := 32;
  FMinClientHeight := 32;
  FDockedWidth := 128;
  FDockedHeight := 128;
  CloseButtonWhenDocked := True;
  DblClickUndock := False;
  FShowCaptionWhenDocked := True;
  FSmoothDockedResize := True;
  BlockSizeUpdate := True;
  SetBounds(Left, Top, 128, 128);
  BlockSizeUpdate := False;
  FullSize := True;
  AddThemeNotification(Self);
  ParentColor := False;
  FUseThemeColor := True;
  UpdateColor;
  SupportedDocks := [dkStandardDock, dkMultiDock];
end;

destructor TTBXDockablePanel.Destroy;
begin
  RemoveThemeNotification(Self);
  inherited;
end;

function TTBXDockablePanel.DoArrange(CanMoveControls: Boolean;
  PreviousDockType: TTBDockType; NewFloating: Boolean; NewDock: TTBDock): TPoint;
begin
  Result := CalcSize(NewDock);
end;

function TTBXDockablePanel.DoBeginDockedResizing(Vertical: Boolean): Boolean;
var
  Sz: Integer;
begin
  Result := True;
  if Vertical then Sz := Height else Sz := Width;
  if Assigned(FOnDockedResizing) then FOnDockedResizing(Self, Vertical, Sz, rsBeginResizing, Result);
  if Result then
    if Vertical then Height := Sz else Width := Sz;
end;

function TTBXDockablePanel.DoDockedResizing(Vertical: Boolean; var NewSize: Integer): Boolean;
const
  MIN_PARENT_CLIENT_SIZE = 32;
var
  NCSizes: TPoint;
  CW, CH: Integer;
  DockParent: TWinControl;
  ClientSize: TPoint;
begin
  NCSizes := CalcNCSizes;
  DockParent := Parent.Parent;
  ClientSize := GetClientSizeEx(DockParent);

  Assert(DockParent <> nil);
  if not Vertical then
  begin
    CW := ClientSize.X - MIN_PARENT_CLIENT_SIZE + Width;
    if NewSize > CW then NewSize := CW;
    CW := NewSize - NCSizes.X;
    if CW < MinClientWidth then CW := MinClientWidth
    else if (MaxClientWidth > MinClientWidth) and (CW > MaxClientWidth) then CW := MaxClientWidth;
    NewSize := CW + NCSizes.X;
  end
  else
  begin
    CH := ClientSize.Y - MIN_PARENT_CLIENT_SIZE + Height;
    if NewSize > CH then NewSize := CH;
    CH := NewSize - NCSizes.Y;
    if CH < MinClientHeight then CH := MinClientHeight
    else if (MaxClientHeight > MinClientHeight) and (CH > MaxClientHeight) then CH := MaxClientHeight;
    NewSize := CH + NCSizes.Y;
  end;
  Result := True;
  if Assigned(FOnDockedResizing) then FOnDockedResizing(Self, Vertical, NewSize, rsResizing, Result);
end;

function TTBXDockablePanel.DoEndDockedResizing(Vertical: Boolean): Boolean;
var
  Sz: Integer;
begin
  Result := True;
  if Vertical then Sz := Height else Sz := Width;
  if Assigned(FOnDockedResizing) then
    FOnDockedResizing(Self, Vertical, Sz, rsEndResizing, Result);
  if Result then
    if Vertical then Height := Sz else Width := Sz;
end;

procedure TTBXDockablePanel.DrawNCArea(const DrawToDC: Boolean;
  const ADC: HDC; const Clip: HRGN);
var
  DC: HDC;
  R, CR: TRect;
  Sz: Integer;
  DockPanelInfo: TTBXDockPanelInfo;
  S: WideString;
begin
  if not Docked or not HandleAllocated then Exit;

  if not DrawToDC then DC := GetWindowDC(Handle)
  else DC := ADC;

  Assert(DC <> 0, 'TTBXToolWindow.DrawNCArea Error');
  try
    GetDockPanelInfo(DockPanelInfo);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    if not DrawToDC then
    begin
      SelectNCUpdateRgn(Handle, DC, Clip);
      CR := R;
      with DockPanelInfo.BorderSize, CR do
      begin
        InflateRect(CR, -X, -Y);
        if DockPanelInfo.ShowCaption then
        begin
          Sz := GetSystemMetrics(SM_CYSMCAPTION);
          if DockPanelInfo.IsVertical then Inc(Top, Sz)
          else Inc(Left, Sz);
        end;
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      end;
    end;
    S := Caption;
    DockPanelInfo.Caption := PWideChar(S);
    CurrentTheme.PaintDockPanelNCArea(DC, R, DockPanelInfo);
  finally
    if not DrawToDC then ReleaseDC(Handle, DC);
  end;
end;

procedure TTBXDockablePanel.GetBaseSize(var ASize: TPoint);
begin
  ASize := CalcSize(CurrentDock);
end;

function TTBXDockablePanel.GetDockedCloseButtonRect(LeftRight: Boolean): TRect;
var
  X, Y, Z: Integer;
begin
  Z := GetSystemMetrics(SM_CYSMCAPTION) - 1;
  if LeftRight or not IsVertCaption then
  begin
    X := (ClientWidth + DockedBorderSize) - Z;
    Y := DockedBorderSize;
  end
  else
  begin
    X := DockedBorderSize;
    Y := ClientHeight + DockedBorderSize - Z;
  end;
  Result := Bounds(X, Y, Z, Z);
end;

procedure TTBXDockablePanel.GetDockPanelInfo(out DockPanelInfo: TTBXDockPanelInfo);
begin
  FillChar(DockPanelInfo, SizeOf(DockPanelInfo), 0);
  DockPanelInfo.WindowHandle := WindowHandle;
  DockPanelInfo.ViewType := GetViewType;
  if CurrentDock <> nil then DockPanelInfo.IsVertical := not IsVertCaption;
  DockPanelInfo.AllowDrag := CurrentDock.AllowDrag;
  DockPanelInfo.BorderStyle := BorderStyle;
  CurrentTheme.GetViewBorder(DockPanelInfo.ViewType, DockPanelInfo.BorderSize);
  DockPanelInfo.ClientWidth := ClientWidth;
  DockPanelInfo.ClientHeight := ClientHeight;
  DockPanelInfo.ShowCaption := ShowCaptionWhenDocked;
  if UseThemeColor then DockPanelInfo.Color := clDefault
  else DockPanelInfo.Color := Color;
  if ShowCaptionWhenDocked and CloseButtonWhenDocked then
  begin
    DockPanelInfo.CloseButtonState := CDBS_VISIBLE;
    if CloseButtonDown then DockPanelInfo.CloseButtonState := DockPanelInfo.CloseButtonState or CDBS_PRESSED;
    if CloseButtonHover then DockPanelInfo.CloseButtonState := DockPanelInfo.CloseButtonState or CDBS_HOT;
  end;
end;

function TTBXDockablePanel.GetFloatingBorderSize: TPoint;
begin
  CurrentTheme.GetViewBorder(GetViewType or VT_FLOATING, Result);
end;

function TTBXDockablePanel.GetFloatingWindowParentClass: TTBFloatingWindowParentClass;
begin
  Result := TTBXFloatingWindowParent;
end;

procedure TTBXDockablePanel.GetMinMaxSize(var AMinClientWidth, AMinClientHeight,
  AMaxClientWidth, AMaxClientHeight: Integer);
begin
  AMinClientWidth := FMinClientWidth;
  AMinClientHeight := FMinClientHeight;
  AMaxClientWidth := FMaxClientWidth;
  AMaxClientHeight := FMaxClientHeight;
end;

function TTBXDockablePanel.GetViewType: Integer;
begin
  Result := VT_DOCKPANEL;
  if Floating then Result := Result or VT_FLOATING;
  if Resizable then Result := Result or VT_RESIZABLE;
end;

function TTBXDockablePanel.IsColorStored: Boolean;
begin
  Result := not (ParentColor or UseThemeColor);
end;

function TTBXDockablePanel.IsVertCaption: Boolean;
begin
  case CaptionRotation of
    dpcrAlwaysHorz: Result := False;
    dpcrAlwaysVert: Result := Docked;
  else // dpcrAuto:
    Result := Docked and (CurrentDock.Position in [dpTop, dpBottom]);
  end;
end;

procedure TTBXDockablePanel.Paint;
begin
  if csDesigning in ComponentState then with Canvas do
  begin
    Pen.Style := psDot;
    Pen.Color := clBtnShadow;
    Brush.Style := bsClear;
    with ClientRect do Rectangle(Left, Top, Right, Bottom);
    Pen.Style := psSolid;
  end;
end;

procedure TTBXDockablePanel.ReadPositionData(const Data: TTBReadPositionData);
begin
  with Data do
  begin
    FDockedWidth := ReadIntProc(Name, rvDockedWidth, FDockedWidth, ExtraData);
    FDockedHeight := ReadIntProc(Name, rvDockedHeight, FDockedHeight, ExtraData);
    FFloatingWidth := ReadIntProc(Name, rvFloatingWidth, FFloatingWidth, ExtraData);
    FFloatingHeight := ReadIntProc(Name, rvFloatingHeight, FFloatingHeight, ExtraData);
    FSplitWidth := ReadIntProc(Name, rvSplitWidth, FSplitWidth, ExtraData);
    FSplitHeight := ReadIntProc(Name, rvSplitHeight, FSplitHeight, ExtraData);
  end;
end;

procedure TTBXDockablePanel.SetBorderSize(Value: Integer);
begin
  if FBorderSize <> Value then
  begin
    FBorderSize := Value;
    Realign;
  end;
end;

procedure TTBXDockablePanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TTBXDockablePanel.SetCaptionRotation(Value: TDPCaptionRotation);
begin
  if FCaptionRotation <> Value then
  begin
    FCaptionRotation := Value;
    if Docked and HandleAllocated then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

procedure TTBXDockablePanel.SetDockedHeight(Value: Integer);
begin
  if Value < MinClientHeight then Value := MinClientHeight;
  if Value <> FDockedHeight then
  begin
    FDockedHeight := Value;
    if Docked and (CurrentDock.Position in [dpTop, dpBottom]) then
    begin
      BlockSizeUpdate := True;
      Height := Value + CalcNCSizes.Y;
      BlockSizeUpdate := False;
    end;
  end;
end;

procedure TTBXDockablePanel.SetDockedWidth(Value: Integer);
begin
  if Value < MinClientWidth then Value := MinClientWidth;
  if Value <> FDockedWidth then
  begin
    FDockedWidth := Value;
    if Docked and (CurrentDock.Position in [dpLeft, dpRight]) then
    begin
      BlockSizeUpdate := True;
      Width := Value + CalcNCSizes.X;
      BlockSizeUpdate := False;
    end;
  end;
end;

procedure TTBXDockablePanel.SetFloatingHeight(Value: Integer);
begin
  { FloatingHeight (and floating width) can be set to 0 while panel is docked.
    This will force to restore floating dimensions from docked size }
  if Value < 0 then Value := 0;
  if not Docked and (Value < MinClientHeight) then Value := MinClientHeight;
  if Value <> FFloatingHeight then
  begin
    FFloatingHeight := Value;
    if not Docked then
    begin
      BlockSizeUpdate := True;
      Height := Value + CalcNCSizes.Y;
      BlockSizeUpdate := False;
    end;
  end;
end;

procedure TTBXDockablePanel.SetFloatingWidth(Value: Integer);
begin
  { See comment for TTBXDockablePanel.SetFloatingHeight }
  if Value < 0 then Value := 0;
  if not Docked and (Value < MinClientWidth) then Value := MinClientWidth;
  if Value <> FFloatingWidth then
  begin
    FFloatingWidth := Value;
    if not Docked then
    begin
      BlockSizeUpdate := True;
      Width := Value + CalcNCSizes.X;
      BlockSizeUpdate := False;
    end;
  end;
end;

procedure TTBXDockablePanel.SetMinClientHeight(Value: Integer);
begin
  if Value < 8 then Value := 8;
  FMinClientHeight := Value;
end;

procedure TTBXDockablePanel.SetMinClientWidth(Value: Integer);
begin
  if Value < 8 then Value := 8;
  FMinClientWidth := Value;
end;

procedure TTBXDockablePanel.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent is TTBXFloatingWindowParent then
    TTBXFloatingWindowParent(AParent).SnapDistance := SnapDistance;
end;

procedure TTBXDockablePanel.SetShowCaptionWhenDocked(Value: Boolean);
begin
  if FShowCaptionWhenDocked <> Value then
  begin
    FShowCaptionWhenDocked := Value;
    if Docked then
    begin
      if HandleAllocated then
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or
          SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    end;
  end;
end;

procedure TTBXDockablePanel.SetSnapDistance(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FSnapDistance := Value;
  if (Parent <> nil) and (Parent is TTBXFloatingWindowParent) then
    TTBXFloatingWindowParent(Parent).SnapDistance := Value;
end;

procedure TTBXDockablePanel.SetSplitHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSplitHeight <> Value then
  begin
    FSplitHeight := Value;
    if Docked and (CurrentDock.Position in [dpLeft, dpRight]) and
      (CurrentDock is TTBXMultiDock) then CurrentDock.ArrangeToolbars;
  end;
end;

procedure TTBXDockablePanel.SetSplitWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSplitWidth <> Value then
  begin
    FSplitWidth := Value;
    if Docked and (CurrentDock.Position in [dpTop, dpBottom]) and
      (CurrentDock is TTBXMultiDock) then CurrentDock.ArrangeToolbars;
  end;
end;

procedure TTBXDockablePanel.SetUseThemeColor(Value: Boolean);
begin
  if FUseThemeColor <> Value then
  begin
    FUseThemeColor := Value;
    if Value then UpdateColor;
  end;
end;

procedure TTBXDockablePanel.SizeChanging(const AWidth, AHeight: Integer);
begin
  if not BlockSizeUpdate then
  begin
    if Docked and (CurrentDock.Position in [dpLeft, dpRight]) then
      FDockedWidth := AWidth - CalcNCSizes.X
    else if Floating then
      FFloatingWidth := AWidth - CalcNCSizes.X;

    if Docked and (CurrentDock.Position in [dpTop, dpBottom]) then
      FDockedHeight := AHeight - CalcNCSizes.Y
    else if Floating then
      FFloatingHeight := AHeight - CalcNCSizes.Y;
  end;
end;

procedure TTBXDockablePanel.TBMGetViewType(var Message: TMessage);
begin
  Message.Result := GetViewType;
end;

procedure TTBXDockablePanel.TBMThemeChange(var Message: TMessage);
begin
  case Message.WParam of
    TSC_BEFOREVIEWCHANGE: BeginUpdate;
    TSC_AFTERVIEWCHANGE:
      begin
        EndUpdate;
        UpdateColor;
        if HandleAllocated and not (csDestroying in ComponentState) and
          (Parent is TTBXFloatingWindowParent) then
          UpdateNCArea(TTBXFloatingWindowParent(Parent), GetWinViewType(Self))
        else
          UpdateNCArea(Self, GetWinViewType(Self));
        Invalidate;
      end;
  end;
end;

procedure TTBXDockablePanel.UpdateColor;
var
  C: TColor;
begin
  if UseThemeColor then
  begin
    C := CurrentTheme.GetViewColor(GetWinViewType(Self));
    if C <> Color then
    try
      FUpdatingColor := True;
      Color := C;
    finally
      FUpdatingColor := False;
    end;
  end;
end;

procedure TTBXDockablePanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  FillRectEx(Message.DC, ClientRect, Color);
  Message.Result := 1;
end;

procedure TTBXDockablePanel.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
  if Docked then
    with Message.CalcSize_Params^ do
    begin
      InflateRect(rgrc[0], -DockedBorderSize, -DockedBorderSize);
      if ShowCaptionWhenDocked then
        if IsVertCaption then Inc(rgrc[0].Left, GetSystemMetrics(SM_CYSMCAPTION))
        else Inc(rgrc[0].Top, GetSystemMetrics(SM_CYSMCAPTION))
    end;
end;

procedure TTBXDockablePanel.WMNCHitTest(var Message: TWMNCHitTest);
const
  CResizeMargin = 2;
var
  P: TPoint;
  R: TRect;
  Sz: Integer;
  IsVertical, UseDefaultHandler: Boolean;
begin
  if Docked then
  begin
    UseDefaultHandler := False;
    if csDesigning in ComponentState then with Message do
    begin
      P := SmallPointToPoint(Pos);
      GetWindowRect(Handle, R);
      if PtInRect(R, P) then
      begin
        Result := 0;
        case CurrentDock.Position of
          dpLeft: if P.X >= R.Right - CResizeMargin then Result := HTRIGHT;
          dpTop: if P.Y >= R.Bottom - CResizeMargin then Result := HTBOTTOM;
          dpRight: if P.X <= R.Left + CResizeMargin then Result := HTLEFT;
          dpBottom: if P.Y <= R.Top + CResizeMargin then Result := HTTOP;
        end;
        if Result = 0 then
        begin
          if (P.X >= R.Right - CResizeMargin) and CanSplitResize(dpRight) then Result := HT_TBX_SPLITRESIZERIGHT
          else if (P.Y >= R.Bottom - CResizeMargin) and CanSplitResize(dpBottom) then Result := HT_TBX_SPLITRESIZEBOTTOM
          else if (P.X <= R.Left + CResizeMargin) and CanSplitResize(dpLeft) then Result := HT_TBX_SPLITRESIZELEFT
          else if (P.Y <= R.Top + CResizeMargin) and CanSplitResize(dpTop) then Result := HT_TBX_SPLITRESIZETOP;
        end;
        UseDefaultHandler := Result <> 0;
      end;
      if UseDefaultHandler then DefaultHandler(Message)
      else inherited;
    end;

    with Message do
    begin
      P := SmallPointToPoint(Pos);
      GetWindowRect(Handle, R);
      if Resizable then
        case CurrentDock.Position of
          dpLeft: if P.X >= R.Right - CResizeMargin then Result := HTRIGHT;
          dpTop: if P.Y >= R.Bottom - CResizeMargin then Result := HTBOTTOM;
          dpRight: if P.X <= R.Left + CResizeMargin then Result := HTLEFT;
          dpBottom: if P.Y <= R.Top + CResizeMargin then Result := HTTOP;
        end;
      if Result = 0 then
      begin
        if (P.X >= R.Right - CResizeMargin) and CanSplitResize(dpRight) then Result := HT_TBX_SPLITRESIZERIGHT
        else if (P.Y >= R.Bottom - CResizeMargin) and CanSplitResize(dpBottom) then Result := HT_TBX_SPLITRESIZEBOTTOM
        else if (P.X <= R.Left + CResizeMargin) and CanSplitResize(dpLeft) then Result := HT_TBX_SPLITRESIZELEFT
        else if (P.Y <= R.Top + CResizeMargin) and CanSplitResize(dpTop) then Result := HT_TBX_SPLITRESIZETOP;
      end;
      if (Result <> HTCLIENT) and ((Result < HTLEFT) or (Result > HTBOTTOM)) and
        ((Result < HT_TBX_SPLITRESIZELEFT) or (Result > HT_TBX_SPLITRESIZEBOTTOM)) then
      begin
        Result := HTNOWHERE;
        InflateRect(R, -DockedBorderSize, -DockedBorderSize);

        if PtInRect(R, P) and ShowCaptionWhenDocked and not (csDesigning in ComponentState) then
        begin
          { caption area }
          IsVertical := not IsVertCaption;
          if CloseButtonWhenDocked then
          begin
            Sz := GetSystemMetrics(SM_CYSMCAPTION);
            if IsVertical then Inc(Sz, 4) else Dec(Sz, 4);
          end
          else Sz := 0;

          if (IsVertical and (P.X >= R.Right - Sz) and (P.Y < R.Top + Sz)) or
            (not IsVertical and (P.Y >= R.Bottom - Sz) and (P.X < R.Left + Sz)) then
            Result := HT_TB2k_Close
          else
            Result := HT_TB2k_Border;
        end;
      end;
    end;
  end
  else inherited;
end;

procedure TTBXDockablePanel.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  OldCursor: HCURSOR;
begin
  if Message.HitTest in [HTLEFT..HTBOTTOM] then BeginDockedSizing(Message.HitTest)
  else if Message.HitTest in [HT_TBX_SPLITRESIZELEFT..HT_TBX_SPLITRESIZEBOTTOM] then BeginSplitResizing(Message.HitTest)
  else
  begin
    if (Message.HitTest = HT_TB2k_Border) and IsMovable then
    begin
      OldCursor := SetCursor(LoadCursor(0, IDC_SIZEALL));
      try
        inherited;
      finally
        SetCursor(OldCursor);
      end;
    end
    else inherited;
  end;
end;

procedure TTBXDockablePanel.WMSetCursor(var Message: TWMSetCursor);
begin
  if Docked and CurrentDock.AllowDrag and
     (Message.CursorWnd = WindowHandle) and
     (Smallint(Message.HitTest) = HT_TB2k_Border) and
     ShowCaptionWhenDocked then
  begin
    SetCursor(LoadCursor(0, IDC_ARROW));
    Message.Result := 1;
    Exit;
  end
  else if Docked and CurrentDock.AllowDrag and (Message.CursorWnd = WindowHandle) then
  begin
    if (Message.HitTest = HT_TBX_SPLITRESIZELEFT) or (Message.HitTest = HT_TBX_SPLITRESIZERIGHT) then
    begin
      SetCursor(LoadCursor(0, IDC_SIZEWE));
      Message.Result := 1;
      Exit;
    end
    else if (Message.HitTest = HT_TBX_SPLITRESIZETOP) or (Message.HitTest = HT_TBX_SPLITRESIZEBOTTOM) then
    begin
      SetCursor(LoadCursor(0, IDC_SIZENS));
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

procedure TTBXDockablePanel.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if (Message.WindowPos^.flags and SWP_NOSIZE) = 0 then
  begin
    Realign;
    Update;
  end;
  if (Message.WindowPos^.flags and SWP_SHOWWINDOW) <> 0 then
  begin
    {UpdateEffectiveColor;
    UpdateChildColors;  }
  end;
end;

procedure TTBXDockablePanel.WritePositionData(const Data: TTBWritePositionData);
begin
  with Data do
  begin
    WriteIntProc(Name, rvDockedWidth, FDockedWidth, ExtraData);
    WriteIntProc(Name, rvDockedHeight, FDockedHeight, ExtraData);
    WriteIntProc(Name, rvFloatingWidth, FFloatingWidth, ExtraData);
    WriteIntProc(Name, rvFloatingHeight, FFloatingHeight, ExtraData);
    WriteIntProc(Name, rvSplitWidth, FSplitWidth, ExtraData);
    WriteIntProc(Name, rvSplitHeight, FSplitHeight, ExtraData);
  end;
end;

end.
