unit aceScrollPanel;

interface

{$I sDefs.inc}

uses
  SysUtils, Classes, Windows, Messages, Graphics, Forms, ExtCtrls, Controls, ImgList, Buttons,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sPanel, sSpeedButton, sCommonData;


const
  CM_PARENTBEVELEDCHANGED = WM_USER + 1;
  CM_PARENTBUTTONVISIBLECHANGED = WM_USER + 3;


type
  TacScrollPanel = class;
  TOnCanExpand = procedure(Sender: TObject; var CanExpand: Boolean) of object;
  TOnCanCollapse = procedure(Sender: TObject; var CanCollapse: Boolean) of object;

  TacScrollPanelBand = class(TsPanel)
  private
    FData: Pointer;
    FExpandedHeight: Integer;
    FButton: TsSpeedButton;
    FExpanded: Boolean;
    FOrder: Integer;
    FBeveled: Boolean;
    FBorderWidth: Integer;
    FParentBeveled: Boolean;
    FParentButtonFont: Boolean;
    FParentButtonVisible: Boolean;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnCanCollapse: TOnCanCollapse;
    FOnCanExpand: TOnCanExpand;
    procedure ButtonClick(Sender: TObject);
    procedure SetExpanded(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    function GetOrder: Integer;
    procedure SetOrder(const Value: Integer);
    procedure SetParentBeveled(const Value: Boolean);
    procedure SetBeveled(const Value: Boolean);
    procedure SetBorderWidth(const Value: Integer);
    function IsBeveledStored: Boolean;
    function GetButtonVisible: Boolean;
    procedure SetButtonVisible(const Value: Boolean);
    function IsButtonVisibleStored: Boolean;
    procedure SetParentButtonVisible(const Value: Boolean);
    procedure CMParentBeveledChanged(var Msg: TMessage); message CM_PARENTBEVELEDCHANGED;
    procedure CMParentButtonVisibleChanged(var Msg: TMessage); message CM_PARENTBUTTONVISIBLECHANGED;
    procedure SetImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    function GetImageIndex: integer;
    function GetImages: TCustomImageList;
    function GetTitleHeight: integer;
    procedure SetTitleHeight(const Value: integer);
  protected
    procedure TextChanged;
    procedure BoundsChanged;
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetZOrder(TopMost: Boolean); override;
    function ScrollMax: TacScrollPanel;
    procedure UpdateSize(ATop: Integer);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CollapsedHeight: Integer;
    procedure ChangeScale(M, D: Integer); override;
  public
    procedure Paint; override;
    function PrepareCache: boolean; override;
    procedure OurPaint(DC: HDC = 0; SendUpdated: boolean = True); override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
    procedure CreateWnd; override;

    procedure AfterConstruction; override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: Pointer read FData write FData;
    property Button: TsSpeedButton read FButton;
  published
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property Caption;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight default 100;
    property Order: Integer read GetOrder write SetOrder stored False;
    property ButtonVisible: Boolean read GetButtonVisible write SetButtonVisible stored IsButtonVisibleStored;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property ImageIndex: integer read GetImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property ParentBeveled: Boolean read FParentBeveled write SetParentBeveled stored IsBeveledStored;
    property ParentButtonVisible: Boolean read FParentButtonVisible write SetParentButtonVisible default True;
    property OnResize;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnCanExpand: TOnCanExpand read FOnCanExpand write FOnCanExpand;
    property OnCanCollapse: TOnCanCollapse read FOnCanCollapse write FOnCanCollapse;
    property Left stored False;
    property Top stored False;
    property Width;
    property Height;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TitleHeight: integer read GetTitleHeight write SetTitleHeight default 28;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property BiDiMode;
    property ParentBiDiMode;
  end;


  TacScrollPanelBands = class(TsPanel)
  private
    FScrolling: Boolean;
  protected
    procedure FocusChanged(Control: TWinControl);
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ScrollControls(const DeltaY: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
  end;


  TacPanelScrollBar = class(TsPanel)
  private
    FMin: Integer;
    FMax: Integer;
    FPos: Integer;
    FPage: Integer;
    Scroll: TsPanel;
    FDesignInteractive: Boolean;
    FInclusive: Boolean;
    FOnChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    procedure SetParam(Index, Value: Integer);
    procedure SetInclusive(Value: Boolean);
  protected
    procedure CreateWnd; override;
    procedure SetTrackBar;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure SetParams(const AMin, AMax, APage, APos: Integer);
    property Pos: Integer index 3 read FPos write SetParam;
    property DesignInteractive: Boolean read FDesignInteractive write FDesignInteractive;
    property Scroller: TsPanel read Scroll;
  published
    property Color;
    property Align;
    property Min: Integer index 0 read FMin write SetParam;
    property Max: Integer index 1 read FMax write SetParam;
    property Page: Integer index 2 read FPage write SetParam;
    property Position: Integer index 3 read FPos write SetParam;
    property Inclusive: Boolean read FInclusive write SetInclusive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TacScrollPanel = class(TsPanel)
  private
    FScrollBar: TacPanelScrollBar;
    FScrollPos: Integer;
    FY: Integer;
    FOnScroll: TNotifyEvent;
    FBeveled: Boolean;
    FButtonVisible: Boolean;
    FAutoHeight: Boolean;
    FExpandedHeight: Integer;
    FOneExpanded: Boolean;
    procedure Correct;
    procedure CorrectHeight;
    procedure BandMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BandMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BandMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBarScroll(Sender: TObject);
    function GetBand(Index: Integer): TacScrollPanelBand;
    function GetBandCount: Integer;
    procedure SetScrollPos(const Value: Integer);
    procedure SetButtonVisible(const Value: Boolean);
    procedure SetBeveled(const Value: Boolean);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    function GetScrollBarWidth: Cardinal;
    procedure SetScrollBarWidth(const Value: Cardinal);
    function GetScrollBarVisible: Boolean;
    procedure SetScrollBarVisible(const Value: Boolean);
    procedure SetOneExpanded(const Value: Boolean);
  protected
    function GetChildParent: TComponent; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
  public
    FPnlEdit: TacScrollPanelBands;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc (var Message: TMessage); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ScrollInView(AControl: TControl);
    procedure MouseControls(AControls: array of TControl);
    procedure MouseClasses(AControlClasses: array of TControlClass);
    function AllCollapsed: Boolean;
    function AllExpanded: Boolean;
    procedure AddBand(Band: TacScrollPanelBand);
    property BandCount: Integer read GetBandCount;
    property Bands[Index: Integer]: TacScrollPanelBand read GetBand;
    property DockManager;
  published
    property ScrollPos: Integer read FScrollPos write SetScrollPos default 0;
    property BorderWidth default 3;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible default True;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight default -1;
    property ScrollBarWidth: Cardinal read GetScrollBarWidth write SetScrollBarWidth default 7;
    property ScrollBarVisible: Boolean read GetScrollBarVisible write SetScrollBarVisible default True;
    property OneExpanded: Boolean read FOneExpanded write SetOneExpanded default False;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property Align default alLeft;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BorderStyle;
    property Color;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property Anchors;
    property Constraints;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
  end;


implementation

uses sSkinProps, sConst, acntUtils, sVCLUtils, sGraphUtils, sMessages;


function PanelBorder(Panel: TCustomPanel): Integer;
begin
  Result := TPanel(Panel).BorderWidth;
  if TPanel(Panel).BevelOuter <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);

  if TPanel(Panel).BevelInner <> bvNone then
    Inc(Result, TPanel(Panel).BevelWidth);
end;


type
  TacScroller = class(TsPanel)
  private
    FY: Integer;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


procedure TacScroller.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FY := Y;
end;


procedure TacScroller.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Sm, T, OldPos: Integer;
begin
  if Shift = [ssLeft] then begin
    Sm := FY - Y;
    T := Top;
    if Sm <> 0 then
      with Parent as TacPanelScrollBar do begin
        OldPos := Pos;
        Pos := Pos - Round(Sm * (FMax - FMin + 1) / ClientHeight);
        if (Pos <> OldPos) and Assigned(FOnScroll) then
          FOnScroll(Parent);
      end;
      
    FY := Y - Top + T;
  end;
end;


procedure TacScroller.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  with (Owner as TacPanelScrollBar) do
    Msg.Result := Integer(FDesignInteractive and (FPage <> FMax - FMin + 1));
end;


constructor TacPanelScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.SkinSection := s_Gauge;
  BevelOuter := bvLowered;
  Color := clBtnShadow;
  Caption := '';
  Scroll := TacScroller.Create(Self);
  Scroll.Parent := Self;
  ControlStyle := ControlStyle - [csSetCaption];
  if (csDesigning in AOwner.ComponentState) then
    ControlStyle := ControlStyle - [csAcceptsControls];

  Scroll.Caption := '';
  Scroll.ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
{$IFDEF DELPHI7UP}
  Scroll.ParentBackground := False;
  ParentBackground := False;
{$ENDIF}  
  FMax := 100;
  FPage := 10;
  Width := 20;
  Height := 100;
end;


procedure TacPanelScrollBar.Loaded;
begin
  inherited Loaded;
  Resize;
end;


procedure TacPanelScrollBar.Resize;
begin
  inherited Resize;
  with Scroll do begin
    Top := BevelWidth;
    Left := BevelWidth;
    Width := Self.Width - 2 * BevelWidth;
  end;
  SetTrackBar;
end;


procedure TacPanelScrollBar.SetTrackBar;
var
  CH, H, T: Integer;
  L, FP, P, P1: Integer;
begin
  if FMin > FMax then
    FMin := FMax;

  if FPage > FMax - FMin + 1 then
    FPage := FMax - FMin + 1;

  if FInclusive then
    P := FPage
  else
    P := 0;

  P1 := FPage - P;
  if FPos > FMax - P then
    FPos := FMax - P;

  if FPos < FMin then
    FPos := FMin;

  L := FMax - FMin + 1;
  CH := Height - 2 * BevelWidth;
  H := Trunc(CH * FPage / L) + 1;
  FP := Trunc((FPos - FMin) / L * (L - P1)) + 1;
  T := Round(CH * FP / L);
  if H < 7 then
    H := 7;

  if H > CH then
    H := CH;

  if T < BevelWidth then
    T := BevelWidth;

  if T + H > Height - BevelWidth then
    T := Height - BevelWidth - H;

  if FPos = FMax - P then
    T := Height - BevelWidth - H;

  Scroll.SetBounds(Scroll.Left, T, Scroll.Width, H);
end;


procedure TacPanelScrollBar.SetParam(Index, Value: Integer);
begin
  case Index of
    0: FMin := Value;
    1: FMax := Value;
    2: FPage := Value;
    3: FPos := Value;
  end;
  SetParams(FMin, FMax, FPage, FPos);
end;


procedure TacPanelScrollBar.SetParams(const AMin, AMax, APage, APos: Integer);
begin
  FMin := AMin;
  FMax := AMax;
  FPage := APage;
  FPos := APos;
  if Assigned(FOnChange) then
    FOnChange(Self);

  SetTrackBar;
end;


procedure TacPanelScrollBar.SetInclusive(Value: Boolean);
begin
  FInclusive := Value;
  SetTrackBar;
end;


procedure TacPanelScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  SetTrackBar;
end;


const
  SpeedSpacing = 4;

type
  TacBandBtn = class(TsSpeedButton)
  private
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    function CurrentState: integer; override;
    procedure Invalidate; override;
  protected
    procedure FontChanged;
  end;


procedure TacBandBtn.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := 1;
end;


function TacBandBtn.CurrentState: integer;
begin
  if (Parent <> nil) and TacScrollPanelBand(Parent).Expanded then
    Result := 1
  else
    Result := inherited CurrentState
end;


procedure TacBandBtn.FontChanged;
begin
  if Parent <> nil then
    with Parent as TacScrollPanelBand do begin
      FParentButtonFont := False;
      Canvas.Font.Assign(Self.Font);
      Invalidate;
    end;
end;


constructor TacScrollPanelBand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption] + [csAcceptsControls];
  SkinData.SkinSection := s_BarPanel;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 100;
  FExpandedHeight := 100;
  FParentButtonFont := True;
  FParentButtonVisible := True;
  FParentBeveled := True;
  FButton := TacBandBtn.Create(Self);
  FButton.SkinData.SkinSection := s_BarTitle;
  with FButton as TacBandBtn do begin
    Alignment := taLeftJustify;
    SetDesigning(False);
    Parent := Self;
    Cursor := crArrow;
    OnClick := ButtonClick;
    Margin := 4;
    Spacing := SpeedSpacing;
    ParentColor := True;
    FButton.ParentBiDiMode := True;
  end;
  TitleHeight := 28;
  Expanded := True;
end;


procedure TacScrollPanelBand.Loaded;
begin
  inherited Loaded;
  SkinData.Loaded;
  Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  TextChanged;
end;


procedure TacScrollPanelBand.BoundsChanged;
begin
  if FExpanded then
    ExpandedHeight := Height;

  if ScrollMax <> nil then
    ScrollMax.CorrectHeight;
end;


procedure TacScrollPanelBand.TextChanged;
begin
  FButton.Caption := Caption;
end;


procedure TacScrollPanelBand.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then begin
    FExpanded := Value;
    if FExpanded and Assigned(FOnExpand) then
      FOnExpand(Self);

    if not FExpanded and Assigned(FOnCollapse) then
      FOnCollapse(Self);

    RequestAlign;
    if ScrollMax <> nil then
      ScrollMax.CorrectHeight;

    if not FExpanded then
      FButton.SkinData.Invalidate
  end;
end;


procedure TacScrollPanelBand.SetExpandedHeight(const Value: Integer);
begin
  if FExpandedHeight <> Value then begin
    FExpandedHeight := Value;
    if FExpanded then
      Height := FExpandedHeight;
  end;
end;


function TacScrollPanelBand.GetOrder: Integer;
var
  I: Integer;
begin
  Result := FOrder;
  if (Parent = nil) or not (Parent is TacScrollPanelBands) then
    Exit;

  for I := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[I] = Self then begin
      Result := I;
      Break;
    end;
end;


procedure TacScrollPanelBand.SetOrder(const Value: Integer);
begin
  if (Parent = nil) or not (Parent is TacScrollPanelBands) then
    Exit;

  if FOrder <> Value then begin
    TacScrollPanelBands(Parent).SetChildOrder(Self, Value);
    FOrder := GetOrder;
    RequestAlign;
  end;
end;


function TacScrollPanelBand.GetButtonVisible: Boolean;
begin
  Result := FButton.Visible;
end;


procedure TacScrollPanelBand.SetButtonVisible(const Value: Boolean);
begin
  if FButton.Visible <> Value then begin
    FParentButtonVisible := False;
    FButton.Visible := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;


function TacScrollPanelBand.IsButtonVisibleStored: Boolean;
begin
  Result := not ParentButtonVisible;
end;


procedure TacScrollPanelBand.SetParentButtonVisible(const Value: Boolean);
begin
  if FParentButtonVisible <> Value then begin
    FParentButtonVisible := Value;
    if Parent <> nil then
      Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  end;
end;


procedure TacScrollPanelBand.CMParentButtonVisibleChanged(var Msg: TMessage);
begin
  if FParentButtonVisible then begin
    if ScrollMax <> nil then
      SetButtonVisible(ScrollMax.FButtonVisible);

    FParentButtonVisible := True;
  end;
end;


procedure TacScrollPanelBand.SetBeveled(const Value: Boolean);
begin
  if FBeveled <> Value then begin
    FParentBeveled := False;
    FBeveled := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;


function TacScrollPanelBand.IsBeveledStored: Boolean;
begin
  Result := not ParentBeveled;
end;


procedure TacScrollPanelBand.SetParentBeveled(const Value: Boolean);
begin
  if FParentBeveled <> Value then begin
    FParentBeveled := Value;
    if Parent <> nil then
      Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  end;
end;


procedure TacScrollPanelBand.CMParentBeveledChanged(var Msg: TMessage);
begin
  if FParentBeveled then begin
    if ScrollMax <> nil then
      SetBeveled(ScrollMax.FBeveled);

    FParentBeveled := True;
  end;
end;

procedure TacScrollPanelBand.ButtonClick(Sender: TObject);
var
  E: Boolean;
begin
  E := True;
  if FExpanded then begin
    if Assigned(FOnCanCollapse) then
      FOnCanCollapse(Self, E);
  end
  else
    if Assigned(FOnCanExpand) then
      FOnCanExpand(Self, E);

  if E then
    Expanded := not FExpanded;
end;


procedure TacScrollPanelBand.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if not (csLoading in ComponentState) then begin
    Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
    Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  end;
end;


procedure TacScrollPanelBand.SetZOrder(TopMost: Boolean);
begin
  inherited SetZOrder(TopMost);
  RequestAlign;
end;


procedure TacScrollPanelBand.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ScrollMax <> nil then
    ScrollMax.BandMouseDown(Self, Button, Shift, X, Y);
end;


procedure TacScrollPanelBand.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ScrollMax <> nil then
    ScrollMax.BandMouseMove(Self, Shift, X, Y);
end;


procedure TacScrollPanelBand.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ScrollMax <> nil then
    ScrollMax.BandMouseUp(Self, Button, Shift, X, Y);
end;


function TacScrollPanelBand.ScrollMax: TacScrollPanel;
begin
  if (Parent <> nil) and (Parent is TacScrollPanelBands) and ((Parent as TacScrollPanelBands).Parent <> nil) then
    Result := Parent.Parent as TacScrollPanel
  else
    Result := nil;
end;


function TacScrollPanelBand.CollapsedHeight: Integer;
begin
  Result := FButton.Height + FButton.Top;
end;


procedure TacScrollPanelBand.UpdateSize(ATop: Integer);
var
  W, H: Integer;
begin
  if (Parent <> nil) and not (Parent is TacScrollPanelBands) then
    Exit;

  if FExpanded then
    H := FExpandedHeight
  else
    H := CollapsedHeight;

  if not Visible then
    H := 0;

  if ScrollMax <> nil then begin
    W := Parent.Width;
    if ScrollMax.ScrollBarVisible then
      W := W - 3;
  end
  else
    W := Width;

  SetBounds(0, ATop, W, H);
  FButton.Left := 0;
  FButton.Width := Width;
end;


procedure TacScrollPanelBand.Paint;
const
  Ex: array [Boolean] of Integer = (BF_TOP, BF_RECT);
var
  R: TRect;
begin
  if not SkinData.Skinned then
    if Canvas.Handle <> 0 then
      if FBeveled then begin
        R.Left := 0;
        if ButtonVisible then
          R.Top := FButton.Top + FButton.Height
        else
          R.Top := 0;

        R.Right := Width;
        R.Bottom := Height;
        Windows.DrawEdge(Canvas.Handle, R, EDGE_ETCHED, Ex[FExpanded]);
        if ButtonVisible then begin
          Canvas.Brush.Color := Color;
          Canvas.Brush.Style := bsSolid;
          if Expanded then
            Canvas.FillRect(Bounds(1, R.Top, Width - 3, 2))
          else
            Canvas.FillRect(Bounds(0, R.Top, Width, 2))
        end;
      end;
end;


procedure TacScrollPanelBand.AlignControls(AControl: TControl; var Rect: TRect);
var
  BevelSize: Integer;
begin
  BevelSize := FBorderWidth;
  if FBeveled then
    Inc(BevelSize, 3);

  InflateRect(Rect, -BevelSize, -BevelSize);
  if ButtonVisible then begin
    Inc(Rect.Top, FButton.Height);
    if FButton.Top > FBorderWidth then
      Inc(Rect.Top, FButton.Top);
  end;
  inherited AlignControls(AControl, Rect);
end;


procedure TacScrollPanelBand.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then begin
    FBorderWidth := Value;
    Realign;
  end;
end;


procedure TacScrollPanelBand.ChangeScale(M, D : Integer);
begin
  inherited ChangeScale(M, D);
  ExpandedHeight := FExpandedHeight * M div D;
end;


procedure TacScrollPanelBands.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
  ScrollMax: TacScrollPanel;
  T: Integer;
  SMax, SPage, SPos: Integer;

  procedure AdjustBottom;
  begin
    if (Controls[ControlCount - 1].BoundsRect.Bottom < Height) and (Controls[0].Top < 0) then
      if Height - (Controls[ControlCount - 1].BoundsRect.Bottom - Controls[0].Top) > 0 then
        ScrollControls(-Controls[0].Top)
      else
        ScrollControls(Height - Controls[ControlCount - 1].BoundsRect.Bottom);
  end;

  procedure AdjustBand;
  var
    Band: TacScrollPanelBand;
  begin
    Band := AControl as TacScrollPanelBand;
    if (Band <> nil) and
          Band.FExpanded and
            (Band.BoundsRect.Bottom > Height) and
              (Band.Top > 0) and
                not (csLoading in Band.ComponentState) then
      ScrollControls(Height - Band.BoundsRect.Bottom);
  end;

begin
  if FScrolling then
    Exit;

  if (Parent <> nil) and (csLoading in Parent.ComponentState) then
    Exit;

  if Parent is TacScrollPanel then
    ScrollMax := Parent as TacScrollPanel
  else
    Exit;

  if (AControl <> nil) and (AControl is TacScrollPanelBand) and (AControl as TacScrollPanelBand).FExpanded and ScrollMax.FOneExpanded then
    for I := 0 to ControlCount - 1 do
      if Controls[I] <> AControl then
        (Controls[I] as TacScrollPanelBand).Expanded := False;
        
  SPos := ScrollMax.FScrollPos;
  if ControlCount > 0 then begin
    for I := 0 to ControlCount - 1 do begin
      if I > 0 then
        T := Controls[I - 1].BoundsRect.Bottom
      else
        T := -ScrollMax.FScrollPos;
        
      if (Controls[I] is TacScrollPanelBand) then
        (Controls[I] as TacScrollPanelBand).UpdateSize(T);
    end;
    AdjustBottom;
    if (AControl is TacScrollPanelBand) then
      AdjustBand;

    SMax := Controls[ControlCount - 1].BoundsRect.Bottom - Controls[0].Top;
    SPos := -Controls[0].Top;
    ScrollMax.FScrollPos := SPos;
  end
  else
    SMax := Height;

  SPage := Height;
  ScrollMax.FScrollBar.SetParams(0, SMax, SPage, SPos);
end;


procedure TacScrollPanelBands.ScrollControls(const DeltaY: Integer);
begin
  FScrolling := True;
  try
    ScrollBy(0, DeltaY);
  finally
    FScrolling := False;
  end;
end;


procedure TacScrollPanelBands.FocusChanged(Control: TWinControl);
begin
  if (Control <> nil) and ContainsControl(Control) and (Parent <> nil) then
    (Parent as TacScrollPanel).ScrollInView(Control);
end;


constructor TacScrollPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.SkinSection := s_Bar;
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Caption := '';
  Width := 250;
  Height := 150;
  Align := alLeft;
  BevelOuter := bvLowered;
  BorderWidth := 3;
  FExpandedHeight := -1;
  FButtonVisible := True;
  FBeveled := True;
  FPnlEdit := TacScrollPanelBands.Create(Self);
  with FPnlEdit do begin
    Align := alClient;
    Parent := Self;
    ControlStyle := ControlStyle + [csAcceptsControls];
    ParentColor := True;
  end;
  FScrollBar := TacPanelScrollBar.Create(Self);
  with FScrollBar do begin
    Inclusive := True;
    Parent := Self;
    Width := 7;
    Align := alRight;
    Max := FPnlEdit.Height;
    Page := Self.Height;
    OnScroll := ScrollBarScroll;
    Visible := True;
    DesignInteractive := True;
  end;
end;


destructor TacScrollPanel.Destroy;
begin
  inherited Destroy;
end;


procedure TacScrollPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_CLIPCHILDREN;
    ExStyle := ExStyle or WS_EX_CONTROLPARENT;
  end;
end;


procedure TacScrollPanel.Loaded;
begin
  inherited Loaded;
  Resize;
  FPnlEdit.Realign;
end;


procedure TacScrollPanel.SetButtonVisible(const Value: Boolean);
begin
  if FButtonVisible <> Value then begin
    FButtonVisible := Value;
    FPnlEdit.NotifyControls(CM_PARENTBUTTONVISIBLECHANGED);
  end;
end;


procedure TacScrollPanel.SetBeveled(const Value: Boolean);
begin
  if FBeveled <> Value then begin
    FBeveled := Value;
    FPnlEdit.NotifyControls(CM_PARENTBEVELEDCHANGED);
  end;
end;


procedure TacScrollPanel.MouseControls(AControls: array of TControl);
var
  I: Integer;
begin
  for I := Low(AControls) to High(AControls) do begin
    TacScrollPanel(AControls[I]).OnMouseDown := BandMouseDown;
    TacScrollPanel(AControls[I]).OnMouseMove := BandMouseMove;
    TacScrollPanel(AControls[I]).OnMouseUp := BandMouseUp;
  end;
end;


procedure TacScrollPanel.MouseClasses(AControlClasses: array of TControlClass);
var
  I, iB, iC: Integer;
begin
  for I := Low(AControlClasses) to High(AControlClasses) do
    for iB := 0 to BandCount - 1 do
      for iC := 0 to Bands[iB].ControlCount - 1 do
        if Bands[iB].Controls[iC] is AControlClasses[I] then begin
          TacScrollPanel(Bands[iB].Controls[iC]).OnMouseDown := BandMouseDown;
          TacScrollPanel(Bands[iB].Controls[iC]).OnMouseMove := BandMouseMove;
          TacScrollPanel(Bands[iB].Controls[iC]).OnMouseUp := BandMouseUp;
        end;
end;


procedure TacScrollPanel.Correct;
var
  Sm: Integer;
  CH: Integer;
begin
  if BandCount > 0 then begin
    Sm := 0;
    CH := FPnlEdit.Height;
    if (Bands[BandCount - 1].BoundsRect.Bottom < CH) and (Bands[0].Top < 0) then
      Sm := (CH - Bands[BandCount - 1].BoundsRect.Bottom);

    if Bands[0].Top + Sm > 0 then
      Sm := -Bands[0].Top;

    if Sm <> 0 then begin
      FPnlEdit.ScrollControls(Sm);
      FScrollBar.Pos := -Bands[0].Top;
      FScrollPos := FScrollBar.Pos;
    end;
  end;
end;


procedure TacScrollPanel.BandMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (BandCount > 0) then
    FY := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
end;


procedure TacScrollPanel.BandMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Sm: Integer;
  CH: Integer;
begin
  if (ssLeft in Shift) and (BandCount > 0) then begin
    Y := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := FPnlEdit.Height;
    if not (Sender = FScrollBar.Scroller) then
      Sm := Y - FY
    else
      Sm := FY - Y;

    if Sm < 0 then begin // Up
      if not (Bands[BandCount - 1].BoundsRect.Bottom > CH) then
        Sm := 0
      else
        if Bands[BandCount - 1].BoundsRect.Bottom + Sm < CH then
          Sm := CH - Bands[BandCount - 1].BoundsRect.Bottom;
    end
    else
      if Sm > 0 then // Down
        if not (Bands[0].Top < 0) then
          Sm := 0
        else
          if Bands[0].Top + Sm > 0 then
            Sm := -Bands[0].Top;

    if Sm <> 0 then begin
      FPnlEdit.ScrollControls(Sm);
      FScrollBar.Pos := -Bands[0].Top;
      FScrollPos := FScrollBar.Pos;
    end;
    FY := Y;
    Correct;
  end;
end;


procedure TacScrollPanel.BandMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;


function TacScrollPanel.GetBand(Index: Integer): TacScrollPanelBand;
begin
  Result := TacScrollPanelBand(FPnlEdit.Controls[Index]);
end;


function TacScrollPanel.GetBandCount: Integer;
begin
  if FPnlEdit <> nil then
    Result := FPnlEdit.ControlCount
  else
    Result := 0
end;


procedure TacScrollPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FPnlEdit.GetChildren(Proc, Root);
end;


function TacScrollPanel.GetChildParent: TComponent;
begin
  Result := FPnlEdit;
end;


procedure TacScrollPanel.SetScrollPos(const Value: Integer);
begin
  if FScrollPos <> Value then begin
    FScrollPos := Value;
    if not (csLoading in ComponentState) then begin
      InAnimationProcess := True;
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
      if FScrollPos > FScrollBar.Max - FScrollBar.Page then
        FScrollPos := FScrollBar.Max - FScrollBar.Page;

      if FScrollPos < 0 then
        FScrollPos := 0;

      FPnlEdit.Realign;
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      InAnimationProcess := False;
      RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_ALLCHILDREN or RDW_INVALIDATE);
    end;
  end;
end;


procedure TacScrollPanel.ScrollBarScroll(Sender: TObject);
begin
  ScrollPos := FScrollBar.Pos;
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;


procedure TacScrollPanel.ScrollInView(AControl: TControl);
var
  I: Integer;
  Band: TacScrollPanelBand;
  Rect: TRect;
begin
  Band := nil;
  for I := 0 to FPnlEdit.ControlCount - 1 do
    if (FPnlEdit.Controls[I] as TacScrollPanelBand).ContainsControl(AControl) then begin
      Band := FPnlEdit.Controls[I] as TacScrollPanelBand;
      Break;
    end;

  if Band = nil then
    raise Exception.Create('Band is empty');

  Band.Expanded := True;
  Rect := AControl.ClientRect;
  Dec(Rect.Top, BevelWidth + BorderWidth + 4);
  Inc(Rect.Bottom, BevelWidth + BorderWidth + 4);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Top < 0 then
    ScrollPos := ScrollPos + Rect.Top
  else
    if Rect.Bottom > ClientHeight then begin
      if Rect.Bottom - Rect.Top > ClientHeight then
        Rect.Bottom := Rect.Top + ClientHeight;

      ScrollPos := ScrollPos + Rect.Bottom - ClientHeight;
    end;
end;


procedure TacScrollPanel.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then begin
    FAutoHeight := Value;
    if FAutoHeight then
      CorrectHeight;
  end;
end;


procedure TacScrollPanel.SetExpandedHeight(const Value: Integer);
begin
  if FExpandedHeight <> Value then begin
    FExpandedHeight := Value;
    if FAutoHeight then
      CorrectHeight;
  end;
end;


procedure TacScrollPanel.Resize;
begin
  inherited Resize;
  if FAutoHeight and (BandCount > 0) and not AllCollapsed and (FExpandedHeight > -1) then
    FExpandedHeight := Height;

  if FAutoHeight then
    CorrectHeight;
end;


procedure TacScrollPanel.CorrectHeight;
var
  I, H: Integer;
  Band: TacScrollPanelBand;
begin
  if not FAutoHeight or (BandCount = 0) then
    Exit;

  if AllCollapsed then begin
    H := 0;
    for I := 0 to BandCount - 1 do
      Inc(H, Bands[I].Height);

    ClientHeight := H + 2 * PanelBorder(Self);
  end
  else
    if FExpandedHeight <> -1 then
      Height := FExpandedHeight
    else begin
      H := 0;
      Band := nil;
      for I := 0 to BandCount - 1 do
        if Bands[I].Height > H then begin
          Band := Bands[I];
          H := Band.Height;
        end;

      H := 0;
      for I := 0 to BandCount - 1 do
        if Bands[I] = Band then
          Inc(H, Bands[I].Height)
        else
          Inc(H, Bands[I].CollapsedHeight);
          
      ClientHeight := H + 2 * PanelBorder(Self);
    end;
end;


function TacScrollPanel.AllCollapsed: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to
    BandCount - 1 do
      if Bands[I].Expanded then
        Exit;

  Result := True;
end;


function TacScrollPanel.AllExpanded: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to BandCount - 1 do
    if not Bands[I].Expanded then
      Exit;

  Result := True;
end;


procedure TacScrollPanel.AddBand(Band: TacScrollPanelBand);
begin
  Band.Parent := GetChildParent as TWinControl;
end;


function TacScrollPanel.GetScrollBarWidth: Cardinal;
begin
  Result := FScrollBar.Width;
end;


procedure TacScrollPanel.SetScrollBarWidth(const Value: Cardinal);
begin
  if Value >= 4 then
    FScrollBar.Width := Value;
end;


function TacScrollPanel.GetScrollBarVisible: Boolean;
begin
  Result := FScrollBar.Visible;
end;


procedure TacScrollPanel.SetScrollBarVisible(const Value: Boolean);
begin
  FScrollBar.Visible := Value;
  if csDesigning in ComponentState then
    if not Value then
      FScrollBar.Parent := nil
    else
      FScrollBar.Parent := Self;
end;


procedure TacScrollPanel.SetOneExpanded(const Value: Boolean);
begin
  if FOneExpanded <> Value then
    FOneExpanded := Value;
end;


constructor TacScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.SkinSection := s_ProgressV;
end;


function TacScrollPanelBand.PrepareCache: boolean;
var
  CI: TCacheInfo;
  h: integer;
begin
  InitCacheBmp(SkinData); // Initializing of acache Bmp
  CI := GetParentCache(SkinData); // Receive information about parent BG image
  if ButtonVisible then begin
    h := FButton.Height + 1;
    // Draw BG under button
    if CI.Ready then // If we have image as Bitmap
      BitBlt(SkinData.FCacheBMP.Canvas.Handle, 0, 0, Width, h, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SrcCopy)
    else // If we have color for filling only
      FillDC(SkinData.FCacheBMP.Canvas.Handle, MkRect(Width, h), CI.FillColor);
      
    if Expanded then
      PaintItem(SkinData, CI, False, 0, Rect(0, FButton.Height, Width, Height), Point(Left, Top + h), SkinData.FCacheBMP, False);
  end
  else
    PaintItem(SkinData, CI, True, 0, MkRect(Width, Height), Point(Left, Top), SkinData.FCacheBMP, True);

  SkinData.BGChanged := False;
  Result := True;
end;


destructor TacScrollPanelBand.Destroy;
begin
  inherited;
end;


procedure TacScrollPanelBand.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
  TextChanged;
end;


procedure TacScrollPanelBand.OurPaint(DC: HDC; SendUpdated: boolean);
var
  NewDC: HDC;
  R: TRect;
  b: boolean;
begin
  if (csDestroying in ComponentState) or
       (csCreating in Parent.ControlState) or
         not Assigned(SkinData) or not SkinData.Skinned then
    Exit;

  if not InUpdating(SkinData, True) then begin // We can draw if control is not in updating process
    if DC <> 0 then
      NewDC := DC
    else
      NewDC := Canvas.Handle;

    b := SkinData.HalfVisible or SkinData.BGChanged;
    if SkinData.RepaintIfMoved then begin
      GetClipBox(NewDC, R);
      SkinData.HalfVisible := (WidthOf(R) <> Width) or (HeightOf(R) <> Height)
    end
    else
      SkinData.HalfVisible := False;

    if b then
      PrepareCache; // Prepare image of control

    CopyWinControlCache(Self, SkinData, MkRect, MkRect(Width, Height), NewDC, True); // Out image to control device context
    sVCLUtils.PaintControls(NewDC, Self, True, Point(0, 0)); // Paint graphic controls
    if SendUpdated then
      SetParentUpdated(Self); // Send message to children who must be updated (background is ready now)
  end;
end;


procedure TacScrollPanelBand.PaintWindow(DC: HDC);
begin
  inherited;
  OurPaint(DC);
end;


procedure TacScrollPanelBand.WndProc(var Message: TMessage);
var
  SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct;
begin
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_GETAPPLICATION: begin
        Message.Result := ACNativeInt(Application);
        Exit;
      end;

      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end;

      AC_REMOVESKIN: begin
        ControlStyle := ControlStyle - [csOpaque];
        if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
          CommonMessage(Message, SkinData);
          Invalidate;
        end;
        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_SETNEWSKIN: begin
        ControlStyle := ControlStyle + [csOpaque];
        if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then
          CommonMessage(Message, SkinData);

        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_REFRESH: begin
        if (LongWord(Message.LParam) = LongWord(SkinData.SkinManager)) then begin
          CommonMessage(Message, SkinData);
          RedrawWindow(Handle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
        end;
        AlphaBroadCast(Self, Message);
        Exit;
      end;

      AC_GETBG: begin
        InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
        if (WidthOf(ClientRect) <> Width) and (PacBGInfo(Message.LParam)^.BgType = btCache) and not PacBGInfo(Message.LParam)^.PleaseDraw then begin
          inc(PacBGInfo(Message.LParam)^.Offset.X, BorderWidth + BevelWidth * (integer(BevelInner <> bvNone) + integer(BevelOuter <> bvNone)));
          inc(PacBGInfo(Message.LParam)^.Offset.Y, BorderWidth + BevelWidth * (integer(BevelInner <> bvNone) + integer(BevelOuter <> bvNone)));
        end;
        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not SkinData.Skinned then
    inherited
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE: begin
            if SkinData.Updating then begin // If control must be repainted after parent BG changing
              SkinData.Updating := False;
              RedrawWindow(Handle, nil, 0, RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_ERASE or RDW_FRAME);
              Exit;
            end;
            Exit;
          end;

          AC_PREPARECACHE: begin
            PrepareCache;
            CommonMessage(Message, SkinData);
            Exit;
          end;

          AC_PREPARING:  // If bkground is not ready then Result is 1
            Message.Result := integer(SkinData.BGChanged or SkinData.Updating)

          else
            if CommonMessage(Message, SkinData) then
              Exit;
        end;

      WM_PRINT: begin // Used for control drawing in before animation
        SkinData.Updating := False;
        if ControlIsReady(Self) then begin
          DC := TWMPaint(Message).DC;
          if SkinData.BGChanged then
            PrepareCache;

          OurPaint(DC, False);
        end;
        Exit;
      end;

      WM_PAINT: begin
        if (not Visible and not (csDesigning in ComponentState)) then begin
          inherited;
          Exit;
        end;
        ControlState := ControlState + [csCustomPaint];
        BeginPaint(Handle, PS);
        if TWMPaint(Message).DC = 0 then
          DC := GetDC(Handle)
        else
          DC := TWMPaint(Message).DC;

        try
          SaveIndex := SaveDC(DC);
          Canvas.Lock;
          try
            Canvas.Handle := DC;
            try
              TControlCanvas(Canvas).UpdateTextFlags;
              OurPaint(DC);
            finally
              Canvas.Handle := 0;
            end;
          finally
            Canvas.Unlock;
          end;
          RestoreDC(DC, SaveIndex);
        finally
          if TWMPaint(Message).DC = 0 then
            ReleaseDC(Handle, DC);

          EndPaint(Handle, PS);
        end;
        ControlState := ControlState - [csCustomPaint];
        Exit;
      end;

      WM_ERASEBKGND: begin
        Message.Result := 1;
        Exit;
      end;

      WM_MOVE:
        SkinData.BGChanged := True; // Full update of client after each panel scrolling

      CM_VISIBLECHANGED: begin
        SkinData.BGChanged := True;
        SkinData.Updating := False;
        inherited;
        Exit;
      end;

      WM_KILLFOCUS, WM_SETFOCUS: begin
        inherited;
        Exit;
      end;
    end;
    if CommonWndProc(Message, SkinData) then
      Exit; // Common handler for all controls in the package / sCommonData.pas

    inherited;
    case Message.Msg of
      CM_TEXTCHANGED: begin
        if Parent <> nil then
          SkinData.Invalidate;

        TextChanged;
        Exit;
      end;

      CM_ENABLEDCHANGED:
        SkinData.Invalidate;

      WM_SETFONT:
        if Caption <> '' then
          SkinData.Invalidate;
    end;
  end;
end;


procedure TacScrollPanel.WndProc(var Message: TMessage);
begin
  inherited;
end;


constructor TacScrollPanelBands.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  SkinData.SkinSection := s_CheckBox;
end;


procedure TacScrollPanelBand.CreateWnd;
begin
  inherited;
  TextChanged;
end;


procedure TacScrollPanelBands.WndProc(var Message: TMessage);
var
  R: TRect;
  DC : hdc;
begin
  inherited;
  case Message.Msg of
    WM_PAINT:
      if (csDesigning in ComponentState) and (ControlCount = 0) then begin
        DC := GetDC(Handle);
        try
          SetBkMode(DC, TRANSPARENT);
          R := ClientRect;
          DrawText(DC, PChar('Right click and choose "Add new panel"'), -1, R, DT_WORDBREAK);
        finally
          ReleaseDC(Handle, DC);
        end
      end;
  end;
end;


procedure TacScrollPanelBand.SetImageIndex(const Value: integer);
begin
  Button.ImageIndex := Value;
end;


procedure TacScrollPanelBand.SetImages(const Value: TCustomImageList);
begin
  Button.Images := Value;
end;


function TacScrollPanelBand.GetImageIndex: integer;
begin
  Result := Button.ImageIndex;
end;


function TacScrollPanelBand.GetImages: TCustomImageList;
begin
  Result := Button.Images;
end;


procedure TacBandBtn.Invalidate;
begin
  Spacing := SpeedSpacing;
  if (Parent <> nil) and (Width <> Parent.Width) then
    Width := Parent.Width;

  inherited;
end;


function TacScrollPanelBand.GetTitleHeight: integer;
begin
  Result := Button.Height;
end;


procedure TacScrollPanelBand.SetTitleHeight(const Value: integer);
begin
  Button.Height := Value;
end;

end.



