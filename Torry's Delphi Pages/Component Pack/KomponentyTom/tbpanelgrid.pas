{*******************************************************}
{                                                       }
{       Lista paneli typu TPanel                        }
{                                                       }
{       Copyright (c) 2002, 2003 Tomasz Bojara          }
{                                                       }
{*******************************************************}

unit tbpanelgrid;

{$R-,H+,X+}

interface

uses SysUtils, Windows, Messages, Classes, Controls, Forms,
  Graphics, Menus;

type

{ TCtrlGrid }

  TCtrlGrid = class;

  TCtrlGridLink = class
  private
    FCtrlGrid: TCtrlGrid;
    FListIndex: TList;
    FActiveRecord: Integer;
    FActive: Boolean;
    FBufferCount: Integer;
    procedure SetActiveRecord(const Value: Integer);
    function GetRecordCount: Integer;
  protected
    procedure ActiveChanged;
    procedure DataSetChanged;
  public
    constructor Create(CtrlGrid: TCtrlGrid);
    destructor Destroy; override;
    procedure Insert(AIndex: Integer);
    procedure Append;
    procedure Delete(AIndex: Integer);
    procedure DeleteAll;
    procedure EnableControls;
    procedure DisableControls;
    property ActiveRecord: Integer read FActiveRecord write SetActiveRecord;
    property RecordCount: Integer read GetRecordCount;
    property Active: Boolean read FActive;
    property BufferCount: Integer read FBufferCount write FBufferCount;
  end;

  TCtrlPanel = class(TWinControl)
  private
    FCtrlGrid: TCtrlGrid;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor CreateLinked(CtrlGrid: TCtrlGrid);
  end;

  TCtrlGridOrientation = (goVertical, goHorizontal);
  TCtrlGridBorder = (gbNone, gbRaised);
  TCtrlGridKey = (gkNull, gkPriorTab, gkNextTab, gkLeft,
    gkRight, gkUp, gkDown, gkScrollUp, gkScrollDown, gkPageUp, gkPageDown,
    gkHome, gkEnd);

  TPaintPanelEvent = procedure(CtrlGrid: TCtrlGrid;
    Index: Integer) of object;

  TCtrlGrid = class(TWinControl)
  private
    FDataLink: TCtrlGridLink;
    FPanel: TCtrlPanel;
    FCanvas: TCanvas;
    FColCount: Integer;
    FRowCount: Integer;
    FPanelWidth: Integer;
    FPanelHeight: Integer;
    FPanelIndex: Integer;
    FPanelCount: Integer;
    FBitmapCount: Integer;
    FPanelBitmap: HBitmap;
    FSaveBitmap: HBitmap;
    FPanelDC: HDC;
    FOrientation: TCtrlGridOrientation;
    FPanelBorder: TCtrlGridBorder;
    FAllowInsert: Boolean;
    FAllowDelete: Boolean;
    FShowFocus: Boolean;
    FFocused: Boolean;
    FClicking: Boolean;
    FSelColorChanged: Boolean;
    FScrollBarKind: Integer;
    FSelectedColor: TColor;
    FOnPaintPanel: TPaintPanelEvent;
    function AcquireFocus: Boolean;
    procedure AdjustSize; reintroduce;
    procedure CreatePanelBitmap;
    procedure DataSetChanged(Reset: Boolean);
    procedure DestroyPanelBitmap;
    procedure DrawPanel(DC: HDC; Index: Integer);
    procedure DrawPanelBackground(DC: HDC; const R: TRect; Erase, Selected: Boolean);
    function FindNext(StartControl: TWinControl; GoForward: Boolean;
     var WrapFlag: Integer): TWinControl;
    function GetPanelBounds(Index: Integer): TRect;
    function PointInPanel(const P: TSmallPoint): Boolean;
    procedure Reset;
    procedure Scroll(Inc: Integer; ScrollLock: Boolean);
    procedure ScrollMessage(var Message: TWMScroll);
    procedure SelectNext(GoForward: Boolean);
    procedure SetColCount(Value: Integer);
    procedure SetOrientation(Value: TCtrlGridOrientation);
    procedure SetPanelBorder(Value: TCtrlGridBorder);
    procedure SetPanelHeight(Value: Integer);
    procedure SetPanelIndex(Value: Integer);
    procedure SetPanelWidth(Value: Integer);
    procedure SetRowCount(Value: Integer);
    procedure SetSelectedColor(Value: TColor);
    procedure UpdateDataLinks(Control: TControl; Inserting: Boolean);
    procedure UpdateScrollBar;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetChildParent: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure PaintPanel(Index: Integer); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure ReadState(Reader: TReader); override;
    property Panel: TCtrlPanel read FPanel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoKey(Key: TCtrlGridKey);
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure GetTabOrderList(List: TList); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Canvas: TCanvas read FCanvas;
    property PanelCount: Integer read FPanelCount;
    property PanelIndex: Integer read FPanelIndex write SetPanelIndex;
    property DataLink: TCtrlGridLink read FDataLink;
  published
    property Align;
    property AllowDelete: Boolean read FAllowDelete write FAllowDelete default True;
    property AllowInsert: Boolean read FAllowInsert write FAllowInsert default True;
    property Anchors;
    property ColCount: Integer read FColCount write SetColCount default 1;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Orientation: TCtrlGridOrientation read FOrientation write SetOrientation default goVertical;
    property PanelBorder: TCtrlGridBorder read FPanelBorder write SetPanelBorder default gbRaised;
    property PanelHeight: Integer read FPanelHeight write SetPanelHeight default 72;
    property PanelWidth: Integer read FPanelWidth write SetPanelWidth default 200;
    property ParentColor;                                                                      
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property RowCount: Integer read FRowCount write SetRowCount default 3;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor
      stored FSelColorChanged default clWindow;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default True;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaintPanel: TPaintPanelEvent read FOnPaintPanel write FOnPaintPanel;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses DBConsts, VDBConsts;

procedure Register;
begin
  RegisterComponents('TOM', [TCtrlGrid]);
end;

{ TDBCtrlGridLink }

constructor TCtrlGridLink.Create(CtrlGrid: TCtrlGrid);
begin
  inherited Create;
  FActiveRecord:= -1;
  FListIndex:= TList.Create;
  FCtrlGrid := CtrlGrid;
  RPR;
  FActive:= True;
end;

procedure TCtrlGridLink.ActiveChanged;
begin
  FCtrlGrid.DataSetChanged(False);
end;

procedure TCtrlGridLink.DataSetChanged;
begin
  FCtrlGrid.DataSetChanged(False);
end;

{ TDBCtrlPanel }

constructor TCtrlPanel.CreateLinked(CtrlGrid: TCtrlGrid);
begin
  inherited Create(CtrlGrid);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque, csReplicatable];
  FCtrlGrid := CtrlGrid;
  Parent := CtrlGrid;
end;

procedure TCtrlPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCtrlPanel.PaintWindow(DC: HDC);
var
  R: TRect;
  Selected: Boolean;
begin
  with FCtrlGrid do
  begin
    if FDataLink.Active then
    begin
      Selected := (FDataLink.ActiveRecord = FPanelIndex);
      DrawPanelBackground(DC, Self.ClientRect, True, Selected);
      FCanvas.Handle := DC;
      try
        FCanvas.Font := Font;
        FCanvas.Brush.Style := bsSolid;
        FCanvas.Brush.Color := Color;
        PaintPanel(FDataLink.ActiveRecord);
        if FShowFocus and FFocused and Selected then
        begin
          R := Self.ClientRect;
          if FPanelBorder = gbRaised then InflateRect(R, -2, -2);
          FCanvas.Brush.Color := Color;
          FCanvas.DrawFocusRect(R);
        end;
      finally
        FCanvas.Handle := 0;
      end;
    end else
      DrawPanelBackground(DC, Self.ClientRect, True, csDesigning in ComponentState);
  end;
end;

procedure TCtrlPanel.CMControlListChange(var Message: TCMControlListChange);
begin
  FCtrlGrid.UpdateDataLinks(Message.Control, Message.Inserting);
end;

procedure TCtrlPanel.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;
begin
  if Tag <> 0 then Exit;
  if Message.DC = 0 then
  begin
    FCtrlGrid.CreatePanelBitmap;
    try
      Message.DC := FCtrlGrid.FPanelDC;
      PaintHandler(Message);
      Message.DC := 0;
      DC := BeginPaint(Handle, PS);
      BitBlt(DC, 0, 0, Width, Height, FCtrlGrid.FPanelDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      FCtrlGrid.DestroyPanelBitmap;
    end;
  end else
    PaintHandler(Message);
end;

procedure TCtrlPanel.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if csDesigning in ComponentState then
    Message.Result := HTCLIENT else
    Message.Result := HTTRANSPARENT;
end;

procedure TCtrlPanel.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

{ TDBCtrlGrid }

constructor TCtrlGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csDoubleClicks];
  TabStop := True;
  FDataLink := TCtrlGridLink.Create(Self);
  FCanvas := TCanvas.Create;
  FPanel := TCtrlPanel.CreateLinked(Self);
  FColCount := 1;
  FRowCount := 3;
  FPanelWidth := 200;
  FPanelHeight := 72;
  FPanelBorder := gbRaised;
  FAllowInsert := True;
  FAllowDelete := True;
  FShowFocus := True;
  FSelectedColor := Color;
  AdjustSize;
  FDataLink.ActiveChanged;
end;

destructor TCtrlGrid.Destroy;
begin
  FCanvas.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

function TCtrlGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if not Focused then
  begin
    SetFocus;
    Result := Focused;
  end;
end;

procedure TCtrlGrid.AdjustSize;
var
  W, H: Integer;
begin
  W := FPanelWidth * FColCount;
  H := FPanelHeight * FRowCount;
  if FOrientation = goVertical then
    Inc(W, GetSystemMetrics(SM_CXVSCROLL)) else
    Inc(H, GetSystemMetrics(SM_CYHSCROLL));
  SetBounds(Left, Top, W, H);
  Reset;
end;

procedure TCtrlGrid.CreatePanelBitmap;
var
  DC: HDC;
begin
  if FBitmapCount = 0 then
  begin
    DC := GetDC(0);
    FPanelBitmap := CreateCompatibleBitmap(DC, FPanel.Width, FPanel.Height);
    ReleaseDC(0, DC);
    FPanelDC := CreateCompatibleDC(0);
    FSaveBitmap := SelectObject(FPanelDC, FPanelBitmap);
  end;
  Inc(FBitmapCount);
end;

procedure TCtrlGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCtrlGrid.CreateWnd;
begin
  inherited CreateWnd;
  if FOrientation = goVertical then
    FScrollBarKind := SB_VERT else
    FScrollBarKind := SB_HORZ;
  if not FDataLink.Active then
    SetScrollRange(Handle, FScrollBarKind, 0, 4, False);
    UpdateScrollBar;
end;

procedure TCtrlGrid.DataSetChanged(Reset: Boolean);
var
  NewPanelIndex, NewPanelCount: Integer;
  FocusedControl: TWinControl;
  R: TRect;
begin
  if csDesigning in ComponentState then
  begin
    NewPanelIndex := 0;
    NewPanelCount := 1;
  end else
    if FDataLink.Active then
    begin
      NewPanelIndex := FDataLink.ActiveRecord;
      NewPanelCount := FDataLink.RecordCount;
//      if NewPanelCount = 0 then NewPanelCount := 1;
    end else
    begin
      NewPanelIndex := 0;
      NewPanelCount := 0;
    end;
  FocusedControl := nil;
  R := GetPanelBounds(NewPanelIndex);
  if Reset or not HandleAllocated then FPanel.BoundsRect := R else
  begin
    FocusedControl := FindControl(GetFocus);
    if (FocusedControl <> FPanel) and FPanel.ContainsControl(FocusedControl) then
      FPanel.SetFocus else
      FocusedControl := nil;
    if NewPanelIndex <> FPanelIndex then
    begin
      SetWindowPos(FPanel.Handle, 0, R.Left, R.Top, R.Right - R.Left,
        R.Bottom - R.Top, SWP_NOZORDER or SWP_NOREDRAW);
      RedrawWindow(FPanel.Handle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN);
    end;
  end;
  FPanelIndex := NewPanelIndex;
  FPanelCount := NewPanelCount;
  FPanel.Visible := FPanelCount > 0;
  FPanel.Invalidate;
  if not Reset then
  begin
    Invalidate;
//    Update;
  end;
//  UpdateScrollBar;
  if (FocusedControl <> nil) and not FClicking and FocusedControl.CanFocus then
    FocusedControl.SetFocus;
end;

procedure TCtrlGrid.DestroyPanelBitmap;
begin
  Dec(FBitmapCount);
  if FBitmapCount = 0 then
  begin
    SelectObject(FPanelDC, FSaveBitmap);
    DeleteDC(FPanelDC);
    DeleteObject(FPanelBitmap);
  end;
end;

procedure TCtrlGrid.DoKey(Key: TCtrlGridKey);
var
  HInc, VInc: Integer;
begin
  if FDataLink.Active then
  begin
    if FOrientation = goVertical then
    begin
      HInc := 1;
      VInc := FColCount;
    end else
    begin
      HInc := FRowCount;
      VInc := 1;
    end;
    case Key of
      gkPriorTab: SelectNext(False);
      gkNextTab: SelectNext(True);
      gkLeft: Scroll(-HInc, False);
      gkRight: Scroll(HInc, False);
      gkUp: Scroll(-VInc, False);
      gkDown: Scroll(VInc, False);
      gkScrollUp: Scroll(-VInc, True);
      gkScrollDown: Scroll(VInc, True);
      gkPageUp: Scroll(-FDataLink.BufferCount, True);
      gkPageDown: Scroll(FDataLink.BufferCount, True);
//      gkHome: First;
//      gkEnd: Last;
    end;
  end;
end;

procedure TCtrlGrid.DrawPanel(DC: HDC; Index: Integer);
var
  SaveActive: Integer;
  R: TRect;
begin
  R := GetPanelBounds(Index);
  if Index < FPanelCount then
  begin
    SaveActive := FDataLink.ActiveRecord;
    FDataLink.ActiveRecord := Index;
    FPanel.PaintTo(FPanelDC, 0, 0);
    FDataLink.ActiveRecord := SaveActive;
  end else
    DrawPanelBackground(FPanelDC, FPanel.ClientRect, True, False);
  BitBlt(DC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
    FPanelDC, 0, 0, SRCCOPY);
end;

procedure TCtrlGrid.DrawPanelBackground(DC: HDC; const R: TRect;
  Erase, Selected: Boolean);
var
  Brush: HBrush;
begin
  if Erase then
  begin
    if Selected then FPanel.Color := FSelectedColor
    else FPanel.Color := Color;
    Brush := CreateSolidBrush(ColorToRGB(FPanel.Color));
    FillRect(DC, R, Brush);
    DeleteObject(Brush);
  end;
  if FPanelBorder = gbRaised then
    DrawEdge(DC, PRect(@R)^, BDR_RAISEDINNER, BF_RECT);
end;

function TCtrlGrid.GetChildParent: TComponent;
begin
  Result := FPanel;
end;

procedure TCtrlGrid.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FPanel.GetChildren(Proc, Root);
end;

function TCtrlGrid.GetPanelBounds(Index: Integer): TRect;
var
  Col, Row: Integer;
begin
  if FOrientation = goVertical then
  begin
    Col := Index mod FColCount;
    Row := Index div FColCount;
  end else
  begin
    Col := Index div FRowCount;
    Row := Index mod FRowCount;
  end;
  Result.Left := FPanelWidth * Col;
  Result.Top := FPanelHeight * Row;
  Result.Right := Result.Left + FPanelWidth;
  Result.Bottom := Result.Top + FPanelHeight;
end;

procedure TCtrlGrid.GetTabOrderList(List: TList);
begin
end;

procedure TCtrlGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  GridKey: TCtrlGridKey;
begin
  inherited KeyDown(Key, Shift);
  GridKey := gkNull;
  case Key of
    VK_LEFT: GridKey := gkLeft;
    VK_RIGHT: GridKey := gkRight;
    VK_UP: GridKey := gkUp;
    VK_DOWN: GridKey := gkDown;
    VK_PRIOR: GridKey := gkPageUp;
    VK_NEXT: GridKey := gkPageDown;
    VK_HOME: GridKey := gkHome;
    VK_END: GridKey := gkEnd;
  end;
  DoKey(GridKey);
end;

procedure TCtrlGrid.PaintWindow(DC: HDC);
var
  I: Integer;
  Brush: HBrush;
begin
  if csDesigning in ComponentState then
  begin
    FPanel.Update;
    Brush := CreateHatchBrush(HS_BDIAGONAL, ColorToRGB(clBtnShadow));
    SetBkColor(DC, ColorToRGB(Color));
    FillRect(DC, ClientRect, Brush);
    DeleteObject(Brush);
    for I := 1 to FColCount * FRowCount - 1 do
      DrawPanelBackground(DC, GetPanelBounds(I), False, False);
  end else
  begin
    CreatePanelBitmap;
    try
      for I := 0 to FColCount * FRowCount - 1 do
        if (FPanelCount <> 0) and (I = FPanelIndex) then
          FPanel.Update else
          DrawPanel(DC, I);
    finally
      DestroyPanelBitmap;
    end;
  end;
  { When width or height are not evenly divisible by panel size, fill the gaps }
  if HandleAllocated then
  begin
    if (Height <> FPanel.Height * FRowCount) then
    begin
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(DC, Rect(0, FPanel.Height * FRowCount, Width, Height), Brush);
      DeleteObject(Brush);
    end;
    if (Width <> FPanel.Width * FColCount) then
    begin
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(DC, Rect(FPanelWidth * FColCount, 0, Width, Height), Brush);
      DeleteObject(Brush);
    end;
  end;
end;

procedure TCtrlGrid.PaintPanel(Index: Integer);
begin
  if Assigned(FOnPaintPanel) then FOnPaintPanel(Self, Index);
end;

function TCtrlGrid.PointInPanel(const P: TSmallPoint): Boolean;
begin
  Result := (FPanelCount > 0) and PtInRect(GetPanelBounds(FPanelIndex),
    SmallPointToPoint(P));
end;

procedure TCtrlGrid.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  FPanel.FixupTabList;
end;

procedure TCtrlGrid.Reset;
begin
  if csDesigning in ComponentState then
    FDataLink.BufferCount := 1 else
    FDataLink.BufferCount := FColCount * FRowCount;
  DataSetChanged(True);
end;

procedure TCtrlGrid.Scroll(Inc: Integer; ScrollLock: Boolean);
var
  NewIndex: Integer;
begin
  if FDataLink.Active and (Inc <> 0) then
  with FDataLink do
  begin
    DisableControls;
    try
      NewIndex:= ActiveRecord + Inc;
      if NewIndex < 0 then NewIndex:= 0
      else
      if NewIndex > RecordCount - 1 then NewIndex:= RecordCount - 1;
      ActiveRecord:= NewIndex;
    finally
      EnableControls;
    end;
  end;
end;
      
procedure TCtrlGrid.ScrollMessage(var Message: TWMScroll);
var
  Key: TCtrlGridKey;
  SI: TScrollInfo;
begin
  if AcquireFocus then
  begin
    Key := gkNull;
    case Message.ScrollCode of
      SB_LINEUP: Key := gkScrollUp;
      SB_LINEDOWN: Key := gkScrollDown;
      SB_PAGEUP: Key := gkPageUp;
      SB_PAGEDOWN: Key := gkPageDown;
      SB_TOP: Key := gkHome;
      SB_BOTTOM: Key := gkEnd;
//      SB_THUMBPOSITION:
//        if FDataLink.Active and FDataLink.DataSet.IsSequenced then
//        begin
//          SI.cbSize := sizeof(SI);
//          SI.fMask := SIF_ALL;
//          GetScrollInfo(Self.Handle, FScrollBarKind, SI);
//          if SI.nTrackPos <= 1 then Key := gkHome
//          else if SI.nTrackPos >= FDataLink.DataSet.RecordCount then Key := gkEnd
//          else
//          begin
//            FDataLink.DataSet.RecNo := SI.nTrackPos;
//            Exit;
//          end;
//        end else
//        begin
//          case Message.Pos of
//            0: Key := gkHome;
//            1: Key := gkPageUp;
//            3: Key := gkPageDown;
//            4: Key := gkEnd;
//          end;
//        end;
    end;
    DoKey(Key);
  end;
end;

function TCtrlGrid.FindNext(StartControl: TWinControl; GoForward: Boolean;
  var WrapFlag: Integer): TWinControl;
var
  I, StartIndex: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    StartIndex := 0;
    I := 0;
    Result := StartControl;
    FPanel.GetTabOrderList(List);
    if List.Count > 0 then
    begin
      StartIndex := List.IndexOf(StartControl);
      if StartIndex = -1 then
        if GoForward then
          StartIndex := List.Count - 1 else
          StartIndex := 0;
      I := StartIndex;
      repeat
        if GoForward then
        begin
          Inc(I);
          if I = List.Count then I := 0;
        end else
        begin
          if I = 0 then I := List.Count;
          Dec(I);
        end;
        Result := List[I];
      until (Result.CanFocus and Result.TabStop) or (I = StartIndex);
    end;
    WrapFlag := 0;
    if GoForward then
    begin
      if I <= StartIndex then WrapFlag := 1;
    end else
    begin
      if I >= StartIndex then WrapFlag := -1;
    end;
  finally
    List.Free;
  end;
end;

procedure TCtrlGrid.SelectNext(GoForward: Boolean);
var
  WrapFlag: Integer;
  ParentForm: TCustomForm;
  ActiveControl, Control: TWinControl;
begin
  ParentForm := GetParentForm(Self);
  if ParentForm <> nil then
  begin
    ActiveControl := ParentForm.ActiveControl;
    if ContainsControl(ActiveControl) then
    begin
      Control := FindNext(ActiveControl, GoForward, WrapFlag);
//      if not (FDataLink.DataSet.State in dsEditModes) then
        FPanel.SetFocus;
      try
        if WrapFlag <> 0 then Scroll(WrapFlag, False);
      except
        ActiveControl.SetFocus;
        raise;
      end;
      if not Control.CanFocus then
        Control := FindNext(Control, GoForward, WrapFlag);
      Control.SetFocus;
    end;
  end;
end;

procedure TCtrlGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  ScrollWidth, ScrollHeight, NewPanelWidth, NewPanelHeight: Integer;
begin
  ScrollWidth := 0;
  ScrollHeight := 0;
  if FOrientation = goVertical then
    ScrollWidth := GetSystemMetrics(SM_CXVSCROLL) else
    ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);
  NewPanelWidth := (AWidth - ScrollWidth) div FColCount;
  NewPanelHeight := (AHeight - ScrollHeight) div FRowCount;
  if NewPanelWidth < 1 then NewPanelWidth := 1;
  if NewPanelHeight < 1 then NewPanelHeight := 1;
  if (FPanelWidth <> NewPanelWidth) or (FPanelHeight <> NewPanelHeight) then
  begin
    FPanelWidth := NewPanelWidth;
    FPanelHeight := NewPanelHeight;
    Reset;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TCtrlGrid.SetColCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  if FColCount <> Value then
  begin
    FColCount := Value;
    AdjustSize;
  end;
end;

procedure TCtrlGrid.SetOrientation(Value: TCtrlGridOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    RecreateWnd;
    AdjustSize;
  end;
end;

procedure TCtrlGrid.SetPanelBorder(Value: TCtrlGridBorder);
begin
  if FPanelBorder <> Value then
  begin
    FPanelBorder := Value;
    Invalidate;
    FPanel.Invalidate;
  end;
end;

procedure TCtrlGrid.SetPanelHeight(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 65535 then Value := 65535;
  if FPanelHeight <> Value then
  begin
    FPanelHeight := Value;
    AdjustSize;
  end;
end;

procedure TCtrlGrid.SetPanelIndex(Value: Integer);
begin
  if FDataLink.Active and (Value < PanelCount) then FDataLink.ActiveRecord:= Value;
end;

procedure TCtrlGrid.SetPanelWidth(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 65535 then Value := 65535;
  if FPanelWidth <> Value then
  begin
    FPanelWidth := Value;
    AdjustSize;
  end;
end;

procedure TCtrlGrid.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  if FRowCount <> Value then
  begin
    FRowCount := Value;
    AdjustSize;
  end;
end;

procedure TCtrlGrid.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
    FSelColorChanged := Value <> Color;
    Invalidate;
    FPanel.Invalidate;
  end;
end;

procedure TCtrlGrid.UpdateDataLinks(Control: TControl; Inserting: Boolean);
//var
//  I: Integer;
//  DataLink: TDataLink;
begin
//  if Inserting and not (csReplicatable in Control.ControlStyle) then
//    DatabaseError(SNotReplicatable);
//  DataLink := TDataLink(Control.Perform(CM_GETDATALINK, 0, 0));
//  if DataLink <> nil then
//  begin
//    DataLink.DataSourceFixed := False;
//    if Inserting then
//    begin
//      DataLink.DataSource := DataSource;
//      DataLink.DataSourceFixed := True;
//    end;
//  end;
//  if Control is TWinControl then
//    with TWinControl(Control) do
//      for I := 0 to ControlCount - 1 do
//        UpdateDataLinks(Controls[I], Inserting);
end;

procedure TCtrlGrid.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
begin
  if FDatalink.Active and HandleAllocated then
    with FDatalink do
    begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, FScrollBarKind, SIOld);
      SINew := SIOld;
      if True then
      begin
        SINew.nMin := 1;
        SINew.nPage := Self.RowCount * Self.ColCount;
        SINew.nMax := DWORD(RecordCount) + SINew.nPage - 1;
//        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := ActiveRecord;
      end
      else
      begin
//        SINew.nMin := 0;
//        SINew.nPage := 0;
//        SINew.nMax := 4;
//        if BOF then SINew.nPos := 0
//        else if EOF then SINew.nPos := 4
//        else SINew.nPos := 2;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, FScrollBarKind, SINew, True);
    end;
end;

procedure TCtrlGrid.WMLButtonDown(var Message: TWMLButtonDown);
var
  I: Integer;
  P: TPoint;
  Window: HWnd;
begin
  if FDataLink.Active then
  begin
    P := SmallPointToPoint(Message.Pos);
    for I := 0 to FPanelCount - 1 do
      if (I <> FPanelIndex) and PtInRect(GetPanelBounds(I), P) then
      begin
        FClicking := True;
        try
          SetPanelIndex(I);
        finally
          FClicking := False;
        end;
        P := ClientToScreen(P);
        Window := WindowFromPoint(P);
        if IsChild(FPanel.Handle, Window) then
        begin
          Windows.ScreenToClient(Window, P);
          Message.Pos := PointToSmallPoint(P);
          with TMessage(Message) do SendMessage(Window, Msg, WParam, LParam);
          Exit;
        end;
        Break;
      end;
  end;
  if AcquireFocus then
  begin
    if PointInPanel(Message.Pos) then
    begin
//      EditMode := False;
      Click;
    end;
    inherited;
  end;
end;

procedure TCtrlGrid.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if PointInPanel(Message.Pos) then DblClick;
  inherited;
end;

procedure TCtrlGrid.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(Message);
end;

procedure TCtrlGrid.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message);
end;

procedure TCtrlGrid.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TCtrlGrid.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCtrlGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocused := True;
  FPanel.Repaint;
end;

procedure TCtrlGrid.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocused := False;
  FPanel.Repaint;
end;

procedure TCtrlGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TCtrlGrid.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

function GetShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

procedure TCtrlGrid.CMChildKey(var Message: TCMChildKey);
var
  ShiftState: TShiftState;
  GridKey: TCtrlGridKey;
begin
  with Message do
    if Sender <> Self then
    begin
      ShiftState := GetShiftState;
      if Assigned(OnKeyDown) then OnKeyDown(Sender, CharCode, ShiftState);
      GridKey := gkNull;
      case CharCode of
        VK_TAB:
          if not (ssCtrl in ShiftState) and
            (Sender.Perform(WM_GETDLGCODE, 0, 0) and DLGC_WANTTAB = 0) then
            if ssShift in ShiftState then
              GridKey := gkPriorTab else
              GridKey := gkNextTab;
      end;
      if GridKey <> gkNull then
      begin
        DoKey(GridKey);
        Result := 1;
        Exit;
      end;
    end;
  inherited;
end;

procedure TCtrlGrid.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if not FSelColorChanged then
    FSelectedColor := Color;
end;

{ Defer action processing to datalink }

function TCtrlGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.FCtrlGrid.ExecuteAction(Action);
end;

function TCtrlGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.FCtrlGrid.UpdateAction(Action);
end;

procedure TCtrlGridLink.Append;
begin
  if FListIndex.Count = 0 then
    Insert(0)
  else
    Insert(FListIndex.Count - 1);
end;

procedure TCtrlGridLink.Delete(AIndex: Integer);
begin
  FListIndex.Delete(AIndex);
  DataSetChanged;  
end;

procedure TCtrlGridLink.DeleteAll;
begin
  FListIndex.Clear;
  DataSetChanged;  
end;

procedure TCtrlGridLink.Insert(AIndex: Integer);
begin
  FListIndex.Insert(AIndex, Pointer(AIndex));
  DataSetChanged;  
end;

procedure TCtrlGridLink.SetActiveRecord(const Value: Integer);
begin
  if FActiveRecord <> Value then
  begin
    FActiveRecord := Value;
    if FCtrlGrid.Tag = 0 then DataSetChanged;
  end;
end;

function TCtrlGridLink.GetRecordCount: Integer;
begin
  Result:= FListIndex.Count;
end;

destructor TCtrlGridLink.Destroy;
begin
  FActive:= False;
  FListIndex.Free;
  inherited;
end;

procedure TCtrlGridLink.DisableControls;
begin
  FCtrlGrid.Tag := 1;
end;

procedure TCtrlGridLink.EnableControls;
begin
  FCtrlGrid.Tag := 0;
end;

end.


