
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit PaneCtrls;

interface

{$I STD.INC}

uses
  Classes, Controls, Forms, Windows, Messages, Graphics, ImgList, GraphTools;

{ TPaneSheet }

type
  TCustomPaneControl = class;

  TPaneSheet = class(TCustomControl)
  private
    FOpened: Boolean;
    FData: Pointer;
    FImageIndex: Integer;
    FPaneControl: TCustomPaneControl;
    FPaneIndex: Integer;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    procedure SetImageIndex(Value: Integer);
    procedure SetOpened(Value: Boolean);
    function GetPaneIndex: Integer;
    procedure SetPaneIndex(Value: Integer);
    procedure SetPaneControl(Value: TCustomPaneControl);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    procedure Loaded; override;
    procedure Open;
    procedure Close;
    property Data: Pointer read FData write FData;
  published
    property Caption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Opened: Boolean read FOpened write SetOpened;
    property PaneIndex: Integer read GetPaneIndex write SetPaneIndex;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write
      FOnCloseQuery;
    property PaneControl: TCustomPaneControl read FPaneControl write SetPaneControl;
  end;

{ TCustomPaneControl }

  TPaneChangeEvent = procedure(Sender: TObject; OldPane, NewPane: TPaneSheet) of object;

  TPaneButton = (pbNone, pbLeftArrow, pbRightArrow, pbClose);

  TCustomPaneControl = class(TCustomControl)
  private
    FActivePane: TPaneSheet;
    FBorderStyle: TBorderStyle;
    FButton: TPaneButton;
    FCanUpdate: Boolean;
    FChangeLink: TChangeLink;
    FHeaderHeight: Integer;
    FImages: TCustomImageList;
    FLeftIndex: Integer;
    FOpenCount: Integer;
    FPanes: TList;
    FTimer: Integer;
    FOnCloseClick: TNotifyEvent;
    FOnPaneChange: TPaneChangeEvent;
    procedure ImagesChange(Sender: TObject);
    procedure UpdateDisplay;
    procedure SetActivePane(Value: TPaneSheet);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButton(Value: TPaneButton);
    function GetButtonRect(Index: TPaneButton): TRect;
    procedure SetImages(Value: TCustomImageList);
    procedure SetLeftIndex(Value: Integer);
    function GetPanes(Index: Integer): TPaneSheet;
    function GetPaneCount: Integer;
    function GetPaneRect(Index: Integer): TRect;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure ButtonClick(Button: TPaneButton); dynamic;
    function ButtonFromPoint(X, Y: Integer): TPaneButton;
    procedure CreateWnd; override;
    function GetClientRect: TRect; override;
    procedure NextPane;
    procedure PriorPane;
    function PaneFromPoint(X, Y: Integer): TPaneSheet;
    procedure Paint; override;
    procedure InsertPane(PaneSheet: TPaneSheet); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure RemovePane(PaneSheet: TPaneSheet); dynamic;
    property ActivePane: TPaneSheet read FActivePane write SetActivePane;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Button: TPaneButton read FButton write SetButton;
    property ButtonRect[Index: TPaneButton]: TRect read GetButtonRect;
    property Images: TCustomImageList read FImages write SetImages;
    property Panes[Index: Integer]: TPaneSheet read GetPanes;
    property PaneCount: Integer read GetPaneCount;
    property PaneRect[Index: Integer]: TRect read GetPaneRect;
    property LeftIndex: Integer read FLeftIndex write SetLeftIndex;
    property OnPaneChange: TPaneChangeEvent read FOnPaneChange write FOnPaneChange;
    property OnCloseClick: TNotifyEvent read FOnCloseClick write FOnCloseClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPane: TPaneSheet;
  end;

{ TPaneControl }

  TPaneControl = class(TCustomPaneControl)
  public
    property Panes;
    property LeftIndex;
    property PaneCount;
  published
    property ActivePane;
    property Align;
    property Anchors;
    property BorderStyle;
    property Font;
    property Images;
    property OnPaneChange;
    property OnCloseClick;
  end;

implementation

{ TPaneSheet }

constructor TPaneSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  Color := clBtnFace;
  ControlStyle := ControlStyle + [csAcceptsControls];
  ParentColor := False;
  Canvas.Brush.Color := clBtnFace;
  FOpened := True;
end;

destructor TPaneSheet.Destroy;
begin
	PaneControl := nil;
  inherited Destroy;
end;

function SortPaneSheets(Item1, Item2: Pointer): Integer;
var
	A: TPaneSheet absolute Item1;
	B: TPaneSheet absolute Item2;
begin
  Result := A.FPaneIndex - B.FPaneIndex;
end;

procedure TPaneSheet.Loaded;
begin
	inherited Loaded;
  if FPaneControl <> nil then
  begin
  	Visible := FPaneControl.ActivePane = Self;
    FPaneControl.FPanes.Sort(SortPaneSheets);
  end;
end;

procedure TPaneSheet.Open;
begin
  Opened := True;
end;

procedure TPaneSheet.Close;
begin
  Opened := False;
end;

procedure TPaneSheet.SetImageIndex(Value: Integer);
begin
  { TODO: Invalidate parent tab }
end;

procedure TPaneSheet.SetOpened(Value: Boolean);
var
  CanClose: Boolean;
  Action: TCloseAction;
begin
  if Value <> FOpened then
  begin
    CanClose := not Value;
    if CanClose and Assigned(FOnCloseQuery) then
      FOnCloseQuery(Self, CanClose);
    FOpened := not CanClose;
    if not FOpened then
    begin
      Action := caHide;
      if Assigned(FOnClose) then
        FOnClose(Self, Action);
      FOpened := Action = caNone;
    end;
    if (FPaneControl <> nil) then
    begin
      if FOpened then
      begin
      	Visible := True;
        FPaneControl.ActivePane := Self;
        Inc(FPaneControl.FOpenCount);
      end
      else
      begin
        Dec(FPaneControl.FOpenCount);
        FPaneControl.NextPane;
        Visible := False;
      end;
      FPaneControl.UpdateDisplay;
    end;
    if Action = caFree then
      Destroy;
  end;
end;

function TPaneSheet.GetPaneIndex: Integer;
begin
  Result := -1;
  if FPaneControl <> nil then
	  Result := FPaneControl.FPanes.IndexOf(Self);
end;

procedure TPaneSheet.SetPaneIndex(Value: Integer);
begin
	if csLoading in ComponentState then
		FPaneIndex := Value
  else if FPaneControl <> nil then
  begin
	 	FPaneControl.FPanes.Move(PaneIndex, Value);
    FPaneControl.Invalidate;
  end;
end;

procedure TPaneSheet.SetPaneControl(Value: TCustomPaneControl);
begin
	if Value <> FPaneControl then
  begin
  	if FPaneControl <> nil then
	    FPaneControl.RemovePane(Self);
    FPaneControl := Value;
  	if FPaneControl <> nil then
    begin
	    FPaneControl.InsertPane(Self);
      Inc(FPaneControl.FOpenCount);
    end;
  end;
end;

procedure TPaneSheet.CMTextChanged(var Message: TMessage);
begin
  if FPaneControl <> nil then
    FPaneControl.UpdateDisplay;
end;

{ TCustomPaneControl }

var
	BitmapBrush: TBitmap;

function CheckeredBitmap: TBitmap;
const
	BackColor: TColor = 0;
  ForeColor: TColor = 0;
begin
	if (BitmapBrush = nil) or (BackColor <> ColorToRGB(clWindow)) or
  	(ForeColor <> ColorToRGB(clInfoBk)) then
  begin
  	BackColor := ColorToRGB(clWindow);
    ForeColor := ColorToRGB(clInfoBk);
    BitmapBrush.Free;
	  BitmapBrush := GetBitmap(BackColor, ForeColor);
  end;
  Result := BitmapBrush;
end;

constructor TCustomPaneControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 200;
  Width := 200;
  FLeftIndex := -1;
  FBorderStyle := bsSingle;
  FCanUpdate := True;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
  FPanes := TList.Create;
end;

destructor TCustomPaneControl.Destroy;
var
  I: Integer;
begin
  FChangeLink.Free;
  FCanUpdate := False;
  for I := FPanes.Count -1 downto 0 do
    RemovePane(TPaneSheet(FPanes[I]));
  FPanes.Free;
  inherited Destroy;
end;

procedure TCustomPaneControl.ButtonClick(Button: TPaneButton);
begin
  case Button of
    pbLeftArrow:
      LeftIndex := LeftIndex - 1;
    pbRightArrow:
      LeftIndex := LeftIndex + 1;
    pbClose:
      begin
        if ActivePane <> nil then
          ActivePane.Close
        else
        if Assigned(FOnCloseClick) then
          FOnCloseClick(Self);
      end;
  end;
end;

function TCustomPaneControl.ButtonFromPoint(X, Y: Integer): TPaneButton;
var
  I: TPaneButton;
begin
  Result := pbNone;
  for I := Low(TPaneButton) to High(TPaneButton) do
    if PtInRect(ButtonRect[I], Point(X, Y)) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TCustomPaneControl.WMTimer(var Message: TWMTimer);
var
  PaneSheet: TPaneSheet;
begin
  if Message.TimerID = 1 then
  begin
    KillTimer(Handle, FTimer);
    PaneSheet := ActivePane;
    ActivePane := nil;
    ActivePane := PaneSheet;
  end;
  Message.Result := 0;
end;

procedure TCustomPaneControl.CreateWnd;
var
  Message: TMessage;
begin
  inherited CreateWnd;
  CMFontChanged(Message);
  UpdateDisplay;
  FTimer := SetTimer(Handle, 1, 10, nil);
end;

function TCustomPaneControl.GetClientRect: TRect;
begin
	Result := inherited GetClientRect;
  Inc(Result.Top, FHeaderHeight + 1);
  InflateRect(Result, -2, -2);
end;

procedure TCustomPaneControl.NextPane;
var
  NewPane: TPaneSheet;
  I: Integer;
begin
   if ActivePane <> nil then
   begin
     NewPane := nil;
     for I := ActivePane.PaneIndex + 1 to FPanes.Count - 1 do
       if Panes[I].Opened then
       begin
         NewPane := Panes[I];
         Break;
       end;
     if NewPane = nil then
	     for I := 0 to ActivePane.PaneIndex do
       if Panes[I].Opened then
       begin
         NewPane := Panes[I];
         Break;
       end;
	    ActivePane := NewPane;
   end;
end;

procedure TCustomPaneControl.PriorPane;
var
  NewPane: TPaneSheet;
  I: Integer;
begin
   if ActivePane <> nil then
   begin
     NewPane := nil;
     for I := ActivePane.PaneIndex - 1 downto 0 do
       if Panes[I].Opened then
       begin
         NewPane := Panes[I];
         Break;
       end;
     if NewPane = nil then
	     for I := FPanes.Count - 1 downto ActivePane.PaneIndex do
       if Panes[I].Opened then
       begin
         NewPane := Panes[I];
         Break;
       end;
     ActivePane := NewPane;
   end;
end;

function TCustomPaneControl.PaneFromPoint(X, Y: Integer): TPaneSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PaneCount - 1 do
    if Panes[I].Opened and PtInRect(PaneRect[I], Point(X, Y)) then
    begin
      Result := Panes[I];
      Break;
    end;
end;

procedure TCustomPaneControl.Paint;
var
  MaxRight: Integer;

  procedure DrawButton(DC: HDC; PaneButton: TPaneButton);
  var
    Rect: TRect;
    State: TDrawState;
    Delta: Integer;
  begin
  	State := [];
    if FActivePane = nil then
    	State := [dsDisabled]
  	else if (PaneButton = pbLeftArrow) and (LeftIndex = 0) then
    	State := [dsDisabled]
  	else if (PaneButton = pbRightArrow) and (LeftIndex >= FOpenCount - 1) then
    	State := [dsDisabled];
    Rect := ButtonRect[PaneButton];
    Delta := 0;
    if (PaneButton = Button) and (State * [dsDisabled] = []) then
      if MouseCapture then
      begin
      	State := State + [dsPressed];
				DrawThemeThinButton(DC, Rect, State);
        Delta := 1;
      end
      else
      begin
        State := State + [dsHot];
				DrawThemeThinButton(DC, Rect, State);
      end;
    if Rect.Left < MaxRight then
			DrawThemeThinButton(DC, Rect, [dsHot]);
    State := State + [dsFlat];
    OffsetRect(Rect, Delta, Delta);
    case PaneButton of
      pbClose:
      	DrawThemeClose(DC, Rect, State);
      pbLeftArrow:
      	DrawThemeArrow(DC, drLeft, Rect, State, clWindowFrame, -1);
      pbRightArrow:
      	DrawThemeArrow(DC, drRight, Rect, State, clWindowFrame, -1);
    end;
    if ActivePane <> nil then with Rect do
      ExcludeClipRect(DC, Left - Delta, Top - Delta, Right - Delta, Bottom - Delta);
  end;

var
  DC: HDC;
  Rect: TRect;
  DrawRect: TRect;
  Brush: HBRUSH;
  Pen: HPEN;
  I: Integer;
begin
  DC := Canvas.Handle;
  if FActivePane <> nil then
	  with ClientRect do
			ExcludeClipRect(DC, Left, Top, Right, Bottom);
  Rect := Classes.Rect(0, 0, Width, Height);
  DrawRect := Rect;
  if FBorderStyle = bsSingle then
  begin
    with ThemePainter do
      if ThemePainter.Enabled then
        DrawElement(DC, GetDetails(teEditTextNormal), Rect)
      else
        DrawFrame(DC, Rect, dfSunken);
    InflateRect(Rect, -GetBorder, -GetBorder);
    FillRect(DC, Rect, COLOR_BTNFACE + 1);
  end
  else
  begin
	  FillRect(DC, Rect, COLOR_BTNFACE + 1);
		InflateRect(Rect, -2, -2);
  end;
  DrawRect.Bottom := DrawRect.Top + FHeaderHeight + 5;
  InflateRect(DrawRect, -2, -2);
  Brush := GetBrush(CheckeredBitmap);
  FillRect(DC, DrawRect, Brush);
  DeleteObject(Brush);
  with Rect do
  begin
    ExcludeClipRect(DC, Left - 2, Top, Left, Bottom);
    ExcludeClipRect(DC, Left - 2, Top - 2, Right + 2, Top);
    ExcludeClipRect(DC, Right, Top, Right + 2, Bottom);
  end;
  MaxRight := 0;
  for I := PaneCount - 1 downto 0 do
    if Panes[I].Opened then
    begin
      MaxRight := PaneRect[I].Right + 1;
      Break;
    end;
  DrawButton(DC, pbClose);
  DrawButton(DC, pbRightArrow);
  DrawButton(DC, pbLeftArrow);
  if FActivePane <> nil then
  begin
    DrawRect := PaneRect[FActivePane.PaneIndex];
		DrawCaption(DC, FActivePane.Caption, DrawRect, drCenter);
    Brush := GetSysColorBrush(COLOR_BTNFACE);
    Inc(DrawRect.Bottom);
    FillRect(DC, DrawRect, Brush);
    Dec(DrawRect.Bottom);
    DeleteObject(Brush);
    InflateRect(DrawRect, 1, 1);
    Pen := SelectObject(DC, GetPen(GetSysColor(COLOR_BTNHIGHLIGHT)));
    with DrawRect do
    begin
      MoveToEx(DC, Left, Bottom - 1, nil);
      LineTo(DC, Left, Top);
      LineTo(DC, Right - 1, Top);
      OverwriteObject(DC, GetPen(GetSysColor(COLOR_3DDKSHADOW)));
      LineTo(DC, Right - 1, Bottom - 1);
      OverwriteObject(DC, Pen);
      DrawCaption(DC, FActivePane.Caption, DrawRect, drCenter);
    end;
    with DrawRect do
      ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
  Pen := SelectObject(DC, GetPen(GetSysColor(COLOR_BTNHIGHLIGHT)));
  with DrawRect do
  begin
    MoveToEx(DC, Left, Bottom, nil);
    LineTo(DC, Right, Bottom);
    Top := Bottom + 1;
    Bottom := Rect.Bottom - 2;
  end;
  OverwriteObject(DC, Pen);
  Brush := GetSysColorBrush(COLOR_BTNFACE);
  if FActivePane = nil then
    FillRect(DC, DrawRect, Brush);
  for I := 0 to PaneCount - 1 do
    if Panes[I] <> ActivePane then
    begin
      DrawRect := GetPaneRect(I);
      DrawCaption(DC, Panes[I].Caption, DrawRect, drCenter);
    end;
  DeleteObject(Brush);
end;

procedure TCustomPaneControl.ImagesChange(Sender: TObject);
begin
  UpdateDisplay;
end;

function TCustomPaneControl.AddPane: TPaneSheet;
begin
	Result := TPaneSheet.Create(Owner);
  InsertPane(Result);
end;

procedure TCustomPaneControl.InsertPane(PaneSheet: TPaneSheet);
begin
  if FPanes.IndexOf(PaneSheet) = -1 then
  begin
    PaneSheet.Visible := False;
    PaneSheet.Parent := Self;
    FPanes.Add(PaneSheet);
    if ActivePane = nil then
      ActivePane := PaneSheet;
  	PaneSheet.PaneControl := Self;
    UpdateDisplay;
  end;
end;

procedure TCustomPaneControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PaneButton: TPaneButton;
  Pane: TPaneSheet;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FButton := pbNone;
    PaneButton := ButtonFromPoint(X, Y);
    if PaneButton <> pbNone then
      Self.Button := PaneButton
    else
    begin
      Pane := PaneFromPoint(X, Y);
      if Pane <> nil then
        ActivePane := Pane;
    end;
  end;
end;

procedure TCustomPaneControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not MouseCapture then
    Button := ButtonFromPoint(X, Y);
end;

procedure TCustomPaneControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PaneButton: TPaneButton;
begin
  inherited MouseUp(Button, Shift, X, Y);
  PaneButton := ButtonFromPoint(X, Y);
  if (PaneButton <> pbNone) and (PaneButton = Self.Button) then
  begin
    Self.Button := pbNone;
    Self.Button := ButtonFromPoint(X, Y);
    ButtonClick(PaneButton);
  end
  else
    Self.Button := ButtonFromPoint(X, Y);
end;

procedure TCustomPaneControl.RemovePane(PaneSheet: TPaneSheet);
var
	I: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
	I := FPanes.IndexOf(PaneSheet);
  if I > -1 then
  begin
    FPanes.Delete(I);
    if FActivePane = PaneSheet then
      FActivePane := nil;
    if LeftIndex > FPanes.Count - 1 then
      LeftIndex := FPanes.Count - 1;
  end;
  Invalidate;
end;

procedure TCustomPaneControl.UpdateDisplay;
begin
  if HandleAllocated then
  begin
    if FLeftIndex = -1 then
      LeftIndex := 0;
    Invalidate;
  end;
end;

procedure TCustomPaneControl.SetActivePane(Value: TPaneSheet);
var
  Form: TCustomForm;
  P: TPaneSheet;
begin
  if Value <> FActivePane then
  begin
  	P := FActivePane;
    if FActivePane <> nil then
      FActivePane.Visible := False;
    FActivePane := Value;
    if FActivePane <> nil then
    begin
      FActivePane.Visible := True;
      FActivePane.BringToFront;
		  if Focused or Windows.IsChild(Handle, Windows.GetFocus) then
      	FActivePane.SelectFirst;
    end;
    if csDesigning in ComponentState then
    begin
      Form := GetParentForm(Self);
      if Form <> nil then
        Form.Designer.Modified;
    end;
    UpdateDisplay;
    if Assigned(FOnPaneChange) then
      FOnPaneChange(Self, P, FActivePane);
  end;
end;

procedure TCustomPaneControl.SetBorderStyle(Value: TBorderStyle);
begin
	if Value <> FBorderStyle then
  begin
  	FBorderStyle := Value;
	  UpdateDisplay;
    if FActivePane <> nil then
    	FActivePane.Invalidate;
  end;
end;

procedure TCustomPaneControl.SetButton(Value: TPaneButton);
var
  Rect: TRect;
begin
  if Value <> FButton then
  begin
    Rect := ButtonRect[FButton];
    InvalidateRect(Handle, @Rect, False);
    FButton := Value;
    Rect := ButtonRect[FButton];
    InvalidateRect(Handle, @Rect, False);
  end;
end;

function TCustomPaneControl.GetButtonRect(Index: TPaneButton): TRect;
const
	ButtonSize = 25;
begin
  SetRectEmpty(Result);
  if Index <> pbNone then
    with Result do
    begin
      Result := Rect(0, 0, Width, Height);
      InflateRect(Result, -2, -2);
      Bottom := Top + FHeaderHeight;
      Dec(Right, 3);
      Left := Right - ButtonSize;
      case Index of
         pbLeftArrow:
           OffsetRect(Result, -ButtonSize * 2 - 2, 0);
         pbRightArrow:
           OffsetRect(Result, -ButtonSize - 1, 0);
      end;
      Result.Bottom := Result.Top + WidthOf(Result);
      InflateRect(Result, -1, -1);
      OffsetRect(Result, 0, 2);
    end;
end;

procedure TCustomPaneControl.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateDisplay;
end;

procedure TCustomPaneControl.SetLeftIndex(Value: Integer);
var
  Rect: TRect;
begin
  if Value <> FLeftIndex then
  begin
    FLeftIndex := Value;
    if FLeftIndex > FOpenCount - 1 then
      FLeftIndex := FOpenCount - 1;
    if FLeftIndex < 0 then
      FLeftIndex := 0;
    Rect := Classes.Rect(0, 0, Width, FHeaderHeight + 3);
    InvalidateRect(Handle, @Rect, True);
  end;
end;

function TCustomPaneControl.GetPanes(Index: Integer): TPaneSheet;
begin
  Result := TPaneSheet(FPanes[Index]);
end;

function TCustomPaneControl.GetPaneCount: Integer;
begin
  Result := FPanes.Count;
end;

function TCustomPaneControl.GetPaneRect(Index: Integer): TRect;
var
  DC: HDC;
  Pane: TPaneSheet;
  PaneIndex: Integer;
  I: Integer;
begin
  DC := Canvas.Handle;
  SetRectEmpty(Result);
  PaneIndex := 0;
  with Result do
    for I := 0 to Index do
    begin
      Pane := Panes[I];
      if Pane.Opened then
      begin
        Inc(PaneIndex);
        if PaneIndex > FLeftIndex then
          Inc(Right, CalculateCaptionSize(DC, Pane.Caption).cX + 16);
      end;
      if I = Index then
      begin
        if Pane.Opened then
        begin
          Result.Left := Right - CalculateCaptionSize(DC, Pane.Caption).cX - 16;
          OffsetRect(Result, 8, 0);
          Bottom := FHeaderHeight + 4;
          Top := CalculateCaptionSize(DC, 'Wg').cY;
          Top := Bottom - (Top + Top div 2);
        end
        else
          SetRectEmpty(Result);
        Break;
      end;
    end;
  if PaneIndex = 0 then
    SetRectEmpty(Result);
end;

procedure TCustomPaneControl.CMDesignHitTest(var Message: TCMDesignHitTest);
const
  HitTests: array[Boolean] of Integer = (0, 1);
begin
  inherited;
  with Message do
    Result := HitTests[PaneFromPoint(XPos, YPos) <> nil];
end;

procedure TCustomPaneControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    if GetKeyState(VK_SHIFT) >= 0 then
	    NextPane
    else
    	PriorPane;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TCustomPaneControl.CMFontChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  Canvas.Font := Font;
  I := Canvas.TextHeight('Wg');
  FHeaderHeight := Trunc(I * 1.75) + 2;
  UpdateDisplay;
end;

procedure TCustomPaneControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomPaneControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not MouseCapture then
    Button := pbNone;
end;

initialization
  BitmapBrush := nil;
  Classes.RegisterClass(TPaneSheet);
finalization
  BitmapBrush.Free;
end.
