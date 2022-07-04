unit mbXPSpin;

interface

uses
  Windows, Classes, ExtCtrls, Controls, Messages, SysUtils, Graphics, Buttons,
  Themes;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause = 100;  { pause before hint window displays (ms)}

type
  TButDirection = (dUP, dDOWN);
  TMouseLoc = (mlNone, mlOver, mlDown);
  TNumGlyphs = Buttons.TNumGlyphs;

  TmbXPTimedButton = class;

{ TmbXPSpinButton }

  TmbXPSpinButton = class (TWinControl)
  private
    FUpButton: TmbXPTimedButton;
    FDownButton: TmbXPTimedButton;
    FFocusedButton: TmbXPTimedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton: TmbXPTimedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TmbXPTimedButton);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

{ TmbXPTimedButton }

  TTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  TmbXPTimedButton = class(TSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    FDirection: TButDirection;
    FMouseLoc: TMouseLoc;
    FMouseOver: Boolean;

    procedure TimerExpired(Sender: TObject);
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
    property Direction: TButDirection read FDirection write FDirection;
  end;

implementation

{$R mbXPSpin}

{ TmbXPSpinButton }

constructor TmbXPSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  { Frames don't look good around the buttons when themes are on }
  if ThemeServices.ThemesEnabled then
    ControlStyle := ControlStyle - [csFramed];
  FUpButton := CreateButton;
  FUpButton.Direction := dUP;
  FDownButton := CreateButton;
  FDownButton.Direction := dDOWN;
  UpGlyph := nil;
  DownGlyph := nil;

  Width := 20;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TmbXPSpinButton.CreateButton: TmbXPTimedButton;
begin
  Result := TmbXPTimedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
end;

procedure TmbXPSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TmbXPSpinButton.CMEnabledChanged(var Message: TMessage); 
begin
 inherited;
 if FUpButton <> nil then
  FUpButton.Enabled := Enabled;
 if FDownButton <> nil then
  FDownButton.Enabled := Enabled;
 Invalidate;
end;

procedure TmbXPSpinButton.AdjustSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  if ThemeServices.ThemesEnabled then
   begin
    FUpButton.SetBounds(0, -1, W, H div 2);
    FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
   end
  else
   begin
    FUpButton.SetBounds(0, 0, W, H div 2);
    FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
   end;
end;

procedure TmbXPSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TmbXPSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TmbXPSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TmbXPSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TmbXPSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TmbXPSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TmbXPTimedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TmbXPSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
   begin
    if Assigned(FOnUpClick) then
     FOnUpClick(Self);
   end
  else
   if Assigned(FOnDownClick) then
    FOnDownClick(Self);
end;

procedure TmbXPSpinButton.SetFocusBtn (Btn: TmbXPTimedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then 
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TmbXPSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TmbXPSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TmbXPSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TmbXPSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'mbSpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TmbXPSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TmbXPSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TmbXPSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TmbXPSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'mbSpinDown');
    FUpButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TmbXPSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TmbXPSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

procedure TmbXPSpinButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

{TmbXPTimedButton}

constructor TmbXPTimedButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FMouseLoc := mlNone;
 FMouseOver := false;
 FDirection := dUP;
end;

destructor TmbXPTimedButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TmbXPTimedButton.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 FMouseLoc := mlOver;
 invalidate;
end;

procedure TmbXPTimedButton.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 FMouseLoc := mlNone;
 invalidate;
end;

procedure TmbXPTimedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseLoc := mlDown;
  invalidate;
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
   begin
    if FRepeatTimer = nil then
     FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
   end;
end;

procedure TmbXPTimedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 invalidate;
 inherited MouseUp (Button, Shift, X, Y);
 if FRepeatTimer <> nil then
   FRepeatTimer.Enabled  := False;
end;

procedure TmbXPTimedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TmbXPTimedButton.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 invalidate;
end;

procedure TmbXPTimedButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure TmbXPTimedButton.Paint;
var
 d: TThemedElementDetails;
begin
 if ThemeServices.ThemesEnabled then
  with ThemeServices do
   begin
    case FDirection of
     dUP:
      if Parent.Enabled then
       case FMouseLoc of
        mlNone: d := GetElementDetails(tsUpNormal);
        mlOver: d := GetElementDetails(tsUpHot);
        mlDown: d := GetElementDetails(tsUpPressed);
       end
      else
       d := GetElementDetails(tsUpDisabled);
     dDOWN:
      if Parent.Enabled then
       case FMouseLoc of
        mlNone: d := GetElementDetails(tsDownNormal);
        mlOver: d := GetElementDetails(tsDownHot);
        mlDown: d := GetElementDetails(tsDownPressed);
       end
      else
       d := GetElementDetails(tsDownDisabled);
     end;
    DrawElement(canvas.Handle, d, ClientRect);
   end
 else
  inherited Paint;
end;

end.
