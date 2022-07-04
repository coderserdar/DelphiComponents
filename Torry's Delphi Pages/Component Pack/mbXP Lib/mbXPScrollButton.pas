unit mbXPScrollButton;

interface

uses
  Messages, SysUtils, Classes, Controls, ExtCtrls, Windows, Graphics, Themes, GraphUtil;

type
  TMouseLoc = (mlNone, mlOver, mlDown);

  TmbXPScrollButton = class(TCustomControl)
  private
   FMouseLoc: TMouseLoc;
   FMouseOver: boolean;
   FScrollDir: TScrollDirection;
   FFlat: boolean;
   FASize: integer;
   FTimer: TTimer;
   FOnScroll: TNotifyEvent;
   FMouseDown: boolean;

   procedure SetScrollDir(Value: TScrollDirection);
   procedure SetFlat(Value: boolean);
   procedure SetASize(Value: integer);
  protected
   procedure DoTimer(Sender: TObject);
   procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
   procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Click; override;
  published
   property Align;
   property Anchors;
   property ScrollDirection: TScrollDirection read FScrollDir write SetScrollDir default sdUp;
   property Flat: boolean read FFlat write SetFlat default false;
   property ArrowSize: integer read FASize write SetASize default 3;
   property Constraints;
   property DragCursor;
   property DragKind;
   property DragMode;
   property Enabled;
   property Font;
   property ParentFont;
   property Color;
   property ParentColor;
   property ParentShowHint;
   property PopupMenu;
   property ShowHint;
   property TabOrder;
   property TabStop default true;
   property Visible;
   property Hint;

   property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
   property OnClick;
   property OnContextPopup;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnStartDock;
   property OnStartDrag;
  end;

implementation

constructor TmbXPScrollButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := ControlStyle + [csOpaque] - [csAcceptsControls, csDoubleClicks];
 FMouseLoc := mlNone;
 FMouseOver := false;
 DoubleBuffered := true;
 TabStop := true;
 FScrollDir := sdUp;
 Height := 18;
 Width := 18;
 FFlat := false;
 FASize := 3;
 FTimer := TTimer.Create(Self);
 FTimer.Enabled := false;
 FTimer.Interval := 100;
 FTimer.OnTimer := DoTimer;
 FMouseDown := false;
end;

destructor TmbXPScrollButton.Destroy;
begin
 FTimer.Free;
 inherited Destroy;
end;

procedure TmbXPScrollButton.WMPaint(var Message: TWMPaint);
var
  C: TControlCanvas;
  R: TRect;
begin
  inherited;
  C := TControlCanvas.Create;
  C.Control := Self;
  c.Brush.Color := Color;
  C.FillRect(ClientRect);
  if (csDesigning in ComponentState) or (Enabled = false) then FMouseLoc := mlNone;
  if ThemeServices.ThemesEnabled then
   begin
    with ThemeServices do
     if Enabled then
      case FMouseLoc of
       mlNone:
        case FScrollDir of
         sdUp: DrawElement(c.Handle, GetElementDetails(tsArrowBtnUpNormal), ClientRect);
         sdDown: DrawElement(c.Handle, GetElementDetails(tsArrowBtnDownNormal), ClientRect);
         sdLeft: DrawElement(c.Handle, GetElementDetails(tsArrowBtnLeftNormal), ClientRect);
         sdRight: DrawElement(c.Handle, GetElementDetails(tsArrowBtnRightNormal), ClientRect);
        end;
       mlOver:
        case FScrollDir of
         sdUp: DrawElement(c.Handle, GetElementDetails(tsArrowBtnUpHot), ClientRect);
         sdDown: DrawElement(c.Handle, GetElementDetails(tsArrowBtnDownHot), ClientRect);
         sdLeft: DrawElement(c.Handle, GetElementDetails(tsArrowBtnLeftHot), ClientRect);
         sdRight: DrawElement(c.Handle, GetElementDetails(tsArrowBtnRightHot), ClientRect);
        end;
       mlDown:
        case FScrollDir of
         sdUp: DrawElement(c.Handle, GetElementDetails(tsArrowBtnUpPressed), ClientRect);
         sdDown: DrawElement(c.Handle, GetElementDetails(tsArrowBtnDownPressed), ClientRect);
         sdLeft: DrawElement(c.Handle, GetElementDetails(tsArrowBtnLeftPressed), ClientRect);
         sdRight: DrawElement(c.Handle, GetElementDetails(tsArrowBtnRightPressed), ClientRect);
        end;
      end
     else
      case FScrollDir of
       sdUp: DrawElement(c.Handle, GetElementDetails(tsArrowBtnUpDisabled), ClientRect);
       sdDown: DrawElement(c.Handle, GetElementDetails(tsArrowBtnDownDisabled), ClientRect);
       sdLeft: DrawElement(c.Handle, GetElementDetails(tsArrowBtnLeftDisabled), ClientRect);
       sdRight: DrawElement(c.Handle, GetElementDetails(tsArrowBtnRightDisabled), ClientRect);
      end;
   end
  else //no themes
   begin
    R := ClientRect;
    if csDesigning in ComponentState then FMouseLoc := mlOver;
    if Enabled then
     begin
      case FMouseLoc of
       mlNone:
        if FFlat then
         c.FillRect(ClientRect)
        else
         DrawEdge(c.Handle, R, BDR_RAISEDINNER, BF_RECT);
       mlOver: DrawEdge(c.Handle, R, BDR_RAISEDINNER, BF_RECT);
       mlDown: DrawEdge(c.Handle, R, BDR_SUNKENOUTER, BF_RECT);
      end;
      Canvas.Pen.Color := clWindowText;
     end
    else
     begin
      DrawFrameControl(c.Handle, R, DFC_BUTTON, 0 or DFCS_BUTTONPUSH or DFCS_FLAT or DFCS_INACTIVE);
      Canvas.Pen.Color := clGrayText;
     end;
    case FScrollDir of
     sdUp, sdDown: DrawArrow(Canvas, FScrollDir, Point((R.Right - R.Left - 2*FASize - 1) div 2, (R.Bottom - R.Top - FASize - 1) div 2), FASize);
     sdLeft, sdRight: DrawArrow(Canvas, FScrollDir, Point((R.Right - R.Left - FASize - 1) div 2, (R.Bottom - R.Top - 2*FASize - 1) div 2), FASize);
    end;
   end;
 // free the canvas
 C.Free;
end;

procedure TmbXPScrollButton.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 if FMouseDown then
  FMouseLoc := mlDown
 else
  FMouseLoc := mlOver;
 Invalidate;
end;

procedure TmbXPScrollButton.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 if FMouseDown then
  FMouseLoc := mlDown
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPScrollButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  begin
   FMouseDown := true;
   SetFocus;
   FMouseLoc := mlDown;
   Invalidate;
   FTimer.Enabled := true;
  end;
end;

procedure TmbXPScrollButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FMouseDown := false;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
 FTimer.Enabled := false;
end;

procedure TmbXPScrollButton.Click;
begin
 if Assigned(FOnScroll) then FOnScroll(Self);
 inherited;
end;

procedure TmbXPScrollButton.DoTimer(Sender: TObject);
begin
 if Assigned(FOnScroll) then FOnScroll(Self);
end;

procedure TmbXPScrollButton.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 FTimer.Enabled := false;
 Invalidate;
end;

procedure TmbXPScrollButton.SetScrollDir(Value: TScrollDirection);
begin
 FScrollDir := Value;
 Invalidate;
end;

procedure TmbXPScrollButton.SetFlat(Value: boolean);
begin
 FFlat := Value;
 Invalidate;
end;

procedure TmbXPScrollButton.SetASize(Value: integer);
begin
 FASize := Value;
 Invalidate;
end;

procedure TmbXPScrollButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure TmbXPScrollButton.CMLostFocus(var Message: TCMLostFocus);
begin
 inherited;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPScrollButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited;
 if (key = VK_RETURN) or (key = VK_SPACE) then
  begin
   FMouseLoc := mlDown;
   Invalidate;
   FTimer.Enabled := true;
  end;
end;

procedure TmbXPScrollButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
 inherited;
 if (key = VK_RETURN) or (key = VK_SPACE) then
  begin
   if FMouseOver then
    FMouseLoc := mlOver
   else
    FMouseLoc := mlNone;
   Invalidate;
   FTimer.Enabled := false;
  end;
end;

end.
