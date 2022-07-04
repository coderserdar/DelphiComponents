unit mbXPArrowButton;

interface

uses
  Messages, SysUtils, Classes, Controls, StdCtrls, Windows, Graphics,
  ImgList, Menus, Themes;

type
  TMouseLoc = (mlNone, mlOver, mlDown);
  TArrowStyle = (asLeft, asRight, asUp, asDown, asNone, asImage);

  TmbXPCustomArrowButton = class(TCustomControl)
  private
   FMouseLoc: TMouseLoc;
   FMouseOver: boolean;
   FImages: TCustomImageList;
   FImageChangeLink: TChangeLink;
   FImageIndex: TImageIndex;
   FMenu: TPopupMenu;
   FArr: TArrowStyle;
   FJust: boolean;
   FOnDrop, FOnClose: TNotifyEvent;

   procedure SetJust(j: boolean);
   procedure SetArr(d: TArrowStyle);
   procedure SetImageIndex(i: TImageIndex);
   procedure ImageListChange(Sender: TObject);
   Procedure SetImages(i: TCustomImageList);
   procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
   procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure PaintArrow(c: TControlCanvas; arrs: TArrowStyle);
   procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure KeyPress(var Key: Char); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

   property JustPaintArrow: boolean read FJust write SetJust default false;
   property ArrowStyle: TArrowStyle read FArr write SetArr default asRight;
   property DropMenu: TPopupMenu read FMenu write FMenu;
   property Images: TCustomImageList read FImages write SetImages;
   property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
   property OnBeforeDropMenu: TNotifyEvent read FOnDrop write FOnDrop;
   property OnAfterDropMenu: TNotifyEvent read FOnClose write FOnClose;
  end;

  TmbXPArrowButton = class(TmbXPCustomArrowButton)
  published
   property DropMenu;
   property Images;
   property ImageIndex default -1;
   property Anchors;
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
   property ArrowStyle;
   property JustPaintArrow;

   property OnBeforeDropMenu;
   property OnAfterDropMenu;
   property OnClick;
   property OnDblClick;
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

constructor TmbXPCustomArrowButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := ControlStyle + [csOpaque, csDoubleClicks] - [csAcceptsControls];
 FMouseLoc := mlNone;
 FMouseOver := false;
 DoubleBuffered := true;
 FImageIndex := -1;
 FImageChangeLink := TChangeLink.Create;
 FImageChangeLink.OnChange := ImageListChange;
 TabStop := true;
 FArr := asRight;
 Height := 20;
 Width := 9;
 FJust := false;
end;

destructor TmbXPCustomArrowButton.Destroy;
begin
 FImageChangeLink.Free;
 inherited Destroy;
end;

procedure TmbXPCustomArrowButton.WMPaint(var Message: TWMPaint);
var
  C: TControlCanvas;
  R, IR: TRect;
begin
  inherited;
  C := TControlCanvas.Create;
  C.Control := Self;
  R := ClientRect;
  C.Brush.Color := Color;
  C.FillRect(R);
  // set info
  if (csDesigning in ComponentState) or (Enabled = false) then FMouseLoc := mlNone;
  // paint the border and button
  if not FJust then
   begin
    if ThemeServices.ThemesEnabled then
     begin
      with ThemeServices do
       if Enabled then
        case FMouseLoc of
         mlNone: DrawElement(c.Handle, GetElementDetails(ttbButtonNormal), ClientRect);
         mlOver: DrawElement(c.Handle, GetElementDetails(ttbButtonHot), ClientRect);
         mlDown: DrawElement(c.Handle, GetElementDetails(ttbButtonPressed), ClientRect);
        end
       else
        DrawElement(c.Handle, GetElementDetails(ttbButtonDisabled), ClientRect);
     end
    else
     begin
      if (csDesigning in ComponentState) or (Enabled = false) then FMouseLoc := mlOver;
      if Enabled then
       case FMouseLoc of
        mlNone: c.FillRect(ClientRect);
        mlOver: DrawEdge(c.Handle, R, BDR_RAISEDINNER, BF_RECT);
        mlDown: DrawEdge(c.Handle, R, BDR_SUNKENOUTER, BF_RECT);
       end
      else
       DrawFrameControl(c.Handle, R, DFC_BUTTON, 0 or DFCS_BUTTONPUSH or DFCS_FLAT or DFCS_INACTIVE);
     end;
   end
  else
   begin
    c.Brush.Color := Color;
    c.FillRect(ClientRect);
   end;
  //draw the image
  if Enabled then
   begin
    c.Pen.Color := clBtnText;
    c.Brush.Color := clBtnText;
   end
  else
   begin
    c.Pen.Color := clGrayText;
    c.Brush.Color := clGrayText;
   end;
  case FArr of
   asLeft, asRight, asUp, asDown: PaintArrow(c, FArr);
   asImage:
    if (FImages <> nil) and (FImageIndex > -1) then
     begin
      with IR do
       begin
        Left := (Width - FImages.Width) div 2;
        Right := Left + FImages.Width;
        Top := (Height - FImages.Height) div 2;
        Bottom := Top + FImages.Height;
       end;
      FImages.Draw(c, ir.left, ir.top, FImageIndex, Enabled);
     end;
  end;
  // free the canvas
  C.Free;
end;

procedure TmbXPCustomArrowButton.PaintArrow(c: TControlCanvas; arrs: TArrowStyle);
var
  X, Y: Integer;
begin
 X := (ClientRect.Left + ClientRect.Right) div 2;
 Y := (ClientRect.Top + ClientRect.Bottom) div 2 - 1;
 if FMouseLoc = mlDown then Inc(Y);
 case arrs of
  asLeft: c.Polygon([Point(X, Y + 2), Point(X, Y - 2), Point(X - 2, Y)]);
  asRight: c.Polygon([Point(X, Y - 2), Point(X, Y + 2), Point(X + 2, Y)]);
  asUp: c.Polygon([Point(X + 2, Y), Point(X - 2, Y), Point(X, Y - 2)]);
  asDown: c.Polygon([Point(X - 2, Y), Point(X + 2, Y), Point(X, Y + 2)]);
 end;
end;

procedure TmbXPCustomArrowButton.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 FMouseLoc := mlOver;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 p: TPoint;
begin
 if Button = mbLeft then
  begin
   SetFocus;
   FMouseLoc := mlDown;
   Invalidate;
   if FMenu <> nil then
    begin
     case FArr of
      asLeft:
       begin
        FMenu.Alignment := paRight;
        p := Point(0, 0);
       end;
      asRight:
       begin
        FMenu.Alignment := paLeft;
        p := Point(Width, 0);
       end;
      asUp:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, 0);
       end;
      asDown:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, Height);
       end;
      asImage:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, Height);
       end;
     end;
     if Assigned(FOnDrop) then FOnDrop(Self);
     FMenu.Popup(ClientToScreen(p).x, ClientToScreen(p).y);
     if Assigned(FOnClose) then FOnClose(Self);
     if FmouseOver then
      FMouseLoc := mlOver
     else
      FMouseLoc := mlNone;
    end;
   Invalidate;
  end;
end;

procedure TmbXPCustomArrowButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 if Operation = opRemove then
  if AComponent = Images then
   SetImages(nil);
end;

procedure TmbXPCustomArrowButton.SetImages(i: TCustomImageList);
begin
 if FImages <> nil then
  FImages.UnRegisterChanges(FImageChangeLink);
 FImages := i;
 if FImages <> nil then
  begin
   FImages.RegisterChanges(FImageChangeLink);
   FImages.FreeNotification(Self);
  end;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.ImageListChange(Sender: TObject);
begin
 if (Images <> nil) and (Sender = Images) then
  Invalidate;
end;

procedure TmbXPCustomArrowButton.SetImageIndex(i: TImageIndex);
begin
 FImageIndex := i;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.KeyPress(var Key: Char);
var
 p: TPoint;
begin
 if key = ' ' then
  begin
   FMouseLoc := mlDown;
   Invalidate;
   if FMenu <> nil then
    begin
     case FArr of
      asLeft:
       begin
        FMenu.Alignment := paRight;
        p := Point(0, 0);
       end;
      asRight:
       begin
        FMenu.Alignment := paLeft;
        p := Point(Width, 0);
       end;
      asUp:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, 0);
       end;
      asDown:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, Height);
       end;
      asImage:
       begin
        FMenu.Alignment := paLeft;
        p := Point(0, Height);
       end;
     end;
     if Assigned(FOnDrop) then FOnDrop(Self);
     FMenu.Popup(ClientToScreen(p).x, ClientToScreen(p).y);
     if Assigned(FOnClose) then FOnClose(Self);
    end;
   if FmouseOver then
    FMouseLoc := mlOver
   else
    FMouseLoc := mlNone;
   Invalidate;
  end;
 inherited KeyPress(Key);
end;

procedure TmbXPCustomArrowButton.SetArr(d: TArrowStyle);
begin
 FArr := d;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
 Message.Result := 1;
end;

procedure TmbXPCustomArrowButton.CMLostFocus(var Message: TCMLostFocus);
begin
 inherited;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPCustomArrowButton.SetJust(j: boolean);
begin
 FJust := j;
 Invalidate;
end;

end.
