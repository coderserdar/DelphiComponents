unit mbXPCheckRadio;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Themes, ActnList;

type
  TMouseLoc = (mlNone, mlOver, mlDown);

  TmbXPCheckRadio = class;

  TmbXPCheckRadioActionLink = class(TControlActionLink)
  protected
   FClient: TmbXPCheckRadio;
   procedure AssignClient(AClient: TObject); override;
   function IsCheckedLinked: Boolean; override;
   function IsGroupIndexLinked: Boolean; override;
   procedure SetGroupIndex(Value: Integer); override;
   procedure SetChecked(Value: Boolean); override;
  end;

  TmbXPCheckRadio = class(TCustomControl)
  private
   FChecked: boolean;
   FGroupIndex: integer;
   FMouseLoc: TMouseLoc;
   FMouseOver: boolean;
   FMouseDown: boolean;
   FOnChange: TNotifyEvent;
   FShowFocus: boolean;

   procedure SetShowFocus(Value: boolean);
   procedure SetGroupIndex(Value: Integer);
   procedure SetChecked(c: boolean);
  protected
   procedure UpdateExclusive;
   procedure Paint; override;
   function GetActionLinkClass: TControlActionLinkClass; override;
   procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
   procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
   procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
   constructor Create(AOwner: TComponent); override;
  published
   property Action;
   property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
   property Checked: boolean read FChecked write SetChecked default false;
   property ShowFocus: boolean read FShowFocus write SetShowFocus default true;
   property Enabled;
   property Anchors;
   property Color;
   property ParentColor default true;
   property Constraints;
   property Font;
   property Caption;
   property DragCursor;
   property DragKind;
   property DragMode;
   property ParentShowHint;
   property ShowHint;
   property ParentBackground default true;
   property ParentFont;
   property TabOrder;
   property TabStop default true;
   property Visible;
   property PopupMenu;

   property OnChange: TNotifyEvent read FOnChange write FOnChange;
   property OnContextPopup;
   property OnClick;
   property OnDblClick;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnStartDock;
   property OnStartDrag;
   property OnMouseMove;
   property OnMouseDown;
   property OnMouseUp;
  end;
  
implementation

{ TmbXPCheckRadioActionLink }

procedure TmbXPCheckRadioActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TmbXPCheckRadio;
end;

function TmbXPCheckRadioActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TmbXPCheckRadioActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TmbXPCheckRadio) and
    (TmbXPCheckRadio(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TmbXPCheckRadioActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TmbXPCheckRadio(FClient).Checked := Value;
end;

procedure TmbXPCheckRadioActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TmbXPCheckRadio(FClient).GroupIndex := Value;
end;

{ TmbXPCheckRadio }

constructor TmbXPCheckRadio.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque, csParentBackground];
 Height := 15;
 Width := 105;
 ParentColor := true;
 TabStop := true;
 FChecked := false;
 FMouseLoc := mlNone;
 FMouseOver := false;
 FMouseDown := false;
 FShowFocus := true;
end;

procedure TmbXPCheckRadio.Paint;
var
 R, TR, CR: TRect;
begin
 if csDesigning in ComponentState then FMouseLoc := mlNone;
 R := ClientRect;
 with R do
  begin
   Right := Left + 13;
   Top := (Height - 13) div 2;
   Bottom := Top + 13;
  end;
 CR := R;
 InflateRect(CR, -4, -4);
 if ThemeServices.ThemesEnabled then
  with ThemeServices do
   begin
    if Enabled then
     begin
      if FChecked then
       case FMouseLoc of
        mlNone: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxMixedNormal), R);
        mlOver: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxMixedHot), R);
        mlDown: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxMixedPressed), R);
       end
      else
       case FMouseLoc of
        mlNone: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal), R);
        mlOver: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedHot), R);
        mlDown: ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedPressed), R);
       end;
     end
    else
     if FChecked then
      ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxMixedDisabled), R)
     else
      ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tbCheckBoxUncheckedDisabled), R);
   end
  else
   begin
    Canvas.Brush.Style := bsSolid;
    if Enabled then
     begin
      Canvas.Brush.Color := clBtnText;
      if FChecked then
       begin
        case FMouseLoc of
         mlNone, mlOver:
          begin
           DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
           Canvas.FillRect(CR);
          end;
         mlDown:
          begin
           DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTON3STATE);
           InflateRect(CR, 2, 2);
           Canvas.Brush.Color := Color;
           Canvas.FillRect(CR);
           InflateRect(CR, -2, -2);
           Canvas.Brush.Color := clBtnText;
           Canvas.FillRect(CR);
          end;
        end;
       end
      else
       case FMouseLoc of
        mlNone, mlOver: DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK);
        mlDown:
         begin
          DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTON3STATE);
          InflateRect(CR, 2, 2);
          Canvas.Brush.Color := Color;
          Canvas.FillRect(CR);
         end;
       end;
     end
    else
     begin
      Canvas.Brush.Color := clGrayText;
      if FChecked then
       begin
        DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
        Canvas.FillRect(CR);
       end
      else
       DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_INACTIVE);
     end;
   end;
 Canvas.Font := Font;
 Canvas.Brush.Style := bsClear;
 TR := ClientRect;
 with TR do
  begin
   Left := R.Right + 4;
   Top := ((Bottom - Top) - Canvas.TextHeight(Caption)) div 2;
   Bottom := Top + Canvas.TextHeight(Caption);
   Right := Left + Canvas.TextWidth(Caption);
   if Right > Width - 1 then Right := Width - 1;
  end;
 DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TR, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
 Canvas.Brush.Style := bsSolid;
 InflateRect(TR, 1, 1);
 if Focused and FShowFocus then DrawFocusRect(Canvas.Handle, TR);
end;

function TmbXPCheckRadio.GetActionLinkClass: TControlActionLinkClass;
begin
 Result := TmbXPCheckRadioActionLink;
end;

procedure TmbXPCheckRadio.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 if not FMouseDown then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlDown;
 Invalidate;
end;

procedure TmbXPCheckRadio.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPCheckRadio.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  begin
   FMouseDown := true;
   FMouseLoc := mlDown;
   //SetFocus;
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPCheckRadio.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  begin
   FMouseDown := false;
   if FMouseOver then
    FMouseLoc := mlOver
   else
    FMouseLoc := mlNone;
   if PtInRect(ClientRect, Point(x, y)) then
    SetChecked(true);
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPCheckRadio.WMSetFocus(var Message: TWMSetFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPCheckRadio.WMKillFocus(var Message: TWMSetFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPCheckRadio.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPCheckRadio.SetChecked(c: boolean);
begin
 if FChecked <> c then
  begin
   FChecked := c;
   if (Action is TCustomAction) then TCustomAction(Action).Checked := c;
   invalidate;
   UpdateExclusive;
   if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TmbXPCheckRadio.SetGroupIndex(Value: Integer);
begin
 if FGroupIndex <> Value then
  begin
   FGroupIndex := Value;
   UpdateExclusive;
  end;
end;

procedure TmbXPCheckRadio.UpdateExclusive;
var
 Msg: TMessage;
begin
 if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
   Msg.Msg := CM_BUTTONPRESSED;
   Msg.WParam := FGroupIndex;
   Msg.LParam := Longint(Self);
   Msg.Result := 0;
   Parent.Broadcast(Msg);
  end;
 invalidate;
end;

procedure TmbXPCheckRadio.CMButtonPressed(var Message: TMessage);
var
 Sender: TmbXPCheckRadio;
begin
 if Message.WParam = FGroupIndex then
  begin
   Sender := TmbXPCheckRadio(Message.LParam);
   if (Sender <> Self) and Sender.Checked and FChecked then
    SetChecked(false);
  end;
end;

procedure TmbXPCheckRadio.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
 inherited;
 if Sender is TCustomAction then
  begin
   if CheckDefaults or (GroupIndex = 0) then
    GroupIndex := TCustomAction(Sender).GroupIndex;
  end;
end;

procedure TmbXPCheckRadio.KeyDown(var Key: Word; Shift: TShiftState);
begin
 if key = VK_SPACE then
  begin
   FMouseLoc := mlDown;
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPCheckRadio.KeyUp(var Key: Word; Shift: TShiftState);
begin
 if key = VK_SPACE then
  begin
   if FmouseOver then
    FMouseLoc := mlOver
   else
    FMouseLoc := mlNone;
   SetChecked(true);
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPCheckRadio.CMTextChanged(var Message: TMessage);
begin
 inherited;
 invalidate;
end;

procedure TmbXPCheckRadio.SetShowFocus(Value: boolean);
begin
 if FShowFocus <> Value then
  begin
   FShowFocus := Value;
   Invalidate;
  end;
end;

end.
