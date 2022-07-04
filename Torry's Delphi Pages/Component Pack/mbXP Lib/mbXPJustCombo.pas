unit mbXPJustCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ExtCtrls, Themes,
  UxTheme, Menus;

type
  TMouseLoc = (mlNone, mlOver, mlDown);

  TmbXPJustCombo = class(TCustomControl)
  private
   FMouseOver: boolean;
   FMenu: TPopupMenu;
   FOnMenuChange: TMenuChangeEvent;
   FOnMenuPopup: TNotifyEvent;
   FOnMenuClose: TNotifyEvent;

   procedure SetMenu(m: TPopupMenu);
  protected
   FMouseLoc: TMouseLoc;

   function GetFocusRect: TRect;
   procedure Paint; override;
   procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
   procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure CMGotFocus(var Message: TMessage); message CM_ENTER;
   procedure CMLostFocus(var Message: TMessage); message CM_EXIT;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   procedure BoxClick(Sender: TObject); dynamic;
   procedure MenuChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); dynamic;
   procedure MenuPopup(Sender: TObject); dynamic;
  public
   constructor Create(AOwner: TComponent); override;

   property DropMenu: TPopupMenu read FMenu write SetMenu;
  published
   property Color default clWindow;
   property ParentColor default false;
   property Enabled;
   property Anchors;
   property Constraints;
   property DragCursor;
   property DragKind;
   property DragMode;
   property ParentShowHint;
   property ShowHint;
   property TabOrder;
   property TabStop default true;
   property Visible;
   property PopupMenu;

   property OnDropMenuChange: TMenuChangeEvent read FOnMenuChange write FOnMenuChange;
   property OnDropMenuPopup: TNotifyEvent read FOnMenuPopup write FOnMenuPopup;
   property OnDropMenuClose: TNotifyEvent read FOnMenuClose write FOnMenuClose;
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
  end;

implementation

constructor TmbXPJustCombo.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Height := 21;
 Width := 145;
 ParentColor := false;
 Color := clWindow;
 TabStop := true;
 OnClick := BoxClick;
end;

procedure TmbXPJustCombo.Paint;
var
 d: TThemedElementDetails;
 R, BR: TRect;
begin
 R := ClientRect;
 BR := R;
 InflateRect(BR, -1, -1);
 BR.Left := BR.Right - GetSystemMetrics(SM_CXVSCROLL);
 if ThemeServices.ThemesEnabled then // XP style
  begin
   // info for the frame
   d.Element := teComboBox;
   d.Part := 0;
   if Focused and (FMouseLoc <> mlDown) then FMouseLoc := mlOver;
   if (csDesigning in ComponentState) or (not Enabled) then FMouseLoc := mlNone;
   case FMouseLoc of
    mlNone: d.State := CBXS_NORMAL;
    mlOver: d.State := CBXS_HOT;
    mlDown: d.State := CBXS_PRESSED;
   end;
   if not Enabled then d.State := CBXS_DISABLED;
   // draw
   ThemeServices.DrawElement(Canvas.Handle, d, R);
   // fill color
   Canvas.Brush.Color := Color;
   InflateRect(R, -2, -2);
   Dec(R.Right, GetSystemMetrics(SM_CXVSCROLL) - 1);
   Canvas.FillRect(R);
   // info for the button
   d.Part := CP_DROPDOWNBUTTON;
   // draw
   ThemeServices.DrawElement(Canvas.Handle, d, bR);
   // if focused
   if Enabled and (Focused or (FMouseLoc = mlOver)) then
    begin
     InflateRect(R, -1, -1);
     DrawFocusRect(Canvas.Handle, R);
    end;
  end
 else
  begin
   Canvas.Brush.Color := Color;
   Canvas.FillRect(R);
   Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 2);
   InflateRect(BR, -1, -1);
   BR.Left := BR.Right - GetSystemMetrics(SM_CXVSCROLL);
   if (csDesigning in ComponentState) or (not Enabled) then FMouseLoc := mlNone;
   if Enabled then
    begin
     case FMouseLoc of
      mlNone, mlOver: DrawFrameControl(Canvas.Handle, BR, DFC_SCROLL, DFCS_SCROLLCOMBOBOX);
      mlDown: DrawFrameControl(Canvas.Handle, BR, DFC_SCROLL, DFCS_FLAT or DFCS_PUSHED or DFCS_SCROLLCOMBOBOX);
     end;
     if Focused or (FMouseLoc = mlOver) then
      begin
       Dec(R.Right, GetSystemMetrics(SM_CXVSCROLL) + 1);
       InflateRect(R, -1, -1);
       DrawFocusRect(Canvas.Handle, R);
      end;
    end
   else
    DrawFrameControl(Canvas.Handle, BR, DFC_SCROLL, DFCS_INACTIVE or DFCS_SCROLLCOMBOBOX);
  end;
// uncomment for shadow border around the selection (like in Office 2003, but ugly)
 if (FMouseLoc = mlOver) or Focused then
  begin
   InflateRect(R, -1, -1);
   Canvas.Brush.Color := clBtnShadow;
   Canvas.FillRect(R);
  end;
end;

procedure TmbXPJustCombo.CMMouseEnter(var Message: TMessage);
begin
 FMouseOver := true;
 FMouseLoc := mlOver;
 Invalidate;
end;

procedure TmbXPJustCombo.CMMouseLeave(var Message: TMessage);
begin
 FMouseOver := false;
 FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPJustCombo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button = mbLeft then
 begin
  FMouseLoc := mlDown;
  SetFocus;
  Invalidate;
  inherited;
 end;
end;

procedure TmbXPJustCombo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
 inherited;
end;

procedure TmbXPJustCombo.CMGotFocus(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPJustCombo.CMLostFocus(var Message: TMessage);
begin
 inherited;
 if FMouseOver then
  FMouseLoc := mlOver
 else
  FMouseLoc := mlNone;
 Invalidate;
end;

procedure TmbXPJustCombo.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPJustCombo.BoxClick(Sender: TObject);
var
 p: TPoint;
begin
 if FMenu <> nil then
  begin
   p := ClientToScreen(Point(0, Height));
   FMouseLoc := mlDown;
   Invalidate;
   FMenu.Popup(p.x, p.y);
   if Assigned(FOnMenuClose) then
    FOnMenuClose(Self);
   if FMouseOver then
    FMouseLoc := mlOver
   else
    FMouseLoc := mlNone;
   Invalidate;
  end;
end;

procedure TmbXPJustCombo.SetMenu(m: TPopupMenu);
begin
 FMenu := m;
 FMenu.OnChange := MenuChange;
 FMenu.OnPopup := MenuPopup;
end;

procedure TmbXPJustCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
 if (key = VK_RETURN) or (key = VK_SPACE) then BoxClick(Self);
end;

procedure TmbXPJustCombo.MenuChange(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
begin
 if Assigned(FOnMenuChange) then
  FOnMenuChange(Self, Source, Rebuild);
end;

procedure TmbXPJustCombo.MenuPopup(Sender: TObject);
begin
 if Assigned(FOnMenuPopup) then
  FOnMenuPopup(Self);
end;

function TmbXPJustCombo.GetFocusRect: TRect;
begin
 Result := ClientRect;
 InflateRect(Result, -2, -2);
 if ThemeServices.ThemesEnabled then
  Dec(Result.Right, GetSystemMetrics(SM_CXVSCROLL) - 1)
 else
  Dec(Result.Right, GetSystemMetrics(SM_CXVSCROLL) + 1);
end;

end.
