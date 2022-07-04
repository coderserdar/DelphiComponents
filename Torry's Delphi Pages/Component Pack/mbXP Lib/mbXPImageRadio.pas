unit mbXPImageRadio;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Themes, ActnList, ImgList;

type
  TCustomSize = (csBig, csMedium, csSmall);

  TmbXPImageRadio = class;

  TmbXPImageRadioActionLink = class(TControlActionLink)
  protected
    FClient: TmbXPImageRadio;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  end;

  TmbXPImageRadio = class(TCustomControl)
  private
   FChecked: boolean;
   FAlt: boolean;
   FCustomSize: TCustomSize;
   FGroupIndex: integer;
   FImages: TCustomImageList;
   FImageChangeLink: TChangeLink;
   FImageIndex: TImageIndex;
   FCenter, FAutoSize: boolean;
   FTransparent: boolean;
   FOnChange: TNotifyEvent;

   procedure SetTransparent(t: boolean);
   procedure SetCenter(c: boolean);
   procedure SetIRAutoSize(a: boolean);
   procedure SetImageIndex(i: TImageIndex);
   procedure ImageListChange(Sender: TObject);
   Procedure SetImages(i: TCustomImageList);
   procedure SetGroupIndex(Value: Integer);
   procedure SetCustomSize(s: TCustomSize);
   procedure SetChecked(c: boolean);
  protected
   procedure UpdateExclusive;
   procedure Paint; override;
   procedure DrawImage(R: TRect); dynamic;
   function GetActionLinkClass: TControlActionLinkClass; override;
   procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
   procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
   procedure CMGotFocus(var Message: TCMGotFocus); message CM_ENTER;
   procedure CMLostFocus(var Message: TCMLostFocus); message CM_EXIT;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Action;
   property AutoSize: boolean read FAutoSize write SetIRAutoSize default false;
   property Center: boolean read FCenter write SetCenter default true;
   property TransparentImage: boolean read FTransparent write SetTransparent default true;
   property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
   property CustomSize: TCustomSize read FCustomSize write SetCustomSize default csBig;
   property Checked: boolean read FChecked write SetChecked default false;
   property AlternateCheckMode: boolean read FAlt write FAlt default false;
   property Images: TCustomImageList read FImages write SetImages;
   property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
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

{ TmbXPImageRadioActionLink }

procedure TmbXPImageRadioActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TmbXPImageRadio;
end;

function TmbXPImageRadioActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TmbXPImageRadioActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TmbXPImageRadio) and
    (TmbXPImageRadio(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TmbXPImageRadioActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TmbXPImageRadio(FClient).Checked := Value;
end;

procedure TmbXPImageRadioActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TmbXPImageRadio(FClient).GroupIndex := Value;
end;

{ TmbXPImageRadio }

constructor TmbXPImageRadio.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque];
 Height := 104;
 Width := 96;
 ParentColor := false;
 Color := clWindow;
 TabStop := true;
 FChecked := false;
 FAlt := false;
 FCustomSize := csBig;
 FImageIndex := -1;
 FImageChangeLink := TChangeLink.Create;
 FImageChangeLink.OnChange := ImageListChange;
 FAutoSize := false;
 FCenter := true;
 FTransparent := true;
end;

destructor TmbXPImageRadio.Destroy;
begin
 FImageChangeLink.Free;
 inherited;
end;

procedure TmbXPImageRadio.Paint;
var
 R: TRect;
begin
 R := ClientRect;
 if ThemeServices.ThemesEnabled then
  begin
   if Enabled then
    ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(teEditTextNormal), R)
   else
    ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(teEditTextDisabled), R);
   Canvas.Brush.Style := bsSolid;
   Canvas.Brush.Color := Color;
   InflateRect(R, -1, -1);
   Canvas.FillRect(R);
   InflateRect(R, -1, -1);
   if Focused and Enabled then
    DrawFocusRect(Canvas.Handle, R);
   InflateRect(R, -1, -1);
   Inc(R.Left, 1);
   Inc(R.Top, 1);
   if FChecked then
    begin
     if Enabled then
      Canvas.Pen.Color := clHighlight
     else
      Canvas.Pen.Color := clGrayText;
     Canvas.Pen.Width := 2;
     Canvas.Brush.Style := bsClear;
     Canvas.Rectangle(R);
    end;
   InflateRect(R, -1, -1);
   DrawImage(R);
  end
 else
  begin
   Canvas.Brush.Style := bsSolid;
   Canvas.Brush.Color := Color;
   Canvas.FillRect(ClientRect);
   DrawEdge(Canvas.Handle, R, EDGE_SUNKEN, BF_RECT);
   InflateRect(R, -3, -3);
   Canvas.Brush.Style := bsClear;
   if Focused and Enabled then
    begin
     Canvas.Pen.Width := 1;
     Canvas.Pen.Color := clWindowFrame;
     Canvas.Rectangle(R);
    end;
   InflateRect(R, -1, -1);
   Inc(R.Left, 1);
   Inc(R.Top, 1);
   if FChecked then
    begin
     if Enabled then
      Canvas.Pen.Color := clHighlight
     else
      Canvas.Pen.Color := clGrayText;
     Canvas.Pen.Width := 2;
     Canvas.Rectangle(R);
    end;
   InflateRect(R, -1, -1);
   DrawImage(R);
  end;
end;

procedure TmbXPImageRadio.DrawImage(R: TRect);
begin
 if FImages <> nil then
  begin
   if FTransparent then
    FImages.DrawingStyle := dsTransparent
   else
    FImages.DrawingStyle := dsNormal;
   if FCenter then
    begin
     R.Left := R.Left + ((R.Right - R.Left) - FImages.Width) div 2;
     R.Top := R.Top + ((R.Bottom - R.Top) - FImages.Height) div 2;
    end;
   // nadji bolje za disabled
   FImages.Draw(Canvas, R.Left, R.Top, FImageIndex, Enabled);
  end;
end;

function TmbXPImageRadio.GetActionLinkClass: TControlActionLinkClass;
begin
 Result := TmbXPImageRadioActionLink;
end;

procedure TmbXPImageRadio.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  begin
   SetFocus;
   if not FAlt then
    SetChecked(true); 
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPImageRadio.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  begin
   if FAlt and PtInRect(ClientRect, Point(x, y)) then
    SetChecked(true); 
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

procedure TmbXPImageRadio.CMGotFocus(var Message: TCMGotFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPImageRadio.CMLostFocus(var Message: TCMLostFocus);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPImageRadio.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TmbXPImageRadio.SetChecked(c: boolean);
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

procedure TmbXPImageRadio.SetCustomSize(s: TCustomSize);
begin
 FCustomSize := s;
 case FCustomSize of
  csBig:
   begin
    Width := 96;
    Height := 104;
   end;
  csMedium:
   begin
    Width := 54;
    Height := 55;
   end;
  csSmall:
   begin
    Width := 36;
    Height := 40;
   end;
 end;
end;

procedure TmbXPImageRadio.SetGroupIndex(Value: Integer);
begin
 if FGroupIndex <> Value then
  begin
   FGroupIndex := Value;
   UpdateExclusive;
  end;
end;

procedure TmbXPImageRadio.UpdateExclusive;
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

procedure TmbXPImageRadio.CMButtonPressed(var Message: TMessage);
var
 Sender: TmbXPImageRadio;
begin
 if Message.WParam = FGroupIndex then
  begin
   Sender := TmbXPImageRadio(Message.LParam);
   if (Sender <> Self) and Sender.Checked and FChecked then
    SetChecked(false);
  end;
end;

procedure TmbXPImageRadio.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 if Operation = opRemove then
  if AComponent = Images then
   SetImages(nil);
end;

procedure TmbXPImageRadio.SetImages(i: TCustomImageList);
begin
 if FImages <> nil then
  FImages.UnRegisterChanges(FImageChangeLink);
 FImages := i;
 if FImages <> nil then
  begin
   FImages.RegisterChanges(FImageChangeLink);
   FImages.FreeNotification(Self);
  end;
 if FAutoSize and (FImages <> nil) then
  begin
   if ThemeServices.ThemesEnabled then
    begin
     Width := FImages.Width + 2*6;
     Height := FImages.Height + 2*6;
    end
   else
    begin
     Width := FImages.Width + 2*7;
     Height := FImages.Height + 2*7;
    end;
  end;
 Invalidate;
end;

procedure TmbXPImageRadio.ImageListChange(Sender: TObject);
begin
 if Sender = Images then
  begin
   if FAutoSize and (FImages <> nil) then
    begin
     if ThemeServices.ThemesEnabled then
      begin
       Width := FImages.Width + 2*6;
       Height := FImages.Height + 2*6;
      end
     else
      begin
       Width := FImages.Width + 2*7;
       Height := FImages.Height + 2*7;
      end;
    end;
   Invalidate;
  end;
end;

procedure TmbXPImageRadio.SetImageIndex(i: TImageIndex);
begin
 if FImageIndex <> i then
  begin
   FImageIndex := i;
   Invalidate;
 end;
end;

procedure TmbXPImageRadio.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
 inherited;
 if Sender is TCustomAction then
  begin
   if CheckDefaults or (GroupIndex = 0) then
    GroupIndex := TCustomAction(Sender).GroupIndex;
  end;
end;

procedure TmbXPImageRadio.SetCenter(c: boolean);
begin
 FCenter := c;
 invalidate;
end;

procedure TmbXPImageRadio.SetIRAutoSize(a: boolean);
begin
 FAutoSize := a;
 if FImages <> nil then
  begin
   if ThemeServices.ThemesEnabled then
    begin
     Width := FImages.Width + 2*6;
     Height := FImages.Height + 2*6;
    end
   else
    begin
     Width := FImages.Width + 2*7;
     Height := FImages.Height + 2*7;
    end;
  end;
 invalidate;
end;

procedure TmbXPImageRadio.SetTransparent(t: boolean);
begin
 FTransparent := t;
 invalidate;
end;

procedure TmbXPImageRadio.KeyDown(var Key: Word; Shift: TShiftState);
begin
 if key = VK_SPACE then
  begin
   SetChecked(true);
   Invalidate;
  end;
 UpdateExclusive;
 inherited;
end;

end.
