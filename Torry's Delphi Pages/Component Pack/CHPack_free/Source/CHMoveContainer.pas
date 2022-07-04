unit CHMoveContainer;

{ ##############################################################################
  TCHMoveContainer

  Version   		:   1.1.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 06.10.2002    - First Release
  1.0.1 - 14.10.2002    - NEW: Popupmenu
  1.0.2 - 16.10.2002    - CHANGE: Reorganize some Sourcecode
  1.0.3 - 15.12.2002    - BUG: repair some memory leaks
  1.1.0 - 28.12.2002    - NEW: "OnMove" event
                        - NEW: "ActivePreview" for preview moving

  ############################################################################ }

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls,
  _CHClassProperty, _CHClassFunction;

type
  TMoveContDirection = (mdUp, mdDown, mdLeft, mdRight, mdUpLeft,
    mdUpRight, mdDownLeft, mdDownRight);
    
  TCHCustomMoveContainer = class;

  TCHCustomMoveContainer = class(TCustomPanel)
  private
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;
    FOnTimer: TNotifyEvent;
    FOnMove: TNotifyEvent;
    FDirection: TMoveContDirection;
    FMoveSpeed: Cardinal;
    FBuffered: Boolean;
    FMoveTimer : TTimer;
    FActive: Boolean;
    FMoveStep: Word;
    FFill : TCHFillB;
    FClientRect : TRect;
    FActivePreview: Boolean;
    FSavLeft : Integer;
    FSavTop : Integer;

    procedure SetBuffered(Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoMove(Sender: TObject);
    procedure SetDirection(const Value: TMoveContDirection);
    procedure SetMoveSpeed(const Value: Cardinal);
    procedure SetActive(const Value: Boolean);
    procedure SetMoveStep(const Value: Word);
    procedure UpdateChanges(Sender: TObject);
    procedure SetActivePreview(const Value: Boolean);
  protected
    procedure Paint; override;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;

    property Active : Boolean read FActive Write SetActive;
    property ActivePreview : Boolean read FActivePreview Write SetActivePreview;
    property Buffered : Boolean read FBuffered write SetBuffered;
    property Direction : TMoveContDirection read FDirection Write SetDirection;
    property MoveSpeed : Cardinal read FMoveSpeed Write SetMoveSpeed;
    property MoveStep : Word read FMoveStep Write SetMoveStep;
    property Fill : TCHFillB read FFill Write FFill;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHMoveContainer = class(TCHCustomMoveContainer)
  published
    property Active;
    property ActivePreview;
    property Buffered;
    property BevelOuter;
    property BevelInner;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Direction;
    property Fill;
    property MoveSpeed;
    property MoveStep;
    property ShowHint;
    property ParentColor;
    property Visible;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnTimer;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnMove;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHMoveContainer]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomMoveContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFill := TCHFillB.Create;
  FFill.OnChange := UpdateChanges;

  Width := 150;
  Height := 150;
  FMoveSpeed := 100;
  FMoveStep := 2;
  BevelOuter := bvNone;
  Caption := ' ';
  FFill.Transparent := False;
  FMoveTimer := TTimer.Create(Self);
  FMoveTimer.Enabled := False;
  FMoveTimer.Interval := FMoveSpeed;
  FMoveTimer.OnTimer := DoMove;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomMoveContainer.Destroy;
begin
  FFill.Free;
  FMoveTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.DoMove(Sender: TObject);
var
  x, y : Integer;
  ContainerRect : TRect;
begin
  // OnTimer event
  if Assigned(FOnTimer) then
    FOnTimer(Self);

  ContainerRect := BoundsRect;

  x := Left;
  y := Top;

  if Direction = mdUp then
  begin
    Dec(y, FMoveStep);
    if ContainerRect.Bottom < 0 then
      y := Parent.Height;
  end
  else if Direction = mdDown then
  begin
    Inc(y, FMoveStep);
    if ContainerRect.Top > Parent.Height then
      y := -Height;
  end
  else if Direction = mdLeft then
  begin
    Dec(x, FMoveStep);
    if ContainerRect.Right < 0 then
      x := Parent.Width;
  end
  else if Direction = mdRight then
  begin
    Inc(x, FMoveStep);
    if ContainerRect.Left > Parent.Width then
      x := -Width;
  end
  else if Direction = mdUpLeft then
  begin
    Dec(y, FMoveStep);
    if ContainerRect.Bottom < 0 then
      y := Parent.Height;
    Dec(x, FMoveStep);
    if ContainerRect.Right < 0 then
      x := Parent.Width;
  end
  else if Direction = mdUpRight then
  begin
    Dec(y, FMoveStep);
    if ContainerRect.Bottom < 0 then
      y := Parent.Height;
    Inc(x, FMoveStep);
    if ContainerRect.Left > Parent.Width then
      x := -Width;
  end
  else if Direction = mdDownLeft then
  begin
    Inc(y, FMoveStep);
    if ContainerRect.Top > Parent.Height then
      y := -Height;
    Dec(x, FMoveStep);
    if ContainerRect.Right < 0 then
      x := Parent.Width;
  end
  else if Direction = mdDownRight then
  begin
    Inc(y, FMoveStep);
    if ContainerRect.Top > Parent.Height then
      y := -Height;
    Inc(x, FMoveStep);
    if ContainerRect.Left > Parent.Width then
      x := -Width;
  end
  else
  begin
    x := Left;
    y := Top;
  end;

  if (x <> Left) or (y <> Top) then
  begin
    // OnMove event
    if Assigned(FOnMove) then
      FOnMove(Self);
    SetBounds(x, y, Width, Height);
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.Paint;
begin
  inherited Paint;

  FClientRect := GetClientRect;
  // Transparent
  if (FFill.Transparent) then
  begin
    DrawTransparentBmp(Self, Canvas);
    Canvas.CopyRect(FClientRect, canvas, FClientRect);
    Brush.Style := bsClear;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    if not (csDesigning in ComponentState) then
    begin
      if FActive then
        FMoveTimer.Enabled := True
      else
        FMoveTimer.Enabled := False;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetActivePreview(const Value: Boolean);
begin
  if FActivePreview <> Value then
  begin
    FActivePreview := Value;

    if FActivePreview then
    begin
      FSavLeft := Left;
      FSavTop := Top;
      FMoveTimer.Enabled := True;
    end
    else
    begin
      FMoveTimer.Enabled := False;
      Left := FSavLeft;
      Top := FSavTop;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetBuffered(Value: Boolean);
begin
  if FBuffered <> Value then
  begin
    FBuffered := Value;
    if (FBuffered = True) then
      Self.DoubleBuffered := True
    else
      Self.DoubleBuffered := False;

    Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetDirection(const Value: TMoveContDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetMoveSpeed(const Value: Cardinal);
begin
  if FMoveSpeed <> Value then
  begin
    FMoveSpeed := Value;
    FMoveTimer.Interval := FMoveSpeed;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.SetMoveStep(const Value: Word);
begin
  if FMoveStep <> Value then
  begin
    FMoveStep := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomMoveContainer.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  Invalidate;
end;


end.
