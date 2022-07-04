unit mbXPSizeGrip;

interface

uses
  SysUtils, Classes, Windows, Controls, Themes, Messages;

type
  TmbXPSizeGrip = class(TCustomControl)
  protected
   procedure Paint; override;
   procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
  published
   property Anchors;
   property Align;
   property PopupMenu;
   property Constraints;
   property ParentBackground default true;
   property Color;
   property ParentColor;
   property Hint;
   property ShowHint;
   property ParentShowHint;

   property OnContextPopup;
   property OnMouseMove;
   property OnMouseDown;
   property OnMouseUp;
  end;

implementation

constructor TmbXPSizeGrip.Create(AOwner: TComponent);
begin
 inherited;
 DoubleBuffered := true;
 ControlStyle := ControlStyle - [csAcceptsControls] + [csOpaque, csParentBackground];
 Width := 15;
 Height := 15;
 TabStop := false;
end;

destructor TmbXPSizeGrip.Destroy;
begin
 inherited;
end;

procedure TmbXPSizeGrip.Paint;
begin
 if ThemeServices.ThemesEnabled then
  ThemeServices.DrawElement(Canvas.Handle, ThemeServices.GetElementDetails(tsGripper), Rect(Width - 15, Height - 15, Width, Height))
 else
  begin
   Canvas.Brush.Color := Color;
   DrawFrameControl(Canvas.Handle, Rect(Width - 15, Height - 15, Width, Height), DFC_SCROLL, DFCS_SCROLLSIZEGRIP );
  end;
end;

procedure TmbXPSizeGrip.WMNCHitTest(var Message: TWMNCHitTest);
var
 p: TPoint;
begin
 inherited;
 p := ScreenToClient(SmallPointToPoint(Message.Pos));
 if not (csDesigning in ComponentState) then
  if (Message.Result = HTCLIENT) and PtInRect(Rect(Width - 15, Height - 15, Width, Height), p) then
   Message.Result := HTBOTTOMRIGHT;
end;

end.
