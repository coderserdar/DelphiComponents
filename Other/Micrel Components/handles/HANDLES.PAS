unit Handles;

{ A first attempt at a StretchHandle component for Delphi... it "owns"
  another component via the FChildControl property, and will resize or
  move its Child along with itself when it is dragged or stretched at
  runtime with the left mouse button. The component is sized 2 pixels
  larger on each side than its FChildControl, and the grab handles are
  5x5.  It will shrink to 5x5.

  I had a hard time getting things refreshed in the right order, especially
  on scrolling Forms... until I simply overrode the WM_ERASEBKGND for both
  Toverlay and TStretchHandle, doing nothing completely (not even calling
  inherited). }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Menus, StdCtrls;
                                       { miscellaneous type declarations }
type
  TDragStyle = (dsMove, dsSizeTopLeft, dsSizeTopRight, dsSizeBottomLeft, dsSizeBottomRight,
                dsSizeTop, dsSizeLeft, dsSizeBottom, dsSizeRight);
  EControlInvalid = class(Exception);
                                       { quick&dirty transparent overlay for rubberbanding }
type
  TOverlay = class(TCustomControl)
  private
    FDragRect: TRect;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    property Canvas;
  public
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Rubberband(NewRect: TRect);
  end;
                                       { TStretchHandle component declaration }
type
  TStretchHandle = class(TCustomControl)
  private
    FDragOffset: TPoint;
    FDragStyle: TDragStyle;
    FDragging: boolean;
    FColor: TColor;
    FChildControl: TControl;
    FOverlay: TOverlay;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMMouse); message WM_LBUTTONUP;
    function GetModifiedRect(XPos, YPos: integer): TRect;
  protected
    procedure Paint; override;
    property Canvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Attach(Child: TControl);
    procedure Detach;
    function IsAttached: boolean;
  published
                                       { new properties }
    property Color: TColor read FColor write FColor default clBlack;
    property Attached: boolean read IsAttached;
                                       { inherited properties }
    property DragCursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
                                       { defined events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
  end;

procedure Register;

implementation

procedure Register;
begin
                                       { add the component to the 'Samples' tab }
  RegisterComponents('Lib', [TStretchHandle]);

end;

constructor TStretchHandle.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
                                       { initialize default properties }
  Width := 24;
  Height := 24;
  FColor := clBlack;
  FChildControl := nil;
                                       { doesn't do anything until it is Attached to something else }
  Enabled := False;
  Visible := False;
                                       { instantiate canvas overlay component for rubberbanding }
  FOverlay := TOverlay.Create(Self);

  with FOverlay do
    begin
      Canvas.Pen.Mode := pmNotXor;
      Canvas.Brush.Style := bsClear;
      Enabled := False;
      Visible := True;
    end;

end;

destructor TStretchHandle.Destroy;
begin

  inherited Destroy;

end;

procedure TStretchHandle.CreateParams(var Params: TCreateParams);
begin
                                       { set default Params values }
  inherited CreateParams(Params);
                                       { then add transparency }
  Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;

end;

procedure TOverlay.CreateParams(var Params: TCreateParams);
begin
                                       { set default Params values }
  inherited CreateParams(Params);
                                       { then add transparency }
  Params.ExStyle := Params.ExStyle + WS_EX_TRANSPARENT;

end;

procedure TStretchHandle.Attach(Child: TControl);
begin
                                       { definitely not allowed! }
  if Child is TForm then
    raise EControlInvalid.Create('Handles can not be attached to this component.');
                                       { auto-detach if still attached (bad dog!) }
  if Attached then
    Detach;
                                       { allows StretchHandle to manage the child control's size & position }
  FChildControl := Child;
                                       { honour the lineage }
  Parent := Child.Parent;
  SetBounds(Child.Left - 2, Child.Top - 2, Child.Width + 5, Child.Height + 5);
  Enabled := True;
  Visible := True;
  BringToFront; 
                                       { adjust transparent, disabled canvas over everything }  
  with FOverlay do
    begin
      Parent := Child.Parent;
      SetBounds(0, 0, Parent.Width, Parent.Height);
      FDragRect := Rect(0, 0, 0, 0);
    end;

end;

procedure TStretchHandle.Detach;
begin
                                       { disable & hide StretchHandle }
  FChildControl := nil;
  Enabled := False;
  Visible := False;
  FOverlay.FDragRect := Rect(0, 0, 0, 0);

end;

function TStretchHandle.IsAttached;
begin
                                       
  Result := FChildControl = nil;

end;

procedure TStretchHandle.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
                                       { completely fake erase, don't call inherited, don't collect $200 }
  Message.Result := 1;

end;

procedure TOverlay.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
                                       { this very deliberately does nothing, seems to cure refresh hassles }
  Message.Result := 1;

end;

procedure TStretchHandle.WMMouseMove(var Message: TWMMouseMove);
var
  ARect, BRect: TRect;
  DragStyle: TDragStyle;
begin

  ARect := GetClientRect;
                                       { default cursor unless near a drag box }
  Cursor := DragCursor;
  DragStyle := dsMove;
                                       { so I don't like long nested if statements... }
  if ((Abs(Message.XPos - ARect.Left) < 5) and (Abs(Message.YPos - ARect.Top) < 5)) then
    begin
      DragStyle := dsSizeTopLeft;
      Cursor := crSizeNWSE;
    end;

  if ((Abs(Message.XPos - ARect.Right) < 5) and (Abs(Message.YPos - ARect.Bottom) < 5)) then
    begin
      DragStyle := dsSizeBottomRight;
      Cursor := crSizeNWSE;
    end;

  if ((Abs(Message.XPos - ARect.Right) < 5) and (Abs(Message.YPos - ARect.Top) < 5)) then
    begin
      DragStyle := dsSizeTopRight;
      Cursor := crSizeNESW;
    end;

  if ((Abs(Message.XPos - ARect.Left) < 5) and (Abs(Message.YPos - ARect.Bottom) < 5)) then
    begin
      DragStyle := dsSizeBottomLeft;
      Cursor := crSizeNESW;
    end;

  if ((Abs(Message.XPos - trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Message.YPos - ARect.Top) < 5)) then
    begin
      DragStyle := dsSizeTop;
      Cursor := crSizeNS;
    end;

  if ((Abs(Message.XPos - trunc(ARect.Right - ARect.Left) / 2) < 3) and (Abs(Message.YPos - ARect.Bottom) < 5)) then
    begin
      DragStyle := dsSizeBottom;
      Cursor := crSizeNS;
    end;

  if ((Abs(Message.YPos - trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(Message.XPos - ARect.Left) < 5)) then
    begin
      DragStyle := dsSizeLeft;
      Cursor := crSizeWE;
    end;

  if ((Abs(Message.YPos - trunc(ARect.Bottom - ARect.Top) / 2) < 3) and (Abs(Message.XPos - ARect.Right) < 5)) then
    begin
      DragStyle := dsSizeRight;
      Cursor := crSizeWE;
    end;
                                       { disallow drag off Parent }
  if FDragging then
    begin
      if (Left + Message.XPos) < 0 then
        Message.XPos := -Left;
      if (Top + Message.YPos) < 0 then
        Message.YPos := -Top;
      if (Left + Message.XPos) > Parent.Width then
        Message.XPos := Parent.Width - Left;
      if (Top + Message.YPos) > Parent.Height then
        Message.YPos := Parent.Height - Top;
                                       { display drag outline }
      FOverlay.RubberBand(GetModifiedRect(Message.XPos, Message.YPos));
    end
  else
    FDragStyle := DragStyle;
                                       { perform default processing }
  inherited;

  Message.Result := 0;

end;

procedure TStretchHandle.WMLButtonDown(var Message: TWMMouse);
begin
                                       { save position relative to Canvas, & which corner/side to drag }
  FDragOffset := Point(Message.XPos, Message.YPos);

  FDragging := True;
                                       { perform default processing }
  inherited;

  Message.Result := 0;

end;

procedure TStretchHandle.WMLButtonUp(var Message: TWMMouse);
var
  ARect: TRect;
begin
                                       { disallow drop off Parent }
  if (Left + Message.XPos) < 0 then
    Message.XPos := -Left;
  if (Top + Message.YPos) < 0 then
    Message.YPos := -Top;
  if (Left + Message.XPos) > Parent.Width then
    Message.XPos := Parent.Width - Left;
  if (Top + Message.YPos) > Parent.Height then
    Message.YPos := Parent.Height - Top;

  if FDragging then
    begin
                                       { obtain new coordinates }
      ARect := GetModifiedRect(Message.XPos, Message.YPos);
                                       { force Paint when size doesn't change but position does }
      if (ARect.Left <> Left) or (ARect.Top <> Top) then
        Invalidate;
                                       { resize, reposition if anything changed }
      SetBounds(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

      FDragging := False;
                                       { make sure rubberband is clean next time 'round }
      FOverlay.FDragRect := Rect(0, 0, 0, 0);

    end;
                                       { perform default processing }
  inherited;
                                       
  Message.Result := 0;

end;

function TStretchHandle.GetModifiedRect(XPos, YPos: integer): TRect;
var
  ARect: TRect;
begin
                                       { compute new position/size, depending on FDragStyle}
  case FDragStyle of

    dsSizeTopLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeTopRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeBottomLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;

    dsSizeBottomRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;

    dsSizeTop:
      begin
        ARect.Left := Left;
        ARect.Top := Top + (YPos - FDragOffset.Y);
        ARect.Right := Width;
        ARect.Bottom := Height - (ARect.Top - Top);
      end;

    dsSizeBottom:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width;
        ARect.Bottom := Height + (YPos - FDragOffset.Y);
      end;

    dsSizeLeft:
      begin
        ARect.Left := Left + (XPos - FDragOffset.X);
        ARect.Top := Top;
        ARect.Right := Width - (ARect.Left - Left);
        ARect.Bottom := Height;
      end;

    dsSizeRight:
      begin
        ARect.Left := Left;
        ARect.Top := Top;
        ARect.Right := Width + (XPos - FDragOffset.X);
        ARect.Bottom := Height;
      end;

  else
    ARect.Left := Left + (XPos - FDragOffset.X);
    ARect.Top := Top + (YPos - FDragOffset.Y);
    ARect.Right := Width;
    ARect.Bottom := Height;
  end;
                                       { impose a minimum size for sanity }
  if ARect.Right < 5 then
    ARect.Right := 5;
  if ARect.Bottom < 5 then
    ARect.Bottom := 5;

  Result := ARect;

end;

procedure TOverlay.Rubberband(NewRect: TRect);
begin

  BringToFront;
                                       { erase previous rectangle, if any, & adjust for TOverlay's position }
  if (FDragRect.Left <> 0) or (FDragRect.Top <> 0) or (FDragRect.Right <> 0) or (FDragRect.Bottom <> 0) then
    Canvas.Rectangle(FDragRect.Left + 2 - Left, FDragRect.Top + 2 - Top,
                     FDragRect.Left + FDragRect.Right - 3 - Left, FDragRect.Top + FDragRect.Bottom - 3 - Top);

  Canvas.Rectangle(NewRect.Left + 2 - Left, NewRect.Top + 2 - Top,
                   NewRect.Left + NewRect.Right - 3 - Left, NewRect.Top + NewRect.Bottom - 3 - Top);

  FDragRect := NewRect;

end;

procedure TStretchHandle.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
   WasVisible: boolean;
begin
                                       { hide & preserve fixed size in design mode }
  WasVisible := Visible;
  if csDesigning in ComponentState then
    begin
      Visible := False;
      inherited SetBounds(ALeft, ATop, 24, 24);
    end
  else                                 { move child also, if any }
    begin
      if FChildControl <> nil then
        FChildControl.SetBounds(ALeft + 2, ATop + 2, AWidth - 5, AHeight - 5);
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    end;
                                       { restore visibility }
  if Visible = False then
    Visible := WasVisible;

end;

procedure TStretchHandle.Paint;
var
   ARect, BoxRect: TRect;
begin

  inherited Paint;

  ARect := Rect(0, 0, Width - 1, Height - 1);

  with Canvas do
    begin

      Brush.Color := FColor;
                                       { draw corner boxes (assuming Canvas is minimum 5x5) }
      BoxRect := Rect(ARect.Left, ARect.Top, ARect.Left + 5, ARect.Top + 5);
      FillRect(BoxRect);
      BoxRect := Rect(ARect.Right - 5, ARect.Top, ARect.Right, ARect.Top + 5);
      FillRect(BoxRect);
      BoxRect := Rect(ARect.Right - 5, ARect.Bottom - 5, ARect.Right, ARect.Bottom);
      FillRect(BoxRect);
      BoxRect := Rect(ARect.Left, ARect.Bottom - 5, ARect.Left + 5, ARect.Bottom);
      FillRect(BoxRect);
                                       { draw center boxes (favouring 1 pixel to the right for even sides) }

      BoxRect := Rect(trunc((ARect.Right - ARect.Left) / 2) - 2,
                      ARect.Top,
                      trunc((ARect.Right - ARect.Left) / 2) + 3,
                      ARect.Top + 5);
      FillRect(BoxRect);
      BoxRect := Rect(trunc((ARect.Right - ARect.Left) / 2) - 2,
                      ARect.Bottom - 5,
                      trunc((ARect.Right - ARect.Left) / 2) + 3,
                      ARect.Bottom);
      FillRect(BoxRect);
      BoxRect := Rect(ARect.Left,
                      trunc((ARect.Bottom - ARect.Top) / 2) - 2,
                      ARect.Left + 5,
                      trunc((ARect.Bottom - ARect.Top) / 2) + 3);
      FillRect(BoxRect);
      BoxRect := Rect(ARect.Right - 5,
                      trunc((ARect.Bottom - ARect.Top) / 2) - 2,
                      ARect.Right,
                      trunc((ARect.Bottom - ARect.Top) / 2) + 3);
      FillRect(BoxRect);

    end;

end;

end.
