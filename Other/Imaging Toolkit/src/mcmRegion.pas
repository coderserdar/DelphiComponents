// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //
//                                                                            //
// For further information / comments, visit our WEB site at                  //
//   www.mcm-design.com                                                       //
// or e-mail to                                                               //
//   CustomerCare@mcm-design.dk                                               //
//----------------------------------------------------------------------------//
//
// $Log:  17585: mcmRegion.pas
//
//    Rev 1.6    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.5    07-03-2004 14:36:00  mcm    Version: IMG 2.4
// Corrected calculation of client coordinates to be in-time in WM_NCHITTEST. 
// Removed double occurance of the OnMouseDown/Up events.

//
//   Rev 1.4    21-01-2004 12:21:46  mcm    Version: IMG 2.3
// Modified how the TmcmRegion control is displayed and updated when properties
// and position are changed.

//
//   Rev 1.3    08-03-2003 20:01:02  mcm    Version: IMG 1.3.2
// Added method to ensure that the TmcmRegion windows isn't updated when when
// mouse-moves result in no position change (cursor outside the border).
// Added method to update the border when the Pen changes.
// Added checks and events for Mouse/Down/Up and Move.

//
//   Rev 1.2    27-01-2003 13:45:58  mcm
// Added Image property, to show "copied" image section. 

//
//   Rev 1.1    27-09-2002 13:25:38  mcm

//
//   Rev 1.0    27-05-2002 16:22:26  mcm

unit mcmRegion;

interface

{$Include 'mcmDefines.pas'}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Controls,
     {$ENDIF}
     mcmImage;

type
  TmcmRegion = class(TCustomControl)
  private
    { Private declarations }
    FCanResize    : boolean;
    FChanged      : boolean;
    FUpdateParent : boolean;
    FLinePen      : TPen;
    FImage        : TmcmImage;
    FLBDown       : boolean;
  protected
    { Protected declarations }
    FxMax       : integer;
    FyMax       : integer;
    FScale      : double;
    FOnResized  : TNotifyEvent;
    FOnMoved    : TNotifyEvent;
    function    GetBottom : integer;
    function    GetCanResize : boolean;
    function    GetImage : TmcmImage;
    function    GetHeight : integer;
    function    GetRight : integer;
    function    GetWidth : integer;
    procedure   OnPenChanged(Sender : TObject);
    procedure   SetBottom(Value : integer);
    procedure   SetHeight(Value : integer);
    procedure   SetImage(Value : TmcmImage);
    procedure   SetLinePen(Value : TPen);
    procedure   SetMaxX(Value : integer);
    procedure   SetMaxY(Value : integer);
    procedure   SetRight(Value : integer);
    procedure   SetScale(Value : double);
    procedure   SetWidth(Value : integer);
    procedure   CreateParams    (var Params    : TCreateParams);  override;
    procedure   WmNcHitTest     (var Msg       : TWmNcHitTest);   message WM_NCHITTEST;
    procedure   WMWinPosChanging(var Msg       : TMessage);       message WM_WINDOWPOSCHANGING;
    procedure   WMWinPosChanged (var Msg       : TMessage);       message WM_WINDOWPOSCHANGED;
    procedure   WMGetMinMaxInfo (var Msg       : TMessage);       message WM_GETMINMAXINFO;
    procedure   WmSize          (var Msg       : TWmSize);        message WM_SIZE;
    procedure   WmMove          (var Msg       : TWmMove);        message WM_MOVE;
    procedure   WmMoving        (var Msg       : TMessage);       message WM_MOVING;
    procedure   WMNCLButtonDown (var Msg       : TMessage);       message WM_NCLBUTTONDOWN;
    procedure   WMNCLButtonUp   (var Msg       : TMessage);       message WM_NCLBUTTONUP;
  public
    { Public declarations }
    constructor Create          (    AOwner    : TComponent);     override;
    destructor  Destroy;                                          override;
    procedure   Paint;                                            override;
    property    Bottom : integer
      read      GetBottom
      write     SetBottom;
    property    Image : TmcmImage
      read      GetImage
      write     SetImage;
    property    Right : integer
      read      GetRight
      write     SetRight;
  published
    { Published declarations }
    property    AllowResize : boolean
      read      GetCanResize
      write     FCanResize;
    property    Cursor;
    property    Height : integer
      read      GetHeight
      write     SetHeight;
    property    Left;
    property    LinePen : TPen
      read      FLinePen
      write     SetLinePen;
    property    PopupMenu;
    property    MaxHeight : integer
      read      FyMax
      write     SetMaxY default 100;
    property    MaxWidth : integer
      read      FxMax
      write     SetMaxX default 100;
    property    Scale : double
      read      FScale
      write     SetScale;
    property    ShowHint;
    property    Top;
    property    Width : integer
      read      GetWidth
      write     SetWidth;
      
    // Events
    property    OnClick;
    property    OnDblClick;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnMoved : TNotifyEvent
      read      FOnMoved
      write     FOnMoved;
    property    OnResized : TNotifyEvent
      read      FOnResized
      write     FOnResized;
  end;

implementation

{$IFDEF GE_DXE2}
 uses System.Types;
{$ELSE}
 {$IFDEF GE_DXE} uses Types; {$ENDIF}
{$ENDIF}

const EdgeSize = 5;

constructor TmcmRegion.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FImage := Nil;
  ControlStyle := ControlStyle + [csCaptureMouse,csClickEvents,csReflector]
                               - [csOpaque,csAcceptsControls];
  FLinePen        := TPen.Create;
  FLinePen.OnChange := OnPenChanged;
  FLinePen.Color  := CLRED;
  FLinePen.Style  := PSDOT;
  FLinePen.Width := 1;

  FOnResized  := Nil;
  FOnMoved    := Nil;
  FCanResize  := True;
  FUpdateParent := False;
  FScale      := 1.0;
  try
    FxMax := TControl(AOwner).Width;
    FyMax := TControl(AOwner).Height;
  except
    FxMax := 100;
    FyMax := 100;
  end;
  Width       := 32;
  Height      := 32;
  FLBDown     := False;
end; // TmcmRegion.Create.


destructor TmcmRegion.Destroy;
begin
  if Assigned(FImage)
  then FImage.Free;
  FImage := Nil;
  if Assigned(FLinePen)
  then FLinePen.Free;
  FLinePen := Nil;
  FChanged := False;
  Inherited Destroy;
end; // TmcmRegion.Destroy.


procedure TmcmRegion.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  if Not(csDesigning in ComponentState)
  then Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end; // TmcmRegion.CreateParams.


procedure TmcmRegion.OnPenChanged(Sender : TObject);
var Rect : TRect;
begin
  if (Parent <> Nil)
  then begin
       Rect := BoundsRect;
       if (Parent is TWinControl)
       then InvalidateRect(Parent.Handle, @Rect, False);
       InvalidateRect(Handle, Nil, (csDesigning in ComponentState));
  end;
end; // TmcmRegion.OnPenChanged.


procedure TmcmRegion.WMGetMinMaxInfo(var Msg : TMessage);
begin
  inherited;
  with PMINMAXINFO(Msg.lParam)^
  do begin
     //POINT ptReserved;
     ptMaxSize.x := FxMax;
     ptMaxSize.y := FyMax;
     ptMaxPosition.x := FxMax;
     ptMaxPosition.y := FyMax;
     // Parameters for sizeing this window, ie. Min & Max size.
     ptMinTrackSize.x := 0;
     ptMinTrackSize.y := 0;
     ptMaxTrackSize.x := FxMax;
     ptMaxTrackSize.y := FyMax;
  end;
  Msg.Result := 0;
end; // TmcmRegion.WMGetMinMaxInfo.


procedure TmcmRegion.WmNcHitTest(var Msg : TWmNcHitTest);
var Pt, CPt, PPt : TPoint;
    InRect       : TRect;
    DoOnMove     : boolean;
begin
  inherited;
  if (csDesigning in ComponentState) or
     (GetKeyState(VK_RBUTTON) < 0)
  then inherited // Insures normal behaviour during design time and right click.
  else begin
       DoOnMove := True;
       Pt := Point(Msg.XPos, Msg.YPos);

       // Calculate parent x,y coordinate.
       PPt := Parent.ScreenToClient(Pt);

       case GetAsyncKeyState(VK_LBUTTON) of
       1 : ; // Key changed.
       0 : if (FLBDown) // move - button is Up
           then begin
                if Assigned(OnMouseUp)
                then OnMouseUp(Self, mbLeft, [], PPt.x, PPt.y);
                FLBDown := False;
                DoOnMove := False;
           end;
       else if Not(FLBDown) // Button is Down
            then begin
                 FLBDown := True;
                 if Assigned(OnMouseDown)
                 then OnMouseDown(Self, mbLeft, [], PPt.x, PPt.y);
                 DoOnMove := False;
            end;
       end;

       if Assigned(OnMouseMove) and DoOnMove
       then begin
            // Fire OnMouseMove event.
            OnMouseMove(Self, [], PPt.x, PPt.y);
       end;

       CPt := ScreenToClient(Pt);
       if FCanResize
       then begin
            InRect := ClientRect;
            inc(InRect.Left, EdgeSize);
            inc(InRect.Top, EdgeSize);
            dec(InRect.Right, EdgeSize);
            dec(InRect.Bottom, EdgeSize);

            if PtInRect(InRect, CPt)
            then Msg.Result := HTCAPTION // Cursor in the centre
            else if (CPt.x < EdgeSize) and (CPt.y < EdgeSize)
                 then Msg.Result := htTopLeft // Cursor on top-left edge
                 else if (CPt.x > Width - EdgeSize) and (CPt.y < EdgeSize)
                      then Msg.Result := htTopRight // Cursor on top-pight edge
                      else if (CPt.x > Width - EdgeSize) and (CPt.y > Height - EdgeSize)
                           then Msg.Result := htBottomRight // Cursor on bottom-right edge
                           else if (CPt.x < EdgeSize) and (CPt.y > Height - EdgeSize)
                                then Msg.Result := htBottomLeft // Cursor on bottom-left edge
                                else if (CPt.x < EdgeSize)
                                     then Msg.Result := htLeft // Cursor on left edge
                                     else if (CPt.y < EdgeSize)
                                          then Msg.Result := htTop // Cursor on top edge
                                          else if (CPt.x > Width - EdgeSize)
                                               then Msg.Result := htRight // Cursor on right edge
                                               else if (CPt.y > Height - EdgeSize)
                                                    then Msg.Result := htBottom // Cursor on bottom edge
                                                    else inherited;
       end
       else begin // Can only move window - not resize.
            if PtInRect(ClientRect, CPt)
            then Msg.Result := HTCAPTION
            else inherited;
       end;
  end;
end; // TmcmRegion.WmNcHitTest.


procedure TmcmRegion.WMWinPosChanging(var Msg : TMessage);
var hx, vy : integer;
begin
  Inherited ;
  with PWINDOWPOS(Msg.lParam)^
  do begin
     // Get parent scroll position.
     hx := 0;
     vy := 0;
     if Assigned(Parent)
     then begin
          if (Parent is TScrollingWinControl)
          then begin
               hx := TScrollingWinControl(Parent).HorzScrollBar.Position;
               vy := TScrollingWinControl(Parent).VertScrollBar.Position;
          end;
     end;

     // Check width and height
     if (cy > FyMax)
     then cy := FyMax;
     if (cx > FxMax)
     then cx := FxMax;

     // Check positions
     if (x < -hx)
     then x := -hx;
     if (y < -vy)
     then y := -vy;
     if (x + cx >= FxMax - hx)
     then x := FxMax - cx - hx;
     if (y + cy >= FyMax - vy)
     then y := FyMax - cy - vy;

     // Make sure that a "moving"-frame is drawn and that background content
     // isn't copied.
     flags := flags or SWP_DRAWFRAME and not SWP_NOCOPYBITS;
  end;
  Msg.Result := 0;
end; // TmcmRegion.WMWinPosChanging.


procedure TmcmRegion.WMWinPosChanged(var Msg : TMessage);
begin
  Inherited ;
  Msg.Result := 0;
end; // TmcmRegion.WMWinPosChanged.


procedure TmcmRegion.WmSize(var Msg : TWmSize);
begin
  Inherited ;
  if Assigned(FOnResized) and Not(FChanged)
  then begin
       if (Width >= 0) and (Height >= 0) and Visible
       then FOnResized(Self);
  end;
  FChanged := False;
  Msg.Result := 0;
end; // TmcmRegion.WmSize.


procedure TmcmRegion.WmMoving(var Msg : TMessage);
var rh, rw : integer;
    pt     : TPoint;
    hx, vy : integer;
begin
  Inherited ;
  if (Parent <> Nil)
  then begin
       if (Parent is TWinControl)
       then begin
            // Get parent scroll position.
            hx := 0;
            vy := 0;
            if Assigned(Parent)
            then begin
                 if (Parent is TScrollingWinControl)
                 then begin
                      hx := TScrollingWinControl(Parent).HorzScrollBar.Position;
                      vy := TScrollingWinControl(Parent).VertScrollBar.Position;
                 end;
            end;

            pt := Parent.ClientToScreen(Point(-hx,-vy));
            rh := Self.Height;
            rw := Self.Width;
            if (PRect(Msg.LParam)^.Left < pt.x)
            then begin
                 PRect(Msg.LParam)^.Left := pt.x;
                 PRect(Msg.LParam)^.Right := PRect(Msg.LParam)^.Left + rw;
            end;
            if (PRect(Msg.LParam)^.Top < pt.y)
            then begin
                 PRect(Msg.LParam)^.Top := pt.y;
                 PRect(Msg.LParam)^.Bottom := PRect(Msg.LParam)^.Top + rh;
            end;

            pt := Parent.ClientToScreen(Point(FxMax-hx,FyMax-vy));
            if (PRect(Msg.LParam)^.Right > pt.x)
            then begin
                 PRect(Msg.LParam)^.Right := pt.x;
                 PRect(Msg.LParam)^.Left := PRect(Msg.LParam)^.Right - rw;
            end;
            if (PRect(Msg.LParam)^.Bottom > pt.y)
            then begin
                 PRect(Msg.LParam)^.Bottom := pt.y;
                 PRect(Msg.LParam)^.Top := PRect(Msg.LParam)^.Bottom - rh;
            end;
            Msg.Result := 0;
       end;
  end;
end; // TmcmRegion.WmMoving.


procedure TmcmRegion.WmMove(var Msg : TWmMove);
begin
  Inherited ;
  FChanged := False;
  if Visible
  then begin
       if Assigned(FOnMoved)
       then FOnMoved(Self);
  end;
  FUpdateParent := True;
  Msg.Result := 0;
end; // TmcmRegion.WmMove.


procedure TmcmRegion.WMNCLButtonDown(var Msg : TMessage);
begin
  Inherited ;
  FLBDown := True;
  {
  if Assigned(OnMouseDown)
  then OnMouseDown(Self, mbLeft, [], Loword(Msg.LParam), Hiword(Msg.LParam));
  }
  Msg.Result := 0;
end; // TmcmRegion.WMNCLButtonDown.


procedure TmcmRegion.WMNCLButtonUp(var Msg : TMessage);
begin
  Inherited ;
  FLBDown := False;
  {
  if Assigned(OnMouseUp)
  then OnMouseUp(Self, mbLeft, [], Loword(Msg.LParam), Hiword(Msg.LParam));
  }
  Msg.Result := 0;
end; // TmcmRegion.WMNCLButtonUp.


procedure TmcmRegion.Paint;
var Rect : TRect;
begin
  // Inherited ;
  if ((FxMax > 0) and (FyMax > 0) and Visible) or (csDesigning in ComponentState)
  then begin
       if FUpdateParent
       then begin
            FUpdateParent := False;
            // Special case that handles scrolling out and into view.
            if Assigned(Parent)
            then begin
                 if (Parent is TWinControl)
                 then begin
                      Rect := BoundsRect;
                      InvalidateRect(Parent.Handle, @Rect, False);
                      Parent.Update;
                      InvalidateRect(Handle, Nil, False);
                 end;
            end;
       end;

       // Paint select rectangle.
       if (FImage <> Nil)
       then begin
            FImage.SetOrigo(Point(0, 0));
            FImage.Draw(GetDC(Handle), FScale);
       end;

       Canvas.Pen.Assign(FLinePen);
       if (csDesigning in ComponentState)
       then begin
            Canvas.Brush.Style := BSSOLID;
            Canvas.Brush.Color := CLWHITE;
       end
       else Canvas.Brush.Style := BSCLEAR;
       Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
  end;
end; // TmcmRegion.Paint.


function TmcmRegion.GetCanResize : boolean;
begin
  if (FImage <> Nil)
  then Result := FCanResize
  else Result := FCanResize;
end; // TmcmRegion.GetCanResize.


function TmcmRegion.GetImage : TmcmImage;
begin
  if (FImage = Nil)
  then FImage := TmcmImage.Create;
  Result := FImage;
end; // TmcmRegion.GetImage.


procedure TmcmRegion.SetImage(Value : TmcmImage);
begin
  if (FImage <> Nil)
  then FImage.Free;
  FImage := Nil;
  FImage := Value;
  if (FImage <> Nil)
  then begin
       Height := Round(FScale * FImage.Height);
       Width  := Round(FScale * FImage.Width);
  end;
  InvalidateRect(Handle, Nil, False);
end; // TmcmRegion.SetImage.


function TmcmRegion.GetHeight : integer;
begin
  Result := Inherited Height;
end; // TmcmRegion.GetHeight.


procedure TmcmRegion.SetHeight(Value : integer);
begin
  Inherited Height := Value;
end; // TmcmRegion.SetHeight.


function TmcmRegion.GetWidth : integer;
begin
  Result := Inherited Width;
end; // TmcmRegion.GetWidth.


procedure TmcmRegion.SetWidth(Value : integer);
begin
  Inherited Width := Value;
end; // TmcmRegion.SetWidth.


function TmcmRegion.GetRight : integer;
begin
  Result := Left + Inherited Width;
end; // TmcmRegion.GetRight.


procedure TmcmRegion.SetRight(Value : integer);
begin
  if (Value < 0)
  then Value := 0;
  if (Value > FxMax)
  then Value := FxMax;
  Inherited Width := (Value - Left);
end; // TmcmRegion.SetRight.


function TmcmRegion.GetBottom : integer;
begin
  Result := Top + Inherited Height;
end; // TmcmRegion.GetBottom.


procedure TmcmRegion.SetBottom(Value : integer);
begin
  if (Value < 0)
  then Value := 0;
  if (Value > FyMax)
  then Value := FyMax;
  Inherited Height := (Value - Top);
end; // TmcmRegion.SetBottom.


procedure TmcmRegion.SetScale(Value : double);
var ARect : TRect;
begin
  ARect        := BoundsRect;
  ARect.Left   := Round(ARect.Left / FScale);
  ARect.Top    := Round(ARect.Top / FScale);
  ARect.Right  := Round(ARect.Right / FScale);
  ARect.Bottom := Round(ARect.Bottom / FScale);

  FScale := Value;

  ARect.Left   := Round(ARect.Left * FScale);
  ARect.Top    := Round(ARect.Top * FScale);
  ARect.Right  := Round(ARect.Right * FScale);
  ARect.Bottom := Round(ARect.Bottom * FScale);

  BoundsRect := ARect;
  FChanged   := True;
end; // TmcmRegion.SetScale.


procedure TmcmRegion.SetMaxX(Value  : integer);
begin
  FxMax := Value;
  if (Width > FxMax)
  then Width := FxMax;
  if ((Left + Width) > FxMax)
  then Left := FxMax - Width;
end; // TmcmRegion.SetMaxX.


procedure TmcmRegion.SetMaxY(Value  : integer);
begin
  FyMax := Value;
  if (Height > FyMax)
  then Height := FyMax;
  if ((Top + Height) > FyMax)
  then Top := FyMax - Height;
end; // TmcmRegion.SetMaxY.


procedure TmcmRegion.SetLinePen(Value : TPen);
begin
  FLinePen.Assign(Value);
end; // TmcmRegion.SetLinePen.

end.

