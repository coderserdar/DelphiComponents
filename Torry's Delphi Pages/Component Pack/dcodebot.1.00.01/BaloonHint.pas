
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BaloonHint;

interface

{$I STD.INC}

uses
  Forms, Windows, Graphics, Messages, Controls, Classes;

{ Default background color for baloon hint window }

const
  COLOR_TOOLTIP = $E1FFFF;

{ Helper baloon routines }

function CreateBaloonRgn(const Rect: TRect): HRGN;
procedure FillBaloonRgn(DC: HDC; Rgn: HRGN);
procedure SetBaloonWindow(Wnd: HWND; const Rect: TRect);

{ TBaloonHintWindow }

type
  TCustomDrawHintEvent = procedure(Sender: TObject; Rect: TRect;
    Data: Pointer) of object;

  TBaloonHintWindow = class(THintWindow)
  private
    FActive: Boolean;
    FClientRect: TRect;
    FControl: TWinControl;
    FCursorPos: TPoint;
    FData: Pointer;
    FDefControlProc: TWndMethod;
    FHintDelay: Cardinal;
    FHintTime: Cardinal;
    FPriorCursor: TCursor;
    FRegion: HRGN;
    FTimer: Cardinal;
    FOnCancel: TNotifyEvent;
    FOnCustomDraw: TCustomDrawHintEvent;
    FOnQueryHint: TNotifyEvent;
    procedure ControlProc(var Message: TMessage);
    procedure SetControl(Value: TWinControl);
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
    procedure Cancel;
    property Control: TWinControl read FControl write SetControl;
    property Data: Pointer read FData;
    property HintDelay: Cardinal read FHintDelay write FHintDelay;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnCustomDraw: TCustomDrawHintEvent read FOnCustomDraw write
      FOnCustomDraw;
    property OnQueryHint: TNotifyEvent read FOnQueryHint write FOnQueryHint;
  end;

implementation

function CreateBaloonRgn(const Rect: TRect): HRGN;
var
  Points: array of TPoint;
begin
  with Rect do
  begin
    SetLength(Points, 19);
    Points[0] := Point(Left, Top + 10);
    Points[1] := Point(Left + 2, Top + 5);
    Points[2] := Point(Left + 5, Top + 2);
    Points[3] := Point(Left + 10, Top + 0);
    Points[4] := Point(Right - 10, Top);
    Points[5] := Point(Right - 5, Top + 2);
    Points[6] := Point(Right - 2, Top + 5);
    Points[7] := Point(Right, Top + 10);
    Points[8] := Point(Right, Bottom - 30);
    Points[9] := Point(Right - 2, Bottom - 25);
    Points[10] := Point(Right - 5, Bottom - 22);
    Points[11] := Point(Right - 10, Bottom - 20);
    Points[12] := Point(Left + (Right - Left) div 2 + 20, Bottom - 20);
    Points[13] := Point(Right, Bottom);
    Points[14] := Point(Left + (Right - Left) div 2 - 20, Bottom - 20);
    Points[15] := Point(Left + 10, Bottom - 20);
    Points[16] := Point(Left + 5, Bottom - 22);
    Points[17] := Point(Left + 2, Bottom - 25);
    Points[18] := Point(Left, Bottom - 30);
  end;
  Result := CreatePolygonRgn(Pointer(Points)^, Length(Points), WINDING);
end;

procedure FillBaloonRgn(DC: HDC; Rgn: HRGN);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(COLOR_TOOLTIP);
  FillRgn(DC, Rgn, Brush);
  DeleteObject(Brush);
  FrameRgn(DC, Rgn, GetStockObject(BLACK_BRUSH), 1, 1);
end;

procedure SetBaloonWindow(Wnd: HWND; const Rect: TRect);
begin
  SetWindowRgn(Wnd, CreateBaloonRgn(Rect), True);
end;

{ TBaloonHintWindow }

constructor TBaloonHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := COLOR_TOOLTIP;
  Visible := False;
  FHintDelay := 3000;
end;

destructor TBaloonHintWindow.Destroy;
begin
  if FRegion <> 0 then
    DeleteObject(FRegion);
  SetControl(nil);
  inherited Destroy;
end;

procedure TBaloonHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  ActivateHintData(Rect, AHint, nil);
end;

procedure TBaloonHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
var
  WindowRect: TRect;
begin
  Cancel;
  if FControl <> nil then
  begin
    FData := AData;
    FClientRect := Rect;
    with FClientRect do
      OffsetRect(FClientRect, 5 - Left, 5 - Top);
    WindowRect := Rect;
    InflateRect(WindowRect, 5, 5);
    Inc(WindowRect.Bottom, 20);
    Rect := WindowRect;
    OffsetRect(Rect, 0, -25);
    with WindowRect do
     OffsetRect(WindowRect, -Left, -Top);
    if FRegion <> 0 then
      DeleteObject(FRegion);
    FRegion := 0;
    FRegion := CreateBaloonRgn(WindowRect);
    SetBaloonWindow(Handle, WindowRect);
    FActive := True;
    FPriorCursor := Screen.Cursor;
    Screen.Cursor := crNone;
    inherited ActivateHint(Rect, AHint);
  end;
end;

procedure TBaloonHintWindow.Cancel;
begin
  ShowWindow(Handle, SW_HIDE);
  if FActive then
  begin
    Screen.Cursor := FPriorCursor;
    if Assigned(FOnCancel) then
      FOnCancel(Self);
  end;
  FActive := False;
end;

procedure TBaloonHintWindow.ControlProc(var Message: TMessage);
var
  Point: TPoint;
begin
  with Message do
    case Msg of
      WM_MOUSEMOVE:
        begin
          GetCursorPos(Point);
          if (Point.X <> FCursorPos.X) or (Point.Y <> FCursorPos.Y) then
            Cancel;
          if not IsWindowVisible(Handle) then
          begin
            FHintTime := GetTickCount;
            if FTimer = 0 then
              FTimer := SetTimer(Handle, 1, 250, nil);
          end;
          FCursorPos := Point;
        end;
      WM_LBUTTONDOWN..WM_MOUSELAST:
         Cancel;
      WM_KILLFOCUS:
        begin
          Cancel;
          if FTimer <> 0 then
            KillTimer(Handle, FTimer);
          FTimer := 0;
        end;
   end;
  FDefControlProc(Message);
end;

procedure TBaloonHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and (not WS_BORDER);
end;

procedure TBaloonHintWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FControl) then
    SetControl(nil);
end;

procedure TBaloonHintWindow.Paint;
begin
  FillBaloonRgn(Canvas.Handle, FRegion);
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, FClientRect, FData);
end;

procedure TBaloonHintWindow.SetControl(Value: TWinControl);
begin
  if Value <> FControl then
  begin
    Cancel;
    if FControl <> nil then
    begin
      FControl.WindowProc := FDefControlProc;
      FControl.RemoveFreeNotification(Self);
    end;
    FControl := Value;
    if FControl <> nil then
    begin
      FControl.FreeNotification(Self);
      FDefControlProc := FControl.WindowProc;
      FControl.WindowProc := ControlProc;
    end;
  end;
end;

procedure TBaloonHintWindow.WMTimer(var Message: TWMTimer);
var
  Point: TPoint;
begin
  if GetTickCount - FHintTime > FHintDelay then
  begin
    GetCursorPos(Point);
    if (Point.X <> FCursorPos.X) or (Point.Y <> FCursorPos.Y) then
      Cancel
    else if (not IsWindowVisible(Handle)) and Assigned(FOnQueryHint) then
      FOnQueryHint(Self);
  end;
end;

end.
