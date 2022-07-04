
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Grip;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Math, StreamTools;

{ The TGripForm is a base form class that defines a dialog style window capable
  of resizing. A graphical grip will be drawn in the lower right corner of any
  class that derives from TGripForm. The TGripForm class also introduces several
  methods form controling form storage and sizing options }

type
  TAnchorData = record
    Control: TControl;
    Anchors: TAnchors;
  end;

  TAnchorDataArray = array of TAnchorData;

  TGripForm = class(TForm)
  private
    FAnchors: TAnchorDataArray;
    FHitBounds: TRect;
    FHitTest: Integer;
    FGripBox: TPaintBox;
    FStream: TStream;
    FSizeable: Boolean;
    FLoaded: Boolean;
    procedure GripPaintBoxPaint(Sender: TObject);
    procedure SetStream(Value: TStream);
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMNCHitText(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
  protected
    procedure AlignBorders(AWidth, AHeight: Integer); virtual;
    function CanLoad: Boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    function GetOffsets: TSize; dynamic;
    procedure Load;
    procedure ReadData(Stream: TStream); dynamic;
    procedure RestoreAnchors;
    procedure RestoreGrip;
    procedure Save;
    procedure SaveAnchors;
    procedure WriteData(Stream: TStream); dynamic;
    property GripBox: TPaintBox read FGripBox;
  public
    constructor Create(AOwner: TComponent); override;
    function ShowModal: Integer; override;
    property Stream: TStream read FStream write SetStream;
    property Sizeable: Boolean read FSizeable write FSizeable;
  end;

  TGripFormClass = class of TGripForm;

{ TOrderedForm }

  TOrderedForm = class(TGripForm)
  private
    FPriorVisible: Boolean;
    FIdent: Integer;
  protected
    procedure Activate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Ident: Integer read FIdent write FIdent;
  end;

procedure HideOrderedWindows;
procedure ShowOrderedWindows;

function GetOrderedWindowCount: Integer;
function GetOrderedWindow(Index: Integer): TOrderedForm;

implementation

{$R *.DFM}

uses
  Consts;

const
  OFFSET_PAINTBOX = 18;

{ TGripForm }

constructor TGripForm.Create(AOwner: TComponent);
var
  Size: TSize;
  MaxWidth: Integer;
  MaxHeight: Integer;
  Control: TControl;
  I: Integer;
begin
  inherited Create(AOwner);
  DesktopFont := True;
  FSizeable := True;
  FGripBox := TPaintBox.Create(Self);
  with FGripBox do
  begin
    Parent := Self;
    RestoreGrip;
    Anchors := [akRight, akBottom];
    OnPaint := GripPaintBoxPaint;
  end;
  SaveAnchors;
  Size := GetOffsets;
  MaxWidth := 100;
  MaxHeight := 100;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control is TWinControl) and (Control.Parent = Self) then
    begin
      MaxWidth := Max(Control.Left + Control.Width + Size.cx, MaxWidth);
      MaxHeight := Max(Control.Top + Control.Height + Size.cy, MaxHeight);
    end;
  end;
  ClientHeight := MaxHeight;
  ClientWidth := MaxWidth;
  AlignBorders(Width, Height);
  RestoreAnchors;
end;

procedure TGripForm.AlignBorders(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

function TGripForm.CanLoad: Boolean;
begin
  Result := False;
end;

procedure TGripForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_SIZEBOX;
    if Owner is TWinControl then
      Params.WndParent := (Owner as TWinControl).Handle;
  end;
end;

procedure TGripForm.Load;
begin
  if (FStream <> nil) and (FStream.Size > 0) then
    ReadData(FStream);
end;

procedure TGripForm.DoShow;
begin
  inherited DoShow;
  if CanLoad  or (not FLoaded) then
  begin
    FLoaded := True;
    Load;
  end;
end;

procedure TGripForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if (Action <> caNone)  then
    Save;
end;

function TGripForm.GetOffsets: TSize;
begin
  Result.cx := 8;
  Result.cy := 8;
end;

procedure TGripForm.ReadData(Stream: TStream);
begin
  SetStreamWorker(Stream);
  Stream.Seek(0, soFromBeginning);
  Left := ReadInteger;
  Top := ReadInteger;
  Width := ReadInteger;
  Height := ReadInteger;
end;

procedure TGripForm.RestoreAnchors;
var
  I: Integer;
begin
  for I := Low(FAnchors) to High(FAnchors) do
    with FAnchors[I] do
      if Control <> FGripBox then
        Control.Anchors := Anchors;
  FAnchors := nil;
end;

procedure TGripForm.RestoreGrip;
begin
  with FGripBox do
    SetBounds(Self.ClientWidth - OFFSET_PAINTBOX,
      Self.ClientHeight - OFFSET_PAINTBOX, OFFSET_PAINTBOX, OFFSET_PAINTBOX);
end;

function TGripForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
  Save;
end;

procedure TGripForm.Save;
begin
  if FStream <> nil then
    WriteData(FStream);
end;

procedure TGripForm.SaveAnchors;
var
  I: Integer;
begin
  SetLength(FAnchors, ControlCount);
  for I := 0 to ControlCount - 1 do
  begin
    FAnchors[I].Control := Controls[I];
    with FAnchors[I] do
    begin
      Anchors := Control.Anchors;
      if Control <> FGripBox then
        Control.Anchors := [akTop, akLeft];
    end;
  end;
end;

procedure TGripForm.WriteData(Stream: TStream);
begin
  SetStreamWorker(Stream);
  Stream.Seek(0, soFromBeginning);
  WriteInteger(Left);
  WriteInteger(Top);
  WriteInteger(Width);
  WriteInteger(Height);
end;

procedure TGripForm.SetStream(Value: TStream);
begin
  FStream := Value;
  Load;
end;

procedure TGripForm.WMCaptureChanged(var Message: TMessage);
begin
  FHitTest := 0;
  inherited;
end;

procedure TGripForm.WMMouseMove(var Message: TWMMouseMove);
var
  Point: TPoint;
begin
  if FSizeable and (FHitTest > 0) then
  begin
    Point.X := Message.XPos;
    Point.Y := Message.YPos;
    Windows.ClientToScreen(Handle, Point);
    case FHitTest of
      HTLEFT:
        if Point.X < FHitBounds.Left then
          SetWindowPos(Handle, 0, Point.X, Top, FHitBounds.Right - Point.X,
            Height, SWP_NOZORDER);
      HTRIGHT:
        SetWindowPos(Handle, 0, 0, 0, Message.XPos + GetSystemMetrics(SM_CXFRAME),
          Height, SWP_NOMOVE or SWP_NOZORDER);
      HTTOP:
        if Point.Y < FHitBounds.Top then
          SetWindowPos(Handle, 0, Left, Point.Y, Width, FHitBounds.Bottom -
            Point.Y, SWP_NOZORDER);
      HTTOPLEFT:
      begin
        if Point.X > FHitBounds.Left then
          Point.X := FHitBounds.Left;
        if Point.Y > FHitBounds.Top then
          Point.Y := FHitBounds.Top;
        SetWindowPos(Handle, 0, Point.X, Point.Y, FHitBounds.Right - Point.X,
          FHitBounds.Bottom - Point.Y, SWP_NOZORDER);
      end;
      HTTOPRIGHT:
      begin
        if Point.Y > FHitBounds.Top then
          Point.Y := FHitBounds.Top;
        SetWindowPos(Handle, 0, Left, Point.Y, Message.XPos -
          GetSystemMetrics(SM_CXFRAME), FHitBounds.Bottom - Point.Y,
          SWP_NOZORDER);
      end;
      HTBOTTOM:
       SetWindowPos(Handle, 0, 0, 0, Width, Point.Y - FHitBounds.Top,
         SWP_NOZORDER or SWP_NOMOVE);
      HTBOTTOMLEFT:
      begin
        if Point.X > FHitBounds.Left then
          Point.X := FHitBounds.Left;
        SetWindowPos(Handle, 0, Point.X, Top, FHitBounds.Right - Point.X,
          Point.Y - FHitBounds.Top, SWP_NOZORDER);
      end;
      HTBOTTOMRIGHT:
        SetWindowPos(Handle, 0, 0, 0, Message.XPos + GetSystemMetrics(SM_CXFRAME) *
          2, Point.Y - FHitBounds.Top + GetSystemMetrics(SM_CYFRAME),
          SWP_NOZORDER or SWP_NOMOVE);
    end;
  end;
  inherited;
end;

procedure TGripForm.WMNCHitText(var Message: TWMNCHitTest);
var
  Point: TPoint;
begin
  Point.X := Message.XPos;
  Point.Y := Message.YPos;
  Windows.ScreenToClient(Handle, Point);
  if FSizeable and (Point.X > ClientWidth - OFFSET_PAINTBOX) and
    (Point.Y > ClientHeight - OFFSET_PAINTBOX) then
    Message.Result := HTBOTTOMRIGHT
  else inherited;
end;

procedure TGripForm.WMNCLButtonDown(var Message: TMessage);
const
  HitTests: set of HTLEFT..HTBOTTOMRIGHT =
    [HTLEFT, HTRIGHT, HTTOP, HTTOPLEFT, HTTOPRIGHT, HTBOTTOM, HTBOTTOMLEFT,
    HTBOTTOMRIGHT];
begin
  if FSizeable then
  begin
    with Message do
      FHitTest := Perform(WM_NCHITTEST, WParam, LParam);
    if FHitTest in HitTests then
    begin
      FHitBounds := BoundsRect;
      SetCapture(Handle);
    end
    else
      FHitTest := 0;
  end;
  inherited;
end;

procedure TGripForm.GripPaintBoxPaint(Sender: TObject);
begin
  if FSizeable then
    with Sender as TPaintBox do
      DrawFrameControl(Canvas.Handle, ClientRect, DFC_SCROLL,
        DFCS_SCROLLSIZEGRIP);
end;

{ TOrderedForm }

var
  Activating: Boolean;
  Hidden: Boolean;
  WindowList: TList;

constructor TOrderedForm.Create(AOwner: TComponent);
begin
  if WindowList = nil then
    WindowList := TList.Create;
  WindowList.Add(Self);
  inherited Create(AOwner);
end;

destructor TOrderedForm.Destroy;
begin
  WindowList.Remove(Self);
  if WindowList.Count = 0 then
  begin
    WindowList.Free;
    WindowList := nil;
  end;
  inherited Destroy;
end;

procedure TOrderedForm.Activate;
begin
  Hidden := False;
  if not Activating then
     with WindowList do
       Move(IndexOf(Self), 0);
end;

procedure HideOrderedWindows;
var
  OrderedForm: TOrderedForm;
  I: Integer;
begin
  if Hidden then Exit;
  Hidden := True;
  if WindowList = nil then Exit;
  for I := 0 to WindowList.Count - 1 do
  begin
    OrderedForm := TOrderedForm(WindowList[I]);
    OrderedForm.FPriorVisible := OrderedForm.Visible;
    OrderedForm.Hide;
  end;
end;

procedure ShowOrderedWindows;
var
  OrderedForm: TOrderedForm;
  I: Integer;
begin
  if not Hidden then Exit;
  Hidden := False;
  if WindowList = nil then Exit;
  Activating := True;
  try
    for I := WindowList.Count - 1 downto 0 do
      begin
        OrderedForm := TOrderedForm(WindowList[I]);
        if OrderedForm.FPriorVisible then
        begin
          OrderedForm.Show;
          OrderedForm.BringToFront;
        end;
      end;
  finally
    Activating := False;
  end;
end;

function GetOrderedWindowCount: Integer;
begin
  Result := 0;
  if WindowList <> nil then
    Result := WindowList.Count;
end;

function GetOrderedWindow(Index: Integer): TOrderedForm;
begin
  if WindowList <> nil then
    Result := TOrderedForm(WindowList[Index])
  else
    raise Exception.Create('Invalid ordered index');
end;

end.
