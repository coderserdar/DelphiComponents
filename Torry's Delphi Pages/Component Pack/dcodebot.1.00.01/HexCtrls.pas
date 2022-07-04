
(********************************************************)
(*                                                      *)
(*      DataCOM Class Library                           *)
(*                                                      *)
(*      Copyright (c) 1999 DataCOM                      *)
(*                                                      *)
(********************************************************)

unit HexCtrl;

{$I STD.INC}

interface

uses
  Classes, Controls, Forms, Graphics, Math, Messages, Windows, WinTools;

type
  THexEditor = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FCount: Integer;
    FMap: TMemoryMappedFile;
    FRange: Integer;
    FScrolling: Boolean;
    FTopRow: Integer;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMCaptureChanged(var Msg: TWMNoParams); message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    function GetFileName: string;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetFileName(const Value: string);
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure UpdateScrollBar;
    property Scrolling: Boolean read FScrolling write FScrolling;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName: string read GetFileName write SetFileName;
  published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Ctl3D;
    property TabStop default True;
  end;

implementation

const
  STD_ITEMHEIGHT = 18;
  STD_ITEMWIDTH = 16;
  STD_SCROLLPIXELS = 20;

constructor THexEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FCount := 0;
  FTopRow := -1;
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  TabStop := True;
  Canvas.Font.Name := 'Courier New';
end;

destructor THexEditor.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure THexEditor.CreateHandle;
begin
  inherited CreateHandle;
  UpdateScrollBar;
end;

procedure THexEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_VSCROLL;
    WindowClass.style := CS_DBLCLKS;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure THexEditor.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure THexEditor.WMCaptureChanged(var Msg: TWMNoParams);
begin
  FScrolling := False;
  inherited;
end;

procedure THexEditor.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure THexEditor.WMTimer(var Msg: TWMTimer);
var
  Point: TPoint;
  ScrollRepeat: Integer;
  ScrollDir: Integer;
  j: Integer;
begin
  if FScrolling then
  begin
    GetCursorPos(Point);
    Windows.ScreenToClient(Handle, Point);
    ScrollDir := -1;
    with Point do
    if Y < 0 then
    begin
      ScrollRepeat := -Y div STD_SCROLLPIXELS;
      ScrollDir := SB_LINEUP;
    end
    else if Y > ClientHeight then
    begin
      ScrollRepeat := (Y - ClientHeight) div STD_SCROLLPIXELS;
      ScrollDir := SB_LINEDOWN;
    end;
    if ScrollDir <> -1 then
    for j := 0 to ScrollRepeat do
      SendMessage(Handle, WM_VSCROLL, ScrollDir, 0)
    else
    begin
      FScrolling := False;
      KillTimer(Handle, Msg.TimerID);
    end;
  end
  else
    KillTimer(Handle, Msg.TimerID);
end;

procedure THexEditor.WMVScroll(var Msg: TWMVScroll);
var
  ScrollInfo: TScrollInfo;
  Rect: TRect;
begin
  if Assigned(FMap) then
  with ScrollInfo do
  begin
    case Msg.ScrollCode of
      SB_BOTTOM: nPos := FCount - 1;
      SB_ENDSCROLL: nPos := -1;
      SB_LINEDOWN: nPos := Min(FTopRow + 1, FCount - 1);
      SB_LINEUP: nPos := Max(FTopRow - 1, 0);
      SB_PAGEDOWN: nPos := Min(FTopRow + ClientHeight div STD_ITEMHEIGHT,
        FCount - 1);
      SB_PAGEUP: nPos := Max(FTopRow - ClientHeight div STD_ITEMHEIGHT, 0);
      SB_THUMBPOSITION, SB_THUMBTRACK: nPos := Msg.Pos;
      SB_TOP: nPos := 0;
    end;
    if nPos > -1 then
    begin
      Windows.GetClientRect(Handle, Rect);
      ScrollWindowEx(Handle, 0, (FTopRow - nPos) * STD_ITEMHEIGHT, nil, @Rect,
        0, nil, SW_INVALIDATE);
      FTopRow := nPos;
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_POS;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    end;
  end;
  Msg.Result := 0;
end;

procedure THexEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_HOME:
      if ssCtrl in Shift then
        SendMessage(Handle, WM_VSCROLL, SB_TOP, 0);
    VK_END:
      if ssCtrl in Shift then
        SendMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);
    VK_PRIOR: SendMessage(Handle, WM_VSCROLL, SB_PAGEUP, 0);
    VK_NEXT: SendMessage(Handle, WM_VSCROLL, SB_PAGEDOWN, 0);
    VK_UP: SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
    VK_DOWN: SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
  end;
end;

procedure THexEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) then
    SetFocus;
end;

procedure THexEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not (Dragging or Scrolling) then
    if (Y < 0) or (Y > ClientHeight) then
    begin
      Scrolling := True;
      SetTimer(Handle, 0, 60, nil);
    end;
end;

procedure THexEditor.Paint;
const
  DT_NORMAL = DT_VCENTER or DT_LEFT or DT_SINGLELINE;
var
  Rect: TRect;
  P: PChar;
  j: Integer;
begin
  if Assigned(FMap) then
  begin
    Windows.GetClientRect(Handle, Rect);
    with Rect do
    for j := FTopRow to FTopRow + ClientHeight div STD_ITEMHEIGHT do
    begin
      Bottom := Top + STD_ITEMHEIGHT;
      Canvas.FillRect(Rect);
      if j < FCount then
      begin
        Inc(Left, 2);
        P := FMap.ViewStart;
        Inc(P, STD_ITEMWIDTH * j);
          DrawText(Canvas.Handle, P, Min(STD_ITEMWIDTH, FMap.ViewEnd - P), Rect, DT_NORMAL);
        Dec(Left, 2);
      end;
      Inc(Top, STD_ITEMHEIGHT);
    end;
  end;
end;

procedure THexEditor.Resize;
begin
  UpdateScrollBar;
  inherited Resize;
end;

procedure THexEditor.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  if Assigned(FMap) then
  with ScrollInfo do
  begin
    FRange := ClientHeight div STD_ITEMHEIGHT + 1;
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := FCount;
    nPage := FRange;
    nPos := FTopRow;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end
  else
    FTopRow := -1;
  ShowScrollBar(Handle, SB_VERT, Assigned(FMap));
end;

function THexEditor.GetFileName: string;
begin
  if Assigned(FMap) then
    Result := FMap.FileName
  else
    Result := '';
end;

procedure THexEditor.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure THexEditor.SetFileName(const Value: string);
var
  NewMap: TMemoryMappedFile;
begin
  FCount := 0;
  FTopRow := -1;
  NewMap := nil;
  try
    FMap.Free;
    FMap := nil;
    if Value <> '' then
      NewMap := TMemoryMappedFile.Create(Value);
    FTopRow := 0;
    with NewMap do
      FCount := (ViewEnd - ViewStart) div STD_ITEMWIDTH +
        Byte((ViewEnd - ViewStart) mod 10 > 0);
  finally
    FMap := NewMap;
    UpdateScrollBar;
    Invalidate;
  end;
end;

end.
