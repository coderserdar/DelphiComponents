unit CHMouse;

{ ##############################################################################
  TCHMouse

  Version   		:   1.1.2
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.1.0 - 17.10.2002    - NEW: WheelScrollLines, Trails (Win 9x), Double-Click-Speed
  1.1.1 - 15.12.2002    - BUG: repair some memory leaks
  1.1.2 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, Forms, SysUtils, Classes, Controls;


type
  TMousePosition = (mpCenter, mpCenterBottom, mpCenterTop, mpCenterLeft, mpCenterRight,
    mpTopLeft, mpTopRight, mpBottomLeft, mpBottomRight);
  TMouseAreaMode = (maControl, maCustom);
  TMousePosMode = (mpControl, mpCustom);
  TMouseSpeed = (msSystem, msSlow, msNormal, msFast);

  TCHMouse = class;

  TCHMousePositioner = class(TPersistent)
  private
    FOwner : TCHMouse;
    FControl: TControl;
    FMousePosition : TMousePosition;
    FMoveSpeed : Word;
    FPosX : Integer;
    FPosY : Integer;
    FPosMode : TMousePosMode;

    procedure SetMoveSpeed(const Value: Word);
    procedure SetMousePosition(const Value: TMousePosition);
    procedure SetPosMode(const Value: TMousePosMode);
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);
  public
    constructor Create(AOwner: TCHMouse); virtual;
  published
    property Control : TControl read FControl write FControl;
    property Mode : TMousePosMode read FPosMode Write SetPosMode;
    property MoveSpeed : Word read FMoveSpeed Write SetMoveSpeed;
    property Position : TMousePosition read FMousePosition Write SetMousePosition;
    property PosX : Integer read FPosX Write SetPosX;
    property PosY : Integer read FPosY Write SetPosY;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHMouseArea= class(TPersistent)
  private
    FOwner : TCHMouse;
    FControl: TControl;
    FMouseAreaMode : TMouseAreaMode;
    FRectArea : TRect;
    FAreaLeft : Word;
    FAreaRight : Word;
    FAreaTop : Word;
    FAreaBottom : Word;

    procedure SetAreaBottom(const Value: Word);
    procedure SetAreaLeft(const Value: Word);
    procedure SetAreaRight(const Value: Word);
    procedure SetAreaTop(const Value: Word);
    procedure setMouseAreaMode(const Value: TMouseAreaMode);
  public
    constructor Create(AOwner: TCHMouse); virtual;
  published
    property AreaLeft : Word read FAreaLeft Write SetAreaLeft;
    property AreaRight : Word read FAreaRight Write SetAreaRight;
    property AreaTop : Word read FAreaTop Write SetAreaTop;
    property AreaBottom : Word read FAreaBottom Write SetAreaBottom;
    property Control : TControl read FControl write FControl;
    property Mode : TMouseAreaMode read FMouseAreaMode Write setMouseAreaMode;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHMouse = class(TComponent)
  private
    FForm : TForm;
    FMousePositioner: TCHMousePositioner;
    FMouseArea : TCHMouseArea;
    FCurPos : TPoint;
    FMouseSpeed : TMouseSpeed;
    FShowMouse : Boolean;
    FRestoreOnExit : Boolean;
    FDefault : Integer;
    FChangeMouseButton : Boolean;
    FWheelScrollLines: Word;
    FTrails: Word;
    FDoubleClickSpeed: Word;

    nDoubleClickSpeed : Word;
    nTrails : Word;
    nWheelScrollLines : Word;
    nSpeedSave : array[0..2] of Integer;

    procedure SetMouseSpeed(const Value: TMouseSpeed);
    procedure SetShowMouse(const Value: Boolean);
    procedure SetChangeMouseButton(const Value: Boolean);
    procedure SetRestoreOnExit(const Value: Boolean);
    procedure SetWheelScrollLines(const Value: Word);
    procedure SetTrails(const Value: Word);
    procedure SetDoubleClickSpeed(const Value: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoMousePositioner;
    procedure DoMouseArea(Active : Boolean);
    procedure DoMouseLeftClick;
    procedure DoMouseRightClick;
    procedure DoMouseMiddleClick;
    function GetMousePosX : Integer;
    function GetMousePosY : Integer;
  published
    property Area : TCHMouseArea read FMouseArea Write FMouseArea;
    property DoubleClickSpeed : Word read FDoubleClickSpeed Write SetDoubleClickSpeed;
    property Positioner : TCHMousePositioner read FMousePositioner Write FMousePositioner;
    property RestoreOnExit : Boolean read FRestoreOnExit Write SetRestoreOnExit;
    property ShowMouse : Boolean read FShowMouse Write SetShowMouse;
    property Speed : TMouseSpeed read FMousespeed Write SetMouseSpeed;
    property SwapMouseButton : Boolean read FChangeMouseButton Write SetChangeMouseButton;
    property WheelScrollLines : Word read FWheelScrollLines Write SetWheelScrollLines;
    property Trails : Word read FTrails Write SetTrails;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHMouse]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHMouse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := TForm(GetParentForm(TControl(AOwner)));
  FMousePositioner := TCHMousePositioner.Create(self);
  FMouseArea := TCHMouseArea.Create(self);

  // save for restore
  SystemParametersInfo(SPI_GETMOUSE, 0, @nSpeedSave, 0);
  SystemParametersInfo(SPI_GETMOUSETRAILS, 0, @nTrails, SPIF_SENDCHANGE);
  SystemParametersInfo(SPI_GETWHEELSCROLLLINES,0 , @nWheelScrollLines, 0);
  if nWheelScrollLines = 0 then
    nWheelScrollLines := 3;
  nDoubleClickSpeed := GetDoubleClickTime();
  if nDoubleClickSpeed = 0 then
    nDoubleClickSpeed := 500;

  FRestoreOnExit := True;
  FShowMouse := True;
  FMouseSpeed := msSystem;
  FChangeMouseButton := False;
  FDoubleClickSpeed := nDoubleClickSpeed;
  FWheelScrollLines := nWheelScrollLines;
  FTrails := nTrails;
  FDefault := Screen.Cursor;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHMouse.Destroy;
begin
  if FRestoreOnExit then
  begin
    Screen.Cursor := FDefault;
    SystemParametersInfo(SPI_SETMOUSE, 1, @nSpeedSave, 0);
    SystemParametersInfo(SPI_SETMOUSETRAILS, 0, @nTrails, SPIF_SENDCHANGE);
    SystemParametersInfo(SPI_SETWHEELSCROLLLINES,0, @nWheelScrollLines, 0);
    SetDoubleClickTime(nDoubleClickSpeed);
  end;
  FForm := nil;
  FMousePositioner.Free;
  FMouseArea.Free;
  ClipCursor(nil);
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.DoMouseArea(Active: Boolean);
var
  nClientLeft, nClientTop : Integer;
begin
  if Active then
  begin
    with FMouseArea do
    begin
      if FMouseAreaMode = maControl then
      begin
        if Assigned(FControl) then
        begin
          FRectArea.TopLeft := FControl.ClientOrigin;
          FRectArea.Right := FRectArea.Left + FControl.Width;
          FRectArea.Bottom := FRectArea.Top + FControl.Height;
        end
      end
      else if FMouseAreaMode = maCustom then
      begin
        nClientLeft := FForm.ClientOrigin.X;
        nClientTop := FForm.ClientOrigin.Y;

        FRectArea.Left := nClientLeft + FAreaLeft;
        FRectArea.Right := nClientLeft + FAreaRight;
        FRectArea.Top := nClientTop + FAreaTop;
        FRectArea.Bottom := nClientTop + FAreaBottom;
      end;
    end;

    GetCursorPos(FCurPos);
    if not PtInRect(FMouseArea.FRectArea, FCurPos) then
      SetCursorPos(FMouseArea.FRectArea.Left, FMouseArea.FRectArea.Top);

    ClipCursor(@FMouseArea.FRectArea);
  end
  else
    ClipCursor(nil);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.DoMouseLeftClick;
begin
  mouse_event(MOUSEEVENTF_LEFTDOWN,0,0,0,0);
  mouse_event(MOUSEEVENTF_LEFTUP,0,0,0,0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.DoMouseRightClick;
begin
  mouse_event(MOUSEEVENTF_RIGHTDOWN,0,0,0,0);
  mouse_event(MOUSEEVENTF_RIGHTUP,0,0,0,0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.DoMouseMiddleClick;
begin
  mouse_event(MOUSEEVENTF_MIDDLEDOWN,0,0,0,0);
  mouse_event(MOUSEEVENTF_MIDDLEUP,0,0,0,0);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.DoMousePositioner;
var
  DestPos : TPoint;
begin
  with FMousePositioner do
  begin
    // move mouse to control
    if FPosMode = mpControl then
    begin
      if Assigned(FControl) then
      begin
        GetCursorPos(FCurPos);

        if FMousePosition = mpCenter then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth  div 2;
          DestPos.Y := FControl.ClientRect.Top + FControl.ClientHeight div 2;
        end;
        if FMousePosition = mpCenterBottom then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth  div 2;
          DestPos.Y := FControl.ClientRect.Bottom;
        end;
        if FMousePosition = mpCenterTop then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth  div 2;
          DestPos.Y := FControl.ClientRect.Top;
        end;
        if FMousePosition = mpCenterLeft then
        begin
          DestPos.X := FControl.ClientRect.Left;
          DestPos.Y := FControl.ClientRect.Top + FControl.ClientHeight div 2;
        end;
        if FMousePosition = mpCenterRight then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth;
          DestPos.Y := FControl.ClientRect.Top + FControl.ClientHeight div 2;
        end;
        if FMousePosition = mpBottomLeft then
        begin
          DestPos.X := FControl.ClientRect.Left;
          DestPos.Y := FControl.ClientRect.Bottom;
        end;
        if FMousePosition = mpBottomRight then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth;
          DestPos.Y := FControl.ClientRect.Bottom;
        end;
        if FMousePosition = mpTopLeft then
        begin
          DestPos.X := FControl.ClientRect.Left;
          DestPos.Y := FControl.ClientRect.Top;
        end;
        if FMousePosition = mpTopRight then
        begin
          DestPos.X := FControl.ClientRect.Left + FControl.ClientWidth;
          DestPos.Y := FControl.ClientRect.Top;
        end;

        DestPos := FControl.ClientToScreen(DestPos);

        if FMoveSpeed > 0 then
        begin
          Repeat
            Sleep(FMoveSpeed);
            Application.Processmessages;
            GetCursorPos(FCurPos);

            // PosX
            if FCurPos.X > DestPos.X  then
              Dec(FCurPos.X)
            else if FCurPos.X < DestPos.X  then
              Inc(FCurPos.X);
            // PosY
            if FCurPos.Y > DestPos.Y then
              Dec(FCurPos.Y)
            else if FCurPos.Y < DestPos.Y then
              Inc(FCurPos.Y);

            SetCursorPos(FCurPos.X, FCurPos.Y);
          until (GetAsyncKeystate(Vk_Return) <> 0) or
                (GetAsyncKeystate(Vk_Escape) <> 0) or
                (GetAsyncKeystate(Vk_Space ) <> 0) or
                ((FCurPos.X = DestPos.X ) and (FCurPos.Y = DestPos.Y));
          SetCursorPos(DestPos.X , DestPos.Y);
        end
        else
        begin
          SetCursorPos(DestPos.X, DestPos.Y);
        end;
      end;
    end
    // move mouse to customize PosX and PosY
    else if FPosMode = mpCustom then
    begin
      if FMousePositioner.FMoveSpeed > 0 then
      begin
        Repeat
          Sleep(FMoveSpeed);
          Application.Processmessages;
          GetCursorPos(FCurPos);

          // PosX
          if FCurPos.X > FPosX  then
            Dec(FCurPos.X)
          else if FCurPos.X < FPosX  then
            Inc(FCurPos.X);
          // PosY
          if FCurPos.Y > FPosY then
            Dec(FCurPos.Y)
          else if FCurPos.Y < FPosY then
            Inc(FCurPos.Y);

          SetCursorPos(FCurPos.X, FCurPos.Y);
        until (GetAsyncKeystate(Vk_Return) <> 0) or
              (GetAsyncKeystate(Vk_Escape) <> 0) or
              (GetAsyncKeystate(Vk_Space ) <> 0) or
              ((FCurPos.X = FPosX ) and (FCurPos.Y = FPosY));
        SetCursorPos(FPosX , FPosY);
      end
      else
      begin
        SetCursorPos(FPosX, FPosY);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHMouse.GetMousePosX: Integer;
begin
  GetCursorPos(FCurPos);
  Result := FCurPos.X;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHMouse.GetMousePosY: Integer;
begin
  GetCursorPos(FCurPos);
  Result := FCurPos.Y;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetMouseSpeed(const Value: TMouseSpeed);
const
  mFast : array[0..2] of Integer = (0,0,2);
  mNormal : array[0..2] of Integer = (0,0,1);
  mSlow : array[0..2] of Integer = (0,0,0);
begin
  if FMousespeed <> Value then
  begin
    FMousespeed := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FMouseSpeed <> msSystem then
        SystemParametersInfo(spi_setmouse, 1, @nSpeedSave, 0)
      else if FMouseSpeed = msSlow then
        SystemParametersInfo(SPI_SETMOUSE, 0, @mSlow, 0)
      else if FMouseSpeed = msNormal then
        SystemParametersInfo(SPI_SETMOUSE, 0, @mSlow, 0)
      else
        SystemParametersInfo(SPI_SETMOUSE, 0, @mFast, 0);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetShowMouse(const Value: Boolean);
begin
  if FShowMouse <> Value then
  begin
    FShowMouse := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FShowMouse = False then
        Screen.Cursor := -1
      else
        Screen.Cursor := FDefault;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetChangeMouseButton(const Value: Boolean);
begin
  if FChangeMouseButton <> Value then
  begin
    FChangeMouseButton := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FChangeMouseButton = True then
        SystemParametersInfo(SPI_SETMOUSEBUTTONSWAP,1, Nil, 0)
      else
        SystemParametersInfo(SPI_SETMOUSEBUTTONSWAP,0, Nil, 0)
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetWheelScrollLines(const Value: Word);
begin
  if FWheelScrollLines <> Value then
  begin
    FWheelScrollLines := Value;
    if not (csDesigning in ComponentState) then
    begin
      SystemParametersInfo(SPI_SETWHEELSCROLLLINES,Value, Nil, 0)
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetTrails(const Value: Word);
begin
  if FTrails <> Value then
  begin
    FTrails := Value;
    if not (csDesigning in ComponentState) then
    begin
      SystemParametersInfo(SPI_SETMOUSETRAILS, Value, nil, SPIF_SENDCHANGE)
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetDoubleClickSpeed(const Value: Word);
begin
  if FDoubleClickSpeed <> Value then
  begin
    FDoubleClickSpeed := Value;
    if not (csDesigning in ComponentState) then
    begin
      SetDoubleClickTime(Value);
    end;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouse.SetRestoreOnExit(const Value: Boolean);
begin
  if FRestoreOnExit <> Value then
    FRestoreOnExit := Value;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHMousePositioner }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHMousePositioner.Create(AOwner: TCHMouse);
begin
  inherited Create;
  FOwner := AOwner;

  FPosMode := mpControl;
  FMoveSpeed := 0;
  FMousePosition := mpCenter;
  FPosX := 0;
  FPosY := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMousePositioner.SetMousePosition(const Value: TMousePosition);
begin
  if FMousePosition <> Value then
    FMousePosition := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMousePositioner.SetMoveSpeed(const Value: Word);
begin
  if FMoveSpeed <> Value then
    FMoveSpeed := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMousePositioner.SetPosMode(const Value: TMousePosMode);
begin
  if FPosMode <> Value then
    FPosMode := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMousePositioner.SetPosX(const Value: Integer);
begin
  if FPosX <> Value then
    FPosX := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMousePositioner.SetPosY(const Value: Integer);
begin
  if FPosY <> Value then
    FPosY := Value;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHMouseArea }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHMouseArea.Create(AOwner: TCHMouse);
begin
  inherited Create;
  FOwner := AOwner;

  FMouseAreaMode := maControl;
  FAreaLeft := 0;
  FAreaRight := 0;
  FAreaTop := 0;
  FAreaBottom := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouseArea.SetAreaBottom(const Value: Word);
begin
  if FAreaBottom <> Value then
    FAreaBottom := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouseArea.SetAreaLeft(const Value: Word);
begin
  if FAreaLeft <> Value then
    FAreaLeft := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouseArea.SetAreaRight(const Value: Word);
begin
  if FAreaRight <> Value then
    FAreaRight := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouseArea.SetAreaTop(const Value: Word);
begin
  if FAreaTop <> Value then
    FAreaTop := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMouseArea.setMouseAreaMode(const Value: TMouseAreaMode);
begin
  if FMouseAreaMode <> Value then
    FMouseAreaMode := Value;
end;

end.
