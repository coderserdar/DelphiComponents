unit CHPanel;

{ ##############################################################################
  TCHPanel

  Version   		:   1.3.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 20.08.2002    - BUG: Transparents funktionierte nicht
  1.1.0 - 29.08.2002    - NEW: OnMouseEnter, OnMouseLeave
                        - NEW: Layout überarbeitet, Resize, Collapse
  1.2.0 - 16.11.2002    - NEW: MouseWheel, MouseWheelDown, MouseWheelUp
  1.2.1 - 15.12.2002    - BUG: repair some memory leaks
  1.2.2 - 28.02.2003    - BUG: move and resize can now use at same time
  1.2.3 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.3.0 - 03.01.2005    - NEW: faster Create() by outsourceing TCHPanelLayout
                        - NEW: Autosizer can resize all child controls when changing the size of CHPanel

  ############################################################################ }

interface

uses
  Windows, Forms, Messages, Classes, Controls, ExtCtrls, Graphics, Types,
  _CHClassProperty, _CHClassFunction, CHButton;

type
  TMoveSpeed = 0..10000;
  TMoveMode = (mmAll, mmControl, mmNone);
  TCollapseDirection = (cdUp, cdDown, cdLeft, cdRight);
  
  TPlacement = record
    Left, Top, Width, Height: Integer;
  end;

  PIntArray = ^TRectArray;
  TRectArray = array[0..4096] of TPlacement;

  TCHPanel = class;

  TCHPanelLayout = class(TPersistent)
  private
    FOwner : TCHPanel;
    FShowControl: Boolean;

    FControlText: TCaption;
    FControlColor: TColor;
    FControlStyle: TPanelBevel;
    FShowMoveDown: Boolean;
    FShowMoveRight: Boolean;
    FShowMoveUp: Boolean;
    FShowMoveLeft: Boolean;
    FSpace : Integer;
    FAllButtonWidth : Integer;
    FControlY: Word;
    FControlX: Word;
    FFont: TFont;
    FControlTextAlignment: TAlignment;

    procedure SetControlColor(const Value: TColor);
    procedure SetControlText(const Value: TCaption);
    procedure SetShowControl(const Value: Boolean);
    procedure SetShowMoveUp(const Value: Boolean);
    procedure SetShowMoveDown(const Value: Boolean);
    procedure SetShowMoveLeft(const Value: Boolean);
    procedure SetShowMoveRight(const Value: Boolean);
    procedure SetSpace(const Value: integer);
    procedure SetControlX(const Value: Word);
    procedure SetControlY(const Value: Word);
    procedure SetControlStyle(const Value: TPanelBevel);
    procedure SetFont(const Value: TFont);
    procedure SetControlTextAlignment(const Value: TAlignment);
    procedure SetMoveButton;
    function GetFont: TFont;
    function GetControlText: TCaption;
  protected

  public
    constructor Create(AOwner : TCHPanel); virtual;
    destructor Destroy; override;
  published
    property ControlColor : TColor read FControlColor Write SetControlColor;
    property ControlFont : TFont read GetFont write SetFont;
    property ControlText : TCaption read GetControlText Write SetControlText;
    property ControlTextAlignment : TAlignment read FControlTextAlignment Write SetControlTextAlignment;
    property ControlStyle : TPanelBevel read FControlStyle Write SetControlStyle;
    property ControlX : Word read FControlX Write SetControlX;
    property ControlY : Word read FControlY Write SetControlY;
    property ShowControl : Boolean read FShowControl Write SetShowControl;
    property ShowMoveUp : Boolean read FShowMoveUp Write SetShowMoveUp;
    property ShowMoveDown : Boolean read FShowMoveDown Write SetShowMoveDown;
    property ShowMoveLeft : Boolean read FShowMoveLeft Write SetShowMoveLeft;
    property ShowMoveRight : Boolean read FShowMoveRight Write SetShowMoveRight;
    property Space : Integer read FSpace Write SetSpace;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHPanelMover = class(TPersistent)
  private
    FOwner : TCHPanel;
    FCanCollapse: Boolean;
    FCanResize: Boolean;
    FAutosizer : Boolean;
    FMoveSpeed: TMoveSpeed;
    FMinHeight : Integer;
    FMaxHeight : Integer;
    FMinWidth : Integer;
    FMaxWidth : Integer;
    FMoveStep : Integer;
    FMoveMode : TMoveMode;
    FMoveTimer : TTimer;
    FMoveDirection : TCollapseDirection;

    procedure SetCanCollapse(const Value: Boolean);
    procedure SetCanResize(const Value: Boolean);
    procedure SetMoveSpeed(const Value: TMoveSpeed);
    procedure SetAutosizer(const Value: Boolean);
    procedure SetMoveMode(const Value: TMoveMode);
    procedure DoAutosizer;
    procedure DoCollapse(Sender: TObject);
  protected

  public
    constructor Create(AOwner : TCHPanel); virtual;
    destructor Destroy; override;
  published
    property Autosizer : Boolean read FAutosizer Write SetAutosizer;
    property CanCollapse : Boolean read FCanCollapse Write SetCanCollapse;
    property CanResize : Boolean read FCanResize Write SetCanResize;
    property CollapseSpeed : TMoveSpeed read FMoveSpeed Write SetMoveSpeed;
    property CollapseStep : Integer read FMoveStep Write FMoveStep;
    property MinHeight : Integer read FMinHeight Write FMinHeight;
    property MaxHeight : Integer read FMaxHeight Write FMaxHeight;
    property MinWidth : Integer read FMinWidth Write FMinWidth;
    property MaxWidth : Integer read FMaxWidth Write FMaxWidth;
    property MoveMode : TMoveMode read FMoveMode Write SetMoveMode;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHPanelSizer = class(TPersistent)
  private
    FOwner : TCHPanel;
    FAutoChildPosTop: Boolean;
    FAutoChildPosLeft: Boolean;
    FAutoChildHeight: Boolean;
    FAutoChildWidth: Boolean;
    pWidth :Integer;
    pHeight:Integer;
    PCtrlsCoordArr:PIntArray;
  protected

  public
    constructor Create(AOwner : TCHPanel); virtual;
    destructor Destroy; override;
  published
    property AutoChildPosLeft : Boolean read FAutoChildPosLeft write FAutoChildPosLeft default False;
    property AutoChildPosTop : Boolean read FAutoChildPosTop write FAutoChildPosTop default False;
    property AutoChildWidth : Boolean  read FAutoChildWidth write FAutoChildWidth default False;
    property AutoChildHeight : Boolean read FAutoChildHeight write FAutoChildHeight default False;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHPanel = class(TPanel)
  private
    FOnMouseDown : TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FWheelAccumulator: Integer;

    FGradient : TCHGradient;
    FPanelMover : TCHPanelMover;
    FPanelLayout: TCHPanelLayout;
    FPanelSizer: TCHPanelSizer;
    FFill : TCHFill;
    FBitmap : TCHBitmap;

    FControlPanel : TPanel;
    FClientRect : TRect;
    FPanelRect : TRect;

    FMoverUp : TCHButton;
    FMoverDown : TCHButton;
    FMoverLeft : TCHButton;
    FMoverRight : TCHButton;
    FBuffered: Boolean;
    FBackgroundBmp : TBitmap;
    FResize: Boolean;
    FResizeStartPos: TPoint;


    procedure GetRectSize;
    procedure MakeForeground;

    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message : TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure MoverUpClick(Sender: TObject);
    procedure MoverDownClick(Sender: TObject);
    procedure MoverLeftClick(Sender: TObject);
    procedure MoverRightClick(Sender: TObject);
    procedure ControlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateChanges(Sender: TObject);
    procedure SetGradient(const Value: TCHGradient);
    procedure SetBuffered(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure Collapse(Direction : TCollapseDirection);
  published
    property OnMouseDown: TNotifyEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;

    property Bitmap : TCHBitmap read FBitmap Write FBitmap;
    property Buffered : Boolean read FBuffered write SetBuffered;
    property Fill : TCHFill read FFill write FFill;
    property Gradient : TCHGradient read FGradient write SetGradient;
    property Layout : TCHPanelLayout read FPanelLayout Write FPanelLayout;
    property Mover : TCHPanelMover read FPanelMover Write FPanelMover;
    property AutoSizer : TCHPanelSizer read FPanelSizer Write FPanelSizer;
  end;

procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHPanel]);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradient := TCHGradient.Create;
  FGradient.OnChange := UpdateChanges;
  FFill := TCHFill.Create;
  FFill.OnChange := UpdateChanges;
  FBitmap := TCHBitmap.Create;
  FBitmap.OnChange := UpdateChanges;

  Width := 200;
  Height := 130;
  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  FBackgroundBmp := TBitmap.Create;
  FPanelLayout := TCHPanelLayout.Create(Self);
  FPanelMover := TCHPanelMover.Create(Self);
  FPanelMover.MoveMode := mmNone;
  FPanelSizer := TCHPanelSizer.Create(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHPanel.Destroy;
begin
  FGradient.Free;
  FBitmap.Free;
  FFill.Free;
  FPanelMover.Free;
  FPanelSizer.Free;
  FPanelLayout.Free;

  FMoverUp.Free;
  FMoverDown.Free;
  FMoverLeft.Free;
  FMoverRight.Free;
  FControlPanel.Free;

  FBackgroundBmp.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.Resize;
var
  I : Integer;
begin
  inherited;

  // Panelsizer
  if not(csDesigning in ComponentState) then
  begin
    if (FPanelSizer.AutoChildPosLeft = false) and (FPanelSizer.AutoChildWidth = false) and
    (FPanelSizer.AutoChildPosTop = false) and (FPanelSizer.AutoChildHeight = false) then
      Exit;

    for  i := 0 to ControlCount - 1 do
    begin
      if(FPanelSizer.AutoChildPosLeft = true) then
       if (FPanelSizer.AutoChildWidth = true) then
       begin
        Controls[i].Left := MulDiv (FPanelSizer.PCtrlsCoordArr[i].Left,Width,FPanelSizer.pWidth);
        Controls[i].Width :=  MulDiv (FPanelSizer.PCtrlsCoordArr[i].Width,Width,FPanelSizer.pWidth);
       end
       else
         Controls[i].Left := Round(FPanelSizer.PCtrlsCoordArr[i].Left * Width / FPanelSizer.pWidth  + ((FPanelSizer.PCtrlsCoordArr[i].Width) * Width / FPanelSizer.pWidth - (FPanelSizer.PCtrlsCoordArr[i].Width))/2);

      if(FPanelSizer.AutoChildPosTop = true) then
       if (FPanelSizer.AutoChildHeight = true) then
       begin
        Controls[i].Top := MulDiv (FPanelSizer.PCtrlsCoordArr[i].Top,Height,FPanelSizer.pHeight);
        Controls[i].Height := MulDiv (FPanelSizer.PCtrlsCoordArr[i].Height,Height,FPanelSizer.pHeight);

       end
       else
         Controls[i].Top := Round(FPanelSizer.PCtrlsCoordArr[i].Top * Height / FPanelSizer.pHeight + ((FPanelSizer.PCtrlsCoordArr[i].Height)  * Height / FPanelSizer.pHeight - (FPanelSizer.PCtrlsCoordArr[i].Height))/2);
    end;
  end;

  // Panellayout
  if FControlPanel <> nil then
    FControlPanel.Width := Self.Width - (FPanelLayout.ControlX * 2);
  FPanelMover.DoAutosizer;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.Paint;
var
  nX_BackgroundBmp, nY_BackgroundBmp, nX_Resize, nY_Resize : Integer;
  TmpFont : TFont;
begin
  inherited Paint;

  Canvas.Font.Assign(Self.Font);

  // Control Panel
  if FControlPanel <> nil then
  begin
    FControlPanel.Width := Self.Width - (FPanelLayout.ControlX * 2);
    FControlPanel.Font := FPanelLayout.FFont;
  end;
  
  // Rect
  GetRectSize;


  // GRADIENT
  if Fill.Style = fsGradient then
  begin
    if Length(FGradient.FGradientColorArray) > 1 then
    begin
      DrawGradient(Self.Canvas, FClientRect, FGradient.FGradientColorArray,
        FGradient.Style, FGradient.Rotation);
    end
    // only one Color = True --> einfarbig
    else if (Length(FGradient.FGradientColorArray) = 1) then
    begin
      Canvas.Brush.Color := FGradient.FGradientColorArray[0];
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(FClientRect);
    end
    // all Colors = False --> ohne Füllung
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(FClientRect);
    end;
  end;

  // NORMAL
  if (FFill.Style = fsNormal) then
  begin
    //
  end;

  // BITMAP
  if (FFill.Style = fsBitmap) then
  begin
    // check Bitmap
    if not FBitmap.Bitmap.Empty then
    begin
      // set Bitmap
      MakeForeground;

      // draw Bitmap
      with Canvas do
      begin

        if FBitmap.Mode = bmNormal then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          nX_BackgroundBmp := FPanelRect.Left;
          nY_BackgroundBmp := FPanelRect.Top;
          Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBackgroundBmp);
        end
        else if FBitmap.Mode = bmStretch then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          StretchDraw(FPanelRect, FBackgroundBmp);
        end
        else if FBitmap.Mode = bmTile then
        begin
          FBackgroundBmp.Canvas.Draw(0, 0, FBitmap.Bitmap);
          Brush.Bitmap := FBackgroundBmp;
          FillRect(FPanelRect);
        end
        else if FBitmap.Mode = bmCenter then
        begin
          // X
          if (FPanelRect.Right - FPanelRect.Left) > FBitmap.Bitmap.Width then
            nX_BackgroundBmp := (FPanelRect.Right div 2) - (FBitmap.Bitmap.Width div 2)
          else
            nX_BackgroundBmp := -(FBitmap.Bitmap.Width - FPanelRect.Right) div 2;

          // Y
          if (FPanelRect.Bottom - FPanelRect.Top) > FBitmap.Bitmap.Height then
            nY_BackgroundBmp := (FPanelRect.Bottom div 2) - (FBitmap.Bitmap.Height div 2)
          else
            nY_BackgroundBmp := -(FBitmap.Bitmap.Height - FPanelRect.Bottom) div 2;

          FBackgroundBmp.Canvas.Draw(nX_BackgroundBmp, nY_BackgroundBmp, FBitmap.Bitmap);
          Draw(0, 0, FBackgroundBmp);
        end;
      end;
    end;
  end;

  // Transparent
  if (FFill.Transparent) and (FFill.Style = fsNormal) then
  begin
    DrawTransparentBmp(Self, Canvas);
    Canvas.CopyRect(FClientRect, canvas, FClientRect);
    Brush.Style := bsClear;
  end;

  // Resize
  if FPanelMover.CanResize then
  begin
    TmpFont := Canvas.Font;
    Canvas.Font.Name := 'Marlett';
    Canvas.Font.Size := 12;
    nX_Resize := ClientWidth - Canvas.TextWidth('o');
    nY_Resize := ClientHeight - Canvas.TextHeight('o');
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clBlack;
    Canvas.TextOut(nX_Resize, nY_Resize, 'o');
    Canvas.Font := TmpFont;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.GetRectSize;
begin
  FClientRect := GetClientRect;

  FPanelRect.Left := FClientRect.Left + BevelWidth + BorderWidth;
  FPanelRect.Top := FClientRect.Top + BevelWidth + BorderWidth;
  FPanelRect.Right := FClientRect.Right - BevelWidth + BorderWidth;
  FPanelRect.Bottom := FClientRect.Bottom - BevelWidth + BorderWidth;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.Collapse(Direction: TCollapseDirection);
begin
  FPanelMover.FMoveDirection := Direction;
  FPanelMover.FMoveTimer.Enabled := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.SetBuffered(const Value: Boolean);
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
procedure TCHPanel.MakeForeground;
var
  BRect : TRect;
begin
  BRect := Bounds(0, 0, Self.Width, Self.Height);
  FBackgroundBmp.Canvas.Brush.Color := clWhite;
  FBackgroundBmp.Canvas.Brush.Style := bsSolid;
  FBackgroundBmp.Canvas.FillRect(BRect);

  if (FBitmap.Mode = bmNormal) or (FBitmap.Mode = bmCenter) then
  begin
    FBackgroundBmp.Width := FPanelRect.Right - FPanelRect.Left;
    FBackgroundBmp.Height := FPanelRect.Bottom - FPanelRect.Top;
  end
  else if (FBitmap.Mode = bmStretch) or (FBitmap.Mode = bmTile) then
  begin
    FBackgroundBmp.Width := FBitmap.Bitmap.Width;
    FBackgroundBmp.Height := FBitmap.Bitmap.Height;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.SetGradient(const Value: TCHGradient);
begin
  FGradient.Assign(Value);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MoverDownClick(Sender: TObject);
begin
  FPanelMover.FMoveDirection := cdDown;
  FPanelMover.FMoveTimer.Enabled := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MoverLeftClick(Sender: TObject);
begin
  FPanelMover.FMoveDirection := cdLeft;
  FPanelMover.FMoveTimer.Enabled := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MoverRightClick(Sender: TObject);
begin
  FPanelMover.FMoveDirection := cdRight;
  FPanelMover.FMoveTimer.Enabled := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MoverUpClick(Sender: TObject);
begin
  FPanelMover.FMoveDirection := cdUp;
  FPanelMover.FMoveTimer.Enabled := True;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.ControlClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if (FPanelMover.MoveMode = mmControl) or (FPanelMover.MoveMode = mmAll) then
  begin
    ReleaseCapture;
    Self.Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);

  // Resize
  if FPanelMover.CanResize then
  begin
    if (Button = mbLeft) and ((Width - x) < 10) and ((Height - y) < 10) then
    begin
      FResize := True;
      MouseCapture := True;
      FResizeStartPos := Point(x, y);
      Screen.Cursor := crSizeNWSE;
    end;
  end;
  // Move
  if (FPanelMover.MoveMode = mmAll) and ((Width - x) > 10) and ((Height - y) > 10) then
  begin
    ReleaseCapture;
    Self.Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end
  else
    inherited;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewRect : TRect;
begin
  if FResize then
  begin
    NewRect := BoundsRect;
    SetBounds(NewRect.left, NewRect.Top, NewRect.Right - NewRect.Left +
      X - FResizeStartPos.X, NewRect.Bottom - NewRect.Top + Y - FResizeStartPos.Y);
    FResizeStartPos := Point(x, y);

    // MinWidth
    if (Width < FPanelMover.FMinWidth) then
    begin
      NewRect.Right := FPanelMover.FMinWidth;
      SetBounds(NewRect.left, NewRect.Top, NewRect.Right, NewRect.Bottom - NewRect.Top + Y - FResizeStartPos.Y);
    end;
    // MaxWidth
    if (Width > FPanelMover.FMaxWidth) then
    begin
      NewRect.Right := FPanelMover.FMaxWidth;
      SetBounds(NewRect.left, NewRect.Top, NewRect.Right, NewRect.Bottom - NewRect.Top + Y - FResizeStartPos.Y);
    end;

    // MinHeight
    if  (Height < FPanelMover.FMinHeight) then
    begin
      NewRect.Bottom := FPanelMover.FMinHeight;
      SetBounds(NewRect.left, NewRect.Top, NewRect.Right - NewRect.Left + X - FResizeStartPos.X, NewRect.Bottom);
    end;
    // MaxHeight
    if  (Height > FPanelMover.FMaxHeight) then
    begin
      NewRect.Bottom := FPanelMover.FMaxHeight;
      SetBounds(NewRect.left, NewRect.Top, NewRect.Right - NewRect.Left + X - FResizeStartPos.X, NewRect.Bottom);
    end;
  end
  else
  begin
    inherited;
    if FPanelMover.CanResize and ((Width - x) < 10) and ((Height - y) < 10) then
      Cursor := crSizeNWSE
    else
      Cursor := crDefault;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FResize then
  begin
    FResize := False;
    MouseCapture := false;
    Screen.Cursor := crDefault;
  end
  else
    inherited;

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanel.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPanel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPanel.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPanel.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ---  CHPanelLayout  ---}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHPanelLayout.Create(AOwner: TCHPanel);
begin
  inherited Create;
  FOwner := AOwner;

  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 10;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;

  FShowControl := False;
  FShowMoveDown := True;
  FShowMoveUp := True;
  FShowMoveRight := True;
  FShowMoveLeft := True;
  FControlColor := clActiveCaption;
  FControlStyle := bvLowered;
  FControlTextAlignment := taRightJustify;
  FControlX := 3;
  FControlY := 3;
  FSpace := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHPanelLayout.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlColor(const Value: TColor);
begin
  if FControlColor <> Value then
  begin
    FControlColor := Value;
    if FOwner.FControlPanel <> nil then
    begin
      FOwner.FControlPanel.Color := Value;
      FOwner.FMoverUp.Color := Value;
      FOwner.FMoverDown.Color := Value;
      FOwner.FMoverLeft.Color := Value;
      FOwner.FMoverRight.Color := Value;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlText(const Value: TCaption);
begin
  if FControlText <> Value then
  begin
    FControlText := Value;
    if FOwner.FControlPanel <> nil then
      FOwner.FControlPanel.Caption := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetMoveButton;
var
  nNextLeft, nTop, nButtonWidth : Integer;
begin
  if FOwner.FControlPanel <> nil then
  begin
    nNextLeft := 3;
    nTop := 3;
    nButtonWidth := FOwner.FMoverUp.Width;

    // Designtime
    if csDesigning in FOwner.ComponentState then
    begin
      FOwner.FMoverUp.Top := -FOwner.FMoverUp.Height;
      FOwner.FMoverDown.Top := -FOwner.FMoverDown.Height;
      FOwner.FMoverLeft.Top := -FOwner.FMoverLeft.Height;
      FOwner.FMoverRight.Top := -FOwner.FMoverRight.Height ;
    end
    // Runtime
    else
    begin
      FOwner.FMoverUp.Visible := False;
      FOwner.FMoverDown.Visible := False;
      FOwner.FMoverLeft.Visible := False;
      FOwner.FMoverRight.Visible := False;
    end;

    // Up
    if FShowMoveUp then
    begin
      FOwner.FMoverUp.Visible := True;
      FOwner.FMoverUp.Left := nNextLeft;
      FOwner.FMoverUp.Top := nTop;
      nNextLeft := nNextLeft + nButtonWidth + FSpace;
    end;
    // Down
    if FShowMoveDown then
    begin
      FOwner.FMoverDown.Visible := True;
      FOwner.FMoverDown.Left := nNextLeft;
      FOwner.FMoverDown.Top := nTop;
      nNextLeft := nNextLeft + nButtonWidth + FSpace;
    end;
    // Left
    if FShowMoveLeft then
    begin
      FOwner.FMoverLeft.Visible := True;
      FOwner.FMoverLeft.Left := nNextLeft;
      FOwner.FMoverLeft.Top := nTop;
      nNextLeft := nNextLeft + nButtonWidth + FSpace;
    end;
    // Right
    if FShowMoveRight then
    begin
      FOwner.FMoverRight.Visible := True;
      FOwner.FMoverRight.Left := nNextLeft;
      FOwner.FMoverRight.Top := nTop;
      nNextLeft := nNextLeft + nButtonWidth + FSpace;
    end;

    FAllButtonWidth := nNextLeft;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetSpace(const Value: integer);
begin
  if FSpace <> Value then
  begin
    if Value < 0 then
      FSpace := 0
    else
      FSpace := Value;
  end;
  SetMoveButton;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetShowControl(const Value: Boolean);
begin
  if FShowControl <> Value then
  begin
    FShowControl := Value;
    if FShowControl then
    begin
      // Control Area
      if FOwner.FControlPanel = nil then
      begin
        FOwner.FControlPanel := TPanel.Create(FOwner);
        with FOwner.FControlPanel do
        begin
          Parent := FOwner;
          ParentFont := False;
          Alignment := taRightJustify;
          DoubleBuffered := True;
          Caption := '';
          Color := FOwner.FPanelLayout.ControlColor;
          BevelOuter := FOwner.FPanelLayout.FControlStyle;

          Height := 26;
          Width := FOwner.Width - (FOwner.FPanelLayout.ControlX * 2);
          Left := FOwner.FPanelLayout.ControlX;
          Top := -Height;
          Visible := False;

          OnMouseDown := FOwner.ControlClick;
        end;

        // Collapse Up
        FOwner.FMoverUp := TCHButton.Create(FOwner.FControlPanel);
        with FOwner.FMoverUp do
        begin
          Parent := FOwner.FControlPanel;
          Width := 20;
          Height := 20;

          Border.Width := 1;
          Caption := '';
          Color := FOwner.FPanelLayout.ControlColor;
          Focus.Active := False;

          with Glyph do
          begin
            Glyph.LoadFromResourceName(hInstance, 'PN_MOVE_UP');
            Alignment := gaCenter;
            AlignMode := gmControl;
            Space := 0;
          end;
          OnClick := FOwner.MoverUpClick;
        end;

        // Collapse Down
        FOwner.FMoverDown := TCHButton.Create(FOwner.FControlPanel);
        with FOwner.FMoverDown do
        begin
          Parent := FOwner.FControlPanel;
          Width := 20;
          Height := 20;

          Border.Width := 1;
          Caption := '';
          Color := FOwner.FPanelLayout.ControlColor;
          Focus.Active := False;

          with Glyph do
          begin
            Glyph.LoadFromResourceName(hInstance, 'PN_MOVE_DOWN');
            Alignment := gaCenter;
            AlignMode := gmControl;
            Space := 0;
          end;
          OnClick := FOwner.MoverDownClick;
        end;

        // Collapse Left
        FOwner.FMoverLeft := TCHButton.Create(FOwner.FControlPanel);
        with FOwner.FMoverLeft do
        begin
          Parent := FOwner.FControlPanel;
          Width := 20;
          Height := 20;

          Border.Width := 1;
          Caption := '';
          Color := FOwner.FPanelLayout.ControlColor;
          Focus.Active := False;

          with Glyph do
          begin
            Glyph.LoadFromResourceName(hInstance, 'PN_MOVE_LEFT');
            Alignment := gaCenter;
            AlignMode := gmControl;
            Space := 0;
          end;
          OnClick := FOwner.MoverLeftClick;
        end;

        // Collapse Right
        FOwner.FMoverRight := TCHButton.Create(FOwner.FControlPanel);
        with FOwner.FMoverRight do
        begin
          Parent := FOwner.FControlPanel;
          Width := 20;
          Height := 20;

          Border.Width := 1;
          Caption := '';
          Color := FOwner.FPanelLayout.ControlColor;
          Focus.Active := False;

          with Glyph do
          begin
            Glyph.LoadFromResourceName(hInstance, 'PN_MOVE_RIGHT');
            Alignment := gaCenter;
            AlignMode := gmControl;
            Space := 0;
          end;
          OnClick := FOwner.MoverRightClick;
        end;
      end;

      FOwner.FControlPanel.Top := FControlY;
      FOwner.FControlPanel.Left := FControlX;
      FOwner.FControlPanel.Visible := True;
      SetMoveButton;
    end
    else
    begin
      if FOwner.FControlPanel <> nil then
      begin
        FOwner.FMoverUp.Free;
        FOwner.FMoverUp := nil;
        FOwner.FMoverDown.Free;
        FOwner.FMoverDown := nil;
        FOwner.FMoverLeft.Free;
        FOwner.FMoverLeft := nil;
        FOwner.FMoverRight.Free;
        FOwner.FMoverRight := nil;
        FOwner.FControlPanel.Free;
        FOwner.FControlPanel := nil;
      end;

//      FOwner.FControlPanel.Top := -FOwner.FControlPanel.Height;
//      FOwner.FControlPanel.Visible := False;

      //
    end;

  end;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetShowMoveDown(const Value: Boolean);
begin
  if FShowMoveDown <> Value then
  begin
    FShowMoveDown := Value;
  end;
  SetMoveButton;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetShowMoveLeft(const Value: Boolean);
begin
  if FShowMoveLeft <> Value then
  begin
    FShowMoveLeft := Value;
  end;
  SetMoveButton;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetShowMoveRight(const Value: Boolean);
begin
  if FShowMoveRight <> Value then
  begin
    FShowMoveRight := Value;
  end;
  SetMoveButton;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetShowMoveUp(const Value: Boolean);
begin
  if FShowMoveUp <> Value then
  begin
    FShowMoveUp := Value;
  end;
  SetMoveButton;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlX(const Value: Word);
begin
  if FControlX <> Value then
  begin
    FControlX := Value;
    if FOwner.FControlPanel <> nil then
      FOwner.FControlPanel.Left := FControlX;
  end;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlY(const Value: Word);
begin
  if FControlY <> Value then
  begin
    FControlY := Value;
    if FOwner.FControlPanel <> nil then
      FOwner.FControlPanel.Top := FControlY;
  end;
  FOwner.FPanelMover.DoAutosizer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlStyle(const Value: TPanelBevel);
begin
  if FControlStyle <> Value then
  begin
    FControlStyle := Value;
    if FOwner.FControlPanel <> nil then
      FOwner.FControlPanel.BevelOuter := FControlStyle;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetFont(const Value: TFont);
begin
  if FOwner.FControlPanel <> nil then
  begin
    FOwner.FControlPanel.Font.Assign(Value);
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPanelLayout.GetFont: TFont;
begin
  if FOwner.FControlPanel <> nil then
    Result := FOwner.FControlPanel.Font
  else
    Result := nil;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelLayout.SetControlTextAlignment(const Value: TAlignment);
begin
  if FControlTextAlignment <> Value then
  begin
    FControlTextAlignment := Value;
    if FOwner.FControlPanel <> nil then
      FOwner.FControlPanel.Alignment := FControlTextAlignment;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHPanelLayout.GetControlText: TCaption;
begin
  if FOwner.FControlPanel <> nil then
    Result := FOwner.FControlPanel.Caption
  else
    Result := '';
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ---  CHPanelMover  ---}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHPanelMover.Create(AOwner: TCHPanel);
begin
  inherited Create;
  FOwner := AOwner;

  FAutosizer := True;
  FMoveSpeed := 20;
  FMoveStep := 10;
  FMoveTimer := TTimer.Create(AOwner);
  FMoveTimer.Enabled := False;
  FMoveTimer.Interval := FMoveSpeed;
  FMoveTimer.OnTimer := DoCollapse;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHPanelMover.Destroy;
begin
  FMoveTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.DoAutosizer;
begin
  if FAutosizer then
  begin
    FMaxHeight := FOwner.Height;
    FMaxWidth := FOwner.Width;
    if FOwner.FPanelLayout.ShowControl then
    begin
      FMinHeight := FOwner.FControlPanel.Top + FOwner.FControlPanel.Height +
        FOwner.FPanelLayout.FControlY;
      FMinWidth := FOwner.FControlPanel.Left + FOwner.FPanelLayout.FAllButtonWidth +
        FOwner.FPanelLayout.FControlX + 3;
    end
    else
    begin
      FMinHeight := 0;
      FMinWidth := 0;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.DoCollapse;
begin
  if FCanCollapse then
  begin
    Autosizer := False;
    FMoveTimer.Interval := FMoveSpeed;

    // Down
    if FMoveDirection = cdDown then
    begin
      if (FOwner.Height <= FMaxHeight) and not (FMoveSpeed = 0) then
      begin
        FOwner.Height := FOwner.Height + FMoveStep;
        FOwner.Repaint;
      end
      else
      begin
        FOwner.Height := FMaxHeight;
        FOwner.Repaint;
        FMoveTimer.Enabled := False;
      end;
    end
    // Up
    else if FMoveDirection = cdUp then
    begin
      if (FOwner.Height >= FMinHeight) and not (FMoveSpeed = 0) then
      begin
        FOwner.Height := FOwner.Height - FMoveStep;
        FOwner.Repaint;
      end
      else
      begin
        FOwner.Height := FMinHeight + 1;
        FOwner.Repaint;
        FMoveTimer.Enabled := False;
      end;
    end
    // Left
    else if FMoveDirection = cdLeft then
    begin
      if FMinWidth < FOwner.FPanelLayout.FAllButtonWidth + FOwner.FPanelLayout.FControlX then
        FMinWidth := FOwner.FPanelLayout.FAllButtonWidth + FOwner.FPanelLayout.FControlX;

      if (FOwner.Width >= FMinWidth) and not (FMoveSpeed = 0) then
      begin
        FOwner.Width := FOwner.Width - FMoveStep;
        FOwner.Repaint;
      end
      else
      begin
        FOwner.Width := FMinWidth;
        FOwner.Repaint;
        FMoveTimer.Enabled := False;
      end;
    end
    // Right
    else if FMoveDirection = cdRight then
    begin
      if (FOwner.Width <= FMaxWidth) and not (FMoveSpeed = 0) then
      begin
        FOwner.Width := FOwner.Width + FMoveStep;
        FOwner.Repaint;
      end
      else
      begin
        FOwner.Width := FMaxWidth;
        FOwner.Repaint;
        FMoveTimer.Enabled := False;
      end;
    end
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.SetAutosizer(const Value: Boolean);
begin
  if FAutosizer <> Value then
  begin
    FAutosizer := Value;
    DoAutosizer;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.SetCanCollapse(const Value: Boolean);
begin
  if FCanCollapse <> Value then
  begin
    FCanCollapse := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.SetCanResize(const Value: Boolean);
begin
  if FCanResize <> Value then
  begin
    FCanResize := Value;
    FOwner.Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.SetMoveMode(const Value: TMoveMode);
begin
  if FMoveMode <> Value then
  begin
    FMoveMode := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHPanelMover.SetMoveSpeed(const Value: TMoveSpeed);
begin
  if FMoveSpeed <> Value then
  begin
    FMoveSpeed := Value;
  end;
end;





{ TCHPanelSizer }

constructor TCHPanelSizer.Create(AOwner: TCHPanel);
begin
  inherited Create;
  FOwner := AOwner;

  FAutoChildPosLeft := False;
  FAutoChildPosTop := False;
  FAutoChildWidth := False;
  FAutoChildHeight := False;
  pWidth := -1;
  pHeight := -1;
  PCtrlsCoordArr := nil;
end;

destructor TCHPanelSizer.Destroy;
begin
  inherited;
  FreeMem(PCtrlsCoordArr);
end;

procedure TCHPanel.Loaded;
var
  i : Integer;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
  begin
    if (FPanelSizer.pWidth = -1) and (FPanelSizer.pHeight = -1) then
    begin
     GetMem(FPanelSizer.PCtrlsCoordArr, ControlCount * sizeof(TRect));
     for  i := 0 to ControlCount - 1 do
     begin
        FPanelSizer.PCtrlsCoordArr[i].Left := Controls[i].Left;
        FPanelSizer.PCtrlsCoordArr[i].Top := Controls[i].Top;
        FPanelSizer.PCtrlsCoordArr[i].Width := Controls[i].Width;
        FPanelSizer.PCtrlsCoordArr[i].Height := Controls[i].Height;
     end;
     FPanelSizer.pWidth := Width;
     FPanelSizer.pHeight := Height;
    end;
  end;
end;

end.
