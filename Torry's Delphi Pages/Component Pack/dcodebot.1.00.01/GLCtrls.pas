
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit GLCtrls;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, BareOpenGL,
  BareOpenGLExt, BareGraphics, BareGraphicObjs;

{ TOpenGLControl }

type
  TRefreshKind = (rkNone, rkIdle, rkThread);

  TOpenGLControl = class(TWinControl)
  private
    FDC: HDC;
    FRC: HGLRC;
    FFrameRate: Single;
    FTimer: TStandardTimer;
    FRefreshKind: TRefreshKind;
    FTextWriter: TTextWriter;
    FOnDraw: TNotifyEvent;
    FOnUnLoad: TNotifyEvent;
    FOnLoad: TNotifyEvent;
    procedure WMCreate(var Msg); message WM_CREATE;
    procedure WMDestroy(var Msg); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetRefreshKind(Value: TRefreshKind);
    function GetTextWriter: TTextWriter;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw; virtual;
    function LockContext: Boolean;
    procedure UnLockContext;
    property FrameRate: Single read FFrameRate;
    property TextWriter: TTextWriter read GetTextWriter;
    property Timer: TStandardTimer read FTimer;
  published
    property Align;
    property Anchors;
    property RefreshKind: TRefreshKind read FRefreshKind write SetRefreshKind;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnUnLoad: TNotifyEvent read FOnUnLoad write FOnUnLoad;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
  end;

procedure BindGraphic(texture: GLuint; Bits: Pointer; Size: Integer); overload;
procedure BindGraphic(texture: GLuint; Graphic: TGraphic; Alpha: Boolean = False); overload;
procedure BindGraphic(texture: GLuint; const FileName: string; Alpha: Boolean = False); overload;

var
  DrawProc: TNotifyEvent;
  LoadProc: TNotifyEvent;
  UnloadProc: TNotifyEvent;

  { GL Utility Toolkit routines }

  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCube: procedure(size: GLdouble); stdcall;
  glutSolidCube: procedure(size: GLdouble); stdcall;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutWireDodecahedron: procedure; stdcall;
  glutSolidDodecahedron: procedure; stdcall;
  glutWireTeapot: procedure(size: GLdouble); stdcall;
  glutSolidTeapot: procedure(size: GLdouble); stdcall;
  glutWireOctahedron: procedure; stdcall;
  glutSolidOctahedron: procedure; stdcall;
  glutWireTetrahedron: procedure; stdcall;
  glutSolidTetrahedron: procedure; stdcall;
  glutWireIcosahedron: procedure; stdcall;
  glutSolidIcosahedron: procedure; stdcall;

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
procedure UnLoadGlut(Module: HMODULE);

implementation

type
  TRefreshManager = class
  private
    FThread: TThread;
    FIdleControls: TList;
    FThreadControls: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed(Control: TOpenGLControl);
    procedure RefreshIdle(Sender: TObject; var Done: Boolean);
    procedure RefreshThread;
  end;

  TRefreshThread = class(TThread)
  private
    FManager: TRefreshManager;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TRefreshManager);
  end;

var
  Manager: TRefreshManager;

constructor TRefreshManager.Create;
begin
  inherited Create;
  FIdleControls := TList.Create;
  FThreadControls := TList.Create;
end;

destructor TRefreshManager.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
  FIdleControls.Free;
  FThreadControls.Free;
  inherited Destroy;
end;

procedure TRefreshManager.Changed(Control: TOpenGLControl);
begin
  FIdleControls.Remove(Control);
  FThreadControls.Remove(Control);
  if Control.RefreshKind = rkIdle then
    FIdleControls.Add(Control)
  else if Control.RefreshKind = rkThread then
  begin
    FThreadControls.Add(Control);
    if FThread = nil then
      FThread := TRefreshThread.Create(Self);
  end;
  if FIdleControls.Count > 0 then
    Application.OnIdle := RefreshIdle
  else
    Application.OnIdle := nil;
  if (FThreadControls.Count = 0) and (FThread <> nil) then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

procedure TRefreshManager.RefreshIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
  for I := 0 to FIdleControls.Count - 1 do
    TOpenGLControl(FIdleControls[I]).Draw;
  Done := False;
end;

procedure TRefreshManager.RefreshThread;
var
  I: Integer;
begin
  for I := 0 to FThreadControls.Count - 1 do
    TOpenGLControl(FThreadControls[I]).Draw;
end;

{ TRefreshThread }

constructor TRefreshThread.Create(Manager: TRefreshManager);
begin
  FManager := Manager;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TRefreshThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(FManager.RefreshThread);
    Sleep(1);
  end;
end;

{ TOpenGLControl }

constructor TOpenGLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then
    ControlState := ControlState + [csCustomPaint];
  FRefreshKind := rkThread;
  FTimer := TStandardTimer.Create;
  Width := 400;
  Height := 300;
end;

destructor TOpenGLControl.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TOpenGLControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if csDesigning in ComponentState then Exit;
  Params.WindowClass.style := Params.WindowClass.style or CS_OWNDC;
end;

procedure TOpenGLControl.WMCreate(var Msg);
var
  Descriptor: TPixelFormatDescriptor;
  Format: Integer;
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  FDC := GetDC(Handle);
  FillChar(Descriptor, SizeOf(TPixelFormatDescriptor), #0);
  with Descriptor do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 16;
    cStencilBits := 1;
    iLayerType := PFD_MAIN_PLANE;
  end;
  Format := ChoosePixelFormat(FDC, @Descriptor);
  if SetPixelFormat(FDC, Format, @Descriptor) then
  begin
    FRC := wglCreateContext(FDC);
    if LockContext then
    try
      wglMakeCurrent(FDC, FRC);
      glxLoadExtensions;
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      glClearColor(0.0, 0.0, 0.0, 1.0);
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      gluPerspective(60, 4 / 3, 0.5, 1000);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      if Assigned(FOnLoad) then
        FOnLoad(Self)
      else if Assigned(LoadProc) then
        LoadProc(Self);
    finally
      UnlockContext;
    end;
  end;
  if FRC <> 0 then
    Manager.Changed(Self);
end;

procedure TOpenGLControl.WMDestroy(var Msg);
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  FRefreshKind := rkNone;
  Manager.Changed(Self);
  if LockContext then
  try
    if Assigned(FOnUnLoad) then
      FOnUnLoad(Self)
    else if Assigned(UnLoadProc) then
      UnLoadProc(Self);
  finally
    UnlockContext;    
    FTextWriter.Free;
    wglDeleteContext(FRC);
    FRC := 0;
  end;
  ReleaseDC(Handle, FDC);
  FDC := 0;
  inherited;
end;

procedure TOpenGLControl.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if csDesigning in ComponentState then
    inherited
  else
    Msg.Result := 0;
end;
procedure TOpenGLControl.PaintWindow(DC: HDC);
var
  Pen: HPEN;
  Brush: HBRUSH;
begin
  if csDesigning in ComponentState then
  begin
    Pen := SelectObject(DC, CreatePen(PS_DASH, 0, 0));
    Brush := SelectObject(DC, GetStockObject(HOLLOW_BRUSH));
    Rectangle(DC, 0, 0, Width, Height);
    SelectObject(DC, Brush);
    DeleteObject(SelectObject(DC, Pen));
  end
  else
    Draw;
end;

procedure TOpenGLControl.Resize;
begin
  inherited Resize;
  if LockContext then
  try
    glViewport(0, 0, Width, Height);
  finally
    UnlockContext;
  end;
end;

procedure TOpenGLControl.Draw;
var
  Interval: Single;
begin
  if LockContext then
  try
    Interval := FTimer.Time;
    FTimer.Calculate;
    if FTimer.Time > Interval then
      FFrameRate := 1 / (FTimer.Time - Interval)
    else
      FFrameRate := -1;
    if Assigned(FOnDraw) then
      FOnDraw(Self)
    else if Assigned(DrawProc) then
      DrawProc(Self)
    else
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    SwapBuffers(FDC);
  finally
    UnlockContext;
  end;
end;

function TOpenGLControl.LockContext: Boolean;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(FDC, FRC);
    Result := True;
  end
  else
    Result := False;
end;

procedure TOpenGLControl.UnLockContext;
begin
  if FRC <> 0 then
    wglMakeCurrent(FDC, 0);
end;

procedure TOpenGLControl.SetRefreshKind(Value: TRefreshKind);
begin
  if Value <> FRefreshKind then
  begin
    FRefreshKind := Value;
    if csDesigning in ComponentState then
      Exit;
    Manager.Changed(Self);
  end;
end;

function TOpenGLControl.GetTextWriter: TTextWriter;
begin
  if FTextWriter = nil then
  begin
    FTextWriter := CreateTextWriter;
    FTextWriter.Shadow := True;
    FTextWriter.ShadowColor^ := fcBlack;
  end;
  Result := FTextWriter;
end;

procedure BindGraphic(texture: GLuint; Bits: Pointer; Size: Integer);
begin
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Size, Size, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, Bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure BindGraphic(texture: GLuint; Graphic: TGraphic; Alpha: Boolean = False);

  function Max(A, B: Integer): Integer;
  begin
    if A > B then Result := A else Result := B;
  end;

var
  Bitmap: TBitmap;
  Bits: Pointer;
  Source: PRGBTriple;
  Dest: PRGBQuad;
  A, B, C: Integer;
begin
  glBindTexture(GL_TEXTURE_2D, texture);
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    A := Max(Graphic.Width, Graphic.Height);
    B := 1;
    while B < A do B := B shl 1;
    Bitmap.Width := B;
    Bitmap.Height := B;
    Bitmap.Canvas.StretchDraw(Bitmap.Canvas.ClipRect, Graphic);
    GetMem(Bits, B * B * SizeOf(TRGBQuad));
    try
      Dest := Bits;
      for A := 0 to B - 1 do
      begin
        Source := Bitmap.ScanLine[A];
        for C := 0 to B - 1 do
        begin
          if Alpha then
          begin
            Dest.rgbReserved := (Source.rgbtRed + Source.rgbtGreen +
              Source.rgbtBlue) div 3;
            Dest.rgbRed := $FF;
            Dest.rgbGreen := $FF;
            Dest.rgbBlue := $FF;
          end
          else
          begin
            Dest.rgbBlue := Source.rgbtRed;
            Dest.rgbGreen := Source.rgbtGreen;
            Dest.rgbRed := Source.rgbtBlue;
            Dest.rgbReserved := $FF;
          end;
          Inc(Dest);
          Inc(Source);
        end;
      end;
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, B, B, 0, GL_RGBA,
        GL_UNSIGNED_BYTE, Bits);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    finally
      FreeMem(Bits);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure BindGraphic(texture: GLuint; const FileName: string; Alpha: Boolean = False);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    BindGraphic(texture, Picture.Graphic, Alpha);
  finally
    Picture.Free;
  end;
end;

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
begin
  Result := LoadLibrary(PChar(ModuleName));
  if Result <> 0 then
  begin
    @glutWireSphere := GetProcAddress(Result, 'glutWireSphere');
    @glutSolidSphere := GetProcAddress(Result, 'glutSolidSphere');
    @glutWireCone := GetProcAddress(Result, 'glutWireCone');
    @glutSolidCone := GetProcAddress(Result, 'glutSolidCone');
    @glutWireCube := GetProcAddress(Result, 'glutWireCube');
    @glutSolidCube := GetProcAddress(Result, 'glutSolidCube');
    @glutWireTorus := GetProcAddress(Result, 'glutWireTorus');
    @glutSolidTorus := GetProcAddress(Result, 'glutSolidTorus');
    @glutWireDodecahedron := GetProcAddress(Result, 'glutWireDodecahedron');
    @glutSolidDodecahedron := GetProcAddress(Result, 'glutSolidDodecahedron');
    @glutWireTeapot := GetProcAddress(Result, 'glutWireTeapot');
    @glutSolidTeapot := GetProcAddress(Result, 'glutSolidTeapot');
    @glutWireOctahedron := GetProcAddress(Result, 'glutWireOctahedron');
    @glutSolidOctahedron := GetProcAddress(Result, 'glutSolidOctahedron');
    @glutWireTetrahedron := GetProcAddress(Result, 'glutWireTetrahedron');
    @glutSolidTetrahedron := GetProcAddress(Result, 'glutSolidTetrahedron');
    @glutWireIcosahedron := GetProcAddress(Result, 'glutWireIcosahedron');
    @glutSolidIcosahedron := GetProcAddress(Result, 'glutSolidIcosahedron');
  end;
end;

procedure UnLoadGlut(Module: HMODULE);
begin
  @glutWireSphere := nil;
  @glutSolidSphere := nil;
  @glutWireCone := nil;
  @glutSolidCone := nil;
  @glutWireCube := nil;
  @glutSolidCube := nil;
  @glutWireTorus := nil;
  @glutSolidTorus := nil;
  @glutWireDodecahedron := nil;
  @glutSolidDodecahedron := nil;
  @glutWireTeapot := nil;
  @glutSolidTeapot := nil;
  @glutWireOctahedron := nil;
  @glutSolidOctahedron := nil;
  @glutWireTetrahedron := nil;
  @glutSolidTetrahedron := nil;
  @glutWireIcosahedron := nil;
  @glutSolidIcosahedron := nil;
  if Module <> 0 then
    FreeLibrary(Module);
end;

initialization
  Manager := TRefreshManager.Create;
finalization
  Manager.Free;
end.

