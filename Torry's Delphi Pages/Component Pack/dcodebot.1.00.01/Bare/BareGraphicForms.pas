                                                   
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareGraphicForms;

interface

{$I BARE.INC}

uses
  Messages, Windows, BareGraphics, BareOpenGL, BareForms, BareOpenGLExt,
  BareInterchange, BareStream, BareZLib, {$IFDEF BARE}BareUtils
  {$ELSE}SysUtils, Classes, StrTools, FileTools{$ENDIF};

function GetTicks: Cardinal;

{ TTimer }

type
  TTimer = class(TObject)
  private
    FTime: Single;
    FInterval: Single;
    FPaused: Boolean;
    procedure SetPaused(Value: Boolean);
  protected
    function InternalTime: Single; virtual; abstract;
    procedure InternalReset; virtual; abstract;
    procedure InternalPaused(NewValue: Boolean); virtual; abstract;
  public
    procedure Calculate;
    procedure Reset;
    property Interval: Single read FInterval;
    property Paused: Boolean read FPaused write SetPaused;
    property Time: Single read FTime;
  end;

  TTimerClass = class of TTimer;

{ TStandardTimer }

  TStandardTimer = class(TTimer)
  private
    FResolution: Int64;
    FStartTime: Int64;
    FCurrentTime: Int64;
  protected
    function InternalTime: Single; override;
    procedure InternalReset; override;
    procedure InternalPaused(NewValue: Boolean); override;
  end;

{ TTextWriter }

  TBitmapFont = record
    CharWidth: Single;
    CharHeight: Single;
    Texture: GLuint;
    TextureWidth: Single;
    TextureHeight: Single;
    TextureOffsetX: Single;
    TextureOffsetY: Single;
    TextureBorder: Single;
  end;
  PBitmapFont = ^TBitmapFont;

  TBitmapFonts = array of TBitmapFont;

  TTextBounds = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
  end;
  PTextBounds = ^TTextBounds;

  TTextWriter = class(TObject)
  private
    FBaseColor: PFloatColor;
    FColors: array[0..2] of TFloatColor;
    FFontIndex: Integer;
    FFonts: TBitmapFonts;
    FLeadColor: PFloatColor;
    FShadowColor: PFloatColor;
    FShadow: Boolean;
    FBounds: TTextBounds;
    FTextBounds: PTextBounds;
    FHorizontalFlip: Single;
    FVerticalFlip: Single;
    FTexture: GLuint;
    FTextureLoaded: Boolean;
    procedure SetHorizontalFlip(Value: Single);
    procedure SetVerticalFlip(Value: Single);
  protected
    procedure InitFonts(var Fonts: TBitmapFonts); virtual;
    procedure InternalLoad(const Fonts: TBitmapFonts; TextureInfo: TTextureInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const Fonts: TBitmapFonts; const FileName: string;
      Alpha: Boolean = False); overload;
    procedure Load(const Fonts: TBitmapFonts; Stream: TStream;
      Format: TTextureFormat; Alpha: Boolean = False); overload;
    procedure Write(const S: string; Size: Double; X, Y: Single);
    property BaseColor: PFloatColor read FBaseColor;
    property FontIndex: Integer read FFontIndex write FFontIndex;
    property LeadColor: PFloatColor read FLeadColor;
    property ShadowColor: PFloatColor read FShadowColor;
    property Shadow: Boolean read FShadow write FShadow;
    property TextBounds: PTextBounds read FTextBounds;
    property HorizontalFlip: Single read FHorizontalFlip write SetHorizontalFlip;
    property VerticalFlip: Single read FVerticalFlip write SetVerticalFlip;
  end;

function DefaultTextWriter: TTextWriter;

{ TVertexObject }

type
  TVertexMode = (vmBuffer, vmArray, vmImmediate);

  TVertexObject = class
  private
    FVertexBuffer: GLuint;
    FIndexBuffer: GLuint;
    FVertices: TVertices;
    FIndexes: TTriangleIndexes;
    FMode: TVertexMode;
    procedure SetMode(Value: TVertexMode);
    function GetTriangleCount: Integer;
    function GetVertexCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    property Mode: TVertexMode read FMode write SetMode;
    property TriangleCount: Integer read GetTriangleCount;
    property VertexCount: Integer read GetVertexCount;
  end;

{ TGraphicForm }

  TGraphicForm = class(TForm)
  private
    FDC: HDC;
    FRC: HGLRC;
    FTime: Cardinal;
    FTopLevel: Boolean;
    FFrame: Integer;
    FFrameTime: Cardinal;
    FFramesPerSecond: Single;
    FTimer: TTimer;
    procedure WMCreate(var Message: TWMCreate); message WM_CREATE;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure InitDisplayInfo(var Info: TDisplayInfo); virtual;
    procedure CreateResourceContext;
    procedure DestroyResourceContext;
    procedure CreateTimer(TimerClass: TTimerClass);
    procedure InitializeLayout; virtual;
    procedure FinalizeLayout; virtual;
    procedure CheckKeys; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; virtual;
    procedure Resize(Width, Height: Integer); override;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
    property TopLevel: Boolean read FTopLevel write FTopLevel;
  public
    destructor Destroy; override;
    procedure Draw; virtual;
    property Frame: Integer read FFrame;
    property FramesPerSecond: Single read FFramesPerSecond;
    property Timer: TTimer read FTimer;
  end;

{ TGraphicApplication }

  TGraphicApplication = class(TApplication)
  private
    FTimer: TTimer;
    FThrottle: Integer;
    FThrottleTime: Single;
    procedure SetThrottle(Value: Integer);
  public
    constructor Create(WindowClass: TWindowClass = nil); override;
    destructor Destroy; override;
    destructor Run; override;
    property Throttle: Integer read FThrottle write SetThrottle;
  end;

{ Scene related objects }

  TSceneObject = class(TObject)
  private
    FActive: Boolean;
  public
    constructor Create; virtual;
    property Active: Boolean read FActive write FActive;
  end;

{ TSkybox }

  TSkyboxPanel = (spFront, spLeft, spRight, spTop, spBack, spBottom);

  TSkybox = class(TSceneObject)
  private
    FPanels: array[TSkyboxPanel] of GLuint;
    function GetPanel(Panel: TSkyboxPanel): GLuint;
    procedure SetPanel(Panel: TSkyboxPanel; Value: GLuint);
  public
    procedure Draw;
    property Panels[Panel: TSkyboxPanel]: GLuint read GetPanel write SetPanel;
  end;

{ TCamera }

  TCamera = class(TSceneObject)
  private
    FChanged: Boolean;
    FPosition: TVertexPoint;
    FDirection: TDirection;
    FReferenceVertex: TVertexPoint;
    FNormal: TVertexPoint;
    FTarget: PVertexPoint;
    FAspectRatio: Single;
    FViewAngle: Single;
    FViewVector: TVertexPoint;
    FViewPlane: TPlane;
    function GetPosition(Index: Integer): Single;
    procedure SetPosition(Index: Integer; Value: Single);
    function GetDirection(Index: Integer): Single;
    procedure SetDirection(Index: Integer; Value: Single);
    procedure SetViewAngle(Value: Single);
  protected
    procedure Change;
    property Changed: Boolean read FChanged;
  public
    constructor Create; override;
    procedure Move(const Direction: TDirection; Distance: Single);
    function RelativeDirection(X, Y, Z: Single): TDirection; overload;
    function RelativeDirection(const V: TVertexPoint): TDirection; overload;
    procedure Setup; virtual;
    property AspectRatio: Single read FAspectRatio write FAspectRatio;
    property Target: PVertexPoint read FTarget write FTarget;
    property Direction: TDirection read FDirection;
    property Position: TVertexPoint read FPosition;
    property X: Single index 0 read GetPosition write SetPosition;
    property Y: Single index 1 read GetPosition write SetPosition;
    property Z: Single index 2 read GetPosition write SetPosition;
    property Heading: Single index 3 read GetDirection write SetDirection;
    property Pitch: Single index 4 read GetDirection write SetDirection;
    property Roll: Single index 5 read GetDirection write SetDirection;
    property ViewAngle: Single read FViewAngle write SetViewAngle;
    property ViewVector: TVertexPoint read FViewVector;
    property ViewPlane: TPlane read FViewPlane;
  end;

{ TShooterCamera }

  TShooterCamera = class(TCamera)
  private
    FSpeed: Single;
  public
    procedure Setup; override;
    property Speed: Single read FSpeed write FSpeed;
  end;

{ TResourceManager }

  TResourceManager = class(TObject)
  private
    FStorage: TFolderStructure;
  protected
    property Storage: TFolderStructure read FStorage;
  public
    constructor Create(Storage: TFolderStructure);
    procedure Open(const Ident: string); virtual; abstract;
    procedure Close(const Ident: string); virtual; abstract;
  end;

{ TModel }

  TModel = class(TObject)
  private
    FPolygons: TPolygons;
    FVertices: TVertices;
    FName: string;
    FSmooth: Boolean;
    procedure UpdateNormals;
    function GetPolygon(Index: Integer): PPolygon;
    function GetPolygonCount: Integer;
  public
    procedure Clear;
    procedure Draw; virtual;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property Polygon[Index: Integer]: PPolygon read GetPolygon;
    property PolygonCount: Integer read GetPolygonCount;
    property Name: string read FName write FName;
    property Smooth: Boolean read FSmooth write FSmooth;
  end;

{ TModelGroup }

  TModelGroup = class(TObject)
  private
    FModels: array of TModel;
    FName: string;
    function GetModel(Index: Integer): TModel;
    function GetModelCount: Integer;
  public
    destructor Destroy; override;
    procedure Draw;
    property Name: string read FName write FName;
    property Model[Index: Integer]: TModel read GetModel; default;
    property ModelCount: Integer read GetModelCount;
  end;

{ TModelManager }

  PModelLink = ^TModelLink;
  TModelLink = record
    Folder: string;
    ModelGroup: TModelGroup;
    Next: PModelLink;
  end;

  TModelManager = class(TResourceManager)
  private
    FModels: PModelLink;
    function CreateLink(Ident: string): PModelLink;
    procedure DestroyLink(Link: PModelLink);
  protected
    function FindLink(const Ident: string): PModelLink;
  public
    destructor Destroy; override;
    procedure Open(const Ident: string); override;
    procedure Close(const Ident: string); override;
    function FindModel(const Ident: string): TModelGroup;
  end;

{ TTextureManager }

  TTextures = array of GLuint;
  TTextureCore = record
    Name: string;
    Info: TTextureInfo;
  end;
  TTextureData = array of TTextureCore;

  PTextureLink = ^TTextureLink;
  TTextureLink = record
    Folder: string;
    Opened: Boolean;
    Locked: Boolean;
    Textures: TTextures;
    Data: TTextureData;
    Next: PTextureLink;
  end;

  TTextureManager = class(TResourceManager)
  private
    FTextureFormat: TTextureFormat;
    FTextures: PTextureLink;
    function CreateLink(Ident: string): PTextureLink;
    procedure DestroyLink(Link: PTextureLink);
  public
    destructor Destroy; override;
    property TextureFormat: TTextureFormat read FTextureFormat write FTextureFormat;
    procedure Open(const Ident: string); override;
    procedure Close(const Ident: string); override;
    procedure Lock(const Ident: string);
    procedure Unlock(const Ident: string);
    function FindLink(const Ident: string): PTextureLink;
    function FindTexture(const Ident: string): GLuint;
    function FindTextures(const Ident: string): TTextures;
    procedure SelectSkybox(const Ident: string);
  end;

{ TSceneForm }

  TSceneInfo = record
    Camera: TCamera;
    Skybox: TSkybox;
    TextWriter: TTextWriter;
    Timer: TTimer;
  end;

  TSceneForm = class(TGraphicForm)
  private
    FStorage: TFolderStructure;
    FModelManager: TModelManager;
    FTextureManager: TTextureManager;
    FSceneIndex: Integer;
    FCamera: TCamera;
    FSkybox: TSkybox;
    FTextWriter: TTextWriter;
    {$IFDEF DEBUGSTRINGS}
    FDebugStrings: TStrings;
    {$ENDIF}
  protected
    procedure OpenStorage(const FileName: string);
    procedure CloseStorage;
    procedure InitDisplayInfo(var Info: TDisplayInfo); override;
    procedure InitSceneInfo(var Info: TSceneInfo); virtual;
    procedure InitializeLayout; override;
    procedure FinalizeLayout; override;
  public
    constructor Create(Parent: HWND = 0); override;
    destructor Destroy; override;
    function FindStream(const Path: string): TStream;
    property Camera: TCamera read FCamera;
    property ModelManager: TModelManager read FModelManager;
    property TextureManager: TTextureManager read FTextureManager;
    property SceneIndex: Integer read FSceneIndex write FSceneIndex;
    property Skybox: TSkybox read FSkybox;
    property Storage: TFolderStructure read FStorage;
    property TextWriter: TTextWriter read FTextWriter;
    {$IFDEF DEBUGSTRINGS}
    property DebugStrings: TStrings read FDebugStrings;
    {$ENDIF}
  end;

procedure OpenModels(const ResName: string);
procedure CloseModels(const ResName: string);
function FindModel(const ResName: string): TModelGroup;
procedure OpenTextures(const ResName: string);
procedure CloseTextures(const ResName: string);
procedure LockTextures(const ResName: string);
procedure UnlockTextures(const ResName: string);
function FindTexture(const ResName: string): GLuint;
function FindTextures(const ResName: string): TTextures;
procedure OpenSkyboxes(const ResName: string);
procedure CloseSkyboxes(const ResName: string);
procedure LockSkyboxes(const ResName: string);
procedure UnlockSkyboxes(const ResName: string);
procedure SelectSkybox(const ResName: string);

{ Variables }

var
  EscapeCode: Byte;
  FullscreenCode: Byte;
  Scene: TSceneForm;

implementation

{$IFDEF DEBUGSTRINGS}
procedure AddDebugText(const S: string; Condition: Boolean = True);
begin
  if (Scene <> nil) and Condition then
      Scene.DebugStrings.Add(S);
end;
{$ENDIF}

function GetTicks: Cardinal;
const
  Resolution: Int64 = 0;
  StartTime: Int64 = 0;
var
  CurrentTime: Int64;
begin
  if Resolution = 0 then
  begin
    QueryPerformanceFrequency(Resolution);
    Resolution := Resolution div 1000;
    QueryPerformanceCounter(StartTime);
  end;
  QueryPerformanceCounter(CurrentTime);
  Result := (CurrentTime - StartTime) div Resolution;
end;

{ TTimer }

procedure TTimer.Calculate;
var
  NewTime: Single;
begin
  NewTime := InternalTime;
  FInterval := NewTime - FTime;
  FTime := NewTime;
end;

procedure TTimer.Reset;
begin
  InternalReset;
  FTime := 0;
end;

procedure TTimer.SetPaused(Value: Boolean);
begin
  if Value <> FPaused then
  begin
    FPaused := Value;
    InternalPaused(Value);
  end;
end;

{ TStandardTimer }

function TStandardTimer.InternalTime: Single;
begin
  if FResolution = 0 then
  begin
    QueryPerformanceFrequency(FResolution);
    FResolution := FResolution div 1000;
    QueryPerformanceCounter(FStartTime);
    QueryPerformanceCounter(FCurrentTime);
  end;
  if not Paused then
    QueryPerformanceCounter(FCurrentTime);
  Result := ((FCurrentTime - FStartTime) div FResolution) / 1000;
end;

procedure TStandardTimer.InternalPaused(NewValue: Boolean);
var
  PauseTime: Int64;
begin
  if NewValue then
    QueryPerformanceCounter(FCurrentTime)
  else
  begin
    QueryPerformanceCounter(PauseTime);
    FStartTime := FStartTime + PauseTime - FCurrentTime;
  end;
end;

procedure TStandardTimer.InternalReset;
begin
  QueryPerformanceCounter(FStartTime);
  FCurrentTime := FStartTime;
end;

{ TTextWriter }

constructor TTextWriter.Create;
begin
  inherited Create;
  FColors[0] := fcWhite;
  FColors[1] := fcWhite;
  FColors[2] := fcBlack;
  FLeadColor := @FColors[0];
  FBaseColor := @FColors[1];
  FShadowColor := @FColors[2];
  FTextBounds := @FBounds;
  FTextBounds.Right := 1;
  FTextBounds.Bottom := 1;
  FHorizontalFlip := 1;
  FVerticalFlip := 1;
  InitFonts(FFonts);
end;

destructor TTextWriter.Destroy;
begin
  if FTextureLoaded then
    glDeleteTextures(1, @FTexture);
  inherited Destroy;
end;

procedure TTextWriter.InternalLoad(const Fonts: TBitmapFonts;
  TextureInfo: TTextureInfo);
var
  I: Integer;
begin
  try
    if not FTextureLoaded then
    begin
      glGenTextures(1, @FTexture);
      BindTexture(TextureInfo, FTexture);
    end;
  finally
    DisposeTexture(TextureInfo);
  end;
  FFonts := Fonts;
  for I := Low(FFonts) to High(FFonts) do
    FFonts[I].Texture := FTexture;
  InitFonts(FFonts);
end;

procedure TTextWriter.Load(const Fonts: TBitmapFonts; const FileName: string;
  Alpha: Boolean = False);
begin
  InternalLoad(Fonts, LoadTexture(FileName, Alpha));
end;

procedure TTextWriter.Load(const Fonts: TBitmapFonts; Stream: TStream;
  Format: TTextureFormat; Alpha: Boolean = False);
begin
  InternalLoad(Fonts, LoadTexture(Stream, Format, Alpha));
end;

procedure TTextWriter.InitFonts(var Fonts: TBitmapFonts);
var
  I: Integer;
begin
  for I := Low(Fonts) to High(Fonts) do
  begin
    glBindTexture(GL_TEXTURE_2D, Fonts[I].Texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;
end;

procedure TTextWriter.Write(const S: string; Size: Double; X, Y: Single);

  procedure WriteCharacters(const BitmapFont: TBitmapFont; X, Y: Single);
  var
    CharX, CharY, TexX, TexY: Single;
    Margin: Single;
    NewLine: Boolean;
    C: Byte;
    I: Integer;
  begin
    with BitmapFont do
    begin
      CharX := CharWidth * Size;
      CharY := CharHeight * Size;
      X := X * CharX + TextBounds.Left;
      Y := Y * CharY + TextBounds.Top;
      Margin := X;
      NewLine := False;
      glBegin(GL_QUADS);
      for I := 1 to Length(S) do
      begin
        if Y > TextBounds.Bottom then Break;
        C := Byte(S[I]);
        case C of
          0..9, 11..12, 14..31, 128..255:
            C := 32;
          10, 13:
            if NewLine then
            begin
              NewLine := False;
              X := Margin;
              Y := Y + CharY;
              Continue;
            end
            else
            begin
              NewLine := True;
              Continue;
            end;
        end;
        NewLine := False;
        if (X + CharX < TextBounds.Left) or (X > TextBounds.Right) or
          (Y + CharY < TextBounds.Top) then
        begin
          X := X + CharX;
          Continue;
        end;
        C := C - 32;
        TexX := TextureOffsetX + (C mod 16) * TextureWidth;
        TexY := TextureOffsetY + (C div 16) * TextureHeight;
        with LeadColor^ do glxColor4f(Red, Green, Blue, Alpha);
        glTexCoord(TexX + TextureBorder, TexY + TextureBorder);
        glVertex2f(X + (1 - FHorizontalFlip) * CharX, Y + (1 - FVerticalFlip) * CharY);
        with BaseColor^ do glxColor4f(Red, Green, Blue, Alpha);
        glTexCoord(TexX + TextureBorder, TexY + TextureHeight - TextureBorder);
        glVertex2f(X + (1 - FHorizontalFlip) * CharX, Y + CharY * FVerticalFlip);
        glTexCoord(TexX + TextureWidth - TextureBorder, TexY + TextureHeight - TextureBorder);
        glVertex2f(X + CharX * FHorizontalFlip, Y + CharY * FVerticalFlip);
        with LeadColor^ do glxColor4f(Red, Green, Blue, Alpha);
        glTexCoord(TexX + TextureWidth - TextureBorder, TexY + TextureBorder);
        glVertex2f(X + CharX * FHorizontalFlip, Y + (1 - FVerticalFlip) * CharY);
        X := X + CharX;
      end;
      glEnd;
    end;
  end;

const
  ShadowOffset = 0.1;
var
  ShadowEnabled: Boolean;
  PriorLead: TFloatColor;
  PriorBase: TFloatColor;
  Culled: GLboolean;
  Textured: GLboolean;
begin
  if (FHorizontalFlip = 0.5) or (FVerticalFlip = 0.5) then Exit;
  ShadowEnabled := FShadow;
  if ShadowEnabled then
  begin
    FShadow := False;
    PriorLead := FColors[0];
    PriorBase := FColors[1];
    FColors[0] := FColors[2];
    FColors[1] := FColors[2];
    FColors[0].Alpha := PriorLead.Alpha;
    FColors[1].Alpha := PriorBase.Alpha;
    X := X + ShadowOffset;
    Y := Y + ShadowOffset;
  end;
  glxBeginOrtho(1, 1);
  glGetBooleanv(GL_CULL_FACE, @Culled);
  glDisable(GL_CULL_FACE);
  glGetBooleanv(GL_TEXTURE_2D, @Textured);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FFonts[FFontIndex].Texture);
  WriteCharacters(FFonts[FFontIndex], X, Y);
  glxEndOrtho;
  if Culled then
    glEnable(GL_CULL_FACE);
  if not Textured then
    glDisable(GL_TEXTURE_2D);
  if ShadowEnabled then
  begin
    FColors[0] := PriorLead;
    FColors[1] := PriorBase;
    Write(S, Size, X - ShadowOffset, Y - ShadowOffset);
    FShadow := True;
  end;
end;

procedure TTextWriter.SetHorizontalFlip(Value: Single);
begin
  if Value < 0 then
    Value := 0
  else if Value > 1 then
    Value := 1;
  FHorizontalFlip := Value;
end;

procedure TTextWriter.SetVerticalFlip(Value: Single);
begin
  if Value < 0 then
    Value := 0
  else if Value > 1 then
    Value := 1;
  FVerticalFlip := Value;
end;

const
  DefaultFont: array[0..1] of TBitmapFont = (
    (CharWidth: 0.01375;
    CharHeight: 0.03;
    Texture: 0;
    TextureWidth: 0.0546875;
    TextureHeight: 0.0859375;
    TextureOffsetX: 0;
    TextureOffsetY: 0;
    TextureBorder: 0.0078125),
    (CharWidth: 0.0125;
    CharHeight: 0.02333;
    Texture: 0;
    TextureWidth: 0.05078125;
    TextureHeight: 0.06640625;
    TextureOffsetX: 0;
    TextureOffsetY: 0.515625;
    TextureBorder: 0.0));

var
  InternalTextWriter: TObject = nil;

function DefaultTextWriter: TTextWriter;
var
  Fonts: TBitmapFonts;
begin
  if InternalTextWriter <> nil then
    Result := InternalTextWriter as TTextWriter
  else
  begin
    Result := TTextWriter.Create;
    SetLength(Fonts, 2);
    Fonts[0] := DefaultFont[0];
    Fonts[1] := DefaultFont[1];
    Result.Load(Fonts, 'font.png');
    InternalTextWriter := Result;
  end;
end;

{ TVertexObject }

constructor TVertexObject.Create;
begin
  inherited Create;
  Mode := vmBuffer;
  glxGenBuffers(2, @FVertexBuffer);
end;

destructor TVertexObject.Destroy;
begin
  glxBindBuffer(GL_ARRAY_BUFFER, 0);
  glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glxDeleteBuffers(2, @FVertexBuffer);
  inherited Destroy;
end;

procedure TVertexObject.Draw;
var
  I: Integer;
begin
  if Length(FIndexes) = 0 then Exit;
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  case FMode of
    vmBuffer:
      begin
        glxBindBuffer(GL_ARRAY_BUFFER, FVertexBuffer);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), Pointer(SizeOf(TVertexPoint)));
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), nil);
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
        glDrawElements(GL_TRIANGLES, Length(FIndexes) * 3, GL_UNSIGNED_INT, nil);
      end;
    vmArray:
      begin
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glxBindBuffer(GL_ARRAY_BUFFER, 0);
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FVertices);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), @FVertices[0].Normal);
        glDrawElements(GL_TRIANGLES, Length(FIndexes) * 3, GL_UNSIGNED_INT, FIndexes);
      end;
    vmImmediate:
      begin
        glBegin(GL_TRIANGLES);
        for I := Low(FIndexes) to High(FIndexes) do
        begin
          with FVertices[FIndexes[I].V0] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FVertices[FIndexes[I].V1] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FVertices[FIndexes[I].V2] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
        end;
        glEnd;
      end;
  end;
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_VERTEX_ARRAY);
end;

procedure TVertexObject.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TVertexObject.LoadFromStream(Stream: TStream);

  procedure ComputeNormal(const I: TTriangleIndex);
  var
    A, B, Normal: TVertexPoint;
  begin
    A.X := FVertices[I.V2].X - FVertices[I.V1].X;
    A.Y := FVertices[I.V2].Y - FVertices[I.V1].Y;
    A.Z := FVertices[I.V2].Z - FVertices[I.V1].Z;
    B.X := FVertices[I.V0].X - FVertices[I.V1].X;
    B.Y := FVertices[I.V0].Y - FVertices[I.V1].Y;
    B.Z := FVertices[I.V0].Z - FVertices[I.V1].Z;
    with Normal do
    begin
      X := A.Y * B.Z - A.Z * B.Y;
      Y := A.Z * B.X - A.X * B.Z;
      Z := A.X * B.Y - A.Y * B.X;
    end;
    FVertices[I.V0].Normal := VertexAdd(FVertices[I.V0].Normal, Normal);
    FVertices[I.V1].Normal := VertexAdd(FVertices[I.V1].Normal, Normal);
    FVertices[I.V2].Normal := VertexAdd(FVertices[I.V2].Normal, Normal);
  end;

  procedure Normalize;
  var
    Normal: TVertexPoint;
    Ratio: Single;
    I: Integer;
  begin
    for I := Low(FVertices) to High(FVertices) do
    begin
      Normal := FVertices[I].Normal;
      with Normal do
      begin
        Ratio := Sqrt(X * X + Y * Y + Z * Z);
        if Ratio > 0 then
        begin
          X := X / Ratio;
          Y := Y / Ratio;
          Z := Z / Ratio;
        end;
      end;
      FVertices[I].Normal := Normal;
    end;
  end;

var
  I: Integer;
begin
  SetLength(FVertices, FindSection(Stream, 'FORM/LWOB/PNTS') div SizeOf(TVertexPoint));
  for I := Low(FVertices) to High(FVertices) do
  begin
    FVertices[I].X := ReadFloat(Stream);
    FVertices[I].Y := ReadFloat(Stream);
    FVertices[I].Z := ReadFloat(Stream);
    FVertices[I].Normal := StockVertex;
  end;
  SetLength(FIndexes, FindSection(Stream, 'FORM/LWOB/POLS') div 10);
  for I := Low(FIndexes) to High(FIndexes) do
  begin
    ReadUint2(Stream);
    FIndexes[I].V0 := ReadUint2(Stream);
    FIndexes[I].V1 := ReadUint2(Stream);
    FIndexes[I].V2 := ReadUint2(Stream);
    ComputeNormal(FIndexes[I]);
    ReadUint2(Stream);
  end;
  Normalize;
  if GL_ARB_VERTEX_BUFFER_OBJECT then
  begin
    glxBindBuffer(GL_ARRAY_BUFFER, FVertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(TVertex) * Length(FVertices), FVertices, GL_STATIC_DRAW);
    glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TTriangleIndex) * Length(FIndexes), FIndexes, GL_STATIC_DRAW);
  end;
end;

procedure TVertexObject.SetMode(Value: TVertexMode);
begin
  if (Value = vmBuffer) and (not GL_ARB_VERTEX_BUFFER_OBJECT) then
    Value := vmArray;
  FMode := Value;
end;

function TVertexObject.GetTriangleCount: Integer;
begin
  Result := Length(FIndexes) div 3;
end;

function TVertexObject.GetVertexCount: Integer;
begin
  Result := Length(FVertices);
end;

{ TGraphicForm }

procedure TGraphicForm.InitDisplayInfo(var Info: TDisplayInfo);
begin
  with Info do
  begin
    Exclusive := Application.Exclusive;
    Width := 640;
    Height := 480;
    ColorDepth := cdTrue;
    Prompt := False;
    Caption := 'OpenGL';
  end;
end;

destructor TGraphicForm.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TGraphicForm.InitializeLayout;
begin
  if FRC <> 0 then
  begin
    glEnable(GL_CULL_FACE);
    glShadeModel(GL_SMOOTH);
    glClearColor(0.0, 0.0, 0.0, 1.0);
    glxAlphaMask(1.0);
    glxColor4f(1.0, 1.0, 1.0, 1.0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(60, 4 / 3, 0.5, 1000);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
  end;
  FTime := GetTicks;
  CreateTimer(TStandardTimer);
end;

procedure TGraphicForm.FinalizeLayout;
begin
end;

procedure TGraphicForm.CheckKeys;
begin
  with Application do
  begin
    Terminated := Keyboard.Key[EscapeCode] or Terminated;
    if Terminated then
      Hide
    else if Active then
      Exclusive := Keyboard.Toggled[FullscreenCode] or FTopLevel;
  end;
end;

procedure TGraphicForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    ClassStyle := ClassStyle or CS_OWNDC;
    Position := CreateWindowPosition(0, 0, 600, 450);
  end;
end;

procedure TGraphicForm.CreateResourceContext;

  procedure HandleError(E: Exception);
  begin
    if E <> nil then
      MessageDialog(E.Message, mtError, mbOk)
    else
      MessageDialog('Rendering context failed to initialize.', mtError, mbOk);
    DestroyResourceContext;
  end;

begin
  if FRC = 0 then
  begin
    FRC := wglCreateContext(FDC);
    wglMakeCurrent(FDC, FRC);
    try
      InitializeLayout;
      with ClientRect do
        Resize(Right - Left, Bottom - Top);
    except
      on E: Exception do HandleError(E);
    end;
  end;
end;

procedure TGraphicForm.DestroyResourceContext;
begin
  if FRC <> 0 then
  begin
    FinalizeLayout;
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
    FRC := 0;
  end;
end;

procedure TGraphicForm.CreateTimer(TimerClass: TTimerClass);
begin
  FTimer.Free;
  FTimer := TimerClass.Create;
end;

procedure TGraphicForm.Draw;
var
  I: Cardinal;
begin
  glxClearErrors;
  if FTimer <> nil then FTimer.Calculate;
  Inc(FFrame);
  if FFrame = 10 then
  begin
    FFramesPerSecond := 0;
    I := GetTicks;
    if I = FFrameTime then
      FFramesPerSecond := 9999
    else
      FFramesPerSecond := 10000 / (I - FFrameTime);
    FFrameTime := I;
    if FRC <> 0 then
	    Paint;
    FFrame := 0;
  end
  else
    Paint;
  CheckKeys;
end;

procedure TGraphicForm.Paint;
begin
  if FRC <> 0 then
  begin
    glClear(GL_COLOR_BUFFER_BIT);
    SwapBuffers(DC);
  end;
end;

procedure TGraphicForm.Resize(Width, Height: Integer);
begin
  if FRC <> 0 then
    glViewport(0, 0, Width, Height);
  Application.Keyboard.Toggled[VK_F1] := Width = Application.Width; // !
end;

procedure TGraphicForm.WMCreate(var Message: TWMCreate);
var
  Descriptor: TPixelFormatDescriptor;
  Format: Integer;
begin
  inherited;
  FDC := GetDC(Handle);
  FillChar(Descriptor, SizeOf(TPixelFormatDescriptor), #0);
  with Descriptor do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 32;
    cDepthBits := 16;
    cStencilBits := 1;
    iLayerType := PFD_MAIN_PLANE;
  end;
  Format := ChoosePixelFormat(FDC, @Descriptor);
  SetPixelFormat(FDC, Format, @Descriptor);
  CreateResourceContext;
  if  FRC = 0 then
    Message.Result := -1;
end;

procedure TGraphicForm.WMDestroy(var Message: TWMDestroy);
begin
  DestroyResourceContext;
  ReleaseDC(Handle, FDC);
  inherited;
end;

procedure TGraphicForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 0;
end;

procedure TGraphicForm.WMPaint(var Message: TWMPaint);
begin
  ValidateRect(Handle, nil);
  Message.Result := 0;
end;

procedure TGraphicForm.WMSetCursor(var Message: TWMSetCursor);
begin
  if Message.HitTest = HTCLIENT then
  begin
    SetCursor(0);
    Message.Result := 0;
  end
  else
    inherited;
end;

{ TSceneObject }

constructor TSceneObject.Create;
begin
  inherited Create;
end;

{ TSkybox }

function TSkybox.GetPanel(Panel: TSkyboxPanel): GLuint;
begin
  Result := FPanels[Panel];
end;

procedure TSkybox.SetPanel(Panel: TSkyboxPanel; Value: GLuint);
begin
  FPanels[Panel] := Value;
  if Value <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, Value);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end;
end;

procedure TSkybox.Draw;
var
  X, Y, Z: Single;
begin
  if not FActive then Exit;
  glClear(GL_DEPTH_BUFFER_BIT);
  if Scene <> nil then
  begin
    X := Scene.Camera.X;
    Y := Scene.Camera.Y;
    Z := Scene.Camera.Z;
  end
  else
  begin
    X := 0;
    Y := 0;
    Z := 0;
  end;
  glxColor4f(1, 1, 1, 1);
  if FPanels[spFront] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spFront]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(X - 2, Y + 2, Z - 2);
    glTexCoord(0, 1);
    glVertex3f(X - 2, Y - 2, Z - 2);
    glTexCoord(1, 1);
    glVertex3f(X + 2, Y - 2, Z - 2);
    glTexCoord(1, 0);
    glVertex3f(X + 2, Y + 2, Z - 2);
    glEnd;
  end;
  if FPanels[spRight] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spRight]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(X - 2, Y + 2, Z + 2);
    glTexCoord(0, 1);
    glVertex3f(X - 2, Y - 2, Z + 2);
    glTexCoord(1, 1);
    glVertex3f(X - 2, Y - 2, Z - 2);
    glTexCoord(1, 0);
    glVertex3f(X - 2, Y + 2, Z - 2);
    glEnd;
  end;
  if FPanels[spLeft] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spLeft]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(X + 2, Y + 2, Z - 2);
    glTexCoord(0, 1);
    glVertex3f(X + 2, Y - 2, Z - 2);
    glTexCoord(1, 1);
    glVertex3f(X + 2, Y - 2, Z + 2);
    glTexCoord(1, 0);
    glVertex3f(X + 2, Y + 2, Z + 2);
    glEnd;
  end;
  if FPanels[spTop] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spTop]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(-2 + X, 2 + Y, 2 + Z);
    glTexCoord(0, 1);
    glVertex3f(-2 + X, 2 + Y, -2 + Z);
    glTexCoord(1, 1);
    glVertex3f(2 + X, 2 + Y, -2 + Z);
    glTexCoord(1, 0);
    glVertex3f(2 + X, 2 + Y, 2 + Z);
    glEnd;
  end;
  if FPanels[spBottom] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spBottom]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(X - 2, Y - 2, Z - 2);
    glTexCoord(0, 1);
    glVertex3f(X - 2, Y - 2, Z + 2);
    glTexCoord(1, 1);
    glVertex3f(X + 2, Y - 2, Z + 2);
    glTexCoord(1, 0);
    glVertex3f(X + 2, Y - 2, Z - 2);
    glEnd;
  end;
  if FPanels[spBack] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, FPanels[spBack]);
    glBegin(GL_QUADS);
    glTexCoord(0, 0);
    glVertex3f(X + 2, Y + 2, Z + 2);
    glTexCoord(0, 1);
    glVertex3f(X + 2, Y - 2, Z + 2);
    glTexCoord(1, 1);
    glVertex3f(X - 2, Y - 2, Z + 2);
    glTexCoord(1, 0);
    glVertex3f(X - 2, Y + 2, Z + 2);
    glEnd;
  end;
  glClear(GL_DEPTH_BUFFER_BIT);
end;

{ TCamera }

constructor TCamera.Create;
begin
  inherited Create;
  FChanged := True;
  FAspectRatio := 1;
  ViewAngle := 80;
end;

procedure TCamera.Change;
begin
  FChanged := True;
end;

procedure TCamera.Move(const Direction: TDirection; Distance: Single);
var
  P: TVertexPoint;
begin
  P := StockVertex;
  P.Z := Distance;
  FPosition := VertexAdd(VertexRotate(P, Direction), FPosition);
  Change;
end;

function TCamera.RelativeDirection(X, Y, Z: Single): TDirection;
var
  P: TFloatPoint;
begin
  X := X - FPosition.X;
  Y := Y - FPosition.Y;
  Z := Z - FPosition.Z;
  P.X := X;
  P.Y := Z;
  Result.Heading := PointAngle(P);
  P.X := Y;
  P.Y := Z;
  Result.Pitch := PointAngle(P);
  Result.Roll := 0;
end;

function TCamera.RelativeDirection(const V: TVertexPoint): TDirection;
begin
  Result := RelativeDirection(V.X, V.Y, V.Z);
end;

procedure TCamera.Setup;

  function Square(A: Single): Single;
  begin
    Result := A * A;
  end;

var
  P1: TFloatPoint;
begin
  if FChanged then
  begin
    FViewVector := StockVertex;
    FViewVector.Z := -1;
    if FTarget <> nil then
    begin
      P1.X := FTarget.Y - FPosition.Y;
      P1.Y := -Sqrt(Square(FTarget.X - FPosition.X) + Square(FTarget.Z - FPosition.Z));
      FDirection.Pitch := PointAngle(P1);
      P1.X := FTarget.X - FPosition.X;
      P1.Y := FTarget.Z - FPosition.Z;
      FDirection.Heading := PointAngle(P1);
    end;
    FViewVector := VertexRotate(FViewVector, FDirection);
    FReferenceVertex := VertexAdd(FViewVector, FPosition);
    FNormal := StockVertex;
    FNormal.Y := 1;
    FNormal := VertexRotate(FNormal, FDirection);
    FViewPlane[0] := VertexRotate(StockPlane[0], FDirection);
    FViewPlane[1] := VertexRotate(StockPlane[1], FDirection);
    FViewPlane[2].X := -FViewPlane[0].X;
    FViewPlane[2].Y := -FViewPlane[0].Y;
    FViewPlane[2].Z := -FViewPlane[0].Z;
    FViewPlane[3].X := -FViewPlane[1].X;
    FViewPlane[3].Y := -FViewPlane[1].Y;
    FViewPlane[3].Z := -FViewPlane[1].Z;
  end;
  with FPosition do
    gluLookAt(X, Y, Z, FReferenceVertex.X, FReferenceVertex.Y, FReferenceVertex.Z,
      FNormal.X, FNormal.Y, FNormal.Z);
  if (Scene <> nil) and (Scene.Skybox <> nil) then Scene.Skybox.Draw;
end;

function TCamera.GetPosition(Index: Integer): Single;
begin
  case Index of
    0: Result := FPosition.X;
    1: Result := FPosition.Y;
  else
    Result := FPosition.Z;
  end;
end;

procedure TCamera.SetPosition(Index: Integer; Value: Single);
var
  Field: PSingle;
begin
  case Index of
    0: Field := @FPosition.X;
    1: Field := @FPosition.Y;
  else
    Field := @FPosition.Z;
  end;
  if Value <> Field^ then
  begin
    Field^ := Value;
    Change;
  end;
end;

function TCamera.GetDirection(Index: Integer): Single;
begin
  case Index of
    3: Result := FDirection.Heading;
    4: Result := FDirection.Pitch;
  else
    Result := FDirection.Roll;
  end;
end;

procedure TCamera.SetDirection(Index: Integer; Value: Single);
var
  Field: PSingle;
begin
  case Index of
    3: Field := @FDirection.Heading;
    4: Field := @FDirection.Pitch;
  else
    Field := @FDirection.Roll;
  end;
  if Value <> Field^ then
  begin
    Field^ := Value;
    Change;
  end;
end;

procedure TCamera.SetViewAngle(Value: Single);
var
  Mode: GLint;
begin
  FViewAngle := Value;
  glGetIntegerv(GL_MATRIX_MODE, @Mode);
  if Mode <> GL_PROJECTION then glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(Value, FAspectRatio, 0.1, 1000);
  if Mode <> GL_PROJECTION then glMatrixMode(Mode);
end;

{ TShooterCamera }

procedure TShooterCamera.Setup;
var
  Window: TWindow;
  Rect: TRect;
  Center: TPoint;
  Point: TPoint;
  Dir: TDirection;
  Dist: Single;
  Head: Single;
begin
  if (FTarget = nil) and Application.Active then
  begin
    Window := Application.Window;
    Rect := Window.ClientRect;
    Center.X := (Rect.Right - Rect.Left) div 2;
    Center.Y := (Rect.Bottom - Rect.Top) div 2;
    Point := Application.Window.ScreenToClient(Application.Mouse.Position);
    with Window.ClientToScreen(Center) do SetCursorPos(X, Y);
    if Point.X <> Center.X then
      Heading := Heading + (Point.X - Center.X) / 10;
    if Point.Y <> Center.Y - 1 then
      Pitch := Pitch - (Point.Y - Center.Y) / 10;
    with Application.Keyboard do
      if Key[VK_W] or Key[VK_A] or Key[VK_S] or Key[VK_D] then
      begin
        Head := 0;
        Dist := -Speed * Scene.Timer.Interval;
        if Key[VK_W] and Key[VK_A] then
          Head := -45
        else if Key[VK_W] and Key[VK_D] then
          Head := 45
        else if Key[VK_S] and Key[VK_A] then
          Head := -135
        else if Key[VK_S] and Key[VK_D] then
          Head := 135
        else if Key[VK_A] then
          Head := -90
        else if Key[VK_D] then
          Head := 90
        else if Key[VK_S] then
          Dist := -Dist;
        Dir := Direction;
        Dir.Heading := Dir.Heading + Head;
        Move(Dir, Dist);
      end;
  end;
  inherited Setup;
end;

{ TModel }

procedure TModel.Clear;
begin
  FVertices := nil;
  FPolygons := nil;
end;

procedure TModel.Draw;
var
  LastVertexCount: Integer;
  I, J: Integer;
begin
  LastVertexCount := 0;
  for I := 0 to Length(FPolygons) - 1 do
    with FPolygons[I] do
    begin
      if LastVertexCount <> VertexCount then
      begin
        if LastVertexCount > 0 then glEnd;
        case VertexCount of
          3: glBegin(GL_TRIANGLES);
          4: glBegin(GL_QUADS);
        end;
        LastVertexCount := VertexCount;
      end;
      if Smooth then
        for J := 0 to VertexCount - 1 do
        begin
          with Vertices[J].Normal do glNormal3f(X, Y, Z);
          with Vertices[J].Point do glVertex3f(X, Y, Z);
        end
      else
      begin
        with FPolygons[I].Normal do glNormal3f(X, Y, Z);
         for J := 0 to VertexCount - 1 do
           with Vertices[J].Point do glVertex3f(X, Y, Z);
      end;
    end;
  if LastVertexCount > 0 then glEnd;
end;

{procedure TModel.DrawNormals;
var
  I: Integer;
begin
  glBegin(GL_LINES);
  for I := 0 to Length(FPolygons) - 1 do
    DrawNormal(FPolygons[I]);
  glEnd;
end;}

procedure TModel.LoadFromFile(const FileName: string);

  procedure LoadDrawingExchangeFile;
  var
    Input: TStrings;
    PolygonIndex: Integer;
    Reading: boolean;
    S: string;
    I: Integer;
  begin
    Input := TStringList.Create;
    try
      Input.LoadFromFile(FileName);
      PolygonIndex := 0;
      for I := 0 to Input.Count - 1 do
      begin
        S := Trim(Input[I]);
        if S = '3DFACE' then Inc(PolygonIndex);
      end;
      if PolygonIndex > 0 then
      begin
        SetLength(FPolygons, PolygonIndex);
        FillChar(FPolygons[0], PolygonIndex * SizeOf(TPolygon), #0);
      end
      else
        Input.Clear;
      PolygonIndex := -1;
      Reading := False;
      I := 0;
      while I < Input.Count do
      begin
        S := Trim(Input[I]);
        if S = '3DFACE' then
        begin
          if Reading and (FPolygons[PolygonIndex].VertexCount < 2) then
            raise Exception.Create('Invalid drawing format');
          Reading := True;
          Inc(PolygonIndex);
          Inc(I, 6);
        end
        else if Reading and (S[1] in ['+', '-', '0'..'9']) then
        begin
          with FPolygons[PolygonIndex] do
          begin
            if VertexCount > 3 then
              raise Exception.Create('Invalid drawing format');
            Vertices[VertexCount].X := StrToFloat(Input[I]);
            Vertices[VertexCount].Y := StrToFloat(Input[I + 2]);
            Vertices[VertexCount].Z := StrToFloat(Input[I + 4]);
            Inc(VertexCount);
          end;
          Inc(I, 6);
        end
        else
        begin
          if Reading and (FPolygons[PolygonIndex].VertexCount < 2) then
            raise Exception.Create('Invalid drawing format');
          Reading := False;
          Inc(I);
        end;
      end;
      UpdateNormals;
    finally
      Input.Free;
    end;
  end;

  procedure LoadLightwaveFile;
  var
    Input: TStream;
    Count: Integer;
    Size: Integer;
    I: Integer;
  begin
    Input := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(FVertices, FindSection(Input, 'FORM/LWOB/PNTS') div 12);
      for I := 0 to Length(FVertices) - 1 do
      begin
        Input.ReadBuffer(FVertices[I], SizeOf(TVertexPoint));
        SwapBufferLong(@FVertices[I], 3);
      end;
      Count := 0;
      Size := FindSection(Input, 'FORM/LWOB/POLS');
      while Size > 0 do
      begin
        I := ReadUint2(Input);
        I := I * SizeOf(Word) + SizeOf(Word);
        Input.Seek(I, soFromCurrent);
        Dec(Size, I + SizeOf(Word));
        Inc(Count);
      end;
      SetLength(FPolygons, Count);
      FindSection(Input, 'FORM/LWOB/POLS');
      for Count := 0 to Length(FPolygons) - 1 do
        with FPolygons[Count] do
        begin
          VertexCount := ReadUint2(Input);
          if (VertexCount < 3) or (VertexCount > 4) then
            raise Exception.Create('Invalid lightwave format');
          for I := 0 to VertexCount - 1 do
             Vertices[I] := @FVertices[ReadUint2(Input)];
          ReadUint2(Input); // Unused surface index
        end;
      UpdateNormals;
    finally
      Input.Free;
    end;
  end;

var
  Stream: TStream;
  S: string;
begin
  Clear;
  S := UpperCase(ExtractFileExt(FileName));
  try
    if S = '.DXF' then
      LoadDrawingExchangeFile
    else if S = '.LWO' then
      LoadLightwaveFile
    else
    begin
      Stream := TFileStream.Create(FileName, fmOpenRead);
      try
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  except
    Clear;
    raise;
  end;
end;

procedure TModel.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure StreamCheck(StreamResult: Boolean);
begin
  if not StreamResult then raise Exception.Create('Stream read/write error');
end;

function ReadBool(Stream: TStream): Boolean;
begin
  StreamCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
end;

function ReadInteger(Stream: TStream): Integer;
begin
  StreamCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
end;

function ReadString(Stream: TStream): string;
var
  I: Integer;
begin
  Result := '';
  I := ReadInteger(Stream);
  if I > 0 then
  begin
    SetLength(Result, I);
    StreamCheck(Stream.Read(PChar(Result)^, I) = I);
  end;
end;

function ReadFloat(Stream: TStream): Single;
begin
  StreamCheck(Stream.Read(Result, SizeOf(Result)) = SizeOf(Result));
end;

procedure ReadBuffer(Stream: TStream; var Buffer; Size: Integer);
begin
  StreamCheck(Stream.Read(Buffer, Size) = Size);
end;

procedure WriteBool(Stream: TStream; Value: Boolean);
begin
  StreamCheck(Stream.Write(Value, SizeOf(Value)) = SizeOf(Value));
end;

procedure WriteInteger(Stream: TStream; Value: Integer);
begin
  StreamCheck(Stream.Write(Value, SizeOf(Value)) = SizeOf(Value));
end;

procedure WriteString(Stream: TStream; const Value: string);
var
  I: Integer;
begin
  if Value <> '' then
  begin
    I := Length(Value);
    WriteInteger(Stream, I);
    StreamCheck(Stream.Write(PChar(Value)^, I) = I);
  end
  else
    WriteInteger(Stream, 0);
end;

procedure WriteFloat(Stream: TStream; Value: Single);
begin
  StreamCheck(Stream.Write(Value, SizeOf(Value)) = SizeOf(Value));
end;

procedure WriteBuffer(Stream: TStream; const Buffer; Size: Integer);
begin
  StreamCheck(Stream.Write(Buffer, Size) = Size);
end;

const
  SModelFile = '{3E4EDE0C-8563-4A0D-8360-59E67A203C20}';

procedure TModel.LoadFromStream(Stream: TStream);
var
  Count: Integer;
  Size: Integer;
  I: Integer;
begin
  SetLength(FVertices, FindSection(Stream, 'FORM/LWOB/PNTS') div 12);
  for I := 0 to Length(FVertices) - 1 do
  begin
    Stream.ReadBuffer(FVertices[I], SizeOf(TVertexPoint));
    SwapBufferLong(@FVertices[I], 3);
  end;
  Count := 0;
  Size := FindSection(Stream, 'FORM/LWOB/POLS');
  while Size > 0 do
  begin
    I := ReadUint2(Stream);
    I := I * SizeOf(Word) + SizeOf(Word);
    Stream.Seek(I, soFromCurrent);
    Dec(Size, I + SizeOf(Word));
    Inc(Count);
  end;
  SetLength(FPolygons, Count);
  FindSection(Stream, 'FORM/LWOB/POLS');
  for Count := 0 to Length(FPolygons) - 1 do
    with FPolygons[Count] do
    begin
      VertexCount := ReadUint2(Stream);
      if (VertexCount < 3) or (VertexCount > 4) then
        raise Exception.Create('Invalid lightwave format');
      for I := 0 to VertexCount - 1 do
         Vertices[I] := @FVertices[ReadUint2(Stream)];
      ReadUint2(Stream); // Unused surface index
    end;
  UpdateNormals;
end;

procedure TModel.SaveToStream(Stream: TStream);
begin
end;

procedure TModel.UpdateNormals;
var
  V: PVertex;
  A, B, C: Integer;
  Ratio: Single;
  I: Integer;
begin
  for I := 0 to Length(FPolygons) - 1 do
    FPolygons[I].Normal := PolygonNormal(FPolygons[I]);
  for I := 0 to Length(FVertices) - 1 do
  begin
    V := @FVertices[I];
    V.Normal := StockVertex;
    C := 0;
    for A := 0 to Length(FPolygons) - 1 do
      for B := 0 to FPolygons[A].VertexCount - 1 do
        if Cardinal(FPolygons[A].Vertices[B]) = Cardinal(V) then
          with V.Normal do
          begin
            X := X + FPolygons[A].Normal.X;
            Y := Y + FPolygons[A].Normal.Y;
            Z := Z + FPolygons[A].Normal.Z;
            Inc(C);
            Break;
          end;
    if C > 0 then
      with V.Normal do
      begin
        Ratio := Sqrt(X * X + Y * Y + Z * Z);
        X := X / Ratio;
        Y := Y / Ratio;
        Z := Z / Ratio;
        { X := X / C; // !
        Y := Y / C;
        Z := Z / C; }
      end;
  end;
end;

function TModel.GetPolygon(Index: Integer): PPolygon;
begin
  Result := @FPolygons[Index];
end;

function TModel.GetPolygonCount: Integer;
begin
  Result := Length(FPolygons);
end;

{ TModelGroup }

destructor TModelGroup.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FModels) - 1 do
    FModels[I].Free;
  inherited Destroy;
end;

procedure TModelGroup.Draw;
var
  I: Integer;
begin
  for I := 0 to Length(FModels) -1 do
    FModels[I].Draw;
end;

function TModelGroup.GetModel(Index: Integer): TModel;
begin
  Result := FModels[Index];
end;

function TModelGroup.GetModelCount: Integer;
begin
  Result := Length(FModels);
end;

{ TResourceManager }

constructor TResourceManager.Create(Storage: TFolderStructure);
begin
  inherited Create;
  FStorage := Storage;
end;

destructor TModelManager.Destroy;
var
  A, B: PModelLink;
begin
  A := FModels;
  while A <> nil do
  begin
    B := A.Next;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Destroying ' + A.Folder);
    {$ENDIF}
    A.ModelGroup.Free;
    Dispose(A);
    A := B;
  end;
  inherited Destroy;
end;

function TModelManager.CreateLink(Ident: string): PModelLink;
var
  Folder: TFolderStructure;
  Stream: TStreamStructure;
  Group: TModelGroup;
  Count: Integer;
  Index: Integer;
  I: Integer;
begin
  Result := FindLink(Ident);
  if Result <> nil then Exit;
  try
    New(Result);
    Result.Folder := Ident;
    Result.ModelGroup := nil;
    Result.Next := nil;
    Folder := Storage;
    Count := FieldCount(Ident, '\');
    for I := 0 to Count - 1 do
      Folder := Folder.OpenFolder(FieldValue(Ident, '\', I), False);
    Folder.Open;
    Count := 0;
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TStreamStructure then
        Inc(Count);
    if Count = 0 then
    begin
      Dispose(Result);
      Result := nil;
      Exit;
    end;
    Group := TModelGroup.Create;
    Group.Name := Ident;
    SetLength(Group.FModels, Count);
    Result.ModelGroup := Group;
    Index := 0;
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TStreamStructure then
      begin
        Stream := Folder[I] as TStreamStructure;
        Group.FModels[Index] := TModel.Create;
        Group.FModels[Index].Name := Stream.Name;
        Group.FModels[Index].LoadFromStream(Stream.AsStream);
        Inc(Index);
      end;
    if FModels = nil then
      FModels := Result
    else
    begin
      Result.Next := FModels;
      repeat
        if Result.Next.Next = nil then
        begin
          Result.Next.Next := Result;
          Result.Next := nil;
        end
        else
          Result.Next := Result.Next.Next;
      until Result.Next = nil;
    end;
  except
    Result.ModelGroup.Free;
    Dispose(Result);
    raise;
  end
end;

procedure TModelManager.DestroyLink(Link: PModelLink);
var
  A, B: PModelLink;
begin
  if Link = nil then Exit;
  Link.ModelGroup.Free;
  Dispose(Link);
  A := FModels;
  B := nil;
  while A <> nil do
    if A = Link then
      if B = nil then
        FModels := FModels.Next
      else
      begin
        B.Next := A.Next;
        A := nil;
      end
    else
    begin
      B := A;
      A := A.Next;
    end;
  Dispose(Link);
end;

function TModelManager.FindLink(const Ident: string): PModelLink;
var
  Link: PModelLink;
begin
  Link := FModels;
  while Link <> nil do
    if Link.Folder = Ident then
    begin
      Result := Link;
      Exit;
    end
    else
      Link := Link.Next;
  Result := nil;
end;

procedure TModelManager.Open(const Ident: string);
var
  Link: PModelLink;
begin
  Link := CreateLink(Ident);
  if Link = nil then
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Failed to open ' + Ident, Link = nil);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Opened ' + Ident);
    {$ENDIF}
  end
end;

procedure TModelManager.Close(const Ident: string);
var
  Link: PModelLink;
begin
  Link := FindLink(Ident);
  if Link <> nil then
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Closed ' + Link.Folder);
    {$ENDIF}
    DestroyLink(Link);
  end
 {$IFDEF DEBUGSTRINGS}
  else
    AddDebugText('Failed to close ' + Ident);
 {$ENDIF}
end;

function TModelManager.FindModel(const Ident: string): TModelGroup;
var
  Link: PModelLink;
begin
  Link := FindLink(Ident);
  if Link <> nil then
    Result := Link.ModelGroup
  else
  begin
   {$IFDEF DEBUGSTRINGS}
    AddDebugText('Failed to find ' + Ident);
   {$ENDIF}
    Result := nil;
  end;
end;

{ TTextureManager }

destructor TTextureManager.Destroy;
var
  A, B: PTextureLink;
  I: Integer;
begin
  A := FTextures;
  while A <> nil do
  begin
    B := A.Next;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Destroying ' + A.Folder);
    {$ENDIF}
    if A.Opened then
    begin
      I := Length(A.Textures);
      if I > 0 then glDeleteTextures(I, @A.Textures[0]);
    end;
    if A.Locked then
      for I := Low(A.Data) to High(A.Data) do
        DisposeTexture(A.Data[I].Info);
    Dispose(A);
    A := B;
  end;
  inherited Destroy;
end;

function TTextureManager.CreateLink(Ident: string): PTextureLink;
var
  Folder: TFolderStructure;
  Stream: TStreamStructure;
  Count: Integer;
  Index: Integer;
  I: Integer;
begin
  Result := FindLink(Ident);
  if Result <> nil then Exit;
  try
    New(Result);
    Result.Folder := Ident;
    Result.Opened := False;
    Result.Locked := False;
    Result.Textures := nil;
    Result.Data := nil;
    Result.Next := nil;
    Folder := Storage;
    Count := FieldCount(Ident, '\');
    for I := 0 to Count - 1 do
      Folder := Folder.OpenFolder(FieldValue(Ident, '\', I), False);
    Folder.Open;
    Count := 0;
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TStreamStructure then
        Inc(Count);
    if Count = 0 then
    begin
      Dispose(Result);
      Result := nil;
      Exit;
    end;
    SetLength(Result.Data, Count);
    Index := 0;
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TStreamStructure then
      begin
        Stream := Folder[I] as TStreamStructure;
        Result.Data[Index].Name := Stream.Name;
        Result.Data[Index].Info := LoadTexture(Stream.AsStream, FTextureFormat);
        {$IFDEF DEBUGSTRINGS}
        AddDebugText('Failed to load ' + Ident + '\' + Stream.Name,
          Result.Data[Index].Info.Bits = nil);
        {$ENDIF}
        Inc(Index);
      end;
    if FTextures = nil then
      FTextures := Result
    else
    begin
      Result.Next := FTextures;
      repeat
        if Result.Next.Next = nil then
        begin
          Result.Next.Next := Result;
          Result.Next := nil;
        end
        else
          Result.Next := Result.Next.Next;
      until Result.Next = nil;
    end;
  except
    for I := Low(Result.Data) to High(Result.Data) do
      DisposeTexture(Result.Data[I].Info);
    Dispose(Result);
    raise;
  end
end;

procedure TTextureManager.DestroyLink(Link: PTextureLink);
var
  A, B: PTextureLink;
  I: Integer;
begin
  if Link = nil then Exit;
  if not Link.Opened then
    glDeleteTextures(Length(Link.Textures), @Link.Textures[0]);
  if not Link.Locked then
    for I := Low(Link.Data) to High(Link.Data) do
    begin
      DisposeTexture(Link.Data[I].Info);
      Link.Data[I].Info.Bits := nil;
    end;
  if not (Link.Opened or Link.Locked) then
  begin
    A := FTextures;
    B := nil;
    while A <> nil do
      if A = Link then
        if B = nil then
          FTextures := FTextures.Next
        else
        begin
          B.Next := A.Next;
          A := nil;
        end
      else
      begin
        B := A;
        A := A.Next;
      end;
    Dispose(Link);
  end;
end;

procedure TTextureManager.Open(const Ident: string);
var
  Link: PTextureLink;
  Count: Integer;
  I: Integer;
begin
  Link := CreateLink(Ident);
  if (Link = nil) or (Link.Opened) then
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Failed to open ' + Ident, Link = nil);
    {$ENDIF}
    Exit;
  end;
  Count := Length(Link.Data);
  SetLength(Link.Textures, Count);
  if Count > 0 then
  begin
    glGenTextures(Count, @Link.Textures[0]);
    for I := Low(Link.Data) to High(Link.Data) do
      {$IFDEF DEBUGSTRINGS}
      if not BindTexture(Link.Data[I].Info, Link.Textures[I]) then
        AddDebugText('Failed to bind ' + Ident + '\' + Link.Data[I].Name);
      {$ELSE}
      BindTexture(Link.Data[I].Info, Link.Textures[I]);
      {$ENDIF}
    Link.Opened := True;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Opened ' + Ident);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('The path "' + Ident + '" contains no information');
    {$ENDIF}
  end;
  DestroyLink(Link);
end;

procedure TTextureManager.Close(const Ident: string);
var
  Link: PTextureLink;
begin
  Link := FindLink(Ident);
  if Link <> nil then
  begin
    Link.Opened := False;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Closed ' + Link.Folder);
    {$ENDIF}
    DestroyLink(Link);
  end
 {$IFDEF DEBUGSTRINGS}
  else
    AddDebugText('Failed to close ' + Ident);
 {$ENDIF}
end;

procedure TTextureManager.Lock(const Ident: string);
var
  Link: PTextureLink;
begin
  Link := CreateLink(Ident);
  if (Link = nil) or (Link.Locked) then
  begin
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Failed to lock ' + Ident, Link = nil);
    {$ENDIF}
  end
  else
  begin
    Link.Locked := True;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Locked ' + Ident);
   {$ENDIF}
  end;
end;

procedure TTextureManager.Unlock(const Ident: string);
var
  Link: PTextureLink;
begin
  Link := FindLink(Ident);
  if Link <> nil then
  begin
    Link.Locked := False;
    {$IFDEF DEBUGSTRINGS}
    AddDebugText('Unlocked ' + Link.Folder);
    {$ENDIF}
    DestroyLink(Link);
  end
 {$IFDEF DEBUGSTRINGS}
  else
    AddDebugText('Failed to unlock ' + Ident);
 {$ENDIF}
end;

function TTextureManager.FindLink(const Ident: string): PTextureLink;
var
  Link: PTextureLink;
begin
  Link := FTextures;
  while Link <> nil do
    if Link.Folder = Ident then
    begin
      Result := Link;
      Exit;
    end
    else
      Link := Link.Next;
  Result := nil;
end;

function TTextureManager.FindTexture(const Ident: string): GLuint;
var
  Path: TFieldPath;
  Link: PTextureLink;
  I: Integer;
begin
  Path := ExtractFieldPath(Ident);
  Link := FindLink(Path.Folder);
  if Link <> nil then
    for I := Low(Link.Textures) to High(Link.Textures) do
      if Link.Data[I].Name = Path.Name then
      begin
        Result := Link.Textures[I];
        Exit;
      end;
  Result := 0;
 {$IFDEF DEBUGSTRINGS}
  AddDebugText('Failed to find ' + Ident);
 {$ENDIF}
end;

function TTextureManager.FindTextures(const Ident: string): TTextures;
var
  Link: PTextureLink;
begin
  Link := FindLink(Ident);
  if Link <> nil then
    Result := Link.Textures
  {$IFDEF DEBUGSTRINGS}
  else
    AddDebugText('Failed to find ' + Ident);
  {$ENDIF}
end;

procedure TTextureManager.SelectSkybox(const Ident: string);
var
  Link: PTextureLink;
  S: string;
  I: Integer;
begin
  Link := FindLink(Ident);
  if (Link = nil) or (Scene = nil) or (Scene.Skybox = nil) then
  begin
    {$IFDEF DEBUGSTRINGS}
    Scene.DebugStrings.Add('Failed to select ' + Ident);
    {$ENDIF}
  end
  else
  begin
    Open(Ident);
    for I := 0 to Length(Link.Data) - 1 do
    begin
      S := Link.Data[I].Name;
      if S = 'front' then
        Scene.Skybox.Panels[spFront] := Link.Textures[I]
      else if S = 'right' then
        Scene.Skybox.Panels[spLeft] := Link.Textures[I]
      else if S = 'left' then
        Scene.Skybox.Panels[spRight] := Link.Textures[I]
      else if S = 'top' then
        Scene.Skybox.Panels[spTop] := Link.Textures[I]
      else if S = 'back' then
        Scene.Skybox.Panels[spBack] := Link.Textures[I]
      else if S = 'bottom' then
        Scene.Skybox.Panels[spBottom] := Link.Textures[I];
    end;
    {$IFDEF DEBUGSTRINGS}
    Scene.DebugStrings.Add('Selected ' + Ident);
    {$ENDIF}
  end;
end;

{ TSceneForm }

constructor TSceneForm.Create(Parent: HWND = 0);
begin
  inherited Create(Parent);
  Scene := Self;
  TopLevel := False;
  {$IFDEF DEBUGSTRINGS}
  FDebugStrings := TStringList.Create;
  FDebugStrings.Add('Scene created');
  {$ENDIF}
end;

destructor TSceneForm.Destroy;
begin
  {$IFDEF DEBUGSTRINGS}
  FDebugStrings.Add('Scene destroyed');
  FDebugStrings.SaveToFile('scene.log');
  FDebugStrings.Free;
  {$ENDIF}
  inherited Create;
end;

procedure TSceneForm.OpenStorage(const FileName: string);
var
  S: string;
begin
  S := FileTempName;
  DecompressFile(FileName, S);
  FStorage := TFolderStructure.Create(S);
  FModelManager := TModelManager.Create(FStorage);
  FTextureManager := TTextureManager.Create(FStorage);
end;

procedure TSceneForm.CloseStorage;
var
  S: string;
begin
  FModelManager.Free;
  FModelManager := nil;
  FTextureManager.Free;
  FTextureManager := nil;
  if FStorage <> nil then
  begin
    S := FStorage.Name;
    FStorage.Free;
    DeleteFile(S);
  end;
  FStorage := nil;
end;

procedure TSceneForm.InitDisplayInfo(var Info: TDisplayInfo);
begin
  with Info do
  begin
    Exclusive := False;
    Width := 800;
    Height := 600;
    ColorDepth := cdTrue;
    Prompt := False;
    Caption := 'OpenGL';
  end;
end;

procedure TSceneForm.InitSceneInfo(var Info: TSceneInfo);
begin
  with Info do
  begin
    if Camera = nil then Camera := TCamera.Create;
    if Skybox = nil then Skybox := TSkybox.Create;
    if Timer = nil then Timer := TStandardTimer.Create;
  end;
end;

procedure TSceneForm.InitializeLayout;
var
  Info: TSceneInfo;
{$IFDEF DEBUGSTRINGS}
  S: string;
  I: Integer;
{$ENDIF}
begin
  inherited InitializeLayout;
  glxAlphaMask(1.0);
  {$IFDEF DEBUGSTRINGS}
  FDebugStrings.Add('Driver by ' + glGetString(GL_VENDOR));
  FDebugStrings.Add('Version ' + glGetString(GL_VERSION));
  FDebugStrings.Add('Extensions');
  S := glGetString(GL_EXTENSIONS);
  if S = '' then
    FDebugStrings.Add('  none')
  else for I := 0 to FieldCount(S, ' ') - 1 do
    FDebugStrings.Add('  ' + FieldValue(S, ' ', I));
  {$ENDIF}
  FillChar(Info, SizeOf(Info), #0);
  InitSceneInfo(Info);
  with Info do
  begin
    FCamera := Camera;
    FSkybox := Skybox;
    FTextWriter := TextWriter;
    FTimer := Timer;
  end;
end;

procedure TSceneForm.FinalizeLayout;
begin
  FTimer.Free;
  FCamera.Free;
  FSkybox.Free;
  FTextWriter.Free;
  CloseStorage;
  inherited FinalizeLayout;;
end;

function TSceneForm.FindStream(const Path: string): TStream;
begin
  if FStorage <> nil then
    Result := StreamFromPath(FStorage, Path)
  else
    Result := nil;
end;

{ TFontTextWriter }

type
  TFontTextWriter = class(TTextWriter)
  protected
    procedure InitFonts(var Fonts: TBitmapFonts); override;
  end;

procedure TFontTextWriter.InitFonts(var Fonts: TBitmapFonts);
var
  Tex: GLuint;
begin
  LockTextures('textwriter');
  OpenTextures('textwriter');
  Tex := FindTexture('textwriter\font');
  glBindTexture(GL_TEXTURE_2D, Tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  SetLength(Fonts, 2);
  with Fonts[0] do
  begin
    CharWidth := 0.01375;
    CharHeight := 0.03;
    Texture := Tex;
    TextureWidth := 0.0546875;
    TextureHeight := 0.0859375;
    TextureBorder := 0.0078125;
    TextureOffsetX := 0;
    TextureOffsetY := 0;
  end;
  with Fonts[1] do
  begin
    CharWidth := 0.0125;
    CharHeight := 0.02333;
    Texture := Tex;
    TextureWidth := 0.05078125;
    TextureHeight := 0.06640625;
    TextureBorder := 0.0;
    TextureOffsetX := 0;
    TextureOffsetY := 0.515625;
  end;
  FontIndex := 0;
end;

procedure OpenModels(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.ModelManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.ModelManager.Open('models\' + FieldValue(ResName, ';', I));
end;

procedure CloseModels(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.ModelManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.ModelManager.Close('models\' + FieldValue(ResName, ';', I));
end;

function FindModel(const ResName: string): TModelGroup;
begin
  if (Scene <> nil) and (Scene.ModelManager <> nil) then
      Result := Scene.ModelManager.FindModel('models\' + ResName)
  else
    Result := nil;
end;

procedure OpenTextures(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Open('textures\' + FieldValue(ResName, ';', I));
end;

procedure CloseTextures(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Close('textures\' + FieldValue(ResName, ';', I));
end;

procedure LockTextures(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Lock('textures\' + FieldValue(ResName, ';', I));
end;

procedure UnlockTextures(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Unlock('textures\' + FieldValue(ResName, ';', I));
end;

function FindTexture(const ResName: string): GLuint;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    Result := Scene.TextureManager.FindTexture('textures\' + ResName)
  else
    Result := 0;
end;

function FindTextures(const ResName: string): TTextures;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    Result := Scene.TextureManager.FindTextures('textures\' + ResName)
  else
    Result := nil;
end;

procedure OpenSkyboxes(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Open('skyboxes\' + FieldValue(ResName, ';', I));
end;

procedure CloseSkyboxes(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Close('skyboxes\' + FieldValue(ResName, ';', I));
end;

procedure LockSkyboxes(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Lock('skyboxes\' + FieldValue(ResName, ';', I));
end;

procedure UnlockSkyboxes(const ResName: string);
var
  I: Integer;
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    for I := 0 to FieldCount(ResName, ';') - 1 do
      Scene.TextureManager.Unlock('skyboxes\' + FieldValue(ResName, ';', I));
end;

procedure SelectSkybox(const ResName: string);
begin
  if (Scene <> nil) and (Scene.TextureManager <> nil) then
    Scene.TextureManager.SelectSkybox('skyboxes\' + ResName);
end;

{ TGraphicApplication }

constructor TGraphicApplication.Create(WindowClass: TWindowClass = nil);
begin
  inherited Create(WindowClass);
  FTimer := TStandardTimer.Create;
  Exclusive := False;
end;

destructor TGraphicApplication.Destroy;
begin
  FTimer.Free;
  Window.Free;
  inherited Destroy;
end;

destructor TGraphicApplication.Run;
label
  Done;
var
  Context: THandle;
  GraphicForm: TGraphicForm;
  Time: Single;
  Msg: TMsg;

  procedure Init;
  var
    DisplayInfo: TDisplayInfo;
  begin
    FillChar(DisplayInfo, SizeOf(DisplayInfo), #0);
    GraphicForm.InitDisplayInfo(DisplayInfo);
    InitDisplay(DisplayInfo);
  end;

begin
  Context := 0;
  if Application.Window is TGraphicForm then
  begin
    try
      if UniqueInstance <> '' then
      begin
        Context := BeginUniqueInstance;
        if Context = 0 then
        begin
          Window.Free;
          goto Done;
        end;
      end;
      GraphicForm := Application.Window as TGraphicForm;
      Init;
      GraphicForm.Show;
      SetForegroundWindow(GraphicForm.Handle);
      FTimer.Calculate;
      Time := FTimer.Time;
      while not Application.Terminated do
      begin
        if FThrottleTime > 0 then
        begin
          FTimer.Calculate;
          while Time + FThrottleTime > FTimer.Time do
          begin
            Sleep(0);
            FTimer.Calculate;
          end;
          Time := FTimer.Time;
        end;
        GraphicForm.Draw;
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
          if Msg.message <> WM_QUIT then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end
          else
            Application.Terminated := True;
      end;
      if Window <> nil then
        DestroyWindow(Window.Handle);
Done:
    except
      on E: Exception do
        MessageDialog(E.Message, mtError, mbOk);
    end;
    Window.Free;
    Window := nil;
    EndUniqueInstance(Context);
  end
  else
    inherited Run;
end;

procedure TGraphicApplication.SetThrottle(Value: Integer);
begin
  if Value < 1 then
    Value := 0
  else if Value < 30 then
    Value := 30;
  FThrottle := Value;
  if FThrottle > 0 then
    FThrottleTime := 1 / FThrottle
  else
    FThrottleTime := 0;
end;

  {TWorldObject = class(TPersistent)
  private
    procedure Render; virtual;
    property Name: string
  end;

  TPlaceable = class(TWorldObject)
  private
    FChanged:
    FAnimationStart: Cardinal;
    FAnimationEnd: Cardinal;
  protected
    procedure FrozenChanged; virtual;
    property Changed: Boolean read FChanged write FChanged;
  public
    procedure Animate(const Vector: TVertexPoint; Distance: Single; Heading: TPosition; Time: Cardinal);

    property Moving: Boolean read FMoving write GetMoving;
    property Frozen: Boolean read FFrozen write SetFrozen;

    property Parent: TPlaceableObject;
    property Direction: TDirection read FDirection write SetDirection;
    property Position: TVertexPoint read FPosition write SetPosition;
    property X: Single index 0 read GetPosition write SetPosition;
    property Y: Single index 1 read GetPosition write SetPosition;
    property Z: Single index 2 read GetPosition write SetPosition;
    property Heading: Single index 3 read GetDirection write SetDirection;
    property Pitch: Single index 4 read GetDirection write SetDirection;
    property Roll: Single index 5 read GetDirection write SetDirection;
  end;

  TTargetable = class(TPlaceable)
  public
    property Target: TPlaceable read FTarget write SetTarget;
  end;

  TCamera = class(TTargetable)
  public
    procedure Render; override;
  end;

  TLight = class(TTargetable)
  end;

  TWorldCollection = class(TPersistent)
  private
  public
    destructor Destroy;
    property Count
    procedure Clear;
  end;

  TLightCollection = class(TWorldCollection)
  end;



  TWorldManager = class
  private
  public
    property Camera: TCamera
    property Lights: TLights
    property Models: TModels
    property References: TReferences
  end;

  TModelDef = class(TObject)
  private
    property Color
    property Smooth
    property Name
    property Specular
    property Glossiness
    property Diffuse
    property Ambient
    property Luminance
  end;


  TModelReference = class(TWorldObject)
  public
    property Model
  end;}

    {procedure OpenSkybox(const Folder: string);
    procedure CloseSkybox(const Folder: string);
    procedure SelectSkybox(const Name: string);
    procedure LockSkybox(const Name: string);
    procedure OpenTexturess(const Folder: string);
    procedure CloseTexturess(const Folder: string);
    procedure SelectTexture(const Name: string);}

        // function FindModel(const Name: string): TModelGroup;
    // FModels: TList;

    {property Model[const Name: string]: TModelGroup read GetModel;
    property Skybox[const Name: string]: GLuint read GetSkybox;
    property Texture[const Name: string]: GLuint read GetTexture;
    property TextureResources[const Name: string]: TResources read GetTextureResources;}

(* constructor TResourceManager.Create;

  procedure AddModels(Folder: TFolderStructure; Path: string);
  var
    ModelGroup: TModelGroup;
    Stream: TStreamStructure;
    Count: Integer;
    Index: Integer;
    S: string;
    I: Integer;
  begin
    Folder.Open;
    Count := 0;
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TStreamStructure then
        Inc(Count);
    if Count > 0 then
    begin
      {$IFDEF DEBUGSTRINGS}
      Scene.DebugStrings.Add('Reading folder ' + Path + Folder.Name);
      {$ENDIF}
      ModelGroup := TModelGroup.Create;
      ModelGroup.Name := Path + Folder.Name;
      SetLength(ModelGroup.FModels, Count);
      Index := 0;
      {$IFDEF DEBUGSTRINGS}
      S := '';
      {$ENDIF}
      for I := 0 to Folder.StructureCount - 1 do
        if Folder[I] is TStreamStructure then
        try
          Stream := Folder[I] as TStreamStructure;
          {$IFDEF DEBUGSTRINGS}
          S := S + Stream.Name + ', ';
          {$ENDIF}
          ModelGroup.FModels[Index] := TModel.Create;
          ModelGroup.FModels[Index].LoadFromStream(Stream.AsStream);
          ModelGroup.FModels[Index].Name := Stream.Name;
          Inc(Index);
        except
          ModelGroup.Free;
          raise;
        end;
      {$IFDEF DEBUGSTRINGS}
      if S <> '' then
        Scene.DebugStrings.Add('Loaded ' + Copy(S, 1, Length(S) - 2));
      {$ENDIF}
      FModels.Add(ModelGroup);
    end;
    S := Path + Folder.Name + '\';
    for I := 0 to Folder.StructureCount - 1 do
      if Folder[I] is TFolderStructure then
        AddModels(Folder[I] as TFolderStructure, S);
  end;

var
  Folder: TFolderStructure;
  I: Integer;
begin
  inherited Create;
  FModels := TList.Create;
  Folder := Scene.Storage.OpenFolder('models');
  Folder.Open;
  {$IFDEF DEBUGSTRINGS}
  Scene.DebugStrings.Add('Loading models');
  {$ENDIF}
  for I := 0 to Folder.StructureCount - 1 do
    if Folder[I] is TFolderStructure then
      AddModels(Folder[I] as TFolderStructure, '');
  {$IFDEF DEBUGSTRINGS}
  Scene.DebugStrings.Add('Completed loading models');
  {$ENDIF}
end; *)

initialization
 EscapeCode := VK_ESCAPE;
 FullscreenCode := VK_F1;
end.
