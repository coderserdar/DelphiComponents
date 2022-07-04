
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareGraphicObjs;

interface

{$I BARE.INC}

uses
  {$IFDEF BARE}BareUtils,{$ELSE}SysUtils, Classes,{$ENDIF}
  BareOpenGL, BareOpenGLExt, BareGraphics, Windows, BareInterchange;

{ Primitive drawing types }

type
  TLightAttribute = record
    Ambient: TFloatColor;
    Diffuse: TFloatColor;
    Specular: TFloatColor;
    Position: TVertexPoint;
    Scale: Single;
    UserData: Pointer;
  end;
  PLightAttribute = ^TLightAttribute;

  TLightAttributes = array of TLightAttribute;

  TSurfaceAttribute = record
    Ambient: TFloatColor;
    Diffuse: TFloatColor;
    Specular: TFloatColor;
    Emission: TFloatColor;
    Shininess: Single;
    Texture: GLuint;
    UserData: Pointer;
  end;
  PSurfaceAttribute = ^TSurfaceAttribute;

  TSurfaceAttributes = array of TSurfaceAttribute;

const
  StockLight: TLightAttribute = (
    Ambient: (Red: 0.2; Green: 0.2; Blue: 0.2; Alpha: 1.0);
    Diffuse: (Red: 0.8; Green: 0.8; Blue: 0.8; Alpha: 1.0);
    Specular: (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);
    Position: (X: 10.0; Y: 10.0; Z: 0.0);
    Scale: 1.0;
    UserData: nil);

  StockSurface: TSurfaceAttribute = (
    Ambient: (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);
    Diffuse: (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);
    Specular: (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);
    Emission: (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);
    Shininess: 10;
    Texture: 0;
    UserData: nil);

procedure SelectAttribute(Light: GLuint; const Attribute: TLightAttribute); overload;
procedure SelectAttribute(const Attribute: TSurfaceAttribute); overload;

type
  TBox = array[0..5] of Single;

  TModelLayer = record
    Box: TBox;
    Pivot: TVertexPoint;
    VertexIndex: Cardinal;
    VertexCount: Cardinal;
    TriangleIndex: Cardinal;
    TriangleCount: Cardinal;
    UserData: Pointer;
  end;
  PModelLayer = ^TModelLayer;

  TModelLayers = array of TModelLayer;

  TModel = record
    Pivot: TVertexPoint;
    Vertices: TVertices;
    Triangles: TTriangleIndexes;
    Layers: TModelLayers;
    UserData: Pointer;
  end;
  PModel = ^TModel;

  TModels = array of TModel;

procedure ObjectLoad(Stream: TStream; var Model: TModel);
procedure ObjectSave(Stream: TStream; const Model: TModel);

{ TTimer }

type
  TTimer = class(TObject, ITimer)
  private
    FTime: Single;
    FInterval: Single;
    FPaused: Boolean;
  protected
    function InternalTime: Single; virtual; abstract;
    procedure InternalReset; virtual; abstract;
    procedure InternalPaused(NewValue: Boolean); virtual; abstract;
    { TTimer.IUnknown }
		function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
		function _AddRef: Integer; stdcall;
		function _Release: Integer; stdcall;
    { TTimer.ITimer }
    function GetInterval: Single;
    function GetPaused: Boolean;
    procedure SetPaused(Value: Boolean);
    function GetTime: Single;
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

{ TBitmapFont explained:

    CharWidth: pixel width / screen width
    CharHeight: pixel height / screen height
    CharCount: numbers of characters per texture row
    Texture: stores the texture
    TextureWidth: pixel width / texture width
    TextureHeight: pixel height / texture height
    TextureOffsetX: texture offset x
    TextureOffsetY: texture offset y
    TextureBorder: an additive value for the above two fields }

  TBitmapFont = record
    CharWidth: Single;
    CharHeight: Single;
    CharCount: Integer;
    Texture: GLuint;
    TextureWidth: Single;
    TextureHeight: Single;
    TextureOffsetX: Single;
    TextureOffsetY: Single;
    TextureBorder: Single;
  end;
  PBitmapFont = ^TBitmapFont;

  TBitmapFonts = array of TBitmapFont;

procedure BuildFonts(const Output: string; const Fonts: TBitmapFonts; const Image: string);

type
  TTextBounds = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
  end;
  PTextBounds = ^TTextBounds;

  TTextWriter = class(TObject)
  private
    FAspectRatio: Single;
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
    procedure LoadFont(const FileName: string);
    procedure Load(const Fonts: TBitmapFonts; const FileName: string;
      Alpha: Boolean = False); overload;
    procedure Load(const Fonts: TBitmapFonts; Stream: TStream;
      Format: TTextureFormat; Alpha: Boolean = False); overload;
    procedure Write(const S: string; Size, X, Y: Single);
		procedure WriteFmt(const S: string;  const Args: array of const;
    	Size, X, Y: Single);
    property AspectRatio: Single read FAspectRatio write FAspectRatio;
    property BaseColor: PFloatColor read FBaseColor;
    property FontIndex: Integer read FFontIndex write FFontIndex;
    property LeadColor: PFloatColor read FLeadColor;
    property ShadowColor: PFloatColor read FShadowColor;
    property Shadow: Boolean read FShadow write FShadow;
    property TextBounds: PTextBounds read FTextBounds;
    property HorizontalFlip: Single read FHorizontalFlip write SetHorizontalFlip;
    property VerticalFlip: Single read FVerticalFlip write SetVerticalFlip;
  end;

function CreateTextWriter(const Name: string = ''): TTextWriter;

type
  TFontCoord = record
    X, Y: Single;
  end;

  TFontChar = record
    Left: Single;
    Top: Single;
    Right: Single;
    Bottom: Single;
    Width: Single;
    Height: Single;
    A: TFontCoord;
    B: TFontCoord;
  end;

  TFontMap = array[$20..$7D] of TFontChar;

  TFontStore = record
    Face: string[50];
    Size: Integer;
    Kerning: Integer;
    Map: TFontMap;
  end;

  TFontWriter = class
  private
    FTexture: GLuint;
    FStore: TFontStore;
    function GetFace: string;
    function GetKerning: Integer;
    function GetSize: Integer;
    function GetTextHeight: Single;
  public
    Origin: TFloatPoint;
    LeadColor: TFloatColor;
    BaseColor: TFloatColor;
    ShadowColor: TFloatColor;
    ShadowOffset: TFloatPoint;
    Screen: TFloatPoint;
    Shadows: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Load(const FileName: string); overload;
    procedure Load(const Store: TFontStore; Bits: Pointer); overload;
    procedure Write(const S: string; Scale, X, Y: Single);
    property Face: string read GetFace;
    property Kerning: Integer read GetKerning;
    property Size: Integer read GetSize;
    property TextHeight: Single read GetTextHeight;
  end;

procedure LoadFont(FontWriter: TFontWriter; const Store: TFontStore; DibBits: Pointer);
procedure SaveFont(const FileName: string; const Store: TFontStore; DibBits: Pointer);
procedure DrawStore(Src, Dest: HDC; const S: string; const Store: TFontStore);

{ TVertexBuffer }

type
  TVertexMode = (vmBuffer, vmArray, vmImmediate);

  TVertexBuffer = class
  private
    FVertexBuffer: GLuint;
    FIndexBuffer: GLuint;
    FMode: TVertexMode;
    FModel: TModel;
    FModelReference: PModel;
    procedure SetMode(Value: TVertexMode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; overload;
    procedure Draw(LayerIndex: Integer); overload;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    property Mode: TVertexMode read FMode write SetMode;
    property Model: PModel read FModelReference;
  end;

implementation

{$R FONT.RES}

procedure SelectAttribute(Light: GLuint; const Attribute: TLightAttribute);
begin
  with Attribute do
  begin
    glLightfv(Light, GL_AMBIENT, @Ambient);
    glLightfv(Light, GL_DIFFUSE, @Diffuse);
    glLightfv(Light, GL_SPECULAR, @Specular);
    glLightfv(Light, GL_POSITION, @Position);
  end;
end;

procedure SelectAttribute(const Attribute: TSurfaceAttribute); overload;
begin
  with Attribute do
  begin
    glMaterialfv(GL_FRONT, GL_AMBIENT, @Ambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, @Diffuse);
    glMaterialfv(GL_FRONT, GL_SPECULAR, @Specular);
    glMaterialfv(GL_FRONT, GL_EMISSION, @Emission);
    glMaterialfv(GL_FRONT, GL_SHININESS , @Shininess);
  end;
end;

procedure ModelCalculateBoxes(var Model: TModel);
{var
  A, B: PSingle;
  I, J: Integer;}
begin
  {if Length(Layer.Vertices) = 0 then Exit;
  A := @Layer.Vertices[0];
  B := @Layer.Box[0];
  for I := 0 to 2 do
  begin
    B^ := A^;
    Inc(B);
    B^ := A^;
    Inc(B);
    Inc(A);
  end;
  for I := 1 to Length(Layer.Vertices) - 1 do
  begin
    A := @Layer.Vertices[I];
    for J := Low(Layer.Box) to High(Layer.Box) do
    begin
      B := @Layer.Box[J];
      if Odd(J) then
      begin
        if A^ > B^ then
          B^ := A^;
        Inc(A);
      end
      else if A^ < B^ then
        B^ := A^;
    end;
  end;}
end;

procedure ObjectClear(var Model: TModel);
begin
  Model.Pivot := StockVertex;
  Model.Vertices := nil;
  Model.Triangles := nil;
  Model.Layers := nil;
end;

procedure ObjectLoad(Stream: TStream; var Model: TModel);
var
  InterFile: TInterchangeFile;
  Node: PInterchangeNode;
  Layer: PModelLayer;

  procedure ComputeNormal(const I: TTriangleIndex);
  var
    A, B, Normal: TVertexPoint;
  begin
    A.X := Model.Vertices[I.V2].X - Model.Vertices[I.V1].X;
    A.Y := Model.Vertices[I.V2].Y - Model.Vertices[I.V1].Y;
    A.Z := Model.Vertices[I.V2].Z - Model.Vertices[I.V1].Z;
    B.X := Model.Vertices[I.V0].X - Model.Vertices[I.V1].X;
    B.Y := Model.Vertices[I.V0].Y - Model.Vertices[I.V1].Y;
    B.Z := Model.Vertices[I.V0].Z - Model.Vertices[I.V1].Z;
    with Normal do
    begin
      X := A.Y * B.Z - A.Z * B.Y;
      Y := A.Z * B.X - A.X * B.Z;
      Z := A.X * B.Y - A.Y * B.X;
    end;
    Model.Vertices[I.V0].Normal := VertexAdd(Model.Vertices[I.V0].Normal, Normal);
    Model.Vertices[I.V1].Normal := VertexAdd(Model.Vertices[I.V1].Normal, Normal);
    Model.Vertices[I.V2].Normal := VertexAdd(Model.Vertices[I.V2].Normal, Normal);
  end;

  procedure Normalize;
  var
    Normal: TVertexPoint;
    Ratio: Single;
    I: Integer;
  begin
    for I := Low(Model.Vertices) to High(Model.Vertices) do
    begin
      Normal := Model.Vertices[I].Normal;
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
      Model.Vertices[I].Normal := Normal;
    end;
  end;

  procedure LoadLwob;
  begin
  end;

  procedure LoadLwo2;

    function CountTriangles: Integer;
    var
      Bytes: Integer;
      I: Integer;
    begin
      Result := 0;
      Bytes := Node.Offset + Node.Size;
      while InterFile.Stream.Position < Bytes do
      begin
        for I := 0 to ReadUint2(InterFile.Stream) - 1 do
          ReadVInt4(InterFile.Stream);
        Inc(Result);
      end;
    end;

  var
    L, V, T: Integer;
    I, J: Integer;
  begin
    with InterFile do
    begin
      Layer := nil;
      L := 0;
      for I := 0 to Length(Root.Children) - 1 do
        if Root.Children[I].Tag = 'LAYR' then
          Inc(L);
      SetLength(Model.Layers, L);
      L := 0;
      V := 0;
      T := 0;
      for I := 0 to Length(Root.Children) - 1 do
      begin
        Node := Root.Children[I];
        if Node.Tag = 'LAYR' then
        begin
          Layer := @Model.Layers[L];
          Inc(L);
        end
        else if Node.Tag = 'PNTS' then
        begin
          Layer.VertexIndex := V;
          Layer.VertexCount := Node.Size div SizeOf(TVertexPoint);
          Inc(V, Layer.VertexCount);
        end
        else if Node.Tag = 'POLS' then
        begin
          Select(Node);
          if not ReadTag(Stream, 'FACE') then Continue;
          Layer.TriangleIndex := T;
          Layer.TriangleCount := CountTriangles;
          Inc(T, Layer.TriangleCount);
        end;
      end;
      SetLength(Model.Vertices, V);
      SetLength(Model.Triangles, T);
      L := 0;
      V := 0;
      T := 0;
      for I := 0 to Length(Root.Children) - 1 do
      begin
        Node := Root.Children[I];
        Select(Node);
        if Node.Tag = 'LAYR' then
        begin
          Layer := @Model.Layers[L];
          // number
          ReadUint2(Stream);
          // flags
          ReadUint2(Stream);
          // pivot
          Layer.Pivot.X := ReadFloat(Stream);
          Layer.Pivot.Y := ReadFloat(Stream);
          Layer.Pivot.Z := ReadFloat(Stream);
          Inc(L);
        end
        else if Node.Tag = 'PNTS' then
          for J := 0 to Layer.VertexCount - 1 do
          begin
            Model.Vertices[V].X := ReadFloat(Stream);
            Model.Vertices[V].Y := ReadFloat(Stream);
            Model.Vertices[V].Z := ReadFloat(Stream);
            Model.Vertices[V].Normal := StockVertex;
            Inc(V);
          end
        else if (Node.Tag = 'POLS') and ReadTag(Stream, 'FACE') then
        begin
          Select(Node);
          ReadTag(Stream);
          for J := 0 to Layer.TriangleCount - 1 do
          begin
            // verts and $3FF
            ReadUint2(Stream);
            Model.Triangles[T].V0 := ReadVint4(Stream) + Layer.VertexIndex;
            Model.Triangles[T].V1 := ReadVint4(Stream) + Layer.VertexIndex;
            Model.Triangles[T].V2 := ReadVint4(Stream) + Layer.VertexIndex;
            ComputeNormal(Model.Triangles[T]);
            Inc(T);
          end;
        end;
      end;
    end;
    Normalize;
    ModelCalculateBoxes(Model);
  end;

  procedure LoadBare;
  var
    I: Integer;
  begin
    Stream.Read(Model.Pivot, SizeOf(Model.Pivot));
    Stream.Read(I, SizeOf(I));
    SetLength(Model.Vertices, I);
    Stream.Read(Model.Vertices[0], I * SizeOf(TVertex));
    Stream.Read(I, SizeOf(I));
    SetLength(Model.Triangles, I);
    Stream.Read(Model.Triangles[0], I * SizeOf(TTriangleIndex));
    Stream.Read(I, SizeOf(I));
    SetLength(Model.Layers, I);
    Stream.Read(Model.Layers[0], I * SizeOf(TModelLayer));
  end;

begin
  ObjectClear(Model);
  InterFile := TInterchangeFile.CreateFromStream(Stream);
  try
    InterFile.EnumChildren(InterFile.Root);
    if InterFile.Root.Tag = 'LWOB' then
      LoadLwob
    else if InterFile.Root.Tag = 'LWO2' then
      LoadLwo2
    else if InterFile.Root.Tag = 'BARE' then
      LoadBare;
  finally
    InterFile.Free;
  end;
end;

procedure ObjectSave(Stream: TStream; const Model: TModel);
const
  Header: array[0..11] of Char = ('F', 'O', 'R', 'M', #0, #0, #0, #0,
    'B', 'A', 'R', 'E');
var
  I: Integer;
begin
  Stream.Write(Header, SizeOf(Header));
  Stream.Write(Model.Pivot, SizeOf(Model.Pivot));
  I := Length(Model.Vertices);
  Stream.Write(I, SizeOf(I));
  Stream.Write(Model.Vertices[0], I * SizeOf(TVertex));
  I := Length(Model.Triangles);
  Stream.Write(I, SizeOf(I));
  Stream.Write(Model.Triangles[0], I * SizeOf(TTriangleIndex));
  I := Length(Model.Layers);
  Stream.Write(I, SizeOf(I));
  Stream.Write(Model.Layers[0], I * SizeOf(TModelLayer));
end;

procedure LayerClear(var Layer: TModelLayer);
begin
  FillChar(Layer, SizeOf(Layer), #0);
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

{ TTimer.IUnknown }

function TTimer.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TTimer._AddRef: Integer;
begin
  Result := 0;
end;

function TTimer._Release: Integer;
begin
  Result := 0;
end;

{ TTimer.ITimer }

function TTimer.GetInterval: Single;
begin
	Result := FInterval;
end;

function TTimer.GetPaused: Boolean;
begin
	Result := FPaused;
end;

procedure TTimer.SetPaused(Value: Boolean);
begin
  if Value <> FPaused then
  begin
    FPaused := Value;
    InternalPaused(Value);
  end;
end;

function TTimer.GetTime: Single;
begin
	Result := FTime;
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

{

const
  Size = 512;
var
  Bitmap: TBitmap;
  CX, CY: Integer;
  X, Y, W: Integer;
  I: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Size;
    Bitmap.Height := Size;
    Bitmap.Canvas.Font := SomeFomt;
    CX := Bitmap.Canvas.TextWidth('X');
    CY := Bitmap.Canvas.Textheight('X');
    W := Size div CX;
    for I := 32 to 127 do
    begin
      X := (I - 32) mod W * CX;
      Y := (I - 32) div W * CY;
      Bitmap.Canvas.TextOut(X, Y, Chr(I));
    end;
    Bitmap.SaveToFile('c:\mono.bmp');
  finally
    Bitmap.Free;
  end;
  Format('CX: %.8f CY: %.8f', [CX/ Size, CY / Size]);
end;

procedure BuildFont(Font: HFONT; const FileName: string);
const
  Size = 256;

  procedure SaveBitmap(DC: HDC; Bitmap: HBITMAP);
  const
    BitSize = Size * Size * SizeOf(TRGBTriple);
    FileHeader:  TBitmapFileHeader = (
      bfType: $4D42;
      bfSize: SizeOf(TBitmapFileHeader) + SizeOf(TBitmapFileHeader) + BitSize;
      bfReserved1: 0;
      bfReserved2: 0;
      bfOffBits: SizeOf(TBitmapFileHeader) + SizeOf(TBitmapFileHeader);
    );
    Info:  TBitmapInfo = ( bmiHeader: (
       biSize: SizeOf(TBitmapInfoHeader);
       biWidth: Size;
       biHeight: Size;
       biPlanes: 1;
       biBitCount: 24;
       biCompression: BI_RGB;
       biSizeImage: 0;
       biXPelsPerMeter: 0;
       biYPelsPerMeter: 0;
       biClrUsed: 0;
       biClrImportant: 0));
  var
    Stream: TStream;
    Bits: Pointer;
  begin
    Stream := TFileStream.Create(FileName, fmCreate);
    GetMem(Bits, BitSize);
    try
      Stream.Write(FileHeader, SizeOf(FileHeader));
      Stream.Write(Info, SizeOf(Info.bmiHeader));
      GetDiBits(DC, Bitmap, 0, Size, Bits, Info, DIB_RGB_COLORS);
      Stream.Write(Bits^, BitSize);
    finally
      FreeMem(Bits);
      Stream.Free;
    end;
  end;

var
  DC, Screen: HDC;
  Bitmap: HBITMAP;
  Point: TSize;
  X, Y, W: Integer;
  C: Char;
  I: Integer;
begin
  Screen := GetDC(0);
  DC := CreateCompatibleDC(Screen);
  ReleaseDC(0, Screen);
  Bitmap := CreateCompatibleBitmap(DC, Size, Size);
  Bitmap := SelectObject(DC, Bitmap);
  SelectObject(DC, Font);
  FillRect(DC, Classes.Rect(0, 0, Size, Size), GetStockObject(BLACK_BRUSH));
  SetTextColor(DC, $FFFFFF);
  SetBkColor(DC, 0);
  SetBKMode(DC, TRANSPARENT);
  GetTextExtentPoint32(DC, 'X', 1, Point);
  W := Size div Point.cX;
  for I := 32 to 127 do
  begin
    X := (I - 32) mod W * Point.cX;
    Y := (I - 32) div W * Point.cY;
    C := Chr(I);
    TextOut(DC, X, Y, @C, 1);
  end;
  SaveBitmap(DC, GetCurrentObject(DC, OBJ_BITMAP));
  SelectObject(DC, Font);
  Bitmap := SelectObject(DC, Bitmap);
  DeleteObject(Bitmap);
  DeleteDC(DC);
end; }

procedure BuildFonts(const Output: string; const Fonts: TBitmapFonts; const Image: string);
var
  Stream: TStream;
  Format: TTextureFormat;
  Texture: TStream;
  Size: Integer;
  I: Integer;
begin
  Stream := TFileStream.Create(Output, fmCreate);
  try
    Size := Length(Fonts);
    Stream.Write(Size, SizeOf(Size));
    for I := Low(Fonts) to High(Fonts) do
      Stream.Write(Fonts[I], SizeOf(Fonts[I]));
    Texture := TFileStream.Create(Image, fmOpenRead);
    try
      Format := ExtractTextureFormat(Image);
      Stream.Write(Format, SizeOf(Format));
      Size := Texture.Size;
      Stream.Write(Size, SizeOf(Size));
      Stream.CopyFrom(Texture, 0);
    finally
      Texture.Free;
    end;
  finally
    Stream.Free;
  end;
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

procedure TTextWriter.LoadFont(const FileName: string);
var
  Stream: tStream;
  Fonts: TBitmapFonts;
  Format: TTextureFormat;
  I: Integer;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.Read(I, SizeOf(I));
    SetLength(Fonts, I);
    for I := Low(Fonts) to High(Fonts) do
      Stream.Read(Fonts[I], SizeOf(Fonts[I]));
    Stream.Read(Format, SizeOf(Format));
    Stream.Read(I, SizeOf(I));
    Load(Fonts, Stream, Format);
  finally
    Stream.Free;
  end;
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

procedure TTextWriter.Write(const S: string; Size, X, Y: Single);

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
        TexX := TextureOffsetX + (C mod CharCount) * TextureWidth;
        TexY := TextureOffsetY + (C div CharCount) * TextureHeight;
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
  if FAspectRatio > 0 then
    glxBeginOrtho(1 * FAspectRatio * 0.75, 1)
  else
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

procedure TTextWriter.WriteFmt(const S: string;  const Args: array of const;
	Size, X, Y: Single);
begin
	Write(Format(S, Args), Size, X, Y);
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


function CreateTextWriter(const Name: string = ''): TTextWriter;
const
  DefaultFont: array[0..1] of TBitmapFont = (
    (CharWidth: 0.01375;
    CharHeight: 0.03;
    CharCount: 16;
    Texture: 0;
    TextureWidth: 0.0546875;
    TextureHeight: 0.0859375;
    TextureOffsetX: 0;
    TextureOffsetY: 0;
    TextureBorder: 0.0078125),
    (CharWidth: 0.0125;
    CharHeight: 0.02333;
    CharCount: 16;
    Texture: 0;
    TextureWidth: 0.05078125;
    TextureHeight: 0.06640625;
    TextureOffsetX: 0;
    TextureOffsetY: 0.515625;
    TextureBorder: 0.0));
var
  Fonts: TBitmapFonts;
  Stream: TStream;
  S: string;
begin
  S := Trim(UpperCase(Name));
  if S = '' then
  begin
    Result := TTextWriter.Create;
    SetLength(Fonts, 2);
    Fonts[0] := DefaultFont[0];
    Fonts[1] := DefaultFont[1];
    Stream := TResourceStream.CreateFromID(MainInstance, 110, RT_RCDATA);
    try
      Result.Load(Fonts, Stream, tfPng);
    finally
      Stream.Free;
    end;
  end
  else
    Result := nil;
end;

function FontBuffer(const Store: TFontStore; DibBits: Pointer): Pointer;
var
  RGBA: PRGBQuad;
  RGB: PRGBTriple;
  Info: TTextureInfo;
  I: Integer;
begin
  GetMem(Result, 256 * 256 * SizeOf(TRGBQuad));
  try
    for I := Low(Store.Map) to High(Store.Map) do
      with Store.Map[I] do
      begin
        A.X := Left / 255;
        A.Y := Top / 255;
        B.X := Right / 255;
        B.Y := Bottom / 255;
      end;
    RGBA := Result;
    RGB := DibBits;
    for I := 0 to 256 * 256 - 1 do
    begin
      RGBA.rgbReserved := (RGB.rgbtBlue + RGB.rgbtRed + RGB.rgbtGreen) div 3;
      RGBA.rgbBlue := High(Byte);
      RGBA.rgbGreen := High(Byte);
      RGBA.rgbRed := High(Byte);
      Inc(RGBA);
      Inc(RGB);
    end;
    Info.Bits := Result;
    Info.Width := 256;
    Info.Height := 256;
    ReverseScanLines(Info);
  except
    FreeMem(Result);
    raise;
  end;
end;

procedure LoadFont(FontWriter: TFontWriter; const Store: TFontStore; DibBits: Pointer);
var
  Buffer: Pointer;
begin
  Buffer := FontBuffer(Store, DibBits);
  try
    FontWriter.Load(Store, Buffer)
  finally
    FreeMem(Buffer);
  end;
end;

procedure SaveFont(const FileName: string; const Store: TFontStore; DibBits: Pointer);
var
  Buffer: Pointer;
  Stream: TFileStream;
begin
  Buffer := FontBuffer(Store, DibBits);
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.Write(Store, SizeOf(Store));
      Stream.Write(Buffer^, 256 * 256 * SizeOf(TRGBQuad));
    finally
      Stream.Free;
    end;
  finally
    FreeMem(Buffer)
  end;
end;

procedure DrawStore(Src, Dest: HDC; const S: string; const Store: TFontStore);
var
  X: Single;
  I: integer;
begin
  X := 1;
  for I := 1 to Length(S) do
  begin
    if (S[I] < '!') or (S[I] > '}') then
    begin
      X := X + Store.Map[$20].Width;
      Continue;
    end;
    with Store.Map[Ord(S[I])] do
    begin
      BitBlt(Dest, Round(X), 1, Round(Width), Round(Height), Src, Round(Left), Round(Top), SRCPAINT);
      X := X + Width;
    end;
  end;
end;

{ TFontWriter }

constructor TFontWriter.Create;
begin
  glGenTextures(1, @FTexture);
  LeadColor := fcWhite;
  BaseColor := fcWhite;
  ShadowColor := fcBlack;
  Screen := CreateFloatPoint(640, 480);
end;

destructor TFontWriter.Destroy;
begin
  glDeleteTextures(1, @FTexture);
end;

procedure TFontWriter.Load(const FileName: string);
var
  Info: TTextureInfo;
  Stream: TFileStream;
begin
  Info.Width := 256;
  Info.Height := 256;
  GetMem(Info.Bits, 256 * 256 * SizeOf(TRGBQuad));
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Stream.Read(FStore, SizeOf(FStore));
      Stream.Read(Info.Bits^, 256 * 256 * SizeOf(TRGBQuad));
    finally
      Stream.Free;
    end;
    BindTexture(Info, FTexture);
  finally
    FreeMem(Info.Bits);
  end;
end;

procedure TFontWriter.Load(const Store: TFontStore; Bits: Pointer);
var
  Info: TTextureInfo;
begin
  FStore := Store;
  Info.Bits := Bits;
  Info.Width := 256;
  Info.Height := 256;
  BindTexture(Info, FTexture);
end;

procedure TFontWriter.Write(const S: string; Scale, X, Y: Single);

    procedure Draw(const Lead, Base: TFloatColor; OX, OY: Single);
    var
      K: Single;
      CX, CY: Single;
      I: Integer;
    begin
      K := (FStore.Kerning * Scale) / 2;
      OX := OX * Scale + Origin.X;
      OY := OY * Scale + Origin.Y;
      CX := X * FStore.Map[$20].Width * Scale + OX;
      CY := Y * FStore.Map[$20].Height * Scale + OY;
      for I := 1 to Length(S) do
      begin
        if CY > Screen.Y then Break;
        if S[I] = #10 then
        begin
          CX := X * FStore.Map[$20].Width * Scale + OX;
          CY := CY + FStore.Map[$20].Height * Scale;
          Continue;
        end
        else if (S[I] < '!') or (S[I] > '}') then
        begin
          CX := CX + FStore.Map[$20].Width * Scale;
          Continue;
        end;
        with FStore.Map[Ord(S[I])] do
        begin
          if CY + Height * Scale < 0 then
            Continue;
          if CX + Width * Scale < 0 then
          begin
            CX := CX + Width * Scale;
            Continue;
          end;
          if CX > Screen.X then
            Continue;
          glColor4f(Lead.Red, Lead.Green, Lead.Blue, 1);
          glTexCoord(B.X, A.Y);
          glVertex2f(CX + Width * Scale + K, CY);
          glTexCoord(A.X, A.Y);
          glVertex2f(CX - K, CY);
          glxColor4f(Base.Red, Base.Green, Base.Blue, 1);
          glTexCoord(A.X, B.Y);
          glVertex2f(CX - K, CY + Height * Scale);
          glTexCoord(B.X, B.Y);
          glVertex2f(CX + Width * Scale + K, CY + Height * Scale);
          CX := CX + Width * Scale;
        end;
      end;
    end;

begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FTexture);
  glxBeginOrtho(Screen.X, Screen.Y);
  glBegin(GL_QUADS);
  if Shadows then
    Draw(ShadowColor, ShadowColor, ShadowOffset.X, ShadowOffset.Y);
  Draw(LeadColor, BaseColor, 0, 0);
  glEnd;
  glxEndOrtho;
  glDisable(GL_TEXTURE_2D);
end;

function TFontWriter.GetFace: string;
begin
  Result := FStore.Face;
end;

function TFontWriter.GetKerning: Integer;
begin
  Result := FStore.Kerning;
end;

function TFontWriter.GetSize: Integer;
begin
  Result := FStore.Size;
end;

function TFontWriter.GetTextHeight: Single;
begin
  Result := FStore.Map[$20].Height;
end;

{ TVertexBuffer }

constructor TVertexBuffer.Create;
begin
  inherited Create;
  Mode := vmBuffer;
  glxGenBuffers(2, @FVertexBuffer);
  FModelReference := @FModel;
end;

destructor TVertexBuffer.Destroy;
begin
  glxBindBuffer(GL_ARRAY_BUFFER, 0);
  glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glxDeleteBuffers(2, @FVertexBuffer);
  inherited Destroy;
end;

procedure TVertexBuffer.Draw;
var
  I: Integer;
begin
  if Length(FModel.Triangles) = 0 then Exit;
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  case FMode of
    vmBuffer:
      begin
        glxBindBuffer(GL_ARRAY_BUFFER, FVertexBuffer);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), Pointer(SizeOf(TVertexPoint)));
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), nil);
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
        glDrawElements(GL_TRIANGLES, Length(FModel.Triangles) * 3, GL_UNSIGNED_INT, nil);
      end;
    vmArray:
      begin
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glxBindBuffer(GL_ARRAY_BUFFER, 0);
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FModel.Vertices);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), @FModel.Vertices[0].Normal);
        glDrawElements(GL_TRIANGLES, Length(FModel.Triangles) * 3, GL_UNSIGNED_INT, FModel.Triangles);
      end;
    vmImmediate:
      begin
        glBegin(GL_TRIANGLES);
        for I := Low(FModel.Triangles) to High(FModel.Triangles) do
        begin
          with FModel.Vertices[FModel.Triangles[I].V0] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FModel.Vertices[FModel.Triangles[I].V1] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FModel.Vertices[FModel.Triangles[I].V2] do
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

procedure TVertexBuffer.Draw(LayerIndex: Integer);
var
  Layer: PModelLayer;
  I: Integer;
begin
  if Length(FModel.Triangles) = 0 then Exit;
  Layer := @FModel.Layers[LayerIndex];
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  case FMode of
    vmBuffer:
      begin
        glxBindBuffer(GL_ARRAY_BUFFER, FVertexBuffer);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), Pointer(SizeOf(TVertexPoint)));
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), nil);
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
        glDrawRangeElements(GL_TRIANGLES, Layer.TriangleIndex,
          (Layer.TriangleIndex + Layer.TriangleCount) * 3,
          Layer.TriangleCount * 3, GL_UNSIGNED_INT,
          Pointer(Layer.TriangleIndex * SizeOf(TTriangleIndex)));
      end;
    vmArray:
      begin
        glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glxBindBuffer(GL_ARRAY_BUFFER, 0);
        glxVertexPointer(3, GL_FLOAT, SizeOf(TVertex), FModel.Vertices);
        glxNormalPointer(GL_FLOAT, SizeOf(TVertex), @FModel.Vertices[0].Normal);
        glDrawRangeElements(GL_TRIANGLES, Layer.TriangleIndex,
          (Layer.TriangleIndex + Layer.TriangleCount) * 3,
          Layer.TriangleCount * 3, GL_UNSIGNED_INT,
          Pointer(Cardinal(FModel.Triangles) + Layer.TriangleIndex * SizeOf(TTriangleIndex)));
      end;
    vmImmediate:
      begin
        glBegin(GL_TRIANGLES);
        for I := Layer.TriangleIndex to Layer.TriangleIndex + Layer.TriangleCount - 1 do
        begin
          with FModel.Vertices[FModel.Triangles[I].V0] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FModel.Vertices[FModel.Triangles[I].V1] do
          begin
            glNormal(Normal.X, Normal.Y, Normal.Z);
            glVertex3f(X, Y, Z);
          end;
          with FModel.Vertices[FModel.Triangles[I].V2] do
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

procedure TVertexBuffer.LoadFromFile(const FileName: string);
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

procedure TVertexBuffer.LoadFromStream(Stream: TStream);
begin
  ObjectLoad(Stream, FModel);
  if GL_ARB_VERTEX_BUFFER_OBJECT then
  begin
    glxBindBuffer(GL_ARRAY_BUFFER, FVertexBuffer);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(TVertex) * Length(FModel.Vertices), FModel.Vertices, GL_STATIC_DRAW);
    glxBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FIndexBuffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(TTriangleIndex) * Length(FModel.Triangles), FModel.Triangles, GL_STATIC_DRAW);
  end;
end;

procedure TVertexBuffer.SetMode(Value: TVertexMode);
begin
  if (Value = vmBuffer) and (not GL_ARB_VERTEX_BUFFER_OBJECT) then
    Value := vmArray;
  FMode := Value;
end;

end.
