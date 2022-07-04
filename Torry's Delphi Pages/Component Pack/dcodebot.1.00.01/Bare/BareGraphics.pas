
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareGraphics;

interface

{$I BARE.INC}
{$DEFINE PNG}
{$DEFINE JPEG}

uses
  {$IFNDEF BARE}SysUtils, Classes, Graphics,{$ENDIF}
  {$IFDEF PNG}BarePng,{$ENDIF}{$IFDEF JPEG}BareJpeg,{$ENDIF}
  BareUtils, BareOpenGL, BareOpenGLExt, BareInterchange, Windows;

type
  ITimer = interface
    function GetInterval: Single;
    function GetPaused: Boolean;
    procedure SetPaused(Value: Boolean);
    function GetTime: Single;
    property Interval: Single read GetInterval;
    property Paused: Boolean read GetPaused write SetPaused;
    property Time: Single read GetTime;
  end;

  TPoints = array of TPoint;

  TFloatPoint = record
    X: Single;
    Y: Single;
  end;
  PFloatPoint = ^TFloatPoint;

  TFloatPoints = array of TFloatPoint;

  TFloatLine = record
    P0: TFloatPoint;
    P1: TFloatPoint;
  end;

  TSlope = record
    Undefined: Boolean;
    Ratio: Single;
    Intercept: Single;
  end;

  TFloatRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;

  TQuad = array[0..3] of Single;

{$IFDEF BARE}
  TColor = record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
  end;
  PColor = ^TColor;
{$ENDIF}

  TFloatColor = record
    case Integer of
    0: (
      Red: Single;
      Green: Single;
      Blue: Single;
      Alpha: Single);
    1: (Values: array[0..3] of Single);
  end;
  PFloatColor = ^TFloatColor;

  TVertexPoint = record
    X, Y, Z: Single;
  end;
  PVertexPoint = ^TVertexPoint;
  TVertexPoints = array of TVertexPoint;

  TVertexLine = record
    A, B: TVertexPoint;
  end;

  TVector = TVertexPoint;
  PVector = PVertexPoint;

  TVertex = record
    case Integer of
      0: (
        Point: TVertexPoint;
        Normal: TVertexPoint);
      1: (
        X: Single;
        Y: Single;
        Z: Single);
  end;
  PVertex = ^TVertex;
  TVertices = array of TVertex;

  TTexturedVertex = record
    case Integer of
      0: (
        Point: TVertexPoint;
        Normal: TVertexPoint;
        S: Single;
        T: Single);
      1: (
        X: Single;
        Y: Single;
        Z: Single);
  end;
  PTexturedVertex = ^TTexturedVertex;

  TTriangleIndex = record
    V0: Integer;
    V1: Integer;
    V2: Integer;
  end;
  TTriangleIndexes = array of TTriangleIndex;

  TQuadIndex = record
    V0: Integer;
    V1: Integer;
    V2: Integer;
    V3: Integer;
  end;
  TQuadIndexes = array of TQuadIndex;

  TPlane = array[0..3] of TVertexPoint;

  TPolygon = record
    VertexCount: Integer;
    Vertices: array[0..3] of PVertex;
    Normal: TVertexPoint;
  end;
  PPolygon = ^TPolygon;

  TPolygons = array of TPolygon;

  TDirection = record
    Heading: Single;
    Pitch: Single;
    Roll: Single;
  end;
  PDirection = ^TDirection;

  TRotationOrder = (roXYZ, roYZX, roZXY, roXZY, roZYX, roYXZ);

  TPlacement = record
  	Position: TVertexPoint;
    Direction: TDirection;
  end;
  PPlacement = ^TPlacement;

  TMatrix = array[0..3, 0..3] of Single;
  PMatrix = ^TMatrix;

	TQuaternion = record
    W, X, Y, Z: Single;
  end;
  PQuaternion = ^TQuaternion;

const
  fcWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);
  fcBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);

{ Float point math }

function Divide(const Quotient, Divisor: Single): Single;
function Remainder(const Quotient, Divisor: Single): Single;
function IntegerPart(const Value: Single): Single;
function FractionalPart(const Value: Single): Single;

{ Basic struct creation routine }

function CreatePoint(X, Y: Integer): TPoint;
function CreateRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
function CreatePoints(const Points: array of Integer): TPoints;
function CreateFloatPoint(X, Y: Single): TFloatPoint;
function CreateFloatLine(X0, Y0, X1, Y1: Single): TFloatLine;
function CreateSlope(const Line: TFloatLine): TSlope;
function CreateFloatRect(Left, Top, Right, Bottom: Single): TFloatRect;
function CreateFloatPoints(const Points: array of Single): TFloatPoints;
function CreateQuad(X1, Y1, X2, Y2: Single): TQuad;
function CreateFloatColor(Red, Green, Blue, Alpha: Single): TFloatColor; overload;
function CreateFloatColor(Color: TColor): TFloatColor; overload;

function CreateDirection(H, P, R: Single): TDirection;
function CreateMatrix(X, Y, Z: Single): TMatrix; overload;
function CreateMatrix(X, Y, Z: Single; Order: TRotationOrder): TMatrix; overload;
function CreateMatrix(Q: TQuaternion): TMatrix; overload;
function CreateQuaternion(X, Y, Z: Single): TQuaternion; overload;
function CreateQuaternion(X, Y, Z: Single; Order: TRotationOrder): TQuaternion; overload;
function CreateQuaternion(const M: TMatrix): TQuaternion; overload;
function CreateVector(X, Y, Z: Single): TVector;
function CreateVertexPoint(X, Y, Z: Single): TVertexPoint;

{ FloatPoint rountines }

function PointIsRight(const Line: TFloatLine; const Slope: TSlope; Points: Pointer; Count: Integer): Boolean; overload;
function PointIsRight(const Line: TFloatLine; Points: Pointer; Count: Integer): Boolean; overload;
function PointIsLeft(const Line: TFloatLine; const Slope: TSlope; Points: Pointer; Count: Integer): Boolean; overload;
function PointIsLeft(const Line: TFloatLine; Points: Pointer; Count: Integer): Boolean; overload;
function PointDistance(const X, Y: Single): Single; overload;
function PointDistance(const Point: TFloatPoint): Single; overload;
function PointRotate(const Point: TFloatPoint; const Angle: Single): TFloatPoint;
function PointAngle(const Point: TFloatPoint): Single;

function DistanceRotate(const Distance: Double; Angle: Single): TFloatPoint;

function PointInPoints(const Point: TPoint; const Points: TPoints): Boolean; overload;
function PointInPoints(const Point: TFloatPoint; const Points: TFloatPoints): Boolean; overload;

function NormalizeAngle(A: Single): Single;

{ Direction routines }

function DirectionAdd(const A, B: TDirection): TDirection;
function DirectionSubtract(const A, B: TDirection): TDirection;

{ Matrix routines }

function MatrixAdd(const A, B: TMatrix): TMatrix;
function MatrixSubtract(const A, B: TMatrix): TMatrix;
function MatrixMultiply(const A, B: TMatrix): TMatrix;
function MatrixDivide(const A, B: TMatrix): TMatrix;
function MatrixScale(const M: TMatrix; X, Y, Z: Single): TMatrix;
function MatrixTranslate(const M: TMatrix; X, Y, Z: Single): TMatrix;
function MatrixRotate(const M: TMatrix; X, Y, Z: Single): TMatrix; overload;
function MatrixRotate(const M: TMatrix; X, Y, Z: Single; Order: TRotationOrder): TMatrix; overload;
function MatrixTransform(const M: TMatrix; const V: TVertexPoint): TVertexPoint;
function MatrixTranspose(const M: TMatrix): TMatrix;

{ Quaternion routines }

function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
function QuaternionMultiply(const A, B: TQuaternion): TQuaternion;
function QuaternionNormalize(const Q: TQuaternion): TQuaternion;

{ Vector routines }

function VectorDotProduct(const A, B: TVector): Single;
function VectorCrossProduct(const A, B: TVector): TVector;
function VectorNormalize(const V: TVector): TVector;

{ Point routines }

function VertexDistance(const A, B: TVertexPoint): Single; overload;
function VertexDistance(const A: TVertexPoint): Single; overload;
function VertexAdd(const A, B: TVertexPoint): TVertexPoint;
function VertexSubtract(const A, B: TVertexPoint): TVertexPoint;
function VertexRotate(const V: TVertexPoint; const Direction: TDirection): TVertexPoint;
function VertexScale(const A: TVertexPoint; Factor: Single): TVertexPoint; overload;
function VertexScale(const A: TVertexPoint; const Factors: TVertexPoint): TVertexPoint; overload;

function IsVertexEqual(const A, B: TVertexPoint): Boolean;

function PointLineDistance(const Point: TVertexPoint; const Line: TVertexLine): Single;
function PointSegmentDistance(const Point: TVertexPoint; const Line: TVertexLine): Single;

{ Plane routines }

procedure PlaneDraw(const Plane: TPlane);
function PlaneAdd(const Plane: TPlane; X, Y, Z: Single): TPlane; overload;
function PlaneAdd(const Plane: TPlane; const V: TVertexPoint): TPlane; overload;
function PlaneRotate(const Plane: TPlane; const Direction: TDirection): TPlane;
function PlaneScale(const Plane: TPlane; Factor: Single): TPlane;

{ Polygon routines }

function PolygonCenter(const Polygon: TPolygon): TVertexPoint;
function PolygonNormal(const Polygon: TPolygon): TVertexPoint;

{ Quad routines }

function QuadDraw(const Quad: TQuad; X, Y: Single; Width: Single = 0;
  Height: Single = 0): TFloatPoint; overload;
function QuadDraw(const Quad: TQuad; Position: TFloatPoint;
  Width: Single = 0; Height: Single = 0): TFloatPoint; overload;

{ Context functions used to translate between pixel coordinates and device space
  coordinates }

function InitDevice(DC: HDC): HDC;
procedure FreeDevice(DC: HDC);

{ Device space coordinate translation routines }

function IntToDevice(Value: Integer; Angle: Integer = 0): Single;
function PointToDevice(const Point: TPoint): TFloatPoint; overload;
function PointToDevice(X, Y: Integer): TFloatPoint; overload;
function RectToDevice(const Rect: TRect): TFloatRect; overload;
function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect; overload;
function DeviceToInt(const Value: Single; Angle: Integer = 0): Integer;
function DeviceToPoint(const Point: TFloatPoint): TPoint; overload;
function DeviceToPoint(const X, Y: Single): TPoint; overload;
function DeviceToRect(const Rect: TFloatRect): TRect; overload;
function DeviceToRect(const ALeft, ATop, ARight, ABottom: Single): TRect; overload;

type
  TOrientation = (orLeft, orUp, orRight, orDown, orCenter);

{ Decive compatible rect querying and transofrmation routines }

procedure OffsetRect(var Rect: TRect; X, Y: Integer); overload;
procedure OffsetRect(var Rect: TFloatRect; const X, Y: Single); overload;
procedure InflateRect(var Rect: TRect; X, Y: Integer); overload;
procedure InflateRect(var Rect: TFloatRect; const X, Y: Single); overload;
function HeightOf(const Rect: TRect): Integer; overload;
function HeightOf(const Rect: TFloatRect): Single; overload;
function HeightOf(const Quad: TQuad): Single; overload;
function HeightOf(const Quads: array of TQuad): Single; overload;
function WidthOf(const Rect: TRect): Integer; overload;
function WidthOf(const Rect: TFloatRect): Single; overload;
function WidthOf(const Quad: TQuad): Single; overload;
function WidthOf(const Quads: array of TQuad): Single; overload;

procedure Slide(var Rect: TRect; Orientation: TOrientation = orDown; Distance: Integer = 0); overload;
procedure Slide(var Rect: TFloatRect; Orientation: TOrientation = orDown; Distance: Single = 0); overload;

{ Drawing routines

  All routines assume a valid rendering context exists and that glBegin has
  been called. }

{ The DrawPolygon procedure draws a normal line segment. }

procedure DrawNormal(const Polygon: TPolygon);

{ The DrawPolygon procedure draws a polygon with normalization. }

procedure DrawPolygon(const Polygon: TPolygon);

{ The FlashColor procedure }

procedure FlashColor(Red, Green, Blue, Alpha: Single);

{ The DrawGrid procedure }

procedure DrawGrid(Width, Length: Integer);

{ The DrawLetterBox procedure }

procedure DrawLetterBox(Percent: Single);

{ Basic geometric shapes }

procedure DrawClub(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
procedure DrawDiamond(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
procedure DrawHeart(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
procedure DrawSpade(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
procedure DrawStar(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
procedure DrawCube(Size: Single);
procedure DrawBox(W, H, D: Single);
procedure DrawWireBox(W, H, D: Single);
procedure DrawAxis(Scale: Single);

{ Extensions }

procedure glxMultMatrix(const M: TMatrix);
procedure glxLoadMatrix(const M: TMatrix);

{ Texture management routines }

function ReadBitmapInfo(const FileName: string): string;
procedure MakeTrueBitmap(const ColorMap, AlphaMap, Output: string);
procedure ExtractAlphaBitmap(const AlphaMap, Output: string);

type
  TTextureInfo = record
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
  end;

procedure ReverseScanLines(const TextureInfo: TTextureInfo);

{ Texture support routines }

type
  TTextureFormat = (tfBitmap, tfTarga{$IFDEF PNG}, tfPng{$ENDIF}{$IFDEF JPEG}, tfJpeg{$ENDIF});

function ExtractTextureFormat(const FileName: string): TTextureFormat;
function LoadTexture(const FileName: string; Alpha: Boolean = False): TTextureInfo; overload;
function LoadTexture(Stream: TStream; Format: TTextureFormat; Alpha: Boolean = False): TTextureInfo; overload;

function BindTexture(const FileName: string; Texture: GLuint; Alpha: Boolean = False): Boolean; overload;
function BindTexture(Stream: TStream; Texture: GLuint; Format: TTextureFormat; Alpha: Boolean = False): Boolean; overload;
function BindTexture(const TextureInfo: TTextureInfo; Texture: GLuint): Boolean; overload;

function IsTextureEmpty(const TextureInfo: TTextureInfo): Boolean;
function IsTextureSquare(const TextureInfo: TTextureInfo): Boolean;

procedure ClearTexture(Size: Cardinal; Texture: GLuint);
procedure DisposeTexture(const TextureInfo: TTextureInfo);

procedure ClampTexture(Texture: GLuint; MinFilter: GLuint = GL_NEAREST;
  MagFilter: GLuint = GL_NEAREST);

var
  TextureBits: Pointer = nil;
  NormalScale: Single = 1.0;

const
  StockDirection: TDirection = (
    Heading: 0; Pitch: 0; Roll: 0);
  StockPlane: TPlane = (
    (X: -0.5; Y: 0.5; Z: 0),
    (X: -0.5; Y: -0.5; Z: 0),
    (X: 0.5; Y: -0.5; Z: 0),
    (X: 0.5; Y: 0.5; Z: 0));
  StockMatrix: TMatrix = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));
  StockQuaternion: TQuaternion = (W: 1; X: 0; Y: 0; Z: 0);
  StockVertex: TVertexPoint = (X: 0; Y: 0; Z: 0);
  StockNormal: TVertexPoint = (X: 0; Y: 1; Z: 0);

var
	DefaultRotationOrder: TRotationOrder = roZXY;

implementation

{ Float point math }

function Divide(const Quotient, Divisor: Single): Single;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Round(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Single): Single;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - (Trunc(Quotient) div Trunc(Divisor)) * Divisor;
end;

function IntegerPart(const Value: Single): Single;
begin
  Result := Trunc(Value);
end;

function FractionalPart(const Value: Single): Single;
begin
  Result := Value - Trunc(Value);
end;

{ Basic struct creation routines }

function CreatePoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function CreateFloatPoint(X, Y: Single): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function CreateFloatLine(X0, Y0, X1, Y1: Single): TFloatLine;
begin
  Result.P0.X := X0;
  Result.P0.Y := Y0;
  Result.P1.X := X1;
  Result.P1.Y := Y1;
end;

function CreateSlope(const Line: TFloatLine): TSlope;
begin
  Result.Undefined := False;
  Result.Ratio := 0;
  if Line.P0.X = Line.P1.X then
  begin
    Result.Undefined := True;
    Result.Intercept := Line.P0.X
  end
  else if Line.P0.Y = Line.P1.Y then
    Result.Intercept := Line.P0.Y
  else
  begin
    Result.Ratio := (Line.P0.Y - Line.P1.Y) / (Line.P0.X - Line.P1.X);
    Result.Intercept := Line.P0.Y - Result.Ratio * Line.P0.X;
  end;
end;

function CreateRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function CreateFloatRect(Left, Top, Right, Bottom: Single): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function CreatePoints(const Points: array of Integer): TPoints;
var
  Lo: Integer;
  I: Integer;
begin
  Result := nil;
  if High(Points) - Low(Points) > 4 then
  begin
    SetLength(Result, (High(Points) - Low(Points) + 1) div 2);
    Lo := Low(Points);
    for I := 0 to Length(Result) - 1 do
    begin
      Result[I].X := Points[I * 2 + Lo];
      Result[I].Y := Points[I * 2 + Lo + 1];
    end;
  end;
end;

function CreateFloatPoints(const Points: array of Single): TFloatPoints;
var
  Lo: Integer;
  I: Integer;
begin
  Result := nil;
  if High(Points) - Low(Points) > 4 then
  begin
    SetLength(Result, (High(Points) - Low(Points) + 1) div 2);
    Lo := Low(Points);
    for I := 0 to Length(Result) - 1 do
    begin
      Result[I].X := Points[I * 2 + Lo];
      Result[I].Y := Points[I * 2 + Lo + 1];
    end;
  end;
end;

function CreateQuad(X1, Y1, X2, Y2: Single): TQuad;
begin
  Result[0] := X1;
  Result[1] := Y1;
  Result[2] := X2;
  Result[3] := Y2;
end;

function CreateFloatColor(Red, Green, Blue, Alpha: Single): TFloatColor;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
  Result.Alpha := Alpha;
end;

function CreateFloatColor(Color: TColor): TFloatColor;
var
  C: array[0..3] of Byte absolute Color;
begin
  if Color < 0 then
    Color := GetSysColor(Color and $000000FF);
  Result := CreateFloatColor(C[0] / $FF, C[1] / $FF, C[2] / $FF, 1);
end;

function CreateDirection(H, P, R: Single): TDirection;
begin
  Result.Heading := H;
  Result.Pitch := P;
  Result.Roll := R;
end;

function CreateMatrix(X, Y, Z: Single): TMatrix; overload;
begin
	Result := MatrixRotate(StockMatrix, X, Y, Z);
end;

function CreateMatrix(X, Y, Z: Single; Order: TRotationOrder): TMatrix; overload;
begin
	Result := MatrixRotate(StockMatrix, X, Y, Z, Order);
end;

function CreateMatrix(Q: TQuaternion): TMatrix; overload;
var
	SW, SX, SY, SZ, S, T: Single;
begin
  Result := StockMatrix;
  SW := Q.W * Q.W;
  SX := Q.X * Q.X;
  SY := Q.Y * Q.Y;
  SZ := Q.Z * Q.Z;
  Result[0, 0] :=  SX - SY - SZ + SW;
  Result[1, 1] := -SX + SY - SZ + SW;
  Result[2, 2] := -SX - SY + SZ + SW;
  S := Q.X * Q.Y;
  T := Q.Z * Q.W;
  Result[1, 0] := 2 * (S + T);
  Result[0, 1] := 2 * (S - T);
  S := Q.X * Q.Z;
  T := Q.Y * Q.W;
  Result[2, 0] := 2 * (S - T);
  Result[0, 2] := 2 * (S + T);
  S := Q.Y * Q.Z;
  T := Q.X * Q.W;
  Result[2, 1] := 2 * (S + T);
  Result[1, 2] := 2 * (S - T);
end;

function CreateQuaternion(X, Y, Z: Single): TQuaternion; overload;
begin
	Result := CreateQuaternion(CreateMatrix(X, Y, Z));
end;

function CreateQuaternion(X, Y, Z: Single; Order: TRotationOrder): TQuaternion; overload;
begin
	Result := CreateQuaternion(CreateMatrix(X, Y, Z, Order));
end;

function CreateQuaternion(const M: TMatrix): TQuaternion; overload;
const
	Epsilon = 0.001;
var
  S, T: Single;
begin
  T := M[0, 0] + M[1, 1] + M[2, 2] + 1;
  if T > Epsilon then
  begin
    S := 0.5 / Sqrt(T);
    Result.W := 0.25 / S;
    Result.X := (M[2, 1] - M[1, 2]) * S;
    Result.Y := (M[0, 2] - M[2, 0]) * S;
    Result.Z := (M[1, 0] - M[0, 1]) * S;
  end
	else if (M[0, 0] > M[1, 1]) and (M[0, 0] > M[2, 2]) then
	begin
    S := 2 * Sqrt(1.0 + M[0, 0] - M[1, 1] - M[2, 2]);
    Result.W := (M[1, 2] - M[2, 1]) / S;
    Result.X := 0.25 * S;
    Result.Y := (M[0, 1] + M[1, 0]) / S;
    Result.Z := (M[0, 2] + M[2, 0]) / S;
  end
	else if M[1, 1] > M[2, 2] then
  begin
    S := 2 * Sqrt(1.0 + M[1, 1] - M[0, 0] - M[2, 2]);
    Result.W := (M[0, 2] - M[2, 0]) / S;
    Result.X := (M[0, 1] + M[1, 0]) / S;
    Result.Y := 0.25 * S;
    Result.Z := (M[1, 2] + M[2, 1]) / S;
  end
  else
  begin
    S := 2 * Sqrt(1.0 + M[2, 2] - M[0, 0] - M[1, 1]);
    Result.W := (M[0, 1] - M[1, 0]) / S;
    Result.X := (M[0, 2] + M[2, 0]) / S;
    Result.Y := (M[1, 2] + M[2, 1]) / S;
    Result.Z := 0.25 * S;
  end;
end;

function CreateVector(X, Y, Z: Single): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function CreateVertexPoint(X, Y, Z: Single): TVertexPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


function PointIsRight(const Line: TFloatLine; const Slope: TSlope; Points: Pointer; Count: Integer): Boolean;
var
  P: PFloatPoint absolute Points;
begin
  Result := False;
  if Slope.Undefined then
  begin
    while Count > 0 do
    begin
      Result := P.X > Slope.Intercept;
      if Result then
        Break;
      Dec(Count);
      Inc(P);
    end;
    if Line.P0.Y > Line.P1.Y then
      Result := not Result;
  end
  else
  if Slope.Ratio = 0 then
  begin
    while Count > 0 do
    begin
      Result := P.Y < Slope.Intercept;
      if Result then
        Break;
      Dec(Count);
      Inc(P);
    end;
    if Line.P0.X > Line.P1.X then
      Result := not Result;
  end
  else
  begin
    while Count > 0 do
    begin
      Result := P.Y < Slope.Ratio * P.X + Slope.Intercept;
      if Result then
        Break;
      Dec(Count);
      Inc(P);
    end;
    if Line.P0.X > Line.P1.X then
      Result := not Result;
  end;
end;

function PointIsRight(const Line: TFloatLine; Points: Pointer; Count: Integer): Boolean; 
begin
  Result := PointIsRight(Line, CreateSlope(Line), Points, Count);
end;

function PointIsLeft(const Line: TFloatLine; const Slope: TSlope; Points: Pointer; Count: Integer): Boolean;
begin
  Result := not PointIsRight(Line, Slope, Points, Count);
end;

function PointIsLeft(const Line: TFloatLine; Points: Pointer; Count: Integer): Boolean;
begin
  Result := not PointIsRight(Line, Points, Count);
end;

function PointDistance(const X, Y: Single): Single;
begin
  Result := Sqrt(X * X + Y * Y);
end;

function PointDistance(const Point: TFloatPoint): Single;
begin
  with Point do
    Result := Sqrt(X * X + Y * Y);
end;

function PointRotate(const Point: TFloatPoint; const Angle: Single): TFloatPoint;
var
  Radians: Single;
begin
  Radians :=   Angle * (PI / 180);
  Result.X := (Point.X * Cos(Radians)) - (Point.Y * Sin(Radians));
  Result.Y := (Point.X * Sin(Radians)) + (Point.Y * Cos(Radians));
end;

function PointAngle(const Point: TFloatPoint): Single;
begin
  if Point.Y = 0 then
    if Point.X < 0 then
      Result := 270
    else if Point.X > 0 then
      Result := 90
    else
      Result := 0
  else
  begin
    Result := -ArcTan(Point.X / Point.Y) * (180 / PI);
    if Point.Y > 0 then
      Result := Result + 180
    else if Point.X < 0 then
      Result := 360 + Result
  end;
end;

function DistanceRotate(const Distance: Double; Angle: Single): TFloatPoint;
begin
  Angle := Angle * (PI / 180);
  Result.X := Distance * Cos(Angle);
  Result.Y := Distance * Sin(Angle);
end;

function PointInPoints(const Point: TPoint; const Points: TPoints): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if Length(Points) > 2 then
  begin
    J := High(Points);
    for I := Low(Points) to High(Points) do
    begin
      if (((Points[I].Y <= Point.Y) and (Point.Y < Points[J].Y)) or
        ((Points[J].Y<= Point.Y) and (Point.Y < Points[I].Y))) and (Point.X <
        (Points[J].X - Points[I].X) * (Point.Y - Points[I].Y) / (Points[J].Y -
        Points[I].Y) + Points[I].X) then
        Result := not Result;
      J := I;
    end;
  end;
end;

function PointInPoints(const Point: TFloatPoint; const Points: TFloatPoints): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if Length(Points) > 2 then
  begin
    J := High(Points);
    for I := Low(Points) to High(Points) do
    begin
      if (((Points[I].Y <= Point.Y) and (Point.Y < Points[J].Y)) or
        ((Points[J].Y<= Point.Y) and (Point.Y < Points[I].Y))) and (Point.X <
        (Points[J].X - Points[I].X) * (Point.Y - Points[I].Y) / (Points[J].Y -
        Points[I].Y) + Points[I].X) then
        Result := not Result;
      J := I;
    end;
  end;
end;

function NormalizeAngle(A: Single): Single;
begin
  Result := Remainder(A, 360);
  if Result > 180 then
    Result := Result - 360
  else if Result < -180 then
    Result := Result + 360;
end;

{ Direction routines }

function DirectionAdd(const A, B: TDirection): TDirection;
begin
  Result.Heading := A.Heading + B.Heading;
  Result.Pitch := A.Pitch + B.Pitch;
  Result.Roll := A.Roll + B.Roll;
end;

function DirectionSubtract(const A, B: TDirection): TDirection;
begin
  Result.Heading := A.Heading - B.Heading;
  Result.Pitch := A.Pitch - B.Pitch;
  Result.Roll := A.Roll - B.Roll;
end;

{ Matrix routines }

function MatrixAdd(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] + B[X, Y];
end;

function MatrixSubtract(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] - B[X, Y];
end;

function MatrixMultiply(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] * B[X, 0] + A[1, Y] * B[X, 1] + A[2, Y] *
      B[X, 2] + A[3, Y] * B[X, 3];
end;

function MatrixDivide(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] / B[X, 0] + A[1, Y] / B[X, 1] + A[2, Y] / B[X, 2];
end;

function MatrixScale(const M: TMatrix; X, Y, Z: Single): TMatrix;
var
  S: TMatrix;
begin
  S := StockMatrix;
  S[0, 0] := X;
  S[1, 1] := Y;
  S[2, 2] := Z;
  Result := MatrixMultiply(M, S);
end;

function MatrixTranslate(const M: TMatrix; X, Y, Z: Single): TMatrix;
var
  T: TMatrix;
begin
  T := StockMatrix;
  T[3, 0] := X;
  T[3, 1] := Y;
  T[3, 2] := Z;
  Result := MatrixMultiply(M, T);
end;

function MatrixRotate(const M: TMatrix; X, Y, Z: Single): TMatrix;
begin
	Result := MatrixRotate(M, X, Y, Z, DefaultRotationOrder);
end;

function MatrixRotate(const M: TMatrix; X, Y, Z: Single; Order: TRotationOrder): TMatrix;
var
  R: TMatrix;

	procedure RotateX;
  begin
    if X <> 0 then
    begin
      R := StockMatrix;
      R[1, 1] := Cos(X);
      R[1, 2] := Sin(X);
      R[2, 1] := -R[1, 2];
      R[2, 2] := R[1, 1];
      Result := MatrixMultiply(Result, R);
    end;
  end;

  procedure RotateY;
  begin
    if Y <> 0 then
    begin
      R := StockMatrix;
      R[0, 0] := Cos(Y);
      R[2, 0] := Sin(Y);
      R[0, 2] := -R[2, 0];
      R[2, 2] := R[0, 0];
      Result := MatrixMultiply(Result, R);
    end;
  end;

  procedure RotateZ;
  begin
    if Z <> 0 then
    begin
      R := StockMatrix;
      R[0, 0] := Cos(Z);
      R[1, 0] := Sin(Z);
      R[0, 1] := -R[1, 0];
      R[1, 1] := R[0, 0];
      Result := MatrixMultiply(Result, R);
    end;
  end;

begin
  Result := M;
  X := X * (PI / 180);
  Y := Y * (PI / 180);
  Z := Z * (PI / 180);
  case Order of
		roXYZ:
    	begin
      	RotateX;
        RotateY;
        RotateZ;
      end;
    roYZX:
    	begin
        RotateY;
        RotateZ;
      	RotateX;
      end;
    roZXY:
    	begin
        RotateZ;
      	RotateX;
        RotateY;
      end;
    roXZY:
    	begin
      	RotateX;
        RotateZ;
        RotateY;
      end;
    roZYX:
    	begin
        RotateZ;
        RotateY;
      	RotateX;
      end;
    roYXZ:
    	begin
        RotateY;
      	RotateX;
        RotateZ;
      end;
  end;
end;

function MatrixTransform(const M: TMatrix; const V: TVertexPoint): TVertexPoint;
begin
  Result.X := M[0, 0] * V.X + M[1, 0] * V.Y + M[2, 0] * V.Z + M[3, 0];
  Result.Y := M[0, 1] * V.X + M[1, 1] * V.Y + M[2, 1] * V.Z + M[3, 1];
  Result.Z := M[0, 2] * V.X + M[1, 2] * V.Y + M[2, 2] * V.Z + M[3, 2];
end;

function MatrixTranspose(const M: TMatrix): TMatrix;
var
	F: Single;
begin
	Result := M;
	F := Result[0, 1]; Result[0, 1] := Result[1, 0]; Result[1, 0] := F;
	F := Result[0, 2]; Result[0, 2] := Result[2, 0]; Result[2, 0] := F;
	F := Result[0, 3]; Result[0, 3] := Result[3, 0]; Result[3, 0] := F;
	F := Result[1, 2]; Result[1, 2] := Result[2, 1]; Result[2, 1] := F;
	F := Result[1, 3]; Result[1, 3] := Result[3, 1]; Result[3, 1] := F;
	F := Result[2, 3]; Result[2, 3] := Result[3, 2]; Result[3, 2] := F;
end;

{ Quaternion routines }

function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
begin
  Result.X := -Q.X;
  Result.Y := -Q.Y;
  Result.Z := -Q.Z;
  Result.W := Q.W;
end;

function QuaternionMultiply(const A, B: TQuaternion): TQuaternion;
begin
  Result.X := A.W * B.X + A.X * B.W + A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.W * B.Y - A.X * B.Z + A.Y * B.W + A.Z * B.X;
  Result.Z := A.W * B.Z + A.X * B.Y - A.Y * B.X + A.Z * B.W;
  Result.W := A.W * B.W - A.X * B.X - A.Y * B.Y - A.Z * B.Z;
	Result := QuaternionNormalize(Result);
end;

function QuaternionNormalize(const Q: TQuaternion): TQuaternion;
var
  Ratio: Single;
begin
  Ratio := Sqrt(Q.W * Q.W + Q.X * Q.X + Q.Y * Q.Y + Q.Z * Q.Z);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    Result.W := Q.W * Ratio;
    Result.X := Q.X * Ratio;
    Result.Y := Q.Y * Ratio;
    Result.Z := Q.Z * Ratio;
  end
  else
    Result := StockQuaternion;
end;

{ Vector routines }

function VectorDotProduct(const A, B: TVector): Single;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function VectorCrossProduct(const A, B: TVector): TVector;
begin
  Result.X := (A.Y * B.Z) - (B.Y * A.Z);
  Result.Y := (A.Z * B.X) - (B.Z * A.X);
  Result.Z := (A.X * B.Y) - (B.X * A.Y);
end;

function VectorNormalize(const V: TVector): TVector;
var
  Ratio: Single;
begin
  Ratio := Sqrt(V.X * V.X + V.Y * V.Y + V.Z * V.Z);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    Result.X := V.X * Ratio;
    Result.Y := V.Y * Ratio;
    Result.Z := V.Z * Ratio;
  end
  else
    Result := StockVertex;
end;

{ Vertex routines }

function VertexDistance(const A, B: TVertexPoint): Single;
var
  C: TVertexPoint;
begin
  C.X := A.X - B.X;
  C.Y := A.Y - B.Y;
  C.Z := A.Z - B.Z;
  with C do Result := Sqrt(X * X + Y * Y + Z * Z);
end;

function VertexDistance(const A: TVertexPoint): Single;
begin
  with A do Result := Sqrt(X * X + Y * Y + Z * Z);
end;

function VertexAdd(const A, B: TVertexPoint): TVertexPoint;
begin
  with Result do
  begin
    X := A.X + B.X;
    Y := A.Y + B.Y;
    Z := A.Z + B.Z;
  end;
end;

function VertexSubtract(const A, B: TVertexPoint): TVertexPoint;
begin
  with Result do
  begin
    X := A.X - B.X;
    Y := A.Y - B.Y;
    Z := A.Z - B.Z;
  end;
end;

function VertexRotate(const V: TVertexPoint; const Direction: TDirection): TVertexPoint;
var
  Radians: Single;
  A, B: Single;
begin
  Result := V;
  with Result do
  begin
    A := X;
    B := Y;
    Radians := Direction.Roll * (PI / 180);
    X := (A * Cos(Radians)) - (B * Sin(Radians));
    Y := (A * Sin(Radians)) + (B * Cos(Radians));
    A := Y;
    B := Z;
    Radians := Direction.Pitch * (PI / 180);
    Y := (A * Cos(Radians)) - (B * Sin(Radians));
    Z := (A * Sin(Radians)) + (B * Cos(Radians));
    A := X;
    B := Z;
    Radians := Direction.Heading * (PI / 180);
    X := (A * Cos(Radians)) - (B * Sin(Radians));
    Z := (A * Sin(Radians)) + (B * Cos(Radians));
  end;
end;

function VertexScale(const A: TVertexPoint; Factor: Single): TVertexPoint;
begin
  with Result do
  begin
    X := A.X * Factor;
    Y := A.Y * Factor;
    Z := A.Z * Factor;
  end;
end;

function VertexScale(const A: TVertexPoint; const Factors: TVertexPoint): TVertexPoint;
begin
  with Result do
  begin
    X := A.X * Factors.X;
    Y := A.Y * Factors.Y;
    Z := A.Z * Factors.Z;
  end;
end;

function IsVertexEqual(const A, B: TVertexPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

function PointLineDistance(const Point: TVertexPoint; const Line: TVertexLine): Single;
const
	MinDistance = 0.0000000001;
var
  V: TVertexPoint;
  D: Single;
begin
  V := VertexSubtract(Line.A, Line.B);
  D := VertexDistance(V);
  if D < MinDistance then
    Result := VertexDistance(VertexSubtract(Point, Line.A))
  else
  begin
    V := VectorCrossProduct(VertexSubtract(Line.A, Point), V);
    Result := VertexDistance(V) / D;
  end;
end;

function PointSegmentDistance(const Point: TVertexPoint;
	const Line: TVertexLine): Single;
begin
  if VectorDotProduct(VertexSubtract(Line.A, Point),
  	VertexSubtract(Line.A, Line.B)) < 0 then
    Result:= VertexDistance(VertexSubtract(Line.A, Point))
  else if VectorDotProduct(VertexSubtract(Line.B, Point),
  	VertexSubtract(Line.B, Line.A)) < 0 then
    Result := VertexDistance(VertexSubtract(Line.A, Point))
  else
    Result := PointLineDistance(Point, Line);
end;

{ Plane routines }

function PlaneAdd(const Plane: TPlane; X, Y, Z: Single): TPlane;
var
  I: Integer;
begin
  Result := Plane;
  for I := Low(Result) to High(Result) do
  begin
    Result[I].X := Result[I].X + X;
    Result[I].Y := Result[I].Y + Y;
    Result[I].Z := Result[I].Z + Z;
  end;
end;

function PlaneAdd(const Plane: TPlane; const V: TVertexPoint): TPlane;
begin
  Result := PlaneAdd(Plane, V.X, V.Y, V.Z);
end;

procedure PlaneDraw(const Plane: TPlane);
begin
  glTexCoord(0, 0);
  glVertex3f(Plane[0].X, Plane[0].Y, Plane[0].Z);
  glTexCoord(0, 1);
  glVertex3f(Plane[1].X, Plane[1].Y, Plane[1].Z);
  glTexCoord(1, 1);
  glVertex3f(Plane[2].X, Plane[2].Y, Plane[2].Z);
  glTexCoord(1, 0);
  glVertex3f(Plane[3].X, Plane[3].Y, Plane[3].Z);
end;

function PlaneRotate(const Plane: TPlane; const Direction: TDirection): TPlane;
begin
  Result[0] := VertexRotate(Result[0], Direction);
  Result[1] := VertexRotate(Result[1], Direction);
  Result[2] := VertexRotate(Result[2], Direction);
  Result[3] := VertexRotate(Result[3], Direction);
end;

function PlaneScale(const Plane: TPlane; Factor: Single): TPlane;
var
  I: Integer;
begin
  Result := Plane;
  for I := Low(Result) to High(Result) do
  begin
    Result[I].X := Result[I].X * Factor;
    Result[I].Y := Result[I].Y * Factor;
    Result[I].Z := Result[I].Z * Factor;
  end;
end;

function PolygonCenter(const Polygon: TPolygon): TVertexPoint;
var
  I: Integer;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  for I := 0 to Polygon.VertexCount - 1 do
  begin
    Result.X := Result.X + Polygon.Vertices[I].X;
    Result.Y := Result.Y + Polygon.Vertices[I].Y;
    Result.Z := Result.Z + Polygon.Vertices[I].Z;
  end;
  if Polygon.VertexCount > 0 then
  begin
    Result.X := Result.X / Polygon.VertexCount;
    Result.Y := Result.Y / Polygon.VertexCount;
    Result.Z := Result.Z / Polygon.VertexCount;
  end;
end;

function PolygonNormal(const Polygon: TPolygon): TVertexPoint;
var
  A, B: TVertexPoint;
  C: Single;
begin
  with Polygon do
    if VertexCount = 3 then
    begin
      A.X := Vertices[2].X - Vertices[1].X;
      A.Y := Vertices[2].Y - Vertices[1].Y;
      A.Z := Vertices[2].Z - Vertices[1].Z;
      B.X := Vertices[0].X - Vertices[1].X;
      B.Y := Vertices[0].Y - Vertices[1].Y;
      B.Z := Vertices[0].Z - Vertices[1].Z;
    end
    else
    begin
      A.X := Vertices[2].X - Vertices[0].X;
      A.Y := Vertices[2].Y - Vertices[0].Y;
      A.Z := Vertices[2].Z - Vertices[0].Z;
      B.X := Vertices[3].X - Vertices[1].X;
      B.Y := Vertices[3].Y - Vertices[1].Y;
      B.Z := Vertices[3].Z - Vertices[1].Z;
    end;
  with Result do
  begin
    X := A.Y * B.Z - A.Z * B.Y;
    Y := A.Z * B.X - A.X * B.Z;
    Z := A.X * B.Y - A.Y * B.X;
    { Adjust distance of result to 1 }
    C := Sqrt(X * X + Y * Y + Z * Z);
    if C > 0 then
    begin
      X := X / C;
      Y := Y / C;
      Z := Z / C;
    end;
  end;
end;

function QuadWidth(const Quad: TQuad): Single; overload;
begin
  Result := Quad[2] - Quad[0];
end;

function QuadWidth(const Quads: array of TQuad): Single; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Quads) to High(Quads) do
    Result := Result + Quads[I, 2] - Quads[I, 0];
end;

function QuadHeight(const Quad: TQuad): Single; overload;
begin
  Result := Quad[3] - Quad[1];
end;

function QuadHeight(const Quads: array of TQuad): Single; overload;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Quads) to High(Quads) do
    Result := Result + Quads[I, 3] - Quads[I, 1];
end;

function QuadDraw(const Quad: TQuad; X, Y: Single; Width: Single = 0;
  Height: Single = 0): TFloatPoint; overload;
begin
  if Width = 0 then Result.X := X + Quad[2] - Quad[0] else Result.X := X + Width;
  if Height = 0 then Result.Y := Y + Quad[3] - Quad[1] else Result.Y := Y + Height;
  glBegin(GL_QUADS);
  glTexCoord2f(Quad[0] / 256, Quad[1] / 256);
  glVertex3f(X, Y, 0);
  glTexCoord2f(Quad[0] / 256, Quad[3] / 256);
  glVertex3f(X, Result.Y, 0);
  glTexCoord2f(Quad[2] / 256, Quad[3] / 256);
  glVertex3f(Result.X, Result.Y, 0);
  glTexCoord2f(Quad[2] / 256, Quad[1] / 256);
  glVertex3f(Result.X, Y, 0);
  glEnd;
end;

function QuadDraw(const Quad: TQuad; Position: TFloatPoint;
  Width: Single = 0; Height: Single = 0): TFloatPoint; overload;
begin
  with Position do
    Result := QuadDraw(Quad, X, Y, Width, Height);
end;

{ Device space coordinate translation routines }

var
  DeviceContext: HDC;

function InitDevice(DC: HDC): HDC;
begin
  Result := DeviceContext;
  DeviceContext := DC;
end;

procedure FreeDevice(DC: HDC);
begin
  DeviceContext := DC;
end;

function IntToDevice(Value: Integer; Angle: Integer = 0): Single;
var
  Ratio: Single;
begin
  case Angle of
    0:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Ratio := 0;
  end;
  Result := Value * Ratio;
end;

function PointToDevice(const Point: TPoint): TFloatPoint;
begin
  Result.x := (Point.x + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Point.y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function PointToDevice(X, Y: Integer): TFloatPoint;
begin
  Result.x := (X + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(const Rect: TRect): TFloatRect;
begin
  Result.Left := (Rect.Left + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (Rect.Top + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (Rect.Right + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (Rect.Bottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect;
begin
  Result.Left := (ALeft + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (ATop + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (ARight + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (ABottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function DeviceToInt(const Value: Single; Angle: Integer = 0): Integer;
var
  Delta: Integer;
  Ratio: Single;
begin
  case Angle of
    0:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Delta := 0;
    Ratio := 0;
  end;
  Result := Trunc(Value * Ratio) - Delta;
end;

function DeviceToPoint(const Point: TFloatPoint): TPoint;
begin
  Result.x := Trunc(Point.x * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Point.y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToPoint(const X, Y: Single): TPoint;
begin
  Result.x := Trunc(X * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const Rect: TFloatRect): TRect;
begin
  Result.Left := Trunc(Rect.Left * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(Rect.Top * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(Rect.Right * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(Rect.Bottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const ALeft, ATop, ARight, ABottom: Single): TRect;
begin
  Result.Left := Trunc(ALeft * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(ATop * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(ARight * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(ABottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

procedure OffsetRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure OffsetRect(var Rect: TFloatRect; const X, Y: Single);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TFloatRect; const X, Y: Single);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function HeightOf(const Rect: TFloatRect): Single;
begin
  Result := Rect.Bottom - Rect.Bottom;
end;


function HeightOf(const Quad: TQuad): Single;
begin
  Result := Quad[3] - Quad[1];
end;

function HeightOf(const Quads: array of TQuad): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Quads) to High(Quads) do
    Result := Result + Quads[I, 3] - Quads[I, 1];
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function WidthOf(const Rect: TFloatRect): Single;
begin
  Result := Rect.Right - Rect.Left;
end;

function WidthOf(const Quad: TQuad): Single;
begin
  Result := Quad[2] - Quad[0];
end;

function WidthOf(const Quads: array of TQuad): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(Quads) to High(Quads) do
    Result := Result + Quads[I, 2] - Quads[I, 0];
end;

procedure Slide(var Rect: TRect; Orientation: TOrientation = orDown;
  Distance: Integer = 0);
begin
  case Orientation of
    orLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    orUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    orRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    orDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

procedure Slide(var Rect: TFloatRect; Orientation: TOrientation = orDown;
  Distance: Single = 0);
begin
  case Orientation of
    orLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    orUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    orRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    orDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

{ Draw routines }

procedure DrawNormal(const Polygon: TPolygon);
var
  A, B: TVertexPoint;
begin
  A := PolygonCenter(Polygon);
  B := PolygonNormal(Polygon);
  B.X := -B.X * NormalScale;
  B.Y := -B.Y * NormalScale;
  B.Z := -B.Z * NormalScale;
  B := VertexAdd(A, B);
  glVertex3f(A.X, A.Y, A.Z);
  glVertex3f(B.X, B.Y, B.Z);
end;

procedure DrawPolygon(const Polygon: TPolygon);
var
  I: Integer;
begin
  with PolygonNormal(Polygon) do
    glNormal3d(X, Y, Z);
  for I := 0 to Polygon.VertexCount - 1 do
    with Polygon.Vertices[I].Point do glVertex3f(X, Y, Z);
end;

procedure FlashColor(Red, Green, Blue, Alpha: Single);
var
  Textured: GLBoolean;
  Color: TFloatColor;
begin
  glGetBooleanv(GL_TEXTURE_2D, @Textured);
  if Textured then glDisable(GL_TEXTURE_2D);
  glGetFloatv(GL_CURRENT_COLOR, @Color);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glOrtho(0, 1, 1, 0, -1, 1);
  glBegin(GL_QUADS);
  glColor4f(Red, Green, Blue, Alpha);
  glVertex2f(0, 0);
  glVertex2f(0, 1);
  glVertex2f(1, 1);
  glVertex2f(1, 0);
  glEnd;
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_TEXTURE_2D);
  glPopMatrix;
  glClear(GL_DEPTH_BUFFER_BIT);
  glColor4fv(@Color);
  if Textured then glEnable(GL_TEXTURE_2D);
end;

procedure DrawGrid(Width, Length: Integer);
var
  W, L: Integer;
begin
  glBegin(GL_LINES);
  //glTranslate(Width / 2, 0, Length / 2);
  for W := 0 to Width do
    for L := 0 to Length do
    begin
      glVertex3f(W, 0, L);
      glVertex3f(W, 0, L + 1);
    end;
  for L := 0 to Length do
    for W := 0 to Width do
    begin
      glVertex3f(W, 0, L);
      glVertex3f(W + 1, 0, L);
    end;
  glEnd;
end;

procedure DrawLetterBox(Percent: Single);
var
  Textured: GLBoolean;
  Color: TFloatColor;
begin
  if Percent < 0 then
    Percent := 0
  else if Percent > 1 then
    Percent := 1;
  glGetBooleanv(GL_TEXTURE_2D, @Textured);
  if Textured then glDisable(GL_TEXTURE_2D);
  glGetFloatv(GL_CURRENT_COLOR, @Color);
  glxBeginOrtho(640, 480);
  glBegin(GL_QUADS);
  glColor4f(0, 0, 0, 1);
  glVertex2f(0, 0);
  glVertex2f(0, 480 * Percent / 2);
  glVertex2f(640 * 2, 480 * Percent  / 2);
  glVertex2f(640 * 2, 0);
  glVertex2f(0, 480 - 480 * Percent / 2);
  glVertex2f(0, 480);
  glVertex2f(640 * 2, 480);
  glVertex2f(640 * 2, 480 - 480 * Percent / 2);
  glEnd;
  glxEndOrtho;
  glClear(GL_DEPTH_BUFFER_BIT);
  glColor4fv(@Color);
  if Textured then glEnable(GL_TEXTURE_2D);
end;

procedure DrawClub(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
const
  ClubPoints: array[0..60] of TFloatPoint = (
    (X: 0.0000; Y: -0.4863), (X: 0.0863; Y: -0.4863),
    (X: 0.0700; Y: -0.4613), (X: 0.0600; Y: -0.4400),
    (X: 0.0513; Y: -0.4163), (X: 0.0425; Y: -0.3900),
    (X: 0.0375; Y: -0.3613), (X: 0.0338; Y: -0.3075),
    (X: 0.0363; Y: -0.2400), (X: 0.0438; Y: -0.1850),
    (X: 0.1238; Y: -0.2475), (X: 0.1563; Y: -0.2750),
    (X: 0.1788; Y: -0.2938), (X: 0.1963; Y: -0.3075),
    (X: 0.2125; Y: -0.3163), (X: 0.2288; Y: -0.3238),
    (X: 0.2475; Y: -0.3313), (X: 0.2663; Y: -0.3350),
    (X: 0.2888; Y: -0.3400), (X: 0.3213; Y: -0.3413),
    (X: 0.3450; Y: -0.3363), (X: 0.3725; Y: -0.3275),
    (X: 0.3938; Y: -0.3163), (X: 0.4150; Y: -0.2988),
    (X: 0.4313; Y: -0.2813), (X: 0.4463; Y: -0.2575),
    (X: 0.4600; Y: -0.2300), (X: 0.4675; Y: -0.2025),
    (X: 0.4713; Y: -0.1763), (X: 0.4738; Y: -0.1275),
    (X: 0.4700; Y: -0.0713), (X: 0.4613; Y: -0.0400),
    (X: 0.4538; Y: -0.0175), (X: 0.4400; Y: 0.0100),
    (X: 0.4225; Y: 0.0388), (X: 0.4000; Y: 0.0613),
    (X: 0.3738; Y: 0.0788), (X: 0.3438; Y: 0.0888),
    (X: 0.2988; Y: 0.0938), (X: 0.2563; Y: 0.0900),
    (X: 0.2238; Y: 0.0813), (X: 0.1863; Y: 0.0700),
    (X: 0.0600; Y: 0.0325), (X: 0.1388; Y: 0.1225),
    (X: 0.1588; Y: 0.1463), (X: 0.1725; Y: 0.1725),
    (X: 0.1825; Y: 0.1988), (X: 0.1925; Y: 0.2300),
    (X: 0.2000; Y: 0.2650), (X: 0.2038; Y: 0.3063),
    (X: 0.1975; Y: 0.3488), (X: 0.1800; Y: 0.3888),
    (X: 0.1488; Y: 0.4288), (X: 0.1225; Y: 0.4513),
    (X: 0.0988; Y: 0.4650), (X: 0.0713; Y: 0.4738),
    (X: 0.0463; Y: 0.4813), (X: 0.0225; Y: 0.4850),
    (X: 0.0000; Y: 0.4850), (X: 0.0; Y: 0.2788),
    (X: 0.0; Y: -0.0875));
var
  I: Integer;
begin
  glPushMatrix;
  glTranslate(Center.X, Center.Y, Center.Z);
  glRotate(Direction.Pitch, 1.0, 0.0, 0.0);
  glRotate(Direction.Heading, 0.0, 1.0, 0.0);
  glRotate(Direction.Roll, 0.0, 0, 1.0);
  glBegin(GL_TRIANGLE_FAN);
  for I := 0 to 9 do
    glVertex2f(ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glVertex2f(ClubPoints[60].X * Size, ClubPoints[60].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(ClubPoints[60].X * Size, ClubPoints[60].Y * Size);
  for I := 9 to 42 do
    glVertex2f(ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glVertex2f(ClubPoints[59].X * Size, ClubPoints[59].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(ClubPoints[59].X * Size, ClubPoints[59].Y * Size);
  for I := 42 to 58 do
    glVertex2f(ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(ClubPoints[0].X * Size, ClubPoints[0].Y * Size);
  glVertex2f(-ClubPoints[60].X * Size, ClubPoints[60].Y * Size);
  for I := 9 downto 1 do
    glVertex2f(-ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(-ClubPoints[60].X * Size, ClubPoints[60].Y * Size);
  glVertex2f(-ClubPoints[59].X * Size, ClubPoints[59].Y * Size);
  for I := 42 downto 9 do
    glVertex2f(-ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(-ClubPoints[59].X * Size, ClubPoints[59].Y * Size);
  for I := 58 downto 42 do
    glVertex2f(-ClubPoints[I].X * Size, ClubPoints[I].Y * Size);
  glEnd;
  glPopMatrix;
end;

procedure DrawDiamond(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
const
  DiamondPoints: array[0..3] of TFloatPoint = (
    (X: 0.0; Y: -0.4813), (X: 0.3725; Y: -0.0),
    (X: 0.0; Y: 0.4813), (X: -0.3725; Y: -0.0));
var
  I: Integer;
begin
  glPushMatrix;
  glTranslate(Center.X, Center.Y, Center.Z);
  glRotate(Direction.Pitch, 1.0, 0.0, 0.0);
  glRotate(Direction.Heading, 0.0, 1.0, 0.0);
  glRotate(Direction.Roll, 0.0, 0, 1.0);
  glBegin(GL_POLYGON);
  for I := Low(DiamondPoints) to High(DiamondPoints) do
    glVertex2f(DiamondPoints[I].X * Size, DiamondPoints[I].Y * Size);
  glEnd;
  glPopMatrix;
end;

procedure DrawHeart(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
const
  HeartPoints: array[0..26] of TFloatPoint = (
    (X: 0.0; Y: -0.4750), (X: 0.3363; Y: -0.0538),
    (X: 0.4063; Y: 0.0400), (X: 0.4425; Y: 0.1063),
    (X: 0.4550; Y: 0.1413), (X: 0.4663; Y: 0.1788),
    (X: 0.4713; Y: 0.2050), (X: 0.4738; Y: 0.2313),
    (X: 0.4738; Y: 0.2650), (X: 0.4713; Y: 0.3100),
    (X: 0.4663; Y: 0.3313), (X: 0.4550; Y: 0.3675),
    (X: 0.4413; Y: 0.3938), (X: 0.4225; Y: 0.4175),
    (X: 0.4050; Y: 0.4325), (X: 0.3850; Y: 0.4488),
    (X: 0.3563; Y: 0.4650), (X: 0.3313; Y: 0.4725),
    (X: 0.3100; Y: 0.4788), (X: 0.2800; Y: 0.4838),
    (X: 0.2400; Y: 0.4838), (X: 0.2063; Y: 0.4775),
    (X: 0.1725; Y: 0.4663), (X: 0.1113; Y: 0.4300),
    (X: 0.0938; Y: 0.4125), (X: 0.0725; Y: 0.3863),
    (X: 0.0; Y: 0.2925));
var
  I: Integer;
begin
  glPushMatrix;
  glTranslate(Center.X, Center.Y, Center.Z);
  glRotate(Direction.Pitch, 1.0, 0.0, 0.0);
  glRotate(Direction.Heading, 0.0, 1.0, 0.0);
  glRotate(Direction.Roll, 0.0, 0, 1.0);
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(0, 0);
  for I := Low(HeartPoints) to High(HeartPoints) do
    glVertex2f(HeartPoints[I].X * Size, HeartPoints[I].Y * Size);
  for I := High(HeartPoints) - 1 downto Low(HeartPoints) do
    glVertex2f(-HeartPoints[I].X * Size, HeartPoints[I].Y * Size);
  glEnd;
  glPopMatrix;
end;

procedure DrawSpade(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
const
  SpadePoints: array[0..36] of TFloatPoint = (
    (X: 0.0000; Y: -0.4875), (X: 0.0838; Y: -0.4875),
    (X: 0.0700; Y: -0.4675), (X: 0.0575; Y: -0.4450),
    (X: 0.0475; Y: -0.4238), (X: 0.0363; Y: -0.3950),
    (X: 0.0300; Y: -0.3763), (X: 0.0238; Y: -0.3463),
    (X: 0.0200; Y: -0.3050), (X: 0.0250; Y: -0.2563),
    (X: 0.0313; Y: -0.2325), (X: 0.0450; Y: -0.2038),
    (X: 0.1238; Y: -0.3175), (X: 0.1400; Y: -0.3388),
    (X: 0.1563; Y: -0.3550), (X: 0.1688; Y: -0.3675),
    (X: 0.1825; Y: -0.3775), (X: 0.1975; Y: -0.3850),
    (X: 0.2150; Y: -0.3913), (X: 0.2375; Y: -0.3938),
    (X: 0.2575; Y: -0.3938), (X: 0.2813; Y: -0.3863),
    (X: 0.3050; Y: -0.3750), (X: 0.3213; Y: -0.3588),
    (X: 0.3363; Y: -0.3413), (X: 0.3488; Y: -0.3238),
    (X: 0.3625; Y: -0.2975), (X: 0.3713; Y: -0.2725),
    (X: 0.3775; Y: -0.2413), (X: 0.3800; Y: -0.1700),
    (X: 0.3713; Y: -0.1313), (X: 0.3638; Y: -0.1025),
    (X: 0.3538; Y: -0.0813), (X: 0.3400; Y: -0.0550),
    (X: 0.3175; Y: -0.0150), (X: 0.2938; Y: 0.0175),
    (X: 0.0000; Y: 0.4838));
var
  I: Integer;
begin
  glPushMatrix;
  glTranslate(Center.X, Center.Y, Center.Z);
  glRotate(Direction.Pitch, 1.0, 0.0, 0.0);
  glRotate(Direction.Heading, 0.0, 1.0, 0.0);
  glRotate(Direction.Roll, 0.0, 0, 1.0);
  glBegin(GL_TRIANGLES);
  glVertex2f(0, 0);
  glVertex2f(SpadePoints[0].X * Size, SpadePoints[0].Y * Size);
  glVertex2f(SpadePoints[8].X * Size, SpadePoints[8].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  for I := 0 to 8 do
    glVertex2f(SpadePoints[I].X * Size, SpadePoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(0, 0);
  for I := 8 to 36 do
    glVertex2f(SpadePoints[I].X * Size, SpadePoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLES);
  glVertex2f(-SpadePoints[0].X * Size, SpadePoints[0].Y * Size);
  glVertex2f(0, 0);
  glVertex2f(-SpadePoints[8].X * Size, SpadePoints[8].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(-SpadePoints[0].X * Size, SpadePoints[0].Y * Size);
  for I := 8 downto 0 do
    glVertex2f(-SpadePoints[I].X * Size, SpadePoints[I].Y * Size);
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(0, 0);
  for I := 36 downto 8 do
    glVertex2f(-SpadePoints[I].X * Size, SpadePoints[I].Y * Size);
  glEnd;
  glPopMatrix;
end;

procedure DrawStar(Size: Single; const Direction: TDirection; const Center: TVertexPoint);
const
  Star: array[0..29] of Single = (
    0.44966, 0.6189, 0.0, 2.0, -0.44966, 0.6189, -0.44966, 0.6189, -1.90211,
    0.61803, -0.72756, -0.2364, -0.72756, -0.2364, -1.17557, -1.61803, 0.0,
    -0.765, 0.0, -0.765, 1.17557, -1.61803, 0.72756, -0.2364, 0.72756, -0.2364,
    1.90211, 0.61803, 0.44966, 0.6189);
var
  I: Integer;
begin
  glPushMatrix;
  glTranslate(Center.X, Center.Y, Center.Z);
  glRotate(Direction.Pitch, 1.0, 0.0, 0.0);
  glRotate(Direction.Heading, 0.0, 1.0, 0.0);
  glRotate(Direction.Roll, 0.0, 0, 1.0);
  Size := Size * 0.25;
  glBegin(GL_TRIANGLES);
  for I := 0 to 14 do
    glVertex3f(Star[I * 2] * Size, Star[I * 2 + 1] * Size, 0);
  glEnd;
  glBegin(GL_POLYGON);
  for I := 0 to 4 do
    glVertex3f(Star[I * 6] * Size, Star[I * 6 + 1] * Size, 0);
  glEnd;
  glPopMatrix;
end;

procedure DrawCube(Size: Single);
begin
	DrawBox(Size, Size, Size);
end;

procedure DrawBox(W, H, D: Single);
begin
  W := W * 0.5;
  H := H * 0.5;
  D := D * 0.5;
  glBegin(GL_QUADS);
  glNormal3f (0, 1, 0);
  glVertex3f(-W, H, -D);
  glVertex3f(-W, H, D);
  glVertex3f(W, H, D);
  glVertex3f(W, H, -D);
  glNormal3f(-1, 0, 0);
  glVertex3f(-W, H, -D);
  glVertex3f(-W, -H, -D);
  glVertex3f(-W, -H, D);
  glVertex3f(-W, H, D);
  glNormal3f(0, 0, 1);
  glVertex3f(-W, H, D);
  glVertex3f(-W, -H, D);
  glVertex3f(W, -H, D);
  glVertex3f(W, H, D);
	glNormal3f(1, 0, 0);
  glVertex3f(W, H, D);
  glVertex3f(W, -H, D);
  glVertex3f(W, -H, -D);
  glVertex3f(W, H, -D);
  glNormal3f(0, 0, -1);
  glVertex3f(W, H, -D);
  glVertex3f(W, -H, -D);
  glVertex3f(-W, -H, -D);
  glVertex3f(-W, H, -D);
  glNormal3f(0, -1, 0);
  glVertex3f(W, -H, -D);
  glVertex3f(W, -H, D);
  glVertex3f(-W, -H, D);
  glVertex3f(-W, -H, -D);
  glEnd;
end;

procedure DrawWireBox(W, H, D: Single);
begin
  W := W * 0.5;
  H := H * 0.5;
  D := D * 0.5;
  glBegin(GL_LINE_LOOP);
  glNormal3f (0, 1, 0);
  glVertex3f(-W, H, -D);
  glVertex3f(-W, H, D);
  glVertex3f(W, H, D);
  glVertex3f(W, H, -D);
  glEnd;
  glBegin(GL_LINE_LOOP);
  glNormal3f(-1, 0, 0);
  glVertex3f(-W, H, -D);
  glVertex3f(-W, -H, -D);
  glVertex3f(-W, -H, D);
  glVertex3f(-W, H, D);
  glEnd;
  glBegin(GL_LINE_LOOP);
  glNormal3f(0, 0, 1);
  glVertex3f(-W, H, D);
  glVertex3f(-W, -H, D);
  glVertex3f(W, -H, D);
  glVertex3f(W, H, D);
  glEnd;
  glBegin(GL_LINE_LOOP);
	glNormal3f(1, 0, 0);
  glVertex3f(W, H, D);
  glVertex3f(W, -H, D);
  glVertex3f(W, -H, -D);
  glVertex3f(W, H, -D);
  glEnd;
  glBegin(GL_LINE_LOOP);
  glNormal3f(0, 0, -1);
  glVertex3f(W, H, -D);
  glVertex3f(W, -H, -D);
  glVertex3f(-W, -H, -D);
  glVertex3f(-W, H, -D);
  glEnd;
  glBegin(GL_LINE_LOOP);
  glNormal3f(0, -1, 0);
  glVertex3f(W, -H, -D);
  glVertex3f(W, -H, D);
  glVertex3f(-W, -H, D);
  glVertex3f(-W, -H, -D);
  glEnd;
end;

procedure DrawAxis(Scale: Single);
begin
	glBegin(GL_LINES);
	glColor3f(1, 0, 0);
  glVertex3f(0, 0, -1 * Scale);
  glVertex3f(0, 0, 1 * Scale);
	glColor3f(0, 1, 0);
  glVertex3f(0, -1 * Scale, 0);
  glVertex3f(0, 1 * Scale, 0);
	glColor3f(0, 0, 1);
  glVertex3f(-1 * Scale, 0, 0);
  glVertex3f(1 * Scale, 0, 0);
  glEnd;
end;

{ Extensions }

procedure glxMultMatrix(const M: TMatrix);
begin
	glMultMatrixf(PGLFloat(@M));
end;

procedure glxLoadMatrix(const M: TMatrix);
begin
	glLoadMatrixf(PGLFloat(@M));
end;

{ Texture routines }

function ReadBitmapInfo(const FileName: string): string;

  function StringFromBuffer(var Buffer; Size: Cardinal): string;
  begin
    Result := '';
    if Size > 0 then
    begin
      SetLength(Result, Size);
      Move(Buffer, PChar(Result)^, Size);
    end;
  end;

var
  Bitmap: file;
  FileHeader: TBitmapFileHeader;
  InfoHeader: TBitmapInfoHeader;
  BytesRead: Cardinal;
begin
  Result := '';
  AssignFile(Bitmap, FileName);
  Reset(Bitmap, 1);
  try
    BlockRead(Bitmap, FileHeader, SizeOf(FileHeader), BytesRead);
    if BytesRead <> SizeOf(FileHeader) then
      raise Exception.Create('File header mismatch');
    with FileHeader do
    begin
      Result := 'TBitmapFileHeader'#13#10;
      Result := Result + '  dfType: ' + StringFromBuffer(bfType, 2) + #13#10;
      Result := Result + '  bfSize: ' + IntToStr(bfSize) + #13#10;
      Result := Result + '  bfOffBits: ' + IntToStr(bfOffBits) + #13#10#13#10;
    end;
    BlockRead(Bitmap, InfoHeader, SizeOf(InfoHeader), BytesRead);
    if BytesRead <> SizeOf(InfoHeader) then
      raise Exception.Create('Info header mismatch');
    with InfoHeader do
    begin
      Result := Result + 'TBitmapInfoHeader' + #13#10;
      Result := Result + '  biSize: ' + IntToStr(biSize) + #13#10;
      Result := Result + '  biWidth: ' + IntToStr(biWidth) + #13#10;
      Result := Result + '  biHeight: ' + IntToStr(biHeight) + #13#10;
      Result := Result + '  biPlanes: ' + IntToStr(biPlanes) + #13#10;
      Result := Result + '  biBitCount: ' + IntToStr(biBitCount) + #13#10;
      Result := Result + '  biCompression: ' + IntToStr(biCompression) + #13#10;
      Result := Result + '  biSizeTexture: ' + IntToStr(biSizeImage) + #13#10;
      Result := Result + '  biXPelsPerMeter: ' + IntToStr(biXPelsPerMeter) + #13#10;
      Result := Result + '  biYPelsPerMeter: ' + IntToStr(biYPelsPerMeter) + #13#10;
      Result := Result + '  biClrUsed: ' + IntToStr(biClrUsed) + #13#10;
      Result := Result + '  biClrImportant: ' + IntToStr(biClrImportant) + #13#10;
    end;
  finally
    CloseFile(Bitmap);
  end;
end;

procedure MakeTrueBitmap(const ColorMap, AlphaMap, Output: string);
var
  ColorStream, AlphaStream, OutputStream: TStream;
  ColorFileHeader, AlphaFileHeader, OutputFileHeader: TBitmapFileHeader;
  ColorInfoHeader, AlphaInfoHeader, OutputInfoHeader: TBitmapInfoHeader;
  ColorTriple, AlphaTriple: TRGBTriple;
  OutputQuad: TRGBQuad;
  Count: Integer;
begin
  ColorStream := nil;
  AlphaStream := nil;
  OutputStream := nil;
  try
    ColorStream := TFileStream.Create(ColorMap, fmOpenRead);
    AlphaStream := TFileStream.Create(AlphaMap, fmOpenRead);
    OutputStream := TFileStream.Create(Output, fmCreate);
    if (ColorStream.Read(ColorFileHeader, SizeOf(ColorFileHeader)) = SizeOf(ColorFileHeader)) and
      (ColorFileHeader.bfType = $4D42) and (ColorFileHeader.bfOffBits = SizeOf(ColorFileHeader) +
      SizeOf(ColorInfoHeader)) and (ColorStream.Read(ColorInfoHeader, SizeOf(ColorInfoHeader)) =
      SizeOf(ColorInfoHeader)) and (ColorInfoHeader.biCompression = BI_RGB) and
      (ColorInfoHeader.biBitCount = 24) and
      (AlphaStream.Read(AlphaFileHeader, SizeOf(AlphaFileHeader)) = SizeOf(AlphaFileHeader)) and
      (AlphaFileHeader.bfType = $4D42) and (AlphaFileHeader.bfOffBits = SizeOf(AlphaFileHeader) +
      SizeOf(AlphaInfoHeader)) and (AlphaStream.Read(AlphaInfoHeader, SizeOf(AlphaInfoHeader)) =
      SizeOf(AlphaInfoHeader)) and (AlphaInfoHeader.biCompression = BI_RGB) and
      (AlphaInfoHeader.biBitCount = 24) and
      (ColorInfoHeader.biWidth = AlphaInfoHeader.biWidth) and
      (ColorInfoHeader.biHeight = AlphaInfoHeader.biHeight) then
    begin
      OutputFileHeader := ColorFileHeader;
      OutputInfoHeader := ColorInfoHeader;
      OutputInfoHeader.biBitCount := 32;
      Count := OutputInfoHeader.biWidth * OutputInfoHeader.biHeight;
      OutputFileHeader.bfSize := SizeOf(OutputFileHeader) + SizeOf(OutputInfoHeader) +
        Count * SizeOf(TRGBQuad);
      OutputStream.Write(OutputFileHeader, SizeOf(OutputFileHeader));
      OutputStream.Write(OutputInfoHeader, SizeOf(OutputInfoHeader));
      while Count > 0 do
      begin
        ColorStream.Read(ColorTriple, SizeOf(ColorTriple));
        AlphaStream.Read(AlphaTriple, SizeOf(AlphaTriple));
        OutputQuad.rgbBlue := ColorTriple.rgbtBlue;
        OutputQuad.rgbGreen := ColorTriple.rgbtGreen;
        OutputQuad.rgbRed := ColorTriple.rgbtRed;
        OutputQuad.rgbReserved := (AlphaTriple.rgbtBlue + AlphaTriple.rgbtGreen +
          AlphaTriple.rgbtRed) div 3;
        OutputStream.Write(OutputQuad, SizeOf(OutputQuad));
        Dec(Count);
      end;
    end;
  finally
    ColorStream.Free;
    AlphaStream.Free;
    OutputStream .Free;
  end;
end;

procedure ExtractAlphaBitmap(const AlphaMap, Output: string);
var
  AlphaStream, OutputStream: TStream;
  AlphaFileHeader, OutputFileHeader: TBitmapFileHeader;
  AlphaInfoHeader, OutputInfoHeader: TBitmapInfoHeader;
  AlphaQuad: TRGBQuad;
  OutputTriple: TRGBTriple;
  Count: Integer;
begin
  AlphaStream := nil;
  OutputStream := nil;
  try
    AlphaStream := TFileStream.Create(AlphaMap, fmOpenRead);
    OutputStream := TFileStream.Create(Output, fmCreate);
    if (AlphaStream.Read(AlphaFileHeader, SizeOf(AlphaFileHeader)) = SizeOf(AlphaFileHeader)) and
      (AlphaFileHeader.bfType = $4D42) and (AlphaFileHeader.bfOffBits = SizeOf(AlphaFileHeader) +
      SizeOf(AlphaInfoHeader)) and (AlphaStream.Read(AlphaInfoHeader, SizeOf(AlphaInfoHeader)) =
      SizeOf(AlphaInfoHeader)) and (AlphaInfoHeader.biCompression = BI_RGB) and
      (AlphaInfoHeader.biBitCount = 32) then
    begin
      OutputFileHeader := AlphaFileHeader;
      OutputInfoHeader := AlphaInfoHeader;
      OutputInfoHeader.biBitCount := 24;
      Count := OutputInfoHeader.biWidth * OutputInfoHeader.biHeight;
      OutputFileHeader.bfSize := SizeOf(OutputFileHeader) + SizeOf(OutputInfoHeader) +
        Count * SizeOf(TRGBTriple);
      OutputStream.Write(OutputFileHeader, SizeOf(OutputFileHeader));
      OutputStream.Write(OutputInfoHeader, SizeOf(OutputInfoHeader));
      while Count > 0 do
      begin
        AlphaStream.Read(AlphaQuad, SizeOf(AlphaQuad));
        OutputTriple.rgbtBlue := AlphaQuad.rgbReserved;
        OutputTriple.rgbtGreen := AlphaQuad.rgbReserved;
        OutputTriple.rgbtRed := AlphaQuad.rgbReserved;
        OutputStream.Write(OutputTriple, SizeOf(OutputTriple));
        Dec(Count);
      end;
    end;
  finally
    AlphaStream.Free;
    OutputStream .Free;
  end;
end;

type
  TTargaHeader = packed record
    IdentSize: Byte;          // size of ID field that follows 18 byte header (0 usually)
    ColorMapType: Byte;       // type of color map 0=none, 1=has palette
    ImageType: Byte;          // type of image 0=none,1=indexed,2=rgb,3=grey,+8=rle packed
    ColormapStart: Short;     // first color map entry in palette
    ColormapLength: Short;    // number of Colors in palette
    ColorMapBits: Byte;       // number of bits per palette entry 15,16,24,32
    xStart: Short;            // image x origin
    yStart: Short;            // image y origin
    Width: Short;             // image width in pixels
    Height: Short;            // image height in pixels
    Bits: Byte;               // image bits per pixel 8,16,24,32
    Descriptor: Byte;         // image descriptor bits (vh flip bits)
  end;                        // pixel data follows header

procedure ReverseScanLines(const TextureInfo: TTextureInfo);
var
  ScanWidth: Integer;
  ScanLine: Pointer;
  A, B: PByte;
begin
  ScanWidth := TextureInfo.Width * SizeOf(TRGBQuad);
  GetMem(ScanLine, ScanWidth);
  try
    A := TextureInfo.Bits;
    B := A;
    Inc(B, ScanWidth * (TextureInfo.Height - 1));
    while Cardinal(A) < Cardinal(B) do
    begin
      Move(A^, ScanLine^, ScanWidth);
      Move(B^, A^, ScanWidth);
      Move(ScanLine^, B^, ScanWidth);
      Inc(A, ScanWidth);
      Dec(B, ScanWidth);
    end;
  finally
    FreeMem(ScanLine);
  end;
end;

function LoadBmpTexture(Stream: TStream; Alpha: Boolean = False): TTextureInfo;

  procedure ReadHeaderBitCount(var BitCount: Integer);
  var
    Position: Longint;
    FileKind: Word;
    FileHeader: TBitmapFileHeader;
    InfoHeader: TBitmapInfoHeader;
    TargaHeader: TTargaHeader;
  begin
    Position := Stream.Position;
    Stream.Read(FileKind, SizeOf(FileKind));
    Stream.Position := Position;
    if (FileKind = $4D42) and (Stream.Read(FileHeader, SizeOf(FileHeader)) =
      SizeOf(FileHeader)) and (FileHeader.bfOffBits = SizeOf(FileHeader) +
      SizeOf(InfoHeader)) and (Stream.Read(InfoHeader, SizeOf(InfoHeader)) =
      SizeOf(InfoHeader)) and (InfoHeader.biCompression = BI_RGB) and
      (InfoHeader.biBitCount in [24, 32]) then
    begin
      Result.Width := InfoHeader.biWidth;
      Result.Height := InfoHeader.biHeight;
      BitCount := InfoHeader.biBitCount;
    end
    else if (Stream.Read(TargaHeader, SizeOf(TargaHeader)) = SizeOf(TargaHeader)) and
     (TargaHeader.ImageType = 2) and (TargaHeader.Bits in [24, 32]) then
    begin
      Result.Width := TargaHeader.Width;
      Result.Height := TargaHeader.Height;
      BitCount := TargaHeader.Bits;
    end
    else
      BitCount := 0;
  end;

var
  BitCount: Integer;
  Size: Integer;
  RGBQuad: PRGBQuad;
  ChunkSize: Integer;
  B: Byte;
begin
  FillChar(Result, SizeOf(Result), #0);
  ReadHeaderBitCount(BitCount);
  if BitCount <> 0 then
  begin
    Size := Result.Width * Result.Height * SizeOf(TRGBQuad);
    if TextureBits = nil then
      GetMem(Result.Bits, Size)
    else
      Result.Bits := TextureBits;
    try
      FillChar(Result.Bits^, Size, #0);
      RGBQuad := Result.Bits;
      if BitCount = 24 then
        ChunkSize := SizeOf(TRGBTriple)
      else
        ChunkSize := SizeOf(TRGBQuad);
      while Size > 0 do
      begin
        if Stream.Read(RGBQuad^, ChunkSize) <> ChunkSize then
          Break;
        with RGBQuad^ do
          if Alpha and (BitCount = 24) then
          begin
            rgbReserved := (rgbRed + rgbGreen + rgbBlue) div 3;
            rgbRed := $FF;
            rgbgreen := $FF;
            rgbBlue := $FF;
          end
          else
          begin
            B := rgbRed;
            rgbRed := rgbBlue;
            rgbBlue := B;
            if BitCount = 24 then
              rgbReserved := $FF;
          end;
        Inc(RGBQuad);
        Dec(Size, SizeOf(TRGBQuad));
      end;
      if Size = 0 then ReverseScanLines(Result);
    except
      if TextureBits = nil then
        FreeMem(Result.Bits);
      raise;
    end;
  end;
end;

{$IFDEF PNG}
function LoadPngTexture(Stream: TStream; Alpha: Boolean = False): TTextureInfo;
var
  Image: TPngImage;
  Size: Integer;
  Bits: PRGBQuad;
  Scanline: PRGBTriple;
  AlphaScanline: PByteArray;
  B: Byte;
  H, W: Integer;
begin
  Image := TPngImage.Create;
  try
    Image.LoadFromStream(Stream);
    FillChar(Result, SizeOf(Result), #0);
    Result.Width := Image.Width;
    Result.Height := Image.Height;
    Size := Result.Height * Result.Width * SizeOf(TRGBQuad);
    GetMem(Result.Bits, Size);
    try
      Bits := Result.Bits;
      for H := 0 to Result.Height - 1 do
      begin
        Scanline := Image.Scanline[H];
        AlphaScanline := Image.AlphaScanline[H];
        for W := 0 to Result.Width - 1 do
          with Bits^ do
          begin
            PRGBTriple(Bits)^ := Scanline^;
            B := rgbBlue;
            rgbBlue := rgbRed;
            rgbRed := B;
            if AlphaScanline <> nil then
              rgbReserved := AlphaScanline[W]
            else
              rgbReserved := $FF;
            if Alpha then
            begin
              rgbReserved := (rgbRed + rgbGreen + rgbBlue) div 3;
              rgbRed := $FF;
              rgbGreen := $FF;
              rgbBlue := $FF;
            end;
            Inc(Scanline);
            Inc(Bits);
          end;
      end;
    except
      if TextureBits = nil then
        FreeMem(Result.Bits);
      raise;
    end;
  finally
    Image.Free;
  end;
end;
{$ENDIF}

{$IFDEF JPEG}
function LoadJpegTexture(Stream: TStream; Alpha: Boolean = False): TTextureInfo;
var
  err: jpeg_error_mgr;
  cinfo: jpeg_decompress_struct;
  Bits: PRGBQuad;
  Scanline: PRGBTriple;
  B: Byte;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  err := jpeg_error;
  cinfo.common.err := @err;
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION, SizeOf(cinfo));
  try
    jpeg_stdio_src(cinfo, Stream);
    jpeg_read_header(cinfo, True);
    Result.Width := cinfo.image_width;
    Result.Height := cinfo.image_height;
    GetMem(Result.Bits, Result.Height * Result.Width * SizeOf(TRGBQuad));
    try
      Bits := Result.Bits;
      GetMem(Scanline, Result.Width * SizeOf(TRGBTriple));
      try
        {cinfo.scale_num := 1;
        cinfo.scale_denom := 1;
        cinfo.dct_method := JDCT_ISLOW;}
        jpeg_start_decompress(cinfo);
        try
          while (cinfo.output_scanline < cinfo.output_height) do
          begin
            jpeg_read_scanlines(cinfo, @Scanline, 1);
            for I := 0 to Result.Width - 1 do
              with Bits^ do
              begin
                PRGBTriple(Bits)^ := Scanline^;
                if Alpha then
                begin
                  rgbReserved := (rgbRed + rgbGreen + rgbBlue) div 3;
                  rgbRed := $FF;
                  rgbGreen := $FF;
                  rgbBlue := $FF;
                end
                else
                begin
                  B := rgbRed;
                  rgbRed := rgbBlue;
                  rgbBlue := B;
                  rgbReserved := $FF;
                end;
                Inc(Bits);
                Inc(Scanline);
              end;
            Dec(Scanline, Result.Width);
          end;
        finally
          jpeg_finish_decompress(cinfo);
        end;
      finally
        FreeMem(Scanline);
      end;
    except
      if TextureBits = nil then
        FreeMem(Result.Bits);
      raise;
    end;
  finally
    jpeg_destroy_decompress(cinfo);
  end;
end;
{$ENDIF}

function ExtractTextureFormat(const FileName: string): TTextureFormat;
var
  S: string;
begin
  S := UpperCase(ExtractFileExt(FileName));
  Result := tfBitmap;
  if S = '.TGA' then
    Result := tfTarga
  {$IFDEF PNG}
  else if S = '.PNG' then
    Result := tfPng
  {$ENDIF}
  {$IFDEF JPEG}
  else if S = '.JPG' then
    Result := tfJpeg
  else if S = '.JPEG' then
    Result := tfJpeg
  {$ENDIF}
end;

function LoadTexture(const FileName: string; Alpha: Boolean = False): TTextureInfo;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    case ExtractTextureFormat(FileName) of
      tfBitmap, tfTarga: Result := LoadBmpTexture(Stream, Alpha);
      {$IFDEF PNG}
      tfPng:  Result := LoadPngTexture(Stream, Alpha);
      {$ENDIF}
      {$IFDEF JPEG}
      tfJpeg: Result := LoadJpegTexture(Stream, Alpha);
      {$ENDIF}
    end;
  finally
    Stream.Free;
  end;
end;

function LoadTexture(Stream: TStream; Format: TTextureFormat; Alpha: Boolean = False): TTextureInfo;
begin
  case Format of
    tfBitmap, tfTarga: Result := LoadBmpTexture(Stream, Alpha);
    {$IFDEF PNG}
    tfPng: Result := LoadPngTexture(Stream, Alpha);
    {$ENDIF}
    {$IFDEF JPEG}
    tfJpeg: Result := LoadJpegTexture(Stream, Alpha);
    {$ENDIF}
  end;
end;

function BindTexture(const FileName: string; Texture: GLuint; Alpha: Boolean = False): Boolean;
var
  Info: TTextureInfo;
begin
  Info := LoadTexture(FileName, Alpha);
  try
    Result := BindTexture(Info, Texture);
  finally
    DisposeTexture(Info);
  end;
end;

function BindTexture(Stream: TStream; Texture: GLuint; Format: TTextureFormat;
  Alpha: Boolean = False): Boolean;
var
  Info: TTextureInfo;
begin
  Info := LoadTexture(Stream, Format, Alpha);
  try
    Result := BindTexture(Info, Texture);
  finally
    DisposeTexture(Info);
  end;
end;

function BindTexture(const TextureInfo: TTextureInfo; Texture: GLuint): Boolean;
begin
  Result := TextureInfo.Bits <> nil;
  if Result then
  begin
    glBindTexture(GL_TEXTURE_2D, Texture);
    glGetError;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TextureInfo.Width, TextureInfo.Height,
      0, GL_RGBA, GL_UNSIGNED_BYTE, TextureInfo.Bits);
    Result := glGetError = GL_NO_ERROR;
    if Result then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
  end;
end;

function IsTextureEmpty(const TextureInfo: TTextureInfo): Boolean;
begin
  Result := (TextureInfo.Width < 1) or (TextureInfo.Height < 1);
end;

function IsTextureSquare(const TextureInfo: TTextureInfo): Boolean;
var
  Bits: Integer;
begin
  if (TextureInfo.Width < 1) or (TextureInfo.Height < 1) then
    Result := False
  else if TextureInfo.Width <> TextureInfo.Height then
    Result := False
  else
  begin
    Bits := TextureInfo.Width;
    while Bits <> 1 do
    begin
      Bits := Bits shr 1;
      if Bits = 0 then Break;
      if Bits and 1 = 1 then Break;
    end;
    Result := Bits = 1;
  end;
end;

procedure ClearTexture(Size: Cardinal; Texture: GLuint);
var
  Data: Pointer;
begin
  Data := AllocMem(Size * Size * SizeOf(TRGBQuad));
  try
    glBindTexture(GL_TEXTURE_2D, Texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Size, Size,
      0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  finally
    FreeMem(Data);
  end;
end;

procedure DisposeTexture(const TextureInfo: TTextureInfo);
begin
  if (TextureInfo.Bits <> nil) and (TextureInfo.Bits <> TextureBits) then
    FreeMem(TextureInfo.Bits);
end;

procedure ClampTexture(Texture: GLuint; MinFilter: GLuint = GL_NEAREST;
  MagFilter: GLuint = GL_NEAREST);
begin
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, MinFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER , MagFilter);
end;

(*

type
  TMatrix = array[0..3, 0..3] of Single;
  PMatrix = ^Matrix;

  Vertex = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
  PVertex = ^Vertex;

  Vector = Vertex;
  PVector = PVertex;

const
  StockMatrix: TMatrix = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));
  StockVertex: TVertex = (X: 0; Y: 0; Z: 0);

function MatrixToStr(const M: Matrix): string;
var
  X, Y: Integer;
begin
  Result := '';
  for Y := 0 to 3 do
  begin
    for X := 0 to 3 do
      Result := Result + Format('%.2f', [M[X, Y]]) + '  ';
    Result := Result + #13#10;
  end;
end;

function MatrixAdd(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] + B[X, Y];
end;

function MatrixSubtract(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] - B[X, Y];
end;

function MatrixMultiply(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] * B[X, 0] + A[1, Y] * B[X, 1] + A[2, Y] *
      B[X, 2] + A[3, Y] * B[X, 3];
end;

function MatrixDivide(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] / B[X, 0] + A[1, Y] / B[X, 1] + A[2, Y] / B[X, 2];
end;

function MatrixScale(const M: TMatrix; X, Y, Z: Single): TMatrix;
var
  S: Matrix;
begin
  S := StockMatrix;
  S[0, 0] := X;
  S[1, 1] := Y;
  S[2, 2] := Z;
  Result := MatrixMultiply(M, S);
end;

function MatrixTranslate(const M: Matrix; X, Y, Z: Single): TMatrix;
var
  T: Matrix;
begin
  T := StockMatrix;
  T[3, 0] := X;
  T[3, 1] := Y;
  T[3, 2] := Z;
  Result := MatrixMultiply(M, T);
end;

function MatrixRotate(const M: TMatrix; X, Y, Z: Single): TMatrix;
var
  R: Matrix;
begin
  Result := M;
  X := X * (PI / 180);
  if X <> 0 then
  begin
    R := StockMatrix;
    R[1, 1] := Cos(X);
    R[1, 2] := Sin(X);
    R[2, 1] := -R[1, 2];
    R[2, 2] := R[1, 1];
    Result := MatrixMultiply(Result, R);
  end;
  Y := Y * (PI / 180);
  if Y <> 0 then
  begin
    R := StockMatrix;
    R[0, 0] := Cos(Y);
    R[2, 0] := Sin(Y);
    R[0, 2] := -R[2, 0];
    R[2, 2] := R[0, 0];
    Result := MatrixMultiply(Result, R);
  end;
  Z := Z * (PI / 180);
  if Z <> 0 then
  begin
    R := StockMatrix;
    R[0, 0] := Cos(Z);
    R[1, 0] := Sin(Z);
    R[0, 1] := -R[1, 0];
    R[1, 1] := R[0, 0];
    Result := MatrixMultiply(Result, R);
  end;
end;

function MatrixTransform(const M: Matrix; const V: Vertex): TVertex;
begin
  Result.X := M[0, 0] * V.X + M[1, 0] * V.Y + M[2, 0] * V.Z + M[3, 0];
  Result.Y := M[0, 1] * V.X + M[1, 1] * V.Y + M[2, 1] * V.Z + M[3, 1];
  Result.Z := M[0, 2] * V.X + M[1, 2] * V.Y + M[2, 2] * V.Z + M[3, 2];
end;

function CreateVertex(X, Y, Z: Single): TVertex;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function VertexToStr(const TV: Vertex): string;
begin
  Result := Format('%.2f %.2f %.2f', [V. X, V.Y, V.Z]);
end;

function CreateVector(X, Y, Z: Single): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function VectorDotProduct(const A, B: TVector): Single;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function VectorCrossProduct(const A, B: TVector): TVector;
begin
  Result.X := (A.Y * B.Z) - (B.Y * A.Z);
  Result.Y := (A.Z * B.X) - (B.Z * A.X);
  Result.Z := (A.X * B.Y) - (B.X * A.Y);
end;

function VectorNormalize(const V: TVector): TVector;
var
  Ratio: Single;
begin
  Ratio := Sqrt(V.X * V.X + V.Y * V.Y + V.Z * V.Z);
  if Ratio <> 0 then
  begin
    Ratio := 1 / Ratio;
    Result.X := V.X * Ratio;
    Result.Y := V.Y * Ratio;
    Result.Z := V.Z * Ratio;
  end
  else
    Result := StockVertex;
end;

*)

end.

