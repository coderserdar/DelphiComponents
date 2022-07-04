
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit MathTools;

interface

{$I STD.INC}

uses
  Windows, SysUtils, StrTools;

{ Unit type conversion routines }

function FeetToMeters(const Value: Double): Double;
function MetersToFeet(const Value: Double): Double;
function FeetToMetersSquared(const Value: Double): Double;
function MetersToFeetSquared(const Value: Double): Double;
function FeetToMetersCubed(const Value: Double): Double;
function MetersToFeetCubed(const Value: Double): Double;
function CelsiusToFahrenheit(const Value: Double): Double;
function FahrenheitToCelsius(const Value: Double): Double;

{ Testing and setting empty memory routines }

function IsEmpty(var Data; DataSize: Integer): Boolean;
procedure SetEmpty(var Data; DataSize: Integer);

{ Floating point versions of the "div" and "mod" functions }

function Divide(const Quotient, Divisor: Double): Double;
function Remainder(const Quotient, Divisor: Double): Double;

function IntegerPart(const Value: Double): Double;
function FractionalPart(const Value: Double): Double;

{ Primitive datatypes }

type
  TIntegers = array of Integer;

  TPoints = array of TPoint;
  TPolygon = type TPoints;
  TRectangle = array[0..3] of TPoint;

  TFloatPoint = record
    X: Double;
    Y: Double;
  end;
  PFloatPoint = ^TFloatPoint;

  TFloatRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;

  TFloatPoints = array of TFloatPoint;
  TFloatPolygon = type TFloatPoints;
  TFloatRectangle = array[0..3] of TFloatPoint;

{ Primitive datatype construction routines }

function GetPoint(X, Y: Integer): TPoint;
function GetRect(ALeft, ATop, ARight, ABottom: Integer): TRect; overload;
function GetRect(const Rectangle: TRectangle): TRect; overload;
function GetPolygon(const Points: array of Integer): TPolygon;
function GetFloatPoint(const X, Y: Double): TFloatPoint;
function GetFloatRect(const ALeft, ATop, ARight, ABottom: Double): TFloatRect;
function GetFloatPolygon(const Points: array of Double): TFloatPolygon;

function PolygonToText(Polygon: TFloatPolygon): string;
function TextToPolygon(const S: string): TFloatPolygon;

{ Primitive datatype manipulations and inqury routines }

function Angle(const A: TPoint; const B: TPoint): Double; overload;
function Angle(const A: TFloatPoint; const B: TFloatPoint): Double; overload;
function Area(const Polygon: TPolygon): Double; overload;
function Area(const Polygon: TFloatPolygon): Double; overload;
function Center(Rect: TRect): TPoint; overload;
function Center(Rect: TFloatRect): TFloatPoint; overload;
function Distance(const A: TPoint; const B: TPoint): Double; overload;
function Distance(const A: TFloatPoint; const B: TFloatPoint): Double; overload;
procedure Offset(var Point: TPoint; X: Integer; Y: Integer); overload;
procedure Offset(var Polygon: TPolygon; X: Integer; Y: Integer); overload;
procedure Offset(var Polygon: TFloatPolygon; X: Double; Y: Double); overload;
function Rotate(const Distance: Double; const Angle: Double): TFloatPoint; overload;
function Rotate(const Point: TPoint; const Angle: Double): TPoint; overload;
function Rotate(const Point: TFloatPoint; const Angle: Double): TFloatPoint; overload;
function Rotate(const Points: TPoints; const Angle: Double): TPoints; overload;
function Rotate(Rect: TRect; const A: TPoint; const B: TPoint): TRectangle; overload;
function Scale(Polygon: TPolygon; Factor: Double): TPolygon; overload;
function Scale(Polygon: TFloatPolygon; Factor: Double): TFloatPolygon; overload;

{ TODO: Code these routines

  procedure Scale
  procedure  Mirror
  procedure Inflate }

{ Primitive datatype hit testings routines }

function PointInPolygon(const Point: TPoint; const Polygon: TPolygon): Boolean; overload;
function PointInPolygon(const Point: TFloatPoint; const Polygon: TFloatPolygon): Boolean; overload;
function RectInRect(const A: TRect; const B: TRect): Boolean;

function GCD(A, B: Integer): Integer;
function Factorial(I: Integer): Double;
function Combin(Range, Count: Integer): Double;

{ Complex Numbers }

type
   TComplex = record
     r: Double;
     i: Double;
   end;

function Complex(const r, i: Double): TComplex;
function ComplexAdd(const A, B: TComplex): TComplex;
function ComplexMultiply(const A, B: TComplex): TComplex;

type
  TMatrix = array[0..3, 0..3] of Single;
  PMatrix = ^TMatrix;

  TVertex = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
  PVertex = ^TVertex;

  TVector = TVertex;
  PVector = PVertex;

{$J-}

const
  StockMatrix: TMatrix = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));
  StockVertex: TVertex = (X: 0; Y: 0; Z: 0);

function Matrix: TMatrix;
function Vertex(X, Y, Z: Single): TVertex;
function Vector(X, Y, Z: Single): TVector;

function MatrixAdd(const A, B: TMatrix): TMatrix;
function MatrixSubtract(const A, B: TMatrix): TMatrix;
function MatrixMultiply(const A, B: TMatrix): TMatrix;
function MatrixDivide(const A, B: TMatrix): TMatrix;
function MatrixScale(const M: TMatrix; X, Y, Z: Single): TMatrix;
function MatrixTranslate(const M: TMatrix; X, Y, Z: Single): TMatrix;
function MatrixRotate(const M: TMatrix; X, Y, Z: Single): TMatrix;
function MatrixTransform(const M: TMatrix; const V: TVertex): TVertex;

function VectorDotProduct(const A, B: TVector): Single;
function VectorCrossProduct(const A, B: TVector): TVector;
function VectorNormalize(const V: TVector): TVector;

const
  NaNSignalingBits: Int64 = $7FF0000000000001;

var
  NaNSignaling: Double absolute NaNSignalingBits;

function NaN: Double;

function PositiveInfinity: Double;
function NegativeInfinity: Double;

function IsNaN(const D: Double): Boolean;
function IsInfinity(const D: Double): Boolean;

function DoubleToHex(const D: Double): string;
function HexToDouble(const Hex: string): Double;

implementation

{ Conversion routines }

const
  OneMeter = 3.2808333;
  OneMeterSquared = OneMeter * OneMeter;
  OneMeterCubed = OneMeter * OneMeter * OneMeter;

function FeetToMeters(const Value: Double): Double;
begin
  Result := Value / OneMeter;
end;

function MetersToFeet(const Value: Double): Double;
begin
  Result := Value * OneMeter;
end;

function FeetToMetersSquared(const Value: Double): Double;
begin
  Result := Value * OneMeterSquared;
end;

function MetersToFeetSquared(const Value: Double): Double;
begin
  Result := Value / OneMeterSquared;
end;

function FeetToMetersCubed(const Value: Double): Double;
begin
  Result := Value * OneMeterCubed;
end;

function MetersToFeetCubed(const Value: Double): Double;
begin
  Result := Value / OneMeterCubed;
end;

function CelsiusToFahrenheit(const Value: Double): Double;
begin
  Result := 32 + 1.8 * Value;
end;

function FahrenheitToCelsius(const Value: Double): Double;
begin
  Result := (Value - 32) * 5 / 9;
end;

{ Testing and setting empty memory routines }

function IsEmpty(var Data; DataSize: Integer): Boolean;
{ TODO: Code this routine in assembler
asm

end;}
var
  B: PByte;
  I: Integer;
begin
  Result := True;
  B := @Data;
  for I := 0 to DataSize - 1 do
  begin
    if B^ > 0 then
    begin
      Result := False;
      Break;
    end;
    Inc(B);
  end;
end;

procedure SetEmpty(var Data; DataSize: Integer);
asm
        PUSH    EDI
        MOV     EDI, EAX
        XOR     EAX, EAX
        MOV     ECX, EDX
        SAR     ECX, 2
        JS      @@Done
        REP     STOSD
        MOV     ECX, EDX
        AND     ECX, 3
        REP     STOSB
@@Done:
        POP     EDI
end;

function Divide(const Quotient, Divisor: Double): Double;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Round(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Double): Double;
begin
  if Divisor = 0 then
    Result := 0
  else
	  Result := Quotient - Trunc(Quotient / Divisor) * Divisor;
end;

function IntegerPart(const Value: Double): Double;
begin
  Result := Trunc(Value);
end;

function FractionalPart(const Value: Double): Double;
begin
  Result := Value - Trunc(Value);
end;

function GetPoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function GetFloatPoint(const X: Double; const Y: Double): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function GetRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function GetRect(const Rectangle: TRectangle): TRect;
var
  I: Integer;
begin
  with Result do
  begin
    Left := Rectangle[0].X;
    Top := Rectangle[0].Y;
    Right := Rectangle[0].X;
    Bottom := Rectangle[0].Y;
    for I := Low(Rectangle) to High(Rectangle) do
      with Rectangle[I] do
      begin
        if Left < X then Left := X;
        if Top < Y then Top := Y;
        if Right > X then Right := X;
        if Bottom > X then Bottom := Y;
      end;
  end;
end;

function GetFloatRect(const ALeft, ATop, ARight, ABottom: Double): TFloatRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function GetPolygon(const Points: array of Integer): TPolygon;
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

function GetFloatPolygon(const Points: array of Double): TFloatPolygon;
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

function PolygonToText(Polygon: TFloatPolygon): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Polygon) to High(Polygon) do
  begin
    Result := Result + Format('%.4f'#13#10, [Polygon[I].X]);
    Result := Result + Format('%.4f'#13#10, [Polygon[I].Y]);
  end;
end;

function TextToPolygon(const S: string): TFloatPolygon;
var
  A, B: string;
  P: PChar;
begin
  Result := nil;
  if S <> '' then
  begin
    P := PChar(S);
    repeat
      A := NextToken(P);
      B := NextToken(P);
      if (A <> '') and (B <> '') then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1].X := StrToFloat(A);
        Result[Length(Result) - 1].Y := StrToFloat(B);
      end;
    until (A = '') or (B = '');
    if Length(Result) < 3 then
      Result := nil;
  end;
end;

function Angle(const A: TPoint; const B: TPoint): Double;
begin
  if A.X = B.X then
    if A.Y = B.Y then
      Result := 0
    else if A.Y > B.Y then
      Result := 90
    else
      Result := 270
  else
    Result := ArcTan((B.Y - A.Y) / (B.X - A.X)) * (180 / PI);
end;

function Angle(const A: TFloatPoint; const B: TFloatPoint): Double;
begin
  if A.X = B.X then
    if A.Y = B.Y then
      Result := 0
    else if A.Y > B.Y then
      Result := 90
    else
      Result := 270
  else
    Result := ArcTan((B.Y - A.Y) / (B.X - A.X)) * (180 / PI);
end;

function Area(const Polygon: TPolygon): Double;
var
  I: Integer;
begin
  Result := 0;
  if Length(Polygon) > 2 then
  begin
    for I := Low(Polygon) to High(Polygon) - 1 do
      Result := Result + Polygon[I].X * Polygon[I + 1].Y - Polygon[I + 1].X *
        Polygon[I].Y;
     Result := Result * 0.5;
  end;
end;

function Area(const Polygon: TFloatPolygon): Double;
var
  I: Integer;
begin
  Result := 0;
  if Length(Polygon) > 2 then
  begin
    for I := Low(Polygon) to High(Polygon) - 1 do
      Result := Result + Polygon[I].X * Polygon[I + 1].Y - Polygon[I + 1].X *
        Polygon[I].Y;
     Result := Result * 0.5;
  end;
end;

function Center(Rect: TRect): TPoint;
begin
  with Rect do
  begin
    Result.X := (Right - Left) div 2 + Left;
    Result.Y := (Bottom - Top) div 2 + Top;
  end;
end;

function Center(Rect: TFloatRect): TFloatPoint;
begin
  with Rect do
  begin
    Result.X := (Right - Left) / 2 + Left;
    Result.Y := (Bottom - Top) / 2 + Top;
  end;
end;

function Distance(const A: TPoint; const B: TPoint): Double;
var
  X, Y: Double;
begin
  X := A.X - B.X;
  Y := A.Y - B.Y;
  Result := Sqrt(X * X + Y * Y);
end;

function Distance(const A: TFloatPoint; const B: TFloatPoint): Double;
var
  X, Y: Double;
begin
  X := A.X - B.X;
  Y := A.Y - B.Y;
  Result := Sqrt(X * X + Y * Y);
end;

procedure Offset(var Point: TPoint; X: Integer; Y: Integer);
begin
  Inc(Point.X, X);
  Inc(Point.Y, Y);
end;

procedure Offset(var Polygon: TPolygon; X: Integer; Y: Integer);
var
  I: Integer;
begin
  for I := Low(Polygon) to High(Polygon) do
  begin
    Polygon[I].X := Polygon[I].X + X;
    Polygon[I].Y := Polygon[I].Y + Y;
  end;
end;

procedure Offset(var Polygon: TFloatPolygon; X: Double; Y: Double);
var
  I: Integer;
begin
  for I := Low(Polygon) to High(Polygon) do
  begin
    Polygon[I].X := Polygon[I].X + X;
    Polygon[I].Y := Polygon[I].Y + Y;
  end;
end;

function Rotate(const Point: TPoint; const Angle: Double): TPoint;
begin
  Result.X := Round((Point.X * Cos(Angle)) - (Point.Y * Sin(Angle)));
  Result.Y := Round((Point.X * Sin(Angle)) + (Point.Y * Cos(Angle)));
end;

function Rotate(const Point: TFloatPoint; const Angle: Double): TFloatPoint;
begin
  Result.X := (Point.X * Cos(Angle)) - (Point.Y * Sin(Angle));
  Result.Y := (Point.X * Sin(Angle)) + (Point.Y * Cos(Angle));
end;

function Rotate(const Points: TPoints; const Angle: Double): TPoints;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := Low(Points) to High(Points) do
    Result[I] := Rotate(Points[I], Angle);
end;

function Rotate(const Distance: Double; const Angle: Double): TFloatPoint;
begin
  Result.X := Distance * Cos(Angle);
  Result.Y := Distance * Sin(Angle);
end;

function Rotate(Rect: TRect; const A: TPoint; const B: TPoint): TRectangle;
var
  FloatRect: TFloatRectangle;
  Angle: Double;
  Center: TFloatPoint;
  I: Integer;
begin
  with Rect do
  begin
    OffsetRect(Rect, -Left, -Top);
    FloatRect[0].X := Right / 2;
    FloatRect[0].Y := Bottom / 2;
    FloatRect[1].X := -FloatRect[0].X;
    FloatRect[1].Y := FloatRect[0].Y;
    FloatRect[2].X := -FloatRect[0].X;
    FloatRect[2].Y := -FloatRect[0].Y;
    FloatRect[3].X := FloatRect[0].X;
    FloatRect[3].Y := -FloatRect[0].Y;
  end;
  if A.X = B.X then
    if A.Y = B.Y then
      Angle := 0
    else if A.Y > B.Y then
      Angle := PI / 2
    else
      Angle := (3 * PI) / 2
  else
    Angle := ArcTan((B.Y - A.Y) / (B.X - A.X));
  for I := Low(FloatRect) to High(FloatRect) do
    FloatRect[I] := Rotate(FloatRect[I], Angle);
  Center.X := (A.X + B.X) / 2;
  Center.Y := (A.Y + B.Y) / 2;
  for I := Low(FloatRect) to High(FloatRect) do
  begin
    Result[I].X := Round((FloatRect[I].X) + Center.X);
    Result[I].Y := Round((FloatRect[I].Y) + Center.Y);
  end;
end;

function Scale(Polygon: TPolygon; Factor: Double): TPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := 0 to Length(Polygon) - 1 do
  begin
    Result[I].X := Round(Polygon[I].X * Factor);
    Result[I].Y := Round(Polygon[I].Y * Factor);
  end;
end;

function Scale(Polygon: TFloatPolygon; Factor: Double): TFloatPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := 0 to Length(Polygon) - 1 do
  begin
    Result[I].X := Polygon[I].X * Factor;
    Result[I].Y := Polygon[I].Y * Factor;
  end;
end;

function PointInPolygon(const Point: TPoint; const Polygon: TPolygon): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if Length(Polygon) > 2 then
  begin
    J := High(Polygon);
    for I := Low(Polygon) to High(Polygon) do
    begin
      if (((Polygon[I].Y <= Point.Y) and (Point.Y < Polygon[J].Y)) or
        ((Polygon[J].Y<= Point.Y) and (Point.Y < Polygon[I].Y))) and (Point.X <
        (Polygon[J].X - Polygon[I].X) * (Point.Y - Polygon[I].Y) / (Polygon[J].Y -
        Polygon[I].Y) + Polygon[I].X) then
        Result := not Result;
      J := I;
    end;
  end;
end;

function PointInPolygon(const Point: TFloatPoint; const Polygon: TFloatPolygon): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if Length(Polygon) > 2 then
  begin
    J := High(Polygon);
    for I := Low(Polygon) to High(Polygon) do
    begin
      if (((Polygon[I].Y <= Point.Y) and (Point.Y < Polygon[J].Y)) or
        ((Polygon[J].Y<= Point.Y) and (Point.Y < Polygon[I].Y))) and (Point.X <
        (Polygon[J].X - Polygon[I].X) * (Point.Y - Polygon[I].Y) / (Polygon[J].Y -
        Polygon[I].Y) + Polygon[I].X) then
        Result := not Result;
      J := I;
    end;
  end;
end;

function RectInRect(const A: TRect; const B: TRect): Boolean;
begin
  Result := (A.Left >= B.Left) and (A.Top >= B.Top) and (A.Right <= B.Right) and
    (A.Bottom <= B.Bottom);
end;


function GCD(A, B: Integer): Integer;
asm
        NEG     EAX
        JE      @3
@1:     NEG     EAX
        XCHG    EAX,EDX
@2:     SUB     EAX,EDX
        JG      @2
        JNE     @1
@3:     ADD     EAX,EDX
        JNE     @4
        INC     EAX
@4:
end;


function Factorial(I: Integer): Double;
begin
  if I < 1 then
    Result := 0
  else
  begin
    Result := 1;
    while I > 1 do
    begin
      Result := Result * I;
      Dec(I);
    end;
  end;
end;

function Combin(Range, Count: Integer): Double;
begin
  if (Range > 0) and (Count < Range) then
    Result := Factorial(Range) / Factorial(Count) / Factorial(Range - Count)
  else
    Result := 1;
end;

function Complex(const r, i: Double): TComplex;
begin
  Result.r := r;
  Result.i := i;
end;

function ComplexAdd(const A, B: TComplex): TComplex;
begin
  Result.r := A.r + B.r;
  Result.i := A.i + B.i;
end;

function ComplexMultiply(const A, B: TComplex): TComplex;
begin
  Result.r := (A.r * B.r) - (A.i * B.i);
  Result.i := (A.r * B.i) + (A.i * B.r);
end;

function Matrix: TMatrix;
begin
  Result := StockMatrix;
end;

function Vertex(X, Y, Z: Single): TVertex;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector(X, Y, Z: Single): TVector;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
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
var
  R: TMatrix;
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

function MatrixTransform(const M: TMatrix; const V: TVertex): TVertex;
begin
  Result.X := M[0, 0] * V.X + M[1, 0] * V.Y + M[2, 0] * V.Z + M[3, 0];
  Result.Y := M[0, 1] * V.X + M[1, 1] * V.Y + M[2, 1] * V.Z + M[3, 1];
  Result.Z := M[0, 2] * V.X + M[1, 2] * V.Y + M[2, 2] * V.Z + M[3, 2];
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

const
  NaNQuietBits: Int64 = Int64($7FFFFFFFFFFFFFFF);
  PositiveInfinityBits: Int64 = Int64($7FF0000000000000);
  NegativeInfinityBits: Int64 = Int64($FFF0000000000000);

var
  DNaNQuiet: Double absolute NaNQuietBits;
  DPositiveInfinity: Double absolute PositiveInfinityBits;
  DNegativeInfinity: Double absolute NegativeInfinityBits;

function IsNaN(const D: Double): Boolean;
var
  Overlay: Int64 absolute D;
begin
  Result := (Overlay and $7FF0000000000000 =  $7ff0000000000000) and
    (Overlay and $000FFFFFFFFFFFFF <> $0000000000000000);
end;

function IsInfinity(const D: Double): Boolean;
var
  Overlay: Int64 absolute D;
begin
  Result := Overlay and $7FF0000000000000 = $7FF0000000000000;
end;

function DoubleToHex(const D: Double): string;
var
  Overlay: array[1..2] of Longint absolute D;
begin
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;

function HexToDouble(const Hex: string): Double;
var
  D: Double;
  Overlay: array[1..2] of Longint absolute D;
begin
  Overlay[1] := StrToInt('$' + Copy(Hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(Hex, 1, 8));
  Result := D;
end;

function NaN: Double;
begin
  Result := DNaNQuiet
end;

function PositiveInfinity: Double;
begin
  Result := DPositiveInfinity
end;

function NegativeInfinity: Double;
begin
  Result := DNegativeInfinity
end;

end.
