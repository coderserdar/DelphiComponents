{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnECC;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���Բ�����㷨��Ԫ
* ��Ԫ���ߣ���Х
* ��    ע��Ŀǰʵ���� Int64 ��Χ���Լ�������ʽ������ y^2 = x^3 + Ax + B mod p
*           ������Բ���ߵļ��㡣
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2018.09.29 V1.3
*               ʵ�ִ�����Բ���߸��� X �� Y �������㷨����Ĭ�����ٶȸ���� Lucas
*           2018.09.13 V1.2
*               ����ʵ�ִ�����Բ���ߵļӽ��ܹ��ܣ�֧�� SM2 �Լ� Secp256k1 ����
*           2018.09.10 V1.1
*               �ܹ�����ϵ����С����Բ���߲���
*           2018.09.05 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$DEFINE USE_LUCAS}
// ��������������Ҳ���Ǹ��� X ������Բ���߷��̵� Y ֵʱʹ�� Lucas �����㷨������
// �粻���壬��ʹ�� Tonelli-Shanks �㷨���㡣Tonelli-Shanks �ٶȽ�����������Χ��
// ���� Lucas ������ 10 �����ϡ�

uses
  SysUtils, Classes, Contnrs, Windows, CnNativeDecl, CnPrimeNumber, CnBigNumber;

type
  ECnEccException = class(Exception);

  TCnEccPrimeType = (pt4U3, pt8U5, pt8U1);
  {* �������ͣ�mod 4 �� 3��mod 8 �� 5��mod 8 �� 1��������Բ���߷����п����� Y}

  TCnInt64EccPoint = packed record
  {* Int64 ��Χ�ڵ���Բ�����ϵĵ������ṹ}
    X: Int64;
    Y: Int64;
  end;

  TCnInt64PublicKey = TCnInt64EccPoint;
  {* Int64 ��Χ�ڵ���Բ���ߵĹ�Կ��G ����� k �κ�ĵ�����}

  TCnInt64PrivateKey = Int64;
  {* Int64 ��Χ�ڵ���Բ���ߵ�˽Կ��������� k ��}

  TCnInt64Ecc = class
  {* ����һ������ p Ҳ���� 0 �� p - 1 �ϵ���Բ���� y^2 = x^3 + Ax + B mod p���������� Int64 ��Χ��}
  private
    FGenerator: TCnInt64EccPoint;
    FCoefficientA: Int64;
    FCoefficientB: Int64;
    FFiniteFieldSize: Int64;
    FOrder: Int64;
    FSizeUFactor: Int64;
    FSizePrimeType: TCnEccPrimeType;
  protected
    // Tonelli-Shanks ģ��������ʣ����⣬���� False ��ʾʧ�ܣ������������б�֤ P Ϊ����
    function TonelliShanks(X, P: Int64; out Y: Int64): Boolean;
    // Lucas ����ģ��������ʣ����⣬���� False ��ʾʧ�ܣ�ֻ��� P Ϊ 8*u + 1 ����ʽ
    function Lucas(X, P: Int64; out Y: Int64): Boolean;
  public
    constructor Create(A, B, FieldPrime, GX, GY, Order: Int64);
    {* ���캯�������뷽�̵� A, B �������������Ͻ� p��G �����ꡢG ��Ľ���}
    destructor Destroy; override;
    {* ��������}

    procedure MultiplePoint(K: Int64; var Point: TCnInt64EccPoint);
    {* ����ĳ�� P �� k * P ֵ��ֵ���·��� P}
    procedure PointAddPoint(var P, Q, Sum: TCnInt64EccPoint);
    {* ���� P + Q��ֵ���� Sum �У�Sum ������ P��Q ֮һ��P��Q ������ͬ}
    procedure PointSubPoint(var P, Q, Diff: TCnInt64EccPoint);
    {* ���� P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ}
    procedure PointInverse(var P: TCnInt64EccPoint);
    {* ���� P �����Ԫ -P��ֵ���·��� P}
    function IsPointOnCurve(var P: TCnInt64EccPoint): Boolean;
    {* �ж� P ���Ƿ��ڱ�������}

    function PlainToPoint(Plain: Int64; var OutPoint: TCnInt64EccPoint): Boolean;
    {* ��Ҫ���ܵ�������ֵ��װ��һ�������ܵĵ�}

    procedure GenerateKeys(out PrivateKey: TCnInt64PrivateKey; out PublicKey: TCnInt64PublicKey);
    {* ����һ�Ը���Բ���ߵĹ�˽Կ��˽Կ��������� k����Կ�ǻ��� G ���� k �γ˷���õ��ĵ����� K}
    procedure Encrypt(var PlainPoint: TCnInt64EccPoint; PublicKey: TCnInt64PublicKey;
      var OutDataPoint1, OutDataPoint2: TCnInt64EccPoint; RandomKey: Int64 = 0);
    {* ��Կ�������ĵ� M���õ��������������ģ��ڲ����������ֵ r��Ҳ���� C1 = M + rK; C2 = r * G
      �������� RandomKey �� 0�����ڲ��������}
    procedure Decrypt(var DataPoint1, DataPoint2: TCnInt64EccPoint;
      PrivateKey: TCnInt64PrivateKey; var OutPlainPoint: TCnInt64EccPoint);
    {* ˽Կ�������ĵ㣬Ҳ���Ǽ��� C1 - k * C2 �͵õ���ԭ�ĵ� M}

    property Generator: TCnInt64EccPoint read FGenerator;
    {* �������� G}
    property CoefficientA: Int64 read FCoefficientA;
    {* ����ϵ�� A}
    property CoefficientB: Int64 read FCoefficientB;
    {* ����ϵ�� B}
    property FiniteFieldSize: Int64 read FFiniteFieldSize;
    {* ��������Ͻ磬���� p}
    property Order: Int64 read FOrder;
    {* ����Ľ���}
  end;

  TCnEccPoint = class(TPersistent)
  {* ��Բ�����ϵĵ�������}
  private
    FY: TCnBigNumber;
    FX: TCnBigNumber;
    procedure SetX(const Value: TCnBigNumber);
    procedure SetY(const Value: TCnBigNumber);
  public
    constructor Create; overload;
    constructor Create(const XDec, YDec: AnsiString); overload;

    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function IsZero: Boolean;
    procedure SetZero;

    property X: TCnBigNumber read FX write SetX;
    property Y: TCnBigNumber read FY write SetY;
  end;

  TCnEccPublicKey = TCnEccPoint;
  {* ��Բ���ߵĹ�Կ��G ����� k �κ�ĵ�����}

  TCnEccPrivateKey = TCnBigNumber;
  {* ��Բ���ߵ�˽Կ��������� k ��}

  TCnEccPredefinedCurveType = (ctCustomized, ctSM2, ctSecp224r1, ctSecp224k1, ctSecp256k1);
  {* ֧�ֵ���Բ��������}

  TCnEcc = class
  {* ����һ������ p Ҳ���� 0 �� p - 1 �ϵ���Բ���� y^2 = x^3 + Ax + B mod p}
  private
    FCoefficientB: TCnBigNumber;
    FCoefficientA: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FBigNumberPool: TObjectList;
    FSizeUFactor: TCnBigNumber;
    FSizePrimeType: TCnEccPrimeType;
    function ObtainBigNumberFromPool: TCnBigNumber;
    procedure RecycleBigNumberToPool(Num: TCnBigNumber);
    function GetBitsCount: Integer;
  protected
    procedure CalcX3AddAXAddB(X: TCnBigNumber); // ���� X^3 + A*X + B��������� X
  public
    constructor Create; overload;
    constructor Create(Predefined: TCnEccPredefinedCurveType); overload;
    constructor Create(const A, B, FieldPrime, GX, GY, Order: AnsiString); overload;
    {* ���캯�������뷽�̵� A, B �������������Ͻ� p��G �����ꡢG ��Ľ�������Ҫʮ�������ַ���}
    destructor Destroy; override;
    {* ��������}

    procedure Load(Predefined: TCnEccPredefinedCurveType); overload;
    procedure Load(const A, B, FieldPrime, GX, GY, Order: AnsiString); overload;
    {* �������߲���}

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
    {* ����ĳ�� P �� k * P ֵ��ֵ���·��� P}
    procedure PointAddPoint(P, Q, Sum: TCnEccPoint);
    {* ���� P + Q��ֵ���� Sum �У�Sum ������ P��Q ֮һ��P��Q ������ͬ}
    procedure PointSubPoint(P, Q, Diff: TCnEccPoint);
    {* ���� P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ}
    procedure PointInverse(P: TCnEccPoint);
    {* ���� P �����Ԫ -P��ֵ���·��� P}
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* �ж� P ���Ƿ��ڱ�������}

    function PlainToPoint(Plain: TCnBigNumber; OutPoint: TCnEccPoint): Boolean;
    {* ��Ҫ���ܵ�������ֵ��װ��һ�������ܵĵ㣬Ҳ����������Ϊ X �󷽳̵� Y}
    function PointToPlain(Point: TCnEccPoint; OutPlain: TCnBigNumber): Boolean;
    {* �����ܳ������ĵ�⿪��һ��������ֵ��Ҳ���ǽ���� X ֵȡ��}

    procedure GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey);
    {* ����һ�Ը���Բ���ߵĹ�˽Կ��˽Կ��������� k����Կ�ǻ��� G ���� k �γ˷���õ��ĵ����� K}
    procedure Encrypt(PlainPoint: TCnEccPoint; PublicKey: TCnEccPublicKey;
      OutDataPoint1, OutDataPoint2: TCnEccPoint);
    {* ��Կ�������ĵ� M���õ��������������ģ��ڲ����������ֵ r��Ҳ���� C1 = M + rK; C2 = r * G}
    procedure Decrypt(DataPoint1, DataPoint2: TCnEccPoint;
      PrivateKey: TCnEccPrivateKey; OutPlainPoint: TCnEccPoint);
    {* ˽Կ�������ĵ㣬Ҳ���Ǽ��� C1 - k * C2 �͵õ���ԭ�ĵ� M}

    property Generator: TCnEccPoint read FGenerator;
    {* �������� G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* ����ϵ�� A}
    property CoefficientB: TCnBigNumber read FCoefficientB;
    {* ����ϵ�� B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* ��������Ͻ磬���� p}
    property Order: TCnBigNumber read FOrder;
    {* ����Ľ���}
    property BitsCount: Integer read GetBitsCount;
    {* ����Բ���ߵ�������λ��}
  end;

function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
{* ��һ�� TCnInt64EccPoint ������ת��Ϊ�ַ���}

function CnEccPointToString(const P: TCnEccPoint): string;
{* ��һ�� TCnEccPoint ������ת��Ϊ�ַ���}

function CnInt64EccGenerateParams(var FiniteFieldSize, CoefficientA, CoefficientB,
  GX, GY, Order: Int64): Boolean;
{* ������Բ���� y^2 = x^3 + Ax + B mod p �ĸ�����������������ʵ�֣�ֻ��������ϵ����С��}

function CnInt64EccDiffieHellmanGenerateOutKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey): Boolean;
{* ��������ѡ�������� PrivateKey ���� ECDH ��ԿЭ�̵������Կ��
   ���� OutPublicKey = SelfPrivateKey * G}

function CnInt64EccDiffieHellmanComputeKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  var OtherPublicKey: TCnInt64PublicKey; var SharedSecretKey: TCnInt64PublicKey): Boolean;
{* ���ݶԷ����͵� ECDH ��ԿЭ�̵������Կ�������ɹ��ϵ���Կ��
   ���� SecretKey = SelfPrivateKey * OtherPublicKey}

function CnEccPointsEqual(P1, P2: TCnEccPoint): Boolean;
{* �ж��������Ƿ����}

function CnEccDiffieHellmanGenerateOutKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
{* ��������ѡ�������� PrivateKey ���� ECDH ��ԿЭ�̵������Կ��
   ���� PublicKey = SelfPrivateKey * G}

function CnEccDiffieHellmanComputeKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  OtherPublicKey: TCnEccPublicKey; SharedSecretKey: TCnEccPublicKey): Boolean;
{* ���ݶԷ����͵� ECDH ��ԿЭ�̵������Կ�������ɹ��ϵ���Կ�㣬һ���õ�� X ����������Կ
   ���� SecretKey = SelfPrivateKey * OtherPublicKey}

implementation

type
  TCnEccPredefinedHexParams = packed record
    P: AnsiString;
    A: AnsiString;
    B: AnsiString;
    GX: AnsiString;
    GY: AnsiString;
    N: AnsiString;
    H: AnsiString;
  end;

const
  ECC_PRE_DEFINED_PARAMS: array[TCnEccPredefinedCurveType] of TCnEccPredefinedHexParams = (
    (P: ''; A: ''; B: ''; GX: ''; GY: ''; N: ''; H: ''),
    ( // SM2
      P: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF';
      A: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFC';
      B: '28E9FA9E9D9F5E344D5A9E4BCF6509A7F39789F515AB8F92DDBCBD414D940E93';
      GX: '32C4AE2C1F1981195F9904466A39C9948FE30BBFF2660BE1715A4589334C74C7';
      GY: 'BC3736A2F4F6779C59BDCEE36B692153D0A9877CC62A474002DF32E52139F0A0';
      N: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFF7203DF6B21C6052B53BBF40939D54123';
      H: '01'
    ),
    ( // ctSecp224r1
      P: '00ffffffffffffffffffffffffffffffff000000000000000000000001';
      A: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE';
      B: '00B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4';
      GX: 'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21';
      GY: 'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34';
      N: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D';
      H: '01'
    ),
    ( // ctSecp224k1
      P: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D';
      A: '00';
      B: '05';
      GX: 'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C';
      GY: '7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5';
      N: '010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7';
      H: '01'
    ),
    ( // secp256k1
      P: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F';
      A: '00';
      B: '07';
      GX: '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798';
      GY: '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8';
      N: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141';
      H: '01'
    )
  );

// ��һ�� TCnInt64EccPoint ������ת��Ϊ�ַ���
function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
begin
  Result := Format('%d,%d', [P.X, P.Y]);
end;

// ��һ�� TCnEccPoint ������ת��Ϊ�ַ���
function CnEccPointToString(const P: TCnEccPoint): string;
begin
  Result := Format('%s,%s', [P.X.ToDec, P.Y.ToDec]);
end;

// �ж��������Ƿ����
function CnEccPointsEqual(P1, P2: TCnEccPoint): Boolean;
begin
  if P1 = P2 then
  begin
    Result := True;
    Exit;
  end;
  Result := (BigNumberCompare(P1.X, P2.X) = 0) and (BigNumberCompare(P1.Y, P2.Y) = 0);
end;

// ������Բ���� y^2 = x^3 + Ax + B mod p �ĸ�������������ʵ��
function CnInt64EccGenerateParams(var FiniteFieldSize, CoefficientA, CoefficientB,
  GX, GY, Order: Int64): Boolean;
var
  I: Integer;
  N: Int64;
  P: TCnInt64EccPoint;
  Ecc64: TCnInt64Ecc;
begin
  // ���裺���ѡ���������� p��������� a��b���� SEA �㷨��������ߵĽ� N
  // �ж� N �Ǵ���������һ�������֮һ�Ǵ�������Ȼ�������������Ϊѭ����Ⱥ�Ľ� n
  // �ٸ��� n Ѱ�һ��� G �����ꡣ��� n �͵��� N ������������� G ���ѡ���С�

  repeat
    // FiniteFieldSize := CnGenerateUInt32Prime; // ����С�������������Ҳ����̫С
    Randomize;
    I := Trunc(Random * (High(CN_PRIME_NUMBERS_SQRT_UINT32) - 100)) + 100;
    FiniteFieldSize := CN_PRIME_NUMBERS_SQRT_UINT32[I];
    CoefficientA := Trunc(Random * 16);
    CoefficientB := Trunc(Random * 256);
    N := 1; // 0,0 ��Ȼ����

    // A��B ���Ƚ�С�����ﲻ�õ������
    if (4 * CoefficientA * CoefficientA * CoefficientA - 27 * CoefficientB * CoefficientB)
      mod FiniteFieldSize = 0 then
      Continue;

    GX := 0;
    GY := 0;

    // ���������Բ���ߵĽף����� SEA��ԭ��ֻ������������ٷ�����������õ¹�ʽ
    // N := 1 + P + ���е����õ�((x^3+ax+b)/p)֮�ͣ����� X �� 0 �� P - 1
    Inc(N, FiniteFieldSize);
    for I := 0 to FiniteFieldSize - 1 do
    begin
      // ������� Int64 ��ת��һ�£����� I �����η����� Integer �����
      N := N + CnInt64Legendre(Int64(I) * Int64(I) * Int64(I) + CoefficientA * I + CoefficientB, FiniteFieldSize);
    end;
  until CnInt64IsPrime(N);

  // Ȼ�������һ�� X �� Y
  Ecc64 := TCnInt64Ecc.Create(CoefficientA, CoefficientB, FiniteFieldSize, 0, 0, FiniteFieldSize);
  repeat
    P.X := Trunc(Random * (FiniteFieldSize - 1)) + 1;
    for I := 0 to FiniteFieldSize - 1 do
    begin
      P.Y := I;
      if Ecc64.IsPointOnCurve(P) then
      begin
        GX := P.X;
        GY := P.Y;
        Break;
      end;
    end;
  until (GX > 0) and (GY > 0);
  Ecc64.Free;

//    ���´�������ٷ�����֤ N �Ƿ���ȷ��ĿǰС��Χ������������û��
//    N := 1;
//    Ecc64 := TCnInt64Ecc.Create(CoefficientA, CoefficientB, FiniteFieldSize, 0, 0, FiniteFieldSize);
//    for I := 0 to FiniteFieldSize - 1 do
//    begin
//      for J := 0 to FiniteFieldSize - 1 do
//      begin
//        P.X := I;
//        P.Y := J;
//        if Ecc64.IsPointOnCurve(P) then
//        begin
//          Inc(N);
//          if (GX = 0) or (GY = 0) then // ��һ������ľ͵�������
//          begin
//            GX := P.X;
//            GY := P.Y;
//          end;
//
//          if P.Y > 0 then
//          begin
//            P.Y := FiniteFieldSize - P.Y;
//            if Ecc64.IsPointOnCurve(P) then
//              Inc(N);
//          end;
//
//          // ��� X �Ѿ������ˣ�ÿ�� X �����ж������� Y��
//          Break;
//        end;
//      end;
//      // Break ���ˣ�������һ�� X ��ѭ��
//    end;

    // N Ϊ������Բ���ߵĽ�

  Order := N;
  Result := True;
end;

// �� X ��� M ��ģ��Ԫ��Ҳ����ģ��Ԫ Y������ (X * Y) mod M = 1����ΧΪ Int64��Ҳ����˵֧�� X Ϊ��ֵ
function Int64ModularInverse(X: Int64; Modulus: Int64): Int64;
var
  Neg: Boolean;
begin
  Neg := False;
  if X < 0 then
  begin
    X := -X;
    Neg := True;
  end;

  // ������ģ��Ԫ������������ģ��Ԫ�ĸ�ֵ����ֵ�������ټ� Modulus
  Result := CnInt64ModularInverse(X, Modulus);
  if Neg and (Result > 0) then
    Result := -Result;

  if Result < 0 then
    Result := Result + Modulus;
end;

function RandomInt64LessThan(HighValue: Int64): Int64;
var
  Hi, Lo: Cardinal;
begin
  Randomize;
  Hi := Trunc(Random * High(Integer) - 1) + 1;   // Int64 ���λ������ 1�����⸺��
  Randomize;
  Lo := Trunc(Random * High(Cardinal) - 1) + 1;
  Result := (Int64(Hi) shl 32) + Lo;
  Result := Result mod HighValue;
end;

{ TCnInt64Ecc }

constructor TCnInt64Ecc.Create(A, B, FieldPrime, GX, GY, Order: Int64);
var
  R: Int64;
begin
  inherited Create;
  if not CnInt64IsPrime(FieldPrime) or not CnInt64IsPrime(Order) then
    raise ECnEccException.Create('Infinite Field and Order must be a Prime Number.');

  if not (GX >= 0) and (GX < FieldPrime) or
    not (GY >= 0) and (GY < FieldPrime) then
    raise ECnEccException.Create('Generator Point must be in Infinite Field.');

  // Ҫȷ�� 4*a^3+27*b^2 <> 0
  if 4 * A * A * A + 27 * B * B = 0 then
    raise ECnEccException.Create('Error: 4 * A^3 + 27 * B^2 = 0');

  FCoefficientA := A;
  FCoefficientB := B;
  FFiniteFieldSize := FieldPrime;
  FGenerator.X := GX;
  FGenerator.Y := GY;
  FOrder := Order;

  R := FFiniteFieldSize mod 4;
  if R = 3 then  // RFC 5639 Ҫ�� p ���� 4u + 3 ����ʽ�Ա㷽��ؼ��� Y������������δ��
  begin
    FSizePrimeType := pt4U3;
    FSizeUFactor := FFiniteFieldSize div 4;
  end
  else
  begin
    R := FFiniteFieldSize mod 8;
    if R = 1 then
    begin
      FSizePrimeType := pt8U1;
      FSizeUFactor := FFiniteFieldSize div 8;
    end
    else if R = 5 then
    begin
      FSizePrimeType := pt8U5;
      FSizeUFactor := FFiniteFieldSize div 8;
    end
    else
      raise ECnEccException.Create('Invalid Finite Field Size.');
  end;
end;

procedure TCnInt64Ecc.Decrypt(var DataPoint1, DataPoint2: TCnInt64EccPoint;
  PrivateKey: TCnInt64PrivateKey; var OutPlainPoint: TCnInt64EccPoint);
var
  P: TCnInt64EccPoint;
begin
  P := DataPoint2;
  MultiplePoint(PrivateKey, P);
  PointSubPoint(DataPoint1, P, OutPlainPoint);
end;

destructor TCnInt64Ecc.Destroy;
begin

  inherited;
end;

procedure TCnInt64Ecc.Encrypt(var PlainPoint: TCnInt64EccPoint;
  PublicKey: TCnInt64PublicKey; var OutDataPoint1,
  OutDataPoint2: TCnInt64EccPoint; RandomKey: Int64);
begin
  if RandomKey = 0 then
  begin
    Randomize;
    RandomKey := Trunc(Random * (FOrder - 1)) + 1; // �� 0 �󵫱Ȼ����С�������
  end;

  if RandomKey mod FOrder = 0 then
    raise ECnEccException.CreateFmt('Error RandomKey %d for Order.', [RandomKey]);

  // M + rK;
  OutDataPoint1 := PublicKey;
  MultiplePoint(RandomKey, OutDataPoint1);
  PointAddPoint(PlainPoint, OutDataPoint1, OutDataPoint1);

  // r * G
  OutDataPoint2 := FGenerator;
  MultiplePoint(RandomKey, OutDataPoint2);
end;

procedure TCnInt64Ecc.GenerateKeys(out PrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey);
begin
  Randomize;
  PrivateKey := Trunc(Random * (FOrder - 1)) + 1; // �� 0 �󵫱Ȼ����С�������
  PublicKey := FGenerator;
  MultiplePoint(PrivateKey, PublicKey);           // ����� PrivateKey ��
end;

function TCnInt64Ecc.IsPointOnCurve(var P: TCnInt64EccPoint): Boolean;
var
  Y2, X3, AX, B: Int64;
begin
  // ���� (Y^2 - X^3 - A*X - B) mod p �Ƿ���� 0��Ӧ�÷�����
  // Ҳ���Ǽ���(Y^2 mod p - X^3 mod p - A*X mod p - B mod p) mod p
  Y2 := MontgomeryPowerMod(P.Y, 2, FFiniteFieldSize);
  X3 := MontgomeryPowerMod(P.X, 3, FFiniteFieldSize);
  AX := Int64MultipleMod(FCoefficientA, P.X, FFiniteFieldSize);
  B := FCoefficientB mod FFiniteFieldSize;

  Result := ((Y2 - X3 - AX - B) mod FFiniteFieldSize) = 0;
end;

function TCnInt64Ecc.Lucas(X, P: Int64; out Y: Int64): Boolean;
var
  G, U, V, Z: Int64;
begin
  Result := False;
  G := X;

  while True do
  begin
    // ���ȡ X
    X := RandomInt64LessThan(P);

    // �ټ��� Lucas �����е� V�����±� K Ϊ (P+1)/2
    CnLucasSequenceMod(X, G, (P + 1) shr 1, P, U, V);

    // V ż��ֱ������ 1 �� mod P��V ����� P ������ 1
    if (V and 1) = 0 then
      Z := (V shr 1) mod P
    else
      Z := (V + P) shr 1;
    // Z := (V div 2) mod P;

    if Int64MultipleMod(Z, Z, P) = G then
    begin
      Y := Z;
      Result := True;
      Exit;
    end
    else if (U > 1) and (U < P - 1) then
      Break;
  end;
end;

procedure TCnInt64Ecc.MultiplePoint(K: Int64; var Point: TCnInt64EccPoint);
var
  E, R: TCnInt64EccPoint;
begin
  if K < 0 then
  begin
    K := -K;
    PointInverse(Point);
  end;

  if K = 0 then
  begin
    Point.X := 0;
    Point.Y := 0;
    Exit;
  end;

  R.X := 0;
  R.Y := 0;
  E := Point;

  while K <> 0 do
  begin
    if (K and 1) <> 0 then
      PointAddPoint(R, E, R);

    PointAddPoint(E, E, E);
    K := K shr 1;
  end;

  Point := R;
end;

function TCnInt64Ecc.PlainToPoint(Plain: Int64;
  var OutPoint: TCnInt64EccPoint): Boolean;
var
  X3, AX, B, G, Y, Z: Int64;
begin
  Result := False;
  if Plain = 0 then
  begin
    OutPoint.X := 0;
    OutPoint.Y := 0;
    Result := True;
    Exit;
  end;

  // �ⷽ���� Y�� (y^2 - (Plain^3 + A * Plain + B)) mod p = 0
  // ע�� Plain ���̫�󣬼�������л���������ô���ֻ���÷����ɡ�
  // (Y^2 mod p - Plain ^ 3 mod p - A * Plain mod p - B mod p) mod p = 0;
  X3 := MontgomeryPowerMod(Plain, 3, FFiniteFieldSize);
  AX := Int64MultipleMod(FCoefficientA, Plain, FFiniteFieldSize);
  B := FCoefficientB mod FFiniteFieldSize;

  G := (X3 + AX + B) mod FFiniteFieldSize; // ���������Ļ�

  // ��Ϊ Y^2 = N * p + B Ҫ���ҳ� N ���ұ�Ϊ��ȫƽ���������� Y ����ֵ
  // Ҫ��Ӳ�� N �� 0 ��ʼ�� 1 ���������������Ƿ���ȫƽ������������������ô��
  // ���ö���ʣ������ģ�Ŀ����󷨣��������� P �����Է����֣�

  case FSizePrimeType of
  pt4U3:  // �ο��ԡ�SM2��Բ���߹�Կ�����㷨����¼ B �еġ�ģ����ƽ��������⡱һ��
    begin
      Y := MontgomeryPowerMod(G, FSizeUFactor + 1, FFiniteFieldSize);
      Z := Int64MultipleMod(Y, Y, FFiniteFieldSize);
      if Z = G then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
    end;
  pt8U5:  // �ο��ԡ�SM2��Բ���߹�Կ�����㷨����¼ B �еġ�ģ����ƽ��������⡱һ��
    begin
      Z := MontgomeryPowerMod(G, 2 * FSizeUFactor + 1, FFiniteFieldSize);
      if Z = 1 then
      begin
        Y := MontgomeryPowerMod(G, FSizeUFactor + 1, FFiniteFieldSize);
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end
      else
      begin
        Z := FFiniteFieldSize - Z;
        if Z = 1 then
        begin
          // y = (2g * (4g)^u) mod p = (2g mod p * (4^u * g^u) mod p) mod p
          Y := (Int64MultipleMod(G, 2, FFiniteFieldSize) *
            MontgomeryPowerMod(4, FSizeUFactor, FFiniteFieldSize) *
            MontgomeryPowerMod(G, FSizeUFactor, FFiniteFieldSize)) mod FFiniteFieldSize;
          OutPoint.X := Plain;
          OutPoint.Y := Y;
          Result := True;
        end;
      end;
    end;
  pt8U1: // �ο��� wikipedia �ϵ� Tonelli-Shanks ����ʣ������㷨�Լ� IEEE P1363 ��� Lucas �����㷨
    begin
{$IFDEF USE_LUCAS}
      // ��SM2��Բ���߹�Կ�����㷨����¼ B �еġ�ģ����ƽ��������⡱һ�� Lucas ���м�������Ľ��ʵ�ڲ���
      if Lucas(G, FFiniteFieldSize, Y) then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
{$ELSE}
      //  ���� Tonelli-Shanks �㷨����ģ��������ʣ����⣬���ڲ���Ҫͨ�����õ·����ж�����Ƿ���ڣ������������ѭ��
      if TonelliShanks(G, FFiniteFieldSize, Y) then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
{$ENDIF}
    end;
  end;
end;

procedure TCnInt64Ecc.PointAddPoint(var P, Q, Sum: TCnInt64EccPoint);
var
  K, X, Y, PX: Int64;
begin
  K := 0;
  if (P.X = 0) and (P.Y = 0) then
  begin
    Sum := Q;
    Exit;
  end
  else if (Q.X = 0) and (Q.Y = 0) then
  begin
    Sum := P;
    Exit;
  end
  else if (P.X = Q.X) and (P.Y = Q.Y) then
  begin
    // ��������ͬһ���㣬����б��Ϊ�����󵼣�3 * X^2 + A / (2 * Y) ���� Y = 0 ��ֱ��������Զ 0��
    X := 3 * P.X * P.X + FCoefficientA;
    Y := 2 * P.Y;

    if Y = 0 then
    begin
      Sum.X := 0;
      Sum.Y := 0;
    end;

    Y := Int64ModularInverse(Y, FFiniteFieldSize);
    K := Int64MultipleMod(X, Y, FFiniteFieldSize); // �õ�б��
  end
  else if (P.X = Q.X) and ((P.Y = -Q.Y) or (P.Y + Q.Y = FFiniteFieldSize)) then        // P = -Q
  begin
    Sum.X := 0;
    Sum.Y := 0;
    Exit;
  end
  else if P.X <> Q.X then
  begin
    // б�� K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
    Y := Q.Y - P.Y;
    X := Q.X - P.X;

    // Y/X = Y*X^-1 = Y * (X ��� p ����Ԫ)
    X := Int64ModularInverse(X, FFiniteFieldSize);
    K := Int64MultipleMod(Y, X, FFiniteFieldSize); // �õ�б��
  end
  else if P.Y <> Q.Y then
  begin
    // P��Q ���� X ��ͬ��Y ��ͬ���ֲ�����Ԫ���������ӣ������ϲ������
    raise ECnEccException.CreateFmt('Can NOT Calucate %d,%d + %d,%d', [P.X, P.Y, Q.X, Q.Y]);
  end;

  // Xsum = (K^2 - X1 - X2) mod p
  X := K * K - P.X - Q.X;
  while X < 0 do
    X := X + FFiniteFieldSize;
  PX := P.X; // ��� Sum �� P ��ͬһ����Ҫ���� P.X �������������ȴ��� P.X
  if X < 0 then
  begin
    X := -X;
    Sum.X := X mod FFiniteFieldSize;
    Sum.X := FFiniteFieldSize - Sum.X;
  end
  else
    Sum.X := X mod FFiniteFieldSize;

  // Ysum = (K * (X1 - Xsum) - Y1) mod p  ע��Ҫȡ��
  //   Ҳ = (K * (X2 - Xsum) - Y2) mod p  ע��Ҫȡ��
  X := PX - Sum.X;
  Y := K * X - P.Y;
  if Y < 0 then
  begin
    Y := -Y;
    Sum.Y := Y mod FFiniteFieldSize;
    Sum.Y := FFiniteFieldSize - Sum.Y;
  end
  else
    Sum.Y := Y mod FFiniteFieldSize;
end;

procedure TCnInt64Ecc.PointInverse(var P: TCnInt64EccPoint);
begin
  // P.Y := -P.Y mod p ע������ĸ�ֵȡģ������ Delphi ��ȡ����ȡģ�ٱ为
  P.Y := FFiniteFieldSize - (P.Y mod FFiniteFieldSize);
end;

procedure TCnInt64Ecc.PointSubPoint(var P, Q, Diff: TCnInt64EccPoint);
var
  Inv: TCnInt64EccPoint;
begin
  Inv.X := Q.X;
  Inv.Y := Q.Y;
  PointInverse(Inv);
  PointAddPoint(P, Inv, Diff);
end;

// ��������ѡ�������� PrivateKey ���� ECDH ��ԿЭ�̵������Կ��
function CnInt64EccDiffieHellmanGenerateOutKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey): Boolean;
begin
  // OutPublicKey = SelfPrivateKey * G
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey > 0) then
  begin
    PublicKey := Ecc.Generator;
    Ecc.MultiplePoint(SelfPrivateKey, PublicKey);
    Result := True;
  end;
end;

// ���ݶԷ����͵� ECDH ��ԿЭ�̵������Կ�������ɹ��ϵ���Կ��
function CnInt64EccDiffieHellmanComputeKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  var OtherPublicKey: TCnInt64PublicKey; var SharedSecretKey: TCnInt64PublicKey): Boolean;
begin
  // SecretKey = SelfPrivateKey * OtherPublicKey
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey > 0) then
  begin
    SharedSecretKey := OtherPublicKey;
    Ecc.MultiplePoint(SelfPrivateKey, SharedSecretKey);
    Result := True;
  end;
end;

function TCnInt64Ecc.TonelliShanks(X, P: Int64; out Y: Int64): Boolean;
var
  I: Integer;
  Q, S, Z, C, R, T, M, B: Int64;
begin
  Result := False;
  if (X <= 0) or (P <= 0) or (X >= P) then
    Exit;

  // ��Ҫͨ�����õ·����ж�����Ƿ���ڣ����������������ѭ��
  if CnInt64Legendre(X, P) <> 1 then
    Exit;

  S := 0;
  Q := P - 1;
  while (Q mod 2) = 0 do
  begin
    Q := Q shr 1;
    Inc(S);
  end;

  Z := 2;
  while Z < P do
  begin
    if CnInt64Legendre(Z, P) = -1 then
      Break;
    Inc(Z);
  end;

  // ����һ�� Z ���� ��� P �����õ·���Ϊ -1
  C := MontgomeryPowerMod(Z, Q, P);
  R := MontgomeryPowerMod(X, (Q + 1) div 2, P);
  T := MontgomeryPowerMod(X, Q, P);
  M := S;

  while True do
  begin
    if T mod P = 1 then
      Break;

    for I := 1 to M - 1 do
    begin
      if MontgomeryPowerMod(T, 1 shl I, P) = 1 then
        Break;
    end;

    B := MontgomeryPowerMod(C, 1 shl (M - I - 1), P);
    M := I; // M ÿ�ض����С���㷨����

    R := Int64MultipleMod(R, B, P);
    T := Int64MultipleMod(Int64MultipleMod(T, B, P),
      B mod P, P); // T*B*B mod P = (T*B mod P) * (B mod P) mod P
    C := Int64MultipleMod(B, B, P);
  end;
  Y := (R mod P + P) mod P;
  Result := True;
end;

{ TCnEccPoint }

procedure TCnEccPoint.Assign(Source: TPersistent);
begin
  if Source is TCnEccPoint then
  begin
    BigNumberCopy(FX, (Source as TCnEccPoint).X);
    BigNumberCopy(FY, (Source as TCnEccPoint).Y);
  end
  else
    inherited;
end;

constructor TCnEccPoint.Create;
begin
  inherited;
  FX := TCnBigNumber.Create;
  FY := TCnBigNumber.Create;
  FX.SetZero;
  FY.SetZero;
end;

constructor TCnEccPoint.Create(const XDec, YDec: AnsiString);
begin
  Create;
  FX.SetDec(XDec);
  FY.SetDec(YDec);
end;

destructor TCnEccPoint.Destroy;
begin
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnEccPoint.IsZero: Boolean;
begin
  Result := FX.IsZero and FY.IsZero;
end;

procedure TCnEccPoint.SetX(const Value: TCnBigNumber);
begin
  BigNumberCopy(FX, Value);
end;

procedure TCnEccPoint.SetY(const Value: TCnBigNumber);
begin
  BigNumberCopy(FY, Value);
end;

procedure TCnEccPoint.SetZero;
begin
  FX.SetZero;
  FY.SetZero;
end;

{ TCnEcc }

procedure TCnEcc.CalcX3AddAXAddB(X: TCnBigNumber);
var
  M: TCnBigNumber;
begin
  M := ObtainBigNumberFromPool;
  try
    BigNumberCopy(M, X);
    BigNumberMul(X, X, X);
    BigNumberMul(X, X, M); // X: X^3

    BigNumberMul(M, M, FCoefficientA); // M: A*X
    BigNumberAdd(X, X, M);             // X: X^3 + A*X
    BigNumberAdd(X, X, FCoefficientB); // X: X^3 + A*X + B
  finally
    RecycleBigNumberToPool(M);
  end;
end;

constructor TCnEcc.Create(const A, B, FieldPrime, GX, GY, Order: AnsiString);
begin
  Create;
  Load(A, B, FIeldPrime, GX, GY, Order);
end;

constructor TCnEcc.Create;
begin
  inherited;
  FGenerator := TCnEccPoint.Create;
  FCoefficientB := TCnBigNumber.Create;
  FCoefficientA := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;

  FSizeUFactor := TCnBigNumber.Create;
end;

constructor TCnEcc.Create(Predefined: TCnEccPredefinedCurveType);
begin
  Create;
  Load(Predefined);
end;

procedure TCnEcc.Decrypt(DataPoint1, DataPoint2: TCnEccPoint;
  PrivateKey: TCnEccPrivateKey; OutPlainPoint: TCnEccPoint);
var
  P: TCnEccPoint;
begin
  if (BigNumberCompare(PrivateKey, CnBigNumberZero) <= 0) or
    not IsPointOnCurve(DataPoint1) or not IsPointOnCurve(DataPoint2) then
    raise ECnEccException.Create('Invalid Private Key or Data.');

  P := TCnEccPoint.Create;
  try
    P.Assign(DataPoint2);
    MultiplePoint(PrivateKey, P);
    PointSubPoint(DataPoint1, P, OutPlainPoint);
  finally
    P.Free;
  end;
end;

destructor TCnEcc.Destroy;
var
  I: Integer;
begin
  if FBigNumberPool <> nil then
  begin
    for I := 0 to FBigNumberPool.Count - 1 do
      FBigNumberPool[I].Free;
    FBigNumberPool.Free;
  end;

  FSizeUFactor.Free;

  FGenerator.Free;
  FCoefficientB.Free;
  FCoefficientA.Free;
  FOrder.Free;
  FFiniteFieldSize.Free;
  inherited;
end;

procedure TCnEcc.Encrypt(PlainPoint: TCnEccPoint;
  PublicKey: TCnEccPublicKey; OutDataPoint1, OutDataPoint2: TCnEccPoint);
var
  RandomKey: TCnBigNumber;
begin
  if not IsPointOnCurve(PublicKey) or not IsPointOnCurve(PlainPoint) then
    raise ECnEccException.Create('Invalid Public Key or Data.');

  RandomKey := ObtainBigNumberFromPool;
  try
    BigNumberRandRange(RandomKey, FOrder);    // �� 0 �󵫱Ȼ����С�������
    if BigNumberIsZero(RandomKey) then
      BigNumberSetOne(RandomKey);

    // M + rK;
    OutDataPoint1.Assign(PublicKey);
    MultiplePoint(RandomKey, OutDataPoint1);
    PointAddPoint(PlainPoint, OutDataPoint1, OutDataPoint1);

    // r * G
    OutDataPoint2.Assign(FGenerator);
    MultiplePoint(RandomKey, OutDataPoint2);
  finally
    RecycleBigNumberToPool(RandomKey);
  end;
end;

procedure TCnEcc.GenerateKeys(PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey);
begin
  BigNumberRandRange(PrivateKey, FOrder);           // �� 0 �󵫱Ȼ����С�������
  if PrivateKey.IsZero then                         // ��һ���õ� 0���ͼ� 1
    PrivateKey.SetOne;

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);             // ����� PrivateKey ��
end;

function TCnEcc.GetBitsCount: Integer;
begin
  Result := FFiniteFieldSize.GetBitsCount;
end;

function TCnEcc.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, A: TCnBigNumber;
begin
  X := ObtainBigNumberFromPool;
  Y := ObtainBigNumberFromPool;
  A := ObtainBigNumberFromPool;

  try
    BigNumberCopy(X, P.X);
    BigNumberCopy(Y, P.Y);

    BigNumberMul(Y, Y, Y);                // Y: Y^2
    BigNumberMod(Y, Y, FFiniteFieldSize); // Y^2 mod P

    CalcX3AddAXAddB(X);                   // X: X^3 + A*X + B
    BigNumberMod(X, X, FFiniteFieldSize); // X: (X^3 + A*X + B) mod P
    Result := BigNumberCompare(X, Y) = 0;
  finally
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(A);
  end;
end;

procedure TCnEcc.Load(Predefined: TCnEccPredefinedCurveType);
begin
  Load(ECC_PRE_DEFINED_PARAMS[Predefined].A, ECC_PRE_DEFINED_PARAMS[Predefined].B,
    ECC_PRE_DEFINED_PARAMS[Predefined].P, ECC_PRE_DEFINED_PARAMS[Predefined].GX,
    ECC_PRE_DEFINED_PARAMS[Predefined].GY, ECC_PRE_DEFINED_PARAMS[Predefined].N);
end;

procedure TCnEcc.Load(const A, B, FieldPrime, GX, GY, Order: AnsiString);
var
  R: DWORD;
begin
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FCoefficientA.SetHex(A);
  FCoefficientB.SetHex(B);
  FFiniteFieldSize.SetHex(FieldPrime);
  FOrder.SetHex(Order);

  // TODO: Ҫȷ�� 4*a^3+27*b^2 <> 0
//  if not BigNumberIsProbablyPrime(FFiniteFieldSize) then
//    raise ECnEccException.Create('Error: Finite Field Size must be Prime.');

  // ȷ�� PrimeType
  R := BigNumberModWord(FFiniteFieldSize, 4);
  BigNumberCopy(FSizeUFactor, FFiniteFieldSize);
  if R = 3 then  // RFC 5639 Ҫ�� p ���� 4u + 3 ����ʽ�Ա㷽��ؼ��� Y������������δ��
  begin
    FSizePrimeType := pt4U3;
    BigNumberDivWord(FSizeUFactor, 4);
  end
  else
  begin
    R := BigNumberModWord(FFiniteFieldSize, 8);
    if R = 1 then
    begin
      FSizePrimeType := pt8U1;
      BigNumberDivWord(FSizeUFactor, 8);
    end
    else if R = 5 then
    begin
      FSizePrimeType := pt8U5;
      BigNumberDivWord(FSizeUFactor, 8);
    end
    else
      raise ECnEccException.Create('Invalid Finite Field Size.');
  end;
end;

procedure TCnEcc.MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
var
  I: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    PointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.X.SetZero;
    Point.Y.SetZero;
    Exit;
  end;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;
    E.X := Point.X;
    E.Y := Point.Y;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);
      PointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
  finally
    R.Free;
    E.Free;
  end;
end;

function TCnEcc.ObtainBigNumberFromPool: TCnBigNumber;
begin
  if FBigNumberPool = nil then
    Result := TCnBigNumber.Create
  else if FBigNumberPool.Count = 0 then
    Result := TCnBigNumber.Create
  else
  begin
    Result := TCnBigNumber(FBigNumberPool.Last);
    Result.Clear;
    FBigNumberPool.Delete(FBigNumberPool.Count - 1);
  end;
end;

function TCnEcc.PlainToPoint(Plain: TCnBigNumber;
  OutPoint: TCnEccPoint): Boolean;
var
  X, Y, Z, U, R, T, L, X3, C, M: TCnBigNumber;
begin
  Result := False;
  if Plain.IsNegative then
    Exit;

  if BigNumberCompare(Plain, FFiniteFieldSize) >= 0 then
    Exit;

  X := nil;
  U := nil;
  Y := nil;
  Z := nil;
  R := nil;
  T := nil;
  L := nil;
  X3 := nil;
  C := nil;
  M := nil;

  try
    X := ObtainBigNumberFromPool;
    Y := ObtainBigNumberFromPool;
    Z := ObtainBigNumberFromPool;
    U := ObtainBigNumberFromPool;
    X3 := ObtainBigNumberFromPool;

    BigNumberCopy(X, Plain);
    BigNumberCopy(U, FSizeUFactor);

    CalcX3AddAXAddB(X);
    BigNumberMod(X, X, FFiniteFieldSize);
    BigNumberCopy(X3, X);    // ����ԭʼ g

    // �ο��ԡ�SM2��Բ���߹�Կ�����㷨����¼ B �еġ�ģ����ƽ��������⡱һ�ڣ����� g �� X ���������ķ����Ұ벿��ֵ
    case FSizePrimeType of
      pt4U3:
        begin
          // ����� g^(u+1) mod p
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Y, X, U, FFiniteFieldSize);
          BigNumberMulMod(Z, Y, Y, FFiniteFieldSize);
          if BigNumberCompare(Z, X) = 0 then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            BigNumberCopy(OutPoint.Y, Y);
            Result := True;
            Exit;
          end;
        end;
      pt8U5:
        begin
          BigNumberMulWord(U, 2);
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Z, X, U, FFiniteFieldSize);
          R := ObtainBigNumberFromPool;
          BigNumberMod(R, Z, FFiniteFieldSize);

          if R.IsOne then
          begin
            // ����� g^(u+1) mod p
            BigNumberCopy(U, FSizeUFactor);
            BigNumberAddWord(U, 1);
            BigNumberMontgomeryPowerMod(Y, X, U, FFiniteFieldSize);

            BigNumberCopy(OutPoint.X, Plain);
            BigNumberCopy(OutPoint.Y, Y);
            Result := True;
          end
          else
          begin
            if R.IsNegative then
              BigNumberAdd(R, R, FFiniteFieldSize);
            BigNumberSub(R, FFiniteFieldSize, R);
            if R.IsOne then
            begin
              // �����(2g ��(4g)^u) mod p = (2g mod p * (4g)^u mod p) mod p
              BigNumberCopy(X, X3);
              BigNumberMulWord(X, 2);
              BigNumberMod(R, X, FFiniteFieldSize);  // R: 2g mod p

              BigNumberCopy(X, X3);
              BigNumberMulWord(X, 4);
              T := ObtainBigNumberFromPool;
              BigNumberMontgomeryPowerMod(T, X, FSizeUFactor, FFiniteFieldSize); // T: (4g)^u mod p
              BigNumberMulMod(Y, R, T, FFiniteFieldSize);

              BigNumberCopy(OutPoint.X, Plain);
              BigNumberCopy(OutPoint.Y, Y);
              Result := True;
            end;
          end;
        end;
      pt8U1: // Lucas ���м��㷨�� Tonelli-Shanks �㷨���ܽ���ģ��������ʣ�����
        begin
{$IFDEF USE_LUCAS}
          if BigNumberLucas(OutPoint.Y, X3, FFiniteFieldSize) then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            Result := True;
          end;
{$ELSE}
          if BigNumberTonelliShanks(OutPoint.Y, X3, FFiniteFieldSize) then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            Result := True;
          end;
{$ENDIF}
        end;
    end;
  finally
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(L);
    RecycleBigNumberToPool(X3);
    RecycleBigNumberToPool(C);
    RecycleBigNumberToPool(M);
  end;
end;

procedure TCnEcc.PointAddPoint(P, Q, Sum: TCnEccPoint);
var
  K, X, Y, A, SX, SY: TCnBigNumber;
begin
  K := nil;
  X := nil;
  Y := nil;
  A := nil;
  SX := nil;
  SY := nil;

  try
    if P.IsZero then
    begin
      Sum.Assign(Q);
      Exit;
    end
    else if Q.IsZero then
    begin
      Sum.Assign(P);
      Exit;
    end
    else if (BigNumberCompare(P.X, Q.X) = 0) and (BigNumberCompare(P.Y, Q.Y) = 0) then
    begin
      // ��������ͬһ���㣬����б��Ϊ�����󵼣�3 * X^2 + A / (2 * Y) ���� Y = 0 ��ֱ��������Զ 0��
      if P.Y.IsZero then
      begin
        Sum.SetZero;
        Exit;
      end;

      X := ObtainBigNumberFromPool;
      Y := ObtainBigNumberFromPool;
      K := ObtainBigNumberFromPool;

      // X := 3 * P.X * P.X + CoefficientA;
      BigNumberMul(X, P.X, P.X);             // X: P.X^2
      BigNumberMulWord(X, 3);                // X: 3 * P.X^2
      BigNumberAdd(X, X, FCoefficientA);     // X: 3 * P.X^2 + A

      // Y := 2 * P.Y;
      BigNumberCopy(Y, P.Y);
      BigNumberMulWord(Y, 2);                // Y: 2 * P.Y

      A := ObtainBigNumberFromPool;
      BigNumberCopy(A, Y);
      BigNumberModularInverse(Y, A, FFiniteFieldSize);

      // K := X * Y mod FFiniteFieldSize;
      BigNumberMulMod(K, X, Y, FFiniteFieldSize);      // �õ�б��
    end
    else // �ǲ�ͬ��
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // ��� X ��ȣ�Ҫ�ж� Y �ǲ��ǻ����������Ϊ 0�����������
      begin
        A := ObtainBigNumberFromPool;
        BigNumberAdd(A, P.Y, Q.Y);
        if BigNumberCompare(A, FFiniteFieldSize) = 0 then  // ��������Ϊ 0
          Sum.SetZero
        else                                               // ������������
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s', [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      // �����X ȷ����ͬ��б�� K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
      X := ObtainBigNumberFromPool;
      Y := ObtainBigNumberFromPool;
      K := ObtainBigNumberFromPool;

      // Y := Q.Y - P.Y;
      // X := Q.X - P.X;
      BigNumberSub(Y, Q.Y, P.Y);
      BigNumberSub(X, Q.X, P.X);

      A := ObtainBigNumberFromPool;
      BigNumberCopy(A, X);
      BigNumberModularInverse(X, A, FFiniteFieldSize);
      BigNumberMulMod(K, Y, X, FFiniteFieldSize);      // �õ�б��
    end;

    BigNumberCopy(X, K);
    BigNumberMul(X, X, K);
    BigNumberSub(X, X, P.X);
    BigNumberSub(X, X, Q.X);    //  X := K * K - P.X - Q.X;

    SX := ObtainBigNumberFromPool;
    if BigNumberIsNegative(X) then // ��ֵ��ģ������ֵ��ģ��ģ����
    begin
      BigNumberSetNegative(X, False);
      BigNumberMod(SX, X, FFiniteFieldSize);
      BigNumberSub(SX, FFiniteFieldSize, SX);
    end
    else
      BigNumberMod(SX, X, FFiniteFieldSize);

    // Ysum = (K * (X1 - Xsum) - Y1) mod p  ע��Ҫȡ��
    //   Ҳ = (K * (X2 - Xsum) - Y2) mod p  ע��Ҫȡ��
    BigNumberSub(X, P.X, SX);
    BigNumberMul(Y, K, X);
    BigNumberSub(Y, Y, P.Y);

    SY := ObtainBigNumberFromPool;
    if BigNumberIsNegative(Y) then
    begin
      BigNumberSetNegative(Y, False);
      BigNumberMod(SY, Y, FFiniteFieldSize);
      BigNumberSub(SY, FFiniteFieldSize, SY);
    end
    else
      BigNumberMod(SY, Y, FFiniteFieldSize);

    BigNumberCopy(Sum.X, SX);
    BigNumberCopy(Sum.Y, SY);
  finally
    RecycleBigNumberToPool(K);
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(A);
    RecycleBigNumberToPool(SX);
    RecycleBigNumberToPool(SY);
  end;
end;

procedure TCnEcc.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.Y) or (BigNumberCompare(P.Y, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create('Inverse Error.');

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

procedure TCnEcc.PointSubPoint(P, Q, Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  Inv.X := Q.X;
  Inv.Y := Q.Y;
  PointInverse(Inv);
  PointAddPoint(P, Inv, Diff);
  Inv.Free;
end;

function TCnEcc.PointToPlain(Point: TCnEccPoint;
  OutPlain: TCnBigNumber): Boolean;
begin
  Result := False;
  if (Point <> nil) and (OutPlain <> nil) and IsPointOnCurve(Point) then
  begin
    BigNumberCopy(OutPlain, Point.X);
    Result := True;
  end;
end;

procedure TCnEcc.RecycleBigNumberToPool(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if FBigNumberPool = nil then
    FBigNumberPool := TObjectList.Create(False);
  FBigNumberPool.Add(Num);
end;

function CnEccDiffieHellmanGenerateOutKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
begin
  // PublicKey = SelfPrivateKey * G
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey <> nil) and not BigNumberIsNegative(SelfPrivateKey) then
  begin
    PublicKey.Assign(Ecc.Generator);
    Ecc.MultiplePoint(SelfPrivateKey, PublicKey);
    Result := True;
  end;
end;

function CnEccDiffieHellmanComputeKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  OtherPublicKey: TCnEccPublicKey; SharedSecretKey: TCnEccPublicKey): Boolean;
begin
  // SecretKey = SelfPrivateKey * OtherPublicKey
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey <> nil) and not BigNumberIsNegative(SelfPrivateKey) then
  begin
    SharedSecretKey.Assign(OtherPublicKey);
    Ecc.MultiplePoint(SelfPrivateKey, SharedSecretKey);
    Result := True;
  end;
end;

end.
