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

unit CnMatrix;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org��
* ��    ע���߽�����ʽ�Ĵ�������ʽ���㷽��������֤ͨ�����������������ܲ�������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2019.06.12 V1.1
*               �����������������
*           2019.06.05 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs;

type
  ECnMatrixException = class(Exception);

  TCnIntMatrix = class(TPersistent)
  {* Int64 ��Χ�ڵ����������ʵ����}
  private
    FMatrix: array of array of Int64;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetValue(Row, Col: Integer): Int64;
  protected
    procedure SetValue(Row, Col: Integer; const Value: Int64); virtual;

    function Add3(X, Y, Z: Int64): Int64; virtual;
    function Mul3(X, Y, Z: Int64): Int64; virtual;
    function NegativeOnePower(N: Integer): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    destructor Destroy; override;

    // ����������ʵ���Զ���ļӼ��˳�������������Ϊ��������û�г�����
    function OperationAdd(X, Y: Int64): Int64; virtual;
    function OperationSub(X, Y: Int64): Int64; virtual;
    function OperationMul(X, Y: Int64): Int64; virtual;
    function OperationDiv(X, Y: Int64): Int64; virtual;

    procedure Mul(Factor: Int64);
    {* �����Ԫ�س���һ������}
    procedure Add(Factor: Int64);
    {* �����Ԫ�ؼ���һ������}
    procedure Divide(Factor: Int64); virtual;
    {* �����Ԫ�س���һ��������������Ϊ��������δʵ�ֳ���}

    procedure SetE(Size: Integer);
    {* ����Ϊ Size �׵�λ����}
    procedure SetZero;
    {* ����Ϊȫ 0 ����}

    function Determinant: Int64; virtual;
    {* ��������ʽֵ}
    function Trace: Int64;
    {* ����ļ���Ҳ���ǶԽ���Ԫ�صĺ�}
    function IsSquare: Boolean;
    {* �Ƿ���}
    function IsZero: Boolean;
    {* �Ƿ�ȫ 0 ����}
    function IsE: Boolean;
    {* �Ƿ�λ����}
    function IsSymmetrical: Boolean;
    {* �Ƿ�ԳƷ���}
    function IsSingular: Boolean;
    {* �Ƿ����췽��Ҳ��������ʽ�Ƿ���� 0}

    procedure DeleteRow(Row: Integer);
    {* ɾ������һ��}
    procedure DeleteCol(Col: Integer);
    {* ɾ������һ��}

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* ������ַ���}

    property Value[Row, Col: Integer]: Int64 read GetValue write SetValue;
    {* ���������±���ʾ���Ԫ�أ��±궼�� 0 ��ʼ}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* ��������}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* ��������}
  end;

  TCnRationalNumber = class(TPersistent)
  {* ��ʾһ��������}
  private
    FNominator: Int64;
    FDenominator: Int64;
    procedure SetDenominator(const Value: Int64);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�������Ҳ�����жϷ�ĸ�Ƿ������� 1}
    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�Ϊ 0}
    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�Ϊ 1}
    function IsNegative: Boolean;
    {* �Ƿ�Ϊ��ֵ}
    procedure Neg;
    {* ����෴��}
    procedure Reciprocal;
    {* ��ɵ���}
    procedure SetZero;
    {* ��Ϊ 0}
    procedure SetOne;
    {* ��Ϊ 1}

    function EqualInt(Value: Int64): Boolean;
    {* �Ƿ�����һֵ���}
    function Equal(Value: TCnRationalNumber): Boolean;
    {* �Ƿ�����һֵ���}

    procedure Add(Value: Int64); overload;
    {* ����һ������}
    procedure Sub(Value: Int64); overload;
    {* ��ȥһ������}
    procedure Mul(Value: Int64); overload;
    {* ����һ������}
    procedure Divide(Value: Int64); overload;
    {* ����һ������}
    procedure Add(Value: TCnRationalNumber); overload;
    {* ����һ��������}
    procedure Sub(Value: TCnRationalNumber); overload;
    {* ��ȥһ��������}
    procedure Mul(Value: TCnRationalNumber); overload;
    {* ����һ��������}
    procedure Divide(Value: TCnRationalNumber); overload;
    {* ����һ��������}

    procedure SetIntValue(Value: Int64);
    {* ֵ��Ϊһ������}
    procedure SetValue(ANominator, ADenominator: Int64);
    {* ֵ��Ϊһ������}
    procedure SetString(const Value: string);
    {* ֵ��Ϊһ���ַ����������Ǵ����֣���� / �ķ���}
    procedure Reduce;
    {* ����Լ��}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ַ���}
    property Nominator: Int64 read FNominator write FNominator;
    {* ����}
    property Denominator: Int64 read FDenominator write SetDenominator; 
    {* ��ĸ}
  end;

  TCn2DObjectList = class
  {* ��ά��������}
  private
    FRowCount: Integer;
    FColCount: Integer;
    FRows: TObjectList;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetValueObject(Row, Col: Integer): TObject;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetValueObject(Row, Col: Integer; const Value: TObject); // һ�� TObjectList
  public
    constructor Create(ARow, ACol: Integer);
    destructor Destroy; override;

    procedure DeleteRow(Row: Integer);
    {* ɾ��һ��}
    procedure DeleteCol(Col: Integer);
    {* ɾ��һ��}

    property ValueObject[Row, Col: Integer]: TObject read GetValueObject write SetValueObject; default;
    {* ��ά����ֵ}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* ����}
    property ColCount: Integer read GetColCount write SetColCount;
    {* ����}
  end;

  TCnRationalMatrix = class(TPersistent)
  {* ��������Χ�ڵ����������ʵ����}
  private
    FMatrix: TCn2DObjectList;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetValue(Row, Col: Integer; const Value: TCnRationalNumber);
    function GetValue(Row, Col: Integer): TCnRationalNumber;
    function GetColCount: Integer;
    function GetRowCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    destructor Destroy; override;

    procedure Mul(Factor: Int64); overload;
    {* �����Ԫ�س���һ������}
    procedure Divide(Factor: Int64); overload;
    {* �����Ԫ�س���һ������}
    procedure Add(Factor: Int64); overload;
    {* �����Ԫ�ؼ���һ������}
    procedure Mul(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�س���һ������}
    procedure Divide(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�س���һ������}
    procedure Add(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�ؼ���һ������}

    procedure SetE(Size: Integer);
    {* ����Ϊ Size �׵�λ����}
    procedure SetZero;
    {* ����Ϊȫ 0 ����}
    procedure DeleteRow(Row: Integer);
    {* ɾ������һ��}
    procedure DeleteCol(Col: Integer);
    {* ɾ������һ��}

    procedure Determinant(D: TCnRationalNumber);
    {* ��������ʽֵ}
    procedure Trace(T: TCnRationalNumber);
    {* ����ļ���Ҳ���ǶԽ���Ԫ�صĺ�}
  
    function IsSquare: Boolean;
    {* �Ƿ���}
    function IsZero: Boolean;
    {* �Ƿ�ȫ 0 ����}
    function IsE: Boolean;
    {* �Ƿ�λ����}
    function IsSymmetrical: Boolean;
    {* �Ƿ�ԳƷ���}
    function IsSingular: Boolean;
    {* �Ƿ����췽��Ҳ��������ʽ�Ƿ���� 0}

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* ������ַ���}

    property Value[Row, Col: Integer]: TCnRationalNumber read GetValue write SetValue;
    {* ���������±���ʾ���Ԫ�أ��±궼�� 0 ��ʼ}
  published
    property ColCount: Integer read GetColCount write SetColCount;
    {* ��������}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* ��������}
  end;

// ============================ �����������㷽�� ===============================

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix); overload;
{* ����������ˣ������ MulResult �����У�Ҫ�� Matrix1 ������ Martrix2 ������ȡ�
  MulResult ������ Matrix1 �� Matrix2}

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix); overload;
{* ���� K ���ݣ������ PowerResult �����У�PowerResult ������ Matrix}

procedure CnMatrixAdd(Matrix1, Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix); overload;
{* ����������ӣ������ AddResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
  AddResult ������ Matrix1 �� Matrix2 ������}

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix); overload;
{* ���������������ˣ������ ProductResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
  ProductResult ������ Matrix1 �� Matrix2 ������}

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnIntMatrix); overload;
{* ת�þ��󣬽���һ������ת�����ڶ�����Matrix1��Matrix2 �������}

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row, Col: Integer; MinorResult: TCnIntMatrix); overload;
{* ����������ʽ��Ҳ��ȥ��ָ�����к�ʣ�µľ���}

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnIntMatrix); overload;
{* ����İ�����}

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix); overload;
{* ����������Ҳ���ǰ������������ʽ��ע�� TCnIntMatrix ��ֱ��֧�������
  ��Ϊ�����ܵ��·���������Ҫ������������������ʾ��������٤�޻�����}

// =========================== �������������㷽�� ==============================

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
{* ��һ����������ת��Ϊ����������}

procedure CnMatrixMul(Matrix1, Matrix2: TCnRationalMatrix; MulResult: TCnRationalMatrix); overload;
{* ����������ˣ������ MulResult �����У�Ҫ�� Matrix1 ������ Martrix2 ������ȡ�
  MulResult ������ Matrix1 �� Matrix2}

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix); overload;
{* ���� K ���ݣ������ PowerResult �����У�PowerResult ������ Matrix}

procedure CnMatrixAdd(Matrix1, Matrix2: TCnRationalMatrix; AddResult: TCnRationalMatrix); overload;
{* ����������ӣ������ AddResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
  AddResult ������ Matrix1 �� Matrix2 ������}

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnRationalMatrix; ProductResult: TCnRationalMatrix); overload;
{* ���������������ˣ������ ProductResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
  ProductResult ������ Matrix1 �� Matrix2 ������}

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnRationalMatrix); overload;
{* ת�þ��󣬽���һ������ת�����ڶ�����Matrix1��Matrix2 �������}

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row, Col: Integer; MinorResult: TCnRationalMatrix); overload;
{* ����������ʽ��Ҳ��ȥ��ָ�����к�ʣ�µľ���}

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnRationalMatrix); overload;
{* ����İ�����}

procedure CnMatrixInverse(Matrix1, Matrix2: TCnRationalMatrix); overload;
{* ����������Ҳ���ǰ������������ʽ����Ҫ��������������ʾ}

// ============================== ���������㷽�� ===============================

procedure CnRationalNumberAdd(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������ӷ��������������}

procedure CnRationalNumberAdd3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������������ӷ�����������Ǽ���}

procedure CnRationalNumberSub(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �����������������������}

procedure CnRationalNumberMul(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������˷��������������}

procedure CnRationalNumberMul3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������������˷�����������ǳ���}

procedure CnRationalNumberDiv(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �����������������������}

function CnRationalNumberCompare(Number1, Number2: TCnRationalNumber): Integer;
{* �Ƚ�������������> = < �ֱ𷵻�1 0 -1}

procedure CnReduceInt64(var X, Y: Int64);
{* ����������С��Ҳ����Լ��}

implementation

procedure CheckCount(Value: Int64);
begin
  if Value <= 0 then
    raise ECnMatrixException.Create('Error Row or Col Count: ' + IntToStr(Value));
end;

// ���� -1 �� N �η��������������ʽ��
function InternalNegativeOnePower(N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (N and 1) * (-2) + 1;
end;

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Int <> nil) and (Rational <> nil) then
  begin
    Rational.ColCount := Int.ColCount;
    Rational.RowCount := Int.RowCount;

    for I := 0 to Rational.RowCount - 1 do
      for J := 0 to Rational.ColCount - 1 do
        Rational.Value[I, J].SetIntValue(Int.Value[I, J]);
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix);
var
  I, J, K: Integer;
  T, Sum: Int64;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create('Matrix Result can not Be Factors.');

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create('Matrix 1 Col Count must Equal to Matrix 2 Row Count.');

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  // Value[I, J] := ���� 1 �� I ������� 2 �� J �ж�Ӧ�˲����
  for I := 0 to Matrix1.RowCount - 1 do
  begin
    for J := 0 to Matrix2.ColCount - 1 do
    begin
      Sum := 0;
      for K := 0 to Matrix1.ColCount - 1 do
      begin
        T := Matrix1.OperationMul(Matrix1.Value[I, K], Matrix2.Value[K, J]);
        Sum := Matrix1.OperationAdd(Sum, T);
      end;
      MulResult.Value[I, J] := Sum;
    end;
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnRationalMatrix; MulResult: TCnRationalMatrix);
var
  I, J, K: Integer;
  T, Sum: TCnRationalNumber;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create('Matrix Result can not Be Factors.');

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create('Matrix 1 Col Count must Equal to Matrix 2 Row Count.');

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  Sum := TCnRationalNumber.Create;
  T := TCnRationalNumber.Create;

  // Value[I, J] := ���� 1 �� I ������� 2 �� J �ж�Ӧ�˲����
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        Sum.SetIntValue(0);
        for K := 0 to Matrix1.ColCount - 1 do
        begin
          CnRationalNumberMul(Matrix1.Value[I, K], Matrix2.Value[K, J], T);
          CnRationalNumberAdd(Sum, T, Sum);
        end;
        MulResult.Value[I, J] := Sum;
      end;
    end;
  finally
    Sum.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix);
var
  I: Integer;
  T: TCnIntMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create('Matrix Power Must Be Square.');

  if K < 0 then
    raise ECnMatrixException.Create('Invalid Matrix Power.');

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnIntMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix);
var
  I: Integer;
  T: TCnRationalMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create('Matrix Power Must Be Square.');

  if K < 0 then
    raise ECnMatrixException.Create('Invalid Matrix Power.');

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnRationalMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnRationalMatrix; AddResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberAdd(Matrix1.Value[I, J], Matrix2.Value[I, J], AddResult.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnRationalMatrix; ProductResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberMul(Matrix1.Value[I, J], Matrix2.Value[I, J], ProductResult.Value[I, J]);
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Tmp: TCnIntMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnIntMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Tmp: TCnRationalMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnRationalMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row, Col: Integer; MinorResult: TCnIntMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.Create('Invalid Minor Row or Col.');

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row, Col: Integer; MinorResult: TCnRationalMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.Create('Invalid Minor Row or Col.');

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Minor: TCnIntMatrix;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create('Only Square can Adjoint.');

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnIntMatrix(Matrix1.ClassType.NewInstance);
  Minor.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1); // ������ʵ��

  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := Matrix1.NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create('Only Square can Adjoint.');

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnRationalMatrix.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1);
  T := TCnRationalNumber.Create;
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I + J));
        Matrix2.Value[I, J] := T;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    T.Free;
    Minor.Free;
  end;
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix);
var
  D: Int64;
begin
  D := Matrix1.Determinant;
  if D = 0 then
    raise ECnMatrixException.Create('NO Inverse Matrix for Deteminant is 0');

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D);
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnRationalMatrix);
var
  D: TCnRationalNumber;
begin
  D := TCnRationalNumber.Create;
  try
    Matrix1.Determinant(D);
    if D.IsZero then
      raise ECnMatrixException.Create('NO Inverse Matrix for Deteminant is 0');

    CnMatrixAdjoint(Matrix1, Matrix2);
    Matrix2.Divide(D);
  finally
    D.Free;
  end;
end;

{ TCnIntMatrix }

procedure TCnIntMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationAdd(FMatrix[I, J], Factor);
end;

function TCnIntMatrix.Add3(X, Y, Z: Int64): Int64;
begin
  Result := OperationAdd(OperationAdd(X, Y), Z);
end;

procedure TCnIntMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnIntMatrix then
  begin
    TCnIntMatrix(Dest).RowCount := FRowCount;
    TCnIntMatrix(Dest).ColCount := FColCount;

    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        TCnIntMatrix(Dest).Value[I, J] := FMatrix[I, J];
  end
  else
    inherited;
end;

constructor TCnIntMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

procedure TCnIntMatrix.DeleteCol(Col: Integer);
var
  T: array of array of Int64;
  I, J, SJ, DJ: Integer;
begin
  if (Col >= 0) or (Col < FColCount) then
  begin
    // ��ÿ Row ��Ԫ��ȡ�����ŵ���ʱ T ��޳��� Col ��
    SetLength(T, FRowCount, FColCount - 1);

    for I := 0 to FRowCount - 1 do
    begin
      SJ := 0;
      DJ := 0;
      while SJ < FColCount do
      begin
        if SJ = Col then
        begin
          Inc(SJ);
          Continue;
        end;
        T[I, DJ] := FMatrix[I, SJ];
        Inc(SJ);
        Inc(DJ);
      end;
    end;

    Dec(FColCount);
    SetLength(FMatrix, FRowCount, FColCount);
    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        FMatrix[I, J] := T[I, J];

    SetLength(T, 0);
  end;
end;

procedure TCnIntMatrix.DeleteRow(Row: Integer);
var
  I, J: Integer;
begin
  if (Row >= 0) or (Row < FRowCount) then
  begin
    // �ѵ� Row + 1 �е� FRowCount - 1 �е�һά���鳯ǰ�ƶ�һ��ĩ��ʱ������
    if Row < FRowCount - 1 then
    begin
      for I := Row + 1 to FRowCount - 1 do
      begin
        for J := 0 to FColCount - 1 do
        begin
          FMatrix[I - 1, J] := FMatrix[I, J];
        end;
      end;
    end;
    Dec(FRowCount);
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

destructor TCnIntMatrix.Destroy;
begin
  SetLength(FMatrix, 0);
  inherited;
end;

function TCnIntMatrix.Determinant: Int64;
var
  I: Integer;
  Minor: TCnIntMatrix;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square can Determinant.');

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := OperationSub(Add3(Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2]),
      Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0]),
      Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])),
      Add3(Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1]),
        Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2]),
        Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0])));
  end
  else
  begin
    // ���ô�������ʽ Minor/Cofactor ����߽�����ʽ
    Result := 0;
    Minor := TCnIntMatrix(ClassType.NewInstance); // ��Ҫ���������ͳһ������
    Minor.Create(FRowCount - 1, FColCount - 1);

    // Minor := Self.clas TCnIntMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := OperationAdd(Result, Mul3(FMatrix[0, I], NegativeOnePower(I), Minor.Determinant));
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnIntMatrix.Divide(Factor: Int64);
begin
  raise ECnMatrixException.Create('Divide NOT Implemented in Int Matrix.');
end;

procedure TCnIntMatrix.DumpToStrings(List: TStrings; Sep: Char = ' ');
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to FRowCount - 1 do
  begin
    S := '';
    for J := 0 to FColCount - 1 do
    begin
      if J = 0 then
        S := IntToStr(FMatrix[I, J])
      else
        S := S + Sep + IntToStr(FMatrix[I, J]);
    end;
    List.Add(S);
  end;
end;

function TCnIntMatrix.GetValue(Row, Col: Integer): Int64;
begin
  Result := FMatrix[Row, Col];
end;

function TCnIntMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if (I = J) and (FMatrix[I, J] <> 1) then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and (FMatrix[I, J] <> 0) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnIntMatrix.IsSingular: Boolean;
begin
  if not IsSquare then
    Result := False
  else
    Result := Determinant = 0;
end;

function TCnIntMatrix.IsSquare: Boolean;
begin
  Result := (FColCount = FRowCount);
end;

function TCnIntMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to I do
      if FMatrix[I, J] <> FMatrix[J, I] then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnIntMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      if FMatrix[I, J] <> 0 then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

procedure TCnIntMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationMul(FMatrix[I, J], Factor);
end;

function TCnIntMatrix.Mul3(X, Y, Z: Int64): Int64;
begin
  Result := OperationMul(OperationMul(X, Y), Z);
end;

function TCnIntMatrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := InternalNegativeOnePower(N);
end;

function TCnIntMatrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnIntMatrix.OperationDiv(X, Y: Int64): Int64;
begin
  raise ECnMatrixException.Create('Operation Div NOT Implemented in Int Matrix.');
end;

function TCnIntMatrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

function TCnIntMatrix.OperationSub(X, Y: Int64): Int64;
begin
  Result := X - Y;
end;

procedure TCnIntMatrix.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    CheckCount(Value);
    FColCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        FMatrix[I, J] := 1
      else
        FMatrix[I, J] := 0;
end;

procedure TCnIntMatrix.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    CheckCount(Value);
    FRowCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetValue(Row, Col: Integer; const Value: Int64);
begin
  FMatrix[Row, Col] := Value;
end;

procedure TCnIntMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

function TCnIntMatrix.Trace: Int64;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square Matrix can Trace.');

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

{ TCnRationalNumber }

procedure TCnRationalNumber.Add(Value: TCnRationalNumber);
begin
  CnRationalNumberAdd(Self, Value, Self);
end;

procedure TCnRationalNumber.Add(Value: Int64);
begin
  FNominator := FNominator + Value * FDenominator;
end;

procedure TCnRationalNumber.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnRationalNumber then
  begin
    TCnRationalNumber(Dest).Nominator := FNominator;
    TCnRationalNumber(Dest).Denominator := FDenominator;
  end
  else
    inherited;
end;

constructor TCnRationalNumber.Create;
begin
  FDenominator := 1;
end;

destructor TCnRationalNumber.Destroy;
begin
  inherited;

end;

procedure TCnRationalNumber.Divide(Value: TCnRationalNumber);
begin
  CnRationalNumberDiv(Self, Value, Self);
end;

procedure TCnRationalNumber.Divide(Value: Int64);
begin
  Denominator := FDenominator * Value;
  Reduce;
end;

function TCnRationalNumber.Equal(Value: TCnRationalNumber): Boolean;
begin
  Result := FNominator * Value.Denominator = FDenominator * Value.Nominator;
end;

function TCnRationalNumber.EqualInt(Value: Int64): Boolean;
begin
  Result := FNominator = FDenominator * Value;
end;

function TCnRationalNumber.IsInt: Boolean;
begin
  Result := (FDenominator = 1) or (FDenominator = -1);
end;

function TCnRationalNumber.IsNegative: Boolean;
begin
  Result := ((FNominator < 0) and (FDenominator > 0))
    or ((FNominator > 0) and (FDenominator < 0))
end;

function TCnRationalNumber.IsOne: Boolean;
begin
  Result := FNominator = FDenominator;
end;

function TCnRationalNumber.IsZero: Boolean;
begin
  Result := FNominator = 0;
end;

procedure TCnRationalNumber.Mul(Value: TCnRationalNumber);
begin
  CnRationalNumberMul(Self, Value, Self);
end;

procedure TCnRationalNumber.Mul(Value: Int64);
begin
  FNominator := FNominator * Value;
  Reduce;
end;

procedure TCnRationalNumber.Neg;
begin
  FNominator := - FNominator;
end;

procedure TCnRationalNumber.Reciprocal;
var
  T: Int64;
begin
  T := FDenominator;
  FDenominator := FNominator;
  FNominator := T;
end;

procedure TCnRationalNumber.Reduce;
begin
  if (FDenominator < 0) and (FNominator < 0) then
  begin
    FDenominator := -FDenominator;
    FNominator := -FNominator;
  end;

  if FNominator = 0 then
  begin
    FDenominator := 1;
    Exit;
  end;

  if not IsInt then
    CnReduceInt64(FNominator, FDenominator);
end;

procedure TCnRationalNumber.SetDenominator(const Value: Int64);
begin
  if Value = 0 then
    raise EDivByZero.Create('Denominator can NOT be Zero.');

  FDenominator := Value;
end;

procedure TCnRationalNumber.SetIntValue(Value: Int64);
begin
  FDenominator := 1;
  FNominator := Value;
end;

procedure TCnRationalNumber.SetOne;
begin
  FDenominator := 1;
  FNominator := 1;
end;

procedure TCnRationalNumber.SetString(const Value: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Value);
  if P > 1 then
  begin
    N := Copy(Value, 1, P - 1);
    D := Copy(Value, P + 1, MaxInt);
    FNominator := StrToInt64(N);
    FDenominator := StrToInt64(D);
  end
  else
  begin
    FNominator := StrToInt64(Value);
    FDenominator := 1;
  end;
end;

procedure TCnRationalNumber.SetValue(ANominator, ADenominator: Int64);
begin
  Denominator := ADenominator;
  Nominator := ANominator;
end;

procedure TCnRationalNumber.SetZero;
begin
  FDenominator := 1;
  FNominator := 0;
end;

procedure TCnRationalNumber.Sub(Value: TCnRationalNumber);
begin
  CnRationalNumberSub(Self, Value, Self);
end;

procedure TCnRationalNumber.Sub(Value: Int64);
begin
  FNominator := FNominator - Value * FDenominator;
end;

function TCnRationalNumber.ToString: string;
begin
  if IsInt or (FNominator = 0) then
    Result := IntToStr(FNominator)
  else
    Result := IntToStr(FNominator) + '/' + IntToStr(FDenominator);
end;

// ������ Int64 �����Լ����Ҫ�󶼴��� 0
function Int64Gcd(A, B: Int64): Int64;
begin
  if B = 0 then
    Result := A
  else
    Result := Int64Gcd(B, A mod B);
end;

// ������ Int64 ����С��������Ҫ�󶼴��� 0���ݲ����ǿ�����������
function Int64Lcm(A, B: Int64): Int64;
var
  D: Int64;
begin
  if A = B then
  begin
    Result := A;
    Exit;
  end;
  
  D := Int64Gcd(A, B);
  if D = 1 then
    Result := A * B
  else
  begin
    // �����ȳ����������
    if A > B then
      Result := A div D * B
    else
      Result := B div D * A;
  end;
end;

procedure CnRationalNumberAdd(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Nominator := Number1.Nominator + Number2.Nominator;
  end
  else
  begin
    // ���ĸ����С������
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Nominator := Number1.Nominator * F1 * SIGN_ARRAY[B1]
      + Number2.Nominator * F2 * SIGN_ARRAY[B2]; // ������������ް취
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberAdd3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberAdd(Number1, Number2, RationalResult);
  CnRationalNumberAdd(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberSub(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Nominator := Number1.Nominator - Number2.Nominator;
  end
  else
  begin
    // ���ĸ����С������
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Nominator := Number1.Nominator * F1 * SIGN_ARRAY[B1]
      - Number2.Nominator * F2 * SIGN_ARRAY[B2]; // ������������ް취
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberMul(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // ���� Number1��Number2 �����Ѿ�Լ���ˣ�ֱ�ӳ�����������Ȼ���Լ
  X := Number1.Nominator;
  Y := Number2.Denominator;
  CnReduceInt64(X, Y);
  if X < Number1.Nominator then
  begin
    // ��Լ����
    RationalResult.Nominator := X * Number2.Nominator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Nominator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      // �е�Լ��
      RationalResult.Nominator := Number1.Nominator * Y;
      RationalResult.Denominator := X * Number2.Denominator;
    end
    else
    begin
      RationalResult.Nominator := Number1.Nominator * Number2.Nominator;
      RationalResult.Denominator := Number1.Denominator * Number2.Denominator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnRationalNumberMul3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberMul(Number1, Number2, RationalResult);
  CnRationalNumberMul(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberDiv(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // ���� Number1��Number2 �����Ѿ�Լ���ˣ�ֱ�ӳ�����������Ȼ���Լ
  X := Number1.Nominator;
  Y := Number2.Nominator;
  CnReduceInt64(X, Y);
  if X < Number1.Nominator then
  begin
    RationalResult.Nominator := X * Number2.Denominator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Denominator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      RationalResult.Nominator := Number1.Nominator * Y;
      RationalResult.Denominator := X * Number2.Nominator;
    end
    else
    begin
      RationalResult.Nominator := Number1.Nominator * Number2.Denominator;
      RationalResult.Denominator := Number1.Denominator * Number2.Nominator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnReduceInt64(var X, Y: Int64);
var
  D: Int64;
begin
  D := Int64Gcd(X, Y);
  if D > 1 then
  begin
    X := X div D;
    Y := Y div D;
  end;
end;

function CnRationalNumberCompare(Number1, Number2: TCnRationalNumber): Integer;
var
  R: Int64;
begin
  if not Number1.IsNegative and Number2.IsNegative then
    Result := 1
  else if Number1.IsNegative and not Number2.IsNegative then
    Result := -1
  else  // ͬ���Ų���Ҫ����
  begin
    R := Number1.Nominator * Number2.Denominator - Number2.Nominator * Number1.Denominator;
    if R > 0 then
      Result := 1
    else if R < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

{ TCn2DObjectList }

constructor TCn2DObjectList.Create(ARow, ACol: Integer);
begin
  inherited Create;
  CheckCount(ARow);
  CheckCount(ACol);

  FRows := TObjectList.Create(True);
  RowCount := ARow;
  ColCount := ACol;
end;

procedure TCn2DObjectList.DeleteCol(Col: Integer);
var
  I: Integer;
begin
  for I := 0 to FRowCount - 1 do
    TObjectList(FRows[I]).Delete(Col);
  Dec(FColCount);
end;

procedure TCn2DObjectList.DeleteRow(Row: Integer);
begin
  FRows.Delete(Row);
  Dec(FRowCount);
end;

destructor TCn2DObjectList.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TCn2DObjectList.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TCn2DObjectList.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TCn2DObjectList.GetValueObject(Row, Col: Integer): TObject;
begin
  Result := TObjectList(FRows[Row])[Col];
end;

procedure TCn2DObjectList.SetColCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FColCount then
  begin
    CheckCount(Value);
    FColCount := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
        FRows[I] := TObjectList.Create(True);

      TObjectList(FRows[I]).Count := FColCount;
    end;
  end;
end;

procedure TCn2DObjectList.SetRowCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FRowCount then
  begin
    CheckCount(Value);
    FRowCount := Value;
    FRows.Count := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
      begin
        FRows[I] := TObjectList.Create(True);
        TObjectList(FRows[I]).Count := FColCount;
      end;
    end;
  end;
end;

procedure TCn2DObjectList.SetValueObject(Row, Col: Integer; const Value: TObject);
begin
  TObjectList(FRows[Row])[Col] := Value;
end;

{ TCnRationalMatrix }

procedure TCnRationalMatrix.Add(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnRationalMatrix then
  begin
    TCnRationalMatrix(Dest).RowCount := RowCount;
    TCnRationalMatrix(Dest).ColCount := ColCount;

    for I := 0 to RowCount - 1 do
      for J := 0 to ColCount - 1 do
        TCnRationalMatrix(Dest).Value[I, J] := TCnRationalNumber(FMatrix[I, J]);
  end
  else
    inherited;
end;

constructor TCnRationalMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FMatrix := TCn2DObjectList.Create(ARowCount, AColCount);
end;

procedure TCnRationalMatrix.DeleteCol(Col: Integer);
begin
  FMatrix.DeleteCol(Col);
end;

procedure TCnRationalMatrix.DeleteRow(Row: Integer);
begin
  FMatrix.DeleteRow(Row);
end;

destructor TCnRationalMatrix.Destroy;
begin
  FMatrix.Free;
  inherited;
end;

procedure TCnRationalMatrix.Determinant(D: TCnRationalNumber);
var
  I: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square can Determinant.');

  if RowCount = 1 then
    D.Assign(Value[0, 0])
  else if RowCount = 2 then
  begin
    T := TCnRationalNumber.Create;
    try
      CnRationalNumberMul(Value[0, 0], Value[1, 1], D);
      CnRationalNumberMul(Value[0, 1], Value[1, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end;
    // [0, 0] * [1, 1] - [0, 1] * [1, 0]
  end
  else if RowCount = 3 then
  begin
    T := TCnRationalNumber.Create;
    D.SetZero;
    try
      CnRationalNumberMul3(Value[0, 0], Value[1, 1], Value[2, 2], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 2], Value[2, 0], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 0], Value[2, 1], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 0], Value[1, 2], Value[2, 1], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 0], Value[2, 2], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 1], Value[2, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end
//    Result := Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2])
//      + Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0])
//      + Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2])
//      - Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0]);
  end
  else
  begin
    // ���ô�������ʽ Minor/Cofactor ����߽�����ʽ
    D.SetZero;
    Minor := TCnRationalMatrix.Create(RowCount - 1, ColCount - 1);
    T := TCnRationalNumber.Create;
    try
      for I := 0 to ColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);

        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I));
        T.Mul(Value[0, I]);
        D.Add(T);
        // Result := Result + (FMatrix[0, I] * NegativeOnePower(I)* Minor.Determinant));
      end;
    finally
      Minor.Free;
      T.Free;
    end;
  end;
end;

procedure TCnRationalMatrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.Divide(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.DumpToStrings(List: TStrings; Sep: Char);
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to RowCount - 1 do
  begin
    S := '';
    for J := 0 to ColCount - 1 do
    begin
      if J = 0 then
        S := Value[I, J].ToString
      else
        S := S + Sep + Value[I, J].ToString;
    end;
    List.Add(S);
  end;
end;

function TCnRationalMatrix.GetColCount: Integer;
begin
  Result := FMatrix.ColCount;
end;

function TCnRationalMatrix.GetRowCount: Integer;
begin
  Result := FMatrix.RowCount;
end;

function TCnRationalMatrix.GetValue(Row, Col: Integer): TCnRationalNumber;
begin
  Result := TCnRationalNumber(FMatrix[Row, Col]);
  if Result = nil then
  begin
    Result := TCnRationalNumber.Create;
    FMatrix[Row, Col] := Result;
  end;
end;

function TCnRationalMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if (I = J) and not Value[I, J].IsOne then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and not Value[I, J].IsZero then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnRationalMatrix.IsSingular: Boolean;
var
  D: TCnRationalNumber;
begin
  if not IsSquare then
    Result := False
  else
  begin
    D := TCnRationalNumber.Create;
    try
      Determinant(D);
      Result := D.IsZero;
    finally
      D.Free;
    end;
  end;
end;

function TCnRationalMatrix.IsSquare: Boolean;
begin
  Result := (ColCount = RowCount);
end;

function TCnRationalMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
    for J := 0 to I do
      if not Value[I, J].Equal(Value[J, I]) then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnRationalMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      if not Value[I, J].IsZero then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

procedure TCnRationalMatrix.Mul(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.SetColCount(const Value: Integer);
begin
  FMatrix.ColCount := Value;
end;

procedure TCnRationalMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        Value[I, J].SetOne
      else
        Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.SetRowCount(const Value: Integer);
begin
  FMatrix.RowCount := Value;
end;

procedure TCnRationalMatrix.SetValue(Row, Col: Integer;
  const Value: TCnRationalNumber);
begin
  if FMatrix[Row, Col] = nil then
    FMatrix[Row, Col] := TCnRationalNumber.Create;
  TCnRationalNumber(FMatrix[Row, Col]).Assign(Value);
end;

procedure TCnRationalMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.Trace(T: TCnRationalNumber);
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square Matrix can Trace.');

  T.SetZero;
  for I := 0 to RowCount - 1 do
    T.Add(Value[I, I]);
end;

end.

