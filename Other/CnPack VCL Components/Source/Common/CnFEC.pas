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

unit CnFEC;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�ǰ�����ʵ�ֵ�Ԫ��Ŀǰ���� Hamming �������룩У��
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org��
* ��    ע���õ�ԪĿǰֻ���������λ
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2019.06.20 V1.1
*               ʵ��٤�޻� 2^8 ���������
*           2019.05.28 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnMatrix;

type
  ECnHammingException = class(Exception);

  ECnCalculationRuleException = class(Exception);

  TCnCalculationRule = class
  {* ��������������������ʵ���������������}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(X, Y: Int64): Int64; virtual;
    function Subtract(X, Y: Int64): Int64; virtual;
    function Multiply(X, Y: Int64): Int64; virtual;
    function Divide(X, Y: Int64): Int64; virtual;
  end;

  TCnGalois2Power8Rule = class(TCnCalculationRule)
  {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����������}
  private
    FExpToValue: array[0..255] of Integer;
    FValueToExp: array[0..255] of Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Add(X, Y: Int64): Int64; override;
    function Subtract(X, Y: Int64): Int64; override;
    function Multiply(X, Y: Int64): Int64; override;
    function Divide(X, Y: Int64): Int64; override;
  end;

  TCnGalois2Power8Matrix = class(TCnIntMatrix)
  {* ٤�޻��� GP(2^8) ��Ķ���ʽ����}
  protected
    procedure SetValue(Row, Col: Integer; const Value: Int64); override;
    function NegativeOnePower(N: Integer): Integer; override;
    // ����ʽ�����еļӼ��滻������Ϊ�Ӽ���Ϊ�����˺㶨���� 1
  public
    function OperationAdd(X, Y: Int64): Int64; override;
    function OperationSub(X, Y: Int64): Int64; override;
    function OperationMul(X, Y: Int64): Int64; override;
    function OperationDiv(X, Y: Int64): Int64; override;

    function Determinant: Int64; override;
    procedure Divide(Factor: Int64); override;
  end;

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* ����һ�� Bits ������ Hamming �룬Ĭ�Ϸ��� 8 bit Ҳ���� 1 �ֽ�}

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* ���� Hamming ������� Bits ��ԭ��У�������ݣ�Ĭ�Ϸ��� 8 bit Ҳ���� 1 �ֽ�}

function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
{* ���� Hamming ����� bit ���ȼ���У�� bit �ĳ���}

function CnGalois2Power8Rule: TCnCalculationRule;
{* ����ȫ�ֵ� GP(2^8) ���������������}

implementation

const
  GALOIS2POWER8_LIMIT = 255;  // ٤�޻��� 2^8 �����Χ
  GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL = $12D; // ٤�޻��� 2^8 ʹ�õĲ���Լ����ʽ֮һ����ȡģ��

var
  FGalois2Power8Rule: TCnCalculationRule = nil;

{* ����ȫ�ֵ� GP(2^8) ���������}
function CnGalois2Power8Rule: TCnCalculationRule;
begin
  if FGalois2Power8Rule = nil then
    FGalois2Power8Rule := TCnGalois2Power8Rule.Create;
  Result := FGalois2Power8Rule;
end;

// BlockBitCount (n), VerificationBitCount (k) ���� 2^k - 1 >= n + k
function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
begin
  if BlockBitCount = 1 then
    Result := 2
  else if BlockBitCount in [2..4] then
    Result := 3
  else if BlockBitCount in [5..11] then
    Result := 4
  else if BlockBitCount in [12..26] then
    Result := 5
  else if BlockBitCount in [27..57] then
    Result := 6
  else if BlockBitCount in [58..120] then
    Result := 7
  else
    raise ECnHammingException.CreateFmt('Error Hamming BlockBitCount: %d', [BlockBitCount]);
end;

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure CalcHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx: Integer;
    Ver: Boolean;
  begin
    InIdx := 0;
    OutIdx := 0;

    // ���������ݲ�������������� Hamming ��У��λ�ռ�
    while InIdx < BlockBitCount do
    begin
      while OutIdx in VERIFICATION_BITS do
      begin
        OutBits.Bits[OutStartOffset + OutIdx] := False;
        Inc(OutIdx);
      end;
      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;

    BitIdx := 0;

    // �����λ Hamming ��У��λ�����ȥ��BitIdx �� 0 ��ʼ��VerificationBitCount - 1 ������
    // Ϊ�˱�����⣬OutIdx ��������±���ĳ� 1 ��ʼ
    while BitIdx < VerificationBitCount do
    begin
      // ���� BitIdx �� Hamming У���룬���㷽���������У��±����ֵ� BitIdx λΪ 1 ��Ҫ�������
      // ��������±�Ϊ VERIFICATION_BITS_COUNT[BitIdx] ��
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor OutBits.Bits[OutStartOffset + OutIdx - 1];
      end;
      OutBits.Bits[OutStartOffset + VERIFICATION_BITS_COUNT[BitIdx] - 1] := Ver;

      Inc(BitIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create('Error InBits Calculate Hamming.');

  if InBits.Size mod BlockBitCount <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Block Bit Count %d.', [InBits.Size, BlockBitCount]);

  OutBits.Size := (InBits.Size div BlockBitCount) * (BlockBitCount + VerificationBitCount);
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
  begin
    CalcHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount);
    Inc(OffsetOut, BlockBitCount + VerificationBitCount);
  end;
end;

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure VerifyHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx, ErrIdx: Integer;
    Ver: Boolean;
  begin
    BitIdx := 0;
    ErrIdx := 0;

    // �����λ Hamming ��У��λ�����ȥ��BitIdx �� 0 ��ʼ��VerificationBitCount - 1 ������
    // Ϊ�˱�����⣬OutIdx ��������±���ĳ� 1 ��ʼ
    while BitIdx < VerificationBitCount do
    begin
      // ���� BitIdx �� Hamming У���룬���㷽���������У��±����ֵ� BitIdx λΪ 1 ��Ҫ�������
      // ��������±�Ϊ VERIFICATION_BITS_COUNT[BitIdx] ��
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor InBits.Bits[InStartOffset + OutIdx - 1];
      end;

      if Ver then  // �д���ƴ����λ��
        ErrIdx := ErrIdx or (1 shl BitIdx);

      Inc(BitIdx);
    end;

    // ����һλ��
    if ErrIdx <> 0 then
    begin
      InBits.Bits[InStartOffset + ErrIdx - 1] := not
        InBits.Bits[InStartOffset + ErrIdx - 1];
    end;

    InIdx := 0;
    OutIdx := 0;
    // ������Ϻ󣬲��������ݲ�����������
    while InIdx < BlockBitCount + VerificationBitCount do
    begin
      while InIdx in VERIFICATION_BITS do
        Inc(InIdx);

      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create('Error InBits Verify Hamming.');

  if InBits.Size mod (BlockBitCount + VerificationBitCount) <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Verify Bit Count %d.', [InBits.Size, VerificationBitCount]);

  OutBits.Size := (InBits.Size div (VerificationBitCount + BlockBitCount)) * BlockBitCount;
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
  begin
    VerifyHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount + VerificationBitCount);
    Inc(OffsetOut, BlockBitCount);
  end;
end;

{ TCnCalculationRule }

function TCnCalculationRule.Add(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnCalculationRule.Subtract(X, Y: Int64): Int64;
begin
  Result := X - Y;
end;

function TCnCalculationRule.Multiply(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

function TCnCalculationRule.Divide(X, Y: Int64): Int64;
begin
  Result := X div Y;
end;

constructor TCnCalculationRule.Create;
begin

end;

destructor TCnCalculationRule.Destroy;
begin
  inherited;

end;

{ TCnGalois2Power8Rule }

procedure CheckGalois2Power8Value(X: Int64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (X < 0) or (X > GALOIS2POWER8_LIMIT) then
    raise ECnCalculationRuleException.CreateFmt('Out of Range for Galois 2^8: %d', [X]);
end;

procedure CheckGalois2Power8Values(X, Y: Int64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (X < 0) or (X > GALOIS2POWER8_LIMIT) or
    (Y < 0) or (Y > GALOIS2POWER8_LIMIT) then
    raise ECnCalculationRuleException.CreateFmt('Out of Range for Galois 2^8: %d, %d', [X, Y]);
end;

function TCnGalois2Power8Rule.Add(X, Y: Int64): Int64;
begin
  CheckGalois2Power8Values(X, Y);
  Result := X xor Y;
end;

function TCnGalois2Power8Rule.Subtract(X, Y: Int64): Int64;
begin
  CheckGalois2Power8Values(X, Y);
  Result := X xor Y;
end;

function TCnGalois2Power8Rule.Multiply(X, Y: Int64): Int64;
var
  A, B: Integer;
begin
  CheckGalois2Power8Values(X, Y);
  if (X = 0) or (Y = 0) then
  begin
    Result := 0;
    Exit;
  end;
  // �鵽����������ӣ���ԭ
  A := FValueToExp[X];
  B := FValueToExp[Y];

  A := (A + B) mod GALOIS2POWER8_LIMIT;
  Result := FExpToValue[A];
end;

function TCnGalois2Power8Rule.Divide(X, Y: Int64): Int64;
var
  A, B: Integer;
begin
  CheckGalois2Power8Values(X, Y);
  // �鵽���������������ԭ
  if X = 0 then
  begin
    Result := 0;
    Exit;
  end;

  A := FValueToExp[X];
  B := FValueToExp[Y];
  if A < B then
    A := A + GALOIS2POWER8_LIMIT;

  A := (A - B) mod GALOIS2POWER8_LIMIT;
  Result := FExpToValue[A];
end;

constructor TCnGalois2Power8Rule.Create;
var
  I, J: Integer;
begin
  inherited;
  // ������Ԫ x ��������������������Ԫ�ص�����ӳ���
  // ��Ӧ����Լ����ʽ�� x8+x5+x3+x2+1��Ҳ����1 0010 1101

  FExpToValue[0] := 1;
  for I := 1 to 254 do
  begin
    J := FExpToValue[I - 1] shl 1;
    if (J and $100) <> 0 then
      J := J xor GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL;
    FExpToValue[I] := J;
  end;
  FExpToValue[255] := 0;

  FValueToExp[0] := 255;
  FValueToExp[1] := 0;
  for I := 1 to 254 do
    FValueToExp[FExpToValue[I]] := I;
end;

destructor TCnGalois2Power8Rule.Destroy;
begin

  inherited;
end;

{ TCnGalois2Power8Matrix }

function TCnGalois2Power8Matrix.Determinant: Int64;
begin
  Result := inherited Determinant;
  if Result < 0 then
    Inc(Result, GALOIS2POWER8_LIMIT)
  else
    Result := Result mod GALOIS2POWER8_LIMIT;
end;

procedure TCnGalois2Power8Matrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J] := OperationDiv(Value[I, J], Factor);
end;

function TCnGalois2Power8Matrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := 1;
end;

function TCnGalois2Power8Matrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Add(X, Y);
end;

function TCnGalois2Power8Matrix.OperationDiv(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Divide(X, Y);
end;

function TCnGalois2Power8Matrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Multiply(X, Y);
end;

function TCnGalois2Power8Matrix.OperationSub(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Subtract(X, Y);
end;

procedure TCnGalois2Power8Matrix.SetValue(Row, Col: Integer;
  const Value: Int64);
begin
  CheckGalois2Power8Value(Value);
  inherited;
end;

end.
