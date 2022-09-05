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

unit CnBigRational;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����޾���������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org��
* ��    ע���ô������ı�ֵ��ʾ������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2019.12.19 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnBigNumber;

type
  TCnBigRationalNumber = class(TPersistent)
  {* ��ʾһ�����޾��ȵĴ�������}
  private
    FNominator: TCnBigNumber;
    FDenominator: TCnBigNumber;
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

    function EqualInt(Value: LongWord): Boolean; overload;
    {* �Ƿ�����һֵ���}
    function EqualInt(Value: TCnBigNumber): Boolean; overload;
    {* �Ƿ�����һֵ���}
    function Equal(Value: TCnBigRationalNumber): Boolean;
    {* �Ƿ�����һֵ���}

    procedure Add(Value: Int64); overload;
    {* ����һ������}
    procedure Sub(Value: Int64); overload;
    {* ��ȥһ������}
    procedure Mul(Value: Int64); overload;
    {* ����һ������}
    procedure Divide(Value: Int64); overload;
    {* ����һ������}
    procedure Add(Value: TCnBigNumber); overload;
    {* ����һ������}
    procedure Sub(Value: TCnBigNumber); overload;
    {* ��ȥһ������}
    procedure Mul(Value: TCnBigNumber); overload;
    {* ����һ������}
    procedure Divide(Value: TCnBigNumber); overload;
    {* ����һ������}
    procedure Add(Value: TCnBigRationalNumber); overload;
    {* ����һ��������}
    procedure Sub(Value: TCnBigRationalNumber); overload;
    {* ��ȥһ��������}
    procedure Mul(Value: TCnBigRationalNumber); overload;
    {* ����һ��������}
    procedure Divide(Value: TCnBigRationalNumber); overload;
    {* ����һ��������}

    procedure SetIntValue(Value: LongWord); overload;
    {* ֵ��Ϊһ������}
    procedure SetIntValue(Value: TCnBigNumber); overload;
    {* ֵ��Ϊһ������}
    procedure SetValue(ANominator, ADenominator: TCnBigNumber); overload;
    {* ֵ��Ϊһ������}
    procedure SetValue(const ANominator, ADenominator: string); overload;
    {* ֵ��Ϊһ���������������ַ����ķ�ʽ����}
    procedure SetString(const Value: string);
    {* ֵ��Ϊһ���ַ����������Ǵ����֣���� / �ķ�������С��}
    procedure SetFloat(AFloat: Extended);
    {* ֵ��Ϊһ�����������Ѹ���������Ч���ֺ�ָ���𿪴���}
    procedure Reduce;
    {* ����Լ��}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ַ���}
    function ToDec(Digits: Integer = 20): string;
    {* �����С����Ĭ����С����� 20 λ����}

    property Nominator: TCnBigNumber read FNominator;
    {* ����}
    property Denominator: TCnBigNumber read FDenominator;
    {* ��ĸ}
  end;

// ============================= �����������㷽�� ==============================

procedure CnBigRationalNumberAdd(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
{* ���������ӷ��������������}

procedure CnBigRationalNumberSub(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
{* �������������������������}

procedure CnBigRationalNumberMul(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
{* ���������˷��������������}

procedure CnBigRationalNumberDiv(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
{* �������������������������}

function CnBigRationalNumberCompare(Number1, Number2: TCnBigRationalNumber): Integer; overload;
{* ���������Ƚϣ�> = < �ֱ𷵻� 1 0 -1}

function CnBigRationalNumberCompare(Number1: TCnBigRationalNumber; Number2: Int64): Integer; overload;
{^ ���������������Ƚϣ�> = < �ֱ𷵻� 1 0 -1}

procedure CnReduceBigNumber(X, Y: TCnBigNumber);
{* ����������С��Ҳ����Լ��}

var
  CnBigRationalNumberOne: TCnBigRationalNumber = nil;
  CnBigRationalNumberZero: TCnBigRationalNumber = nil;

implementation

procedure CnBigRationalNumberAdd(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, R, F1, F2, D1, D2: TCnBigNumber;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    BigNumberAdd(RationalResult.Nominator, Number1.Nominator, Number2.Nominator);
    Exit;
  end
  else if Number1.IsZero then
  begin
    if Number2 <> RationalResult then
      RationalResult.Assign(Number2);
  end
  else if Number2.IsZero then
  begin
    if Number1 <> RationalResult then
      RationalResult.Assign(Number1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // ���ĸ����С������
      M := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      F1 := TCnBigNumber.Create;
      F2 := TCnBigNumber.Create;
      D1 := TCnBigNumber.Create;
      D2 := TCnBigNumber.Create;

      BigNumberCopy(D1, Number1.Denominator);
      BigNumberCopy(D2, Number2.Denominator);

      B1 := Number1.Denominator.IsNegative;
      B2 := Number2.Denominator.IsNegative;

      D1.SetNegative(False);
      D2.SetNegative(False);

      BigNumberLcm(M, D1, D2);
      BigNumberDiv(F1, R, M, D1);
      BigNumberDiv(F2, R, M, D2);

      BigNumberCopy(RationalResult.Denominator, M);
      BigNumberMul(R, Number1.Nominator, F1);
      if B1 then
        R.SetNegative(not R.IsNegative);
      BigNumberMul(M, Number2.Nominator, F2);
      if B2 then
        M.SetNegative(not M.IsNegative);

      BigNumberAdd(RationalResult.Nominator, R, M);
    finally
      D2.Free;
      D1.Free;
      F2.Free;
      F1.Free;
      R.Free;
      M.Free;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnBigRationalNumberSub(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
begin
  Number2.Nominator.SetNegative(not Number2.Nominator.IsNegative);
  CnBigRationalNumberAdd(Number1, Number2, RationalResult);
  if RationalResult <> Number2 then
    Number2.Nominator.SetNegative(not Number2.Nominator.IsNegative);
end;

procedure CnBigRationalNumberMul(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
var
  N: TCnBigNumber;
begin
  N := TCnBigNumber.Create;
  try
    BigNumberMul(N, Number1.Nominator, Number2.Nominator);
    BigNumberMul(RationalResult.Denominator, Number1.Denominator, Number2.Denominator);
    BigNumberCopy(RationalResult.Nominator, N);
  finally
    N.Free;
  end;
  RationalResult.Reduce;
end;

procedure CnBigRationalNumberDiv(Number1, Number2: TCnBigRationalNumber; RationalResult: TCnBigRationalNumber);
var
  N: TCnBigNumber;
begin
  if Number2.IsZero then
    raise EDivByZero.Create('Divide by Zero.');

  N := TCnBigNumber.Create;
  try
    BigNumberMul(N, Number1.Nominator, Number2.Denominator);
    BigNumberMul(RationalResult.Denominator, Number1.Denominator, Number2.Nominator);
    BigNumberCopy(RationalResult.Nominator, N);
  finally
    N.Free;
  end;
  RationalResult.Reduce;
end;

function CnBigRationalNumberCompare(Number1, Number2: TCnBigRationalNumber): Integer;
var
  Res: TCnBigRationalNumber;
begin
  if not Number1.IsNegative and Number2.IsNegative then
    Result := 1
  else if Number1.IsNegative and not Number2.IsNegative then
    Result := -1
  else if Number1.IsZero and Number2.IsZero then
    Result := 0
  else if Number1.IsInt and Number2.IsInt then
    Result := BigNumberCompare(Number1.Nominator, Number2.Nominator)
  else
  begin
    //  ͬ�ţ��������Ƚ�
    Res := TCnBigRationalNumber.Create;
    try
      CnBigRationalNumberSub(Number1, Number2, Res);
      if Res.IsZero then
        Result := 0
      else if Res.IsNegative then
        Result := -1
      else
        Result := 1;
    finally
      Res.Free;
    end;
  end;
end;

function CnBigRationalNumberCompare(Number1: TCnBigRationalNumber; Number2: Int64): Integer;
var
  Res: TCnBigNumber;
begin
  if not Number1.IsNegative and (Number2 < 0) then
    Result := 1
  else if Number1.IsNegative and (Number2 > 0) then
    Result := -1
  else if Number1.IsZero and (Number2 = 0) then
    Result := 0
  else
  begin
    Res := TCnBigNumber.Create;
    try
      Res.SetInt64(Number2);
      if not Number1.IsInt then
        BigNumberMul(Res, Number1.Denominator, Res);
      Result := BigNumberCompare(Number1.Nominator, Res);
    finally
      Res.Free;
    end;
  end;
end;

procedure CnReduceBigNumber(X, Y: TCnBigNumber);
var
  N, R: TCnBigNumber;
begin
  N := TCnBigNumber.Create;
  if BigNumberGcd(N, X, Y) then
  begin
    if not N.IsOne then
    begin
      R := TCnBigNumber.Create;
      BigNumberDiv(X, R, X, N);
      BigNumberDiv(Y, R, Y, N);
      R.Free;
    end;
  end;
  N.Free;
end;

{ TCnBigRationalNumber }

procedure TCnBigRationalNumber.Add(Value: TCnBigNumber);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberAdd(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Add(Value: Int64);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberAdd(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Add(Value: TCnBigRationalNumber);
begin
  CnBigRationalNumberAdd(Self, Value, Self);
end;

procedure TCnBigRationalNumber.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnBigRationalNumber then
  begin
    BigNumberCopy(TCnBigRationalNumber(Dest).Nominator, FNominator);
    BigNumberCopy(TCnBigRationalNumber(Dest).Denominator, FDenominator);
  end
  else
    inherited;
end;

constructor TCnBigRationalNumber.Create;
begin
  FNominator := TCnBigNumber.Create;
  FDenominator := TCnBigNumber.Create;
  FDenominator.SetOne;
  FNominator.SetZero;
end;

destructor TCnBigRationalNumber.Destroy;
begin
  FDenominator.Free;
  FNominator.Free;
  inherited;
end;

procedure TCnBigRationalNumber.Divide(Value: Int64);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberDiv(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Divide(Value: TCnBigNumber);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberDiv(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Divide(Value: TCnBigRationalNumber);
begin
  CnBigRationalNumberDiv(Self, Value, Self);
end;

function TCnBigRationalNumber.Equal(Value: TCnBigRationalNumber): Boolean;
begin
  Result := CnBigRationalNumberCompare(Self, Value) = 0;
end;

function TCnBigRationalNumber.EqualInt(Value: TCnBigNumber): Boolean;
begin
  if FDenominator.IsOne then
    Result := BigNumberCompare(Value, FNominator) = 0
  else if FDenominator.IsNegOne then
    Result := (BigNumberUnsignedCompare(Value, FNominator) = 0)
      and (FNominator.IsNegative <> Value.IsNegative)
  else
    Result := False;
end;

function TCnBigRationalNumber.EqualInt(Value: LongWord): Boolean;
begin
  if FDenominator.IsOne then
    Result := FNominator.IsWord(Value)
  else if FDenominator.IsNegOne then
    Result := BigNumberAbsIsWord(FNominator, Value) and FNominator.IsNegative
  else
    Result := False;
end;

function TCnBigRationalNumber.IsInt: Boolean;
begin
  Result := FDenominator.IsOne or FDenominator.IsNegOne;
end;

function TCnBigRationalNumber.IsNegative: Boolean;
begin
  Result := FNominator.IsNegative <> FDenominator.IsNegative;
end;

function TCnBigRationalNumber.IsOne: Boolean;
begin
  Result := BigNumberCompare(FNominator, FDenominator) = 0;
end;

function TCnBigRationalNumber.IsZero: Boolean;
begin
  Result := FNominator.IsZero;
end;

procedure TCnBigRationalNumber.Mul(Value: TCnBigRationalNumber);
begin
  CnBigRationalNumberMul(Self, Value, Self);
end;

procedure TCnBigRationalNumber.Mul(Value: TCnBigNumber);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberMul(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Mul(Value: Int64);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberMul(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Neg;
begin
  FNominator.SetNegative(not FNominator.IsNegative);
  if FNominator.IsNegative and FDenominator.IsNegative then
  begin
    FNominator.SetNegative(False);
    FDenominator.SetNegative(False);
  end;
end;

procedure TCnBigRationalNumber.Reciprocal;
var
  T: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  BigNumberCopy(T, FDenominator);
  BigNumberCopy(FDenominator, FNominator);
  BigNumberCopy(FNominator, T);
end;

procedure TCnBigRationalNumber.Reduce;
begin
  if FDenominator.IsNegative and FNominator.IsNegative then
  begin
    FDenominator.SetNegative(False);
    FNominator.SetNegative(False);
  end
  else if FDenominator.IsNegative and not FNominator.IsNegative then  // ��ĸ�ĸ����Ƶ�����
  begin
    FDenominator.SetNegative(False);
    FNominator.SetNegative(True);
  end;

  if FNominator.IsZero then
  begin
    FDenominator.SetOne;
    Exit;
  end;

  if not IsInt then
    CnReduceBigNumber(FNominator, FDenominator);
end;

procedure TCnBigRationalNumber.SetIntValue(Value: LongWord);
begin
  FNominator.SetWord(Value);
  FDenominator.SetOne;
end;

procedure TCnBigRationalNumber.SetFloat(AFloat: Extended);
var
  F: TFloatRec;
  I, L: Integer;
begin
  // ���������λ����Ч������ָ��
  FloatToDecimal(F, AFloat, fvExtended, 18, 9999);

  L := StrLen(F.Digits);
  // ��ĸ�� 10 �� L - F.Exponent �η��������Ǵ��� Digits
  FDenominator.SetOne;
  for I := 1 to L - F.Exponent do
    FDenominator.MulWord(10);

  FNominator.SetDec(F.Digits);
  FNominator.SetNegative(F.Negative);
  Reduce;
end;

procedure TCnBigRationalNumber.SetIntValue(Value: TCnBigNumber);
begin
  BigNumberCopy(FNominator, Value);
  FDenominator.SetOne;
end;

procedure TCnBigRationalNumber.SetOne;
begin
  FNominator.SetOne;
  FDenominator.SetOne;
end;

procedure TCnBigRationalNumber.SetString(const Value: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Value);
  if P > 1 then
  begin
    N := Copy(Value, 1, P - 1);
    D := Copy(Value, P + 1, MaxInt);
    FNominator.SetDec(N);
    FDenominator.SetDec(D);
    Reduce;
  end
  else
  begin
    P := Pos('.', Value);
    if P > 1 then
    begin
      // ����С����
      N := Copy(Value, 1, P - 1);
      D := Copy(Value, P + 1, MaxInt);
      FNominator.SetDec(N + D);
      FDenominator.SetOne;
      for P := 1 to Length(D) do
        FDenominator.MulWord(10);
      Reduce;
    end
    else
    begin
      FNominator.SetDec(Value);
      FDenominator.SetOne;
    end;
  end;
end;

procedure TCnBigRationalNumber.SetValue(ANominator,
  ADenominator: TCnBigNumber);
begin
  BigNumberCopy(FNominator, ANominator);
  BigNumberCopy(FDenominator, ADenominator);
end;

procedure TCnBigRationalNumber.SetValue(const ANominator,
  ADenominator: string);
begin
  FNominator.SetDec(ANominator);
  FDenominator.SetDec(ADenominator);
end;

procedure TCnBigRationalNumber.SetZero;
begin
  FNominator.SetZero;
  FDenominator.SetOne;
end;

procedure TCnBigRationalNumber.Sub(Value: Int64);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberSub(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRationalNumber.Sub(Value: TCnBigRationalNumber);
begin
  CnBigRationalNumberSub(Self, Value, Self);
end;

procedure TCnBigRationalNumber.Sub(Value: TCnBigNumber);
var
  N: TCnBigRationalNumber;
begin
  N := TCnBigRationalNumber.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberSub(Self, N, Self);
  finally
    N.Free;
  end;
end;

function TCnBigRationalNumber.ToDec(Digits: Integer): string;
var
  Remain, Res: TCnBigNumber;
  I: Integer;
  R: string;
  IsNeg: Boolean;
begin
  Remain := TCnBigNumber.Create;
  Res := TCnBigNumber.Create;

  // ����˼�����ȳ����õ��������֣�������������ͼ�����0����
  try
    if IsInt then
    begin
      Result := FNominator.ToDec;
      Exit;
    end;
    IsNeg := IsNegative;
    if IsNeg then
      Neg;

    BigNumberDiv(Res, Remain, FNominator, FDenominator);
    Result := Res.ToDec;
    if Remain.IsZero or (Digits <= 0) then
    begin
      if IsNeg then
        Neg;
      Exit;
    end;

    R := '.';
    for I := 1 to Digits do
    begin
      // Remain * 10������������̣������ͼ� 0����һ�ּ����� 10
      Remain.MulWord(10);
      if BigNumberCompare(Remain, FDenominator) > 0 then
      begin
        BigNumberDiv(Res, Remain, Remain, FDenominator);
        R := R + Res.ToDec;
        if Remain.IsZero then
          Break;
      end
      else
      begin
        R := R + '0';
      end;
    end;

    if IsNeg then
      Neg;
    Result := Result + R;
  finally
    Res.Free;
    Remain.Free;
  end;
end;

function TCnBigRationalNumber.ToString: string;
begin
  if FDenominator.IsOne then
    Result := FNominator.ToDec
  else
    Result := FNominator.ToDec + ' / ' + FDenominator.ToDec;
end;

initialization
  CnBigRationalNumberOne := TCnBigRationalNumber.Create;
  CnBigRationalNumberZero := TCnBigRationalNumber.Create;
  CnBigRationalNumberOne.SetOne;
  CnBigRationalNumberZero.SetZero;

finalization
  CnBigRationalNumberOne.Free;
  CnBigRationalNumberZero.Free;

end.
