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

unit CnFloatConvert;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������ת��
* ��Ԫ���ߣ���ǬԪ(wqyfavor@163.com)
* ��    ע���õ�Ԫʵ����������Extended����ת��Ϊ�����ˡ�ʮ�������ַ����ĺ�����
*           �㷨�Ƕ�ȡExtended�������ڴ��еĶ��������ݽ���ת��������Extended���͵�˵��
*           ���Բο��������ϡ�Double��Single����Ϊϵͳͨ��֧�ֵĸ������ͣ���Delphi���е�
*           Extended�ڴ洢��ʽ�����в�ͬ�����߾���β����񻯣���Double��Singleβ��������
*           ����Ĭ�ϵ�1������β������������Ϊ1.001������Double��Single�д洢Ϊ001����ȥ
*           С����ǰ��1������Extended��洢Ϊ1001��
*           NaN��Ϊ "not a number"�����Ǹ���������ο�Math.pas��Ԫ�еĳ���NaN
*           InfinityΪ����󣬶���ο�Math.pas��Ԫ�еĳ���Infinity��NegInfinity.
*           ����һ��DecimalExp��AlwaysUseExponent������
*           ��ʮ���Ƹ�������ת������������ʱ�������ָ����ʽ����ѧ���㷨������Щ���
*           Ҳֻ����ָ����ʽ������1E-1000������ָ��ʱ��0.0000000...0001)��ת����ָ������
*           ҲӦ������Ӧ���Ʊ�ʾ������ʱ��������ʮ���Ʊ�ʾָ�����֣���������ƴ�
*           1.001E101����ֵΪ100100����ָ����ʮ���Ʊ������һЩ1.001D5����ʾ��С����
*           ����5λ��DecimalExp�����������ָ���Ƿ���ʮ���Ʊ��ָ�����ֵġ�ע�⣬��ʮ����
*           ����ʾָ�����޹涨��﷨��������ʹ��"D"����ʾ��"E"Ϊ����Ӧ���Ʊ�ʾ�����⣬����
*           ʮ�����ƱȽ����⣬"D"��"E"��Ϊʮ�����������ַ�������ʮ�����Ʊ��ʱʹ����"^"
*           �ַ����������3.BD^D(12)��A.BD^E(ABCE)���粻ϲ�����ָ�ʽ���������޸ġ�
*           AlwaysUseExponent����ָ���Ƿ�һ���ÿ�ѧ������������100.111λ���Ƚ��٣�
*           �����Զ��жϲ���Ҫʹ�ÿ�ѧ����������AlwaysUseExponentΪ��ʱ��һ�����Ϊָ��
*           ��ʽ1.00111E2��
*           const
*             MaxBinDigits = 120;
*             MaxHexDigits = 30;
*             MaxOctDigits = 40;
*           ����������ָ������������λ����������������ʱ����һ��ʹ�ÿ�ѧ��������
* ����ƽ̨��WinXP + Delphi 2009
* ���ݲ��ԣ�Delphi 2007
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2009.1.12
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF WIN64}

uses
  SysUtils;

{ FloatDecimalToBinExtended, FloatDecimalToOctExtended��FloatDecimalToHexExtended
  �������� FloatDecimalToBinaryExtended ���̣�FloatDecimalToBinaryExtended ��������}

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString; // Convert to binary

function FloatDecimalToOctExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString; // Convert to octal

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString; // Convert to hexdecimal

{$ENDIF}

implementation

{$IFNDEF WIN64}

type
  PConvertFloatSystem = ^TConvertFloatSystem;
  TConvertFloatSystem = record
    Negative: Boolean;
    ExpFlag, ExponentI: Integer;
  end;

const
  MaxBinDigits = 120;
  MaxHexDigits = 30;
  MaxOctDigits = 40;

function FloatDecimalToBinaryExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean; var ForHexOct: PConvertFloatSystem): AnsiString;
var
  Neg: Boolean;
  i, Flag, IntExp: Integer;
  Exp: AnsiString;
label UseExponent;
begin
{
Extended(32.125) in memory:
0   100000000000100  10000000 10000000 00000000 00000000 00000000 00000000 00000000 00000000
    9      8      7      6      5      4      3   2nd Byte  1stByte   0
sign exponent      digits
0 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  + Inf
1 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  - Inf
1 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  Nan
0 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  -Nan
}
  SetLength(Result, 255);
  SetLength(Exp, 2 * SizeOf(Extended) + 1);
  Neg := False;
  asm
    push EBX
    push ESI
    mov EBX, Result // Address of Result
    mov EBX, [EBX]
    mov EAX, 0
    // Test if fIN equals 0
    lea ESI, fIn[7] // get the first byte of digits
    mov AL, [ESI]
    test AL, 128 // 10000000B
    jz @Zero
    mov ECX, 0
    lea ESI, fIn[8]
    mov AX, [ESI]  // Get first two bytes
    test AX, 32768  // 32768D = 1000000000000000B
    jz @Positive
    mov Neg, 1
    sub AX, 32768 // Sign bit <- 0
  @Positive:
    // Test if fIn is NaN or Infinity
    cmp AX, 32767
    jnz @NotNAN_INF
    mov DL, [ESI - 1]
    test DL, 64  // 01000000B
    jz @INF
    mov Flag, 4  // NaN
    jmp @Done
  @INF:
    mov Flag, 3  // INF
    jmp @Done
  @NotNAN_INF:
    sub AX, 16383 // AX = AX - 011111111111111B
    jns @ExpPositive
    sub AX, 1
    not AX
    mov Flag, 2 // // Exponent sign negative
    jmp @JudgeDecimalExp
  @ExpPositive:
    mov Flag, 1 // Exponent sign positive
  @JudgeDecimalExp:
    mov IntExp, EAX
    cmp DecimalExp, 1
    je @MoveDigits
    // Binary string exponent. Convert AX to binary string and store it in Exp
    lea EBX, Exp
    mov EBX, [EBX]
    push ECX
    mov [EBX], 69 // 'E' // "D" for decimal exponent
    mov ECX, 1
    cmp Flag, 2
    jnz @NoNegativeInExp
    mov [EBX + 1], 45 // '-' // Add a "-" to exponent string
    mov ECX, 2
  @NoNegativeInExp:
    mov ESI, 0 // flag whehter "1" appears
    // Move exponent digits to Exp
    mov DX, 32768 // 1000000000000000
  @NextExpDigit:
    test AX, DX
    jz @AppendExp0
    mov [EBX + ECX], 49 // '1'
    mov ESI, 1
    jmp @NextExpIncECX
  @AppendExp0:
    cmp ESI, 0
    jz @NextExpNoIncECX // do not append this "0"
    mov [EBX + ECX], 48 // '0'
  @NextExpIncECX:
    inc ECX
  @NextExpNoIncECX:
    shr DX, 1
    cmp DX, 0
    jne @NextExpDigit
    pop ECX
    mov EBX, Result
    mov EBX, [EBX]
    jmp @MoveDigits
  @MoveDigits:
    // Move digits to Result
    mov ESI, 8
  @NextByte:
    dec ESI
    mov EAX, EBX
    lea EBX, fIn[ESI]
    mov DL, [EBX]
    mov EBX, EAX
    mov AL, 128 // 10000000
  @NextDigit:
    test DL, AL
    jz @Append0
    mov [EBX + ECX], 49 // '1'
    mov i, ECX
    jmp @Next
  @Append0:
    mov [EBX + ECX], 48 // '0'
  @Next:
    inc ECX
    shr AL, 1
    cmp AL, 0
    jne @NextDigit
    cmp ESI, 0 // if the last byte
    jne @NextByte
    jmp @Done
  @Zero:
    mov Flag, 0
  @Done:
    pop ESI
    pop EBX
  end;
  case Flag of
    0:
    begin
      ForHexOct := nil;
      Result := '0';
      Exit;
    end;
    1, 2:
    begin
      // Delete redundant "0" in Result
      Delete(Result, i + 2, MaxInt); // i stores the position of the last 1 in Result
      if Assigned(ForHexOct) then
      begin
        // Copy to ForHexOct
        with ForHexOct^ do
        begin
          Negative := Neg;
          ExpFlag := Flag;
          ExponentI := IntExp;
        end;
        Exit;
      end;
      // Add dot and exponent to Result
      if (IntExp = 0) then
      begin
        if (Length(Result) > 1) then
          Insert('.', Result, 2);
      end
      else
      begin
        { Decide whether use exponent. For example "1000.101" shouldn't be output
          as 1.000101E11 when AlwaysUseExponent is False. }
        if AlwaysUseExponent then
        begin
UseExponent:
          if DecimalExp then
            if Flag = 1 then
              Exp := 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp))
            else
              Exp := 'D-' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp));
          if Length(Result) >=2 then
            Insert('.', Result, 2);
          Result := Result + Exp;
        end
        else
        begin
          // IntExp may be negative.
          if Flag = 1 then
          begin
            // Calculate all digits required without exponent
            if IntExp <= Length(Result) - 2 then
            begin
              // Do not use exponent
              Insert('.', Result, IntExp + 2);
            end
            else if IntExp = Length(Result) - 1 then
              { 1.001, Exp = 3, output 1001  }
            else
            begin
              if IntExp + 1> MaxBinDigits then
                goto UseExponent
              else
              begin
                Inc(IntExp);
                i := Length(Result);
                // Add zeros at tail
                SetLength(Result, IntExp);
                for i := i + 1 to IntExp do
                  Result := '0';
              end;
            end;
          end
          else
          begin
            if IntExp + Length(Result) > MaxBinDigits then
              goto UseExponent
            else
            begin
              // Add leading zeros and place "."
              SetLength(Exp, 1 + IntExp);
              Exp[1] := '0';
              Exp[2] := '.';
              for i := 3 to IntExp + 1 do
                Exp := '0';   //}
              Result := Exp + Result;
            end;
          end;
        end;
      end;
    end;
    3: // INF
    begin
      ForHexOct := nil;
      Result := 'INF';
    end;
    4: // NaN
    begin
      ForHexOct := nil;
      Result := 'NaN';
      Exit;
    end;
  end;
  if Neg then
    Result := '-' + Result;
end;

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
var
  PTmp: PConvertFloatSystem;
begin
  PTmp := nil;
  Result := FloatDecimalToBinaryExtended(fIn, DecimalExp, AlwaysUseExponent, PTmp);
end;

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToHex: array[0..15] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  BinPow: array[0..3] of Integer = (8, 4, 2, 1);

  function IntToHex(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..5] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToHex[Int mod 16];
      Inc(k);
      Int := Int div 16;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToHex(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..20] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (4 - (l mod 4)) mod 4
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 4) or (i = l) then
      begin
        Buf[m] := DecToHex[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.BD^D(12)
      A.BD^E(ABCE)
      AB.FFFF }
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      { Algorithm:
          X.XXXXXXXX^Y  Shift Count     Exp
          1.00000001^0 = 1.00000001 = 1.01^0  (16)
          1.00000001^1 = 10.0000001 = 2.02^0  (16)
          1.00000001^2 = 100.000001 = 4.04^0  (16)
          1.00000001^3 = 1000.00001 = 8.08^0  (16)
          1.00000001^4 = 1.00000001^100 = 1.01^1  (16)
          1.00000001^5 = 10.0000001^100 = 2.02^1  (16)
          Shift Count = Y mod 4
          Exp = Y div 4
          X.XXXXXXXXX^Y  Y < 0                      Exp
          1.00000001^-1 = 0.100000001 = 1000.00001^-100 = 8.08^-1
          1.00000001^-2 = 0.0100000001 = 100.000001^-100 = 4.04^-1
          1.00000001^-3 = 0.00100000001 = 10.0000001^-100 = 2.02^-1
          1.00000001^-4 = 0.000100000001 = 1.00000001^-100 = 1.01^-1
          1.00000001^-5 = 0.0000100000001 = 1000.00001^-100 = 8.08^-2
          Shift Count = 4 - (Abs(Y) mod 4)
          Exp = -(Abs(Y) div 4 + 1)      }
      if ExpFlag = 1 then
      begin
        t := ExponentI div 4; // Exp
        i := ExponentI mod 4; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 4 + 1); // Exp
        i := (4 - (ExponentI mod 4)) mod 4; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToHex(Result, True);
      end
      else if k = i then
        Result := ToHex(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + '^D(' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t)) + ')'
        else
        begin
          if ExpFlag = 1 then
            Result := Result + '^E(' + IntToHex(t) + ')'
          else // t < 0
            Result := Result + '^E(-' + IntToHex(-t) + ')';
        end;
      end;
    end
    else
    begin
      {  Always remember that Result equals "XXXXXXXX" not "X.XXXXXXX".
        Judge whether to use exponent:
        There are K "X" after '.', K = Length(Result) - 1, no "." in Result originally.
        X.XXXXXXX^Y  (Binary string, ExponentI = Abs(Y))
        case Y >= 0  (Condition: ExpFlag = 2)
          Y <= K:
            Y+1 binary digits on left side of '.', K-Y digits on right side��
            totally requires ((Y+1 - 1) div 4 + 1) + ((K-Y - 1) div 4 + 1) hex digits
          Y > K:
            Y+1 binary digits on left side, totally ((Y+1 - 1) div 4 + 1) hex digits
        case Y<0  (Condition: ExpFlag = 1) 0.XXXX or 0.000XXXX
            One digit '0' on left side and K+1+Abs(Y)-1 digits on right side,
            totally 1 + ((K+1+Abs(Y)-1-1) div 4 + 1) hex digits.
        Compare hdc = hex digit count with MaxHexDigits. If hdc > MaxHexDigits,
        goto UseExponent. }
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToHex(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            // No possible that "ExponentI div 4 + (k - ExponentI - 1) div 4 + 2" > MaxHexDigits
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToHex(Result, True)
          else
          begin
            t := ExponentI div 4 + 1;
            if t > MaxHexDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToHex(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 4; {1 + ((K+1+Abs(Y)-1-1) div 4 + 1)}
          if t > MaxHexDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToHex(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;
function FloatDecimalToOctExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToOct: array[0..7] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7');
  BinPow: array[0..2] of Integer = (4, 2, 1);

  function IntToOct(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..10] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToOct[Int mod 8];
      Inc(k);
      Int := Int div 8;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToOct(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..30] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (3 - (l mod 3)) mod 3
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 3) or (i = l) then
      begin
        Buf[m] := DecToOct[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.333D12  // 12 is decimal
      2.22E33  // 33 is octal}
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      if ExpFlag = 1 then
      begin
        t := ExponentI div 3; // Exp
        i := ExponentI mod 3; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 3 + 1); // Exp
        i := (3 - (ExponentI mod 3)) mod 3; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToOct(Result, True);
      end
      else if k = i then
        Result := ToOct(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t))
        else
        begin
          if ExpFlag = 1 then
            Result := Result + 'E' + IntToOct(t)
          else // t < 0
            Result := Result + 'E-' + IntToOct(-t);
        end;
      end;
    end
    else
    begin
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToOct(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToOct(Result, True)
          else
          begin
            t := ExponentI div 3 + 1;
            if t > MaxHexDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToOct(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 3;
          if t > MaxHexDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToOct(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;

{$ENDIF}
end.
