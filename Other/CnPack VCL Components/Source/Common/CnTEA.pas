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

unit CnTEA;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�TEA �㷨��Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org)
* ��    ע������ TEA/XTEA/XXTEA
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2018.09.03 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils;

const
  CN_TEA_ROUND_COUNT = 32;

type
  ECnTeaException = class(Exception);

  TCnTeaKey = array[0..3] of LongWord;  // TEA �㷨����Կ��ʽ���ĸ� 32Bit ��

  TCnTeaData = array[0..1] of LongWord; // TEA �㷨�����ݸ�ʽ������ 32Bit ��

  TCnXXTeaData = array[0..16383] of LongWord;

  PCnXXTeaData = ^TCnXXTeaData;         // XXTEA �㷨֧�ָ����� LongWord ����

procedure CnTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* TEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����}

procedure CnTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* TEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����}

procedure CnXTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* XTEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����}

procedure CnXTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* XTEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����}

procedure CnXXTeaEncrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
{* XXTEA ���ܣ�128 Bits ��Կ���� 4 �ֽ����������ȵ���������Ϊ����}

procedure CnXXTeaDecrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
{* XXTEA ���ܣ�128 Bits ��Կ���� 4 �ֽ����������ȵ���������Ϊ����}

implementation

const
  CN_TEA_DELTA = $9E3779B9;

// �� K[0]/K[1]/K[2]/K[3] Ϊ��Կ���� TEA �㷨������ L/R ���ܳ�����
procedure TeaEncrypt(K: TCnTeaKey; var L, R: LongWord;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: LongWord;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RoundCount.');

  D := CN_TEA_DELTA;
  S := 0;
  for I := 1 to RoundCount do
  begin
    S := S + D;
    L := L + (((R shl 4) + K[0]) xor (R + S) xor ((R shr 5) + K[1]));
    R := R + (((L shl 4) + K[2]) xor (L + S) xor ((L shr 5) + K[3]));
  end;
end;

// �� K[0]/K[1]/K[2]/K[3] Ϊ��Կ���� TEA �㷨������ L/R ���ܳ�����
procedure TeaDecrypt(K: TCnTeaKey; var L, R: LongWord;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: LongWord;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  if RoundCount >= CN_TEA_ROUND_COUNT then // 32 ��Ҫ�ƶ� 5 λ��16 ��Ҫ 4 λ
    S := D shl 5
  else
    S := D shl 4;

  for I := 1 to RoundCount do
  begin
    R := R - (((L shl 4) + K[2]) xor (L + S) xor ((L shr 5) + K[3]));
    L := L - (((R shl 4) + K[0]) xor (R + S) xor ((R shr 5) + K[1]));
    S := S - D;
  end;
end;

// �� K[0]/K[1]/K[2]/K[3] Ϊ��Կ���� XTEA �㷨������ L/R ���ܳ�����
procedure XTeaEncrypt(K: TCnTeaKey; var L, R: LongWord;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: LongWord;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  S := 0;
  for I := 1 to RoundCount do
  begin
    L := L + ((((R shl 4) xor (R shr 5)) + R) xor (S + K[S and 3]));
    S := S + D;
    R := R + ((((L shl 4) xor (L shr 5)) + L) xor (S + K[(S shr 11) and 3]));
  end;
end;

// �� K[0]/K[1]/K[2]/K[3] Ϊ��Կ���� XTEA �㷨������ L/R ���ܳ�����
procedure XTeaDecrypt(K: TCnTeaKey; var L, R: LongWord;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: LongWord;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  S := D * LongWord(RoundCount);
  for I := 1 to RoundCount do
  begin
    R := R - ((((L shl 4) xor (L shr 5)) + L) xor (S + K[(S shr 11) and 3]));
    S := S - D;
    L := L - ((((R shl 4) xor (R shr 5)) + R) xor (S + K[S and 3]));
  end;
end;

function MX(Z, Y, S, P, E: LongWord; var Key: TCnTeaKey): LongWord;
begin
  Result := (((Z shr 5) xor (Y shl 2)) + ((Y shr 3) xor (Z shl 4))) xor
    ((S xor Y) + (Key[(P and 3) xor E] xor Z) );
end;

// TEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����
procedure CnTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  TeaEncrypt(Key, Data[0], Data[1], RoundCount);
end;

// TEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����
procedure CnTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  TeaDecrypt(Key, Data[0], Data[1], RoundCount);
end;

// XTEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����
procedure CnXTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  XTeaEncrypt(Key, Data[0], Data[1], RoundCount);
end;

// XTEA ���ܣ�128 Bits ��Կ���� 64 Bits ����Ϊ����
procedure CnXTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  XTeaDecrypt(Key, Data[0], Data[1], RoundCount);
end;

// XXTEA ���ܣ�128 Bits ��Կ���� 4 �ֽ����������ȵ���������Ϊ����
procedure CnXXTeaEncrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
var
  Z, Y, X, Sum, E, P: LongWord;
  Q: Integer;
begin
  if DataLongWordLength <= 0 then
    raise ECnTeaException.Create('Error Tea Data.');

  Q := 6 + 52 div DataLongWordLength;
  Z := Data^[DataLongWordLength - 1];
  Sum := 0;

  repeat
    Sum := Sum + CN_TEA_DELTA;
    E := (Sum shr 2) and 3;
    for P := 0 to DataLongWordLength - 2 do
    begin
      Y := Data^[P + 1];
      X := Data^[P];
      X := X + MX(Z, Y, Sum, P, E, Key);
      Data^[P] := X;
      Z := X;
    end;
    Y := Data^[0];
    X := Data^[DataLongWordLength - 1];
    X := X + MX(Z, Y, Sum, DataLongWordLength - 1, E, Key);
    Data^[DataLongWordLength - 1] := X;
    Z := X;
    Dec(Q);
  until Q = 0;
end;

// XXTEA ���ܣ�128 Bits ��Կ���� 4 �ֽ����������ȵ���������Ϊ����
procedure CnXXTeaDecrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
var
  Z, Y, X, Sum, E, P: LongWord;
  Q: Integer;
begin
  if DataLongWordLength <= 0 then
    raise ECnTeaException.Create('Error Tea Data.');

  Q := 6 + 52 div DataLongWordLength;
  Y := Data^[0];

  Sum := LongWord(Q) * CN_TEA_DELTA;
  repeat
    E := (Sum shr 2) and 3;
    for P := DataLongWordLength - 1 downto 1 do
    begin
      Z := Data^[P - 1];
      X := Data^[P];
      X := X - MX(Z, Y, Sum, P, E, Key);
      Data^[P] := X;
      Y := X;
    end;
    Z := Data^[DataLongWordLength - 1];
    X := Data^[0];
    X := X - MX(Z, Y, Sum, 0, E, Key);
    Data^[0] := X;
    Y := X;
    Sum := Sum - CN_TEA_DELTA;
    Dec(Q);
  until Q = 0;
end;

end.
