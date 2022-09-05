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

unit CnSM4;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������������㷨 SM4 ��Ԫ
* ��Ԫ���ߣ���Х��liuxiao@cnpack.org)
* ��    ע���ο������㷨�����ĵ� SM4 Encryption alogrithm
*           ���ο���ֲgoldboar��C����
* ����ƽ̨��Windows 7 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2014.09.25 V1.0
*               ��ֲ��������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils {$IFDEF MSWINDOWS}, Windows {$ENDIF};

const
  SM4_ENCRYPT = 1;
  SM4_DECRYPT = 0;

  SM4_KEYSIZE = 16;
  SM4_BLOCKSIZE = 16;

type
  TSM4Context = packed record
    Mode: Integer;              {!<  encrypt/decrypt   }
    Sk: array[0..SM4_KEYSIZE * 2 - 1] of LongWord;  {!<  SM4 subkeys       }
  end;

procedure SM4CryptEcbStr(Mode: Integer; Key: AnsiString;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-ECB ��װ�õ���� AnsiString �ļӽ��ܷ���
 |<PRE>
  Mode     SM4_ENCRYPT �� SM4_DECRYPT
  Key      16 �ֽ����룬̫����ضϣ�������#0
  Input    input �ַ������䳤���粻�� 16 ����������ʱ�ᱻ��� #0 �����ȴﵽ 16 �ı���
  Output   output ��������䳤�ȱ�����ڻ���� (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4CryptCbcStr(Mode: Integer; Key: AnsiString;
  Iv: PAnsiChar; const Input: AnsiString; Output: PAnsiChar);
{* SM4-CBC ��װ�õ���� AnsiString �ļӽ��ܷ���
 |<PRE>
  Mode     SM4_ENCRYPT �� SM4_DECRYPT
  Key      16 �ֽ����룬̫����ضϣ�������#0
  Iv       16 �ֽڳ�ʼ����������������л�ı䣬��˵�������Ҫ����ԭʼ����
  Input    input string
  Output   output ��������䳤�ȱ�����ڻ���� (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4SetKeyEnc(var Ctx: TSM4Context; Key: PAnsiChar);
{* ʹ����Կ Key ���� SM4 ���ܳ�ʼ��
 |<PRE>
  Ctx      SM4 context to be initialized
  Key      16-byte secret key
 |</PRE>}

procedure SM4SetKeyDec(var Ctx: TSM4Context; Key: PAnsiChar);
{* ʹ����Կ Key ���� SM4 ���ܳ�ʼ��
 |<PRE>
  Ctx      SM4 context to be initialized
  Key      16-byte secret key
 |</PRE>}

procedure SM4CryptEcb(var Ctx: TSM4Context; Mode: Integer; Length: Integer;
  Input: PAnsiChar; Output: PAnsiChar);
{* SM4-ECB block encryption/decryption
 |<PRE>
  Ctx      SM4 context
  Mode     SM4_ENCRYPT or SM4_DECRYPT
  Length   length of the input data������ 16 ��������
  Input    input block
  Output   output block�����������볤�ڻ���� Length
 |</PRE>}

procedure SM4CryptCbc(var Ctx: TSM4Context; Mode: Integer; Length: Integer;
  Iv: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
{* SM4-CBC buffer encryption/decryption
 |<PRE>
  Ctx      SM4 context
  Mode     SM4_ENCRYPT or SM4_DECRYPT
  Length   length of the input data������ 16 ��������
  Iv       16-byte initialization vector (updated after use)
  Input    input block
  Output   output block�����������볤�ڻ���� Length
 |</PRE>}

implementation

const
  SBoxTable: array[0..SM4_KEYSIZE - 1] of array[0..SM4_KEYSIZE - 1] of Byte = (
    ($D6, $90, $E9, $FE, $CC, $E1, $3D, $B7, $16, $B6, $14, $C2, $28, $FB, $2C, $05),
    ($2B, $67, $9A, $76, $2A, $BE, $04, $C3, $AA, $44, $13, $26, $49, $86, $06, $99),
    ($9C, $42, $50, $F4, $91, $EF, $98, $7A, $33, $54, $0B, $43, $ED, $CF, $AC, $62),
    ($E4, $B3, $1C, $A9, $C9, $08, $E8, $95, $80, $DF, $94, $FA, $75, $8F, $3F, $A6),
    ($47, $07, $A7, $FC, $F3, $73, $17, $BA, $83, $59, $3C, $19, $E6, $85, $4F, $A8),
    ($68, $6B, $81, $B2, $71, $64, $DA, $8B, $F8, $EB, $0F, $4B, $70, $56, $9D, $35),
    ($1E, $24, $0E, $5E, $63, $58, $D1, $A2, $25, $22, $7C, $3B, $01, $21, $78, $87),
    ($D4, $00, $46, $57, $9F, $D3, $27, $52, $4C, $36, $02, $E7, $A0, $C4, $C8, $9E),
    ($EA, $BF, $8A, $D2, $40, $C7, $38, $B5, $A3, $F7, $F2, $CE, $F9, $61, $15, $A1),
    ($E0, $AE, $5D, $A4, $9B, $34, $1A, $55, $AD, $93, $32, $30, $F5, $8C, $B1, $E3),
    ($1D, $F6, $E2, $2E, $82, $66, $CA, $60, $C0, $29, $23, $AB, $0D, $53, $4E, $6F),
    ($D5, $DB, $37, $45, $DE, $FD, $8E, $2F, $03, $FF, $6A, $72, $6D, $6C, $5B, $51),
    ($8D, $1B, $AF, $92, $BB, $DD, $BC, $7F, $11, $D9, $5C, $41, $1F, $10, $5A, $D8),
    ($0A, $C1, $31, $88, $A5, $CD, $7B, $BD, $2D, $74, $D0, $12, $B8, $E5, $B4, $B0),
    ($89, $69, $97, $4A, $0C, $96, $77, $7E, $65, $B9, $F1, $09, $C5, $6E, $C6, $84),
    ($18, $F0, $7D, $EC, $3A, $DC, $4D, $20, $79, $EE, $5F, $3E, $D7, $CB, $39, $48)
  );

  FK: array[0..3] of LongWord = ($3B1BAC6, $56AA3350, $677D9197, $B27022DC);

  CK: array[0..SM4_KEYSIZE * 2 - 1] of LongWord = (
    $00070E15, $1C232A31, $383F464D, $545B6269,
    $70777E85, $8C939AA1, $A8AFB6BD, $C4CBD2D9,
    $E0E7EEF5, $FC030A11, $181F262D, $343B4249,
    $50575E65, $6C737A81, $888F969D, $A4ABB2B9,
    $C0C7CED5, $DCE3EAF1, $F8FF060D, $141B2229,
    $30373E45, $4C535A61, $686F767D, $848B9299,
    $A0A7AEB5, $BCC3CAD1, $D8DFE6ED, $F4FB0209,
    $10171E25, $2C333A41, $484F565D, $646B7279 );

procedure GetULongBe(var N: LongWord; B: PAnsiChar; I: Integer);
var
  D: LongWord;
begin
  D := (LongWord(B[I]) shl 24) or (LongWord(B[I + 1]) shl 16) or
    (LongWord(B[I + 2]) shl 8) or (LongWord(B[I + 3]));
  N := D;
end;

procedure PutULongBe(N: LongWord; B: PAnsiChar; I: Integer);
begin
  B[I] := AnsiChar(N shr 24);
  B[I + 1] := AnsiChar(N shr 16);
  B[I + 2] := AnsiChar(N shr 8);
  B[I + 3] := AnsiChar(N);
end;

function SM4Shl(X: LongWord; N: Integer): LongWord;
begin
  Result := (X and $FFFFFFFF) shl N;
end;

function ROTL(X: LongWord; N: Integer): LongWord;
begin
  Result := SM4Shl(X, N) or (X shr (32 - N));
end;

procedure Swap(var A: LongWord; var B: LongWord);
var
  T: LongWord;
begin
  T := A;
  A := B;
  B := T;
end;

function SM4SBox(Inch: Byte): Byte;
var
  PTable: Pointer;
begin
  PTable := @(SboxTable[0][0]);
  Result := PByte(Integer(PTable) + Inch)^;
end;

function SM4Lt(Ka: LongWord): LongWord;
var
  BB: LongWord;
  A: array[0..3] of Byte;
  B: array[0..3] of Byte;
begin
  BB := 0;
  PutULongBe(Ka, @(A[0]), 0);
  B[0] := SM4SBox(A[0]);
  B[1] := SM4SBox(A[1]);
  B[2] := SM4SBox(A[2]);
  B[3] := SM4SBox(A[3]);
  GetULongBe(BB, @(B[0]), 0);

  Result := BB xor (ROTL(BB, 2)) xor (ROTL(BB, 10)) xor (ROTL(BB, 18))
    xor (ROTL(BB, 24));
end;

function SM4F(X0: LongWord; X1: LongWord; X2: LongWord; X3: LongWord; RK: LongWord): LongWord;
begin
  Result := X0 xor SM4Lt(X1 xor X2 xor X3 xor RK);
end;

function SM4CalciRK(Ka: LongWord): LongWord;
var
  BB: LongWord;
  A: array[0..3] of Byte;
  B: array[0..3] of Byte;
begin
  PutULongBe(Ka, @(A[0]), 0);
  B[0] := SM4SBox(A[0]);
  B[1] := SM4SBox(A[1]);
  B[2] := SM4SBox(A[2]);
  B[3] := SM4SBox(A[3]);
  GetULongBe(BB, @(B[0]), 0);
  Result := BB xor ROTL(BB, 13) xor ROTL(BB, 23);
end;

// SK Points to 32 DWord Array; Key Points to 16 Byte Array
procedure SM4SetKey(SK: PLongWord; Key: PAnsiChar);
var
  MK: array[0..3] of LongWord;
  K: array[0..35] of LongWord;
  I: Integer;
begin
  GetULongBe(MK[0], Key, 0);
  GetULongBe(MK[1], Key, 4);
  GetULongBe(MK[2], Key, 8);
  GetULongBe(MK[3], Key, 12);

  K[0] := MK[0] xor FK[0];
  K[1] := MK[1] xor FK[1];
  K[2] := MK[2] xor FK[2];
  K[3] := MK[3] xor FK[3];

  for I := 0 to 31 do
  begin
    K[I + 4] := K[I] xor SM4CalciRK(K[I + 1] xor K[I+2] xor K[I + 3] xor CK[I]);
    (PLongWord(Integer(SK) + I * SizeOf(LongWord)))^ := K[I + 4];
  end;
end;

// SK Points to 32 DWord Array; Input/Output Points to 16 Byte Array
procedure SM4OneRound(SK: PLongWord; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  UlBuf: array[0..35] of LongWord;
begin
  FillChar(UlBuf[0], SizeOf(UlBuf), 0);

  GetULongBe(UlBuf[0], Input, 0);
  GetULongBe(UlBuf[1], Input, 4);
  GetULongBe(UlBuf[2], Input, 8);
  GetULongBe(UlBuf[3], Input, 12);

  for I := 0 to 31 do
  begin
    UlBuf[I + 4] := SM4F(UlBuf[I], UlBuf[I + 1], UlBuf[I + 2], UlBuf[I + 3],
      (PLongWord(Integer(SK) + I * SizeOf(LongWord)))^);
  end;

  PutULongBe(UlBuf[35], Output, 0);
  PutULongBe(UlBuf[34], Output, 4);
  PutULongBe(UlBuf[33], Output, 8);
  PutULongBe(UlBuf[32], Output, 12);
end;

procedure SM4SetKeyEnc(var Ctx: TSM4Context; Key: PAnsiChar);
begin
  Ctx.Mode := SM4_ENCRYPT;
  SM4SetKey(@(Ctx.Sk[0]), Key);
end;

procedure SM4SetKeyDec(var Ctx: TSM4Context; Key: PAnsiChar);
var
  I: Integer;
begin
  Ctx.Mode := SM4_DECRYPT;
  SM4SetKey(@(Ctx.Sk[0]), Key);

  for I := 0 to SM4_KEYSIZE - 1 do
    Swap(Ctx.Sk[I], Ctx.Sk[31 - I]);
end;

procedure SM4CryptEcbStr(Mode: Integer; Key: AnsiString;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TSM4Context;
begin
  if Length(Key) < SM4_KEYSIZE then
    while Length(Key) < SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > SM4_KEYSIZE then
    Key := Copy(Key, 1, SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptEcb(Ctx, SM4_ENCRYPT, Length(Input), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[1]));
    SM4CryptEcb(Ctx, SM4_DECRYPT, Length(Input), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptEcb(var Ctx: TSM4Context; Mode: Integer; Length: Integer;
  Input: PAnsiChar; Output: PAnsiChar);
var
  EndBuf: array[0..SM4_BLOCKSIZE - 1] of Byte;
begin
  while Length > 0 do
  begin
    if Length >= SM4_BLOCKSIZE then
    begin
      SM4OneRound(@(Ctx.Sk[0]), Input, Output);
    end
    else
    begin
      // β������ 16���� 0
      FillChar(EndBuf[0], SM4_BLOCKSIZE, 0);
      Move(Input^, EndBuf[0], Length);
      SM4OneRound(@(Ctx.Sk[0]), @(EndBuf[0]), Output);
    end;
    Inc(Input, SM4_BLOCKSIZE);
    Inc(Output, SM4_BLOCKSIZE);
    Dec(Length, SM4_BLOCKSIZE);
  end;
end;

procedure SM4CryptCbcStr(Mode: Integer; Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TSM4Context;
begin
  if Length(Key) < SM4_KEYSIZE then
    while Length(Key) < SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > SM4_KEYSIZE then
    Key := Copy(Key, 1, SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptCbc(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[1]));
    SM4CryptCbc(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptCbc(var Ctx: TSM4Context; Mode: Integer; Length: Integer;
  Iv: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  EndBuf: array[0..SM4_BLOCKSIZE - 1] of Byte;
begin
  if Mode = SM4_ENCRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= SM4_BLOCKSIZE then
      begin
        for I := 0 to SM4_BLOCKSIZE - 1 do
          (PByte(Integer(Output) + I))^ := (PByte(Integer(Input) + I))^
            xor (PByte(Integer(Iv) + I))^;

        SM4OneRound(@(Ctx.Sk[0]), Output, Output);
        Move(Output[0], Iv[0], 16);
      end
      else
      begin
        // β������ 16���� 0
        FillChar(EndBuf[0], SizeOf(EndBuf), 0);
        Move(Input^, EndBuf[0], Length);

        for I := 0 to SM4_BLOCKSIZE - 1 do
          (PByte(Integer(Output) + I))^ := EndBuf[I]
            xor (PByte(Integer(Iv) + I))^;

        SM4OneRound(@(Ctx.Sk[0]), Output, Output);
        Move(Output[0], Iv[0], 16);
      end;

      Inc(Input, SM4_BLOCKSIZE);
      Inc(Output, SM4_BLOCKSIZE);
      Dec(Length, SM4_BLOCKSIZE);
    end;
  end
  else if Mode = SM4_DECRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= SM4_BLOCKSIZE then
      begin
        // CopyMemory(@(Temp[0]), Input, SM4_BLOCKSIZE);
        SM4OneRound(@(Ctx.Sk[0]), Input, Output);

        for I := 0 to 15 do
          (PByte(Integer(Output) + I))^ := (PByte(Integer(Output) + I))^
            xor (PByte(Integer(Iv) + I))^;

        Move(Input^, Iv[0], SM4_BLOCKSIZE);
      end
      else
      begin
        // β������ 16���� 0
        FillChar(EndBuf[0], SizeOf(EndBuf), 0);
        Move(Input^, EndBuf[0], Length);
        SM4OneRound(@(Ctx.Sk[0]), @(EndBuf[0]), Output);

        for I := 0 to 15 do
          (PByte(Integer(Output) + I))^ := (PByte(Integer(Output) + I))^
            xor (PByte(Integer(Iv) + I))^;

        Move(EndBuf[0], Iv[0], SM4_BLOCKSIZE);
      end;

      Inc(Input, SM4_BLOCKSIZE);
      Inc(Output, SM4_BLOCKSIZE);
      Dec(Length, SM4_BLOCKSIZE);
    end;
  end;
end;

end.
