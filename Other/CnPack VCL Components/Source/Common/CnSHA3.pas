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

unit CnSHA3;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�SHA3(SHA3-224/256/384/512) �㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х��Liu Xiao��
* ��    ע��D567�¿������з��� Int64 �����޷��� UInt64 ������ SHA3_512/384��ԭ����
*           ���ڲ���������޷������ļӼ���λ�Լ�������������Ƶȶ���ͬ��Ψһ��
*           ͬ���ǱȽϣ�������Ԫ��û�����ƵıȽϡ�
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.12.12 V1.2
*               ֧�� TBytes
*           2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2017.11.10 V1.0
*               ������Ԫ������������ Keccak C ������ Pascal ��������ֲ����
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  TSHA3GeneralDigest = array[0..63] of Byte;

  TSHA3_224Digest = array[0..27] of Byte;

  TSHA3_256Digest = array[0..31] of Byte;

  TSHA3_384Digest = array[0..47] of Byte;

  TSHA3_512Digest = array[0..63] of Byte;

  TSHA3Context = packed record
    State: array[0..24] of Int64;
    Index: LongWord;
    DigestLen: LongWord;
    Round: LongWord;
    BlockLen: LongWord;
    Block: array[0..255] of Byte;
    Ipad: array[0..143] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..143] of Byte;      {!< HMAC: outer padding        }
  end;

  TSHA3CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* ���Ȼص��¼���������}

function SHA3_224Buffer(const Buffer; Count: LongWord): TSHA3_224Digest;
{* �����ݿ���� SHA3_224 ����
 |<PRE>
   const Buffer     - Ҫ��������ݿ�
   Count: LongWord  - ���ݿ鳤��
 |</PRE>}

function SHA3_256Buffer(const Buffer; Count: LongWord): TSHA3_256Digest;
{* �����ݿ���� SHA3_256 ����
 |<PRE>
   const Buffer     - Ҫ��������ݿ�
   Count: LongWord  - ���ݿ鳤��
 |</PRE>}

function SHA3_384Buffer(const Buffer; Count: LongWord): TSHA3_384Digest;
{* �����ݿ���� SHA3_384 ����
 |<PRE>
   const Buffer     - Ҫ��������ݿ�
   Count: LongWord  - ���ݿ鳤��
 |</PRE>}

function SHA3_512Buffer(const Buffer; Count: LongWord): TSHA3_512Digest;
{* �����ݿ���� SHA3_512 ����
 |<PRE>
  const Buffer     - Ҫ��������ݿ�
  Count: LongWord  - ���ݿ鳤��
 |</PRE>}

{$IFDEF TBYTES_DEFINED}

function SHA3_224Bytes(Data: TBytes): TSHA3_224Digest;
{* ���ֽ�������� SHA3_224 ����
 |<PRE>
   Data     - Ҫ������ֽ�����
 |</PRE>}

function SHA3_256Bytes(Data: TBytes): TSHA3_256Digest;
{* ���ֽ�������� SHA3_256 ����
 |<PRE>
   Data     - Ҫ������ֽ�����
 |</PRE>}

function SHA3_384Bytes(Data: TBytes): TSHA3_384Digest;
{* ���ֽ�������� SHA3_384 ����
 |<PRE>
   Data     - Ҫ������ֽ�����
 |</PRE>}

function SHA3_512Bytes(Data: TBytes): TSHA3_512Digest;
{* ���ֽ�������� SHA3_512 ����
 |<PRE>
   Data     - Ҫ������ֽ�����
 |</PRE>}

{$ENDIF}

function SHA3_224String(const Str: string): TSHA3_224Digest;
{* �� String �������ݽ��� SHA3_224 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ת���� AnsiString ���м���
 |<PRE>
   Str: string       - Ҫ������ַ���
 |</PRE>}

function SHA3_256String(const Str: string): TSHA3_256Digest;
{* �� String �������ݽ��� SHA3_256 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ת���� AnsiString ���м���
 |<PRE>
   Str: string       - Ҫ������ַ���
 |</PRE>}

function SHA3_384String(const Str: string): TSHA3_384Digest;
{* �� String �������ݽ��� SHA3_384 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ת���� AnsiString ���м���
 |<PRE>
   Str: string       - Ҫ������ַ���
 |</PRE>}

function SHA3_512String(const Str: string): TSHA3_512Digest;
{* �� String �������ݽ��� SHA3_512 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ת���� AnsiString ���м���
 |<PRE>
   Str: string       - Ҫ������ַ���
 |</PRE>}

function SHA3_224UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_224 ���㣬������ת��
 |<PRE>
   Str: UnicodeString/WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function SHA3_256UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_256 ���㣬������ת��
 |<PRE>
   Str: UnicodeString/WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function SHA3_384UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_384 ���㣬������ת��
 |<PRE>
   Str: UnicodeString/WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function SHA3_512UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_512 ���㣬������ת��
 |<PRE>
   Str: UnicodeString/WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function SHA3_224StringA(const Str: AnsiString): TSHA3_224Digest;
{* �� AnsiString �������ݽ��� SHA3_224 ����
 |<PRE>
   Str: AnsiString       - Ҫ������ַ���
 |</PRE>}

function SHA3_224StringW(const Str: WideString): TSHA3_224Digest;
{* �� WideString �������ݽ��� SHA3_224 ���㣬����ǰ����� WideCharToMultyByte ����ת��
 |<PRE>
   Str: WideString       - Ҫ������ַ���
 |</PRE>}

function SHA3_256StringA(const Str: AnsiString): TSHA3_256Digest;
{* �� AnsiString �������ݽ��� SHA3_256 ����
 |<PRE>
   Str: AnsiString       - Ҫ������ַ���
 |</PRE>}

function SHA3_256StringW(const Str: WideString): TSHA3_256Digest;
{* �� WideString�������ݽ��� SHA3_256 ���㣬����ǰ����� WideCharToMultyByte ����ת��
 |<PRE>
   Str: WideString       - Ҫ������ַ���
 |</PRE>}

function SHA3_384StringA(const Str: AnsiString): TSHA3_384Digest;
{* �� AnsiString �������ݽ��� SHA3_384 ����
 |<PRE>
   Str: AnsiString       - Ҫ������ַ���
 |</PRE>}

function SHA3_384StringW(const Str: WideString): TSHA3_384Digest;
{* �� WideString �������ݽ��� SHA3_384 ���㣬����ǰ����� WideCharToMultyByte ����ת��
 |<PRE>
   Str: WideString       - Ҫ������ַ���
 |</PRE>}

function SHA3_512StringA(const Str: AnsiString): TSHA3_512Digest;
{* �� AnsiString �������ݽ��� SHA3_512 ����
|<PRE>
 Str: AnsiString       - Ҫ������ַ���
|</PRE>}

function SHA3_512StringW(const Str: WideString): TSHA3_512Digest;
{* �� WideString �������ݽ��� SHA512 ���㣬����ǰ����� WideCharToMultyByte ����ת��
|<PRE>
 Str: WideString       - Ҫ������ַ���
|</PRE>}

function SHA3_224File(const FileName: string; CallBack: TSHA3CalcProgressFunc =
  nil): TSHA3_224Digest;
{* ��ָ���ļ����ݽ��� SHA3_256 ����
 |<PRE>
   FileName: string  - Ҫ������ļ���
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_224Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_224Digest;
{* ��ָ�������ݽ��� SHA3_224 ����
 |<PRE>
   Stream: TStream  - Ҫ�����������
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_256File(const FileName: string; CallBack: TSHA3CalcProgressFunc =
  nil): TSHA3_256Digest;
{* ��ָ���ļ����ݽ��� SHA3_256 ����
 |<PRE>
   FileName: string  - Ҫ������ļ���
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_256Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_256Digest;
{* ��ָ�������ݽ��� SHA3_256 ����
 |<PRE>
   Stream: TStream  - Ҫ�����������
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_384File(const FileName: string; CallBack: TSHA3CalcProgressFunc =
  nil): TSHA3_384Digest;
{* ��ָ���ļ����ݽ��� SHA3_384 ����
 |<PRE>
   FileName: string  - Ҫ������ļ���
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_384Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_384Digest;
{* ��ָ�������ݽ��� SHA3_384 ����
 |<PRE>
   Stream: TStream  - Ҫ�����������
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_512File(const FileName: string; CallBack: TSHA3CalcProgressFunc =
  nil): TSHA3_512Digest;
{* ��ָ���ļ����ݽ��� SHA3_512 ����
 |<PRE>
   FileName: string  - Ҫ������ļ���
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_512Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_512Digest;
{* ��ָ�������ݽ��� SHA3_512 ����
 |<PRE>
   Stream: TStream  - Ҫ�����������
   CallBack: TSHA3CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function SHA3_224Print(const Digest: TSHA3_224Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_224 ����ֵ
 |<PRE>
   Digest: TSHA3_224Digest  - ָ���� SHA3_224 ����ֵ
 |</PRE>}

function SHA3_256Print(const Digest: TSHA3_256Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_256 ����ֵ
 |<PRE>
   Digest: TSHA3_256Digest  - ָ���� SHA3_256 ����ֵ
 |</PRE>}

function SHA3_384Print(const Digest: TSHA3_384Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_384 ����ֵ
 |<PRE>
   Digest: TSHA3_384Digest  - ָ���� SHA3_384 ����ֵ
 |</PRE>}

function SHA3_512Print(const Digest: TSHA3_512Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_512 ����ֵ
 |<PRE>
   Digest: TSHA3_512Digest  - ָ���� SHA3_512 ����ֵ
 |</PRE>}

function SHA3_224Match(const D1, D2: TSHA3_224Digest): Boolean;
{* �Ƚ����� SHA3_224 ����ֵ�Ƿ����
 |<PRE>
   D1: TSHA3_224Digest   - ��Ҫ�Ƚϵ� SHA3_224 ����ֵ
   D2: TSHA3_224Digest   - ��Ҫ�Ƚϵ� SHA3_224 ����ֵ
 |</PRE>}

function SHA3_256Match(const D1, D2: TSHA3_256Digest): Boolean;
{* �Ƚ����� SHA3_256 ����ֵ�Ƿ����
 |<PRE>
   D1: TSHA3_256Digest   - ��Ҫ�Ƚϵ� SHA3_256 ����ֵ
   D2: TSHA3_256Digest   - ��Ҫ�Ƚϵ� SHA3_256 ����ֵ
 |</PRE>}

function SHA3_384Match(const D1, D2: TSHA3_384Digest): Boolean;
{* �Ƚ����� SHA3_384 ����ֵ�Ƿ����
 |<PRE>
   D1: TSHA3_384Digest   - ��Ҫ�Ƚϵ� SHA3_384 ����ֵ
   D2: TSHA3_384Digest   - ��Ҫ�Ƚϵ� SHA3_384 ����ֵ
 |</PRE>}

function SHA3_512Match(const D1, D2: TSHA3_512Digest): Boolean;
{* �Ƚ����� SHA3_512 ����ֵ�Ƿ����
 |<PRE>
   D1: TSHA3_512Digest   - ��Ҫ�Ƚϵ� SHA3_512 ����ֵ
   D2: TSHA3_512Digest   - ��Ҫ�Ƚϵ� SHA3_512 ����ֵ
 |</PRE>}

function SHA3_224DigestToStr(aDig: TSHA3_224Digest): string;
{* SHA3_224 ����ֵת string
 |<PRE>
   aDig: TSHA3_224Digest   - ��Ҫת���� SHA3_224 ����ֵ
 |</PRE>}

function SHA3_256DigestToStr(aDig: TSHA3_256Digest): string;
{* SHA3_256 ����ֵת string
 |<PRE>
   aDig: TSHA3_256Digest   - ��Ҫת���� SHA3_256 ����ֵ
 |</PRE>}

function SHA3_384DigestToStr(aDig: TSHA3_384Digest): string;
{* SHA3_384 ����ֵת string
 |<PRE>
   aDig: TSHA3_384Digest   - ��Ҫת���� SHA3_384 ����ֵ
 |</PRE>}

function SHA3_512DigestToStr(aDig: TSHA3_512Digest): string;
{* SHA3_512 ����ֵת string
 |<PRE>
   aDig: TSHA3_512Digest   - ��Ҫת���� SHA3_512 ����ֵ
 |</PRE>}

//procedure SHA3Init(var Context: TSHA3Context; SHA3Type: TSHA3Type);
//procedure SHA3Update(var Context: TSHA3Context; Buffer: PAnsiChar; Len: Cardinal);
//procedure SHA3Final(var Context: TSHA3Context; var Digest: TSHA3GeneralDigest);

procedure SHA3_224Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_224Digest);

procedure SHA3_256Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_256Digest);

procedure SHA3_384Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_384Digest);

procedure SHA3_512Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_512Digest);

{* Hash-based Message Authentication Code (based on SHA3 224/256/384/512) }

implementation

type
  TSHA3Type = (stSHA3_224, stSHA3_256, stSHA3_384, stSHA3_512);

{$IFDEF SUPPORT_UINT64}
  TUInt64 = UInt64;
{$ELSE}
  // D 5,6,7 ���������з��ŵ� Int64 �������޷��ŵ� Int64
  TUInt64 = Int64;
{$ENDIF}

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  SHA3_ROUNDS = 24;
  SHA3_STATE_LEN = 25;

  SHA3_224_OUTPUT_LENGTH_BYTE = 28;
  SHA3_256_OUTPUT_LENGTH_BYTE = 32;
  SHA3_384_OUTPUT_LENGTH_BYTE = 48;
  SHA3_512_OUTPUT_LENGTH_BYTE = 64;

  SHA3_224_BLOCK_SIZE_BYTE = 144;
  SHA3_256_BLOCK_SIZE_BYTE = 136;
  SHA3_384_BLOCK_SIZE_BYTE = 104;
  SHA3_512_BLOCK_SIZE_BYTE = 72;

  HMAC_SHA3_224_BLOCK_SIZE_BYTE = SHA3_224_BLOCK_SIZE_BYTE;
  HMAC_SHA3_256_BLOCK_SIZE_BYTE = SHA3_256_BLOCK_SIZE_BYTE;
  HMAC_SHA3_384_BLOCK_SIZE_BYTE = SHA3_384_BLOCK_SIZE_BYTE;
  HMAC_SHA3_512_BLOCK_SIZE_BYTE = SHA3_512_BLOCK_SIZE_BYTE;

  HMAC_SHA3_224_OUTPUT_LENGTH_BYTE = SHA3_224_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_256_OUTPUT_LENGTH_BYTE = SHA3_256_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_384_OUTPUT_LENGTH_BYTE = SHA3_384_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_512_OUTPUT_LENGTH_BYTE = SHA3_512_OUTPUT_LENGTH_BYTE;

  KECCAKF_ROUND_CONSTS: array[0..23] of TUInt64 = (
    $0000000000000001, $0000000000008082, $800000000000808A,
    $8000000080008000, $000000000000808B, $0000000080000001,
    $8000000080008081, $8000000000008009, $000000000000008A,
    $0000000000000088, $0000000080008009, $000000008000000A,
    $000000008000808B, $800000000000008B, $8000000000008089,
    $8000000000008003, $8000000000008002, $8000000000000080,
    $000000000000800A, $800000008000000A, $8000000080008081,
    $8000000000008080, $0000000080000001, $8000000080008008
  );

  KECCAKF_ROT_CONSTS: array[0..23] of Integer = (
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
  );

  KECCAKF_PILN: array[0..23] of Integer = (
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
  );

function ROTL64(Q: TUInt64; N: Integer): TUInt64;
begin
  Result := (Q shl N) xor (Q shr (64 - N));
end;

procedure SHA3_Transform(var Context: TSHA3Context);
type
  PUInt64Array = ^TUInt64Array;
  TUInt64Array = array[0..4095] of TUInt64;
var
  I, J, R, L: Integer;
  P: PUInt64Array;
  T: TUInt64;
  BC: array[0..4] of TUInt64;
begin
  P := PUInt64Array(@(Context.Block[0]));
  I := 0;
  L := Integer(Context.BlockLen div 8);
  while I < L do
  begin
    Context.State[I] := Context.State[I] xor P^[I];
    Inc(I);
  end;

  for R := 0 to Context.Round - 1 do
  begin
    // Theta
    for I := 0 to 4 do
    begin
      BC[I] := Context.State[I] xor Context.State[I + 5] xor Context.State[I + 10]
        xor Context.State[I + 15] xor Context.State[I + 20];
    end;
    for I := 0 to 4 do
    begin
      T := BC[(I + 4) mod 5] xor ROTL64(BC[(I + 1) mod 5], 1);
      for J := 0 to 4 do
        Context.State[5 * J + I] := Context.State[5 * J + I] xor T;
    end;

    // Rho Pi
    T := Context.State[1];
    for I := 0 to 23 do
    begin
      J := KECCAKF_PILN[I];
      BC[0] := Context.State[J];
      Context.State[J] := ROTL64(T, KECCAKF_ROT_CONSTS[I]);
      T := BC[0];
    end;

    // Chi
    for J := 0 to 4 do
    begin
      for I := 0 to 4 do
        BC[I] := Context.State[5 * J + I];

      for I := 0 to 4 do
        Context.State[5 * J + I] := Context.State[5 * J + I] xor
          ((not BC[(I + 1) mod 5]) and BC[(I + 2) mod 5]);
    end;

    // Iota
    Context.State[0] := Context.State[0] xor KECCAKF_ROUND_CONSTS[R];
  end;
end;

procedure SHA3Init(var Context: TSHA3Context; SHA3Type: TSHA3Type);
begin
  FillChar(Context.State, SizeOf(Context.State), 0);
  FillChar(Context.Block, SizeOf(Context.Block), 0);
  Context.Index := 0;
  Context.Round := SHA3_ROUNDS;

  case SHA3Type of
  stSHA3_224:
    begin
      Context.BlockLen := SHA3_224_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_224_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_256:
    begin
      Context.BlockLen := SHA3_256_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_256_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_384:
    begin
      Context.BlockLen := SHA3_384_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_384_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_512:
    begin
      Context.BlockLen := SHA3_512_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_512_OUTPUT_LENGTH_BYTE;
    end;
  end;
end;

procedure SHA3Update(var Context: TSHA3Context; Buffer: PAnsiChar; Len: Cardinal);
var
  R, Idx: Cardinal;
begin
  Idx := Context.Index;
  repeat
    if Len < Context.BlockLen - Idx then
      R := Len
    else
      R := Context.BlockLen - Idx;

    FillChar(Context.Block, SizeOf(Context.Block), 0);
    Move(Buffer^, Context.Block[Idx], R);

    if (Idx + R) < Context.BlockLen then
    begin
      Idx := Idx + R;
      Break;
    end;

    SHA3_Transform(Context);
    Dec(Len, R);
    Idx := 0;
    Inc(Buffer, R);
  until False;
  Context.Index := Idx;
end;

procedure SHA3UpdateW(var Context: TSHA3Context; Buffer: PWideChar; Len: LongWord);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, Len * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Buffer, Len, // ����ҳĬ���� 0
      PAnsiChar(Content), Len * SizeOf(WideChar), nil, nil);
    SHA3Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Buffer);
  A := AnsiString(S);
  SHA3Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SHA3Final(var Context: TSHA3Context; var Digest: TSHA3GeneralDigest);
begin
  Context.Block[Context.Index] := 6;
  Context.Block[Context.BlockLen - 1] := Context.Block[Context.BlockLen - 1] or $80;
  SHA3_Transform(Context);
  Move(Context.State[0], Digest[0], Context.DigestLen);
end;

// �����ݿ���� SHA3_224 ����
function SHA3_224Buffer(const Buffer; Count: LongWord): TSHA3_224Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_256 ����
function SHA3_256Buffer(const Buffer; Count: LongWord): TSHA3_256Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_384 ����
function SHA3_384Buffer(const Buffer; Count: LongWord): TSHA3_384Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_512 ����
function SHA3_512Buffer(const Buffer; Count: LongWord): TSHA3_512Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

{$IFDEF TBYTES_DEFINED}

// ���ֽ�������� SHA3_224 ����
function SHA3_224Bytes(Data: TBytes): TSHA3_224Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_256 ����
function SHA3_256Bytes(Data: TBytes): TSHA3_256Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_384 ����
function SHA3_384Bytes(Data: TBytes): TSHA3_384Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_512 ����
function SHA3_512Bytes(Data: TBytes): TSHA3_512Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

{$ENDIF}

// �� String �������ݽ��� SHA3_224 ����
function SHA3_224String(const Str: string): TSHA3_224Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_224StringA(AStr);
end;

// �� String �������ݽ��� SHA3_256 ����
function SHA3_256String(const Str: string): TSHA3_256Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_256StringA(AStr);
end;

// �� String �������ݽ��� SHA3_384 ����
function SHA3_384String(const Str: string): TSHA3_384Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_384StringA(AStr);
end;

// �� String �������ݽ��� SHA3_512 ����
function SHA3_512String(const Str: string): TSHA3_512Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_512StringA(AStr);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_224 ���㣬������ת��
function SHA3_224UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_224Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_256 ���㣬������ת��
function SHA3_256UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_256Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_384 ���㣬������ת��
function SHA3_384UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_384Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_512 ���㣬������ת��
function SHA3_512UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA3_512Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ���SHA224 ����
function SHA3_224StringA(const Str: AnsiString): TSHA3_224Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_224 ����
function SHA3_224StringW(const Str: WideString): TSHA3_224Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_256 ����
function SHA3_256StringA(const Str: AnsiString): TSHA3_256Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_256 ����
function SHA3_256StringW(const Str: WideString): TSHA3_256Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_384 ����
function SHA3_384StringA(const Str: AnsiString): TSHA3_384Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_384 ����
function SHA3_384StringW(const Str: WideString): TSHA3_384Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_512 ����
function SHA3_512StringA(const Str: AnsiString): TSHA3_512Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_512 ����
function SHA3_512StringW(const Str: WideString): TSHA3_512Digest;
var
  Context: TSHA3Context;
  Res: TSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

function InternalSHA3Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TSHA3GeneralDigest; SHA3Type: TSHA3Type; CallBack: TSHA3CalcProgressFunc = nil): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
  Context: TSHA3Context;
begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  SHA3Init(Context, SHA3Type);

  GetMem(Buf, BufLen);
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SHA3Update(Context, Buf, ReadBytes);

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SHA3Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� SHA3_224 ����
function SHA3_224Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_224Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, 4096 * 1024, Dig, stSHA3_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_224Digest));
end;

// ��ָ�������� SHA3_256 ����
function SHA3_256Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_256Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, 4096 * 1024, Dig, stSHA3_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_256Digest));
end;

// ��ָ�������� SHA3_384 ����
function SHA3_384Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_384Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, 4096 * 1024, Dig, stSHA3_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_384Digest));
end;

// ��ָ�������� SHA3_512 ����
function SHA3_512Stream(Stream: TStream; CallBack: TSHA3CalcProgressFunc = nil):
  TSHA3_512Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, 4096 * 1024, Dig, stSHA3_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_512Digest));
end;

function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  H: THandle;
  Info: BY_HANDLE_FILE_INFORMATION;
  Rec: Int64Rec;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
  Result := False;
  IsEmpty := False;
  H := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if H = INVALID_HANDLE_VALUE then
    Exit;
  try
    if not GetFileInformationByHandle(H, Info) then
      Exit;
  finally
    CloseHandle(H);
  end;
  Rec.Lo := Info.nFileSizeLow;
  Rec.Hi := Info.nFileSizeHigh;
  Result := (Rec.Hi > 0) or (Rec.Lo > MAX_FILE_SIZE);
  IsEmpty := (Rec.Hi = 0) and (Rec.Lo = 0);
{$ELSE}
  Result := True; // �� Windows ƽ̨���� True����ʾ�� Mapping
{$ENDIF}
end;

function InternalSHA3File(const FileName: string; SHA3Type: TSHA3Type;
  CallBack: TSHA3CalcProgressFunc): TSHA3GeneralDigest;
var
{$IFDEF MSWINDOWS}
  Context: TSHA3Context;
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;
begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHA3Stream(Stream, 4096 * 1024, Result, SHA3Type, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SHA3Init(Context, SHA3Type);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                SHA3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create('MapViewOfFile Failed. ' + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create('CreateFileMapping Failed. ' + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    SHA3Final(Context, Result);
{$ENDIF}
  end;
end;

// ��ָ���ļ����ݽ��� SHA3_224 ����
function SHA3_224File(const FileName: string; CallBack: TSHA3CalcProgressFunc):
  TSHA3_224Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_224Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_256 ����
function SHA3_256File(const FileName: string; CallBack: TSHA3CalcProgressFunc):
  TSHA3_256Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_256Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_384 ����
function SHA3_384File(const FileName: string; CallBack: TSHA3CalcProgressFunc):
  TSHA3_384Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_384Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_512 ����
function SHA3_512File(const FileName: string; CallBack: TSHA3CalcProgressFunc):
  TSHA3_512Digest;
var
  Dig: TSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TSHA3_512Digest));
end;

const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

// ��ʮ�����Ƹ�ʽ��� SHA3_224 ����ֵ
function SHA3_224Print(const Digest: TSHA3_224Digest): string;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to SHA3_224_OUTPUT_LENGTH_BYTE - 1 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4)
      and $0F] + Digits[Digest[I] and $0F]);
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_256 ����ֵ
function SHA3_256Print(const Digest: TSHA3_256Digest): string;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to SHA3_256_OUTPUT_LENGTH_BYTE - 1 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4)
      and $0F] + Digits[Digest[I] and $0F]);
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_384 ����ֵ
function SHA3_384Print(const Digest: TSHA3_384Digest): string;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to SHA3_384_OUTPUT_LENGTH_BYTE - 1 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4)
      and $0F] + Digits[Digest[I] and $0F]);
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_512 ����ֵ
function SHA3_512Print(const Digest: TSHA3_512Digest): string;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to SHA3_512_OUTPUT_LENGTH_BYTE - 1 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4)
      and $0F] + Digits[Digest[I] and $0F]);
end;

// �Ƚ����� SHA3_224 ����ֵ�Ƿ����
function SHA3_224Match(const D1, D2: TSHA3_224Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 28) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA3_256 ����ֵ�Ƿ����
function SHA3_256Match(const D1, D2: TSHA3_256Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 32) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA3_384 ����ֵ�Ƿ����
function SHA3_384Match(const D1, D2: TSHA3_384Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 48) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA3_512 ����ֵ�Ƿ����
function SHA3_512Match(const D1, D2: TSHA3_512Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 64) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// SHA3_224 ����ֵת string
function SHA3_224DigestToStr(aDig: TSHA3_224Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 28);
  for I := 1 to 28 do
    Result[I] := Chr(aDig[I - 1]);
end;

// SHA3_256 ����ֵת string
function SHA3_256DigestToStr(aDig: TSHA3_256Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 32);
  for I := 1 to 32 do
    Result[I] := Chr(aDig[I - 1]);
end;

// SHA3_384 ����ֵת string
function SHA3_384DigestToStr(aDig: TSHA3_384Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 48);
  for I := 1 to 48 do
    Result[I] := Chr(aDig[I - 1]);
end;

// SHA3_512 ����ֵת string
function SHA3_512DigestToStr(aDig: TSHA3_512Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 64);
  for I := 1 to 64 do
    Result[I] := Chr(aDig[I - 1]);
end;

procedure SHA3_224HmacInit(var Context: TSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA3_224Digest;
begin
  if KeyLength > HMAC_SHA3_224_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_224Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_224_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_224_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_224_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_224_BLOCK_SIZE_BYTE);
end;

procedure SHA3_256HmacInit(var Context: TSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA3_256Digest;
begin
  if KeyLength > HMAC_SHA3_256_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_256Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_256_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_256_BLOCK_SIZE_BYTE);
end;

procedure SHA3_384HmacInit(var Context: TSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA3_384Digest;
begin
  if KeyLength > HMAC_SHA3_384_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_384Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_384_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_384_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_384_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_384_BLOCK_SIZE_BYTE);
end;

procedure SHA3_512HmacInit(var Context: TSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA3_512Digest;
begin
  if KeyLength > HMAC_SHA3_512_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_512Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_512_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_512_BLOCK_SIZE_BYTE);
end;

procedure SHA3_224HmacUpdate(var Context: TSHA3Context; Input: PAnsiChar; Length:
  LongWord);
begin
  SHA3Update(Context, Input, Length);
end;

procedure SHA3_256HmacUpdate(var Context: TSHA3Context; Input: PAnsiChar; Length:
  LongWord);
begin
  SHA3Update(Context, Input, Length);
end;

procedure SHA3_384HmacUpdate(var Context: TSHA3Context; Input: PAnsiChar; Length:
  LongWord);
begin
  SHA3Update(Context, Input, Length);
end;

procedure SHA3_512HmacUpdate(var Context: TSHA3Context; Input: PAnsiChar; Length:
  LongWord);
begin
  SHA3Update(Context, Input, Length);
end;

procedure SHA3_224HmacFinal(var Context: TSHA3Context; var Output: TSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_224_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_224_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_256HmacFinal(var Context: TSHA3Context; var Output: TSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_256_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_256_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_384HmacFinal(var Context: TSHA3Context; var Output: TSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_384_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_384_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_512HmacFinal(var Context: TSHA3Context; var Output: TSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_512_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_512_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_224Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_224Digest);
var
  Context: TSHA3Context;
  Dig: TSHA3GeneralDigest;
begin
  SHA3_224HmacInit(Context, Key, KeyLength);
  SHA3_224HmacUpdate(Context, Input, Length);
  SHA3_224HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_256Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_256Digest);
var
  Context: TSHA3Context;
  Dig: TSHA3GeneralDigest;
begin
  SHA3_256HmacInit(Context, Key, KeyLength);
  SHA3_256HmacUpdate(Context, Input, Length);
  SHA3_256HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_384Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_384Digest);
var
  Context: TSHA3Context;
  Dig: TSHA3GeneralDigest;
begin
  SHA3_384HmacInit(Context, Key, KeyLength);
  SHA3_384HmacUpdate(Context, Input, Length);
  SHA3_384HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_512Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA3_512Digest);
var
  Context: TSHA3Context;
  Dig: TSHA3GeneralDigest;
begin
  SHA3_512HmacInit(Context, Key, KeyLength);
  SHA3_512HmacUpdate(Context, Input, Length);
  SHA3_512HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

end.
