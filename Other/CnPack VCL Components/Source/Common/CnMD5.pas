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

{******************************************************************************}
{        �õ�Ԫ����Ronald L. Rivest��MD5.pas��д��������MD5.pas��������        }
{ -----------------------------------------------------------------------------}
{                                                                              }
{                                 MD5 Message-Digest for Delphi 4              }
{                                                                              }
{                                 Delphi 4 Unit implementing the               }
{                      RSA Data Security, Inc. MD5 Message-Digest Algorithm    }
{                                                                              }
{                          Implementation of Ronald L. Rivest's RFC 1321       }
{                                                                              }
{                      Copyright ?1997-1999 Medienagentur Fichtner & Meyer     }
{                                  Written by Matthias Fichtner                }
{                                                                              }
{ -----------------------------------------------------------------------------}
{        See RFC 1321 for RSA Data Security's copyright and license notice!    }
{ -----------------------------------------------------------------------------}
{        The latest release of md5.pas will always be available from           }
{        the distribution site at: http://www.fichtner.net/delphi/md5/         }
{ -----------------------------------------------------------------------------}
{                       Please send questions, bug reports and suggestions     }
{                      regarding this code to: mfichtner@fichtner-meyer.com    }
{ -----------------------------------------------------------------------------}
{                        This code is provided "as is" without express or      }
{                     implied warranty of any kind. Use it at your own risk.   }
{******************************************************************************}

unit CnMD5;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�MD5 �㷨��Ԫ
* ��Ԫ���ߣ����壨QSoft�� hq.com@263.net; http://qsoft.51.net
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.12.12 V1.4
*               ֧�� TBytes
*           2019.04.15 V1.3
*               ֧�� Win32/Win64/MacOS
*           2014.11.14 V1.2
*               ����л��� Pascal ��֧�ֿ�ƽ̨
*           2003.09.18 V1.1
*               �ò������ҵ��˸õ�Ԫԭ���ߵİ�Ȩ����
*           2003.09.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  Classes, SysUtils {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  TMD5Count = array[0..1] of LongWord;
  TMD5State = array[0..3] of LongWord;
  TMD5Block = array[0..15] of LongWord;
  TMD5CBits = array[0..7] of byte;
  TMD5Digest = array[0..15] of byte;
  TMD5Buffer = array[0..63] of byte;

  TMD5Context = record
    State   : TMD5State;
    Count   : TMD5Count;
    Buffer  : TMD5Buffer;
    Ipad    : array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad    : array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TMD5CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* ���Ȼص��¼���������}

//----------------------------------------------------------------
// �û� API ��������
//----------------------------------------------------------------

function MD5Buffer(const Buffer; Count: LongWord): TMD5Digest;
{* �����ݿ���� MD5 ����
 |<PRE>
   const Buffer     - Ҫ��������ݿ�
   Count: LongWord  - ���ݿ鳤��
 |</PRE>}

{$IFDEF TBYTES_DEFINED}

function MD5Bytes(Data: TBytes): TMD5Digest;
{* �� TBytes ���� MD5 ����
 |<PRE>
   Data     - Ҫ������ֽ�����
 |</PRE>}

{$ENDIF}

function MD5String(const Str: string): TMD5Digest;
{* �� String �������ݽ��� MD5 ���㡣ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ת���� AnsiString ���м���
 |<PRE>
   Str: string       - Ҫ������ַ���
 |</PRE>}

function MD5StringA(const Str: AnsiString): TMD5Digest;
{* �� AnsiString �������ݽ��� MD5 ����
 |<PRE>
   Str: AnsiString       - Ҫ������ַ���
 |</PRE>}

function MD5StringW(const Str: WideString): TMD5Digest;
{* �� WideString �������ݽ��� MD5 ���㣬����ǰ����� WideCharToMultyByte ����ת��
 |<PRE>
   Str: WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function MD5UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TMD5Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� MD5 ���㣬������ת��
 |<PRE>
   Str: UnicodeString/WideString       - Ҫ����Ŀ��ַ���
 |</PRE>}

function MD5File(const FileName: string;
  CallBack: TMD5CalcProgressFunc = nil): TMD5Digest;
{* ��ָ���ļ����ݽ��� MD5 ����
 |<PRE>
   FileName: string  - Ҫ������ļ���
   CallBack: TMD5CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function MD5Stream(Stream: TStream;
  CallBack: TMD5CalcProgressFunc = nil): TMD5Digest;
{* ��ָ�������ݽ��� MD5 ����
 |<PRE>
   Stream: TStream  - Ҫ�����������
   CallBack: TMD5CalcProgressFunc - ���Ȼص�������Ĭ��Ϊ��
 |</PRE>}

function MD5Print(const Digest: TMD5Digest): string;
{* ��ʮ�����Ƹ�ʽ��� MD5 ����ֵ
 |<PRE>
   Digest: TMD5Digest  - ָ���� MD5 ����ֵ
 |</PRE>}

function MD5Match(const D1, D2: TMD5Digest): Boolean;
{* �Ƚ����� MD5 ����ֵ�Ƿ����
 |<PRE>
   D1: TMD5Digest   - ��Ҫ�Ƚϵ� MD5 ����ֵ
   D2: TMD5Digest   - ��Ҫ�Ƚϵ� MD5 ����ֵ
 |</PRE>}

function MD5DigestToStr(aDig: TMD5Digest): string;
{* MD5 ����ֵת string
 |<PRE>
   aDig: TMD5Digest   - ��Ҫת���� MD5 ����ֵ
 |</PRE>}

procedure MD5Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TMD5Digest);

{* Hash-based Message Authentication Code (based on MD5) }

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_MD5_BLOCK_SIZE_BYTE = 64;
  HMAC_MD5_OUTPUT_LENGTH_BYTE = 16;

var
  PADDING: TMD5Buffer = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );

function F(x, y, z: LongWord): LongWord;
begin
  Result := (x and y) or ((not x) and z);
//  AND EDX, EAX
//  NOT EAX
//  AND EAX, ECX
//  OR EAX, EDX
end;

function G(x, y, z: LongWord): LongWord;
begin
  Result := (x and z) or (y and (not z));
//  AND EAX, ECX
//  NOT ECX
//  AND EDX, ECX
//  OR EAX, EDX
end;

function H(x, y, z: LongWord): LongWord;
begin
  Result := x xor y xor z;
//  XOR EAX, EDX
//  XOR EAX, ECX
end;

function I(x, y, z: LongWord): LongWord;
begin
  Result := y xor (x or (not z));
//  NOT ECX
//  OR EAX, ECX
//  XOR EAX, EDX
end;


procedure ROT(var x: LongWord; n: BYTE);
begin
  x := (x shl n) or (x shr (32 - n));
//  PUSH EBX
//  MOV CL, $20
//  SUB CL, DL
//  MOV EBX, [EAX]
//  SHR EBX, CL
//  MOV ECX, EDX
//  MOV EDX, [EAX]
//  SHL EDX, CL
//  OR EBX, EDX
//  MOV [EAX], EBX
//  POP EBX
end;

procedure FF(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  Inc(a, F(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure GG(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  Inc(a, G(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure HH(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  Inc(a, H(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure II(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  Inc(a, I(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

// Encode Count bytes at Source into (Count / 4) DWORDs at Target
procedure Encode(Source, Target: pointer; Count: LongWord);
var
  S: PByte;
  T: PLongWord;
  I: LongWord;
begin
  S := Source;
  T := Target;
  for I := 1 to Count div 4 do
  begin
    T^ := S^;
    Inc(S);
    T^ := T^ or (S^ shl 8);
    Inc(S);
    T^ := T^ or (S^ shl 16);
    Inc(S);
    T^ := T^ or (S^ shl 24);
    Inc(S);
    Inc(T);
  end;
end;

// Decode Count DWORDs at Source into (Count * 4) Bytes at Target
procedure Decode(Source, Target: pointer; Count: LongWord);
var
  S: PLongWord;
  T: PByte;
  I: LongWord;
begin
  S := Source;
  T := Target;
  for I := 1 to Count do
  begin
    T^ := S^ and $ff;
    Inc(T);
    T^ := (S^ shr 8) and $ff;
    Inc(T);
    T^ := (S^ shr 16) and $ff;
    Inc(T);
    T^ := (S^ shr 24) and $ff;
    Inc(T);
    Inc(S);
  end;
end;

// Transform State according to first 64 bytes at Buffer
procedure Transform(Buffer: pointer; var State: TMD5State);
var
  a, b, c, d: LongWord;
  Block: TMD5Block;
begin
  Encode(Buffer, @Block, 64);
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  FF (a, b, c, d, Block[ 0],  7, $d76aa478);
  FF (d, a, b, c, Block[ 1], 12, $e8c7b756);
  FF (c, d, a, b, Block[ 2], 17, $242070db);
  FF (b, c, d, a, Block[ 3], 22, $c1bdceee);
  FF (a, b, c, d, Block[ 4],  7, $f57c0faf);
  FF (d, a, b, c, Block[ 5], 12, $4787c62a);
  FF (c, d, a, b, Block[ 6], 17, $a8304613);
  FF (b, c, d, a, Block[ 7], 22, $fd469501);
  FF (a, b, c, d, Block[ 8],  7, $698098d8);
  FF (d, a, b, c, Block[ 9], 12, $8b44f7af);
  FF (c, d, a, b, Block[10], 17, $ffff5bb1);
  FF (b, c, d, a, Block[11], 22, $895cd7be);
  FF (a, b, c, d, Block[12],  7, $6b901122);
  FF (d, a, b, c, Block[13], 12, $fd987193);
  FF (c, d, a, b, Block[14], 17, $a679438e);
  FF (b, c, d, a, Block[15], 22, $49b40821);
  GG (a, b, c, d, Block[ 1],  5, $f61e2562);
  GG (d, a, b, c, Block[ 6],  9, $c040b340);
  GG (c, d, a, b, Block[11], 14, $265e5a51);
  GG (b, c, d, a, Block[ 0], 20, $e9b6c7aa);
  GG (a, b, c, d, Block[ 5],  5, $d62f105d);
  GG (d, a, b, c, Block[10],  9,  $2441453);
  GG (c, d, a, b, Block[15], 14, $d8a1e681);
  GG (b, c, d, a, Block[ 4], 20, $e7d3fbc8);
  GG (a, b, c, d, Block[ 9],  5, $21e1cde6);
  GG (d, a, b, c, Block[14],  9, $c33707d6);
  GG (c, d, a, b, Block[ 3], 14, $f4d50d87);
  GG (b, c, d, a, Block[ 8], 20, $455a14ed);
  GG (a, b, c, d, Block[13],  5, $a9e3e905);
  GG (d, a, b, c, Block[ 2],  9, $fcefa3f8);
  GG (c, d, a, b, Block[ 7], 14, $676f02d9);
  GG (b, c, d, a, Block[12], 20, $8d2a4c8a);
  HH (a, b, c, d, Block[ 5],  4, $fffa3942);
  HH (d, a, b, c, Block[ 8], 11, $8771f681);
  HH (c, d, a, b, Block[11], 16, $6d9d6122);
  HH (b, c, d, a, Block[14], 23, $fde5380c);
  HH (a, b, c, d, Block[ 1],  4, $a4beea44);
  HH (d, a, b, c, Block[ 4], 11, $4bdecfa9);
  HH (c, d, a, b, Block[ 7], 16, $f6bb4b60);
  HH (b, c, d, a, Block[10], 23, $bebfbc70);
  HH (a, b, c, d, Block[13],  4, $289b7ec6);
  HH (d, a, b, c, Block[ 0], 11, $eaa127fa);
  HH (c, d, a, b, Block[ 3], 16, $d4ef3085);
  HH (b, c, d, a, Block[ 6], 23,  $4881d05);
  HH (a, b, c, d, Block[ 9],  4, $d9d4d039);
  HH (d, a, b, c, Block[12], 11, $e6db99e5);
  HH (c, d, a, b, Block[15], 16, $1fa27cf8);
  HH (b, c, d, a, Block[ 2], 23, $c4ac5665);
  II (a, b, c, d, Block[ 0],  6, $f4292244);
  II (d, a, b, c, Block[ 7], 10, $432aff97);
  II (c, d, a, b, Block[14], 15, $ab9423a7);
  II (b, c, d, a, Block[ 5], 21, $fc93a039);
  II (a, b, c, d, Block[12],  6, $655b59c3);
  II (d, a, b, c, Block[ 3], 10, $8f0ccc92);
  II (c, d, a, b, Block[10], 15, $ffeff47d);
  II (b, c, d, a, Block[ 1], 21, $85845dd1);
  II (a, b, c, d, Block[ 8],  6, $6fa87e4f);
  II (d, a, b, c, Block[15], 10, $fe2ce6e0);
  II (c, d, a, b, Block[ 6], 15, $a3014314);
  II (b, c, d, a, Block[13], 21, $4e0811a1);
  II (a, b, c, d, Block[ 4],  6, $f7537e82);
  II (d, a, b, c, Block[11], 10, $bd3af235);
  II (c, d, a, b, Block[ 2], 15, $2ad7d2bb);
  II (b, c, d, a, Block[ 9], 21, $eb86d391);
  Inc(State[0], a);
  Inc(State[1], b);
  Inc(State[2], c);
  Inc(State[3], d);
end;

// Initialize given Context
procedure MD5Init(var Context: TMD5Context);
begin
  with Context do
  begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    // ZeroMemory(@Buffer, SizeOf(TMD5Buffer));
    FillChar(Buffer, SizeOf(TMD5Buffer), 0);
  end;
end;

// Update given Context to include Length bytes of Input
procedure MD5Update(var Context: TMD5Context; Input: PAnsiChar; Length: LongWord);
var
  Index: LongWord;
  PartLen: LongWord;
  I: LongWord;
begin
  with Context do
  begin
    Index := (Count[0] shr 3) and $3f;
    Inc(Count[0], Length shl 3);
    if Count[0] < (Length shl 3) then Inc(Count[1]);
    Inc(Count[1], Length shr 29);
  end;

  PartLen := 64 - Index;
  if Length >= PartLen then
  begin
    Move(Input^, Context.Buffer[Index], PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I + 63 < Length do
    begin
      Transform(@Input[I], Context.State);
      Inc(I, 64);
    end;
    Index := 0;
  end
  else
    I := 0;

  Move(Input[I], Context.Buffer[Index], Length - I);
end;

procedure MD5UpdateW(var Context: TMD5Context; Input: PWideChar; CharLength: LongWord);
var
{$IFDEF MSWINDOWS}
  pContent: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(pContent, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(pContent), CharLength * SizeOf(WideChar), nil, nil);
    MD5Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  MD5Update(Context, @A[1], Length(A));
{$ENDIF}
end;

// Finalize given Context, create Digest
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);
var
  Bits: TMD5CBits;
  Index: LongWord;
  PadLen: LongWord;
begin
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3f;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  MD5Update(Context, @PADDING, PadLen);
  MD5Update(Context, @Bits, 8);
  Decode(@Context.State, @Digest, 4);
end;

function InternalMD5Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TMD5Digest; CallBack: TMD5CalcProgressFunc = nil): Boolean;
var
  Context: TMD5Context;
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
begin
  Result := False;
  Size := Stream.Size;
  if Size = 0 then
    Exit;

  SavePos := Stream.Position;
  TotalBytes := 0;

  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  MD5Init(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        MD5Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    MD5Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

//----------------------------------------------------------------
// �û� API ����ʵ��
//----------------------------------------------------------------

// �����ݿ���� MD5 ����
function MD5Buffer(const Buffer; Count: Longword): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(Buffer), Count);
  MD5Final(Context, Result);
end;

{$IFDEF TBYTES_DEFINED}

function MD5Bytes(Data: TBytes): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(@Data[0]), Length(Data));
  MD5Final(Context, Result);
end;

{$ENDIF}

// �� String �������ݽ��� MD5 ����
function MD5String(const Str: string): TMD5Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := MD5StringA(AStr);
end;

// �� AnsiString �������ݽ��� MD5 ����
function MD5StringA(const Str: AnsiString): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(Str), Length(Str));
  MD5Final(Context, Result);
end;

// �� WideString �������ݽ��� MD5 ����
function MD5StringW(const Str: WideString): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5UpdateW(Context, PWideChar(Str), Length(Str));
  MD5Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� MD5 ���㣬������ת��
function MD5UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  MD5Final(Context, Result);
end;

// ��ָ���ļ����ݽ��� MD5 ����
function MD5File(const FileName: string;
  CallBack: TMD5CalcProgressFunc): TMD5Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TMD5Context;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
  var
    H: THandle;
    Info: BY_HANDLE_FILE_INFORMATION;
    Rec : Int64Rec;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Result := False;
    IsEmpty := False;
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then Exit;
    try
      if not GetFileInformationByHandle(H, Info) then Exit;
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

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalMD5Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    MD5Init(Context);
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
                MD5Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
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
    MD5Final(Context, Result);
{$ENDIF}
  end;
end;

// ��ָ�������� MD5 ����
function MD5Stream(Stream: TStream;
  CallBack: TMD5CalcProgressFunc = nil): TMD5Digest;
begin
  InternalMD5Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// ��ʮ�����Ƹ�ʽ��� MD5 ����ֵ
function MD5Print(const Digest: TMD5Digest): string;
var
  I: Byte;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4) and $0F] +
              Digits[Digest[I] and $0F]);
end;

// �Ƚ����� MD5 ����ֵ�Ƿ����
function MD5Match(const D1, D2: TMD5Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 16) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// MD5 ����ֵת string
function MD5DigestToStr(aDig: TMD5Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 16);
  for I := 1 to 16 do
    Result[I] := Chr(aDig[I - 1]);
end;

procedure MD5HmacInit(var Ctx: TMD5Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TMD5Digest;
begin
  if KeyLength > HMAC_MD5_BLOCK_SIZE_BYTE then
  begin
    Sum := MD5Buffer(Key, KeyLength);
    KeyLength := HMAC_MD5_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Ctx.Ipad, HMAC_MD5_BLOCK_SIZE_BYTE, $36);
  FillChar(Ctx.Opad, HMAC_MD5_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := Byte(Ctx.Ipad[I] xor Byte(Key[I]));
    Ctx.Opad[I] := Byte(Ctx.Opad[I] xor Byte(Key[I]));
  end;

  MD5Init(Ctx);
  MD5Update(Ctx, @(Ctx.Ipad[0]), HMAC_MD5_BLOCK_SIZE_BYTE);
end;

procedure MD5HmacUpdate(var Ctx: TMD5Context; Input: PAnsiChar; Length: LongWord);
begin
  MD5Update(Ctx, Input, Length);
end;

procedure MD5HmacFinal(var Ctx: TMD5Context; var Output: TMD5Digest);
var
  Len: Integer;
  TmpBuf: TMD5Digest;
begin
  Len := HMAC_MD5_OUTPUT_LENGTH_BYTE;
  MD5Final(Ctx, TmpBuf);
  MD5Init(Ctx);
  MD5Update(Ctx, @(Ctx.Opad[0]), HMAC_MD5_BLOCK_SIZE_BYTE);
  MD5Update(Ctx, @(TmpBuf[0]), Len);
  MD5Final(Ctx, Output);
end;

procedure MD5Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TMD5Digest);
var
  Ctx: TMD5Context;
begin
  MD5HmacInit(Ctx, Key, KeyLength);
  MD5HmacUpdate(Ctx, Input, Length);
  MD5HmacFinal(Ctx, Output);
end;

end.
