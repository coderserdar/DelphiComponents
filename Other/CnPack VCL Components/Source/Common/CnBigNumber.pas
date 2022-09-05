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

unit CnBigNumber;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������㷨��Ԫ
* ��Ԫ���ߣ���Х
* ��    ע���󲿷ִ� Openssl �� C ������ֲ����
*           Word ϵ�в�������ָ������ DWORD �������㣬�� Words ϵ�в�������ָ
*           �����м��������̡�
*           Div ʹ�û��ͨ�ˣ��� Mod Word �ƺ��������⡣
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2020.01.16 V1.5
*               �Ż��˷��� MulMod ���ٶȣ�ȥ��������
*           2019.04.16 V1.4
*               ֧�� Win32/Win64/MacOS
*           2017.04.04 V1.3
*               ����������������ص� Bug������չŷ�������ⷨ��������
*           2016.09.26 V1.2
*               �����������㣻�����ظĳ�ȫ�ַ�ʽ�����Ч��
*           2014.11.05 V1.1
*               �����ӽṹ��ʽ��Ϊ����ʽ�����Ӳ��ַ���
*           2014.10.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNativeDecl {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  Contnrs {$IFDEF UNICODE}, AnsiStrings {$ENDIF};

const
  BN_FLG_MALLOCED       = $1;    // �����������е� D �ڴ��Ƕ�̬������������й���
  BN_FLG_STATIC_DATA    = $2;    // �����������е� D �ڴ���ָ���ⲿ�ľ�̬����
  // BN_FLG_CONSTTIME      = $4;

  BN_FLG_FREE           = $8000;

  BN_BITS_UINT_32       = 32;
  BN_BITS_UINT_64       = 64;
  BN_BYTES              = 4;
  BN_BITS2              = 32;     // D �����е�һ��Ԫ����������λ��
  BN_BITS4              = 16;
  BN_TBIT               = $80000000;
  BN_MASK2              = $FFFFFFFF;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
  BN_MASK2h1            = $FFFF8000;
  BN_MASK3S             = $7FFFFFFFFFFFFFFF;
  BN_MASK3U             = $FFFFFFFFFFFFFFFF;

  BN_MILLER_RABIN_DEF_COUNT = 50; // Miller-Rabin �㷨��Ĭ�ϲ��Դ���

type
  ERandomAPIError = class(Exception);

  TLongWordArray = array [0..MaxInt div SizeOf(Integer) - 1] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TInt64Array = array [0..MaxInt div SizeOf(Int64) - 1] of Int64;
  PInt64Array = ^TInt64Array;

{$IFDEF SUPPORT_UINT64}
  TUInt64Array = array [0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  PUInt64Array = ^TUInt64Array;
{$ENDIF}

  {* ��������һ�������Ķ��� }
  TCnBigNumber = class(TObject)
  private
{$IFDEF DEBUG}
    FIsFromPool: Boolean;
{$ENDIF}
    function GetDecString: string;
    function GetHexString: string;
    function GetDebugDump: string;
  public
    D: PLongWord;       // һ�� array[0..Top-1] of LongWord ���飬Խ����Խ�����λ
    Top: Integer;       // Top ��ʾ���ޣ�D[Top] Ϊ 0��D[Top - 1] �����λ��Ч��
    DMax: Integer;      // D ����Ĵ洢����
    Neg: Integer;       // 1 Ϊ����0 Ϊ��
    Flags: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    {* ��ʼ��Ϊȫ 0���������� D �ڴ�}

    procedure Clear;
    {* ���������ݿռ��� 0�������ͷ� D �ڴ� }

    function IsZero: Boolean;
    {* ���ش����Ƿ�Ϊ 0 }

    procedure SetZero;
    {* ����������Ϊ 0 }

    function IsOne: Boolean;
    {* ���ش����Ƿ�Ϊ 1 }

    function IsNegOne: Boolean;
    {* ���ش����Ƿ�Ϊ -1 }

    function SetOne: Boolean;
    {* ����������Ϊ 1 }

    function IsOdd: Boolean;
    {* ���ش����Ƿ�Ϊ���� }

    function GetBitsCount: Integer;
    {* ���ش����ж��ٸ���Ч bit }

    function GetBytesCount: Integer;
    {* ���ش����ж��ٸ���Ч bytes }

    function GetWord: LongWord;
    {* ȡ DWORD ����ֵ }

    function SetWord(W: LongWord): Boolean;
    {* �������� DWORD ����ֵ }

    function GetInt64: Int64;
    {* ȡ Int64 ����ֵ }

    function SetInt64(W: Int64): Boolean;
    {* �������� Int64 ����ֵ }

{$IFDEF SUPPORT_UINT64}

    function GetUInt64: UInt64;
    {* ȡ UInt64 ����ֵ }

    function SetUInt64(W: UInt64): Boolean;
    {* �������� UInt64 ����ֵ }

{$ENDIF}

    function IsWord(W: LongWord): Boolean;
    {* �����Ƿ����ָ�� DWORD}

    function AddWord(W: LongWord): Boolean;
    {* ��������һ�� DWORD������Է������У���������Ƿ�ɹ�}

    function SubWord(W: LongWord): Boolean;
    {* ������ȥһ�� DWORD������Է������У���������Ƿ�ɹ�}

    function MulWord(W: LongWord): Boolean;
    {* ��������һ�� DWORD������Է������У���������Ƿ�ɹ�}

    function ModWord(W: LongWord): LongWord;
    {* ������һ�� DWORD ���࣬��������}

    function DivWord(W: LongWord): LongWord;
    {* ��������һ�� DWORD�������·��������У���������}

    procedure SetNegative(Negative: Boolean);
    {* ���ô����Ƿ�ֵ }

    function IsNegative: Boolean;
    {* ���ش����Ƿ�ֵ }

    function ClearBit(N: Integer): Boolean;
    {* �������ĵ� N �� Bit �� 0�����سɹ����N �����λ 0 �����λ GetBitsCount - 1 }

    function SetBit(N: Integer): Boolean;
    {* �������ĵ� N �� Bit �� 1�����سɹ����N �����λ 0 �����λ GetBitsCount - 1 }

    function IsBitSet(N: Integer): Boolean;
    {* ���ش����ĵ� N �� Bit �Ƿ�Ϊ 1��N �����λ 0 �����λ GetBitsCount - 1 }

    function WordExpand(Words: Integer): TCnBigNumber;
    {* ��������չ��֧�� Words �� DWORD���ɹ�������չ�Ĵ��������� Self��ʧ�ܷ��� nil}

    function ToBinary(const Buf: PAnsiChar): Integer;
    {* ������ת���ɶ��������ݷ��� Buf �У�Buf �ĳ��ȱ�����ڵ����� BytesCount��
       ���� Buf д��ĳ���}
    function SetBinary(Buf: PAnsiChar; Len: Integer): Boolean;
    {* ����һ�������ƿ������ֵ}

    class function FromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
    {* ����һ�������ƿ����һ���µĴ�������}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ת���ַ��� }

    function ToHex: string;
    {* ������ת��ʮ�������ַ���}

    function SetHex(const Buf: AnsiString): Boolean;
    {* ����һ��ʮ�������ַ���������ֵ}

    class function FromHex(const Buf: AnsiString): TCnBigNumber;
    {* ����һ��ʮ�������ַ�������һ���µĴ�������}

    function ToDec: string;
    {* ������ת��ʮ�����ַ���}

    function SetDec(const Buf: AnsiString): Boolean;
    {* ����һ��ʮ�����ַ���������ֵ}

    class function FromDec(const Buf: AnsiString): TCnBigNumber;
    {* ����һ��ʮ�����ַ����������µĴ�������}

    property DecString: string read GetDecString;
    property HexString: string read GetHexString;

    property DebugDump: string read GetDebugDump;
  end;
  PCnBigNumber = ^TCnBigNumber;

  TCnBigNumberList = class(TObjectList)
  {* ���ɴ����Ķ����б�ͬʱӵ�д���������}
  private

  protected
    function GetItem(Index: Integer): TCnBigNumber;
    procedure SetItem(Index: Integer; ABigNumber: TCnBigNumber);
  public
    constructor Create(AOwnsObjects: Boolean); overload;

    function Add(ABigNumber: TCnBigNumber): Integer;
    {* ��Ӵ�������ע����Ӻ������ֶ��ͷ�}
    function Remove(ABigNumber: TCnBigNumber): Integer;
    function IndexOfValue(ABigNumber: TCnBigNumber): Integer;
    procedure Insert(Index: Integer; ABigNumber: TCnBigNumber);
    procedure RemoveDuplicated;
    property Items[Index: Integer]: TCnBigNumber read GetItem write SetItem; default;
  end;

function BigNumberNew: TCnBigNumber;
{* ����һ����̬����Ĵ������󣬵�ͬ�� TCnBigNumber.Create }

procedure BigNumberFree(const Num: TCnBigNumber);
{* ����Ҫ�ͷ�һ���� BigNumerNew ���������Ĵ������󣬲�����Ҫ�ͷ��� D ����
   ��ͬ��ֱ�ӵ��� Free }

procedure BigNumberInit(const Num: TCnBigNumber);
{* ��ʼ��һ����������ȫΪ 0���������� D �ڴ�}

procedure BigNumberClear(const Num: TCnBigNumber);
{* ���һ���������󣬲��������ݿռ��� 0�������ͷ� D �ڴ� }

function BigNumberIsZero(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ������������Ĵ����Ƿ�Ϊ 0 }

function BigNumberSetZero(const Num: TCnBigNumber): Boolean;
{* ��һ������������Ĵ�������Ϊ 0 }

function BigNumberIsOne(const Num: TCnBigNumber): Boolean;
{* ����һ������������Ĵ����Ƿ�Ϊ 1 }

function BigNumberIsNegOne(const Num: TCnBigNumber): Boolean;
{* ����һ������������Ĵ����Ƿ�Ϊ -1 }

function BigNumberSetOne(const Num: TCnBigNumber): Boolean;
{* ��һ������������Ĵ�������Ϊ 1 }

function BigNumberIsOdd(const Num: TCnBigNumber): Boolean;
{* ����һ������������Ĵ����Ƿ�Ϊ���� }

function BigNumberGetBitsCount(const Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Ч bit }

function BigNumberGetBytesCount(const Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Ч bytes }

function BigNumberGetWord(const Num: TCnBigNumber): LongWord;
{* ȡһ�������������ֵ }

function BigNumberSetWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ��һ������������ֵ }

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
{* ȡһ�������������ֵ Int64 }

function BigNumberSetInt64(const Num: TCnBigNumber; W: Int64): Boolean;
{* ��һ������������ֵ Int64 }

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(const Num: TCnBigNumber): UInt64;
{* ȡһ�������������ֵ UInt64 }

function BigNumberSetUInt64(const Num: TCnBigNumber; W: UInt64): Boolean;
{* ��һ������������ֵ UInt64 }

{$ENDIF}

function BigNumberIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ĳ�����Ƿ����ָ�� DWORD}

function BigNumberAbsIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ĳ��������ֵ�Ƿ����ָ�� DWORD}

function BigNumberAddWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ��������һ�� DWORD������Է� Num �У���������Ƿ�ɹ�}

function BigNumberSubWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ������ȥһ�� DWORD������Է� Num �У���������Ƿ�ɹ�}

function BigNumberMulWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* ��������һ�� DWORD������Է� Num �У���������Ƿ�ɹ�}

function BigNumberModWord(const Num: TCnBigNumber; W: LongWord): LongWord;
{* ������һ�� DWORD ���࣬��������}

function BigNumberDivWord(const Num: TCnBigNumber; W: LongWord): LongWord;
{* ��������һ�� DWORD�������·��� Num �У���������}

procedure BigNumberSetNegative(const Num: TCnBigNumber; Negative: Boolean);
{* ��һ���������������Ƿ�ֵ }

function BigNumberIsNegative(const Num: TCnBigNumber): Boolean;
{* ����һ�����������Ƿ�ֵ }

function BigNumberClearBit(const Num: TCnBigNumber; N: Integer): Boolean;
{* ��һ����������ĵ� N �� Bit �� 0�����سɹ����N Ϊ 0 ʱ������������λ��}

function BigNumberSetBit(const Num: TCnBigNumber; N: Integer): Boolean;
{* ��һ����������ĵ� N �� Bit �� 1�����سɹ����N Ϊ 0 ʱ������������λ��}

function BigNumberIsBitSet(const Num: TCnBigNumber; N: Integer): Boolean;
{* ����һ����������ĵ� N �� Bit �Ƿ�Ϊ 1��N Ϊ 0 ʱ������������λ��}

function BigNumberWordExpand(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
{* ��һ������������չ��֧�� Words �� DWORD���ɹ�������չ�Ĵ��������ַ��ʧ�ܷ��� nil}

function BigNumberToBinary(const Num: TCnBigNumber; Buf: PAnsiChar): Integer;
{* ��һ������ת���ɶ��������ݷ��� Buf �У�Buf �ĳ��ȱ�����ڵ����� BytesCount��
   ���� Buf д��ĳ��ȣ�ע�ⲻ����������}

function BigNumberFromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
{* ��һ�������ƿ�ת���ɴ�������ע�ⲻ���������š���������ʱ������ BigNumberFree �ͷ�}

function BigNumberSetBinary(Buf: PAnsiChar; Len: Integer;
  const Res: TCnBigNumber): Boolean;
{* ��һ�������ƿ鸳ֵ��ָ����������ע�ⲻ����������}

function BigNumberToString(const Num: TCnBigNumber): string;
{* ��һ����������ת���ַ��������� - ��ʾ}

function BigNumberToHex(const Num: TCnBigNumber): string;
{* ��һ����������ת��ʮ�������ַ��������� - ��ʾ}

function BigNumberSetHex(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
{* ��һ��ʮ�������ַ�����ֵ��ָ���������󣬸��� - ��ʾ���ڲ����ܰ����س�����}

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
{* ��һ��ʮ�������ַ���ת��Ϊ�������󣬸��� - ��ʾ����������ʱ������ BigNumberFree �ͷ�}

function BigNumberToDec(const Num: TCnBigNumber): AnsiString;
{* ��һ����������ת��ʮ�����ַ��������� - ��ʾ}

function BigNumberSetDec(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
{* ��һ��ʮ�����ַ�����ֵ��ָ���������󣬸��� - ��ʾ���ڲ����ܰ����س�����}

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
{* ��һ��ʮ�����ַ���ת��Ϊ�������󣬸��� - ��ʾ����������ʱ������ BigNumberFree �ͷ�}

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* �����űȽ�������������ǰ�ߴ��ڵ���С�ں��߷ֱ𷵻� 1��0��-1 }

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* �޷��űȽ�������������ǰ�ߴ��ڵ���С�ں��߷ֱ𷵻� 1��0��-1 }

function BigNumberDuplicate(const Num: TCnBigNumber): TCnBigNumber;
{* ����������һ���������󣬷��ش��´���������Ҫ�� BigNumberFree ���ͷ�}

function BigNumberCopy(const Dst: TCnBigNumber; const Src: TCnBigNumber): TCnBigNumber;
{* ����һ���������󣬳ɹ����� Dst}

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
{* ���������������������}

function BigNumberRandBytes(const Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* �����̶��ֽڳ��ȵ��������}

function BigNumberRandBits(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
{* �����̶�λ���ȵ��������}

function BigNumberRandRange(const Num: TCnBigNumber; const Range: TCnBigNumber): Boolean;
{* ���� [0, Range) ֮����������}

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* �������������޷�����ӣ�������� Res �У���������Ƿ�ɹ�}

function BigNumberUnsignedSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* �������������޷��������Num1 �� Num2��������� Res �У�
  ��������Ƿ�ɹ����� Num1 < Num2 ��ʧ��}

function BigNumberAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* �������������������ӣ�������� Res �У���������Ƿ�ɹ�}

function BigNumberSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* ����������������������������� Res �У���������Ƿ�ɹ�}

function BigNumberShiftLeftOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* ��һ������������һλ��������� Res �У����������Ƿ�ɹ�}

function BigNumberShiftRightOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* ��һ������������һλ��������� Res �У����������Ƿ�ɹ�}

function BigNumberShiftLeft(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
{* ��һ������������ N λ��������� Res �У����������Ƿ�ɹ�}

function BigNumberShiftRight(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
{* ��һ������������ N λ��������� Res �У����������Ƿ�ɹ�}

function BigNumberSqr(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* ����һ���������ƽ��������� Res �У�����ƽ�������Ƿ�ɹ�}

function BigNumberMul(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* ��������������ĳ˻�������� Res �У����س˻������Ƿ�ɹ�}

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* ���������������Num / Divisor���̷� Res �У������� Remain �У����س��������Ƿ�ɹ���
   Res ������ Num}

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* �������������࣬Num mod Divisor�������� Remain �У�������������Ƿ�ɹ���Remain ������ Num}

function BigNumberNonNegativeMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* ����������Ǹ����࣬Num mod Divisor�������� Remain �У�0 <= Remain < |Divisor|
   Remain ʼ�մ����㣬������������Ƿ�ɹ�}

function BigNumberExp(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
{* ����� Num �� Exponent  �η������س˷������Ƿ�ɹ��������ʱ}

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* �������� Num1 �� Num2 �����Լ��}

function BigNumberLcm(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* �������� Num1 �� Num2 ����С������}

function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
{* ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ���
  ע��: ��������������Ը�ֵ��Ҳ���Ǿ�����ֵ�������}

function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
{* ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ���
  ע��: A��B �����Ǹ�ֵ���˻�Ϊ��ʱ�����Ϊ C - �˻�Ϊ������}

function BigNumberDirectMulMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* ��ͨ���� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ���
  ע�⣺λ������ʱ���÷���������� BigNumberMulMod ����Ҫ�첻�٣�����ͬ��}

function BigNumberPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* ���ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ�����ܱ�������ɸ��������ô�Լ�ٷ�֮ʮ}

function BigNumberMontgomeryPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* �ɸ����������ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ�������Բ���Բ���}

function BigNumberIsProbablyPrime(const Num: TCnBigNumber; TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* �������ж�һ�������Ƿ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��}

function BigNumberGeneratePrime(const Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* ����һ��ָ���ֽ�λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��}

function BigNumberGeneratePrimeByBitsCount(const Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* ����һ��ָ��������λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��}

function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
{* ԭ���жϸ����������ж� R �Ƿ���� Prime - 1 ��ÿ�����ӣ����� R ^ (ʣ�����ӵĻ�) mod Prime <> 1
   Factors ������ Prime - 1 �Ĳ��ظ����������б��ɴ� BigNumberFindFactors ��ȡ��ȥ�ض���}

function BigNumberIsInt32(const Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 32 λ�з������ͷ�Χ�ڵ���}

function BigNumberIsUInt32(const Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 32 λ�޷������ͷ�Χ�ڵ���}

function BigNumberIsInt64(const Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 64 λ�з������ͷ�Χ�ڵ���}

function BigNumberIsUInt64(const Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 64 λ�޷������ͷ�Χ�ڵ���}

procedure BigNumberExtendedEuclideanGcd(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* ��չŷ�����շת��������Ԫһ�β������� A * X + B * Y = 1 ��������
   A, B ����֪������X, Y �ǽ�����Ľ����ע�� X �п���С�� 0������Ҫ�����������ټ��� B}

procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 ��������
   A, B ����֪������X, Y �ǽ�����Ľ����ע�� X �п���С�� 0������Ҫ�����������ټ��� B
   X ����Ϊ A ��� B ��ģ��Ԫ�أ���˱��㷨Ҳ������ A ��� B ��ģ��Ԫ��
   �����ڿ������� -Y�����Ա���������һ�����ǵ�ͬ�� ��}

procedure BigNumberModularInverse(const Res: TCnBigNumber; X, Modulus: TCnBigNumber);
{* �� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ��
   �����������б�֤ X��Modulus ����}

function BigNumberLegendre(A, P: TCnBigNumber): Integer;
{* �ö��λ����ɵݹ�������õ·��� ( A / P) ��ֵ���Ͽ�}

function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
{* ��ŷ���б𷨼������õ·��� ( A / P) ��ֵ������}

function BigNumberTonelliShanks(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
{* ʹ�� Tonelli-Shanks �㷨����ģ��������ʣ����⣬Ҳ������ Res^2 mod P = A�������Ƿ��н�
   �����������б�֤ P Ϊ���������������������η�}

function BigNumberLucas(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
{* ʹ�� IEEE P1363 �淶�е� Lucas ���н���ģ��������ʣ����⣬Ҳ������ Res^2 mod P = A�������Ƿ��н�}

procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
{* �ҳ��������������б�}

function BigNumberLucasSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
{* ���� IEEE P1363 �Ĺ淶��˵���� Lucas ���У������������б�֤ N Ϊ������
   Lucas ���еݹ鶨��Ϊ��V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2
   V ���� Vk mod N��Q ���� Y ^ (K div 2) mod N }

function BigNumberDebugDump(const Num: TCnBigNumber): string;
{* ��ӡ�����ڲ���Ϣ}

function RandBytes(Buf: PAnsiChar; Len: Integer): Boolean;
{* ʹ�� Windows API ʵ������������}

var
  CnBigNumberOne: TCnBigNumber = nil;     // ��ʾ 1 �ĳ���
  CnBigNumberZero: TCnBigNumber = nil;    // ��ʾ 0 �ĳ���

implementation

uses
  CnPrimeNumber;

const
  Hex: string = '0123456789ABCDEF';

  BN_CTX_POOL_SIZE = 16;
  BN_CTX_START_FRAMES = 32;
  BN_DEC_CONV = 1000000000;
  BN_DEC_FMT = '%u';
  BN_DEC_FMT2 = '%.9u';
  BN_PRIME_NUMBERS = 2048;

{$IFNDEF MSWINDOWS}
  MAXDWORD = LongWord($FFFFFFFF);
{$ENDIF}

var
  FLocalBigNumberPool: TObjectList = nil;

{$IFDEF MSWINDOWS}

const
  ADVAPI32 = 'advapi32.dll';

  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $8;
  CRYPT_DELETEKEYSET = $10;

  PROV_RSA_FULL = 1;
  NTE_BAD_KEYSET = $80090016;

function CryptAcquireContext(phProv: PULONG; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: LongWord; dwFlags: LongWord): BOOL;
  stdcall; external ADVAPI32 name 'CryptAcquireContextA';

function CryptReleaseContext(hProv: ULONG; dwFlags: LongWord): BOOL;
  stdcall; external ADVAPI32 name 'CryptReleaseContext';

function CryptGenRandom(hProv: ULONG; dwLen: LongWord; pbBuffer: PAnsiChar): BOOL;
  stdcall; external ADVAPI32 name 'CryptGenRandom';

{$ENDIF}

{* �����ز���������ʼ}

function ObtainBigNumberFromPool: TCnBigNumber;
begin
  if FLocalBigNumberPool.Count = 0 then
  begin
    Result := TCnBigNumber.Create;
{$IFDEF DEBUG}
    Result.FIsFromPool := True;
{$ENDIF}
  end
  else
  begin
    Result := TCnBigNumber(FLocalBigNumberPool.Items[FLocalBigNumberPool.Count - 1]);
    FLocalBigNumberPool.Delete(FLocalBigNumberPool.Count - 1);
    Result.Clear;
  end;
end;

procedure RecycleBigNumberToPool(Num: TCnBigNumber);
begin
  if Num <> nil then
    FLocalBigNumberPool.Add(Num);
end;

{* �����ز�����������}

procedure BigNumberSetFlag(const Num: TCnBigNumber; N: Integer); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Num.Flags := Num.Flags or N;
end;

function BigNumberGetFlag(const Num: TCnBigNumber; N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Num.Flags and N;
end;

function BigNumberNew: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
end;

procedure BigNumberInit(const Num: TCnBigNumber);
begin
  // FillChar(Num, SizeOf(TCnBigNumber), 0);
  if Num = nil then
    Exit;
  Num.Flags := 0;
  Num.Top := 0;
  Num.Neg := 0;
  Num.DMax := 0;
  Num.D := nil;
end;

procedure BigNumberFree(const Num: TCnBigNumber);
begin
  Num.Free;
end;

function BigNumberIsZero(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Num.Top = 0);
end;

function BigNumberSetZero(const Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 0);
end;

// ����һ�������ṹ��Ĵ����ľ���ֵ�Ƿ�Ϊָ���� DWORD ֵ
function BigNumberAbsIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := True;
  if (W = 0) and (Num.Top = 0) then
    Exit;
  if (Num.Top = 1) and (PLongWordArray(Num.D)^[0] = W) then
    Exit;
  Result := False;
end;

function BigNumberIsOne(const Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 0) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberIsNegOne(const Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 1) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberSetOne(const Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 1);
end;

function BigNumberIsOdd(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top > 0) and ((PLongWordArray(Num.D)^[0] and 1) <> 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberGetWordBitsCount(L: LongWord): Integer;
const
  Bits: array[0..255] of Byte = (
    0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
  );
begin
  if (L and $FFFF0000) <> 0 then
  begin
    if (L and $FF000000) <> 0 then
      Result := Bits[L shr 24] + 24
    else
      Result := Bits[L shr 16] + 16;
  end
  else
  begin
    if (L and $FF00) <> 0 then
      Result := Bits[L shr 8] + 8
    else
      Result := Bits[L];
  end;
end;

function BigNumberGetBitsCount(const Num: TCnBigNumber): Integer;
var
  I: Integer;
begin
  Result := 0;
  if BigNumberIsZero(Num) then
    Exit;

  I := Num.Top - 1;
  Result := ((I * BN_BITS2) + BigNumberGetWordBitsCount(PLongWordArray(Num.D)^[I]));
end;

function BigNumberGetBytesCount(const Num: TCnBigNumber): Integer;
begin
  Result := (BigNumberGetBitsCount(Num) + 7) div 8;
end;

function BigNumberExpandInternal(const Num: TCnBigNumber; Words: Integer): PLongWord;
var
  A, B, TmpA: PLongWord;
  I: Integer;
  A0, A1, A2, A3: LongWord;
begin
  Result := nil;
  if Words > (MaxInt div (4 * BN_BITS2)) then
    Exit;

  if BigNumberGetFlag(Num, BN_FLG_STATIC_DATA) <> 0 then
    Exit;

  A := PLongWord(GetMemory(SizeOf(MAXDWORD) * Words));
  if A = nil then
    Exit;

  FillChar(A^, SizeOf(MAXDWORD) * Words, 0);

  // ����Ƿ�Ҫ����֮ǰ��ֵ
  B := Num.D;
  if B <> nil then
  begin
    TmpA := A;
    I :=  Num.Top shr 2;
    while I > 0 do
    begin
      A0 := PLongWordArray(B)^[0];
      A1 := PLongWordArray(B)^[1];
      A2 := PLongWordArray(B)^[2];
      A3 := PLongWordArray(B)^[3];

      PLongWordArray(TmpA)^[0] := A0;
      PLongWordArray(TmpA)^[1] := A1;
      PLongWordArray(TmpA)^[2] := A2;
      PLongWordArray(TmpA)^[3] := A3;

      Dec(I);
      TmpA := PLongWord(Integer(TmpA) + 4 * SizeOf(LongWord));
      B := PLongWord(Integer(B) + 4 * SizeOf(LongWord));
    end;

    case Num.Top and 3 of
      3:
        begin
          PLongWordArray(TmpA)^[2] := PLongWordArray(B)^[2];
          PLongWordArray(TmpA)^[1] := PLongWordArray(B)^[1];
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      2:
        begin
          PLongWordArray(TmpA)^[1] := PLongWordArray(B)^[1];
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      1:
        begin
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      0:
        begin
          ;
        end;
    end;
  end;

  Result := A;
end;

function BigNumberExpand2(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
var
  P: PLongWord;
begin
  Result := nil;
  if Words > Num.DMax then
  begin
    P := BigNumberExpandInternal(Num, Words);
    if P = nil then
      Exit;

    if Num.D <> nil then
      FreeMemory(Num.D);
    Num.D := P;
    Num.DMax := Words;

    Result := Num;
  end;
end;

function BigNumberWordExpand(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
begin
  if Words <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, Words);
end;

function BigNumberExpandBits(const Num: TCnBigNumber; Bits: Integer): TCnBigNumber;
begin
  if ((Bits + BN_BITS2 - 1) div BN_BITS2) <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, (Bits + BN_BITS2 - 1) div BN_BITS2);
end;

procedure BigNumberClear(const Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if Num.D <> nil then
    FillChar(Num.D^, Num.DMax * SizeOf(LongWord), 0);
  Num.Top := 0;
  Num.Neg := 0;
end;

function BigNumberSetWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(LongWord) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PLongWordArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetWord(const Num: TCnBigNumber): LongWord;
begin
  if Num.Top > 1 then
    Result := BN_MASK2
  else if Num.Top = 1 then
    Result := PLongWordArray(Num.D)^[0]
  else
    Result := 0;
end;

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3S
  else if Num.Top = 2 then
  begin
    Result := PInt64Array(Num.D)^[0];
    if Result < 0 then      // Int64 high bit set
      Result := BN_MASK3S
    else if Num.Neg <> 0 then // �����󷴼�һ
      Result := (not Result) + 1;
  end
  else if Num.Top = 1 then
    Result := Int64(PLongWordArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetInt64(const Num: TCnBigNumber; W: Int64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  if W >= 0 then
  begin
    Num.Neg := 0;
    PInt64Array(Num.D)^[0] := W;
    if W = 0 then
      Num.Top := 0
    else
      Num.Top := 2;
  end                 
  else // W < 0
  begin
    Num.Neg := 1;
    PInt64Array(Num.D)^[0] := (not W) + 1;
    Num.Top := 2;
  end;
  Result := True;
end;

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(const Num: TCnBigNumber): UInt64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3U
  else if Num.Top = 2 then
    Result := PUInt64Array(Num.D)^[0]
  else if Num.Top = 1 then
    Result := UInt64(PLongWordArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64(const Num: TCnBigNumber; W: UInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(UInt64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PUInt64Array(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 2
  else
    Num.Top := 0;
  Result := True;
end;

{$ENDIF}

// ĳ�����Ƿ����ָ�� DWORD
function BigNumberIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := False;
  if (W = 0) or (Num.Neg = 0) then
    if BigNumberAbsIsWord(Num, W) then
      Result := True;
end;

// ���� Top ��֤ D[Top - 1] ָ�����λ�� 0 ��
procedure BigNumberCorrectTop(const Num: TCnBigNumber);
var
  Ftl: PLongWord;
  Top: Integer;
begin
  Top := Num.Top;
  Ftl := @(PLongWordArray(Num.D)^[Top - 1]);
  while Top > 0 do
  begin
    if Ftl^ <> 0 then
      Break;

    Ftl := PLongWord(Integer(Ftl) - SizeOf(LongWord));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(const Num: TCnBigNumber; Buf: PAnsiChar): Integer;
var
  I, N: Integer;
  L: LongWord;
begin
  N := BigNumberGetBytesCount(Num);
  I := N;
  while I > 0 do
  begin
    Dec(I);
    L := PLongWordArray(Num.D)^[I div BN_BYTES];
    Buf^ := AnsiChar(Chr(L shr (8 * (I mod BN_BYTES)) and $FF));

    Buf := PAnsiChar(Integer(Buf) + 1);
  end;
  Result := N;
end;

function BigNumberFromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBinary(Buf, Len, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberSetBinary(Buf: PAnsiChar; Len: Integer;
  const Res: TCnBigNumber): Boolean;
var
  I, M, N, L: LongWord;
begin
  Result := False;
  L := 0;
  N := Len;
  if N = 0 then
  begin
    Res.Top := 0;
    Exit;
  end;

  I := ((N - 1) div BN_BYTES) + 1;
  M := (N - 1) mod BN_BYTES;

  if BigNumberWordExpand(Res, I) = nil then
  begin
    BigNumberFree(Res);
    Exit;
  end;

  Res.Top := I;
  Res.Neg := 0;
  while N > 0 do
  begin
    L := (L shl 8) or Ord(Buf^);
    Buf := PAnsiChar(Integer(Buf) + 1);

    if M = 0 then
    begin
      Dec(I);
      PLongWordArray(Res.D)^[I] := L;
      L := 0;
      M := BN_BYTES - 1;
    end
    else
      Dec(M);

    Dec(N);
  end;
  BigNumberCorrectTop(Res);
  Result := True;
end;

procedure BigNumberSetNegative(const Num: TCnBigNumber; Negative: Boolean);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Negative then
    Num.Neg := 1
  else
    Num.Neg := 0;
end;

function BigNumberIsNegative(const Num: TCnBigNumber): Boolean;
begin
  Result := Num.Neg <> 0;
end;

function BigNumberClearBit(const Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] and LongWord(not (1 shl J));
  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberSetBit(const Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J, K: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
  begin
    if BigNumberWordExpand(Num, I + 1) = nil then
      Exit;

    for K := Num.Top to I do
      PLongWordArray(Num.D)^[K] := 0;

    Num.Top := I + 1;
  end;

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] or LongWord(1 shl J);
  Result := True;
end;

function BigNumberIsBitSet(const Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  if (LongWord(PLongWordArray(Num.D)^[I] shr J) and LongWord(1)) <> 0 then
    Result := True;
end;

function BigNumberCompareWords(var Num1: TCnBigNumber; var Num2: TCnBigNumber;
  N: Integer): Integer;
var
  I: Integer;
  A, B: LongWord;
begin
  A := PLongWordArray(Num1.D)^[N - 1];
  B := PLongWordArray(Num2.D)^[N - 1];

  if A <> B then
  begin
    if A > B then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  for I := N - 2 downto 0 do
  begin
    A := PLongWordArray(Num1.D)^[I];
    B := PLongWordArray(Num2.D)^[I];

    if A <> B then
    begin
      if A > B then
        Result := 1
      else
        Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: LongWord;
begin
  if (Num1 = nil) or (Num2 = nil) then
  begin
    if Num1 <> nil then
      Result := -1
    else if Num2 <> nil then
      Result := 1
    else
      Result := 0;

    Exit;
  end;

  if Num1.Neg <> Num2.Neg then
  begin
    if Num1.Neg <> 0 then
      Result := -1
    else
      Result := 1;
    Exit;
  end;

  if Num1.Neg = 0 then
  begin
    Gt := 1;
    Lt := -1;
  end
  else
  begin
    Gt := -1;
    Lt := 1;
  end;

  if Num1.Top > Num2.Top then
  begin
    Result := Gt;
    Exit;
  end
  else if Num1.Top < Num2.Top then
  begin
    Result := Lt;
    Exit;
  end;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PLongWordArray(Num1.D)^[I];
    T2 := PLongWordArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := Gt;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := Lt;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: LongWord;
begin
  Result := Num1.Top - Num2.Top;
  if Result <> 0 then
    Exit;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PLongWordArray(Num1.D)^[I];
    T2 := PLongWordArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := 1;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

// ʹ�� Windows API ʵ������������
function RandBytes(Buf: PAnsiChar; Len: Integer): Boolean;
var
{$IFDEF MSWINDOWS}
  HProv: Cardinal;
  Res: LongWord;
{$ELSE}
  F: TFileStream;
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
  HProv := 0;
  if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, 0) then
  begin
    Res := GetLastError;
    if Res = NTE_BAD_KEYSET then // KeyContainer �����ڣ����½��ķ�ʽ
    begin
      if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, CRYPT_NEWKEYSET) then
        raise ERandomAPIError.CreateFmt('Error CryptAcquireContext NewKeySet $%8.8x', [GetLastError]);
    end
    else
        raise ERandomAPIError.CreateFmt('Error CryptAcquireContext $%8.8x', [Res]);
  end;

  if HProv <> 0 then
  begin
    try
      Result := CryptGenRandom(HProv, Len, Buf);
      if not Result then
        raise ERandomAPIError.CreateFmt('Error CryptGenRandom $%8.8x', [GetLastError]);
    finally
      CryptReleaseContext(HProv, 0);
    end;
  end;
{$ELSE}
  // MacOS �µ�������ʵ�֣����ö�ȡ /dev/random ���ݵķ�ʽ
  F := nil;
  try
    F := TFileStream.Create('/dev/random', fmOpenRead);
    Result := F.Read(Buf^, Len) = Len;
  finally
    F.Free;
  end;
{$ENDIF}
end;

// �����̶��ֽڳ��ȵ��������
function BigNumberRandBytes(const Num: TCnBigNumber; BytesCount: Integer): Boolean;
begin
  Result := False;
  if BytesCount < 0 then
    Exit;
  if BytesCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  if BigNumberWordExpand(Num, (BytesCount + 3) div 4) <> nil then
  begin
    Result := RandBytes(PAnsiChar(Num.D), BytesCount);
    if Result then
    begin
      Num.Top := (BytesCount + 3) div 4;
      BigNumberCorrectTop(Num);
    end;
  end;
end;

// �����̶�λ���ȵ��������
function BigNumberRandBits(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  C, I: Integer;
begin
  Result := False;
  if BitsCount < 0 then
    Exit;
  if BitsCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  // Ҫ���� N bits ������������ֽڼ���Ҳ���� (N + 7) div 8 bytes
  C := (BitsCount + 7) div 8;
  if not BigNumberRandBytes(Num, C) then
    Exit;

  // ��ͷ�Ͽ����ж���ģ��ٰ� C * 8 - 1 �� N ֮���λ���㣬ֻ�� 0 �� N - 1 λ
  if BitsCount <= C * 8 - 1 then
    for I := C * 8 - 1 downto BitsCount do
      if not BigNumberClearBit(Num, I) then
        Exit;

  Result := True;
end;

function BigNumberRandRange(const Num: TCnBigNumber; const Range: TCnBigNumber): Boolean;
var
  N, C, I: Integer;
begin
  Result := False;
  if (Range = nil) or (Num = nil) or (Range.Neg <> 0) or BigNumberIsZero(Range) then
    Exit;

  N := BigNumberGetBitsCount(Range);
  if N = 1 then
    BigNumberSetZero(Num)
  else
  begin
    // Ҫ���� N bits ������������ֽڼ���Ҳ���� (N + 7) div 8 bytes
    C := (N + 7) div 8;
    if not BigNumberRandBytes(Num, C) then
      Exit;

    // ��ͷ�Ͽ����ж���ģ��ٰ� C * 8 - 1 �� N + 1 ֮���λ����
    if N + 1 <= C * 8 - 1 then
      for I := C * 8 - 1 downto N + 1 do
        if BigNumberIsBitSet(Num, I) then
          if not BigNumberClearBit(Num, I) then
            Exit;
    // �Ӹ� IsBitSet ���жϣ���Ϊ ClearBit ���жϴ� Clear ��λ�Ƿ񳬳� Top��
    // ������ɵ�λ�������� 0�����Ѿ��� CorrectTop�ˣ���ô ClearBit �����

    while BigNumberCompare(Num, Range) >= 0 do
    begin
      if not BigNumberSub(Num, Num, Range) then
        Exit;
    end;
  end;
  Result := True;
end;

function BigNumberDuplicate(const Num: TCnBigNumber): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if BigNumberCopy(Result, Num) = nil then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberCopy(const Dst: TCnBigNumber; const Src: TCnBigNumber): TCnBigNumber;
var
  I: Integer;
  A, B: PLongWordArray;
  A0, A1, A2, A3: LongWord;
begin
  if Dst = Src then
  begin
    Result := Dst;
    Exit;
  end;

  if BigNumberWordExpand(Dst, Src.Top) = nil then
  begin
    Result := nil;
    Exit;
  end;

  A := PLongWordArray(Dst.D);
  B := PLongWordArray(Src.D);

  for I := (Src.Top shr 2) downto 1 do
  begin
    A0 := B[0]; A1 := B[1]; A2 := B[2]; A3 := B[3];
    A[0] := A0; A[1] := A1; A[2] := A2; A[3] := A3;

    A := PLongWordArray(Integer(A) + 4 * SizeOf(LongWord));
    B := PLongWordArray(Integer(B) + 4 * SizeOf(LongWord));
  end;

  case Src.Top and 3 of
  3:
    begin
      A[2] := B[2];
      A[1] := B[1];
      A[0] := B[0];
    end;
  2:
    begin
      A[1] := B[1];
      A[0] := B[0];
    end;
  1:
    begin
      A[0] := B[0];
    end;
  0:
    begin

    end;
  end;

  Dst.Top := Src.Top;
  Dst.Neg := Src.Neg;
  Result := Dst;
end;

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
var
  OldFlag1, OldFlag2: LongWord;
  TmpD: PLongWord;
  TmpTop, TmpDMax, TmpNeg: Integer;
begin
  OldFlag1 := Num1.Flags;
  OldFlag2 := Num2.Flags;

  TmpD := Num1.D;
  TmpTop := Num1.Top;
  TmpDMax := Num1.DMax;
  TmpNeg := Num1.Neg;

  Num1.D := Num2.D;
  Num1.Top := Num2.Top;
  Num1.DMax := Num2.DMax;
  Num1.Neg := Num2.Neg;

  Num2.D := TmpD;
  Num2.Top := TmpTop;
  Num2.DMax := TmpDMax;
  Num2.Neg := TmpNeg;

  // �����������Խ���
  Num1.Flags := (OldFlag1 and BN_FLG_MALLOCED) or (OldFlag2 and BN_FLG_STATIC_DATA);
  Num2.Flags := (OldFlag2 and BN_FLG_MALLOCED) or (OldFlag1 and BN_FLG_STATIC_DATA);
end;

// ============================ �ͽ����㶨�忪ʼ ===============================

// UInt64 �ķ�ʽ���� N ƽ��
procedure Sqr(var L: LongWord; var H: LongWord; N: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(N, N);
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  L := LongWord(T) and BN_MASK2;
  H := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 �ķ�ʽ���� A * B + R + C
procedure MulAdd(var R: LongWord; A: LongWord; B: LongWord; var C: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + R + C;
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  R := LongWord(T) and BN_MASK2;
  C := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 �ķ�ʽ���� A * B + C
procedure Mul(var R: LongWord; A: LongWord; B: LongWord; var C: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + C;
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  R := LongWord(T) and BN_MASK2;
  C := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// ============================ �ͽ����㶨����� ===============================

{* Words ϵ���ڲ����㺯����ʼ }

function BigNumberAddWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): LongWord;
var
  LL: Int64;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  LL := 0;
  while (N and (not 3)) <> 0 do
  begin
    LL := LL + Int64(AP[0]) + Int64(BP[0]);
    RP[0] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[1]) + Int64(BP[1]);
    RP[1] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[2]) + Int64(BP[2]);
    RP[2] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[3]) + Int64(BP[3]);
    RP[3] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    LL := LL + Int64(AP[0]) + Int64(BP[0]);
    RP[0] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
  Result := LongWord(LL);
end;

function BigNumberSubWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): LongWord;
var
  T1, T2, C: LongWord;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  C := 0;
  while (N and (not 3)) <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[1];
    T2 := BP^[1];
    RP^[1] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[2];
    T2 := BP^[2];
    RP^[2] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[3];
    T2 := BP^[3];
    RP^[3] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: LongWord): LongWord;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    MulAdd(RP^[1], AP^[1], W, Result);
    MulAdd(RP^[2], AP^[2], W, Result);
    MulAdd(RP^[3], AP^[3], W, Result);

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
end;

// AP ָ��� N �����ֶ����� W������ĵ� N λ�� RP �У���λ�ŷ���ֵ
function BigNumberMulWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: LongWord): LongWord;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);
    Mul(RP^[1], AP^[1], W, Result);
    Mul(RP^[2], AP^[2], W, Result);
    Mul(RP^[3], AP^[3], W, Result);

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));

    Dec(N);
  end;
end;

procedure BigNumberSqrWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer);
begin
  if N = 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    Sqr(RP^[2], RP^[3], AP^[1]);
    Sqr(RP^[4], RP^[5], AP^[2]);
    Sqr(RP^[6], RP^[7], AP^[3]);

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 8 * SizeOf(LongWord));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
    Dec(N);
  end;
end;

// 64 λ���������� 32 λ�����������̣�Result := H L div D�������̵ĸ� 32 λ
// ���Ҫ��֤ D �����λΪ 1���̵ĸ� 32 λ�Ż�Ϊ 0���˺������òŲ���������� 32 λ�²ſ����� DIV ָ���Ż�
function BigNumberDivWords(H: LongWord; L: LongWord; D: LongWord): LongWord;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

{$IFDEF WIN64}
  Result := LongWord(((UInt64(H) shl 32) or UInt64(L)) div UInt64(D));
{$ELSE}
  Result := 0;
  asm
    MOV EAX, L
    MOV EDX, H
    DIV ECX       // DIV ò�Ƶ��� DIVL������Ż�������� _lludiv �ĺ�ʱ���� 20%
    MOV Result, EAX
  end;
//  asm
//    PUSH 0
//    PUSH D
//    MOV EAX, L
//    MOV EDX, H
//    CALL System.@_lludiv;
//    // Delphi ������ʵ�ֵ� 64 λ�޷��ų������������Ҫ��
//    // Dividend(EAX(lo):EDX(hi)), Divisor([ESP+8](hi):[ESP+4](lo))
//    MOV Result, EAX
//  end;
{$ENDIF}
end;

{*  Words ϵ���ڲ����㺯������ }

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
  Carry, T1, T2: LongWord;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max + 1) = nil then
    Exit;

  Res.Top := Max;
  AP := PLongWord(A.D);
  BP := PLongWord(B.D);
  RP := PLongWord(Res.D);

  Carry := BigNumberAddWords(PLongWordArray(RP), PLongWordArray(AP), PLongWordArray(BP), Min);

  AP := PLongWord(Integer(AP) + Min * SizeOf(LongWord));
//  BP := PDWORD(Integer(BP) + Min * SizeOf(DWORD));
  RP := PLongWord(Integer(RP) + Min * SizeOf(LongWord));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if T2 <> 0 then
      begin
        Carry := 0;
        Break;
      end;
    end;

    if Carry <> 0 then
    begin
      RP^ := 1;
      Inc(Res.Top);
    end;
  end;

  if (Dif <> 0) and (RP <> AP) then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
    end;
  end;

  Res.Neg := 0;
  Result := True;
end;

function BigNumberUnsignedSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif, I: Integer;
  AP, BP, RP: PLongWord;
  Carry, T1, T2: LongWord;
begin
  Result := False;

  Max := Num1.Top;
  Min := Num2.Top;
  Dif := Max - Min;

  if Dif < 0 then
    Exit;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  AP := PLongWord(Num1.D);
  BP := PLongWord(Num2.D);
  RP := PLongWord(Res.D);

  Carry := 0;
  for I := Min downto 1 do
  begin
    T1 := AP^;
    T2 := BP^;
    AP := PLongWord(Integer(AP) + SizeOf(LongWord));
    BP := PLongWord(Integer(BP) + SizeOf(LongWord));
    if Carry <> 0 then
    begin
      if T1 <= T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2 - 1) and BN_MASK2;
    end
    else
    begin
      if T1 < T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2) and BN_MASK2;
    end;
    RP^ := T1 and BN_MASK2;
    RP := PLongWord(Integer(RP) + SizeOf(LongWord));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
      if T1 <> 0 then
        Break;
    end;
  end;

  if RP <> AP then
  begin
    while True do
    begin
      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
    end;
  end;

  Res.Top := Max;
  Res.Neg := 0;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Neg: Integer;
begin
  Result := False;

  Neg := Num1.Neg;
  A := Num1;
  B := Num2;

  if Neg <> Num2.Neg then // One is negative
  begin
    if Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end;

    // A is positive and B is negative
    if BigNumberUnsignedCompare(A, B) < 0 then
    begin
      if not BigNumberUnsignedSub(Res, B, A) then
        Exit;
      Res.Neg := 1;
    end
    else
    begin
      if not BigNumberUnsignedSub(Res, A, B) then
        Exit;
      Res.Neg := 0;
    end;
    Result := True;
    Exit;
  end;

  Result := BigNumberUnsignedAdd(Res, A, B);
  Res.Neg := Neg;
end;

function BigNumberSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Max, Add, Neg: Integer;
begin
  Result := False;
  Add := 0;
  Neg := 0;
  A := Num1;
  B := Num2;

  if A.Neg <> 0 then
  begin
    if B.Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end
    else // A Negative B Positive
    begin
      Add := 1;
      Neg := 1;
    end;
  end
  else
  begin
    if B.Neg <> 0 then // A Positive B Negative
    begin
      Add := 1;
      Neg := 0;
    end;
  end;

  if Add = 1 then
  begin
    if not BigNumberUnsignedAdd(Res, A, B) then
      Exit;

    Res.Neg := Neg;
    Result := True;
    Exit;
  end;

  if A.Top > B.Top then
    Max := A.Top
  else
    Max := B.Top;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  if BigNumberUnsignedCompare(A, B) < 0 then
  begin
    if not BigNumberUnsignedSub(Res, B, A) then
      Exit;
    Res.Neg := 1;
  end
  else
  begin
    if not BigNumberUnsignedSub(Res, A, B) then
      Exit;
    Res.Neg := 0;
  end;
  Result := True;
end;

function BigNumberShiftLeftOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  RP, AP: PLongWord;
  I: Integer;
  T, C: LongWord;
begin
  Result := False;

  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;

    Res.Top := Num.Top;
  end
  else
  begin
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;
  end;

  AP := Num.D;
  RP := Res.D;
  C := 0;
  for I := 0 to Num.Top - 1 do
  begin
    T := AP^;
    AP := PLongWord(Integer(AP) + SizeOf(LongWord));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PLongWord(Integer(RP) + SizeOf(LongWord));

    if (T and BN_TBIT) <> 0 then
      C := 1
    else
      C := 0;
  end;

  if C <> 0 then
  begin
    RP^ := 1;
    Inc(Res.Top);
  end;
  Result := True;
end;

function BigNumberShiftRightOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  RP, AP: PLongWord;
  I, J: Integer;
  T, C: LongWord;
begin
  Result := False;
  if BigNumberIsZero(Num) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := Num.Top;
  AP := Num.D;

  if PLongWordArray(AP)^[I - 1] = 1 then
    J := I - 1
  else
    J := I;

  if Res <> Num then
  begin
    if BigNumberWordExpand(Res, J) = nil then
      Exit;
    Res.Neg := Num.Neg;
  end;

  RP := Res.D;
  Dec(I);
  T := PLongWordArray(AP)^[I];

  if (T and 1) <> 0 then
    C := BN_TBIT
  else
    C := 0;

  T := T shr 1;
  if T <> 0 then
    PLongWordArray(RP)^[I] := T;

  while I > 0 do
  begin
    Dec(I);
    T := PLongWordArray(AP)^[I];
    PLongWordArray(RP)^[I] := ((T shr 1) and BN_MASK2) or C;

    if (T and 1) <> 0 then
      C := BN_TBIT
    else
      C := 0;
  end;

  Res.Top := J;
  Result := True;
end;

function BigNumberShiftLeft(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, NW, LB, RB: Integer;
  L: LongWord;
  T, F: PLongWordArray;
begin
  Result := False;
  Res.Neg := Num.Neg;
  NW := N div BN_BITS2;

  if BigNumberWordExpand(Res, Num.Top + NW + 1) = nil then
    Exit;

  LB := N mod BN_BITS2;
  RB := BN_BITS2 - LB;

  F := PLongWordArray(Num.D);
  T := PLongWordArray(Res.D);

  T^[Num.Top + NW] := 0;
  if LB = 0 then
  begin
    for I := Num.Top - 1 downto 0 do
      T^[NW + I] := F^[I];
  end
  else
  begin
    for I := Num.Top - 1 downto 0 do
    begin
      L := F[I];
      T^[NW + I + 1] := T^[NW + I + 1] or ((L shr RB) and BN_MASK2);
      T^[NW + I] := (L shl LB) and BN_MASK2;
    end;
  end;

  FillChar(Pointer(T)^, NW * SizeOf(LongWord), 0);
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: LongWord;
  T, F: PLongWordArray;
begin
  Result := False;

  NW := N div BN_BITS2;
  RB := N mod BN_BITS2;
  LB := BN_BITS2 - RB;

  if (NW >= Num.Top) or (Num.Top = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := (BigNumberGetBitsCount(Num) - N + (BN_BITS2 - 1)) div BN_BITS2;
  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, I) = nil then
      Exit;
  end
  else
  begin
    if N = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;

  F := PLongWordArray(Integer(Num.D) + NW * SizeOf(LongWord));
  T := PLongWordArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PLongWordArray(Integer(F) + SizeOf(LongWord));
      T := PLongWordArray(Integer(T) + SizeOf(LongWord));
    end;
  end
  else
  begin
    L := F^[0];
    F := PLongWordArray(Integer(F) + SizeOf(LongWord));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PLongWordArray(Integer(F) + SizeOf(LongWord));
      T := PLongWordArray(Integer(T) + SizeOf(LongWord));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

{* ������ Word ����ϵ�к�����ʼ}

function BigNumberAddWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  I: Integer;
  L: LongWord;
begin
  Result := False;
  W := W and BN_MASK2;

  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    Exit;
  end;

  if Num.Neg <> 0 then // �����ü���
  begin
    Num.Neg := 0;
    Result := BigNumberSubWord(Num, W);
    if not BigNumberIsZero(Num) then
      Num.Neg := 1 - Num.Neg;
    Exit;
  end;

  I := 0;
  while (W <> 0) and (I < Num.Top) do
  begin
    L := (PLongWordArray(Num.D)^[I] + W) and BN_MASK2;
    PLongWordArray(Num.D)^[I] := L;
    if W > L then // ����ȼ���С��˵��������߽�λ�ˣ��ѽ�λ�ø� W��������
      W := 1
    else
      W := 0;
    Inc(I);
  end;

  if (W <> 0) and (I = Num.Top) then // �����λ��Ȼ���������λ
  begin
    if BigNumberWordExpand(Num, Num.Top + 1) = nil then
      Exit;
    Inc(Num.Top);
    PLongWordArray(Num.D)^[I] := W;
  end;
  Result := True;
end;

function BigNumberSubWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  I: Integer;
begin
  W := W and BN_MASK2;

  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    if Result then
      BigNumberSetNegative(Num, True);
    Exit;
  end;

  if Num.Neg <> 0 then
  begin
    Num.Neg := 0;
    Result := BigNumberAddWord(Num, W);
    Num.Neg := 1;
    Exit;
  end;

  if (Num.Top = 1) and (PLongWordArray(Num.D)^[0] < W) then // ������
  begin
    PLongWordArray(Num.D)^[0] := W - PLongWordArray(Num.D)^[0];
    Num.Neg := 1;
    Result := True;
    Exit;
  end;

  I := 0;
  while True do
  begin
    if PLongWordArray(Num.D)^[I] >= W then // ����ֱ�Ӽ�
    begin
      PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] - W;
      Break;
    end
    else
    begin
      PLongWordArray(Num.D)^[I] := (PLongWordArray(Num.D)^[I] - W) and BN_MASK2;
      Inc(I);
      W := 1;  // �������н�λ
    end;
  end;

  if (PLongWordArray(Num.D)^[I] = 0) and (I = Num.Top - 1) then
    Dec(Num.Top);
  Result := True;
end;

function BigNumberMulWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  L: LongWord;
begin
  Result := False;
  W := W and BN_MASK2;
  if Num.Top <> 0 then
  begin
    if W = 0 then
      BigNumberSetZero(Num)
    else
    begin
      L := BigNumberMulWords(PLongWordArray(Num.D), PLongWordArray(Num.D), Num.Top, W);
      if L <> 0 then
      begin
        if BigNumberWordExpand(Num, Num.Top + 1) = nil then
          Exit;
        PLongWordArray(Num.D)^[Num.Top] := L;
        Inc(Num.Top);
      end;
    end;
  end;
  Result := True;
end;

function BigNumberModWord(const Num: TCnBigNumber; W: LongWord): LongWord;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  Result := 0;
  W := W and BN_MASK2;
  for I := Num.Top - 1 downto 0 do
  begin
    Result := ((Result shl BN_BITS4) or ((PLongWordArray(Num.D)^[I] shr BN_BITS4) and BN_MASK2l)) mod W;
    Result := ((Result shl BN_BITS4) or (PLongWordArray(Num.D)^[I] and BN_MASK2l)) mod W;
  end;
end;

function BigNumberDivWord(const Num: TCnBigNumber; W: LongWord): LongWord;
var
  I, J: Integer;
  L, D: LongWord;
begin
  W := W and BN_MASK2;
  if W = 0 then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J; // ��֤ W ���λΪ 1
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  for I := Num.Top - 1 downto 0 do
  begin
    L := PLongWordArray(Num.D)^[I];
    D := BigNumberDivWords(Result, L, W); // W ��֤�����λΪ 1��������� 32 λ
    Result := (L - ((D * W) and BN_MASK2)) and BN_MASK2;

    PLongWordArray(Num.D)^[I] := D;
  end;

  if (Num.Top > 0) and (PLongWordArray(Num.D)^[Num.Top - 1] = 0) then
    Dec(Num.Top);
  Result := Result shr J;
end;

{* ������ Word ����ϵ�к�������}

function BigNumberToString(const Num: TCnBigNumber): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    Result := '0';
    Exit;
  end;
  if BigNumberIsNegative(Num) then
    Result := '-';

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 4;
    while J >= 0 do
    begin
      V := ((PLongWordArray(Num.D)^[I]) shr LongWord(J)) and $0F;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[V + 1];
        Z := 1;
      end;
      Dec(J, 4);
    end;
  end;
end;

function BigNumberToHex(const Num: TCnBigNumber): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    Result := '0';
    Exit;
  end;
  if BigNumberIsNegative(Num) then
    Result := '-';

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 8;
    while J >= 0 do
    begin
      V := ((PLongWordArray(Num.D)^[I]) shr LongWord(J)) and $FF;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[(V shr 4) + 1];
        Result := Result + Hex[(V and $0F) + 1];
        Z := 1;
      end;
      Dec(J, 8);
    end;
  end;
end;

function BigNumberSetHex(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, H, M, J, I, K, C: Integer;
  L: LongWord;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // ����Ч����
  I := 0;
  while PAnsiChar(Integer(P) + I)^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberWordExpand(Res, I * 4) = nil then
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := I;
  H := 0;
  while J > 0 do
  begin
    L := 0;
    if BN_BYTES * 2 <= J then
      M := BN_BYTES * 2
    else
      M := J;

    while True do
    begin
      C := Ord(PAnsiChar(Integer(P) + J - M)^);
      if (C >= Ord('0')) and (C <= Ord('9')) then
        K := C - Ord('0')
      else if (C >= Ord('a')) and (C <= Ord('f')) then
        K := C - Ord('a') + 10
      else if (C >= Ord('A')) and (C <= Ord('F')) then
        K := C - Ord('A') + 10
      else
        K := 0;

      L := (L shl 4) or LongWord(K);

      Dec(M);
      if M <= 0 then
      begin
        PLongWordArray(Res.D)^[H] := L;
        Inc(H);
        Break;
      end;
    end;
    Dec(J, BN_BYTES * 2);
  end;

  Res.Top := H;
  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetHex(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberToDec(const Num: TCnBigNumber): AnsiString;
var
  I, N, R, Len: Integer;
  BnData, LP: PLongWord;
  T: TCnBigNumber;
  P: PAnsiChar;

  function BufRemain(Nu: Integer; Pt: PAnsiChar; Res: PAnsiChar): Integer;
  begin
    Result := Nu + 3 - (Integer(Pt) - Integer(Res));
  end;

begin
  Result := '';

  I := BigNumberGetBitsCount(Num) * 3;
  N := ((I div 10) + (I div 1000) + 1) + 1;

  BnData := nil;
  T := nil;
  try
    BnData := PLongWord(GetMemory(((N div 9) + 1) * SizeOf(LongWord)));
    if BnData = nil then
      Exit;

    SetLength(Result, N + 3);
    FillChar(Result[1], Length(Result), 0);

    T := BigNumberNew;
    if T = nil then
      Exit;

    if BigNumberCopy(T, Num) = nil then
      Exit;

    P := @(Result[1]);
    LP := BnData;

    if BigNumberIsZero(T) then
    begin
      P^ := '0';
      Inc(P);
      P^ := Chr(0);
    end
    else
    begin
      if BigNumberIsNegative(T) then
      begin
        P^ := '-';
        Inc(P);
      end;

      while not BigNumberIsZero(T) do
      begin
        LP^ := BigNumberDivWord(T, BN_DEC_CONV);
        LP := PLongWord(Integer(LP) + SizeOf(LongWord));
      end;
      LP := PLongWord(Integer(LP) - SizeOf(LongWord));

      R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
      AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT), Length(BN_DEC_FMT), [LP^]);
{$ELSE}
      FormatBuf(P^, R, BN_DEC_FMT, Length(BN_DEC_FMT), [LP^]);
{$ENDIF}
      while P^ <> #0 do
        Inc(P);
      while LP <> BnData do
      begin
        LP := PLongWord(Integer(LP) - SizeOf(LongWord));
        R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
        AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT2), Length(BN_DEC_FMT2), [LP^]);
{$ELSE}
        FormatBuf(P^, R, BN_DEC_FMT2, Length(BN_DEC_FMT2), [LP^]);
{$ENDIF}
        while P^ <> #0 do
          Inc(P);
      end;
    end;
  finally
    if BnData <> nil then
      FreeMemory(BnData);
    if T <> nil then
      BigNumberFree(T);
  end;

  Len := SysUtils.StrLen(PAnsiChar(Result));
  if Len >= 0 then
    SetLength(Result, Len); // ȥ��β������� #0
end;

function BigNumberSetDec(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, J, I: Integer;
  L: LongWord;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // ����Ч����
  I := 0;
  while PAnsiChar(Integer(P) + I)^ in ['0'..'9'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberWordExpand(Res, I * 4) = nil then
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := 9 - (I mod 9);
  if J = 9 then
    J := 0;
  L := 0;

  while P^ <> #0 do
  begin
    L := L * 10;
    L := L + Ord(P^) - Ord('0');
    Inc(P);
    Inc(J);
    if J = 9 then
    begin
      BigNumberMulWord(Res, BN_DEC_CONV);
      BigNumberAddWord(Res, L);
      L := 0;
      J := 0;
    end;
  end;

  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetDec(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

// Tmp should have 2 * N DWORDs
procedure BigNumberSqrNormal(R: PLongWord; A: PLongWord; N: Integer; Tmp: PLongWord);
var
  I, J, Max: Integer;
  AP, RP: PLongWordArray;
begin
  Max := N * 2;
  AP := PLongWordArray(A);
  RP := PLongWordArray(R);
  RP[0] := 0;
  RP[Max - 1] := 0;

  RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
  J := N - 1;

  if J > 0 then
  begin
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP[J] := BigNumberMulWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(LongWord))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP[J] := BigNumberMulAddWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(LongWord))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
  end;

  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(R), Max);
  BigNumberSqrWords(PLongWordArray(Tmp), PLongWordArray(A), N);
  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(Tmp), Max);
end;

function BigNumberSqr(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  Max, AL: Integer;
  Tmp, RR: TCnBigNumber;
  T: array[0..15] of LongWord;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num.Top;
  if AL <= 0 then
  begin
    Res.Top := 0;
    Res.Neg := 0;
    Result := True;
    Exit;
  end;

  RR := nil;
  Tmp := nil;
  IsFromPool := False;

  try
    if Num <> Res then
      RR := Res
    else
    begin
      RR := ObtainBigNumberFromPool;
      IsFromPool := True;
    end;

    Tmp := ObtainBigNumberFromPool;
    if (RR = nil) or (Tmp = nil) then
      Exit;

    Max := 2 * AL;
    if BigNumberWordExpand(RR, Max) = nil then
      Exit;

    if AL = 4 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 4, @(T[0]));
    end
    else if AL = 8 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 8, @(T[0]));
    end
    else
    begin
      if BigNumberWordExpand(Tmp, Max) = nil then
        Exit;
      BigNumberSqrNormal(RR.D, Num.D, AL, Tmp.D);
    end;

    RR.Neg := 0;
    if PLongWordArray(Num.D)^[AL - 1] = (PLongWordArray(Num.D)^[AL - 1] and BN_MASK2l) then
      RR.Top := Max - 1
    else
      RR.Top := Max;

    if RR <> Res then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      RecycleBigNumberToPool(RR);
    RecycleBigNumberToPool(Tmp);
  end;
end;

procedure BigNumberMulNormal(R: PLongWord; A: PLongWord; NA: Integer; B: PLongWord;
  NB: Integer);
var
  RR: PLongWord;
  Tmp: Integer;
begin
  if NA < NB then
  begin
    Tmp := NA;
    NA := NB;
    NB := Tmp;

    RR := B;
    B := A;
    A := RR;
  end;

  RR := PLongWord(Integer(R) + NA * SizeOf(LongWord));
  if NB <= 0 then
  begin
    BigNumberMulWords(PLongWordArray(R), PLongWordArray(A), NA, 0);
    Exit;
  end
  else
    RR^ := BigNumberMulWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

  while True do
  begin
    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));

    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);
  end;
end;

function BigNumberMul(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Top, AL, BL: Integer;
  RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num1.Top;
  BL := Num2.Top;

  if (AL = 0) or (BL = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;
  Top := AL + BL;

  RR := nil;
  IsFromPool := False;

  try
    if (Res = Num1) or (Res = Num2) then
    begin
      RR := ObtainBigNumberFromPool;
      IsFromPool := True;
      if RR = nil then
        Exit;
    end
    else
      RR := Res;

    if Num1.Neg <> Num2.Neg then
      RR.Neg := 1
    else
      RR.Neg := 0;

    if BigNumberWordExpand(RR, Top) = nil then
      Exit;
    RR.Top := Top;
    BigNumberMulNormal(RR.D, Num1.D, AL, Num2.D, BL);

    if RR <> Res then
      BigNumberCopy(Res, RR);

    BigNumberCorrectTop(Res);
    Result := True;
  finally
    if IsFromPool then
      RecycleBigNumberToPool(RR);
  end;
end;

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
var
  Tmp, SNum, SDiv, SRes: TCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg, BackupTop, BackupDMax, BackupFlag, BackupNeg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H, QL, QH: LongWord;
  Resp, WNump, BackupD: PLongWord;
  WNum: TCnBigNumber;
  T2: TUInt64;
begin
  Result := False;
  if (Num.Top > 0) and (PLongWordArray(Num.D)^[Num.Top - 1] = 0) then
    Exit;

  if BigNumberIsZero(Divisor) then
    Exit;

  if BigNumberUnsignedCompare(Num, Divisor) < 0 then
  begin
    if BigNumberCopy(Remain, Num) = nil then
      Exit;
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  WNum := nil;
  Tmp := nil;
  SNum := nil;
  SDiv := nil;
  BackupTop := 0;
  BackupDMax := 0;
  BackupNeg := 0;
  BackupD := nil;

  try
    Tmp := ObtainBigNumberFromPool;
    SNum := ObtainBigNumberFromPool;
    SDiv := ObtainBigNumberFromPool;
    SRes := Res;

    if (Tmp = nil) or (SNum = nil) or (SDiv = nil) or (SRes = nil) then
      Exit;

    // �ѳ������Ƶ����λ�� 1������ SDiv���� ȷ������� D0 ���λ�� 1
    NormShift := BN_BITS2 - (BigNumberGetBitsCount(Divisor) mod BN_BITS2);
    if not BigNumberShiftLeft(SDiv, Divisor, NormShift) then
      Exit;

    SDiv.Neg := 0;
    // �ѱ�����ͬ�����ƣ���������һ����
    NormShift := NormShift + BN_BITS2;
    if not BigNumberShiftLeft(SNum, Num, NormShift) then
      Exit;

    SNum.Neg := 0;
    DivN := SDiv.Top;
    NumN := SNum.Top;
    Loop := NumN - DivN;

    WNum := ObtainBigNumberFromPool;
    BackupNeg := WNum.Neg;
    BackupD := WNum.D;
    BackupTop := WNum.Top;
    BackupDMax := WNum.DMax;

    // ע�� WNum ��Ҫʹ���ⲿ�� D���ѳ������ó����Ķ����ȱ���
    WNum.Neg := 0;
    WNum.D := PLongWord(Integer(SNum.D) + Loop * SizeOf(LongWord));
    WNum.Top := DivN;
    WNum.DMax := SNum.DMax - Loop;

    D0 := PLongWordArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PLongWordArray(SDiv.D)^[DivN - 2];
    // D0 D1 �� SDiv ������� DWORD

    WNump := PLongWord(Integer(SNum.D) + (NumN - 1) * SizeOf(LongWord));

    if Num.Neg <> Divisor.Neg then
      SRes.Neg := 1
    else
      SRes.Neg := 0;

    if BigNumberWordExpand(SRes, Loop + 1) = nil then
      Exit;

    SRes.Top := Loop;
    Resp := PLongWord(Integer(SRes.D) + (Loop - 1) * SizeOf(LongWord));

    if BigNumberWordExpand(Tmp, DivN + 1) = nil then
      Exit;

    if BigNumberUnsignedCompare(WNum, SDiv) >= 0 then
    begin
      BigNumberSubWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
        PLongWordArray(SDiv.D), DivN);
      Resp^ := 1;
    end
    else
      Dec(SRes.Top);

    if SRes.Top = 0 then
      SRes.Neg := 0
    else
      Resp := PLongWord(Integer(Resp) - SizeOf(LongWord));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // �� N0/N1/D0/D1 �����һ�� Q ʹ | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PLongWord(Integer(WNump) - SizeOf(LongWord)))^;

      if N0 = D0 then
        Q := BN_MASK2
      else
      begin
        Q := BigNumberDivWords(N0, N1, D0); // D0 �������ı�֤���λ�� 1
        Rem := (N1 - Q * D0) and BN_MASK2;

        T2 := UInt64Mul(D1, Q);
        T2H := (T2 shr 32) and BN_MASK2;
        T2L := T2 and BN_MASK2;

        while True do
        begin
          if (T2H < Rem) or ((T2H = Rem) and
             (T2L <= (PLongWord(Integer(WNump) - 2 * SizeOf(LongWord)))^)) then
             Break;
          Dec(Q);
          Inc(Rem, D0);
          if Rem < D0 then
            Break;
          if T2L < D1 then
            Dec(T2H);
          Dec(T2L, D1);
        end;
      end;

      L0 := BigNumberMulWords(PLongWordArray(Tmp.D), PLongWordArray(SDiv.D), DivN, Q);
      PLongWordArray(Tmp.D)^[DivN] := L0;
      WNum.D := PLongWord(Integer(WNum.D) - SizeOf(LongWord));

      if BigNumberSubWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
        PLongWordArray(Tmp.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
          PLongWordArray(SDiv.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PLongWord(Integer(WNump) - SizeOf(LongWord));
      Resp := PLongWord(Integer(Resp) - SizeOf(LongWord));
    end;

    BigNumberCorrectTop(SNum);
    Neg := Num.Neg;
    BigNumberShiftRight(Remain, SNum, NormShift);
    if not BigNumberIsZero(Remain) then
      Remain.Neg := Neg;

    Result := True;
  finally
    RecycleBigNumberToPool(Tmp);
    RecycleBigNumberToPool(SNum);
    RecycleBigNumberToPool(SDiv);
    // �ָ� WNum ���ݲ��ӻس�����
    WNum.Neg := BackupNeg;
    WNum.D := BackupD;
    WNum.Top := BackupTop;
    WNum.DMax := BackupDMax;
    RecycleBigNumberToPool(WNum);
  end;
end;

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
var
  Res: TCnBigNumber;
begin
  Res := ObtainBigNumberFromPool;
  try
    Result := BigNumberDiv(Res, Remain, Num, Divisor);
  finally
    RecycleBigNumberToPool(Res);
  end;
end;

function BigNumberNonNegativeMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberMod(Remain, Num, Divisor) then
    Exit;
  Result := True;
  if Remain.Neg = 0 then
    Exit;

  // ���� -|Divisor| < Remain < 0��������Ҫ Remain := Remain + |Divisor|
  if Divisor.Neg <> 0 then
    Result := BigNumberSub(Remain, Remain, Divisor)
  else
    Result := BigNumberAdd(Remain, Remain, Divisor);
end;

function BigNumberExp(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
var
  I, Bits: Integer;
  V, RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  RR := nil;
  V := nil;
  IsFromPool := False;

  try
    if (Res = Num) or (Res = Exponent) then
    begin
      RR := ObtainBigNumberFromPool;
      IsFromPool := True;
    end
    else
      RR := Res;

    V := ObtainBigNumberFromPool;
    if (RR = nil) or (V = nil) then
      Exit;

    if BigNumberCopy(V, Num) = nil then
      Exit;

    Bits := BigNumberGetBitsCount(Exponent);
    if BigNumberIsOdd(Exponent) then
    begin
      if BigNumberCopy(RR, Num) = nil then
        Exit;
    end
    else
    begin
      if not BigNumberSetOne(RR) then
        Exit;
    end;

    for I := 1 to Bits - 1 do
    begin
      if not BigNumberSqr(V, V) then
        Exit;

      if BigNumberIsBitSet(Exponent, I) then
        if not BigNumberMul(RR, RR, V) then
          Exit;
    end;

    if Res <> RR then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      RecycleBigNumberToPool(RR);
    RecycleBigNumberToPool(V);
  end;
end;

// շת������� A �� B �����Լ������Լ������ A �� B �У����ص�ַ
function EuclidGcd(A: TCnBigNumber; B: TCnBigNumber): TCnBigNumber;
var
  T: TCnBigNumber;
  Shifts: Integer;
begin
  Result := nil;
  Shifts := 0;
  while not BigNumberIsZero(B) do
  begin
    if BigNumberIsOdd(A) then
    begin
      if BigNumberIsOdd(B) then
      begin
        // A �� B ��
        if not BigNumberSub(A, A, B) then
          Exit;
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else  // A �� B ż
      begin
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end;
    end
    else // A ż
    begin
      if BigNumberIsOdd(B) then
      begin
        // A ż B ��
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else // A ż B ż
      begin
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        Inc(Shifts);
      end;
    end;
  end;

  if Shifts <> 0 then
    if not BigNumberShiftLeft(A, A, Shifts) then
      Exit;
  Result := A;
end;

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  T, A, B: TCnBigNumber;
begin
  Result := False;

  A := nil;
  B := nil;

  try
    A := ObtainBigNumberFromPool;
    B := ObtainBigNumberFromPool;
    if (A = nil) or (B = nil) then
      Exit;

    if BigNumberCopy(A, Num1) = nil then
      Exit;
    if BigNumberCopy(B, Num2) = nil then
      Exit;

    A.Neg := 0;
    B.Neg := 0;
    if BigNumberCompare(A, B) < 0 then
    begin
      T := A;
      A := B;
      B := T;
    end;

    T := EuclidGcd(A, B);
    if T = nil then
      Exit;

    if BigNumberCopy(Res, T) = nil then
      Exit;

    Result := True;
  finally
    RecycleBigNumberToPool(A);
    RecycleBigNumberToPool(B);
  end;
end;

function BigNumberLcm(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  G, M, R: TCnBigNumber;
begin
  Result := False;
  if BigNumberCompare(Num1, Num2) = 0 then
  begin
    BigNumberCopy(Res, Num1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := ObtainBigNumberFromPool;
    M := ObtainBigNumberFromPool;
    R := ObtainBigNumberFromPool;

    if not BigNumberGcd(G, Num1, Num2) then
      Exit;

    if not BigNumberMul(M, Num1, Num2) then
      Exit;

    if not BigNumberDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(M);
    RecycleBigNumberToPool(G);
  end;
end;

// ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�}
function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
var
  T, P: TCnBigNumber;
begin
  if not BigNumberIsNegative(A) and not BigNumberIsNegative(B) then
    Result := BigNumberUnsignedMulMod(Res, A, B, C)
  else if BigNumberIsNegative(A) and BigNumberIsNegative(B) then
  begin
    T := ObtainBigNumberFromPool;
    P := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, A);
      BigNumberCopy(P, B);
      BigNumberSetNegative(T, False);
      BigNumberSetNegative(P, False);
      Result := BigNumberUnsignedMulMod(Res, T, P, C);
    finally
      RecycleBigNumberToPool(T);
      RecycleBigNumberToPool(P);
    end;
  end
  else if BigNumberIsNegative(A) and not BigNumberIsNegative(B) then // A ��
  begin
    T := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, A);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, T, B, C);
      BigNumberSub(Res, C, Res);
    finally
      RecycleBigNumberToPool(T);
    end;
  end
  else if not BigNumberIsNegative(A) and BigNumberIsNegative(B) then // B ��
  begin
    T := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, B);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, A, T, C);
      BigNumberSub(Res, C, Res);
    finally
      RecycleBigNumberToPool(T);
    end;
  end
  else
    Result := False;
end;

// ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�}
function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
var
  AA, BB: TCnBigNumber;
begin
  Result := False;
  AA := nil;
  BB := nil;

  try
    // ʹ����ʱ��������֤ A��B �����ֵ�������仯
    AA := ObtainBigNumberFromPool;
    BB := ObtainBigNumberFromPool;

    BigNumberCopy(AA, A);
    BigNumberCopy(BB, B);
    BigNumberSetNegative(AA, False); // ȫ������
    BigNumberSetNegative(BB, False);

    if not BigNumberMod(AA, AA, C) then
      Exit;

    if not BigNumberMod(BB, BB, C) then
      Exit;

    Res.SetZero; // ��� Res �� A �� B���������������� AA �� BB���ı� A �� B��Ӱ��

    while not BB.IsZero do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberAdd(Res, Res, AA) then
          Exit;

        if not BigNumberMod(Res, Res, C) then
          Exit;
      end;

      if not BigNumberShiftLeftOne(AA, AA) then
        Exit;

      if BigNumberCompare(AA, C) >= 0 then
        if not BigNumberMod(AA, AA, C) then
          Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;
  finally
    RecycleBigNumberToPool(AA);
    RecycleBigNumberToPool(BB);
  end;
  Result := True;
end;

{* ��ͨ���� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ���}
function BigNumberDirectMulMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
begin
  Result := False;
  if A = B then
  begin
    if not BigNumberSqr(Res, A) then
      Exit;
  end
  else
  begin
    if not BigNumberMul(Res, A, B) then
      Exit;
  end;

  if not BigNumberNonNegativeMod(Res, Res, C) then
    Exit;
  Result := True;
end;

// ���ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ
function BigNumberPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  I, J, Bits, WStart, WEnd, Window, WValue, Start: Integer;
  D: TCnBigNumber;
  Val: array[0..31] of TCnBigNumber;

  function WindowBit(B: Integer): Integer;
  begin
    if B > 671 then
      Result := 6
    else if B > 239 then
      Result := 5
    else if B > 79 then
      Result := 4
    else if B > 23 then
      Result := 3
    else
      Result := 1;
  end;

begin
  Result := False;
  Bits := BigNumberGetBitsCount(B);

  if Bits = 0 then
  begin
    if BigNumberAbsIsWord(C, 1) then
      BigNumberSetZero(Res)
    else
      BigNumberSetOne(Res);
    Result := True;
    Exit;
  end;

  D := nil;
  for I := Low(Val) to High(Val) do
    Val[I] := nil;

  try
    Val[0] := ObtainBigNumberFromPool;
    if not BigNumberNonNegativeMod(Val[0], A, C) then
      Exit;

    if BigNumberIsZero(Val[0]) then
    begin
      if not BigNumberSetZero(Res) then
        Exit;
      Result := True;
      Exit;
    end;

    Window := WindowBit(Bits);
    D := ObtainBigNumberFromPool;
    if Window > 1 then
    begin
      if not BigNumberDirectMulMod(D, Val[0], Val[0], C) then
        Exit;

      J := 1 shl (Window - 1);
      for I := 1 to J - 1 do
      begin
        Val[I] := ObtainBigNumberFromPool;
        if not BigNumberDirectMulMod(Val[I], Val[I - 1], D, C) then
          Exit;
      end;
    end;

    Start := 1;
    WStart := Bits - 1;

    if not BigNumberSetOne(Res) then
      Exit;

    while True do
    begin
      if not BigNumberIsBitSet(B, WStart) then
      begin
        if Start = 0 then
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;

        if WStart = 0 then
          Break;

        Dec(WStart);
        Continue;
      end;

      WValue := 1;
      WEnd := 0;
      for I := 1 to Window - 1 do
      begin
        if WStart - I < 0 then
          Break;

        if BigNumberIsBitSet(B, WStart - I) then
        begin
          WValue := WValue shl (I - WEnd);
          WValue := WValue or 1;
          WEnd := I;
        end;
      end;

      J := WEnd + 1;
      if Start = 0 then
      begin
        for I := 0 to J - 1 do
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;
      end;

      if not BigNumberDirectMulMod(Res, Res, Val[WValue shr 1], C) then
        Exit;

      WStart := WStart - WEnd - 1;
      Start := 0;
      if WStart < 0 then
        Break;
    end;
    Result := True;
  finally
    RecycleBigNumberToPool(D);
    for I := Low(Val) to High(Val) do
      RecycleBigNumberToPool(Val[I]);
  end;
end;

// �ɸ����������ټ��� (A ^ B) mod C�������ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ
function BigNumberMontgomeryPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  T, AA, BB: TCnBigNumber;
begin
  Result := False;
  if B.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end;

  AA := nil;
  BB := nil;
  T := nil;

  try
    AA := ObtainBigNumberFromPool;
    BB := ObtainBigNumberFromPool;
    T := ObtainBigNumberFromPool;

    if not T.SetOne then
      Exit;

    if not BigNumberMod(AA, A, C) then
      Exit;

    if BigNumberCopy(BB, B) = nil then
      Exit;

    while not BB.IsOne do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberMulMod(T, AA, T, C) then
          Exit;
      end;
      if not BigNumberMulMod(AA, AA, AA, C) then
        Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;

    if not BigNumberMulMod(Res, AA, T, C) then
      Exit;
  finally
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(AA);
    RecycleBigNumberToPool(BB);
  end;
  Result := True;
end;

function BigNumberFermatCheckComposite(const A, B, C: TCnBigNumber; T: Integer): Boolean;
var
  I: Integer;
  R, L, S: TCnBigNumber;
begin
  Result := False;

  R := nil;
  L := nil;
  S := nil;

  try
    R := ObtainBigNumberFromPool;
    if not BigNumberPowerMod(R, A, C, B) then
      Exit;

    L := ObtainBigNumberFromPool;
    if BigNumberCopy(L, R) = nil then // L := R;
      Exit;

    S := ObtainBigNumberFromPool;
    for I := 1 to T do
    begin
      if not BigNumberMulMod(R, R, R, B) then
        Exit;

      if R.IsOne and not L.IsOne then
      begin
        BigNumberSub(S, B, L);
        if not S.IsOne then
        begin
          Result := True;
          Exit;
        end;
      end;

      if BigNumberCopy(L, R) = nil then
        Exit;
    end;

    Result := not R.IsOne;
  finally
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(L);
    RecycleBigNumberToPool(S);
  end;
end;

// TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberIsProbablyPrime(const Num: TCnBigNumber; TestCount: Integer): Boolean;
var
  I, T: Integer;
  X, R, W: TCnBigNumber;
begin
  Result := False;
  if TestCount <= 1 then
    Exit;

  // �ų��� ������0��1 �Լ� 2 ֮���ż����
  if Num.IsZero or Num.IsNegative or Num.IsOne or (not Num.IsOdd and not BigNumberAbsIsWord(Num, 2))then
    Exit;

  // С�������ȶԱ��жϣ����� 2
  X := ObtainBigNumberFromPool;
  try
    X.SetWord(CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)]);
    if BigNumberCompare(Num, X) <= 0 then
    begin
      for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
      begin
        if BigNumberAbsIsWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    RecycleBigNumberToPool(X);
  end;

  // ����С���������������� 2 �ˣ���Ϊ 2 ֮���ż���Ѿ����ų���
  for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) + 1 to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
  begin
    if BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) = 0 then
      Exit;
  end;

  // ��©���ˣ����� Miller-Rabin Test
  X := nil;
  R := nil;
  W := nil;

  try
    X := ObtainBigNumberFromPool;
    R := ObtainBigNumberFromPool;
    W := ObtainBigNumberFromPool;

    if BigNumberCopy(X, Num) = nil then
      Exit;

    if not BigNumberSubWord(X, 1) then
      Exit;

    if BigNumberCopy(W, X) = nil then  // W := Num - 1;
      Exit;

    T := 0;
    while not X.IsOdd do // X and 1 = 0
    begin
      if not BigNumberShiftRightOne(X, X) then
        Exit;
      Inc(T);
    end;

    for I := 1 to TestCount do
    begin
      if not BigNumberRandRange(R, W) then
        Exit;

      if not BigNumberAddWord(R, 1) then
        Exit;

      if BigNumberFermatCheckComposite(R, Num, X, T) then
        Exit;
    end;
  finally
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(W);
  end;
  Result := True;
end;

function InternalGenerateProbablePrime(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  Mods: array[0..BN_PRIME_NUMBERS - 1] of Cardinal;
  Delta, MaxDelta: Cardinal;
  I: Integer;
label
  AGAIN;
begin
  Result := False;

AGAIN:
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  for I := 1 to BN_PRIME_NUMBERS - 1 do
    Mods[I] := BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]);

  MaxDelta := BN_MASK2 - CN_PRIME_NUMBERS_SQRT_UINT32[BN_PRIME_NUMBERS];
  Delta := 0;

  for I := 1 to BN_PRIME_NUMBERS - 1 do
  begin
    if ((Mods[I] + Delta) mod CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]) <= 1 then
    begin
      Inc(Delta, 2);
      if Delta > MaxDelta then
        goto AGAIN;
      Continue;
    end;
  end;

  if not BigNumberAddWord(Num, Delta) then
    Exit;
  Result := True;
end;

// ����һ��ָ��λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberGeneratePrime(const Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer): Boolean;
begin
  Result := False;
  if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
    Exit;

  while not BigNumberIsProbablyPrime(Num, TestCount) do
  begin
    if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
      Exit;
  end;
  Result := True;
end;

// ����һ��ָ��������λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberGeneratePrimeByBitsCount(const Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := False;
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  if not BigNumberSetBit(Num, BitsCount - 1) then
    Exit;

  if not Num.IsOdd then
    Num.AddWord(1);

  while not BigNumberIsProbablyPrime(Num, TestCount) do
  begin
    Num.AddWord(2);
//    if not BigNumberRandBits(Num, BitsCount) then
//      Exit;
//    if not BigNumberSetBit(Num, BitsCount - 1) then
//      Exit;
//
//    if not Num.IsOdd then
//      Num.AddWord(1);
  end;
  Result := True;
end;

// �� R �Ƿ���� Prime - 1 ��ÿ�����ӣ����� R ^ (ʣ�����ӵĻ�) mod Prime <> 1
function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
var
  I: Integer;
  Res, SubOne, T, Remain: TCnBigNumber;
begin
  Result := False;
  Res := ObtainBigNumberFromPool;
  T := ObtainBigNumberFromPool;
  Remain := ObtainBigNumberFromPool;
  SubOne := ObtainBigNumberFromPool;

  BigNumberCopy(SubOne, Prime);
  BigNumberSubWord(SubOne, 1);

  try
    for I := 0 to Factors.Count - 1 do
    begin
      BigNumberDiv(T, Remain, SubOne, Factors[I]);
      BigNumberMontgomeryPowerMod(Res, R, T, Prime);
      if Res.IsOne then
        Exit;
    end;
    Result := True;
  finally
    RecycleBigNumberToPool(Res);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(Remain);
    RecycleBigNumberToPool(SubOne);
  end;
end;

// �����Ƿ���һ�� 32 λ�з������ͷ�Χ�ڵ���
function BigNumberIsInt32(const Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_32 then // ����
    Exit;
  if C < BN_BITS_UINT_32 then // С�� 32 λ����
  begin
    Result := True;
    Exit;
  end;

  // 32 λ
  if Num.IsNegative then // ������С�� -$80000000 �򳬽�
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1) then
      Result := True  // ���λ��Ϊ 1��˵������ֵС�� $80000000
    else
    begin
      // ���λΪ 1������λ��Ҫȫ 0 ������ Int32
      for C := 0 to BN_BITS_UINT_32 - 2 do
        if BigNumberIsBitSet(Num, C) then // ֻҪ�и� 1 �ͱ�ʾ������
          Exit;
      Result := True;
    end;
  end
  else // ��������Ҫ�ж����λ�Ƿ��� 1���� 1 �򳬽磬Ҳ���Ǵ��� $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1);
end;

// �����Ƿ���һ�� 32 λ�޷������ͷ�Χ�ڵ���
function BigNumberIsUInt32(const Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_32);
end;

// �����Ƿ���һ�� 64 λ�з������ͷ�Χ�ڵ���
function BigNumberIsInt64(const Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_64 then // ����
    Exit;
  if C < BN_BITS_UINT_64 then // С�� 32 λ����
  begin
    Result := True;
    Exit;
  end;

  // 64 λ
  if Num.IsNegative then // ������С�� -$80000000 00000000 �򳬽�
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1) then
      Result := True  // ���λ��Ϊ 1��˵������ֵС�� $80000000 00000000
    else
    begin
      // ���λΪ 1������λ��Ҫȫ 0 ������ Int64
      for C := 0 to BN_BITS_UINT_64 - 2 do
        if BigNumberIsBitSet(Num, C) then // ֻҪ�и� 1 �ͱ�ʾ������
          Exit;
      Result := True;
    end;
  end
  else // ��������Ҫ�ж����λ�Ƿ��� 1���� 1 �򳬽磬Ҳ���Ǵ��� $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1);
end;

// �����Ƿ���һ�� 64 λ�޷������ͷ�Χ�ڵ���
function BigNumberIsUInt64(const Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_64);
end;

// ��չŷ�����շת��������Ԫһ�β������� A * X + B * Y = 1 ��������
procedure BigNumberExtendedEuclideanGcd(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := ObtainBigNumberFromPool;
      P := ObtainBigNumberFromPool;
      M := ObtainBigNumberFromPool;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd(B, P, X, Y);
      BigNumberCopy(T, X);
      BigNumberCopy(X, Y);

      // �� CorrectTop ���� Top ֵ��̫��ԭ����
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // T := X;
      // X := Y;
      // Y := T - (A div B) * Y;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, Y);
      BigNumberSub(Y, T, P);
    finally
      RecycleBigNumberToPool(M);
      RecycleBigNumberToPool(P);
      RecycleBigNumberToPool(T);
    end;
  end;
end;

// ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 ��������
procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := ObtainBigNumberFromPool;
      P := ObtainBigNumberFromPool;
      M := ObtainBigNumberFromPool;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd2(B, P, Y, X);

      // �� CorrectTop ���� Top ֵ��̫��ԭ����
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // Y := Y - (A div B) * X;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, X);
      BigNumberSub(Y, Y, P);
    finally
      RecycleBigNumberToPool(M);
      RecycleBigNumberToPool(P);
      RecycleBigNumberToPool(T);
    end;
  end;
end;

// �� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ�������������б�֤ X��Modulus ����
procedure BigNumberModularInverse(const Res: TCnBigNumber; X, Modulus: TCnBigNumber);
var
  Neg: Boolean;
  X1, Y: TCnBigNumber;
begin
  Neg := False;

  X1 := nil;
  Y := nil;

  try
    X1 := ObtainBigNumberFromPool;
    Y := ObtainBigNumberFromPool;

    BigNumberCopy(X1, X);
    if BigNumberIsNegative(X1) then
    begin
      BigNumberSetNegative(X1, False);
      Neg := True;
    end;

    // ��������ģ��Ԫ��������ģ��Ԫ����������ģ��Ԫ�ĸ�ֵ��������ĸ�ֵ�������ټ� Modulus
    BigNumberExtendedEuclideanGcd2(X1, Modulus, Res, Y);
    // ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 ��������

    if Neg then
      BigNumberSetNegative(Res, not BigNumberIsNegative(Res));

    if BigNumberIsNegative(Res) then
      BigNumberAdd(Res, Res, Modulus);
  finally
    RecycleBigNumberToPool(X1);
    RecycleBigNumberToPool(Y);
  end;
end;

// �ö��λ����ɵݹ�������õ·��� ( A / P) ��ֵ���Ͽ�
function BigNumberLegendre(A, P: TCnBigNumber): Integer;
var
  AA, Q: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise Exception.Create('A, P Must > 0');

  if A.IsOne then
  begin
    Result := 1;
    Exit;
  end;

  AA := ObtainBigNumberFromPool;
  Q := ObtainBigNumberFromPool;

  try
    if A.IsOdd then
    begin
      // ����
      BigNumberMod(AA, P, A);
      Result := BigNumberLegendre(AA, A);

      // ���� (A-1)*(P-1)/4 �� -1 ���
      BigNumberSub(AA, A, CnBigNumberOne);
      BigNumberSub(Q, P, CnBigNumberOne);
      BigNumberMul(Q, AA, Q);
      BigNumberShiftRight(Q, Q, 2);

      if Q.IsOdd then // ������ -1 �˻��ǵ� -1
        Result := -Result;
    end
    else
    begin
      // ż��
      BigNumberShiftRight(AA, A, 1);
      Result := BigNumberLegendre(AA, P);

      // ���� (P^2 - 1)/8 �� -1 ���
      BigNumberMul(Q, P, P);
      BigNumberSubWord(Q, 1);
      BigNumberShiftRight(Q, Q, 3);

      if Q.IsOdd then // ������ -1 �˻��ǵ� -1
        Result := -Result;
    end;
  finally
    RecycleBigNumberToPool(Q);
    RecycleBigNumberToPool(AA);
  end;
end;

// ��ŷ���б𷨼������õ·��� ( A / P) ��ֵ������
function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
var
  R, Res: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise Exception.Create('A, P Must > 0');

  R := ObtainBigNumberFromPool;
  Res := ObtainBigNumberFromPool;

  try
    // ���������P ������ A ʱ���� 0����������ʱ����� A ����ȫƽ�����ͷ��� 1�����򷵻� -1
    BigNumberMod(R, A, P);
    if R.IsZero then
      Result := 0
    else
    begin
      BigNumberCopy(R, P);
      BigNumberSubWord(R, 1);
      BigNumberShiftRightOne(R, R);
      BigNumberMontgomeryPowerMod(Res, A, R, P);

      if Res.IsOne then // ŷ���б�
        Result := 1
      else
        Result := -1;
    end;
  finally
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(Res);
  end;
end;

// ʹ�� Tonelli Shanks �㷨����ģ��������ʣ����⣬�����������б�֤ P Ϊ���������������������η�
function BigNumberTonelliShanks(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  Q, Z, C, R, T, N, L, U, B: TCnBigNumber;
  S, I, M: Integer;
begin
  Result := False;
  if (Res = nil) or A.IsZero or A.IsNegative or P.IsZero or P.IsNegative
    or (BigNumberCompare(A, P) >= 0) then
    Exit;

  // ������õ·��Ų�Ϊ 1��˵���޽⣬����Ͳ�������
  if BigNumberLegendre(A, P) <> 1 then
    Exit;

  Q := ObtainBigNumberFromPool;
  Z := ObtainBigNumberFromPool;
  C := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;
  T := ObtainBigNumberFromPool;
  L := ObtainBigNumberFromPool;
  U := ObtainBigNumberFromPool;
  B := ObtainBigNumberFromPool;
  N := ObtainBigNumberFromPool;

  try
    S := 0;
    BigNumberSub(Q, P, CnBigNumberOne);
    while not Q.IsOdd do
    begin
      BigNumberShiftRightOne(Q, Q);
      Inc(S);
    end;

    // ����һ�� Z ���� ��� P �����õ·���Ϊ -1
    Z.SetWord(2);
    while BigNumberCompare(Z, P) < 0 do
    begin
      if BigNumberLegendre(Z, P) = -1 then
        Break;
      BigNumberAddWord(Z, 1);
    end;

    BigNumberAdd(N, Q, CnBigNumberOne);
    BigNumberShiftRight(N, N, 1);
    BigNumberMontgomeryPowerMod(C, Z, Q, P);
    BigNumberMontgomeryPowerMod(R, A, N, P);
    BigNumberMontgomeryPowerMod(T, A, Q, P);
    M := S;

    while True do
    begin
      BigNumberMod(U, T, P);
      if U.IsOne then
        Break;

      for I := 1 to M - 1 do
      begin
        U.SetOne;
        BigNumberShiftLeft(U, U, I);
        BigNumberMontgomeryPowerMod(N, T, U, P);
        if N.IsOne then
          Break;
      end;

      U.SetOne;
      BigNumberShiftLeft(U, U, M - I - 1);
      BigNumberMontgomeryPowerMod(B, C, U, P);
      M := I;
      BigNumberMulMod(R, R, B, P);

      // T := T*B*B mod P = (T*B mod P) * (B mod P) mod P
      BigNumberMulMod(U, T, B, P); // U := T*B mod P
      BigNumberMod(L, B, P);       // L := B mod P
      BigNumberMulMod(T, U, L, P);

      BigNumberMulMod(C, B, B, P);
    end;

    BigNumberMod(L, R, P);
    BigNumberAdd(L, L, P);
    BigNumberMod(Res, L, P);
    Result := True;
  finally
    RecycleBigNumberToPool(Q);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(C);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(L);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(B);
    RecycleBigNumberToPool(N);
  end;
end;

// ʹ�� IEEE P1363 �淶�е� Lucas ���н���ģ��������ʣ�����
function BigNumberLucas(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  G, X, Z, U, V, T: TCnBigNumber;
begin
  Result := False;

  G := nil;
  X := nil;
  Z := nil;
  U := nil;
  V := nil;
  T := nil;

  try
    G := ObtainBigNumberFromPool;
    X := ObtainBigNumberFromPool;
    Z := ObtainBigNumberFromPool;
    U := ObtainBigNumberFromPool;
    V := ObtainBigNumberFromPool;
    T := ObtainBigNumberFromPool;

    while True do
    begin
      if not BigNumberRandRange(X, P) then
        Exit;

      BigNumberCopy(T, P);
      BigNumberAddWord(T, 1);
      BigNumberShiftRight(T, T, 1);
      if not BigNumberLucasSequenceMod(X, A, T, P, U, V) then
        Exit;

      BigNumberCopy(Z, V);
      if not V.IsOdd then
      begin
        BigNumberShiftRight(Z, Z, 1);
        BigNumberMod(Z, Z, P);
      end
      else
      begin
        BigNumberAdd(Z, Z, P);
        BigNumberShiftRight(Z, Z, 1);
      end;

      if not BigNumberUnsignedMulMod(T, Z, Z, P) then
        Exit;

      if BigNumberCompare(T, A) = 0 then
      begin
        BigNumberCopy(Res, Z);
        Result := True;
        Exit;
      end
      else if BigNumberCompare(U, CnBigNumberOne) > 0 then
      begin
        BigNumberCopy(T, P);
        BigNumberSubWord(T, 1);

        if BigNumberCompare(U, T) < 0 then
          Break;
      end;
    end;
  finally
    RecycleBigNumberToPool(G);
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(V);
    RecycleBigNumberToPool(T);
  end;
end;

procedure BigNumberPollardRho(X: TCnBigNumber; C: TCnBigNumber; Res: TCnBigNumber);
var
  I, K, X0, Y0, Y, D, X1, R: TCnBigNumber;
begin
  I := ObtainBigNumberFromPool;
  K := ObtainBigNumberFromPool;
  X0 := ObtainBigNumberFromPool;
  X1 := ObtainBigNumberFromPool;
  Y0 := ObtainBigNumberFromPool;
  Y := ObtainBigNumberFromPool;
  D := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;

  try
    I.SetOne;
    K.SetZero;
    BigNumberAddWord(K, 2);
    BigNumberCopy(X1, X);
    BigNumberSubWord(X1, 1);
    BigNumberRandRange(X0, X1);
    BigNumberAddWord(X1, 1);
    BigNumberCopy(Y, X0);

    while True do
    begin
      BigNumberAddWord(I, 1);

      BigNumberMulMod(R, X0, X0, X);
      BigNumberAdd(R, R, C);
      BigNumberMod(X0, R, X);

      BigNumberSub(Y0, Y, X0);
      BigNumberGcd(D, Y0, X);

      if not D.IsOne and (BigNumberCompare(D, X) <> 0) then
      begin
        BigNumberCopy(Res, D);
        Exit;
      end;

      if BigNumberCompare(Y, X0) = 0 then
      begin
        BigNumberCopy(Res, X);
        Exit;
      end;

      if BigNumberCompare(I, K) = 0 then
      begin
        BigNumberCopy(Y, X0);
        BigNumberMulWord(K, 2);
      end;
    end;
  finally
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(I);
    RecycleBigNumberToPool(K);
    RecycleBigNumberToPool(X0);
    RecycleBigNumberToPool(X1);
    RecycleBigNumberToPool(Y0);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(D);
  end;
end;

// �ҳ��������������б�
procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
var
  P, R, S, D: TCnBigNumber;
begin
  if BigNumberIsProbablyPrime(Num) then
  begin
    Factors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  P := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;
  S := ObtainBigNumberFromPool;
  D := ObtainBigNumberFromPool;
  try
    P := BigNumberCopy(P, Num);

    while BigNumberCompare(P, Num) >= 0 do
    begin
      BigNumberCopy(S, Num);
      BigNumberSubWord(S, 1);
      BigNumberRandRange(R, S);
      BigNumberAddWord(R, 1);
      BigNumberPollardRho(P, R, P);
    end;

    BigNumberFindFactors(P, Factors);
    D := ObtainBigNumberFromPool;
    BigNumberDiv(D, R, Num, P);
    BigNumberFindFactors(D, Factors);
  finally
    RecycleBigNumberToPool(D);
    RecycleBigNumberToPool(S);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(P);
  end;
end;

// ���� IEEE P1363 �Ĺ淶��˵���� Lucas ����
function BigNumberLucasSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
var
  C, I: Integer;
  V0, V1, Q0, Q1, T0, T1, C2: TCnBigNumber;
begin
  Result := False;
  if K.IsNegative then
    Exit;

  if K.IsZero then
  begin
    Q.SetOne;
    V.SetWord(2);
    Result := True;
    Exit;
  end
  else if K.IsOne then
  begin
    Q.SetOne;
    BigNumberCopy(V, X);
    Result := True;
    Exit;
  end;

  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;
  C2 := nil;

  try
    V0 := ObtainBigNumberFromPool;
    V1 := ObtainBigNumberFromPool;
    Q0 := ObtainBigNumberFromPool;
    Q1 := ObtainBigNumberFromPool;
    T0 := ObtainBigNumberFromPool;
    T1 := ObtainBigNumberFromPool;
    C2 := ObtainBigNumberFromPool;

    C2.SetWord(2);
    V0.SetWord(2);
    BigNumberCopy(V1, X);
    Q0.SetOne;
    Q1.SetOne;

    C := BigNumberGetBitsCount(K);
    if C < 1 then
      Exit;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberMulMod(Q0, Q0, Q1, N) then
        Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberMulMod(Q1, Q0, Y, N) then
          Exit;

        if not BigNumberMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;

        if not BigNumberMulMod(T0, V1, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, C2, Q1, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;

        if not BigNumberMulMod(T0, V0, V0, N) then
          Exit;
        if not BigNumberMulMod(T1, C2, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;
      end;
    end;

    BigNumberCopy(Q, Q0);
    BigNumberCopy(V, V0);
    Result := True;
  finally
    RecycleBigNumberToPool(V0);
    RecycleBigNumberToPool(V1);
    RecycleBigNumberToPool(Q0);
    RecycleBigNumberToPool(Q1);
    RecycleBigNumberToPool(T0);
    RecycleBigNumberToPool(T1);
    RecycleBigNumberToPool(C2);
  end;
end;

// ��ӡ�����ڲ���Ϣ
function BigNumberDebugDump(const Num: TCnBigNumber): string;
var
  I: Integer;
begin
  Result := '';
  if Num = nil then
    Exit;

//    D: PDWORD;          // һ�� array[0..Top-1] of DWORD ���飬Խ����Խ�����λ
//    Top: Integer;       // Top ��ʾ���ޣ�D[Top] Ϊ 0��D[Top - 1] �����λ��Ч��
//    DMax: Integer;      // D ����Ĵ洢����
//    Neg: Integer;       // 1 Ϊ����0 Ϊ��
//    Flags: Integer;

  Result := Format('Flag %d. Neg %d. DMax %d. Top %d.', [Num.Flags, Num.Neg, Num.DMax, Num.Top]);
  if (Num.D <> nil) and (Num.Top > 0) then
    for I := 0 to Num.Top do
      Result := Result + Format(' $%8.8x', [PLongWordArray(Num.D)^[I]]);
end;

{ TCnBigNumber }

function TCnBigNumber.AddWord(W: LongWord): Boolean;
begin
  Result := BigNumberAddWord(Self, W);
end;

procedure TCnBigNumber.Clear;
begin
  BigNumberClear(Self);
end;

function TCnBigNumber.ClearBit(N: Integer): Boolean;
begin
  Result := BigNumberClearBit(Self, N);
end;

constructor TCnBigNumber.Create;
begin
  inherited;
  Flags := BN_FLG_MALLOCED;
  Top := 0;
  Neg := 0;
  DMax := 0;
  D := nil;
end;

destructor TCnBigNumber.Destroy;
begin
{$IFDEF DEBUG}
  if FIsFromPool then
    raise Exception.Create('Error. Try to Free a Big Number From Pool.');
{$ENDIF}

  if (D <> nil) and (BigNumberGetFlag(Self, BN_FLG_STATIC_DATA) = 0) then
    FreeMemory(Self.D);     // �����ⲿ����ľ�̬���ݣ���Ҫ�ͷ�
  if BigNumberGetFlag(Self, BN_FLG_MALLOCED) <> 0 then
  begin
    // Dispose(Num);
  end
  else
  begin
    BigNumberSetFlag(Self, BN_FLG_FREE);
    D := nil;
  end;
  inherited;
end;

function TCnBigNumber.DivWord(W: LongWord): LongWord;
begin
  Result := BigNumberDivWord(Self, W);
end;

class function TCnBigNumber.FromBinary(Buf: PAnsiChar;
  Len: Integer): TCnBigNumber;
begin
  Result := BigNumberFromBinary(Buf, Len);
end;

class function TCnBigNumber.FromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromDec(Buf);
end;

class function TCnBigNumber.FromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromHex(Buf);
end;

function TCnBigNumber.GetBitsCount: Integer;
begin
  Result := BigNumberGetBitsCount(Self);
end;

function TCnBigNumber.GetBytesCount: Integer;
begin
  Result := BigNumberGetBytesCount(Self);
end;

function TCnBigNumber.GetWord: LongWord;
begin
  Result := BigNumberGetWord(Self);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.GetUInt64: UInt64;
begin
  Result := BigNumberGetUInt64(Self);
end;

{$ENDIF}

procedure TCnBigNumber.Init;
begin
  BigNumberInit(Self);
end;

function TCnBigNumber.IsBitSet(N: Integer): Boolean;
begin
  Result := BigNumberIsBitSet(Self, N);
end;

function TCnBigNumber.IsNegative: Boolean;
begin
  Result := BigNumberIsNegative(Self);
end;

function TCnBigNumber.IsOdd: Boolean;
begin
  Result := BigNumberIsOdd(Self);
end;

function TCnBigNumber.IsOne: Boolean;
begin
  Result := BigNumberIsOne(Self);
end;

function TCnBigNumber.IsNegOne: Boolean;
begin
  Result := BigNumberIsNegOne(Self);
end;

function TCnBigNumber.IsWord(W: LongWord): Boolean;
begin
  Result := BigNumberIsWord(Self, W);
end;

function TCnBigNumber.IsZero: Boolean;
begin
  Result := BigNumberIsZero(Self);
end;

function TCnBigNumber.ModWord(W: LongWord): LongWord;
begin
  Result := BigNumberModWord(Self, W);
end;

function TCnBigNumber.MulWord(W: LongWord): Boolean;
begin
  Result := BigNumberMulWord(Self, W);
end;

function TCnBigNumber.SetBit(N: Integer): Boolean;
begin
  Result := BigNumberSetBit(Self, N);
end;

function TCnBigNumber.SetDec(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetDec(Buf, Self);
end;

function TCnBigNumber.SetBinary(Buf: PAnsiChar; Len: Integer): Boolean;
begin
  Result := BigNumberSetBinary(Buf, Len, Self);
end;

function TCnBigNumber.SetHex(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetHex(Buf, Self);
end;

procedure TCnBigNumber.SetNegative(Negative: Boolean);
begin
  BigNumberSetNegative(Self, Negative);
end;

function TCnBigNumber.SetOne: Boolean;
begin
  Result := BigNumberSetOne(Self);
end;

function TCnBigNumber.SetWord(W: LongWord): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.SetUInt64(W: UInt64): Boolean;
begin
  Result := BigNumberSetUInt64(Self, W);
end;

{$ENDIF}

procedure TCnBigNumber.SetZero;
begin
  BigNumberSetZero(Self);
end;

function TCnBigNumber.SubWord(W: LongWord): Boolean;
begin
  Result := BigNumberSubWord(Self, W);
end;

function TCnBigNumber.ToBinary(const Buf: PAnsiChar): Integer;
begin
  Result := BigNumberToBinary(Self, Buf);
end;

function TCnBigNumber.ToDec: string;
begin
  Result := string(BigNumberToDec(Self));
end;

function TCnBigNumber.ToHex: string;
begin
  Result := BigNumberToHex(Self);
end;

function TCnBigNumber.ToString: string;
begin
  Result := BigNumberToString(Self);
end;

function TCnBigNumber.WordExpand(Words: Integer): TCnBigNumber;
begin
  Result := BigNumberWordExpand(Self, Words);
end;


function TCnBigNumber.GetDecString: string;
begin
  Result := ToDec;
end;

function TCnBigNumber.GetHexString: string;
begin
  Result := ToHex;
end;

function TCnBigNumber.GetDebugDump: string;
begin
  Result := BigNumberDebugDump(Self);
end;

procedure FreeBigNumberPool;
var
  I: Integer;
begin
  for I := 0 to FLocalBigNumberPool.Count - 1 do
  begin
{$IFDEF DEBUG}
    TCnBigNumber(FLocalBigNumberPool[I]).FIsFromPool := False;
{$ENDIF}
    TObject(FLocalBigNumberPool[I]).Free;
  end;
  FreeAndNil(FLocalBigNumberPool);
end;

function TCnBigNumber.GetInt64: Int64;
begin
  Result := BigNumberGetInt64(Self);
end;

function TCnBigNumber.SetInt64(W: Int64): Boolean;
begin
  Result := BigNumberSetInt64(Self, W);
end;

{ TCnBigNumberList }

function TCnBigNumberList.Add(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Add(ABigNumber);
end;

constructor TCnBigNumberList.Create(AOwnsObjects: Boolean);
begin
  if not AOwnsObjects then
    raise Exception.Create('MUST Owns Objects.');
  inherited Create(True);
end;

function TCnBigNumberList.GetItem(Index: Integer): TCnBigNumber;
begin
  Result := TCnBigNumber(inherited GetItem(Index));
end;

function TCnBigNumberList.IndexOfValue(ABigNumber: TCnBigNumber): Integer;
begin
  Result := 0;
  while (Result < Count) and (BigNumberCompare(Items[Result], ABigNumber) <> 0) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TCnBigNumberList.Insert(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited Insert(Index, ABigNumber);
end;

function TCnBigNumberList.Remove(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Remove(ABigNumber);
end;

procedure TCnBigNumberList.RemoveDuplicated;
var
  I, Idx: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    // ȥ���ظ�����
    Idx := IndexOfValue(Items[I]);
    if (Idx >= 0) and (Idx <> I) then
      Delete(I);
  end;
end;

procedure TCnBigNumberList.SetItem(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited SetItem(Index, ABigNumber);
end;

initialization
  FLocalBigNumberPool := TObjectList.Create(False);
  CnBigNumberOne := TCnBigNumber.Create;
  CnBigNumberOne.SetOne;
  CnBigNumberZero := TCnBigNumber.Create;
  CnBigNumberZero.SetZero;

finalization
  CnBigNumberOne.Free;
  CnBigNumberZero.Free;
  FreeBigNumberPool;

end.

