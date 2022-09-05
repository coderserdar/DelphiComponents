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

unit CnBloomFilter;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����ϣ����ӳ��Ŀ��ٲ��� BloomFilter ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х
* ��    ע����ϣ�������� = ln2 * (Bit ���� / �ַ�������) ʱ�ж����ţ�����������ʱ
*               ��ϣ�����������˹�����������ʱ�����Է��������Σ�
*               1����ϣ�������� 4������ 32������С��ģ���ݣ�������С����λ�ڴ��
*               2����ϣ�������� 6������ 30
*               3����ϣ�������� 8������ 25
*               4����ϣ�������� 10������ 20
*               5����ϣ�������� 12������ 18
*               6����ϣ�������� 15������ 15�����ڴ��ģ���ݣ��������󣬵�λ�ڴ�С
*               ���ϣ�������� CRC32 ����ͬ�ĳ�ʼֵ����
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2015.05.22 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnCRC32;

const
  CN_LN_2 = 0.69314718;

  CN_BLOOM_HASH_CRC32S: array[0..15] of DWORD =
    ($00000000, $11111111, $22222222, $33333333, $44444444,
     $55555555, $66666666, $77777777, $88888888, $99999999,
     $AAAAAAAA, $BBBBBBBB, $CCCCCCCC, $DDDDDDDD, $EEEEEEEE,
     $FFFFFFFF);

type
  ECnBloomFilterSizeException = class(Exception);

  TCnBloomFilterCapacity = (bfc10Power3, bfc10Power4, bfc10Power5, bfc10Power6,
    bfc10Power7, bfc10Power8);

  TCnHashStringFunc = function (const Str: string): Cardinal;

  TCnStringBloomFilter = class
  {* �ַ��� Bloom Filter ���ϣ���ٲ��ҵ�Ԫ}
  private
    FBits: TBits;
    FHashFuncCount: Integer;
    FHashResults: array of Cardinal;
    FBitSize: Cardinal;
    FCapacity: Integer;
    FCount: Integer;
    procedure CalcSize(ACapacity: TCnBloomFilterCapacity);
  public
    constructor Create(ACapacity: TCnBloomFilterCapacity = bfc10Power4);
    destructor Destroy; override;

    function StrExists(const Str: string): Boolean;
    {* ����ַ����Ƿ����}
    function AddString(const Str: string): Boolean;
    {* ���һ���ַ����Ĺ�ϣ���}
    property Count: Integer read FCount;
    {* ��ӵ��ַ�������}
  end;

implementation

{ TCnStringBloomFilter }

function TCnStringBloomFilter.AddString(const Str: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Str = '' then
    Exit;

  for I := 0 to FHashFuncCount - 1 do
  begin
    FHashResults[I] := StrCRC32(CN_BLOOM_HASH_CRC32S[I], Str) mod FBitSize;
    FBits[FHashResults[I]] := True;
  end;
  Inc(FCount);
  Result := True;
end;

procedure TCnStringBloomFilter.CalcSize(ACapacity: TCnBloomFilterCapacity);
begin
  case ACapacity of
    bfc10Power3:
      begin
        FHashFuncCount := 4;
        FCapacity := 1000;
        FBitSize := FCapacity * 32;
      end;
    bfc10Power4:
      begin
        FHashFuncCount := 6;
        FCapacity := 10000;
        FBitSize := FCapacity * 30;
      end;
    bfc10Power5:
      begin
        FHashFuncCount := 8;
        FCapacity := 100000;
        FBitSize := FCapacity * 25
      end;
    bfc10Power6:
      begin
        FHashFuncCount := 10;
        FCapacity := 1000000;
        FBitSize := FCapacity * 20;
      end;
    bfc10Power7:
      begin
        FHashFuncCount := 12;
        FCapacity := 10000000;
        FBitSize := FCapacity * 18;
      end;
    bfc10Power8:
      begin
        FHashFuncCount := 15;
        FCapacity := 100000000;
        FBitSize := FCapacity * 15;
      end;
  else
    raise ECnBloomFilterSizeException.Create('NO Proper Size Specified.');
  end;

  SetLength(FHashResults, FHashFuncCount);
end;

constructor TCnStringBloomFilter.Create(ACapacity: TCnBloomFilterCapacity);
begin
  inherited Create;
  CalcSize(ACapacity);
  FBits := TBits.Create;
  FBits.Size := FBitSize;
end;

destructor TCnStringBloomFilter.Destroy;
begin
  FBits.Free;
  inherited;
end;

function TCnStringBloomFilter.StrExists(const Str: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FHashFuncCount - 1 do
  begin
    FHashResults[I] := StrCRC32(CN_BLOOM_HASH_CRC32S[I], Str) mod FBitSize;
    if not FBits[FHashResults[I]] then
      Exit;
  end;
  Result := True;
end;

end.
