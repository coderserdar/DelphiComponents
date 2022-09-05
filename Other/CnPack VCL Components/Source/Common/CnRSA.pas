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

unit CnRSA;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�RSA �㷨��Ԫ
* ��Ԫ���ߣ���Х
* ��    ע������ Int64 ��Χ�ڵ� RSA �㷨�Լ������㷨����Կ Exponent �̶�ʹ�� 65537��
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2019.04.19 V1.6
*               ֧�� Win32/Win64/MacOS
*           2018.06.15 V1.5
*               ֧���ļ�ǩ������֤�������� Openssl �е��÷�����ԭʼǩ����ɢ��ǩ�����ࣺ
*               openssl rsautl -sign -in hello -inkey rsa.pem -out hello.default.sign.openssl
*               // ˽Կԭʼǩ����ֱ�Ӱ��ļ����ݲ������˽Կ���ܲ��洢����ͬ�ڼ��ܣ���Ӧ CnRSASignFile ָ�� sdtNone
*               openssl dgst -md5 -sign rsa.pem -out hello.md5.sign.openssl hello
*               openssl dgst -sha1 -sign rsa.pem -out hello.sha1.sign.openssl hello
*               openssl dgst -sha256 -sign rsa.pem -out hello.sha256.sign.openssl hello
*               // ˽Կɢ��ǩ������ָ��ɢ���㷨��Ĭ�� md5����Ӧ CnRSASignFile ��ָ��ɢ���㷨��
*               // ԭʼ�ļ�ɢ��ֵ���� BER ������ PKCS1 �����˽Կ���ܲ��洢��ǩ���ļ�
*               openssl dgst -verify rsa_pub.pem -signature hello.sign.openssl hello
*               // ��Կɢ����֤ԭʼ�ļ���ǩ���ļ���ɢ���㷨������ǩ���ļ��С�
*               // ��Ӧ CnRSAVerify����Կ�⿪ǩ���ļ���ȥ�� PKCS1 �����ٽ⿪ BER ���벢�ȶ�ɢ��ֵ
*           2018.06.14 V1.5
*               ֧���ļ��ӽ��ܣ������� Openssl �е��÷����磺
*               openssl rsautl -encrypt -in hello -inkey rsa_pub.pem -pubin -out hello.en.pub.openssl
*               openssl rsautl -encrypt -in hello -inkey rsa.pem -out hello.en.pub.openssl
*               // �ù�Կ���ܣ���ͬ�ڷ��� CnRSAEncryptFile ������ PublicKey
*               openssl rsautl -decrypt -in hello.en.pub.openssl -inkey rsa.pem -out hello.de.priv.openssl
*               // ��˽Կ���ܣ���ͬ�ڷ��� CnRSADecryptFile ������ PrivateKey
*               ע�� Openssl �ᳫ��Կ����˽Կ���ܣ�������Ҳʵ����˽Կ���ܹ�Կ����
*           2018.06.05 V1.4
*               �� Int64 ֧����չ�� UInt64
*           2018.06.02 V1.4
*               �ܹ�����˽Կ����ɼ��� Openssl ��δ���ܵĹ�˽Կ PEM ��ʽ�ļ�
*           2018.05.27 V1.3
*               �ܹ��� Openssl 1.0.2 ���ɵ�δ���ܵĹ�˽Կ PEM ��ʽ�ļ��ж��빫˽Կ����
*               openssl genrsa -out private_pkcs1.pem 2048
*                  // PKCS#1 ��ʽ�Ĺ�˽Կ
*               openssl pkcs8 -topk8 -inform PEM -in private_pkcs1.pem -outform PEM -nocrypt -out private_pkcs8.pem
*                  // PKCS#8 ��ʽ�Ĺ�˽Կ
*               openssl rsa -in private_pkcs1.pem -outform PEM -RSAPublicKey_out -out public_pkcs1.pem
*                  // PKCS#1 ��ʽ�Ĺ�Կ
*               openssl rsa -in private_pkcs1.pem -outform PEM -pubout -out public_pkcs8.pem
*                  // PKCS#8 ��ʽ�Ĺ�Կ
*           2018.05.22 V1.2
*               ����˽Կ��ϳɶ����Է���ʹ��
*           2017.04.05 V1.1
*               ʵ�ִ����� RSA ��Կ������ӽ���
*           2017.04.03 V1.0
*               ������Ԫ��Int64 ��Χ�ڵ� RSA �� CnPrimeNumber �ж�������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnPrimeNumber, CnBigNumber,
  CnBase64, CnBerUtils, CnNativeDecl, CnMD5, CnSHA1, CnSHA2;

const
  CN_PKCS1_BLOCK_TYPE_PRIVATE_00       = 00;
  CN_PKCS1_BLOCK_TYPE_PRIVATE_FF       = 01;
  CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM    = 02;
  {* RSA �ӽ���ʱ������������ֶ�}

  CN_RSA_PKCS1_PADDING_SIZE            = 11;

  // ���� OID ��Ԥ��д��������̬���������
  OID_RSAENCRYPTION_PKCS1: array[0..8] of Byte = ( // 1.2.840.113549.1.1.1
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $01
  );  // $2A = 40 * 1 + 2

  OID_SIGN_MD5: array[0..7] of Byte = (            // 1.2.840.113549.2.5
    $2A, $86, $48, $86, $F7, $0D, $02, $05
  );

  OID_SIGN_SHA1: array[0..4] of Byte = (           // 1.3.14.3.2.26
    $2B, $0E, $03, $02, $1A
  );

  OID_SIGN_SHA256: array[0..8] of Byte = (         // 2.16.840.1.101.3.4.2.1
    $60, $86, $48, $01, $65, $03, $04, $02, $01
  );

type
  TCnRSASignDigestType = (sdtNone, sdtMD5, sdtSHA1, sdtSHA256);
  {* RSA ǩ����֧�ֵ�����ժҪ�㷨������ժҪ}

  TCnRSAKeyType = (cktPKCS1, cktPKCS8);
  {* RSA ��Կ�ļ���ʽ}

  TCnRSAPrivateKey = class(TPersistent)
  {* RSA ˽Կ}
  private
    FPrimeKey1: TCnBigNumber;
    FPrimeKey2: TCnBigNumber;
    FPrivKeyProduct: TCnBigNumber;
    FPrivKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    property PrimeKey1: TCnBigNumber read FPrimeKey1 write FPrimeKey1;
    {* ������ 1��p}
    property PrimeKey2: TCnBigNumber read FPrimeKey2 write FPrimeKey2;
    {* ������ 2��q}
    property PrivKeyProduct: TCnBigNumber read FPrivKeyProduct write FPrivKeyProduct;
    {* �������˻� n��Ҳ�� Modulus}
    property PrivKeyExponent: TCnBigNumber read FPrivKeyExponent write FPrivKeyProduct;
    {* ˽Կָ�� d}
    property BitsCount: Integer read GetBitsCount;
    {* ��Կ��λ����Ҳ�������˻�����Чλ��}
  end;

  TCnRSAPublicKey = class(TPersistent)
  {* RSA ��Կ}
  private
    FPubKeyProduct: TCnBigNumber;
    FPubKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    property PubKeyProduct: TCnBigNumber read FPubKeyProduct write FPubKeyProduct;
    {* �������˻� n��Ҳ�� Modulus}
    property PubKeyExponent: TCnBigNumber read FPubKeyExponent write FPubKeyExponent;
    {* ��Կָ�� e��65537}
    property BitsCount: Integer read GetBitsCount;
    {* ��Կ��λ����Ҳ�������˻�����Чλ��}
  end;

// UInt64 ��Χ�ڵ� RSA �ӽ���ʵ��

function CnInt64RSAGenerateKeys(out PrimeKey1: Cardinal; out PrimeKey2: Cardinal;
  out PrivKeyProduct: TUInt64; out PrivKeyExponent: TUInt64;
  out PubKeyProduct: TUInt64; out PubKeyExponent: TUInt64; HighBitSet: Boolean = True): Boolean;
{* ���� RSA �㷨����Ĺ�˽Կ�������������� Cardinal��Keys �������� UInt64
   HighBitSet Ϊ True ʱҪ���������λΪ 1���ҳ˻��� 64 Bit}

function CnInt64RSAEncrypt(Data: TUInt64; PrivKeyProduct: TUInt64;
  PrivKeyExponent: TUInt64; out Res: TUInt64): Boolean;
{* �����������ɵ�˽Կ�����ݽ��м��ܣ����ؼ����Ƿ�ɹ�}

function CnInt64RSADecrypt(Res: TUInt64; PubKeyProduct: TUInt64;
  PubKeyExponent: TUInt64; out Data: TUInt64): Boolean;
{* �����������ɵĹ�Կ�����ݽ��н��ܣ����ؽ����Ƿ�ɹ�}

// ������Χ�ڵ� RSA �ӽ���ʵ��

function CnRSAGenerateKeysByPrimeBits(PrimeBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
{* ���� RSA �㷨����Ĺ�˽Կ��PrimeBits �������Ķ�����λ�������������Ϊ���ɡ�
   PrimeBits ȡֵΪ 512/1024/2048�ȣ�ע��Ŀǰ���ǳ˻��ķ�Χ}

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
{* ���� RSA �㷨����Ĺ�˽Կ��ModulusBits �������˻��Ķ�����λ�������������Ϊ���ɡ�
   ModulusBits ȡֵΪ 512/1024/2048��}

function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
{* �� PEM ��ʽ�ļ��м��ع�˽Կ���ݣ���ĳԿ����Ϊ��������}

function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1): Boolean;
{* ����˽Կд�� PEM ��ʽ�ļ��У������Ƿ�ɹ�}

function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
{* �� PEM ��ʽ�ļ��м��ع�Կ���ݣ������Ƿ�ɹ�}

function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8): BOolean;
{* ����Կд�� PEM ��ʽ�ļ��У������Ƿ�ɹ�}

function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
{* �����������ɵ�˽Կ�����ݽ��м��ܣ����ؼ����Ƿ�ɹ�}

function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
{* �����������ɵĹ�Կ�����ݽ��н��ܣ����ؽ����Ƿ�ɹ�}

// RSA �������ļ��ӽ���ʵ��

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ�����ݿ���м��ܣ�����䣬����� OutBuf �У�
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ�����ݿ���м��ܣ�����䣬����� OutBuf �У�
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ�����ݿ�����������ܣ������ OutBuf �У����������ݳ���
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ�����ݿ�����������ܽ���� OutBuf �У����������ݳ���
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ�����ݿ���м��ܣ�����ǰʹ�� PKCS1 ��䣬����� OutBuf �У�
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ�����ݿ���м��ܣ�����ǰʹ�� PKCS1 ��䣬����� OutBuf �У�
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ�����ݿ���н��ܣ����⿪ PKCS1 ��䣬����� OutBuf �У����������ݳ���
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ�����ݿ���н��ܣ����⿪�� PKCS1 ��䣬����� OutBuf �У����������ݳ���
  OutBuf ���Ȳ��ܶ�����Կ���ȣ�1024 Bit �� �� 128 �ֽ�}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ���ļ����м��ܣ�����ǰʹ�� PKCS1 ��䣬���������ļ���}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ���ļ����м��ܣ�����ǰʹ�� PKCS1 ��䣬���������ļ���}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* �ù�Կ���ļ����н��ܣ����⿪�� PKCS1 ��䣬���������ļ���}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* ��˽Կ���ļ����н��ܣ����⿪�� PKCS1 ��䣬���������ļ���}

// RSA �ļ�ǩ������֤ʵ��

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
{* ��˽Կǩ��ָ���ļ���
   δָ������ժҪ�㷨ʱ���ڽ�Դ�ļ��� PKCS1 Private_FF ��������
   ��ָ��������ժҪ�㷨ʱ��ʹ��ָ������ժҪ�㷨���ļ����м���õ�ɢ��ֵ��
   ԭʼ�Ķ�����ɢ��ֵ���� BER ������ PKCS1 ��������˽Կ����}

function CnRSAVerifyFile(const InFileName, InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
{* �ù�Կ��ǩ��ֵ��ָ֤���ļ���Ҳ����ָ������ժҪ�㷨���ļ����м���õ�ɢ��ֵ��
   ���ù�Կ����ǩ�����ݲ��⿪ PKCS1 �����ٽ⿪ BER ����õ�ɢ���㷨��ɢ��ֵ��
   ���ȶ�����������ɢ��ֵ�Ƿ���ͬ��������֤�Ƿ�ͨ��}

function CnRSASignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
{* ��˽Կǩ��ָ���ڴ���}

function CnRSAVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
{* �ù�Կ��ǩ��ֵ��ָ֤���ڴ���}

// Diffie-Hellman ��ɢ������Կ�����㷨

function CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime, MinRoot: TCnBigNumber): Boolean;
{* ���� Diffie-Hellman ��ԿЭ���㷨���������������Сԭ�����漰�����طֽ���˽���}

function CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey: TCnBigNumber;
  const OutPublicKey: TCnBigNumber): Boolean;
{* ��������ѡ�������� PrivateKey ���� Diffie-Hellman ��ԿЭ�̵������Կ
   ���� OutPublicKey = (Root ^ SelfPrivateKey) mod Prime}

function CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey: TCnBigNumber;
  const SecretKey: TCnBigNumber): Boolean;
{* ���ݶԷ����͵� Diffie-Hellman ��ԿЭ�̵������Կ�������ɹ��ϵ���Կ
   ���� SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime}

// ������������

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
{* �� PEM ��ʽ������ļ�����ָ֤��ͷβ�����ʵ�����ݲ����� Base64 ����}

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
{* �� PEM ��ʽ������ļ�����ָ֤��ͷβ�����ʵ�����ݲ����� Base64 ����}

function SaveMemoryToPemFile(const FileName, Head, Tail: string;
  MemoryStream: TMemoryStream): Boolean;
{* �� Stream �����ݽ��� Base64 �������в������ļ�ͷβ��д���ļ�}

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
{* �� BER �������� OID ��ȡ���Ӧ��ɢ��ժҪ����}

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
{* ��һ������������д��һ�� Ber ���͸�ʽ�Ľڵ�}

function AddDigestTypeOIDNodeToWriter(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* ��һ��ɢ���㷨�� OID д��һ�� Ber �ڵ�}

function GetDigestNameFromSignDigestType(Digest: TCnRSASignDigestType): string;
{* ��ǩ��ɢ���㷨ö��ֵ��ȡ������}

implementation

const
  // PKCS#1
  PEM_RSA_PRIVATE_HEAD = '-----BEGIN RSA PRIVATE KEY-----';
  PEM_RSA_PRIVATE_TAIL = '-----END RSA PRIVATE KEY-----';

  PEM_RSA_PUBLIC_HEAD = '-----BEGIN RSA PUBLIC KEY-----';
  PEM_RSA_PUBLIC_TAIL = '-----END RSA PUBLIC KEY-----';

  // PKCS#8
  PEM_PRIVATE_HEAD = '-----BEGIN PRIVATE KEY-----';
  PEM_PRIVATE_TAIL = '-----END PRIVATE KEY-----';

  PEM_PUBLIC_HEAD = '-----BEGIN PUBLIC KEY-----';
  PEM_PUBLIC_TAIL = '-----END PUBLIC KEY-----';

// ���ù�˽Կ�����ݽ��мӽ��ܣ�ע��ӽ���ʹ�õ���ͬһ�׻��ƣ���������
function Int64RSACrypt(Data: TUInt64; Product: TUInt64; Exponent: TUInt64;
  out Res: TUInt64): Boolean;
begin
  Res := MontgomeryPowerMod(Data, Exponent, Product);
  Result := True;
end;

function GetInt64BitCount(A: TUInt64): Integer;
var
  I: Integer;
begin
  I := 0;
  while (A shr I) <> 0 do
    Inc(I);

  Result := I;
end;

// ���� RSA �㷨����Ĺ�˽Կ�������������� Cardinal��Keys �������� TUInt64
function CnInt64RSAGenerateKeys(out PrimeKey1: Cardinal; out PrimeKey2: Cardinal;
  out PrivKeyProduct: TUInt64; out PrivKeyExponent: TUInt64;
  out PubKeyProduct: TUInt64; out PubKeyExponent: TUInt64; HighBitSet: Boolean): Boolean;
var
  N: Cardinal;
  Succ: Boolean;
  Product, Y: TUInt64;
begin
  repeat
    PrimeKey1 := CnGenerateUInt32Prime(HighBitSet);

    N := Trunc(Random * 1000);
    Sleep(N);

    PrimeKey2 := CnGenerateUInt32Prime(HighBitSet);
    if HighBitSet then
    begin
      Product := TUInt64(PrimeKey1) * TUInt64(PrimeKey2);
      Succ := GetInt64BitCount(Product) = 64;
    end
    else
      Succ := True;
  until Succ;

  if PrimeKey2 > PrimeKey1 then  // һ��ʹ p > q
  begin
    N := PrimeKey1;
    PrimeKey1 := PrimeKey2;
    PrimeKey2 := N;
  end;

  PrivKeyProduct := TUInt64(PrimeKey1) * TUInt64(PrimeKey2);
  PubKeyProduct := TUInt64(PrimeKey2) * TUInt64(PrimeKey1);   // �� n �ڹ�˽Կ������ͬ��
  PubKeyExponent := 65537;                                    // �̶�

  Product := TUInt64(PrimeKey1 - 1) * TUInt64(PrimeKey2 - 1);

  //                      e                d             (p-1)(q-1)
  // ��շת������� PubKeyExponent * PrivKeyExponent mod Product = 1 �е� PrivKeyExponent
  // r = (p-1)(q-1) Ҳ���ǽⷽ�� e * d + r * y = 1������ e��r ��֪���� d �� y��
  CnInt64ExtendedEuclideanGcd(PubKeyExponent, Product, PrivKeyExponent, Y);
  while UInt64IsNegative(PrivKeyExponent) do
  begin
     // ���������� d С�� 0���򲻷�����������Ҫ�� d ���� r���ӵ�������Ϊֹ
     PrivKeyExponent := PrivKeyExponent + Product;
  end;
  Result := True;
end;

// �����������ɵ�˽Կ�����ݽ��м��ܣ����ؼ����Ƿ�ɹ�
function CnInt64RSAEncrypt(Data: TUInt64; PrivKeyProduct: TUInt64;
  PrivKeyExponent: TUInt64; out Res: TUInt64): Boolean;
begin
  Result := Int64RSACrypt(Data, PrivKeyProduct, PrivKeyExponent, Res);
end;

// �����������ɵĹ�Կ�����ݽ��н��ܣ����ؽ����Ƿ�ɹ�
function CnInt64RSADecrypt(Res: TUInt64; PubKeyProduct: TUInt64;
  PubKeyExponent: TUInt64; out Data: TUInt64): Boolean;
begin
  Result := Int64RSACrypt(Res, PubKeyProduct, PubKeyExponent, Data);
end;

function CnRSAGenerateKeysByPrimeBits(PrimeBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  N: Integer;
  Suc: Boolean;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if PrimeBits <= 16 then
    Exit;

  PrivateKey.Clear;
  PublicKey.Clear;

  Suc := False;
  while not Suc do
  begin
    if not BigNumberGeneratePrime(PrivateKey.PrimeKey1, PrimeBits div 8) then
      Exit;

    N := Trunc(Random * 1000);
    Sleep(N);

    if not BigNumberGeneratePrime(PrivateKey.PrimeKey2, PrimeBits div 8) then
      Exit;

    // TODO: p �� q �Ĳ�ܹ�С��������ʱ�� Continue

    // һ��Ҫ�� Prime1 > Prime2 �Ա���� CRT �Ȳ���
    if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
      BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

    if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    // p��q �Ļ��Ƿ����� Bit ����������ʱ�� Continue
    if PrivateKey.PrivKeyProduct.GetBitsCount <> PrimeBits * 2 then
      Continue;

    // TODO: pq �Ļ��� NAF ϵ���Ƿ�����������������ʱ�� Continue

    if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    PublicKey.PubKeyExponent.SetDec('65537');

    Rem := nil;
    Y := nil;
    R := nil;
    S1 := nil;
    S2 := nil;
    One := nil;

    try
      Rem := TCnBigNumber.Create;
      Y := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      S1 := TCnBigNumber.Create;
      S2 := TCnBigNumber.Create;
      One := TCnBigNumber.Create;

      BigNumberSetOne(One);
      BigNumberSub(S1, PrivateKey.PrimeKey1, One);
      BigNumberSub(S2, PrivateKey.PrimeKey2, One);
      BigNumberMul(R, S1, S2);     // ���������R = (p - 1) * (q - 1)

      // �� e Ҳ���� PubKeyExponent��65537����Ի��� R ��ģ��Ԫ�� d Ҳ���� PrivKeyExponent
      BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y);

      // ���������� d С�� 0���򲻷�����������Ҫ�� d ���ϻ��� R
      if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
         BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);

      // TODO: d ����̫С��������ʱ�� Continue
    finally
      One.Free;
      S2.Free;
      S1.Free;
      R.Free;
      Y.Free;
      Rem.Free;
    end;

    Suc := True;
  end;
  Result := True;
end;

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  N, PB1, PB2, MinDB, MinW: Integer;
  Suc: Boolean;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if ModulusBits < 128 then
    Exit;

  PrivateKey.Clear;
  PublicKey.Clear;
  Suc := False;

  PB1 := (ModulusBits + 1) div 2;
  PB2 := ModulusBits - PB1;
  MinDB := ModulusBits div 2 - 100;
  if MinDB < ModulusBits div 3 then
    MinDB := ModulusBits div 3;
  MinW := ModulusBits shr 2;

  while not Suc do
  begin
    if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey1, PB1) then
      Exit;

    N := Trunc(Random * 1000);
    Sleep(N);

    if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey2, PB2) then
      Exit;

    // TODO: p �� q �Ĳ�ܹ�С��������ʱ�� Continue

    // һ��Ҫ�� Prime1 > Prime2 �Ա���� CRT �Ȳ���
    if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
      BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

    if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    // p��q �Ļ��Ƿ����� Bit ����������ʱ�� Continue
    if PrivateKey.PrivKeyProduct.GetBitsCount <> ModulusBits then
      Continue;

    // TODO: pq �Ļ��� NAF ϵ���Ƿ�����������������ʱ�� Continue

    if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    PublicKey.PubKeyExponent.SetDec('65537');

    Rem := nil;
    Y := nil;
    R := nil;
    S1 := nil;
    S2 := nil;
    One := nil;

    try
      Rem := TCnBigNumber.Create;
      Y := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      S1 := TCnBigNumber.Create;
      S2 := TCnBigNumber.Create;
      One := TCnBigNumber.Create;

      BigNumberSetOne(One);
      BigNumberSub(S1, PrivateKey.PrimeKey1, One);
      BigNumberSub(S2, PrivateKey.PrimeKey2, One);
      BigNumberMul(R, S1, S2);     // ���������R = (p - 1) * (q - 1)

      // �� e Ҳ���� PubKeyExponent��65537����Ի��� R ��ģ��Ԫ�� d Ҳ���� PrivKeyExponent
      BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y);

      // ���������� d С�� 0���򲻷�����������Ҫ�� d ���ϻ��� R
      if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
         BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);

      // TODO: d ����̫С��������ʱ�� Continue
    finally
      One.Free;
      S2.Free;
      S1.Free;
      R.Free;
      Y.Free;
      Rem.Free;
    end;

    Suc := True;
  end;
  Result := True;
end;

procedure SplitStringToList(const S: string; List: TStrings);
const
  LINE_WIDTH = 64;
var
  C, R: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  if S <> '' then
  begin
    R := S;
    while R <> '' do
    begin
      C := Copy(R, 1, LINE_WIDTH);
      Delete(R, 1, LINE_WIDTH);
      List.Add(C);
    end;
  end;
end;

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  I: Integer;
  S: string;
  Sl: TStringList;
begin
  Result := False;

  if (Stream <> nil) and (Stream.Size > 0) and (ExpectHead <> '') and (ExpectTail <> '') then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromStream(Stream);
      if Sl.Count > 2 then
      begin
        if Trim(Sl[0]) <> ExpectHead then
          Exit;

        if Trim(Sl[Sl.Count - 1]) = '' then
          Sl.Delete(Sl.Count - 1);

        if Trim(Sl[Sl.Count - 1]) <> ExpectTail then
          Exit;

        Sl.Delete(Sl.Count - 1);
        Sl.Delete(0);
        S := '';
        for I := 0 to Sl.Count - 1 do
          S := S + Sl[I];

        // To De Base64 S
        MemoryStream.Clear;
        Result := (BASE64_OK = Base64Decode(S, MemoryStream, False));
      end;
    finally
      Sl.Free;
    end;
  end;
end;

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadPemStreamToMemory(Stream, ExpectHead, ExpectTail, MemoryStream);
  finally
    Stream.Free;
  end;
end;

function SaveMemoryToPemFile(const FileName, Head, Tail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  S: string;
  List: TStringList;
begin
  Result := False;
  if (MemoryStream <> nil) and (MemoryStream.Size <> 0) then
  begin
    MemoryStream.Position := 0;
    if Base64_OK = Base64Encode(MemoryStream, S) then
    begin
      List := TStringList.Create;
      try
        SplitStringToList(S, List);

        List.Insert(0, Head);
        List.Add(Tail);

        List.SaveToFile(FileName);
        Result := True;
      finally
        List.Free;
      end;
    end;
  end;
end;

procedure PutIndexedBigIntegerToBigInt(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
var
  P: Pointer;
begin
  if (Node = nil) or (Node.BerDataLength <= 0) then
    Exit;

  P := GetMemory(Node.BerDataLength);
  Node.CopyDataTo(P);
  BigNumber.SetBinary(P, Node.BerDataLength);
  FreeMemory(P);
end;

// �� PEM ��ʽ�ļ��м��ع�˽Կ����
(*
PKCS#1:
  RSAPrivateKey ::= SEQUENCE {                        0
    version Version,                                  1 0
    modulus INTEGER, �C n                             2 ��˽Կ
    publicExponent INTEGER, �C e                      3 ��Կ
    privateExponent INTEGER, �C d                     4 ˽Կ
    prime1 INTEGER, �C p                              5 ˽Կ
    prime2 INTEGER, �C q                              6 ˽Կ
    exponent1 INTEGER, �C d mod (p-1)                 7 CRT ϵ�� 1
    exponent2 INTEGER, �C d mod (q-1)                 8 CRT ϵ�� 2
    coefficient INTEGER, �C (1/q) mod p               9 CRT ϵ�� 3��q ��� p ��ģ��Ԫ
    otherPrimeInfos OtherPrimeInfos OPTIONAL          10

    ģ��Ԫ x = (1/q) mod p �ɵ� xq = 1 mod p Ҳ�� xq = 1 + yp Ҳ���� qx + (-p)y = 1
    ��������չŷ�����շת�����ֱ�����
  }

PKCS#8:
  PrivateKeyInfo ::= SEQUENCE {
    version         Version,
    algorithm       AlgorithmIdentifier,
    PrivateKey      OCTET STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  PrivateKey ������ PKCS#1 �� RSAPrivateKey �ṹ
  Ҳ����
  SEQUENCE (3 elem)
    INTEGER 0
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    OCTET STRING (1 elem)
      SEQUENCE (9 elem)
        INTEGER 0
        INTEGER                                       8 ��˽Կ Modulus
        INTEGER                                       9 ��Կ   e
        INTEGER                                       10 ˽Կ  d
        INTEGER                                       11 ˽Կ  p
        INTEGER                                       12 ˽Կ  q
        INTEGER
        INTEGER
        INTEGER
*)
function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
var
  MemStream: TMemoryStream;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  MemStream := nil;
  Reader := nil;

  try
    MemStream := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_RSA_PRIVATE_HEAD, PEM_RSA_PRIVATE_TAIL, MemStream) then
    begin
      // �� PKCS#1 �� PEM ��˽Կ��ʽ
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
      Reader.ParseToTree;
      if Reader.TotalCount >= 8 then
      begin
        Node := Reader.Items[1]; // 0 ������ Sequence��1 �� Version
        if Node.AsByte = 0 then // ֻ֧�ְ汾 0
        begin
          // 2 �� 3 ���ɹ�Կ
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[2], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[3], PublicKey.PubKeyExponent);
          end;

          // 2 4 5 6 ����˽Կ
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[2], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[4], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Reader.Items[5], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Reader.Items[6], PrivateKey.PrimeKey2);
          end;

          Result := True;
        end;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL, MemStream) then
    begin
      // �� PKCS#8 �� PEM ��˽Կ��ʽ
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
      Reader.ParseToTree;
      if Reader.TotalCount >= 12 then
      begin
        Node := Reader.Items[1]; // 0 ������ Sequence��1 �� Version
        if Node.AsByte = 0 then // ֻ֧�ְ汾 0
        begin
          // 8 �� 9 ���ɹ�Կ
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[8], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[9], PublicKey.PubKeyExponent);
          end;
      
          // 8 10 11 12 ����˽Կ
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[8], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[10], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Reader.Items[11], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Reader.Items[12], PrivateKey.PrimeKey2);
          end;
      
          Result := True;
        end;
      end;
    end;
  finally
    MemStream.Free;
    Reader.Free;
  end;
end;

// �� PEM ��ʽ�ļ��м��ع�Կ����
// ע�� PKCS#8 �� PublicKey �� PEM �ڱ�׼ ASN.1 ������һ���װ��
// �� Modulus �� Exponent ������ BitString �У���Ҫ Paser ��������
(*
PKCS#1:
  RSAPublicKey ::= SEQUENCE {
      modulus           INTEGER,  -- n
      publicExponent    INTEGER   -- e
  }

PKCS#8:
  PublicKeyInfo ::= SEQUENCE {
    algorithm       AlgorithmIdentifier,
    PublicKey       BIT STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  Ҳ����
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING (1 elem)
      SEQUENCE (2 elem)
        INTEGER     - Modulus
        INTEGER     - Exponent
*)
function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Mem: TMemoryStream;
  Reader: TCnBerReader;
begin
  Result := False;
  Mem := nil;
  Reader := nil;

  try
    Mem := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_PUBLIC_HEAD, PEM_PUBLIC_TAIL, Mem) then
    begin
      // �� PKCS#8 ��ʽ�Ĺ�Կ
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
      Reader.ParseToTree;
      if Reader.TotalCount >= 7 then
      begin
        // 6 �� 7 ���ɹ�Կ
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Reader.Items[6], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Reader.Items[7], PublicKey.PubKeyExponent);
        end;

        Result := True;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_RSA_PUBLIC_HEAD, PEM_RSA_PUBLIC_TAIL, Mem) then
    begin
      // �� PKCS#1 ��ʽ�Ĺ�Կ
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size);
      Reader.ParseToTree;
      if Reader.TotalCount >= 3 then
      begin
        // 1 �� 2 ���ɹ�Կ
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Reader.Items[1], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Reader.Items[2], PublicKey.PubKeyExponent);
        end;
      
        Result := True;
      end;
    end;
  finally
    Mem.Free;
    Reader.Free;
  end;
end;

// ����������λ�� 1������Ҫǰ�油 0 �����븺���ı�������
function CalcIntegerTLV(BigNumber: TCnBigNumber): Cardinal;
begin
  Result := BigNumber.GetBytesCount;
  if BigNumber.IsBitSet((Result * 8) - 1) then
    Inc(Result);
end;

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  P: Pointer;
  C, D: Integer;
begin
  Result := nil;
  if (Writer = nil) or (Num = nil) then
    Exit;

  // Integer ������Ҫ�������λ
  C := CalcIntegerTLV(Num);
  if C <= 0 then
    Exit;

  P := GetMemory(C);
  D := C - Num.GetBytesCount;

  FillChar(P^, D, 0);
  Num.ToBinary(PAnsiChar(Integer(P) + D));

  Result := Writer.AddBasicNode(CN_BER_TAG_INTEGER, P, C, Parent);
  FreeMemory(P);
end;

// ����˽Կд�� PEM ��ʽ�ļ���
function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  N, T, R1, R2, X, Y : TCnBigNumber;
  B: Byte;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  if (PrivateKey = nil) or (PrivateKey.PrivKeyProduct.GetBytesCount <= 0) or
    (PrivateKey.PrivKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  Writer := nil;
  T := nil;
  R1 := nil;
  R2 := nil;
  N := nil;
  X := nil;
  Y := nil;

  try
    T := BigNumberNew;
    R1 := BigNumberNew;
    R2 := BigNumberNew;
    N := BigNumberNew;
    X := BigNumberNew;
    Y := BigNumberNew;
    if not T.SetOne then
      Exit;

    BigNumberSub(N, PrivateKey.PrimeKey1, T);
    BigNumberMod(R1, PrivateKey.PrivKeyExponent, N); // R1 = d mod (p - 1)

    BigNumberSub(N, PrivateKey.PrimeKey2, T);
    BigNumberMod(R2, PrivateKey.PrivKeyExponent, N); // R2 = d mod (q - 1)

    // X = �ǲ������� qx + (-p)y = 1 �Ľ�
    BigNumberExtendedEuclideanGcd(PrivateKey.PrimeKey2, PrivateKey.PrimeKey1, X, Y);
    if BigNumberIsNegative(X) then
      BigNumberAdd(X, X, PrivateKey.PrimeKey1);

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    B := 0;
    if KeyType = cktPKCS1 then
    begin
      // ƴ PKCS1 ��ʽ������
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Root);
      AddBigNumberToWriter(Writer, R1, Root);
      AddBigNumberToWriter(Writer, R2, Root);
      AddBigNumberToWriter(Writer, X, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // ƴ PKCS8 ��ʽ������
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // �� Node1 �� ObjectIdentifier �� Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_OCTET_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);

      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Node);
      AddBigNumberToWriter(Writer, R1, Node);
      AddBigNumberToWriter(Writer, R2, Node);
      AddBigNumberToWriter(Writer, X, Node);
    end;

    // ������ˣ������ Base64 �ٷֶ���ƴͷβ���д�ļ�
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);

    if KeyType = cktPKCS1 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_RSA_PRIVATE_HEAD,
        PEM_RSA_PRIVATE_TAIL, Mem)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_PRIVATE_HEAD,
        PEM_PRIVATE_TAIL, Mem);
  finally
    BigNumberFree(T);
    BigNumberFree(R1);
    BigNumberFree(R2);
    BigNumberFree(N);
    BigNumberFree(X);
    BigNumberFree(Y);

    Mem.Free;
    Writer.Free;
  end;
end;

// ����Կд�� PEM ��ʽ�ļ���
function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  Writer := nil;
  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    if KeyType = cktPKCS1 then
    begin
      // ƴ PKCS1 ��ʽ�����ݣ��Ƚϼ�
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // ƴ PKCS8 ��ʽ������
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // �� Node �� ObjectIdentifier �� Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_BIT_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
    end;

    // ������ˣ������ Base64 �ٷֶ���ƴͷβ���д�ļ�
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);

    if KeyType = cktPKCS1 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_RSA_PUBLIC_HEAD,
        PEM_RSA_PUBLIC_TAIL, Mem)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_PUBLIC_HEAD,
        PEM_PUBLIC_TAIL, Mem);
  finally
    Mem.Free;
    Writer.Free;
  end;
end;

// ���ù�˽Կ�����ݽ��мӽ��ܣ�ע��ӽ���ʹ�õ���ͬһ�׻��ƣ���������
function RSACrypt(Data: TCnBigNumber; Product: TCnBigNumber; Exponent: TCnBigNumber;
  Res: TCnBigNumber): Boolean;
begin
  Result := BigNumberMontgomeryPowerMod(Res, Data, Exponent, Product);
end;

// �����������ɵ�˽Կ�����ݽ��м��ܣ����ؼ����Ƿ�ɹ�
function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res);
end;

// �����������ɵĹ�Կ�����ݽ��н��ܣ����ؽ����Ƿ�ɹ�
function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Res, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Data);
end;

{ TCnRSAPrivateKey }

procedure TCnRSAPrivateKey.Assign(Source: TPersistent);
begin
  if Source is TCnRSAPrivateKey then
  begin
    BigNumberCopy(FPrimeKey1, (Source as TCnRSAPrivateKey).PrimeKey1);
    BigNumberCopy(FPrimeKey2, (Source as TCnRSAPrivateKey).PrimeKey2);
    BigNumberCopy(FPrivKeyProduct, (Source as TCnRSAPrivateKey).PrivKeyProduct);
    BigNumberCopy(FPrivKeyExponent, (Source as TCnRSAPrivateKey).PrivKeyExponent);
  end
  else
    inherited;
end;

procedure TCnRSAPrivateKey.Clear;
begin
  FPrimeKey1.Clear;
  FPrimeKey2.Clear;
  FPrivKeyProduct.Clear;
  FPrivKeyExponent.Clear;
end;

constructor TCnRSAPrivateKey.Create;
begin
  FPrimeKey1 := TCnBigNumber.Create;
  FPrimeKey2 := TCnBigNumber.Create;
  FPrivKeyProduct := TCnBigNumber.Create;
  FPrivKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPrivateKey.Destroy;
begin
  FPrimeKey1.Free;
  FPrimeKey2.Free;
  FPrivKeyProduct.Free;
  FPrivKeyExponent.Free;
  inherited;
end;

function TCnRSAPrivateKey.GetBitsCount: Integer;
begin
  Result := FPrivKeyProduct.GetBitsCount;
end;

{ TCnRSAPublicKey }

procedure TCnRSAPublicKey.Assign(Source: TPersistent);
begin
  if Source is TCnRSAPublicKey then
  begin
    BigNumberCopy(FPubKeyProduct, (Source as TCnRSAPublicKey).PubKeyProduct);
    BigNumberCopy(FPubKeyExponent, (Source as TCnRSAPublicKey).PubKeyExponent);
  end
  else
    inherited;
end;

procedure TCnRSAPublicKey.Clear;
begin
  FPubKeyProduct.Clear;
  FPubKeyExponent.Clear;
end;

constructor TCnRSAPublicKey.Create;
begin
  FPubKeyProduct := TCnBigNumber.Create;
  FPubKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPublicKey.Destroy;
begin
  FPubKeyExponent.Free;
  FPubKeyProduct.Free;
  inherited;
end;

function TCnRSAPublicKey.GetBitsCount: Integer;
begin
  Result := FPubKeyProduct.GetBitsCount;
end;

{ RSA ���ܽ�������}

function RSACryptRawData(Data: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; Exponent, Product: TCnBigNumber): Boolean;
var
  D, R: TCnBigNumber;
begin
  Result := False;
  if (Data <> nil) and (DataLen > 0) then
  begin
    R := TCnBigNumber.Create;
    D := TCnBigNumber.FromBinary(PAnsiChar(Data), DataLen);

    if RSACrypt(D, Product, Exponent, R) then
    begin
      R.ToBinary(OutBuf);
      OutLen := R.GetBytesCount;
      Result := True;
    end;
  end;
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataLen, OutBuf, OutLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataLen, OutBuf, OutLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataLen, OutBuf, OutLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataLen, OutBuf, OutLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

// �����ݿ鲹���������д�� Stream �У����سɹ����
// PaddingType ȡ 0��1��2��BlockLen �ֽ����� 128 ��
// EB = 00 || BT || PS || 00 || D
function PKCS1AddPadding(PaddingType, BlockSize: Integer; Data: Pointer; DataLen: Integer;
  outStream: TStream): Boolean;
var
  I: Integer;
  B, F: Byte;
begin
  Result := False;
  if (Data = nil) or (DataLen <= 0) then
    Exit;

  // ���������
  if DataLen > BlockSize - CN_RSA_PKCS1_PADDING_SIZE then
    Exit;

  B := 0;
  outStream.Write(B, 1);       // дǰ���ֽ� 00
  B := PaddingType;
  F := BlockSize - DataLen - 3; // 3 ��ʾһ��ǰ�� 00��һ�������ֽڡ�һ������� 00 ��β

  case PaddingType of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        outStream.Write(B, 1);
        B := 0;
        for I := 1 to F do
          outStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF:
      begin
        outStream.Write(B, 1);
        B := $FF;
        for I := 1 to F do
          outStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        outStream.Write(B, 1);
        Randomize;
        for I := 1 to F do
        begin
          B := Trunc(Random(255));
          if B = 0 then
            Inc(B);
          outStream.Write(B, 1);
        end;
      end;
  else
    Exit;
  end;

  B := 0;
  outStream.Write(B, 1);
  outStream.Write(Data^, DataLen);
  Result := True;
end;

// ��һƬ�ڴ�����ָ���� Padding ��ʽ������� RSA �ӽ��ܼ���
function RSAPaddingCrypt(PaddingType, BlockSize: Integer; PlainData: Pointer;
  DataLen: Integer; OutBuf: Pointer; Exponent, Product: TCnBigNumber): Boolean;
var
  Stream: TMemoryStream;
  Res, Data: TCnBigNumber;
begin
  Result := False;
  Res := nil;
  Data := nil;
  Stream := nil;
  try
    Stream := TMemoryStream.Create;
    if not PKCS1AddPadding(PaddingType, BlockSize, PlainData, DataLen, Stream) then
      Exit;

    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(Stream.Memory), Stream.Size);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    Res.ToBinary(PAnsiChar(OutBuf));
    Result := True;
  finally
    Stream.Free;
    Data.Free;
    Res.Free;
  end;
end;

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM, PublicKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PublicKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PublicKey.BitsCount div 8);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PrivateKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PrivateKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PrivateKey.BitsCount div 8);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function PKCS1RemovePadding(InData: Pointer; InDataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer): Boolean;
var
  P: PAnsiChar;
  I, J, Start: Integer;
begin
  Result := False;
  OutLen := 0;
  I := 0;

  P := PAnsiChar(InData);
  while P[I] = #0 do // ���ַ���һ���� #0�������Ѿ���ȥ����
    Inc(I);

  if I >= InDataLen then
    Exit;

  Start := 0;
  case Ord(P[I]) of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        // �� P[I + 1] ��ʼѰ�ҷ� 00 ����
        J := I + 1;
        while J < InDataLen do
        begin
          if P[J] <> #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF,
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        // �� P[I + 1] ��ʼѰ�ҵ���һ�� 00 ��ı���
        J := I + 1;
        while J < InDataLen do
        begin
          if P[J] = #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;

        if Start <> 0 then
          Inc(Start);
      end;
  end;

  if Start > 0 then
  begin
    Move(P[Start], OutBuf^, InDataLen - Start);
    OutLen := InDataLen - Start;
    Result := True;
  end;
end;

// ��һƬ�ڴ�������� RSA �ӽ��ܼ������չ�ֵ� Padding ��ʽ���ԭʼ����
function RSADecryptPadding(BlockSize: Integer; EnData: Pointer; DataLen: Integer;
  OutBuf: Pointer; out OutLen: Integer; Exponent, Product: TCnBigNumber): Boolean;
var
  Stream: TMemoryStream;
  Res, Data: TCnBigNumber;
  ResBuf: array of Byte;
begin
  Result := False;
  Res := nil;
  Data := nil;
  Stream := nil;
  try
    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnData), DataLen);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    SetLength(ResBuf, Res.GetBytesCount);
    Res.ToBinary(PAnsiChar(@ResBuf[0]));

    Result := PKCS1RemovePadding(@ResBuf[0], Length(ResBuf), OutBuf, OutLen);
  finally
    Stream.Free;
    Res.Free;
    Data.Free;
  end;
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSADecryptPadding(PublicKey.GetBitsCount div 8, EnData, DataLen,
    OutBuf, OutLen, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSADecryptPadding(PrivateKey.GetBitsCount div 8, EnData, DataLen,
    OutBuf, OutLen, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Stream: TMemoryStream;
  Res: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PublicKey.GetBitsCount div 8 then
      Exit;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PublicKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PrivateKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PrivateKey.GetBitsCount div 8 then
      Exit;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PrivateKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

// RSA �ļ�ǩ������֤ʵ��

// ����ָ������ժҪ�㷨����ָ�����Ķ�����ɢ��ֵ��д�� Stream
function CalcDigestStream(InStream: TStream; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
begin
  Result := False;
  case SignType of
    sdtMD5:
      begin
        Md5 := MD5Stream(InStream);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    sdtSHA1:
      begin
        Sha1 := SHA1Stream(InStream);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    sdtSHA256:
      begin
        Sha256 := SHA256Stream(InStream);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

// ����ָ������ժҪ�㷨�����ļ��Ķ�����ɢ��ֵ��д�� Stream
function CalcDigestFile(const FileName: string; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
begin
  Result := False;
  case SignType of
    sdtMD5:
      begin
        Md5 := MD5File(FileName);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    sdtSHA1:
      begin
        Sha1 := SHA1File(FileName);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    sdtSHA256:
      begin
        Sha256 := SHA256File(FileName);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

function AddDigestTypeOIDNodeToWriter(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
begin
  Result := nil;
  case ASignType of
    sdtMD5:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_MD5[0],
        SizeOf(OID_SIGN_MD5), AParent);
    sdtSHA1:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA1[0],
        SizeOf(OID_SIGN_SHA1), AParent);
    sdtSHA256:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA256[0],
        SizeOf(OID_SIGN_SHA256), AParent);
  end;
end;

{
  ͨ������ժҪ�㷨���������ժҪ�󣬻�Ҫ���� BER ������ PKCS1 Padding
  BER ����ĸ�ʽ���£�
  DigestInfo ::= SEQUENCE )
    digestAlgorithm DigestAlgorithmIdentifier,
    digest Digest )

  DigestAlgorithmIdentifier ::= AlgorithmIdentifier
  Digest ::= OCTET STRING

  Ҳ���ǣ�
  SEQUENCE
    SEQUENCE
      OBJECT IDENTIFIER
      NULL
    OCTET STRING
}

function CnRSASignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: array of Byte;
  Writer: TCnBerWriter;
  Root, Node: TCnBerWriteNode;
begin
  Result := False;
  Stream := nil;
  EnStream := nil;
  BerStream := nil;
  Writer := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;
    EnStream := TMemoryStream.Create;

    if SignType = sdtNone then
    begin
      // ������ժҪ��ֱ�������ݶ���
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        InStream.Memory, InStream.Size, EnStream) then
        Exit;
    end
    else // ������ժҪ
    begin
      if not CalcDigestStream(InStream, SignType, Stream) then // ��������ɢ��ֵ
        Exit;

      BerStream := TMemoryStream.Create;
      Writer := TCnBerWriter.Create;

      // Ȼ�󰴸�ʽ���� BER ����
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
      AddDigestTypeOIDNodeToWriter(Writer, SignType, Node);
      Writer.AddNullNode(Node);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, Stream.Memory, Stream.Size, Root);
      Writer.SaveToStream(BerStream);

      // �ٰ� BER ���������� PKCS1 ������
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        BerStream.Memory, BerStream.Size, EnStream) then
        Exit;
    end;

    // ˽Կ��������
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnStream.Memory), EnStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      // ������˽Կ���ܺ���������ļ�
      Stream.Clear;
      Stream.Write(ResBuf[0], Res.GetBytesCount);
      Stream.SaveToStream(OutSignStream);
      Result := True;
    end;
  finally
    Stream.Free;
    EnStream.Free;
    BerStream.Free;
    Data.Free;
    Res.Free;
    Writer.Free;
    SetLength(ResBuf, 0);
  end;
end;

function CnRSAVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
var
  Stream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf, BerBuf: array of Byte;
  BerLen: Integer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;

    // ��������ǩ�������ȹ�Կ����
    Data := TCnBigNumber.FromBinary(PAnsiChar(InSignStream.Memory), InSignStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      if SignType = sdtNone then
      begin
        // ��ժҪʱ��ֱ�ӱȶԽ���������ԭʼ�ļ�
        Result := InStream.Size = Res.GetBytesCount;
        if Result then
          Result := CompareMem(InStream.Memory, @ResBuf[0], InStream.Size);
      end
      else
      begin
        // �� Res �н�� PKCS1 ���������
        SetLength(BerBuf, Length(ResBuf));
        if not PKCS1RemovePadding(@ResBuf[0], Length(ResBuf), BerBuf, BerLen) then
          Exit;

        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
          Exit;

        // �⿪ Ber ������ı���������㷨����ʹ�� SignType ԭʼֵ
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
          Exit;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = sdtNone then
          Exit;

        if not CalcDigestStream(InStream, SignType, Stream) then // ��������ɢ��ֵ
          Exit;

        // �� Ber �����ɢ��ֵ�Ƚ�
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);
      end;
    end;
  finally
    Stream.Free;
    Reader.Free;
    Data.Free;
    Res.Free;
    SetLength(ResBuf, 0);
    SetLength(BerBuf, 0);
  end;
end;

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: array of Byte;
  Writer: TCnBerWriter;
  Root, Node: TCnBerWriteNode;
begin
  Result := False;
  Stream := nil;
  EnStream := nil;
  BerStream := nil;
  Writer := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;
    EnStream := TMemoryStream.Create;

    if SignType = sdtNone then
    begin
      // ������ժҪ��ֱ�������ݶ���
      Stream.LoadFromFile(InFileName);
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        Stream.Memory, Stream.Size, EnStream) then
        Exit;
    end
    else // ������ժҪ
    begin
      if not CalcDigestFile(InFileName, SignType, Stream) then // �����ļ���ɢ��ֵ
        Exit;

      BerStream := TMemoryStream.Create;
      Writer := TCnBerWriter.Create;

      // Ȼ�󰴸�ʽ���� BER ����
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
      AddDigestTypeOIDNodeToWriter(Writer, SignType, Node);
      Writer.AddNullNode(Node);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, Stream.Memory, Stream.Size, Root);
      Writer.SaveToStream(BerStream);

      // �ٰ� BER ���������� PKCS1 ������
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        BerStream.Memory, BerStream.Size, EnStream) then
        Exit;
    end;

    // ˽Կ��������
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnStream.Memory), EnStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      // ������˽Կ���ܺ���������ļ�
      Stream.Clear;
      Stream.Write(ResBuf[0], Res.GetBytesCount);
      Stream.SaveToFile(OutSignFileName);
      Result := True;
    end;
  finally
    Stream.Free;
    EnStream.Free;
    BerStream.Free;
    Data.Free;
    Res.Free;
    Writer.Free;
    SetLength(ResBuf, 0);
  end;
end;

function CnRSAVerifyFile(const InFileName, InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, Sign: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf, BerBuf: array of Byte;
  BerLen: Integer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  Sign := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;
    Sign := TMemoryStream.Create;

    // ��������ǩ���ļ��ȹ�Կ����
    Sign.LoadFromFile(InSignFileName);
    Data := TCnBigNumber.FromBinary(PAnsiChar(Sign.Memory), Sign.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      if SignType = sdtNone then
      begin
        Stream.LoadFromFile(InFileName); // ��ժҪʱ��ֱ�ӱȶԽ���������ԭʼ�ļ�
        Result := Stream.Size = Res.GetBytesCount;
        if Result then
          Result := CompareMem(Stream.Memory, @ResBuf[0], Stream.Size);
      end
      else
      begin
        // �� Res �н�� PKCS1 ���������
        SetLength(BerBuf, Length(ResBuf));
        if not PKCS1RemovePadding(@ResBuf[0], Length(ResBuf), BerBuf, BerLen) then
          Exit;

        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
          Exit;

        // �⿪ Ber ������ı���������㷨����ʹ�� SignType ԭʼֵ
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
          Exit;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = sdtNone then
          Exit;

        if not CalcDigestFile(InFileName, SignType, Stream) then // �����ļ���ɢ��ֵ
          Exit;

        // �� Ber �����ɢ��ֵ�Ƚ�
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);
      end;
    end;
  finally
    Stream.Free;
    Reader.Free;
    Sign.Free;
    Data.Free;
    Res.Free;
    SetLength(ResBuf, 0);
    SetLength(BerBuf, 0);
  end;
end;

// ���� Diffie-Hellman ��ԿЭ���㷨���������������Сԭ�����漰�����طֽ���˽���
function CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime, MinRoot: TCnBigNumber): Boolean;
var
  I: Integer;
  Num, PrimeSubOne: TCnBigNumber;
  Factors: TCnBigNumberList;
begin
  Result := False;
  if BitsCount <= 16 then
    Exit;

  if not BigNumberGeneratePrimeByBitsCount(Prime, BitsCount) then
    Exit;

  Factors := TCnBigNumberList.Create;
  PrimeSubOne := BigNumberNew;
  Num := BigNumberNew;

  try
    BigNumberCopy(PrimeSubOne, Prime);
    BigNumberSubWord(PrimeSubOne, 1);
    BigNumberFindFactors(PrimeSubOne, Factors);
    Factors.RemoveDuplicated;

    MinRoot.SetZero;
    for I := 2 to MaxInt do // ����̫��Ĵ���
    begin
      Num.SetWord(I);
      if BigNumberCheckPrimitiveRoot(Num, Prime, Factors) then
      begin
        MinRoot.SetWord(I);
        Result := True;
        Exit;
      end;
    end;
  finally
    Factors.Free;
    BigNumberFree(PrimeSubOne);
    BigNumberFree(Num);
  end;
end;

// ��������ѡ�������� PrivateKey ���� Diffie-Hellman ��ԿЭ�̵������Կ
function CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey: TCnBigNumber;
  const OutPublicKey: TCnBigNumber): Boolean;
begin
  // OutPublicKey = (Root ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(OutPublicKey, Root, SelfPrivateKey, Prime);
end;

// ���ݶԷ����͵� Diffie-Hellman ��ԿЭ�̵������Կ�������ɹ��ϵ���Կ
function CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey: TCnBigNumber;
  const SecretKey: TCnBigNumber): Boolean;
begin
  // SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(SecretKey, OtherPublicKey, SelfPrivateKey, Prime);
end;

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
begin
  Result := sdtNone;
  if (OidLen = SizeOf(OID_SIGN_MD5)) and CompareMem(OID, @OID_SIGN_MD5[0], OidLen) then
    Result := sdtMD5
  else if (OidLen = SizeOf(OID_SIGN_SHA1)) and CompareMem(OID, @OID_SIGN_SHA1[0], OidLen) then
    Result := sdtSHA1
  else if (OidLen = SizeOf(OID_SIGN_SHA256)) and CompareMem(OID, @OID_SIGN_SHA256[0], OidLen) then
    Result := sdtSHA256;
end;

function GetDigestNameFromSignDigestType(Digest: TCnRSASignDigestType): string;
begin
  case Digest of
    sdtNone: Result := '<None>';
    sdtMD5: Result := 'MD5';
    sdtSHA1: Result := 'SHA1';
    sdtSHA256: Result := 'SHA256' ;
  else
    Result := '<Unknown>';
  end;
end;

end.
