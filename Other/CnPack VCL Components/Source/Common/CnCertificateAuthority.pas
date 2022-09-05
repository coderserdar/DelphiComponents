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

unit CnCertificateAuthority;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�CA ֤����֤��Ԫ
* ��Ԫ���ߣ���Х
* ��    ע�����ɿͻ��� CSR �ļ���֤��ǩ���������������
*               openssl req -new -key clientkey.pem -out client.csr -config /c/Program\ Files/Git/ssl/openssl.cnf
*               ���� clientkey.pem ��Ԥ�����ɵ� RSA ˽Կ
*           һ����������ǩ���� crt ֤�飺
*               openssl req -new -x509 -keyout ca.key -out ca.crt -config /c/Program\ Files/Git/ssl/openssl.cnf
*           ���������� Key �Դ� Key ���ɵ� CSR �����ļ�������ǩ����
*               openssl x509 -req -days 365 -in client.csr -signkey clientkey.pem -out selfsigned.crt
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2019.05.06 V1.2
*               ֧�� Win32/Win64/MacOS
*           2018.07.22 V1.1
*               �������ǩ��֤��Ĺ���
*           2018.06.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  CnBigNumber, CnRSA, CnBerUtils, CnMD5, CnSHA1, CnSHA2;

const
  CN_CRT_BASIC_VERSION_1      = 0;
  CN_CRT_BASIC_VERSION_2      = 1;
  CN_CRT_BASIC_VERSION_3      = 2;

type
  TCnCASignType = (ctMd5RSA, ctSha1RSA, ctSha256RSA);
  {* ֤��ǩ��ʹ�õ�ɢ��ǩ���㷨��ctSha1RSA ��ʾ�� Sha1 �� RSA}

  TCnCertificateBaseInfo = class(TPersistent)
  {* ����֤���а�������ͨ�ֶ���Ϣ}
  private
    FCountryName: string;
    FOrganizationName: string;
    FEmailAddress: string;
    FLocalityName: string;
    FCommonName: string;
    FOrganizationalUnitName: string;
    FStateOrProvinceName: string;
  public
    procedure Assign(Source: TPersistent); override;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
  published
    property CountryName: string read FCountryName write FCountryName;
    {* ������}
    property StateOrProvinceName: string read FStateOrProvinceName write FStateOrProvinceName;
    {* ������ʡ��}
    property LocalityName: string read FLocalityName write FLocalityName;
    {* �������������}
    property OrganizationName: string read FOrganizationName write FOrganizationName;
    {* ��֯��}
    property OrganizationalUnitName: string read FOrganizationalUnitName write FOrganizationalUnitName;
    {* ��֯��λ��}
    property CommonName: string read FCommonName write FCommonName;
    {* ����}
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    {* �����ʼ���ַ}
  end;

  // ������֤�����������

  TCnCertificateRequestInfo = class(TCnCertificateBaseInfo);
  {* ֤�������а����Ļ�����Ϣ}

  TCnRSACertificateRequest = class(TObject)
  {* ����֤�������е���Ϣ��������ͨ�ֶΡ���Կ��ժҪ������ǩ����}
  private
    FCertificateRequestInfo: TCnCertificateRequestInfo;
    FPublicKey: TCnRSAPublicKey;
    FCASignType: TCnCASignType;
    FSignValue: Pointer;
    FSignLength: Integer;
    FDigestLength: Integer;
    FDigestValue: Pointer;
    FDigestType: TCnRSASignDigestType;
    procedure SetCertificateRequestInfo(const Value: TCnCertificateRequestInfo);
    procedure SetPublicKey(const Value: TCnRSAPublicKey); // ǩ�� Length Ϊ Key �� Bit ���� 2048 Bit��
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property CertificateRequestInfo: TCnCertificateRequestInfo
      read FCertificateRequestInfo write SetCertificateRequestInfo;
    {* ֤�� DN ��Ϣ}
    property PublicKey: TCnRSAPublicKey read FPublicKey write SetPublicKey;
    {* �ͻ��˹�Կ}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* �ͻ���ʹ�õ�ɢ����ǩ���㷨}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* ɢ�к�ǩ���Ľ��}
    property SignLength: Integer read FSignLength write FSignLength;
    {* ɢ�к�ǩ���Ľ������}
    property DigestType: TCnRSASignDigestType read FDigestType write FDigestType;
    {* �ͻ���ɢ��ʹ�õ�ɢ���㷨��Ӧ�� CASignType �������}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* ɢ��ֵ���м�������ֱ�Ӵ洢�� CSR �ļ���}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* ɢ��ֵ�ĳ���}
  end;

  // ������֤�������������������֤����֤������

{
   Name ::= CHOICE
     rdnSequence  RDNSequence

   RDNSequence ::= SEQUENCE OF RelativeDistinguishedName

   RelativeDistinguishedName ::=
     SET SIZE (1..MAX) OF AttributeTypeAndValue

   AttributeTypeAndValue ::= SEQUENCE
     type     AttributeType,
     value    AttributeValue

   AttributeType ::= OBJECT IDENTIFIER

   AttributeValue ::= ANY -- DEFINED BY AttributeType

   DirectoryString ::= CHOICE
         teletexString           TeletexString (SIZE (1..MAX)),
         printableString         PrintableString (SIZE (1..MAX)),
         universalString         UniversalString (SIZE (1..MAX)),
         utf8String              UTF8String (SIZE (1..MAX)),
         bmpString               BMPString (SIZE (1..MAX))
}

  TCnCertificateNameInfo = class(TCnCertificateBaseInfo)
  {* ���� Subject �� Issuer �Ļ�����Ϣ������}
  private
    FSurName: string;
    FTitle: string;
    FGivenName: string;
    FInitials: string;
    FSerialNumber: string;
    FPseudonym: string;
    FGenerationQualifier: string;
  public
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    property Title: string read FTitle write FTitle;
    property SurName: string read FSurName write FSurName;
    property GivenName: string read FGivenName write FGivenName;
    property Initials: string read FInitials write FInitials;
    property Pseudonym: string read FPseudonym write FPseudonym;
    property GenerationQualifier: string read FGenerationQualifier write FGenerationQualifier;
  end;

  TCnCertificateSubjectInfo = class(TCnCertificateNameInfo);
  {* ֤�������а����ı�ǩ���ߵĻ�����Ϣ��Ҳ������� Name}

  TCnCertificateIssuerInfo = class(TCnCertificateNameInfo);
  {* ֤�������а�����ǩ���ߵĻ�����Ϣ��Ҳ������� Name}

  TCnUTCTime = class(TObject)
  {* ֤���д������ʱ��Ľ�����}
  private
    FUTCTimeString: string;
    FDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetUTCTimeString(const Value: string);
  public
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property UTCTimeString: string read FUTCTimeString write SetUTCTimeString;
  end;

{
   Extension  ::=  SEQUENCE
        extnID      OBJECT IDENTIFIER,
        critical    BOOLEAN DEFAULT FALSE,
        extnValue   OCTET STRING
                    -- contains the DER encoding of an ASN.1 value
                    -- corresponding to the extension type identified
                    -- by extnID
}

  TCnCerKeyUsage = (kuDigitalSignature, kuContentCommitment, kuKeyEncipherment,
    kuDataEncipherment, kuKeyAgreement, kuKeyCertSign, kuCRLSign, kuEncipherOnly,
    kuDecipherOnly);
  TCnCerKeyUsages = set of TCnCerKeyUsage;

  TCnExtendedKeyUsage = (ekuServerAuth, ekuClientAuth, ekuCodeSigning, ekuEmailProtection,
    ekuTimeStamping, ekuOCSPSigning);
  TCnExtendedKeyUsages = set of TCnExtendedKeyUsage;

{
  ��׼��չ�����������ݣ�
  // Authority Key Identifier       ǩ������Կ��ʶ�� array of Byte
  // Subject Key Identifier         ��ǩ���߹�Կ��� array of Byte
  // Key Usage                      ��Կ�÷����� TCnCerKeyUsages
  // Certificate Policies
  // Policy Mappings
  // Subject Alternative Name       ��ǩ���ߵ�������ƣ��ַ����б�
  // Issuer Alternative Name        ǩ���ߵ�������ƣ��ַ����б�
  // Subject Directory Attributes
  // Basic Constraints              �������ƣ��Ƿ� CA �Լ�Ƕ�ײ���
  // Name Constraints
  // Policy Constraints
  // Extended Key Usage             ��ǿ����Կ�÷�����
  // CRL Distribution Points        CRL ���� URL���ַ����б�
  // Inhibit anyPolicy
  // Freshest CRL (a.k.a. Delta CRL Distribution Point)
}
  TCnCertificateStandardExtensions = class(TObject)
  {* ֤���׼��չ����}
  private
    FKeyUsage: TCnCerKeyUsages;
    FSubjectAltName: TStrings;
    FIssuerAltName: TStrings;
    FAuthorityKeyIdentifier: AnsiString;
    FSubjectKeyIdentifier: AnsiString;
    FCRLDistributionPoints: TStrings;
    FExtendedKeyUsage: TCnExtendedKeyUsages;
    FBasicConstraintsCA: Boolean;
    FBasicConstraintsPathLen: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property KeyUsage: TCnCerKeyUsages read FKeyUsage write FKeyUsage;
    property ExtendedKeyUsage: TCnExtendedKeyUsages read FExtendedKeyUsage write FExtendedKeyUsage;
    property BasicConstraintsCA: Boolean read FBasicConstraintsCA write FBasicConstraintsCA;
    property BasicConstraintsPathLen: Integer read FBasicConstraintsPathLen write FBasicConstraintsPathLen;
    property SubjectAltName: TStrings read FSubjectAltName;
    property IssuerAltName: TStrings read FIssuerAltName;
    property CRLDistributionPoints: TStrings read FCRLDistributionPoints;
    property AuthorityKeyIdentifier: AnsiString read FAuthorityKeyIdentifier write FAuthorityKeyIdentifier;
    property SubjectKeyIdentifier: AnsiString read FSubjectKeyIdentifier write FSubjectKeyIdentifier;
  end;

{
  ˽�л�������չ�����������ݣ�
  // Authority Information Access   ǩ���ߵ���Ϣ������ ocsp �� caIssuers �� URL
  // Subject Information Access     ûɶ����
}

  TCnCertificatePrivateInternetExtensions = class(TObject)
  private
    FAuthorityInformationAccessCaIssuers: string;
    FAuthorityInformationAccessOcsp: string;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property AuthorityInformationAccessOcsp: string read FAuthorityInformationAccessOcsp
      write FAuthorityInformationAccessOcsp;
    {* �ϼ�ǩ��֤�� Ocsp �� URL}
    property AuthorityInformationAccessCaIssuers: string read FAuthorityInformationAccessCaIssuers
      write FAuthorityInformationAccessCaIssuers;
    {* �ϼ�ǩ������֤����� URL}
  end;

{
  TBSCertificate  ::=  SEQUENCE
    version         [0]  EXPLICIT Version DEFAULT v1,
    serialNumber         CertificateSerialNumber,
    signature            AlgorithmIdentifier,
    issuer               Name,
    validity             Validity,
    subject              Name,
    subjectPublicKeyInfo SubjectPublicKeyInfo,
    issuerUniqueID  [1]  IMPLICIT UniqueIdentifier OPTIONAL,
                         -- If present, version MUST be v2 or v3
    subjectUniqueID [2]  IMPLICIT UniqueIdentifier OPTIONAL,
                         -- If present, version MUST be v2 or v3
    extensions      [3]  EXPLICIT Extensions OPTIONAL
                         -- If present, version MUST be v3
}

  TCnRSABasicCertificate = class(TObject)
  {* ֤���еĻ�����Ϣ��}
  private
    FSerialNumber: string;
    FNotAfter: TCnUTCTime;
    FNotBefore: TCnUTCTime;
    FVersion: Integer;
    FSubject: TCnCertificateSubjectInfo;
    FSubjectUniqueID: string;
    FIssuer: TCnCertificateIssuerInfo;
    FIssuerUniqueID: string;
    FSubjectPublicKey: TCnRSAPublicKey;
    FCASignType: TCnCASignType;
    FPrivateInternetExtension: TCnCertificatePrivateInternetExtensions;
    FStandardExtension: TCnCertificateStandardExtensions;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Version: Integer read FVersion write FVersion;
    {* �汾�ţ�ֵ 0��1��2 ��ʾ�汾��Ϊ v1��v2��v3��Ĭ�� v1 ʱ��ʡ��
      �� extensions ʱ������ v3���� extensions ���� UniqueIdentifier ʱ v2
      �������ɰ汾 v3 ��}
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    {* ���кţ�����Ӧ�������ͣ��������ַ�������}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* �ͻ���ʹ�õ�ɢ����ǩ���㷨��Ӧ����֤�����ı���һֱ}
    property Subject: TCnCertificateSubjectInfo read FSubject write FSubject;
    {* ��ǩ���ߵĻ�����Ϣ}
    property SubjectPublicKey: TCnRSAPublicKey read FSubjectPublicKey write FSubjectPublicKey;
    {* ��ǩ���ߵĹ�Կ}
    property SubjectUniqueID: string read FSubjectUniqueID write FSubjectUniqueID;
    {* v2 ʱ��ǩ���ߵ�Ψһ ID}
    property Issuer: TCnCertificateIssuerInfo read FIssuer write FIssuer;
    {* ǩ���ߵĻ�����Ϣ}
    property IssuerUniqueID: string read FIssuerUniqueID write FIssuerUniqueID;
    {* v2 ʱǩ���ߵ�Ψһ ID}
    property NotBefore: TCnUTCTime read FNotBefore;
    {* ��Ч����ʼ}
    property NotAfter: TCnUTCTime read FNotAfter;
    {* ��Ч�ڽ���}

    property StandardExtension: TCnCertificateStandardExtensions read FStandardExtension;
    {* ��׼��չ���󼯺�}
    property PrivateInternetExtension: TCnCertificatePrivateInternetExtensions read FPrivateInternetExtension;
    {* ˽�л�������չ���󼯺�}
  end;

{
  Certificate  ::=  SEQUENCE
    tbsCertificate       TBSCertificate,
    signatureAlgorithm   AlgorithmIdentifier,
    signatureValue       BIT STRING
}

  TCnRSACertificate = class(TObject)
  {* ����һ������֤�飬ע�����в���ǩ���ߵĹ�Կ����Կֻ�б�ǩ���ߵ�}
  private
    FDigestLength: Integer;
    FSignLength: Integer;
    FDigestValue: Pointer;
    FSignValue: Pointer;
    FCASignType: TCnCASignType;
    FDigestType: TCnRSASignDigestType;
    FBasicCertificate: TCnRSABasicCertificate;
    function GetIsSelfSigned: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property IsSelfSigned: Boolean read GetIsSelfSigned;
    {* �Ƿ���ǩ��֤�飬ʹ��ǩ�����뱻ǩ������Ϣ�Ƿ���ͬ���ж�}

    property BasicCertificate: TCnRSABasicCertificate read FBasicCertificate;
    {* ֤�������Ϣ��}
    property CASignType: TCnCASignType read FCASignType write FCASignType;
    {* �ͻ���ʹ�õ�ɢ����ǩ���㷨}
    property SignValue: Pointer read FSignValue write FSignValue;
    {* ɢ�к�ǩ���Ľ��}
    property SignLength: Integer read FSignLength write FSignLength;
    {* ɢ�к�ǩ���Ľ������}
    property DigestType: TCnRSASignDigestType read FDigestType write FDigestType;
    {* �ͻ���ɢ��ʹ�õ�ɢ���㷨��Ӧ�� CASignType �������}
    property DigestValue: Pointer read FDigestValue write FDigestValue;
    {* ɢ��ֵ���м�������ֱ�Ӵ洢�� CRT �ļ���}
    property DigestLength: Integer read FDigestLength write FDigestLength;
    {* ɢ��ֵ�ĳ���}
  end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ��ɢ���㷨���� CSR ��ʽ��֤�������ļ�}

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore, NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* ���ݹ�˽Կ��һЩ DN ��Ϣ�Լ�ָ��ɢ���㷨���� CRT ��ʽ����ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ}

function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* ���� PEM ��ʽ�� CSR �ļ��������ݷ��� TCnRSACertificateRequest ������}

function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
{* ���� PEM ��ʽ�� CSR �ļ��������ݷ��� TCnRSACertificateRequest ������}

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
{* ��֤һ CSR �ļ��������Ƿ�Ϻ�ǩ��}

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
{* ��֤һ CSR ���������Ƿ�Ϻ�ǩ��}

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
{* ��֤һ��ǩ���� CRT �ļ��������Ƿ�Ϻ�ǩ��}

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
{* ��֤һ��ǩ���� CRT ���������Ƿ�Ϻ�ǩ��}

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnRSACertificate): Boolean;
{* ���� PEM ��ʽ�� CRT ֤���ļ��������ݷ��� TCnRSACertificate ��}

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnRSACertificate): Boolean;
{* ���� PEM ��ʽ�� CRT ֤�����������ݷ��� TCnRSACertificate ��}

function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean;
{* �� CRT ֤���������Ӧ˽Կǩ��֤���������ɱ�ǩ��֤�飬Ŀǰֻ֧�� v1 ��ʽ}

// ������������

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* ��һ��ɢ���㷨�� OID д��һ�� Ber �ڵ�}

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
{* ��֤���ǩ��ɢ���㷨ö��ֵ��ȡ������}

implementation

const
  // PKCS#10
  PEM_CERTIFICATE_REQUEST_HEAD = '-----BEGIN CERTIFICATE REQUEST-----';
  PEM_CERTIFICATE_REQUEST_TAIL = '-----END CERTIFICATE REQUEST-----';
  PEM_CERTIFICATE_HEAD = '-----BEGIN CERTIFICATE-----';
  PEM_CERTIFICATE_TAIL = '-----END CERTIFICATE-----';

  OID_DN_COUNTRYNAME             : array[0..2] of Byte = ($55, $04, $06); // 2.5.4.6
  OID_DN_STATEORPROVINCENAME     : array[0..2] of Byte = ($55, $04, $08); // 2.5.4.8
  OID_DN_LOCALITYNAME            : array[0..2] of Byte = ($55, $04, $07); // 2.5.4.7
  OID_DN_ORGANIZATIONNAME        : array[0..2] of Byte = ($55, $04, $0A); // 2.5.4.10
  OID_DN_ORGANIZATIONALUNITNAME  : array[0..2] of Byte = ($55, $04, $0B); // 2.5.4.11
  OID_DN_COMMONNAME              : array[0..2] of Byte = ($55, $04, $03); // 2.5.4.3
  OID_DN_EMAILADDRESS            : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $09, $01
  ); // 1.2.840.113549.1.9.1

  // ��չ�ֶ��ǵ� OID
  OID_EXT_SUBJECTKEYIDENTIFIER   : array[0..2] of Byte = ($55, $1D, $0E); // 2.5.29.14
  OID_EXT_KEYUSAGE               : array[0..2] of Byte = ($55, $1D, $0F); // 2.5.29.15
  OID_EXT_SUBJECTALTNAME         : array[0..2] of Byte = ($55, $1D, $11); // 2.5.29.17
  OID_EXT_ISSUERTALTNAME         : array[0..2] of Byte = ($55, $1D, $12); // 2.5.29.18
  OID_EXT_BASICCONSTRAINTS       : array[0..2] of Byte = ($55, $1D, $13); // 2.5.29.19
  OID_EXT_CRLDISTRIBUTIONPOINTS  : array[0..2] of Byte = ($55, $1D, $1F); // 2.5.29.31
  OID_EXT_CERTIFICATEPOLICIES    : array[0..2] of Byte = ($55, $1D, $20); // 2.5.29.32
  OID_EXT_AUTHORITYKEYIDENTIFIER : array[0..2] of Byte = ($55, $1D, $23); // 2.5.29.35
  OID_EXT_EXTKEYUSAGE            : array[0..2] of Byte = ($55, $1D, $25); // 2.5.29.37
  OID_EXT_AUTHORITYINFOACCESS    : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $01, $01
  ); // 1.3.6.1.5.5.7.1.1
  OID_EXT_AUTHORITYINFOACCESS_OCSP         : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $01
  ); // 1.3.6.1.5.5.7.48.1
  OID_EXT_AUTHORITYINFOACCESS_CAISSUERS    : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $02
  ); // 1.3.6.1.5.5.7.48.2

  // authorityInfoAccess Subs
  OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $01
  ); // 1.3.6.1.5.5.7.48.1
  OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $30, $02
  ); // 1.3.6.1.5.5.7.48.2

  // Extended Key Usages
  OID_EXT_EXT_KEYUSAGE_SERVERAUTH  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $01
  ); // 1.3.6.1.5.5.7.3.1
  OID_EXT_EXT_KEYUSAGE_CLIENTAUTH  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $02
  ); // 1.3.6.1.5.5.7.3.2
  OID_EXT_EXT_KEYUSAGE_CODESIGNING : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $03
  ); // 1.3.6.1.5.5.7.3.3
  OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $04
  ); // 1.3.6.1.5.5.7.3.4
  OID_EXT_EXT_KEYUSAGE_TIMESTAMPING : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $08
  ); // 1.3.6.1.5.5.7.3.8
  OID_EXT_EXT_KEYUSAGE_OCSPSIGNING  : array[0..7] of Byte = (
    $2B, $06, $01, $05, $05, $07, $03, $09
  ); // 1.3.6.1.5.5.7.3.9

  OID_SHA1_RSAENCRYPTION          : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $05
  ); // 1.2.840.113549.1.1.5
  OID_SHA256_RSAENCRYPTION        : array[0..8] of Byte = (
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $0B
  ); // 1.2.840.113549.1.1.11

  SCRLF = #13#10;

  // ���ڽ����ַ������ݵĳ���
  SDN_COUNTRYNAME                = 'CountryName';
  SDN_STATEORPROVINCENAME        = 'StateOrProvinceName';
  SDN_LOCALITYNAME               = 'LocalityName';
  SDN_ORGANIZATIONNAME           = 'OrganizationName';
  SDN_ORGANIZATIONALUNITNAME     = 'OrganizationalUnitName';
  SDN_COMMONNAME                 = 'CommonName';
  SDN_EMAILADDRESS               = 'EmailAddress';

var
  DummyPointer: Pointer;
  DummyInteger: Integer;
//  DummyCASignType: TCnCASignType;
  DummyDigestType: TCnRSASignDigestType;

function PrintHex(const Buf: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  if Len <= 0 then
    Exit;

  P := PByteArray(Buf);
  if P = nil then
    Exit;

  for I := 0 to Len - 1 do
  begin
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P[I] shr 4) and $0F] +
              Digits[P[I] and $0F]);
  end;
end;

function AddCASignTypeOIDNodeToWriter(AWriter: TCnBerWriter; CASignType: TCnCASignType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
begin
  Result := nil;
  case CASignType of
    ctSha1RSA:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA1_RSAENCRYPTION[0],
        SizeOf(OID_SHA1_RSAENCRYPTION), AParent);
    ctSha256RSA:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SHA256_RSAENCRYPTION[0],
        SizeOf(OID_SHA256_RSAENCRYPTION), AParent);
    // TODO: �����㷨����֧��
  end;
end;

// ����ָ������ժҪ�㷨�������ݵĶ�����ɢ��ֵ��д�� Stream��Buffer ��ָ��
function CalcDigestData(const Buffer; Count: Integer; CASignType: TCnCASignType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
begin
  Result := False;
  case CASignType of
    ctMd5RSA:
      begin
        Md5 := MD5Buffer(Buffer, Count);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    ctSha1RSA:
      begin
        Sha1 := SHA1Buffer(Buffer, Count);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    ctSha256RSA:
      begin
        Sha256 := SHA256Buffer(Buffer, Count);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

function GetRSASignTypeFromCASignType(CASignType: TCnCASignType): TCnRSASignDigestType;
begin
  Result := sdtSHA1;
  case CASignType of
    ctMd5RSA:
      Result := sdtMD5;
    ctSha1RSA:
      Result := sdtSHA1;
    ctSha256RSA:
      Result := sdtSHA256;
  end;
end;

{ д���¸�ʽ�Ĺ�Կ�ڵ�
  SEQUENCE(2 elem)    - PubNode
    SEQUENCE(2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING(1 elem)
      SEQUENCE(2 elem)
        INTEGER
        INTEGER
}
procedure WritePublicKeyToNode(AWriter: TCnBerWriter; PubNode: TCnBerWriteNode;
  PublicKey: TCnRSAPublicKey);
var
  Node: TCnBerWriteNode;
begin
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, PubNode);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
    SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
  AWriter.AddNullNode(Node);
  Node := AWriter.AddContainerNode(CN_BER_TAG_BIT_STRING, PubNode);
  Node := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyProduct, Node);
  AddBigNumberToWriter(AWriter, PublicKey.PubKeyExponent, Node);
end;

function GenerateSignatureNode(AWriter: TCnBerWriter; Root, NodeToSign: TCnBerWriteNode;
  PrivateKey: TCnRSAPrivateKey; CASignType: TCnCASignType): Boolean;
var
  ValueStream, DigestStream: TMemoryStream;
  HashWriter: TCnBerWriter;
  HashRoot, HashNode, Node: TCnBerWriteNode;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  ValueStream := nil;
  DigestStream := nil;
  HashWriter := nil;

  try
    // �ó� InfoRoot ������
    ValueStream := TMemoryStream.Create;
    NodeToSign.SaveToStream(ValueStream);

    // ������ Hash
    DigestStream := TMemoryStream.Create;
    CalcDigestData(ValueStream.Memory, ValueStream.Size, CASignType, DigestStream);

    // �� Hash ����ǩ���㷨ƴ�� BER ����
    HashWriter := TCnBerWriter.Create;
    HashRoot := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE);
    Node := HashWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, HashRoot);
    AddDigestTypeOIDNodeToWriter(HashWriter, GetRSASignTypeFromCASignType(CASignType), Node);
    HashWriter.AddNullNode(Node);
    HashWriter.AddBasicNode(CN_BER_TAG_OCTET_STRING, DigestStream, HashRoot);

    // ���ô� Stream���������ɵ� BER ��ʽ����
    DigestStream.Clear;
    HashWriter.SaveToStream(DigestStream);

    // RSA ˽Կ���ܴ� BER ��õ�ǩ��ֵ������ǰ��Ҫ PKCS1 ����
    SetLength(OutBuf, PrivateKey.BitsCount div 8);
    OutLen := PrivateKey.BitsCount div 8;
    if not CnRSAEncryptData(DigestStream.Memory, DigestStream.Size,
      @OutBuf[0], PrivateKey) then
      Exit;

    // ���� Hash �㷨˵��
    HashNode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
    AddCASignTypeOIDNodeToWriter(AWriter, CASignType, HashNode);
    AWriter.AddNullNode(HashNode);

    // д������ǩ��ֵ
    AWriter.AddBasicNode(CN_BER_TAG_BIT_STRING, @OutBuf[0], OutLen, Root);
    Result := True;
  finally
    HashWriter.Free;
    DigestStream.Free;
    ValueStream.Free;
  end;
end;

function CnCANewCertificateSignRequest(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCSRFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; CASignType: TCnCASignType): Boolean;
var
  B: Byte;
  OutBuf: array of Byte;
  Writer, HashWriter: TCnBerWriter;
  Stream, DigestStream, ValueStream: TMemoryStream;
  Root, DNRoot, InfoRoot, PubNode: TCnBerWriteNode;

  procedure WriteDNNameToNode(AWriter: TCnBerWriter; DNOID: Pointer; DNOIDLen: Integer;
    const DN: string; SuperParent: TCnBerWriteNode; ATag: Integer = CN_BER_TAG_PRINTABLESTRING);
  var
    ANode: TCnBerWriteNode;
    AnsiDN: AnsiString;
  begin
    // Superparent �� DNRoot�������� Set���� Sequence��Sequence ��� OID �� PrintableString
    ANode := AWriter.AddContainerNode(CN_BER_TAG_SET, SuperParent);
    ANode := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, ANode);
    AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, PByte(DNOID), DNOIDLen, ANode);
    AnsiDN := AnsiString(DN);
    AWriter.AddBasicNode(ATag, @AnsiDN[1], Length(AnsiDN), ANode);
  end;

begin
  Result := False;

  if (PrivateKey = nil) or (PublicKey = nil) or (OutCSRFile = '') then
    Exit;

  if (Length(CountryName) <> 2) or (StateOrProvinceName = '') or (LocalityName = '')
    or (OrganizationName = '') or (OrganizationalUnitName = '') or (CommonName = '')
    or (EmailAddress = '') then
    Exit;

  B := 0;
  Writer := nil;
  HashWriter := nil;
  Stream := nil;
  DigestStream := nil;
  ValueStream := nil;
  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    InfoRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // �� Info дһ��ֱ���ӽڵ�
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, InfoRoot);          // �汾
    DNRoot := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot);  // DN
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, InfoRoot); // ��Կ
    Writer.AddRawNode($A0, @B, 1, InfoRoot);                           // ������

    // д DN �ڵ������
    WriteDNNameToNode(Writer, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName, DNRoot);
    WriteDNNameToNode(Writer, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, DNRoot, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ������
    WritePublicKeyToNode(Writer, PubNode, PublicKey);

    // ���� InfoRoot �������ժҪ��ǩ��
    GenerateSignatureNode(Writer, Root, InfoRoot, PrivateKey, CASignType);

    // ����
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCSRFile, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, Stream);
  finally
    Writer.Free;
    HashWriter.Free;
    Stream.Free;
    ValueStream.Free;
    DigestStream.Free;
    SetLength(OutBuf, 0);
  end;
end;

procedure ExtractDNValuesToList(DNRoot: TCnBerReadNode; List: TStringList);
var
  I: Integer;
  Node, StrNode: TCnBerReadNode;
begin
  if (DNRoot = nil) or (List = nil) then
    Exit;

  List.Clear;

  // ѭ������ DN ��
  for I := 0 to DNRoot.Count - 1 do
  begin
    Node := DNRoot.Items[I]; // Set
    if (Node.BerTag = CN_BER_TAG_SET) and (Node.Count = 1) then
    begin
      Node := Node.Items[0]; // Sequence
      if (Node.BerTag = CN_BER_TAG_SEQUENCE) and (Node.Count = 2) then
      begin
        StrNode := Node.Items[1];
        Node := Node.Items[0];
        if Node.BerTag = CN_BER_TAG_OBJECT_IDENTIFIER then
        begin
          if CompareObjectIdentifier(Node, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME)) then
            List.Values[SDN_COUNTRYNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME)) then
            List.Values[SDN_STATEORPROVINCENAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME)) then
            List.Values[SDN_LOCALITYNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME)) then
            List.Values[SDN_ORGANIZATIONNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME)) then
            List.Values[SDN_ORGANIZATIONALUNITNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME)) then
            List.Values[SDN_COMMONNAME] := StrNode.AsString
          else if CompareObjectIdentifier(Node, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS)) then
            List.Values[SDN_EMAILADDRESS] := StrNode.AsString  // Email is not PrintableString
        end;
      end;
    end;
  end;
end;

function ExtractCASignType(ObjectIdentifierNode: TCnBerReadNode): TCnCASignType;
begin
  Result := ctSha256RSA; // Default
  if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA1_RSAENCRYPTION[0],
    SizeOf(OID_SHA1_RSAENCRYPTION)) then
    Result := ctSha1RSA
  else if CompareObjectIdentifier(ObjectIdentifierNode, @OID_SHA256_RSAENCRYPTION[0],
    SizeOf(OID_SHA256_RSAENCRYPTION)) then
    Result := ctSha256RSA;
end;

// �����½ṹ�н����Կ
{
BIT STRING -- PubNode
  SEQUENCE
    INTEGER
    INTEGER 65537
}
function ExtractPublicKey(PubNode: TCnBerReadNode; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := False;
  if (PubNode.Count = 1) and (PubNode.Items[0].Count = 2) then
  begin
    PubNode := PubNode.Items[0]; // Sequence
    PublicKey.PubKeyProduct.SetBinary(PAnsiChar(
      PubNode.Items[0].BerDataAddress), PubNode.Items[0].BerDataLength);
    PublicKey.PubKeyExponent.SetBinary(PAnsiChar(
      PubNode.Items[1].BerDataAddress), PubNode.Items[1].BerDataLength);
    Result := True;
  end;
end;

// ����֪��Կ�����������½ṹ���ó�ǩ��ֵ���ܲ�ȥ�� PKCS1 �����õ�ժҪֵ
// ����޹�Կ����ֻȡǩ��ֵ�����⿪
{
  SEQUENCE
    OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
    NULL
  BIT STRING
}
function ExtractSignaturesByPublicKey(PublicKey: TCnRSAPublicKey;
  HashNode, SignNode: TCnBerReadNode; out CASignType: TCnCASignType;
  out DigestType: TCnRSASignDigestType; out SignValue, DigestValue: Pointer;
  out SignLength, DigestLength: Integer): Boolean;
var
  P: Pointer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  Result := False;

  // �ҵ�ǩ���㷨
  if HashNode.Count = 2 then
    CASignType := ExtractCASignType(HashNode.Items[0]);

  // ����ǩ�����ݣ����� BIT String ��ǰ������ 0
  FreeMemory(SignValue);
  SignLength := SignNode.BerDataLength - 1;
  SignValue := GetMemory(SignLength);
  P := Pointer(Integer(SignNode.BerDataAddress) + 1);
  Move(P^, SignValue^, SignLength);

  // �޹�Կʱ�����ܣ�ֻ��
  if PublicKey = nil then
  begin
    Result := True;
    Exit;
  end;

  // �⿪ RSA ǩ����ȥ�� PKCS1 ��������ݵõ� DER ����� Hash ֵ���㷨
  SetLength(OutBuf, PublicKey.BitsCount div 8);
  Reader := nil;

  try
    if CnRSADecryptData(SignValue, SignLength, @OutBuf[0], OutLen, PublicKey) then
    begin
      Reader := TCnBerReader.Create(@OutBuf[0], OutLen);
      Reader.ParseToTree;

      if Reader.TotalCount < 5 then
        Exit;

      Node := Reader.Items[2];
      DigestType := GetDigestSignTypeFromBerOID(Node.BerDataAddress,
        Node.BerDataLength);
      if DigestType = sdtNone then
        Exit;

      // ��ȡ Ber �����ɢ��ֵ
      Node := Reader.Items[4];
      FreeMemory(DigestValue);
      DigestLength := Node.BerDataLength;
      DigestValue := GetMemory(DigestLength);
      Move(Node.BerDataAddress^, DigestValue^, DigestLength);

      Result := True;
    end;
  finally
    SetLength(OutBuf, 0);
    Reader.Free;
  end;
end;

function ExtractExtensions(Root: TCnBerReadNode; StandardExt: TCnCertificateStandardExtensions;
  PrivateInternetExt: TCnCertificatePrivateInternetExtensions): Boolean;
var
  I, J: Integer;
  ExtNode, OidNode, ValueNode: TCnBerReadNode;
  Buf: array of Byte;
  KU: TCnCerKeyUsages;
begin
  Result := False;
  if (Root = nil) or (Root.Count < 1) then
    Exit;

  for I := 0 to Root.Count - 1 do
  begin
    ExtNode := Root.Items[I];
    if ExtNode.Count > 0 then
    begin
      OidNode := ExtNode.Items[0];
      ValueNode := nil;
      if ExtNode.Count > 1 then
      begin
        if (ExtNode.Items[1].BerTag = CN_BER_TAG_BOOLEAN) and (ExtNode.Count > 2) then
          ValueNode := ExtNode.Items[2] // Critical���ݲ�����
        else
          ValueNode := ExtNode.Items[1];
      end;

      if ValueNode = nil then
        Continue;
      if (ValueNode.BerTag <> CN_BER_TAG_OCTET_STRING) or (ValueNode.Count <> 1) then
        Continue;

      ValueNode := ValueNode.Items[0]; // ָ�� OctetString ���ӽڵ㣬Value ����
      if CompareObjectIdentifier(OidNode, @OID_EXT_SUBJECTKEYIDENTIFIER, SizeOf(OID_EXT_SUBJECTKEYIDENTIFIER)) then
      begin
        StandardExt.SubjectKeyIdentifier := ValueNode.AsRawString;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_KEYUSAGE, SizeOf(OID_EXT_KEYUSAGE)) then
      begin
        if ValueNode.BerTag = CN_BER_TAG_BIT_STRING then
        begin
          SetLength(Buf, ValueNode.BerDataLength);
          if Length(Buf) >= 2 then
          begin
            ValueNode.CopyDataTo(@Buf[0]);
            // Buf[1] Ҫ shr Buf[0] λ
            Buf[1] := Buf[1] shr Buf[0];
            Move(Buf[0], KU, 1);
            StandardExt.KeyUsage := KU;
          end;
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_SUBJECTALTNAME, SizeOf(OID_EXT_SUBJECTALTNAME)) then
      begin
        StandardExt.SubjectAltName.Clear;
        for J := 0 to ValueNode.Count - 1 do
          StandardExt.SubjectAltName.Add(ValueNode[J].AsRawString);
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_ISSUERTALTNAME, SizeOf(OID_EXT_ISSUERTALTNAME)) then
      begin
        StandardExt.IssuerAltName.Clear;
        for J := 0 to ValueNode.Count - 1 do
          StandardExt.IssuerAltName.Add(ValueNode[J].AsRawString);
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_BASICCONSTRAINTS, SizeOf(OID_EXT_BASICCONSTRAINTS)) then
      begin
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].BerTag = CN_BER_TAG_BOOLEAN then
            StandardExt.BasicConstraintsCA := ValueNode[J].AsBoolean
          else if ValueNode[J].BerTag = CN_BER_TAG_INTEGER then
            StandardExt.BasicConstraintsPathLen := ValueNode[J].AsInteger;
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_CRLDISTRIBUTIONPOINTS, SizeOf(OID_EXT_CRLDISTRIBUTIONPOINTS)) then
      begin
        StandardExt.CRLDistributionPoints.Clear;
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].Count = 1 then
            if ValueNode[J][0].Count = 1 then
              if ValueNode[J][0][0].Count = 1 then
                StandardExt.CRLDistributionPoints.Add(ValueNode[J][0][0][0].AsRawString);
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_CERTIFICATEPOLICIES, SizeOf(OID_EXT_CERTIFICATEPOLICIES)) then
      begin
        // TODO: �������ӵ� CERTIFICATEPOLICIES
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYKEYIDENTIFIER, SizeOf(OID_EXT_AUTHORITYKEYIDENTIFIER)) then
      begin
        if ValueNode.Count = 1 then
          StandardExt.AuthorityKeyIdentifier := ValueNode[0].AsRawString;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_EXTKEYUSAGE, SizeOf(OID_EXT_EXTKEYUSAGE)) then
      begin
        StandardExt.ExtendedKeyUsage := [];
        for J := 0 to ValueNode.Count - 1 do
        begin
          if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_SERVERAUTH[0], SizeOf(OID_EXT_EXT_KEYUSAGE_SERVERAUTH)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuServerAuth]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_CLIENTAUTH[0], SizeOf(OID_EXT_EXT_KEYUSAGE_CLIENTAUTH)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuClientAuth]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_CODESIGNING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_CODESIGNING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuCodeSigning]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION[0], SizeOf(OID_EXT_EXT_KEYUSAGE_EMAILPROTECTION)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuEmailProtection]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_TIMESTAMPING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_TIMESTAMPING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuTimeStamping]
          else if CompareObjectIdentifier(ValueNode[J], @OID_EXT_EXT_KEYUSAGE_OCSPSIGNING[0], SizeOf(OID_EXT_EXT_KEYUSAGE_OCSPSIGNING)) then
            StandardExt.ExtendedKeyUsage := StandardExt.ExtendedKeyUsage + [ekuOCSPSigning];
        end;
      end
      else if CompareObjectIdentifier(OidNode, @OID_EXT_AUTHORITYINFOACCESS, SizeOf(OID_EXT_AUTHORITYINFOACCESS)) then
      begin
        for J := 0 to ValueNode.Count - 1 do
        begin
          if ValueNode[J].Count = 2 then
          begin
            if CompareObjectIdentifier(ValueNode[J].Items[0], @OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP[0], SizeOf(OID_EXT_EXT_AUTHORITYINFOACCESS_OCSP)) then
              PrivateInternetExt.AuthorityInformationAccessOcsp := ValueNode[J].Items[1].AsRawString
            else if CompareObjectIdentifier(ValueNode[J].Items[0], @OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS[0], SizeOf(OID_EXT_EXT_AUTHORITYINFOACCESS_CAISSUERS)) then
              PrivateInternetExt.AuthorityInformationAccessCaIssuers := ValueNode[J].Items[1].AsRawString
          end;
        end;
      end;
    end;
  end;
  SetLength(Buf, 0);
  Result := True;
end;


function CnCALoadCertificateSignRequestFromFile(const FileName: string;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCALoadCertificateSignRequestFromStream(Stream, CertificateRequest);
  finally
    Stream.Free;
  end;
end;

{
  CSR �ļ��Ĵ����ʽ���£�

  SEQUENCE
    SEQUENCE
      INTEGER0
      SEQUENCE
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.6countryName(X.520 DN component)
            PrintableString  CN
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.8stateOrProvinceName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.7localityName(X.520 DN component)
            PrintableString  ShangHai
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.10organizationName(X.520 DN component)
            PrintableString  CnPack
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.11organizationalUnitName(X.520 DN component)
            PrintableString  CnPack Team
        SET
          SEQUENCE
            OBJECT IDENTIFIER 2.5.4.3commonName(X.520 DN component)
            PrintableString  cnpack.org
        SET
          SEQUENCE
           OBJECT IDENTIFIER  1.2.840.113549.1.9.1 emailAddress
           IA5String  master@cnpack.org
      SEQUENCE
        SEQUENCE
          OBJECT IDENTIFIER1.2.840.113549.1.1.1rsaEncryption(PKCS #1)
          NULL
        BIT STRING
          SEQUENCE
            INTEGER
            INTEGER 65537
      [0]
    SEQUENCE
      OBJECT IDENTIFIER 1.2.840.113549.1.1.5sha1WithRSAEncryption(PKCS #1)
      NULL
    BIT STRING  Digest ֵ���� RSA ���ܺ�Ľ��
}
function CnCALoadCertificateSignRequestFromStream(Stream: TStream;
  CertificateRequest: TCnRSACertificateRequest): Boolean;
var
  IsRSA: Boolean;
  Reader: TCnBerReader;
  MemStream: TMemoryStream;
  DNRoot, PubNode, HashNode, SignNode: TCnBerReadNode;
  List: TStringList;
begin
  Result := False;

  Reader := nil;
  MemStream := nil;
  try
    MemStream := TMemoryStream.Create;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;
    if (Reader.TotalCount >= 42) and (Reader.Items[2].BerTag = CN_BER_TAG_INTEGER)
      and (Reader.Items[2].AsInteger = 0) then // ��������ô����汾�ű���Ϊ 0
    begin
      DNRoot := Reader.Items[3];
      PubNode := DNRoot.GetNextSibling;
      if PubNode = nil then
        Exit;

      HashNode := Reader.Items[1].GetNextSibling;
      if (HashNode = nil) or (HashNode.Count <> 2) then
        Exit;

      SignNode := HashNode.GetNextSibling;
      if (SignNode = nil) or (SignNode.BerTag <> CN_BER_TAG_BIT_STRING)
        or (SignNode.BerDataLength <= 2) then
        Exit;

      IsRSA := False;
      if (PubNode.Count = 2) and (PubNode.Items[0].Count = 2) then
        IsRSA := CompareObjectIdentifier(PubNode.Items[0].Items[0],
          @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

      if not IsRSA then // �㷨���� RSA
        Exit;

      List := TStringList.Create;
      try
        ExtractDNValuesToList(DNRoot, List);

        CertificateRequest.CertificateRequestInfo.CountryName := List.Values[SDN_COUNTRYNAME];
        CertificateRequest.CertificateRequestInfo.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
        CertificateRequest.CertificateRequestInfo.LocalityName := List.Values[SDN_LOCALITYNAME];
        CertificateRequest.CertificateRequestInfo.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
        CertificateRequest.CertificateRequestInfo.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
        CertificateRequest.CertificateRequestInfo.CommonName := List.Values[SDN_COMMONNAME];
        CertificateRequest.CertificateRequestInfo.EmailAddress := List.Values[SDN_EMAILADDRESS];
      finally
        List.Free;
      end;

      // �⿪��Կ
      PubNode := PubNode.Items[1]; // BitString
      if not ExtractPublicKey(PubNode, CertificateRequest.PublicKey) then
        Exit;

      Result := ExtractSignaturesByPublicKey(CertificateRequest.PublicKey,
        HashNode, SignNode, CertificateRequest.FCASignType, CertificateRequest.FDigestType,
        CertificateRequest.FSignValue, CertificateRequest.FDigestValue,
        CertificateRequest.FSignLength, CertificateRequest.FDigestLength);
    end;
  finally
    Reader.Free;
    MemStream.Free;
  end;
end;

function CnCAVerifyCertificateSignRequestFile(const FileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifyCertificateSignRequestStream(Stream);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifyCertificateSignRequestStream(Stream: TStream): Boolean;
var
  CSR: TCnRSACertificateRequest;
  Reader: TCnBerReader;
  MemStream, DigestStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CSR := nil;
  Reader := nil;
  MemStream := nil;
  DigestStream := nil;

  try
    CSR := TCnRSACertificateRequest.Create;
    if not CnCALoadCertificateSignRequestFromStream(Stream, CSR) then
      Exit;

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_REQUEST_HEAD,
      PEM_CERTIFICATE_REQUEST_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      InfoRoot := Reader.Items[1];

      // ������ Hash
      DigestStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CSR.CASignType, DigestStream);

      if DigestStream.Size = CSR.DigestLength then
        Result := CompareMem(DigestStream.Memory, CSR.DigestValue, DigestStream.Size);
    end;
  finally
    CSR.Free;
    Reader.Free;
    MemStream.Free;
    DigestStream.Free;
  end;
end;

function CnCAVerifySelfSignedCertificateFile(const FileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCAVerifySelfSignedCertificateStream(Stream);
  finally
    Stream.Free;
  end;
end;

function CnCAVerifySelfSignedCertificateStream(Stream: TStream): Boolean;
var
  CRT: TCnRSACertificate;
  Reader: TCnBerReader;
  MemStream, DigestStream: TMemoryStream;
  InfoRoot: TCnBerReadNode;
  P: Pointer;
begin
  Result := False;
  CRT := nil;
  Reader := nil;
  MemStream := nil;
  DigestStream := nil;

  try
    CRT := TCnRSACertificate.Create;
    if not CnCALoadCertificateFromStream(Stream, CRT) then
      Exit;

    if not CRT.IsSelfSigned then
      raise Exception.Create('NOT Self-Sign. Can NOT Verify.');

    MemStream := TMemoryStream.Create;
    Stream.Position := 0;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, MemStream) then
      Exit;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount > 2 then
    begin
      InfoRoot := Reader.Items[1];

      // ������ Hash
      DigestStream := TMemoryStream.Create;
      P := InfoRoot.BerAddress;
      CalcDigestData(P, InfoRoot.BerLength, CRT.CASignType, DigestStream);

      if DigestStream.Size = CRT.DigestLength then
        Result := CompareMem(DigestStream.Memory, CRT.DigestValue, DigestStream.Size);
    end;
  finally
    CRT.Free;
    Reader.Free;
    MemStream.Free;
    DigestStream.Free;
  end;
end;

{ TCnCertificateBasicInfo }

procedure TCnCertificateBaseInfo.Assign(Source: TPersistent);
begin
  if Source is TCnCertificateBaseInfo then
  begin
    FCountryName := (Source as TCnCertificateBaseInfo).CountryName;
    FOrganizationName := (Source as TCnCertificateBaseInfo).OrganizationName;
    FEmailAddress := (Source as TCnCertificateBaseInfo).EmailAddress;
    FLocalityName := (Source as TCnCertificateBaseInfo).LocalityName;
    FCommonName := (Source as TCnCertificateBaseInfo).CommonName;
    FOrganizationalUnitName := (Source as TCnCertificateBaseInfo).OrganizationalUnitName;
    FStateOrProvinceName := (Source as TCnCertificateBaseInfo).StateOrProvinceName;
  end
  else
    inherited;
end;

function TCnCertificateBaseInfo.ToString: string;
begin
  Result := 'CountryName: ' + FCountryName;
  Result := Result + SCRLF + 'StateOrProvinceName: ' + FStateOrProvinceName;
  Result := Result + SCRLF + 'LocalityName: ' + FLocalityName;
  Result := Result + SCRLF + 'OrganizationName: ' + FOrganizationName;
  Result := Result + SCRLF + 'OrganizationalUnitName: ' + FOrganizationalUnitName;
  Result := Result + SCRLF + 'CommonName: ' + FCommonName;
  Result := Result + SCRLF + 'EmailAddress: ' + FEmailAddress;
end;

{ TCnRSACertificateRequest }

constructor TCnRSACertificateRequest.Create;
begin
  inherited;
  FCertificateRequestInfo := TCnCertificateRequestInfo.Create;
  FPublicKey := TCnRSAPublicKey.Create;
end;

destructor TCnRSACertificateRequest.Destroy;
begin
  FCertificateRequestInfo.Free;
  FPublicKey.Free;
  FreeMemory(FSignValue);
  FreeMemory(FDigestValue);
  inherited;
end;

procedure TCnRSACertificateRequest.SetCertificateRequestInfo(
  const Value: TCnCertificateRequestInfo);
begin
  FCertificateRequestInfo.Assign(Value);
end;

procedure TCnRSACertificateRequest.SetPublicKey(
  const Value: TCnRSAPublicKey);
begin
  FPublicKey.Assign(Value);
end;

function TCnRSACertificateRequest.ToString: string;
begin
  Result := FCertificateRequestInfo.ToString;
  Result := Result + SCRLF + 'Public Key Modulus: ' + FPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Public Key Exponent: ' + FPublicKey.PubKeyExponent.ToDec;
  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  Result := Result + SCRLF + 'Signature Hash: ' + GetDigestNameFromSignDigestType(FDigestType);
  Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
end;

function GetCASignNameFromSignType(Sign: TCnCASignType): string;
begin
  case Sign of
    ctMd5RSA: Result := 'MD5 RSA';
    ctSha1RSA: Result := 'SHA1 RSA';
    ctSha256RSA: Result := 'SHA256 RSA';
  else
    Result := '<Unknown>';
  end;
end;

{ TCnUTCTime }

procedure TCnUTCTime.SetDateTime(const Value: TDateTime);
var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;
begin
  FDateTime := Value;
  
  // ��ʱ������ת�����ַ������� FUTCTimeString��ʹ�� YYMMDDhhmm[ss]Z �ĸ�ʽ
  DecodeDate(FDateTime, Year, Month, Day);
  DecodeTime(FDateTime, Hour, Minute, Sec, MSec);

  Year := Year mod 100; // ֻȡ����λ
  FUTCTimeString := Format('%2.2d%2.2d%2.2d%2.2d%2.2d', [Year, Month, Day, Hour, Minute]);
  if Sec <> 0 then
    FUTCTimeString := FUTCTimeString + Format('%2.2d', [Sec]);
  FUTCTimeString := FUTCTimeString + 'Z';
end;

procedure TCnUTCTime.SetUTCTimeString(const Value: string);
var
  Year, Month, Day, Hour, Minute, Sec, DeltaHour, DeltaMin: Word;
  Idx: Integer;
  Plus: Boolean;
  DeltaTime: TDateTime;
begin
  FUTCTimeString := Value;
  //  ���� String ��ʱ�䲢�� FDateTime����ʽ�� YYMMDDhhmm[ss]Z �� YYMMDDhhmm[ss](+|-)hhmm
  if Length(FUTCTimeString) > 10 then // ���ٵ��� 11 ��
  begin
    Idx := 1;
    Year := StrToInt(Copy(FUTCTimeString, Idx, 2)) + 2000;  // 1
    Inc(Idx, 2);
    Month := StrToInt(Copy(FUTCTimeString, Idx, 2));        // 3
    Inc(Idx, 2);
    Day := StrToInt(Copy(FUTCTimeString, Idx, 2));          // 5
    Inc(Idx, 2);
    Hour := StrToInt(Copy(FUTCTimeString, Idx, 2));         // 7
    Inc(Idx, 2);
    Minute := StrToInt(Copy(FUTCTimeString, Idx, 2));       // 9
    Inc(Idx, 2);

    Sec := 0;
    if FUTCTimeString[Idx] in ['0'..'9'] then   // �� ss    // 11
    begin
      Sec := StrToInt(Copy(FUTCTimeString, Idx, 2));
      Inc(Idx, 2);
    end;

    if Idx <= Length(FUTCTimeString) then
    begin
      // ��ʱ Idx ֱ�ӣ���Խ�����ܵ� ss��ָ�� Z �� +-
      if FUTCTimeString[Idx] in ['+', '-'] then
      begin
        Plus := FUTCTimeString[Idx] = '+';
        Inc(Idx);
        DeltaHour := 0;
        DeltaMin := 0;
        if Idx <= Length(FUTCTimeString) then
        begin
          DeltaHour := StrToInt(Copy(FUTCTimeString, Idx, 2));
          Inc(Idx, 2);
          if Idx <= Length(FUTCTimeString) then
            DeltaMin := StrToInt(Copy(FUTCTimeString, Idx, 2));
        end;

        FDateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, 0);
        DeltaTime := EncodeTime(DeltaHour, DeltaMin, 0, 0);

        if Plus then
          FDateTime := FDateTime + DeltaTime
        else
          FDateTime := FDateTime - DeltaTime;
      end
      else if FUTCTimeString[Idx] = 'Z' then
        FDateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, 0);
    end;
  end;
end;

{ TCnRSACertificate }

function CnCALoadCertificateFromFile(const FileName: string;
  Certificate: TCnRSACertificate): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnCALoadCertificateFromStream(Stream, Certificate);
  finally
    Stream.Free;
  end;
end;

function CnCALoadCertificateFromStream(Stream: TStream;
  Certificate: TCnRSACertificate): Boolean;
var
  Mem: TMemoryStream;
  Reader: TCnBerReader;
  SerialNum: TCnBigNumber;
  Root, Node, VerNode, SerialNode: TCnBerReadNode;
  BSCNode, SignAlgNode, SignValueNode: TCnBerReadNode;
  List: TStringList;
  IsRSA: Boolean;
begin
  Result := False;

  Mem := nil;
  Reader := nil;
  try
    Mem := TMemoryStream.Create;
    if not LoadPemStreamToMemory(Stream, PEM_CERTIFICATE_HEAD, PEM_CERTIFICATE_TAIL, Mem) then
      Exit;

    Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
    Reader.ParseToTree;

    Root := Reader.Items[0];
    if Root.Count <> 3 then
      Exit;

    // �õ�����Ҫ���ڵ�
    BSCNode := Root.Items[0];
    SignAlgNode := Root.Items[1];
    SignValueNode := Root.Items[2];

    // BSC ����
    if BSCNode.Count < 6 then
      Exit;

    // �ж� Version������û��
    Certificate.BasicCertificate.Version := CN_CRT_BASIC_VERSION_1;
    if (BSCNode.Items[0].BerTag = 0) and (BSCNode.Items[0].Count = 1) then
    begin
      SerialNode := BSCNode.Items[1];

      // A0 �ֽڿ�ͷ��һ���ڵ㣬������һ�� Integer �ڵ㣬���Ǳ�׼���������Ľڵ�
      VerNode := BSCNode.Items[0].Items[0];
      Certificate.BasicCertificate.Version := VerNode.AsByte;
    end
    else
      SerialNode := BSCNode.Items[0];

    // ���к�
    SerialNum := TCnBigNumber.Create;
    try
      SerialNode.AsBigNumber(SerialNum);
      Certificate.BasicCertificate.SerialNumber := SerialNum.ToDec;
    finally
      FreeAndNil(SerialNum);
    end;

    // ������Ϣ�е�ǩ���㷨�ֶ�
    Node := SerialNode.GetNextSibling;
    if (Node <> nil) and (Node.Count = 2) then
      Certificate.BasicCertificate.CASignType := ExtractCASignType(Node.Items[0]);

    // �����ڶ������ֶ�
    List := TStringList.Create;
    try
      Node := Node.GetNextSibling; // ǩ���㷨�ڵ���ͬ���ڵ��� Issuer
      ExtractDNValuesToList(Node, List);
      Certificate.BasicCertificate.Issuer.CountryName := List.Values[SDN_COUNTRYNAME];
      Certificate.BasicCertificate.Issuer.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
      Certificate.BasicCertificate.Issuer.LocalityName := List.Values[SDN_LOCALITYNAME];
      Certificate.BasicCertificate.Issuer.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
      Certificate.BasicCertificate.Issuer.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
      Certificate.BasicCertificate.Issuer.CommonName := List.Values[SDN_COMMONNAME];
      Certificate.BasicCertificate.Issuer.EmailAddress := List.Values[SDN_EMAILADDRESS];

      Node := Node.GetNextSibling; // Issuer �ڵ���ͬ���ڵ����� UTC Time
      if Node.Count = 2 then
      begin
        Certificate.BasicCertificate.NotBefore.UTCTimeString := Node.Items[0].AsString;
        Certificate.BasicCertificate.NotAfter.UTCTimeString := Node.Items[1].AsString;
      end;

      Node := Node.GetNextSibling; // UTC Time �ڵ���ͬ���ڵ��� Subject
      ExtractDNValuesToList(Node, List);
      Certificate.BasicCertificate.Subject.CountryName := List.Values[SDN_COUNTRYNAME];
      Certificate.BasicCertificate.Subject.StateOrProvinceName := List.Values[SDN_STATEORPROVINCENAME];
      Certificate.BasicCertificate.Subject.LocalityName := List.Values[SDN_LOCALITYNAME];
      Certificate.BasicCertificate.Subject.OrganizationName := List.Values[SDN_ORGANIZATIONNAME];
      Certificate.BasicCertificate.Subject.OrganizationalUnitName := List.Values[SDN_ORGANIZATIONALUNITNAME];
      Certificate.BasicCertificate.Subject.CommonName := List.Values[SDN_COMMONNAME];
      Certificate.BasicCertificate.Subject.EmailAddress := List.Values[SDN_EMAILADDRESS];
    finally
      List.Free;
    end;

    Node := Node.GetNextSibling; // Subject �ڵ���ͬ���ڵ��ǹ�Կ
    IsRSA := False;
    if (Node.Count = 2) and (Node.Items[0].Count = 2) then
      IsRSA := CompareObjectIdentifier(Node.Items[0].Items[0],
        @OID_RSAENCRYPTION_PKCS1[0], SizeOf(OID_RSAENCRYPTION_PKCS1));

    if not IsRSA then // �㷨���� RSA
      Exit;

    // �⿪��Կ
    Node := Node.Items[1]; // ָ�� BitString
    if not ExtractPublicKey(Node, Certificate.BasicCertificate.SubjectPublicKey) then
      Exit;

    // ��ǩ��֤����Խ⿪ɢ��ֵ
    if Certificate.IsSelfSigned then
      Result := ExtractSignaturesByPublicKey(Certificate.BasicCertificate.SubjectPublicKey,
        SignAlgNode, SignValueNode, Certificate.FCASignType, Certificate.FDigestType,
        Certificate.FSignValue, Certificate.FDigestValue, Certificate.FSignLength,
        Certificate.FDigestLength)
    else
      // �⿪ǩ����ע��֤�鲻��ǩ�������Ĺ�Կ���������޷������õ�����ɢ��ֵ
      Result := ExtractSignaturesByPublicKey(nil, SignAlgNode, SignValueNode, Certificate.FCASignType,
        DummyDigestType, Certificate.FSignValue, DummyPointer, Certificate.FSignLength,
        DummyInteger);

    // �⿪��׼��չ��˽�л�������չ�ڵ�
    if Result then
    begin
      Node := (Node.Parent as TCnBerReadNode).GetNextSibling;
      if (Node <> nil) then  // BITString ������������
      begin
        Reader.ManualParseNodeData(Node);
        if Node.Count = 1 then
          Node := Node.Items[0];

        Result := ExtractExtensions(Node, Certificate.BasicCertificate.StandardExtension,
          Certificate.BasicCertificate.PrivateInternetExtension);
      end;
    end;
  finally
    Mem.Free;
    Reader.Free;
  end;
end;

{ TCnRSACertificate }

constructor TCnRSACertificate.Create;
begin
  FBasicCertificate := TCnRSABasicCertificate.Create;
end;

destructor TCnRSACertificate.Destroy;
begin
  FBasicCertificate.Free;
  inherited;
end;

function TCnRSACertificate.GetIsSelfSigned: Boolean;
begin
  Result := (FBasicCertificate.Subject.CountryName = FBasicCertificate.Issuer.CountryName)
    and (FBasicCertificate.Subject.OrganizationName = FBasicCertificate.Issuer.OrganizationName)
    and (FBasicCertificate.Subject.CommonName = FBasicCertificate.Issuer.CommonName);
end;

function TCnRSACertificate.ToString: string;
begin
  if IsSelfSigned then
    Result := 'Self-Signature ';
  Result := Result + FBasicCertificate.ToString;
  Result := Result + SCRLF + 'CA Signature Type: ' + GetCASignNameFromSignType(FCASignType);
  Result := Result + SCRLF + 'Signature: ' + PrintHex(FSignValue, FSignLength);
  if FDigestValue <> nil then
  begin
    Result := Result + SCRLF + 'Hash: ' + GetDigestNameFromSignDigestType(FDigestType);
    Result := Result + SCRLF + 'Digest: ' + PrintHex(FDigestValue, FDigestLength);
  end
  else
    Result := Result + SCRLF + '<No Digest>';
end;

{ TCnRSABasicCertificate }

constructor TCnRSABasicCertificate.Create;
begin
  FNotBefore := TCnUTCTime.Create;
  FNotAfter := TCnUTCTime.Create;
  FIssuer := TCnCertificateIssuerInfo.Create;
  FSubject := TCnCertificateSubjectInfo.Create;
  FSubjectPublicKey := TCnRSAPublicKey.Create;
  FStandardExtension := TCnCertificateStandardExtensions.Create;
  FPrivateInternetExtension := TCnCertificatePrivateInternetExtensions.Create;
end;

destructor TCnRSABasicCertificate.Destroy;
begin
  FPrivateInternetExtension.Free;
  FStandardExtension.Free;
  FIssuer.Free;
  FSubjectPublicKey.Free;
  FSubject.Free;
  FNotBefore.Free;
  FNotAfter.Free;
  inherited;
end;

function TCnRSABasicCertificate.ToString: string;
begin
  Result := 'Version: ' + IntToStr(FVersion);
  Result := Result + SCRLF + 'SerialNumber: ' + FSerialNumber;
  Result := Result + SCRLF + 'Issuer: ';
  Result := Result + SCRLF + FIssuer.ToString;
  Result := Result + SCRLF + 'IssuerUniqueID: ' + FIssuerUniqueID;
  Result := Result + SCRLF + 'Validity From: ' + DateTimeToStr(FNotBefore.DateTime) + ' To: ' + DateTimeToStr(FNotAfter.DateTime);
  Result := Result + SCRLF + 'Subject: ';
  Result := Result + SCRLF + FSubject.ToString;
  Result := Result + SCRLF + 'SubjectUniqueID: ' + FSubjectUniqueID;
  Result := Result + SCRLF + 'Subject Public Key Modulus: ' + SubjectPublicKey.PubKeyProduct.ToDec;
  Result := Result + SCRLF + 'Subject Public Key Exponent: ' + SubjectPublicKey.PubKeyExponent.ToDec;
  Result := Result + SCRLF + FStandardExtension.ToString;
  Result := Result + SCRLF + FPrivateInternetExtension.ToString;
end;

{ TCnCertificatePrivateInternetExtensions }

function TCnCertificatePrivateInternetExtensions.ToString: string;
begin
  Result := 'AuthorityInformationAccess Ocsp: ' + FAuthorityInformationAccessOcsp;
  Result := Result + SCRLF + 'AuthorityInformationAccess CaIssusers: ' + FAuthorityInformationAccessCaIssuers;
end;

{ TCnCertificateStandardExtensions }

constructor TCnCertificateStandardExtensions.Create;
begin
  inherited;
  FSubjectAltName := TStringList.Create;
  FIssuerAltName := TStringList.Create;
  FCRLDistributionPoints := TStringList.Create;
end;

destructor TCnCertificateStandardExtensions.Destroy;
begin
  FCRLDistributionPoints.Free;
  FIssuerAltName.Free;
  FSubjectAltName.Free;
  inherited;
end;

function TCnCertificateStandardExtensions.ToString: string;
var
  SetVal: Integer;
begin
  SetVal := 0;
  Move(FKeyUsage, SetVal, SizeOf(FKeyUsage));
  Result := 'Standard Extension Key Usage: ' + IntToHex(SetVal, 2);
  SetVal := 0;
  Move(FExtendedKeyUsage, SetVal, SizeOf(FExtendedKeyUsage));
  Result := Result + SCRLF + 'Extended Key Usage: ' + IntToHex(SetVal, 2);
  Result := Result + SCRLF + 'Basic Constraints is CA: ' + InttoStr(Integer(FBasicConstraintsCA));
  Result := Result + SCRLF + 'Basic Constraints Path Len: ' + InttoStr(FBasicConstraintsPathLen);
  Result := Result + SCRLF + 'Authority Key Identifier: ' + PrintHex(Pointer(FAuthorityKeyIdentifier), Length(FAuthorityKeyIdentifier));
  Result := Result + SCRLF + 'Subject Key Identifier: ' + PrintHex(Pointer(FSubjectKeyIdentifier), Length(FSubjectKeyIdentifier));
  Result := Result + SCRLF + 'Subject Alternative Names: ' + SCRLF + FSubjectAltName.Text;
  Result := Result + SCRLF + 'Issuer Alternative Names: '+ SCRLF + FIssuerAltName.Text;
  Result := Result + SCRLF + 'CRL Distribution Points: '+ SCRLF + FCRLDistributionPoints.Text;
end;

{
  SET(1 elem)
    SEQUENCE(2 elem)
      OBJECT IDENTIFIER (X.520 DN component)
      PrintableString
}
function AddDNOidValueToWriter(AWriter: TCnBerWriter; DNRoot: TCnBerWriteNode;
  AOID: PByte; AOIDLen: Integer; const DN: string; BerTag: Integer = CN_BER_TAG_PRINTABLESTRING): TCnBerWriteNode;
begin
  Result := AWriter.AddContainerNode(CN_BER_TAG_SET, DNRoot);
  Result := AWriter.AddContainerNode(CN_BER_TAG_SEQUENCE, Result);
  AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, AOID, AOIDLen, Result);
  AWriter.AddAnsiStringNode(BerTag, DN, Result);
end;

function CnCANewSelfSignedCertificate(PrivateKey: TCnRSAPrivateKey; PublicKey:
  TCnRSAPublicKey; const OutCRTFile: string; const CountryName: string; const
  StateOrProvinceName: string; const LocalityName: string; const OrganizationName:
  string; const OrganizationalUnitName: string; const CommonName: string; const
  EmailAddress: string; const IntSerialNum: string; NotBefore, NotAfter: TDateTime;
  CASignType: TCnCASignType = ctSha1RSA): Boolean;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
begin
  Result := False;
  if NotAfter <= NotBefore then
    Exit;

  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // д��ǩ����
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // дǩ����
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0], SizeOf(OID_DN_COUNTRYNAME), CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0], SizeOf(OID_DN_STATEORPROVINCENAME), StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0], SizeOf(OID_DN_LOCALITYNAME), LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0], SizeOf(OID_DN_ORGANIZATIONNAME), OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0], SizeOf(OID_DN_ORGANIZATIONALUNITNAME), OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0], SizeOf(OID_DN_COMMONNAME), CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0], SizeOf(OID_DN_EMAILADDRESS), EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ�����
    WritePublicKeyToNode(Writer, PubNode, PublicKey);

    // ���㲢дǩ��ֵ
    if not GenerateSignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // ����
    FreeAndNil(Stream);
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
  end;
end;

function CnCASignCertificate(PrivateKey: TCnRSAPrivateKey; const CRTFile: string;
  const CSRFile: string; const OutCRTFile: string; const IntSerialNum: string;
  NotBefore, NotAfter: TDateTime; CASignType: TCnCASignType = ctSha1RSA): Boolean;
var
  Writer: TCnBerWriter;
  Root, BasicNode, SubjectNode: TCnBerWriteNode;
  ValidNode, PubNode, IssuerNode, Node: TCnBerWriteNode;
  SerialNum: TCnBigNumber;
  UTCTime: TCnUTCTime;
  Stream: TMemoryStream;
  Buf: array of Byte;
  CSR: TCnRSACertificateRequest;
  CRT: TCnRSACertificate;
begin
  Result := False;
  if (PrivateKey = nil) or not FileExists(CRTFile) or not FileExists(CSRFile) then
    Exit;

  CSR := nil;
  CRT := nil;
  Writer := nil;
  SerialNum := nil;
  UTCTime := nil;
  Stream := nil;

  try
    CSR := TCnRSACertificateRequest.Create;
    if not CnCAVerifyCertificateSignRequestFile(CSRFile) or not
      CnCALoadCertificateSignRequestFromFile(CSRFile, CSR) then
      Exit;

    CRT := TCnRSACertificate.Create;
    if not CnCALoadCertificateFromFile(CRTFile, CRT) then
      Exit;

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    BasicNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // �汾���ԣ�д���к�
    SerialNum := TCnBigNumber.Create;
    SerialNum.SetDec(IntSerialNum);
    SetLength(Buf, SerialNum.GetBytesCount);
    SerialNum.ToBinary(@Buf[0]);
    Writer.AddBasicNode(CN_BER_TAG_INTEGER, @Buf[0], Length(Buf), BasicNode);

    // д�㷨
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    AddCASignTypeOIDNodeToWriter(Writer, CASignType, Node);
    Writer.AddNullNode(Node);

    SubjectNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    ValidNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    IssuerNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);
    PubNode := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, BasicNode);

    // д��ǩ����
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CSR.CertificateRequestInfo.CountryName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CSR.CertificateRequestInfo.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CSR.CertificateRequestInfo.LocalityName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CSR.CertificateRequestInfo.OrganizationName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CSR.CertificateRequestInfo.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CSR.CertificateRequestInfo.CommonName);
    AddDNOidValueToWriter(Writer, SubjectNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CSR.CertificateRequestInfo.EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Чʱ��
    UTCTime := TCnUTCTime.Create;
    UTCTime.SetDateTime(NotBefore);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);
    UTCTime.SetDateTime(NotAfter);
    Writer.AddAnsiStringNode(CN_BER_TAG_UTCTIME, UTCTime.UTCTimeString, ValidNode);

    // дǩ����
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COUNTRYNAME[0],
      SizeOf(OID_DN_COUNTRYNAME), CRT.BasicCertificate.Issuer.CountryName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_STATEORPROVINCENAME[0],
      SizeOf(OID_DN_STATEORPROVINCENAME), CRT.BasicCertificate.Issuer.StateOrProvinceName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_LOCALITYNAME[0],
      SizeOf(OID_DN_LOCALITYNAME), CRT.BasicCertificate.Issuer.LocalityName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONNAME[0],
      SizeOf(OID_DN_ORGANIZATIONNAME), CRT.BasicCertificate.Issuer.OrganizationName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_ORGANIZATIONALUNITNAME[0],
      SizeOf(OID_DN_ORGANIZATIONALUNITNAME), CRT.BasicCertificate.Issuer.OrganizationalUnitName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_COMMONNAME[0],
      SizeOf(OID_DN_COMMONNAME), CRT.BasicCertificate.Issuer.CommonName);
    AddDNOidValueToWriter(Writer, IssuerNode, @OID_DN_EMAILADDRESS[0],
      SizeOf(OID_DN_EMAILADDRESS), CRT.BasicCertificate.Issuer.EmailAddress, CN_BER_TAG_IA5STRING);

    // д��Կ�ڵ�����
    WritePublicKeyToNode(Writer, PubNode, CRT.BasicCertificate.SubjectPublicKey);

    // ���㲢дǩ��ֵ
    if not GenerateSignatureNode(Writer, Root, BasicNode, PrivateKey, CASignType) then
      Exit;

    // ����
    FreeAndNil(Stream);
    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);
    Result := SaveMemoryToPemFile(OutCRTFile, PEM_CERTIFICATE_HEAD,
      PEM_CERTIFICATE_TAIL, Stream);
    Result := True;
  finally
    Writer.Free;
    Stream.Free;
    SerialNum.Free;
    UTCTime.Free;
    CSR.Free;
    CRT.Free;
  end;
end;

end.
