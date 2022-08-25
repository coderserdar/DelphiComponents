{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Authors:      Arno Garrels
              Angus Robertson <delphi@magsys.co.uk>
Creation:     Aug 26, 2007
Description:  SSL key and X509 certification creation
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2007-2022 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Jun 30, 2008 A.Garrels made some changes to prepare SSL code for Unicode.
Jun 30, 2008 A.Garrels added some RSA and Blowfish crypto functions.
Jul 11, 2008 v1.01 RTT <pdfe@oniduo.pt> contributed function CreateCertRequest(),
             slightly modified by A. Garrels.
Jan 29, 2009 V1.03 A.Garrels added overloads which take UnicodeStrings to
             CreateCertRequest() and CreateSelfSignedCert() in D2009 and better.
             Both functions now create UTF-8 certificate fields if they contain
             characters beyond the ASCII range.
Apr 24, 2011 V1.04 Record TEVP_PKEY_st changed in OpenSSL 1.0.0 and had to be
             declared as dummy. Use new functions from OverbyteIcsLibeay to
             make this unit compatible with OpenSSL 1.0.0+.
Apr 24, 2011 V1.05 Include OverbyteIcsTypes.pas to make inlining work.
Nov 12, 2013 V1.06 Angus allow private key and certificate to be saved to separate files
Feb 14, 2014 V1.07 Angus added class TX509Ex derived from TX509Base adding
             properties for most common certificate entries including extensions
             Optionally add clear text comments to PEM files to easily identify
             certifcates.
Apr 18, 2014 V1.07a Arno removed some compiler warnings.
Jul 07, 2014 V1.08 Angus improved certificate comment
June 2015    Angus moved to main source dir
Oct 25, 2015 V1.09 Angus added SignatureAlgorithm property to TX509Ex so we can check
             certificates are SHA256, also KeyInfo, SerialNumHex
             CertInfo provides multiline string of main certificate information for logging
Nov 5, 2015  V8.20 Angus removed a compiler warning, version matches wsocket
Mar 17, 2015 V8.21 Angus use SHA256 for unicode self signed and non-unicode request
May 24, 2016 V8.27 Angus, initial support for OpenSSL 1.1.0
Aug 27, 2016 V8.32 Angus, moved sslRootCACertsBundle long constant from twsocket and
               aplit smaller and make function so it will compile under C++ Builder
Oct 18, 2016 V8.35 Angus, no longer need OverbyteIcsLibeayEx
             added CreateRsaKeyPair
Oct 26, 2016  V8.36 Now using new names for imports renamed in OpenSSL 1.1.0
Nov 22, 2016  V8.39 Moved TX509Ex properties into TX509Class in OverbyteIcsWSocket
Jan 27, 2017  V8.40 Angus New TSslCertTools component to create and sign certificates
              Create Elliptic Curve/ECDSA private keys
              Create certificate request from old certificate
              Sign certificate requests as CA
              Create DHParam files
              Add alternate extended properties to certs, DNS and IP addresses
              Added TEvpCipher and TEvpDigest (hash) types supported by OpenSSL
              Display Sha1Hex in General certificate as Fingerprint
Feb 24, 2017  V8.41 Added CreateCertBundle to build a new PEM or PKCS12 file
                 combining certificate, private key and intermediate files
              Added more certificate request extension properties including alt domains
              Creating requests now adds alternate domains, IP addresses, etc
              Create certificate from request now optionally copies extensions
              The old CreateCertRequest and CreateSelfSignedCert functions now
                 use the TSslCertTools component and provide backward compatibitity
Mar 3, 2017  V8.42 Angus TULargeInteger now ULARGE_INTEGER
Jun 21, 2017 V8.49 Added ISRG Root X1 certificate for Let's Encrypt
             Fixed AV creating second EC key (OpenSSL function fails)
             Now creating X25519 Elliptic Curve private keys
             Total rewrite creating private keys using EVP_PKEY_CTX functions
Sep 22, 2017 V8.50 Alternate DNS names now added correctly to requests and certs
             Corrected X25519 private keys to ED25519, requires OpenSSL 1.1.1
Nov 3, 2017  V8.51 Tested ED25519 keys, can now sign requests and certs
             Added RSA-PSS keys and SHA3 digest hashes, requires OpenSSL 1.1.1
Feb 21, 2018 V8.52 Added DigiCert Global Root G2 and G3 root certificates
             Added key and signature to ReqCertInfo
             Added SaveToDERText
Mar 27, 2018 V8.53 Added GlobalSign Root CA - R2 and GlobalSign ECC Root CA - R5
                root certificates, R2 used by Google.
             Added DST Root CA X3 root, used by Let's Encrypt crossed signing
Oct 2, 2018  V8.57 tidy up UnwrapNames.
             DoSelfSignCert can now use a CSR instead of properties
             Added DoClearCA
             Added SaveToCADatabase which saves CA database entry to CADBFile
             Added COMODO ECC Certification Authority root
             Build with FMX
Aug 07, 2019 V8.62  Added literals for various types to assist apps.
             Added AcmeIdentifier property for ACME validation certificate
             Builds without USE_SSL
Oct 24, 2019 V8.63 Added 'Starfield Services Root Certificate Authority - G2'
               (used by Amazon buckets), and 'Amazon Root CA 1', CA 2, CA 3,
               CA 4 which are replacing Starfield.  Removed expired certs.
May 18, 2020 V8.64 DoKeyPair raises exception for unknown key type.
              CreateSelfSignedCert ignored Days and always created 7 day expiry.
              Added support for International Domain Names for Applications (IDNA),
                i.e. using accents and unicode characters in domain names.
              X509 certificates always have A-Lavels (Punycode ASCII) domain names,
                never UTF8 or Unicode.
              CreateRsaKeyPair uses Unicode file names, as do all other SSL
                certificate functions (through IcsSslOpenFileBio).
              Added CreateSelfSignCertEx to create self signed certificates with
                 subject alternate names and specific key type, used by TSocketServer
                 to start servers with missing certificates.
              Renamed SaveToDERText to SaveReqToDERText for clarity, added
                SaveCertToDERText.
              Added root 'AAA Certificate Services' for Comodo, aka Sectigo.
              Simplified building alternate subject name extensions so that both
                DNS and IP Address can be used together, and IP address is now
                saved correctly.
              ClearAltStack dies on Win64 so suppressed part of the code until
                work out why.
Sep 03, 2020  V8.65 Clear TX509Base safely.
              Fixed four corrupted root certificates, two had 8-bit comment
                characters that caused problems if Windows was set to read UTF8,
                so suppressed all comments which also saves apace.
              All sslRootCACerts0xx constants now generated by rootcatool.exe
                 application to avoid errors, removed three old certificates
                 and added another Go Daddy.
              Write comments as UTF8 to certificate request file.
              Posix fixes.
Mar 16, 2021  V8.66 Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.
              Added support for YuOpenSSL which provides OpenSSL in a pre-built
                 DCU linked into applications, rather than using external DLLs.
              Removed support for OpenSSL 1.0.2 and 1.1.0 whose support ceased Dec 2019.
Sep 30, 2021  V8.67 OpenSSL 3.0 makes several old ciphers and digests legacy so
                 default for encrypting private key files is now PrivKeyEncAES256.
              Updated root certificate bundle, one old cert gone.
              Added new Let's Encrypt ECDSA X2 root not yet in public bundles.
              Create private key pair with PrivKeyECsecp256k kobitz curve.
              Added TSslCertTools GetKeyParams get specified key parameters for
                OpenSSL 3.0 and later.
Nov 05, 2021 V8.68 Removed DST Root CA X3 certificate which expired 30 Sept 2021.
              Support OpenSSL 3.0 for YuOpenSSL.
              Updated sslRootCACertsBundle again, few gone, now total 59 certificates.
May 25, 2022 V8.69 Fixed memory leak, thanks to Yunga.
              Updated sslRootCACertsBundle, many gone, some new, now total 53 certificates.


Pending - long term
Create string and file encryption component from existing functions

OpenSSL 3.0 deprecates RSA_public_encrypt, RSA_private_decrypt, need to be replaced
by EVP_PKEY_encrypt_init_ex, EVP_PKEY_encrypt, EVP_PKEY_decrypt_init_ex and EVP_PKEY_decrypt.



Using TSslCertTools component
=============================

The main test application for the component is the OverbyteIcsPemtool sample,
which illustrates use of all the methods and properties.

Message digests or hashes:
    TEvpDigest = (Digest_md5, Digest_mdc2, Digest_sha1, Digest_sha224,
        Digest_sha256, Digest_sha384, Digest_sha512, Digest_ripemd160);

Private key algorithm and key length in bits, bracketed comment is security
level and effective bits, beware long RSA key lengths increase SSL overhead heavily:
    TSslPrivKeyType = (
        PrivKeyRsa1024,   // level 1 - 80 bits
        PrivKeyRsa2048,   // level 2 - 112 bits
        PrivKeyRsa3072,   // level 3 - 128 bits
        PrivKeyRsa4096,   // level 3 - 148 bits?
        PrivKeyRsa7680,   // level 4 - 192 bits
        PrivKeyRsa15360,  // level 5 - 256 bits
        PrivKeyECsecp256, // level 3 - 128 bits
        PrivKeyECsecp384, // level 4 - 192 bits
        PrivKeyECsecp512, // level 5 - 256 bits
        PrivKeyECX25519); // level 3 - 128 bits

Private key file encryption:
   TSslPrivKeyCipher = (
        PrivKeyEncNone, PrivKeyEncAES128, PrivKeyEncAES192, PrivKeyEncAES256);


Create a new private key file
-----------------------------
A private key is required to create a self signed certificate or a certificate
request, and needs to be installed on any SSL servers (never distribute it).
1 - Set property PrivKeyType (RSA or EC) from TSslPrivKeyType.
2 - Create keys using DoKeyPair method checking exception for any errors.
3 - PrivateKey property contains pointer to new private key.
4 - If file to be encrypted, set property PrivKeyCipher from TSslPrivKeyCipher.
5 - Save private key to file using PrivateKeySaveToPemFile method with optional password.
6 - Optionally save public key to file using PublicKeySaveToPemFile method.

Create a new certificate request from properties
------------------------------------------------
A certificate request is needed to buy a commercial SSL certificate from a public
certificate authority and most importantly specifies the host domain name of the
public SSL server it will secure.
1 - Create a new private key (see above) or load an old key using
PrivateKeyLoadFromPemFile or PrivateKeyLoadFromText methods.
2 - Specify request properties, CommonName (host domain name), Country, State,
Locality, Organization, OrgUnit, KeyDigiSign, KeyKeyEnc, etc, as needed.
3 - Create request using DoCertReqProps method checking exception for any errors.
4 - X509Req property contains pointer to new request.
5 - Save request to PEM file using SaveReqToFile method.
6 - Optionally save request to string using SaveReqToText method.

Create a new certificate request from old certificate
-----------------------------------------------------
This is a shorter way to create a new request when renewing an existing
certificate with the same private key.
1 - Load existing certificate using LoadFromFile or LoadFromText methods.
2 - Load private key for existing certificate using PrivateKeyLoadFromPemFile or
PrivateKeyLoadFromText methods.
3 - Create request using DoCertReqOld method checking exception for any errors.
4 - X509Req property contains pointer to new request.
5 - Save request to PEM file using SaveReqToFile method.
6 - Optionally save request to string using SaveReqToText method.

Create new self signed certificate from properties
--------------------------------------------------
Self signed certificates are mostly used for testing SSL applications on
temporary servers, prior to final deployment to a public server with a
commercial SSL certificate.  Can also used for internal networks.   May
also be used to create your own CA certificate if you want to sign your
own certificates.
1 - Create a new private key (see above) or load an old key using
PrivateKeyLoadFromPemFile or PrivateKeyLoadFromText methods.
2 - Specify certificate properties, CommonName (host domain name), Country, State,
Locality, Organization, OrgUnit, KeyDigiSign, KeyKeyEnc, etc, as needed.
3 - Select CertDigest (hash) property from TEvpDigest.
4 - Create certificate using DoSelfSignCert method checking exception for any errors.
5 - X509 property contains pointer to new certificate.
6 - If file to be encrypted, set property PrivKeyCipher from TSslPrivKeyCipher.
7 - Save certificate to file using SaveToFile method with the file extension
specifying the format that should be used.  Options include IncludePrivateKey
which will save the private key into the same PEM or P12 file, and optional password.
8 - Optionally save certificate to string using SaveCertToText method.

Create new CA signed certificate from certificate request
---------------------------------------------------------
This is how commercial certificate authorities create SSL certificates from
a request, signing it with their own CA certificate (root or intermediate) that
will be trusted by Windows due to the root already being installed locally.
For development, you can create your own CA root certificate and install it in
the Windows certificate store of any test computers, then create certificates
signed by the root and they will be trusted by Windows without needing to accept
security exceptions as happens with self signed certificates.
1 - The CA certificate and CA private key need to loaded using LoadFromFile and
PrivateKeyLoadFromPemFile into X509 and PrivateKey, and these properties
assigned to X509CA and PrivKeyCA respectively.
2 - Load certificate request using LoadReqFromFile.
3 - Currently the subject certificate properties are taken from the request and
can not be edited, extended properties are currently taken from properties,
KeyDigiSign, KeyKeyEnc, etc, as needed.  This needs more work for flexibility.
4 - Select CertDigest (hash) property from TEvpDigest.
5 - Create certificate using DoSignCertReq method checking exception for any errors.
6 - X509 property contains pointer to new certificate.
7 - If file to be encrypted, set property PrivKeyCipher from TSslPrivKeyCipher.
8 - Save certificate to file using SaveToFile method with the file extension
specifying the format that should be used.  Options include IncludePrivateKey
which will save the private key into the same PEM or P12 file, and optional password.
9 - Optionally save certificate to string using SaveCertToText method.

Beware the private key used to create the request must be loaded into PrivateKey
property before saving a private key with the certificate, otherwise the CA key
may be incorrectly saved.

Pending - save details of certificates created to database, to support
transparency and certificate revocation lists.  Currently certificates have
random serial numbers, should allow sequential numbers to be allocated.

Convert certificate from one file format to another
---------------------------------------------------
1 - Load existing certificate using LoadFromFile or LoadFromText methods.
2 - Optionally load private key for existing certificate using
PrivateKeyLoadFromPemFile or PrivateKeyLoadFromText methods.
4 - If file to be encrypted, set property PrivKeyCipher from TSslPrivKeyCipher.
4 - Save certificate to file using SaveToFile method with the file extension
specifying the format that should be used.  Options include IncludePrivateKey
which will save the private key into the same PEM or P12 file, and optional password.
One use for this is to convert base64 DER/PEM certificates into P12/PVX format
that can be easily installed into the Windows certificate store.

Create New DHParams File
------------------------
DHParams contain prime numbers needed to support DH and DHE ciphers (but not
ECDH and ECDHE).  Ideally they should be unique per server and/or application
and some applications even generate new params each day.  But finding prime
numbers is time consuming, the shortest 1,024 bits can take up a minute, 2,048
bits several minutes, 4,096 bits a few hours, and gave up with 8,192 bits after
two days.  ICS include constants sslDHParams2048 and sslDHParams4096 to save
you calculating your own.
1 - Assign OnKeyProgress event handler with Application.ProcessMessages and
optionally a progress indicator so the application remains responsive while
calculating DHParams.
2 - Create DHParams using DoDHParams method passing new file name and number of
bits, 768, 1024, 20248, 4096, 8192.
3 - Optionally save DHParams string returned by DoDHParams method.

Create Certificate Bundle
-------------------------
Builds a new PEM or PKCS12 file by combining certificate, private key and
intermediate files (in any formats with correct file extension).  For servers,
a bundle file is easier to distribute and load than three separate files.
1 - CreateCertBundle is a simple function, that requires four full file names
for the three input files and output file, optional load and save passwords,
and the cipher optionally to encrypt the output file.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslX509Utils;
{$ENDIF}

{$I include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       ON}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

{$IFDEF USE_SSL}

uses

{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsSSLEAY, OverbyteIcsLibeay,
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWSocket,
{$ENDIF FMX}
    OverByteIcsMD5,
    OverbyteIcsTypes,
    OverbyteIcsLogger,
    OverbyteIcsMimeUtils,
    OverbyteIcsUtils
 {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

const
    BF_BLOCK_SIZE     = 8;

    DigestDispList: array [0..8] of TEvpDigest =
        (Digest_sha1, Digest_sha224, Digest_sha256, Digest_sha384, Digest_sha512,
         Digest_sha3_224, Digest_sha3_256, Digest_sha3_384, Digest_sha3_512);

    DigestListLitsLast = 8;
    DigestListLits: array [0..DigestListLitsLast] of PChar = (   { V8.62 }
        'SHA1 (old)', 'SHA224', 'SHA256', 'SHA384', 'SHA512',
        'SHA3_224', 'SHA3_256', 'SHA3_384', 'SHA3_512');

  { not showing RSA PSS yet }
    SslPrivKeyTypeLitsLast1 = 9;
    SslPrivKeyTypeLitsLast2 = 15;
    SslPrivKeyTypeLits: array [0..SslPrivKeyTypeLitsLast2] of PChar = (   { V8.62 }
        'RSA 1,024 bits (level 1 - 80 bits)',
        'RSA 2,048 bits (level 2 - 112 bits)',
        'RSA 3,072 bits (level 3 - 128 bits, NIST min)',
        'RSA 4,096 bits (level 3 - 128 bits)',
        'RSA 7,680 bits (level 4 - 192 bits)',
        'RSA 15,360 bits (level 5 - 256 bits)',
        'Elliptic Curve secp256  (level 3 - 128 bits)',
        'Elliptic Curve secp384  (level 4 - 192 bits)',
        'Elliptic Curve secp512  (level 5 - 256 bits)',
        'EdDSA ED25519 (level 3 - 128 bits)',
        'RSA-PSS 2,048 bits (level 2 - 112 bits)',
        'RSA-PSS 3,072 bits (level 3 - 128 bits)',
        'RSA-PSS 4,096 bits (level 3 - 128 bits)',
        'RSA-PSS 7,680 bits (level 4 - 192 bits)',
        'RSA-PSS 15,360 bits (level 5 - 256 bits)',
        'Elliptic Curve secp256k (level 3 - 128 bits)');

    SslPrivKeyCipherLits: array[TSslPrivKeyCipher] of PChar = (   { V8.62 }
        'None', 'Triple DES', 'IDEA', 'AES128', 'AES192', 'AES256'{, 'Blowfish'});   { V8.67 Blowfish gone }

    SslCertFileOpenExts = 'Certs *.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|' +
                 '*.pem;*.cer;*.crt;*.der;*.p12;*.pfx;*.p7*;*.spc|All Files *.*|*.*';    { V8.62 }

type
    TCryptProgress = procedure(Obj: TObject; Count: Int64; var Cancel: Boolean);

    PMD5Digest = ^TMD5Digest;
    TCipherType = (ctBfCbc, ctBfCfb64, ctBfOfb, ctBfEcb);

    TIVector = array[0..8] of Byte;
    PIVector = ^TIVector;

    TCipherKey = array[0..47] of Byte;
    TCipherKeyLen = (cklDefault, ckl64bit, ckl128bit, ckl256bit);
    PCipherKey = ^TCipherKey;

    TCipherSalt = String[PKCS5_SALT_LEN];//array[0..PKCS5_SALT_LEN -1] of Byte;

    TCiphContext = packed record
        Key       : TCipherKey;
        IV        : TIVector;
          //IVLen     : Integer;
        Ctx       : PEVP_CIPHER_CTX;
        Cipher    : PEVP_CIPHER;
        Encrypt   : Boolean;
        BlockSize : Integer;
    end;
    PCiphContext = ^TCiphContext;

    TAltItem = record      { V8.64 }
        AltName:  AnsiString;
        AltType:  Integer;
        PunyFlag: Boolean;
    end;

type
    ECertToolsException = class(Exception);

  { V8.40 component to create SSL certificates and keys }
    TSslCertTools = class(TX509Base)
    private
        FNewCert           : PX509;
        FNewReq            : PX509_REQ;
        FRsaKey            : PRSA;
        FPrivKey           : PEVP_PKEY;
        FECkey             : PEC_KEY;
        FECgroup           : PEC_GROUP;
        FX509Req           : Pointer;
        FX509CA            : Pointer;
        FPrivKeyCA         : PEVP_PKEY;
        FCountry           : String;
        FState             : String;
        FLocality          : String;
        FOrganization      : String;
        FOrgUnit           : String;
        FDescr             : String;
        FEmail             : String;
        FCommonName        : String;
        FAltDNSList        : TStrings;
        FAltIpList         : TStrings;
        FAltEmailList      : TStrings;
        FAltIssuer         : String;
        FCRLDistPoint      : String;  // URI:http://myhost.com/myca.crl
        FAuthInfoAcc       : String;  // OCSP;URI:http://ocsp.myhost.com/
        FBasicIsCA         : Boolean;
        FBasicPathLen      : Integer;
        FKeyCertSign       : Boolean;
        FKeyCRLSign        : Boolean;
        FKeyDigiSign       : Boolean;
        FKeyDataEnc        : Boolean;
        FKeyKeyEnc         : Boolean;
        FKeyKeyAgree       : Boolean;
        FKeyNonRepud       : Boolean;
        FKeyExtClient      : Boolean;
        FKeyExtServer      : Boolean;
        FKeyExtEmail       : Boolean;
        FKeyExtCode        : Boolean;
        FExpireDays        : Integer;
        FSerialNum         : Int64;
        FAddComments       : boolean;
        FPrivKeyType       : TSslPrivKeyType;
        FPrivKeyCipher     : TSslPrivKeyCipher;
        FCertDigest        : TEvpDigest;
        FOnKeyProgress     : TNotifyEvent;
        FAltAnsiStr        : array of AnsiString;
        FAltIa5Str         : array of PASN1_STRING;
        FAltGenStr         : array of PGENERAL_NAME;
        FCADBFile          : String;   { V8.57 }
        FAcmeIdentifier    : String;   { V8.62 }
        FAltItems          : array of TAltItem;  { V8.64 }
        FTotAltItems       : Integer;            { V8.64 }
    protected
        function    BuildBasicCons(IsCA: Boolean): AnsiString;
        function    BuildKeyUsage: AnsiString;
        function    BuildExKeyUsage: AnsiString;
        procedure   SetCertExt(Cert: PX509; Nid: integer; const List: AnsiString);
        procedure   BuildAltPropList;                      { V8.64 }
        procedure   BuildAltReqList;                       { V8.64 }
        function    BuildAltStackAll: PStack;              { V8.64 }
        procedure   BuildCertAltSubjAll(Cert: PX509);      { V8.64 }
        procedure   ClearAltStack;
        procedure   FreeAndNilX509CA;
        procedure   FreeAndNilPrivKeyCA;
        procedure   FreeAndNilX509Req;
        procedure   SetX509Req(X509Req: Pointer);
        procedure   SetX509CA(Cert: Pointer);
        procedure   SetPrivKeyCA(Pkey: PEVP_PKEY);
        function    GetReqSubjOneLine: String;
        function    GetReqSubjCName: String;
        function    GetReqSubjOName: String;
        function    GetReqSubjAltNameDNS: String;
        function    GetReqSubjAltNameIP: String;
        function    GetReqKeyUsage: String;
        function    GetReqExKeyUsage: String;
        function    GetReqCertInfo: String;
        function    GetIsReqLoaded: Boolean;
        function    GetIsCALoaded: Boolean;
        procedure   WriteReqToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = ''); virtual;
        function    GetReqKeyInfo: string;       { V8.52 }
        function    GetReqSignAlgo: String;      { V8.52 }
    public
        constructor Create(AOwner: TComponent);
        destructor  Destroy; override;
        procedure   DoCertReqProps;
        procedure   DoCertReqOld;
        procedure   DoSelfSignCert(UseCSR: Boolean = False);   { V8.57 added UseCSR }
        procedure   DoSignCertReq(CopyExtns: Boolean);
        procedure   DoKeyPair;
//        procedure   DoKeyPairOld;
        function    DoDHParams(const FileName: String; Bits: integer): String;
        procedure   DoClearCerts;
        procedure   DoClearCA;  { V8.57 split from DoClearCerts }
        function    SaveToCADatabase(const CertFName: String = 'unknown'): Boolean;  { V8.57 }
        procedure   CreateCertBundle(const CertFile, PKeyFile, InterFile, LoadPw,
                      SaveFile, SavePw: String; KeyCipher: TSslPrivKeyCipher = PrivKeyEncNone);
        function    GetReqEntryByNid(ANid: Integer): String;
        function    GetRequestRawText: String;
        procedure   LoadReqFromFile(const FileName: String);
        procedure   SaveReqToFile(const FileName: String; AddInfoText: Boolean = FALSE);
        function    SaveReqToText(AddInfoText: Boolean = FALSE): String;
        function    SaveReqToDERText: AnsiString;                             { V8.52, V8.64 was SaveToDERText }
        function    SaveCertToDERText: AnsiString;                            { V8.64 }
        function    GetKeyParams(var Params: POSSL_PARAM): Boolean;           { V8.67 }
        function    GetReqExtValuesByName(const ShortName, FieldName: String): String;
        function    GetReqExtByName(const S: String): TExtension;
        property    X509Req             : Pointer       read FX509Req       write SetX509Req;
        property    X509CA              : Pointer       read FX509CA        write SetX509CA;
        property    PrivKeyCA           : PEVP_PKEY     read FPrivKeyCA     write SetPrivKeyCA;
        property    ReqSubjOneLine      : String        read GetReqSubjOneLine;
        property    ReqSubjCName        : String        read GetReqSubjCName;
        property    ReqSubjOName        : String        read GetReqSubjOName;
        property    ReqSubjAltNameDNS   : String        read GetReqSubjAltNameDNS;
        property    ReqSubjAltNameIP    : String        read GetReqSubjAltNameIP;
        property    ReqKeyUsage         : String        read GetReqKeyUsage;
        property    ReqExKeyUsage       : String        read GetReqExKeyUsage;
        property    ReqCertInfo         : String        read GetReqCertInfo;
        property    IsReqLoaded         : Boolean       read GetIsReqLoaded;
        property    IsCALoaded          : Boolean       read GetIsCALoaded;
        property    ReqKeyInfo          : string        read GetReqKeyInfo;       { V8.52 }
        property    ReqSignAlgo         : string        read GetReqSignAlgo;      { V8.52 }
    published
        property Country           : String             read FCountry       write FCountry;
        property State             : String             read FState         write FState;
        property Locality          : String             read FLocality      write FLocality;
        property Organization      : String             read FOrganization  write FOrganization;
        property OrgUnit           : String             read FOrgUnit       write FOrgUnit;
        property Descr             : String             read FDescr         write FDescr;
        property Email             : String             read FEmail         write FEmail;
        property CommonName        : String             read FCommonName    write FCommonName;
        property AltDNSList        : TStrings           read FAltDNSList    write FAltDNSList;
        property AltIpList         : TStrings           read FAltIpList     write FAltIpList;
        property AltEmailList      : TStrings           read FAltEmailList  write FAltEmailList;
        property AltIssuer         : String             read FAltIssuer     write FAltIssuer;
        property CRLDistPoint      : String             read FCRLDistPoint  write FCRLDistPoint;
        property AuthInfoAcc       : String             read FAuthInfoAcc   write FAuthInfoAcc;
        property AcmeIdentifier    : String             read FAcmeIdentifier write FAcmeIdentifier; { V8.62 }
        property BasicIsCA         : Boolean            read FBasicIsCA     write FBasicIsCA;
        property BasicPathLen      : Integer            read FBasicPathLen  write FBasicPathLen;
        property KeyCertSign       : Boolean            read FKeyCertSign   write FKeyCertSign;
        property KeyCRLSign        : Boolean            read FKeyCRLSign    write FKeyCRLSign;
        property KeyDigiSign       : Boolean            read FKeyDigiSign   write FKeyDigiSign;
        property KeyDataEnc        : Boolean            read FKeyDataEnc    write FKeyDataEnc;
        property KeyKeyEnc         : Boolean            read FKeyKeyEnc     write FKeyKeyEnc;
        property KeyKeyAgree       : Boolean            read FKeyKeyAgree   write FKeyKeyAgree;
        property KeyNonRepud       : Boolean            read FKeyNonRepud   write FKeyNonRepud;
        property KeyExtClient      : Boolean            read FKeyExtClient  write FKeyExtClient;
        property KeyExtServer      : Boolean            read FKeyExtServer  write FKeyExtServer;
        property KeyExtEmail       : Boolean            read FKeyExtEmail   write FKeyExtEmail;
        property KeyExtCode        : Boolean            read FKeyExtCode    write FKeyExtCode;
        property ExpireDays        : Integer            read FExpireDays    write FExpireDays;
        property SerialNum         : Int64              read FSerialNum     write FSerialNum;
        property AddComments       : boolean            read FAddComments   write FAddComments;
        property PrivKeyType       : TSslPrivKeyType    read FPrivKeyType   write FPrivKeyType;
        property PrivKeyCipher     : TSslPrivKeyCipher  read FPrivKeyCipher write FPrivKeyCipher;
        property CertDigest        : TEvpDigest         read FCertDigest    write FCertDigest;
        property CADBFile          : String             read FCADBFile      write FCADBFile;    { V8.57 }
        property OnKeyProgress     : TNotifyEvent       read FOnKeyProgress write FOnKeyProgress;
    end;



procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: String;
  Bits: Integer; Comment: boolean = false); overload;
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer;
  IsCA: Boolean; Days: Integer;
  const KeyFileName: String = ''; Comment: boolean = false);  overload;
procedure CreateSelfSignCertEx(const FileName, CName: String; AltDNSLst: TStrings;
       PKeyType: TSslPrivKeyType = PrivKeyECsecp256; const PW: String = '';
                                             const AcmeId: String = '');  { V8.64 }


{ RSA crypto functions }

procedure CreateRsaKeyPair(const PubFName, PrivFName: String; Bits: Integer);  { V8.35 }

type
  TRsaPadding = (rpPkcs1, rpPkcs1Oaep, rpNoPadding);
  { rpPkcs1 - This currently is the most widely used mode             }
  { rpPkcs1Oaep - This mode is recommended for all new applications   }
  { rpNoPadding - Don't use                                           }
  { http://www.openssl.org/docs/crypto/RSA_public_encrypt.html        }

  function DecryptPrivateRSA(
      PrivKey     : PEVP_PKEY;
      InBuf       : Pointer;
      InLen       : Cardinal;
      OutBuf      : Pointer;
      var OutLen  : Cardinal;
      Padding     : TRsaPadding): Boolean;

  function EncryptPublicRSA(
      PubKey      : PEVP_PKEY;
      InBuf       : Pointer;
      InLen       : Cardinal;
      OutBuf      : Pointer;
      var OutLen  : Cardinal;
      Padding     : TRsaPadding): Boolean;

function StrEncRsa(PubKey: PEVP_PKEY; const S: AnsiString; B64: Boolean;
    Padding: TRsaPadding = rpPkcs1): AnsiString;
function StrDecRsa(PrivKey: PEVP_PKEY; S: AnsiString; B64: Boolean;
    Padding: TRsaPadding = rpPkcs1): AnsiString;

{ Symmetric cipher stuff, far from being completed.       }
{ Should probably be implemented as a class or component. }


procedure CiphPasswordToKey(InBuf: AnsiString; Salt: TCipherSalt; Count: Integer;
    var Key: TCipherKey; var KeyLen: Integer; var IV: TIVector; var IVLen: Integer);
procedure CiphInitialize(var CiphCtx: TCiphContext; const Pwd: AnsiString; Key: PCipherKey;
    IVector: PIVector; CipherType: TCipherType; KeyLen: TCipherKeyLen; Enc: Boolean);
procedure CiphSetIVector(var CiphCtx: TCiphContext; IVector: PIVector);
procedure CiphFinalize(var CiphCtx: TCiphContext);
procedure CiphUpdate(const InBuf; InLen: Integer; const OutBuf;
    var OutLen: Integer; CiphCtx: TCiphContext);
procedure CiphFinal(const OutBuf; var OutLen : Integer; CiphCtx : TCiphContext);

function StrEncBf(const S: AnsiString; const Pwd: AnsiString; IV: PIVector;
    KeyLen: TCipherKeyLen; B64: Boolean): AnsiString;
function StrDecBf(S: AnsiString; const Pwd: AnsiString; IV: PIVector;
    KeyLen: TCipherKeyLen; B64: Boolean): AnsiString;

procedure StreamEncrypt(SrcStream: TStream; DestStream: TStream;
    StrBlkSize: Integer; CiphCtx: TCiphContext; RandomIV: Boolean); overload;
procedure StreamDecrypt(SrcStream: TStream; DestStream: TStream;
    StrBlkSize: Integer; CiphCtx: TCiphContext; RandomIV: Boolean); overload;
procedure StreamEncrypt(SrcStream: TStream; DestStream: TStream;
    StrBlkSize: Integer; CiphCtx: TCiphContext; RandomIV: Boolean;
    Obj: TObject; ProgressCallback : TCryptProgress); overload;
procedure StreamDecrypt(SrcStream: TStream; DestStream: TStream;
    StrBlkSize: Integer; CiphCtx: TCiphContext; RandomIV: Boolean;
    Obj: TObject; ProgressCallback : TCryptProgress); overload;
function sslRootCACertsBundle: string ;


const
{ V8.32 C++ Builder max constant 2048 so split into separate certificates }

{ V8.65 all following constants generated by rootcatool.exe application
   to avoid manual editing errors.

PEM bundle file sslRootCACertsBundle.pem built by Magenta Systems Ltd at 2021-07-06T17:27:09

ICS has three root bundles, whose contents are listed at

http://wiki.overbyte.eu/wiki/index.php/FAQ_ICS_SSL/TLS_CA_Trusted_Store_Contents

The built-in bundle (constants below) is 59 certificates covering all major
commercial SSL/TLS certificate suppliers and Cloud providers, typically used
for REST APIs and similar frameworks.  We welcome suggestions for widely used
but missing root certificates.  Note this root store is automatically used by the
TSslHttpRest and TIcsHttpMulti components, but not by TSslHttpProt or TSslWSocket,
you need to load sslRootCACertsBundle specifically.

The file TrustedCaBundle.pem contains 55 root certificates, adding some regional
and less common suppliers.  You can easily add extra certificates to this
bundle by copying from RootCaCertsBundle.pem, or adding your own private roots.

The file RootCaCertsBundle.pem contains 278 root certificates and is identical
to the Microsoft Windows certificate store covering all regional suppliers.
}

{

PEM bundle file sslRootCACertsBundle.pem built by Magenta Systems Ltd at 2022-05-25T11:50:39

{
Issued to (CN): AAA Certificate Services, (O): Comodo CA Limited
Issuer: Self Signed
Expires: 2028-12-31T23:59:59, Signature: sha1WithRSAEncryption
Valid From: 2004-01-01T00:00:00, Serial Number: 01
Fingerprint (sha256): d7a7a0fb5d7e2731d771e9484ebcdef71d5f0c3e0a2948782bc83ee0ea699ef4
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts001 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEMjCCAxqgAwIBAgIBATANBgkqhkiG9w0BAQUFADB7MQswCQYDVQQGEwJHQjEb'+#13#10+
  'MBkGA1UECAwSR3JlYXRlciBNYW5jaGVzdGVyMRAwDgYDVQQHDAdTYWxmb3JkMRow'+#13#10+
  'GAYDVQQKDBFDb21vZG8gQ0EgTGltaXRlZDEhMB8GA1UEAwwYQUFBIENlcnRpZmlj'+#13#10+
  'YXRlIFNlcnZpY2VzMB4XDTA0MDEwMTAwMDAwMFoXDTI4MTIzMTIzNTk1OVowezEL'+#13#10+
  'MAkGA1UEBhMCR0IxGzAZBgNVBAgMEkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4GA1UE'+#13#10+
  'BwwHU2FsZm9yZDEaMBgGA1UECgwRQ29tb2RvIENBIExpbWl0ZWQxITAfBgNVBAMM'+#13#10+
  'GEFBQSBDZXJ0aWZpY2F0ZSBTZXJ2aWNlczCCASIwDQYJKoZIhvcNAQEBBQADggEP'+#13#10+
  'ADCCAQoCggEBAL5AnfRu4ep2hxxNRUSOvkbIgwadwSr+GB+O5AL686tdUIoWMQua'+#13#10+
  'BtDFcCLNSS1UY8y2bmhGC1Pqy0wkwLxyTurxFa70VJoSCsN6sjNg4tqJVfMiWPPe'+#13#10+
  '3M/vg4aijJRPn2jymJBGhCfHdr/jzDUsi14HZGWCwEiwqJH5YZ92IFCokcdmtet4'+#13#10+
  'YgNW8IoaE+oxox6gmf049vYnMlhvB/VruPsUK6+3qszWY19zjNoFmag4qMsXeDZR'+#13#10+
  'rOme9Hg6jc8P2ULimAyrL58OAd7vn5lJ8S3frHRNG5i1R8XlKdH5kBjHYpy+g8cm'+#13#10+
  'ez6KJcfA3Z3mNWgQIJ2P2N7Sw4ScDV7oL8kCAwEAAaOBwDCBvTAdBgNVHQ4EFgQU'+#13#10+
  'oBEKIz6W8Qfs4q8p74Klf9AwpLQwDgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQF'+#13#10+
  'MAMBAf8wewYDVR0fBHQwcjA4oDagNIYyaHR0cDovL2NybC5jb21vZG9jYS5jb20v'+#13#10+
  'QUFBQ2VydGlmaWNhdGVTZXJ2aWNlcy5jcmwwNqA0oDKGMGh0dHA6Ly9jcmwuY29t'+#13#10+
  'b2RvLm5ldC9BQUFDZXJ0aWZpY2F0ZVNlcnZpY2VzLmNybDANBgkqhkiG9w0BAQUF'+#13#10+
  'AAOCAQEACFb8AvCb6P+k+tZ7xkSAzk/ExfYAWMymtrwUSWgEdujm7l3sAg9g1o1Q'+#13#10+
  'GE8mTgHj5rCl7r+8dFRBv/38ErjHT1r0iWAFf2C3BUrz9vHCv8S5dIa2LX1rzNLz'+#13#10+
  'Rt0vxuBqw8M0Ayx9lt1awg6nCpnBBYurDC/zXDrPbDdVCYfeU0BsWO/8tqtlbgT2'+#13#10+
  'G9w84FoVxp7Z8VlIMCFlA2zs6SFz7JsDoeA3raAVGI/6ugLOpyypEBMs1OUIJqsi'+#13#10+
  'l2D4kF501KKaU73yqWjgom7C12yxow+ev+to51byrvLjKzg6CYG1a4XXvi3tPxq3'+#13#10+
  'smPi9WIsgtRqAEFQ8TmDn5XpNpaYbg=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Amazon Root CA 1, (O): Amazon
Issuer: Self Signed
Expires: 2038-01-17T00:00:00, Signature: sha256WithRSAEncryption
Valid From: 2015-05-26T00:00:00, Serial Number: 066c9fcf99bf8c0a39e2f0788a43e696365bca
Fingerprint (sha256): 8ecde6884f3d87b1125ba31ac3fcb13d7016de7f57cc904fe1cb97c6ae98196e
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts002 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDQTCCAimgAwIBAgITBmyfz5m/jAo54vB4ikPmljZbyjANBgkqhkiG9w0BAQsF'+#13#10+
  'ADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6'+#13#10+
  'b24gUm9vdCBDQSAxMB4XDTE1MDUyNjAwMDAwMFoXDTM4MDExNzAwMDAwMFowOTEL'+#13#10+
  'MAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJv'+#13#10+
  'b3QgQ0EgMTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALJ4gHHKeNXj'+#13#10+
  'ca9HgFB0fW7Y14h29Jlo91ghYPl0hAEvrAIthtOgQ3pOsqTQNroBvo3bSMgHFzZM'+#13#10+
  '9O6II8c+6zf1tRn4SWiw3te5djgdYZ6k/oI2peVKVuRF4fn9tBb6dNqcmzU5L/qw'+#13#10+
  'IFAGbHrQgLKm+a/sRxmPUDgH3KKHOVj4utWp+UhnMJbulHheb4mjUcAwhmahRWa6'+#13#10+
  'VOujw5H5SNz/0egwLX0tdHA114gk957EWW67c4cX8jJGKLhD+rcdqsq08p8kDi1L'+#13#10+
  '93FcXmn/6pUCyziKrlA4b9v7LWIbxcceVOF34GfID5yHI9Y/QCB/IIDEgEw+OyQm'+#13#10+
  'jgSubJrIqg0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMC'+#13#10+
  'AYYwHQYDVR0OBBYEFIQYzIU07LwMlJQuCFmcx7IQTgoIMA0GCSqGSIb3DQEBCwUA'+#13#10+
  'A4IBAQCY8jdaQZChGsV2USggNiMOruYou6r4lK5IpDB/G/wkjUu0yKGX9rbxenDI'+#13#10+
  'U5PMCCjjmCXPI6T53iHTfIUJrU6adTrCC2qJeHZERxhlbI1Bjjt/msv0tadQ1wUs'+#13#10+
  'N+gDS63pYaACbvXy8MWy7Vu33PqUXHeeE6V/Uq2V8viTO96LXFvKWlJbYK8U90vv'+#13#10+
  'o/ufQJVtMVT8QtPHRh8jrdkPSHCa2XV4cdFyQzR1bldZwgJcJmApzyMZFo6IQ6XU'+#13#10+
  '5MsI+yMRQ+hDKXJioaldXgjUkK642M4UwtBV8ob2xJNDd2ZhwLnoQdeXeGADbkpy'+#13#10+
  'rqXRfboQnoZsG4q5WTP468SQvvG5'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Amazon Root CA 2, (O): Amazon
Issuer: Self Signed
Expires: 2040-05-26T00:00:00, Signature: sha384WithRSAEncryption
Valid From: 2015-05-26T00:00:00, Serial Number: 066c9fd29635869f0a0fe58678f85b26bb8a37
Fingerprint (sha256): 1ba5b2aa8c65401a82960118f80bec4f62304d83cec4713a19c39c011ea46db4
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts003 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFQTCCAymgAwIBAgITBmyf0pY1hp8KD+WGePhbJruKNzANBgkqhkiG9w0BAQwF'+#13#10+
  'ADA5MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6'+#13#10+
  'b24gUm9vdCBDQSAyMB4XDTE1MDUyNjAwMDAwMFoXDTQwMDUyNjAwMDAwMFowOTEL'+#13#10+
  'MAkGA1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJv'+#13#10+
  'b3QgQ0EgMjCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAK2Wny2cSkxK'+#13#10+
  'gXlRmeyKy2tgURO8TW0G/LAIjd0ZEGrHJgw12MBvIITplLGbhQPDW9tK6Mj4kHbZ'+#13#10+
  'W0/jTOgGNk3Mmqw9DJArktQGGWCsN0R5hYGCrVo34A3MnaZMUnbqQ523BNFQ9lXg'+#13#10+
  '1dKmSYXpN+nKfq5clU1Imj+uIFptiJXZNLhSGkOQsL9sBbm2eLfq0OQ6PBJTYv9K'+#13#10+
  '8nu+NQWpEjTj82R0Yiw9AElaKP4yRLuH3WUnAnE72kr3H9rN9yFVkE8P7K6C4Z9r'+#13#10+
  '2UXTu/Bfh+08LDmG2j/e7HJV63mjrdvdfLC6HM783k81ds8P+HgfajZRRidhW+me'+#13#10+
  'z/CiVX18JYpvL7TFz4QuK/0NURBs+18bvBt+xa47mAExkv8LV/SasrlX6avvDXbR'+#13#10+
  '8O70zoan4G7ptGmh32n2M8ZpLpcTnqWHsFcQgTfJU7O7f/aS0ZzQGPSSbtqDT6Zj'+#13#10+
  'mUyl+17vIWR6IF9sZIUVyzfpYgwLKhbcAS4y2j5L9Z469hdAlO+ekQiG+r5jqFoz'+#13#10+
  '7Mt0Q5X5bGlSNscpb/xVA1wf+5+9R+vnSUeVC06JIglJ4PVhHvG/LopyboBZ/1c6'+#13#10+
  '+XUyo05f7O0oYtlNc/LMgRdg7c3r3NunysV+Ar3yVAhU/bQtCSwXVEqY0VThUWcI'+#13#10+
  '0u1ufm8/0i2BWSlmy5A5lREedCf+3euvAgMBAAGjQjBAMA8GA1UdEwEB/wQFMAMB'+#13#10+
  'Af8wDgYDVR0PAQH/BAQDAgGGMB0GA1UdDgQWBBSwDPBMMPQFWAJI/TPlUq9LhONm'+#13#10+
  'UjANBgkqhkiG9w0BAQwFAAOCAgEAqqiAjw54o+Ci1M3m9Zh6O+oAA7CXDpO8Wqj2'+#13#10+
  'LIxyh6mx/H9z/WNxeKWHWc8w4Q0QshNabYL1auaAn6AFC2jkR2vHat+2/XcycuUY'+#13#10+
  '+gn0oJMsXdKMdYV2ZZAMA3m3MSNjrXiDCYZohMr/+c8mmpJ5581LxedhpxfL86kS'+#13#10+
  'k5Nrp+gvU5LEYFiwzAJRGFuFjWJZY7attN6a+yb3ACfAXVU3dJnJUH/jWS5E4ywl'+#13#10+
  '7uxMMne0nxrpS10gxdr9HIcWxkPo1LsmmkVwXqkLN1PiRnsn/eBG8om3zEK2yygm'+#13#10+
  'btmlyTrIQRNg91CMFa6ybRoVGld45pIq2WWQgj9sAq+uEjonljYE1x2igGOpm/Hl'+#13#10+
  'urR8FLBOybEfdF849lHqm/osohHUqS0nGkWxr7JOcQ3AWEbWaQbLU8uz/mtBzUF+'+#13#10+
  'fUwPfHJ5elnNXkoOrJupmHN5fLT0zLm4BwyydFy4x2+IoZCn9Kr5v2c69BoVYh63'+#13#10+
  'n749sSmvZ6ES8lgQGVMDMBu4Gon2nL2XA46jCfMdiyHxtN/kHNGfZQIG6lzWE7OE'+#13#10+
  '76KlXIx3KadowGuuQNKotOrN8I1LOJwZmhsoVLiJkO/KdYE+HvJkJMcYr07/R54H'+#13#10+
  '9jVlpNMKVv/1F2Rs76giJUmTtt8AF9pYfl3uxRuw0dFfIRDH+fO6AgonB8Xx1sfT'+#13#10+
  '4PsJYGw='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Amazon Root CA 3, (O): Amazon
Issuer: Self Signed
Expires: 2040-05-26T00:00:00, Signature: ecdsa-with-SHA256
Valid From: 2015-05-26T00:00:00, Serial Number: 066c9fd5749736663f3b0b9ad9e89e7603f24a
Fingerprint (sha256): 18ce6cfe7bf14e60b2e347b8dfe868cb31d02ebb3ada271569f50343b46db3a4
Public Key: ECDSA Key Encryption prime256v1 256 bits, 128 security bits }

sslRootCACerts004 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIBtjCCAVugAwIBAgITBmyf1XSXNmY/Owua2eiedgPySjAKBggqhkjOPQQDAjA5'+#13#10+
  'MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6b24g'+#13#10+
  'Um9vdCBDQSAzMB4XDTE1MDUyNjAwMDAwMFoXDTQwMDUyNjAwMDAwMFowOTELMAkG'+#13#10+
  'A1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJvb3Qg'+#13#10+
  'Q0EgMzBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABCmXp8ZBf8ANm+gBG1bG8lKl'+#13#10+
  'ui2yEujSLtf6ycXYqm0fc4E7O5hrOXwzpcVOho6AF2hiRVd9RFgdszflZwjrZt6j'+#13#10+
  'QjBAMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0PAQH/BAQDAgGGMB0GA1UdDgQWBBSr'+#13#10+
  'ttvXBp43rDCGB5Fwx5zEGbF4wDAKBggqhkjOPQQDAgNJADBGAiEA4IWSoxe3jfkr'+#13#10+
  'BqWTrBqYaGFy+uGh0PsceGCmQ5nFuMQCIQCcAu/xlJyzlvnrxir4tiz+OpAUFteM'+#13#10+
  'YyRIHN8wfdVoOw=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Amazon Root CA 4, (O): Amazon
Issuer: Self Signed
Expires: 2040-05-26T00:00:00, Signature: ecdsa-with-SHA384
Valid From: 2015-05-26T00:00:00, Serial Number: 066c9fd7c1bb104c2943e5717b7b2cc81ac10e
Fingerprint (sha256): e35d28419ed02025cfa69038cd623962458da5c695fbdea3c22b0bfb25897092
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts005 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIB8jCCAXigAwIBAgITBmyf18G7EEwpQ+Vxe3ssyBrBDjAKBggqhkjOPQQDAzA5'+#13#10+
  'MQswCQYDVQQGEwJVUzEPMA0GA1UEChMGQW1hem9uMRkwFwYDVQQDExBBbWF6b24g'+#13#10+
  'Um9vdCBDQSA0MB4XDTE1MDUyNjAwMDAwMFoXDTQwMDUyNjAwMDAwMFowOTELMAkG'+#13#10+
  'A1UEBhMCVVMxDzANBgNVBAoTBkFtYXpvbjEZMBcGA1UEAxMQQW1hem9uIFJvb3Qg'+#13#10+
  'Q0EgNDB2MBAGByqGSM49AgEGBSuBBAAiA2IABNKrijdPo1MN/sGKe0uoe0ZLY7Bi'+#13#10+
  '9i0b2whxIdIA6GO9mif78DluXeo9pcmBqqNbIJhFXRbb/egQbeOc4OO9X4Ri83Bk'+#13#10+
  'M6DLJC9wuoihKqB1+IGuYgbEgds5bimwHvouXKNCMEAwDwYDVR0TAQH/BAUwAwEB'+#13#10+
  '/zAOBgNVHQ8BAf8EBAMCAYYwHQYDVR0OBBYEFNPsxzplbszh2naaVvuc84ZtV+WB'+#13#10+
  'MAoGCCqGSM49BAMDA2gAMGUCMDqLIfG9fhGt0O9Yli/W651+kI0rz2ZVwyzjKKlw'+#13#10+
  'CkcO8DdZEv8tmZQoTipPNU0zWgIxAOp1AE47xDqUEpHJWEadIRNyp4iciuRMStuW'+#13#10+
  '1KyLa2tJElMzrdfkviT8tQp21KW8EA=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Baltimore CyberTrust Root, (O): Baltimore, (OU): CyberTrust
Issuer: Self Signed
Expires: 2025-05-12T23:59:00, Signature: sha1WithRSAEncryption
Valid From: 2000-05-12T18:46:00, Serial Number: 020000b9
Fingerprint (sha256): 16af57a9f676b0ab126095aa5ebadef22ab31119d644ac95cd4b93dbf3f26aeb
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts006 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDdzCCAl+gAwIBAgIEAgAAuTANBgkqhkiG9w0BAQUFADBaMQswCQYDVQQGEwJJ'+#13#10+
  'RTESMBAGA1UEChMJQmFsdGltb3JlMRMwEQYDVQQLEwpDeWJlclRydXN0MSIwIAYD'+#13#10+
  'VQQDExlCYWx0aW1vcmUgQ3liZXJUcnVzdCBSb290MB4XDTAwMDUxMjE4NDYwMFoX'+#13#10+
  'DTI1MDUxMjIzNTkwMFowWjELMAkGA1UEBhMCSUUxEjAQBgNVBAoTCUJhbHRpbW9y'+#13#10+
  'ZTETMBEGA1UECxMKQ3liZXJUcnVzdDEiMCAGA1UEAxMZQmFsdGltb3JlIEN5YmVy'+#13#10+
  'VHJ1c3QgUm9vdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKMEuyKr'+#13#10+
  'mD1X6CZymrV51Cni4eiVgLGw41uOKymaZN+hXe2wCQVt2yguzmKiYv60iNoS6zjr'+#13#10+
  'IZ3AQSsBUnuId9Mcj8e6uYi1agnnc+gRQKfRzMpijS3ljwumUNKoUMMo6vWrJYeK'+#13#10+
  'mpYcqWe4PwzV9/lSEy/CG9VwcPCPwBLKBsua4dnKM3p31vjsufFoREJIE9LAwqSu'+#13#10+
  'XmD+tqYF/LTdB1kC1FkYmGP1pWPgkAx9XbIGevOF6uvUA65ehD5f/xXtabz5OTZy'+#13#10+
  'dc93Uk3zyZAsuT3lySNTPx8kmCFcB5kpvcY67Oduhjprl3RjM71oGDHweI12v/ye'+#13#10+
  'jl0qhqdNkNwnGjkCAwEAAaNFMEMwHQYDVR0OBBYEFOWdWTCCR1jMrPoIVDaGezq1'+#13#10+
  'BE3wMBIGA1UdEwEB/wQIMAYBAf8CAQMwDgYDVR0PAQH/BAQDAgEGMA0GCSqGSIb3'+#13#10+
  'DQEBBQUAA4IBAQCFDF2O5G9RaEIFoN27TyclhAO992T9Ldcw46QQF+vaKSm2eT92'+#13#10+
  '9hkTI7gQCvlYpNRhcL0EYWoSihfVCr3FvDB81ukMJY2GQE/szKN+OMY3EU/t3Wgx'+#13#10+
  'jkzSswF07r51XgdIGn9w/xZchMB5hbgF/X++ZRGjD8ACtPhSNzkE1akxehi/oCr0'+#13#10+
  'Epn3o0WC4zxe9Z2etciefC7IpJ5OCBRLbf1wbWsaY71k5h+3zvDyny67G7fyUIhz'+#13#10+
  'ksLi4xaNmjICq44Y3ekQEe5+NauQrz4wlHrQMz2nZQ/1/I6eYs9HRCwBXbsdtTLS'+#13#10+
  'R9I4LtD+gdwyah617jzV/OeBHRnDJELqYzmp'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): COMODO Certification Authority, (O): COMODO CA Limited
Issuer: Self Signed
Expires: 2030-12-31T23:59:59, Signature: sha1WithRSAEncryption
Valid From: 2011-01-01T00:00:00, Serial Number: 20a4c47fdddfe1c75363071388776012
Fingerprint (sha256): 1a0d20445de5ba1862d19ef880858cbce50102b36e8f0a040c3c69e74522fe6e
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts007 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIID0DCCArigAwIBAgIQIKTEf93f4cdTYwcTiHdgEjANBgkqhkiG9w0BAQUFADCB'+#13#10+
  'gTELMAkGA1UEBhMCR0IxGzAZBgNVBAgTEkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4G'+#13#10+
  'A1UEBxMHU2FsZm9yZDEaMBgGA1UEChMRQ09NT0RPIENBIExpbWl0ZWQxJzAlBgNV'+#13#10+
  'BAMTHkNPTU9ETyBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTAeFw0xMTAxMDEwMDAw'+#13#10+
  'MDBaFw0zMDEyMzEyMzU5NTlaMIGBMQswCQYDVQQGEwJHQjEbMBkGA1UECBMSR3Jl'+#13#10+
  'YXRlciBNYW5jaGVzdGVyMRAwDgYDVQQHEwdTYWxmb3JkMRowGAYDVQQKExFDT01P'+#13#10+
  'RE8gQ0EgTGltaXRlZDEnMCUGA1UEAxMeQ09NT0RPIENlcnRpZmljYXRpb24gQXV0'+#13#10+
  'aG9yaXR5MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA0ECLi3LjkRv3'+#13#10+
  'UcEbVASY06m/weaKXTuH+7uIzg3jLz8GlvCiKVCZrts7oVewdFFxze1CkU1B/qnI'+#13#10+
  '2GqGd0S7WWaXUF601CxwRM/aN5VCaTwwxHGzUvAhTaHYujl8HJ6jJJ3ygxaYqhZ8'+#13#10+
  'Q5sVW7euNJH+1GImGEaaP+vB+fGQV+useg2L23IwambV4EajcNxo2f8ESIl33rXp'+#13#10+
  '+2dtQem8Ob0y2WIC8bGoPW43nOIv4tOiJovGuFVDiOEjPqXSJDlqR6sA1KGzqSX+'+#13#10+
  'DT+nHbrTUcELpNqsOO9VUCQFZUaTNE8tja3G1CEZ0o7KBWFxB3NH5YoZEr0ETc5O'+#13#10+
  'nKVIrLsm9wIDAQABo0IwQDAdBgNVHQ4EFgQUC1jli8ZMFTekQKkwqSG+RzZaVv8w'+#13#10+
  'DgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEFBQAD'+#13#10+
  'ggEBAC/JxBwHO89hAgCx2SFRdXIDMLDEFh9sAIsQrK/xR9SuEDwMGvjUk2ysEDd8'+#13#10+
  't6aDZK3N3w6HM503sMZ7OHKx8xoOo/lVem0DZgMXlUrxsXrfViEGQo+x06iF3u6X'+#13#10+
  'HWLrp+cxEmbDD6ZLLkGC9/3JG6gbr+48zuOcrigHoSybJMIPIyaDMouGDx8rEkYl'+#13#10+
  'Fo92kANr3ryqImhrjKGsKxE5pttwwn1y6TPn/CbxdFqR5p2ErPioBhlG5qfpqjQi'+#13#10+
  'pKGfeq23sqSaM4hxAjwu1nqyH6LKwN0vEJT9s4yEIHlG1QXUEOTS22RPuFvuG8Ug'+#13#10+
  'R1uUq27UlTMdphVx8fiUylQ5PsE='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): COMODO ECC Certification Authority, (O): COMODO CA Limited
Issuer: Self Signed
Expires: 2038-01-18T23:59:59, Signature: ecdsa-with-SHA384
Valid From: 2008-03-06T00:00:00, Serial Number: 1f47afaa62007050544c019e9b63992a
Fingerprint (sha256): 1793927a0614549789adce2f8f34f7f0b66d0f3ae3a3b84d21ec15dbba4fadc7
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts008 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICiTCCAg+gAwIBAgIQH0evqmIAcFBUTAGem2OZKjAKBggqhkjOPQQDAzCBhTEL'+#13#10+
  'MAkGA1UEBhMCR0IxGzAZBgNVBAgTEkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4GA1UE'+#13#10+
  'BxMHU2FsZm9yZDEaMBgGA1UEChMRQ09NT0RPIENBIExpbWl0ZWQxKzApBgNVBAMT'+#13#10+
  'IkNPTU9ETyBFQ0MgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDgwMzA2MDAw'+#13#10+
  'MDAwWhcNMzgwMTE4MjM1OTU5WjCBhTELMAkGA1UEBhMCR0IxGzAZBgNVBAgTEkdy'+#13#10+
  'ZWF0ZXIgTWFuY2hlc3RlcjEQMA4GA1UEBxMHU2FsZm9yZDEaMBgGA1UEChMRQ09N'+#13#10+
  'T0RPIENBIExpbWl0ZWQxKzApBgNVBAMTIkNPTU9ETyBFQ0MgQ2VydGlmaWNhdGlv'+#13#10+
  'biBBdXRob3JpdHkwdjAQBgcqhkjOPQIBBgUrgQQAIgNiAAQDR3svdcmCFYX7deSR'+#13#10+
  'FtSrYpn1PlILBs5BAH+X4QokPB0BBO490o0JlwzgdeT6+3eKKvUDYEs2ixYjFq0J'+#13#10+
  'cfRK9ChQtP6IHG4/bC8vCVlbpVsLM5niwz2J+Wos77LTBumjQjBAMB0GA1UdDgQW'+#13#10+
  'BBR1cacZSBm8nZ3qQUfflMRId5nTeTAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/'+#13#10+
  'BAUwAwEB/zAKBggqhkjOPQQDAwNoADBlAjEA7wNbeqy3eApyt4jf/7VGFAkK+qDm'+#13#10+
  'fQjGGoe9GKhzvSbKYAydzpmfz1wPMOG+FDHqAjAU9JM8SaczepBGR7NjfRObTrdv'+#13#10+
  'GDeAU/7dIOA1mjbRxwG55tzd8/8dLDoWV9mSOdY='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): COMODO RSA Certification Authority, (O): COMODO CA Limited
Issuer: Self Signed
Expires: 2038-01-18T23:59:59, Signature: sha384WithRSAEncryption
Valid From: 2010-01-19T00:00:00, Serial Number: 4caaf9cadb636fe01ff74ed85b03869d
Fingerprint (sha256): 52f0e1c4e58ec629291b60317f074671b85d7ea80d5b07273463534b32b40234
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts009 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIF2DCCA8CgAwIBAgIQTKr5yttjb+Af907YWwOGnTANBgkqhkiG9w0BAQwFADCB'+#13#10+
  'hTELMAkGA1UEBhMCR0IxGzAZBgNVBAgTEkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4G'+#13#10+
  'A1UEBxMHU2FsZm9yZDEaMBgGA1UEChMRQ09NT0RPIENBIExpbWl0ZWQxKzApBgNV'+#13#10+
  'BAMTIkNPTU9ETyBSU0EgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMTAwMTE5'+#13#10+
  'MDAwMDAwWhcNMzgwMTE4MjM1OTU5WjCBhTELMAkGA1UEBhMCR0IxGzAZBgNVBAgT'+#13#10+
  'EkdyZWF0ZXIgTWFuY2hlc3RlcjEQMA4GA1UEBxMHU2FsZm9yZDEaMBgGA1UEChMR'+#13#10+
  'Q09NT0RPIENBIExpbWl0ZWQxKzApBgNVBAMTIkNPTU9ETyBSU0EgQ2VydGlmaWNh'+#13#10+
  'dGlvbiBBdXRob3JpdHkwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQCR'+#13#10+
  '6FSS0gpWsawNJN3Fz0RndJkrN6N9I3AAcbxT38T6KhKPS38QVr2fcHK3YX/JSw8X'+#13#10+
  'pz3jsARh7v8Rl8f0hj4K+j5c+ZPmNHrZFGvnnLOFoIJ6dq9xkNfs/Q36nGz637CC'+#13#10+
  '9BR++b7Epi9Pf5l/tfxnQ3K9DADWietrLNPtj5gcFKt+5eNu/Nio5JIk2kNrYrhV'+#13#10+
  '/erBvGy2i/MOjZrkm2xpmfh4SDBF1a3hDTxFYPwyllEnvGfDyi62a+pGx8cgoLEf'+#13#10+
  'Zd5ICLqkTqnyg0Y3hOvozIFIQ2dOciqbXL1MGyiKXCJ7tKuY2e7gUYPDCUZObT6Z'+#13#10+
  '+pUX2nwzV0E8jVHtC7ZcryxjGt9XyD+86V3Em69FmeKjWiS0uqlWPc9vqv9JWL7w'+#13#10+
  'qP/0uK3pN/u6uPQLOvnoQ0IeidiEyxPx2bvhiWC4jChWrBQdnArncevPDt09qZah'+#13#10+
  'SL0896+1DSJMwBGB7FY79tOi4lu3sgQiUpWAk2nojkxl8ZEDLXB0AuqLZxUpaVIC'+#13#10+
  'u9ffUGpVRr+goyhhf3DQw6KqLCGqR84onAZFdr+CGCe01a60y1Dma/RMhnEw6abf'+#13#10+
  'Fobg2P9A3fvQQoh/ozM6LlweQRGBY84YcWsr7KaKtzFcOmpH4MN5WdYgGq/yapiq'+#13#10+
  'crxXStJLnbsQ/LBMQeXtHT1eKJ2czL+zUdqnR+WEUwIDAQABo0IwQDAdBgNVHQ4E'+#13#10+
  'FgQUu69+Aj36pvE8hI6t7jiY7NkyMtQwDgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB'+#13#10+
  '/wQFMAMBAf8wDQYJKoZIhvcNAQEMBQADggIBAArx1UaEt65Ru2yyTUEUAJNMnMvl'+#13#10+
  'wFTPoCWOAvn9sKIN9SCYPBMtrFaisNZ+EZLpLrqeLppysb0ZRGxhNaKatBYSaVqM'+#13#10+
  '4dc+pBroLwP0rmEdEBsqpIt6xf4FpuHA1sj+nq6PK7o9mfjYcwlYRm6mnPTXJ9OV'+#13#10+
  '2jeDchzTc+CiR5kDOF3VSXkAKRzH7JsgHAckaVd4sjn8OoSgtZx8jb8uk2Intzna'+#13#10+
  'FxiuvTwJaP+EmzzV1gsD41eeFPfR60/IvYcjt7ZJQ3mFXLrrkguhxuhoqEwWsRqZ'+#13#10+
  'CuhTLJK7oQkYdQxlqHvLI7cawiiFwxv/0Cti76R7CZGYZ4wUAc1oBmpjIXUDgIiK'+#13#10+
  'boHGhfKppC3n9KUkEEeDys30jXlYsQab5xoq2Z0B15R97QNKyvDb6KkBPvVWmcke'+#13#10+
  'jkk9u+UJueBPSZI9FoJAzMxZxuY67RIuaTxslbH9qh17f4a+Hg4yRvv7E491f0yL'+#13#10+
  'S0Zj/gA0QHDBw7mh3aZw4gSzQbzpgJHqZJx64SIDqZxubw5lT2yHh17zbqD5daWb'+#13#10+
  'QOhTsiedSrnAdyGN/4fy3ryM7xfft0kL0fJuMAsaDk527RH89elWsn2/x20Kk4yl'+#13#10+
  '0MC2Hb46TpSi125sC8KKfPog88Tk5c0NqMuRkrF8hey1FGlmDoLnzc7ILaZRfyHB'+#13#10+
  'NVOFBkpdn627G190'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Assured ID Root CA, (O): DigiCert Inc, (OU): www.digicert.com
Issuer: Self Signed
Expires: 2031-11-10T00:00:00, Signature: sha1WithRSAEncryption
Valid From: 2006-11-10T00:00:00, Serial Number: 0ce7e0e517d846fe8fe560fc1bf03039
Fingerprint (sha256): 3e9099b5015e8f486c00bcea9d111ee721faba355a89bcf1df69561e3dc6325c
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts010 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDtzCCAp+gAwIBAgIQDOfg5RfYRv6P5WD8G/AwOTANBgkqhkiG9w0BAQUFADBl'+#13#10+
  'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3'+#13#10+
  'd3cuZGlnaWNlcnQuY29tMSQwIgYDVQQDExtEaWdpQ2VydCBBc3N1cmVkIElEIFJv'+#13#10+
  'b3QgQ0EwHhcNMDYxMTEwMDAwMDAwWhcNMzExMTEwMDAwMDAwWjBlMQswCQYDVQQG'+#13#10+
  'EwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3d3cuZGlnaWNl'+#13#10+
  'cnQuY29tMSQwIgYDVQQDExtEaWdpQ2VydCBBc3N1cmVkIElEIFJvb3QgQ0EwggEi'+#13#10+
  'MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCtDhXO5EOAXLGH87dg+XESpa7c'+#13#10+
  'JpSIqvTO9SA5KFhgDPiA2qkVlTJhPLWxKISKityfCgyDF3qPkKyK53lTXDGEKvYP'+#13#10+
  'mDI2dsze3Tyoou9q+yHyUmHfnyDXH+Kx2f4YZNISW1/5WBg1vEfNoTb5a3/UsDg+'+#13#10+
  'wRvDjDPZ2C8Y/igPs6eD1sNuRMBhNZYW/lmci3Zt1/GiSw0r/wty2p5g0I6QNcZ4'+#13#10+
  'VYcgoc/lbQrISXwxmDNsIumH0DJaoroTghHtORedmTpyoeb6pNnVFzF1roV9Iq4/'+#13#10+
  'AUaG9ih5yLHa5FcXxH4cDrC0kqZWs72yl+2qp/C3xag/lRbQ/6GW6whfGHdPAgMB'+#13#10+
  'AAGjYzBhMA4GA1UdDwEB/wQEAwIBhjAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQW'+#13#10+
  'BBRF66Kv9JLLgjEtUYunpyGd823IDzAfBgNVHSMEGDAWgBRF66Kv9JLLgjEtUYun'+#13#10+
  'pyGd823IDzANBgkqhkiG9w0BAQUFAAOCAQEAog683+Lt8ONyc3pklL/3cmbYMuRC'+#13#10+
  'dWKuh+vy1dneVrOfzM4UKLkNl2BcEkxY5NM9g0lFWJc1aRqoR+pWxnmrEthngYTf'+#13#10+
  'fwk8lOa4JiwgvT2zKIn3X/8i4peEH+ll74fg38FnSbNd67IJKusm7Xi+fT8r87cm'+#13#10+
  'NW1fiQG2SVufAQWbqz0lwcy2f8Lxb4bG+mRo64EtlOtCt/qMHt1i8b5QZ7dsvfPx'+#13#10+
  'H2sMNgcWfzd8qVttevESRmCD1ycEvkvOl77DZypoEd+A5wwzZr8TDRRu838fYxAe'+#13#10+
  '+o0bJW1sj6W3YQGx0qMmoRBxna3iw/nDmVG3KwcIzi7mULKn+gpFL6Lw8g=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert CS ECC P384 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: ecdsa-with-SHA384
Valid From: 2021-01-15T00:00:00, Serial Number: 03698fe712d519f3ced0fdb7b1643011
Fingerprint (sha256): 26c56ad2208d1e9b152f66853bf4797cbeb7552c1f3f477251e8cb1ae7e797bf
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts011 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICFjCCAZ2gAwIBAgIQA2mP5xLVGfPO0P23sWQwETAKBggqhkjOPQQDAzBNMQsw'+#13#10+
  'CQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xJTAjBgNVBAMTHERp'+#13#10+
  'Z2lDZXJ0IENTIEVDQyBQMzg0IFJvb3QgRzUwHhcNMjEwMTE1MDAwMDAwWhcNNDYw'+#13#10+
  'MTE0MjM1OTU5WjBNMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIElu'+#13#10+
  'Yy4xJTAjBgNVBAMTHERpZ2lDZXJ0IENTIEVDQyBQMzg0IFJvb3QgRzUwdjAQBgcq'+#13#10+
  'hkjOPQIBBgUrgQQAIgNiAAR/FK2Ftpf9AiE1TWDoOJOTmz0FEG2v0/7v+rv7c5nz'+#13#10+
  '7DISjcdouIveiaKIVHeNuyF+M5VWlgno1YyhBLibbhkAYuhCKKZYN4QZVSZ7Mzdn'+#13#10+
  '8ppyraGurgBCPBx+uHqeIZyjQjBAMB0GA1UdDgQWBBTwjJhxOThlwjobphdmHcjt'+#13#10+
  'Zd6SNjAOBgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAKBggqhkjOPQQD'+#13#10+
  'AwNnADBkAjAjb+EAGSZQ5EYgZYs3p8/rBuHMMskqoewyDXOiHgIcNWEqTmmrOXft'+#13#10+
  'l4jAfWvqid0CMEPx0VijdT6Gm7ZVEYsX9z3+CmnFf07GdRtalMvqERHGCCKI3tB6'+#13#10+
  'oqV56OMhp80Tsw=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert CS RSA4096 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: sha384WithRSAEncryption
Valid From: 2021-01-15T00:00:00, Serial Number: 06cee131be6d55c807f7c0c7fb44e620
Fingerprint (sha256): 7353b6d6c2d6da4247773f3f07d075decb5134212bead0928ef1f46115260941
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts012 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFZDCCA0ygAwIBAgIQBs7hMb5tVcgH98DH+0TmIDANBgkqhkiG9w0BAQwFADBM'+#13#10+
  'MQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xJDAiBgNVBAMT'+#13#10+
  'G0RpZ2lDZXJ0IENTIFJTQTQwOTYgUm9vdCBHNTAeFw0yMTAxMTUwMDAwMDBaFw00'+#13#10+
  'NjAxMTQyMzU5NTlaMEwxCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5EaWdpQ2VydCwg'+#13#10+
  'SW5jLjEkMCIGA1UEAxMbRGlnaUNlcnQgQ1MgUlNBNDA5NiBSb290IEc1MIICIjAN'+#13#10+
  'BgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAtjNzgNhiA3AULBEcOV58rnyDhh3+'+#13#10+
  'Ji9MJK2L6oNfqbw9W/wLmEwCRzDs4v7s6DRbZl6/O9cspiX/jFmz3+rafCnZRlBy'+#13#10+
  'CB1u0RsK3R/NmYn6Dw9zxOGcHXUyzW+X2ipqlbJsyQnQ6gt7fRcGSZnv1t7gyFPU'+#13#10+
  'rsZ38Ya7Ixy4wN9Z94590e+C5iaLWji1/3XVstlPCfM3iFDaEaSKFBTRUwQAffNq'+#13#10+
  'RBj+UHAyBxyomg46HcUKH24LJmm3PKJXcCyG+kxulalYQ7msEtb/P+3XQxdrTM6e'+#13#10+
  'xJCr//oQUJqjkFfW54wQrp8WGs81HX/Xdu2KnDWnKLinXSH8MDfd3ggZTxXG56ba'+#13#10+
  'kEeO95RTTI5TAr79meXqhtCvAwLTm6qT8asojiAB/0z7zLcpQPWHpBITBR9DbtdR'+#13#10+
  'UJ84tCDtFwkSj8y5Ga+fzb5pEdOvVRBtF4Z5llLGsgCd5a84sDX0iGuPDgQ9fO6v'+#13#10+
  'zdNqEErGzYbKIj2hSlz7Dv+I31xip8C5HtmsbH44N/53kyXChYpPtTcGWgaBFPHO'+#13#10+
  'lJ2ZkeoyWs5nPW4EZq0MTy2jLvee9Xid9wr9fo/jQopVlrzxnzct/J5flf6MGBv8'+#13#10+
  'jv1LkK/XA2gSY6zik6eiywTlT2TOA/rGFJ/Zi+jM1GKMa+QALBmfGgbGMYFU+1Mk'+#13#10+
  'mq9Vmbqdda64wt0CAwEAAaNCMEAwHQYDVR0OBBYEFGgBk7HSSkBCaZRGLBxaiKkl'+#13#10+
  'tEdPMA4GA1UdDwEB/wQEAwIBhjAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEB'+#13#10+
  'DAUAA4ICAQCS/O64AnkXAlF9IcVJZ6ek8agkOOsMaOpaQmuc9HPBaUotszcFUEKY'+#13#10+
  'kp4GeSwuBpn2798roM2zkgGDtaDLJ7U8IxqYSaLsLZmlWUOs0rGT1lfXHLyT1sZA'+#13#10+
  '4bNvGVW3E9flQzOktavL2sExZA101iztw41u67uvGUdhYS3A9AW5b3jcOvdCQGVT'+#13#10+
  'kb2ZDZOSVKapN1krm8uZxrw99wSE8JQzHQ+CWjnLLkXDKBmjspuYyPwxa2CP9umG'+#13#10+
  'KLzgPH10XRaJW2kkxxCLxEu7Nk/UWT/DsKSRmfgu0UoBnfWIEu+/WhFqWU9Za1pn'+#13#10+
  '84+0Ew/A2C89KHKqGX8RfWpbn5XnX7eUT/E+oVr/Lcyd3yd3jzJzHGcKdvP6XLG/'+#13#10+
  'vB29DCibsscXZwszD8O9Ntz7ukILq+2Ew2LWhBapsQdrqW7uxs/msEQpwvCzYYAq'+#13#10+
  'i2/SFFwlh1Rk86RMwaH4p2vq/uo6/HnbDo/cxvPJ1Gze6YOhjh0i7Mk6sgB73Dun'+#13#10+
  'Qhp/3IupET2Op8Agb10JXUNE5o9mzKlbB/Hvm3oOs1ThlP0OLMaT11X9cZg1uAlK'+#13#10+
  '/8YpKCz2Ui3bFBiSJ+IWfozK1GG+goeR65g3P79fXXc/NKwbOEOraHKZMh46Ghml'+#13#10+
  'ozhMI9ej58zVKpIXkAtaS70WvfuGauKJmezkoFUYyaMIHxPgMghy0A=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Client ECC P384 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: ecdsa-with-SHA384
Valid From: 2021-01-15T00:00:00, Serial Number: 064fa6a62829141f0e9d8362e1175e3a
Fingerprint (sha256): cd851c5c25467c0095a43927d0b2292d932975e84dce4c5e794542c6574c3aa4
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts013 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICHzCCAaWgAwIBAgIQBk+mpigpFB8OnYNi4RdeOjAKBggqhkjOPQQDAzBRMQsw'+#13#10+
  'CQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xKTAnBgNVBAMTIERp'+#13#10+
  'Z2lDZXJ0IENsaWVudCBFQ0MgUDM4NCBSb290IEc1MB4XDTIxMDExNTAwMDAwMFoX'+#13#10+
  'DTQ2MDExNDIzNTk1OVowUTELMAkGA1UEBhMCVVMxFzAVBgNVBAoTDkRpZ2lDZXJ0'+#13#10+
  'LCBJbmMuMSkwJwYDVQQDEyBEaWdpQ2VydCBDbGllbnQgRUNDIFAzODQgUm9vdCBH'+#13#10+
  'NTB2MBAGByqGSM49AgEGBSuBBAAiA2IABGccPoSdUFeo//Ftm0qfjw07rsbG8eqB'+#13#10+
  'zMO1usko/cGp2vZs9iBCAYRKWmL9DF88W3kWed3X5delJjH1ZMbzJL/19kUvIYZt'+#13#10+
  '83aXZQUguq+5kcUq7pjF/pcb86fx59LPrqNCMEAwHQYDVR0OBBYEFLMsqpCF7XM8'+#13#10+
  '9c7QwUtG0sVgbZR5MA4GA1UdDwEB/wQEAwIBhjAPBgNVHRMBAf8EBTADAQH/MAoG'+#13#10+
  'CCqGSM49BAMDA2gAMGUCMA+xAdFskP8ICqhlLebjF8aVdClLedTB64pmR9WrI38F'+#13#10+
  'XyW4HOyOxz2nObN2wJVzdAIxAPxsGdjNe5nR/lSqYzVCmsBe59u5HcnCny2HfswZ'+#13#10+
  'Nm9hi+opTmtpPNJloAPNqSXojA=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Client RSA4096 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: sha384WithRSAEncryption
Valid From: 2021-01-15T00:00:00, Serial Number: 04c8fc03a854eb98a09b02883c66a3c0
Fingerprint (sha256): 3af21466920fa91fa31f4331e445c16be2330024bfd2c239c0d85ab530714766
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts014 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFbDCCA1SgAwIBAgIQBMj8A6hU65igmwKIPGajwDANBgkqhkiG9w0BAQwFADBQ'+#13#10+
  'MQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xKDAmBgNVBAMT'+#13#10+
  'H0RpZ2lDZXJ0IENsaWVudCBSU0E0MDk2IFJvb3QgRzUwHhcNMjEwMTE1MDAwMDAw'+#13#10+
  'WhcNNDYwMTE0MjM1OTU5WjBQMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNl'+#13#10+
  'cnQsIEluYy4xKDAmBgNVBAMTH0RpZ2lDZXJ0IENsaWVudCBSU0E0MDk2IFJvb3Qg'+#13#10+
  'RzUwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDj5uj0wpUboAFRLX+I'+#13#10+
  'QSuQu82uUBQ2Z5vvcB6iQP+eAEwpG0yWS14MOh8mMifsddzfgmhVsFKbtnxboguE'+#13#10+
  '15i2LlAWtXz5u3f6Mz+krrB2VW1VGG6fF4Yk9EuUMIZyhTeVT8H5rCjRgcu/MWOF'+#13#10+
  'eTMCNOD4kT5fcb49xWdRQT4iE3MYDXNRFd5odImBB2yZhmksQQ6Vl/MWA1ZQgWqW'+#13#10+
  '2YbSp1DvlXT+AgPJUb2DvrAA/Rt6I3PfR5ux4sgA6U92sicwkcq696rhA2an39jj'+#13#10+
  '80pCMcwHQ/WouQXqhCf5e0QocgAv8+rzif5MgcFElEk6C1PJAU9nl30lyeDfCmE+'+#13#10+
  'Zoja1fhUoqtUsdXc/iSCZrS3giGWY6KTt7Nwh6EgNPr6WF0vexfIH1y2lDVgw5q7'+#13#10+
  'jiL4prUp3Cbe51dJgU1kue/CPIgLTMwBmNJYLVJEOrPOwz6+zrnDS+XRUbbhMdEy'+#13#10+
  'R0HPwJ+Wk1tl55NGtYsVlAJSurMuYlkHtDYFkVNec4Ho/7lMdo/ak3PArTgX7uf4'+#13#10+
  'owW0fV9gNpwqoVBIfhUeRXn4IFEF9uxiHWrRLw2L73RO8aNsNu4TRrE/CIIyu7n0'+#13#10+
  'YLmR4delkuCVoEGFFdjM0Y2htyQy7WOSRbDqB+jkdCDxJrlj9cLHbY2xVuheRRyM'+#13#10+
  'vNFAj2LuSb00sKKWwTkd/M1l7wIDAQABo0IwQDAdBgNVHQ4EFgQUk2078qYZI73o'+#13#10+
  'uEyOuc/V/vzEvXowDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQFMAMBAf8wDQYJ'+#13#10+
  'KoZIhvcNAQEMBQADggIBAJf7rW+sE2WxsOHVVpktlQqqgbVwgROdfZfVRyH+127F'+#13#10+
  'RTQA63zRgbUkkfsLmn7eYl+OUMT7RUYdU0vWsBnhj6GFc4V4lgG0uOg70K+Qa5Lv'+#13#10+
  'fPZnUjFmF55q4se9VUqSD2a0RQYAzJu2QQAsCkAKdLbYSsF0KKE1KCS11q7FjpQJ'+#13#10+
  'APog8EoWRf+HcFf6MyqaA+awMwy4vsOWO06jolechshjIte2FoLwOHF1j4uK4TUQ'+#13#10+
  'twrG651sWHJdC2jEq2XlS9I7gKqHn1ikGiKsq4pRyVN2hmIuLu8vVKfD1XLwoCxf'+#13#10+
  'f3T0i+QnCziTjdh5GO6rqvQK9MF5kjpFXnJkNqWaTl0y3GFFNDo3Lo8CtIC2DoVH'+#13#10+
  'pILSqMpngrkjnuNNENi3xq6YXBV2oyyHhggwDW9u1Bb2yokrgF1cXdoLfo+811E4'+#13#10+
  'oz3688ZMCpdmhHObOXSf+z1qwJvC4anrv7Lrl2Eb+d5G9lXiKwy0Z+hcVhDWuTl8'+#13#10+
  '4q0yIbA1IbrW0cRQAx2xAEc3u3ZAITXwLmXbUsDpttcIYNqOiVHYeK/7aAzRvkmf'+#13#10+
  'ICdRoooBEuLAf+CN0+sxBeYlYUKIAONWBaBDF01dFTkRr1Ebb/Xe38SzyyAeWIVg'+#13#10+
  'ZAcMGDpl6+ohzswf6wSVPAxNqDe1t8GeWAM1oDzRSTALzohNU1kgK5YqxhXHkA5A'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert ECC P384 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: ecdsa-with-SHA384
Valid From: 2021-01-15T00:00:00, Serial Number: 0df3d93765a379c59566ea92e2244f34
Fingerprint (sha256): c1468cf2254e6004b24696aba209d1a30ba6e2dff68a9a4e32c6ab414f90c8d9
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts015 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICETCCAZegAwIBAgIQDfPZN2WjecWVZuqS4iRPNDAKBggqhkjOPQQDAzBKMQsw'+#13#10+
  'CQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xIjAgBgNVBAMTGURp'+#13#10+
  'Z2lDZXJ0IEVDQyBQMzg0IFJvb3QgRzUwHhcNMjEwMTE1MDAwMDAwWhcNNDYwMTE0'+#13#10+
  'MjM1OTU5WjBKMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4x'+#13#10+
  'IjAgBgNVBAMTGURpZ2lDZXJ0IEVDQyBQMzg0IFJvb3QgRzUwdjAQBgcqhkjOPQIB'+#13#10+
  'BgUrgQQAIgNiAAT8WR/OmWx/mw62KWNvxoXzCtPWm65XFUwO7V3jCX5tKqOGqrp4'+#13#10+
  'oKdxvUT6CMBKBtZv3SxKOHTl0L3/ev/lOU69vRceH0Ot1bwn2Eu/dowwMqT7+VPl'+#13#10+
  '2Ko4U12ooDegZwqjQjBAMB0GA1UdDgQWBBSSlvfmutURuvkiLnt+WtnwJeUFGzAO'+#13#10+
  'BgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAwNoADBl'+#13#10+
  'AjEA/cBN8aSn26cMJhH0Sb0HOGMrRGIGeQjHw9TPmz6rOieqkMf9WaK4MlLbyo4X'+#13#10+
  'CwqQAjBdGuxRidRk3PnlHji9Wy7j5UTkOxh61/CVQI/y68/0+dBlokHysOZ8wTYs'+#13#10+
  'j1453Tc='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Global Root CA, (O): DigiCert Inc, (OU): www.digicert.com
Issuer: Self Signed
Expires: 2031-11-10T00:00:00, Signature: sha1WithRSAEncryption
Valid From: 2006-11-10T00:00:00, Serial Number: 083be056904246b1a1756ac95991c74a
Fingerprint (sha256): 4348a0e9444c78cb265e058d5e8944b4d84f9662bd26db257f8934a443c70161
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts016 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDrzCCApegAwIBAgIQCDvgVpBCRrGhdWrJWZHHSjANBgkqhkiG9w0BAQUFADBh'+#13#10+
  'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3'+#13#10+
  'd3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD'+#13#10+
  'QTAeFw0wNjExMTAwMDAwMDBaFw0zMTExMTAwMDAwMDBaMGExCzAJBgNVBAYTAlVT'+#13#10+
  'MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j'+#13#10+
  'b20xIDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IENBMIIBIjANBgkqhkiG'+#13#10+
  '9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4jvhEXLeqKTTo1eqUKKPC3eQyaKl7hLOllsB'+#13#10+
  'CSDMAZOnTjC3U/dDxGkAV53ijSLdhwZAAIEJzs4bg7/fzTtxRuLWZscFs3YnFo97'+#13#10+
  'nh6Vfe63SKMI2tavegw5BmV/Sl0fvBf4q77uKNd0f3p4mVmFaG5cIzJLv07A6Fpt'+#13#10+
  '43C/dxC//AH2hdmoRBBYMql1GNXRor5H4idq9Joz+EkIYIvUX7Q6hL+hqkpMfT7P'+#13#10+
  'T19sdl6gSzeRntwi5m3OFBqOasv+zbMUZBfHWymeMr/y7vrTC0LUq7dBMtoM1O/4'+#13#10+
  'gdW7jVg/tRvoSSiicNoxBN33shbyTApOB6jtSj1etX+jkMOvJwIDAQABo2MwYTAO'+#13#10+
  'BgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUA95QNVbR'+#13#10+
  'TLtm8KPiGxvDl7I90VUwHwYDVR0jBBgwFoAUA95QNVbRTLtm8KPiGxvDl7I90VUw'+#13#10+
  'DQYJKoZIhvcNAQEFBQADggEBAMucN6pIExIK+t1EnE9SsPTfrgT1eXkIoyQY/Esr'+#13#10+
  'hMAtudXH/vTBH1jLuG2cenTnmCmrEbXjcKChzUyImZOMkXDiqw8cvpOp/2PV5Adg'+#13#10+
  '06O/nVsJ8dWO41P0jmP6P6fbtGbfYmbW0W5BjfIttep3Sp+dWOIrWcBAI+0tKIJF'+#13#10+
  'PnlUkiaY4IBIqDfv8NZ5YBberOgOzW6sRBc4L0na4UU+Krk2U886UAb3LujEV0ls'+#13#10+
  'YSEY1QSteDwsOoBrp+uvFRTp2InBuThs4pFsiv9kuXclVzDAGySj4dzp30d8tbQk'+#13#10+
  'CAUw7C29C79Fv1C5qfPrmAESrciIxpg0X40KPMbp1ZWVbd4='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Global Root G2, (O): DigiCert Inc, (OU): www.digicert.com
Issuer: Self Signed
Expires: 2038-01-15T12:00:00, Signature: sha256WithRSAEncryption
Valid From: 2013-08-01T12:00:00, Serial Number: 033af1e6a711a9a0bb2864b11d09fae5
Fingerprint (sha256): cb3ccbb76031e5e0138f8dd39a23f9de47ffc35e43c1144cea27d46a5ab1cb5f
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts017 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDjjCCAnagAwIBAgIQAzrx5qcRqaC7KGSxHQn65TANBgkqhkiG9w0BAQsFADBh'+#13#10+
  'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3'+#13#10+
  'd3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBH'+#13#10+
  'MjAeFw0xMzA4MDExMjAwMDBaFw0zODAxMTUxMjAwMDBaMGExCzAJBgNVBAYTAlVT'+#13#10+
  'MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j'+#13#10+
  'b20xIDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IEcyMIIBIjANBgkqhkiG'+#13#10+
  '9w0BAQEFAAOCAQ8AMIIBCgKCAQEAuzfNNNx7a8myaJCtSnX/RrohCgiN9RlUyfuI'+#13#10+
  '2/Ou8jqJkTx65qsGGmvPrC3oXgkkRLpimn7Wo6h+4FR1IAWsULecYxpsMNzaHxmx'+#13#10+
  '1x7e/dfgy5SDN67sH0NO3Xss0r0upS/kqbitOtSZpLYl6ZtrAGCSYP9PIUkY92eQ'+#13#10+
  'q2EGnI/yuum06ZIya7XzV+hdG82MHauVBJVJ8zUtluNJbd134/tJS7SsVQepj5Wz'+#13#10+
  'tCO7TG1F8PapspUwtP1MVYwnSlcUfIKdzXOS0xZKBgyMUNGPHgm+F6HmIcr9g+UQ'+#13#10+
  'vIOlCsRnKPZzFBQ9RnbDhxSJITRNrw9FDKZJobq7nMWxM4MphQIDAQABo0IwQDAP'+#13#10+
  'BgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBhjAdBgNVHQ4EFgQUTiJUIBiV'+#13#10+
  '5uNu5g/6+rkS7QYXjzkwDQYJKoZIhvcNAQELBQADggEBAGBnKJRvDkhj6zHd6mcY'+#13#10+
  '1Yl9PMWLSn/pvtsrF9+wX3N3KjITOYFnQoQj8kVnNeyIv/iPsGEMNKSuIEyExtv4'+#13#10+
  'NeF22d+mQrvHRAiGfzZ0JFrabA0UWTW98kndth/Jsw1HKj2ZL7tcu7XUIOGZX1NG'+#13#10+
  'Fdtom/DzMNU+MeKNhJ7jitralj41E6Vf8PlwUHBHQRFXGU7Aj64GxJUTFy8bJZ91'+#13#10+
  '8rGOmaFvE7FBcf6IKshPECBV1/MUReXgRPTqh5Uykw7+U0b6LJ3/iyK5S9kJRaTe'+#13#10+
  'pLiaWN0bfVKfjllDiIGknibVb63dDcY3fe0Dkhvld1927jyNxF1WW6LZZm6zNTfl'+#13#10+
  'MrY='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert Global Root G3, (O): DigiCert Inc, (OU): www.digicert.com
Issuer: Self Signed
Expires: 2038-01-15T12:00:00, Signature: ecdsa-with-SHA384
Valid From: 2013-08-01T12:00:00, Serial Number: 055556bcf25ea43535c3a40fd5ab4572
Fingerprint (sha256): 31ad6648f8104138c738f39ea4320133393e3a18cc02296ef97c2ac9ef6731d0
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts018 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICPzCCAcWgAwIBAgIQBVVWvPJepDU1w6QP1atFcjAKBggqhkjOPQQDAzBhMQsw'+#13#10+
  'CQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3d3cu'+#13#10+
  'ZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBHMzAe'+#13#10+
  'Fw0xMzA4MDExMjAwMDBaFw0zODAxMTUxMjAwMDBaMGExCzAJBgNVBAYTAlVTMRUw'+#13#10+
  'EwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5jb20x'+#13#10+
  'IDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IEczMHYwEAYHKoZIzj0CAQYF'+#13#10+
  'K4EEACIDYgAE3afZu4q4C/sLfyHS8L6+c/MzXRq8NOrexpu80JX28MzQC7phW1FG'+#13#10+
  'fp4tn+6OYwwX7Adw9c+ELkCDnOg/QW07rdOkFFk2eJ0DQ+4QE2xy3q6Ip6FrtUPO'+#13#10+
  'Z9wj/wMco+I+o0IwQDAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwIBhjAd'+#13#10+
  'BgNVHQ4EFgQUs9tIpPmhxdiuNkHMEWNpYim8S8YwCgYIKoZIzj0EAwMDaAAwZQIx'+#13#10+
  'AK288mw/EkrRLTnDCgmXc/SINoyIJ7vmiI1Qhadj+Z4y3maTD/HMsQmP3Wyr+mt/'+#13#10+
  'oAIwOWZbwmSNuJ5Q3KjVSaLtx9zRSX8XAbjIho9OjIgrqJqpisXRAL34VOKa5Vt8'+#13#10+
  'sycX'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert High Assurance EV Root CA, (O): DigiCert Inc, (OU): www.digicert.com
Issuer: Self Signed
Expires: 2031-11-10T00:00:00, Signature: sha1WithRSAEncryption
Valid From: 2006-11-10T00:00:00, Serial Number: 02ac5c266a0b409b8f0b79f2ae462577
Fingerprint (sha256): 7431e5f4c3c1ce4690774f0b61e05440883ba9a01ed00ba6abd7806ed3b118cf
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts019 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDxTCCAq2gAwIBAgIQAqxcJmoLQJuPC3nyrkYldzANBgkqhkiG9w0BAQUFADBs'+#13#10+
  'MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3'+#13#10+
  'd3cuZGlnaWNlcnQuY29tMSswKQYDVQQDEyJEaWdpQ2VydCBIaWdoIEFzc3VyYW5j'+#13#10+
  'ZSBFViBSb290IENBMB4XDTA2MTExMDAwMDAwMFoXDTMxMTExMDAwMDAwMFowbDEL'+#13#10+
  'MAkGA1UEBhMCVVMxFTATBgNVBAoTDERpZ2lDZXJ0IEluYzEZMBcGA1UECxMQd3d3'+#13#10+
  'LmRpZ2ljZXJ0LmNvbTErMCkGA1UEAxMiRGlnaUNlcnQgSGlnaCBBc3N1cmFuY2Ug'+#13#10+
  'RVYgUm9vdCBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMbM5XPm'+#13#10+
  '+9S75S0tMqbf5YE/yc0lSbZxKsPVlDRnogocsF9ppkCxxLeyj9CYpKlBWTrT3JTW'+#13#10+
  'PNt0OKRKzE0lgvdKpVMSOO7zSW1xkX5jtqumX8OkhPhPYlG++MXs2ziS4wblCJEM'+#13#10+
  'xChBVfvLWokVfnHoNb9Ncgk9vjo4UFt3MRuNs8ckRZqnrG0AFFoEt7oT61EKmEFB'+#13#10+
  'Ik5lYYeBQVCmeVyJ3hlKV9Uu5l0cUyx+mM0aBhakaHPQNAQTXKFx01p8VdteZOE3'+#13#10+
  'hzBWBOURtCmAEvF5OYiiAhF8J2a3iLd48soKqDirCmTCv2ZdlYTBoSUeh10aUAsg'+#13#10+
  'EsxBu24LUTi4S8sCAwEAAaNjMGEwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQF'+#13#10+
  'MAMBAf8wHQYDVR0OBBYEFLE+w2kD+L9HAdSYJhoIAu9jZCvDMB8GA1UdIwQYMBaA'+#13#10+
  'FLE+w2kD+L9HAdSYJhoIAu9jZCvDMA0GCSqGSIb3DQEBBQUAA4IBAQAcGgaX3Nec'+#13#10+
  'nzyIZgYIVyHbIUf4KmeqvxgydkAQV8GK83rZEWWONfqe/EW1ntlMMUu4kehDLI6z'+#13#10+
  'eM7b41N5cdblIZQB2lWHmiRk9opmzN6cN82oNLFpmyPInngiK3BD41VHMWEZ71jF'+#13#10+
  'hS9OMPagMRYjyOfiZRYzy78aG6A9+MpeizGLYAiJLQwGXFK3xPkKmNEVX58Svnw2'+#13#10+
  'Yzi9RKR/5CYrCsSXaQ3pjOLAEFe4yHYSkVXySGnYvCoCWw9E1CAx2/S6cCZdkGCe'+#13#10+
  'vEsXCS+0yx5DaMkHJ8HSXPfqIbloEpw8nL+e/IBcm2PN7EeqJSdnoDfzAIJ9VNep'+#13#10+
  '+OkuE6N36B9K'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert RSA4096 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: sha384WithRSAEncryption
Valid From: 2021-01-15T00:00:00, Serial Number: 08bfa26f9a3f3365a2acf0a638c40170
Fingerprint (sha256): e46a392204a8dca342a71c1ca9a60c9185b9a930370120c3b9c7e3856f0d8f3b
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts020 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFXjCCA0agAwIBAgIQCL+ib5o/M2WirPCmOMQBcDANBgkqhkiG9w0BAQwFADBJ'+#13#10+
  'MQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xITAfBgNVBAMT'+#13#10+
  'GERpZ2lDZXJ0IFJTQTQwOTYgUm9vdCBHNTAeFw0yMTAxMTUwMDAwMDBaFw00NjAx'+#13#10+
  'MTQyMzU5NTlaMEkxCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5EaWdpQ2VydCwgSW5j'+#13#10+
  'LjEhMB8GA1UEAxMYRGlnaUNlcnQgUlNBNDA5NiBSb290IEc1MIICIjANBgkqhkiG'+#13#10+
  '9w0BAQEFAAOCAg8AMIICCgKCAgEAqr4NsgZ9JvlH6uQb50JpuJnCue4ksUaQy1kk'+#13#10+
  'UlQ1piTCX5EZyLZC1vNHZZVk54VlZ6mufABP4HgDUK3zf464EeeBYrGL3/JJJgne'+#13#10+
  'Dxa82iibociXL5OQ2iAq44TU/6mesC2/tADemx/IoGNTaIVvTYXGqmP5jbI1dmJ0'+#13#10+
  'A9yTmGgFns2QZd3SejGrJC1tQC6QP2NsLOv6HoBUjXkCkBSztU9O9YgEQ4DDSLMm'+#13#10+
  'L6xRlTJVJS9BlrBWoQg73JgfcoUsd8qYzDj7jnLJbewF7O1NtzxbFFCF3Zf7WfeQ'+#13#10+
  'EvQTv4NNgLIVZRGXYOXWXOYEtVDmcTO2IJOpaAA4zknbtFw7ctdFXFS/zTwBIx58'+#13#10+
  '1vhpLKUACmwySLTecC06ExfBf2TL8zDtoT2WZ/GUtWBsW2lo9YIzCaK22fOFsm6g'+#13#10+
  'lPDCxH2hLMpz9a7gUpyiZuYDzurf7RjUuWOL9+j/+7Nbj0PFr7d0lFA1Za7WL/GF'+#13#10+
  'j1OhcPSNMl28lsMewgQEnAQPs11+iSDKXicNiUoSI7T2xN3YH/hoszb4HrzG94S2'+#13#10+
  '6IpOiDA4wCbYcAoJOjQOa4ISlhwv5p6t2HE1gbGMBm70bmb/S0quvfD+11xfU7sy'+#13#10+
  'PM1i0RSgKR8Q3qlyT7GtZOWDKo+L6oSV7pglmJqzcTzBp1DyrEJiMcKhkMbu4reK'+#13#10+
  'qLW2GzsCAwEAAaNCMEAwHQYDVR0OBBYEFGJtt5FPxOqjYmCPoNC+tY8GfGgAMA4G'+#13#10+
  'A1UdDwEB/wQEAwIBhjAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3DQEBDAUAA4IC'+#13#10+
  'AQBh6PsnbdbiuLMJr6rwsYJM/j0XiU0tFZ377tC7hOyEddtDE96Mn8cp74d0yxNw'+#13#10+
  'gVYAdPyu9Nk63iIIUaWgXIJmtntMqdqPq6wcQZZm1p3eVua/TrGyXl/Aw27UwoSQ'+#13#10+
  '9X2xuhbRKYrInenP0McZOz/P7vfhM65CyJjACJ7zWvPf1Cs7jqgoVhnHTnc8JVTc'+#13#10+
  'uEhI0fknaj7sE6+yBYn9VV/zfY4NnAldLIp+hc744b8RPTKMWtd+PfQzWM+iBZij'+#13#10+
  's/vOib/9whbdbtyISQ0LoAP/50XpBMHp/aqddfi4H4eD2es501qny5isE4kA/G+V'+#13#10+
  'TuF9EUZt9jhGoxOgLAH1Ys+/HFCRJ3Rdt+xHfNDRdct77tFNIwrDYKV3LYDaZw+O'+#13#10+
  'a3YH8KYP6oSuHnm/CIraCfP07rU289R6Q7qUNeH6wTsblpmkV2PrtaiC9634d9d2'+#13#10+
  'hvN2U1Zb/CZChM6fg5GRr/S+cBWApdjoabHYkVS4GbJi+aL6Ve0Ev7lEhuTP8ZsA'+#13#10+
  'vxEPvrV0JFH/dzRj7EgjDugR63dt2sqCkb6khJNM2qH+zAaE6CHoVLrm0x1jPcJa'+#13#10+
  '/ObJg55yZKmGWQCMwvcTg7bQpDHGrJGOe6QiVhPGdccjvItb/EY9/l1SKa+v6MnD'+#13#10+
  'dkvoq0cC8poN0yyIgAeGwGMPAkyOBFN2uVhCb3wpcF2/Jw=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert SMIME ECC P384 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: ecdsa-with-SHA384
Valid From: 2021-01-15T00:00:00, Serial Number: 053f6ea00601727ded3fc3a3b6a3d6ef
Fingerprint (sha256): e8e8176536a60cc2c4e10187c3befca20ef263497018f566d5bea0f94d0c111b
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts021 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICHDCCAaOgAwIBAgIQBT9uoAYBcn3tP8OjtqPW7zAKBggqhkjOPQQDAzBQMQsw'+#13#10+
  'CQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xKDAmBgNVBAMTH0Rp'+#13#10+
  'Z2lDZXJ0IFNNSU1FIEVDQyBQMzg0IFJvb3QgRzUwHhcNMjEwMTE1MDAwMDAwWhcN'+#13#10+
  'NDYwMTE0MjM1OTU5WjBQMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQs'+#13#10+
  'IEluYy4xKDAmBgNVBAMTH0RpZ2lDZXJ0IFNNSU1FIEVDQyBQMzg0IFJvb3QgRzUw'+#13#10+
  'djAQBgcqhkjOPQIBBgUrgQQAIgNiAAQWnVXlttT7+2drGtShqtJ3lT6I5QeftnBm'+#13#10+
  'ICikiOxwNa+zMv83E0qevAED3oTBuMbmZUeJ8hNVv82lHghgf61/6GGSKc8JR14L'+#13#10+
  'HMAfpL/yW7yY75lMzHBrtrrQKB2/vgSjQjBAMB0GA1UdDgQWBBRzemuW20IHi1Jm'+#13#10+
  'wmQyF/7gZ5AurTAOBgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAKBggq'+#13#10+
  'hkjOPQQDAwNnADBkAjA3RPUygONx6/Rtz3zMkZrDbnHY0iNdkk2CQm1cYZX2kfWn'+#13#10+
  'CPZql+mclC2YcP0ztgkCMAc8L7lYgl4Po2Kok2fwIMNpvwMsO1CnO69BOMlSSJHW'+#13#10+
  'Dvu8YDB8ZD8SHkV/UT70pg=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): DigiCert TLS RSA4096 Root G5, (O): DigiCert, Inc.
Issuer: Self Signed
Expires: 2046-01-14T23:59:59, Signature: sha384WithRSAEncryption
Valid From: 2021-01-15T00:00:00, Serial Number: 08f9b478a8fa7eda6a333789de7ccf8a
Fingerprint (sha256): 371a00dc0533b3721a7eeb40e8419e70799d2b0a0f2c1d80693165f7cec4ad75
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts022 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFZjCCA06gAwIBAgIQCPm0eKj6ftpqMzeJ3nzPijANBgkqhkiG9w0BAQwFADBN'+#13#10+
  'MQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQsIEluYy4xJTAjBgNVBAMT'+#13#10+
  'HERpZ2lDZXJ0IFRMUyBSU0E0MDk2IFJvb3QgRzUwHhcNMjEwMTE1MDAwMDAwWhcN'+#13#10+
  'NDYwMTE0MjM1OTU5WjBNMQswCQYDVQQGEwJVUzEXMBUGA1UEChMORGlnaUNlcnQs'+#13#10+
  'IEluYy4xJTAjBgNVBAMTHERpZ2lDZXJ0IFRMUyBSU0E0MDk2IFJvb3QgRzUwggIi'+#13#10+
  'MA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQCz0PTJeRGd/fxmgefM1eS87IE+'+#13#10+
  'ajWOLrfn3q/5B03PMJ3qCQuZvWxX2hhKuHisOjmopkisLnLlvevxGs3npAOpPxG0'+#13#10+
  '2C+JFvuUAT27L/gTBaF4HI4o4EXgg/RZG5Wzrn4DReW+wkL+7vI8toUTmDKdFqgp'+#13#10+
  'wgscONyfMXdcvyej/Cestyu9dJsXLfKB2l2w4SMXPohKEiPQ6s+d3gMXsUJKoBZM'+#13#10+
  'pG2T6T867jp8nVid9E6P/DsjyG244gXazOvswzH016cpVIDPRFtMbzCe88zdH5RD'+#13#10+
  'nU1/cHAN1DrRN/BsnZvAFJNY781BOHW8EwOVfH/jXOnVDdXifBBiqmvwPXbzP6Po'+#13#10+
  'sMH976pXTayGpxi0KcEsDr9kvimM2AItzVwv8n/vFfQMFawKsPHTDU9qTXeXAaDx'+#13#10+
  'Zre3zu/O7Oyldcqs4+Fj97ihBMi8ez9dLRYiVu1ISf6nL3kwJZu6ay0/nTvEF+cd'+#13#10+
  'Lvvyz6b84xQslpghjLSR6Rlgg/IwKwZzUNWYOwbpx4oMYIwo+FKbbuH2TbsGJJvX'+#13#10+
  'KyY//SovcfXWJL5/MZ4PbeiPT02jP/816t9JXkGPhvnxd3lLG7SjXi/7RgLQZhNe'+#13#10+
  'XoVPzthwiHvOAbWWl9fNff2C+MIkwcoBOU+NosEUQB+cZtUMCUbW8tDRSHZWOkPL'+#13#10+
  'tgoRObqME2wGtZ7P6wIDAQABo0IwQDAdBgNVHQ4EFgQUUTMc7TZArxfTJc1paPKv'+#13#10+
  'TiM+s0EwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcN'+#13#10+
  'AQEMBQADggIBAGCmr1tfV9qJ20tQqcQjNSH/0GEwhJG3PxDPJY7Jv0Y02cEhJhxw'+#13#10+
  'GXIeo8mH/qlDZJY6yFMECrZBu8RHANmfGBg7sg7zNOok992vIGCukihfNudd5N7H'+#13#10+
  'PNtQOa27PShNlnx2xlv0wdsUpasZYgcYQF+Xkdycx6u1UQ3maVNVzDl92sURVXLF'+#13#10+
  'O4uJ+DQtpBflF+aZfTCIITfNMBc9uPK8qHWgQ9w+iUuQrm0D4ByjoJYJu32jtyoQ'+#13#10+
  'REtGBzRj7TG5BO6jm5qu5jF49OokYTurWGT/u4cnYiWB39yhL/btp/96j1EuMPik'+#13#10+
  'AdKFOV8BmZZvWltwGUb+hmA+rYAQCd05JS9Yf7vSdPD3Rh9GOUrYU9DzLjtxpdRv'+#13#10+
  '/PNn5AeP3SYZ4Y1b+qOTEZvpyDrDVWiakuFSdjjo4bq9+0/V77PnSIMx8IIh47a+'+#13#10+
  'p6tv75/fTM8BuGJqIz3nCU2AG3swpMPdB380vqQmsvZB6Akd4yCYqjdP//fx4ilw'+#13#10+
  'MUc/dNAUFvohigLVigmUdy7yWSiLfFCSCmZ4OIN1xLVaqBHG5cGdZlXPU8Sv13WF'+#13#10+
  'qUITVuwhd4GTWgzqltlJyqEI8pc7bZsEGCREjnwB8twl2F6GmrE52/WRMmrRpnCK'+#13#10+
  'ovfepEWFJqgejF0pW8hL2JpqA15w8oVPbEtoL8pU9ozaMv7Da4M/OMZ+'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Entrust Root Certification Authority, (O): Entrust, Inc., (OU): www.entrust.net/CPS is incorporated by reference, (c) 2006 Entrust, Inc.
Issuer: Self Signed
Expires: 2026-11-27T20:53:42, Signature: sha1WithRSAEncryption
Valid From: 2006-11-27T20:23:42, Serial Number: 456b5054
Fingerprint (sha256): 73c176434f1bc6d5adf45b0e76e727287c8de57616c1e6e6141a2b2cbc7d8e4c
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts023 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEkTCCA3mgAwIBAgIERWtQVDANBgkqhkiG9w0BAQUFADCBsDELMAkGA1UEBhMC'+#13#10+
  'VVMxFjAUBgNVBAoTDUVudHJ1c3QsIEluYy4xOTA3BgNVBAsTMHd3dy5lbnRydXN0'+#13#10+
  'Lm5ldC9DUFMgaXMgaW5jb3Jwb3JhdGVkIGJ5IHJlZmVyZW5jZTEfMB0GA1UECxMW'+#13#10+
  'KGMpIDIwMDYgRW50cnVzdCwgSW5jLjEtMCsGA1UEAxMkRW50cnVzdCBSb290IENl'+#13#10+
  'cnRpZmljYXRpb24gQXV0aG9yaXR5MB4XDTA2MTEyNzIwMjM0MloXDTI2MTEyNzIw'+#13#10+
  'NTM0MlowgbAxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1FbnRydXN0LCBJbmMuMTkw'+#13#10+
  'NwYDVQQLEzB3d3cuZW50cnVzdC5uZXQvQ1BTIGlzIGluY29ycG9yYXRlZCBieSBy'+#13#10+
  'ZWZlcmVuY2UxHzAdBgNVBAsTFihjKSAyMDA2IEVudHJ1c3QsIEluYy4xLTArBgNV'+#13#10+
  'BAMTJEVudHJ1c3QgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTCCASIwDQYJ'+#13#10+
  'KoZIhvcNAQEBBQADggEPADCCAQoCggEBALaVtkNC+sZtKm9I35RMOVcF7sN5EUFo'+#13#10+
  'Nu3s/poBj6E4KPz3EEZmLk0eGrEaTsbRwJWIsMn/MYszA9u3g3s+IIRe7bJWKKf4'+#13#10+
  '4LlAcTfFy0cOlypowCKVYhXbR9n10Cv/gkvJrT7eTNuQgFA/CYqEAOwwCj0Yzfv9'+#13#10+
  'KlmaI5UXLEWeH25DeW0MXJj+SKfFI0dcXv1u5x609mhF0YaDW6KKjbHjKYD+JXGI'+#13#10+
  'rb68j6xSlkuqUY3kEzEZ6E5Nn9uss2rVvDlUccp6en+Q3X0dgNmBu1kmwhH+5pPi'+#13#10+
  '94DkZfs0Nw4pgHBNrziGLp5/V6+eF67rHMsoIV+2HNjnogQi+dPa2MsCAwEAAaOB'+#13#10+
  'sDCBrTAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zArBgNVHRAEJDAi'+#13#10+
  'gA8yMDA2MTEyNzIwMjM0MlqBDzIwMjYxMTI3MjA1MzQyWjAfBgNVHSMEGDAWgBRo'+#13#10+
  'kORnpKZTgMeGZqTx90tD+4S9bTAdBgNVHQ4EFgQUaJDkZ6SmU4DHhmak8fdLQ/uE'+#13#10+
  'vW0wHQYJKoZIhvZ9B0EABBAwDhsIVjcuMTo0LjADAgSQMA0GCSqGSIb3DQEBBQUA'+#13#10+
  'A4IBAQCT1DCw1wMgKtD5Y+iRDAUgqV8ZyntyTtSx29CW+1RaGSwMCPeyvIWonX9t'+#13#10+
  'O1KzKtvn1ISMY/YPyyYBkVBs9F8U4pN0wBOeMDpQ47RgxRzwIkSNcUesyBrJ6Zua'+#13#10+
  'AGAT/3B+XxFNSRuzFVJ7yVTav52Vr2ua2J7p8eRDjeIRRDq/r72DQnNSi6q7pynP'+#13#10+
  '9WQcCk3RvKqsnyrQ/39/2n3qse0wJcGE2jTSW3iDVuycNsMm4hH2Z0kdkquM++v/'+#13#10+
  'eu6FSqdQgPCnXEqULl8FmTxSQeDNtGPPAUO6nIPcj2A781q0tHuu2guQOHXvgR1m'+#13#10+
  '0vdXcDazv/wor3ElhVsT/h5/WrQ8'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Entrust Root Certification Authority - G2, (O): Entrust, Inc., (OU): See www.entrust.net/legal-terms, (c) 2009 Entrust, Inc. - for authorized use only
Issuer: Self Signed
Expires: 2030-12-07T17:55:54, Signature: sha256WithRSAEncryption
Valid From: 2009-07-07T17:25:54, Serial Number: 4a538c28
Fingerprint (sha256): 43df5774b03e7fef5fe40d931a7bedf1bb2e6b42738c4e6d3841103d3aa7f339
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts024 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEPjCCAyagAwIBAgIESlOMKDANBgkqhkiG9w0BAQsFADCBvjELMAkGA1UEBhMC'+#13#10+
  'VVMxFjAUBgNVBAoTDUVudHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50'+#13#10+
  'cnVzdC5uZXQvbGVnYWwtdGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3Qs'+#13#10+
  'IEluYy4gLSBmb3IgYXV0aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVz'+#13#10+
  'dCBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IC0gRzIwHhcNMDkwNzA3MTcy'+#13#10+
  'NTU0WhcNMzAxMjA3MTc1NTU0WjCBvjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUVu'+#13#10+
  'dHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50cnVzdC5uZXQvbGVnYWwt'+#13#10+
  'dGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3QsIEluYy4gLSBmb3IgYXV0'+#13#10+
  'aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVzdCBSb290IENlcnRpZmlj'+#13#10+
  'YXRpb24gQXV0aG9yaXR5IC0gRzIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK'+#13#10+
  'AoIBAQC6hLZy254Ma+KZ6TABp3bqMriVQRrJ2mFOWHLP/vaCeb9zYQYKpSfYs1/T'+#13#10+
  'RU4cctZOMvJyig/3gxnQaoCAAEUesMfnmr8SVycco2gvCoe9amsOXmXzHHfV1IWN'+#13#10+
  'cCG0szLni6LVhjkCsbjSR87kyUnEO6fe+1R9V77w6G7CebI6C1XiUJgWMhNcL3hW'+#13#10+
  'wcKUs/Ja5CeanyTXxuzQmyWC48zCxEXFjJd6BmsqEZ+pCm5IO2/b1BEZQvePB7/1'+#13#10+
  'U1+cPvQXLOZprE4yTGJ36rfo5bs0vBmLrpxR57d+tVOxMyLlbc9wPBr64ptntoP0'+#13#10+
  'jaWvYkxN4FisZDQSA/i2jZRjJKRxAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAP'+#13#10+
  'BgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBRqciZ60B7vfec7aVHUbI2fkBJmqzAN'+#13#10+
  'BgkqhkiG9w0BAQsFAAOCAQEAeZ8dlsa2eT8ijYfThwMEYGprmi5ZiXMRrEPR9RP/'+#13#10+
  'jTkrwPK9T3CMqS/qF8QLVJ7UG5aYMzyorWKiAHarWWluBh1+xLlEjZivEtRh2woZ'+#13#10+
  'Rkfz6/djwUAFQKXSt/S1mja/qYh2iARVBCuch38aNzx+LaUa2NSJXsq9rD1s2G2v'+#13#10+
  '1fN2D807iDginWyTmsQ9v4IbZT+mD12q/OWyFcq1rca8PdCE6OoGcrBNOTJ4vz4R'+#13#10+
  'nAuknZoh8/CbCzB428Hch0P+vGOaysXCHMnHjf87ElgI5rY97HosTvuDls4MPGmH'+#13#10+
  'VHOkc8KT/1EQrBVUAdj8BbGJoX90g5pJ19xOe4pIb4tF9g=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Entrust.net Certification Authority (2048), (O): Entrust.net, (OU): www.entrust.net/CPS_2048 incorp. by ref. (limits liab.), (c) 1999 Entrust.net Limited
Issuer: Self Signed
Expires: 2029-07-24T14:15:12, Signature: sha1WithRSAEncryption
Valid From: 1999-12-24T17:50:51, Serial Number: 3863def8
Fingerprint (sha256): 6dc47172e01cbcb0bf62580d895fe2b8ac9ad4f873801e0c10b9c837d21eb177
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts025 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEKjCCAxKgAwIBAgIEOGPe+DANBgkqhkiG9w0BAQUFADCBtDEUMBIGA1UEChML'+#13#10+
  'RW50cnVzdC5uZXQxQDA+BgNVBAsUN3d3dy5lbnRydXN0Lm5ldC9DUFNfMjA0OCBp'+#13#10+
  'bmNvcnAuIGJ5IHJlZi4gKGxpbWl0cyBsaWFiLikxJTAjBgNVBAsTHChjKSAxOTk5'+#13#10+
  'IEVudHJ1c3QubmV0IExpbWl0ZWQxMzAxBgNVBAMTKkVudHJ1c3QubmV0IENlcnRp'+#13#10+
  'ZmljYXRpb24gQXV0aG9yaXR5ICgyMDQ4KTAeFw05OTEyMjQxNzUwNTFaFw0yOTA3'+#13#10+
  'MjQxNDE1MTJaMIG0MRQwEgYDVQQKEwtFbnRydXN0Lm5ldDFAMD4GA1UECxQ3d3d3'+#13#10+
  'LmVudHJ1c3QubmV0L0NQU18yMDQ4IGluY29ycC4gYnkgcmVmLiAobGltaXRzIGxp'+#13#10+
  'YWIuKTElMCMGA1UECxMcKGMpIDE5OTkgRW50cnVzdC5uZXQgTGltaXRlZDEzMDEG'+#13#10+
  'A1UEAxMqRW50cnVzdC5uZXQgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgKDIwNDgp'+#13#10+
  'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEArU1LqRKGsuqjIAcVFmQq'+#13#10+
  'K0vRvwtKTY7tgHalZ7d4QMBzQshowNtTK91euHaYNZOLGp18EzoOH1u3Hs/lJBQe'+#13#10+
  'sYGpjX24zGtLA/ECDNyrpUAkAH90lKGdCCmziAv1h3edVc3kw37XamSrhRSGlVuX'+#13#10+
  'MlBvPci6Zgzj/L24ScF2iUkZ/cCovYmjZy/Gn7xxGWC4LeksyZB2ZnuU4q941mVT'+#13#10+
  'XTzWnLLPKQP5L6RQstRIzgUyVYr9smRMDuSYB3Xbf9+5CFVghTAp+XtIpGmG4zU/'+#13#10+
  'HoZdenoVve8AjhUiVBcAkCaTvA5JaJG/+EfTnZVCwQ5N328mz8MYIWJmQ3DW1cAH'+#13#10+
  '4QIDAQABo0IwQDAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNV'+#13#10+
  'HQ4EFgQUVeSB0RGAvtiJuQijMfmhJAkWuXAwDQYJKoZIhvcNAQEFBQADggEBADub'+#13#10+
  'j1abMOdTmXx6eadNl9cZlZD7Bh/KM3xGY4+WZiT6QBshJ8rmcnPyT/4xmf3IDExo'+#13#10+
  'U8aAghOY+rat2l098c5u9hURlIIM7j+VrxGrD9cv3h8Dj1csHsm7mhpElesYT6Yf'+#13#10+
  'zX1XEC+bBAlahLVu2B064dae0Wx5XnkcFMXj0EyTO2U87d89vqbllRrDtRnDvV5b'+#13#10+
  'u/8j72gZyxKTJ1wDLW8w0B62GqzeWvfRqqgnpv55gcR5mTNXuhKwqeBCbJPKVt7+'+#13#10+
  'bYQLCIt+jerXmCHG8+c8eS9enNFMFY3h7CI3zJpDC5fcgJCNs2ebb0gIFVbPv/Er'+#13#10+
  'fF6adulZkMV8gzURZVE='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GTS Root R1, (O): Google Trust Services LLC
Issuer: Self Signed
Expires: 2036-06-22T00:00:00, Signature: sha384WithRSAEncryption
Valid From: 2016-06-22T00:00:00, Serial Number: 6e47a9c54b470c0dec33d089b91cf4e1
Fingerprint (sha256): 2a575471e31340bc21581cbd2cf13e158463203ece94bcf9d3cc196bf09a5472
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts026 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFWjCCA0KgAwIBAgIQbkepxUtHDA3sM9CJuRz04TANBgkqhkiG9w0BAQwFADBH'+#13#10+
  'MQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExM'+#13#10+
  'QzEUMBIGA1UEAxMLR1RTIFJvb3QgUjEwHhcNMTYwNjIyMDAwMDAwWhcNMzYwNjIy'+#13#10+
  'MDAwMDAwWjBHMQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNl'+#13#10+
  'cnZpY2VzIExMQzEUMBIGA1UEAxMLR1RTIFJvb3QgUjEwggIiMA0GCSqGSIb3DQEB'+#13#10+
  'AQUAA4ICDwAwggIKAoICAQC2EQKLHuOhd5s73L+UPreVp0A8of2C+X0yBoJx9vaM'+#13#10+
  'f/vo27xqLpeXo4xL+Sv2sfnOhB2x+cWX3u+58qPpvBKJXqeqUqv4IyfLpLGcY9vX'+#13#10+
  'mX7wCl7raKb0xlpHDU0QM+NOsROjyBhsS+z8CZDfnWQpJSMHobTSPS5g4M/SCYe7'+#13#10+
  'zUjwTcLCeoiKu7rPWRnWr4+wB7CeMfGCwcDfLqZtbBkOtdh+JhpFAz2weaSUKK0P'+#13#10+
  'fyblqAj+lug8aJRT7oM6iCsVlgmy4HqMLnXWnOunVmSPlk9orj2XwoSPwLxAwAtc'+#13#10+
  'vfaHszVsrBhQf4TgTM2S0yDpM7xSma8ytSmzJSq0SPly4cpk9+aCEI3oncKKiPo4'+#13#10+
  'Zor8Y/kB+Xj9e1x3+naH+uzfsQ55lVe0vSbv1gHR6xYKu44LtcXFilWr06zqkUsp'+#13#10+
  'zBmkMiVOKvFlRNACzqrOSbTqn3yDsEB750Orp2yjj32JgfpMpf/VjsPOS+C12LOO'+#13#10+
  'Rc92wO1AK/1TD7Cn1TsNsYqiA94xrcx36m97PtbfkSIS5r762DL8EGMUUXLeXdYW'+#13#10+
  'k70paDPvOmbsB4om3xPXV2V4J95eSRQAogB/mqghtqmxlbCluQ0WEdrHbEg8QOB+'+#13#10+
  'DVrNVjzRlwW5y0vtOUucxD/SVRNuJLDWcfr0wbrM7Rv1/oFB2ACYPTrIrnqYNxgF'+#13#10+
  'lQIDAQABo0IwQDAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNV'+#13#10+
  'HQ4EFgQU5K8rJnEaK0gnhS9SZizv8IkTcT4wDQYJKoZIhvcNAQEMBQADggIBADiW'+#13#10+
  'Cu49tJYeX++dnAsznyvgyv3SjgofQXSlfKqE1OXyHuY3UjKcC9FhHb8owbZEKTV1'+#13#10+
  'd5iyfNm9dKyKaOOpMQkpAWBz40d8U6iQSifvS9efk+eCNs6aaAyC58/UEBZvXw6Z'+#13#10+
  'XPYfcX3v73svfuo21pdwCxXu11xWajOl40k4DLh9+42FpLFZXvRq4d2h9mREruZR'+#13#10+
  'gyFmxhE+885H7pwoHyXa/6xmld01D1zvICxi/ZG6qcz8WpyTgYMpl0p8WnK0OdC3'+#13#10+
  'd8t5/Wk6kjftbjhlRn7pYL15iJdfOBL07q9bgsiG1eGZbYwE8na6SfZu6W0eX6Dv'+#13#10+
  'J4J2QPim01hcDyxC2kLGe4g0x8HYRZvBPsVhHdljUEn2NIVq4BjFbkerQUIpm/Zg'+#13#10+
  'DdIx02OYI5NaAIFItO/Nis3Jz5nu2Z6qNuFoS3FJFDYoOj0dzpqPJeaAcWErtXvM'+#13#10+
  '+SUWgeExX6GjfhaknBZqlxi9dnKlC54dNuYvoS++cJEPqOba+MSSQGwlfnuzCdyy'+#13#10+
  'F62ARPBopY+Udf90WuioAnwMCeKpSwughQtiue+hMZL77/ZRBIls6Kl0obsXs7X9'+#13#10+
  'SQ98POyDGCBDTtWTurQ0sR8WNh8M5mQ5Fkzc4P4dyKliPUDqysU0ArSuiYgzNdws'+#13#10+
  'E3PYJ/HQcu51OyLemGhmW/HGY0dVHLqlCFF1pkgl'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GTS Root R2, (O): Google Trust Services LLC
Issuer: Self Signed
Expires: 2036-06-22T00:00:00, Signature: sha384WithRSAEncryption
Valid From: 2016-06-22T00:00:00, Serial Number: 6e47a9c65ab3e720c5309a3f6852f26f
Fingerprint (sha256): c45d7bb08e6d67e62e4235110b564e5f78fd92ef058c840aea4e6455d7585c60
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts027 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFWjCCA0KgAwIBAgIQbkepxlqz5yDFMJo/aFLybzANBgkqhkiG9w0BAQwFADBH'+#13#10+
  'MQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExM'+#13#10+
  'QzEUMBIGA1UEAxMLR1RTIFJvb3QgUjIwHhcNMTYwNjIyMDAwMDAwWhcNMzYwNjIy'+#13#10+
  'MDAwMDAwWjBHMQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNl'+#13#10+
  'cnZpY2VzIExMQzEUMBIGA1UEAxMLR1RTIFJvb3QgUjIwggIiMA0GCSqGSIb3DQEB'+#13#10+
  'AQUAA4ICDwAwggIKAoICAQDO3v2m++zsFDQ8BwZabFn3GTXd98GdVarTzTukk3Lv'+#13#10+
  'CvptnfbwhYBboUhSnznFt+4orO/LdmgUud+tAWyZH8QiHZ/+cnfgLFuv5AS/T3Kg'+#13#10+
  'GjSY6Dlo7JUle3ah5mm5hRm9iYz+re026nO8/4Piy33B0s5Ks40FnotJk9/BW9Bu'+#13#10+
  'XvAuMC6C/Pq8tBcKSOWIm8Wba96wyrQD8Nr0kLhlZPdcTK3ofmZemde4wj7I0BOd'+#13#10+
  're7kRXuJVfeKH2JShBKzwkCX44ofR5GmdFrS+LFjKBC4swm4VndAoiaYecb+3yXu'+#13#10+
  'PuWgf9RhD1FLPD+M2uFwdNjCaKH5wQzpoeJ/u1U8dgbuak7MkogwTZq9TwtImoS1'+#13#10+
  'mKPV+3PBV2HdKFZ1E66HjucMUQkQdYhMvI35ezzUIkgfKtzra7tEscszcTJGr61K'+#13#10+
  '8YzodDqs5xoic4DSMPclQsciOzsSrZYuxsN2B6ogtzVJV+mSSeh2FnIxZyuWfoqj'+#13#10+
  'x5RWIr9qS34BIbIjMt/kmkRtWVtd9QCgHJvGeJeNkP+byKq0rxFROV7Z+2et1VsR'+#13#10+
  'nTKaG73VululycslaVNVJ1zgyjbLiGH7HrfQy+4W+9OmTN6SpdTi3/UGVN4unUu0'+#13#10+
  'kzCqgc7dGtxRcw1PcOnlthYhGXmy5okLdWTK1au8CcEYof/UVKGFPP0UJAOyh9Ok'+#13#10+
  'twIDAQABo0IwQDAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNV'+#13#10+
  'HQ4EFgQUu//KjiOfT5nK2+JopqUVJxce2Q4wDQYJKoZIhvcNAQEMBQADggIBALZp'+#13#10+
  '8KZ3/p7uC4Gt4cCpx/k1HUCCq+YEtN/L9x0Pg/B+E02NjO7jMyLDOfxA325BS0JT'+#13#10+
  'vhaI8dI4XsRomRyYUpOM52jtG2pzegVATX9lO9ZY8c6DR2Dj/5epnGB3GFW1fgiT'+#13#10+
  'z9D2PGcDFWEJ+YF59exTpJ/JjwGLc8R3dtyDovUMSRqodt6Sm2T4syzFJ9MHwAiA'+#13#10+
  'pJiS4wGWAqoC7o87xdFtCjMwc3i5T1QWvwsHoaRc5svJXISPD+AVdyx+Jn7axEvb'+#13#10+
  'pxZ3B7DNdehyQtaVhJ2Gg/LkkM0JR9SLA3DaWsYDQvTtN6LwG1BUSw7YhN4ZKJmB'+#13#10+
  'R64JGz9I0cNv4rBgF/XuIwKl2gBbbZCr7qLpGzvpx0QnRY5rn/WkhLx3+WuXrD5R'+#13#10+
  'RaIRpsyF7gpo8j5QOHokYh4XIDdtak23CZvJ/KRY9bb7nE4Yu5UC56GtmwfuNmsk'+#13#10+
  '0jmGwZODUNKBRqhfYlcsu2xkiAhu7xNUX90txGdj08+JN7+dIPT7eoOboB6BAFDC'+#13#10+
  '5AwiWVIQ7UNWhwD4FFKnHYuTjKJNRn8nxnGbJN7k2oaLDX5rIMHAnuFl2GqjpuiF'+#13#10+
  'izoHCBy69Y9Vmhh1fuXsgWbRIXOhNUQLgD1bnF5vKheW0YMjiGZt5obicDIvUiLn'+#13#10+
  'yOd/xCxgXS/Dr55FBcOEArf9LAhST4Ldo/DUhgkC'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GTS Root R3, (O): Google Trust Services LLC
Issuer: Self Signed
Expires: 2036-06-22T00:00:00, Signature: ecdsa-with-SHA384
Valid From: 2016-06-22T00:00:00, Serial Number: 6e47a9c76ca9732440890f0355dd8d1d
Fingerprint (sha256): 15d5b8774619ea7d54ce1ca6d0b0c403e037a917f131e8a04e1e6b7a71babce5
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts028 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICDDCCAZGgAwIBAgIQbkepx2ypcyRAiQ8DVd2NHTAKBggqhkjOPQQDAzBHMQsw'+#13#10+
  'CQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExMQzEU'+#13#10+
  'MBIGA1UEAxMLR1RTIFJvb3QgUjMwHhcNMTYwNjIyMDAwMDAwWhcNMzYwNjIyMDAw'+#13#10+
  'MDAwWjBHMQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZp'+#13#10+
  'Y2VzIExMQzEUMBIGA1UEAxMLR1RTIFJvb3QgUjMwdjAQBgcqhkjOPQIBBgUrgQQA'+#13#10+
  'IgNiAAQfTzOHMymKoYTey8chWEGJ6ladK0uFxh1MJ7x/JlFyb+Kf1qPKzEUURout'+#13#10+
  '736GjOyxfi//qXGdGIRFBEFVbivqJn+7kAHjSxm65FSWRQmx1WyRRK2EE46ajA2A'+#13#10+
  'DDL24CejQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMBAf8EBTADAQH/MB0GA1Ud'+#13#10+
  'DgQWBBTB8Sa6oC2uhYHP0/EqEr24Cmf9vDAKBggqhkjOPQQDAwNpADBmAjEAgFuk'+#13#10+
  'fCPAlaUs3L6JbyO5o91lAFJekazInXJ0glMLfalAvWhgxeG4VDvBNhcl2MG9AjEA'+#13#10+
  'njWSdIUlUfUk7GRSJFClH9voy8l27OyCbvWFGFPouOOaKaqW04MjyaR7YbPMAuhd'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GTS Root R4, (O): Google Trust Services LLC
Issuer: Self Signed
Expires: 2036-06-22T00:00:00, Signature: ecdsa-with-SHA384
Valid From: 2016-06-22T00:00:00, Serial Number: 6e47a9c88b94b6e8bb3b2ad8a2b2c199
Fingerprint (sha256): 71cca5391f9e794b04802530b363e121da8a3043bb26662fea4dca7fc951a4bd
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts029 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICCjCCAZGgAwIBAgIQbkepyIuUtui7OyrYorLBmTAKBggqhkjOPQQDAzBHMQsw'+#13#10+
  'CQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZpY2VzIExMQzEU'+#13#10+
  'MBIGA1UEAxMLR1RTIFJvb3QgUjQwHhcNMTYwNjIyMDAwMDAwWhcNMzYwNjIyMDAw'+#13#10+
  'MDAwWjBHMQswCQYDVQQGEwJVUzEiMCAGA1UEChMZR29vZ2xlIFRydXN0IFNlcnZp'+#13#10+
  'Y2VzIExMQzEUMBIGA1UEAxMLR1RTIFJvb3QgUjQwdjAQBgcqhkjOPQIBBgUrgQQA'+#13#10+
  'IgNiAATzdHOnaItgrkO4NcWBMHtLSZ37wWHO5t5GvWvVYRg1rkDdc/eJkTBa6zzu'+#13#10+
  'hXyiQHY7qca4R9gq55KRanPpsXI5nymfopjTX15YhmUPoYRlBtHci8nHc8iMai/l'+#13#10+
  'xKvRHYqjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMBAf8EBTADAQH/MB0GA1Ud'+#13#10+
  'DgQWBBSATNbrdP9JNqPV2Py1PsVq8JQdjDAKBggqhkjOPQQDAwNnADBkAjBqUFJ0'+#13#10+
  'CMRw3J5QdCHojXohw0+WbhXRIjVhLfoIN+4Zba3bssx9BzT1YBkstTTZbyACMANx'+#13#10+
  'sbqjYAuG7ZoIapVon+Kz4ZNkfF6Tpt95LY2F45TPI11xzPKwTdb+mciUqXWi4w=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GlobalSign, (O): GlobalSign, (OU): GlobalSign ECC Root CA - R4
Issuer: Self Signed
Expires: 2038-01-19T03:14:07, Signature: ecdsa-with-SHA256
Valid From: 2012-11-13T00:00:00, Serial Number: 2a38a41c960a04de42b228a50be8349802
Fingerprint (sha256): bec94911c2955676db6c0a550986d76e3ba005667c442c9762b4fbb773de228c
Public Key: ECDSA Key Encryption prime256v1 256 bits, 128 security bits }

sslRootCACerts030 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIB4TCCAYegAwIBAgIRKjikHJYKBN5CsiilC+g0mAIwCgYIKoZIzj0EAwIwUDEk'+#13#10+
  'MCIGA1UECxMbR2xvYmFsU2lnbiBFQ0MgUm9vdCBDQSAtIFI0MRMwEQYDVQQKEwpH'+#13#10+
  'bG9iYWxTaWduMRMwEQYDVQQDEwpHbG9iYWxTaWduMB4XDTEyMTExMzAwMDAwMFoX'+#13#10+
  'DTM4MDExOTAzMTQwN1owUDEkMCIGA1UECxMbR2xvYmFsU2lnbiBFQ0MgUm9vdCBD'+#13#10+
  'QSAtIFI0MRMwEQYDVQQKEwpHbG9iYWxTaWduMRMwEQYDVQQDEwpHbG9iYWxTaWdu'+#13#10+
  'MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEuMZ5049sJQ6fLjkZHAOkrprlOQcJ'+#13#10+
  'FspjsbmG+IpXwVfOQvpzofdlQv8ewQCybnMO/8ch5RikqtlxP6jUuc6MHaNCMEAw'+#13#10+
  'DgYDVR0PAQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFFSwe61F'+#13#10+
  'uOJAf/sKbvu+M8k8o4TVMAoGCCqGSM49BAMCA0gAMEUCIQDckqGgE6bPA7DmxCGX'+#13#10+
  'kPoUVy0D7O48027KqGx2vKLeuwIgJ6iFJzWbVsaj8kfSt24bAgAXqmemFZHe+pTs'+#13#10+
  'ewv4n4Q='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GlobalSign, (O): GlobalSign, (OU): GlobalSign ECC Root CA - R5
Issuer: Self Signed
Expires: 2038-01-19T03:14:07, Signature: ecdsa-with-SHA384
Valid From: 2012-11-13T00:00:00, Serial Number: 605949e0262ebb55f90a778a71f94ad86c
Fingerprint (sha256): 179fbc148a3dd00fd24ea13458cc43bfa7f59c8182d783a513f6ebec100c8924
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts031 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICHjCCAaSgAwIBAgIRYFlJ4CYuu1X5CneKcflK2GwwCgYIKoZIzj0EAwMwUDEk'+#13#10+
  'MCIGA1UECxMbR2xvYmFsU2lnbiBFQ0MgUm9vdCBDQSAtIFI1MRMwEQYDVQQKEwpH'+#13#10+
  'bG9iYWxTaWduMRMwEQYDVQQDEwpHbG9iYWxTaWduMB4XDTEyMTExMzAwMDAwMFoX'+#13#10+
  'DTM4MDExOTAzMTQwN1owUDEkMCIGA1UECxMbR2xvYmFsU2lnbiBFQ0MgUm9vdCBD'+#13#10+
  'QSAtIFI1MRMwEQYDVQQKEwpHbG9iYWxTaWduMRMwEQYDVQQDEwpHbG9iYWxTaWdu'+#13#10+
  'MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAER0UOlvt9Xb/pOdEh+J8LttV7HpI6SFkc'+#13#10+
  '8GIxLcB6KP4ap1yztsyX50XUWPrRd21DosCHZTQKH3rd6zwzocWdTaRvQZU4f8ke'+#13#10+
  'hOvRnkmSh5SHDDqFSmafnVmTTZdhBoZKo0IwQDAOBgNVHQ8BAf8EBAMCAQYwDwYD'+#13#10+
  'VR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUPeYpSJvqB8ohREom3m7e0oPQn1kwCgYI'+#13#10+
  'KoZIzj0EAwMDaAAwZQIxAOVpEslu28YxuglB4Zf4+/2a4n0Sye18ZNPLBSWLVtmg'+#13#10+
  '515dTguDnFt2KaAJJiFqYgIwcdK1j1zqO+F4CYWodZI7yFz9SO8NdCKoCOJuxUnO'+#13#10+
  'xwy8p2Fp8fc74SrL+SvzZpA3'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GlobalSign, (O): GlobalSign, (OU): GlobalSign Root CA - R2
Issuer: Self Signed
Expires: 2021-12-15T08:00:00, Signature: sha1WithRSAEncryption
Valid From: 2006-12-15T08:00:00, Serial Number: 0400000000010f8626e60d
Fingerprint (sha256): ca42dd41745fd0b81eb902362cf9d8bf719da1bd1b1efc946f5b4c99f42c1b9e
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts032 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDujCCAqKgAwIBAgILBAAAAAABD4Ym5g0wDQYJKoZIhvcNAQEFBQAwTDEgMB4G'+#13#10+
  'A1UECxMXR2xvYmFsU2lnbiBSb290IENBIC0gUjIxEzARBgNVBAoTCkdsb2JhbFNp'+#13#10+
  'Z24xEzARBgNVBAMTCkdsb2JhbFNpZ24wHhcNMDYxMjE1MDgwMDAwWhcNMjExMjE1'+#13#10+
  'MDgwMDAwWjBMMSAwHgYDVQQLExdHbG9iYWxTaWduIFJvb3QgQ0EgLSBSMjETMBEG'+#13#10+
  'A1UEChMKR2xvYmFsU2lnbjETMBEGA1UEAxMKR2xvYmFsU2lnbjCCASIwDQYJKoZI'+#13#10+
  'hvcNAQEBBQADggEPADCCAQoCggEBAKbPJA6+Lm8omUVCxKs+IVSbC9N/hHD6ErPL'+#13#10+
  'v4dfxn+G07IwXNb9rfF73OX4YJYJkhD10FPe+3t+c4isUoh7SqbKSaZeqKeMWhG8'+#13#10+
  'eoLrvozps6yWJQeXSpkqBy+0Hne/ig+1AnwblrjFuTosvNYSuetZfeLQBoZfXklq'+#13#10+
  'tTleiDTsvHgMCJiEbKjNS7SgfQx5TfC4LcshytVsW33hoCmEofnTlEnLJGKRILzd'+#13#10+
  'C9XZzPnqJworc5HGnRusyMvo4KD0L5CLTfuwNhv2GXqF4G3yYROIXJ/gkwpRl4pa'+#13#10+
  'zq+r1feqCapgvdzZX99yqWATXgAByUr6P6TqBwMhAo6CygPCm48CAwEAAaOBnDCB'+#13#10+
  'mTAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUm+IH'+#13#10+
  'V2ccHsBqBt5ZtJot39wZhi4wNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL2NybC5n'+#13#10+
  'bG9iYWxzaWduLm5ldC9yb290LXIyLmNybDAfBgNVHSMEGDAWgBSb4gdXZxwewGoG'+#13#10+
  '3lm0mi3f3BmGLjANBgkqhkiG9w0BAQUFAAOCAQEAmYFThxxol4aR7OBKuEQLq4Gs'+#13#10+
  'J0/WwbgcQ3izDJr86iw8bmEbTUsp9Z8FHSbBuOmDAGJFtqkIk7mpM0sYmsL4h4hO'+#13#10+
  '291xNBrBVNpGP+DTKqttVCL1OmLNIG+6KYnX3ZHu01yiPqFbQfXf5WRDLenVOavS'+#13#10+
  'ot+3i9DAgBkcRcAtjOj4LaR0VknFBbVPFd5uRHg5h6h+u/N5GJG79G+dwfCMNYxd'+#13#10+
  'AfvDbbnvRG15RjF+Cv6pgsH/76tuIMRQyV+dTZsXjAzlAcmgQWpzU/qlULRuJQ/7'+#13#10+
  'TBj0/VLZjmmx6BEP3ojY+x1J96relc8geMJgEtslQIxq/H5COEBkEveegeGTLg=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): GlobalSign Root CA, (O): GlobalSign nv-sa, (OU): Root CA
Issuer: Self Signed
Expires: 2028-01-28T12:00:00, Signature: sha1WithRSAEncryption
Valid From: 1998-09-01T12:00:00, Serial Number: 040000000001154b5ac394
Fingerprint (sha256): ebd41040e4bb3ec742c9e381d31ef2a41a48b6685c96e7cef3c1df6cd4331c99
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts033 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDdTCCAl2gAwIBAgILBAAAAAABFUtaw5QwDQYJKoZIhvcNAQEFBQAwVzELMAkG'+#13#10+
  'A1UEBhMCQkUxGTAXBgNVBAoTEEdsb2JhbFNpZ24gbnYtc2ExEDAOBgNVBAsTB1Jv'+#13#10+
  'b3QgQ0ExGzAZBgNVBAMTEkdsb2JhbFNpZ24gUm9vdCBDQTAeFw05ODA5MDExMjAw'+#13#10+
  'MDBaFw0yODAxMjgxMjAwMDBaMFcxCzAJBgNVBAYTAkJFMRkwFwYDVQQKExBHbG9i'+#13#10+
  'YWxTaWduIG52LXNhMRAwDgYDVQQLEwdSb290IENBMRswGQYDVQQDExJHbG9iYWxT'+#13#10+
  'aWduIFJvb3QgQ0EwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDaDuaZ'+#13#10+
  'jc6j40+Kfvvxi4Mla+pIH/EqsLmVEQS98GPR4mdmzxzdzxtIK+6NiY6arymAZavp'+#13#10+
  'xy0Sy6scTHAHoT0KMM0VjU/43dSMUBUc71DuxC73/OlS8pF94G3VNTCOXkNz8kHp'+#13#10+
  '1Wrjsok6Vjk4bwY8iGlbKk3Fp1S4bInMm/k8yuX9ifUSPJJ4ltbcdG6TRGHRjcdG'+#13#10+
  'snUOhugZitVtbNV4FpWi6cgKOOvyJBNPc1STE4U6G7weNLWLBYy5d4ux2x8gkasJ'+#13#10+
  'U26Qzns3dLlwR5EiUWMWea6xrkEmCMgZK9FGqkjWZCrXgzT/LCrBbBlDSgeF59N8'+#13#10+
  '9iFo7+ryUp9/k5DPAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNVHRMBAf8E'+#13#10+
  'BTADAQH/MB0GA1UdDgQWBBRge2YaRQ2XyolQL30EzTSo//z9SzANBgkqhkiG9w0B'+#13#10+
  'AQUFAAOCAQEA1nPnfE920I2/7LqivjTFKDK1fPxsnCwrvQmeU79rXqoRSLblCKOz'+#13#10+
  'yj1hTdNGCbM+w6DjY1Ub8rrvrTnhQ7k4o+YviiY776BQVvnGCv04zcQLcFGUl5gE'+#13#10+
  '38NflNUVyRRBnMRddWQVDf9VMOyGj/8N7yy5Y0b2qvzfvGn9LhJIZJrglfCm7ymP'+#13#10+
  'AbEVtQwdpf5pLGkkeB6zpxxxYu7KyJesF12KwvhHhm4qxFYxldBniYUr+WymXUad'+#13#10+
  'DKqC5JlR3XC321Y9YeRq4VzW9v493kHMB65jUr9TU/Qr6cf9tveCX4XSQRjbgbME'+#13#10+
  'HMUfpIBvFSDJ3gyICh3WZlXi/EjJKSZp4A=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Go Daddy Root Certificate Authority - G2, (O): GoDaddy.com, Inc.
Issuer: Self Signed
Expires: 2037-12-31T23:59:59, Signature: sha256WithRSAEncryption
Valid From: 2009-09-01T00:00:00, Serial Number: 00
Fingerprint (sha256): 45140b3247eb9cc8c5b4f0d7b53091f73292089e6e5a63e2749dd3aca9198eda
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts034 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDxTCCAq2gAwIBAgIBADANBgkqhkiG9w0BAQsFADCBgzELMAkGA1UEBhMCVVMx'+#13#10+
  'EDAOBgNVBAgTB0FyaXpvbmExEzARBgNVBAcTClNjb3R0c2RhbGUxGjAYBgNVBAoT'+#13#10+
  'EUdvRGFkZHkuY29tLCBJbmMuMTEwLwYDVQQDEyhHbyBEYWRkeSBSb290IENlcnRp'+#13#10+
  'ZmljYXRlIEF1dGhvcml0eSAtIEcyMB4XDTA5MDkwMTAwMDAwMFoXDTM3MTIzMTIz'+#13#10+
  'NTk1OVowgYMxCzAJBgNVBAYTAlVTMRAwDgYDVQQIEwdBcml6b25hMRMwEQYDVQQH'+#13#10+
  'EwpTY290dHNkYWxlMRowGAYDVQQKExFHb0RhZGR5LmNvbSwgSW5jLjExMC8GA1UE'+#13#10+
  'AxMoR28gRGFkZHkgUm9vdCBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkgLSBHMjCCASIw'+#13#10+
  'DQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL9xYgjx+lk09xvJGKP3gElY6SKD'+#13#10+
  'E6bFIEMBO4Tx5oVJnyfq9oQbTqC023CYxzIBsQU+B07u9PpPL1kwIuerGVZr4oAH'+#13#10+
  '/PMWdYA5UXvl+TW2dE6pjYIT5LY/qQOD+qK+ihVqf94Lw7YZFAXK6sOoBJQ7Rnwy'+#13#10+
  'DfMAZiLIjWltNowRGLfTshxgtDj6AozO091GB94KPutdfMh8+7ArU6SSYmlRJQVh'+#13#10+
  'GkSBjCypQ5Yj36w6gZoOKcUcqeldHraenjAKOc7xiID7S13MMuyFYkMlNAJWJwGR'+#13#10+
  'tDtwKj9useiciAF9n9T521NtYJ2/LOdYq7hfRvzOxBsDPAnrSTFcaUaz4EcCAwEA'+#13#10+
  'AaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYE'+#13#10+
  'FDqahQcQZyi27/a9BUFuIMGU2g/eMA0GCSqGSIb3DQEBCwUAA4IBAQCZ21151fmX'+#13#10+
  'WWcDYfF+OwYxdS2hII5PZYe096acvNjpL9DbWu7PdIxztDhC2gV7+AJ1uP2lsdeu'+#13#10+
  '9tfeE8tTEH6KRtGX+rcuKxGrkLAngPnon1rpN5+r5N9ss4UXnT3ZJE95kTXWXwTr'+#13#10+
  'gIOrmgIttRD02JDHBHNA7XIloKmf7J6raBKZV8aPEjoJpL1E/QYVN8Gb5DKj7Tjo'+#13#10+
  '2GTzLH4U/ALqn83/B2gX2yKQOC16jdFU8WnjXzPKej17CuPKf1855eJ1usV2GDPO'+#13#10+
  'LPAvTK33sefOT6jEm0pUBsV/fdUID+Ic/n4XuKxe9tQWskMJDE32p2u0mYRlynqI'+#13#10+
  '4uJEvlz36hz1'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): ISRG Root X1, (O): Internet Security Research Group
Issuer: Self Signed
Expires: 2035-06-04T11:04:38, Signature: sha256WithRSAEncryption
Valid From: 2015-06-04T11:04:38, Serial Number: 8210cfb0d240e3594463e0bb63828b00
Fingerprint (sha256): 96bcec06264976f37460779acf28c5a7cfe8a3c0aae11a8ffcee05c0bddf08c6
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts035 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFazCCA1OgAwIBAgIRAIIQz7DSQONZRGPgu2OCiwAwDQYJKoZIhvcNAQELBQAw'+#13#10+
  'TzELMAkGA1UEBhMCVVMxKTAnBgNVBAoTIEludGVybmV0IFNlY3VyaXR5IFJlc2Vh'+#13#10+
  'cmNoIEdyb3VwMRUwEwYDVQQDEwxJU1JHIFJvb3QgWDEwHhcNMTUwNjA0MTEwNDM4'+#13#10+
  'WhcNMzUwNjA0MTEwNDM4WjBPMQswCQYDVQQGEwJVUzEpMCcGA1UEChMgSW50ZXJu'+#13#10+
  'ZXQgU2VjdXJpdHkgUmVzZWFyY2ggR3JvdXAxFTATBgNVBAMTDElTUkcgUm9vdCBY'+#13#10+
  'MTCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAK3oJHP0FDfzm54rVygc'+#13#10+
  'h77ct984kIxuPOZXoHj3dcKi/vVqbvYATyjb3miGbESTtrFj/RQSa78f0uoxmyF+'+#13#10+
  '0TM8ukj13Xnfs7j/EvEhmkvBioZxaUpmZmyPfjxwv60pIgbz5MDmgK7iS4+3mX6U'+#13#10+
  'A5/TR5d8mUgjU+g4rk8Kb4Mu0UlXjIB0ttov0DiNewNwIRt18jA8+o+u3dpjq+sW'+#13#10+
  'T8KOEUt+zwvo/7V3LvSye0rgTBIlDHCNAymg4VMk7BPZ7hm/ELNKjD+Jo2FR3qyH'+#13#10+
  'B5T0Y3HsLuJvW5iB4YlcNHlsdu87kGJ55tukmi8mxdAQ4Q7e2RCOFvu396j3x+UC'+#13#10+
  'B5iPNgiV5+I3lg02dZ77DnKxHZu8A/lJBdiB3QW0KtZB6awBdpUKD9jf1b0SHzUv'+#13#10+
  'KBds0pjBqAlkd25HN7rOrFleaJ1/ctaJxQZBKT5ZPt0m9STJEadao0xAH0ahmbWn'+#13#10+
  'OlFuhjuefXKnEgV4We0+UXgVCwOPjdAvBbI+e0ocS3MFEvzG6uBQE3xDk3SzynTn'+#13#10+
  'jh8BCNAw1FtxNrQHusEwMFxIt4I7mKZ9YIqioymCzLq9gwQbooMDQaHWBfEbwrbw'+#13#10+
  'qHyGO0aoSCqI3Haadr8faqU9GY/rOPNk3sgrDQoo//fb4hVC1CLQJ13hef4Y53CI'+#13#10+
  'rU7m2Ys6xt0nUW7/vGT1M0NPAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAPBgNV'+#13#10+
  'HRMBAf8EBTADAQH/MB0GA1UdDgQWBBR5tFnme7bl5AFzgAiIyBpY9umbbjANBgkq'+#13#10+
  'hkiG9w0BAQsFAAOCAgEAVR9YqbyyqFDQDLHYGmkgJykIrGF1XIpu+ILlaS/V9lZL'+#13#10+
  'ubhzEFnTIZd+50xx+7LSYK05qAvqFyFWhfFQDlnrzuBZ6brJFe+GnY+EgPbk6ZGQ'+#13#10+
  '3BebYhtF8GaV0nxvwuo77x/Py9auJ/GpsMiu/X1+mvoiBOv/2X/qkSsisRcOj/KK'+#13#10+
  'NFtY2PwByVS5uCbMiogziUwthDyC3+6WVwW6LLv3xLfHTjuCvjHIInNzktHCgKQ5'+#13#10+
  'ORAzI4JMPJ+GslWYHb4phowim57iaztXOoJwTdwJx4nLCgdNbOhdjsnvzqvHu7Ur'+#13#10+
  'TkXWStAmzOVyyghqpZXjFaH3pO3JLF+l+/+sKAIuvtd7u+Nxe5AW0wdeRlN8NwdC'+#13#10+
  'jNPElpzVmbUq4JUagEiuTDkHzsxHpFKVK7q4+63SM1N95R1NbdWhscdCb+ZAJzVc'+#13#10+
  'oyi3B43njTOQ5yOf+1CceWxG1bQVs5ZufpsMljq4Ui0/1lvh+wjChP4kqKOJ2qxq'+#13#10+
  '4RgqsahDYVvTH9w7jXbyLeiNdd8XM2w9U/t7y0Ff/9yi0GE44Za4rF2LN9d11TPA'+#13#10+
  'mRGunUHBcnWEvgJBQl9nJEiU0Zsnvgc/ubhPgXRR4Xq37Z0j4r7g1SgEEzwxA57d'+#13#10+
  'emyPxgcYxn/eR44/KJ4EBs+lVDR3veyJm+kXQ99b21/+jh5Xos1AnX5iItreGCc='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): ISRG Root X2, (O): Internet Security Research Group
Issuer: Self Signed
Expires: 2040-09-17T16:00:00, Signature: ecdsa-with-SHA384
Valid From: 2020-09-04T00:00:00, Serial Number: 41d29dd172eaeea780c12c6ce92f8752
Fingerprint (sha256): 69729b8e15a86efc177a57afb7171dfc64add28c2fca8cf1507e34453ccb1470
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts036 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICGzCCAaGgAwIBAgIQQdKd0XLq7qeAwSxs6S+HUjAKBggqhkjOPQQDAzBPMQsw'+#13#10+
  'CQYDVQQGEwJVUzEpMCcGA1UEChMgSW50ZXJuZXQgU2VjdXJpdHkgUmVzZWFyY2gg'+#13#10+
  'R3JvdXAxFTATBgNVBAMTDElTUkcgUm9vdCBYMjAeFw0yMDA5MDQwMDAwMDBaFw00'+#13#10+
  'MDA5MTcxNjAwMDBaME8xCzAJBgNVBAYTAlVTMSkwJwYDVQQKEyBJbnRlcm5ldCBT'+#13#10+
  'ZWN1cml0eSBSZXNlYXJjaCBHcm91cDEVMBMGA1UEAxMMSVNSRyBSb290IFgyMHYw'+#13#10+
  'EAYHKoZIzj0CAQYFK4EEACIDYgAEzZvVn4CDCuwJSvMWSj5cz3es3mcFDR0HttwW'+#13#10+
  '+1qLFNvicWDEukWVEYmO6gbf9yoWHKS5xcUy4APgHoIYOIvXRdgKam7mAHf7AlF9'+#13#10+
  'ItgKbppbd9/w+kHsOdx1ymgHDB/qo0IwQDAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0T'+#13#10+
  'AQH/BAUwAwEB/zAdBgNVHQ4EFgQUfEKWrt5LSDv6kviejM9ti6lyN5UwCgYIKoZI'+#13#10+
  'zj0EAwMDaAAwZQIwe3lORlCEwkSHRhtFcP9Ymd70/aTSVaYgLXTWNLxBo1BfASdW'+#13#10+
  'tL4ndQavEi51mI38AjEAi/V3bNTIZargCyzuFJ0nN6T5U6VR5CmD1/iQMVtCnwr1'+#13#10+
  '/q4AaOeMSQ+2b1tbFfLn'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): LAWtrust Root Certification Authority 2048, (O): LAWtrust, (OU): LAW Trusted Third Party Services PTY Ltd.
Issuer: Self Signed
Expires: 2032-05-16T16:10:18, Signature: sha1WithRSAEncryption
Valid From: 2012-05-16T15:40:18, Serial Number: 4fb3d0ee
Fingerprint (sha256): 9b14e8f5f6ea167666e76dcd6becc190861d5e8970b99a9470f0231236049704
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts037 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEHjCCAwagAwIBAgIET7PQ7jANBgkqhkiG9w0BAQUFADCBiTELMAkGA1UEBhMC'+#13#10+
  'WkExETAPBgNVBAoTCExBV3RydXN0MTIwMAYDVQQLEylMQVcgVHJ1c3RlZCBUaGly'+#13#10+
  'ZCBQYXJ0eSBTZXJ2aWNlcyBQVFkgTHRkLjEzMDEGA1UEAxMqTEFXdHJ1c3QgUm9v'+#13#10+
  'dCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAyMDQ4MB4XDTEyMDUxNjE1NDAxOFoX'+#13#10+
  'DTMyMDUxNjE2MTAxOFowgYkxCzAJBgNVBAYTAlpBMREwDwYDVQQKEwhMQVd0cnVz'+#13#10+
  'dDEyMDAGA1UECxMpTEFXIFRydXN0ZWQgVGhpcmQgUGFydHkgU2VydmljZXMgUFRZ'+#13#10+
  'IEx0ZC4xMzAxBgNVBAMTKkxBV3RydXN0IFJvb3QgQ2VydGlmaWNhdGlvbiBBdXRo'+#13#10+
  'b3JpdHkgMjA0ODCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKTckbEK'+#13#10+
  'FR42rhFERZfVJTWHixsK0c9w+iZBsfxKDahatWan3B9uHQjppoYLZkRcuFCiMJYC'+#13#10+
  'C4jIFVQXr/rX5GoPgMfO5eimmbJLf5JNNmVU7iEwI+QPx0LnXcwvGz5rCqc+0Y8H'+#13#10+
  'Lti3+s8YVTWZs9BSuw3nqUsb+/tG/wEJsjdPsf15Ovg27GMq3Ps48bfoYeCR0rt4'+#13#10+
  'FTZ0vR21Xtm9tm4I/Hn2un/kHC1AvR22A6QCyOtqGNt3ZWe1k2o64N0kV6uB4v1x'+#13#10+
  '19de7Y78YMXnufwjprlr99zTJgKabuADhfvFp8ZR7MlpE/QWC+00ASIje90rQZap'+#13#10+
  'Okzqald1KwsPFD8CAwEAAaOBizCBiDArBgNVHRAEJDAigA8yMDEyMDUxNjE1NDAx'+#13#10+
  'OFqBDzIwMzIwNTE2MTYxMDE4WjALBgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUXN46'+#13#10+
  'MzRJZMSSMXxVXvXyO0/uwx0wHQYDVR0OBBYEFFzeOjM0SWTEkjF8VV718jtP7sMd'+#13#10+
  'MAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQEFBQADggEBAJYl5BxGneuWSlaE5zbA'+#13#10+
  'r7IxxqtnyTv3X3GZZK5U4w1KccxcfNI1u0cSx7PEkW1UCTbFREaCF1InNnmLukSU'+#13#10+
  'tIJxZdM1Vf7Drj8j9vpFho1VjvbHmc/PP+RHepzwqVQIuqQ/lIxALIQkAyJFx3Ep'+#13#10+
  'GFxV/O9dh/2nmoMD3L++jESN6/FiWlNpjYADYLMP53hDTKnZsXJAy1hEx3Xo1oni'+#13#10+
  'Sv73kKyE9ybEQOGUuFPcsgPyJiQXZc2yxtOTncJhG1GfzSQbALNltD5qs98Gha2c'+#13#10+
  'h3bc08fCFrHFult+FUU9Nnuc8yanErD2np40mrN3C6pHDoXsFWENtjplBI59Oz+I'+#13#10+
  'c88='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Microsoft ECC Root Certificate Authority 2017, (O): Microsoft Corporation
Issuer: Self Signed
Expires: 2042-07-18T23:16:04, Signature: ecdsa-with-SHA384
Valid From: 2019-12-18T23:06:45, Serial Number: 66f23daf87de8bb14aea0c573101c2ec
Fingerprint (sha256): 358df39d764af9e1b766e9c972df352ee15cfac227af6ad1d70e8e4a6edcba02
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts038 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICWTCCAd+gAwIBAgIQZvI9r4fei7FK6gxXMQHC7DAKBggqhkjOPQQDAzBlMQsw'+#13#10+
  'CQYDVQQGEwJVUzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMTYwNAYD'+#13#10+
  'VQQDEy1NaWNyb3NvZnQgRUNDIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IDIw'+#13#10+
  'MTcwHhcNMTkxMjE4MjMwNjQ1WhcNNDIwNzE4MjMxNjA0WjBlMQswCQYDVQQGEwJV'+#13#10+
  'UzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMTYwNAYDVQQDEy1NaWNy'+#13#10+
  'b3NvZnQgRUNDIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IDIwMTcwdjAQBgcq'+#13#10+
  'hkjOPQIBBgUrgQQAIgNiAATUvD0CQnVBEyPNgASGAlEvaqiBYgtlzPbKnR5vSmZR'+#13#10+
  'ogPZnZH6thaxjG7efM3beaYvzrvOcS/lpaso7GMEZpn4+vKTEAXhgShC48Zo9OYb'+#13#10+
  'hGBKia/teQ87zvH2RPUBeMCjVDBSMA4GA1UdDwEB/wQEAwIBhjAPBgNVHRMBAf8E'+#13#10+
  'BTADAQH/MB0GA1UdDgQWBBTIy5lycFIM+Oa+sgRXKSrPQhDtNTAQBgkrBgEEAYI3'+#13#10+
  'FQEEAwIBADAKBggqhkjOPQQDAwNoADBlAjBY8k3qDPlfXu5gKcs68tvWMoQZP3zV'+#13#10+
  'L8KxzJOuULsJMsbG7X7JNpQS5GiFBqIb0C8CMQCZ6Ra0DvpWSNSkMBaReNtUjGUB'+#13#10+
  'iudQZsIxtzm6uBoiB078a1QWIP8rtedMDE2mT3M='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Microsoft Identity Verification Root Certificate Authority 2020, (O): Microsoft Corporation
Issuer: Self Signed
Expires: 2045-04-16T18:44:40, Signature: sha384WithRSAEncryption
Valid From: 2020-04-16T18:36:16, Serial Number: 5498d2d1d45b1995481379c811c08799
Fingerprint (sha256): 5367f20c7ade0e2bca790915056d086b720c33c1fa2a2661acf787e3292e1270
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts039 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFzDCCA7SgAwIBAgIQVJjS0dRbGZVIE3nIEcCHmTANBgkqhkiG9w0BAQwFADB3'+#13#10+
  'MQswCQYDVQQGEwJVUzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMUgw'+#13#10+
  'RgYDVQQDEz9NaWNyb3NvZnQgSWRlbnRpdHkgVmVyaWZpY2F0aW9uIFJvb3QgQ2Vy'+#13#10+
  'dGlmaWNhdGUgQXV0aG9yaXR5IDIwMjAwHhcNMjAwNDE2MTgzNjE2WhcNNDUwNDE2'+#13#10+
  'MTg0NDQwWjB3MQswCQYDVQQGEwJVUzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBv'+#13#10+
  'cmF0aW9uMUgwRgYDVQQDEz9NaWNyb3NvZnQgSWRlbnRpdHkgVmVyaWZpY2F0aW9u'+#13#10+
  'IFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IDIwMjAwggIiMA0GCSqGSIb3DQEB'+#13#10+
  'AQUAA4ICDwAwggIKAoICAQCzkSoHgwZn/Z6d4MfAt6TmQgR/D6bbX/vVWtdFoPt3'+#13#10+
  'C/CA86ZtWk15U9ighoRXRSDHolT7x6K/isduNfOiFcQvTuNKhZZJDf++mdgU9rwn'+#13#10+
  'B+5Cmyv1C5IG5P1pE2WokXLymITrgz0O5NdxEkghyw3t9kdJt5v5yccXtoRP/7is'+#13#10+
  'mtdzZ0mF44a9N0DQJYbU3rXCbWJq1al4vC1vSfnlbBQU/RTH02UWN97LbrxeKY39'+#13#10+
  'YpsVLNYF5rmJMjOjYsfX1lJnCMQu9FYrnguHzOyntKaq6wXNGVelOgsEJxyRZ54t'+#13#10+
  'Yi0vHr7awCDLBBnKM/uJvpjicqByNb554ZyDb+RtF2+Q8z0AhnU4jtDgSZq729P4'+#13#10+
  'MMrVV4hoTXLTv21/cdj9vQ2ukmRIt1tveSa1zZuVIYTR7w8yPXtXjPNFB0x84F4Y'+#13#10+
  'DjV2i22eyzZ0qwX44HNdMlaUZ5clCsY1PZSX58FEi4D9wfj0dBnlMPYG+yFXPgYc'+#13#10+
  'i2sVhidJe4KTylnodUfoPzj0x1N5oLa04lxR771fOMET5ngMlVouxUBZKMwPJMDs'+#13#10+
  'ugl3I5k4prYc2se6ILbXN9h/N68I4ztx225zG32ZcrDkhjNZdLUWAHtQbcaGE9r9'+#13#10+
  'xDmCPSQAmmDaupTABVEsNKxQmROHu7MFgLJNMAJcuCaDXbRjc++uI5VPYCi+N9Vb'+#13#10+
  'pQIDAQABo1QwUjAOBgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAdBgNV'+#13#10+
  'HQ4EFgQUyH7SaoUqG8oZmAQHJ89QEE9oqKIwEAYJKwYBBAGCNxUBBAMCAQAwDQYJ'+#13#10+
  'KoZIhvcNAQEMBQADggIBAK9q3eYZ5y2UQxlOy+lQlWSlA5ECi+I2gDsVolLCFhm2'+#13#10+
  'alpddEMw9Jv/YHQJsSEekBZtxSSPXGaIY/RPzH3yEkxAEIsBn9qpyK7ylRvPnQXr'+#13#10+
  'ST50oGhb5VYsZRyCflPaVtlGF3mSRcQQNghSKRfLL6byftRpJIoej7BzDcwcSquy'+#13#10+
  'qu2nkWMBZCKoMrh+MiizZ3MtkbTcMQEL90cKpvHXSu1WYMQsCKN7QLC8dCdSh9a+'+#13#10+
  'iN03ioluZ4gd9cldoP62qzqA1xqXPBc2IkEerE3Vg+Y8OL1PMOlUqdO2BMMydmG7'+#13#10+
  'sBjFKxizwIDVt5WwXlFNIvzsWKro2JS0pS7tkt7nGHwhV91VY/e/bc0f0qZ3KHDH'+#13#10+
  '4ls6WwjSW07IAJaz4YM2r4YKZVx09ursemp0oPBL7u+Uo6xQ8oft1zowg8n7fVe+'+#13#10+
  '5eP4QcrlZK6zo+xY7IWazO+56vNWGLlcc5qvxXcXg1nbNxoYclSlQdK2I3WjQ5rl'+#13#10+
  'd3yWebdBjb/s3ICgn9F3dVhfNRPgJRpnC33OJfoHCuRhIdjUHOUHxjaZ9JbQxhX+'+#13#10+
  'Ts3Xroud2xb9BMaSvdSI5qmjqrv3ZDg7X8wM0DW+dBkDpsWqTKJhNoI+HfMrvJdd'+#13#10+
  '20t4Oy31O+9gI+j17AsjNpWvmGa/U9N7uGlKKpZmacSUxvRfbqyYeIiABlyisu2i'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Microsoft RSA Root Certificate Authority 2017, (O): Microsoft Corporation
Issuer: Self Signed
Expires: 2042-07-18T23:00:23, Signature: sha384WithRSAEncryption
Valid From: 2019-12-18T22:51:22, Serial Number: 1ed397095fd8b4b347701eaabe7f45b3
Fingerprint (sha256): c741f70f4b2a8d88bf2e71c14122ef53ef10eba0cfa5e64cfa20f418853073e0
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts040 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFqDCCA5CgAwIBAgIQHtOXCV/YtLNHcB6qvn9FszANBgkqhkiG9w0BAQwFADBl'+#13#10+
  'MQswCQYDVQQGEwJVUzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMTYw'+#13#10+
  'NAYDVQQDEy1NaWNyb3NvZnQgUlNBIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5'+#13#10+
  'IDIwMTcwHhcNMTkxMjE4MjI1MTIyWhcNNDIwNzE4MjMwMDIzWjBlMQswCQYDVQQG'+#13#10+
  'EwJVUzEeMBwGA1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMTYwNAYDVQQDEy1N'+#13#10+
  'aWNyb3NvZnQgUlNBIFJvb3QgQ2VydGlmaWNhdGUgQXV0aG9yaXR5IDIwMTcwggIi'+#13#10+
  'MA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDKW76UM4wplZEWCpW9R2LBifOZ'+#13#10+
  'Nt9GkMml7Xhqb0eRaPgnZ1AzHaGm++DlQ6OEAlcBXZxIQIJTELy/xztokLaCLeX0'+#13#10+
  'ZdDMbRnMlfl7rEqUrQ7eS0MdhweSE5CAg2Q1OQT85elss7YfUJQ4ZVBcF0a5toW1'+#13#10+
  'HLUX6NZFndiyJrDKxHBKrmCk3bPZ7Pw71VdyvD/IybLeS2v4I2wDwAW9lcfNcztm'+#13#10+
  'gGTjGqwu+UcF8ga2m3P1eDNbx6H7JyqhtJqRjJHTOoI+dkC0zVJhUXAoP8XFWvLJ'+#13#10+
  'jEm7FFtNyP9nTUwSlq31/niol4fX/V4ggNyhSyL71Imtus5Hl0dVe49FyGcohJUc'+#13#10+
  'aDDv70ngNXtk55iwlNpNhTs+VcQor1fznhPbRiefHqJeRIOkpcrVE7NLP8TjwuaG'+#13#10+
  'YaRSMLl6IE9vDzhTyzMMEyuP1pq9KsgtsRx9S1HKR9FIJ3Jdh+vVReZIZZ2vUpC6'+#13#10+
  'W6IYZVcSn2i51BVrlMRpIpj0M+Dt+VGOQVDJNE92kKz8OMHY4Xu54+OU4UZpyw4K'+#13#10+
  'UGsTuqwPN1q3ErWQgR5WrlcihtnJ0tHXUeOrO8ZV/R4O03QK0dqq6mm4lyiPSMQH'+#13#10+
  '+FJDOvTKVTUssKZqwJz58oHhEmrARdlns87/I6KJClTUFLkqqNfs+avNJVgyeY+Q'+#13#10+
  'W5g5xAgGwax/Dj0ApQIDAQABo1QwUjAOBgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/'+#13#10+
  'BAUwAwEB/zAdBgNVHQ4EFgQUCctZf4aycI8awznjwNnpv7tNsiMwEAYJKwYBBAGC'+#13#10+
  'NxUBBAMCAQAwDQYJKoZIhvcNAQEMBQADggIBAKyvPl3CEZaJjqPnktaXFbgToqZC'+#13#10+
  'LgLNFgVZJ8og6Lq46BrsTaiXVq5lQ7GPAJtSzVXNUzltYkyLDVt8LkS/gxCP81OC'+#13#10+
  'gMNPOsduET/m4xaRhPtthH80dK2Jp86519efhGSSvpWhrQlTM93uCupKUY5vVau6'+#13#10+
  'tZRGrox/2KJQJWVggEbbMwSubLWYdFQl3JPk+ONVFT24bcMKpBLBaYVu32TxU5nh'+#13#10+
  'SnUgnZUP5NbcA/FZGOhHibJXWpS2qdgXKxdJ5XbLwVaZOjex/2kskZGT4d9Mozd2'+#13#10+
  'TaGf+G0eHdP67Pv0RR0Tbc/3WeUiJ3IrhvNXuzDtJE3cfVa7o7P4NHmJweDyAmH3'+#13#10+
  'pvwPuxwXC65B2Xy9J6P9LjrRk5Sxcx0ki69bIImtt2dmefU6xqaWM/5TkshGsRGR'+#13#10+
  'xpl/j8nWZjEgQRCHLQzWwa80mMpkg/sTV9HB8Dx6jKXB/ZUhoHHBk2dxEuqPiApp'+#13#10+
  'GWSZI1b7rCoucL5mxAyE7+WL85MB+GqQk2dLsmijtWKP6T+MejteD+eMuMZ87zf9'+#13#10+
  'dOLITzNy4ZQ5bb0Sr74MTnB8G2+NszKTc0QWbej09+CVgI+WXTik9KveCjCHk9hN'+#13#10+
  'AHFiRSdLOkKEW39lt2c0Ui2cFmuqqNh7o0JMcccMyj6D5KbvtwEwXlGjefVwaaZB'+#13#10+
  'RA+GsCyRxj3qrg+E'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Microsoft Time Stamp Root Certificate Authority 2014, (O): Microsoft Corporation
Issuer: Self Signed
Expires: 2039-10-22T22:15:19, Signature: sha256WithRSAEncryption
Valid From: 2014-10-22T22:08:57, Serial Number: 2fd67a432293329045e953343ee27466
Fingerprint (sha256): 65af95f4be86847344634282f941b2e605063ef0c8542f014ca088d182109e4f
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts041 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIGAzCCA+ugAwIBAgIQL9Z6QyKTMpBF6VM0PuJ0ZjANBgkqhkiG9w0BAQsFADCB'+#13#10+
  'kzELMAkGA1UEBhMCVVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcTB1Jl'+#13#10+
  'ZG1vbmQxHjAcBgNVBAoTFU1pY3Jvc29mdCBDb3Jwb3JhdGlvbjE9MDsGA1UEAxM0'+#13#10+
  'TWljcm9zb2Z0IFRpbWUgU3RhbXAgUm9vdCBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkg'+#13#10+
  'MjAxNDAeFw0xNDEwMjIyMjA4NTdaFw0zOTEwMjIyMjE1MTlaMIGTMQswCQYDVQQG'+#13#10+
  'EwJVUzETMBEGA1UECBMKV2FzaGluZ3RvbjEQMA4GA1UEBxMHUmVkbW9uZDEeMBwG'+#13#10+
  'A1UEChMVTWljcm9zb2Z0IENvcnBvcmF0aW9uMT0wOwYDVQQDEzRNaWNyb3NvZnQg'+#13#10+
  'VGltZSBTdGFtcCBSb290IENlcnRpZmljYXRlIEF1dGhvcml0eSAyMDE0MIICIjAN'+#13#10+
  'BgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEArgHUXaBYyu3ozOE3RkYQW3rEUrgL'+#13#10+
  'WI8FPV2prVHiA4ngBhL4AnhrXgRQz7bWBAxUHnk3IDzjfmMdRXeYFB2+diLcWqo/'+#13#10+
  '5G9AYiRjyDzATIcoPWt4a5g5lRpBf3NR/gf8FHzzj4QJ4fjCL6FOvTl9zGNniQyA'+#13#10+
  'BM2wgskAiz4JhwOdwnlCxFwhkSuVGmw1R2zIvzwKTur2hXDVxV/BnkfbXMIyYVoI'+#13#10+
  '1nGdLIGffri+baHYZkNpCuTzcvCRSyhgqNXj3YSuKGVVn4QrSnXtJKYsdTHUhXd0'+#13#10+
  '8oBVAmNB8nAI9MjCU5HbFAdlIAmB5orXmw/KDNcbX/3R5XSFXBD7msmmK55Dlsxb'+#13#10+
  'cnPQD1WZhxgbPfgpeLBv0XS85SC6Q4sUOGlkoXMPwRYpeU+bhSlosT6ZKo+y3EcG'+#13#10+
  'zd/Q6yLcHlccflmQJaMDgr6Myx2buY0quKEQ5/qtFv7s5VPGrcCXfESbgfN6pvn/'+#13#10+
  'rvqsF6mmYL1nPHlshQtVrzHEw1mQDqHVfEg5i63juw7k5frf/dqdnltvGzIOpjfT'+#13#10+
  'qqosBBdl08ZORyStglCZQSvWs+cmWrE1m+ZxVeHIb6JEHchchPz5eAF2wT53k/Ki'+#13#10+
  'lOHacDDsZAquoqEdP4NDc0DS4IlwWa+NLtTUIQphpPT3I4ZDgCiyHEMMRdr8Bvgl'+#13#10+
  'QAd1aXjjphOD15cCAwEAAaNRME8wCwYDVR0PBAQDAgGGMA8GA1UdEwEB/wQFMAMB'+#13#10+
  'Af8wHQYDVR0OBBYEFMvR8s5I/QGf6laqV9F+mVj4P//gMBAGCSsGAQQBgjcVAQQD'+#13#10+
  'AgEAMA0GCSqGSIb3DQEBCwUAA4ICAQAT2NPko9gmzv07R++AvvujloXafZWcKQBi'+#13#10+
  'Rr5ICsoU7B1Osh71zkavWQEQOP3iZQu/+A/mddncM8qD764gV30n+shcID4Lhiy8'+#13#10+
  'EnYDXNMhU6DPPvdFGSIPbiE3xmiHxJwpVaOQ6KkevrNB549H6R00xWQkW0wy7Laa'+#13#10+
  'DLhW4AbkQIvyEAf6jo5mIOYcS+Slo7suBulFe57J/5SKV8Fpo11lWN20wmNKpt1j'+#13#10+
  'MRiv7RYY2sFqPx/Sqpa2YW/Vgym0eWbBwVADHNDqLsa6z8aYbdYbxs4QsMnxQxor'+#13#10+
  '1/8VNIY72Uo8bT4juwI9zlTDSiXvRjx5W46zwiqCEkVSlsIJ1Ep4nt1vn/mfcEqa'+#13#10+
  'o03vLfqqlvq0fdY2l87w2HzSL1ZUCgBg0DyOaOLNKao9LiCDy7JVRqDfuJF5KJJB'+#13#10+
  'Dv4mOEN103el3YdS8U2dv9yjLfIeD0kspRGwijYTObD1G5J3tIPdmJ4Fr6CjCdDf'+#13#10+
  'HXaYQkQBc7CyqTtS5bZvq4zy1Rcpf2/45aM0625FkkhNAlW2N6ECsTTfx7KSPQK9'+#13#10+
  'NxoG4aGAjpIlMc72geeu5ZIXrFnEkqzfyCwnUkIeJh14h7lOi/dHescBcNWhyQui'+#13#10+
  'Igg4/MqowjtT3As2O+Gjyq33tgjDE1WvAzpptOmk0S3NZ9TDQspjX56ApOxjbHLE'+#13#10+
  'WOUH+pb4jQ=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): QuoVadis Root CA 2, (O): QuoVadis Limited
Issuer: Self Signed
Expires: 2031-11-24T18:23:33, Signature: sha1WithRSAEncryption
Valid From: 2006-11-24T18:27:00, Serial Number: 0509
Fingerprint (sha256): 85a0dd7dd720adb7ff05f83d542b209dc7ff4528f7d677b18389fea5e5c49e86
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts042 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFtzCCA5+gAwIBAgICBQkwDQYJKoZIhvcNAQEFBQAwRTELMAkGA1UEBhMCQk0x'+#13#10+
  'GTAXBgNVBAoTEFF1b1ZhZGlzIExpbWl0ZWQxGzAZBgNVBAMTElF1b1ZhZGlzIFJv'+#13#10+
  'b3QgQ0EgMjAeFw0wNjExMjQxODI3MDBaFw0zMTExMjQxODIzMzNaMEUxCzAJBgNV'+#13#10+
  'BAYTAkJNMRkwFwYDVQQKExBRdW9WYWRpcyBMaW1pdGVkMRswGQYDVQQDExJRdW9W'+#13#10+
  'YWRpcyBSb290IENBIDIwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQCa'+#13#10+
  'GMpLlA0ALa8DKYrwD4HIrkwZhR0In6spRIXzL4GtMh6QRr+jhiYaHv5+HBg6XJxg'+#13#10+
  'Fyo6dIMzMH1hVBHL7avg5tKifvVrbxi3Cgst/ek+7wrGsxDp3MJGF/hd/aTa/55J'+#13#10+
  'WpzmM+Yklvc/ulsrHHo1wtZn/qtmUIttKGAr79dgw8eTvI02kfN/+NsRE8Scd3bB'+#13#10+
  'rrcCaoF6qUWD4gXmuVbBlDePSHFjIuwXZQeVikvfj8ZaCuWw419eaxGrDPmF60Tp'+#13#10+
  '+ARz8un+XJiM9XOva7R+zdRcAitMOeGylZUtQofX1bOQQ7dsE/He3fbE+Ik/0XX1'+#13#10+
  'ksOR1YqI0JDs3G3eicJlcZaLDQP9nL9bFqyS2+r+eXyt66/3FsvbzSUr5R/7mp/i'+#13#10+
  'Ucw6UwxI5g69ybR2BlLmEROFcmMDBOAENisgGQLodKcftslWZvB1JdxnwQ5hYIiz'+#13#10+
  'PtGo/KPaHbDRsSNU30R2be1B2MGyIrZTHN81Hdyhdyox5C315eXbyOD/5YDXC2Og'+#13#10+
  '/zOhD7osFRXql7PSorW+8oyWHhqPHWykYTe5hnMz15eWniN9gqRMgeKh0bpnX5UH'+#13#10+
  'oycR7hYQe7xFSkyyBNKr79X9DFHOUGoIMfmR2gyPZFwDwzqLID9ujWc9Otb+fVuI'+#13#10+
  'yV77zGHcizN300QyNQliBJIWENieJ0f7OyHj+OsdWwIDAQABo4GwMIGtMA8GA1Ud'+#13#10+
  'EwEB/wQFMAMBAf8wCwYDVR0PBAQDAgEGMB0GA1UdDgQWBBQahGK8SEwzJQTU7tD2'+#13#10+
  'A8QZRtGUazBuBgNVHSMEZzBlgBQahGK8SEwzJQTU7tD2A8QZRtGUa6FJpEcwRTEL'+#13#10+
  'MAkGA1UEBhMCQk0xGTAXBgNVBAoTEFF1b1ZhZGlzIExpbWl0ZWQxGzAZBgNVBAMT'+#13#10+
  'ElF1b1ZhZGlzIFJvb3QgQ0EgMoICBQkwDQYJKoZIhvcNAQEFBQADggIBAD4KFk2f'+#13#10+
  'BluornFdLwUvZ+YTRYPENvbzwCYMDbVHZF34tHLJRqUDGCdViXh9duqWNIAXINzn'+#13#10+
  'g/iN/Ae42l9NLmeyhP3ZRPx3UIHmfLTJDQtyU/h2BwdBR5YM++CCJpNVjP4iH2Bl'+#13#10+
  'fF/nJrP3MpCYUNQ3cVX2kiF495V5+vgtJodmVjB3pjd4M1IQWK4/YY7yarHvGH5K'+#13#10+
  'WWPKjaJW1acvvFYfzznB4vsKqBUsfU16Y8Zsl0Q80m/DShcK+JDSV6IZUaUtl0Ha'+#13#10+
  'B0+pUNqQjZRG4T7wlP0QADj1O+hA4bRuVhogzG9Yje0uRY/W6ZM/57Es3zrWIozc'+#13#10+
  'hLsib9D45MY56QSIPMO661V6bYCZJPVsAfv4l7CUW+v90m/xd2gNNWQjrLhVoQPR'+#13#10+
  'TUIZ3Ph1WVaj+ahJefivDrkRoHy3au000LYmYjgahwz46P0u05B/B5EqHdZ+XIWD'+#13#10+
  'mbA4CD/pXvk1B+TJYm5Xf6dQlfe6yJvmjqIBxdZmv3lh8zwc4bmCXF2gw+nYSL0Z'+#13#10+
  'ohEUGW6yhhtoPkg3Goi3XZZenMfvJ2II4pEZXNLxId26F0KCl3GBUzGpn/Z9Yr9y'+#13#10+
  '4aOTHcyKJloJONDO1w2AFrR4pTqHTI2KpdVGl/IsELm8VCLAAVBpQ570su9t+Oza'+#13#10+
  '8eOx79+Rj1QqCyXBJhnEUhAFZdWCEOrCMc0u'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SSL.com EV Root Certification Authority ECC, (O): SSL Corporation
Issuer: Self Signed
Expires: 2041-02-12T18:15:23, Signature: ecdsa-with-SHA256
Valid From: 2016-02-12T18:15:23, Serial Number: 2c299c5b16ed0595
Fingerprint (sha256): 22a2c1f7bded704cc1e701b5f408c310880fe956b5de2a4a44f99c873a25a7c8
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts043 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIClDCCAhqgAwIBAgIILCmcWxbtBZUwCgYIKoZIzj0EAwIwfzELMAkGA1UEBhMC'+#13#10+
  'VVMxDjAMBgNVBAgMBVRleGFzMRAwDgYDVQQHDAdIb3VzdG9uMRgwFgYDVQQKDA9T'+#13#10+
  'U0wgQ29ycG9yYXRpb24xNDAyBgNVBAMMK1NTTC5jb20gRVYgUm9vdCBDZXJ0aWZp'+#13#10+
  'Y2F0aW9uIEF1dGhvcml0eSBFQ0MwHhcNMTYwMjEyMTgxNTIzWhcNNDEwMjEyMTgx'+#13#10+
  'NTIzWjB/MQswCQYDVQQGEwJVUzEOMAwGA1UECAwFVGV4YXMxEDAOBgNVBAcMB0hv'+#13#10+
  'dXN0b24xGDAWBgNVBAoMD1NTTCBDb3Jwb3JhdGlvbjE0MDIGA1UEAwwrU1NMLmNv'+#13#10+
  'bSBFViBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IEVDQzB2MBAGByqGSM49'+#13#10+
  'AgEGBSuBBAAiA2IABKoSR5CYG/vvw0AHgyBO8TCCogbR8pKGYfL2IWjKAMTH6kMA'+#13#10+
  'VIbc/R/fALhBYlzccBYy3h+Z1MzFB8gIH2EWB1E9fVwHU+M1OIzfzZ/ZLg1Kthku'+#13#10+
  'WnBaBu2+8KGwytAJKaNjMGEwHQYDVR0OBBYEFFvKXuXe0oGqzagtZFG22XKbl+ZP'+#13#10+
  'MA8GA1UdEwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUW8pe5d7SgarNqC1kUbbZcpuX'+#13#10+
  '5k8wDgYDVR0PAQH/BAQDAgGGMAoGCCqGSM49BAMCA2gAMGUCMQCK5kCJN+vp1RPZ'+#13#10+
  'ytRrJPOwPYdGWBrssd9v+1a6cGvHOMzosYxPD/fxZ3YOg9AeUY8CMD32IygmTMZg'+#13#10+
  'h5Mmm7I1HrrW9zzRHM76JTymGoEVW/MSD2zuZYrJh6j5B+BimoxcSg=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SSL.com EV Root Certification Authority RSA R2, (O): SSL Corporation
Issuer: Self Signed
Expires: 2042-05-30T18:14:37, Signature: sha256WithRSAEncryption
Valid From: 2017-05-31T18:14:37, Serial Number: 56b629cd34bc78f6
Fingerprint (sha256): 2e7bf16cc22485a7bbe2aa8696750761b0ae39be3b2fe9d0cc6d4ef73491425c
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts044 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIF6zCCA9OgAwIBAgIIVrYpzTS8ePYwDQYJKoZIhvcNAQELBQAwgYIxCzAJBgNV'+#13#10+
  'BAYTAlVTMQ4wDAYDVQQIDAVUZXhhczEQMA4GA1UEBwwHSG91c3RvbjEYMBYGA1UE'+#13#10+
  'CgwPU1NMIENvcnBvcmF0aW9uMTcwNQYDVQQDDC5TU0wuY29tIEVWIFJvb3QgQ2Vy'+#13#10+
  'dGlmaWNhdGlvbiBBdXRob3JpdHkgUlNBIFIyMB4XDTE3MDUzMTE4MTQzN1oXDTQy'+#13#10+
  'MDUzMDE4MTQzN1owgYIxCzAJBgNVBAYTAlVTMQ4wDAYDVQQIDAVUZXhhczEQMA4G'+#13#10+
  'A1UEBwwHSG91c3RvbjEYMBYGA1UECgwPU1NMIENvcnBvcmF0aW9uMTcwNQYDVQQD'+#13#10+
  'DC5TU0wuY29tIEVWIFJvb3QgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgUlNBIFIy'+#13#10+
  'MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAjzZlQOHWTcDXtOlG2mvq'+#13#10+
  'M0fNTPl9fb69LT3w23jhhqXZuglXaO1XPqDQCEGD5yhBJB/jchXQARr7XnAjssuf'+#13#10+
  'OePPxU7Gkm0mxnu7s9onnQqG6YE3Bf7wcXHswxzpY6IXFJ3vG2fThVUCAtZJycxa'+#13#10+
  '4bH3bzKfydQ7iEGonL3Lq9ttewkfokxykNorCPzPPFTOZw+oz12WGQvE43LrrdF9'+#13#10+
  'HSfvkusQv1vrO6/PgN3B0pYEW3p+pKk8OHakYo6gOV7qd89dAFmPZiw+B6KjBSYR'+#13#10+
  'aZfqhbcPlgtLyEDhULouisv3D5oi53+aNxPN8k0TayHRwMwi8qFG9kRpnMphNQcA'+#13#10+
  'b9ZhCBHqurj26bNg5U257J8UZslXWNvNh2n4ioYSA0e/ZhN2rHd9NCSFg83XqpyQ'+#13#10+
  'Gp8hLH94t2S42Oim9HizVcuE0jLEeK6jj2HdzghTreyI/BXkmg3mnxp3zkyPuBQV'+#13#10+
  'PWKchjgGAGYS5Fl2WlPAApiiECtoRHuOec4zSnaqW4EWG7WK2NAAe15itAnWhmMO'+#13#10+
  'pgWVSbooi4iTsjQc2KRVbrcc0N6ZVTsj9CLg+SlmJuwgUHfbSguPvuUCYHBBXtSu'+#13#10+
  'UDkiFCbLsjtzdFVHB3mBOagwE0TlBIqulhMlQg+5U8Sb/M3kHN48+qvWBkofZ6aY'+#13#10+
  'MBzdLNvcGJVXZsb/XItW9XcCAwEAAaNjMGEwDwYDVR0TAQH/BAUwAwEB/zAfBgNV'+#13#10+
  'HSMEGDAWgBT5YLvU49U09rj1BoAlp3PbRmmonjAdBgNVHQ4EFgQU+WC71OPVNPa4'+#13#10+
  '9QaAJadz20ZpqJ4wDgYDVR0PAQH/BAQDAgGGMA0GCSqGSIb3DQEBCwUAA4ICAQBW'+#13#10+
  's47LCp1Jjr+kxJG7ZhcFUZh1++VQLHqe8RT6q9OKPv+RKY9ji9i0qVQBDb6Thi/5'+#13#10+
  'Sm3HXvVX+cpVHBK+Rw82xd9qt9t1wkclf7nxY/hoLVUE0fKNsKTPvDxeH3jnpaAg'+#13#10+
  'cLAExbf3cqfeIg29MyVGjGSSJuM+LmOW2puMPfgYCdcDzH2GguDKBAdRUNf/ktUM'+#13#10+
  '79qGn5nX67evaOI5JpS6aLe/g9Pqemc9YmeuJeVy6OLk7K4S9ksrPJ/psEDzOFSz'+#13#10+
  '/bdoyNrGj1E8svuR3Bznm53htw1yj+KkxKl4+esUrMZDBcJlOSgYAsOCsp0FvmXt'+#13#10+
  'll9ldDz7CTUue5wT/RsPXcdtgTpWD8w74a8CLyKsRspGPKAcTNZEtF4uXBVmCeEm'+#13#10+
  'Kf7GUmG6sXP/wwyc5WxqlD8UykAWlYTzWamsX0xhk23RO8yilQwipmdnRC652dKK'+#13#10+
  'QbNmC1r7fSOl8hqw/96bg5Qu0T/fkreRrwU7ZcegbLHNYhLDkBvjJc40vG93drEQ'+#13#10+
  'w/cFGsDWr3RiSBd3kmmQYRzelYB0VI8YHMPzA9C/pEN1hlMYegouCRw2n5H9gooi'+#13#10+
  'S9EOUCXdywMMF8mDAAhONU2Ki+3wApRmLER/y5UnlhetCTCstnEXbosX9hwJ1C07'+#13#10+
  'mKVx01QT2WDz9UtmT/rx7iASjbSsV7FFY6GsdqnC+w=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SSL.com Root Certification Authority ECC, (O): SSL Corporation
Issuer: Self Signed
Expires: 2041-02-12T18:14:03, Signature: ecdsa-with-SHA256
Valid From: 2016-02-12T18:14:03, Serial Number: 75e6dfcbc1685ba8
Fingerprint (sha256): 3417bb06cc6007da1b961c920b8ab4ce3fad820e4aa30b9acbc4a74ebdcebc65
Public Key: ECDSA Key Encryption secp384r1 384 bits, 192 security bits }

sslRootCACerts045 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIICjTCCAhSgAwIBAgIIdebfy8FoW6gwCgYIKoZIzj0EAwIwfDELMAkGA1UEBhMC'+#13#10+
  'VVMxDjAMBgNVBAgMBVRleGFzMRAwDgYDVQQHDAdIb3VzdG9uMRgwFgYDVQQKDA9T'+#13#10+
  'U0wgQ29ycG9yYXRpb24xMTAvBgNVBAMMKFNTTC5jb20gUm9vdCBDZXJ0aWZpY2F0'+#13#10+
  'aW9uIEF1dGhvcml0eSBFQ0MwHhcNMTYwMjEyMTgxNDAzWhcNNDEwMjEyMTgxNDAz'+#13#10+
  'WjB8MQswCQYDVQQGEwJVUzEOMAwGA1UECAwFVGV4YXMxEDAOBgNVBAcMB0hvdXN0'+#13#10+
  'b24xGDAWBgNVBAoMD1NTTCBDb3Jwb3JhdGlvbjExMC8GA1UEAwwoU1NMLmNvbSBS'+#13#10+
  'b290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IEVDQzB2MBAGByqGSM49AgEGBSuB'+#13#10+
  'BAAiA2IABEVuqVDEpiM2nl8ojRfLliJkP9x6jh3MCLOicSS6jkm5BBtHllirLZXI'+#13#10+
  '7Z4INcgn64mMU1jrYor+8FsPazFSY0E7ic3s7LaNGdM0B9y7xgZ/wkWV7Mt/qCPg'+#13#10+
  'CemB+vNH06NjMGEwHQYDVR0OBBYEFILRhXMw5zUE044CkvvlpNHEIejNMA8GA1Ud'+#13#10+
  'EwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUgtGFczDnNQTTjgKS++Wk0cQh6M0wDgYD'+#13#10+
  'VR0PAQH/BAQDAgGGMAoGCCqGSM49BAMCA2cAMGQCMG/n61kRpGDPYbCWe+0F+S8T'+#13#10+
  'kdzt5fxQaxFGRrMcIQBiu77D5+jNB5n5DQtdcj7EqgIwH7y6C+IwJPt8bYBVCpk+'+#13#10+
  'gA0z5Wajs6O7pdWLjwkspl1+4vAHCGht0nxpbl/f5Wpl'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SSL.com Root Certification Authority RSA, (O): SSL Corporation
Issuer: Self Signed
Expires: 2041-02-12T17:39:39, Signature: sha256WithRSAEncryption
Valid From: 2016-02-12T17:39:39, Serial Number: 7b2c9bd316803299
Fingerprint (sha256): 85666a562ee0be5ce925c1d8890a6f76a87ec16d4d7d5f29ea7419cf20123b69
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts046 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIF3TCCA8WgAwIBAgIIeyyb0xaAMpkwDQYJKoZIhvcNAQELBQAwfDELMAkGA1UE'+#13#10+
  'BhMCVVMxDjAMBgNVBAgMBVRleGFzMRAwDgYDVQQHDAdIb3VzdG9uMRgwFgYDVQQK'+#13#10+
  'DA9TU0wgQ29ycG9yYXRpb24xMTAvBgNVBAMMKFNTTC5jb20gUm9vdCBDZXJ0aWZp'+#13#10+
  'Y2F0aW9uIEF1dGhvcml0eSBSU0EwHhcNMTYwMjEyMTczOTM5WhcNNDEwMjEyMTcz'+#13#10+
  'OTM5WjB8MQswCQYDVQQGEwJVUzEOMAwGA1UECAwFVGV4YXMxEDAOBgNVBAcMB0hv'+#13#10+
  'dXN0b24xGDAWBgNVBAoMD1NTTCBDb3Jwb3JhdGlvbjExMC8GA1UEAwwoU1NMLmNv'+#13#10+
  'bSBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IFJTQTCCAiIwDQYJKoZIhvcN'+#13#10+
  'AQEBBQADggIPADCCAgoCggIBAPkP3aMrfcvQKv7sZ4Wm5y4bunfh4/WvpOz6Sl2R'+#13#10+
  'xFdHaxh3a3by/ZPkPQ/CFp4LZsNWlJ4Xg4XOVu/yFv0AYvUiCVToZRdOQbngT0aX'+#13#10+
  'qhvIuG5iXmmxX9sqAn78bMrzQdjt0Oj8P2FI7bADFB0QDksZ4LtO7IZl/zbzXmcC'+#13#10+
  'C52GVWH9ejjt/uIZALdvoVBidXQ8oPrIJZK0bnoix/geoeOy3ZExqysdBP+lSgQ3'+#13#10+
  '6YWkMyv94tZVNHwZpEpox7Ko07fKoZOI68GXvIz5HdkihCR0xwQ9aqkpk8zruFvh'+#13#10+
  '/l8lqjRYyMEjVJ0bmBHDOJx+PYZspQ9AhnwC9FwCTyjLrnGfDzrIM/4RJTXq/LrF'+#13#10+
  'YD3ZfBjVsqnTdXgDciLKOsMf7yzlLqn6niy2UUb9rwPW6mBo6oUWNmuF6R7As93E'+#13#10+
  'JNyAKoFBbZQ+yODJgUEAnl6/f8UImKIYLEJAs/lvOCdLToD0PYFH4Ih86hzOtXVc'+#13#10+
  'US4cK38acijnALXRdMbX5J+tB5O2UzU1/Dfkw/ZdFr4hc96SCvigY2q8lpJqPvi8'+#13#10+
  'ZVWb3vUNiSYE/CUapiVpy8JtynziWV+XrOvvLsi81xtZPCvM8hnIk2snYxnP/Okm'+#13#10+
  '+Mpxm3+T/jRnhE6Z6/yzeAkzcLpmpnbtG3PrGqUNxCITIJRWCk4sbE6x/c+cCbqi'+#13#10+
  'M+2HAgMBAAGjYzBhMB0GA1UdDgQWBBTdBAkHovV6fVJTEpKV7jiAJQ2mWTAPBgNV'+#13#10+
  'HRMBAf8EBTADAQH/MB8GA1UdIwQYMBaAFN0ECQei9Xp9UlMSkpXuOIAlDaZZMA4G'+#13#10+
  'A1UdDwEB/wQEAwIBhjANBgkqhkiG9w0BAQsFAAOCAgEAIBgRlCn7Jp0cHh5wYfGV'+#13#10+
  'cpNxJK1ok1iOMq8bs3AD/CUrdIWQPXhq9LmLpZc7tRiRux6n+UBbkflVma8eEdBc'+#13#10+
  'Hadm47GUBwwyOabqG7B52B2ccETjit3E+ZUfijhDPwGFpUenPUayvOUiaPd7nNgs'+#13#10+
  'PgohyC0zrL/FgZkxdMF1ccW+sfAjRfSda/wZY52jvATGGAslu1OJD7OAUN5F7kR/'+#13#10+
  'q5R4ZJjT9ijdh9hwZXT7DrkT66cPYakylszeu+1jTBi7qUD3oFRuIIhxdRjqerQ0'+#13#10+
  'cuAjJ3dctpDqhiVAq+8zD8ufgr6iIPv2tS0a5sKFsXQP+8hlAqRSAUfdSSLBv9jr'+#13#10+
  'a6x+3uxjMxW3IwiPxg+NQVrdjsW5j+VFP3jbutIbQLH+cU0/4IGiul607BXgk90I'+#13#10+
  'H37hVZkLId6Tngr75qNJvTYw/ud3sqB1l7UtgYgXZSD32pAAn8lSzDLKNXz1PQ/Y'+#13#10+
  'K9f1JmzJBjSWFupwWRoyeXkLtoh/D1JIPb9s2KJELtFOt3JY04kTlf5Eq/jXixtu'+#13#10+
  'nLwsoFvVagCvXzfh1foQC5ichucmj87w7G6KVwuA406ywKBjYZC6VWg3dGq2ktuf'+#13#10+
  'oYYitmUnDuy2n0Jg5GfCtdpBC8TTi2EbvPofkSvXRAdeuims2cXp71NIWuuA8ShY'+#13#10+
  'Ic2wBlX7Jz9TkHCpBB5XJ7k='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SecureTrust CA, (O): SecureTrust Corporation
Issuer: Self Signed
Expires: 2029-12-31T19:40:55, Signature: sha1WithRSAEncryption
Valid From: 2006-11-07T19:31:18, Serial Number: 0cf08e5c0816a5ad427ff0eb271859d0
Fingerprint (sha256): f1c1b50ae5a20dd8030ec9f6bc24823dd367b5255759b4e71b61fce9f7375d73
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts047 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIDuDCCAqCgAwIBAgIQDPCOXAgWpa1Cf/DrJxhZ0DANBgkqhkiG9w0BAQUFADBI'+#13#10+
  'MQswCQYDVQQGEwJVUzEgMB4GA1UEChMXU2VjdXJlVHJ1c3QgQ29ycG9yYXRpb24x'+#13#10+
  'FzAVBgNVBAMTDlNlY3VyZVRydXN0IENBMB4XDTA2MTEwNzE5MzExOFoXDTI5MTIz'+#13#10+
  'MTE5NDA1NVowSDELMAkGA1UEBhMCVVMxIDAeBgNVBAoTF1NlY3VyZVRydXN0IENv'+#13#10+
  'cnBvcmF0aW9uMRcwFQYDVQQDEw5TZWN1cmVUcnVzdCBDQTCCASIwDQYJKoZIhvcN'+#13#10+
  'AQEBBQADggEPADCCAQoCggEBAKukgeWVzfX2FI7CT8rU4niVWJxB4Q2ZQCQXOZEz'+#13#10+
  'Zum+4YOvYlyJ0fwkW2Gz4BERQRwdbvC4u/jep4G6pkjGnx29vo6pQT64lO0pGtSO'+#13#10+
  '0gMdA+9tDWccV9cGrcrI9f4Or2YlSASWC12juhbDCE/RRvgUXPLIXgGZbf2IzIao'+#13#10+
  'wW8xQmxSPmjL8xk037uHGFaAJsTQ3MBv396gwpEWoGQRS0S8Hvbn+mPeZqx2pHGj'+#13#10+
  '7DaUaHp3pLHnDi+BeuK1cobvomuL8A/b01k/unK8RCSc43Oz969XL0Imnal0ugBS'+#13#10+
  '8kvNU3xHCzaFDmapCJcWNFfBZveA4+1wVMeT4C4oFVmHursCAwEAAaOBnTCBmjAT'+#13#10+
  'BgkrBgEEAYI3FAIEBh4EAEMAQTALBgNVHQ8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB'+#13#10+
  '/zAdBgNVHQ4EFgQUQjK2FvoE/f5dS3rD/fdMQB1aQ68wNAYDVR0fBC0wKzApoCeg'+#13#10+
  'JYYjaHR0cDovL2NybC5zZWN1cmV0cnVzdC5jb20vU1RDQS5jcmwwEAYJKwYBBAGC'+#13#10+
  'NxUBBAMCAQAwDQYJKoZIhvcNAQEFBQADggEBADDtT0rhWDpSclu1pqNlGKa7UTt3'+#13#10+
  '6Z3q059c4EVlew3KW+JwULKUBRSuSceNQQcSc5R+DCMh/bwQf2AQWnL1mA6s7Ll/'+#13#10+
  '3XpvXdMc9P+IBWlCqQVxyLesJugutIxq/3HcuLHfmbx8IVQr5Fiiu1cprp6poxkm'+#13#10+
  'D5kuCLDv/WnPmRoJjeOnnyvJNjR7JLN4TJUXpAYmHrZkUjZfYGfZnMUFdAvnZyPS'+#13#10+
  'CPyI6a6Lf+Ew9Dd+/cYy2i2eRDAwbO4H3tI0/NL/QPZL9GZGBlSm8jIKYyYwa5vR'+#13#10+
  '3ItHuuG51WLQoqD0ZwV4KWMabwTW+MZMo5qxN7SN5ShLHZ4swrhovO0C7jE='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Starfield Root Certificate Authority - G2, (O): Starfield Technologies, Inc.
Issuer: Self Signed
Expires: 2037-12-31T23:59:59, Signature: sha256WithRSAEncryption
Valid From: 2009-09-01T00:00:00, Serial Number: 00
Fingerprint (sha256): 2ce1cb0bf9d2f9e102993fbe215152c3b2dd0cabde1c68e5319b839154dbb7f5
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts048 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIID3TCCAsWgAwIBAgIBADANBgkqhkiG9w0BAQsFADCBjzELMAkGA1UEBhMCVVMx'+#13#10+
  'EDAOBgNVBAgTB0FyaXpvbmExEzARBgNVBAcTClNjb3R0c2RhbGUxJTAjBgNVBAoT'+#13#10+
  'HFN0YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAMTKVN0YXJmaWVs'+#13#10+
  'ZCBSb290IENlcnRpZmljYXRlIEF1dGhvcml0eSAtIEcyMB4XDTA5MDkwMTAwMDAw'+#13#10+
  'MFoXDTM3MTIzMTIzNTk1OVowgY8xCzAJBgNVBAYTAlVTMRAwDgYDVQQIEwdBcml6'+#13#10+
  'b25hMRMwEQYDVQQHEwpTY290dHNkYWxlMSUwIwYDVQQKExxTdGFyZmllbGQgVGVj'+#13#10+
  'aG5vbG9naWVzLCBJbmMuMTIwMAYDVQQDEylTdGFyZmllbGQgUm9vdCBDZXJ0aWZp'+#13#10+
  'Y2F0ZSBBdXRob3JpdHkgLSBHMjCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC'+#13#10+
  'ggEBAL3twQP89o/8ArFvW59I2Z154qK3A2FWGMNHttfKPTUuiUP3oWmb3ooa/RMg'+#13#10+
  'nLRJdzIpVv257IzdIvpy3Cdhl+72WoTsbhm5iSzchFvVdPtrX8WJpRBSiUZV9Lh1'+#13#10+
  'HOZ/5FSuS/hVclcCGfgXcVnrHigHdMWdSL5stPSksPNkN3mSwOxGXn/hbVNMYq/N'+#13#10+
  'Hwtjuzqd+/x5AJhhdM8mgkBj87JyahkNmcrUDnXMN/uLicFZ8WJ/X7NfZTD4p7dN'+#13#10+
  'dloedl40wOiWVpmKs/B/pM293DIxfJHP4F8R+GuqSVzRmZTRouNjWwl2tVZi4Ut0'+#13#10+
  'HZbUJtQIBFnQmA4O5t78w+wfkPECAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAO'+#13#10+
  'BgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYEFHwMMh+n2TB/xH1oo2Kooc6rB1snMA0G'+#13#10+
  'CSqGSIb3DQEBCwUAA4IBAQARWfolTwNvlJk7mh+ChTnUdgWUXuEok21iXQnCoKjU'+#13#10+
  'sHU48TRqneSfioYmUeYs0cYtbpUgSpIB7LiKZ3sx4mcujJUDJi5DnUox9g61DLu3'+#13#10+
  '4jd/IroAow57UvtruzvE03lRTs2Q9GcHGcg8RnoNAX3FWOdt5oUwF5okxBDgBPfg'+#13#10+
  '8n/Uqgr/Qh037ZTlZFkSIHc40zI+OIF1lnP6aI+xy84fxez6nH7PfrHxBy22/L/K'+#13#10+
  'pL/QlwVKvOoYKAKQvVR4CSFx09F9HdkWsKlhPdAKACL8x3vLCWRFCztAgfd9fDL1'+#13#10+
  'mMpYjn0q7pBZc2T5NnReJaH1ZgUufzkVqSr7UIuOhWn0'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Starfield Services Root Certificate Authority - G2, (O): Starfield Technologies, Inc.
Issuer: Self Signed
Expires: 2037-12-31T23:59:59, Signature: sha256WithRSAEncryption
Valid From: 2009-09-01T00:00:00, Serial Number: 00
Fingerprint (sha256): 568d6905a2c88708a4b3025190edcfedb1974a606a13c6e5290fcb2ae63edab5
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts049 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIID7zCCAtegAwIBAgIBADANBgkqhkiG9w0BAQsFADCBmDELMAkGA1UEBhMCVVMx'+#13#10+
  'EDAOBgNVBAgTB0FyaXpvbmExEzARBgNVBAcTClNjb3R0c2RhbGUxJTAjBgNVBAoT'+#13#10+
  'HFN0YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xOzA5BgNVBAMTMlN0YXJmaWVs'+#13#10+
  'ZCBTZXJ2aWNlcyBSb290IENlcnRpZmljYXRlIEF1dGhvcml0eSAtIEcyMB4XDTA5'+#13#10+
  'MDkwMTAwMDAwMFoXDTM3MTIzMTIzNTk1OVowgZgxCzAJBgNVBAYTAlVTMRAwDgYD'+#13#10+
  'VQQIEwdBcml6b25hMRMwEQYDVQQHEwpTY290dHNkYWxlMSUwIwYDVQQKExxTdGFy'+#13#10+
  'ZmllbGQgVGVjaG5vbG9naWVzLCBJbmMuMTswOQYDVQQDEzJTdGFyZmllbGQgU2Vy'+#13#10+
  'dmljZXMgUm9vdCBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkgLSBHMjCCASIwDQYJKoZI'+#13#10+
  'hvcNAQEBBQADggEPADCCAQoCggEBANUMOsQq+U7i9b4Zl1+OiFOxHz/Lz58gE20p'+#13#10+
  'OsgPfTz3a3Y4Y9k2YKibXlwAgLIvWX/2h/klQ4bnaRtSmpDhcePYLQ1Ob/bISdm2'+#13#10+
  '8xpWriu2dBTrz/sm4xq6HZYuajtYlIlHVv8loJNwU4PahHQUw2eeBGg6345AWh1K'+#13#10+
  'Ts9DkTvnVtYAcMtS7nt9rjrnvDH5RfbCYM8TWQIrgMw0R9+53pBlbQLPLJGmpufe'+#13#10+
  'hRhJfGZOozptqbXuNC66DQO4M99H67FrjSXZm86B0UVGMpZwh94CDklDhbZsc7tk'+#13#10+
  '6mFBrMnUVN+HL8cisibMn1lUaJ/8viovxFUcdUBgF4UCVTmLfwUCAwEAAaNCMEAw'+#13#10+
  'DwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYEFJxfAN+q'+#13#10+
  'AdcwKziIorhtSpzyEZGDMA0GCSqGSIb3DQEBCwUAA4IBAQBLNqaEd2ndOxmfZyMI'+#13#10+
  'bw5hyf2E3F/YNoHN2BtBLZ9g3ccaaNnRbobhiCPPE95Dz+I0swSdHynVv/heyNXB'+#13#10+
  've6SbzJ08pGCL72CQnqtKrcgfU28elUSwhXqvfdqlS5sdJ/PHLTyxQGjhdByPq1z'+#13#10+
  'qwubdQxtRbeOlKyWN7Wg0I8VRw7j6IPdj/3vQQF3zCepYoUz8jcI73HPdwbeyBkd'+#13#10+
  'iEDPfUYd/x7H4c7/I9vG+o1VTqkC50cRRj70/b17KSa7qWFiNyi2LSr2EIZkyXCn'+#13#10+
  '0q23KXB56jzaYyWf/Wi3MOxw+3WKt21gZ7IeyLnp2KhvAotnDU0mV3HaIPzBSlCN'+#13#10+
  'sSi6'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): (Blank), (O): Starfield Technologies, Inc., (OU): Starfield Class 2 Certification Authority
Issuer: Self Signed
Expires: 2034-06-29T17:39:16, Signature: sha1WithRSAEncryption
Valid From: 2004-06-29T17:39:16, Serial Number: 00
Fingerprint (sha256): 1465fa205397b876faa6f0a9958e5590e40fcc7faa4fb7c2c8677521fb5fb658
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts050 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEDzCCAvegAwIBAgIBADANBgkqhkiG9w0BAQUFADBoMQswCQYDVQQGEwJVUzEl'+#13#10+
  'MCMGA1UEChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMp'+#13#10+
  'U3RhcmZpZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDQw'+#13#10+
  'NjI5MTczOTE2WhcNMzQwNjI5MTczOTE2WjBoMQswCQYDVQQGEwJVUzElMCMGA1UE'+#13#10+
  'ChMcU3RhcmZpZWxkIFRlY2hub2xvZ2llcywgSW5jLjEyMDAGA1UECxMpU3RhcmZp'+#13#10+
  'ZWxkIENsYXNzIDIgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwggEgMA0GCSqGSIb3'+#13#10+
  'DQEBAQUAA4IBDQAwggEIAoIBAQC3Msj+6XGmBIWtDBFk385N78gDGIc/oav7PKaf'+#13#10+
  '8MOh2tTYbitTkPskpD6E8J7oX+zlJ0T1KKY/e97gKvDIr1MvnsoFAZMej2YcOadN'+#13#10+
  '+lq2cwQlZut3f+dZxkqZJRRU6ybH838Z1TBwj6+wRir/resp7defqgSHo9T5iaU0'+#13#10+
  'X9tDkYI22WY8sbi5gv2cOj4QyDvvBmVmepsZGD3/cVE8MC5fvj13c7JdBmzDI1aa'+#13#10+
  'K4UmkhynArPkPw2vCHmCuDY96pzTNbO8acr1zJ3o/WSNF4Azbl5KXZnJHoe0nRrA'+#13#10+
  '1W4TNSNe35tfPe/W93bC6j67eA0cQmdrBNj41tpvi/JEoAGrAgEDo4HFMIHCMB0G'+#13#10+
  'A1UdDgQWBBS/X7fRzt0fhvRbVazc1xDCDqmI5zCBkgYDVR0jBIGKMIGHgBS/X7fR'+#13#10+
  'zt0fhvRbVazc1xDCDqmI56FspGowaDELMAkGA1UEBhMCVVMxJTAjBgNVBAoTHFN0'+#13#10+
  'YXJmaWVsZCBUZWNobm9sb2dpZXMsIEluYy4xMjAwBgNVBAsTKVN0YXJmaWVsZCBD'+#13#10+
  'bGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8w'+#13#10+
  'DQYJKoZIhvcNAQEFBQADggEBAAWdP4id0ckaVaGsafPzWdqbAYcaT1epoXkJKtv3'+#13#10+
  'L7IezMdeatiDh6GX70k1PncGQVhiv45YuApnP+yz3SFmH8lU+nLMPUxA2IGvd56D'+#13#10+
  'eruix/U0F47ZEUD0/CwqTRV/p2JdLiXTAAsgGh1o+Re49L2L7ShZ3U0WixeDyLJl'+#13#10+
  'xy16paq8U4Zt3VekyvggQQto8PT7dL5WXXp59fkdheMtlb71cZBDzI0fmgAKhynp'+#13#10+
  'VSJYACPq4xJDKVtHCN2MQWplBqjlIapBtJUhlbl90TSrE9atvNziPTnNvT51cKEY'+#13#10+
  'WQPJIrSPnNVeKtelttQKbfi3QBFGmh95DmK/D5fs4C8fF5Q='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): SwissSign Gold CA - G2, (O): SwissSign AG
Issuer: Self Signed
Expires: 2036-10-25T08:30:35, Signature: sha1WithRSAEncryption
Valid From: 2006-10-25T08:30:35, Serial Number: bb401c43f55e4fb0
Fingerprint (sha256): 62dd0be9b9f50a163ea0f8e75c053b1eca57ea55c8688f647c6881f2c8357b95
Public Key: RSA Key Encryption 4096 bits, 128 security bits }

sslRootCACerts051 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIFujCCA6KgAwIBAgIJALtAHEP1Xk+wMA0GCSqGSIb3DQEBBQUAMEUxCzAJBgNV'+#13#10+
  'BAYTAkNIMRUwEwYDVQQKEwxTd2lzc1NpZ24gQUcxHzAdBgNVBAMTFlN3aXNzU2ln'+#13#10+
  'biBHb2xkIENBIC0gRzIwHhcNMDYxMDI1MDgzMDM1WhcNMzYxMDI1MDgzMDM1WjBF'+#13#10+
  'MQswCQYDVQQGEwJDSDEVMBMGA1UEChMMU3dpc3NTaWduIEFHMR8wHQYDVQQDExZT'+#13#10+
  'd2lzc1NpZ24gR29sZCBDQSAtIEcyMIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIIC'+#13#10+
  'CgKCAgEAr+TufoskDhJuqVAtFkQ7kpJcyrhdhJJCEyq8ZVeCQD5XJM1QiyUqt2/8'+#13#10+
  '76LQwB8CJEoTlo8jE+YoWACjR8cGp4QjK7u9lit/VcyLwVcfDmJlD909Vopz2q5+'+#13#10+
  'bbqBHH5CjCA12UNNhPqE21Is8w4ndwtrvxEvcnifLtg+5hg3Wipy+dpikJKVyh+c'+#13#10+
  '6bM8K8vzARO/Ws/BtQpgvd21mWRTuKCWs2/iJneRjOBiEAKfNA+k1ZIzUd6+jbqE'+#13#10+
  'emA8atufK+ze3gE/bk3lUIbLtK/tREDFylqM2tIrfKjuvqblCqoOpd8FUrdVxyJd'+#13#10+
  'MmqXl2MT28nbeTZ7hTpKxVKJ+STnnXepgv9VHKVxaSvRAiTysybUa9oEVeXBCsdt'+#13#10+
  'MDeQKuSeFDNeFhdVxVu1yzSJkvGdJo+hB9TGsnhQ2wwMC3wLjEHXuendjIj3o02y'+#13#10+
  'MszYF9rNt85mndT9Xv+9lz4pded+p2JYryU0pUHHPbwNUMoDAw8IWh+Vc3hiv69y'+#13#10+
  'FGkOpeUDDniOJihC8AcLYiAQZzlG+qkDzAQ4embvIIO1jEpWjpEA/I5cgt6IoMPi'+#13#10+
  'aG59je883WX0XaxR7ySArqpWl2/5rX3aYT+YdzylkbYcjCbaZaIJbcHiVOO5ykxM'+#13#10+
  'gI93e2CaHt+28kgeDrpOVG2Y4OGiGqJ3UM/EY5LsRxmd6+ZrzsECAwEAAaOBrDCB'+#13#10+
  'qTAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUWyV7'+#13#10+
  'lqRlUX64OfPAeGZe6Drn8O4wHwYDVR0jBBgwFoAUWyV7lqRlUX64OfPAeGZe6Drn'+#13#10+
  '8O4wRgYDVR0gBD8wPTA7BglghXQBWQECAQEwLjAsBggrBgEFBQcCARYgaHR0cDov'+#13#10+
  'L3JlcG9zaXRvcnkuc3dpc3NzaWduLmNvbS8wDQYJKoZIhvcNAQEFBQADggIBACe6'+#13#10+
  '45R88a7A3hfm5djV9VSwg/S7zV4Fe0+fdWavPOhWfvxyeDgD2StiGwC5+OlgzczO'+#13#10+
  'UYrHUDFu4Up+GC9pWbY9ZIEr44OE5iKHjn3g7gKZYbge9LgriBIWhMIxkziWMaa5'+#13#10+
  'O1M/wySTVltpkuzFwbs4AOPsF6m43Md8AYOfMke6UiI0HTJ6CVanfCU2qT1L2sCC'+#13#10+
  'bwq7EsiHSycR+R4tx5M/nttfJmtS2S6K8RTGRI0Vqbe/vd6mGu6uLftIdxf+u+yv'+#13#10+
  'GPUqUfA5hJeVbG4bwyvEdGB5JbAKJ9/fXtI5z0V9QkvfsywexcZdylU6oJxpmo/a'+#13#10+
  '77KwPJ+HbBIrZXAVUjEaJM9vMSNQH4xPjyPDdEFjHFWoFN0+4FFQz/EbMFYOkrCC'+#13#10+
  'hdiDyyJkvC24JdVUorgG6q2SpCSgwYa1ShNqR88uC1aVVMvOmttqtKay20EIhid3'+#13#10+
  '92qgQmwLOM7XdVAyksLfKzAiSNDVQTglXaTpXZ/GlHXQRf0wl0OPkKsKx4ZzYEpp'+#13#10+
  'Ld6leNcG2mqeSz53OiATIgHQv2ieY2BrNU0LbbqhPcCT4H8js1WtciVORvnSFu+w'+#13#10+
  'ZMEBnunKoGqYDs/YYPIvSbjkQuE4NRb0yG5P94FW6LqjviOvrv1vA+ACOzB2+htt'+#13#10+
  'Qc8Bsem4yWb02ybzOqR08kkkW8mw0FfB+j564ZfJ'+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): (Blank), (O): The Go Daddy Group, Inc., (OU): Go Daddy Class 2 Certification Authority
Issuer: Self Signed
Expires: 2034-06-29T17:06:20, Signature: sha1WithRSAEncryption
Valid From: 2004-06-29T17:06:20, Serial Number: 00
Fingerprint (sha256): c3846bf24b9e93ca64274c0ec67c1ecc5e024ffcacd2d74019350e81fe546ae4
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts052 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIIEADCCAuigAwIBAgIBADANBgkqhkiG9w0BAQUFADBjMQswCQYDVQQGEwJVUzEh'+#13#10+
  'MB8GA1UEChMYVGhlIEdvIERhZGR5IEdyb3VwLCBJbmMuMTEwLwYDVQQLEyhHbyBE'+#13#10+
  'YWRkeSBDbGFzcyAyIENlcnRpZmljYXRpb24gQXV0aG9yaXR5MB4XDTA0MDYyOTE3'+#13#10+
  'MDYyMFoXDTM0MDYyOTE3MDYyMFowYzELMAkGA1UEBhMCVVMxITAfBgNVBAoTGFRo'+#13#10+
  'ZSBHbyBEYWRkeSBHcm91cCwgSW5jLjExMC8GA1UECxMoR28gRGFkZHkgQ2xhc3Mg'+#13#10+
  'MiBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTCCASAwDQYJKoZIhvcNAQEBBQADggEN'+#13#10+
  'ADCCAQgCggEBAN6d1+pXGEmhW+vXX0iG6r7d/+TvZxz0ZWizV3GgXne77ZtJ6XCA'+#13#10+
  'PVYYYwhv2vLM0D9/AlQiVBDYsoHUwHU9S3/Hd8M+eKsaA7Ugay9qK7HFiH7Eux6w'+#13#10+
  'wdhFJ2+qN1j3hybX2C32qRe3H3I2TqYXP2WYktsqbl2i/ojgC95/5Y0V4evLOtXi'+#13#10+
  'EqITLdiOr18SPaAIBQi2XKVlOARFmR6jYGB0xUGlcmIbYsUfb18aQr4CUWWoriMY'+#13#10+
  'avx4A6lNf4DD+qta/KFApMoZFv6yyO9ecw3ud72a9nmYvLEHZ6IVDd2gWMZEewo+'+#13#10+
  'YihfukEHU1jPEX44dMX4/7VpkI+EdOqXG68CAQOjgcAwgb0wHQYDVR0OBBYEFNLE'+#13#10+
  'sNKR1EwRcbNhyz2h/t2oatTjMIGNBgNVHSMEgYUwgYKAFNLEsNKR1EwRcbNhyz2h'+#13#10+
  '/t2oatTjoWekZTBjMQswCQYDVQQGEwJVUzEhMB8GA1UEChMYVGhlIEdvIERhZGR5'+#13#10+
  'IEdyb3VwLCBJbmMuMTEwLwYDVQQLEyhHbyBEYWRkeSBDbGFzcyAyIENlcnRpZmlj'+#13#10+
  'YXRpb24gQXV0aG9yaXR5ggEAMAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQEFBQAD'+#13#10+
  'ggEBADJL87LKPpH8EsahB4yOd6AzBhRckB4Y9wimPQoZ+YeAEW5p5JYXMP80kWNy'+#13#10+
  'OO7MHAGjHZQopDH2esRU1/blMVgDoszOYtuURXO1v0XJJLXVggKtI3lpjbi2Tc7P'+#13#10+
  'TMozI+gciKqdi0FuFskg5YmezTvacPd+mSYgFFQlq25zheabIZ0KbIIOqPjCDPoQ'+#13#10+
  'HmyW74cNxA9hi63ugyuV+I6ShHI56yDqg+2DzZduCLzrTia2cyvk0/ZM/iZx4mER'+#13#10+
  'dEr/VxqHD3VILs9RaRegAhJhldXRQLIQTO7ErBBDpqWeCtWVYpoNz4iCxTIM5Cuf'+#13#10+
  'ReYNnyicsbkqWletNw+vHX/bvZ8='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;

{ 
Issued to (CN): Visa Information Delivery Root CA, (O): VISA, (OU): Visa International Service Association
Issuer: Self Signed
Expires: 2025-06-29T17:42:42, Signature: sha1WithRSAEncryption
Valid From: 2005-06-27T17:42:42, Serial Number: 5b57d7a84cb0afd9d36f4ba031b4d6e2
Fingerprint (sha256): c57a3acbe8c06ba1988a83485bf326f2448775379849de01ca43571af357e74b
Public Key: RSA Key Encryption 2048 bits, 112 security bits }

sslRootCACerts053 =
  '-----BEGIN CERTIFICATE-----'+#13#10+
  'MIID+TCCAuGgAwIBAgIQW1fXqEywr9nTb0ugMbTW4jANBgkqhkiG9w0BAQUFADB5'+#13#10+
  'MQswCQYDVQQGEwJVUzENMAsGA1UEChMEVklTQTEvMC0GA1UECxMmVmlzYSBJbnRl'+#13#10+
  'cm5hdGlvbmFsIFNlcnZpY2UgQXNzb2NpYXRpb24xKjAoBgNVBAMTIVZpc2EgSW5m'+#13#10+
  'b3JtYXRpb24gRGVsaXZlcnkgUm9vdCBDQTAeFw0wNTA2MjcxNzQyNDJaFw0yNTA2'+#13#10+
  'MjkxNzQyNDJaMHkxCzAJBgNVBAYTAlVTMQ0wCwYDVQQKEwRWSVNBMS8wLQYDVQQL'+#13#10+
  'EyZWaXNhIEludGVybmF0aW9uYWwgU2VydmljZSBBc3NvY2lhdGlvbjEqMCgGA1UE'+#13#10+
  'AxMhVmlzYSBJbmZvcm1hdGlvbiBEZWxpdmVyeSBSb290IENBMIIBIjANBgkqhkiG'+#13#10+
  '9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyREA4R/QkkfpLx0cYjga/EhIPZpchH0MZsRZ'+#13#10+
  'FfP6C2ITtf/Wc+MtgD4yTK0yoiXvni3d+aCtEgK3GDvkdgYrgF76ROJFZwUQjQ9l'+#13#10+
  'x42gRT05DbXvWFoy7dTglCZ9z/Tt2Cnktv9oxKgmkeHY/CyfpCBg1S8xth2JlGMR'+#13#10+
  '0ug/GMO5zANuegZOv438p5Lt5So+du2Gl+RMFQqEPwqN5uJSqAe0VtmB4gWdQ8on'+#13#10+
  'Bj2ZAM2R73QW7UW0Igt2vA4JaSiNtaAG/Y/58VXWHGgbq7rDtNK1R30X0kJV0rGA'+#13#10+
  'ib3RSwB3LpG7bOjbIucV5mQgJoVjoA1e05w6g1x/KmNTmOGRVwIDAQABo30wezAP'+#13#10+
  'BgNVHRMBAf8EBTADAQH/MDkGA1UdIAQyMDAwLgYFZ4EDAgEwJTAVBggrBgEFBQcC'+#13#10+
  'ARYJMS4yLjMuNC41MAwGCCsGAQUFBwICMAAwDgYDVR0PAQH/BAQDAgEGMB0GA1Ud'+#13#10+
  'DgQWBBRPitp2/2d3I5qmgH1924h1hfeBejANBgkqhkiG9w0BAQUFAAOCAQEACUW1'+#13#10+
  'QdUHdDJydgDPmYt+telnG/Su+DPaf1cregzlN43bJaJosMP7NwjoJY/H2He4XLWb'+#13#10+
  '5rXEkl+xH1UyUwF7mtaUoxbGxEvt8hPZSTB4da2mzXgwKvXuHyzF5Qjy1hOB0/pS'+#13#10+
  'WaF9ARpVKJJ7TOJQdGKBsF2Ty4fSCLqZLgfxbqwMsd9sysXI3rDXjIhekqvbgeLz'+#13#10+
  'PqZr+pfgFhwCCLSMQWl5Ll3u7Qk9wR094DZ6jj6+JCVCRUS3HyabH4OlM0Vc2K+j'+#13#10+
  'INsF/64Or7GNtRf9HYEJvrPxHINxl3JVwhYj4ASeaO4KwhVbwtw94Tc/XrGcexDo'+#13#10+
  'c5lC3rAi4/UZqweYCw=='+#13#10+
  '-----END CERTIFICATE-----'+#13#10;


{$ENDIF}  { USE_SSL }

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function sslRootCACertsBundle: string ;  { V8.32 }
begin
    result :=
        sslRootCACerts001 + sslRootCACerts002 + sslRootCACerts003 + sslRootCACerts004 + sslRootCACerts005 +
        sslRootCACerts006 + sslRootCACerts007 + sslRootCACerts008 + sslRootCACerts009 + sslRootCACerts010 +
        sslRootCACerts011 + sslRootCACerts012 + sslRootCACerts013 + sslRootCACerts014 + sslRootCACerts015 +
        sslRootCACerts016 + sslRootCACerts017 + sslRootCACerts018 + sslRootCACerts019 + sslRootCACerts020 +
        sslRootCACerts021 + sslRootCACerts022 + sslRootCACerts023 + sslRootCACerts024 + sslRootCACerts025 +
        sslRootCACerts026 + sslRootCACerts027 + sslRootCACerts028 + sslRootCACerts029 + sslRootCACerts030 +
        sslRootCACerts031 + sslRootCACerts032 + sslRootCACerts033 + sslRootCACerts034 + sslRootCACerts035 +
        sslRootCACerts036 + sslRootCACerts037 + sslRootCACerts038 + sslRootCACerts039 + sslRootCACerts040 +   { V8.68 lots more }
        sslRootCACerts041 + sslRootCACerts042 + sslRootCACerts043 + sslRootCACerts044 + sslRootCACerts045 +
        sslRootCACerts046 + sslRootCACerts047 + sslRootCACerts048 + sslRootCACerts049 + sslRootCACerts050 +
        sslRootCACerts051 + sslRootCACerts052 + sslRootCACerts053 ;                                           { V8.69 some gone }
    end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RaiseLastOpenSslError(
    EClass          : ExceptClass;
    Dump            : Boolean = FALSE;
    const CustomMsg : AnsiString  = '');
const
    CRLF = AnsiString(#13#10);
begin
    if Length(CustomMsg) > 0 then
        raise EClass.Create(String(CRLF + CustomMsg + CRLF +
                            LastOpenSslErrMsg(Dump) + CRLF))
    else
        raise EClass.Create(String(CRLF + LastOpenSslErrMsg(Dump) + CRLF));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AddNameEntryByTxt(Name: PX509_NAME; const Field: AnsiString; const Value: String);
var
    AStr : AnsiString;
    SType: Cardinal;
begin
    if IsUsAscii(Value) then begin
        AStr  := AnsiString(Value);
        SType := MBSTRING_ASC;
    end
    else begin
        AStr  := StringToUtf8(Value);
        { If we used MBSTRING_UTF8 the string would be converted to Ansi }
        { with current code page by OpenSSL silently, strange.           }
        SType := V_ASN1_UTF8STRING;
    end;
    if Length(AStr) = 0 then exit;
    if X509_NAME_add_entry_by_txt(Name, PAnsiChar(Field),
                                  SType, PAnsiChar(AStr), -1, -1, 0) = 0 then
         RaiseLastOpenSslError(ECertToolsException, FALSE, 'Can not add name entry ' + Field);
end;


{ V8.40 component to create SSL certificates and keys }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
Constructor TSslCertTools.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FPrivKeyType := PrivKeyRsa2048;
    FPrivKeyCipher := PrivKeyEncNone;
    FCertDigest := Digest_sha256;
    FBasicPathLen := 0;
    FNewCert := Nil;
    FNewReq := Nil;
    FRsaKey := Nil;
    FPrivKey := Nil;
    FECkey := Nil;
    FECgroup := Nil;
    FX509Req := nil;
    FX509CA := Nil;
    FPrivKeyCA := Nil;
    FAltDNSList  := TStringList.Create;
    FAltIpList := TStringList.Create;
    FAltEmailList := TStringList.Create;
    SetLength (FAltAnsiStr, 0);
    SetLength (FAltGenStr, 0);
    SetLength (FAltIa5Str, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslCertTools.Destroy;
begin
    FAltDNSList.Free;      { V8.69 even if no SSL }
    FAltIpList.Free;
    FAltEmailList.Free;
    if FSslInitialized then begin
     // must free public pointers before those they point to
        ClearAll;   { V8.65 }
        if Assigned(FNewCert) then
            X509_free(FNewCert);
        if Assigned(FNewReq) then
            X509_REQ_free(FNewReq);
        if Assigned(FPrivKey) then
            EVP_PKEY_free(FPrivKey);
         FreeAndNilX509Req;
         FreeAndNilX509CA;
         FreeAndNilPrivKeyCA;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.FreeAndNilX509Req;
begin
    if Assigned(FX509Req) then begin
        X509_REQ_free(FX509Req);
        FX509Req := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.FreeAndNilX509CA;
begin
    if NOT FSslInitialized then Exit;
    if Assigned(FX509CA) then begin
        X509_free(FX509CA);
        FX509CA := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.FreeAndNilPrivKeyCA;
begin
    if NOT FSslInitialized then Exit;
    if Assigned(FPrivKeyCA) then begin
        EVP_PKEY_free(FPrivKeyCA);
        FPrivKeyCA := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.SetX509CA(Cert: Pointer);
begin
    InitializeSsl;
    FreeAndNilX509CA;
    if Assigned(Cert) then begin
        FX509CA := X509_dup(Cert);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetIsReqLoaded: Boolean;
begin
    Result := Assigned(FX509Req);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetIsCALoaded: Boolean;
begin
    Result := Assigned(FX509CA);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.SetPrivKeyCA(Pkey: PEVP_PKEY);
begin
    InitializeSsl;
    FreeAndNilPrivKeyCA;
    if Assigned(PKey) then begin
        FPrivKeyCA := Ics_EVP_PKEY_dup(PKey);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.SetX509Req(X509Req: Pointer);
begin
    InitializeSsl;
    FreeAndNilX509Req;
    if Assigned(X509Req) then begin
        FX509Req := X509_REQ_dup(X509Req);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns formatted text with raw certificate request fields }
function TSslCertTools.GetRequestRawText: String;
var
    Bio  : PBIO;
    Len  : Integer;
    AStr : AnsiString;
begin
    Result := '';
    if FX509Req = nil then Exit;
    Bio := BIO_new(BIO_s_mem);
    if Assigned(Bio) then
    try
        X509_REQ_print(Bio, FX509Req);
        Len := BIO_ctrl(Bio, BIO_CTRL_PENDING_, 0, nil);            { V8.66 literal changed }
        SetLength(AStr, Len);
        if Len > 0 then begin
            Bio_read(Bio, PAnsiChar(AStr), Len);
            SetLength(AStr, StrLen(PAnsiChar(AStr)));
            Result := String(AStr);
        end;
    finally
        bio_free(Bio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ PKCS#10 certificate request, typical file extension .PEM, .DER }
procedure TSslCertTools.LoadReqFromFile(const FileName: String);
var
    FileBio : PBIO;
    Req : PX509_REQ;
begin
    InitializeSsl;
    FileBio := IcsSslOpenFileBio(FileName, bomReadOnly);
    try
        Req := PEM_read_bio_X509_REQ(FileBio, Nil, Nil, Nil);  { base64 version }
        if NOT Assigned (Req) then begin
            BIO_ctrl(FileBio, BIO_CTRL_RESET, 0, nil);
            Req := d2i_X509_REQ_bio(FileBio, Nil);            { DER binary version }
        end;
        if NOT Assigned (Req) then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                                  'Error reading certificate request from file');
        SetX509Req(Req);
        X509_REQ_free(Req);
    finally
        bio_free(FileBio);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ write PKCS10 certificate request to BIO }
procedure TSslCertTools.WriteReqToBio(ABio: PBIO; AddInfoText: Boolean = FALSE; const FName: String = '');
var
    comments: AnsiString;
begin
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if not Assigned(FX509Req) then
        raise EX509Exception.Create('FX509Req not assigned');
    if AddInfoText then begin
        comments := StringToUtf8(GetReqCertInfo) + #13#10;   { V8.65 UTF8 not ANSI }
        if BIO_write(ABio, @comments [1], Length (comments)) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing raw text to ' + FName);
    end;
    if PEM_write_bio_X509_REQ(ABio, PX509_REQ(FX509Req)) = 0 then
        RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing request to BIO' + FName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ save PKCS10 certificate request }
procedure TSslCertTools.SaveReqToFile(const FileName: String; AddInfoText: Boolean = FALSE);
var
    FileBio : PBIO;
begin
    if not Assigned(FX509Req) then
        raise EX509Exception.Create('Certificate request not assigned');
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);
    try
        WriteReqToBio(FileBIO, AddInfoText);
    finally
        bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns base64 encoded DER PEM certificate request }
function TSslCertTools.SaveReqToText(AddInfoText: Boolean = FALSE): String;
var
    Bio  : PBIO;
    Len  : Integer;
    AStr : AnsiString;
begin
    Result := '';
    if FX509Req = nil then Exit;
    Bio := BIO_new(BIO_s_mem);
    if Assigned(Bio) then
    try
        WriteReqToBio(Bio, AddInfoText);
        Len := BIO_ctrl(Bio, BIO_CTRL_PENDING_, 0, nil);             { V8.66 literal changed }
        SetLength(AStr, Len);
        if Len > 0 then begin
            Bio_read(Bio, PAnsiChar(AStr), Len);
            SetLength(AStr, StrLen(PAnsiChar(AStr)));
            Result := String(AStr);
        end;
    finally
        bio_free(Bio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns binary DER certificate request }
function TSslCertTools.SaveReqToDERText: AnsiString;      { V8.52, V8.64 was SaveToDERText }
var
    Bio  : PBIO;
    Len: Integer;
begin
    Result := '';
    if FX509Req = nil then Exit;
    Bio := BIO_new(BIO_s_mem);
    if Assigned(Bio) then
    try
        if i2d_X509_REQ_bio(Bio, PX509_REQ(FX509Req)) = 0 then
           RaiseLastOpenSslError(EX509Exception, TRUE, 'Error writing request to BIO');

        Len := BIO_ctrl(Bio, BIO_CTRL_PENDING_, 0, nil);          { V8.66 literal changed }
        if Len > 0 then begin
            SetLength(Result, Len);
            Bio_read(Bio, PAnsiChar(Result), Len);
        end;
    finally
        bio_free(Bio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ returns binary DER certificate }
function TSslCertTools.SaveCertToDERText: AnsiString;                  { V8.64 }
var
    Bio  : PBIO;
    Len: Integer;
begin
    Result := '';
    if not Assigned(X509) then Exit;
    Bio := BIO_new(BIO_s_mem);
    if Assigned(Bio) then
    try
        if i2d_X509_bio(Bio, X509) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                           'Error writing DER certificate to BIO');
        Len := BIO_ctrl(Bio, BIO_CTRL_PENDING_, 0, nil);         { V8.66 literal changed }
        if Len > 0 then begin
            SetLength(Result, Len);
            Bio_read(Bio, PAnsiChar(Result), Len);
        end;
    finally
        bio_free(Bio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns a CRLF-separated list if multiple entries exist }
function TSslCertTools.GetReqEntryByNid(ANid: Integer): String;
var
    Name    : PX509_NAME;
begin
    Result := '';
    if not Assigned(X509Req) then
        Exit;
    Name := X509_REQ_get_subject_name(X509Req);   { V8.66 }
    if Name <> nil then
        Result := GetPX509NameByNid(Name, ANid);  { V8.41 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSubjOneLine: String;
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(FX509Req) then
        Exit;

    SetLength(Str, 512);
    Str := X509_NAME_oneline(X509_REQ_get_subject_name(FX509Req),
                                              PAnsiChar(Str), Length(Str));  { V8.66 }
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSubjCName: String;
begin
    Result := GetReqEntryByNid(NID_commonName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSubjOName: String;
begin
    Result := GetReqEntryByNid(NID_organizationName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqExtByName(const S: String): TExtension;
var
    ExtnStack : PStack;
    I         : Integer;
    Ext       : PX509_EXTENSION;
    Count     : Integer;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    if NOT Assigned(FX509Req) then Exit;
    ExtnStack := X509_REQ_get_extensions(FX509Req);
    if NOT Assigned(ExtnStack) then Exit;
    Count := OPENSSL_sk_num(ExtnStack);
    for I := 0 to Count - 1 do begin
        Ext := PX509_EXTENSION(OPENSSL_sk_value(ExtnStack, I));
        if not Assigned(Ext) then
            Continue;
        if CheckExtName(Ext, S) then begin    { V8.41 simplify }
            Result := GetExtDetail(Ext);
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqExtValuesByName(const ShortName, FieldName: String): String;
var
    Ext    : TExtension;
begin
    Result := '';
    if not Assigned(X509Req) then
        Exit;
    Ext := GetReqExtByName(ShortName);
    if Length(Ext.ShortName) > 0 then begin
        Result := GetExtField(Ext, FieldName);   { V8.41 simplify }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSubjAltNameDNS: String;
begin
   Result := GetReqExtValuesByName('subjectAltName', 'DNS');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSubjAltNameIP: String;
begin
   Result := GetReqExtValuesByName('subjectAltName', 'IP');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqKeyUsage: String;
begin
    Result := GetReqExtValuesByName('keyUsage', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqExKeyUsage: String;
begin
    Result := GetReqExtValuesByName('extendedKeyUsage', '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqKeyInfo: string;       { V8.52 }
var
    pubkey: PEVP_PKEY;
begin
    result := '' ;
    if NOT Assigned(FX509Req) then Exit;
    pubkey := X509_REQ_get_pubkey(FX509Req);
    Result := GetKeyDesc(pubkey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqSignAlgo: String;      { V8.52 }
var
    Nid: integer ;
    Str : AnsiString;
//    MyX509Req: PX509_REQ;
begin
    Result := '';
    if NOT Assigned(FX509Req) then Exit;
    Nid := X509_REQ_get_signature_nid(FX509Req);
    if Nid <> NID_undef then begin
        SetLength(Str, 256);
        Str := OBJ_nid2ln(Nid);
        SetLength(Str, IcsStrLen(PAnsiChar(Str)));
        Result := String(Str);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.GetReqCertInfo: String;
begin
    Result := 'Request for (CN): ' + IcsUnwrapNames (ReqSubjCName);
    if ReqSubjOName <> '' then
        Result := Result + ', (O): ' + IcsUnwrapNames (ReqSubjOName) + #13#10
    else
        Result := Result + #13#10;
    if ReqSubjAltNameDNS <> '' then
        Result := Result + 'Alt Domains: ' + IcsUnwrapNames (ReqSubjAltNameDNS) + #13#10;
    if ReqSubjAltNameIP <> '' then
        Result := Result + 'Alt IPs: ' + IcsUnwrapNames (ReqSubjAltNameIP) + #13#10;
    if ReqKeyUsage <> '' then
        Result := Result + 'Key Usage: ' + IcsUnwrapNames(ReqKeyUsage) + #13#10;
    if ReqExKeyUsage <> '' then
        Result := Result + 'Extended Key Usage: ' + IcsUnwrapNames(ReqExKeyUsage) + #13#10;
    Result := Result + 'Signature: ' + ReqSignAlgo + #13#10;
    Result := Result + 'Public Key: ' + ReqKeyInfo;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.SetCertExt(Cert: PX509; Nid: integer; const List: AnsiString);
var
    Ex: PX509_EXTENSION;
begin
    if List = '' then exit;
    Ex := X509V3_EXT_conf_nid(nil, nil, Nid, PAnsiChar(List));
    if not Assigned(Ex) then
        RaiseLastOpenSslError(ECertToolsException, FALSE,
                'Failed to add extension to certificate, Nid ' + IntToStr (Nid));
    X509_add_ext(cert, Ex, -1);
    X509_EXTENSION_free(Ex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.BuildBasicCons(IsCA: Boolean): AnsiString;
begin
    if IsCA then
       Result := 'critical,CA:TRUE, pathlen:' + IcsIntToStrA(FBasicPathLen) // 0=not sign other CAs
    else
       Result := 'critical,CA:FALSE';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.BuildKeyUsage: AnsiString;
begin
    Result := '';
    if FKeyCertSign then Result := Result + ', keyCertSign';
    if FKeyCRLSign  then Result := Result + ', cRLSign';
    if FKeyDigiSign then Result := Result + ', digitalSignature';
    if FKeyDataEnc  then Result := Result + ', dataEncipherment';
    if FKeyKeyEnc   then Result := Result + ', keyEncipherment';
    if FKeyKeyAgree then Result := Result + ', keyAgreement';
    if FKeyNonRepud then Result := Result + ', nonRepudiation';
    { others: encipherOnly, decipherOnly }
    if Result <> '' then Result := 'critical' + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.BuildExKeyUsage: AnsiString;
begin
    Result := '';
    if FKeyExtClient then Result := Result + ', clientAuth';
    if FKeyExtServer then Result := Result + ', serverAuth';
    if FKeyExtEmail  then Result := Result + ', emailProtection';
    if FKeyExtCode   then Result := Result + ', codeSigning';
    if Result <> '' then Result := Copy (Result, 3, 999);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build stack of extension multiple values from combined list of different types }
function TSslCertTools.BuildAltStackAll: PStack;
var
    I: Integer;
    ASocketFamily: TSocketFamily;
    ConvOk: Boolean;
    v4Addr: TIcsIPv4Address;
    v6Addr: TIcsIPv6Address;
begin
    result := OPENSSL_sk_new_null;
    if FTotAltItems = 0 then Exit;
    SetLength (FAltAnsiStr, FTotAltItems);    { V8.50 separate memory to allow stack to be built }
    SetLength (FAltIa5Str, FTotAltItems);
    SetLength (FAltGenStr, FTotAltItems);
    for I := 0 to FTotAltItems - 1 do begin
        FAltGenStr[I] := GENERAL_NAME_new;
        if NOT Assigned(FAltGenStr[I]) then Exit;
        FAltIa5Str[I] := ASN1_STRING_new;
        if NOT Assigned(FAltIa5Str[I]) then Exit;
        if FAltItems[I].AltType = GEN_IPADD then begin  { V8.64 IP saved in binary }
            if NOT WSocketIsIP(String(FAltItems[I].AltName), ASocketFamily) then
                FAltAnsiStr[I] := FAltItems[I].AltName
            else begin
                if ASocketFamily = sfIPv4 then begin
                     v4Addr := WSocketStrToIPv4(String(FAltItems[I].AltName), ConvOk);
                     SetLength(FAltAnsiStr[I], 4);
                     Move(v4Addr,  FAltAnsiStr[I] [1], 4);
                end;
                if ASocketFamily = sfIPv6 then begin
                     v6Addr := WSocketStrToIPv6(String(FAltItems[I].AltName), ConvOk);
                     SetLength(FAltAnsiStr[I], 16);
                     Move(v6Addr,  FAltAnsiStr[I] [1], 16);
                end;
            end;
        end
        else
            FAltAnsiStr[I] := FAltItems[I].AltName;
        if FAltAnsiStr[I] <> '' then begin    { V8.50 skip blanks }
            if ASN1_STRING_set(FAltIa5Str[I], PAnsiChar(FAltAnsiStr[I]), Length(FAltAnsiStr[I])) = 0 then   { V8.50 was set0 }
                RaiseLastOpenSslError(ECertToolsException, FALSE, 'Invalid subject_alt_name: ' + String(FAltItems[I].AltName));
            GENERAL_NAME_set0_value(FAltGenStr[I], FAltItems[I].AltType, FAltIa5Str[I]);
            OPENSSL_sk_push(result, Pointer(FAltGenStr[I]));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ clear alt extension stack }
procedure TSslCertTools.ClearAltStack;
//var
//    I: Integer;
begin
  { V8.64 fails on Win64, so possible memory leak without this
   if Length (FAltGenStr) > 0 then begin
        for I := 0 to Length (FAltGenStr) - 1 do begin
            if Assigned(FAltGenStr[I]) then GENERAL_NAME_free(FAltGenStr[I]);
        end;
    end;
    if Length (FAltIa5Str) > 0 then begin
        for I := 0 to Length (FAltIa5Str) - 1 do begin
            if Assigned(FAltIa5Str[I]) then ASN1_STRING_free(FAltIa5Str[I]);
        end;
    end;
    }
    SetLength (FAltAnsiStr, 0);
    SetLength (FAltGenStr, 0);
    SetLength (FAltIa5Str, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ build stack for an X509 certificate }
procedure TSslCertTools.BuildCertAltSubjAll(Cert: PX509);  { V8.64 }
var
    MultiValuesStack : PStack;
begin
    if FTotAltItems = 0 then Exit;
    MultiValuesStack := BuildAltStackAll;
    if NOT Assigned(MultiValuesStack) then Exit;
    try
        if X509_add1_ext_i2d(Cert, NID_subject_alt_name, MultiValuesStack, 0, 0) = 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set subject_alt_name');
    finally
        OPENSSL_sk_free(MultiValuesStack);
        ClearAltStack;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ combine various alternate names into sincle list from properties }
procedure TSslCertTools.BuildAltPropList;  { V8.64 }
var
    I, J: Integer;
    ErrFlag, CheckFlag: Boolean;
begin
    FTotAltItems := FAltDNSList.Count + FAltIPList.Count + FAltEmailList.Count;
    SetLength(FAltItems, FTotAltItems);
    if FTotAltItems = 0 then Exit;
    J := 0;

  { subject alternate name - DNS or domain names }
    if FAltDNSList.Count <> 0 then begin
        for I := 0 to FAltDNSList.Count - 1 do begin
            CheckFlag := (Pos('*', FAltDNSList[I]) = 0);  // wild card will fail domain name check
         { V8.64 convert domain to A-Label }
            FAltItems[J].AltName := AnsiString(IcsIDNAToASCII(IcsTrim(FAltDNSList[I]), CheckFlag, ErrFlag));
            FAltItems[J].AltType := GEN_DNS;
            J := J + 1;
        end;
    end;

  { subject alternate name - IP addresses }
    if FAltIPList.Count <> 0 then begin
        for I := 0 to FAltIPList.Count - 1 do begin
            FAltItems[J].AltName := AnsiString(IcsTrim(FAltIPList[I]));
            FAltItems[J].AltType := GEN_IPADD;
            J := J + 1;
        end;
    end;

  { subject alternate name - email addresses }
    if FAltEmailList.Count <> 0 then begin
        for I := 0 to FAltEmailList.Count - 1 do begin
            FAltItems[J].AltName := AnsiString(IcsTrim(FAltEmailList[I]));
            FAltItems[J].AltType := GEN_EMAIL;
            J := J + 1;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ combine various alternate names into sincle list from properties }
procedure TSslCertTools.BuildAltReqList;  { V8.64 }
var
    I, J: Integer;
    AltItems: String;
    TempList: TStringList;
begin
    FTotAltItems := 0;
    TempList := TStringList.Create;
    try
        AltItems :=  ReqSubjAltNameDNS;
        J := 0;
        if AltItems <> '' then begin
            TempList.Text := AltItems;
            if TempList.Count > 0 then begin
                FTotAltItems := TempList.Count;
                SetLength(FAltItems, FTotAltItems);
                for I := 0 to TempList.Count - 1 do begin
                    FAltItems[J].AltName := AnsiString(IcsTrim(TempList[I]));
                    FAltItems[J].AltType := GEN_DNS;
                    J := J + 1;
                end;
            end;
        end;
        AltItems :=  ReqSubjAltNameIP;
        J := 0;
        if AltItems <> '' then begin
            TempList.Text := AltItems;
            if TempList.Count > 0 then begin
                FTotAltItems := FTotAltItems + TempList.Count;
                SetLength(FAltItems, FTotAltItems);
                for I := 0 to TempList.Count - 1 do begin
                    FAltItems[J].AltName := AnsiString(IcsTrim(TempList[I]));
                    FAltItems[J].AltType := GEN_IPADD;
                    J := J + 1;
                end;
            end;
        end;
    finally
        TempList.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A certificate request is the second step in acquiring a commercial X509 }
{ certificate (usually paid) from a certificate issuing authority. }
{ A private key needs to be created first, then all the certificate subject }
{ properties such as domain name, company name, etc, are specified and an }
{ X509 Request is created.  The request should be passed to the chosen }
{ certificate authority who will create a new X509 certificate signed by }
{ one of their issuing certificates.  For the new X509 certificate to be }
{ trusted by SSL/TLS applications, the issuing certificate needs to installed }
{ in a local root certificate store, which is normally part of the browser or
{ other software.  Often, the CA will use an intermediate certificate which
{ is itself signed by a root certificate, and this intermediate certificiate }
{ needs to be installed on the SSL/TLS server with the X509 certificate so it
{ is sent to clients.   }
{ This method needs PrivateKey completed, and creates the request }
{ in X509Req from where is may be saved by SaveReqTo.   }
procedure TSslCertTools.DoCertReqProps;

  procedure Add_Ext(sk :PSTACK_OF_X509_EXTENSION; Nid :Integer; const Value :AnsiString);
  var
      Ext : PX509_EXTENSION;
  begin
      if Value = '' then Exit;
      Ext := X509V3_EXT_conf_nid(nil, nil, NID, PAnsiChar(value));
      if not Assigned(Ext) then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to create extension');
      OPENSSL_sk_push(sk, Pointer(ext));
  end;

 { build stack for an X509 certificate request }
    procedure BuildReqAltSubjAll(sk :PSTACK_OF_X509_EXTENSION);  { V8.64 }
    var
        MultiValuesStack : PStack;
    begin
        if FTotAltItems = 0 then Exit;
        MultiValuesStack := BuildAltStackAll;
        if NOT Assigned(MultiValuesStack) then Exit;
        try
          if X509V3_add1_i2d(@sk, NID_subject_alt_name, MultiValuesStack, 0, 0) = 0 then
                RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to create extension Altname stack');
        finally
            OPENSSL_sk_free(MultiValuesStack);
            ClearAltStack;
        end;
    end;


var
    SubjName  : PX509_NAME;
    Exts      : PSTACK_OF_X509_EXTENSION;
    AName     : String;  { V8.64 }
    ErrFlag, CheckFlag : Boolean;
begin
    InitializeSsl;
    if NOT Assigned(PrivateKey) then
        raise ECertToolsException.Create('Must create private key first');

 { V8.64 convert domain to A-Label (Punycode ASCII, validate for allowed characters }
    if NOT FBasicIsCA then begin
        CheckFlag := (Pos('*', FCommonName) = 0);  // wild card will fail domain name check
        AName := IcsIDNAToASCII(IcsTrim(FCommonName), CheckFlag, ErrFlag);
        if ErrFlag then
            raise ECertToolsException.Create('Invalid Common Name, Illegal Characters');
        end
    else
        AName := IcsTrim(FCommonName); // CA is usually a company name, not a domain

    if Assigned(FNewReq) then X509_REQ_free(FNewReq);
    FNewReq := X509_Req_new;
    if NOT Assigned(FNewReq) then
         raise ECertToolsException.Create('Can not create new request object');

   if X509_REQ_set_version(FNewReq, 2) = 0 then   // v3
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set version in request');

    if X509_REQ_set_pubkey(FNewReq, PrivateKey) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to add public key to request');

    SubjName := X509_REQ_get_subject_name(FNewReq);  { V8.66 }
    if not Assigned(SubjName) then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Can not read X509 subject_name');
    AddNameEntryByTxt(SubjName, 'CN', AName);      { V8.64 }
    AddNameEntryByTxt(SubjName, 'OU', FOrgUnit);
    AddNameEntryByTxt(SubjName, 'ST', FState);
    AddNameEntryByTxt(SubjName, 'O',  FOrganization);
    AddNameEntryByTxt(SubjName, 'C',  FCountry);
    AddNameEntryByTxt(SubjName, 'L',  FLocality);
    AddNameEntryByTxt(SubjName, 'D',  FDescr);

    if Length(AnsiString(FEmail)) > 0 then
        X509_NAME_add_entry_by_NID(SubjName, NID_pkcs9_emailAddress,
                    MBSTRING_ASC, PAnsiChar(AnsiString(FEmail)), -1, -1, 0);

 { add extensions - create new stack  }
    Exts := OPENSSL_sk_new_null;
    Add_Ext(Exts, NID_key_usage, BuildKeyUsage);

  { Extended Key usage }
    Add_Ext(Exts, NID_ext_key_usage, BuildExKeyUsage);

  { subject alternate names }
  { V8.64 add DNS, IPs and emails together, previously only one }
    BuildAltPropList;
    BuildReqAltSubjAll(Exts);

 { finally add stack of extensions to request }
    if X509_REQ_add_extensions(FNewReq, Exts) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to add extensions to request');
    OPENSSL_sk_pop_free(Exts, @X509_EXTENSION_free);

 { sign request with private key and digest/hash }
    if EVP_PKEY_base_id(PrivateKey) = EVP_PKEY_ED25519 then begin   { V8.51 no digest for EVP_PKEY_ED25519 }
        if X509_REQ_sign(FNewReq, PrivateKey, Nil) = 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign request with ED25519 key');
    end
    else begin
        if X509_REQ_sign(FNewReq, PrivateKey, IcsSslGetEVPDigest(FCertDigest)) = 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign request with digest');
    end;
    SetX509Req(FNewReq);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ similar to DoCertReqProps but creates a new X509 certificate request from }
{ an existing certificate, ie to renew a expiring certificate. }
{ This method needs X509 and PrivateKey completed, and creates }
{ the request in X509Req from where is may be saved by SaveReqTo.   }
procedure TSslCertTools.DoCertReqOld;
begin
    InitializeSsl;
    if NOT Assigned(X509) then
        raise ECertToolsException.Create('Must open old certificate first');
    if NOT Assigned(PrivateKey) then
        raise ECertToolsException.Create('Must create private key first');
    if X509_check_private_key(X509, PrivateKey) < 1 then
        raise ECertToolsException.Create('Old certificate and private key do not match');

  { create new request from old certificate - warning ignores alternate subject DNS }
    if Assigned(FNewReq) then X509_REQ_free(FNewReq);
    if EVP_PKEY_base_id(PrivateKey) = EVP_PKEY_ED25519 then    { V8.51 no digest for EVP_PKEY_ED25519 }
        FNewReq := X509_to_X509_REQ(X509, PrivateKey, Nil)
    else
        FNewReq := X509_to_X509_REQ(X509, PrivateKey, IcsSslGetEVPDigest(FCertDigest));
    if NOT Assigned (FNewReq) then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign request with digest');
     SetX509Req(FNewReq);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ create a self signed certificate from properties or request and private key }
procedure TSslCertTools.DoSelfSignCert(UseCSR: Boolean = False);   { V8.57 added UseCSR }
var
    SubjName  : PX509_NAME;
//    TempSerial : ULARGE_INTEGER; { V8.42 was TULargeInteger } { 64-bit integer record }
    AName     : String;  { V8.64 }
    ErrFlag, CheckFlag : Boolean;
begin
    InitializeSsl;
    if NOT Assigned(PrivateKey) then
        raise ECertToolsException.Create('Must create private key first');
    if UseCSR then begin   { V8.57 }
        if NOT Assigned(FX509Req) then
            raise ECertToolsException.Create('Must open certificate request first');
    end;

 { V8.64 convert domain to A-Label (Punycode ASCII, validate for allowed characters }
    if NOT FBasicIsCA then begin
        CheckFlag := (Pos('*', FCommonName) = 0);  // wild card will fail domain name check
        AName := IcsIDNAToASCII(IcsTrim(FCommonName), CheckFlag, ErrFlag);
        if ErrFlag then
            raise ECertToolsException.Create('Invalid Common Name, Illegal Characters');
     end
     else
        AName := IcsTrim(FCommonName); // CA is usually a company name, not a domain

    if Assigned(FNewCert) then X509_free(FNewCert);
    FNewCert := X509_new;
    if NOT Assigned(FNewCert) then
         RaiseLastOpenSslError(ECertToolsException, FALSE, 'Can not create new X509 object');

   { set version, serial number, expiry }
    if X509_set_version(FNewCert, 2) = 0 then   // v3
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set version to certificate');;
    if FSerialNum = 0 then begin
    //    TempSerial.LowPart := IcsRandomInt($7FFFFFFF);
    //    TempSerial.HighPart := IcsRandomInt($7FFFFFFF);
    //    FSerialNum := TempSerial.QuadPart;
    //    FSerialNum := IcsRandomInt($7FFFFFFF) * IcsRandomInt($7FFFFFFF);  { V8.65 }
        FSerialNum := IcsMakeLongLong(IcsRandomInt($7FFFFFFF), IcsRandomInt($7FFFFFFF));  { V8.65 }
    end;
    if FExpireDays < 7 then FExpireDays := 7;  { V8.62 was 30 }
    ASN1_INTEGER_set_int64(X509_get_serialNumber(FNewCert), FSerialNum);
    X509_gmtime_adj(X509_get0_notBefore(FNewCert), 0);                                { V8.66 }
    X509_gmtime_adj(X509_get0_notAfter(FNewCert), 60 * 60 * 24 * FExpireDays);        { V8.66 }

    if X509_set_pubkey(FNewCert, PrivateKey) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to add public key to certificate');

 { V8.57 see if using CSR for out new certificate }
    if UseCSR then begin

      { set the cert subject name to the request subject }
        SubjName := X509_NAME_dup(X509_REQ_get_subject_name(X509Req));       { V8.66 }
        if NOT Assigned(SubjName) or (X509_set_subject_name(FNewCert, SubjName) = 0) then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set subject from request');

      { It's self signed so set the issuer name to be the same as the subject. }
        X509_set_issuer_name(FNewCert, SubjName);

      { Add basic contraints }
        SetCertExt(FNewCert, NID_basic_constraints, BuildBasicCons(FBasicIsCA));

     { copy some extension from request to certificate }
        BuildAltReqList;  { V8.64 }
        BuildCertAltSubjAll(FNewCert);

     { Key usage }
        SetCertExt(FNewCert, NID_key_usage, AnsiString(IcsUnwrapNames(ReqKeyUsage)));

    { Extended Key usage }
        SetCertExt(FNewCert, NID_ext_key_usage, AnsiString(IcsUnwrapNames(ReqExKeyUsage)));
    end

 { build certificate from properties }
    else begin

    { build certificate subject name as one line
      ie Subject: C=GB, ST=Surrey, L=Croydon, O=Magenta Systems Ltd, CN=Magenta Systems Ltd }
        SubjName := X509_get_subject_name(FNewCert);
        if not Assigned(SubjName) then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Can not read X509 subject_name');
        AddNameEntryByTxt(SubjName, 'CN', AName);      { V8.64 }
        AddNameEntryByTxt(SubjName, 'OU', FOrgUnit);
        AddNameEntryByTxt(SubjName, 'ST', FState);
        AddNameEntryByTxt(SubjName, 'O',  FOrganization);
        AddNameEntryByTxt(SubjName, 'C',  FCountry);
        AddNameEntryByTxt(SubjName, 'L',  FLocality);
        AddNameEntryByTxt(SubjName, 'D',  FDescr);

        if Length(AnsiString(FEmail)) > 0 then
            X509_NAME_add_entry_by_NID(SubjName, NID_pkcs9_emailAddress,
                        MBSTRING_ASC, PAnsiChar(AnsiString(FEmail)), -1, -1, 0);

      { It's self signed so set the issuer name to be the same as the subject. }
        X509_set_issuer_name(FNewCert, SubjName);

      { Add basic contraints }
        SetCertExt(FNewCert, NID_basic_constraints, BuildBasicCons(FBasicIsCA));

      { Key usage }
        SetCertExt(FNewCert, NID_key_usage, BuildKeyUsage);

      { Extended Key usage }
        SetCertExt(FNewCert, NID_ext_key_usage, BuildExKeyUsage);

      { subject alternate names }
      { V8.64 add DNS, IPs and emails together, previously only one }
        BuildAltPropList;
        BuildCertAltSubjAll(FNewCert);
    end;

  { Issuer Alternative Name. }
    if FAltIssuer <> '' then begin
        SetCertExt(FNewCert, NID_issuer_alt_name, AnsiString(FAltIssuer));
    end;

  { CRL distribution points - URI:http://myhost.com/myca.crl }
    if FCRLDistPoint <> '' then begin
        SetCertExt(FNewCert, NID_crl_distribution_points, AnsiString(FCRLDistPoint));
    end;

  { Authority Info Access - OCSP;URI:http://ocsp.myhost.com/ }
    if FAuthInfoAcc <> '' then begin
        SetCertExt(FNewCert, NID_info_access, AnsiString(FAuthInfoAcc));
    end;

  { V8.62 acmeIdentifier "1.3.6.1.5.5.7.1.31" or OBJ_id_pe,31L, dynamically created NID,
    expecting binary sha256 digest in hex }
//    if Length(FAcmeIdentifier) = 64 then begin
    if Length(FAcmeIdentifier) > 0 then begin
        SetCertExt(FNewCert, ICS_NID_acmeIdentifier,
                        AnsiString('critical,DER:04:20:' + FAcmeIdentifier));  // octet string(4), 32 long, converted from hex to binary
//                            AnsiString('critical,DER:' + FAcmeIdentifier));  // octet string(4), 32 long, converted from hex to binary
    end;

  { self sign it with our key and hash digest }
    if EVP_PKEY_base_id(PrivateKey) = EVP_PKEY_ED25519 then begin   { V8.51 no digest for EVP_PKEY_ED25519 }
        if X509_sign(FNewCert, PrivateKey, Nil) = 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign certifcate with ED25519 key');
    end
    else begin
        if X509_sign(FNewCert, PrivateKey, IcsSslGetEVPDigest(FCertDigest)) <= 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign certificate with digest');
    end;
    SetX509(FNewCert);

  { add certificate to our database ? }
  { note this is done in OverbyteIcsSslX509Certs }
 end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslCertTools.SaveToCADatabase(const CertFName: String = 'unknown'): Boolean;
var
    FStream: TFileStream;
    CertRec: String;
const
  DateTimeMask = 'yymmddhhnnss"Z"';
begin
    Result := False;
    if FCADBFile = '' then Exit;
    if NOT IsCertLoaded then Exit;

// CA database record, similar to OpenSSL but with SANs on the end
// 1 - status - V=valid, R=revoked, E=expired
// 2 - cert expire date
// 3 - cert revoked date (empty is not)
// 4 - serial number in hex
// 5 - cert file name or 'unknown'
// 6 - cert subject including common name
// 7 - cert subject alternate names
    CertRec := 'V' + IcsTAB + FormatDateTime(DateTimeMask, ValidNotAfter) +
      IcsTAB + {no revoke data } IcsTAB + SerialNumHex + IcsTAB + CertFName +
      IcsTAB + SubjectOneLine + IcsTAB + IcsUnwrapNames(GetSubjectAltName.Value) + icsCRLF;
        //  IcsUnwrapNames(SubAltNameDNS) + icsCRLF;

    if NOT ForceDirectories (ExtractFileDir (FCADBFile)) then
        raise ECertToolsException.Create('Failed to create CA DB Directory: ' + FCADBFile);
    FStream := Nil;
    try
        if NOT FileExists((FCADBFile)) then
            FStream := TFileStream.Create (FCADBFile, fmCreate)
        else begin
            FStream := TFileStream.Create (FCADBFile, fmOpenReadWrite OR fmShareDenyWrite);
            FStream.Seek(0, soEnd);
        end;
        FStream.WriteBuffer(AnsiString(CertRec) [1], Length (CertRec));
        Result := True;
    finally
        FStream.Free;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ create a new CA signed certificate from a certificate request }
{ need to create a CA certificate (X509CA) and private key (PrivKeyCA) first }
{ not finished, does not copy extensions }
procedure TSslCertTools.DoSignCertReq(CopyExtns: Boolean);
var
    SubjName  : PX509_NAME;
//    TempSerial : ULARGE_INTEGER; { V8.42 was TULargeInteger } { 64-bit integer record }
begin
    InitializeSsl;
    if NOT Assigned(FX509CA) then
        raise ECertToolsException.Create('Must open CA certificate first');
    if NOT Assigned(FX509Req) then
        raise ECertToolsException.Create('Must open certificate request first');
    if NOT Assigned(FPrivKeyCA) then
        raise ECertToolsException.Create('Must open CA private key first');
    if X509_check_private_key(FX509CA, FPrivKeyCA) = 0 then
        raise ECertToolsException.Create('CA certificate and private key do not match');

    if Assigned(FNewCert) then X509_free(FNewCert);
    FNewCert := X509_new;
    if NOT Assigned(FNewCert) then
         RaiseLastOpenSslError(ECertToolsException, FALSE, 'Can not create new X509 object');

   { set version, serial number, expiry and public key }
    if X509_set_version(FNewCert, 2) = 0 then  { version 3}
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set version to certificate');
    if FSerialNum = 0 then begin
     // TempSerial.LowPart := IcsRandomInt($7FFFFFFF);
     // TempSerial.HighPart := IcsRandomInt($7FFFFFFF);
     // FSerialNum := TempSerial.QuadPart;
    //    FSerialNum := IcsRandomInt($7FFFFFFF) * IcsRandomInt($7FFFFFFF);  { V8.65 }
        FSerialNum := IcsMakeLongLong(IcsRandomInt($7FFFFFFF), IcsRandomInt($7FFFFFFF));  { V8.65 }
    end;
    if FExpireDays < 7 then FExpireDays := 7;   { V8.62 was 30 }
    if ASN1_INTEGER_set_int64(X509_get_serialNumber(FNewCert), FSerialNum) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set serial num');
    X509_gmtime_adj(X509_get0_notBefore(FNewCert), 0);                              { V8.66 }
    X509_gmtime_adj(X509_get0_notAfter(FNewCert), 60 * 60 * 24 * FExpireDays);      { V8.66 }

  { public key from request, not CA }
    if X509_set_pubkey(FNewCert, X509_REQ_get0_pubkey(X509Req)) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set public key to certificate');

  { set the cert subject name to the request subject }
    SubjName := X509_NAME_dup(X509_REQ_get_subject_name(X509Req));                 { V8.66 }
    if NOT Assigned(SubjName) or (X509_set_subject_name(FNewCert, SubjName) = 0) then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set subject from request');

  { set the issuer name to CA subject }
    if X509_set_issuer_name(FNewCert, X509_get_subject_name(FX509CA)) = 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to set issuer from CA');

  { set extensions }

  { Add basic contraints }
    SetCertExt(FNewCert, NID_basic_constraints, BuildBasicCons(FBasicIsCA));

     if NOT CopyExtns then begin

      { subject alternate names }
      { V8.64 add DNS, IPs and emails together, previously only one }
        BuildAltPropList;
        BuildCertAltSubjAll(FNewCert);

     { Key usage }
        SetCertExt(FNewCert, NID_key_usage, BuildKeyUsage);

    { Extended Key usage }
        SetCertExt(FNewCert, NID_ext_key_usage, BuildExKeyUsage);

    end
    else begin
     { copy some extension from request to certificate }
        BuildAltReqList;  { V8.64 }
        BuildCertAltSubjAll(FNewCert); { V8.64 }

     { Key usage }
        SetCertExt(FNewCert, NID_key_usage, AnsiString(IcsUnwrapNames(ReqKeyUsage)));

    { Extended Key usage }
        SetCertExt(FNewCert, NID_ext_key_usage, AnsiString(IcsUnwrapNames(ReqExKeyUsage)));

    end;

  { Sign it with CA certificate and hash digest }
     if EVP_PKEY_base_id(FPrivKeyCA) = EVP_PKEY_ED25519 then begin   { V8.51 no digest for EVP_PKEY_ED25519 }
        if X509_sign(FNewCert, FPrivKeyCA, Nil) = 0 then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign certifcate with ED25519 key');
    end
    else begin
       if X509_sign(FNewCert, FPrivKeyCA, IcsSslGetEVPDigest(FCertDigest)) <= 0 then
        RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to sign certificate with digest');
    end;
    SetX509(FNewCert);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BNGENCBcallFunc(p: Integer; n: Integer; cb: PBN_GENCB): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
//    c: AnsiChar;
    Arg: Pointer;
    CertTools: TSslCertTools;
begin
    result := 1; // success, will 0 cause the function to fail??
    if NOT Assigned(cb) then Exit;
    try
        Arg := BN_GENCB_get_arg(cb);
        if NOT Assigned (Arg) then exit;
        CertTools := TSslCertTools(Arg);
        with CertTools do begin
          { good idea to call ProcessMessages in event so program remains responsive!!! }
            if Assigned(CertTools.FOnKeyProgress) then
                    CertTools.FOnKeyProgress(CertTools);
        end;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EVPPKEYCBcallFunc(pctx: PEVP_PKEY_CTX): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
var
    Arg: Pointer;
    CertTools: TSslCertTools;
begin
    result := 1; // success, will 0 cause the function to fail??
    if NOT Assigned(pctx) then Exit;
    try
        Arg := EVP_PKEY_CTX_get_app_data(pctx);
        if NOT Assigned (Arg) then exit;
        CertTools := TSslCertTools(Arg);
        with CertTools do begin
          { good idea to call ProcessMessages in event so program remains responsive!!! }
            if Assigned(CertTools.FOnKeyProgress) then
                    CertTools.FOnKeyProgress(CertTools);
        end;
    except
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Creating a key pair is the first step in creating X509 certificates.  }
{ The private key must be kept secure and never distributed, it will be }
{ needed to use the X509 certificate with an SSL server or client, to sign }
{ X509 certificate requests or self signed X509 certificates or to sign }
{ other documents.  The public key may be extracted from the private key. }
{ The public key is added to the X509 certificate and request and is not
{ normally needed as a separate file }
{ This method creates a new key pair in Privatekey using RSA or EC
{ Elliptic Curve) methods with varying key sizes according to the PrivKeyType
{ property. Various SaveTo methods may be used to write the keys to files, }
{ optionally encrypted with a password to protect it.   }
procedure TSslCertTools.DoKeyPair;
var
    Bits, KeyNid, CurveNid: Integer;
    Pctx: PEVP_PKEY_CTX;      { V8.49 total rewrite uising these methods }
    KeyInfo: string;
begin
    InitializeSsl;
    if NOT ICS_RAND_INIT_DONE then IcsRandPoll;
 //   callback := Nil;

 { note private keys can use DSA, but this is now obsolete }
    if Assigned(FPrivKey) then
        EVP_PKEY_free(FPrivKey);   { V8.49 free and new }
    FPrivKey := Nil;

    CurveNid := 0;
    Bits := 0;
    if (FPrivKeyType >= PrivKeyRsa1024) and (FPrivKeyType <= PrivKeyRsa15360) then begin
        KeyNid := EVP_PKEY_RSA;
        KeyInfo := 'RSA';
        case FPrivKeyType of
            PrivKeyRsa1024:  Bits := 1024;
            PrivKeyRsa2048:  Bits := 2048;
            PrivKeyRsa3072:  Bits := 3072;
            PrivKeyRsa4096:  Bits := 4096;
            PrivKeyRsa7680:  Bits := 7680;
            PrivKeyRsa15360: Bits := 15360;
        else
            Bits := 2048;
        end;
    end
    else if (FPrivKeyType = PrivKeyEd25519) then begin   { V8.50 needs OpenSSL 1.1.1 }
        KeyNid := EVP_PKEY_ED25519;
        KeyInfo := 'ED25519';
    end
    else if (FPrivKeyType >= PrivKeyRsaPss2048) and (FPrivKeyType <= PrivKeyRsaPss15360) then begin    { V8.51 needs OpenSSL 1.1.1 }
        KeyNid := EVP_PKEY_RSA_PSS;
        KeyInfo := 'RSA-PSS';
        case FPrivKeyType of
            PrivKeyRsaPss2048:  Bits := 2048;
            PrivKeyRsaPss3072:  Bits := 3072;
            PrivKeyRsaPss4096:  Bits := 4096;
            PrivKeyRsaPss7680:  Bits := 7680;
            PrivKeyRsaPss15360: Bits := 15360;
        else
            Bits := 2048;
        end;
    end
    else if (FPrivKeyType >= PrivKeyECsecp256) and (FPrivKeyType <= PrivKeyECsecp512) then begin
        KeyNid := EVP_PKEY_EC;
        KeyInfo := 'EC';
        case FPrivKeyType of
            PrivKeyECsecp256:  CurveNid := NID_X9_62_prime256v1;  // aka secp256r1
            PrivKeyECsecp384:  CurveNid := NID_secp384r1;
            PrivKeyECsecp512:  CurveNid := NID_secp521r1;
            PrivKeyECsecp256k: CurveNid := NID_secp256k1;      { V8.67 Kobitz curve }
            else
                CurveNid := NID_X9_62_prime256v1;
        end;
    end
    else begin
        RaiseLastOpenSslError(ECertToolsException, true, 'Unknown Private Key Type');  { V8.64 need an error }
        Exit;
    end;

 { initialise context for private keys }
    Pctx := EVP_PKEY_CTX_new_id(KeyNid, Nil);
    if NOT Assigned(Pctx) then
            RaiseLastOpenSslError(ECertToolsException, true, 'Failed to create new ' + KeyInfo + ' key');
    if EVP_PKEY_keygen_init(Pctx) = 0 then
            RaiseLastOpenSslError(ECertToolsException, true, 'Failed to init ' + KeyInfo + ' keygen');
    if (KeyNid = EVP_PKEY_RSA) or (KeyNid = EVP_PKEY_RSA_PSS) then begin
        if (Bits > 0) and (EVP_PKEY_CTX_set_rsa_keygen_bits(Pctx, Bits) = 0) then
            RaiseLastOpenSslError(ECertToolsException, true, 'Failed to set RSA bits');
         // EVP_PKEY_CTX_set_rsa_padding
         // EVP_PKEY_CTX_set_signature_md
    end;
    if (CurveNid > 0) and (EVP_PKEY_CTX_set_ec_paramgen_curve_nid(Pctx, CurveNid) = 0) then
                RaiseLastOpenSslError(ECertToolsException, true, 'Failed to set EC curve');
    if (KeyNid = EVP_PKEY_RSA_PSS) then begin
        // pending - various macros to restrict digests, MGF1 and minimum salt length
        // EVP_PKEY_CTX_set_rsa_pss_keygen_md
        // EVP_PKEY_CTX_set_rsa_pss_saltlen
        // EVP_PKEY_CTX_set_rsa_pss_keygen_mgf1_md
    end;


  { progress callback, really only needed for slow RSA }
    EVP_PKEY_CTX_set_app_data(Pctx, Self);
    EVP_PKEY_CTX_set_cb(Pctx, @EVPPKEYCBcallFunc);

  { generate private key pair }
    if EVP_PKEY_keygen(Pctx, @FPrivKey) = 0 then
            RaiseLastOpenSslError(ECertToolsException, true, 'Failed to generate ' + KeyInfo + ' key');
    if NOT Assigned(FPrivKey) then
            RaiseLastOpenSslError(ECertToolsException, true, 'Failed to create new ' + KeyInfo + ' key, empty');
    SetPrivateKey(FPrivKey);
    EVP_PKEY_CTX_free(Pctx);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This method creates DHParams which are needed by servers using DH and DHE
{ key exchange (but not for ECDH and ECDHE).  DHParams are usually created once }
{ for a server or application, if they become known it can speed up cracking. }
{ This method only needs a length and output file name specified. }
{ Beware, very time consuming, 1024 less than a min, 2048 few mins, 4096 few hours, }
{ 8192 gave up after two days, so use the OnKeyProgress event to to call }
{ ProcessMessages in event so program remains responsive!!!   }
function TSslCertTools.DoDHParams(const FileName: String; Bits: integer): String;
var
    FileBio   : PBIO;
    DhParam   : PDH;
    Ret, Err, Len : Integer;
    callback  : PBN_GENCB;
    AStr      : AnsiString;
begin
    InitializeSsl;
    callback := Nil;
    result := '';
    DhParam := Nil;
    FileBio := IcsSslOpenFileBio(FileName, bomWrite);
    if NOT Assigned(FileBio) then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to open file = ' + FileName);
    try
      { create progress callback }
        callback := BN_GENCB_new;
        if Assigned(callback) then
            BN_GENCB_set(callback, @BNGENCBcallFunc, Self);

      { generate DHParams }
        DhParam := DH_new;
        Ret := DH_generate_parameters_ex(DhParam, Bits, 2, callback);
        if (Ret = 0) then
            RaiseLastOpenSslError(ECertToolsException, FALSE, 'Failed to generate DHParams');
        Ret := DH_check(DhParam, @Err);
        if (Ret = 0) then
            RaiseLastOpenSslError(ECertToolsException, FALSE,
                'Failed to check DHParams, Error ' + IntToHex (Err, 8));

        ret := PEM_write_bio_DHparams(FileBio, DhParam);
        if ret = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                                  'Error writing DHParams to ' + FileName);
        BIO_free(FileBIO);

      { now save params again to string }
        FileBio := BIO_new(BIO_s_mem);
        if Assigned(FileBio) then begin
            PEM_write_bio_DHparams(FileBio, DhParam);
            Len := BIO_ctrl(FileBio, BIO_CTRL_PENDING_, 0, nil);     { V8.66 literal changed }
            SetLength(AStr, Len);
            if Len > 0 then begin
                Bio_read(FileBio, PAnsiChar(AStr), Len);
                SetLength(AStr, StrLen(PAnsiChar(AStr)));
                Result := String(AStr);
            end;
        end;

    finally
        if Assigned(FileBIO) then
            BIO_free(FileBIO);
        if Assigned(DhParam) then
            DH_free(DhParam);
        if Assigned (callback) then
            BN_GENCB_free(callback);
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.DoClearCerts;
begin
    InitializeSsl;
    X509 := Nil;
    PrivateKey := Nil;
    X509Req := Nil;
    X509Inters := Nil;
    FreeAndNilX509Req;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslCertTools.DoClearCA;  { V8.57 split from DoClearCerts }
begin
    InitializeSsl;
    FreeAndNilX509CA;
    FreeAndNilPrivKeyCA;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ method to create an SSL server certificate bundle file with an SSL certificate,
  matching public key and optional itermediate certificate(s), to avoid
  distributing and loading them all separately, output file format is determined
  by the file extension, PEM, PFX or P12 - no others can have cert and pkey }
procedure TSslCertTools.CreateCertBundle(const CertFile, PKeyFile, InterFile, LoadPw,
                      SaveFile, SavePw: String; KeyCipher: TSslPrivKeyCipher = PrivKeyEncNone);
begin
    ClearAll;
    LoadFromFile(CertFile, croNo, croTry, '');
    if NOT IsCertLoaded then
        raise Exception.Create('No certificate found 9n file');
    PrivateKeyLoadFromPemFile(PKeyFile, LoadPw);
    if InterFile <> '' then
        LoadIntersFromPemFile(InterFile);
    SaveToFile(SaveFile, true, true, IsInterLoaded, SavePw, KeyCipher);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ OpenSSL 3.0 get private key parameters specified in an array }
{ array must be set-up with one record for each key required and a blank }
function TSslCertTools.GetKeyParams(var Params: POSSL_PARAM): Boolean;  { V8.67 }
begin
    Result := False;
    if ICS_OPENSSL_VERSION_MAJOR < 3 then Exit;
    if NOT Assigned(PrivateKey) then
        raise ECertToolsException.Create('Must create private key first');
    Result := (EVP_PKEY_get_params(PrivateKey, @Params) = 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ V8.41 replaced old code with TSslCertTools component }
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer;
  IsCA: Boolean; Days: Integer;
  const KeyFileName: String = ''; Comment: boolean = false);
var
    MySslCertTools: TSslCertTools;
    KeyType: TSslPrivKeyType;
begin
    MySslCertTools := TSslCertTools.Create(nil);
    try
        MySslCertTools.Country        := Country;
        MySslCertTools.State          := State;
        MySslCertTools.Locality       := Locality;
        MySslCertTools.Organization   := Organization;
        MySslCertTools.OrgUnit        := OUnit;
        MySslCertTools.Email          := EMail;
        MySslCertTools.CommonName     := CName;
        MySslCertTools.BasicIsCA      := IsCA;
        MySslCertTools.ExpireDays     := Days;   { V8.64 got lost }
        if Bits > 4096 then
            KeyType := PrivKeyRsa7680
        else if Bits > 3072 then
            KeyType := PrivKeyRsa4096
        else if Bits > 2048 then
            KeyType := PrivKeyRsa3072
        else if Bits > 1024 then
            KeyType := PrivKeyRsa2048
        else
            KeyType := PrivKeyRsa1024;
        MySslCertTools.PrivKeyType := KeyType;

      { create private and public key, then create SSL certificate }
        MySslCertTools.DoKeyPair;
        MySslCertTools.DoSelfSignCert;

      { now save them to PEM files - no passwords }
        if KeyFileName <> '' then begin
            MySslCertTools.PrivateKeySaveToPemFile(KeyFileName);
            MySslCertTools.SaveToPemFile(FileName, False, Comment, False);
        end
        else begin
            MySslCertTools.SaveToPemFile(FileName, True, Comment, False);
       end;
    finally
        MySslCertTools.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create a self signed certificate with subject alternative names, specific
// private key, optionally with ACME tls-alpn-01 challenge identifier
// used by TSocketServer to create certificate if missing so it can start-up
procedure CreateSelfSignCertEx(const FileName, CName: String; AltDNSLst: TStrings;
        PKeyType: TSslPrivKeyType = PrivKeyECsecp256; const PW: String = '';
                                                      const AcmeId: String = '');  { V8.64 }
var
    MySslCertTools: TSslCertTools;
    PrivKeyType: TSslPrivKeyCipher;
begin
    MySslCertTools := TSslCertTools.Create(nil);
    try
        MySslCertTools.CommonName := CName;
        if Assigned(AltDNSLst) then
            MySslCertTools.AltDNSList.Assign(AltDNSLst)
        else
            MySslCertTools.AltDNSList.Add(CName);
        MySslCertTools.AcmeIdentifier := AcmeId;
        MySslCertTools.BasicIsCA := False;
        MySslCertTools.PrivKeyType := PKeyType;
        if AcmeId <> '' then
            MySslCertTools.ExpireDays := 7
        else
            MySslCertTools.ExpireDays := 2000;
        MySslCertTools.KeyCertSign := true;
        MySslCertTools.KeyDigiSign := true;
        MySslCertTools.KeyNonRepud := true;
        MySslCertTools.KeyKeyEnc := true;
        MySslCertTools.KeyExtServer := true;

      { create private and public key, then create SSL certificate }
        MySslCertTools.DoKeyPair;
        MySslCertTools.DoSelfSignCert;

      { now save them to file - no password unless PKX }
        PrivKeyType := PrivKeyEncNone;
        if PW <> '' then PrivKeyType := PrivKeyEncAES256;  { V8.67 was PrivKeyEncTripleDES }
        MySslCertTools.SaveToFile(FileName, True, True, False, PW, PrivKeyType);
    finally
        MySslCertTools.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.41 replaced old code with TSslCertTools component }
procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: String;
  Bits: Integer; Comment: boolean = false);
var
    MySslCertTools: TSslCertTools;
    KeyType: TSslPrivKeyType;
begin
    MySslCertTools := TSslCertTools.Create(nil);
    try
        MySslCertTools.Country        := Country;
        MySslCertTools.State          := State;
        MySslCertTools.Locality       := Locality;
        MySslCertTools.Organization   := Organization;
        MySslCertTools.OrgUnit        := OUnit;
        MySslCertTools.Email          := EMail;
        MySslCertTools.CommonName     := CName;
        if Bits > 4096 then
            KeyType := PrivKeyRsa7680
        else if Bits > 3072 then
            KeyType := PrivKeyRsa4096
        else if Bits > 2048 then
            KeyType := PrivKeyRsa3072
        else if Bits > 1024 then
            KeyType := PrivKeyRsa2048
        else
            KeyType := PrivKeyRsa1024;
        MySslCertTools.PrivKeyType := KeyType;

      { create private and public key, then create certificate request }
        MySslCertTools.DoKeyPair;
        MySslCertTools.DoCertReqProps;

      { now save them to PEM files - no passwords }
        MySslCertTools.PrivateKeySaveToPemFile(KeyFileName);
        MySslCertTools.SaveReqToFile(RequestFileName, Comment);
    finally
        MySslCertTools.Free;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncryptPublicRSA(
    PubKey      : PEVP_PKEY;
    InBuf       : Pointer;
    InLen       : Cardinal;
    OutBuf      : Pointer;
    var OutLen  : Cardinal;
    Padding     : TRsaPadding): Boolean;
var
    Bytes     : Word;
    MaxBytes  : Word;
    BlockSize : Word;
    BytesRet  : Integer;
    InBufPtr  : PAnsiChar;
    OutBufPtr : PAnsiChar;
    PadSize   : Byte;
    IntPad    : Integer;
begin
    Result := FALSE;
  {  if not LibeayExLoaded then
        LoadLibeayEx;   }
    if not Assigned(PubKey) then
        raise Exception.Create('Public key not assigned');
    if Ics_Ssl_EVP_PKEY_GetType(PubKey) <> EVP_PKEY_RSA then
        raise Exception.Create('No RSA key');
    if (InBuf = nil) or (InLen = 0) then
        raise Exception.Create('Invalid input buffer');
    case Padding of
      rpNoPadding :
          begin
              IntPad := RSA_NO_PADDING;
              PadSize := 0;
          end;
      rpPkcs1Oaep :
          begin
              IntPad := RSA_PKCS1_OAEP_PADDING;
              PadSize := RSA_PKCS1_OAEP_PADDING_SIZE;
          end;
      else
        IntPad := RSA_PKCS1_PADDING;
        PadSize := RSA_PKCS1_PADDING_SIZE;
    end;
    BlockSize := EVP_PKEY_size(PubKey);
    MaxBytes := BlockSize - PadSize;
    { Calculate the required result buffer size }
    if InLen <= MaxBytes then
        BytesRet := BlockSize
    else
        BytesRet := (InLen div MaxBytes + 1) * BlockSize;
    if (OutLen = 0) or (OutBuf = nil) or
       (Integer(OutLen) < BytesRet) then begin
       { Return required size and exit }
        OutLen := BytesRet;
        Exit; //***
    end;
    InBufPtr  := InBuf;
    OutBufPtr := OutBuf;
    OutLen   := 0;
    repeat
        if InLen > MaxBytes then
            Bytes := MaxBytes
        else
            Bytes := InLen;
        if Bytes > 0 then begin
            BytesRet := RSA_public_encrypt(
                                            Bytes,
                                            InBufPtr,
                                            OutBufPtr,
                                            Ics_Ssl_EVP_PKEY_GetKey(PubKey),
                                            IntPad);
            if BytesRet <> BlockSize then
            begin
                if BytesRet = -1 then
                    RaiseLastOpenSslError(Exception, TRUE,
                                          'Function RSA_public_encrypt:')
                else
                    raise Exception.Create('f_RSA_public_encrypt: ' +
                                        'Ciphertext must match length of key');
            end;
            Dec(InLen, Bytes);
            Inc(InBufPtr, Bytes);
            Inc(OutBufPtr, BytesRet);
            Inc(OutLen, BytesRet);
        end;
    until InLen = 0;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DecryptPrivateRSA(
    PrivKey     : PEVP_PKEY;
    InBuf       : Pointer;
    InLen       : Cardinal;
    OutBuf      : Pointer;
    var OutLen  : Cardinal;
    Padding     : TRsaPadding): Boolean;
var
    Bytes     : Word;
    BlockSize : Word;
    BytesRet  : Integer;
    InBufPtr  : PAnsiChar;
    OutBufPtr : PAnsiChar;
    IntPad    : Integer;
begin
    Result := FALSE;
  {  if not LibeayExLoaded then
        LoadLibeayEx;    }
    if PrivKey = nil then
        raise Exception.Create('Private key not loaded');
    if Ics_Ssl_EVP_PKEY_GetType(PrivKey) <> EVP_PKEY_RSA then
        raise Exception.Create('No RSA key');
    if (InBuf = nil) or (InLen = 0) then
        raise Exception.Create('Invalid input buffer');
    if (OutLen = 0) or (OutBuf = nil) or
       (InLen > OutLen) then begin
       { Return required size and exit }
        OutLen := InLen;
        Exit; //***
    end;
    case Padding of
      rpNoPadding : IntPad := RSA_NO_PADDING;
      rpPkcs1Oaep : IntPad := RSA_PKCS1_OAEP_PADDING;
      else
        IntPad := RSA_PKCS1_PADDING;
    end;
    Blocksize := EVP_PKEY_size(PrivKey);
    OutLen    := 0;
    InBufPtr  := InBuf;
    OutBufPtr := OutBuf;
    repeat
        if InLen > BlockSize then
            Bytes := BlockSize
        else
            Bytes := InLen;
        if Bytes > 0 then begin
            BytesRet := RSA_private_decrypt(
                                             Bytes,
                                             InBufPtr,
                                             OutBufPtr,
                                             Ics_Ssl_EVP_PKEY_GetKey(PrivKey),
                                             IntPad);
            if BytesRet = -1 then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Function RSA_private_decrypt:');
            Dec(InLen, Bytes);
            Inc(InBufPtr, Bytes);
            Inc(OutBufPtr, BytesRet);
            Inc(OutLen, BytesRet);
        end;
    until InLen = 0;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Takes plain text, returns an encrypted and base64 encoded string }
function StrEncRsa(
    PubKey  : PEVP_PKEY;
    const S : AnsiString;
    B64     : Boolean;
    Padding : TRsaPadding = rpPkcs1): AnsiString;
var
    Len : Cardinal;
begin
    { First call is to get return buffer size }
    EncryptPublicRSA(PubKey, PAnsiChar(S), Length(S), nil, Len, Padding);
    SetLength(Result, Len);
    if EncryptPublicRSA(PubKey, PAnsiChar(S), Length(S), PAnsiChar(Result), Len, Padding) then
    begin
        if B64 then
            Result := Base64Encode(Result);
    end
    else
        SetLength(Result, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Takes an encryted and base64 encoded string, returns plain text }
function StrDecRsa(
    PrivKey : PEVP_PKEY;
    S       : AnsiString;
    B64     : Boolean;
    Padding : TRsaPadding = rpPkcs1): AnsiString;
var
    Len : Cardinal;
begin
    if B64 then
        S := Base64Decode(S);
    Len := Length(S);
    SetLength(Result, Len);
    if DecryptPrivateRSA(PrivKey, PAnsiChar(S), Len, PAnsiChar(Result), Len, Padding) then
        { Adjust string length! }
        SetLength(Result, Len)
    else
        SetLength(Result, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphFinalize(var CiphCtx: TCiphContext);
begin
    if Assigned(CiphCtx.Ctx) then begin
   //    EVP_CIPHER_CTX_cleanup(CiphCtx.Ctx);     { V8.66 }
        EVP_CIPHER_CTX_free(CiphCtx.Ctx);
    end;
    FillChar(CiphCtx, SizeOf(CiphCtx), #0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CalcMD5(
    Buffer: Pointer;
    BufSize: Integer;
    MD5Digest : PMD5Digest);
var
    I          : Integer;
    MD5Context : TMD5Context;
begin
    for I := 0 to 15 do
        MD5Digest^[I] := I + 1;
    MD5Init(MD5Context);
    MD5UpdateBuffer(MD5Context, Buffer, BufSize);
    MD5Final(MD5Digest^, MD5Context);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphPasswordToKey(
    InBuf       : AnsiString;
    Salt        : TCipherSalt;
    Count       : Integer;
    var Key     : TCipherKey;
    var KeyLen  : Integer;
    var IV      : TIVector;
    var IVLen   : Integer);
var
    I, nKey, nIV : Integer;
    MD5Digest  : TMD5Digest;
    MD5Context : TMD5Context;
    AddDigest  : Boolean;
begin
    FillChar(Key, SizeOf(TCipherKey), #0);
    FillChar(IV, SizeOf(TIVector), #0);
    if (KeyLen = 0) or (Length(InBuf)= 0) then Exit;
    if KeyLen > SizeOf(TCipherKey) then
        KeyLen := SizeOf(TCipherKey);
    nKey := 0;
    nIV  := 0;
    AddDigest := False;
    for I := 0 to 15 do
        MD5Digest[I] := I + 1;
    while True do
    begin
       MD5Init(MD5Context);
       if AddDigest then
          MD5UpdateBuffer(MD5Context, @MD5Digest[0], SizeOf(MD5Digest))
       else
          AddDigest := TRUE;
       MD5UpdateBuffer(MD5Context, @InBuf[1], Length(InBuf));
       if Length(Salt) > 0 then
            MD5UpdateBuffer(MD5Context, @Salt[1], Length(Salt));
       MD5Final(MD5Digest, MD5Context);
       for I := 1 to Count do begin
          MD5Init(MD5Context);
          MD5UpdateBuffer(MD5Context, @MD5Digest[0], SizeOf(MD5Digest));
          MD5Final(MD5Digest, MD5Context);
       end;
       I := 0;
       if nKey <= KeyLen then
       begin
           while True do
           begin
              if (nKey > KeyLen) or (I > SizeOf(MD5Digest)) then
                  Break;
              Key[nKey] := MD5Digest[I];
              Inc(nkey);
              Inc(I);
           end;
       end;
       if nIV <= IVLen then
       begin
           while True do
           begin
              if (nIV > IVLen) or (I > SizeOf(MD5Digest)) then
                  Break;
              IV[nIV] := MD5Digest[I];
              Inc(nIV);
              Inc(I);
           end;
       end;
       if (nKey > KeyLen) and (nIV > IVLen) then Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphInitialize(
    var CiphCtx : TCiphContext;
    const Pwd   : AnsiString;
    Key         : PCipherKey; // if not nil Pwd is ignored and the user is responsible to provide a valid Key and IV
    IVector     : PIVector;
    CipherType  : TCipherType;
    KeyLen      : TCipherKeyLen;
    Enc         : Boolean);
var
    SetKeySize  : Integer;
    //I           : Integer;
    PIV         : Pointer;
    KLen, IVLen : Integer;
begin
 {   if not LibeayExLoaded then
        LoadLibeayEx;   }
    SetKeySize := 0;
    KLen := 0;
    CiphFinalize(CiphCtx);
    CiphCtx.Encrypt := Enc;
    case CipherType of
        ctBfCbc, {ctBfCfb64,} ctBfOfb, ctBfEcb :
          { Blowfish keysize 32-448 bits in steps of 8 bits, default 128 bits }
            begin
                CiphCtx.BlockSize := BF_BLOCK_SIZE;
                IVLen := SizeOf(TIVector);
                case KeyLen of
                  cklDefault, ckl128bit : KLen := 16;
                  ckl64bit  : begin Klen := 8; SetKeySize := KLen; end;
                  ckl256bit : begin Klen := EVP_MAX_KEY_LENGTH; SetKeySize := KLen; end;
                end;
                if CipherType = ctBfCbc then
                    CiphCtx.Cipher := EVP_bf_cbc
                else if CipherType = ctBfOfb then
                    CiphCtx.Cipher := EVP_bf_ofb
                else begin
                    CiphCtx.Cipher := EVP_bf_ecb;
                    IVLen := 0;
                end;
            end;
        else
            raise Exception.Create('Not implemented');
    end;
    { Make the key and IV based on password, this is simple, not compatible }
    { with any standards. }
    if Key = nil then begin
        CalcMD5(@Pwd[1], Length(Pwd), @CiphCtx.Key[0]);     //128-bit key
        if KLen + IVLen > 16 then
            CalcMD5(@CiphCtx.Key[0], 16, @CiphCtx.Key[16]); //256-bit key
        if KLen + IVLen > 32 then
            CalcMD5(@CiphCtx.Key[0], 32, @CiphCtx.Key[32]); //384-bit key
        {if KeyLen + CiphCtx.IVLen > 48 then
            CalcMD5(@CiphCtx.Key[0], 48, @CiphCtx.Key[48]); //512-bit key}
    end
    else
        CiphCtx.Key := Key^;
    if IVLen > 0 then begin
        if IVector = nil then
            Move(CiphCtx.Key[KLen], CiphCtx.IV[0], SizeOf(TIVector))
        else
            Move(IVector^[0], CiphCtx.IV[0], SizeOf(TIVector));
        PIV := @CiphCtx.IV[0];
    end
    else
        PIV := nil;

    CiphCtx.Ctx := EVP_CIPHER_CTX_new;
    try
        EVP_CIPHER_CTX_reset(CiphCtx.Ctx);
        if SetKeySize > 0 then begin
            if EVP_CipherInit_ex(CiphCtx.Ctx, CiphCtx.Cipher, nil, nil, nil, Ord(Enc)) = 0 then          { V8.66 }
                RaiseLastOpenSslError(Exception, TRUE, 'Function EVP_CipherInit_ex:');
            EVP_CIPHER_CTX_set_key_length(CiphCtx.Ctx, SetKeySize);
        end;
        if SetKeySize > 0 then begin
            if EVP_CipherInit_ex(CiphCtx.Ctx, nil, nil, @CiphCtx.key[0], PIV, Ord(Enc)) = 0 then          { V8.66 }
                RaiseLastOpenSslError(Exception, TRUE,  'Function EVP_CipherInit_ex:');
        end
        else begin
            if EVP_CipherInit_ex(CiphCtx.Ctx, CiphCtx.Cipher, nil, @CiphCtx.key[0], PIV, Ord(Enc)) = 0 then     { V8.66 }
                RaiseLastOpenSslError(Exception, TRUE, 'Function EVP_CipherInit_ex:');
        end;
    except
        CiphFinalize(CiphCtx);
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphSetIVector(
    var CiphCtx : TCiphContext;
    IVector     : PIVector);
var
    PIV : Pointer;
begin
    if not Assigned(CiphCtx.Ctx) then
        raise Exception.Create('Cipher context not initialized');
    if IVector = nil then begin
        FillChar(CiphCtx.IV, SizeOf(TIVector), #0);
        PIV := nil;
    end
    else begin
        CiphCtx.IV := IVector^;
        PIV := @CiphCtx.IV[0];
    end;
    if EVP_CipherInit_ex(CiphCtx.Ctx, nil, nil, nil, PIV, Ord(CiphCtx.Encrypt)) = 0 then    { V8.66 }
        RaiseLastOpenSslError(Exception, TRUE, 'Function EVP_CipherInit_ex:');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphUpdate(
    const InBuf;
    InLen : Integer;
    const OutBuf;
    var OutLen : Integer;
    CiphCtx : TCiphContext);
begin
    if not Assigned(CiphCtx.Ctx) then
        raise Exception.Create('Cipher context not initialized');
    if EVP_CipherUpdate(CiphCtx.Ctx, PAnsiChar(@OutBuf), @OutLen, PAnsiChar(@InBuf), InLen) = 0 then    { V8.66 }
        RaiseLastOpenSslError(Exception, TRUE,  'Function EVP_CipherUpdate:');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphFinal(
    const OutBuf;
    var OutLen : Integer;
    CiphCtx : TCiphContext);
begin
    if not Assigned(CiphCtx.Ctx) then
        raise Exception.Create('Cipher context not initialized');
    if EVP_CipherFinal_ex(CiphCtx.Ctx, PAnsiChar(@OutBuf), @OutLen) = 0 then   { V8.66 }
        RaiseLastOpenSslError(Exception, TRUE, 'Function EVP_CipherFinal_ex:');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Takes plain text, returns an encrypted string, optionally base64 encoded }
function StrEncBf(
    const S   : AnsiString;
    const Pwd : AnsiString;
    IV        : PIVector;
    KeyLen    : TCipherKeyLen;
    B64       : Boolean): AnsiString;
var
    Len, TmpLen : Integer;
    CiphCtx : TCiphContext;
begin
    FillChar(CiphCtx, SizeOf(CiphCtx), #0);
    CiphInitialize(CiphCtx, Pwd, nil, IV, ctBfCbc, KeyLen, True);
    try
        Len := Length(S);
        SetLength(Result, Len + CiphCtx.BlockSize);
        CiphUpdate(S[1], Length(S), Result[1], Len, CiphCtx);
        CiphFinal(Result[Len + 1], TmpLen, CiphCtx);
        Inc(Len, TmpLen);
        SetLength(Result, Len);
    finally
        CiphFinalize(CiphCtx);
    end;
    if B64 then
         Result := Base64Encode(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StrDecBf(
    S         : AnsiString;
    const Pwd : AnsiString;
    IV        : PIVector;
    KeyLen    : TCipherKeyLen;
    B64       : Boolean): AnsiString;
var
    Len, TmpLen : Integer;
    CiphCtx : TCiphContext;
begin
    FillChar(CiphCtx, SizeOf(CiphCtx), #0);
    CiphInitialize(CiphCtx, Pwd, nil, IV, ctBfCbc, KeyLen, False);
    try
        if B64 then
            S := Base64Decode(S);
        Len := Length(S);
        SetLength(Result, Len + CiphCtx.BlockSize);
        CiphUpdate(S[1], Length(S), Result[1], Len, CiphCtx);
        CiphFinal(Result[Len + 1], TmpLen, CiphCtx);
        Inc(Len, TmpLen);
        SetLength(Result, Len);
    finally
        CiphFinalize(CiphCtx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StreamEncrypt(
    SrcStream  : TStream;
    DestStream : TStream;
    StrBlkSize : Integer;
    CiphCtx    : TCiphContext;
    RandomIV   : Boolean);
var
    Bytes, RetLen, TmpLen : Integer;
    Len : Int64;
    InBuf : array of Byte;
    OutBuf : array of Byte;
    IV : TIVector;
begin
    if RandomIV then begin
        RAND_bytes(@IV[0], SizeOf(TIVector));
        CiphSetIVector(CiphCtx, @IV);
    end;
    if StrBlkSize < 1024 then StrBlkSize := 1024;
    SetLength(InBuf, StrBlkSize);
    SetLength(OutBuf, StrBlkSize + CiphCtx.BlockSize); // Room for padding
    Len := SrcStream.Size;
    SrcStream.Position  := 0;
    DestStream.Position := 0;
    { The IV must be known to decrypt, it may be public.   }
    { Without a random IV we always get the same cipher    }
    { text when using both same key and data.              }
    { http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation }
    if RandomIV then // prepend the Initialization Vector data
        DestStream.Write(IV[0], SizeOf(TIVector));
    TmpLen := 0;
    RetLen := TmpLen;
    while True do begin
        Bytes := SrcStream.Read(InBuf[0], StrBlkSize);
        if Bytes > 0 then begin
            Dec(Len, Bytes);
            CiphUpdate(InBuf[0], Bytes, OutBuf[0], RetLen, CiphCtx);
            if Len <= 0 then begin
                CiphFinal(OutBuf[RetLen], TmpLen, CiphCtx);
                Inc(RetLen, TmpLen);
            end;
            if RetLen <> 0 then
                DestStream.Write(OutBuf[0], RetLen);
        end
        else
            Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StreamEncrypt(
    SrcStream  : TStream;
    DestStream : TStream;
    StrBlkSize : Integer;
    CiphCtx    : TCiphContext;
    RandomIV   : Boolean;
    Obj        : TObject;
    ProgressCallback : TCryptProgress);
var
    Bytes, RetLen, TmpLen : Integer;
    Len : Int64;
    InBuf : array of Byte;
    OutBuf : array of Byte;
    IV : TIVector;
    Cancel: Boolean;
begin
    Cancel := FALSE;
    if RandomIV then begin
        RAND_bytes(@IV[0], SizeOf(TIVector));
        CiphSetIVector(CiphCtx, @IV);
    end;
    if StrBlkSize < 1024 then StrBlkSize := 1024;
    SetLength(InBuf, StrBlkSize);
    SetLength(OutBuf, StrBlkSize + CiphCtx.BlockSize); // Room for padding
    Len := SrcStream.Size;
    SrcStream.Position  := 0;
    DestStream.Position := 0;
    { The IV must be known to decrypt, it may be public.   }
    { Without a random IV we always get the same cipher    }
    { text when using both same key and data.              }
    { http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation }
    if RandomIV then// prepend the Initialization Vector data
        DestStream.Write(IV[0], SizeOf(TIVector));
    TmpLen := 0;
    RetLen := TmpLen;
    while True do begin
        Bytes := SrcStream.Read(InBuf[0], StrBlkSize);
        if Bytes > 0 then begin
            Dec(Len, Bytes);
            CiphUpdate(InBuf[0], Bytes, OutBuf[0], RetLen, CiphCtx);
            if Len <= 0 then begin
                CiphFinal(OutBuf[RetLen], TmpLen, CiphCtx);
                Inc(RetLen, TmpLen);
            end;
            if RetLen <> 0 then begin
                DestStream.Write(OutBuf[0], RetLen);
                if Assigned(ProgressCallback) then begin
                    ProgressCallback(Obj, SrcStream.Position, Cancel);
                    if Cancel then
                        Break;
                end;
            end;
        end
        else
            Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StreamDecrypt(
    SrcStream  : TStream;
    DestStream : TStream;
    StrBlkSize : Integer;
    CiphCtx    : TCiphContext;
    RandomIV   : Boolean);
var
    Bytes, RetLen, TmpLen : Integer;
    Len : Int64;
    InBuf : array of Byte;
    OutBuf : array of Byte;
    IV : TIVector;
begin
    SrcStream.Position := 0;
    if RandomIV then begin
        SrcStream.Read(IV[0], SizeOf(TIVector));
        CiphSetIVector(CiphCtx, @IV);
    end;
    if StrBlkSize < 1024 then StrBlkSize := 1024;
    SetLength(InBuf, StrBlkSize);
    SetLength(OutBuf, StrBlkSize + CiphCtx.BlockSize);
    Len := SrcStream.Size;
    DestStream.Position := 0;
    TmpLen := 0;
    RetLen := TmpLen;
    while True do begin
        Bytes := SrcStream.Read(InBuf[0], StrBlkSize);
        if Bytes > 0 then begin
            Dec(Len, Bytes);
            CiphUpdate(InBuf[0], Bytes, OutBuf[0], RetLen, CiphCtx);
            if Len <= 0 then begin
                CiphFinal(OutBuf[RetLen], TmpLen, CiphCtx);
                Inc(RetLen, TmpLen);
            end;
            if RetLen <> 0 then
                DestStream.Write(OutBuf[0], RetLen);
        end
        else
            Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StreamDecrypt(
    SrcStream  : TStream;
    DestStream : TStream;
    StrBlkSize : Integer;
    CiphCtx    : TCiphContext;
    RandomIV   : Boolean;
    Obj        : TObject;
    ProgressCallback : TCryptProgress);
var
    Bytes, RetLen, TmpLen : Integer;
    Len : Int64;
    InBuf : array of Byte;
    OutBuf : array of Byte;
    IV : TIVector;
    Cancel: Boolean;
begin
    Cancel := FALSE;
    SrcStream.Position := 0;
    if RandomIV then begin
        SrcStream.Read(IV[0], SizeOf(TIVector));
        CiphSetIVector(CiphCtx, @IV);
    end;
    if StrBlkSize < 1024 then StrBlkSize := 1024;
    SetLength(InBuf, StrBlkSize);
    SetLength(OutBuf, StrBlkSize + CiphCtx.BlockSize);
    Len := SrcStream.Size;
    DestStream.Position := 0;
    TmpLen := 0;
    RetLen := TmpLen;
    while True do begin
        Bytes := SrcStream.Read(InBuf[0], StrBlkSize);
        if Bytes > 0 then begin
            Dec(Len, Bytes);
            CiphUpdate(InBuf[0], Bytes, OutBuf[0], RetLen, CiphCtx);
            if Len <= 0 then begin
                CiphFinal(OutBuf[RetLen], TmpLen, CiphCtx);
                Inc(RetLen, TmpLen);
            end;
            if RetLen <> 0 then begin
                DestStream.Write(OutBuf[0], RetLen);
                if Assigned(ProgressCallback) then begin
                    ProgressCallback(Obj, SrcStream.Position, Cancel);
                    if Cancel then
                        Break;
                end;
            end;
        end
        else
            Break;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure CreateRsaKeyPair(const PubFName, PrivFName: String; Bits: Integer);   { V8.35 }
var
    Bne       : PBIGNUM;
    Rsa       : PRSA;
    PubBIO, PrivBio : PBIO;
    Ret       : Integer;
begin
    if NOT ICS_RAND_INIT_DONE then IcsRandPoll;
    PubBIO := nil;
    PrivBIO := nil;
    Rsa := nil;
  { generate fixed odd number exponent }
    Bne := BN_new;
    Ret := BN_set_word(Bne, RSA_F4);
    if Ret = 0 then
        raise Exception.Create('Failed to create exponent');
    try

      { generate RSA key paid }
        Rsa := RSA_new;
        Ret := RSA_generate_key_ex (Rsa, Bits, Bne, nil);
        if (Ret = 0) or (not Assigned(Rsa)) then
            raise Exception.Create('Failed to generate rsa key');

      { save public key file }
        PubBIO := BIO_new_file(PAnsiChar(StringToUtf8(PubFname)), PAnsiChar('w+'));
        Ret := PEM_write_bio_RSAPublicKey (PubBIO, Rsa);
        if Ret = 0 then
            raise Exception.Create('Failed to save public key file: ' + PubFname);

       { save private key file }
        PrivBIO := BIO_new_file(PAnsiChar(StringToUtf8(PrivFname)), PAnsiChar('w+'));
        Ret := PEM_write_bio_RSAPrivateKey (PrivBIO, Rsa, nil, nil, 0, nil, nil);
        if Ret = 0 then
            raise Exception.Create('Failed to save private key file: ' + PrivFname);

    finally
        if Assigned(Bne) then
            BN_free(Bne);
        if Assigned(Rsa) then
            RSA_free(Rsa);
        if Assigned(PubBio) then
            BIO_free(PubBio);
        if Assigned(PrivBio) then
            BIO_free(PrivBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF}  { USE_SSL }


end.
