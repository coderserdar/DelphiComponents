{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Aug 26, 2007
Description:
Version:      1.03
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007-2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslX509Utils;

interface

uses
    Windows, SysUtils, Classes, OverbyteIcsSSLEAY, OverbyteIcsLibeay,
    OverbyteIcsLibeayEx, OverByteIcsMD5, OverbyteIcsMimeUtils, OverbyteIcsUtils;

procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: AnsiString;
  Bits: Integer);  overload;
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: AnsiString; Bits: Integer;
  IsCA: Boolean; Days: Integer); overload;

{$IFDEF UNICODE}
procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: String;
  Bits: Integer);  overload;
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer;
  IsCA: Boolean; Days: Integer); overload;
{$ENDIF UNICODE}

{ RSA crypto functions }

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

const
  BF_BLOCK_SIZE     = 8;

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


implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LastOpenSslErrMsg(Dump: Boolean): AnsiString;
var
    ErrMsg  : AnsiString;
    ErrCode : Integer;
begin
    ErrCode := f_ERR_get_error;
    SetLength(Result, 512);
    f_ERR_error_string_n(ErrCode, PAnsiChar(Result), Length(Result));
    SetLength(Result, StrLen(PAnsiChar(Result)));
    if Dump then begin
        ErrCode := f_ERR_get_error;
        while ErrCode <> 0 do begin
            SetLength(ErrMsg, 512);
            f_ERR_error_string_n(ErrCode, PAnsiChar(ErrMsg), Length(ErrMsg));
            SetLength(ErrMsg, StrLen(PAnsiChar(ErrMsg)));
            Result := Result + #13#10 + ErrMsg;
            ErrCode := f_ERR_get_error;
        end;
    end;
end;


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
function AddNameEntryByTxt(Name: PX509_NAME; const Field: AnsiString;
 const Value: String): Integer;
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
    if Length(AStr) > 0 then
        Result := f_X509_NAME_add_entry_by_txt(Name, PAnsiChar(Field), SType,
                                               PAnsiChar(AStr), -1, -1, 0)
    else
        Result := 0;                                 
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF UNICODE}
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer;
  IsCA: Boolean; Days: Integer);
var
    X         : PX509;
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Ex        : PX509_EXTENSION;
begin
    FileBio := nil;
    X       := nil;
    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;
    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
        raise Exception.Create('Could not create key object');
    try
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PAnsiChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        X := f_X509_new;
        if not Assigned(X) then
            raise Exception.Create('Cert object nil');

        f_X509_set_version(X, 2);
        f_ASN1_INTEGER_set(f_X509_get_serialNumber(X), 0{serial});
        f_X509_gmtime_adj(f_Ics_X509_get_notBefore(X), 0);
        f_X509_gmtime_adj(f_Ics_X509_get_notAfter(X), 60 * 60 * 24 * Days);
        f_X509_set_pubkey(X, PK);

        Name := f_X509_get_subject_name(X);
        if not Assigned(Name) then
            raise Exception.Create('Function "f_X509_get_subject_name" failed');

        { This function creates and adds the entry, working out the
        correct string type and performing checks on its length.
        Normally we'd check the return value for errors...  	}

        AddNameEntryByTxt(Name, 'CN', CName);
        AddNameEntryByTxt(Name, 'OU', OUnit);
        AddNameEntryByTxt(Name, 'ST', State);
        AddNameEntryByTxt(Name, 'O',  Organization);
        AddNameEntryByTxt(Name, 'C',  Country);
        AddNameEntryByTxt(Name, 'L',  Locality);

        if Length(AnsiString(Email)) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                        MBSTRING_ASC, PAnsiChar(AnsiString(Email)), -1, -1, 0);

        { It's self signed so set the issuer name to be the same as the
        subject. }
        f_X509_set_issuer_name(X, Name);

        {* Add extension using V3 code: we can set the config file as NULL
        * because we wont reference any other sections. We can also set
        * the context to NULL because none of these extensions below will need
        * to access it.
        *}
        { Add various extensions }
        if IsCA then
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                        PAnsiChar('critical,CA:TRUE'))
        else
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                       PAnsiChar('critical,CA:FALSE'));

        if not Assigned(Ex) then
            raise Exception.Create('Function f_X509V3_EXT_conf_nid failed');
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        (* Optional extensions

        { Purposes }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_key_usage,
                                PAnsiChar('critical, keyCertSign, cRLSign'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        { Some Netscape specific extensions }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_comment,
                                PAnsiChar('ICS Group'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_cert_type,
                                PAnsiChar('SSL CA, S/MIME CA, Object Signing CA'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        {Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_crl_distribution_points,
                                PAnsiChar('URI:http://www.domain.com/CRL/class1.crl'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);}

        *)

        { Sign it }
        if f_X509_sign(X, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign certificate');

        { We write private key as well as certificate to the same file }
        FileBio := f_BIO_new_file(PAnsiChar(AnsiString(FileName)), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');

        { Write private key }
        { Callback, old format }
        //if f_PEM_write_bio_PrivateKey(FileBio, PK, f_EVP_des_ede3_cbc, nil, 0, @PasswordCallback, nil) = 0 then
        { Plain, old format }
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');

        { Write certificate }
        if f_PEM_write_bio_X509(FileBio, X) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
        if Assigned(PK) then
            f_EVP_PKEY_free(PK);
        if Assigned(X) then
            f_X509_free(X);
        if Assigned(FileBio) then
            f_BIO_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: String;
  Bits: Integer);

  function Add_Ext(sk : PStack; Nid : Integer; Value : PAnsiChar): Boolean;
  var
      Ext : PX509_EXTENSION;
  begin
      Ext := f_X509V3_EXT_conf_nid(nil, nil, NID, value);
      if not Assigned(Ext) then
          Result := FALSE
      else
          Result := f_sk_push(sk, Pointer(ext)) = 1;
  end;

var
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Req       : PX509_REQ;
    Exts      : PStack;
begin
    FileBio := nil;
    //Name    := nil;
    //PK      := nil;
    //exts    := nil;
    Req     := nil;

    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;

    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
      raise Exception.Create('Could not create key object');

    try
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PAnsiChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        Req := f_X509_Req_new;

        f_X509_REQ_set_pubkey(Req, pk);

        f_X509_REQ_set_version(Req, 2);

        Name := f_X509_REQ_get_subject_name(Req);

        { This function creates and adds the entry, working out the
          correct string type and performing checks on its length.
          Normally we'd check the return value for errors...
        }

        AddNameEntryByTxt(Name, 'CN', CName);
        AddNameEntryByTxt(Name, 'OU', OUnit);
        AddNameEntryByTxt(Name, 'ST', State);
        AddNameEntryByTxt(Name, 'O',  Organization);
        AddNameEntryByTxt(Name, 'C',  Country);
        AddNameEntryByTxt(Name, 'L',  Locality);

        if Length(AnsiString(Email)) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                        MBSTRING_ASC, PAnsiChar(AnsiString(Email)), -1, -1, 0);

        Exts := f_sk_new_null;
        Add_Ext(Exts, NID_key_usage, 'critical, digitalSignature, keyEncipherment');

        f_X509_REQ_add_extensions(Req, Exts);

        f_sk_pop_free(Exts, @f_X509_EXTENSION_free);

        if f_X509_REQ_sign(Req, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign request');

        FileBio := f_BIO_new_file(PAnsiChar(AnsiString(KeyFileName)), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');
        f_BIO_free(FileBio);
        FileBio := f_BIO_new_file(PAnsiChar(AnsiString(RequestFileName)), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        { Write request }
        if f_PEM_write_bio_X509_REQ(FileBio, OverByteIcsSSLEAY.PX509_REQ(Req)) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
      if Assigned(PK) then
        f_EVP_PKEY_free(PK);
      if Assigned(Req) then
        f_X509_REQ_free(Req);
      if Assigned(FileBio) then
        f_BIO_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF UNICODE}


procedure CreateSelfSignedCert(const FileName, Country, State,
    Locality, Organization, OUnit, CName, Email: AnsiString;
    Bits: Integer; IsCA: Boolean; Days: Integer);
var
    X         : PX509;
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Ex        : PX509_EXTENSION;
begin
    FileBio := nil;
    X       := nil;
    //PK      := nil;
    //Name    := nil;
    //Ex      := nil;
    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;
    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
        raise Exception.Create('Could not create key object');
    try
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PAnsiChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        X := f_X509_new;
        if not Assigned(X) then
            raise Exception.Create('Cert object nil');

        f_X509_set_version(X, 2);
        f_ASN1_INTEGER_set(f_X509_get_serialNumber(X), 0{serial});
        f_X509_gmtime_adj(f_Ics_X509_get_notBefore(X), 0);
        f_X509_gmtime_adj(f_Ics_X509_get_notAfter(X), 60 * 60 * 24 * Days);
        f_X509_set_pubkey(X, PK);

        Name := f_X509_get_subject_name(X);
        if not Assigned(Name) then
            raise Exception.Create('Function "f_X509_get_subject_name" failed');

        { This function creates and adds the entry, working out the
        correct string type and performing checks on its length.
        Normally we'd check the return value for errors...  	}

        AddNameEntryByTxt(Name, 'CN', String(CName));
        AddNameEntryByTxt(Name, 'OU', String(OUnit));
        AddNameEntryByTxt(Name, 'ST', String(State));
        AddNameEntryByTxt(Name, 'O',  String(Organization));
        AddNameEntryByTxt(Name, 'C',  String(Country));
        AddNameEntryByTxt(Name, 'L',  String(Locality));

        if Length(Email) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                                         MBSTRING_ASC, PAnsiChar(Email), -1, -1, 0);

        { It's self signed so set the issuer name to be the same as the
        subject. }
        f_X509_set_issuer_name(X, Name);

        {* Add extension using V3 code: we can set the config file as NULL
        * because we wont reference any other sections. We can also set
        * the context to NULL because none of these extensions below will need
        * to access it.
        *}
        { Add various extensions }
        if IsCA then
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                        PAnsiChar('critical,CA:TRUE'))
        else
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                       PAnsiChar('critical,CA:FALSE'));

        if not Assigned(Ex) then
            raise Exception.Create('Function f_X509V3_EXT_conf_nid failed');
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        (* Optional extensions

        { Purposes }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_key_usage,
                                PAnsiChar('critical, keyCertSign, cRLSign'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        { Some Netscape specific extensions }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_comment,
                                PAnsiChar('ICS Group'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_cert_type,
                                PAnsiChar('SSL CA, S/MIME CA, Object Signing CA'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        {Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_crl_distribution_points,
                                PAnsiChar('URI:http://www.domain.com/CRL/class1.crl'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);}

        *)

        { Sign it }
        if f_X509_sign(X, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign certificate');

        { We write private key as well as certificate to the same file }
        FileBio := f_BIO_new_file(PAnsiChar(FileName), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');

        { Write private key }
        { Callback, old format }
        //if f_PEM_write_bio_PrivateKey(FileBio, PK, f_EVP_des_ede3_cbc, nil, 0, @PasswordCallback, nil) = 0 then
        { Plain, old format }
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');

        { Write certificate }
        if f_PEM_write_bio_X509(FileBio, X) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
        if Assigned(PK) then
            f_EVP_PKEY_free(PK);
        if Assigned(X) then
            f_X509_free(X);
        if Assigned(FileBio) then
            f_BIO_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CreateCertRequest(const RequestFileName, KeyFileName, Country, State,
  Locality, Organization, OUnit, CName, Email: AnsiString; Bits: Integer);

  function Add_Ext(sk : PStack; Nid : Integer; Value : PAnsiChar): Boolean;
  var
      Ext : PX509_EXTENSION;
  begin
      Ext := f_X509V3_EXT_conf_nid(nil, nil, NID, value);
      if not Assigned(Ext) then
          Result := FALSE
      else
          Result := f_sk_push(sk, Pointer(ext)) = 1;
  end;

var
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Req       : PX509_REQ;
    Exts      : PStack;
begin
    FileBio := nil;
    //Name    := nil;
    //PK      := nil;
    //exts    := nil;
    Req     := nil;

    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;

    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
      raise Exception.Create('Could not create key object');

    try
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PAnsiChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        Req := f_X509_Req_new;

        f_X509_REQ_set_pubkey(Req, pk);

        f_X509_REQ_set_version(Req, 2);

        Name := f_X509_REQ_get_subject_name(Req);

        { This function creates and adds the entry, working out the
          correct string type and performing checks on its length.
          Normally we'd check the return value for errors...
        }

        AddNameEntryByTxt(Name, 'CN', String(CName));
        AddNameEntryByTxt(Name, 'OU', String(OUnit));
        AddNameEntryByTxt(Name, 'ST', String(State));
        AddNameEntryByTxt(Name, 'O',  String(Organization));
        AddNameEntryByTxt(Name, 'C',  String(Country));
        AddNameEntryByTxt(Name, 'L',  String(Locality));

        if Length(Email) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                                         MBSTRING_ASC, PAnsiChar(Email), -1, -1, 0);

        Exts := f_sk_new_null;
        Add_Ext(Exts, NID_key_usage, 'critical, digitalSignature, keyEncipherment');

        f_X509_REQ_add_extensions(Req, Exts);

        f_sk_pop_free(Exts, @f_X509_EXTENSION_free);

        if f_X509_REQ_sign(Req, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign request');

        FileBio := f_BIO_new_file(PAnsiChar(KeyFileName), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');
        f_BIO_free(FileBio);
        FileBio := f_BIO_new_file(PAnsiChar(RequestFileName), PAnsiChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        { Write request }
        if f_PEM_write_bio_X509_REQ(FileBio, OverByteIcsSSLEAY.PX509_REQ(Req)) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
      if Assigned(PK) then
        f_EVP_PKEY_free(PK);
      if Assigned(Req) then
        f_X509_REQ_free(Req);
      if Assigned(FileBio) then
        f_BIO_free(FileBio);
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
    if not LibeayExLoaded then
        LoadLibeayEx;
    if not Assigned(PubKey) then
        raise Exception.Create('Public key not assigned');
    if PubKey^.type_ <> EVP_PKEY_RSA then
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
    BlockSize := f_EVP_PKEY_size(PubKey);
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
            BytesRet := f_RSA_public_encrypt(
                                            Bytes,
                                            InBufPtr,
                                            OutBufPtr,
                                            PubKey^.rsa,
                                            IntPad);
            if BytesRet <> BlockSize then
            begin
                if BytesRet = -1 then
                    RaiseLastOpenSslError(Exception, TRUE,
                                          'Function f_RSA_public_encrypt:')
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
    if not LibeayExLoaded then
        LoadLibeayEx;
    if PrivKey = nil then
        raise Exception.Create('Private key not loaded');
    if PrivKey^.type_ <> EVP_PKEY_RSA then
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
    Blocksize := f_EVP_PKEY_size(PrivKey);
    OutLen    := 0;
    InBufPtr  := InBuf;
    OutBufPtr := OutBuf;
    repeat
        if InLen > BlockSize then
            Bytes := BlockSize
        else
            Bytes := InLen;
        if Bytes > 0 then begin
            BytesRet := f_RSA_private_decrypt(
                                             Bytes,
                                             InBufPtr,
                                             OutBufPtr,
                                             PrivKey^.rsa,
                                             IntPad);
            if BytesRet = -1 then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Function f_RSA_private_decrypt:');
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
        f_EVP_CIPHER_CTX_cleanup(CiphCtx.Ctx);
        f_EVP_CIPHER_CTX_free(CiphCtx.Ctx);
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
    if not LibeayExLoaded then
        LoadLibeayEx;
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
                    CiphCtx.Cipher := f_EVP_bf_cbc
                else if CipherType = ctBfOfb then
                    CiphCtx.Cipher := f_EVP_bf_ofb
                else begin
                    CiphCtx.Cipher := f_EVP_bf_ecb;
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

    CiphCtx.Ctx := f_EVP_CIPHER_CTX_new;
    try
        f_EVP_CIPHER_CTX_init(CiphCtx.Ctx);
        if SetKeySize > 0 then begin
            if not f_EVP_CipherInit_ex(CiphCtx.Ctx, CiphCtx.Cipher, nil, nil, nil,
                                       Ord(Enc)) then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Function f_EVP_CipherInit_ex:');
            f_EVP_CIPHER_CTX_set_key_length(CiphCtx.Ctx, SetKeySize);
        end;
        if SetKeySize > 0 then begin
            if not f_EVP_CipherInit_ex(CiphCtx.Ctx, nil, nil, @CiphCtx.key[0],
                                       PIV, Ord(Enc)) then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Function f_EVP_CipherInit_ex:');
        end
        else begin
            if not f_EVP_CipherInit_ex(CiphCtx.Ctx, CiphCtx.Cipher, nil,
                                       @CiphCtx.key[0],
                                       PIV, Ord(Enc)) then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Function f_EVP_CipherInit_ex:');
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
    if not f_EVP_CipherInit_ex(CiphCtx.Ctx, nil, nil, nil,
                               PIV, Ord(CiphCtx.Encrypt)) then
        RaiseLastOpenSslError(Exception, TRUE, 'Function f_EVP_CipherInit_ex:');
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
    if not f_EVP_CipherUpdate(CiphCtx.Ctx, PAnsiChar(@OutBuf), OutLen,
                              PAnsiChar(@InBuf), InLen) then
        RaiseLastOpenSslError(Exception, TRUE,
                              'Function f_EVP_CipherUpdate:');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CiphFinal(
    const OutBuf;
    var OutLen : Integer;
    CiphCtx : TCiphContext);
begin
    if not Assigned(CiphCtx.Ctx) then
        raise Exception.Create('Cipher context not initialized');
    if not f_EVP_CipherFinal_ex(CiphCtx.Ctx, PAnsiChar(@OutBuf), OutLen) then
        RaiseLastOpenSslError(Exception, TRUE,
                              'Function f_EVP_CipherFinal_ex:');
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
        f_RAND_bytes(@IV[0], SizeOf(TIVector));
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
        f_RAND_bytes(@IV[0], SizeOf(TIVector));
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


end.
