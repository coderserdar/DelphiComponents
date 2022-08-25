{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  JOSE - Json Object Signing and Encryption, used for:
                 JWS (Json Web Signing)
                 JWT (Json Web Tokens)
                 JWK (Json Web Key)
                 JWE (Json Web Encryption) not done yet
                 variously used by OAuth1, ACME and other protocols.
              Includes OpenSSL Message Authentication Code functions used
              for signing JOSE structures with secret or private/public keys.
Creation:     Feb 2018
Updated:      Mar 2022
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2022 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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


Overview
--------

Message Authentication Code are a way of verifying a block of data
has not been corrupted or changed, using a key.

Keyed-Hash Message Authentication Code (HMAC)
---------------------------------------------
Uses the same secret key to generate and verify a message, using
a hash alogrithm.

Cipher-based Message Authentication Code (CMAC)  (1.1.0 and later)
-----------------------------------------------
Uses the same secret key to generate and verify a message, using
a block cipher-based message authentication code algorithm.
Aka One-key MAC (OMAC).  Not done yet.

Asymmetric Key-Hash Message Authentication Code (Async MAC)
-------------------------------------------------------
Uses a private key to generate a message, but a public key to
verify it, using a hash alogrithm.



Json Web Signing or Tokens (JWS or JWT)
---------------------------------------

REST clients and servers typically use Json Web Signing or Tokens (JWS or JWT) to
ensure the payload sent to the server has not been corrupted and is from an
authenticated source.  This is done by creating a SHA based message authentication
code (MAC) or signature using a preagreed secret phrase or a public key for more
modern servers.

A secret phrase must always be shared in advance between client and servers,
usually via a web site account, and uses HMAC (Hash Message Authentication Code).

Using a private/public key pair is better since only the client need to know the
private key and it can never be stolen from the server which only needs the public
key. There are several ways for the client and server to share the public key.

The least safe way is for a web site account to generate both private and public
keys which are manually copied to the client with a key ID or account number
which is all that needs to sent in JWS/JWT requests.

Better, is for the client to generate the private/public key pair and either
upload the public key to a web site account or leave it accessible on the internet
typically with a .well-known URL that may be sent in the JWS.

Easiest, is for the client to generate the private/public key pair and send the
public key as a Json Web Key to the server which creates an account and returns a
Key ID which is used subsequently instead of sending the JWK again.  This is how
Let's Encrypt works, so no web site account page is needed.

The IcsJoseJWSJson and IcsJoseJWSComp functions are used by the REST client to
generate a JWS/JWT request with a payload signed by either an HmacSecret key or
a Private Key.

REST servers should use function IcsJoseCheckJWS to retrieve the payload and
verify it's integrity and authenticity, it returns TJsonVerify with verious
responses, JVerifyOkNewPubKey means the application should keep the public key
supplied in the JWS packet for future use, JVerifyNeedPubKey sets NewKeyId and
the application should call the function again with the saved public key selected
by KeyId. JVerifyOkOldPubKey or JVerifyOkSecret means verification passed with
an existing publc key or HMAC secret.

To prevent replay attacks, the client JWS may include a nonce returned by the
server in the previous response (perhaps as Replay-Nonce: header), which the
application needs to generate, save  and pass for checking.




Updates:
May 21, 2018  - 8.54 - baseline
Oct 2, 2018   - 8.57 - build with FMX
Aug 07, 2019  - 8.62 - build Jason Web Token (JWT)
                       Builds without USE_SSL
May 17, 2020  - 8.64 - IcsJoseFindAlg accepts RSA-PSS keys for jsigRsa256/512
                         (Google) and Ed25519 keys for jsigEdDSA
                       IcsJoseJWKPubKey needs OpenSSL 1.1.1e to support RSA-PSS keys.
                       Fixed a bug in IcsBase64UrlDecode thanks to Linden Roth.
                       Cleaned up signing and verifying functions.
                       Fixed various EVP digests with size_t for Win64, thanks
                         to Alexander Pastuhov.
Dec 09, 2020  - 8.65 - Added all Jose parameters and values as constants, added
                           IcsJsonPair to build name:values.
                       Fixed bug in IcsJoseHeader building bad json for KID only.
                       Added IcsJoseFindAlgType to find algorithm from literal.
                       Added IcsJoseJWKThumbprint.
                       IcsJoseJWKPubKey now optionally adds private key to JWK.
                       Added IcsJoseJWKGetPKey to get public or private key from JWK.
                       Added IcsJoseCheckSig to check JWS signature against
                          secret or public key.
                       Added IcsJoseCheckJWS for servers to check JWS or JWT from
                         Json or Compact Serialization formats and get payload.
                       RSA-PSS and Ed25519 keys work for signing and verify.
                       Posix fixes.
                       Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Mar 15, 2021 V8.66     Renamed all OpenSSL functions to original names removing ICS
                         f_ prefix.
                       Corrected IcsJoseJWKPubKey for Win64.
                       Added support for YuOpenSSL which provides OpenSSL in a pre-built
                          DCU linked into applications, rather than using external DLLs.
                       Removed support for OpenSSL 1.0.2 and 1.1.0 whose support ceased
                          Dec 2019, also Fips mode pending 3.0.
Sept 23, 2021 V8.67    Support longer RSA keys.
                       Moved IcsBase64UrlDecode, IcsBase64UrlEncode, IcsBase64UrlEncodeA
                         and IcsJsonPair to Utils to ease circular referencing.
                       Using TRestParam to build Json instead of strings and IcsJsonPair,
                         can not use ISuperObject due to it not writing out Json in order
                         specified on entry which is needed for JWS.
                       OpenSSL 3.0 deprecates low level RSA/EC function and provides
                         EVP_PKEY alternatives using OSSL_PKEY_PARAM_xx, so rewritten
                         IcsJoseJWKPubKey and IcsJoseJWKGetPKey using different functions
                         for 1.1.1 and 3.0.
                       Use AnsiStrings and functions when dealing with binary data to avoid
                         possible issues with string conversions and nulls.
Oct 06, 2021 V8.68     Support OpenSSL 3.0 for YuOpenSSL.
May 18, 2022 V8.69     Turn off deprecated warnings for YuOpenSSL 3.0 due to use of old
                         EC_KEY_ and RSA_ functions for OpenSSL 1.1.1 that can not be
                         removed until we cease support for 1.1.1.
                       Removed WSocket from uses for projects that don't need it.
                       Moved hash digest functions to OverbyteIcsLIBEAY so they can be
                         used with Json stuff.  



Pending
-------

A REST web server implementatoon to test JWS/JWT properly.



}

{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslJose;
{$ENDIF}

{$I Include\OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

{$IFDEF USE_SSL}
{$WARN SYMBOL_DEPRECATED OFF}    { VuOpenSSL 3.0 warns about too many old EC_KEY and RSA_ functions }  

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Ics.Posix.WinTypes,
    Ics.Posix.PXMessages,
    Posix.SysTypes,     // size_t
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    {$Ifdef Rtl_Namespaces}System.StrUtils{$Else}StrUtils{$Endif},   { V8.65 }
    OverbyteIcsSsleay, OverbyteIcsLibeay,
{$IFDEF FMX}
//    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
//    OverbyteIcsWSocket,                { V8.69 no longer needed }
{$ENDIF FMX}
    OverbyteIcsTypes,
    OverbyteIcsUrl,          { V8.67 }
    OverbyteIcsSuperObject,  { V8.65 }
    OverbyteIcsUtils
 {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

{ NOTE - these components only build with SSL, there is no non-SSL option }

type
//  EDigestException = class(Exception);

{ the JOSE Json Web Signature algorithm "alg" }
{ https://www.iana.org/assignments/jose/jose.xhtml }
    TJoseAlg = (jsigNone,
       jsigHmac256, jsigHmac384, jsigHmac512,    { HMAC with SHA-x -  }
       jsigRsa256, jsigRsa384, jsigRsa512,       { RSASSA-PKCS1-v1_5 using SHA-x }
       jsigEcdsa256, jsigEcdsa384, jsigEcdsa512, { ECDSA using P-x and SHA-x }
       jsigRsaPss256, jsigRsaPss384, jsigRsaPss512, { RSASSA-PSS using SHA-x and MGF1, 1.1.1 and later }
       jsigEdDSA);                               { Ed25519 no hash }

{ result for IcsJoseCheckJWS verification in servers  }
    TJoseVerify = (JVerifyUnknown, JVerifyOkOldPubKey, JVerifyOkNewPubKey,
                   JVerifyOkSecret, JVerifyNeedPubKey, JVerifyNeedSecret,
                   JVerifyFailNoJWS, JVerifyFailBadHeader, JVerifyFailNoAlg,
                   JVerifyFailBadPubKey, JVerifyFailNonce, JVerifyFailSignature,
                   JVerifyFailExcept);

const

// JSON Object Signing and Encryption (JOSE)
// https://www.iana.org/assignments/jose/jose.xhtml

    // JWS params
    JoseParams_alg   = 'alg';     // Algorithm
    JoseParams_jku   = 'jku';     // JWK Set URL
    JoseParams_jwk   = 'jwk';     // JSON Web Key
    JoseParams_kid   = 'kid';     // Key ID
    JoseParams_x5u   = 'x5u';     // X.509 URL
    JoseParams_x5c   = 'x5c';     // X.509 Certificate Chain
    JoseParams_x5t   = 'x5t';     // X.509 Certificate SHA-1 Thumbprint
    JoseParams_x5tS256 = 'x5t#S256'; // X.509 Certificate SHA-256 Thumbprint
    JoseParams_typ   = 'typ';     // Type
    JoseParams_cty   = 'cty';     // Content Type
    JoseParams_crit  = 'crit';    // Critical
    JoseParams_b64   = 'b64';     // Base64url_Encode Payload
    JoseParams_ppt   = 'ppt';     // PASSporT extension identifier
    JoseParams_url   = 'url';     // URL
    JoseParams_nonce = 'nonce';   // Nonce

    // JWE params
    JoseParame_alg   = 'alg';    // Algorithm
    JoseParame_enc   = 'enc';    // Encryption Algorithm
    JoseParame_zip   = 'zip';    // Compression Algorithm
    JoseParame_jku   = 'jku';    // JWK Set URL
    JoseParame_jwk   = 'jwk';    // JSON Web Key
    JoseParame_kid   = 'kid';    // Key ID
    JoseParame_x5u   = 'x5u';    // X.509 URL
    JoseParame_x5c   = 'x5c';    // X.509 Certificate Chain
    JoseParame_x5t   = 'x5t';    // X.509 Certificate SHA-1 Thumbprint
    JoseParame_x5tS256 = 'x5t#S256'; // X.509 Certificate SHA-256 Thumbprint
    JoseParame_typ   = 'typ';    // Type
    JoseParame_cty   = 'cty';    // Content Type
    JoseParame_crit  = 'crit';   // Critical
    JoseParame_epk   = 'epk';    // Ephemeral Public Key
    JoseParame_apu   = 'apu';    // Agreement PartyUInfo
    JoseParame_apv   = 'apv';    // Agreement PartyVInfo
    JoseParame_iv    = 'iv';     // Initialization Vector
    JoseParame_tag   = 'tag';    // Authentication Tag
    JoseParame_p2s   = 'p2s';    // PBES2 Salt Input
    JoseParame_p2c   = 'p2c';    // PBES2 Count
    JoseParame_iss   = 'iss';    // Issuer
    JoseParame_sub   = 'sub';    // Subject
    JoseParame_aud   = 'aud';    // Audience
    JoseParame_url   = 'url';    // URL
    JoseParame_nonce = 'nonce';  // Nonce

    // JWK params, some all key types, some specific
    JoseParamk_kty   = 'kty';    // Key Type
    JoseParamk_use   = 'use';    // Public Key Use
    JoseParamk_key_ops = 'key_ops'; // Key Operations
    JoseParamk_alg   = 'alg';    // Algorithm
    JoseParamk_kid   = 'kid';    // Key ID
    JoseParamk_x5u   = 'x5u';    // X.509 URL
    JoseParamk_x5c   = 'x5c';    // X.509 Certificate Chain
    JoseParamk_x5t   = 'x5t';    // X.509 Certificate  SHA-1 Thumbprint
    JoseParamk_x5tS256 = 'x5t#S256';  // X.509 Certificate SHA-256 Thumbprint
    JoseParamk_ext   = 'ext';    // Extractable
    JoseParamke_crv   = 'crv';   // Curve                  EC Public
    JoseParamke_x     = 'x';     // X Coordinate           EC Public
    JoseParamke_y     = 'y';     // Y Coordinate           EC Public
    JoseParamke_d     = 'd';     // ECC Private Key        EC Private
    JoseParamkr_n     = 'n';     // Modulus                RSA Public
    JoseParamkr_e     = 'e';     // Exponent               RSA Public
    JoseParamkr_d     = 'd';     // Private Exponent       RSA Private
    JoseParamkr_p     = 'p';     // First Prime Factor     RSA Private
    JoseParamkr_q     = 'q';     // Second Prime Factor    RSA Private
    JoseParamkr_dp    = 'dp';    // irst Factor CRT Exponent   RSA Private
    JoseParamkr_dq    = 'dq';    // Second Factor CRT Exponent RSA Private
    JoseParamkr_qi    = 'qi';    // First CRT Coefficient  RSA Private
    JoseParamkr_oth   = 'oth';   // Other Primes Info      RSA Private
    JoseParamko_k     = 'k';     // Key Value HMAC         oct Private
    JoseParamko_crv   = 'crv';   // The subtype of key pair OKP Public
    JoseParamko_d     = 'd';     // The private key        OKP Private
    JoseParamko_x     = 'x';     // The public key         OKP Public

    // JWK publc key use
    JoseLituse_sig   = 'sig';    // Digital Signature or MAC
    JoseLituse_enc   = 'enc';    // Encryption

    // JWK key type
    JoseLitkty_EC    = 'EC';     // Elliptic Curve
    JoseLitkty_RSA   = 'RSA';    // RSA
    JoseLitkty_oct   = 'oct';    // Octet sequence HMAC
    JoseLitkty_OKP   = 'OKP';    // Octet string key pairs raw keys

    // JWK Curve Name
    JoseLitcrv_P256    = 'P-256';   // P-256 Curve
    JoseLitcrv_P384    = 'P-384';   // P-384 Curve
    JoseLitcrv_P521    = 'P-521';   // P-521 Curve
    JoseLitcrv_Ed25519 = 'Ed25519'; // Ed25519 signature algorithm key pairs
    JoseLitcrv_Ed448   = 'Ed448';   // Ed448 signature algorithm key pairs
    JoseLitcrv_X25519  = 'X25519';  // X25519 function key pairs
    JoseLitcrv_X448    = 'X448';    // X448 function key pairs
    JoseLitcrv_secp256k1 = 'secp256k1'; // SECG secp256k1 curve

    // JWK and JWS Algorithm
    JoseLitalg_none  = 'none';    // No digital signature or MAC performed
    JoseLitalg_HS256 = 'HS256';   // HMAC using SHA-256
    JoseLitalg_HS384 = 'HS384';   // HMAC using SHA-384
    JoseLitalg_HS512 = 'HS512';   // HMAC using SHA-512
    JoseLitalg_RS256 = 'RS256';   // RSASSA-PKCS1-v1_5 using SHA-256
    JoseLitalg_RS384 = 'RS384';   // RSASSA-PKCS1-v1_5 using SHA-384
    JoseLitalg_RS512 = 'RS512';   // RSASSA-PKCS1-v1_5 using SHA-512
    JoseLitalg_ES256 = 'ES256';   // ECDSA using P-256 and SHA-256
    JoseLitalg_ES384 = 'ES384';   // ECDSA using P-384 and SHA-384
    JoseLitalg_ES512 = 'ES512';   // ECDSA using P-521 and SHA-512
    JoseLitalg_PS256 = 'PS256';   // RSASSA-PSS using SHA-256 and MGF1 with SHA-256
    JoseLitalg_PS384 = 'PS384';   // RSASSA-PSS using SHA-384 and MGF1 with SHA-384
    JoseLitalg_PS512 = 'PS512';   // RSASSA-PSS using SHA-512 and MGF1 with SHA-512
    JoseLitalg_EdDSA = 'EdDSA';   // EdDSA signature algorithms

  // JWS JSON Serialization used by Acme
    JsonSerialheader    = 'header';
    JsonSerialprotected = 'protected';    // same as header
    JsonSerialpayload   = 'payload';
    JsonSerialsignature = 'signature';


    JoseAlgLits: array[TJoseAlg] of String = (JoseLitalg_none,
           JoseLitalg_HS256, JoseLitalg_HS384, JoseLitalg_HS512,
           JoseLitalg_RS256, JoseLitalg_RS384, JoseLitalg_RS512,
           JoseLitalg_ES256, JoseLitalg_ES384, JoseLitalg_ES512,
           JoseLitalg_PS256, JoseLitalg_PS384, JoseLitalg_PS512,
           JoseLitalg_EdDSA);

    JoseVerifyLits: array[TJoseVerify] of string = ('Unknown',
        'Verified OK with old public key',  // used passed PublicKey and optionally KeyId
        'Verified OK with new public key',  // PublicKey have been updated with new key
        'Verified OK with shared secret',   // used passed HmacSecret
        'Repeat with old public key for key id',
                                 // KeyId updated with account for required PublicKey
        'Repeat with shared secret',        // no HmacSecret passed
        'Verify failed, no JWS found',      // do not recognise passed JWS as Json or Compact
        'Verify failed, invalid header Json', // no Json header
        'Verify failed, no algorithm found',// no supported JWS algorithm found
        'Verify failed, bad public key in JWK', // invalid public key
        'Verify failed, invalid nonce',     // mismatch passed and sent nomce
        'Verify failed, signature invalid', // invalid verify
        'Verify failed, exception');        // unknown exception error

{ public functions }

{ digests and hashes - note all digests are binary bytes in AnsiStrings }
{
function IcsHMACDigest(const Data, Key: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
function IcsHMACDigestEx(const Data, Key: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
function IcsHMACDigestVerify(const Data, Key, OldDigest: AnsiString;
                        HashDigest: TEvpDigest = Digest_sha256): Boolean;
function IcsHashDigest(const Data: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
function IcsAsymSignDigest(const Data: AnsiString; PrivateKey: PEVP_PKEY;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
function IcsAsymVerifyDigest(const Data, OldDigest: AnsiString; PublicKey: PEVP_PKEY;
                HashDigest: TEvpDigest = Digest_sha256): Boolean; Overload;
}

{ RFC7515 Jose Header for Json Web Signature or Token, with Acme private fields }
{ V8.67 returns UTF-8 encoded Json }
function IcsJoseHeader(const Alg, Typ, Jwk, Kid, Nonce: string;
                                                const Url: string = ''): AnsiString;

{ RFC7515 find Json Web Signature hash type }
function IcsJoseFindHash(JoseAlg: TJoseAlg): TEvpDigest;

{ RFC7515 find Json Web Signature type from literal }
function IcsJoseFindAlgType(Alg: String): TJoseAlg;   { V8.65 }

{ RFC7515 find Json Web Signature algorithm and check private key matches it }
function IcsJoseFindAlg(JoseAlg: TJoseAlg; PrivateKey: PEVP_PKEY): string;

{ RFC7517 Jose JSON Web Key (JWK) with Hmac shared secret key }
function IcsJoseJWKHmac(const Secret, Alg: String;
                  const Kid: String = ''; const Use: String = ''): String;

{ RFC7517 get Hmac shared key from Jose JSON Web Key (JWK) to verify JWS }
function IcsJoseJWKGetHmac(JWK: String): String;    { V8.65 }

{ RFC7517 Jose JSON Web Key (JWK) with public key }
function IcsJoseJWKPubKey(PrivateKey: PEVP_PKEY; const Alg: String; const
    Kid: String = ''; const Use: String = ''; Priv: Boolean = False): String;  { V8.65 added Priv }

{ RFC7638 get JWK Thumbprint }
function IcsJoseJWKThumbprint(JWK: String; HashDigest: TEvpDigest = Digest_sha256): String;    { V8.65 }

{ RFC7517 get public key from Jose JSON Web Key (JWK) to verify JWS }
function IcsJoseJWKGetPKey(JWK: String; var Kid: String): PEVP_PKEY;    { V8.65 }

{ RFC7515 get JWS signature from protected header and payload }
{ by periods, ie xxx.xxx.xxx }
function IcsJoseGetSig(JoseAlg: TJoseAlg; const CombinedEn, HmacSecret: AnsiString;
                                                   PrivateKey: PEVP_PKEY): String;

{ RFC7515 check JWS signature agsinst protected header and payload }
function IcsJoseCheckSig(JoseAlg: TJoseAlg; const CombinedEn, SignatureEn,
                       HmacSecret: AnsiString; PublicKey: PEVP_PKEY): Boolean;  { V8.65 }

{ RFC7515 build Json Web Signature or Token, with Acme private fields, }
{ using JWS Compact Serialization with is three base64url blocks separated }
{ by periods, ie xxx.xxx.xxx }
function IcsJoseJWSComp(JoseAlg: TJoseAlg; const Payload, HmacSecret: string;
          PrivateKey: PEVP_PKEY; const Typ, Jwk, Kid, Nonce: string;
                                                 const Url: string = ''): string;

{ RFC7515 build Json Web Signature or Token, with Acme private fields, }
{ using JWS JSON Serialization with is three Json blocks }
{ used by ACME v2 }
function IcsJoseJWSJson(JoseAlg: TJoseAlg; const Payload, HmacSecret: string;
          PrivateKey: PEVP_PKEY; const Typ, Jwk, Kid, Nonce: string;
                                                 const Url: string = ''): string;

{ build Json Web Token (JWT) by Base64Url encoding three components as long string V8.62 }
function IcsJoseJWT(const Header, Payload, Signature: string): string;

{ RFC7515 verify JWS or JWT from Json or Compact Serialization formats and get payload }
function IcsJoseCheckJWS(const JWS, OldNonce, HmacSecret: String;
          var NewPublicKey: PEVP_PKEY; var NewKeyid, Payload: String): TJoseVerify;  { V8.65 }



{$ENDIF USE_SSL}

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* V8.69 moved to OverbyteIcsLIBEAY

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
{ create HMAC hash keyed message authentication code for some data with a simple
  key, using md5, sha1, sha256, sha512, etc, the old way  }

function IcsHMACDigest(const Data, Key: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
var
    DigLen: integer;
    Digest: array [0..EVP_MAX_MD_SIZE] of Byte;   // binary result, not hex or base64
begin
    Result := '';
    if ICS_OPENSSL_VERSION_NUMBER = 0 then IcsLoadSsl;
    DigLen := 0;
    HMAC(IcsSslGetEVPDigest(HashDigest), @Key[1], Length(Key),                   { V8.66 was PAnsiChar }
                                   @Data[1], Length(Data), @Digest, @DigLen);
    if DigLen > 0 then begin
        SetLength( Result, DigLen);
        Move(Digest[0], Result[1], DigLen);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ create HMAC hash keyed message authentication code for some data with a simple
  key, using md5, sha1, sha256, sha512, etc, the new way  }

function IcsHMACDigestEx(const Data, Key: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
var
    Signature: array [0..1025] of AnsiChar;   // binary result, not hex or base64
    PKey: PEVP_PKEY;
    Etype: PEVP_MD;
    DigestCtx: PEVP_MD_CTX;
    PkeyCtx: PEVP_PKEY_CTX;   { V8.64 }
    SigLen: size_t;           { V8.64 }
    Ret: integer;
begin
    Result := '';
    PKey := EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, Nil, PAnsiChar(Key), Length(Key));
    DigestCtx := EVP_MD_CTX_new;
    try
        Etype := IcsSslGetEVPDigest(HashDigest);
        if NOT Assigned(Etype) then
              Raise EDigestException.Create('Unsupported hash digest ' +
                           GetEnumName(TypeInfo(TEvpDigest), Ord(HashDigest)));
    // ?? this might not be necessary...test it
        Ret := EVP_DigestInit_Ex(DigestCtx, Etype, Nil);
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to initialise hash digest');
        Ret := EVP_DigestSignInit(DigestCtx, @PkeyCtx, Etype, Nil, PKey);    { V8.66 }
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to initialise signing digest');
     // Update can be called repeatedly for large streams
        ret := EVP_DigestSignUpdate(DigestCtx, Pointer(Data), Length(Data));
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to update signing digest');
        SigLen := SizeOf(Signature);
        ret := EVP_DigestSignFinal(DigestCtx, @Signature, @SigLen);
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to finalise signing digest');
        if SigLen > 0 then begin
            SetLength(Result, SigLen);
            Move(Signature[0], Result[1], SigLen);
        end;
    finally
        EVP_MD_CTX_free(DigestCtx);
        EVP_PKEY_free(Pkey);  { V8.64 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ verify a HMAC hash keyed message authentication code for some data with a simple
  key, using md5, sha1, sha256, sha512, etc, the new way  }

function IcsHMACDigestVerify(const Data, Key, OldDigest: AnsiString;
                        HashDigest: TEvpDigest = Digest_sha256): Boolean;
var
    NewDigest: AnsiString;
begin
    NewDigest := IcsHMACDigestEx(Data, Key, HashDigest);
  // constant time comparison to avoid timing attacks
    Result := (CRYPTO_memcmp(PAnsiChar(OldDigest), PAnsiChar(NewDigest), Length(NewDigest)) = 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ create a hash digest for a string, using numerous hashes, SHA, SHA2, SHA3, shake }

function IcsHashDigest(const Data: AnsiString;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
var
    DigestCtx: PEVP_MD_CTX;
    Etype: PEVP_MD;
    DigLen, Ret: integer;
    Digest: array [0..EVP_MAX_MD_SIZE] of AnsiChar;   // binary result, not hex or base64
begin
    Result := '';
    DigestCtx := EVP_MD_CTX_new;
    try
        Etype := IcsSslGetEVPDigest(HashDigest);
        if NOT Assigned(Etype) then
              Raise EDigestException.Create('Unsupported hash digest ' +
                           GetEnumName(TypeInfo(TEvpDigest), Ord(HashDigest)));
        Ret := EVP_DigestInit_Ex(DigestCtx, Etype, Nil);
        if (Ret = 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to initialise hash digest');
     // Update can be called repeatedly for large streams
        ret := EVP_DigestUpdate(DigestCtx, PByte(Data), Length(Data));
        if (Ret = 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to update hash digest');
        ret := EVP_DigestFinal_Ex(DigestCtx, @Digest, @DigLen);
        if (Ret = 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to finalise hash digest');
        if DigLen > 0 then begin
            SetLength( Result, DigLen);
            Move(Digest[0], Result[1], DigLen);
        end;
    finally
        EVP_MD_CTX_free(DigestCtx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ create asymmetic MAC hash keyed message authentication code for some data with
  a private key, using md5, sha1, sha256, sha512, etc  }
{Note - ECDSA keys contain a random element so are dfferent each time for same data }

function IcsAsymSignDigest(const Data: AnsiString; PrivateKey: PEVP_PKEY;
                HashDigest: TEvpDigest = Digest_sha256): AnsiString;
var
    Signature: array [0..8000] of AnsiChar;   // binary result, not hex or base64, V8.67 larger
    Etype: PEVP_MD;
    PkeyCtx: PEVP_PKEY_CTX;
    DigestCtx: PEVP_MD_CTX;
    SigLen: size_t;           { V8.64 }
    Ret, keytype: integer;
begin
    Result := '';
    if not Assigned(PrivateKey) then
              Raise EDigestException.Create('Private key required');
    keytype := EVP_PKEY_base_id(PrivateKey);
    if (keytype <> EVP_PKEY_RSA) and (keytype <> EVP_PKEY_EC) and
        (keytype <> EVP_PKEY_ED25519) and (keytype <> EVP_PKEY_RSA_PSS) then
              Raise EDigestException.Create('Unsupported private key type');

    DigestCtx := EVP_MD_CTX_new;
    PkeyCtx := Nil;
    SigLen := SizeOf(Signature);
    try
        Etype := IcsSslGetEVPDigest(HashDigest);
        if keytype = EVP_PKEY_ED25519 then
            EType := Nil // Needs 1.1.1
        else if NOT Assigned(Etype) then
            Raise EDigestException.Create('Unsupported hash digest ' +
                           GetEnumName(TypeInfo(TEvpDigest), Ord(HashDigest)));
        Ret := EVP_DigestSignInit(DigestCtx, @PkeyCtx, Etype, Nil, PrivateKey);
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                         'Failed to initialise signing digest');

 { send an algorithm specific control operation to context }
 { V8.64 these were never called since PkeyCtx was never set due to
     DigestSignInit not having a var declaration, but don't seem to make
     any differece to the output so not yet used }
(*     if PkeyCtx <> Nil then begin

      // pending, 3.0 uses APIs not macros, need to create in OverbyteIcsSSLEAY

      { do we really need to set digest type?? }
         // EVP_PKEY_CTX_set_signature_md macro, API in 3.0
          if EVP_PKEY_CTX_ctrl(PkeyCtx, -1, EVP_PKEY_OP_TYPE_SIG,
                                              EVP_PKEY_CTRL_MD, 0, Etype) = 0 then   { V8.64 }
                RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to set digest type for signing digest');

     { set specific EC curve NID}
          if (keytype = EVP_PKEY_EC) then begin
         //  EVP_PKEY_CTX_set_ec_paramgen_curve_nid macro, API in 3.0
            if EVP_PKEY_CTX_ctrl(
                 PkeyCtx, EVP_PKEY_EC, EVP_PKEY_OP_PARAMGEN OR EVP_PKEY_OP_KEYGEN,
                    EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID, NID_X9_62_prime256v1, Nil) = 0 then
                RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to set EC curve for signing digest');
            end;
     { pending, may need to set RSA PSS stuff }

        end;   *)

(*      ret := EVP_DigestSign(DigestCtx, @Signature, @SigLen,
                                                PAnsiChar(Data), Length(Data));  // Needs 1.1.1  }
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to finalise signing digest');
        if SigLen > 0 then begin
            SetLength(Result, SigLen);
            Move(Signature[0], Result[1], SigLen);
        end;
    finally
        EVP_MD_CTX_free(DigestCtx);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ verify asymmetic MAC hash keyed message authentication code for some data with
  a public key object, using md5, sha1, sha256, sha512, etc  }

function IcsAsymVerifyDigest(const Data, OldDigest: AnsiString; PublicKey: PEVP_PKEY;
                HashDigest: TEvpDigest = Digest_sha256): Boolean;
var
    Etype: PEVP_MD;
    DigestCtx: PEVP_MD_CTX;
    PkeyCtx: PEVP_PKEY_CTX;   { V8.64 }
    Ret, keytype: integer;
begin
    Result := false;
    if not Assigned(PublicKey) then
              Raise EDigestException.Create('Public key required');
    keytype := EVP_PKEY_base_id(PublicKey);
    if (keytype <> EVP_PKEY_RSA) and (keytype <> EVP_PKEY_EC) and
        (keytype <> EVP_PKEY_ED25519) and (keytype <> EVP_PKEY_RSA_PSS) then
              Raise EDigestException.Create('Unsupported public key type');
    DigestCtx := EVP_MD_CTX_new;
    PkeyCtx := Nil;
    try
        Etype := IcsSslGetEVPDigest(HashDigest);
        if keytype = EVP_PKEY_ED25519 then
            EType := Nil // Needs 1.1.1
        else if NOT Assigned(Etype) then
            Raise EDigestException.Create('Unsupported hash digest ' +
                           GetEnumName(TypeInfo(TEvpDigest), Ord(HashDigest)));
        Ret := EVP_DigestVerifyInit(DigestCtx, @PkeyCtx, Etype, Nil, PublicKey);
        if (Ret <= 0) then RaiseLastOpenSslError(EDigestException, FALSE,
                                    'Failed to initialise signing digest');
        ret := EVP_DigestVerify(DigestCtx, PAnsiChar(OldDigest),
                            Length(OldDigest), PAnsiChar(Data), Length(Data));
        if (Ret = 1) then
            Result := True
        else
        if (Ret = 0) then
            Result := False
        else
            RaiseLastOpenSslError(EDigestException, FALSE, 'Failed to verify signing digest');
    finally
        EVP_MD_CTX_free(DigestCtx);
    end;
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 Jose Header for Json Web Signature or Token, with Acme private fields }
{ V8.67 rewrite to create UTF-8 encoded Json properly }
function IcsJoseHeader(const Alg, Typ, Jwk, Kid, Nonce: string;
                                                const Url: string = ''): AnsiString;
var
    JsonResult: TRestParams;
begin
    JsonResult := TRestParams.Create(Nil);
    JsonResult.PContent := PContJson;
    JsonResult.AddItem(JoseParams_alg, Alg, RPTypeStr);
    if Typ <> '' then
        JsonResult.AddItem(JoseParams_typ, Typ, RPTypeStr);
    if Jwk <> '' then
        JsonResult.AddItem(JoseParams_jwk, Jwk, RPTypeObj);  { already Json }
    if Kid <> '' then   { V8.65 }
        JsonResult.AddItem(JoseParams_kid, Kid, RPTypeStr);
    if Nonce <> '' then
        JsonResult.AddItem(JoseParams_nonce, Nonce, RPTypeStr);
    if Url <> '' then
        JsonResult.AddItem(JoseParams_url, Url, RPTypeStr);
    Result := JsonResult.GetParameters;
    JsonResult.Free;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7517 Jose JSON Web Key (JWK) with Hmac shared secret key }
{ V8.67 rewrite to create Json properly }
function IcsJoseJWKHmac(const Secret, Alg: String;
                  const Kid: String = ''; const Use: String = ''): String;
var
    Klen: Integer;
    JsonResult: TRestParams;
begin
    if Pos ('HS', Alg) <> 1 then
             Raise EDigestException.Create('Need HSxxx Alg for HMAC secret');
  // secret key should be minimum length of hash bits, ie 32, 48 or 64 chars
    Klen := 0;
    if Alg = JoseLitalg_HS256 then
        Klen := 32
    else if Alg = JoseLitalg_HS384 then
        Klen := 48
     else if Alg = JoseLitalg_HS512 then
        Klen := 64;
    if Klen < Length(Secret) then
             Raise EDigestException.Create('Need Longer HMAC secret');
    JsonResult := TRestParams.Create(Nil);
    JsonResult.PContent := PContJson;
    JsonResult.AddItem(JoseParamk_kty, JoseLitkty_oct, RPTypeStr);   { octet sequence }
    JsonResult.AddItem(JoseParamko_k, IcsBase64UrlEncode(Secret));
    JsonResult.AddItem(JoseParamk_alg, Alg, RPTypeStr);
    if Kid <> '' then
        JsonResult.AddItem(JoseParamk_kid, Kid, RPTypeStr);
    if Use <> '' then
        JsonResult.AddItem(JoseParamk_use, Use, RPTypeStr);
    Result := String(JsonResult.GetParameters);
    JsonResult.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7517 get Hmac shared key from Jose JSON Web Key (JWK) to verify JWS }

function IcsJoseJWKGetHmac(JWK: String): String;    { V8.65 }
var
    JwkJson: ISuperObject;
begin
    JwkJson := SO(JWK);
    if NOT Assigned(JwkJson) then
          Raise EDigestException.Create('JWK not valid Json');
    if JwkJson.S[JoseParamk_kty] = JoseLitkty_oct then begin
        Result := IcsBase64UrlDecode(JwkJson.S[JoseParamko_k]);
    end
    else
        Raise EDigestException.Create('JWK does not contain public key');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7517 Jose JSON Web Key (JWK) with RSA, EC or Ed25519 public key }
{ V8.65 optionally export private key fields }
{ RFC7638 JWK Thumbprint which specifies public members must be in alphabetic
  order and no optional fields }
{ V8.67 rewrite to create Json properly }
{ V8.67 replaced all low level RSA and EC functions with EVP_PKEY_get_x_param functions
  for OpenSSL 3.0 }
function IcsJoseJWKPubKey(PrivateKey: PEVP_PKEY; const Alg: String;  const
        Kid: String = ''; const Use: String = ''; Priv: Boolean = False): String;  { V8.65 added Private }
var
    MyRSA: PRSA;
    eckey: PEC_KEY;
    ecgroup: PEC_GROUP;
    KeyType, Nid, PBuffLen: Integer;
    KeyLen, BufSize: size_t;   { V8.66 }
    JCurve: String;
    big1, big2, big3: PBIGNUM;
    Buff, CurveNid: AnsiString;
    PBuff: PAnsiString;
    JsonResult: TRestParams;
begin
    Result := '';
    if ICS_OPENSSL_VERSION_NUMBER = 0 then IcsLoadSsl;
    if (not Assigned(PrivateKey)) then
                  Raise EDigestException.Create('Private key required');
    JsonResult := TRestParams.Create(Nil);
    JsonResult.PContent := PContJson;
    BufSize := 512;
    try
        KeyType := EVP_PKEY_base_id(PrivateKey);
        if (keytype = EVP_PKEY_RSA) or (keytype = EVP_PKEY_RSA_PSS) then begin
            if (Alg <> '') and (Pos ('RS', Alg) <> 1) and (Pos ('PS', Alg) <> 1) then
                            Raise EDigestException.Create('Need RSxxx Alg for RSA key');

          { V8.64 RSA functions need 1.1.1e to work with RSA-PSS }
            KeyLen := EVP_PKEY_size(PrivateKey) * 8;    { V8.67 }
            if KeyLen <= 0 then
                 Raise EDigestException.Create('Failed to read RSA key');

          { V8.67 low level stuff accessed by provider for OpenSSL 3.0 and later }
            if ICS_OPENSSL_VERSION_MAJOR >= 3 then begin
                JsonResult.AddItemA(JoseParamkr_e, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_E));
                JsonResult.AddItem(JoseParamk_kty, JoseLitkty_RSA);
                JsonResult.AddItemA(JoseParamkr_n, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_N));

              // output private parameters, don't care about fingerprint order
                if Priv then begin
                    JsonResult.AddItemA(JoseParamkr_d, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_D));
                    JsonResult.AddItemA(JoseParamkr_p, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_FACTOR1));
                    JsonResult.AddItemA(JoseParamkr_q, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_FACTOR2));
                    JsonResult.AddItemA(JoseParamkr_dp, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_EXPONENT1));
                    JsonResult.AddItemA(JoseParamkr_dq, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_EXPONENT2));
                    JsonResult.AddItemA(JoseParamkr_qi, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_RSA_COEFFICIENT1));
                end;
            end
            else begin  // OpenSSL 1.1.1
                MyRSA := EVP_PKEY_get1_RSA(PrivateKey);
                if NOT Assigned(myRSA) then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to read RSA key');
                try
                    RSA_get0_key(MyRSA, @big1, @big2, @big3);               { V8.66, don't free bigs }

                 // no spaces, alpha order, or thumbnail fails
                    JsonResult.AddItemA(JoseParamkr_e, IcsBase64UrlEncodeA(IcsGetBinNum(big2)));
                    JsonResult.AddItem(JoseParamk_kty, JoseLitkty_RSA, RPTypeStr);
                    JsonResult.AddItemA(JoseParamkr_n, IcsBase64UrlEncodeA(IcsGetBinNum(big1)));

                  // output private parameters, don't care about fingerprint order
                    if Priv then begin
                        JsonResult.AddItemA(JoseParamkr_d, IcsBase64UrlEncodeA(IcsGetBinNum(big3)));
                        RSA_get0_factors(MyRSA, @big1, @big2);
                        JsonResult.AddItemA(JoseParamkr_p, IcsBase64UrlEncodeA(IcsGetBinNum(big1)));
                        JsonResult.AddItemA(JoseParamkr_q, IcsBase64UrlEncodeA(IcsGetBinNum(big2)));
                        RSA_get0_crt_params(MyRSA, @big1, @big2, @big3);
                        JsonResult.AddItemA(JoseParamkr_dp, IcsBase64UrlEncodeA(IcsGetBinNum(big1)));
                        JsonResult.AddItemA(JoseParamkr_dq, IcsBase64UrlEncodeA(IcsGetBinNum(big2)));
                        JsonResult.AddItemA(JoseParamkr_qi, IcsBase64UrlEncodeA(IcsGetBinNum(big3)));
                    end;
                finally
                    RSA_free(MyRSA);
                end;
            end;
        end
        else if (keytype = EVP_PKEY_EC) then begin
            if (Alg <> '') and (Pos ('ES', Alg) <> 1) then
                     Raise EDigestException.Create('Need ESxxx Alg for ECDSA key');

          { V8.67 low level stuff accessed by provider for OpenSSL 3.0 and later }
            if ICS_OPENSSL_VERSION_MAJOR >= 3 then begin
                JCurve := JoseLitcrv_P256;
                CurveNid := IcsEvpGetParamUtf8(PrivateKey, OSSL_PKEY_PARAM_GROUP_NAME);
                if CurveNid = 'prime256v1' then      // aka secp256r1
                    JCurve := JoseLitcrv_P256
                else if CurveNid = 'secp256k1' then   // kobitz version, probably not for JWS
                    JCurve := JoseLitcrv_P256
                else if CurveNid = 'secp384r1' then
                    JCurve := JoseLitcrv_P384
                else if CurveNid = 'secp521r1' then
                    JCurve := JoseLitcrv_P521;
                JsonResult.AddItem(JoseParamke_crv, JCurve, RPTypeStr);
                JsonResult.AddItem(JoseParamk_kty, JoseLitkty_EC, RPTypeStr);
                JsonResult.AddItemA(JoseParamke_x, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_EC_PUB_X));
                JsonResult.AddItemA(JoseParamke_y, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_EC_PUB_Y));
           //     JsonResult.AddItemA('pub', IcsEvpGetParamBuff(PrivateKey, OSSL_PKEY_PARAM_PUB_KEY));            // TEMP DIAG
                if Priv then begin
                   JsonResult.AddItemA(JoseParamke_d, IcsEvpGetParamBig(PrivateKey, OSSL_PKEY_PARAM_PRIV_KEY));
                end;
            end
            else begin
                eckey := EVP_PKEY_get1_EC_KEY(PrivateKey);
                if eckey = nil then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to read ECDSA key');
                ecgroup := EC_KEY_get0_group(eckey);
                try
                    if Assigned (ecgroup) then begin
                        Nid := EC_GROUP_get_curve_name(ecgroup);
                        case Nid of
                            NID_X9_62_prime256v1: JCurve := JoseLitcrv_P256;   // aka secp256r1
                            NID_secp256k1: JCurve := JoseLitcrv_P256;
                            NID_secp384r1: JCurve := JoseLitcrv_P384;
                            NID_secp521r1: JCurve := JoseLitcrv_P521;
                            else
                                JCurve := JoseLitcrv_P256;
                        end;

                     { V8.65 get x/y from raw buffer rather than affine_coordinates }
                        PBuffLen := EC_KEY_key2buf(eckey, EC_KEY_get_conv_form(eckey), @PBuff, Nil);
                        if PBuffLen = 0 then
                             Raise EDigestException.Create('Failed to read EC key data');
                        SetLength(Buff, PBuffLen);
                        Move(PBuff^, Buff[1], PBuffLen);
                        OPENSSL_free(PBuff);
                        KeyLen := (PBuffLen - 1) div 2;
                        JsonResult.AddItem(JoseParamke_crv, JCurve, RPTypeStr);
                        JsonResult.AddItem(JoseParamk_kty, JoseLitkty_EC, RPTypeStr);
                        JsonResult.AddItemA(JoseParamke_x, IcsBase64UrlEncodeA(Copy(Buff, 2, KeyLen)));
                        JsonResult.AddItemA(JoseParamke_y, IcsBase64UrlEncodeA(Copy(Buff, Keylen + 2, KeyLen)));

                        if Priv then begin
                            PBuffLen := EC_KEY_priv2buf(eckey, @PBuff);
                            if PBuffLen > 0 then begin
                                SetLength(Buff, PBuffLen);
                                Move(PBuff^, Buff[1], PBuffLen);
                                OPENSSL_free(PBuff);
                                JsonResult.AddItemA(JoseParamke_d, IcsBase64UrlEncodeA(Buff));
                            end;
                        end;
                    end;
                finally
                    EC_KEY_free(eckey);    { don't free group, corrupts PrivateKey }
                end;
            end;
        end
        else if (keytype = EVP_PKEY_ED25519) then begin // different type of EC, 1.1.1 and later
            if (Alg <> '') and (Alg <> 'EdDSA') then
                     Raise EDigestException.Create('Need EdDSA Alg for Ed25519 key');
            SetLength(Buff, BufSize);
            FillChar(Buff[1], BufSize, #0);
            KeyLen := BufSize;
            if EVP_PKEY_get_raw_public_key(PrivateKey, @Buff[1], @KeyLen) = 0 then   { V8.64 correct function }
               IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to read Ed25519 public key');
            SetLength(Buff, KeyLen);
            JsonResult.AddItem(JoseParamke_crv, JoseLitcrv_Ed25519, RPTypeStr);    { V8.65 wrong order }
            JsonResult.AddItem(JoseParamk_kty, JoseLitkty_OKP, RPTypeStr);   // Ocktet string key pairs
            JsonResult.AddItemA(JoseParamko_x, IcsBase64UrlEncodeA(Buff));      { V8.64 }
            if Priv then begin
                SetLength(Buff, BufSize);
                FillChar(Buff[1], BufSize, #0);
                KeyLen := BufSize;
                if EVP_PKEY_get_raw_private_key(PrivateKey, @Buff[1], @KeyLen) = 0 then   { V8.64 correct function }
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to read Ed25519 private key');
                if KeyLen > 0 then begin
                    SetLength(Buff, KeyLen);
                    JsonResult.AddItemA(JoseParamko_d, IcsBase64UrlEncodeA(Buff));
                end;
            end;
        end
        else begin
            Result := '';
            Exit;
        end;

     // add other optional elements
        if Alg <> '' then
            JsonResult.AddItem(JoseParamk_alg, Alg, RPTypeStr);
        if Kid <> '' then
            JsonResult.AddItem(JoseParamk_kid, Kid, RPTypeStr);
        if Use <> '' then
            JsonResult.AddItem(JoseParamk_use, Use, RPTypeStr);
        Result := String(JsonResult.GetParameters);
    finally
        JsonResult.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7638 get JWK Thumbprint, Json members must be in alphabetic order with no
spaces or leading zero octets.  Only valid for public keys, only required members
allowed, kty, crv, x, y, e, e, k per key type. }

{ As noted in [RFC7518] any prepended zero octets in the fields of a JWK object
   MUST be stripped before doing the computation. }

function IcsJoseJWKThumbprint(JWK: String; HashDigest: TEvpDigest = Digest_sha256): String;    { V8.65 }
begin
    Result := String(IcsBase64UrlEncodeA(IcsHashDigest(AnsiString(JWK), HashDigest)));   { V8.67 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7517 get public and private key from Jose JSON Web Key (JWK) to verify JWS }
{ may return Kid which is used to find this key later }

function IcsJoseJWKGetPKey(JWK: String; var Kid: String): PEVP_PKEY;    { V8.65 }
var
    JwkJson: ISuperObject;
    rsakey: PRSA;
    eckey: PEC_KEY;
    pubox, prvod, JCurve, Alg: String;
    Nid, keytype, ret, ctype, Keylen: Integer;
    Buff, CurveNid, KeyBin, binpubx, binpuby: AnsiString;
    pctx: PEVP_PKEY_CTX;
    ParamArray: POSSL_PARAM;
    BuildParams: POSSL_PARAM_BLD;
    bnpube, bnpubn: PBIGNUM;
    bnprvd, bnprvp, bnprvq, bnprvdp, bnprvdq, bnprvqi: PBIGNUM;

    function BNfromBase64(const S: String): PBIGNUM;
    var
        Raw: AnsiString;
    begin
        if S = '' then
            Result := Nil
        else begin
            Raw := IcsBase64UrlDecodeA(AnsiString(S));
            Result := BN_bin2bn(@Raw[1], Length(Raw), nil);
        end;
    end;

    function BNfromBin(const S: AnsiString): PBIGNUM;
    begin
        if S = '' then
            Result := Nil
        else Result := BN_bin2bn(@S[1], Length(S), nil);
    end;

    function BinFromBase64(const S: String): AnsiString;
    begin
        if S = '' then
            Result := ''
        else
            Result := IcsBase64UrlDecodeA(AnsiString(S));
    end;

begin
    Result := Nil;
    if ICS_OPENSSL_VERSION_NUMBER = 0 then IcsLoadSsl;
    JwkJson := SO(JWK);
    if NOT Assigned(JwkJson) then
          Raise EDigestException.Create('JWK not valid Json');
    Alg := JwkJson.S[JoseParamk_alg];  // beware RSA-PSS needs algorithm to be detected
    Kid := JwkJson.S[JoseParamk_kid];
    if JwkJson.S[JoseParamk_kty] = JoseLitkty_RSA then begin
        bnpube := BnFromBase64(JwkJson.S[JoseParamkr_e]);
        bnpubn := BnFromBase64(JwkJson.S[JoseParamkr_n]);
        bnprvd := BnFromBase64(JwkJson.S[JoseParamkr_d]);  // bnprv will be nil if not found
        bnprvp := BnFromBase64(JwkJson.S[JoseParamkr_p]);
        bnprvq := BnFromBase64(JwkJson.S[JoseParamkr_q]);
        bnprvdp := BnFromBase64(JwkJson.S[JoseParamkr_dp]);
        bnprvdq := BnFromBase64(JwkJson.S[JoseParamkr_dq]);
        bnprvqi := BnFromBase64(JwkJson.S[JoseParamkr_qi]);
        if (NOT Assigned (bnpube)) or (NOT Assigned(bnpubn)) then
            Raise EDigestException.Create('Failed to read JWK key values');

      { V8.67 low level stuff accessed by provider for OpenSSL 3.0 and later }
        if ICS_OPENSSL_VERSION_MAJOR >= 3 then begin
            BuildParams := OSSL_PARAM_BLD_new;
            ctype := EVP_PKEY_PUBLIC_KEY;
            OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_E, bnpube);
            OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_N, bnpubn);
            if Assigned(bnprvd) then begin  // got part of the private key
                OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_D, bnprvd);
                ctype := EVP_PKEY_KEYPAIR;
                if Assigned(bnprvp) and Assigned(bnprvq) then begin
                    OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_FACTOR1, bnprvp);
                    OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_FACTOR2, bnprvq);
                end;
                if Assigned(bnprvdp) and Assigned(bnprvdq) and Assigned(bnprvqi) then begin
                    OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_EXPONENT1, bnprvdp);
                    OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_EXPONENT2, bnprvdq);
                    OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_RSA_COEFFICIENT1, bnprvqi);
                end;
            end;
            ParamArray := OSSL_PARAM_BLD_to_param(BuildParams);
            OSSL_PARAM_BLD_free(BuildParams);
            if NOT Assigned(ParamArray) then
                Raise EDigestException.Create('Failed to build RSA key parameter array');
         //   Alg := IcsParamPrint(ParamArray); // !!! TEMP print our new array
            pctx := EVP_PKEY_CTX_new_from_name(Nil, 'RSA', Nil);
       //     pctx := EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, Nil);
            if (pctx = Nil) then
                IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create RSA context');
            try
                if EVP_PKEY_fromdata_init(pctx) <> 1 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to init RSA parameters');
                ret := EVP_PKEY_fromdata(pctx, @Result, ctype, ParamArray);
                if ret = -2 then
                    Raise EDigestException.Create('Parameters not supported for RSA keys')
                else if ret < 1 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set RSA params');
            finally
                OSSL_PARAM_free(ParamArray);
                EVP_PKEY_CTX_free(pctx);
            end;
        end
        else begin
            rsakey := RSA_new;
            try
                if RSA_set0_key(rsakey, bnpubn, bnpube, bnprvd) = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create RSA key');

                if Assigned(bnprvp) and Assigned(bnprvq) then begin
                    if RSA_set0_factors(rsakey, bnprvp, bnprvq) = 0 then
                        IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create private RSA key');
                end;
                if Assigned(bnprvdp) and Assigned(bnprvdq) and Assigned(bnprvqi) then begin
                    if RSA_set0_crt_params(rsakey, bnprvdp, bnprvdq, bnprvqi) = 0 then
                        IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create private RSA key');
                end;
                Result := EVP_PKEY_new;
                if (Pos ('PS', Alg) = 1) then begin  // RSA-PSS key
                    ret := EVP_PKEY_assign(Result, EVP_PKEY_RSA_PSS, rsakey);
                    if ret = 1 then RSA_up_ref(rsakey);
                end
                else
                    ret := EVP_PKEY_set1_RSA(Result, rsakey);
                if ret = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set RSA key');
            finally
                RSA_free(rsakey);
            end;
        end;
        BN_free(bnpube);   { V8.67 must free pointers }
        BN_free(bnpubn);
        BN_free(bnprvd);
        BN_free(bnprvp);
        BN_free(bnprvq);
        BN_free(bnprvdp);
        BN_free(bnprvdq);
        BN_free(bnprvqi);
    end
    else if JwkJson.S[JoseParamk_kty] = JoseLitkty_EC then begin
        binpubx := BinFromBase64(JwkJson.S[JoseParamke_x]);
        binpuby := BinFromBase64(JwkJson.S[JoseParamke_y]);
        bnprvd := BnFromBase64(JwkJson.S[JoseParamke_d]);
        JCurve := JwkJson.S[JoseParamke_crv];
        if JCurve = JoseLitcrv_P256 then begin
            nid := NID_X9_62_prime256v1;   // aka NID_secp256r1
            CurveNid := 'prime256v1';
            Keylen := 32;
        end
        else if JCurve = JoseLitcrv_P384 then begin
            nid :=  NID_secp384r1;
            CurveNid := 'secp384r1';
            Keylen := 48;
        end
        else if JCurve = JoseLitcrv_P521 then begin
            nid := NID_secp521r1;
            CurveNid := 'secp521r1';
            Keylen := 66;
        end
        else
            Raise EDigestException.Create('Unknown EC Curve');
        if (binpubx = '') or (binpuby = '') then
            Raise EDigestException.Create('Failed to read JWK key values');

      { V8.67 low level stuff accessed by provider for OpenSSL 3.0 and later }
        if ICS_OPENSSL_VERSION_MAJOR >= 3 then begin

        // build public key from x and y since OSSL_PKEY_PARAM_EC_PUB_X and OSSL_PKEY_PARAM_EC_PUB_Y don't work
            SetLength(KeyBin, (keylen * 2) + 1);
            KeyBin[1] := AnsiChar(POINT_CONVERSION_UNCOMPRESSED);
            Move(binpubx[1], KeyBin[2], Keylen);
            Move(binpuby[1], KeyBin[KeyLen + 2], Keylen);

            BuildParams := OSSL_PARAM_BLD_new;
            OSSL_PARAM_BLD_push_utf8_string(BuildParams, OSSL_PKEY_PARAM_GROUP_NAME, PAnsiChar(CurveNid), Length(CurveNid));
            OSSL_PARAM_BLD_push_octet_string(BuildParams, OSSL_PKEY_PARAM_PUB_KEY, PAnsiChar(KeyBin), Length(KeyBin));
            ctype := EVP_PKEY_PUBLIC_KEY;
            if Assigned(bnprvd) then begin  // got part of the private key
                OSSL_PARAM_BLD_push_BN(BuildParams, OSSL_PKEY_PARAM_PRIV_KEY, bnprvd);
                ctype := EVP_PKEY_KEYPAIR;
            end;
            ParamArray := OSSL_PARAM_BLD_to_param(BuildParams);
            OSSL_PARAM_BLD_free(BuildParams);
            if NOT Assigned(ParamArray) then
                Raise EDigestException.Create('Failed to build EC key parameter array');
            pctx := EVP_PKEY_CTX_new_from_name(Nil, 'EC', Nil);
       //     pctx := EVP_PKEY_CTX_new_id(nid, Nil);
            if (pctx = Nil) then
                IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create RSA context');
            try
                if EVP_PKEY_fromdata_init(pctx) <> 1 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to init EC parameters');
                ret := EVP_PKEY_fromdata(pctx, @Result, ctype, ParamArray);
                if ret = -2 then
                    Raise EDigestException.Create('Parameters not supported for EC keys')
                else if ret < 1 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set EC params');

            // test some stuff
               if CurveNid <> IcsEvpGetParamUtf8(Result, OSSL_PKEY_PARAM_GROUP_NAME) then
                    Raise EDigestException.Create('Mismatch EC Curve');

            finally
                OSSL_PARAM_free(ParamArray);
                EVP_PKEY_CTX_free(pctx);
            end;
        end
        else begin
            eckey := EC_KEY_new_by_curve_name(nid);
            if NOT Assigned(eckey) then
                  IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to create new EC key');
            try
                if EC_KEY_set_public_key_affine_coordinates(eckey, BNfromBin(binpubx), BNfromBin(binpuby)) = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set EC key points');
                if Assigned(bnprvd) then begin
                    if EC_KEY_set_private_key(eckey, bnprvd) = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set EC private key');
                end;
                if EC_KEY_check_key(eckey) = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Invalid public EC key');
                Result := EVP_PKEY_new;
                if EVP_PKEY_set1_EC_KEY(Result, eckey) = 0 then
                    IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set EC key');
            finally
                EC_KEY_free(eckey);
            end;
        end;
        BN_free(bnprvd);
    end
    else if JwkJson.S[JoseParamk_kty] = JoseLitkty_OKP then begin
        pubox := JwkJson.S[JoseParamko_x];
        prvod := JwkJson.S[JoseParamko_d];
        JCurve := JwkJson.S[JoseParamko_crv];
        if JCurve = JoseLitcrv_Ed25519 then
            keytype := EVP_PKEY_ED25519
        else
            Raise EDigestException.Create('Unknown OKP Curve');
        if prvod <> '' then begin
            Buff := IcsBase64UrlDecodeA(AnsiString(prvod));
            if Length(Buff) < 20 then
                Raise EDigestException.Create('Invalid raw key');
            Result := EVP_PKEY_new_raw_private_key(keytype, Nil, @Buff[1], Length(Buff));
           // sets public key from private part
        end
        else begin
            Buff := IcsBase64UrlDecodeA(AnsiString(pubox));
            if Length(Buff) < 20 then
                Raise EDigestException.Create('Invalid raw key');
            Result := EVP_PKEY_new_raw_public_key(keytype, Nil, @Buff[1], Length(Buff));
        end;
        if NOT Assigned(Result) then
            IcsRaiseLastOpenSslError(EDigestException, FALSE, 'Failed to set Ed25519 key');
    end
    else
          Raise EDigestException.Create('JWK does not contain public key');

   if NOT Assigned(Result) then
       Raise EDigestException.Create('Failed to create key')
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 find Json Web Signature hash type }

function IcsJoseFindHash(JoseAlg: TJoseAlg): TEvpDigest;
begin
    Result := Digest_sha256;
    case JoseAlg of
        jsigHmac256, jsigRsa256, jsigEcdsa256, jsigRsaPss256: Result := Digest_sha256;
        jsigHmac384, jsigRsa384, jsigEcdsa384, jsigRsaPss384: Result := Digest_sha384;
        jsigHmac512, jsigRsa512, jsigEcdsa512, jsigRsaPss512: Result := Digest_sha512;
        jsigEdDSA: Result := Digest_none;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 find Json Web Signature type from literal }

function IcsJoseFindAlgType(Alg: String): TJoseAlg;   { V8.65 }
var
    JoseAlg: TJoseAlg;
begin
    for JoseAlg := Low(TJoseAlg) to High(TJoseAlg) do begin
        if JoseAlgLits[JoseAlg] = Alg then begin
            Result := JoseAlg;
            Exit;
        end;
    end;
    Result := jsigNone;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 find Json Web Signature algorithm literal and check private key matches it }

function IcsJoseFindAlg(JoseAlg: TJoseAlg; PrivateKey: PEVP_PKEY): string;
var
    KeyType, Nid: integer;
    eckey: PEC_KEY;
    ecgroup: PEC_GROUP;
begin
    Result := 'none';
    if ICS_OPENSSL_VERSION_NUMBER = 0 then IcsLoadSsl;
    if (JoseAlg >= jsigHmac256) and (JoseAlg <= jsigHmac512) then begin
        Result := JoseAlgLits[JoseAlg];
    end
    else if (JoseAlg >= jsigRsa256) then begin
        if not Assigned(PrivateKey) then
                  Raise EDigestException.Create('PrivateKey key required');
        KeyType := EVP_PKEY_base_id(PrivateKey);

        if (JoseAlg >= jsigRsa256) and (JoseAlg <= jsigRsa512) then begin
            if (EVP_PKEY_bits(PrivateKey) < 2048) then    { V8.64 clearer exceptions }
                   Raise EDigestException.Create('RSA private key 2,048 or longer required');
            if (keytype <> EVP_PKEY_RSA) and (keytype <> EVP_PKEY_RSA_PSS) then  { V8.64 allow PSS as well }
                   Raise EDigestException.Create('RSA private key required');
        end
        else if (JoseAlg >= jsigEcdsa256) and (JoseAlg <= jsigEcdsa512) then begin
            if (keytype <> EVP_PKEY_EC) then
                   Raise EDigestException.Create('ECDSA key required');
            eckey := EVP_PKEY_get1_EC_KEY(PrivateKey);
            if eckey = nil then
                 Raise EDigestException.Create('Failed to read ECDSA key');
            Nid := NID_X9_62_prime256v1;  // or NID_secp256k1
            try
                ecgroup := EC_KEY_get0_group(eckey);
                if Assigned (ecgroup) then Nid := EC_GROUP_get_curve_name(ecgroup);
            finally
                EC_KEY_free(eckey);
            end;
            case JoseAlg of
                jsigEcdsa256: begin
                    if (Nid <> NID_X9_62_prime256v1) and (Nid <> NID_secp256k1) then   // V8.67 alternates
                        Raise EDigestException.Create('P-256 ECDSA key required for EC256');
                end;
                jsigEcdsa384: begin
                    if Nid <> NID_secp384r1 then
                        Raise EDigestException.Create('P-384 ECDSA key required for EC384');
                end;
                jsigEcdsa512: begin
                    if Nid <> NID_secp521r1 then
                        Raise EDigestException.Create('P-512 ECDSA key required for EC512');
                end;
            end;

        end
        else if (JoseAlg >= jsigRsaPss256) and (JoseAlg <= jsigRsaPss512) then begin
            if (EVP_PKEY_bits(PrivateKey) < 2048) then   { V8.64 clearer exceptions }
                   Raise EDigestException.Create('RSA-PSS private key 2,048 or longer required');
            if (keytype <> EVP_PKEY_RSA_PSS) then  { V8.64 }
                   Raise EDigestException.Create('RSA-PSS private key required');
        end
        else if (JoseAlg = jsigEdDSA) then begin
            if (keytype <> EVP_PKEY_ED25519) then  // different type of EC, 1.1.1 and later
                Raise EDigestException.Create('EdDSA private key required');
        end;
        Result := JoseAlgLits[JoseAlg];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 get JWS signature from protected header and payload }
{ returns base64 } 
function IcsJoseGetSig(JoseAlg: TJoseAlg; const CombinedEn, HmacSecret: AnsiString;
                                                   PrivateKey: PEVP_PKEY): String;
var
    HashDigest: TEvpDigest;
begin
    Result := '';
    HashDigest := IcsJoseFindHash(JoseAlg);
    if ((JoseAlg >= jsigHmac256) and (JoseAlg <= jsigHmac512)) then begin
        if HmacSecret = '' then
              Raise EDigestException.Create('HMAC secret key required');
        Result := String(IcsBase64UrlEncodeA(IcsHMACDigestEx(CombinedEn, HmacSecret, HashDigest)));
    end
    else if (JoseAlg >= jsigRsa256) then begin
        Result := String(IcsBase64UrlEncodeA(IcsAsymSignDigest(CombinedEn, PrivateKey, HashDigest)));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 check JWS signature agsinst protected header and payload }

function IcsJoseCheckSig(JoseAlg: TJoseAlg; const CombinedEn, SignatureEn,
                              HmacSecret: AnsiString; PublicKey: PEVP_PKEY): Boolean;  { V8.65 }
var
    HashDigest: TEvpDigest;
begin
    Result := False;
    HashDigest := IcsJoseFindHash(JoseAlg);
    if ((JoseAlg >= jsigHmac256) and (JoseAlg <= jsigHmac512)) then begin
        if HmacSecret = '' then
              Raise EDigestException.Create('HMAC secret key required');
        Result := IcsHMACDigestVerify(CombinedEn, HmacSecret, SignatureEn, HashDigest);
    end
    else if (JoseAlg >= jsigRsa256) then begin
        Result := IcsAsymVerifyDigest(CombinedEn, SignatureEn, PublicKey, HashDigest);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 build Json Web Signature (JWS) or Json Web Token (JWT), }
{ with Acme private fields, using JWS Compact Serialization which is three }
{ base64url blocks separated by periods, ie xxx.xxx.xxx }

function IcsJoseJWSComp(JoseAlg: TJoseAlg; const Payload, HmacSecret: string;
          PrivateKey: PEVP_PKEY; const Typ, Jwk, Kid, Nonce: string;
                                                 const Url: string = ''): string;
var
    Alg: String;
    ProtectedEn, PayloadEn, SignatureEn, CombinedEn: AnsiString;  { V8.67 }
begin
    Result := '';

  // not allowed Jwk and Kid together
    if (Jwk <> '') and (Kid <> '') then Exit;

  // find algorithm and hash, checking private key matches
    Alg := IcsJoseFindAlg(JoseAlg, PrivateKey);

  // build Json header and base64url encode it, V8.67 IcsJoseHeader now does UTF-8 }
    ProtectedEn := IcsBase64UrlEncodeA(IcsJoseHeader(Alg, Typ, Jwk, Kid, Nonce, Url));

  // base64url encode payload, which may be json
    PayloadEn := IcsBase64UrlEncodeA(StringToUtf8(Payload));

  // combine header and payload and get signature
    CombinedEn := ProtectedEn + '.' + PayloadEn;
    SignatureEn := AnsiString(IcsJoseGetSig(JoseAlg, CombinedEn, AnsiString(HmacSecret), PrivateKey));

 // combine all three together with periods
    Result := String(ProtectedEn + '.' + PayloadEn + '.' + SignatureEn);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* Acme V2 - draft 10
 new account sends jwk
 {
     "protected": base64url({
       "alg": "ES256",
       "jwk": {...},
       "nonce": "6S8IqOGY7eL2lsGoTZYifg",
       "url": "https://example.com/acme/new-account"
     }),
     "payload": base64url(xxx),
     "signature": "RZPOnYoPs1PhjszF...-nh6X1qtOFPB519I"
 }

 subsequent requests send kid (from HTTP Location: response to new account
 {
     "protected": base64url({
       "alg": "ES256",
       "kid": /* account key */,
       "nonce": "ax5RnthDqp_Yf4_HZnFLmA",
       "url": "https://example.com/acme/acct/1"
     }),
     "payload":base64url(xxx),
     "signature": "hDXzvcj8T6fbFbmn...rDzXzzvzpRy64N0o"
   }
*)


{ RFC7515 build Json Web Signature or Token, with Acme private fields, }
{ using JWS JSON Serialization which is three Json blocks }
{ used by Let's Encrypt ACME v2 }
{ note return is UTF-8 Json, but all base64 encoded }
function IcsJoseJWSJson(JoseAlg: TJoseAlg; const Payload, HmacSecret: string;
          PrivateKey: PEVP_PKEY; const Typ, Jwk, Kid, Nonce: string;
                                                 const Url: string = ''): string;
var
    Alg: String;
    ProtectedEn, PayloadEn, SignatureEn, CombinedEn: AnsiString;
    JsonResult: TRestParams;
begin
    Result := '';

  // not allowed Jwk and Kid together
    if (Jwk <> '') and (Kid <> '') then Exit;

  // find algorithm and hash, checking private key matches
    Alg := IcsJoseFindAlg(JoseAlg, PrivateKey);

  // build Json header and base64url encode it, V8.67 IcsJoseHeader now does UTF-8
    ProtectedEn := IcsBase64UrlEncodeA(IcsJoseHeader(Alg, Typ, Jwk, Kid, Nonce, Url));

  // base64url encode payload, which may be json
    PayloadEn := IcsBase64UrlEncodeA(StringToUtf8(Payload));

  // combine header and payload and get signature
    CombinedEn := ProtectedEn + '.' + PayloadEn;
    SignatureEn := AnsiString(IcsJoseGetSig(JoseAlg, CombinedEn, AnsiString(HmacSecret), PrivateKey));

 // combine all three base64 strings together as Json
    JsonResult := TRestParams.Create(Nil);  { V8.67 properly }
    JsonResult.PContent := PContJson;
    JsonResult.AddItemA(JsonSerialprotected, ProtectedEn);
    JsonResult.AddItemA(JsonSerialpayload, PayloadEn);
    JsonResult.AddItemA(JsonSerialsignature, SignatureEn);
    Result := String(JsonResult.GetParameters);
    JsonResult.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ build Json Web Token (JWT) by Base64Url encoding three components as long string V8.62 }
function IcsJoseJWT(const Header, Payload, Signature: string): string;
begin
    Result := String(IcsBase64UrlEncodeA(StringToUtf8(Header)) + AnsiChar('.') +
                      IcsBase64UrlEncodeA(StringToUtf8(Payload)) + AnsiChar('.') +
                             IcsBase64UrlEncodeA(StringToUtf8(Signature)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ RFC7515 verify JWS or JWT from Json or Compact Serialization formats and get payload }
{ see description at top of this unit for usage information }

function IcsJoseCheckJWS(const JWS, OldNonce, HmacSecret: String;
           var NewPublicKey: PEVP_PKEY; var NewKeyid, Payload: String): TJoseVerify;   { V8.65 }
var
    I, J: Integer;
    ProtectedEn, PayloadEn, SignatureEn: AnsiString;
    JwsJson, HdrJson: ISuperObject;
    Alg, Jwk, Kid, Nonce{, Typ, Use, Url}: String;
    JoseAlg: TJoseAlg;
    CombinedEn: AnsiString;

    function GetBlock(const S: AnsiString): String;
    begin
        Result := Utf8ToStringW(IcsBase64UrlDecodeA(S));
    end;

begin
    Result := JVerifyFailNoJWS;
    Payload := '';
    try

     // Json block
        if Pos ('{', JWS) = 1 then begin
        JwsJson := SO(JWS);
            if NOT Assigned(JwsJson) then Exit;
             ProtectedEn := AnsiString(JwsJson.S[JsonSerialprotected]);
             if ProtectedEn = '' then ProtectedEn := AnsiString(JwsJson.S[JsonSerialheader]);
             PayloadEn := AnsiString(JwsJson.S[JsonSerialpayload]);
             SignatureEn := AnsiString(JwsJson.S[JsonSerialsignature]);
        end

     // check for compact serialisation
        else begin
            I := IcsPosEx('.', JWS, 1);
            if I <= 2 then Exit;
            ProtectedEn := AnsiString(Copy(JWS, 1, I - 1));
            J := PosEx('.', JWS, I + 1);
            if J <= I then Exit;
            PayloadEn := AnsiString(Copy(JWS, I + 1, J - I - 1));
            SignatureEn := AnsiString(Copy(JWS, J + 1, 9999));
        end;

     // signature is against base64 encoded data
        CombinedEn := AnsiString(ProtectedEn + '.' + PayloadEn);

     // get fields from protected header
        Result := JVerifyFailBadHeader;
        HdrJson := SO(GetBlock(ProtectedEn));
        if NOT Assigned(HdrJson) then Exit;
        Alg := HdrJson.S[JoseParams_alg];
        Jwk := HdrJson.S[JoseParams_jwk];
        Kid := HdrJson.S[JoseParams_kid];
        Nonce := HdrJson.S[JoseParams_nonce];
    //  Typ := HdrJson.S[JoseParams_typ];   // do we care, should be jws
    //  Use := HdrJson.S[JoseParams_use];   // do we care, should be sig
    //  Url := HdrJson.S[JoseParams_url];   // might be URL of public key

    // see if got a JWK or KID which means used existing public key
    // should never have KWK and KID together
        JoseAlg := IcsJoseFindAlgType(Alg);
        if (JoseAlg >= jsigRsa256) then begin

          // we have a new publio key, application should save it
            if Jwk <> '' then begin
                NewPublicKey := IcsJoseJWKGetPKey(Jwk, NewKeyId);
                Result := JVerifyFailBadPubKey;
                if NOT Assigned(NewPublicKey) then Exit;
            end

          // KeyId see if we have marching Id and public key else request it
            else if Kid <> '' then begin
                if (NOT Assigned(NewPublicKey)) or (NewKeyId <> kid) then begin
                    Result := JVerifyNeedPubKey;  //then call this function again!!!
                    NewKeyId := kid;
                    Exit;
                end;
            end
            else begin
                Exit;
            end;
        end
        else if (JoseAlg >= jsigHmac256) then begin
            Result := JVerifyNeedSecret;
            if HmacSecret = '' then Exit;  // get secret and call this function again
        end
        else begin
            Result := JVerifyFailNoAlg;
            Exit;  // no algorithm is fatal
        end;

    // check nonce matches what we expect, if anything
         Result := JVerifyFailNonce;
        if (OldNonce <> '') and (nonce <> OldNonce) then Exit;

    // finally verify signature
        if IcsJoseCheckSig(JoseAlg, CombinedEn, IcsBase64UrlDecodeA(SignatureEn),
                                                 AnsiString(HmacSecret), NewPublicKey) then begin
            if (JoseAlg >= jsigRsa256) then begin
               if Jwk <> '' then
                    Result := JVerifyOkNewPubKey
               else
                    Result := JVerifyOkOldPubKey;
            end
            else
                Result := JVerifyOkSecret;
            Payload := GetBlock(PayloadEn);
        end
        else
            Result := JVerifyFailSignature;
    except
        Result := JVerifyFailExcept;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
