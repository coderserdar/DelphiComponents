{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for LIBEAY32.DLL (OpenSSL)
              This is only the subset needed by ICS.
Creation:     Jan 12, 2003
Version:      1.05
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added. New version check,
             see comments in source. In order to disable version check uncomment
             define NO_OSSL_VERSION_CHECK below and rebuild all. New functions
             OpenSslVersion, OpenSslCompilerFlags, OpenSslBuiltOn,
             OpenSslPlatForm, OpenSslDir all return a string type.
Jan 27, 2006 A. Garrels, made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2006 A. Garrels: Added functions f_Ics_X509_get_notBefore,
             f_Ics_X509_get_notAfter, Asn1ToUTDateTime.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
May 24, 2007 A.Garrels: Added code to handle ASN1 BMPString and Utf8 string
             types.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
Jul 18, 2008 A. Garrels made some changes to get rid of some string cast
             warnings.
Jun 05, 2008 A.Garrels revised Asn1ToString(), made some string casts explicit.
Aug 19, 2008 A.Garrels checked against OpenSSL v0.9.8h and added that version
             as maximum version.
Nov 17, 2008 A.Garrels checked against OpenSSL v0.9.8i and added that version
             as maximum version.
Apr 10, 2009 A.Garrels checked against OpenSSL v0.9.8k and made it the maximum
             supported version.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

{$WARN SYMBOL_DEPRECATED OFF}
{$I OverbyteIcsDefs.inc}
{$I OverbyteIcsSslDefs.inc}

{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}

unit OverbyteIcsLIBEAY;

interface

{$IFDEF USE_SSL}

uses
    Windows, SysUtils, OverbyteIcsSSLEAY, OverbyteIcsUtils;

const
    IcsLIBEAYVersion   = 105;
    CopyRight : String = ' IcsLIBEAY (c) 2003-2009 F. Piette V1.05 ';

type
    EIcsLibeayException = class(Exception);

    TStatLockLockCallback = procedure(Mode : Integer; N : Integer; const _File : PAnsiChar; Line : Integer); cdecl;
    TStatLockIDCallback   = function : Longword; cdecl;

    TCRYPTO_dynlock_value_st = packed record
        Mutex : THandle;
    end;
    PCRYPTO_dynlock_value = ^TCRYPTO_dynlock_value_st;
    CRYPTO_dynlock_value  = TCRYPTO_dynlock_value_st;

    TDynLockCreateCallback  = function(const _file : PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
    TDynLockLockCallback    = procedure(Mode : Integer; L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;
    TDynLockDestroyCallback = procedure(L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;

const
    V_ASN1_UNIVERSAL                    = $00;
    V_ASN1_APPLICATION                  = $40;
    V_ASN1_CONTEXT_SPECIFIC             = $80;
    V_ASN1_PRIVATE                      = $c0;

    V_ASN1_CONSTRUCTED                  = $20;
    V_ASN1_PRIMITIVE_TAG                = $1f;

    V_ASN1_UNDEF                        = -1;
    V_ASN1_EOC                          = 0;
    V_ASN1_BOOLEAN                      = 1;
    V_ASN1_INTEGER                      = 2;
    V_ASN1_BIT_STRING                   = 3;
    V_ASN1_OCTET_STRING                 = 4;
    V_ASN1_NULL                         = 5;
    V_ASN1_OBJECT                       = 6;
    V_ASN1_OBJECT_DESCRIPTOR            = 7;
    V_ASN1_EXTERNAL                     = 8;
    V_ASN1_REAL                         = 9;
    V_ASN1_ENUMERATED                   = 10;
    V_ASN1_UTF8STRING                   = 12;
    V_ASN1_SEQUENCE                     = 16;
    V_ASN1_SET                          = 17;
    V_ASN1_NUMERICSTRING                = 18;
{ An ASN.1 NumericString object may represent any arbitrary string of numeric }
{ characters including the space character: 0,1,2,...,9,SPACE                 }
    V_ASN1_PRINTABLESTRING              = 19;
{ An ASN.1 PrintableString may represent any arbitrary string of  printable  }
{ characters (A,B,...,Z; a,b,...,z; 0,1,...,9; space ' () + , - . / : = ?)   }
    V_ASN1_T61STRING                    = 20;
    V_ASN1_TELETEXSTRING                = 20;  (* alias *)
    V_ASN1_VIDEOTEXSTRING               = 21;
    V_ASN1_IA5STRING                    = 22;
{ An ASN.1 IA5String object may represent any arbitrary string of ASCII   }
{ characters. The term IA5 denotes International Alphabet 5 (= ASCII).    }
    V_ASN1_UTCTIME                      = 23;
    V_ASN1_GENERALIZEDTIME              = 24;
    V_ASN1_GRAPHICSTRING                = 25;
    V_ASN1_ISO64STRING                  = 26;
    V_ASN1_VISIBLESTRING                = 26;  (* alias *)
    V_ASN1_GENERALSTRING                = 27;
{ The ASN.1 character string type GeneralString encompasses all registered }
{ graphic and character sets (see ISO 2375) plus SPACE and DELETE.         }
    V_ASN1_UNIVERSALSTRING              = 28;
{ UniversalString is defined in ASN.1:1993.                                }
    V_ASN1_BMPSTRING                    = 30;
{ BMPString is a subtype of the UniversalString type and models the Basic  }
{ Multilingual Plane of ISO/IEC/ITU 10646-1, a two-octet (USC-2) encoding  }
{ form, which is identical to Unicode 1.1.                                 }

    {ERR_NUM_ERRORS   = 10;
    ERR_TXT_MALLOCED = 1; }
    ERR_TXT_STRING   = 2;
    { Changed from 32 in v0.9.7 up }
    ERR_R_FATAL      = 64;

    // Libraries for SSLErr()
    { Currently not used, commented 12/07/05
    ERR_LIB_NONE     = 1;
    ERR_LIB_SYS      = 2;
    ERR_LIB_BN       = 3;
    ERR_LIB_RSA      = 4;
    ERR_LIB_DH       = 5;
    ERR_LIB_EVP      = 6;
    ERR_LIB_BUF      = 7;
    ERR_LIB_OBJ      = 8;
    ERR_LIB_PEM      = 9;
    ERR_LIB_DSA      = 10;
    ERR_LIB_X509     = 11;
    ERR_LIB_METH     = 12;
    ERR_LIB_ASN1     = 13;
    ERR_LIB_CONF     = 14;
    ERR_LIB_CRYPTO   = 15;
    ERR_LIB_SSL      = 20;
    ERR_LIB_SSL23    = 21;
    ERR_LIB_SSL2     = 22;
    ERR_LIB_SSL3     = 23;
    ERR_LIB_RSAREF   = 30;
    ERR_LIB_PROXY    = 31;
    ERR_LIB_BIO      = 32;
    ERR_LIB_PKCS7    = 33;
    ERR_LIB_X509V3   = 34;
    ERR_LIB_PKCS12   = 35;
    ERR_LIB_RAND     = 36;
    ERR_LIB_DSO      = 37;
    ERR_LIB_COMP     = 41;
    ERR_LIB_USER     = 128;
    }

    NID_undef                       =  0;  //AG
    NID_rsaEncryption               =  6;  //AG
    NID_commonName                  = 13;  //AG
    NID_countryName                 = 14;  //AG
    NID_localityName                = 15;  //AG
    NID_stateOrProvinceName         = 16;  //AG
    NID_organizationName            = 17;  //AG
    NID_organizationalUnitName      = 18;  //AG
    NID_pkcs9_emailAddress          = 48;  //AG

    { Asn1.h - For use with ASN1_mbstring_copy() } //AG
    MBSTRING_FLAG  = $1000;               //AG
    MBSTRING_ASC   = MBSTRING_FLAG or 1;  //AG
    MBSTRING_BMP   = MBSTRING_FLAG or 2;  //AG
    { 0.9.7 }
    MBSTRING_UNIV  : Longword = MBSTRING_FLAG or 3;
    MBSTRING_UTF8  : Longword = MBSTRING_FLAG or 4;
    (*
    { 0.9.8 they are set dynamically on load }
    MBSTRING_UNIV  = MBSTRING_FLAG or 4;  //AG
    MBSTRING_UTF8  = MBSTRING_FLAG;       //AG
    *)

    RSA_F4         = $10001;              //AG
    EVP_PKEY_RSA   = NID_rsaEncryption;   //AG
    
    { Crypto.h - params for f_SSLeay_version() }
    SSLEAY_VERSION      = 0;
    SSLEAY_OPTIONS      = 1; //no longer supported
    SSLEAY_CFLAGS       = 2;
    SSLEAY_BUILT_ON     = 3;
    SSLEAY_PLATFORM     = 4;
    SSLEAY_DIR          = 5; // since 0.9.7

    
    X509_V_OK                                           = 0;
    // illegal error (for uninitialized values, to avoid X509_V_OK): 1
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT                = 2;
    X509_V_ERR_UNABLE_TO_GET_CRL                        = 3;
    X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE         = 4;
    X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE          = 5;
    X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY       = 6;
    X509_V_ERR_CERT_SIGNATURE_FAILURE                   = 7;
    X509_V_ERR_CRL_SIGNATURE_FAILURE                    = 8;
    X509_V_ERR_CERT_NOT_YET_VALID                       = 9;
    X509_V_ERR_CERT_HAS_EXPIRED                         = 10;
    X509_V_ERR_CRL_NOT_YET_VALID                        = 11;
    X509_V_ERR_CRL_HAS_EXPIRED                          = 12;
    X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD           = 13;
    X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD            = 14;
    X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD           = 15;
    X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD           = 16;
    X509_V_ERR_OUT_OF_MEM                               = 17;
    X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT              = 18;
    X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN                = 19;
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY        = 20;
    X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE          = 21;
    X509_V_ERR_CERT_CHAIN_TOO_LONG                      = 22;
    X509_V_ERR_CERT_REVOKED                             = 23;
    X509_V_ERR_INVALID_CA                               = 24;
    X509_V_ERR_PATH_LENGTH_EXCEEDED                     = 25;
    X509_V_ERR_INVALID_PURPOSE                          = 26;
    X509_V_ERR_CERT_UNTRUSTED                           = 27;
    X509_V_ERR_CERT_REJECTED                            = 28;
    // These are 'informational' when looking for issuer cert
    X509_V_ERR_SUBJECT_ISSUER_MISMATCH                  = 29;
    X509_V_ERR_AKID_SKID_MISMATCH                       = 30;
    X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH              = 31;
    X509_V_ERR_KEYUSAGE_NO_CERTSIGN                     = 32;

    X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER	                = 33;
    X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION	            = 34;
    X509_V_ERR_KEYUSAGE_NO_CRL_SIGN	                    = 35;
    X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION	        = 36;
    X509_V_ERR_INVALID_NON_CA	                        = 37;
    X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED	            = 38;
    X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE	        = 39;
    X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED	        = 40;

    X509_V_ERR_INVALID_EXTENSION	                    = 41;
    X509_V_ERR_INVALID_POLICY_EXTENSION	                = 42;
    X509_V_ERR_NO_EXPLICIT_POLICY	                    = 43;

    // The application is not happy
    X509_V_ERR_APPLICATION_VERIFICATION                 = 50;

{$IFNDEF OPENSSL_NO_ENGINE}
const
    // engine.h //

    //* These flags are used to control combinations of algorithm (methods)
    //* by bitwise "OR"ing.
    ENGINE_METHOD_RSA                   = $0001;
    ENGINE_METHOD_DSA                   = $0002;
    ENGINE_METHOD_DH                    = $0004;
    ENGINE_METHOD_RAND                  = $0008;
    ENGINE_METHOD_ECDH                  = $0010;
    ENGINE_METHOD_ECDSA                 = $0020;
    ENGINE_METHOD_CIPHERS               = $0040;
    ENGINE_METHOD_DIGESTS               = $0080;
    ENGINE_METHOD_STORE                 = $0100;
    //* Obvious all-or-nothing cases. */
    ENGINE_METHOD_ALL                   = $FFFF;
    ENGINE_METHOD_NONE                  = $0000;

    //* Error codes for the ENGINE functions. */

    //* Function codes. */
    {
    ENGINE_F_DYNAMIC_CTRL                 = 180;
    ENGINE_F_DYNAMIC_GET_DATA_CTX         = 181;
    ENGINE_F_DYNAMIC_LOAD                 = 182;
    ENGINE_F_DYNAMIC_SET_DATA_CTX         = 183;
    ENGINE_F_ENGINE_ADD                   = 105;
    ENGINE_F_ENGINE_BY_ID                 = 106;
    ENGINE_F_ENGINE_CMD_IS_EXECUTABLE     = 170;
    ENGINE_F_ENGINE_CTRL                  = 142;
    ENGINE_F_ENGINE_CTRL_CMD              = 178;
    ENGINE_F_ENGINE_CTRL_CMD_STRING       = 171;
    ENGINE_F_ENGINE_FINISH                = 107;
    ENGINE_F_ENGINE_FREE_UTIL             = 108;
    ENGINE_F_ENGINE_GET_CIPHER            = 185;
    ENGINE_F_ENGINE_GET_DEFAULT_TYPE      = 177;
    ENGINE_F_ENGINE_GET_DIGEST            = 186;
    ENGINE_F_ENGINE_GET_NEXT              = 115;
    ENGINE_F_ENGINE_GET_PREV              = 116;
    ENGINE_F_ENGINE_INIT                  = 119;
    ENGINE_F_ENGINE_LIST_ADD              = 120;
    ENGINE_F_ENGINE_LIST_REMOVE           = 121;
    ENGINE_F_ENGINE_LOAD_PRIVATE_KEY      = 150;
    ENGINE_F_ENGINE_LOAD_PUBLIC_KEY       = 151;
    ENGINE_F_ENGINE_LOAD_SSL_CLIENT_CERT  = 192;
    ENGINE_F_ENGINE_NEW                   = 122;
    ENGINE_F_ENGINE_REMOVE                = 123;
    ENGINE_F_ENGINE_SET_DEFAULT_STRING    = 189;
    ENGINE_F_ENGINE_SET_DEFAULT_TYPE      = 126;
    ENGINE_F_ENGINE_SET_ID                = 129;
    ENGINE_F_ENGINE_SET_NAME              = 130;
    ENGINE_F_ENGINE_TABLE_REGISTER        = 184;
    ENGINE_F_ENGINE_UNLOAD_KEY            = 152;
    ENGINE_F_ENGINE_UNLOCKED_FINISH       = 191;
    ENGINE_F_ENGINE_UP_REF                = 190;
    ENGINE_F_INT_CTRL_HELPER              = 172;
    ENGINE_F_INT_ENGINE_CONFIGURE         = 188;
    ENGINE_F_INT_ENGINE_MODULE_INIT       = 187;
    ENGINE_F_LOG_MESSAGE                  = 141;
    }
    //* Reason codes. */
    {
    ENGINE_R_ALREADY_LOADED               = 100;
    ENGINE_R_ARGUMENT_IS_NOT_A_NUMBER     = 133;
    ENGINE_R_CMD_NOT_EXECUTABLE           = 134;
    ENGINE_R_COMMAND_TAKES_INPUT          = 135;
    ENGINE_R_COMMAND_TAKES_NO_INPUT       = 136;
    ENGINE_R_CONFLICTING_ENGINE_ID        = 103;
    ENGINE_R_CTRL_COMMAND_NOT_IMPLEMENTED = 119;
    ENGINE_R_DH_NOT_IMPLEMENTED           = 139;
    ENGINE_R_DSA_NOT_IMPLEMENTED          = 140;
    ENGINE_R_DSO_FAILURE                  = 104;
    ENGINE_R_DSO_NOT_FOUND                = 132;
    ENGINE_R_ENGINES_SECTION_ERROR        = 148;
    ENGINE_R_ENGINE_IS_NOT_IN_LIST        = 105;
    ENGINE_R_ENGINE_SECTION_ERROR         = 149;
    ENGINE_R_FAILED_LOADING_PRIVATE_KEY   = 128;
    ENGINE_R_FAILED_LOADING_PUBLIC_KEY    = 129;
    ENGINE_R_FINISH_FAILED                = 106;
    ENGINE_R_GET_HANDLE_FAILED            = 107;
    ENGINE_R_ID_OR_NAME_MISSING           = 108;
    ENGINE_R_INIT_FAILED                  = 109;
    ENGINE_R_INTERNAL_LIST_ERROR          = 110;
    ENGINE_R_INVALID_ARGUMENT             = 143;
    ENGINE_R_INVALID_CMD_NAME             = 137;
    ENGINE_R_INVALID_CMD_NUMBER           = 138;
    ENGINE_R_INVALID_INIT_VALUE           = 151;
    ENGINE_R_INVALID_STRING               = 150;
    ENGINE_R_NOT_INITIALISED              = 117;
    ENGINE_R_NOT_LOADED                   = 112;
    ENGINE_R_NO_CONTROL_FUNCTION          = 120;
    ENGINE_R_NO_INDEX                     = 144;
    ENGINE_R_NO_LOAD_FUNCTION             = 125;
    ENGINE_R_NO_REFERENCE                 = 130;
    ENGINE_R_NO_SUCH_ENGINE               = 116;
    ENGINE_R_NO_UNLOAD_FUNCTION           = 126;
    ENGINE_R_PROVIDE_PARAMETERS           = 113;
    ENGINE_R_RSA_NOT_IMPLEMENTED          = 141;
    ENGINE_R_UNIMPLEMENTED_CIPHER         = 146;
    ENGINE_R_UNIMPLEMENTED_DIGEST         = 147;
    ENGINE_R_VERSION_INCOMPATIBILITY      = 145;
    }
{$ENDIF}

const
    BIO_CTRL_RESET         = 1;  // opt - rewind/zero etc
    BIO_CTRL_EOF           = 2;  // opt - are we at the eof
    BIO_CTRL_INFO          = 3;  // opt - extra tit-bits
    BIO_CTRL_SET           = 4;  // man - set the 'IO' type
    BIO_CTRL_GET           = 5;  // man - get the 'IO' type
    BIO_CTRL_PUSH          = 6;  // opt - internal, used to signify change
    BIO_CTRL_POP           = 7;  // opt - internal, used to signify change
    BIO_CTRL_GET_CLOSE     = 8;  // man - set the 'close' on free
    BIO_CTRL_SET_CLOSE     = 9;  // man - set the 'close' on free
    BIO_CTRL_PENDING       = 10; // opt - is their more data buffered
    BIO_CTRL_FLUSH         = 11; // opt - 'flush' buffered output
    BIO_CTRL_DUP           = 12; // man - extra stuff for 'duped' BIO
    BIO_CTRL_WPENDING      = 13; // opt - number of bytes still to write
    BIO_CTRL_SET_CALLBACK  = 14; // opt - set callback function
    BIO_CTRL_GET_CALLBACK  = 15; // opt - set callback function
    BIO_CTRL_SET_FILENAME  = 30; // BIO_s_file special

    BIO_C_SET_CONNECT                       = 100;
    BIO_C_DO_STATE_MACHINE                  = 101;
    BIO_C_SET_NBIO                          = 102;
    BIO_C_SET_PROXY_PARAM                   = 103;
    BIO_C_SET_FD                            = 104;
    BIO_C_GET_FD                            = 105;
    BIO_C_SET_FILE_PTR                      = 106;
    BIO_C_GET_FILE_PTR                      = 107;
    BIO_C_SET_FILENAME                      = 108;
    BIO_C_SET_SSL                           = 109;
    BIO_C_GET_SSL                           = 110;
    BIO_C_SET_MD                            = 111;
    BIO_C_GET_MD                            = 112;
    BIO_C_GET_CIPHER_STATUS                 = 113;
    BIO_C_SET_BUF_MEM                       = 114;
    BIO_C_GET_BUF_MEM_PTR                   = 115;
    BIO_C_GET_BUFF_NUM_LINES                = 116;
    BIO_C_SET_BUFF_SIZE                     = 117;
    BIO_C_SET_ACCEPT                        = 118;
    BIO_C_SSL_MODE                          = 119;
    BIO_C_GET_MD_CTX                        = 120;
    BIO_C_GET_PROXY_PARAM                   = 121;
    BIO_C_SET_BUFF_READ_DATA                = 122; // data to read first
    BIO_C_GET_CONNECT                       = 123;
    BIO_C_GET_ACCEPT                        = 124;
    BIO_C_SET_SSL_RENEGOTIATE_BYTES         = 125;
    BIO_C_GET_SSL_NUM_RENEGOTIATES          = 126;
    BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT       = 127;
    BIO_C_FILE_SEEK                         = 128;
    BIO_C_GET_CIPHER_CTX                    = 129;
    BIO_C_SET_BUF_MEM_EOF_RETURN            = 130; // return end of input value
    BIO_C_SET_BIND_MODE                     = 131;
    BIO_C_GET_BIND_MODE                     = 132;
    BIO_C_FILE_TELL                         = 133;
    BIO_C_GET_SOCKS                         = 134;
    BIO_C_SET_SOCKS                         = 135;

    BIO_C_SET_WRITE_BUF_SIZE                = 136; // for BIO_s_bio
    BIO_C_GET_WRITE_BUF_SIZE                = 137;
    BIO_C_MAKE_BIO_PAIR                     = 138;
    BIO_C_DESTROY_BIO_PAIR                  = 139;
    BIO_C_GET_WRITE_GUARANTEE               = 140;
    BIO_C_GET_READ_REQUEST                  = 141;
    BIO_C_SHUTDOWN_WR                       = 142;
    BIO_C_NREAD0                            = 143;
    BIO_C_NREAD                             = 144;
    BIO_C_NWRITE0                           = 145;
    BIO_C_NWRITE                            = 146;
    BIO_C_RESET_READ_REQUEST                = 147;

    BIO_NOCLOSE                             = 0;
    BIO_CLOSE                               = 1;

const
    BIO_FLAGS_READ                          = 1;
    BIO_FLAGS_WRITE                         = 2;
    BIO_FLAGS_IO_SPECIAL                    = 4;
    BIO_FLAGS_RWS                           = (BIO_FLAGS_READ or
                                               BIO_FLAGS_WRITE or
                                               BIO_FLAGS_IO_SPECIAL);
    BIO_FLAGS_SHOULD_RETRY                  = 8;

const
    // These are passed by the BIO callback //
    BIO_CB_FREE     = $01;
    BIO_CB_READ     = $02;
    BIO_CB_WRITE    = $03;
    BIO_CB_PUTS     = $04;
    BIO_CB_GETS     = $05;
    BIO_CB_CTRL     = $06;

    // The callback is called before and after the underling operation,
    // The BIO_CB_RETURN flag indicates if it is after the call
    BIO_CB_RETURN   = $80;

const
    CRYPTO_LOCK     = 1;
    CRYPTO_UNLOCK   = 2;
    CRYPTO_READ     = 4;
    CRYPTO_WRITE    = 8;

const
    X509V3_EXT_DYNAMIC      = $1;
    X509V3_EXT_CTX_DEP      = $2;
    X509V3_EXT_MULTILINE    = $4;

{$IFNDEF OPENSSL_NO_ENGINE}
type
    TEngine_st = record
        Dummy : array [0..0] of Byte;
    end;
    PENGINE = ^TEngine_st;

    TUi_method_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_METHOD = ^TUi_method_st;

    TUi_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI = ^TUi_st;

    TUi_string_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_STRING = ^TUi_string_st;

    TPinCallBack = function(ui: PUI; uis: PUI_STRING): Integer; cdecl; //AG
{$ENDIF}

const
    f_SSLeay :                                 function: Longword; cdecl = nil; //AG
    f_SSLeay_version :                         function(t: Integer): PAnsiChar; cdecl = nil; //AG
    f_ERR_get_error_line_data :                function(const FileName: PPAnsiChar; Line: PInteger; const Data: PPAnsiChar; Flags: PInteger): Cardinal; cdecl = nil;
    f_ERR_peek_error :                         function : Cardinal; cdecl = nil;
    f_ERR_peek_last_error :                    function : Cardinal; cdecl = nil;
    f_ERR_get_error :                          function: Cardinal; cdecl = nil;
    f_ERR_error_string :                       function(Err: Cardinal; Buf: PAnsiChar): PAnsiChar; cdecl = nil;
    f_ERR_error_string_n :                     procedure(Err: Cardinal; Buf: PAnsiChar; Len: Cardinal); cdecl = nil;
    f_ERR_clear_error :                        procedure; cdecl = nil; //empties the current thread's error queue
    f_ERR_remove_state :                       procedure(ThreadID: Longword); cdecl = nil;
    f_ERR_free_strings :                       procedure; cdecl = nil; //"Brutal" (thread-unsafe) Application-global cleanup functions
    f_RAND_seed :                              procedure(Buf: Pointer; Num: Integer); cdecl = nil;
    f_BIO_new :                                function(BioMethods: PBIO_METHOD): PBIO; cdecl = nil;
    f_BIO_new_socket :                         function(Sock: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_new_fd :                             function(Fd: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_new_file :                           function(FileName: PAnsiChar; Mode: PAnsiChar): PBIO; cdecl = nil;
    f_BIO_new_mem_buf :                        function(Buf : Pointer; Len : Integer): PBIO; cdecl = nil;
    f_BIO_new_bio_pair :                       function(Bio1: PPBIO; WriteBuf1: Integer; Bio2: PPBIO; WriteBuf2: Integer): Integer; cdecl = nil;
    f_BIO_ctrl :                               function(bp: PBIO; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt; cdecl = nil;
    f_BIO_ctrl_pending :                       function(b: PBIO): Integer; cdecl = nil;
    f_BIO_ctrl_get_write_guarantee :           function(b: PBIO): Integer; cdecl = nil;
    f_BIO_ctrl_get_read_request :              function(b: PBIO): Integer; cdecl = nil;
    f_d2i_X509_bio :                           function(B: PBIO; X509: PPX509): PX509; cdecl = nil;
    f_d2i_X509 :                               function(C509: PPX509; Buf: PPAnsiChar; Len: Integer): PX509; cdecl = nil;
    f_BIO_free :                               function(B: PBIO): Integer; cdecl = nil;
    f_BIO_read :                               function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
    f_BIO_nread :                              function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nread0 :                             function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_nwrite :                             function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nwrite0 :                            function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_gets :                               function(B: PBIO; Buf: PAnsiChar; Size: Integer): Integer; cdecl = nil;
    f_BIO_puts :                               function(B: PBIO; Buf: PAnsiChar): Integer; cdecl = nil;
    f_BIO_push :                               function(B: PBIO; B_Append: PBIO): PBIO; cdecl = nil;
    f_BIO_write :                              function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
    f_BIO_s_mem :                              function : PBIO_METHOD; cdecl = nil;
    f_BIO_get_retry_BIO :                      function(B: PBIO; Reason : PInteger): PBIO; cdecl = nil;
    f_BIO_get_retry_reason :                   function(B: PBIO): Integer; cdecl = nil;
    f_CRYPTO_num_locks :                       function: Integer; cdecl = nil;
    f_CRYPTO_set_id_callback :                 procedure(CB : TStatLockIDCallback); cdecl = nil;
    f_CRYPTO_set_locking_callback :            procedure(CB : TStatLockLockCallback); cdecl = nil;
    f_CRYPTO_set_dynlock_create_callback :     procedure(CB : TDynLockCreateCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_lock_callback :       procedure(CB : TDynLockLockCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_destroy_callback :    procedure(CB : TDynLockDestroyCallBack); cdecl = nil;
    f_X509_dup :                               function(X: PX509): PX509; cdecl = nil;//AG;
    f_X509_STORE_add_cert :                    function(Store: PX509_STORE; Cert: PX509): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_get_ex_data :             function(C509: PX509_STORE_CTX; Idx: Integer): Pointer; cdecl = nil;
    f_X509_STORE_CTX_get_current_cert :        function(C509: PX509_STORE_CTX): PX509; cdecl = nil;
    f_X509_STORE_CTX_get_error :               function(C509: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_get_error_depth :         function(C509: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_get_chain :               function(C509: PX509_STORE_CTX): PStack; cdecl = nil;//AG;
    f_X509_STORE_CTX_trusted_stack :           procedure(C509: PX509_STORE_CTX; STACK_OF_X509: PSTACK); cdecl = nil;//AG;
    f_X509_STORE_add_crl :                     function(Store: PX509_STORE; CRL: PX509_CRL): Integer; cdecl = nil;//AG;
    f_X509_STORE_free :                        procedure(Store: PX509_STORE); cdecl = nil;//AG;
    f_X509_verify_cert_error_string :          function(ErrCode : Integer): PAnsiChar; cdecl = nil;
    f_X509_get_issuer_name :                   function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_get_subject_name :                  function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_get_serialNumber :                  function(Cert: PX509): PASN1_INTEGER; cdecl = nil;
    f_ASN1_INTEGER_get :                       function(Asn1_Int : PASN1_INTEGER): Integer; cdecl = nil;
    f_ASN1_STRING_print :                      function(B: PBIO; v: PASN1_STRING): integer; cdecl = nil;//AG;
    f_ASN1_item_free :                         procedure(Val: PASN1_VALUE; const It: PASN1_ITEM); cdecl = nil; //AG
    f_X509_NAME_oneline :                      function(CertName: PX509_NAME; Buf: PAnsiChar; BufSize: Integer): PAnsiChar; cdecl = nil;
    f_X509_NAME_get_text_by_NID :              function(CertName: PX509_NAME; Nid: Integer; Buf : PAnsiChar; Len : Integer): Integer; cdecl = nil;
    f_X509_NAME_get_index_by_NID:              function(CertName: PX509_NAME; Nid: Integer; LastPost: Integer): Integer; cdecl = nil; //AG
    f_ASN1_STRING_free :                       procedure(a: PASN1_STRING); cdecl = nil;//AG;
    f_X509_NAME_free :                         procedure(AName: PX509_NAME); cdecl = nil;//AG;
    f_X509_get_ext :                           function(Cert: PX509; Loc : Integer): PX509_EXTENSION; cdecl = nil;
    f_X509_get_ext_count :                     function(Cert: PX509): Integer; cdecl = nil;
    f_X509_free :                              procedure(C509: PX509); cdecl = nil;
    f_X509_CRL_free :                          procedure(CRL: PX509_CRL); cdecl = nil;//AG
    f_X509V3_EXT_get :                         function(Ext: PX509_EXTENSION): PX509V3_EXT_METHOD; cdecl = nil;
    f_X509V3_EXT_print :                       function(B: PBIO; Ext: PX509_EXTENSION; Flag: Integer; Indent: Integer):Integer; cdecl = nil;//AG;
    f_X509V3_EXT_d2i :                         function(Ext: PX509_EXTENSION): Pointer; cdecl = nil;//AG;
    f_X509V3_conf_free :                       procedure(Val: PCONF_VALUE); cdecl = nil;//AG
    f_X509_EXTENSION_get_object :              function(Ext: PX509_EXTENSION): PASN1_OBJECT; cdecl = nil;
    f_X509_EXTENSION_get_data :                function(Ext : PX509_EXTENSION): PASN1_OCTET_STRING; cdecl = nil;//AG;
    f_X509_EXTENSION_get_critical :            function(Ext: PX509_EXTENSION): Integer; cdecl = nil;//AG;
    f_X509_subject_name_hash :                 function(Cert: PX509): Cardinal; cdecl = nil;
    f_X509_print :                             function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_X509_digest :                            function(Cert: PX509; Type_: PEVP_MD; Buf: PAnsiChar; BufSize: PInteger): Integer; cdecl = nil; //AG
    f_X509_check_private_key :                 function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil; //AG
    f_EVP_sha1 :                               function: PEVP_MD; cdecl = nil;//AG
    f_EVP_PKEY_free :                          procedure(PKey: PEVP_PKEY); cdecl = nil;//AG
    f_EVP_PKEY_new :                           function: PEVP_PKEY; cdecl = nil;//AG
    f_EVP_PKEY_assign :                        function(PKey: PEVP_PKEY; Type_: Integer; Key: PAnsiChar): Integer; cdecl = nil;//AG
    f_RSA_generate_key :                       function(Num: Integer; E: Cardinal; CallBack: TRSA_genkey_cb; cb_arg: Pointer): PRSA; cdecl = nil;//AG
    f_OBJ_nid2sn :                             function(N: Integer): PAnsiChar; cdecl = nil;
    f_OBJ_obj2nid :                            function(O: PASN1_OBJECT): Integer; cdecl = nil;
    f_sk_num :                                 function(Stack: PSTACK): Integer; cdecl = nil;
    f_sk_value :                               function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;
    f_sk_new_null:                             function: PSTACK; cdecl = nil;//AG;
    { This function free()'s a stack structure.  The elements in the stack will not be freed }
    f_sk_free :                                procedure(Stack: PSTACK); cdecl = nil;//AG;
    { This function calls 'func' for each element on the stack, passing the element as the argument.  sk_free() is then called to free the 'stack' structure.}
    f_sk_pop_free :                            procedure(Stack: PSTACK; PFreeProc: Pointer); cdecl = nil;//AG;
    { Append 'data' to the stack.  0 is returned if there is a failure (due to a malloc failure), else 1 }
    f_sk_push :                                function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    { Remove the item at location 'loc' from the stack and returns it. Returns NULL if the 'loc' is out of range }
    f_sk_delete :                              function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;//AG;
    { Return and delete the last element on the stack }
    f_sk_pop :                                 function(Stack: PSTACK): PAnsiChar; cdecl = nil;//AG;
    f_sk_find :                                function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    f_sk_insert :                              function(Stack: PSTACK; Data: PAnsiChar; Index: Integer): Integer; cdecl = nil;//AG;
    f_sk_dup :                                 function(Stack: PSTACK): PSTACK; cdecl = nil;//AG;
    f_PEM_write_bio_X509 :                     function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_PEM_write_bio_X509_REQ :                 function(B: PBIO; Cert_Req: PX509_REQ) : Integer; cdecl = nil;
    f_PEM_write_bio_X509_CRL :                 function(B: PBIO; CRL: PX509_CRL) : Integer; cdecl = nil;
    f_PEM_read_bio_X509_CRL :                  function(B: PBIO; CRL: PPX509_CRL; CallBack: TPem_password_cb; UData: PAnsiChar): PX509_CRL; cdecl = nil;//AG
    f_PEM_read_bio_X509 :                      function(B: PBIO; C509: PPX509; CallBack: TPem_password_cb; UData: PAnsiChar): PX509; cdecl = nil;
    f_PEM_X509_INFO_read_bio :                 function(B: PBIO; Stack: PSTACK_OF_X509_INFO; CallBack: TPem_password_cb; UData: PAnsiChar): PSTACK_OF_X509_INFO; cdecl = nil;//AG;
    f_CRYPTO_free :                            procedure(P: Pointer); cdecl = nil;//AG
    f_X509_NAME_ENTRY_get_object :             function(Ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl = nil;//AG
    f_X509_NAME_get_entry :                    function(Name: PX509_NAME; Loc: Integer): PX509_NAME_ENTRY; cdecl = nil;//AG
    f_X509_NAME_entry_count :                  function(Name: PX509_NAME) : Integer; cdecl = nil; //AG
    f_X509_NAME_ENTRY_get_data :               function(Ne: PX509_NAME_ENTRY) : PASN1_STRING; cdecl = nil;//AG
    f_X509_set_version :                       function(Cert: PX509; Version: LongInt): Integer; cdecl = nil;//AG
    f_ASN1_STRING_to_UTF8 :                    function(POut: PPAnsiChar; PIn: PASN1_STRING) : Integer; cdecl = nil;//AG
    f_ASN1_INTEGER_set :                       function(a: PASN1_INTEGER; v: LongInt) : Integer; cdecl = nil;//AG
    f_ASN1_item_d2i :                          function(Val: PPASN1_VALUE; _In: PPAnsiChar; Len: Longword; const It: PASN1_ITEM): PASN1_VALUE; cdecl = nil;//AG;
    //ASN1_VALUE * ASN1_item_d2i(ASN1_VALUE **val, unsigned char **in, long len, const ASN1_ITEM *it);
    f_i2a_ASN1_OBJECT :                        function(B: PBIO; A: PASN1_OBJECT): Integer; cdecl = nil;//AG;
    f_X509_gmtime_adj :                        function(S: PASN1_TIME; Adj: LongInt): PASN1_TIME; cdecl = nil;//AG
    f_X509_set_pubkey :                        function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_X509_new :                               function: PX509; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_txt :             function(Name: PX509_NAME; Field: PAnsiChar; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_NID :             function(Name: PX509_NAME; Nid: Integer; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_new :                          function: PX509_NAME; cdecl = nil;//AG
    f_X509_set_issuer_name :                   function(Cert: PX509; Name: PX509_NAME): Integer; cdecl = nil;//AG
    f_X509_sign :                              function(Cert: PX509; PKey: PEVP_PKEY; const Md: PEVP_MD): Integer; cdecl = nil;//AG
    f_X509_INFO_free :                         procedure(Xi: PX509_INFO); cdecl = nil;//AG;
    f_X509_CRL_dup :                           function(CRL: PX509_CRL): PX509_CRL; cdecl = nil;//AG;
    f_X509_PKEY_free :                         procedure(PKey: PX509_PKEY); cdecl = nil;//AG;
    f_i2d_X509 :                               function(Cert: PX509; pOut: PPAnsiChar): Integer; cdecl = nil;//AG
    f_i2d_PrivateKey :                         function(A: PEVP_PKEY; PP: PPAnsiChar): Integer; cdecl = nil;//AG
    f_PEM_read_bio_PrivateKey :                function(B: PBIO; X:PPEVP_PKEY; CB: TPem_password_cb; UData: PAnsiChar): PEVP_PKEY; cdecl = nil; //AG
    f_PEM_write_bio_PrivateKey :               function(B: PBIO; X: PEVP_PKEY; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil;//AG
    f_i2d_ASN1_bytes :                         function(A : PASN1_STRING; var p: PAnsiChar; tag: Integer; xclass: Integer): Integer; cdecl = nil;//AG
    f_X509_get_pubkey :                        function(Cert: PX509): PEVP_PKEY; cdecl = nil; //AG;
    f_X509_PUBKEY_free :                       procedure(Key: PEVP_PKEY); cdecl = nil; //AG;
    {
    f_OPENSSL_add_all_algorithms_noconf :      procedure; cdecl = nil;
    f_OPENSSL_add_all_algorithms_conf :        procedure; cdecl = nil;
    f_OpenSSL_add_all_ciphers :                procedure; cdecl = nil;
    f_OpenSSL_add_all_digests :                procedure; cdecl = nil;
    f_EVP_cleanup :                            procedure; cdecl = nil;
    }
{$IFNDEF OPENSSL_NO_ENGINE}
    f_ENGINE_load_builtin_engines :            procedure; cdecl = nil; //AG;
    f_ENGINE_register_all_complete :           procedure; cdecl = nil; //AG;
    f_ENGINE_cleanup :                         procedure; cdecl = nil; //AG;
    f_ENGINE_by_id :                           function(const id: PAnsiChar): PENGINE; cdecl = nil; //AG;
    f_ENGINE_init :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_finish :                          function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_set_default :                     function(e: PENGINE; flags: Cardinal): Integer; cdecl = nil; //AG;
    f_ENGINE_ctrl_cmd_string :                 function(e: PENGINE; const cmd_name: PAnsiChar; const arg: PAnsiChar; cmd_optional: Integer): Integer; cdecl = nil; //AG;
    f_ENGINE_free :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    //* The following functions handle keys that are stored in some secondary
    //* location, handled by the engine.  The storage may be on a card or
    //* whatever. */
    f_ENGINE_load_private_key :                function(e: PENGINE; key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    f_ENGINE_load_public_key :                 function(e: PENGINE; const key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    { Since V0.98i there's also: 
    int ENGINE_load_ssl_client_cert(ENGINE *e, SSL *s,
    STACK_OF(X509_NAME) *ca_dn, X509 **pcert, EVP_PKEY **ppkey,
    STACK_OF(X509) **pother,
    UI_METHOD *ui_method, void *callback_data);
    }
    // ui.h //
    f_UI_new :                                 function: PUI; cdecl = nil; //AG;
    f_UI_new_method :                          function(const method: PUI_METHOD): PUI; cdecl = nil; //AG;
    f_UI_free :                                procedure(ui: PUI); cdecl = nil; //AG;
    f_UI_create_method :                       function(name: PAnsiChar): PUI_METHOD; cdecl = nil; //AG;
    f_UI_destroy_method :                      procedure(ui_method: PUI_METHOD); cdecl = nil; //AG;
    f_UI_set_ex_data :                         function(r: PUI; idx: Integer; arg: Pointer): Integer; cdecl = nil; //AG;
    f_UI_get_ex_data :                         function(r: PUI; idx: Integer): Pointer; cdecl = nil; //AG;
    f_UI_method_set_reader :                   function(method: PUI_METHOD; reader: TPinCallBack):Integer; cdecl = nil; //AG;
    f_UI_set_result :                          function(ui: PUI; uis: PUI_STRING; const result: PAnsiChar): Integer; cdecl = nil; //AG;
    f_UI_OpenSSL :                             function: PUI_METHOD; cdecl = nil; //AG;
 (*
    http://openssl.org/docs/crypto/engine.html
    Here we'll assume we want to load and register all ENGINE implementations
    bundled with OpenSSL, such that for any cryptographic algorithm required by
    OpenSSL - if there is an ENGINE that implements it and can be initialise, it
    should be used. The following code illustrates how this can work;

    /* Load all bundled ENGINEs into memory and make them visible */
    ENGINE_load_builtin_engines();
    /* Register all of them for every algorithm they collectively implement */
    ENGINE_register_all_complete();

    That's all that's required. Eg. the next time OpenSSL tries to set up an
    RSA key, any bundled ENGINEs that implement RSA_METHOD will be passed to
    ENGINE_init() and if any of those succeed, that ENGINE will be set as the
    default for RSA use from then on.
    *)
{$ENDIF}

function Load : Boolean;
function WhichFailedToLoad : String;
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal;
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal;
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal;
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean;
function BIO_get_flags(b: PBIO): Integer;
function BIO_should_retry(b: PBIO): Boolean;
function BIO_should_read(b: PBIO): Boolean;
function BIO_should_write(b: PBIO): Boolean;
function BIO_should_io_special(b: PBIO): Boolean;
function BIO_retry_type(b: PBIO): Integer;
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM;
function OpenSslVersion : String;
function OpenSslCompilerFlags : String;
function OpenSslBuiltOn : String;
function OpenSslPlatForm : String;
function OpenSslDir : String;
function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME; out UT: TDateTime): Boolean;
function Asn1ToString(PAsn1 : PASN1_STRING): String;
{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer;
function f_Ics_UI_get_app_data(r: PUI): Pointer;
{$ENDIF}

const
    GLIBEAY_DLL_Handle   : THandle = 0;
    GLIBEAY_DLL_Name     : String  = 'LIBEAY32.DLL';
    GLIBEAY_DLL_FileName : String  = '*NOT LOADED*';

    { Version stuff added 07/12/05                                            }
    ICS_OPENSSL_VERSION_NUMBER  : Longword  = 0;

    { MMNNFFPPS: major minor fix patch status                                 }
    { The status nibble has one of the values 0 for development, 1 to e for   }
    { betas 1 to 14, and f for release.                                       }

    { for example                                                             }
    { 0x000906000 == 0.9.6 dev                                                }
    { 0x000906023 == 0.9.6b beta 3                                            }
    { 0x00090605f == 0.9.6e release                                           }

    { Versions prior to 0.9.3 have identifiers < 0x0930. Versions between     }
    { 0.9.3 and 0.9.5 had a version identifier with this interpretation:      }
    { MMNNFFRBB major minor fix final beta/patch                              }

    { for example                                                             }
    { 0x000904100 == 0.9.4 release                                            }
    { 0x000905000 == 0.9.5 dev                                                }

    { Version 0.9.5a had an interim interpretation that is like the current   }
    { one, except the patch level got the highest bit set, to keep continuity.}
    { The number was therefore 0x0090581f.                                    }

    //OSSL_VER_0906G = $0090607f; no longer supported
    OSSL_VER_0907G = $0090707f;
    OSSL_VER_0907I = $0090709f;
    OSSL_VER_0908  = $00908000;
    OSSL_VER_0908A = $0090801f;
    OSSL_VER_0908E = $0090805f;
    OSSL_VER_0908H = $0090808f;
    OSSL_VER_0908I = $0090809f;
    OSSL_VER_0908K = $009080bf;
    // Or should we also create an dynamic array of Longword we would add only
    // tested/wanted versions?
{$IFDEF BEFORE_OSSL_098E}
    MIN_OSSL_VER   = OSSL_VER_0907G;
    MAX_OSSL_VER   = OSSL_VER_0908K;
{$ELSE}
    MIN_OSSL_VER   = OSSL_VER_0908E;
    MAX_OSSL_VER   = OSSL_VER_0908K;
{$ENDIF}
{$ENDIF} // USE_SSL
implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Load : Boolean;
var
    ErrCode : Integer;
begin
    ICS_OPENSSL_VERSION_NUMBER := 0;

    if GLIBEAY_DLL_Handle <> 0 then begin
        Result := TRUE;
        Exit;                                 // Already loaded
    end;
    GLIBEAY_DLL_Handle := LoadLibrary(PChar(GLIBEAY_DLL_Name));
    if GLIBEAY_DLL_Handle < HINSTANCE_ERROR then begin
        ErrCode            := GLIBEAY_DLL_Handle;
        GLIBEAY_DLL_Handle := 0;
        raise EIcsLIBEAYException.Create('Unable to load ' +
                                         GLIBEAY_DLL_Name +
                                         '. Error #' + IntToStr(ErrCode));
    end;
    SetLength(GLIBEAY_DLL_FileName, 256);
    SetLength(GLIBEAY_DLL_FileName, GetModuleFileName(GLIBEAY_DLL_Handle,
                 PChar(GLIBEAY_DLL_FileName), Length(GLIBEAY_DLL_FileName)));

    //This function is available in all versions so we can safely call it
    f_SSLeay := GetProcAddress(GLIBEAY_DLL_Handle, 'SSLeay');
    if @f_SSLeay = nil then begin
        Result := False;
        Exit;
    end;
    ICS_OPENSSL_VERSION_NUMBER := f_SSLeay;

    { Version Check }
{$IFNDEF NO_OSSL_VERSION_CHECK}
    if (ICS_OPENSSL_VERSION_NUMBER < MIN_OSSL_VER) or
       (ICS_OPENSSL_VERSION_NUMBER > MAX_OSSL_VER) then begin
        FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
        OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0;
        raise EIcsLibeayException.Create(
                  'Unsupported OpenSSL version (0x' +
                  IntToHex(ICS_OPENSSL_VERSION_NUMBER, 8) + ') !'#13#10 +
                  'Supported versions are 0x' +
                  IntToHex(MIN_OSSL_VER, 8) +
                  ' - 0x' + IntToHex(MAX_OSSL_VER, 8) + #13#10 +
                  'FileName: ' + GLIBEAY_DLL_FileName);
    end;
{$ENDIF}
    { Let's set some values of constants having changed in v0.9.8 }
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_0908 then begin
        SSL_CTRL_EXTRA_CHAIN_CERT    := 14;                     // Ssl.h
        SSL_CTRL_GET_SESSION_REUSED  :=  8;                     // Ssl.h
        MBSTRING_UNIV                := MBSTRING_FLAG or 4;     // Asn1.h
        MBSTRING_UTF8                := MBSTRING_FLAG;          // Asn1.h
    end
    else begin
        {SSL_CTRL_EXTRA_CHAIN_CERT    := 12;                     // Ssl.h
        SSL_CTRL_GET_SESSION_REUSED  :=  6;                     // Ssl.h
        MBSTRING_UNIV                := MBSTRING_FLAG or 3;     // Asn1.h
        MBSTRING_UTF8                := MBSTRING_FLAG or 4;     // Asn1.h }
    end;

    f_SSLeay_version                         := GetProcAddress(GLIBEAY_DLL_Handle, 'SSLeay_version');
    f_ERR_get_error_line_data                := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_get_error_line_data');
    f_ERR_peek_error                         := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_peek_error');
    f_ERR_peek_last_error                    := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_peek_last_error');
    f_ERR_get_error                          := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_get_error');
    f_ERR_error_string                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_error_string');
    f_ERR_error_string_n                     := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_error_string_n');
    f_ERR_clear_error                        := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_clear_error');
    f_ERR_remove_state                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_remove_state');
    f_ERR_free_strings                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ERR_free_strings');
    f_RAND_seed                              := GetProcAddress(GLIBEAY_DLL_Handle, 'RAND_seed');
    f_BIO_new                                := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new');
    f_BIO_new_socket                         := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new_socket');
    f_BIO_new_fd                             := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new_fd');
    f_BIO_new_file                           := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new_file');
    f_BIO_new_mem_buf                        := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new_mem_buf');
    f_BIO_new_bio_pair                       := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_new_bio_pair');
    f_BIO_ctrl                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_ctrl');
    f_BIO_ctrl_pending                       := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_ctrl_pending');
    f_BIO_ctrl_get_write_guarantee           := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_ctrl_get_write_guarantee');
    f_BIO_ctrl_get_read_request              := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_ctrl_get_read_request');
    f_d2i_X509_bio                           := GetProcAddress(GLIBEAY_DLL_Handle, 'd2i_X509_bio');
    f_d2i_X509                               := GetProcAddress(GLIBEAY_DLL_Handle, 'd2i_X509');
    f_BIO_read                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_read');
    f_BIO_nread                              := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_nread');
    f_BIO_nread0                             := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_nread0');
    f_BIO_nwrite                             := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_nwrite');
    f_BIO_nwrite0                            := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_nwrite0');
    f_BIO_write                              := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_write');
    f_BIO_free                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_free');
    f_BIO_gets                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_gets');
    f_BIO_puts                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_puts');
    f_BIO_push                               := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_push');
    f_BIO_s_mem                              := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_s_mem');
    f_BIO_get_retry_BIO                      := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_get_retry_BIO');
    f_BIO_get_retry_reason                   := GetProcAddress(GLIBEAY_DLL_Handle, 'BIO_get_retry_reason');
    f_CRYPTO_num_locks                       := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_num_locks');
    f_CRYPTO_set_locking_callback            := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_set_locking_callback');
    f_CRYPTO_set_id_callback                 := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_set_id_callback');
    f_CRYPTO_set_dynlock_create_callback     := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_set_dynlock_create_callback');
    f_CRYPTO_set_dynlock_lock_callback       := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_set_dynlock_lock_callback');
    f_CRYPTO_set_dynlock_destroy_callback    := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_set_dynlock_destroy_callback');
    f_X509_dup                               := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_dup'); //AG
    f_X509_STORE_add_cert                    := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_add_cert'); //AG
    f_X509_STORE_CTX_get_ex_data             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_get_ex_data');
    f_X509_STORE_CTX_get_current_cert        := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_get_current_cert');
    f_X509_STORE_CTX_get_error               := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_get_error');
    f_X509_STORE_CTX_get_error_depth         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_get_error_depth');
    f_X509_STORE_CTX_get_chain               := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_get_chain'); //AG
    f_X509_STORE_CTX_trusted_stack           := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_CTX_trusted_stack'); //AG
    f_X509_STORE_add_crl                     := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_add_crl'); //AG
    f_X509_STORE_free                        := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_STORE_free'); //AG
    f_X509_verify_cert_error_string          := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_verify_cert_error_string');
    f_X509_get_issuer_name                   := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_issuer_name');
    f_X509_get_subject_name                  := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_subject_name');
    f_X509_get_serialNumber                  := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_serialNumber');
    f_X509_NAME_oneline                      := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_oneline');
    f_X509_NAME_get_text_by_NID              := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_get_text_by_NID');
    f_X509_NAME_get_index_by_NID             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_get_index_by_NID'); //AG
    f_ASN1_STRING_free                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_STRING_free'); //AG
    f_X509_NAME_free                         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_free');
    f_X509_get_ext                           := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_ext');
    f_X509_get_ext_count                     := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_ext_count');
    f_X509_free                              := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_free');
    f_X509_CRL_free                          := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_CRL_free');
    f_X509V3_EXT_get                         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509V3_EXT_get');
    f_X509V3_EXT_print                       := GetProcAddress(GLIBEAY_DLL_Handle, 'X509V3_EXT_print'); //AG
    f_X509V3_EXT_d2i                         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509V3_EXT_d2i'); //AG
    f_X509V3_conf_free                       := GetProcAddress(GLIBEAY_DLL_Handle, 'X509V3_conf_free'); //AG
    f_X509_EXTENSION_get_object              := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_EXTENSION_get_object');
    f_X509_EXTENSION_get_data                := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_EXTENSION_get_data'); //AG
    f_X509_EXTENSION_get_critical            := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_EXTENSION_get_critical'); //AG
    f_X509_subject_name_hash                 := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_subject_name_hash');
    f_X509_print                             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_print');
    f_X509_digest                            := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_digest'); //AG
    f_X509_check_private_key                 := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_check_private_key'); //AG
    f_EVP_sha1                               := GetProcAddress(GLIBEAY_DLL_Handle, 'EVP_sha1'); //AG
    f_EVP_PKEY_new                           := GetProcAddress(GLIBEAY_DLL_Handle, 'EVP_PKEY_new'); //AG
    f_EVP_PKEY_free                          := GetProcAddress(GLIBEAY_DLL_Handle, 'EVP_PKEY_free'); //AG
    f_EVP_PKEY_assign                        := GetProcAddress(GLIBEAY_DLL_Handle, 'EVP_PKEY_assign'); //AG
    f_RSA_generate_key                       := GetProcAddress(GLIBEAY_DLL_Handle, 'RSA_generate_key'); //AG
    f_OBJ_nid2sn                             := GetProcAddress(GLIBEAY_DLL_Handle, 'OBJ_nid2sn');
    f_OBJ_obj2nid                            := GetProcAddress(GLIBEAY_DLL_Handle, 'OBJ_obj2nid');
    f_sk_num                                 := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_num');
    f_sk_value                               := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_value');
    f_sk_new_null                            := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_new_null'); //AG
    f_sk_free                                := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_free'); //AG
    f_sk_pop_free                            := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_pop_free'); //AG
    f_sk_push                                := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_push'); //AG
    f_sk_delete                              := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_delete'); //AG
    f_sk_pop                                 := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_pop'); //AG
    f_sk_find                                := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_find'); //AG
    f_sk_insert                              := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_insert'); //AG
    f_sk_dup                                 := GetProcAddress(GLIBEAY_DLL_Handle, 'sk_dup'); //AG
    f_PEM_write_bio_X509                     := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_write_bio_X509');
    f_PEM_write_bio_X509_REQ                 := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_write_bio_X509_REQ');
    f_PEM_write_bio_X509_CRL                 := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_write_bio_X509_CRL');
    f_PEM_read_bio_X509_CRL                  := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_read_bio_X509_CRL');//AG
    f_PEM_read_bio_X509                      := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_read_bio_X509');
    f_PEM_X509_INFO_read_bio                 := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_X509_INFO_read_bio'); //AG
    f_CRYPTO_free                            := GetProcAddress(GLIBEAY_DLL_Handle, 'CRYPTO_free'); //AG
    f_X509_NAME_ENTRY_get_object             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_ENTRY_get_object'); //AG
    f_X509_NAME_get_entry                    := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_get_entry'); //AG
    f_X509_NAME_entry_count                  := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_entry_count'); //AG
    f_X509_NAME_ENTRY_get_data               := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_ENTRY_get_data'); //AG
    f_X509_set_version                       := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_set_version'); //AG
    f_ASN1_STRING_to_UTF8                    := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_STRING_to_UTF8'); //AG
    f_ASN1_INTEGER_set                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_INTEGER_set'); //AG
    f_ASN1_INTEGER_get                       := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_INTEGER_get');
    f_ASN1_STRING_print                      := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_STRING_print'); //AG
{v0.9.7g}
    f_ASN1_item_d2i                          := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_item_d2i'); //AG
    f_ASN1_item_free                         := GetProcAddress(GLIBEAY_DLL_Handle, 'ASN1_item_free'); //AG
{v0.9.7g end}
    f_i2a_ASN1_OBJECT                        := GetProcAddress(GLIBEAY_DLL_Handle, 'i2a_ASN1_OBJECT'); //AG
    f_X509_gmtime_adj                        := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_gmtime_adj'); //AG
    f_X509_set_pubkey                        := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_set_pubkey'); //AG
    f_X509_new                               := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_new'); //AG
    f_X509_NAME_add_entry_by_txt             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_add_entry_by_txt'); //AG
    f_X509_NAME_add_entry_by_NID             := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_add_entry_by_NID'); //AG
    f_X509_NAME_new                          := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_NAME_new'); //AG
    f_X509_set_issuer_name                   := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_set_issuer_name'); //AG
    f_X509_sign                              := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_sign'); //AG
    f_X509_INFO_free                         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_INFO_free'); //AG
    f_X509_CRL_dup                           := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_CRL_dup'); //AG
    f_X509_PKEY_free                         := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_PKEY_free'); //AG
    f_i2d_X509                               := GetProcAddress(GLIBEAY_DLL_Handle, 'i2d_X509'); //AG
    f_i2d_PrivateKey                         := GetProcAddress(GLIBEAY_DLL_Handle, 'i2d_PrivateKey'); //AG
    f_PEM_write_bio_PrivateKey               := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_write_bio_PrivateKey'); //AG
    f_PEM_read_bio_PrivateKey                := GetProcAddress(GLIBEAY_DLL_Handle, 'PEM_read_bio_PrivateKey'); //AG
    f_i2d_ASN1_bytes                         := GetProcAddress(GLIBEAY_DLL_Handle, 'i2d_ASN1_bytes'); //AG
    f_X509_get_pubkey                        := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_get_pubkey');//AG
    f_X509_PUBKEY_free                       := GetProcAddress(GLIBEAY_DLL_Handle, 'X509_PUBKEY_free'); //AG
    {
    f_OPENSSL_add_all_algorithms_noconf      := GetProcAddress(GLIBEAY_DLL_Handle, 'OPENSSL_add_all_algorithms_noconf');
    f_OPENSSL_add_all_algorithms_conf        := GetProcAddress(GLIBEAY_DLL_Handle, 'OPENSSL_add_all_algorithms_conf');
    f_OpenSSL_add_all_ciphers                := GetProcAddress(GLIBEAY_DLL_Handle, 'OpenSSL_add_all_ciphers');
    f_OpenSSL_add_all_digests                := GetProcAddress(GLIBEAY_DLL_Handle, 'OpenSSL_add_all_digests');
    f_EVP_cleanup                            := GetProcAddress(GLIBEAY_DLL_Handle, 'EVP_cleanup');
    }
{$IFNDEF OPENSSL_NO_ENGINE}
    f_ENGINE_load_builtin_engines            := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_load_builtin_engines'); //AG
    f_ENGINE_register_all_complete           := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_register_all_complete'); //AG
    f_ENGINE_cleanup                         := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_cleanup'); //AG
    f_ENGINE_by_id                           := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_by_id'); //AG
    f_ENGINE_init                            := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_init'); //AG
    f_ENGINE_finish                          := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_finish'); //AG
    f_ENGINE_set_default                     := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_set_default'); //AG
    f_ENGINE_ctrl_cmd_string                 := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_ctrl_cmd_string'); //AG
    f_ENGINE_free                            := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_free'); //AG
    f_ENGINE_load_private_key                := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_load_private_key'); //AG
    f_ENGINE_load_public_key                 := GetProcAddress(GLIBEAY_DLL_Handle, 'ENGINE_load_public_key'); //AG
    f_UI_new                                 := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_new'); //AG
    f_UI_new_method                          := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_new_method'); //AG
    f_UI_free                                := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_free'); //AG
    f_UI_create_method                       := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_create_method'); //AG
    f_UI_destroy_method                      := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_destroy_method'); //AG
    f_UI_set_ex_data                         := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_set_ex_data'); //AG
    f_UI_get_ex_data                         := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_get_ex_data'); //AG
    f_UI_method_set_reader                   := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_method_set_reader'); //AG
    f_UI_set_result                          := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_set_result'); //AG
    f_UI_OpenSSL                             := GetProcAddress(GLIBEAY_DLL_Handle, 'UI_OpenSSL'); //AG
{$ENDIF}
    // Check if any failed
    Result := not ((@f_SSLeay                                 = nil) or
                   (@f_SSLeay_version                         = nil) or
                   (@f_ERR_get_error_line_data                = nil) or
                   (@f_ERR_peek_error                         = nil) or
                   (@f_ERR_peek_last_error                    = nil) or
                   (@f_ERR_get_error                          = nil) or
                   (@f_ERR_error_string                       = nil) or
                   (@f_ERR_error_string_n                     = nil) or
                   (@f_ERR_clear_error                        = nil) or
                   (@f_ERR_remove_state                       = nil) or
                   (@f_ERR_free_strings                       = nil) or
                   (@f_RAND_seed                              = nil) or
                   (@f_BIO_new                                = nil) or
                   (@f_BIO_new_socket                         = nil) or
                   (@f_BIO_new_fd                             = nil) or
                   (@f_BIO_new_file                           = nil) or
                   (@f_BIO_new_mem_buf                        = nil) or
                   (@f_BIO_new_bio_pair                       = nil) or
                   (@f_BIO_ctrl                               = nil) or
                   (@f_BIO_s_mem                              = nil) or
                   (@f_BIO_get_retry_BIO                      = nil) or
                   (@f_BIO_get_retry_reason                   = nil) or
                   (@f_BIO_ctrl_pending                       = nil) or
                   (@f_BIO_ctrl_get_read_request              = nil) or // B.S.
                   (@f_BIO_ctrl_get_write_guarantee           = nil) or
                   (@f_d2i_X509_bio                           = nil) or
                   (@f_d2i_X509                               = nil) or
                   (@f_BIO_free                               = nil) or
                   (@f_BIO_read                               = nil) or
                   (@f_BIO_nread                              = nil) or
                   (@f_BIO_nread0                             = nil) or
                   (@f_BIO_gets                               = nil) or
                   (@f_BIO_puts                               = nil) or
                   (@f_BIO_push                               = nil) or
                   (@f_BIO_write                              = nil) or
                   (@f_BIO_nwrite                             = nil) or
                   (@f_BIO_nwrite0                            = nil) or
                   (@f_CRYPTO_num_locks                       = nil) or
                   (@f_CRYPTO_set_locking_callback            = nil) or
                   (@f_CRYPTO_set_id_callback                 = nil) or
                   (@f_CRYPTO_set_dynlock_create_callback     = nil) or
                   (@f_CRYPTO_set_dynlock_lock_callback       = nil) or
                   (@f_CRYPTO_set_dynlock_destroy_callback    = nil) or
                   (@f_X509_dup                               = nil) or
                   (@f_X509_STORE_add_cert                    = nil) or
                   (@f_X509_STORE_CTX_get_ex_data             = nil) or
                   (@f_X509_STORE_CTX_get_current_cert        = nil) or
                   (@f_X509_STORE_CTX_get_error               = nil) or
                   (@f_X509_STORE_CTX_get_error_depth         = nil) or
                   (@f_X509_STORE_CTX_get_chain               = nil) or
                   (@f_X509_STORE_CTX_trusted_stack           = nil) or
                   (@f_X509_STORE_add_crl                     = nil) or
                   (@f_X509_STORE_free                        = nil) or
                   (@f_X509_verify_cert_error_string          = nil) or
                   (@f_X509_get_issuer_name                   = nil) or
                   (@f_X509_get_subject_name                  = nil) or
                   (@f_X509_get_serialNumber                  = nil) or
                   (@f_X509_NAME_oneline                      = nil) or
                   (@f_X509_NAME_get_text_by_NID              = nil) or
                   (@f_X509_NAME_get_index_by_NID             = nil) or //AG
                   (@f_ASN1_STRING_free                       = nil) or //AG
                   (@f_X509_NAME_free                         = nil) or
                   (@f_X509_get_ext                           = nil) or
                   (@f_X509_get_ext_count                     = nil) or
                   (@f_X509_free                              = nil) or
                   (@f_X509_CRL_free                          = nil) or
                   (@f_X509V3_EXT_get                         = nil) or
                   (@f_X509V3_EXT_print                       = nil) or
                   (@f_X509V3_EXT_d2i                         = nil) or
                   (@f_X509V3_conf_free                       = nil) or
                   (@f_X509_EXTENSION_get_object              = nil) or
                   (@f_X509_EXTENSION_get_data                = nil) or
                   (@f_X509_EXTENSION_get_critical            = nil) or
                   (@f_X509_subject_name_hash                 = nil) or
                   (@f_X509_print                             = nil) or
                   (@f_X509_digest                            = nil) or //AG
                   (@f_X509_check_private_key                 = nil) or //AG
                   (@f_EVP_sha1                               = nil) or //AG
                   (@f_EVP_PKEY_free                          = nil) or //AG
                   (@f_EVP_PKEY_new                           = nil) or //AG
                   (@f_EVP_PKEY_assign                        = nil) or //AG
                   (@f_RSA_generate_key                       = nil) or //AG
                   (@f_OBJ_nid2sn                             = nil) or
                   (@f_OBJ_obj2nid                            = nil) or
                   (@f_sk_num                                 = nil) or
                   (@f_sk_value                               = nil) or
                   (@f_sk_new_null                            = nil) or
                   (@f_sk_free                                = nil) or
                   (@f_sk_pop_free                            = nil) or
                   (@f_sk_push                                = nil) or
                   (@f_sk_delete                              = nil) or
                   (@f_sk_pop                                 = nil) or
                   (@f_sk_find                                = nil) or
                   (@f_sk_insert                              = nil) or
                   (@f_sk_dup                                 = nil) or
                   (@f_PEM_write_bio_X509                     = nil) or
                   (@f_PEM_write_bio_X509_REQ                 = nil) or
                   (@f_PEM_write_bio_X509_CRL                 = nil) or
                   (@f_PEM_read_bio_X509_CRL                  = nil) or
                   (@f_PEM_read_bio_X509                      = nil) or
                   (@f_PEM_X509_INFO_read_bio                 = nil) or
                   (@f_CRYPTO_free                            = nil) or
                   (@f_X509_NAME_get_entry                    = nil) or
                   (@f_X509_NAME_ENTRY_get_object             = nil) or
                   (@f_X509_NAME_entry_count                  = nil) or
                   (@f_X509_NAME_ENTRY_get_data               = nil) or
                   (@f_X509_set_version                       = nil) or
                   (@f_ASN1_STRING_to_UTF8                    = nil) or
                   (@f_ASN1_INTEGER_set                       = nil) or
                   (@f_ASN1_INTEGER_get                       = nil) or
                   (@f_ASN1_STRING_print                      = nil) or
                   (@f_ASN1_item_free                         = nil) or
                   (@f_ASN1_item_d2i                          = nil) or
                   (@f_i2a_ASN1_OBJECT                        = nil) or
                   (@f_X509_gmtime_adj                        = nil) or
                   (@f_X509_set_pubkey                        = nil) or
                   (@f_X509_new                               = nil) or
                   (@f_X509_NAME_add_entry_by_txt             = nil) or
                   (@f_X509_NAME_add_entry_by_NID             = nil) or
                   (@f_X509_NAME_new                          = nil) or
                   (@f_X509_set_issuer_name                   = nil) or
                   (@f_X509_sign                              = nil) or
                   (@f_X509_INFO_free                         = nil) or
                   (@f_X509_CRL_dup                           = nil) or
                   (@f_X509_PKEY_free                         = nil) or
                   (@f_i2d_X509                               = nil) or
                   (@f_i2d_PrivateKey                         = nil) or
                   (@f_PEM_read_bio_PrivateKey                = nil) or
                   (@f_PEM_write_bio_PrivateKey               = nil) or
                   (@f_i2d_ASN1_bytes                         = nil) or
                   (@f_X509_get_pubkey                        = nil) or
                   (@f_X509_PUBKEY_free                       = nil){ or
                   (@f_OPENSSL_add_all_algorithms_noconf      = nil) or
                   (@f_OPENSSL_add_all_algorithms_conf        = nil) or
                   (@f_OpenSSL_add_all_ciphers                = nil) or
                   (@f_OpenSSL_add_all_digests                = nil) or
                   (@f_EVP_cleanup                            = nil)}
                {$IFNDEF OPENSSL_NO_ENGINE}
                                                                     or
                   (@f_ENGINE_load_builtin_engines            = nil) or
                   (@f_ENGINE_register_all_complete           = nil) or
                   (@f_ENGINE_cleanup                         = nil) or
                   (@f_ENGINE_by_id                           = nil) or
                   (@f_ENGINE_init                            = nil) or
                   (@f_ENGINE_finish                          = nil) or
                   (@f_ENGINE_set_default                     = nil) or
                   (@f_ENGINE_ctrl_cmd_string                 = nil) or
                   (@f_ENGINE_free                            = nil) or
                   (@f_ENGINE_load_private_key                = nil) or
                   (@f_ENGINE_load_public_key                 = nil) or
                   (@f_UI_new                                 = nil) or
                   (@f_UI_new_method                          = nil) or
                   (@f_UI_free                                = nil) or
                   (@f_UI_create_method                       = nil) or
                   (@f_UI_destroy_method                      = nil) or
                   (@f_UI_set_ex_data                         = nil) or
                   (@f_UI_get_ex_data                         = nil) or
                   (@f_UI_method_set_reader                   = nil) or
                   (@f_UI_set_result                          = nil) or
                   (@f_UI_OpenSSL                             = nil)
                {$ENDIF}
                   );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WhichFailedToLoad : String;
begin
    Result := '';
    if @f_SSLeay                                 = nil then Result := Result + ' SSLeay';
    if @f_SSLeay_version                         = nil then Result := Result + ' SSLeay_version';
    if @f_ERR_get_error_line_data                = nil then Result := Result + ' ERR_get_error_line_data';
    if @f_ERR_peek_error                         = nil then Result := Result + ' ERR_peek_error';
    if @f_ERR_peek_last_error                    = nil then Result := Result + ' ERR_peek_last_error';
    if @f_ERR_get_error                          = nil then Result := Result + ' ERR_get_error';
    if @f_ERR_error_string                       = nil then Result := Result + ' ERR_error_string';
    if @f_ERR_error_string_n                     = nil then Result := Result + ' ERR_error_string_n';
    if @f_ERR_clear_error                        = nil then Result := Result + ' ERR_clear_error';
    if @f_ERR_remove_state                       = nil then Result := Result + ' ERR_remove_state';
    if @f_ERR_free_strings                       = nil then Result := Result + ' ERR_free_strings';
    if @f_RAND_seed                              = nil then Result := Result + ' RAND_seed';
    if @f_BIO_new                                = nil then Result := Result + ' BIO_new';
    if @f_BIO_new_socket                         = nil then Result := Result + ' BIO_new_socket';
    if @f_BIO_new_fd                             = nil then Result := Result + ' BIO_new_fd';
    if @f_BIO_new_file                           = nil then Result := Result + ' BIO_new_file';
    if @f_BIO_new_mem_buf                        = nil then Result := Result + ' BIO_new_mem_buf';
    if @f_BIO_ctrl                               = nil then Result := Result + ' BIO_ctrl';
    if @f_BIO_ctrl_get_read_request              = nil then Result := Result + ' BIO_ctrl_get_read_request'; // B.S.
    if @f_BIO_ctrl_pending                       = nil then Result := Result + ' BIO_ctrl_pending';
    if @f_BIO_ctrl_get_write_guarantee           = nil then Result := Result + ' BIO_ctrl_get_write_guarantee';
    if @f_d2i_X509_bio                           = nil then Result := Result + ' d2i_X509_bio';
    if @f_d2i_X509                               = nil then Result := Result + ' d2i_X509';
    if @f_BIO_free                               = nil then Result := Result + ' BIO_free';
    if @f_BIO_read                               = nil then Result := Result + ' BIO_read';
    if @f_BIO_nread                              = nil then Result := Result + ' BIO_nread';
    if @f_BIO_nread0                             = nil then Result := Result + ' BIO_nread0';
    if @f_BIO_gets                               = nil then Result := Result + ' BIO_gets';
    if @f_BIO_puts                               = nil then Result := Result + ' BIO_puts';
    if @f_BIO_push                               = nil then Result := Result + ' BIO_push';
    if @f_BIO_write                              = nil then Result := Result + ' BIO_write';
    if @f_BIO_nwrite                             = nil then Result := Result + ' BIO_nwrite';
    if @f_BIO_nwrite0                            = nil then Result := Result + ' BIO_nwrite0';
    if @f_CRYPTO_num_locks                       = nil then Result := Result + ' CRYPTO_num_locks';
    if @f_CRYPTO_set_locking_callback            = nil then Result := Result + ' CRYPTO_set_locking_callback';
    if @f_CRYPTO_set_id_callback                 = nil then Result := Result + ' CRYPTO_set_id_callback';
    if @f_CRYPTO_set_dynlock_create_callback     = nil then Result := Result + ' CRYPTO_set_dynlock_create_callback';
    if @f_CRYPTO_set_dynlock_lock_callback       = nil then Result := Result + ' CRYPTO_set_dynlock_lock_callback';
    if @f_CRYPTO_set_dynlock_destroy_callback    = nil then Result := Result + ' CRYPTO_set_dynlock_destroy_callback';
    if @f_X509_dup                               = nil then Result := Result + ' X509_dup';//AG
    if @f_X509_STORE_add_cert                    = nil then Result := Result + ' X509_STORE_add_cert';//AG
    if @f_X509_STORE_CTX_get_ex_data             = nil then Result := Result + ' X509_STORE_CTX_get_ex_data';
    if @f_X509_STORE_CTX_get_current_cert        = nil then Result := Result + ' X509_STORE_CTX_get_current_cert';
    if @f_X509_STORE_CTX_get_error               = nil then Result := Result + ' X509_STORE_CTX_get_error';
    if @f_X509_STORE_CTX_get_error_depth         = nil then Result := Result + ' X509_STORE_CTX_get_error_depth';
    if @f_X509_STORE_CTX_get_chain               = nil then Result := Result + ' X509_STORE_CTX_get_chain';//AG
    if @f_X509_STORE_CTX_trusted_stack           = nil then Result := Result + ' X509_STORE_CTX_trusted_stack';//AG
    if @f_X509_STORE_add_crl                     = nil then Result := Result + ' X509_STORE_add_crl';//AG
    if @f_X509_STORE_free                        = nil then Result := Result + ' X509_STORE_free';//AG
    if @f_X509_verify_cert_error_string          = nil then Result := Result + ' X509_verify_cert_error_string';
    if @f_X509_get_issuer_name                   = nil then Result := Result + ' X509_get_issuer_name';
    if @f_X509_get_subject_name                  = nil then Result := Result + ' X509_get_subject_name';
    if @f_X509_get_serialNumber                  = nil then Result := Result + ' X509_get_serialNumber';
    if @f_X509_NAME_oneline                      = nil then Result := Result + ' X509_NAME_oneline';
    if @f_X509_NAME_get_text_by_NID              = nil then Result := Result + ' X509_NAME_get_text_by_NID';
    if @f_X509_NAME_get_index_by_NID             = nil then Result := Result + ' X509_NAME_get_index_by_NID';//AG
    if @f_ASN1_STRING_free                       = nil then Result := Result + ' ASN1_STRING_free';//AG
    if @f_X509_NAME_free                         = nil then Result := Result + ' X509_NAME_free';
    if @f_X509_get_ext                           = nil then Result := Result + ' X509_get_ext';
    if @f_X509_get_ext_count                     = nil then Result := Result + ' X509_get_ext_count';
    if @f_X509_CRL_free                          = nil then Result := Result + ' X509_CRL_free';//AG
    if @f_X509_free                              = nil then Result := Result + ' X509_free';//AG
    if @f_X509V3_EXT_get                         = nil then Result := Result + ' X509V3_EXT_get';
    if @f_X509V3_EXT_print                       = nil then Result := Result + ' X509V3_EXT_print';//AG
    if @f_X509V3_EXT_d2i                         = nil then Result := Result + ' X509V3_EXT_d2i';//AG
    if @f_X509V3_conf_free                       = nil then Result := Result + ' X509V3_conf_free';//AG
    if @f_X509_EXTENSION_get_object              = nil then Result := Result + ' X509_EXTENSION_get_object';
    if @f_X509_EXTENSION_get_data                = nil then Result := Result + ' X509_EXTENSION_get_data';//AG
    if @f_X509_EXTENSION_get_critical            = nil then Result := Result + ' X509_EXTENSION_get_critical';//AG
    if @f_X509_subject_name_hash                 = nil then Result := Result + ' X509_subject_name_hash';
    if @f_X509_print                             = nil then Result := Result + ' X509_print';
    if @f_X509_digest                            = nil then Result := Result + ' X509_digest'; //AG
    if @f_X509_check_private_key                 = nil then Result := Result + ' X509_check_private_key'; //AG
    if @f_EVP_sha1                               = nil then Result := Result + ' EVP_sha1'; //AG
    if @f_EVP_PKEY_free                          = nil then Result := Result + ' EVP_PKEY_free'; //AG
    if @f_EVP_PKEY_new                           = nil then Result := Result + ' EVP_PKEY_new'; //AG
    if @f_EVP_PKEY_assign                        = nil then Result := Result + ' EVP_PKEY_assign'; //AG
    if @f_RSA_generate_key                       = nil then Result := Result + ' RSA_generate_key'; //AG
    if @f_OBJ_nid2sn                             = nil then Result := Result + ' OBJ_nid2sn';
    if @f_OBJ_obj2nid                            = nil then Result := Result + ' OBJ_obj2nid';
    if @f_sk_num                                 = nil then Result := Result + ' sk_num';
    if @f_sk_value                               = nil then Result := Result + ' sk_value';
    if @f_sk_new_null                            = nil then Result := Result + ' sk_new_null';//AG
    if @f_sk_free                                = nil then Result := Result + ' sk_free';//AG
    if @f_sk_pop_free                            = nil then Result := Result + ' f_sk_pop_free';//AG
    if @f_sk_push                                = nil then Result := Result + ' sk_push';//AG
    if @f_sk_delete                              = nil then Result := Result + ' sk_delete';//AG
    if @f_sk_pop                                 = nil then Result := Result + ' sk_pop';//AG
    if @f_sk_find                                = nil then Result := Result + ' sk_find';//AG
    if @f_sk_insert                              = nil then Result := Result + ' sk_insert';//AG
    if @f_sk_dup                                 = nil then Result := Result + ' sk_dup';//AG
    if @f_PEM_write_bio_X509_REQ                 = nil then Result := Result + ' PEM_write_bio_X509_REQ';
    if @f_PEM_write_bio_X509_CRL                 = nil then Result := Result + ' PEM_write_bio_X509_CRL';
    if @f_PEM_read_bio_X509_CRL                  = nil then Result := Result + ' PEM_read_bio_X509_CRL'; //AG
    if @f_PEM_read_bio_X509                      = nil then Result := Result + ' PEM_read_bio_X509';
    if @f_PEM_X509_INFO_read_bio                 = nil then Result := Result + ' PEM_X509_INFO_read_bio'; //AG
    if @f_CRYPTO_free                            = nil then Result := Result + ' CRYPTO_free';//AG
    if @f_X509_NAME_ENTRY_get_object             = nil then Result := Result + ' X509_NAME_ENTRY_get_object';//AG
    if @f_X509_NAME_get_entry                    = nil then Result := Result + ' X509_NAME_get_entry';//AG
    if @f_X509_NAME_entry_count                  = nil then Result := Result + ' X509_NAME_entry_count';//AG
    if @f_X509_NAME_ENTRY_get_data               = nil then Result := Result + ' X509_NAME_ENTRY_get_data';//AG
    if @f_X509_set_version                       = nil then Result := Result + ' X509_set_version';//AG
    if @f_ASN1_STRING_to_UTF8                    = nil then Result := Result + ' ASN1_STRING_to_UTF8';//AG
    if @f_ASN1_INTEGER_set                       = nil then Result := Result + ' ASN1_INTEGER_set';//AG
    if @f_ASN1_INTEGER_get                       = nil then Result := Result + ' ASN1_INTEGER_get';
    if @f_ASN1_STRING_print                      = nil then Result := Result + ' ASN1_STRING_print';//AG
    if @f_ASN1_item_d2i                          = nil then Result := Result + ' ASN1_item_d2i';//AG
    if @f_ASN1_item_free                         = nil then Result := Result + ' ASN1_item_free';//AG
    if @f_i2a_ASN1_OBJECT                        = nil then Result := Result + ' f_i2a_ASN1_OBJECT';//AG
    if @f_X509_gmtime_adj                        = nil then Result := Result + ' X509_gmtime_adj';//AG
    if @f_X509_set_pubkey                        = nil then Result := Result + ' X509_set_pubkey';//AG
    if @f_X509_new                               = nil then Result := Result + ' X509_new';//AG
    if @f_X509_NAME_add_entry_by_txt             = nil then Result := Result + ' X509_NAME_add_entry_by_txt';//AG
    if @f_X509_NAME_add_entry_by_NID             = nil then Result := Result + ' X509_NAME_add_entry_by_NID';//AG
    if @f_X509_NAME_new                          = nil then Result := Result + ' X509_NAME_new';//AG
    if @f_X509_set_issuer_name                   = nil then Result := Result + ' X509_set_issuer_name';//AG
    if @f_X509_sign                              = nil then Result := Result + ' X509_sign';//AG
    if @f_X509_INFO_free                         = nil then Result := Result + ' X509_INFO_free';//AG
    if @f_X509_CRL_dup                           = nil then Result := Result + ' X509_CRL_dup';//AG
    if @f_X509_PKEY_free                         = nil then Result := Result + ' X509_PKEY_free';//AG
    if @f_i2d_X509                               = nil then Result := Result + ' i2d_X509';//AG
    if @f_i2d_PrivateKey                         = nil then Result := Result + ' i2d_PrivateKey';//AG
    if @f_PEM_read_bio_PrivateKey                = nil then Result := Result + ' PEM_read_bio_PrivateKey';//AG
    if @f_PEM_write_bio_PrivateKey               = nil then Result := Result + ' PEM_write_bio_PrivateKey';//AG
    if @f_i2d_ASN1_bytes                         = nil then Result := Result + ' i2d_ASN1_bytes';//AG
    if @f_X509_get_pubkey                        = nil then Result := Result + ' X509_get_pubkey';//AG
    if @f_X509_PUBKEY_free                       = nil then Result := Result + ' X509_PUBKEY_free';//AG
    {
    if @f_OPENSSL_add_all_algorithms_noconf      = nil then Result := Result + ' OPENSSL_add_all_algorithms_noconf';
    if @f_OPENSSL_add_all_algorithms_conf        = nil then Result := Result + ' OPENSSL_add_all_algorithms_conf';
    if @f_OpenSSL_add_all_ciphers                = nil then Result := Result + ' OpenSSL_add_all_ciphers';
    if @f_OpenSSL_add_all_digests                = nil then Result := Result + ' OpenSSL_add_all_ciphers';
    if @f_EVP_cleanup                            = nil then Result := Result + ' EVP_cleanup';
    }
{$IFNDEF OPENSSL_NO_ENGINE}
    if @f_ENGINE_load_builtin_engines            = nil then Result := Result + ' ENGINE_load_builtin_engines';//AG
    if @f_ENGINE_register_all_complete           = nil then Result := Result + ' ENGINE_register_all_complete';//AG
    if @f_ENGINE_cleanup                         = nil then Result := Result + ' ENGINE_cleanup';//AG
    if @f_ENGINE_by_id                           = nil then Result := Result + ' ENGINE_by_id';//AG
    if @f_ENGINE_init                            = nil then Result := Result + ' ENGINE_init';//AG
    if @f_ENGINE_finish                          = nil then Result := Result + ' ENGINE_finish';//AG
    if @f_ENGINE_set_default                     = nil then Result := Result + ' ENGINE_set_default';//AG
    if @f_ENGINE_ctrl_cmd_string                 = nil then Result := Result + ' ENGINE_ctrl_cmd_string';//AG
    if @f_ENGINE_free                            = nil then Result := Result + ' ENGINE_free';//AG
    if @f_ENGINE_load_private_key                = nil then Result := Result + ' ENGINE_load_private_key';//AG
    if @f_ENGINE_load_public_key                 = nil then Result := Result + ' ENGINE_load_public_key';//AG
    if @f_UI_new                                 = nil then Result := Result + ' UI_new';//AG
    if @f_UI_new_method                          = nil then Result := Result + ' UI_new_method';//AG
    if @f_UI_free                                = nil then Result := Result + ' UI_free';//AG
    if @f_UI_create_method                       = nil then Result := Result + ' UI_create_method';//AG
    if @f_UI_destroy_method                      = nil then Result := Result + ' UI_destroy_method';//AG
    if @f_UI_set_ex_data                         = nil then Result := Result + ' UI_set_ex_data';//AG
    if @f_UI_get_ex_data                         = nil then Result := Result + ' UI_get_ex_data';//AG
    if @f_UI_method_set_reader                   = nil then Result := Result + ' UI_method_set_reader';//AG
    if @f_UI_set_result                          = nil then Result := Result + ' UI_set_result';//AG
    if @f_UI_OpenSSL                             = nil then Result := Result + ' UI_OpenSSL';//AG
{$ENDIF}
    if Length(Result) > 0 then
       Delete(Result, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer;
begin
    Result := f_UI_set_ex_data(r, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_UI_get_app_data(r: PUI): Pointer;
begin
    Result := f_UI_get_ex_data(r, 0);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal;
begin
    Result := (ErrCode and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 24) and $FF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 12) and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean;
begin
    Result := ((ErrCode and ERR_R_FATAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;        {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        PCInfo := Pointer(PDWord(X)^);
        Result := PCInfo^.Validity^.notBefore;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;         {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        PCInfo := Pointer(PDWord(X)^);
        Result := PCInfo^.Validity^.notAfter;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME;               {AG 03/03/06}
    out UT: TDateTime): Boolean;

    function IncHour(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 24) + IncBy) / 24;
    end;

    function IncMin(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 1440) + IncBy) / 1440;
    end;

var
    Y, M, D, H, N, S : Word;
    I : Integer;
    YC : Word;  { Current century }
    P  : PAnsiChar;
    Offset : Integer;
    Str    : AnsiString;
    IntH, IntM : Integer;
    Sign : Boolean;
begin
    Result  := FALSE;
    UT      := MinDateTime;
    if not Assigned(Asn1Time) then
        Exit;
    try
        I := Asn1Time^.length;
        if I < 10 then Exit;
        P   := Asn1Time.data;
        Y := 0; M := 0; D := 0; {H := 0; N := 0;} S := 0;
        
        if Asn1Time^.Type_ = V_ASN1_UTCTIME then begin
            {if I < 10 then
                Exit;}
            for I := 0 to 10 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            DecodeDate(Now, Y, M, D);
            YC := (Trunc(Y / 100) * 100);
            Y := atoi(P[0] + P[1]);
            if Y < 50 then   { fix century }
                Y := Y + YC
            else
                Y := Y + YC - 100;
            M := atoi(P[2] + P[3]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[4] + P[5]);
            H := atoi(P[6] + P[7]);
            N := atoi(P[8] + P[9]);
            { Do we have seconds? }
            if (P[10] in ['0'..'9']) and
               (P[11] in ['0'..'9']) then
            S := atoi(P[10] + P[11]);
        end else
        if Asn1Time^.Type_ = V_ASN1_GENERALIZEDTIME then begin
            if I < 12 then Exit;
            for I := 0 to 12 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            Y := atoi(P[0] + P[1] + P[2] + P[3]);
            M := atoi(P[4] + P[5]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[6] + P[7]);
            H := atoi(P[8] + P[9]);
            N := atoi(P[10] + P[11]);
            { Do we have seconds? }
            if (P[12] in ['0'..'9']) and
               (P[13] in ['0'..'9']) then
            S := atoi(P[12] + P[13]);
        end else
            Exit;
        UT := EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0);

        { Timezone Offset                                          }
        { '980101000000Z' sample V_ASN1_UTCTIME GMT                }
        { '990630000000+1000' sample timezone + 10 hours           }
        { '20000322085551Z' sample V_ASN1_GENERALIZEDTIME GMT      }
        I := Asn1Time^.length;
        if P[I - 1] <> 'Z' then  // Z = GMT = offset = 0
           { Offset := 0         // offset 0
        else} begin              // get the offset
            SetLength(Str, I);
            Dec(I);
            while I >= 0 do begin
                if P[I] in ['0'..'9'] then
                    Dec(I)
                else begin
                    if P[I] in ['-', '+'] then
                    begin
                        if P[I] = '-' then
                            Sign := TRUE
                        else
                            Sign := FALSE;
                        StrECopy(PAnsiChar(Str), PAnsiChar(@P[I + 1]));
                        SetLength(Str, StrLen(PAnsiChar(Str)));
                        Offset := atoi(Str);
                        if Sign then
                            Offset := -Offset;
                        if (Offset <> 0) and (Offset >= -1200) and
                           (Offset <= 1300) then begin
                            IntH := (Offset div 100);
                            IntM := (Offset mod 100);
                            if IntH <> 0 then
                                UT := IncHour(UT, IntH);
                            if IntM <> 0 then
                                UT := IncMin(UT,  IntM);
                        end;
                    end;
                    Break;
                end;
            end;
        end;
        Result := True;
    except
        // do nothing
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_get_flags(b: PBIO): Integer;
begin
    // This is a hack : BIO structure has bnot been defined. But I know
    // flags member is the 6th 32 bit field in the structure (index is 5)
    // This could change when OpenSSL is updated. Check "struct bio_st".
    Result := PInteger(PAnsiChar(b) + 5 * SizeOf(Integer))^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_retry(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_SHOULD_RETRY) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM;
begin
    Result := iptr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_read(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_READ) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_write(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_WRITE) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_io_special(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_IO_SPECIAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_retry_type(b: PBIO): Integer;
begin
    Result := (BIO_get_flags(b) and BIO_FLAGS_RWS);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslVersion : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_VERSION)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslCompilerFlags : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_CFLAGS)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslBuiltOn : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_BUILT_ON)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslPlatForm : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_PLATFORM)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslDir : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_DIR)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BMPStrToWideStr(Str : PAnsiChar; Len : Integer): UnicodeString;
var
    I : Integer;
begin
    SetLength(Result, Len shr 1);
    for I := 0 to (Len shr 1) - 1 do
        Result[I + 1] := WideChar(Byte(Str[I * 2 + 1]) or Byte(Str[I * 2]) shl 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeOctetStr(Str : PAnsiChar; Len : Integer) : String;
var
    I : Integer;
    Item : String;
begin
    if (Len = 0) or (Str = nil) then Exit;
    SetLength(Result, Len * 3);
    I := 0;
    while I <= Len - 1 do begin
        Item := IntToHex(Ord(Str[I]), 2) + ':';
        Move(Item[1], Result[I * 3 + 1], 3 * SizeOf(Char));
        Inc(I);
    end;
    SetLength(Result, Length(Result) - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToString(PAsn1 : PASN1_STRING): String;
{$IFDEF UNICODE}
var
    Len : Integer;
{$ENDIF}
begin
    if (PAsn1 = nil) or (PAsn1^.data = nil) or (PAsn1^.length <= 0) then
        Exit;
    case PAsn1^.type_ of

      V_ASN1_OCTET_STRING :
          Result := EncodeOctetStr(PAsn1^.data, PAsn1^.length);
{$IFNDEF UNICODE}
      V_ASN1_UTF8STRING :
      begin  { Slow, but rarely used }
          SetLength(Result, PAsn1^.length);
          Move(PAnsiChar(PAsn1^.data)^, PAnsiChar(Result)^, PAsn1^.length);
          Result := Utf8ToStringA(Result); { convert to Ansi }
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order and convert to Ansi }
          Result := UnicodeToAnsi(BMPStrToWideStr(PAsn1^.data, PAsn1^.length));

      else  { dump }
          SetLength(Result, PAsn1^.length);
          Move(Pointer(PAsn1^.data)^, Pointer(Result)^, PAsn1^.length);
{$ELSE}
      V_ASN1_UTF8STRING :
      begin
          Len := MultiByteToWideChar(CP_UTF8, 0, PAsn1^.data,  PAsn1^.length,
                                     nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
              MultiByteToWideChar(CP_UTF8, 0, PAsn1^.data, PAsn1^.length,
                                  Pointer(Result), Len);
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order }
          Result := BMPStrToWideStr(PAsn1^.data, PAsn1^.length);

      else  { dump }
          Len := MultiByteToWideChar(CP_ACP, 0, PAsn1^.data,
                                     PAsn1^.length, nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
              MultiByteToWideChar(CP_ACP, 0, PAsn1^.data, PAsn1^.length,
                                  Pointer(Result), Len);
{$ENDIF}
      end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} //USE_SSL
end.
