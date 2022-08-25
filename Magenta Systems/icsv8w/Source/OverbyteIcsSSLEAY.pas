{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for SSLEAY32.DLL (OpenSSL)
              Renamed libssl32.dll for OpenSSL 1.1.0 and later
              This is only the subset needed by ICS.
Creation:     Jan 12, 2003
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2022 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Dec 11, 2004 Fixed Load to correctly check for LoadLibrary failure and to
             return correct error number.
Nov 08, 2005 F. Piette Worked around Delphi 6 bug related to type redefinition.
             Search for NoTypeEnforce in the type declarations.
Nov 08, 2005 Arno Garrels - Checked non-dummy structures against different
             OpenSSL versions see comments.
             Changed declaration of TX509_EXTENSION according OpenSSL v.0.9.7g.
Nov 19, 2005 F. Piette fixed internal error for Delphi 5. Same fix as Delphi 6.
             Introduced symbol NoTypeEnforce for that purpose.
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added.
Jan 27, 2006 A. Garrels made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
             Added a few constants and dummy records.
Aug 02, 2008 Still one PChar caught in one of the records.
Dec 20, 2009 A.Garrels added plenty of stuff. Some is not yet used some is, like
             Server Name Indication (SNI).
May 08, 2010 A. Garrels added two declarations required to support
             Open SSL 0.9.8n.
Apr 23, 2011 A. Garrels added C-macro f_SSL_clear_options.
Apr 24, 2011 Arno - Record TEVP_PKEY_st changed in 1.0.0 and had to
             be declared as dummy. See helper functions Ics_Ssl_EVP_PKEY_xxx
             in OverbyteIcsLibeay.pas.
May 03, 2011 Arno added some function declarations.
May 31, 2011 Arno removed the packed modifier from non-dummy records.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 13, 2015 V8.01 Angus updated SSL_OP option literals, added TLS v1.1 and 1.2 methods
             Added functions need to generate DH keys for EDH ciphers with Forward Secrecy
             Note, only OpenSSL 1.0.1 and later are now supported, removed various conditionals
May 08, 2015 V8.02 Angus adding missing SSL_OP_SINGLE_ECDH_USE
Nov 20, 2015 V8.03 Eugene Kotlyarov added RSA key related stuff
Mar 3, 2016  V8.04 Angus support define OPENSSL_ALLOW_SSLV2 to load old OpenSSL
                     DLLs that still export such methods
May 24, 2016 V8.27 Angus match version to Wsocket where most of this API is used
             Initial support for OpenSSL 1.1.0, new DLL file names, old exports gone
             Moved all public GLIBEAY_xx variables here from OverbyteIcsLIBEAY
             Add public variable GSSLEAY_DLL_IgnoreNew which should be set to TRUE before calling
              any SSL functions if OpenSSL 1.1.0 should be ignored.  Otherwise libcrypto-1_1.dll
              found in the PATH will override libeay32.dll in the local directory
             Added public variable GSSL_BUFFER_SIZE defaults to 16384, previously fixed
               at 4096, may improve SSL performance if larger
             Added public variable GSSL_DLL_DIR if set before OpenSSL loaded,
               will use this directory for DLLs, must have trailing \
             Load now SsleayLoad, WhichFailedToLoad now SsleayWhichFailedToLoad
             Added f_SSL_get_ciphers and related functions to get lists of ciphers
             Added TSslHandshakeState more detail about handshakes in 1.1.0
             GetFileVerInfo renamed IcsGetFileVerInfo to prevent conflicts with other libs
June 26, 2016 V8.29 Angus Implement GSSL_DLL_DIR properly to report full file path on error
Aug 5, 2016   V8.31 Angus testing OpenSSL 1.1.0 beta 6
Aug 27, 2016  V8.32 Angus, suuport final release OpenSSL 1.1.0
                OpenSSL 64-bit DLLs have different file names with -x64 added
Sept 5, 2016  V8.34 Angus, make ICS work up to OpenSSL release 1.1.1
                (security and bug releases are 1.1.0a/b/etc with no changed exports, in theory)
              Added public variable GSSLEAY_DLL_IgnoreOld so only OpenSSL 1.1.0 and later are loaded
Oct 18, 2016  V8.35 Angus, major rewrite to simplify loading OpenSSL DLL functions
              Reversed V8.34 fix so this release only supports 1.1.0 not 1.1.1
              OPENSSL_ALLOW_SSLV2 gone with all SSLv2 functions
              stub more removed functions to save some exceptions
Oct 26, 2016  V8.36 Angus more clean up of old stuff gone from 1.1.0
Nov 15, 2016  V8.38 Added public variable GSSL_SignTest_Check to check OpenSSL
                DLLs are digitally signed, and GSSL_SignTest_Certificate to
                check for a valid certificate, both default to false
              Moved IcsGetFileVerInfo to OverbyteIcsUtils
Nov 22, 2016  V8.39 Added functions to check certificate params using X509_VERIFY_PARAM, X509_OBJECT
              Minimum OpenSSL supported is now 1.0.2 (1.0.1 support ceases Dec 2016)
Jan 27, 2017  V8.40 Added more functions to get and check context certs
              Added Protocol Message callback functions for handshake debugging
              Added Security Level functions (1.1.0 and later)
Feb 24, 2017  V8.41 Added more constants
June 13, 2017 V8.49 Fixes to build on MacOs
Sep 22, 2017  V8.50 Added more types
Nov 22, 2017  V8.51 Testing OpenSSL 1.1.1 that adds TLS/1.3
              Corrected various set options to be exports with 1.1.0 instead
                of macros earlier, also SSL_session_reused
              Added more constants and functions for 1.1.1, f_SSL_xx_groups
Feb 27, 2018  V8.52 Added more EVP functions for keys, hashing and signing
Jun 20, 2018  V8.55 Testing with OpenSSL 1.1.1 beta
Oct 10, 2018  V8.57 added APLN APIs and literals
                    Support OpenSSL 1.1.1 final with TLS/1.3
                    Moved some SSL types and lits from Wsocket
                    EVP_MAX_KEY_LENGTH now 64
Oct 19, 2018  V8.58 version only
Nov 27, 2018  V8.59 version only
Mar 18, 2019  V8.60 Next major OpenSSL version is 3.0.0 (due mid 2020)
                    Added sslSrvSecTls12Less and sslSrvSecTls13Only to disable
                      in server IcsHosts if TLS1.3 fails.
Jul 15, 2019  V8.62 Removed two ciphers from TSslPrivKeyCipher which we did not use.
                    SuppProtoAcmeV1 gone, two more added.
                    Added ICS_NID_acmeIdentifier created dynamically on startup.
May 07, 2020  V8.64 Changed sslSrvSecDefault to sslSrvSecHigh since TLSv1.1
                       disabled in most browsers from early 2020.
                    Added SSL_client_hello functions and SSL_bytes_to_cipher_list.
                    Added ChallDnsAuto and ChallDnsMan
                    Fixed declarations of f_SSL_clear, TProto_msg_cb and
                      f_SSL_bytes_to_cipher_list thanks to Ralf Junker.
Dec 14, 2020 V8.65 OpenSSL versions only
                   GSSLEAY_DLL_IgnoreOld defaults true since OpenSSL 1.0.2 out of support.
                   Initial support for testing OpenSSL 3.0 beta.
                   GSSLEAY_DLL_IgnoreNew now means use 1.1.1 not 3.0.0.
                   Open SSL DLL in same directory as LIBEAY DLL, better exceptions.
                   Changed Longint to Integer and LongWord to Cardinal to keep
                      MacOS64 happy since it thinks both are 64-bits.
                   Added Posix.SysTypes for size_t.
                   Changed MACOS to POSIX for Unix support.
                   Corrected TProto_msg_cb len to size_t (not actually used anywhere).
Mar 16, 2021 V8.66 Renamed all OpenSSL functions to original names removing ICS
                     f_ prefix.  This will require application or component changes
                     if you access any of these functions directly.
                   Removed support for OpenSSL 1.0.2 and 1.1.0 whose support ceased
                     Dec 2019, also Fips mode pending 3.0.
                   Added support for YuOpenSSL which provides OpenSSL in a pre-built
                     DCU linked into applications, rather than using external DLLs.
                   Most OpenSSL type pointers to Dummy array now simple Pointer type.
                   Added GSSLStaticLinked true/false if OpenSSL statically linked.
                   Updated comments for TSslSrvSecurity.
Sep 27, 2021 V8.67 Updated to support OpenSSL 3.0, SSL_get_peer_certificate export renamed.
                   OpenSSL 3.0 makes several old ciphers and digests legacy meaning they are
                     not normally loaded, so removed types PrivKeyEncBlowfish128,
                     Cipher_des_ede3_cbc, Cipher_idea_cbc, Cipher_bf_cbc,
                     and SslPrivKeyEvpBits so new applications don't use them.
                   Added new version variables ICS_OPENSSL_VERSION_MAJOR/MINOR/PATCH/
                     PRE_RELEASE, and ICS_OSSL3_LOADED_LEGACY and ICS_OSSL3_LOADED_FIPS
                     if these OpenSSL 3.0 providers are loaded.
                   GSSLEAY_LOAD_LEGACY set to true caused legacy provider to be loaded
                     on start-up, specifically for DES3 and MD5.
                   Added many OSSL parameter constants to access OpenSSL providers
                     for low level functions such as RSA and ECDSA keys.
                   Added PrivKeyECsecp256k private key alternate to PrivKeyECsecp256.
Oct 07, 2021 V8.68 Support OpenSSL 3.0 for YuOpenSSL.
                   Corrected TPKCS7_st for 3.0.
Apr 01, 2022 V8.69 Added lots of OCSP types to check certificates.
                   Added several xx_tlsext_status_xx macros for OCSP stapling.
                   Changed sslCliSecDefault to sslCliSecTls12 for 1.2 or better.
                   Public variable GSSLEAY_DLL_IgnoreOld default is false again to make
                     it easier to use 1.1 instead of 3.0 (both true not allowed).



YuOpenSSL is a commercial product from https://www.yunqa.de/ and is supplied as
separate compiled DCUs for Delphi 5 to 10.4.

DEFINE YuOpenSSL in Include\OverbyteIcsDefs.inc determines whether the DCU is linked
or the external DLLs.  Note only one version of OpenSSL can be linked with YuOpenSSL,
whereas different DLLs can be supported.


NOTE OpenSSL 3.0.0 and later block SHA1, MD5, TLS1, TLS1.1 except for sslSecLevelAny
so some of our security levels need reviewing


Notes - OpenSSL ssleay32 changes between 1.0.2 and 1.1.0 - August 2016

file ssleay32.dll > libssl-1_1.dll and libssl-1_1-x64.dll

OpenSSL now auto initialises using OPENSSL_init_crypto and OPENSSL_init_ssl
so these are gone:
method SSL_library_init
method SSL_load_error_strings

method SSL_state > SSL_get_state (with different return value)

new version selection using:
TLS_client_method
TLS_method
TLS_server_method
SSL_set_min_proto_version
SSL_set_max_proto_version

Old exports gone:
SSLv3_method
SSLv3_client_method
SSLv3_server_method
SSLv23_method
SSLv23_client_method
SSLv23_server_method
All version specific TLSv1_1x_methods deprecated so don't load them either

Macros which are now new exported functions (not done until V8.51, sorry):
SSL_CTX_get_options;
SSL_get_options
SSL_CTX_clear_options
SSL_clear_options
SSL_CTX_set_options
SSL_set_options
SSL_session_reused

Notes - OpenSSL libssl-1_1 changes between 1.1.0 and 1.1.1 - November 2017
Missed some macro functions now exports.

Notes - OpenSSL libcrypto-3 changes from libcrypto-1_1 - June 2021
Exports renamed, adding 1/0, old names are kept as non-deprecated alias macros
SSL_get_peer_certificate to SSL_get1_peer_certificate


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$A8}

unit OverbyteIcsSSLEAY;

interface

{$IFDEF USE_SSL}

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Errno,
    Posix.SysTypes, { V8.65 for size_t }
    System.Types,   { V8.49 types needed for DWORD }
  {$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsTypes,
    OverbyteIcsUtils
    {$IFDEF YuOpenSSL}, YuOpenSSL{$ENDIF YuOpenSSL};

const
    IcsSSLEAYVersion   = 869;
    CopyRight : String = ' IcsSSLEAY (c) 2003-2022 F. Piette V8.69 ';

{$IF defined(YuOpenSSL) and not declared (OPENSSL_VERSION_MAJOR_)}   { V8.68 }
    OPENSSL_VERSION_MAJOR_ = 1;
    OPENSSL_VERSION_MINOR_ = 1;
    OPENSSL_VERSION_PATCH_ = 1;
    OPENSSL_VERSION_PRE_RELEASE_ = '';
{$IFEND}

    EVP_MAX_IV_LENGTH                 = 16;       { 03/02/07 AG }
    EVP_MAX_BLOCK_LENGTH              = 32;       { 11/08/07 AG }
    EVP_MAX_KEY_LENGTH                = 64;       { 11/08/07 AG  { V8.52 was 32 }

{ const - why were these variables ever declared as const??? }
{ V8.27 consolidated from LIBEAY so all in one place }
{ V8.65 POSIX not MACOS }
var
    GLIBEAY_DLL_Handle          : THandle = 0;
    GLIBEAY_110DLL_Name         : String  =
            {$IFDEF POSIX}'/usr/lib/libcrypto.dylib';{$ELSE}   { V8.32 !!!! not tested, unknown file name }
                {$IFDEF CPUX64}'libcrypto-1_1-x64.dll';{$ELSE}'libcrypto-1_1.dll';{$ENDIF} {$ENDIF}     { V8.27 }
    GLIBEAY_300DLL_Name         : String  =
            {$IFDEF POSIX}'/usr/lib/libcrypto.dylib';{$ELSE}   { !!!! not tested, unknown file name }
                {$IFDEF CPUX64}'libcrypto-3-x64.dll';{$ELSE}'libcrypto-3.dll';{$ENDIF} {$ENDIF}     { V8.67 }
    GSSLEAY_110DLL_Name         : String  =
            {$IFDEF POSIX}'/usr/lib/libssl.dylib';{$ELSE}
                {$IFDEF CPUX64}'libssl-1_1-x64.dll';{$ELSE}'libssl-1_1.dll';{$ENDIF}{$ENDIF}   { V8.27 }
    GSSLEAY_300DLL_Name         : String  =
            {$IFDEF POSIX}'/usr/lib/libssl.dylib';{$ELSE}
                {$IFDEF CPUX64}'libssl-3-x64.dll';{$ELSE}'libssl-3.dll';{$ENDIF}{$ENDIF}   { V8.67 }
    {$IFDEF YuOpenSSL}
    GLIBEAY_DLL_FileName        : String  = 'Statically Linked';  { V8.66 }
    GSSLEAY_DLL_FileName        : String  = 'Statically Linked';  { V8.66 }
    GSSLStaticLinked            : Boolean = True;                 { V8.66 }
    {$ELSE}
    GLIBEAY_DLL_FileName        : String  = '*NOT LOADED*';
    GSSLEAY_DLL_FileName        : String  = '*NOT_LOADED*';
    GSSLStaticLinked            : Boolean = False;                 { V8.66 }
    {$ENDIF YuOpenSSL}
    GSSLEAY_DLL_Handle          : THandle = 0;
    GSSLEAY_DLL_FileVersion     : String = '';
    GSSLEAY_DLL_FileDescription : String = '';
 { V8.65 don't attempt to find new name libcrypto-3_0.dll, use libcrypto-1_1.dll }
    GSSLEAY_DLL_IgnoreNew       : Boolean = False;
 { V8.69 don't attempt to use old name libcrypto-1_1.dll, use libcrypto-3_0.dll }
    GSSLEAY_DLL_IgnoreOld       : Boolean = False;  { V8.69 false, don't ignore 1.1 }
 { NOTE - both true now allowed } 
 { V8.27 write buffer size, was fixed at 4096, but send used a 16K buffer }
    GSSL_BUFFER_SIZE            : Integer = 16384;
 { V8.27 if set before OpenSSL loaded, will use this directory for DLLs, must have trailing \ }
    GSSL_DLL_DIR                : string = '';

 { V8.38 wintrust stuff for authenticode digital signing checking }
    GSSL_SignTest_Check         : Boolean = False;    { check OpenSSL DLLs are digitally signed }
    GSSL_SignTest_Certificate   : Boolean = False;    { False only checks hash, True also checks certificate (longer) }

    { Version stuff added 07/12/05  = V8.27 moved from OverbyteIcsLIBEAY  }
    ICS_OPENSSL_VERSION_NUMBER  : Cardinal = {$IFDEF YuOpenSSL}YuOpenSSL.OPENSSL_VERSION_NUMBER{$ELSE}0{$ENDIF};
    ICS_SSL_NO_RENEGOTIATION    : Boolean = TRUE;    { V8.67 we removed renegotiation }
    ICS_RAND_INIT_DONE          : Boolean = FALSE;   { V8.35 have we initialised random numbers }

  { V8.67 note ICS_OPENSSL_VERSION_NUMBER is deprecated in OpenSSL 3.0 and later, use OPENSSL_VERSION_xx macros instead }
  { note need ICS prefix since OPENSSL_version_major is an export and we are case insensitive }
    ICS_OPENSSL_VERSION_MAJOR   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_MAJOR_{$ELSE}0{$ENDIF};           { V8.67 1 or 3, macros in OpenSSL 3.0 }
    ICS_OPENSSL_VERSION_MINOR   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_MINOR_{$ELSE}0{$ENDIF};           { V8.67 0 >  }
    ICS_OPENSSL_VERSION_PATCH   : Integer = {$IFDEF YuOpenSSL}OPENSSL_VERSION_PATCH_{$ELSE}0{$ENDIF};           { V8.67 0 >  }
    ICS_OPENSSL_VERSION_PRE_RELEASE : String = {$IFDEF YuOpenSSL}OPENSSL_VERSION_PRE_RELEASE_{$ELSE}''{$ENDIF}; { V8.67 0 >  }
    ICS_OSSL3_LOADED_LEGACY     : Boolean = False;   { V8.67 is OpenSSL 3.0 legacy provider loaded }
    ICS_OSSL3_LOADED_FIPS       : Boolean = False;   { V8.67 is OpenSSL 3.0 FIPS provider loaded }
    GSSLEAY_LOAD_LEGACY         : Boolean = False;   { V8.67 should legacy provider be loaded on start-up }
    GSSLEAY_PROVIDER_DEFAULT    : Pointer;           { V8.67 POSSL_PROVIDER for default provider, if loaded }
    GSSLEAY_PROVIDER_LEGACY     : Pointer;           { V8.67 POSSL_PROVIDER for legacy provider, if loaded }
    GSSLEAY_PROVIDER_FIPS       : Pointer;           { V8.67 POSSL_PROVIDER for FIPS provider, if loaded }


  { V8.62 dynamically added NID objects, if any set need call OBJ_cleanup on close down }
    ICS_NID_acmeIdentifier      : Integer = 0;


const
 { found in \include\openssl\opensslv.h, moved to \VERSION.dat for 3.0
{ only supporting versions with TLS 1.1, 1.2 and 1.3 }
{ last digit is 0 for dev/beta, F for final release }
{ V8.27 moved from OverbyteIcsLIBEAY  }
{ V8.66 1.0.2 and 1.1.0 support ceased Dec 2019, now removed from ICS }
    OSSL_VER_MIN    = $0000000F; // minimum version     { V8.35 }
    OSSL_VER_1101   = $1010100F; // 1.1.1 base                { V8.57 }
    OSSL_VER_1101A  = $1010101F; // 1.1.1a                    { V8.59 }
    OSSL_VER_1101B  = $1010102F; // 1.1.1b                    { V8.59 }
    OSSL_VER_1101C  = $1010103F; // 1.1.1c                    { V8.64 }
    OSSL_VER_1101D  = $1010104F; // 1.1.1d                    { V8.65 }
    OSSL_VER_1101E  = $1010105F; // 1.1.1e                    { V8.65 }
    OSSL_VER_1101F  = $1010106F; // 1.1.1f                    { V8.65 }
    OSSL_VER_1101G  = $1010107F; // 1.1.1g                    { V8.65 }
    OSSL_VER_1101H  = $1010108F; // 1.1.1h                    { V8.65 }
    OSSL_VER_1101ZZ = $10101FFF; // 1.1.1zz not yet released  { V8.57 }
    OSSL_VER_3000   = $30000000; // 3.0.0 base                { V8.65 }
    OSSL_VER_3LAST  = $3FFFFFFF; // 3 last                    { V8.67 }
    OSSL_VER_MAX    = $FFFFFFFF; // maximum version           { V8.35 }

    { Basically versions listed above are tested if not otherwise commented.  }
    { Versions between are assumed to work, however they are untested.        }
    { OpenSSL libraries for ICS are available for download here:              }
    { http://wiki.overbyte.be/wiki/index.php/ICS_Download                     }

    MIN_OSSL_VER   = OSSL_VER_1101;   { V8.66 minimum is now 1.1.1 }
//    MAX_OSSL_VER   = OSSL_VER_1101ZZ; { V8.57 1.1.1zz }
    MAX_OSSL_VER   = OSSL_VER_3LAST;    { V8.67 }

    { V8.41 PEM base64 file titles }
    PEM_STRING_HDR_BEGIN   = '-----BEGIN ';    { six hyphens }
    PEM_STRING_HDR_END     = '-----END ';
    PEM_STRING_HDR_TAIL    = '-----'+#13#10;
    PEM_STRING_X509_OLD    = 'X509 CERTIFICATE' ;
    PEM_STRING_X509        = 'CERTIFICATE' ;
    PEM_STRING_X509_TRUSTED= 'TRUSTED CERTIFICATE' ;
    PEM_STRING_X509_REQ_OLD= 'NEW CERTIFICATE REQUEST' ;
    PEM_STRING_X509_REQ    = 'CERTIFICATE REQUEST' ;
    PEM_STRING_X509_CRL    = 'X509 CRL' ;
    PEM_STRING_EVP_PKEY    = 'ANY PRIVATE KEY' ;
    PEM_STRING_PUBLIC      = 'PUBLIC KEY' ;
    PEM_STRING_RSA         = 'RSA PRIVATE KEY' ;
    PEM_STRING_RSA_PUBLIC  = 'RSA PUBLIC KEY' ;
    PEM_STRING_DSA         = 'DSA PRIVATE KEY' ;
    PEM_STRING_DSA_PUBLIC  = 'DSA PUBLIC KEY' ;
    PEM_STRING_PKCS7       = 'PKCS7' ;
    PEM_STRING_PKCS7_SIGNED= 'PKCS #7 SIGNED DATA' ;
    PEM_STRING_PKCS8       = 'ENCRYPTED PRIVATE KEY' ;
    PEM_STRING_PKCS8INF    = 'PRIVATE KEY' ;
    PEM_STRING_DHPARAMS    = 'DH PARAMETERS' ;
    PEM_STRING_DHXPARAMS   = 'X9.42 DH PARAMETERS' ;
    PEM_STRING_SSL_SESSION = 'SSL SESSION PARAMETERS' ;
    PEM_STRING_DSAPARAMS   = 'DSA PARAMETERS' ;
    PEM_STRING_ECDSA_PUBLIC= 'ECDSA PUBLIC KEY' ;
    PEM_STRING_ECPARAMETERS= 'EC PARAMETERS' ;
    PEM_STRING_ECPRIVATEKEY= 'EC PRIVATE KEY' ;
    PEM_STRING_PARAMETERS  = 'PARAMETERS' ;
    PEM_STRING_CMS         = 'CMS' ;

{ V8.56 TLS Application-Layer Protocol Negotiation (ALPN) Protocol IDs }
{ received from one client: h2,h2-14,h2-15,h2-16,h2-17,spdy/1,spdy/2,spdy/3,spdy/3.1,spdy/4,http/1.1,h2-fb,webrtc,c-webrtc,ftp }
    ALPN_ID_HTTP10        = 'http/1.0';
    ALPN_ID_HTTP11        = 'http/1.1';
    ALPN_ID_HTTP2         = 'h2';
    ALPN_ID_HTTP2S        = 'h2s';
    ALPN_ID_HTTP214       = 'h2-14';   { and -15, -16, -17 }
//    ALPN_ID_SPDY1         = 'spdy/1';
//    ALPN_ID_SPDY2         = 'spdy/2';
    ALPN_ID_SPDY3         = 'spdy/3';
    ALPN_ID_SPDY31        = 'spdy/3.1';
    ALPN_ID_TURN          = 'stun.turn';
    ALPN_ID_STUN          = 'stun.nat-discovery';
    ALPN_ID_WEBRTC        = 'webrtc';
    ALPN_ID_CWEBRTC       = 'c-webrtc';
    ALPN_ID_FTP           = 'ftp';
    ALPN_ID_IMAP          = 'imap';
    ALPN_ID_POP3          = 'pop3';
    ALPN_ID_ACME_TLS1     = 'acme-tls/1';


type

    TOSSLImports = record   { V8.35 }
        F: PPointer;   // function pointer
        N: PAnsiChar;  // export name
        MI: Cardinal;  // minimum OpenSSL version
        MX: Cardinal;  // maximum OpenSSL version
    end;

    EIcsSsleayException = class(Exception);
    PPChar   = ^PChar;
    PPAnsiChar = ^PAnsiChar;
    //PInteger = ^Integer;
    PPByte = ^PByte;              { V8.66 }
    PCRYPTO_THREADID = Pointer; { V8.66 ^TCRYPTO_THREADID_st;  }
{$IFNDEF OPENSSL_NO_ENGINE}
    PENGINE = Pointer; { V8.66 ^TEngine_st; }
{$ENDIF}
    PSSL            = Pointer; { V8.66 ^TSSL_st;}
    PPSSL           = ^PSSL;    { V8.51 }
    PSSL_SESSION    = Pointer; { V8.66 ^TSSL_SESSION_st; }
    PPSSL_SESSION   = ^PSSL_SESSION;
    PBIO            = Pointer; { V8.66 ^TBIO_st; }
    PPBIO           = ^PBIO;
    PBIO_METHOD     = Pointer; { V8.66 ^TBIO_METHOD_st; }
    PSSL_CTX        = Pointer; { V8.66 ^TSSL_CTX_st; }
    PSSL_METHOD     = Pointer; { V8.66 ^TSSL_METHOD_st; }
    PSSL_CIPHER     = Pointer; { V8.66 ^TSSL_CIPHER_st; }
    PX509_STORE = Pointer; { V8.66 ^TX509_STORE_st; }
    PX509_STORE_CTX = Pointer; { V8.66 ^TX509_STORE_CTX_st; }
    PX509_NAME = Pointer; { V8.66 ^TX509_NAME_st;  }
    BN_ULONG = Cardinal;               { V8.03 }
    PBN_ULONG = ^BN_ULONG;

    TBIGNUM_st = packed record         { V8.03 }
        d: PBN_ULONG;                  { V8.52 need for array }
        top: Integer;
        dmax: Integer;
        neg: Integer;
        flags: Integer;
    end;
    PBIGNUM =  Pointer; { V8.66 ^TBIGNUM_st; }
    PPBIGNUM = ^PBIGNUM;                    { V8.52 }
    TBIGNUMS = array [0..0] of TBIGNUM_st;  { V8.52 }
    PBIGNUMS = ^TBIGNUMS;                   { V8.52 }
    PPBIGNUMS = ^PBIGNUMS;                  { V8.52 }
    PSTACK = Pointer; { V8.66 ^TSTACK_st; }
    PASN1_TYPE = Pointer; { V8.66 ^TASN1_TYPE_st; }
    PX509_VERIFY_PARAM = Pointer; { V8.66 ^TX509_VERIFY_PARAM_st; }
    PBN_CTX = Pointer; { V8.66 ^TBN_CTX_st; }
    PEC_GROUP = Pointer; { V8.66 ^TEC_GROUP_st; }
    PEC_METHOD = Pointer; { V8.66 ^TEC_METHOD_st; }

    TEC_POINT_st = packed record                  { V8.40 }
     //   Dummy : array [0..0] of Byte;
        meth: PEC_METHOD;                  { V8.52 need full data for array }
        X: PBIGNUM;
        Y: PBIGNUM;
        Z: PBIGNUM;
        Z_is_one: Integer;
    end;
    PEC_POINT = ^TEC_POINT_st;
    TEC_POINTS = array[0..0] of TEC_POINT_st;    { V8.52 }
    PEC_POINTS = ^TEC_POINTS;                    { V8.52 }
    PEC_PKPARAMETERS = Pointer; { V8.66 ^TEC_PKPARAMETERS_st;  }
    PEC_PARAMETERS = Pointer; { V8.66 ^TEC_PARAMETERS_st; }

    TEC_BUILTIN_CURVE_st = packed record          { V8.40  }
        nid: Integer;
        comment: PAnsiChar;
    end;
    PEC_BUILTIN_CURVE = ^TEC_BUILTIN_CURVE_st;
    TEC_BUILTIN_CURVES = array of TEC_BUILTIN_CURVE_st;
    PBN_MONT_CTX = Pointer; { V8.66 ^TBN_MONT_CTX_st; }
    PSTACK_OF_X509_EXTENSION    = PStack;     //AG
    PPSTACK_OF_X509_EXTENSION   = ^PSTACK_OF_X509_EXTENSION; { V8.41 }
    PSTACK_OF_X509_ALGOR        = PStack;     //AG
    PSTACK_OF_X509              = PSTACK;     //AG
    PSTACK_OF_X509_CRL          = PSTACK;     //AG
    PPSTACK_OF_X509             = ^PSTACK_OF_X509; //AG
    PSTACK_OF_PKCS7_RECIP_INFO  = PStack;     //AG
    PSTACK_OF_X509_ATTRIBUTE    = PStack;     //AG
    PSTACK_OF_PKCS7_SIGNER_INFO = PStack;     //AG
    PSTACK_OF_509_LOOKUP        = PStack;     //AG
    PSTACK_OF_X509_OBJECT       = PStack;     //AG
    PSTACK_OF_X509_NAME = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;
    PSTACK_OF_X509_INFO = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;
    PSTACK_OF_X509_VERIFY_PARAM = PStack;     { V8.39 }
    PSTACK_OF_SSL_CIPHER        = PSTACK;                 { V8.27 }
    PPSTACK_OF_SSL_CIPHER       = ^PSTACK_OF_SSL_CIPHER;  { V8.27 }
    PCRYPTO_EX_DATA             = PSTACK;                 { V8.40 }
    PX509_LOOKUP_METHOD = Pointer; { V8.66 ^TX509_lookup_method_st; }
    PX509_LOOKUP = Pointer; { V8.66 ^TX509_lookup_st; }
    PX509_OBJECT = Pointer; { V8.66 ^TX509_OBJECT_st;  }
    TX509_LOOKUP_TYPE = (X509_LU_NONE, X509_LU_X509, X509_LU_CRL);  { V8.39 }
    PX509_NAME_ENTRY = Pointer; { V8.66 ^TX509_NAME_ENTRY_st; }
    PEVP_MD = Pointer; { V8.66 ^TEVP_MD_st; }
    PEVP_MD_CTX = Pointer; { V8.66 ^TEVP_MD_CTX_st; }
    PPEVP_MD_CTX = ^PEVP_MD_CTX;        { V8.66 }
    PEVP_PKEY_CTX = Pointer; { V8.66 ^TEVP_PKEY_CTX_st; }
    PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;       { V8.66 }
    PRSA = Pointer; { V8.66 ^TRSA_st;  }
    PPRSA = ^PRSA;                        { V8.03 }
    TCRYPTO_EX_DATA = record
        sk: PSTACK;
        dummy: Integer;
    end;
    PDSA = Pointer; { V8.66 ^TDSA_st; }
    PDH = Pointer; { V8.66 ^TDH_st; }
    PPDH = ^PDH;                        { V8.40 }
    PEC_KEY = Pointer; { V8.66 ^TEC_KEY_st; }
    PPEC_KEY = ^PEC_KEY;            { V8.66 }
    PEVP_PKEY = Pointer; { V8.66 ^TEVP_PKEY_st;  }
    PPEVP_PKEY = ^PEVP_PKEY;
    PEVP_CIPHER = Pointer; { V8.66 ^TEVP_CIPHER_st;  }
    PASN1_OBJECT = Pointer; { V8.66 ^TASN1_OBJECT_st; }
    PSTACK_OF_ASN1_OBJECT = PStack;     { V8.39 }

    TX509_ALGOR_st = record
        algorithm : PASN1_OBJECT;
        parameter : PASN1_TYPE;
    end;
    PX509_ALGOR = ^TX509_ALGOR_st;
    PX509_PURPOSE = Pointer; { V8.66 ^TX509_PURPOSE_st; }

    TASN1_STRING_st = record
        length : Integer;
        type_  : Integer;
        data   : PAnsiChar;
        //* The value of the following field depends on the type being
        //* held.  It is mostly being used for BIT_STRING so if the
        //* input data has a non-zero 'unused bits' value, it will be
        //* handled correctly */
        flags  : Cardinal;
    end;
    PASN1_STRING       = Pointer; { V8.66 ^TASN1_STRING_st; }
    PASN1_OCTET_STRING = Pointer; { V8.66 ^TASN1_OCTET_STRING; }
    PASN1_BIT_STRING   = Pointer; { V8.66 ^TASN1_BIT_STRING; }
    PASN1_TIME = Pointer; { V8.66 ^TASN1_TIME;  }
    PASN1_INTEGER = Pointer; { V8.66 ^TASN1_INTEGER; }
    PASN1_VALUE  = Pointer; { V8.66 ^TASN1_VALUE_st;  }
    PPASN1_VALUE = ^PASN1_VALUE;
    PASN1_GENERALIZEDTIME = Pointer; { V8.69 }

    Tasn1_pctx_st = packed record      { V8.40 }
        flags: DWORD;
        nm_flags: DWORD;
        cert_flags: DWORD;
        oid_flags: DWORD;
        str_flags: DWORD;
    end;
    PASN1_PCTX = ^Tasn1_pctx_st;       { V8.40 }

    POTHERNAME = ^TOTHERNAME;           { V8.50 }
    PPOTHERNAME = ^POTHERNAME;
    TOTHERNAME = record
        type_id: PASN1_OBJECT;
        value: PASN1_TYPE;
    end;

    PEDIPARTYNAME = ^TEDIPARTYNAME;     { V8.50 }
    PPEDIPARTYNAME = ^PEDIPARTYNAME;
    TEDIPARTYNAME = record
        nameAssigner: PASN1_STRING;
        partyName: PASN1_STRING;
    end;

    TASN1_IA5STRING = TASN1_STRING_st;     { V8.50 }
    PASN1_IA5STRING = ^TASN1_IA5STRING;
const
    GEN_OTHERNAME  = 0;          { V8.40 type of GENERAL_NAME }
    GEN_EMAIL      = 1;
    GEN_DNS        = 2;
    GEN_X400       = 3;
    GEN_DIRNAME    = 4;
    GEN_EDIPARTY   = 5;
    GEN_URI        = 6;
    GEN_IPADD      = 7;
    GEN_RID        = 8;
type
    TGENERAL_NAME_st  = packed record     { V8.40 }
        Dummy : array [0..0] of Byte;
    end;
    PGENERAL_NAME  = Pointer; { V8.66 ^TGENERAL_NAME_st; }
    PEVP_CIPHER_INFO = Pointer; { V8.66 ^EVP_CIPHER_INFO; }
    PX509_PKEY = Pointer; { V8.66 ^TPrivate_key_st; }
    PX509_CRL_INFO = Pointer; { V8.66 ^TX509_CRL_INFO_st; }
    PX509_CRL = Pointer; { V8.66 ^TX509_CRL_st; }
    PPX509_CRL = ^PX509_CRL;
    PX509  = Pointer; { V8.66 ^TX509_st;  }
    PPX509 = ^PX509;
    PX509_INFO = Pointer; { V8.66 ^TX509_INFO_st; }
    PX509_VAL = Pointer; { V8.66 ^TX509_VAL_st; }
    PX509_PUBKEY = Pointer; { V8.66 ^TX509_PUBKEY_st; }
    PX509_CINF = Pointer; { V8.66 ^TX509_CINF_st; }
    PX509_EXTENSION = Pointer; { V8.66 ^TX509_EXTENSION_st;  }

// V8.35 moved lots of declarations from OverbyteIcsLibeayEx so they are all together
type
    PEVP_CIPHER_CTX = Pointer; { V8.66 ^TEVP_CIPHER_CTX_st;  }
{$IFDEF OPENSSL_NO_ENGINE}
    PEngine = Pointer; { V8.66 ^TEngine_st; }
{$ENDIF}

    TASN1_ENCODING_st = packed record
        enc       : PAnsiChar;
        len       : Cardinal;
        modified  : Integer;
    end;
    TASN1_ENCODING = TASN1_ENCODING_st;
    PASN1_ENCODING = ^TASN1_ENCODING_st;
    PLHASH = Pointer; { V8.66 ^TLHASH_st; }
    PX509V3_CTX = Pointer; { V8.66 ^TX509V3_CTX_st; }
    PX509_REQ_INFO = Pointer; { V8.66 ^TX509_REQ_INFO_st; }
    PX509_REQ = Pointer; { V8.66 ^TX509_REQ_st;  }
    PPX509_REQ = ^PX509_REQ;            { V8.40 }

    TECDSA_SIG_st = packed record       { V8.40 }
        r  : PBIGNUM;
        s  : PBIGNUM;
    end;
    ECDSA_SIG = TECDSA_SIG_st;
    PECDSA_SIG = ^TECDSA_SIG_st;
    PBN_GENCB = Pointer; { V8.66 ^TBN_GENCB; }
    PPKCS7_ISSUER_AND_SERIAL = Pointer; { V8.66 ^TPKCS7_ISSUER_AND_SERIAL_st; }
    PPKCS7_ENC_CONTENT = Pointer; { V8.66 ^TPKCS7_ENC_CONTENT_st; }
    PPKCS7_DIGEST = Pointer; { V8.66 ^TPKCS7_DIGEST_st; }
    PPKCS7_ENCRYPT = Pointer; { V8.66 ^TPKCS7_ENCRYPT_st; }
    PPPKCS7 = Pointer; { V8.66 ^PPKCS7; }
    PPKCS12 = Pointer; { V8.66 ^TPKCS12_st; }
    PPPKCS12 = ^PPKCS12;
    PV3_EXT_CTX = Pointer; { V8.66 ^TV3_EXT_CTX_st; }
    TCONF_VALUE = record
        Section : PAnsiChar;
        Name    : PAnsiChar;
        Value   : PAnsiChar;
    end;
    PCONF_VALUE = ^TCONF_VALUE;
    PASN1_ITEM  = Pointer; { V8.66 ^TASN1_ITEM_st;  }

{ V8.69 OCSP types to check certificates, all really record structures }
    POCSP_BASICRESP      = Pointer;
    POCSP_CERTID         = Pointer;
    POCSP_CERTSTATUS     = Pointer;
    POCSP_CRLID          = Pointer;
    POCSP_ONEREQ         = Pointer;
    POCSP_REQUEST        = Pointer;
    POCSP_RESPDATA       = Pointer;
    POCSP_RESPBYTES      = Pointer;
    POCSP_RESPONSE       = Pointer;
    POCSP_RESPID         = Pointer;
    POCSP_REVOKEDINFO    = Pointer;
    POCSP_SERVICELOC     = Pointer;
    POCSP_SIGNATURE      = Pointer;
    POCSP_SINGLERESP     = Pointer;



{ V8.67 new objects for OpenSSL 3.0 mostly for providers }
{ core.h }
{* Base types
 * ----------
 *
 * These are the types that the OpenSSL core and providers have in common
 * to communicate data between them.}

{ Opaque handles to be used with core upcall functions from providers }
type
    OSSL_CORE_HANDLE = Pointer;  // ossl_core_handle_st ;
    OPENSSL_CORE_CTX = Pointer;  // openssl_core_ctx_st
    OSSL_CORE_BIO = Pointer;     // ossl_core_bio_st

{* Dispatch table element.  function_id numbers and the functions are defined
 * in core_dispatch.h, see macros with 'OSSL_CORE_MAKE_FUNC' in their names.
 *
 * An array of these is always terminated by function_id == 0 }
    Tossl_dispatch_st = record
        function_id: Integer;
        functionptr: Pointer;
    end;

{* Other items, essentially an int<->pointer map element.
 *
 * We make this type distinct from OSSL_DISPATCH to ensure that dispatch
 * tables remain tables with function pointers only.
 *
 * This is used whenever we need to pass things like a table of error reason
 * codes <-> reason string maps, ...
 *
 * Usage determines which field works as key if any, rather than field order.
 *
 * An array of these is always terminated by id == 0 && ptr == NULL }
    Tossl_item_st = record
        id: LongWord;
        ptr: Pointer;
   end;

{* Type to tie together algorithm names, property definition string and
 * the algorithm implementation in the form of a dispatch table.
 *
 * An array of these is always terminated by algorithm_names == NULL }
    Tossl_algorithm_st = record
        algorithm_names: PAnsiChar;         { key }
        property_definition: PAnsiChar;     { key }
        implementationptr: Pointer;  { POSSL_DISPATCH;  }
        algorithm_description: PAnsiChar;
    end;

{ Type to pass object data in a uniform way, without exposing the object
 * structure.
 *
 * An array of these is always terminated by key == NULL }
    ossl_param_st = record
        key: PAnsiChar;              { the name of the parameter }
        data_type: LongWord;         { declare what kind of content is in buffer }
        data: Pointer;               { value being passed in or out }
        data_size: size_t;           { data size }
        return_size: size_t;         { returned content size }
    end;
    Tossl_param_st = ossl_param_st;  { V8.68 }

const

{ Currently supported OSSL_PARAM data types */
/*
 * OSSL_PARAM_INTEGER and OSSL_PARAM_UNSIGNED_INTEGER
 * are arbitrary length and therefore require an arbitrarily sized buffer,
 * since they may be used to pass numbers larger than what is natively
 * available.
 *
 * The number must be buffered in native form, i.e. MSB first on B_ENDIAN
 * systems and LSB first on L_ENDIAN systems.  This means that arbitrary
 * native integers can be stored in the buffer, just make sure that the
 * buffer size is correct and the buffer itself is properly aligned (for
 * example by having the buffer field point at a C integer). }
    OSSL_PARAM_INTEGER             = 1;
    OSSL_PARAM_UNSIGNED_INTEGER    = 2;
{* OSSL_PARAM_REAL
 * is a C binary floating point values in native form and alignment. }
    OSSL_PARAM_REAL                = 3;
{ OSSL_PARAM_UTF8_STRING
 * is a printable string.  It is expected to be printed as it is. }
    OSSL_PARAM_UTF8_STRING         = 4;
{* OSSL_PARAM_OCTET_STRING
 * is a string of bytes with no further specification.  It is expected to be
 * printed as a hexdump. }
    OSSL_PARAM_OCTET_STRING        = 5;
{* OSSL_PARAM_UTF8_PTR
 * is a pointer to a printable string.  It is expected to be printed as it is.
 *
 * The difference between this and OSSL_PARAM_UTF8_STRING is that only pointers
 * are manipulated for this type.
 *
 * This is more relevant for parameter requests, where the responding
 * function doesn't need to copy the data to the provided buffer, but
 * sets the provided buffer to point at the actual data instead.
 *
 * WARNING!  Using these is FRAGILE, as it assumes that the actual
 * data and its location are constant.
 *
 * EXTRA WARNING!  If you are not completely sure you most likely want
 * to use the OSSL_PARAM_UTF8_STRING type.  }
    OSSL_PARAM_UTF8_PTR            = 6;
{* OSSL_PARAM_OCTET_PTR
 * is a pointer to a string of bytes with no further specification.  It is
 * expected to be printed as a hexdump.
 *
 * The difference between this and OSSL_PARAM_OCTET_STRING is that only pointers
 * are manipulated for this type.
 *
 * This is more relevant for parameter requests, where the responding
 * function doesn't need to copy the data to the provided buffer, but
 * sets the provided buffer to point at the actual data instead.
 *
 * WARNING!  Using these is FRAGILE, as it assumes that the actual
 * data and its location are constant.
 *
 * EXTRA WARNING!  If you are not completely sure you most likely want
 * to use the OSSL_PARAM_OCTET_STRING type. }
    OSSL_PARAM_OCTET_PTR           = 7;

{ corenames.h }
{ Well known parameter names that core passes to providers }
	OSSL_PROV_PARAM_CORE_VERSION            = 'openssl-version'; { utf8_ptr }
	OSSL_PROV_PARAM_CORE_PROV_NAME          = 'provider-name';   { utf8_ptr }
	OSSL_PROV_PARAM_CORE_MODULE_FILENAME    = 'module-filename'; { utf8_ptr }

{ Well known parameter names that Providers can define }
	OSSL_PROV_PARAM_NAME            = 'name';                { utf8_string }
	OSSL_PROV_PARAM_VERSION         = 'version';             { utf8_string }
	OSSL_PROV_PARAM_BUILDINFO       = 'buildinfo';           { utf8_string }
	OSSL_PROV_PARAM_STATUS          = 'status';              { uint }
	OSSL_PROV_PARAM_SECURITY_CHECKS = 'security-checks';     { uint }

{ Self test callback parameters }
	OSSL_PROV_PARAM_SELF_TEST_PHASE = 'st-phase'; { utf8_string }
	OSSL_PROV_PARAM_SELF_TEST_TYPE  = 'st-type';  { utf8_string }
	OSSL_PROV_PARAM_SELF_TEST_DESC  = 'st-desc';  { utf8_string }

{* Provider-native object abstractions
 *
 * These are used when a provider wants to pass object data or an object
 * reference back to libcrypto.  This is only useful for provider functions
 * that take a callback to which an OSSL_PARAM array with these parameters
 * can be passed.
 *
 * This set of parameter names is explained in detail in provider-object(7)
 * (doc/man7/provider-object.pod)
 }
	OSSL_OBJECT_PARAM_TYPE           = 'type';      { INTEGER }
	OSSL_OBJECT_PARAM_DATA_TYPE      = 'data-type'; { UTF8_STRING }
	OSSL_OBJECT_PARAM_DATA_STRUCTURE = 'data-structure'; { UTF8_STRING }
	OSSL_OBJECT_PARAM_REFERENCE      = 'reference'; { OCTET_STRING }
	OSSL_OBJECT_PARAM_DATA           = 'data'; { OCTET_STRING or UTF8_STRING }
	OSSL_OBJECT_PARAM_DESC           = 'desc';      { UTF8_STRING }

{
 * Algorithm parameters
 * If= 'engine'; or= 'properties'; are specified, they should always be paired
 * with the algorithm type.
 * Note these are common names that are shared by many types (such as kdf, mac,
 * and pkey) e.g: see OSSL_MAC_PARAM_DIGEST below.
 }
	OSSL_ALG_PARAM_DIGEST       = 'digest';    { utf8_string }
	OSSL_ALG_PARAM_CIPHER       = 'cipher';    { utf8_string }
	OSSL_ALG_PARAM_ENGINE       = 'engine';    { utf8_string }
	OSSL_ALG_PARAM_MAC          = 'mac';       { utf8_string }
	OSSL_ALG_PARAM_PROPERTIES   = 'properties';{ utf8_string }

{ cipher parameters }
	OSSL_CIPHER_PARAM_PADDING           = 'padding';      { uint }
	OSSL_CIPHER_PARAM_USE_BITS          = 'use-bits';     { uint }
	OSSL_CIPHER_PARAM_TLS_VERSION       = 'tls-version';  { uint }
	OSSL_CIPHER_PARAM_TLS_MAC           = 'tls-mac';      { octet_ptr }
	OSSL_CIPHER_PARAM_TLS_MAC_SIZE      = 'tls-mac-size'; { size_t }
	OSSL_CIPHER_PARAM_MODE              = 'mode';         { uint }
	OSSL_CIPHER_PARAM_BLOCK_SIZE        = 'blocksize';    { size_t }
	OSSL_CIPHER_PARAM_AEAD              = 'aead';         { int, 0 or 1 }
	OSSL_CIPHER_PARAM_CUSTOM_IV         = 'custom-iv';    { int, 0 or 1 }
	OSSL_CIPHER_PARAM_CTS               = 'cts';          { int, 0 or 1 }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK   = 'tls-multi';    { int, 0 or 1 }
	OSSL_CIPHER_PARAM_HAS_RAND_KEY      = 'has-randkey';  { int, 0 or 1 }
	OSSL_CIPHER_PARAM_KEYLEN            = 'keylen';       { size_t }
	OSSL_CIPHER_PARAM_IVLEN             = 'ivlen';        { size_t }
	OSSL_CIPHER_PARAM_IV                = 'iv';           { octet_string OR octet_ptr }
	OSSL_CIPHER_PARAM_UPDATED_IV        = 'updated-iv';   { octet_string OR octet_ptr }
	OSSL_CIPHER_PARAM_NUM               = 'num';          { uint }
	OSSL_CIPHER_PARAM_ROUNDS            = 'rounds';       { uint }
	OSSL_CIPHER_PARAM_AEAD_TAG          = 'tag';          { octet_string }
	OSSL_CIPHER_PARAM_AEAD_TLS1_AAD     = 'tlsaad';       { octet_string }
	OSSL_CIPHER_PARAM_AEAD_TLS1_AAD_PAD     = 'tlsaadpad';    { size_t }
	OSSL_CIPHER_PARAM_AEAD_TLS1_IV_FIXED    = 'tlsivfixed';   { octet_string }
	OSSL_CIPHER_PARAM_AEAD_TLS1_GET_IV_GEN  = 'tlsivgen';     { octet_string }
	OSSL_CIPHER_PARAM_AEAD_TLS1_SET_IV_INV  = 'tlsivinv';     { octet_string }
	OSSL_CIPHER_PARAM_AEAD_IVLEN        = OSSL_CIPHER_PARAM_IVLEN;
	OSSL_CIPHER_PARAM_AEAD_TAGLEN       = 'taglen';       { size_t }
	OSSL_CIPHER_PARAM_AEAD_MAC_KEY      = 'mackey';       { octet_string }
	OSSL_CIPHER_PARAM_RANDOM_KEY        = 'randkey';      { octet_string }
	OSSL_CIPHER_PARAM_RC2_KEYBITS       = 'keybits';      { size_t }
	OSSL_CIPHER_PARAM_SPEED             = 'speed';        { uint }
	OSSL_CIPHER_PARAM_CTS_MODE          = 'cts_mode';     { utf8_string }
{ For passing the AlgorithmIdentifier parameter in DER form }
	OSSL_CIPHER_PARAM_ALGORITHM_ID_PARAMS   = 'alg_id_param'; { octet_string }

	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_MAX_SEND_FRAGMENT     = 'tls1multi_maxsndfrag'; { uint }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_MAX_BUFSIZE           = 'tls1multi_maxbufsz';   { size_t }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_INTERLEAVE            = 'tls1multi_interleave'; { uint }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_AAD                   = 'tls1multi_aad';        { octet_string }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_AAD_PACKLEN           = 'tls1multi_aadpacklen'; { uint }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_ENC                   = 'tls1multi_enc';        { octet_string }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_ENC_IN                = 'tls1multi_encin';      { octet_string }
	OSSL_CIPHER_PARAM_TLS1_MULTIBLOCK_ENC_LEN                = 'tls1multi_enclen';     { size_t }

{ OSSL_CIPHER_PARAM_CTS_MODE Values }
	OSSL_CIPHER_CTS_MODE_CS1    = 'CS1';
	OSSL_CIPHER_CTS_MODE_CS2    = 'CS2';
	OSSL_CIPHER_CTS_MODE_CS3    = 'CS3';

{ digest parameters }
	OSSL_DIGEST_PARAM_XOFLEN        = 'xoflen';        { size_t }
	OSSL_DIGEST_PARAM_SSL3_MS       = 'ssl3-ms';       { octet string }
	OSSL_DIGEST_PARAM_PAD_TYPE      = 'pad-type';      { uint }
	OSSL_DIGEST_PARAM_MICALG        = 'micalg';        { utf8 string }
	OSSL_DIGEST_PARAM_BLOCK_SIZE    = 'blocksize';     { size_t }
	OSSL_DIGEST_PARAM_SIZE          = 'size';          { size_t }
	OSSL_DIGEST_PARAM_XOF           = 'xof';           { int, 0 or 1 }
	OSSL_DIGEST_PARAM_ALGID_ABSENT  = 'algid-absent';  { int, 0 or 1 }

{ Known DIGEST names (not a complete list) }
	OSSL_DIGEST_NAME_MD5            = 'MD5';
	OSSL_DIGEST_NAME_MD5_SHA1       = 'MD5-SHA1';
	OSSL_DIGEST_NAME_SHA1           = 'SHA1';
	OSSL_DIGEST_NAME_SHA2_224       = 'SHA2-224';
	OSSL_DIGEST_NAME_SHA2_256       = 'SHA2-256';
	OSSL_DIGEST_NAME_SHA2_384       = 'SHA2-384';
	OSSL_DIGEST_NAME_SHA2_512       = 'SHA2-512';
	OSSL_DIGEST_NAME_SHA2_512_224   = 'SHA2-512/224';
	OSSL_DIGEST_NAME_SHA2_512_256   = 'SHA2-512/256';
	OSSL_DIGEST_NAME_MD2            = 'MD2';
	OSSL_DIGEST_NAME_MD4            = 'MD4';
	OSSL_DIGEST_NAME_MDC2           = 'MDC2';
	OSSL_DIGEST_NAME_RIPEMD160      = 'RIPEMD160';
	OSSL_DIGEST_NAME_SHA3_224       = 'SHA3-224';
	OSSL_DIGEST_NAME_SHA3_256       = 'SHA3-256';
	OSSL_DIGEST_NAME_SHA3_384       = 'SHA3-384';
	OSSL_DIGEST_NAME_SHA3_512       = 'SHA3-512';
	OSSL_DIGEST_NAME_KECCAK_KMAC128 = 'KECCAK-KMAC-128';
	OSSL_DIGEST_NAME_KECCAK_KMAC256 = 'KECCAK-KMAC-256';
	OSSL_DIGEST_NAME_SM3            = 'SM3';

{ MAC parameters }
	OSSL_MAC_PARAM_KEY              = 'key';            { octet string }
	OSSL_MAC_PARAM_IV               = 'iv';             { octet string }
	OSSL_MAC_PARAM_CUSTOM           = 'custom';         { utf8 string }
	OSSL_MAC_PARAM_SALT             = 'salt';           { octet string }
	OSSL_MAC_PARAM_XOF              = 'xof';            { int, 0 or 1 }
	OSSL_MAC_PARAM_DIGEST_NOINIT    = 'digest-noinit';  { int, 0 or 1 }
	OSSL_MAC_PARAM_DIGEST_ONESHOT   = 'digest-oneshot'; { int, 0 or 1 }
	OSSL_MAC_PARAM_C_ROUNDS         = 'c-rounds';       { unsigned int }
	OSSL_MAC_PARAM_D_ROUNDS         = 'd-rounds';       { unsigned int }

{
 * If= 'engine'; or= 'properties'; are specified, they should always be paired
 * with= 'cipher'; or= 'digest';.
 }
	OSSL_MAC_PARAM_CIPHER        = OSSL_ALG_PARAM_CIPHER;    { utf8 string }
	OSSL_MAC_PARAM_DIGEST        = OSSL_ALG_PARAM_DIGEST;    { utf8 string }
	OSSL_MAC_PARAM_PROPERTIES    = OSSL_ALG_PARAM_PROPERTIES;{ utf8 string }
	OSSL_MAC_PARAM_SIZE          = 'size';                    { size_t }
	OSSL_MAC_PARAM_BLOCK_SIZE    = 'block-size';              { size_t }
	OSSL_MAC_PARAM_TLS_DATA_SIZE = 'tls-data-size';           { size_t }

{ Known MAC names }
	OSSL_MAC_NAME_BLAKE2BMAC = 'BLAKE2BMAC';
	OSSL_MAC_NAME_BLAKE2SMAC = 'BLAKE2SMAC';
	OSSL_MAC_NAME_CMAC       = 'CMAC';
	OSSL_MAC_NAME_GMAC       = 'GMAC';
	OSSL_MAC_NAME_HMAC       = 'HMAC';
	OSSL_MAC_NAME_KMAC128    = 'KMAC128';
	OSSL_MAC_NAME_KMAC256    = 'KMAC256';
	OSSL_MAC_NAME_POLY1305   = 'POLY1305';
	OSSL_MAC_NAME_SIPHASH    = 'SIPHASH';

{ KDF / PRF parameters }
	OSSL_KDF_PARAM_SECRET       = 'secret';    { octet string }
	OSSL_KDF_PARAM_KEY          = 'key';       { octet string }
	OSSL_KDF_PARAM_SALT         = 'salt';      { octet string }
	OSSL_KDF_PARAM_PASSWORD     = 'pass';      { octet string }
	OSSL_KDF_PARAM_DIGEST       = OSSL_ALG_PARAM_DIGEST;    { utf8 string }
	OSSL_KDF_PARAM_CIPHER       = OSSL_ALG_PARAM_CIPHER;    { utf8 string }
	OSSL_KDF_PARAM_MAC          = OSSL_ALG_PARAM_MAC;       { utf8 string }
	OSSL_KDF_PARAM_MAC_SIZE     = 'maclen';    { size_t }
	OSSL_KDF_PARAM_PROPERTIES   = OSSL_ALG_PARAM_PROPERTIES;{ utf8 string }
	OSSL_KDF_PARAM_ITER         = 'iter';      { unsigned int }
	OSSL_KDF_PARAM_MODE         = 'mode';      { utf8 string or int }
	OSSL_KDF_PARAM_PKCS5        = 'pkcs5';     { int }
	OSSL_KDF_PARAM_UKM          = 'ukm';       { octet string }
	OSSL_KDF_PARAM_CEK_ALG      = 'cekalg';    { utf8 string }
	OSSL_KDF_PARAM_SCRYPT_N     = 'n';         { uint64_t }
	OSSL_KDF_PARAM_SCRYPT_R     = 'r';         { uint32_t }
	OSSL_KDF_PARAM_SCRYPT_P     = 'p';         { uint32_t }
	OSSL_KDF_PARAM_SCRYPT_MAXMEM    = 'maxmem_bytes'; { uint64_t }
	OSSL_KDF_PARAM_INFO             = 'info';      { octet string }
	OSSL_KDF_PARAM_SEED             = 'seed';      { octet string }
	OSSL_KDF_PARAM_SSHKDF_XCGHASH   = 'xcghash'; { octet string }
	OSSL_KDF_PARAM_SSHKDF_SESSION_ID = 'session_id'; { octet string }
	OSSL_KDF_PARAM_SSHKDF_TYPE      = 'type';      { int }
	OSSL_KDF_PARAM_SIZE             = 'size';      { size_t }
	OSSL_KDF_PARAM_CONSTANT         = 'constant';  { octet string }
	OSSL_KDF_PARAM_PKCS12_ID        = 'id';        { int }
	OSSL_KDF_PARAM_KBKDF_USE_L      = 'use-l';             { int }
	OSSL_KDF_PARAM_KBKDF_USE_SEPARATOR  = 'use-separator';     { int }
	OSSL_KDF_PARAM_X942_ACVPINFO        = 'acvp-info';
	OSSL_KDF_PARAM_X942_PARTYUINFO      = 'partyu-info';
	OSSL_KDF_PARAM_X942_PARTYVINFO      = 'partyv-info';
	OSSL_KDF_PARAM_X942_SUPP_PUBINFO    = 'supp-pubinfo';
	OSSL_KDF_PARAM_X942_SUPP_PRIVINFO   = 'supp-privinfo';
	OSSL_KDF_PARAM_X942_USE_KEYBITS     = 'use-keybits';

{ Known KDF names }
	OSSL_KDF_NAME_HKDF          = 'HKDF';
	OSSL_KDF_NAME_PBKDF1        = 'PBKDF1';
	OSSL_KDF_NAME_PBKDF2        = 'PBKDF2';
	OSSL_KDF_NAME_SCRYPT        = 'SCRYPT';
	OSSL_KDF_NAME_SSHKDF        = 'SSHKDF';
	OSSL_KDF_NAME_SSKDF         = 'SSKDF';
	OSSL_KDF_NAME_TLS1_PRF      = 'TLS1-PRF';
	OSSL_KDF_NAME_X942KDF_ASN1  = 'X942KDF-ASN1';
	OSSL_KDF_NAME_X942KDF_CONCAT= 'X942KDF-CONCAT';
	OSSL_KDF_NAME_X963KDF       = 'X963KDF';
	OSSL_KDF_NAME_KBKDF         = 'KBKDF';
	OSSL_KDF_NAME_KRB5KDF       = 'KRB5KDF';

{ Known RAND names }
	OSSL_RAND_PARAM_STATE                = 'state';
	OSSL_RAND_PARAM_STRENGTH             = 'strength';
	OSSL_RAND_PARAM_MAX_REQUEST          = 'max_request';
	OSSL_RAND_PARAM_TEST_ENTROPY         = 'test_entropy';
	OSSL_RAND_PARAM_TEST_NONCE           = 'test_nonce';

{ RAND/DRBG names }
	OSSL_DRBG_PARAM_RESEED_REQUESTS      = 'reseed_requests';
	OSSL_DRBG_PARAM_RESEED_TIME_INTERVAL = 'reseed_time_interval';
	OSSL_DRBG_PARAM_MIN_ENTROPYLEN       = 'min_entropylen';
	OSSL_DRBG_PARAM_MAX_ENTROPYLEN       = 'max_entropylen';
	OSSL_DRBG_PARAM_MIN_NONCELEN         = 'min_noncelen';
	OSSL_DRBG_PARAM_MAX_NONCELEN         = 'max_noncelen';
	OSSL_DRBG_PARAM_MAX_PERSLEN          = 'max_perslen';
	OSSL_DRBG_PARAM_MAX_ADINLEN          = 'max_adinlen';
	OSSL_DRBG_PARAM_RESEED_COUNTER       = 'reseed_counter';
	OSSL_DRBG_PARAM_RESEED_TIME          = 'reseed_time';
	OSSL_DRBG_PARAM_PROPERTIES           =  OSSL_ALG_PARAM_PROPERTIES;
	OSSL_DRBG_PARAM_DIGEST               =  OSSL_ALG_PARAM_DIGEST;
	OSSL_DRBG_PARAM_CIPHER               =  OSSL_ALG_PARAM_CIPHER;
	OSSL_DRBG_PARAM_MAC                  =  OSSL_ALG_PARAM_MAC;
	OSSL_DRBG_PARAM_USE_DF               = 'use_derivation_function';

{ DRBG call back parameters }
	OSSL_DRBG_PARAM_ENTROPY_REQUIRED     = 'entropy_required';
	OSSL_DRBG_PARAM_PREDICTION_RESISTANCE= 'prediction_resistance';
	OSSL_DRBG_PARAM_MIN_LENGTH           = 'minium_length';
	OSSL_DRBG_PARAM_MAX_LENGTH           = 'maxium_length';
	OSSL_DRBG_PARAM_RANDOM_DATA          = 'random_data';
	OSSL_DRBG_PARAM_SIZE                 = 'size';

{ PKEY parameters }
{ Common PKEY parameters }
	OSSL_PKEY_PARAM_BITS             = 'bits'; { integer }
	OSSL_PKEY_PARAM_MAX_SIZE         = 'max-size'; { integer }
	OSSL_PKEY_PARAM_SECURITY_BITS    = 'security-bits'; { integer }
	OSSL_PKEY_PARAM_DIGEST           =  OSSL_ALG_PARAM_DIGEST;
	OSSL_PKEY_PARAM_CIPHER           =  OSSL_ALG_PARAM_CIPHER; { utf8 string }
	OSSL_PKEY_PARAM_ENGINE           =  OSSL_ALG_PARAM_ENGINE; { utf8 string }
	OSSL_PKEY_PARAM_PROPERTIES       =  OSSL_ALG_PARAM_PROPERTIES;
	OSSL_PKEY_PARAM_DEFAULT_DIGEST   = 'default-digest'; { utf8 string }
	OSSL_PKEY_PARAM_MANDATORY_DIGEST = 'mandatory-digest'; { utf8 string }
	OSSL_PKEY_PARAM_PAD_MODE         = 'pad-mode';
	OSSL_PKEY_PARAM_DIGEST_SIZE      = 'digest-size';
	OSSL_PKEY_PARAM_MASKGENFUNC      = 'mgf';
	OSSL_PKEY_PARAM_MGF1_DIGEST      = 'mgf1-digest';
	OSSL_PKEY_PARAM_MGF1_PROPERTIES  = 'mgf1-properties';
	OSSL_PKEY_PARAM_ENCODED_PUBLIC_KEY= 'encoded-pub-key';
	OSSL_PKEY_PARAM_GROUP_NAME       = 'group';
	OSSL_PKEY_PARAM_DIST_ID          = 'distid';
	OSSL_PKEY_PARAM_PUB_KEY          = 'pub';
	OSSL_PKEY_PARAM_PRIV_KEY         = 'priv';

{ Diffie-Hellman/DSA Parameters }
	OSSL_PKEY_PARAM_FFC_P            = 'p';
	OSSL_PKEY_PARAM_FFC_G            = 'g';
	OSSL_PKEY_PARAM_FFC_Q            = 'q';
	OSSL_PKEY_PARAM_FFC_GINDEX       = 'gindex';
	OSSL_PKEY_PARAM_FFC_PCOUNTER     = 'pcounter';
	OSSL_PKEY_PARAM_FFC_SEED         = 'seed';
	OSSL_PKEY_PARAM_FFC_COFACTOR     = 'j';
	OSSL_PKEY_PARAM_FFC_H            = 'hindex';
	OSSL_PKEY_PARAM_FFC_VALIDATE_PQ  = 'validate-pq';
	OSSL_PKEY_PARAM_FFC_VALIDATE_G   = 'validate-g';
	OSSL_PKEY_PARAM_FFC_VALIDATE_LEGACY= 'validate-legacy';

{ Diffie-Hellman params }
	OSSL_PKEY_PARAM_DH_GENERATOR     = 'safeprime-generator';
	OSSL_PKEY_PARAM_DH_PRIV_LEN      = 'priv_len';

{ Elliptic Curve Domain Parameters }
	OSSL_PKEY_PARAM_EC_PUB_X  = 'qx';
	OSSL_PKEY_PARAM_EC_PUB_Y  = 'qy';

{ Elliptic Curve Explicit Domain Parameters }
	OSSL_PKEY_PARAM_EC_FIELD_TYPE                = 'field-type';
	OSSL_PKEY_PARAM_EC_P                         = 'p';
	OSSL_PKEY_PARAM_EC_A                         = 'a';
	OSSL_PKEY_PARAM_EC_B                         = 'b';
	OSSL_PKEY_PARAM_EC_GENERATOR                 = 'generator';
	OSSL_PKEY_PARAM_EC_ORDER                     = 'order';
	OSSL_PKEY_PARAM_EC_COFACTOR                  = 'cofactor';
	OSSL_PKEY_PARAM_EC_SEED                      = 'seed';
	OSSL_PKEY_PARAM_EC_CHAR2_M                   = 'm';
	OSSL_PKEY_PARAM_EC_CHAR2_TYPE                = 'basis-type';
	OSSL_PKEY_PARAM_EC_CHAR2_TP_BASIS            = 'tp';
	OSSL_PKEY_PARAM_EC_CHAR2_PP_K1               = 'k1';
	OSSL_PKEY_PARAM_EC_CHAR2_PP_K2               = 'k2';
	OSSL_PKEY_PARAM_EC_CHAR2_PP_K3               = 'k3';
	OSSL_PKEY_PARAM_EC_DECODED_FROM_EXPLICIT_PARAMS= 'decoded-from-explicit';

{ Elliptic Curve Key Parameters }
	OSSL_PKEY_PARAM_USE_COFACTOR_FLAG = 'use-cofactor-flag';
	OSSL_PKEY_PARAM_USE_COFACTOR_ECDH = OSSL_PKEY_PARAM_USE_COFACTOR_FLAG;

{ RSA Keys }
{
 * n, e, d are the usual public and private key components
 *
 * rsa-num is the number of factors, including p and q
 * rsa-factor is used for each factor: p, q, r_i (i = 3, ...)
 * rsa-exponent is used for each exponent: dP, dQ, d_i (i = 3, ...)
 * rsa-coefficient is used for each coefficient: qInv, t_i (i = 3, ...)
 *
 * The number of rsa-factor items must be equal to the number of rsa-exponent
 * items, and the number of rsa-coefficients must be one less.
 * (the base i for the coefficients is 2, not 1, at least as implied by
 * RFC 8017)
 }
	OSSL_PKEY_PARAM_RSA_N           = 'n';
	OSSL_PKEY_PARAM_RSA_E           = 'e';
	OSSL_PKEY_PARAM_RSA_D           = 'd';
	OSSL_PKEY_PARAM_RSA_FACTOR1     = 'rsa-factor1';
	OSSL_PKEY_PARAM_RSA_EXPONENT1   = 'rsa-exponent1';
	OSSL_PKEY_PARAM_RSA_FACTOR2     = 'rsa-factor2';
	OSSL_PKEY_PARAM_RSA_EXPONENT2   = 'rsa-exponent2';
	OSSL_PKEY_PARAM_RSA_COEFFICIENT1 = 'rsa-coefficient1';

{ RSA padding modes }
	OSSL_PKEY_RSA_PAD_MODE_NONE     = 'none';
	OSSL_PKEY_RSA_PAD_MODE_PKCSV15  = 'pkcs1';
	OSSL_PKEY_RSA_PAD_MODE_OAEP     = 'oaep';
	OSSL_PKEY_RSA_PAD_MODE_X931     = 'x931';
	OSSL_PKEY_RSA_PAD_MODE_PSS      = 'pss';

{ RSA pss padding salt length }
	OSSL_PKEY_RSA_PSS_SALT_LEN_DIGEST   = 'digest';
	OSSL_PKEY_RSA_PSS_SALT_LEN_MAX      = 'max';
	OSSL_PKEY_RSA_PSS_SALT_LEN_AUTO     = 'auto';

{ Key generation parameters }
	OSSL_PKEY_PARAM_RSA_BITS          = OSSL_PKEY_PARAM_BITS;
	OSSL_PKEY_PARAM_RSA_PRIMES        = 'primes';
	OSSL_PKEY_PARAM_RSA_DIGEST        = OSSL_PKEY_PARAM_DIGEST;
	OSSL_PKEY_PARAM_RSA_DIGEST_PROPS  = OSSL_PKEY_PARAM_PROPERTIES;
	OSSL_PKEY_PARAM_RSA_MASKGENFUNC   = OSSL_PKEY_PARAM_MASKGENFUNC;
	OSSL_PKEY_PARAM_RSA_MGF1_DIGEST   = OSSL_PKEY_PARAM_MGF1_DIGEST;
	OSSL_PKEY_PARAM_RSA_PSS_SALTLEN   = 'saltlen';

{ Key generation parameters }
	OSSL_PKEY_PARAM_FFC_TYPE      = 'type';
	OSSL_PKEY_PARAM_FFC_PBITS     = 'pbits';
	OSSL_PKEY_PARAM_FFC_QBITS     = 'qbits';
	OSSL_PKEY_PARAM_FFC_DIGEST    = OSSL_PKEY_PARAM_DIGEST;
	OSSL_PKEY_PARAM_FFC_DIGEST_PROPS = OSSL_PKEY_PARAM_PROPERTIES;

	OSSL_PKEY_PARAM_EC_ENCODING                 = 'encoding'; { utf8_string }
	OSSL_PKEY_PARAM_EC_POINT_CONVERSION_FORMAT  = 'point-format';
	OSSL_PKEY_PARAM_EC_GROUP_CHECK_TYPE         = 'group-check';
	OSSL_PKEY_PARAM_EC_INCLUDE_PUBLIC           = 'include-public';

{ OSSL_PKEY_PARAM_EC_ENCODING values }
	OSSL_PKEY_EC_ENCODING_EXPLICIT  = 'explicit';
	OSSL_PKEY_EC_ENCODING_GROUP     = 'named_curve';

	OSSL_PKEY_EC_POINT_CONVERSION_FORMAT_UNCOMPRESSED   = 'uncompressed';
	OSSL_PKEY_EC_POINT_CONVERSION_FORMAT_COMPRESSED     = 'compressed';
	OSSL_PKEY_EC_POINT_CONVERSION_FORMAT_HYBRID         = 'hybrid';

	OSSL_PKEY_EC_GROUP_CHECK_DEFAULT    = 'default';
	OSSL_PKEY_EC_GROUP_CHECK_NAMED      = 'named';
	OSSL_PKEY_EC_GROUP_CHECK_NAMED_NIST = 'named-nist';

{ Key Exchange parameters }
	OSSL_EXCHANGE_PARAM_PAD                = 'pad'; { uint }
	OSSL_EXCHANGE_PARAM_EC_ECDH_COFACTOR_MODE= 'ecdh-cofactor-mode'; { int }
	OSSL_EXCHANGE_PARAM_KDF_TYPE           = 'kdf-type'; { utf8_string }
	OSSL_EXCHANGE_PARAM_KDF_DIGEST         = 'kdf-digest'; { utf8_string }
	OSSL_EXCHANGE_PARAM_KDF_DIGEST_PROPS   = 'kdf-digest-props'; { utf8_string }
	OSSL_EXCHANGE_PARAM_KDF_OUTLEN         = 'kdf-outlen'; { size_t }
{ The following parameter is an octet_string on set and an octet_ptr on get }
	OSSL_EXCHANGE_PARAM_KDF_UKM            = 'kdf-ukm';

{ Signature parameters }
	OSSL_SIGNATURE_PARAM_ALGORITHM_ID    = 'algorithm-id';
	OSSL_SIGNATURE_PARAM_PAD_MODE        = OSSL_PKEY_PARAM_PAD_MODE;
	OSSL_SIGNATURE_PARAM_DIGEST          = OSSL_PKEY_PARAM_DIGEST;
	OSSL_SIGNATURE_PARAM_PROPERTIES      = OSSL_PKEY_PARAM_PROPERTIES;
	OSSL_SIGNATURE_PARAM_PSS_SALTLEN     = 'saltlen';
	OSSL_SIGNATURE_PARAM_MGF1_DIGEST     =  OSSL_PKEY_PARAM_MGF1_DIGEST;
	OSSL_SIGNATURE_PARAM_MGF1_PROPERTIES = OSSL_PKEY_PARAM_MGF1_PROPERTIES;
	OSSL_SIGNATURE_PARAM_DIGEST_SIZE     = OSSL_PKEY_PARAM_DIGEST_SIZE;

{ Asym cipher parameters }
	OSSL_ASYM_CIPHER_PARAM_DIGEST                 = OSSL_PKEY_PARAM_DIGEST;
	OSSL_ASYM_CIPHER_PARAM_PROPERTIES             = OSSL_PKEY_PARAM_PROPERTIES;
	OSSL_ASYM_CIPHER_PARAM_ENGINE                 = OSSL_PKEY_PARAM_ENGINE;
	OSSL_ASYM_CIPHER_PARAM_PAD_MODE               = OSSL_PKEY_PARAM_PAD_MODE;
	OSSL_ASYM_CIPHER_PARAM_MGF1_DIGEST            = OSSL_PKEY_PARAM_MGF1_DIGEST;
	OSSL_ASYM_CIPHER_PARAM_MGF1_DIGEST_PROPS      = OSSL_PKEY_PARAM_MGF1_PROPERTIES;
	OSSL_ASYM_CIPHER_PARAM_OAEP_DIGEST            = OSSL_ALG_PARAM_DIGEST;
	OSSL_ASYM_CIPHER_PARAM_OAEP_DIGEST_PROPS      = 'digest-props';
{ The following parameter is an octet_string on set and an octet_ptr on get }
	OSSL_ASYM_CIPHER_PARAM_OAEP_LABEL             = 'oaep-label';
	OSSL_ASYM_CIPHER_PARAM_TLS_CLIENT_VERSION     = 'tls-client-version';
	OSSL_ASYM_CIPHER_PARAM_TLS_NEGOTIATED_VERSION = 'tls-negotiated-version';

{
 * Encoder / decoder parameters
 }
	OSSL_ENCODER_PARAM_CIPHER           = OSSL_ALG_PARAM_CIPHER;
	OSSL_ENCODER_PARAM_PROPERTIES       = OSSL_ALG_PARAM_PROPERTIES;
{ Currently PVK only, but reusable for others as needed }
	OSSL_ENCODER_PARAM_ENCRYPT_LEVEL    = 'encrypt-level';
	OSSL_ENCODER_PARAM_SAVE_PARAMETERS  = 'save-parameters'; { integer }

	OSSL_DECODER_PARAM_PROPERTIES     = OSSL_ALG_PARAM_PROPERTIES;

{ Passphrase callback parameters }
	OSSL_PASSPHRASE_PARAM_INFO   = 'info';

{ Keygen callback parameters, from provider to libcrypto }
	OSSL_GEN_PARAM_POTENTIAL         = 'potential'; { integer }
	OSSL_GEN_PARAM_ITERATION         = 'iteration'; { integer }

{ ACVP Test parameters : These should not be used normally }
	OSSL_PKEY_PARAM_RSA_TEST_XP1    = 'xp1';
	OSSL_PKEY_PARAM_RSA_TEST_XP2    = 'xp2';
	OSSL_PKEY_PARAM_RSA_TEST_XP     = 'xp';
	OSSL_PKEY_PARAM_RSA_TEST_XQ1    = 'xq1';
	OSSL_PKEY_PARAM_RSA_TEST_XQ2    = 'xq2';
	OSSL_PKEY_PARAM_RSA_TEST_XQ     = 'xq';
	OSSL_PKEY_PARAM_RSA_TEST_P1     = 'p1';
	OSSL_PKEY_PARAM_RSA_TEST_P2     = 'p2';
	OSSL_PKEY_PARAM_RSA_TEST_Q1     = 'q1';
	OSSL_PKEY_PARAM_RSA_TEST_Q2     = 'q2';
	OSSL_SIGNATURE_PARAM_KAT        = 'kat';

{ KEM parameters }
	OSSL_KEM_PARAM_OPERATION         = 'operation';

{ OSSL_KEM_PARAM_OPERATION values }
	OSSL_KEM_PARAM_OPERATION_RSASVE  = 'RSASVE';

{ Capabilities }

{ TLS-GROUP Capability }
	OSSL_CAPABILITY_TLS_GROUP_NAME           = 'tls-group-name';
	OSSL_CAPABILITY_TLS_GROUP_NAME_INTERNAL  = 'tls-group-name-internal';
	OSSL_CAPABILITY_TLS_GROUP_ID             = 'tls-group-id';
	OSSL_CAPABILITY_TLS_GROUP_ALG            = 'tls-group-alg';
	OSSL_CAPABILITY_TLS_GROUP_SECURITY_BITS  = 'tls-group-sec-bits';
	OSSL_CAPABILITY_TLS_GROUP_IS_KEM         = 'tls-group-is-kem';
	OSSL_CAPABILITY_TLS_GROUP_MIN_TLS        = 'tls-min-tls';
	OSSL_CAPABILITY_TLS_GROUP_MAX_TLS        = 'tls-max-tls';
	OSSL_CAPABILITY_TLS_GROUP_MIN_DTLS       = 'tls-min-dtls';
	OSSL_CAPABILITY_TLS_GROUP_MAX_DTLS       = 'tls-max-dtls';

{-
 * storemgmt parameters
 }

{
 * Used by storemgmt_ctx_set_params():
 *
 * - OSSL_STORE_PARAM_EXPECT is an INTEGER, and the value is any of the
 *   OSSL_STORE_INFO numbers.  This is used to set the expected type of
 *   object loaded.
 *
 * - OSSL_STORE_PARAM_SUBJECT, OSSL_STORE_PARAM_ISSUER,
 *   OSSL_STORE_PARAM_SERIAL, OSSL_STORE_PARAM_FINGERPRINT,
 *   OSSL_STORE_PARAM_DIGEST, OSSL_STORE_PARAM_ALIAS
 *   are used as search criteria.
 *   (OSSL_STORE_PARAM_DIGEST is used with OSSL_STORE_PARAM_FINGERPRINT)
 }
	OSSL_STORE_PARAM_EXPECT     = 'expect';       { INTEGER }
	OSSL_STORE_PARAM_SUBJECT    = 'subject'; { DER blob => OCTET_STRING }
	OSSL_STORE_PARAM_ISSUER     = 'name'; { DER blob => OCTET_STRING }
	OSSL_STORE_PARAM_SERIAL     = 'serial';       { INTEGER }
	OSSL_STORE_PARAM_DIGEST     = 'digest';       { UTF8_STRING }
	OSSL_STORE_PARAM_FINGERPRINT = 'fingerprint'; { OCTET_STRING }
	OSSL_STORE_PARAM_ALIAS      = 'alias';        { UTF8_STRING }

{ You may want to pass properties for the provider implementation to use }
	OSSL_STORE_PARAM_PROPERTIES = 'properties';   { utf8_string }
{ OSSL_DECODER input type if a decoder is used by the store }
	OSSL_STORE_PARAM_INPUT_TYPE = 'input-type';   { UTF8_STRING }

{ Key data subset selection - individual bits }
    OSSL_KEYMGMT_SELECT_PRIVATE_KEY          = $01;
    OSSL_KEYMGMT_SELECT_PUBLIC_KEY           = $02;
    OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS    = $04;
    OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS     = $80;

{ Key data subset selection - combinations }
    OSSL_KEYMGMT_SELECT_ALL_PARAMETERS = OSSL_KEYMGMT_SELECT_DOMAIN_PARAMETERS OR OSSL_KEYMGMT_SELECT_OTHER_PARAMETERS;
    OSSL_KEYMGMT_SELECT_KEYPAIR = OSSL_KEYMGMT_SELECT_PRIVATE_KEY OR OSSL_KEYMGMT_SELECT_PUBLIC_KEY;
    OSSL_KEYMGMT_SELECT_ALL = OSSL_KEYMGMT_SELECT_KEYPAIR OR OSSL_KEYMGMT_SELECT_ALL_PARAMETERS;

{ Easy to use macros for EVP_PKEY related selections }
    EVP_PKEY_KEY_PARAMETERS = OSSL_KEYMGMT_SELECT_ALL_PARAMETERS;
    EVP_PKEY_PUBLIC_KEY = EVP_PKEY_KEY_PARAMETERS OR OSSL_KEYMGMT_SELECT_PUBLIC_KEY;
    EVP_PKEY_KEYPAIR = EVP_PKEY_PUBLIC_KEY OR OSSL_KEYMGMT_SELECT_PRIVATE_KEY;

{ types.h }
type
    POSSL_PROVIDER = Pointer;          { ossl_provider_st;  Provider Object  }
    POPENSSL_INIT_SETTINGS = Pointer;  { ossl_init_settings_st }
    POSSL_HTTP_REQ_CTX = Pointer;      { ossl_http_req_ctx_st }
    POSSL_STORE_INFO = Pointer;        { ossl_store_info_st }
    POSSL_STORE_SEARCH = Pointer;      { ossl_store_search_st }
    POSSL_LIB_CTX = Pointer;           { ossl_lib_ctx_st OSSL_LIB_CTX }
    POSSL_DISPATCH = Pointer;          { ossl_dispatch_st }
    POSSL_ITEM = Pointer;              { ossl_item_st }
    POSSL_ALGORITHM = Pointer;         { ossl_algorithm_st }
    {$IF declared(OPENSSL_VERSION_MAJOR_) and (OPENSSL_VERSION_MAJOR_ >= Integer(3))}
    OSSL_PARAM = YuOpenSSL.OSSL_PARAM;
    POSSL_PARAM = YuOpenSSL.OSSL_PARAM_ptr;
    {$ELSE}
    OSSL_PARAM = ossl_param_st;        { ossl_param_st }
    POSSL_PARAM = ^OSSL_PARAM;         { ossl_param_st }
    {$IFEND}
    PPOSSL_PARAM = ^POSSL_PARAM;
    POSSL_PARAM_BLD = Pointer;         { ossl_param_bld_st }
    POSSL_ENCODER = Pointer;           { ossl_encoder_st }
    POSSL_ENCODER_CTX = Pointer;       { ossl_encoder_ctx_st }
    POSSL_DECODER = Pointer;           { ossl_decoder_st }
    POSSL_DECODER_CTX = Pointer;       { ossl_decoder_ctx_st }
    POSSL_SELF_TEST = Pointer;         { ossl_self_test_st }

    EParametertException = class(Exception); 


{ old structures that we seem to need for old code }
    TPKCS7_SIGNER_INFO_st = record
        version             : PASN1_INTEGER;                // version 1
        issuer_and_serial   : PPKCS7_ISSUER_AND_SERIAL;
        digest_alg          : PX509_ALGOR;
        auth_attr           : PSTACK_OF_X509_ATTRIBUTE;     // [ 0 ]
        digest_enc_alg      : PX509_ALGOR;
        enc_digest          : PASN1_OCTET_STRING;
        unauth_attr         : PSTACK_OF_X509_ATTRIBUTE;     // [ 1 ]
        // The private key to sign with //
        pkey                : PEVP_PKEY;
    end;
    PKCS7_SIGNER_INFO_OSSL = ^TPKCS7_SIGNER_INFO_st;

    TPKCS7_ENVELOPED_st = record
        version       : PASN1_INTEGER;
        recipientinfo : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data      : PPKCS7_ENC_CONTENT;
    end;
    PPKCS7_ENVELOPE = ^TPKCS7_ENVELOPED_st;
    PPKCS7  = ^TPKCS7_st;

    TPKCS7_SIGNED_st = record
        version     : PASN1_INTEGER;
        md_algs     : PSTACK_OF_X509_ALGOR;
        cert        : PSTACK_OF_X509;
        crl         : PSTACK_OF_X509_CRL;
        signer_info : PSTACK_OF_PKCS7_SIGNER_INFO;
        contents    : PPKCS7;
    end;
    PPKCS7_SIGNED = ^TPKCS7_SIGNED_st;

    PKCS7_signedandenveloped = record
        version         : PASN1_INTEGER;
        md_algs         : PSTACK_OF_X509_ALGOR;
        cert            : PSTACK_OF_X509;
        crl             : PSTACK_OF_X509_CRL;
        signer_info     : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data        : PPKCS7_ENC_CONTENT;
        recipientinfo   : PSTACK_OF_PKCS7_RECIP_INFO;
    end;
    PPKCS7_SIGN_ENVELOPE = ^PKCS7_signedandenveloped;

    PKCS7_CTX_st = record       { V8.68 }
        libctx: Pointer;
        propq: PAnsiChar;
    end;
    PKCS7_CTX = PKCS7_CTX_st;

    TPKCS7_st = record                         //AG  V8.66 can not find PKC7_xx APIs to get this stuff
      { The following is non NULL if it contains ASN1 encoding of this structure }
        asn1        : PAnsiChar;
        length      : Integer;
        state       : Integer;
        detached    : Integer;
        type_       : PASN1_OBJECT;
   {     case Integer of  }
        d: record case Byte of    { V8.68 changed from variant to named record }
        0: (ptr                  : PAnsiChar);
        // NID_pkcs7_data
        1: (data                 : PASN1_OCTET_STRING);
        // NID_pkcs7_signed
        2: (sign                 : PPKCS7_SIGNED);
        // NID_pkcs7_enveloped
        3: (enveloped            : PPKCS7_ENVELOPE);
        // NID_pkcs7_signedAndEnveloped
        4: (signed_and_enveloped : PPKCS7_SIGN_ENVELOPE);
        // NID_pkcs7_digest
        5: (digest               : PPKCS7_DIGEST);
        // NID_pkcs7_encrypted
        6: (encrypted            : PPKCS7_ENCRYPT);
        // Anything else
        7: (other                : PASN1_TYPE);
        end;
        Ctx: PKCS7_CTX;      { V8.68 }
    end;


    PASN1_ITEM_EXP     = function: PASN1_ITEM; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_NEW    = function: Pointer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_FREE   = procedure(Arg: Pointer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_D2I    = function(Arg1: Pointer; Arg2: PPAnsiChar; Arg3: Integer): Pointer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_I2D    = function(Arg1: Pointer; Arg2: PPAnsiChar): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_I2S    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer): PAnsiChar; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_S2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_I2V    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; ExtList: PSTACK): PSTACK; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_V2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; Values: PSTACK): Pointer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_I2R    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; Output: PBIO; Indent: Integer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PX509V3_EXT_R2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}

    TX509V3_EXT_METHOD = record // struct v3_ext_method
        ext_nid   : Integer;
        ext_flags : Integer;
        // If this is set the following four fields are ignored - since v0.9.0.7
        it        : PASN1_ITEM_EXP;                              //AG
        // Old style ASN1 calls
        ext_new   : PX509V3_EXT_NEW;
        ext_free  : PX509V3_EXT_FREE;
        d2i       : PX509V3_EXT_D2I;
        i2d       : PX509V3_EXT_I2D;
        // The following pair is used for string extensions
        i2s       : PX509V3_EXT_I2S;
        s2i       : PX509V3_EXT_S2I;
        // The following pair is used for multi-valued extensions
        i2v       : PX509V3_EXT_I2V;
        v2i       : PX509V3_EXT_V2I;
        // The following are used for raw extensions
        i2r       : PX509V3_EXT_I2R;
        r2i       : PX509V3_EXT_R2I;
        // Any extension specific data
        usr_data  : Pointer;
    end;
    PX509V3_EXT_METHOD = ^TX509V3_EXT_METHOD;

    TPoint_Conversion_Form_t = byte;             { V8.52 }
       { the point is encoded as z||x, where the octet z specifies
         *  which solution of the quadratic equation y is  }
const
    POINT_CONVERSION_COMPRESSED = 2;
        { the point is encoded as z||x||y, where z is the octet 0x04  }
    POINT_CONVERSION_UNCOMPRESSED = 4;
        { the point is encoded as z||x||y, where the octet z specifies
           which solution of the quadratic equation y is  }
    POINT_CONVERSION_HYBRID = 6;

type
  { V8.27  The valid handshake states (one for each type message sent and one for each
           type of message received). There are also two "special" states:
     TLS = TLS or DTLS state
     DTLS = DTLS specific state
     CR/SR = Client Read/Server Read
     CW/SW = Client Write/Server Write
       The "special" states are:
     TLS_ST_BEFORE = No handshake has been initiated yet
     TLS_ST_OK = A handshake has been successfully completed }

    OSSL_HANDSHAKE_STATE = (       { V8.66 was TSslHandshakeState for SSL_get_state }
        TLS_ST_Before,
        TLS_ST_OK,
        DTLS_ST_CR_Hello_Verify_Request,
        TLS_ST_CR_Srvr_Hello,
        TLS_ST_CR_Cert,
        TLS_ST_CR_Cert_Status,
        TLS_ST_CR_Key_Exch,
        TLS_ST_CR_Cert_Req,
        TLS_ST_CR_Srvr_Done,
        TLS_ST_CR_Session_Ticket,
        TLS_ST_CR_Change,
        TLS_ST_CR_Finished,
        TLS_ST_CW_Client_Hello,
        TLS_ST_CW_Cert,
        TLS_ST_CW_Key_Exch,
        TLS_ST_CW_Cert_Verify,
        TLS_ST_CW_Change,
        TLS_ST_CW_Next_Proto,
        TLS_ST_CW_Finished,
        TLS_ST_SW_Hello_Req,
        TLS_ST_SR_Client_Hello,
        DTLS_ST_SW_Hello_Verify_Request,
        TLS_ST_SW_Server_Hello,
        TLS_ST_SW_Cert,
        TLS_ST_SW_Key_Exch,
        TLS_ST_SW_Cert_Req,
        TLS_ST_SW_Server_Done,
        TLS_ST_SR_Cert,
        TLS_ST_SR_Key_Exch,
        TLS_ST_SR_Cert_Verify,
        TLS_ST_SR_Next_Proto,
        TLS_ST_SR_Change,
        TLS_ST_SR_Finished,
        TLS_ST_SW_Session_Ticket,
        TLS_ST_SW_Cert_Status,
        TLS_ST_SW_Change,
        TLS_ST_SW_Finished,
        TLS_ST_SW_Encrypted_Extensions,  { V8.51 lots more for TLS/1.3 }
        TLS_ST_CR_Encrypted_Extensions,
        TLS_ST_CR_Cert_Vrfy,
        TLS_ST_SW_Cert_Vrfy,
        TLS_ST_CR_Hello_Req,
        TLS_ST_SW_Hello_Retry_Request,
        TLS_ST_CR_Hello_Retry_Request,
        TLS_ST_SW_Key_Update,
        TLS_ST_CW_Key_Update,
        TLS_ST_SR_Key_Update,
        TLS_ST_CR_Key_Update,
        TLS_ST_Early_Data,
        TLS_ST_Pending_Early_Data_End,
        TLS_ST_CW_End_Of_Early_Data,
        TLS_ST_SR_End_Of_Early_Data) ;

(*  callback prototypes, need to create a function or procedure with these parameters and pass a pointer to the OSL function that sets it.
    V8.65 no longer using the actual callback types in function definitions, just plain pointers.
type
    TPem_password_cb = function(Buf      : PAnsiChar;
                                Num      : Integer;
                                RWFlag   : Integer;
                                UserData : Pointer) : Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}

    TRSA_genkey_cb = procedure(N1, N2 : Integer;     //AG
                               cb_arg : Pointer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}

    TSetVerify_cb = function(Ok : Integer; StoreCtx : PX509_STORE_CTX) : Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    TSetInfo_cb   = procedure(const ssl: PSSL; CbType: Integer; Val: Integer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}

    TNew_session_cb = function(const Ssl : PSSL; Sess : PSSL_SESSION): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PNew_session_cb = ^TNew_session_cb;
    TRemove_session_cb = procedure(const Ctx : PSSL_CTX; Sess : PSSL_SESSION); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PRemove_session_cb = ^TRemove_session_cb;
    TGet_session_cb = function(const Ssl : PSSL; SessID : Pointer; IdLen : Integer; Ref : PInteger) : PSSL_SESSION; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PGet_session_cb = ^TGet_session_cb;
    TClient_cert_cb = function (Ssl : PSSL; X509 : PPX509; PKEY : PPEVP_PKEY): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    PClient_cert_cb = ^TClient_cert_cb;

    TCallback_ctrl_fp = procedure (p : Pointer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}
    TSsl_servername_cb = function (s: PSSL; ad: PInteger; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}  { V8.66 var gone }

    TProto_msg_cb = procedure (write_p, version, content_type: integer;
              buf: PAnsiChar; len: size_t; ssl: PSSL; arg: Pointer); {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}   { V8.40 handshake protocol message callback, V8.64 not a function, V8.65 corrected len to size_t }

    TSecurity_level_cb = function  (s: PSSL; ctx: PSSL_CTX; op, bits,
              nid: integer; other, ex: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}  { V8.40 security level callback }

    TSsl_alpn_cb = function (s: PSSL; output: PPointer; outlen: PInteger; { V8.66 var gone }
              input: Pointer; inlen: Integer; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF}  { V8.56 application layer protocol callback }

    TSsl_client_hello_cb = function(s: PSSL; al: PInteger; arg: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF} { V8.64 client hello callback, 1.1.1 and later, replaces servername_cb } { V8.66 var gone }

    TOSSL_PROVIDER_cb = function(provider: POSSL_PROVIDER; cbdata: Pointer): Integer; {$IFNDEF YuOpenSSL}cdecl;{$ENDIF} { V8.67 }

*)

const
    SSL2_VERSION                                = $0002;
    SSL2_VERSION_MAJOR                          = $00;
    SSL2_VERSION_MINOR                          = $02;

    SSL3_VERSION                                = $0300;
    SSL3_VERSION_MAJOR                          = $03;
    SSL3_VERSION_MINOR                          = $00;

    TLS1_VERSION                                = $0301;
    TLS1_VERSION_MAJOR                          = $03;
    TLS1_VERSION_MINOR                          = $01;

    TLS1_1_VERSION                              = $0302;  // V8.01
    TLS1_1_VERSION_MAJOR                        = $03;    // V8.01
    TLS1_1_VERSION_MINOR                        = $02;    // V8.01

    TLS1_2_VERSION                              = $0303;  // V8.01
    TLS1_2_VERSION_MAJOR                        = $03;    // V8.01
    TLS1_2_VERSION_MINOR                        = $03;    // V8.01

    TLS1_3_VERSION                              = $0304;  // V8.40
    TLS1_3_VERSION_MAJOR                        = $03;    // V8.40
    TLS1_3_VERSION_MINOR                        = $04;    // V8.40

    TLS_MAX_VERSION                             = TLS1_3_VERSION;  // V8.51
    TLS_ANY_VERSION                             = $10000;          // V8.27

{$IFNDEF NO_DEBUG_LOG}
{ V8.40 literals for Protocol Message debugging }
type
    TLitLookups = record
        S: String;
        L: integer;
    end ;

const
{ V8.51 following for rotoMsgCallback }
    SSL3_RT_CHANGE_CIPHER_SPEC     = 20;
    SSL3_RT_ALERT                  = 21;
    SSL3_RT_HANDSHAKE              = 22;
    SSL3_RT_APPLICATION_DATA       = 23;
    DTLS1_RT_HEARTBEAT             = 24;

    LitsSslVersions:  array[0..4] of TLitLookups = (
        (S: 'SSL 3.0'; L: SSL3_VERSION),
        (S: 'TLS 1.0'; L: TLS1_VERSION),
        (S: 'TLS 1.1'; L: TLS1_1_VERSION),
        (S: 'TLS 1.2'; L: TLS1_2_VERSION),
        (S: 'TLS 1.3'; L: TLS1_3_VERSION) );


    LitsAlertTypes:  array[0..31] of TLitLookups = (
        (S: 'Close Notify'; L:  0),
        (S: 'Unexpected Message'; L:  10),
        (S: 'Bad Record Mac'; L:  20),
        (S: 'Decryption Failed'; L:  21),
        (S: 'Record Overflow'; L:  22),
        (S: 'Decompression Failure'; L:  30),
        (S: 'Handshake Failure'; L:  40),
        (S: 'Bad Certificate'; L:  42),
        (S: 'Unsupported Certificate'; L:  43),
        (S: 'Certificate Revoked'; L:  44),
        (S: 'Certificate Expired'; L:  45),
        (S: 'Certificate Unknown'; L:  46),
        (S: 'Illegal Parameter'; L:  47),
        (S: 'Unknown CA'; L:  48),
        (S: 'Access Denied'; L:  49),
        (S: 'Decode Error'; L:  50),
        (S: 'Decrypt Error'; L:  51),
        (S: 'Export Restriction'; L:  60),
        (S: 'Protocol Version'; L:  70),
        (S: 'Insufficient Security'; L:  71),
        (S: 'Internal Error'; L:  80),
        (S: 'Inappropriate Fallback'; L:  86),    { V8.51 }
        (S: 'User Cancelled'; L:  90),
        (S: 'No Renegotiation'; L:  100),
        (S: 'Missing Extension'; L:  109),         { V8.51 }
        (S: 'Unsupported Extension'; L:  110),
        (S: 'Certificate Unobtainable'; L:  111),
        (S: 'Unrecognized Name'; L:  112),
        (S: 'Bad Certificate Status Response'; L:  113),
        (S: 'Bad Certificate Hash Value'; L:  114),
        (S: 'Unknown PSK Identity'; L:  115),
        (S: 'Certificate required'; L:  116) ) ;   { V8.51 }

    SSL3_MT_HELLO_REQUEST                 = 0;
    SSL3_MT_CLIENT_HELLO                  = 1;
    SSL3_MT_SERVER_HELLO                  = 2;
    SSL3_MT_NEWSESSION_TICKET             = 4;
    SSL3_MT_END_OF_EARLY_DATA             = 5;
//    SSL3_MT_HELLO_RETRY_REQUEST           = 6;   V8.66 gone 1.1.1
    SSL3_MT_ENCRYPTED_EXTENSIONS          = 8;
    SSL3_MT_CERTIFICATE                   = 11;
    SSL3_MT_SERVER_KEY_EXCHANGE           = 12;
    SSL3_MT_CERTIFICATE_REQUEST           = 13;
    SSL3_MT_SERVER_DONE                   = 14;
    SSL3_MT_CERTIFICATE_VERIFY            = 15;
    SSL3_MT_CLIENT_KEY_EXCHANGE           = 16;
    SSL3_MT_FINISHED                      = 20;
    SSL3_MT_CERTIFICATE_URL               = 21;  { V8.66 new }
    SSL3_MT_CERTIFICATE_STATUS            = 22;
    SSL3_MT_SUPPLEMENTAL_DATA             = 23;  { V8.66 new }
    SSL3_MT_KEY_UPDATE                    = 24;
    SSL3_MT_NEXT_PROTO                    = 67;
    SSL3_MT_MESSAGE_HASH                  = 254;
    DTLS1_MT_HELLO_VERIFY_REQUEST         = 3;

    LitsHandshake:  array[0..19] of TLitLookups = (
        (S: 'Hello Request'; L:  SSL3_MT_HELLO_REQUEST),
        (S: 'Client Hello'; L:  SSL3_MT_CLIENT_HELLO),
        (S: 'Server Hello'; L:  SSL3_MT_SERVER_HELLO),
        (S: 'Hello Verify Request'; L:  3),
        (S: 'New Session Ticket'; L:  SSL3_MT_NEWSESSION_TICKET),
        (S: 'End of early data'; L:  SSL3_MT_END_OF_EARLY_DATA),       { V8.51 }
//        (S: 'Hello retry request'; L:SSL3_MT_HELLO_RETRY_REQUEST),     { V8.51 }  V8.66 gone 1.1.1
        (S: 'Encrypted extensions'; L:SSL3_MT_ENCRYPTED_EXTENSIONS),   { V8.51 }
        (S: 'Certificate'; L:  SSL3_MT_CERTIFICATE),
        (S: 'Server Key Exchange'; L:  SSL3_MT_SERVER_KEY_EXCHANGE),
        (S: 'Certificate Request'; L:  SSL3_MT_CERTIFICATE_REQUEST),
        (S: 'Server Hello Done'; L:  SSL3_MT_SERVER_DONE),
        (S: 'Certificate Verify'; L:  SSL3_MT_CERTIFICATE_VERIFY),
        (S: 'Client Key Exchange'; L:  SSL3_MT_CLIENT_KEY_EXCHANGE),
        (S: 'Finished'; L:  SSL3_MT_FINISHED),
        (S: 'Certificate URL'; L:  SSL3_MT_CERTIFICATE_URL),
        (S: 'Certificate Status'; L:  SSL3_MT_CERTIFICATE_STATUS),
        (S: 'Supplemental Data'; L:  SSL3_MT_SUPPLEMENTAL_DATA) ,
        (S: 'Key update'; L:SSL3_MT_KEY_UPDATE),                { V8.51 }
        (S: 'Next Proto'; L:SSL3_MT_NEXT_PROTO),                { V8.51 }
        (S: 'Message Hash'; L:SSL3_MT_MESSAGE_HASH) ) ;         { V8.51 }

{$ENDIF}

const

 {   DTLS1_2_VERSION is for UDP, sorry not supported yet }

    BIO_NOCLOSE                                 = 0;
    BIO_CLOSE                                   = 1;
    SSL_ERROR_NONE                              = 0;
    SSL_ERROR_SSL                               = 1;
    SSL_ERROR_WANT_READ                         = 2;
    SSL_ERROR_WANT_WRITE                        = 3;
    SSL_ERROR_WANT_X509_LOOKUP                  = 4;
    SSL_ERROR_SYSCALL                           = 5;
    SSL_ERROR_ZERO_RETURN                       = 6;
    SSL_ERROR_WANT_CONNECT                      = 7;
    SSL_ERROR_WANT_ACCEPT                       = 8;
    SSL_ERROR_WANT_ASYNC                        = 9;  { V8.51 following new }
    SSL_ERROR_WANT_ASYNC_JOB                    = 10;
    SSL_ERROR_WANT_CLIENT_HELLO_CB              = 11;

    X509_FILETYPE_PEM                           = 1;
    X509_FILETYPE_ASN1                          = 2;
    X509_FILETYPE_DEFAULT                       = 3;

    X509_EXT_PACK_UNKNOWN                       = 1;
    X509_EXT_PACK_STRING                        = 2;

    SSL_FILETYPE_ASN1                           = X509_FILETYPE_ASN1;
    SSL_FILETYPE_PEM                            = X509_FILETYPE_PEM;
    SSL_VERIFY_NONE                             = 0;
    SSL_VERIFY_PEER                             = 1;
    SSL_VERIFY_FAIL_IF_NO_PEER_CERT             = 2;
    SSL_VERIFY_CLIENT_ONCE                      = 4;

    { V8.27 Flags for building certificate chains )
    { treat any existing certificates as untrusted CAs }
    SSL_BUILD_CHAIN_FLAG_UNTRUSTED              = $00000001;
    { Don't include root CA in chain }
    SSL_BUILD_CHAIN_FLAG_NO_ROOT                = $00000002;
    { Just check certificates already there }
    SSL_BUILD_CHAIN_FLAG_CHECK                  = $00000004;
    { Ignore verification errors }
    SSL_BUILD_CHAIN_FLAG_IGNORE_ERROR           = $00000008;
    { clear verification errors from queue }
    SSL_BUILD_CHAIN_FLAG_CLEAR_ERROR            = $00000010;

    { Removed 12/07/05 - due to changes in v0.9.8a - restored and corrected V8.01 }
 { V8.51 pending, remove some of these once support for 1.0.2 is abandoned }
//  SSL_CTRL_NEED_TMP_RSA                       = 1;     // V8.51 gone in 1.1.0
//  SSL_CTRL_SET_TMP_RSA                        = 2;     // V8.51 gone in 1.1.0
    SSL_CTRL_SET_TMP_DH                         = 3;
    SSL_CTRL_SET_TMP_ECDH                       = 4;
//  SSL_CTRL_SET_TMP_RSA_CB                     = 5;     // V8.51 gone in 1.1.0
    SSL_CTRL_SET_TMP_DH_CB                      = 6;
//  SSL_CTRL_SET_TMP_ECDH_CB                    = 7;     // V8.51 gone in 1.1.0
    SSL_CTRL_GET_SESSION_REUSED                 = 8;     // V8.51 gone in 1.1.0
    SSL_CTRL_GET_CLIENT_CERT_REQUEST            = 9;
    SSL_CTRL_GET_NUM_RENEGOTIATIONS             = 10;
    SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS           = 11;
    SSL_CTRL_GET_TOTAL_RENEGOTIATIONS           = 12;
    SSL_CTRL_GET_FLAGS                          = 13;
    SSL_CTRL_EXTRA_CHAIN_CERT                   = 14;
    SSL_CTRL_SET_MSG_CALLBACK                   = 15;
    SSL_CTRL_SET_MSG_CALLBACK_ARG               = 16;
    SSL_CTRL_SET_MTU                            = 17; // only applies to datagram connections
    SSL_CTRL_SESS_NUMBER                        = 20;
    SSL_CTRL_SESS_CONNECT                       = 21;
    SSL_CTRL_SESS_CONNECT_GOOD                  = 22;
    SSL_CTRL_SESS_CONNECT_RENEGOTIATE           = 23;
    SSL_CTRL_SESS_ACCEPT                        = 24;
    SSL_CTRL_SESS_ACCEPT_GOOD                   = 25;
    SSL_CTRL_SESS_ACCEPT_RENEGOTIATE            = 26;
    SSL_CTRL_SESS_HIT                           = 27;
    SSL_CTRL_SESS_CB_HIT                        = 28;
    SSL_CTRL_SESS_MISSES                        = 29;
    SSL_CTRL_SESS_TIMEOUTS                      = 30;
    SSL_CTRL_SESS_CACHE_FULL                    = 31;
    SSL_CTRL_OPTIONS                            = 32;     // V8.51 gone in 1.1.0
    SSL_CTRL_MODE                               = 33;
    SSL_CTRL_GET_READ_AHEAD                     = 40;
    SSL_CTRL_SET_READ_AHEAD                     = 41;
    SSL_CTRL_SET_SESS_CACHE_SIZE                = 42;
    SSL_CTRL_GET_SESS_CACHE_SIZE                = 43;
    SSL_CTRL_SET_SESS_CACHE_MODE                = 44;
    SSL_CTRL_GET_SESS_CACHE_MODE                = 45;
    SSL_CTRL_GET_MAX_CERT_LIST                  = 50;   // V8.01
    SSL_CTRL_SET_MAX_CERT_LIST                  = 51;   // V8.01
    SSL_CTRL_SET_MAX_SEND_FRAGMENT              = 52;   // V8.01
    SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           = 53;
    SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          = 54;
    SSL_CTRL_SET_TLSEXT_HOSTNAME                = 55;
    SSL_CTRL_SET_TLSEXT_DEBUG_CB                = 56;
    SSL_CTRL_SET_TLSEXT_DEBUG_ARG               = 57;
    SSL_CTRL_GET_TLSEXT_TICKET_KEYS             = 58;   // V8.01
    SSL_CTRL_SET_TLSEXT_TICKET_KEYS             = 59;   // V8.01
//  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        = 60;   // V8.01   // V8.51 gone in 1.1.0
//  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     = 61;   // V8.01   // V8.51 gone in 1.1.0
//  SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG   = 62;   // V8.01 // V8.51 gone in 1.1.0
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           = 63;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       = 64;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         = 65;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         = 66;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         = 67;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          = 68;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          = 69;   // V8.01
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    = 70;   // V8.01
    SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    = 71;   // V8.01
    SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB           = 72;   // V8.01
    DTLS_CTRL_GET_TIMEOUT                       = 73;   // V8.51
    DTLS_CTRL_HANDLE_TIMEOUT                    = 74;   // V8.51
    SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB        = 75;   // V8.01
    SSL_CTRL_SET_SRP_VERIFY_PARAM_CB            = 76;   // V8.01
    SSL_CTRL_GET_RI_SUPPORT                     = 76; { 0.9.8n }
    SSL_CTRL_CLEAR_OPTIONS                      = 77;   //  0.9.8n - V8.51 gone in 1.1.0
    SSL_CTRL_CLEAR_MODE                         = 78;   // V8.01
    SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB         = 77;   // V8.01
    SSL_CTRL_SET_SRP_ARG                        = 78;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_USERNAME           = 79;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH           = 80;   // V8.01
    SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD           = 81;   // V8.01
    SSL_CTRL_TLS_EXT_SEND_HEARTBEAT             = 85;   // V8.01
    SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING      = 86;   // V8.01
    SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS  = 87;   // V8.01
    SSL_CTRL_GET_EXTRA_CHAIN_CERTS              = 82;   // V8.01
    SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS            = 83;   // V8.01
    SSL_CTRL_CHAIN                              = 88;   // V8.01
    SSL_CTRL_CHAIN_CERT                         = 89;   // V8.01
    SSL_CTRL_GET_CURVES                         = 90;   // V8.01  V8.51 curves now named groups for 1.1.1
    SSL_CTRL_SET_CURVES                         = 91;   // V8.01
    SSL_CTRL_SET_CURVES_LIST                    = 92;   // V8.01
    SSL_CTRL_GET_SHARED_CURVE                   = 93;   // V8.01
    SSL_CTRL_GET_GROUPS                         = 90;   // V8.51
    SSL_CTRL_SET_GROUPS                         = 91;   // V8.51
    SSL_CTRL_SET_GROUPS_LIST                    = 92;   // V8.51
    SSL_CTRL_GET_SHARED_GROUP                   = 93;   // V8.51
    SSL_CTRL_SET_ECDH_AUTO                      = 94;   // V8.01  // V8.51 gone in 1.1.0
    SSL_CTRL_SET_SIGALGS                        = 97;   // V8.01
    SSL_CTRL_SET_SIGALGS_LIST                   = 98;   // V8.01
    SSL_CTRL_CERT_FLAGS                         = 99;   // V8.01
    SSL_CTRL_CLEAR_CERT_FLAGS                   = 100;   // V8.01
    SSL_CTRL_SET_CLIENT_SIGALGS                 = 101;   // V8.01
    SSL_CTRL_SET_CLIENT_SIGALGS_LIST            = 102;   // V8.01
    SSL_CTRL_GET_CLIENT_CERT_TYPES              = 103;   // V8.01
    SSL_CTRL_SET_CLIENT_CERT_TYPES              = 104;   // V8.01
    SSL_CTRL_BUILD_CERT_CHAIN                   = 105;   // V8.01
    SSL_CTRL_SET_VERIFY_CERT_STORE              = 106;   // V8.01
    SSL_CTRL_SET_CHAIN_CERT_STORE               = 107;   // V8.01
    SSL_CTRL_GET_PEER_SIGNATURE_NID             = 108;   // V8.01
    SSL_CTRL_GET_SERVER_TMP_KEY                 = 109;   // V8.01
    SSL_CTRL_GET_RAW_CIPHERLIST                 = 110;   // V8.01
    SSL_CTRL_GET_EC_POINT_FORMATS               = 111;   // V8.01
    SSL_CTRL_GET_CHAIN_CERTS                    = 115;   // V8.01
    SSL_CTRL_SELECT_CURRENT_CERT                = 116;   // V8.01
    SSL_CTRL_SET_CURRENT_CERT                   = 117;   // V8.01
    SSL_CTRL_SET_DH_AUTO                        = 118;   // V8.27
    SSL_CTRL_CHECK_PROTO_VERSION                = 119;   // V8.01
    DTLS_CTRL_SET_LINK_MTU                      = 120;   // V8.01
    DTLS_CTRL_GET_LINK_MIN_MTU                  = 121;   // V8.01
    SSL_CTRL_GET_EXTMS_SUPPORT                  = 122;   // V8.27
    SSL_CTRL_SET_MIN_PROTO_VERSION              = 123;   // V8.27
    SSL_CTRL_SET_MAX_PROTO_VERSION              = 124;   // V8.27
    SSL_CTRL_SET_SPLIT_SEND_FRAGMENT            = 125;   // V8.27
    SSL_CTRL_SET_MAX_PIPELINES                  = 126;   // V8.27
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE         = 127;   // V8.51
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB           = 128;   // V8.51
    SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG       = 129;   // V8.51
    SSL_CTRL_GET_MIN_PROTO_VERSION              = 130;   // V8.51 1.1.1
    SSL_CTRL_GET_MAX_PROTO_VERSION              = 131;   // V8.51 1.1.1
    SSL_CTRL_GET_SIGNATURE_NID                  = 132;   // V8.69
    SSL_CTRL_GET_TMP_KEY                        = 133;   // V8.69

    SSL_CERT_SET_FIRST                          = 1;   // V8.27
    SSL_CERT_SET_NEXT                           = 2;   // V8.27
    SSL_CERT_SET_SERVER                         = 3;   // V8.27

    SSL_OP_MICROSOFT_SESS_ID_BUG                = $00000001;   // gone V8.51
    SSL_OP_NETSCAPE_CHALLENGE_BUG               = $00000002;   // gone V8.51

  // Allow initial connection to servers that don't support RI
    SSL_OP_LEGACY_SERVER_CONNECT                = $00000004;   // new V8.51
    SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG     = $00000008;   // gone V8.51
    SSL_OP_TLSEXT_PADDING                       = $00000010;   // V8.01
    SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG          = $00000000;   // gone V8.01
    SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER           = $00000020;   // gone V8.51
    SSL_OP_SAFARI_ECDHE_ECDSA_BUG               = $00000040;   // V8.01
    SSL_OP_MSIE_SSLV2_RSA_PADDING               = $00000000;   //gone V8.01
    SSL_OP_SSLEAY_080_CLIENT_DH_BUG             = $00000080;   // gone V8.51
    SSL_OP_TLS_D5_BUG                           = $00000100;   // gone V8.51
    SSL_OP_TLS_BLOCK_PADDING_BUG                = $00000200;   // gone V8.51

  // In TLSv1.3 allow a non-(ec)dhe based kex_mode
    SSL_OP_ALLOW_NO_DHE_KEX                     = $00000400;   // new V8.51

    // Disable SSL 3.0/TLS 1.0 CBC vulnerability workaround that was added
    // in OpenSSL 0.9.6d.  Usually (depending on the application protocol)
    // the workaround is not needed.  Unfortunately some broken SSL/TLS
    //implementations cannot handle it at all, which is why we include
    //it in SSL_OP_ALL.
    SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS          = $00000800;

    //* DTLS options */ since 0.9.8
    SSL_OP_NO_QUERY_MTU                         = $00001000;
    //Turn on Cookie Exchange (on relevant for servers)
    SSL_OP_COOKIE_EXCHANGE                      = $00002000;

    // Don't use RFC4507 ticket extension
    SSL_OP_NO_TICKET                            = $00004000;

    // Use Cisco's "speshul" version of DTLS_BAD_VER (as client)
    SSL_OP_CISCO_ANYCONNECT                     = $00008000;    // V8.01

    // As server, disallow session resumption on renegotiation
    SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION  = $00010000;

    // Don't use compression even if supported
    SSL_OP_NO_COMPRESSION                          = $00020000; // 1.0.0x

    // Permit unsafe legacy renegotiation { 0.9.8n }
    // which can be set with SSL_CTX_set_options(). This is really
    // not recommended unless you know what you are doing.
    SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION    = $00040000;

    // If set, always create a new key when using tmp_ecdh parameters
    SSL_OP_SINGLE_ECDH_USE                       = $00080000;  // V8.02 gone V8.51

  // Disable encrypt-then-mac
    SSL_OP_NO_ENCRYPT_THEN_MAC                   = $00080000;   // new V8.51

 // If set, always create a new key when using tmp_ecdh parameters
    SSL_OP_SINGLE_DH_USE                        = $00100000;    // gone V8.51

   // Set to always use the tmp_rsa key when doing RSA operations,
   // even when this violates protocol specs
    SSL_OP_EPHEMERAL_RSA                        = $00200000;     // gone V8.51

    // Set on servers to choose the cipher according to the server's
    // preferences */
    SSL_OP_CIPHER_SERVER_PREFERENCE             = $00400000;

    // If set, a server will allow a client to issue a SSLv3.0 version number
    // as latest version supported in the premaster secret, even when TLSv1.0
    // (version 3.1) was announced in the client hello. Normally this is
    // forbidden to prevent version rollback attacks.
    SSL_OP_TLS_ROLLBACK_BUG                     = $00800000;

   //  following lot deprecated for 1.1.0 and later, use min/max[proto instead
    SSL_OP_NO_SSLv2                             = $01000000;
    SSL_OP_NO_SSLv3                             = $02000000;
    SSL_OP_NO_TLSv1                             = $04000000;
    SSL_OP_NO_TLSv1_2                           = $08000000;     // V8.01
    SSL_OP_NO_TLSv1_1                           = $10000000;     // V8.01
    SSL_OP_NO_TLSv1_3                           = $20000000;     // new V8.51

// These next two were never actually used for anything since SSLeay
// zap so we have some more flags.
    SSL_OP_PKCS1_CHECK_1                        = $00000000;    // gone V8.01
    SSL_OP_PKCS1_CHECK_2                        = $00000000;    // gone V8.01

    SSL_OP_NETSCAPE_CA_DN_BUG                   = $20000000;

    SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG      = $40000000;    // gone V8.51

  // Disallow all renegotiation */
    SSL_OP_NO_RENEGOTIATION                     = $40000000;    // new V8.51

    // Make server add server-hello extension from early version of
    // cryptopro draft, when GOST ciphersuite is negotiated.
    // Required for interoperability with CryptoPro CSP 3.x
    SSL_OP_CRYPTOPRO_TLSEXT_BUG                 = $80000000; // 1.0.0x

    //SSL_OP_ALL: various bug workarounds that should be rather harmless.
    SSL_OP_ALL                                  = { $00000BFF; }   // V8.01
       (SSL_OP_CRYPTOPRO_TLSEXT_BUG OR SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS OR
        SSL_OP_LEGACY_SERVER_CONNECT OR SSL_OP_TLSEXT_PADDING OR SSL_OP_SAFARI_ECDHE_ECDSA_BUG);   // V8.51
    //SSL_OP_ALL                                  = $80000FFF; 1.0.0d


// Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
// when just a single record has been written):
    SSL_MODE_ENABLE_PARTIAL_WRITE               = $00000001;
// Make it possible to retry SSL_write() with changed buffer location (buffer
// contents must stay the same!); this is not the default to avoid the
// misconception that non-blocking SSL_write() behaves like non-blocking write():
    SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER         = $00000002;  { V8.51 following modes added } 
// Never bother the application with retries if the transport is blocking:
    SSL_MODE_AUTO_RETRY                         = $00000004;
// Don't attempt to automatically build certificate chain
    SSL_MODE_NO_AUTO_CHAIN                      = $00000008;
// Save RAM by releasing read and write buffers when they're empty. (SSL3 and
// TLS only.) Released buffers are freed.
    SSL_MODE_RELEASE_BUFFERS                    = $00000010;
// Send the current time in the Random fields of the ClientHello and
// ServerHello records for compatibility with hypothetical implementations
// that require it.
    SSL_MODE_SEND_CLIENTHELLO_TIME             = $00000020;
    SSL_MODE_SEND_SERVERHELLO_TIME             = $00000040;
// Send TLS_FALLBACK_SCSV in the ClientHello. To be set only by applications
// that reconnect with a downgraded protocol version; see
// draft-ietf-tls-downgrade-scsv-00 for details. DO NOT ENABLE THIS if your
// application attempts a normal handshake. Only use this in explicit
// fallback retries, following the guidance in
// draft-ietf-tls-downgrade-scsv-00.
    SSL_MODE_SEND_FALLBACK_SCSV                = $00000080;
// Support Asynchronous operation
    SSL_MODE_ASYNC                             = $00000100;    { 1.1.0 and later }

    SSL_SESS_CACHE_OFF                          = $0000;
    SSL_SESS_CACHE_CLIENT                       = $0001;
    SSL_SESS_CACHE_SERVER                       = $0002;
    SSL_SESS_CACHE_BOTH                         = (SSL_SESS_CACHE_CLIENT or SSL_SESS_CACHE_SERVER);
    SSL_SESS_CACHE_NO_AUTO_CLEAR                = $0080;
    //* enough comments already ... see SSL_CTX_set_session_cache_mode(3) */
    SSL_SESS_CACHE_NO_INTERNAL_LOOKUP           = $0100;
    SSL_SESS_CACHE_NO_INTERNAL_STORE            = $0200;
    SSL_SESS_CACHE_NO_INTERNAL                  = (SSL_SESS_CACHE_NO_INTERNAL_LOOKUP or SSL_SESS_CACHE_NO_INTERNAL_STORE);

    SSL_SESSION_CACHE_MAX_SIZE_DEFAULT          = (1024 * 20);

 { V8.27 values for handshake SSL_state up to 1.1.0, no longer used except in info callback }
    SSL_ST_CONNECT                              = $1000;
    SSL_ST_ACCEPT                               = $2000;
    SSL_ST_MASK                                 = $0FFF;
    SSL_ST_INIT                                 = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
    SSL_ST_BEFORE                               = $4000;
    SSL_ST_OK                                   = $03;
    SSL_ST_RENEGOTIATE                          = ($04 or SSL_ST_INIT);

    SSL_CB_LOOP                                 = 1;
    SSL_CB_EXIT                                 = 2;
    SSL_CB_READ                                 = 4;
    SSL_CB_WRITE                                = 8;
    SSL_CB_ALERT                                = $4000; // used in callback
    SSL_CB_READ_ALERT                           = (SSL_CB_ALERT or SSL_CB_READ);
    SSL_CB_WRITE_ALERT                          = (SSL_CB_ALERT or SSL_CB_WRITE);
    SSL_CB_ACCEPT_LOOP                          = (SSL_ST_ACCEPT or SSL_CB_LOOP);
    SSL_CB_ACCEPT_EXIT                          = (SSL_ST_ACCEPT or SSL_CB_EXIT);
    SSL_CB_CONNECT_LOOP                         = (SSL_ST_CONNECT or SSL_CB_LOOP);
    SSL_CB_CONNECT_EXIT                         = (SSL_ST_CONNECT or SSL_CB_EXIT);
    SSL_CB_HANDSHAKE_START                      = $10;
    SSL_CB_HANDSHAKE_DONE                       = $20;

    SSL_NOTHING                                 = 1;
    SSL_WRITING                                 = 2;
    SSL_READING                                 = 3;
    SSL_X509_LOOKUP                             = 4;

    // Used in SSL_set_shutdown()/SSL_get_shutdown()
    SSL_SENT_SHUTDOWN                           = 1;
    SSL_RECEIVED_SHUTDOWN                       = 2;

    X509_TRUST_COMPAT                           = 1; //AG
    X509_TRUST_SSL_CLIENT                       = 2; //AG
    X509_TRUST_SSL_SERVER                       = 3; //AG
    X509_TRUST_EMAIL                            = 4; //AG
    X509_TRUST_OBJECT_SIGN                      = 5; //AG
    X509_TRUST_OCSP_SIGN                        = 6; //AG
    X509_TRUST_OCSP_REQUEST                     = 7; //AG

    SSL_MAX_SSL_SESSION_ID_LENGTH               = 32; //AG
    SSL_MAX_SID_CTX_LENGTH                      = 32; //AG

    {* ExtensionType values from RFC 3546 *}
    TLSEXT_TYPE_server_name                     = 0;
    TLSEXT_TYPE_max_fragment_length             = 1;
    TLSEXT_TYPE_client_certificate_url          = 2;
    TLSEXT_TYPE_trusted_ca_keys                 = 3;
    TLSEXT_TYPE_truncated_hmac                  = 4;
    TLSEXT_TYPE_status_request                  = 5;
    TLSEXT_TYPE_elliptic_curves                 = 10;
    TLSEXT_TYPE_ec_point_formats                = 11;
    TLSEXT_TYPE_signature_algorithms            = 13;  { V8.56 }
    TLSEXT_TYPE_use_srtp                        = 14;  { V8.56 }
    TLSEXT_TYPE_heartbeat                       = 15;  { V8.56 }
    TLSEXT_TYPE_application_layer_protocol_negotiation = 16;  { V8.56 }
    TLSEXT_TYPE_signed_certificate_timestamp    = 18;  { V8.56 }
    TLSEXT_TYPE_padding                         = 21;  { V8.56 }
    TLSEXT_TYPE_encrypt_then_mac                = 22;  { V8.56 }
    TLSEXT_TYPE_extended_master_secret          = 23;  { V8.56 }
    TLSEXT_TYPE_session_ticket                  = 35;
    { As defined for TLS1.3 }
    TLSEXT_TYPE_psk                             = 41;  { V8.56 }
    TLSEXT_TYPE_early_data                      = 42;  { V8.56 }
    TLSEXT_TYPE_supported_versions              = 43;  { V8.56 }
    TLSEXT_TYPE_cookie                          = 44;  { V8.56 }
    TLSEXT_TYPE_psk_kex_modes                   = 45;  { V8.56 }
    TLSEXT_TYPE_certificate_authorities         = 47;  { V8.56 }
    TLSEXT_TYPE_post_handshake_auth             = 49;  { V8.56 }
    TLSEXT_TYPE_signature_algorithms_cert       = 50;  { V8.56 }
    TLSEXT_TYPE_key_share                       = 51;  { V8.56 }
 { Temporary extension type }
    TLSEXT_TYPE_renegotiate                     = $ff01;  { V8.56 }
    TLSEXT_TYPE_next_proto_neg                  = 13172;  { V8.64 }

    TLSEXT_MAXLEN_host_name                     = 255;
    TLSEXT_NAMETYPE_host_name                   = 0;
    TLSEXT_STATUSTYPE_ocsp                      = 1;  { V8.69 status request value from RFC3546 }

    SSL_TLSEXT_ERR_OK                           = 0;
    SSL_TLSEXT_ERR_ALERT_WARNING                = 1;
    SSL_TLSEXT_ERR_ALERT_FATAL                  = 2;
    SSL_TLSEXT_ERR_NOACK                        = 3;

  { V8.64 return from SSL_client_hello_cb_fn }
    SSL_CLIENT_HELLO_SUCCESS                    = 1;
    SSL_CLIENT_HELLO_ERROR                      = 0;
    SSL_CLIENT_HELLO_RETRY                      = -1;

// V8.51 Extension context codes
// This extension is only allowed in TLS
    SSL_EXT_TLS_ONLY                        = $0001;
// This extension is only allowed in DTLS
    SSL_EXT_DTLS_ONLY                       = $0002;
// Some extensions may be allowed in DTLS but we don't implement them for it
    SSL_EXT_TLS_IMPLEMENTATION_ONLY         = $0004;
// Most extensions are not defined for SSLv3 but EXT_TYPE_renegotiate is
    SSL_EXT_SSL3_ALLOWED                    = $0008;
///Extension is only defined for TLS1.2 and below
    SSL_EXT_TLS1_2_AND_BELOW_ONLY           = $0010;
// Extension is only defined for TLS1.3 and above
    SSL_EXT_TLS1_3_ONLY                     = $0020;
// Ignore this extension during parsing if we are resuming
    SSL_EXT_IGNORE_ON_RESUMPTION            = $0040;
    SSL_EXT_CLIENT_HELLO                    = $0080;
// Really means TLS1.2 or below */
    SSL_EXT_TLS1_2_SERVER_HELLO             = $0100;
    SSL_EXT_TLS1_3_SERVER_HELLO             = $0200;
    SSL_EXT_TLS1_3_ENCRYPTED_EXTENSIONS     = $0400;
    SSL_EXT_TLS1_3_HELLO_RETRY_REQUEST      = $0800;
    SSL_EXT_TLS1_3_CERTIFICATE              = $1000;
    SSL_EXT_TLS1_3_NEW_SESSION_TICKET       = $2000;
    SSL_EXT_TLS1_3_CERTIFICATE_REQUEST      = $4000;

type
  { V8.57 whether an SSL server asks a client to send an SSL certificate }
    TSslCliCertMethod = (sslCliCertNone,
                         sslCliCertOptional,
                         sslCliCertRequire);

  { V8.57 certificate supplier protocol, determines which functions are used to get certificates }
    TSupplierProto = (SuppProtoNone, SuppProtoOwnCA, SuppProtoAcmeV2,    { V8.62 Acmev1 gone }
                      SuppProtoCertCentre, SuppProtoServtas);

 { V8.57 challenge types, differing certificate types support differing challenges,
     some have to be processed manually taking several days. }
    TChallengeType = (ChallNone, ChallFileUNC, ChallFileFtp, ChallFileSrv,
                      ChallFileApp, ChallDnsAuto, ChallDnsMan, ChallEmail,      { V8.64 DnsMan added }
                      ChallAlpnUNC, ChallAlpnSrv, ChallAlpnApp, ChallManual);   { V8.62 App added }

{ V8.40 OpenSSL streaming ciphers with various modes }
{ note OpenSSL 3 considers Blowfish, Cast, DES, IDEA, RC2/4/5 and SEED legacy and not supported by default }
{ pending support SM4, Camellia }
    TEvpCipher = (
        Cipher_none,
        Cipher_aes_128_cbc,
        Cipher_aes_128_cfb,
        Cipher_aes_128_ecb,
        Cipher_aes_128_ofb,
        Cipher_aes_128_gcm,
        Cipher_aes_128_ocb,
        Cipher_aes_128_ccm,
        Cipher_aes_192_cbc,
        Cipher_aes_192_cfb,
        Cipher_aes_192_ecb,
        Cipher_aes_192_ofb,
        Cipher_aes_192_gcm,
        Cipher_aes_192_ocb,
        Cipher_aes_192_ccm,
        Cipher_aes_256_cbc,    { used for PKC12 }
        Cipher_aes_256_cfb,
        Cipher_aes_256_ecb,
        Cipher_aes_256_ofb,
        Cipher_aes_256_gcm,
        Cipher_aes_256_ocb,
        Cipher_aes_256_ccm,
        Cipher_bf_cbc,        { blowfish needs key length set, 128, 192 or 256 }
        Cipher_bf_cfb64,
        Cipher_bf_ecb,
        Cipher_bf_ofb,
        Cipher_chacha20,      { chacha20 fixed 256 key }
        Cipher_des_ede3_cbc,
        Cipher_des_ede3_cfb64,
        Cipher_des_ede3_ecb,
        Cipher_des_ede3_ofb,
        Cipher_idea_cbc,      { IDEA fixed 128 key }
        Cipher_idea_cfb64,
        Cipher_idea_ecb,
        Cipher_idea_ofb);


{ V8.40 OpenSSL message digests or hashes }
{ note OpenSSL 3 considers MD5, MDC2, Whirlppol and RIPEMD legacy and not supported by default }
    TEvpDigest = (
        Digest_md5,
        Digest_mdc2,
        Digest_sha1,
        Digest_sha224,
        Digest_sha256,
        Digest_sha384,
        Digest_sha512,
        Digest_ripemd160,
        Digest_sha3_224,    { following V8.51 }
        Digest_sha3_256,
        Digest_sha3_384,
        Digest_sha3_512,
        Digest_shake128,
        Digest_shake256,
        Digest_None);       { V8.52 }

{ V8.40 ICS private key algorithm and key length in bits }
{ bracketed comment is security level and effective bits,
  beware long RSA key lengths increase SSL overhead heavily }
{ creating new RSA keys is computationally expensive, 4096 bits
  a couple of seconds, 7680 maybe a minute, 15360 hours }
    TSslPrivKeyType = (
        PrivKeyRsa1024,   { level 1 - 80 bits  }
        PrivKeyRsa2048,   { level 2 - 112 bits }
        PrivKeyRsa3072,   { level 3 - 128 bits }
        PrivKeyRsa4096,   { level 3 - 128 bits }
        PrivKeyRsa7680,   { level 4 - 192 bits }
        PrivKeyRsa15360,  { level 5 - 256 bits }
        PrivKeyECsecp256, { level 3 - 128 bits secp256r1 }
        PrivKeyECsecp384, { level 4 - 192 bits }
        PrivKeyECsecp512, { level 5 - 256 bits }
        PrivKeyEd25519,   { level 3 - 128 bits }    { V8.50 was PrivKeyECX25519 }
        PrivKeyRsaPss2048,   { level 2 - 112 bits } { V8.51 several RsaPss keys }
        PrivKeyRsaPss3072,   { level 3 - 128 bits }
        PrivKeyRsaPss4096,   { level 3 - 128 bits }
        PrivKeyRsaPss7680,   { level 4 - 192 bits }
        PrivKeyRsaPss15360,  { level 5 - 256 bits }
        PrivKeyECsecp256k);  { level 3 - 128 bits secp256k1 }    { V8.67 alternate kobitz curve }

{ V8.40 ICS private key file encryption and mapping to OpenSSL params }
   TSslPrivKeyCipher = (
        PrivKeyEncNone,
        PrivKeyEncTripleDES,    { V8.67 legacy algorithms need special loaded in OpenSSL 3.0 }
        PrivKeyEncIDEA,
        PrivKeyEncAES128,
        PrivKeyEncAES192,
        PrivKeyEncAES256);
  //      PrivKeyEncBlowfish128);
      {  PrivKeyEncBlowfish192,   V8.62 now sure we need these
        PrivKeyEncBlowfish256); }

const
    SslPrivKeyEvpCipher: array[TSslPrivKeyCipher] of TEvpCipher = (
        Cipher_none,
        Cipher_des_ede3_cbc,
        Cipher_idea_cbc,
        Cipher_aes_128_cbc,
        Cipher_aes_192_cbc,
        Cipher_aes_256_cbc);
     //   Cipher_bf_cbc);

 //   SslPrivKeyEvpBits: array[TSslPrivKeyCipher] of integer = (    { V8.67 Blowfish gone }
 //        0,0,0,0,0,0,128{,192,256});

type
   { V8.57 SSL/TLS certifioate root validation method }
    TCertVerMethod   = (CertVerNone, CertVerBundle, CertVerWinStore);

   { V8.57 Logging debug level }
    THttpDebugLevel  = (DebugNone, DebugConn, DebugParams, DebugSsl, DebugHdr, DebugBody, DebugSslLow);

   { V8.40 options to read pkey and inters from cert PEM and P12 files,
     croTry will silently fail, croYes will fail with exception  }
    TCertReadOpt = (croNo, croTry, croYes);             { V8.39 }

   { V8.41 SSL/TLS certificate validation result, V8.57 added None }
    TChainResult = (chainOK, chainFail, chainWarn, chainNone);

   { V8.40 1.1.0 and later, sets OpenSSL security level to a number }
   { V8.65 NOTE OpenSSL 3.0.0 and later block SHA1, MD5, TLS1, TLS1.1 except for sslSecLevelAny }
    TSslSecLevel = (
                     sslSecLevelAny,        { 0 - anything allowed, old compatibility }
                     sslSecLevel80bits,     { 1 - default, RSA/DH keys=>1024, ECC=>160, no MD5 }
                     sslSecLevel112bits,    { 2 - RSA/DH keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs }
                     sslSecLevel128bits,    { 3 - RSA/DH keys=>3072, ECC=>256, FS forced, no TLS/1.0  }
                     sslSecLevel192bits,    { 4 - RSA/DH keys=>7680, ECC=>384, no SHA1 suites, no TLS/1.1  }
                     sslSecLevel256bits);   { 5 - RSA/DH keys=>15360, ECC=>512  }

   { V8.45 SSL server security level, used by TIcsHost, sets protocol, cipher and SslSecLevel }
   { warning, requiring key lengths higher than 2048 requires all SSL certificates in the chain to
     have that minimum key length, including the root }
   { V8.55 sslSrvSecInter/FS, sslCliSecInter now requires TLS1.1, PCI council EOF TLS1.0 30 June 2018 }
   { V8.60 added sslSrvSecTls12Less and sslSrvSecTls13Only to disable TLS1.3 if it fails }
   { V8.66 Updated SslSrvSecurity levels: sslSrvSecInter, sslSrvSecInterFS and sslSrvSecHigh now all the
      same TLSv1.2 or 1.3, sslSrvSecTls12Less now TLSv1.2 only, sslSrvSecSsl3 not supported, only
      sslSrvSecBack supports TLSv1 and 1.1, sslSrvSecTls13Only unchanged TLSv1.3 only. }

    TSslSrvSecurity = (
                     sslSrvSecNone,         { 0 - all protocols and ciphers, any key lengths }
                     sslSrvSecSsl3,         { 1 - was SSL3 only, no longer supported }
                     sslSrvSecBack,         { 2 - TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     sslSrvSecInter,        { 3 - now same as sslSrvSecHigh }
                     sslSrvSecInterFS,      { 4 - now same as sslSrvSecHigh }
                     sslSrvSecHigh,         { 5 - TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslSrvSecHigh128,      { 6 - TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     sslSrvSecHigh192,      { 7 - TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
                     sslSrvSecTls12Less,    { 8 - TLSv1.2 only, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslSrvSecTls13Only);   { 9 - TLSv1.3 only, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }

const
    sslSrvSecDefault = sslSrvSecHigh;       { V8.55 recommended default, V8.64 improved default since TLSv1.1 disabled in most browsers from early 2020 }

type
   { V8.54 SSL client security level, used by context, sets protocol, cipher and SslSecLevel }
    TSslCliSecurity = (
                     sslCliSecIgnore,       { 0 - ignore, use old settings }
                     sslCliSecNone,         { 1 - all protocols and ciphers, any key lengths }
                     sslCliSecSsl3Only,     { 2 - SSLv3 only, all ciphers, any key lengths, MD5 }
                     sslCliSecTls1Only,     { 3 - TLSv1 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls11Only,    { 4 - TLSv1.1 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls12Only,    { 5 - TLSv1.2 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls13Only,    { 6 - TLSv1.3 only, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecTls1,         { 7 - TLSv1 or later, all ciphers, RSA/DH keys=>1024 }
                     sslCliSecTls11,        { 8 - TLSv1.1 or later, all ciphers, RSA/DH keys=>1024 }
                     sslCliSecTls12,        { 9 - TLSv1.2 or later, all ciphers, RSA/DH keys=>2048 }
                     sslCliSecBack,         { 10 - TLSv1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                     sslCliSecInter,        { 11 - TLSv1.1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslCliSecHigh,         { 12 - TLSv1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                     sslCliSecHigh128,      { 13 - TLSv1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                     sslCliSecHigh192);     { 14 - TLSv1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }

const
    sslCliSecDefault = sslCliSecTls12;  { V8.55 recommended default, V8.69 changed to 1.2 or better }


{$IFNDEF YuOpenSSL}  { only needed if using external DLL, otherwise all defined in YuOpenSSL.dcu }
const
    BIO_f_ssl :                              function : PBIO_METHOD; cdecl = nil;
    SSL_CIPHER_description :                 function(Cipher: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl = nil;
    SSL_CIPHER_get_bits :                    function(Cipher, Alg_Bits: Pointer): Integer; cdecl = nil;
    SSL_CIPHER_get_name :                    function(Cipher: Pointer): PAnsiChar; cdecl = nil;
    SSL_CTX_add_client_CA :                  function(C: PSSL_CTX; CaCert: PX509): Integer; cdecl = nil; //AG
    SSL_CTX_callback_ctrl:                   function(ctx: PSSL_CTX; cb_id: Integer; fp: Pointer{TCallback_ctrl_fp}): Integer; cdecl = nil;
    SSL_CTX_check_private_key :              function(Ctx: PSSL_CTX): integer; cdecl = nil;     { V8.40 }
    SSL_CTX_ctrl :                           function(C: PSSL_CTX; Cmd: Integer; LArg: Integer; PArg: PAnsiChar): Integer; cdecl = nil;
    SSL_CTX_free :                           procedure(C: PSSL_CTX); cdecl = nil;
    SSL_CTX_get0_certificate :               function(Ctx: PSSL_CTX): PX509; cdecl = nil;       { V8.40 }
    SSL_CTX_get0_param :                     function(Ctx: PSSL_CTX): PX509_VERIFY_PARAM; cdecl = nil;                { V8.39 1.0.2 }
    SSL_CTX_get0_privatekey :                function(Ctx: PSSL_CTX): PEVP_PKEY; cdecl = nil;   { V8.40 }
    SSL_CTX_get0_security_ex_data :          function(Ctx: PSSL_CTX): Pointer; cdecl = nil;            { V8.40 }
    SSL_CTX_get_cert_store :                 function(const Ctx: PSSL_CTX): PX509_STORE; cdecl = nil; //AG
    SSL_CTX_get_client_cert_cb:              function(CTX: PSSL_CTX): Pointer{TClient_cert_cb}; cdecl = nil; //AG
    SSL_CTX_get_ex_data :                    function(const C: PSSL_CTX; Idx: Integer): PAnsiChar; cdecl = nil;
    SSL_CTX_get_security_level :             function(Ctx: PSSL_CTX): Integer; cdecl = nil;             { V8.40 }
    SSL_CTX_get_verify_depth :               function(const ctx: PSSL_CTX): Integer; cdecl = nil; //AG
    SSL_CTX_get_verify_mode :                function(const C: PSSL_CTX): Integer; cdecl = nil; //AG
    SSL_CTX_load_verify_locations :          function(C: PSSL_CTX; const FileName: PAnsiChar; const SearchPath: PAnsiChar): Integer; cdecl = nil;
    SSL_CTX_new :                            function(Meth: PSSL_METHOD): PSSL_CTX; cdecl = nil;
    SSL_CTX_sess_get_get_cb:                 function(CTX: PSSL_CTX): Pointer{TGet_session_cb}; cdecl = nil; //AG
    SSL_CTX_sess_get_new_cb:                 function (CTX: PSSL_CTX): Pointer{TNew_session_cb}; cdecl = nil; //AG
    SSL_CTX_sess_get_remove_cb:              function(CTX: PSSL_CTX): Pointer{TRemove_session_cb}; cdecl = nil; //AG
    SSL_CTX_sess_set_get_cb:                 procedure(Ctx: PSSL_CTX; CB: Pointer{TGet_session_cb}); cdecl = nil; //AG
    SSL_CTX_sess_set_new_cb:                 procedure(Ctx: PSSL_CTX; CB: Pointer{TNew_session_cb}); cdecl = nil; //AG
    SSL_CTX_sess_set_remove_cb:              procedure(Ctx: PSSL_CTX; CB: Pointer{TRemove_session_cb}); cdecl = nil; //AG
    SSL_CTX_set0_security_ex_data :          procedure(Ctx: PSSL_CTX;  ex: Pointer);  cdecl = nil;      { V8.40 }
    SSL_CTX_set1_param :                     function(Ctx: PSSL_CTX; vpm: PX509_VERIFY_PARAM): integer; cdecl = nil;  { V8.39 1.0.2 }
    SSL_CTX_set_alpn_protos :                function(Ctx: PSSL_CTX; protos: Pointer; protos_len: integer): integer; cdecl = nil;  { V8.56 }
    SSL_CTX_set_alpn_select_cb :             procedure(Ctx: PSSL_CTX; cb: Pointer{TSsl_alpn_cb}; arg: Pointer); cdecl = nil;  { V8.56 }
    SSL_CTX_set_cipher_list :                function(C: PSSL_CTX; CipherString: PAnsiChar): Integer; cdecl = nil;
    SSL_CTX_set_client_CA_list :             procedure(C: PSSL_CTX; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG
    SSL_CTX_set_client_cert_cb:              procedure(CTX: PSSL_CTX; CB: Pointer{TClient_cert_cb}); cdecl = nil; //AG
    SSL_CTX_set_client_hello_cb :            procedure(C: PSSL_CTX; cb: Pointer{TSsl_client_hello_cb}; arg: Pointer); cdecl = nil;  { V8.64 }
    SSL_CTX_set_default_passwd_cb :          procedure(C: PSSL_CTX; CallBack: Pointer{TPem_password_cb}); cdecl = nil;
    SSL_CTX_set_default_passwd_cb_userdata : procedure(C: PSSL_CTX; UData: Pointer); cdecl = nil;
    SSL_CTX_set_default_verify_paths :       function(C: PSSL_CTX): Integer; cdecl = nil;
    SSL_CTX_set_ex_data :                    function(C: PSSL_CTX; Idx: Integer; Arg: PAnsiChar): Integer; cdecl = nil;
    SSL_CTX_set_info_callback:               procedure(ctx: PSSL_CTX; cb : Pointer{TSetInfo_cb}); cdecl = nil;
    SSL_CTX_set_msg_callback :               procedure(Ctx: PSSL_CTX; cb: Pointer{TProto_msg_cb}); cdecl = nil;  { V8.40 }
    SSL_CTX_set_security_callback :          procedure(Ctx: PSSL_CTX; cb: Pointer{TSecurity_level_cb}); cdecl = nil;   { V8.40 }
    SSL_CTX_set_security_level :             procedure(Ctx: PSSL_CTX; level: Integer); cdecl = nil;     { V8.40 }
    SSL_CTX_set_session_id_context :         function(Ctx: PSSL_CTX; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    SSL_CTX_set_timeout :                    function(Ctx: PSSL_CTX; Timeout: Cardinal): Cardinal; cdecl = nil;
    SSL_CTX_set_trust :                      function(C: PSSL_CTX; Trust: Integer): Integer; cdecl = nil; //AG
    SSL_CTX_set_verify :                     procedure(C: PSSL_CTX; Mode: Integer; CallBack : Pointer{TSetVerify_cb}); cdecl = nil;
    SSL_CTX_set_verify_depth :               procedure(C: PSSL_CTX; Depth: Integer); cdecl = nil;
    SSL_CTX_use_PrivateKey :                 function(C: PSSL_CTX; pkey: PEVP_PKEY): Integer; cdecl = nil;
    SSL_CTX_use_PrivateKey_file :            function(C: PSSL_CTX; const FileName: PAnsiChar; CertType: Integer): Integer; cdecl = nil;
    SSL_CTX_use_certificate :                function(C: PSSL_CTX; Cert: PX509): Integer; cdecl = nil;     { V8.27 }
    SSL_CTX_use_certificate_chain_file :     function(C: PSSL_CTX; const FileName: PAnsiChar): Integer; cdecl = nil;
    SSL_CTX_use_certificate_file :           function(C: PSSL_CTX; const FileName: PAnsiChar; type_: Integer): Integer; cdecl = nil; //AG
    SSL_SESSION_get_id:                      function (const Ses: PSSL_SESSION; Len: PInteger): PAnsiChar; cdecl = nil; //AG    { V8.66 var gone }
    SSL_SESSION_get_time :                   function(const Sess: PSSL_SESSION): Cardinal; cdecl = nil;
    SSL_SESSION_get_timeout :                function(const Sess: PSSL_SESSION): Cardinal; cdecl = nil;
    SSL_SESSION_set_time :                   function(Sess: PSSL_SESSION; T: Cardinal): Cardinal; cdecl = nil;
    SSL_SESSION_set_timeout :                function(Sess: PSSL_SESSION; T: Cardinal): Cardinal; cdecl = nil;
    SSL_accept :                             function(S: PSSL): Integer; cdecl = nil;
    SSL_add_client_CA :                      function(ssl: PSSL; CaCert: PX509): Integer; cdecl = nil; //AG
    SSL_alert_desc_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    SSL_alert_type_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    SSL_bytes_to_cipher_list :               function(s: PSSL; cbytes: PAnsiChar; len: size_t; isv2format: integer; sk: PPSTACK_OF_SSL_CIPHER; scvsvs: PPSTACK_OF_SSL_CIPHER): Integer; cdecl = nil;   { V8.64, V8.66 var gone }
    SSL_callback_ctrl:                       function(s: PSSL; cb_id: Integer; fp: Pointer{TCallback_ctrl_fp}): Integer; cdecl = nil;
    SSL_clear :                              function(S: PSSL): Integer; cdecl = nil;    { V8.64 was procedure }
    SSL_client_hello_isv2 :                  function(s: PSSL): Integer; cdecl = nil;    { V8.64 }
    SSL_client_hello_get0_legacy_version :   function(s: PSSL): Cardinal; cdecl = nil;   { V8.64 }
    SSL_client_hello_get0_random :           function(s: PSSL; OutData: PPAnsiChar): size_t; cdecl = nil;       { V8.64 }   { V8.66 var gone }
    SSL_client_hello_get0_session_id :       function(s: PSSL; OutData: PPAnsiChar): size_t; cdecl = nil;       { V8.64 }   { V8.66 var gone }
    SSL_client_hello_get0_ciphers :          function(s: PSSL; OutData: PPAnsiChar): size_t; cdecl = nil;       { V8.64 }   { V8.66 var gone }
    SSL_client_hello_get0_compression_methods : function(s: PSSL; OutData: PPAnsiChar): size_t; cdecl = nil;    { V8.64 }   { V8.66 var gone }
    SSL_client_hello_get1_extensions_present :  function(s: PSSL; OutData: PPAnsiChar; OutLen: Psize_t): Integer; cdecl = nil;        { V8.64 }   { V8.66 var gone }
    SSL_client_hello_get0_ext :              function(s: PSSL; EType: Cardinal; OutData: PPAnsiChar; OutLen: Psize_t): Integer; cdecl = nil; { V8.64 }  { V8.66 var gone }
    SSL_connect :                            function(S: PSSL): Integer; cdecl = nil;
    SSL_ctrl :                               function(S: PSSL; Cmd: Integer; LArg: Integer; PArg: Pointer): Integer; cdecl = nil;
    SSL_do_handshake :                       function(S: PSSL): Integer; cdecl = nil; //AG
    SSL_free :                               procedure(S: PSSL); cdecl = nil;
    SSL_get0_alpn_selected :                 procedure(S: PSSL; data: PPointer; len: PInteger); cdecl = nil;  { V8.56 }
    SSL_get0_param :                         function(S: PSSL): PX509_VERIFY_PARAM; cdecl = nil;                      { V8.39 1.0.2 }
    SSL_get0_security_ex_data :              function(S: PSSL): Pointer; cdecl = nil;                  { V8.40 }
    SSL_get1_session :                       function(S: PSSL): PSSL_SESSION; cdecl = nil;
    SSL_get1_supported_ciphers :             function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    SSL_get_SSL_CTX:                         function(const S: PSSL): PSSL_CTX; cdecl = nil;
    SSL_get_cipher_list :                    function(S: PSSL; Priority: Integer): PAnsiChar; cdecl = nil;  { V8.27 }
    SSL_get_ciphers :                        function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    SSL_get_client_CA_list :                 function(const S: PSSL): PSTACK_OF_X509_NAME; cdecl = nil;
    SSL_get_client_ciphers :                 function(S: PSSL): PSTACK_OF_SSL_CIPHER; cdecl = nil;   { V8.27 }
    SSL_get_current_cipher :                 function(S: PSSL): Pointer; cdecl = nil;
    SSL_get_error :                          function(S: PSSL; ErrCode: Integer): Integer; cdecl = nil;
    SSL_get_ex_data :                        function(S: PSSL; Idx: Integer): Pointer; cdecl = nil;
    SSL_get_ex_data_X509_STORE_CTX_idx:      function: Integer; cdecl = nil;
    SSL_get_fd:                              function(S: PSSL): Integer; cdecl = nil; // B.S.
    SSL_get_peer_cert_chain :                function(const S: PSSL): PSTACK_OF_X509; cdecl = nil;
    SSL_get_peer_certificate :               function(S: PSSL): PX509; cdecl = nil;                  { V8.67 now exported as SSL_get1_peer_certificate in OpenSSL 3.0 } 
    SSL_get_rbio :                           function(S: PSSL): PBIO; cdecl = nil;
    SSL_get_rfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.
    SSL_get_servername:                      function(const S: PSSL; const type_: Integer): PAnsiChar; cdecl = nil;
    SSL_get_servername_type:                 function(const S: PSSL): Integer; cdecl = nil;
    SSL_get_session :                        function(S: PSSL): PSSL_SESSION; cdecl = nil;
    SSL_get_shutdown :                       function(S: PSSL): Integer; cdecl = nil;
    SSL_get_security_level :                 function(S: PSSL): Integer; cdecl = nil;                   { V8.40 }
    SSL_get_state :                          function(S: PSSL): OSSL_HANDSHAKE_STATE; cdecl = nil;   { V8.27, V8.66 }
    SSL_get_verify_depth :                   function(const S: PSSL): Integer; cdecl = nil;
    SSL_get_verify_result :                  function(S: PSSL): Integer; cdecl = nil;
    SSL_get_version :                        function(S: PSSL): PAnsiChar; cdecl = nil;
    SSL_get_wbio :                           function(S: PSSL): PBIO; cdecl = nil;
    SSL_get_wfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.
    SSL_is_server :                          function(S: PSSL): Integer; cdecl = nil;                                    { V8.69 }
    SSL_load_client_CA_file :                function(const FileName: PAnsiChar): PSTACK_OF_X509_NAME; cdecl = nil; //AG
    SSL_new :                                function(Ctx: PSSL_CTX): PSSL; cdecl = nil;
    SSL_read :                               function(S: PSSL; Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    SSL_renegotiate :                        function(S: PSSL): Integer; cdecl = nil; //AG
    SSL_renegotiate_pending :                function(S: PSSL): Integer; cdecl = nil; //AG
    SSL_select_next_proto :                  function (output: PPointer; outlen: PInteger; input: Pointer; inlen: Integer; client: Pointer; client_len: Integer): Integer; cdecl = nil; { V8.56 } { V8.66 var gone }
    SSL_session_free :                       procedure(Session: PSSL_SESSION); cdecl = nil;
    SSL_set0_security_ex_data :              procedure(S: PSSL;  ex: Pointer);  cdecl = nil;            { V8.40 }
    SSL_set1_param :                         function(S: PSSL; vpm: PX509_VERIFY_PARAM): integer; cdecl = nil;        { V8.39 1.0.2 }
    SSL_set_SSL_CTX:                         function(S: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl = nil;
    SSL_set_accept_state :                   procedure(S: PSSL); cdecl = nil; //AG
    SSL_set_alpn_protos :                    function(S: PSSL; protos: TBytes; protos_len: integer): integer; cdecl = nil;  { V8.56 }
    SSL_set_bio :                            procedure(S: PSSL; RBio: PBIO; WBio: PBIO); cdecl = nil;
    SSL_set_client_CA_list :                 procedure(s: PSSL; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG
    SSL_set_connect_state :                  procedure(S: PSSL); cdecl = nil;
    SSL_set_ex_data :                        function(S: PSSL; Idx: Integer; Arg: Pointer): Integer; cdecl = nil;
    SSL_set_fd:                              function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    SSL_set_info_callback :                  procedure(S: PSSL; cb : Pointer{TSetInfo_cb}); cdecl = nil;
    SSL_set_msg_callback :                   procedure(S: PSSL; cb: Pointer{TProto_msg_cb}); cdecl = nil;        { V8.40 }
    SSL_set_rfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    SSL_set_security_level :                 procedure(S: PSSL; level: Integer); cdecl = nil;           { V8.40 }
    SSL_set_security_callback :              procedure(S: PSSL; cb: Pointer{TSecurity_level_cb}); cdecl = nil;   { V8.40 }
    SSL_set_session :                        function(S: PSSL; Session: PSSL_SESSION): Integer; cdecl = nil;
    SSL_set_session_id_context :             function(S: PSSL; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    SSL_set_shutdown :                       procedure(S: PSSL; Mode: Integer); cdecl = nil;
    SSL_set_verify :                         procedure(S: PSSL; Mode: Integer; CallBack : Pointer{TSetVerify_cb}); cdecl = nil;
    SSL_set_verify_result :                  procedure(S: PSSL; VResult: Integer); cdecl = nil;
    SSL_set_wfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    SSL_shutdown :                           function(S: PSSL): Integer; cdecl = nil;
    SSL_state_string :                       function(S: PSSL): PAnsiChar; cdecl = nil;    { V8.40 }
    SSL_state_string_long :                  function(S: PSSL): PAnsiChar; cdecl = nil;
    SSL_version :                            function(const S: PSSL): Integer; cdecl = nil; //AG
    SSL_want :                               function(S: PSSL): Integer; cdecl = nil;
    SSL_write :                              function(S: PSSL; const Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    TLS_client_method :                      function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    TLS_method :                             function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    TLS_server_method :                      function: PSSL_METHOD; cdecl = nil;   { V8.27 }
    d2i_SSL_SESSION :                        function(Session: PPSSL_SESSION; const pp: PPAnsiChar; Length: Cardinal): PSSL_SESSION; cdecl = nil;
    i2d_SSL_SESSION :                        function(InSession: PSSL_SESSION; pp: PPAnsiChar): Integer; cdecl = nil;
    SSL_CTX_set_options :                    function(C: PSSL_CTX; Op: Integer): Integer; cdecl = nil;  // V8.51 x_options exported as of 1.1.0
    SSL_CTX_get_options :                    function(C: PSSL_CTX): Integer; cdecl = nil;               // V8.51 x_options exported as of 1.1.0
    SSL_CTX_clear_options :                  function(C: PSSL_CTX; Op: Integer): Integer;  cdecl = nil; // V8.51 x_options exported as of 1.1.0
    SSL_get_options :                        function(S: PSSL): Integer; cdecl = nil;                   // V8.51 x_options exported as of 1.1.0
    SSL_set_options :                        function(S: PSSL; Op: Integer): Integer; cdecl = nil;      // V8.51 x_options exported as of 1.1.0
    SSL_clear_options :                      function(S: PSSL; Op: Integer): Integer;  cdecl = nil;     // V8.51 x_options exported as of 1.1.0
    SSL_session_reused :                     function(SSL: PSSL): Integer; cdecl = nil;                 // V8.51 exported as of 1.1.0
    BIO_new_ssl :                            function(Ctx: PSSL_CTX; Client: Boolean): PBIO; cdecl = nil;  // V8.51  not new, but not used before
    BIO_new_ssl_connect :                    function(Ctx: PSSL_CTX): PBIO; cdecl = nil;                   // V8.51
    BIO_new_buffer_ssl_connect :             function(Ctx: PSSL_CTX): PBIO; cdecl = nil;                   // V8.51
    BIO_ssl_copy_session_id :                function(BioTo: PBIO; BioFrom: PBIO): Integer ; cdecl = nil;  // V8.51
    BIO_ssl_shutdown :                       procedure(Bio: PBIO); cdecl = nil;                            // V8.51
{$IFNDEF OPENSSL_NO_ENGINE}
    SSL_CTX_set_client_cert_engine :         function(Ctx: PSSL_CTX; e: PENGINE): Integer; cdecl = nil; //AG
{$ENDIF}

{$ENDIF YuOpenSSL}


function SsleayLoad : Boolean;
function SslGetImports (Handle: THandle; List: array of TOSSLImports): string ;  { V8.35 }

{ V8.38 Windows API to check authenticode code signing digital certificate on OpenSSL files }
{$IFDEF MSWINDOWS} { V8.49 }
procedure IcsVerifySslDll (const Fname: string);
{$ENDIF}

// macro functions not exported from DLL
function  SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  SSL_CTX_set_tmp_dh(C: PSSL_CTX; DH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  SSL_CTX_set_tmp_ecdh(C: PSSL_CTX; ECDH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  SSL_set_tmp_dh(S: PSSL; DH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}            { V8.01 }
function  SSL_set_tmp_ecdh(S: PSSL; ECDH: Pointer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.01 }
function  SSL_CTX_set_min_proto_version(C: PSSL_CTX; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_set_max_proto_version(C: PSSL_CTX;  version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_set_min_proto_version(S: PSSL; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_set_max_proto_version(S: PSSL; version: integer) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_get_min_proto_version(C: PSSL_CTX) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}              { V8.51 added 1.1.1  }
function  SSL_CTX_get_max_proto_version(C: PSSL_CTX) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}              { V8.51 added 1.1.1  }
function  SSL_get_min_proto_version(S: PSSL) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}                      { V8.51 added 1.1.1  }
function  SSL_get_max_proto_version(S: PSSL) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}                      { V8.51 added 1.1.1  }
function  SSL_CTX_set0_chain(C: PSSL_CTX; sk: PSTACK_OF_X509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_add0_chain_cert(C: PSSL_CTX; Cert: PX509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_get0_chain_certs(C: PSSL_CTX; sk: PPSTACK_OF_X509): Integer;  {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_clear_chain_certs(C: PSSL_CTX): Integer;   {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }
function  SSL_CTX_build_cert_chain(C: PSSL_CTX; flags: integer): Integer;   {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.27 }

function Ics_SSL_set_tlsext_host_name(const S: PSSL; const name: String): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.66 renamed since not OpenSSL compat }
function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: Pointer{TCallback_ctrl_fp}): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function SSL_set_tlsext_debug_callback(S: PSSL; cb: Pointer{TCallback_ctrl_fp}): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}

{ V8.69 OCSP stapling macros, request OCSP during handshake and get it }
function SSL_get_tlsext_status_type(S: PSSL): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.69 }
function SSL_set_tlsext_status_type(S: PSSL; stype: integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.69 }
function SSL_get_tlsext_status_ocsp_resp(S: PSSL; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.69 }
function SSL_set_tlsext_status_ocsp_resp(S: PSSL; arg: Pointer; arglen: integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}    { V8.69 }
function SSL_CTX_get_tlsext_status_cb(ctx: PSSL_CTX; cb: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }
function SSL_CTX_set_tlsext_status_cb(ctx: PSSL_CTX; cb: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }
function SSL_CTX_get_tlsext_status_arg(ctx: PSSL_CTX; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }
function SSL_CTX_set_tlsext_status_arg(ctx: PSSL_CTX; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }
function SSL_CTX_get_tlsext_status_type(ctx: PSSL_CTX): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }
function SSL_CTX_set_tlsext_status_type(ctx: PSSL_CTX; stype: integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.69 }

function SSL_get1_groups(Ctx: PSSL_CTX; GList: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }
function SSL_get_shared_group(Ssl: PSSL; N: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }
function SSL_CTX_set1_groups(Ctx: PSSL_CTX; GList: Pointer; GListlen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }
function SSL_CTX_set1_groups_list(Ctx: PSSL_CTX; GList: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }
function SSL_set1_groups(Ctx: PSSL_CTX; GList: Pointer; GListlen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }
function SSL_set1_groups_list(Ctx: PSSL_CTX; GList: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF} { V8.51 OpenSSL 1.1.1 }

function  SSL_CTX_set_mode(C: PSSL_CTX; version: integer) : Integer;     { V8.51 }
function  SSL_CTX_get_mode(C: PSSL_CTX) : Integer;                       { V8.51 }
function  SSL_CTX_clear_mode(C: PSSL_CTX;  version: integer) : Integer;  { V8.51 }
function  SSL_set_mode(S: PSSL; version: integer) : Integer;             { V8.51 }
function  SSL_get_mode(S: PSSL) : Integer;                               { V8.51 }
function  SSL_clear_mode(S: PSSL; version: integer) : Integer;           { V8.51 }

function IcsSslStub: integer;                            { V8.35 }

procedure  SSL_CTX_set_msg_callback_arg(Ctx: PSSL_CTX; arg: Pointer);  {$IFDEF USE_INLINE} inline; {$ENDIF}  { V8.40 }
procedure  SSL_set_msg_callback_arg(S: PSSL; arg: Pointer); {$IFDEF USE_INLINE} inline; {$ENDIF}             { V8.40 }


{$IFNDEF YuOpenSSL}
// V8.35 all OpenSSL exports now in tables, with versions if only available conditionally
const
    GSSLEAYImports1: array[0..159] of TOSSLImports = (
    (F: @@BIO_f_ssl;                              N: 'BIO_f_ssl';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@BIO_new_buffer_ssl_connect;             N: 'BIO_new_buffer_ssl_connect';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),    { V8.51 }
    (F: @@BIO_new_ssl;                            N: 'BIO_new_ssl';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),    { V8.51 }
    (F: @@BIO_new_ssl_connect;                    N: 'BIO_new_ssl_connect';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),    { V8.51 }
    (F: @@BIO_ssl_copy_session_id;                N: 'BIO_ssl_copy_session_id';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),    { V8.51 }
    (F: @@BIO_ssl_shutdown;                       N: 'BIO_ssl_shutdown';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),    { V8.51 }
    (F: @@SSL_CIPHER_description;                 N: 'SSL_CIPHER_description';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CIPHER_get_bits;                    N: 'SSL_CIPHER_get_bits';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CIPHER_get_name;                    N: 'SSL_CIPHER_get_name';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_add_client_CA;                  N: 'SSL_CTX_add_client_CA';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_callback_ctrl;                  N: 'SSL_CTX_callback_ctrl';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_check_private_key;              N: 'SSL_CTX_check_private_key';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@SSL_CTX_clear_options;                  N: 'SSL_CTX_clear_options';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_CTX_ctrl;                           N: 'SSL_CTX_ctrl';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_free;                           N: 'SSL_CTX_free';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_get0_certificate;               N: 'SSL_CTX_get0_certificate';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@SSL_CTX_get0_param;                     N: 'SSL_CTX_get0_param';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@SSL_CTX_get0_privatekey;                N: 'SSL_CTX_get0_privatekey';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@SSL_CTX_get0_security_ex_data;          N: 'SSL_CTX_get0_security_ex_data';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_CTX_set_alpn_protos;                N: 'SSL_CTX_set_alpn_protos';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.56 }
    (F: @@SSL_CTX_set_alpn_select_cb;             N: 'SSL_CTX_set_alpn_select_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.56 }
    (F: @@SSL_CTX_get_cert_store;                 N: 'SSL_CTX_get_cert_store';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_get_client_cert_cb;             N: 'SSL_CTX_get_client_cert_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_get_ex_data;                    N: 'SSL_CTX_get_ex_data';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_get_options;                    N: 'SSL_CTX_get_options';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_CTX_get_security_level;             N: 'SSL_CTX_get_security_level';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_CTX_get_verify_depth;               N: 'SSL_CTX_get_verify_depth';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_get_verify_mode;                N: 'SSL_CTX_get_verify_mode';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_load_verify_locations;          N: 'SSL_CTX_load_verify_locations';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_new;                            N: 'SSL_CTX_new';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_get_get_cb;                N: 'SSL_CTX_sess_get_get_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_get_new_cb;                N: 'SSL_CTX_sess_get_new_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_get_remove_cb;             N: 'SSL_CTX_sess_get_remove_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_set_get_cb;                N: 'SSL_CTX_sess_set_get_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_set_new_cb;                N: 'SSL_CTX_sess_set_new_cb';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_sess_set_remove_cb;             N: 'SSL_CTX_sess_set_remove_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set0_security_ex_data;          N: 'SSL_CTX_set0_security_ex_data';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_CTX_set1_param;                     N: 'SSL_CTX_set1_param';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@SSL_CTX_set_cipher_list;                N: 'SSL_CTX_set_cipher_list';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_client_CA_list;             N: 'SSL_CTX_set_client_CA_list';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_client_cert_cb;             N: 'SSL_CTX_set_client_cert_cb';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_client_hello_cb;             N: 'SSL_CTX_set_client_hello_cb';              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_CTX_set_default_passwd_cb;          N: 'SSL_CTX_set_default_passwd_cb';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_default_passwd_cb_userdata; N: 'SSL_CTX_set_default_passwd_cb_userdata';    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_default_verify_paths;       N: 'SSL_CTX_set_default_verify_paths';          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_ex_data;                    N: 'SSL_CTX_set_ex_data';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_info_callback;              N: 'SSL_CTX_set_info_callback';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_msg_callback;               N: 'SSL_CTX_set_msg_callback';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@SSL_CTX_set_options;                    N: 'SSL_CTX_set_options';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_CTX_set_security_callback;          N: 'SSL_CTX_set_security_callback';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_CTX_set_security_level;             N: 'SSL_CTX_set_security_level';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_CTX_set_session_id_context;         N: 'SSL_CTX_set_session_id_context';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_timeout;                    N: 'SSL_CTX_set_timeout';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_trust;                      N: 'SSL_CTX_set_trust';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_verify;                     N: 'SSL_CTX_set_verify';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_set_verify_depth;               N: 'SSL_CTX_set_verify_depth';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_use_PrivateKey;                 N: 'SSL_CTX_use_PrivateKey';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_use_PrivateKey_file;            N: 'SSL_CTX_use_PrivateKey_file';               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_use_certificate;                N: 'SSL_CTX_use_certificate';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_use_certificate_chain_file;     N: 'SSL_CTX_use_certificate_chain_file';        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_CTX_use_certificate_file;           N: 'SSL_CTX_use_certificate_file';              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_free;                       N: 'SSL_SESSION_free';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_get_id;                     N: 'SSL_SESSION_get_id';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_get_time;                   N: 'SSL_SESSION_get_time';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_get_timeout;                N: 'SSL_SESSION_get_timeout';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_set_time;                   N: 'SSL_SESSION_set_time';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_SESSION_set_timeout;                N: 'SSL_SESSION_set_timeout';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_accept;                             N: 'SSL_accept';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_add_client_CA;                      N: 'SSL_add_client_CA';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_alert_desc_string_long;             N: 'SSL_alert_desc_string_long';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_alert_type_string_long;             N: 'SSL_alert_type_string_long';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_bytes_to_cipher_list;               N: 'SSL_bytes_to_cipher_list';                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_callback_ctrl;                      N: 'SSL_callback_ctrl';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_clear;                              N: 'SSL_clear';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_clear_options;                      N: 'SSL_clear_options';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_client_hello_isv2;                   N: 'SSL_client_hello_isv2';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_legacy_version;    N: 'SSL_client_hello_get0_legacy_version';     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_random;            N: 'SSL_client_hello_get0_random';             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_session_id;        N: 'SSL_client_hello_get0_session_id';         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_ciphers;           N: 'SSL_client_hello_get0_ciphers';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_compression_methods; N: 'SSL_client_hello_get0_compression_methods'; MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get1_extensions_present;  N: 'SSL_client_hello_get1_extensions_present';  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_client_hello_get0_ext;               N: 'SSL_client_hello_get0_ext';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.64 }
    (F: @@SSL_connect;                            N: 'SSL_connect';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_ctrl;                               N: 'SSL_ctrl';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_do_handshake;                       N: 'SSL_do_handshake';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_free;                               N: 'SSL_free';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get0_alpn_selected;                 N: 'SSL_get0_alpn_selected';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.56 }
    (F: @@SSL_get0_param;                         N: 'SSL_get0_param';                            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),     { V8.39 }
    (F: @@SSL_get0_security_ex_data;              N: 'SSL_get0_security_ex_data';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_get1_session;                       N: 'SSL_get1_session';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get1_supported_ciphers;             N: 'SSL_get1_supported_ciphers';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_SSL_CTX;                        N: 'SSL_get_SSL_CTX';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_cipher_list;                    N: 'SSL_get_cipher_list';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_ciphers;                        N: 'SSL_get_ciphers';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_client_CA_list;                 N: 'SSL_get_client_CA_list';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_client_ciphers;                 N: 'SSL_get_client_ciphers';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_current_cipher;                 N: 'SSL_get_current_cipher';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_error;                          N: 'SSL_get_error';                             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_ex_data;                        N: 'SSL_get_ex_data';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_ex_data_X509_STORE_CTX_idx;     N: 'SSL_get_ex_data_X509_STORE_CTX_idx';        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_fd;                             N: 'SSL_get_fd';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_options;                        N: 'SSL_get_options';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_get_peer_cert_chain;                N: 'SSL_get_peer_cert_chain';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_peer_certificate;               N: 'SSL_get_peer_certificate';                  MI: OSSL_VER_MIN; MX: OSSL_VER_1101ZZ),  { V8.64 export renamed for 3.0 }
    (F: @@SSL_get_peer_certificate;               N: 'SSL_get1_peer_certificate';                 MI: OSSL_VER_3000; MX: OSSL_VER_MAX),   { V8.67 new export name in 3.0 }
    (F: @@SSL_get_rbio;                           N: 'SSL_get_rbio';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_rfd;                            N: 'SSL_get_rfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_security_level;                 N: 'SSL_get_security_level';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_get_servername;                     N: 'SSL_get_servername';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_servername_type;                N: 'SSL_get_servername_type';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_session;                        N: 'SSL_get_session';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_shutdown;                       N: 'SSL_get_shutdown';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_state;                          N: 'SSL_get_state';                             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_verify_depth;                   N: 'SSL_get_verify_depth';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_verify_result;                  N: 'SSL_get_verify_result';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_version;                        N: 'SSL_get_version';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_wbio;                           N: 'SSL_get_wbio';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_get_wfd;                            N: 'SSL_get_wfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_is_server;                          N: 'SSL_is_server';                             MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.69 }
    (F: @@SSL_load_client_CA_file;                N: 'SSL_load_client_CA_file';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_new;                                N: 'SSL_new';                                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_read;                               N: 'SSL_read';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_renegotiate;                        N: 'SSL_renegotiate';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_renegotiate_pending;                N: 'SSL_renegotiate_pending';                   MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_select_next_proto;                  N: 'SSL_select_next_proto';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.56 }
    (F: @@SSL_session_reused;                     N: 'SSL_session_reused';                        MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_set0_security_ex_data;              N: 'SSL_set0_security_ex_data';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_set1_param;                         N: 'SSL_set1_param';                            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.39 }
    (F: @@SSL_set_SSL_CTX;                        N: 'SSL_set_SSL_CTX';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_accept_state;                   N: 'SSL_set_accept_state';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_alpn_protos;                    N: 'SSL_set_alpn_protos';                       MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.56 }
    (F: @@SSL_set_bio;                            N: 'SSL_set_bio';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_client_CA_list;                 N: 'SSL_set_client_CA_list';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_connect_state;                  N: 'SSL_set_connect_state';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_ex_data;                        N: 'SSL_set_ex_data';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_fd;                             N: 'SSL_set_fd';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_info_callback;                  N: 'SSL_set_info_callback';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_msg_callback;                   N: 'SSL_set_msg_callback';                      MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),   { V8.40 }
    (F: @@SSL_set_options;                        N: 'SSL_set_options';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.51 }
    (F: @@SSL_set_rfd;                            N: 'SSL_set_rfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_security_callback;              N: 'SSL_set_security_callback';                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_set_security_level;                 N: 'SSL_set_security_level';                    MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),  { V8.40 }
    (F: @@SSL_set_session;                        N: 'SSL_set_session';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_session_id_context;             N: 'SSL_set_session_id_context';                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_shutdown;                       N: 'SSL_set_shutdown';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_verify;                         N: 'SSL_set_verify';                            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_verify_result;                  N: 'SSL_set_verify_result';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_set_wfd;                            N: 'SSL_set_wfd';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_shutdown;                           N: 'SSL_shutdown';                              MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_state_string;                       N: 'SSL_state_string';                          MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),      { V8.40 }
    (F: @@SSL_state_string_long;                  N: 'SSL_state_string_long';                     MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_version;                            N: 'SSL_version';                               MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_want;                               N: 'SSL_want';                                  MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@SSL_write;                              N: 'SSL_write';                                 MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@TLS_client_method;                      N: 'TLS_client_method';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@TLS_method;                             N: 'TLS_method';                                MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@TLS_server_method;                      N: 'TLS_server_method';                         MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@d2i_SSL_SESSION;                        N: 'd2i_SSL_SESSION';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX),
    (F: @@i2d_SSL_SESSION;                        N: 'i2d_SSL_SESSION';                           MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) );

{$IFNDEF OPENSSL_NO_ENGINE}
    GSSLEAYImports2: array[0..0] of TOSSLImports = (
    (F: @@SSL_CTX_set_client_cert_engine;         N: 'SSL_CTX_set_client_cert_engine';            MI: OSSL_VER_MIN; MX: OSSL_VER_MAX) );
{$ENDIF}

{$ENDIF YuOpenSSL}

{$ENDIF} // USE_SSL

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF MSWINDOWS}

{ V8.38 Windows API to check authenticode code signing digital certificate on OpenSSL files }
procedure IcsVerifySslDll (const Fname: string);
var
    ErrCode: integer;
    TrustResp: String;
begin
    ErrCode := IcsVerifyTrust (FName, NOT GSSL_SignTest_Certificate, false, TrustResp);
    if (ErrCode = TRUST_E_SUBJECT_NOT_TRUSTED) or (ErrCode = TRUST_E_BAD_DIGEST) or
         (ErrCode = TRUST_E_NOSIGNATURE) or (ErrCode = TRUST_E_EXPLICIT_DISTRUST) then begin
         raise  EIcsSsleayException.Create('Failed to load ' + FName + ' - ' + TrustResp);
    end;
    if GSSL_SignTest_Certificate and ((ErrCode = CERT_E_CHAINING) or
        (ErrCode = CERT_E_UNTRUSTEDROOT) or (ErrCode = CERT_E_UNTRUSTEDTESTROOT)) then begin
         raise  EIcsSsleayException.Create('Failed to load ' + FName + ' - ' + TrustResp);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}

// import OpenSSL functions from DLLs, returns blank for OK, or list of missing exports

function SslGetImports (Handle: THandle; List: array of TOSSLImports): string ;  { V8.35 }
var
    I: integer ;
begin
    result := '';
    if (Length (List) = 0) then begin
        result := 'No import list specified' ;
    end;
    for I := 0 to Length(List) - 1 do begin
        if (ICS_OPENSSL_VERSION_NUMBER >= List[I].MI) and
               (ICS_OPENSSL_VERSION_NUMBER <= List[I].MX) then begin
            {$IFDEF POSIX}               { V8.65 }
            List[I].F^ := GetProcAddress (Handle, PChar(string(List[I].N)));
            {$ELSE}
            List[I].F^ := GetProcAddress (Handle, List[I].N);
            {$ENDIF}
            if List[I].F^ = nil then
                result := result + String(List[I].N) + ',' ;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SsleayLoad : Boolean;      {  V8.27 make unique }
{$IFNDEF YuOpenSSL}
var
    errs: String;   { V8.29 }
{$ENDIF YuOpenSSL}
begin
    Result := TRUE;

{$IFNDEF YuOpenSSL}
    if GSSLEAY_DLL_Handle <> 0 then Exit; // Already loaded

  { V8.27 sanity check }
    if ICS_OPENSSL_VERSION_NUMBER = 0 then begin
       raise EIcsSsleayException.Create('Must load libcrypto DLL before libssl DLL');  { V8.65 modern names }
    end;

 { V8.65 see if LibeayLoad so we report that error again and not this unit }
    if ICS_OPENSSL_VERSION_NUMBER = 999 then begin
       raise EIcsSsleayException.Create('Failed to load OpenSSL file ' + GLIBEAY_DLL_FileName);
    end;

{ V8.65 open SSL DLL in same directory as LIBEAY DLL }
    GSSLEAY_DLL_FileName := ExtractFilePath(GLIBEAY_DLL_FileName);
    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_3000 then
        GSSLEAY_DLL_FileName := GSSLEAY_DLL_FileName + GSSLEAY_300DLL_Name
    else
        GSSLEAY_DLL_FileName := GSSLEAY_DLL_FileName + GSSLEAY_110DLL_Name;
    GSSLEAY_DLL_Handle := LoadLibrary(PChar(GSSLEAY_DLL_FileName));
    if GSSLEAY_DLL_Handle < 32 then begin
        GSSLEAY_DLL_Handle := 0;
        FreeLibrary(GLIBEAY_DLL_Handle);     { V8.65 free other DLL now }
        GLIBEAY_DLL_Handle := 0;
        ICS_OPENSSL_VERSION_NUMBER := 0;
        raise EIcsSsleayException.Create('Failed to load OpenSSL file ' + GSSLEAY_DLL_FileName);
    end;

{ get full file name }
    SetLength(GSSLEAY_DLL_FileName, 256);
    SetLength(GSSLEAY_DLL_FileName, GetModuleFileName(GSSLEAY_DLL_Handle,
                 PChar(GSSLEAY_DLL_FileName), Length(GSSLEAY_DLL_FileName)));

  {$IFDEF MSWINDOWS}
    IcsGetFileVerInfo(GSSLEAY_DLL_FileName,      { V8.27 use full path }
                   GSSLEAY_DLL_FileVersion,
                   GSSLEAY_DLL_FileDescription);

   { V8.38 check authenticode digital signature on DLL }
    if GSSL_SignTest_Check then IcsVerifySslDll (GSSLEAY_DLL_FileName);
  {$ENDIF}

  { V8.35 load all main GSSLEAY_DLL exports }
    errs := SslGetImports (GSSLEAY_DLL_Handle, GSSLEAYImports1) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + GSSLEAY_DLL_FileName + '. Can not find: ' + errs);

{$IFNDEF OPENSSL_NO_ENGINE}
  { V8.35 load engine GSSLEAY_DLL exports }
    errs := SslGetImports (GSSLEAY_DLL_Handle, GSSLEAYImports2) ;
    if errs <> '' then
        raise  EIcsSsleayException.Create('Unable to load ' + GSSLEAY_DLL_FileName + '. Can not find: ' + errs);
{$ENDIF}
{$ENDIF YuOpenSSL}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer;
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_MODE, Mode, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer;
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_SIZE, CacheSize, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Cardinal;
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_EXTRA_CHAIN_CERT, 0, PAnsiChar(Cert))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_SSL_set_tlsext_host_name(const S: PSSL; const name: String): Integer;   { V8.66 renamed since not OpenSSL compat }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_HOSTNAME,
                          TLSEXT_NAMETYPE_host_name, Pointer(AnsiString(name)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: Pointer{TCallback_ctrl_fp}): Integer;
begin
    Result := SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, {$IFDEF YuOpenSSL}SSL_CTX_ctrl_callback(cb){$ELSE}cb{$ENDIF});   { V8.66 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): Integer;
begin
    Result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_set_tlsext_debug_callback(S: PSSL; cb: Pointer{TCallback_ctrl_fp}): Integer;
begin
    Result := SSL_callback_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_CB, {$IFDEF YuOpenSSL}SSL_ctrl_callback(cb){$ELSE}cb{$ENDIF});   { V8.66 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Integer;
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set_tmp_dh(C: PSSL_CTX; DH: Pointer) : Integer;   { V8.01 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_SET_TMP_DH, 0, DH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set_tmp_ecdh(C: PSSL_CTX; ECDH: Pointer) : Integer;    { V8.01 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_SET_TMP_ECDH, 0, ECDH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set_tmp_dh(S: PSSL; DH: Pointer) : Integer;     { V8.01 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TMP_DH, 0, DH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set_tmp_ecdh(S: PSSL; ECDH: Pointer) : Integer;    { V8.01 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TMP_ECDH, 0, ECDH);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set_min_proto_version(C: PSSL_CTX; version: integer) : Integer;  { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_SET_MIN_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set_max_proto_version(C: PSSL_CTX;  version: integer) : Integer;  { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_SET_MAX_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set_min_proto_version(S: PSSL; version: integer) : Integer;     { V8.27 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_MIN_PROTO_VERSION, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set_max_proto_version(S: PSSL; version: integer) : Integer;     { V8.27 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_MAX_PROTO_VERSION, version, Nil);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_get_min_proto_version(C: PSSL_CTX) : Integer;               { V8.51 added 1.1.1  }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_GET_MIN_PROTO_VERSION, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_get_max_proto_version(C: PSSL_CTX) : Integer;               { V8.51 added 1.1.1  }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_GET_MAX_PROTO_VERSION, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_get_min_proto_version(S: PSSL) : Integer;                       { V8.51 added 1.1.1  }
begin
    Result := SSL_ctrl(S, SSL_CTRL_GET_MIN_PROTO_VERSION, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_get_max_proto_version(S: PSSL) : Integer;                       { V8.51 added 1.1.1  }
begin
    Result := SSL_ctrl(S, SSL_CTRL_GET_MAX_PROTO_VERSION, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set0_chain(C: PSSL_CTX; sk: PSTACK_OF_X509): Integer;       { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_CHAIN, 0, PAnsiChar(sk));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_add0_chain_cert(C: PSSL_CTX; Cert: PX509): Integer;         { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_CHAIN_CERT, 0, PAnsiChar(Cert));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_get0_chain_certs(C: PSSL_CTX; sk: PPSTACK_OF_X509): Integer; { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_GET_CHAIN_CERTS, 0, PAnsiChar(Sk));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_clear_chain_certs(C: PSSL_CTX): Integer;                    { V8.27 }
begin
    Result := SSL_CTX_set0_chain (C, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_build_cert_chain(C: PSSL_CTX; flags: integer): Integer;     { V8.27 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_BUILD_CERT_CHAIN, flags, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ stub for old removed functions }
function IcsSslStub: integer;                            { V8.35 }
begin
    result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure  SSL_CTX_set_msg_callback_arg(Ctx: PSSL_CTX; arg: Pointer);    { V8.40 }
begin
    SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_MSG_CALLBACK_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure  SSL_set_msg_callback_arg(S: PSSL; arg: Pointer);           { V8.40 }
begin
    SSL_ctrl(S, SSL_CTRL_SET_MSG_CALLBACK_ARG, 0, arg);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_get1_groups(Ctx: PSSL_CTX; GList: Pointer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_GET_GROUPS, 0, GList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_get_shared_group(Ssl: PSSL; N: Integer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_ctrl(Ssl, SSL_CTRL_GET_SHARED_GROUP, N, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set1_groups(Ctx: PSSL_CTX; GList: Pointer; GListlen: Integer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_GROUPS, GListlen, GList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set1_groups_list(Ctx: PSSL_CTX; GList: Pointer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_GROUPS_LIST, 0, GList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set1_groups(Ctx: PSSL_CTX; GList: Pointer; GListlen: Integer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_GROUPS, GListlen, GList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set1_groups_list(Ctx: PSSL_CTX; GList: Pointer): Integer; { V8.51 OpenSSL 1.1.1 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_GROUPS_LIST, 0, GList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_set_mode(C: PSSL_CTX; version: integer) : Integer;     { V8.51 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_MODE, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_get_mode(C: PSSL_CTX) : Integer;                       { V8.51 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_MODE, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_CTX_clear_mode(C: PSSL_CTX;  version: integer) : Integer;  { V8.51 }
begin
    Result := SSL_CTX_ctrl(C, SSL_CTRL_CLEAR_MODE, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_set_mode(S: PSSL; version: integer) : Integer;             { V8.51 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_MODE, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_get_mode(S: PSSL) : Integer;                               { V8.51 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_MODE, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  SSL_clear_mode(S: PSSL; version: integer) : Integer;           { V8.51 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_CLEAR_MODE, version, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ V8.69 OCSP stapling, request OCSP during handshake and get it }
function SSL_get_tlsext_status_type(S: PSSL): Integer;    { V8.69 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_set_tlsext_status_type(S: PSSL; stype: integer): Integer;    { V8.69 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE, stype, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_get_tlsext_status_ocsp_resp(S: PSSL; arg: Pointer): Integer;     { V8.69 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_set_tlsext_status_ocsp_resp(S: PSSL; arg: Pointer; arglen: integer): Integer;     { V8.69 }
begin
    Result := SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP, arglen, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_get_tlsext_status_cb(ctx: PSSL_CTX; cb: Pointer): Integer;   { V8.69 }
begin
    Result := SSL_CTX_callback_ctrl(ctx, SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB, {$IFDEF YuOpenSSL}SSL_CTX_ctrl_callback(cb){$ELSE}cb{$ENDIF});
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_tlsext_status_cb(ctx: PSSL_CTX; cb: Pointer): Integer; { V8.69 }
begin
    Result := SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB, {$IFDEF YuOpenSSL}SSL_CTX_ctrl_callback(cb){$ELSE}cb{$ENDIF});
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_get_tlsext_status_arg(ctx: PSSL_CTX; arg: Pointer): Integer;   { V8.69 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_tlsext_status_arg(ctx: PSSL_CTX; arg: Pointer): Integer;   { V8.69 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_get_tlsext_status_type(ctx: PSSL_CTX): Integer;  { V8.69 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE, 0, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SSL_CTX_set_tlsext_status_type(ctx: PSSL_CTX; stype: integer): Integer;   { V8.69 }
begin
    Result := SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE, stype, Nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}//USE_SSL

end.







