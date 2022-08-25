{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  HTTPS REST functions, descends from THttpCli, and publishes all
              it's properties and events with additional methods and properties
              for making REST (REpresentional State Transfer) client requests.
              The TSslHttpRest component is a high level version of THttpCli
              that bundles all the extra components for extra functionality,
              including SSL configuration and certificate validation with a
              root bundle, SSL session caching, content compression, content
              code page decoding, persistent cookies, Json handling, logging,
              client SSL certificate.
              Includes functions for OAuth2 and OAuth1A authentication, and
              components to send SMS messages and Tweets.
Creation:     Apr 2018
Updated:      May 2022
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

TSslHttpRest
------------
This descends from THttpCli, and publishes all it's properties and events with
additional methods and properties for making REST (REpresentional State Transfer)
client requests.  It bundles many optional HTTP units, including SslContext,
content compression, cookie support, SSL/TLS certificate chain checks, content
code page decoding to widestring, OCSP checking, etc, logging, Json or XML
parameters and responses, usually simplifying HTTP support to a few lines and
one or two event handlers.

Note TSslHttpRest is designed for standalone use making single HTTP requests,
it can not currently share an SslContext or cookies which makes it less
efficient for sharing multiple requests in parallel with multiple THttpCli
components.


TIcsInetAlive
-------------
Component to check for IPv4 and/or IPv6 internet connectivity, using Ping
and/or HTTP.


TDnsQueryHttps
--------------
Component to support DOH, DNS over HTTPS lookups similarly to TDnsQuery.



TIcsSMS
-------
Send SMS using a bureau, you will need an account. Supports https://www.kapow.co.uk/
from where you set-up an account for £6.50 (about $9) which gives 100 message
credits and the SMS Works at https://thesmsworks.co.uk/ where you set-up an
account with a few free SMS messages, then spend a mininum of £10 which buys
350 message credits. SMS Works allows you to set your own sender number or name
free of charge.


TOcspHttp
---------
Component to support OCSP (Online Certificate Status Protocol) that replaces
CRL (Certificate Revocation Lists) as the way to confirm SSL/TLS certificates
are legitimate and not revoked for security reasons.  The component will check
an OCSP stapled response sent by a server during an SSL/TLS handshake or use
HTTP to contact the certificate issuer's OCSP server directly to get OCSP status.
OCSP responses are cached and optionally saved to a file for reloading later or
sharing with other applications, they generally have a seven day refresh life,
but ideally need to be checked more often. TOcspHttp can also download the issuer
or intermediate certificate if missing.

OCSP support is built into to the components TSslHttpRest, TIcsIpStrmLog,
TIcsMailQueue, TSslFtpClient, TSslHttpServer, TSslFtpServer, and
TSslWSocketServer.  The OverbyteIcsHttpsTst sample illustrates how it can
be used with other components like THttpCli in the onSslHandshakeDone event.


Note as of V8.69, the TRestOAuth, TSimpleWebSrv, TIcsTwitter and TIcsRestEmail
components were moved to a new unit OverbyteIcsSslHttpOAuth.


Updates:
May 21, 2018  - V8.54 - baseline
Jul  2, 2018  - V8.55 - Improved Json error handling
                        Builds with NO_DEBUG_LOG
Oct 2, 2018   - V8.57 - Need OAuth local web server for all auth methods.
                        Builds with FMX
Nov 2, 2018   - V8.58 - Bug fixes, call RequestDone event if it fails
                        Descend components from TIcsWndControl not TComponent
Feb 6, 2019   - V8.60 - Default with SocketFamily Any for both IPv4 and IPv6.
                        SessionConnect logging shows IP address as well as host name.
                        Increased OAuth web server timeout from 2 to 30 mins.
Apr 26, 2019  - V8.61 - Prevent TSslHttpCli events being overwritten by TSslHttpRest events.
                        ResponseXX properties available in OnRequestDone and OnRestRequestDone.
                        Return javascript content as well as XML and Json
                        Posted content-type header now specifies UTF-8.
                        Added new TDnsQueryHttps component to make DNS over HTTPS
                           queries using wire format.
                        Added new TIcsSms component to send SMS text messages via
                           an HTTP bureau, you will need an account. Initially
                           supporting https://www.kapow.co.uk/ from where you set-up
                           an account for £6.50 (about $9) which gives 100 message
                           credits. Other similar bureaus can be added, provided
                           there is an account for testing.
Aug 07, 2019  - V8.62 - Add AsyncReq to TIcsSms methods for flexibility.
                        Supporting The SMS Works at https://thesmsworks.co.uk/ for SMS.
                        Simple web server breaks down full URL for proxy requests.
                        TRestParams can add Json parameters as PContJson which
                          means arrays and nested Json can be added.
                        TSimpleWebSrv now supports SSL, with certificate bunder
                          and host name, supports SSL ALPN extension.
                        Added SslAllowSelfSign property to connect OK to sites
                          with self signed SSL certificates.
                        Builds without USE_SSL
Nov 11, 2019  - V8.63 - The SMS Works sync delivery works OK, try and return
                          similar delivery responses as Kapow.
                        Ensure default CA bundle gets loaded if SslRootFile
                          blank (broken in V8.62).
                        Web server now lonnger adds date/time when logging allowing
                          application to do it instead.
                        OAuth2 don't kill old refresh token if no refresh is available,
                           Google APIs provides a single refresh that remains valid for
                           weeks rather than a new one with each access token. Clarified
                           the OAuth documentation to explain the Google process.
                        OAuth has extra TOAuthOptions OAopAuthPrompt and OAopAuthAccess
                           for Google, OAopAuthPrompt uses property LoginPrompt usually
                           'consent', OAopAuthAccess and RefreshOffline=True requests a
                           Refresh Token.
May 05, 2020  - V8.64 - Added support for International Domain Names for Applications (IDNA),
                         i.e. using accents and unicode characters in domain names.
                        Only  REST change here is to report A-Label domain looked up by DNS.
                        SimpleWebSrv returns host: name in Unicode.
                        Added more parameter content types: PContXML, PContBodyUrlEn,
                          PContBodyJson, PContBodyXML. The existing PContUrlEn and
                          PContJson now specify REST params are sent as URL ? arguments,
                          while the PContBodyxx version send params as content body.
                        This fixes a bug that meant PUT request params were always sent
                          as URL ? arguments.  Note POST is always content body so
                          the wrong PContent is corrected automatically for backward
                          compatibility.
                        XML content type is experimental, not tested.
                        Verifying the SSL certificate chain with the Windows Store
                          works again.
                        TDnsQueryHttps component now uses strings and support IDNs.
                        TSimpleWebSrv no longer processes ALPN, done in SocketServer.
                        Made TSimpleWebSrv.WebServer writable to set properties.
Dec 15, 2020 - V8.65 - Added OAuth1A for Twitter, note uses extra secret tokens that
                          OAuth2 does not need.  Each request made using OAuth1A requires
                          a unique signature sent as an Authorization: OAuth header
                          unlike OAuth2 that uses the same bearer token for all requests.
                        Added new TIcsTwitter component to send and receive tweets,
                          requires a developer account from Twitter.  Includes login
                          to Twitter, send tweet, search tweets and get specific
                          tweets, all responses are Json which the application needs
                          to untangle.
                        Don't skip OAuth2 auto refresh if using old refresh token.
                        Using new Superobject ParseStringEx function that logs parsing
                          errors for ResponseJson if it returns nil.
                        For DebugHeaders, report TCP buffer size.
                        Don't add ? to URL if no parameters.
                        Don't try and use Windows certificate store on Posix, or
                           ShellExecute to run a program, pending replacement.
                         Initial Posox support.
                        Changing IcsSms AccountJson now gets a new JWT. Cleaned
                           up SMS error reporting for bad authentication.
                        Added several TOAuthUri records designed to set-up common OAuth2
                          account settings for providers like Google, Twitter, Microsoft
                          and Sipgate, by using the LoadAuthUri method.  Note avoid
                          dragging all URIs into all applications, they need to be
                          referenced specifically in applications.  The REST sample
                          builds an array to allow them to be selected from a list.
                        TRestOAuth has new AccName and ConsoleUrl properties for display.
                        TSimpleWebSrv has new WebSrvIP2 for a second address, setting
                          WebSrvIP to localhost sets 127.0.0.1 and [::1].
                        Added OAuth2 Console URL and button which simply launches
                          the browser to the account console page where an
                          application is set-up and you get the secrets.
                        Added TOAuthOption OAopAuthGrantedScope which include existing
                          granted scopes when adding new ones.
                        If SSL handshake fails due to bad certificate or chain, remove
                          SSL session from cache so an immediate retry does not succeed by
                          skipping the certificate checks.
                        TRestParams AddItem has new overloaded methods to add Integer,
                          Double and Boolean values, AddItemDT for TDateTime, AddItemSO
                          for ISuperObject and AddItemAR for TStrings as Json array.
                           Numerics and bool are saved as Json without quotes which is
                           required for strict Json parsers. Another AddItem overload
                           has a RParamType parameter as TRParamType allowing a string
                           to be added as RPTypeStr, RPTypeInt, RPTypeDate, RPTypeFloat,
                           RPTypeBool, RPTypeObj or RPTypeArray.
                        TRestParams has new RfcStrict method that forces RFC3986 strict
                          URL percent encoding, four unreserved chars (- . _ -) are
                          not percent encoded.
                        TRestParams has new RemoveItem method to delete a parameter.
                        TRestParams has new PContent type of PContCommaList which saves
                           parameters as a name="value", comma separated list for Twitter.
                        Added new TIcsRestEmail component that provides basic support
                          for Google and Microsoft Outlook email REST APIs including
                          OAuth2 login and refresh to get an access token for SMTP
                          smtpAuthXOAuth2 and smtpAuthOAuthBearer authentication.
                        TIcsRestEmail has methods to send and read email, to list IDs in
                          a mailbox, read headers and message bodies by ID, send emails
                          and delete emails.  The OverbyteIcsHttpRestTst1 sample shows
                          how to read all headers, and how to prepare email using the
                          SMTP component (Google) or Json (Microsoft) to be sent using
                          the REST API.
                        TSimpleWebSrv.StopSrv has new argument that does not close
                          current clients so it can be called from a client event
                          to stop new requests.
                        Don't attempt to send a blank POST body.
                        Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Mar 19, 2021 - V8.66 -  TIcsRestEmail has new OAAuthType property and event for OAuth2
                           browser URL perhaps via email for servers.
                        TIcsRestEmail sets OAuth2 errors, and clears old tokens.
                        Increased default timeout for TIcsRestEmail AOuth2 login
                           to 120 seconds since several web page warnings may be seen.
                        Added TIcsInetAlive component to check for IPv4 and/or IPv6
                           internet connectivity, using Ping and/or HTTP, defaulting to
                           www.msftconnecttest.com run by Microsoft for Windows 10 alive
                           checking, online and offline check intervals may be set,
                           event when online changes.
                        Report if OpenSSL statically linked.
                        Ensure SSL initialised for non-REST requests like GET/PUT.
                        Made IcsEscapeJson public.
Sep 07, 2021 - V8.67 -  Moved TRestParams to OverbyteIcsUrl to ease circular referencing
                          and reduce size of this unit.
                        TIcsInetAlive better error for ping hops exceeded, and change
                           default from 16 to 25.
                        Replaced Stream.Seek with Stream.Position.
Dec 20, 2021 - V8.68 -  Improved request failed error reporting by adding RequestDoneErrorStr.
                        Previously this component always downloaded content to a TMemoryStream
                          with content size being limited to MaxBodySize (default 100 MByte),
                          and generally restricted by memory to less than 250 MByte.
                        Added HttpMemStrategy property with THttpMemStrategy on how to handle
                          downloads: HttpStratMem only TMemoryStream; HttpStratTemp uses a work
                          file in the system temporary directory for sizes larger than
                          MaxBodySize; HttpStratFile always writes a named file HttpDownFileName
                          (with .part extension during download); HttpStratResume is similar to
                          HttpStratFile but supports resume of failed partial downloads (with
                          .http extension for resume information). Property ResumeMinSize defines
                          the minimum sized partial file that should be resumed, rather than start
                          again (default 64K).
                        Note MaxBodySize remains the maximum size for ResponseRaw (unicode
                          string), JSON and XML parsing.
                        BREAKING CHANGE, ResponseStream property is now TStream so use of
                          SaveToFile method will need to be cast as TMemoryStream.  Or change
                          to use HttpStratFile method with HttpDownFileName, see above.
                        NOTE, ResponseStream remains open after request completes and may
                          occupy a lot of memory or leave file open (read only), also ResponseRaw
                          string, so use ClearResp method when no longer needed.
                        Log better error if response does not contain Json.
                        Added ShowProgress property that causes download information to be sent
                          to the OnHttpRestProg event using LogOption=loProgress, showing progress
                          in KBytes or MBytes updated every ProgIntSecs seconds, default two,
                          expected to be displayed as a caption.
May 20, 2022 - V8.69 -  Builds on MacOS again, added IOUtils for TPath.
                        Split TRestOAuth, TSimpleWebSrv, TIcsTwitter and TIcsRestEmail components
                          to a new unit OverbyteIcsSslHttpOAuth, to ease maintenance and use.
                        Added new TOcspHttp component to support OCSP (Online Certificate Status
                          Protocol) that replaces CRL (Certificate Revocation Lists) as the way to
                          confirm SSL/TLS certificates are legitimate and not revoked for security
                          reasons.  The component will check an OCSP stapled response sent by a server
                          during an SSL/TLS handshake or use HTTP to contact the certificate issuer's
                          OCSP server directly to get OCSP status.  OCSP responses are cached and
                          optionally saved to a file for reloading later or sharing with other
                          applications, they generally have a seven day refresh life, but ideally
                          need to be checked more often. TOcspHttp can also download the issuer or
                          intermediate certificate if missing.
                        The TOcspHttp component properties are exposed in TSslHttpRest, TIcsIpStrmLog,
                          TIcsMailQueue, TSslFtpClient TIcsMailQueue, TSslFtpClient, TSslHttpServer,
                          TSslFtpServer, and TSslWSocketServer. but are only effective if SslRevocation
                          is set with verify method CertVerBundle. CacheFName is the optional file name
                          to save the cache, OcspCache.recs in the samples which is flushed to disk
                          after CacheFlushMins, including stapled responses if CacheStapled=True.
                          OcspStapleOnly=True means only used stapled responses and don't make HTTP
                          requests, OcspMaxDays is expiry of a status response, CacheRefrDays is how
                          often to refresh responses since certificates may be revoked with only
                          a couple of days notice in serious cases (Let's Encrypt on 28 Jan 2022).
                          OcspHttpProxy allows a proxy URL to be specified.  There are two main
                          responses from the OCSP server, OcspRespStatus is whether the OCSP returned
                          information about the certificate and OcspCertStatus is that result.
                          TSslHttpRest.TransferSslHandshakeDone shows how TOcspHttp is used to check
                          the handshake certificates, just a few lines of code.
                        Note revocation checking is soft fail due to uncertainties in the internet,
                          the OCSP server may be offline or not accessible via firewalls or proxies.
                          During an SSL/TLS handshake, if TOcspHttp needs to make an HTTP request this
                          is done asynchronously to avoid delaying the handshake with the response
                          cached ready for the next request. If checking certificates off-line (for
                          servers for instance) the component will synchronously check OCSP waiting
                          for a response (if a non-zero timeout is specified).
                        Previously the TSslHttpRest SslRevocation property was only effective when
                          checking the windows certificate store, now it also works with bundle files
                          using the TOcspHttp component and OCSP stapling if available.
                        Upon completion, ReasonPhrase now also has status, so OK becomes 200 OK, etc.
                        Allow POST/PUT a file specified in HttpUploadFile using HttpUploadStrat of
                          HttpUploadSimple with parameters in the URL or HttpUploadMIME for multipart
                          with parameters in the first MIME part.



Pending - more documentation
Pending - better SSL error handling when connections fail, due to too high security in particular.
Pending - REST response for DelphiXE Json Objects Framework

}

{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslHttpRest;
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

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.ShellAPI{$ELSE}ShellAPI{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    System.IOUtils,           { V8.69 }
    Ics.Posix.WinTypes,
    Ics.Posix.PXMessages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Sysutils{$ELSE}Sysutils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
    OverbyteIcsSsleay, OverbyteIcsLibeay,
    OverbyteIcsWinsock,
    OverbyteIcsTypes,
    OverbyteIcsUtils,
    OverbyteIcsUrl,
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
    Ics.Fmx.OverbyteIcsHttpProt,
    Ics.Fmx.OverbyteIcsSslSessionCache,
    Ics.Fmx.OverbyteIcsSslX509Utils,
    Ics.Fmx.OverbyteIcsMsSslUtils,
    Ics.Fmx.OverbyteIcsSslJose,
    Ics.Fmx.OverbyteIcsDnsQuery,
    Ics.Fmx.OverbyteIcsPing,
    Ics.Fmx.OverbyteIcsIcmp,
{$ELSE}
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
    OverbyteIcsHttpProt,
    OverbyteIcsSslSessionCache,
    OverbyteIcsSslX509Utils,
    OverbyteIcsMsSslUtils,
    OverbyteIcsDnsQuery,
    OverbyteIcsPing,
    OverbyteIcsIcmp,
{$ENDIF FMX}
{$IFDEF MSWINDOWS}
    OverbyteIcsWinCrypt,
{$ENDIF MSWINDOWS}
    OverbyteIcsHttpCCodZLib,
    OverbyteIcsHttpContCod,
    OverbyteIcsLogger,         { for TLogOption }
    OverbyteIcsCookies,
    OverbyteIcsMimeUtils,
    OverbyteIcsFormDataDecoder,
    OverbyteIcsCharsetUtils,
    OverbyteIcsSuperObject,
    OverbyteIcsTicks64,
    OverbyteIcsStreams;   { V8.68 }

{ NOTE - these components only build with SSL, there is no non-SSL option }

const
    THttpRestVersion = 869;
    CopyRight : String = ' TSslHttpRest (c) 2022 F. Piette V8.69 ';
    DefMaxBodySize = 100*IcsMBYTE; { max memory/string size 100Mbyte }  { V8.67 literal safer }
 //   TestState = 'Testing-Redirect';
    MimeDnsJson       = 'application/dns-json';
    MimeDnsMess       = 'application/dns-message';
    MimeAppCert       = 'application/pkix-cert';             { V8.69 }
    MimeOcspRequest   = 'application/ocsp-request';          { V8.69 }
    MimeMultipart     = 'multipart/form-data; boundary=';    { V8.69 }
    MimeAppBinary     = 'application/binary';                { V8.69 }
    MimeAppForm       = 'application/x-www-form-urlencoded; charset=UTF-8';  { V8.69 }
    MimeAppXml        = 'application/xml; charset=UTF-8';    { V8.69 }
    MimeAppJson       = 'application/json; charset=UTF-8';   { V8.69 }

    OAuthErrBase                     = {$IFDEF MSWINDOWS} 1 {$ELSE} 1061 {$ENDIF};
    OAuthErrNoError                  = 0;
    OAuthErrParams                   = OAuthErrBase;
    OAuthErrBadGrant                 = OAuthErrBase+1;
    OAuthErrWebSrv                   = OAuthErrBase+2;
    OAuthErrBrowser                  = OAuthErrBase+3;
    OAuthErrEvent                    = OAuthErrBase+4;     { V8.65 }

    ResInfURL = 0; ResInfDateTime = 1; ResInfEtag = 2;
    ResInfContSize = 3;  ResInfAttempts = 4; ResInfLastBegin = 5;    { V8.68 resume download file info  }

type
{ event handlers }
  THttpRestProgEvent = procedure (Sender: TObject; LogOption: TLogOption; const Msg: string) of object;
  THttpMemStrategy = (HttpStratMem, HttpStratTemp, HttpStratFile, HttpStratResume);   { V8.68 }
  THttpUploadStrat = (HttpUploadNone, HttpUploadSimple, HttpUploadMIME);              { V8.69 }

type

{ V8.69 TOcspHttp supports Online Certificate Status Protocol to check revocation }
{ optionally caches OCSP responses received using HTTP or stapled }
    TOcspCache = record
        CertCName: String;
        CertStatus: Integer;
        RespDT: TDateTime;
        CacheDT: TDateTime;
        ReqLen: Integer;
        ReqRaw: AnsiString;
        RespLen: Integer;
        RespRaw: AnsiString;
    end;

TOcspHttp = Class(TIcsWndControl)
  private
    { Private declarations }
    FPostStream: TMemoryStream;
    FResponseStream: TMemoryStream;
    FCacheFlushTimer: TIcsTimer;
    FDebugLevel: THttpDebugLevel;
    FOnOcspProg: THttpRestProgEvent;
    FCertCName: String;
    FOcspCertIDRaw: AnsiString;
    FOcspReqRaw: AnsiString;
    FOcspRespRaw: AnsiString;
    FOcspRespDT: TDateTime;
    FOcspCacheDT: TDateTime;
    FOcspCache: array of TOcspCache;
    FCacheTot: Integer;
    FCacheFName: String;
    FCacheChanged: Boolean;
    FCacheStapled: Boolean;
    FCacheFileShared: Boolean;
    FCacheFileDT: TDateTime;
    FOcspStapleOnly: Boolean;
    FOcspMaxDays: Integer;
    FCacheRefrDays: Integer;
    FCacheFlushMins: Integer;
    FOcspLastResp: String;
    FOcspHttpProxy: String;
  protected
    { Protected declarations }
    procedure onHttpCommand (Sender: TObject; var S: String) ;
    procedure onHttpHeaderData (Sender : TObject);
    procedure onHttpSessionConnected (Sender : TObject);
    procedure onHttpSessionClosed(Sender: TObject);
    procedure onHttpORequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
    procedure onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure doOcspLog(LogOption: TLogOption; const Msg: string);
    procedure CacheFlushOnTimer(Sender: TObject);
    procedure ClearCacheItem(Item: Integer);
    procedure DelReqFromCache;
    procedure NewCacheRecs(Tot: Integer);
    function  FindReqInCache: Integer;
    procedure SaveRespToCache;
    function  FindRespInCache: Boolean;
    function  GetCacheAsStr(Item: Integer): String;
    function  GetRespStatus: Integer;
    function  GetCertStatus: Integer;
    function  GetCertReason: Integer;
    procedure ProcResponse;
  public
    { Public declarations }
    HttpCliO: THttpCli;
    HttpCliI: THttpCli;
    OcspCert: TX509Base;
    OcspInters: TX509List;    // we use OpenSSL OCSP functions in this component
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    procedure    ClearOcsp;
    procedure    ClearCache;
    function     ReadCacheFromFile: Boolean;
    function     SaveCacheToFile(CloseDown: Boolean = True): Boolean;
    function     OcspHttpRequest(const URL: String; Timeout: Integer = 0) : Boolean;    { timeout is seconds, zero none }
    function     IssuerHttpRequest(const URL: String; Timeout: Integer = 10): Boolean;
    function     CheckOcspRevoked(X509Store: PX509_STORE; Timeout: Integer = 0): Boolean;
    property     OcspReqRaw: AnsiString             read  FOcspReqRaw
                                                    write FOcspReqRaw;
    property     OcspRespRaw: AnsiString            read  FOcspRespRaw
                                                    write FOcspRespRaw;
    property     OcspRespDT: TDateTime              read  FOcspRespDT;
    property     OcspRespStatus: Integer            read  GetRespStatus;
    property     OcspCertStatus: Integer            read  GetCertStatus;
    property     OcspCertReason: Integer            read  GetCertReason;
    property     CacheTot: Integer                  read  FCacheTot;
    property     OcspLastResp: String               read  FOcspLastResp;
  published
    { Published declarations }
    property     OcspStapleOnly: Boolean            read  FOcspStapleOnly
                                                    write FOcspStapleOnly;
    property     CacheFName: String                 read  FCacheFName
                                                    write FCacheFName;
    property     CacheFileShared: Boolean           read  FCacheFileShared
                                                    write FCacheFileShared;
    property     OcspMaxDays: Integer               read  FOcspMaxDays
                                                    write FOcspMaxDays;
    property     CacheRefrDays: Integer             read  FCacheRefrDays
                                                    write FCacheRefrDays;
    property     CacheFlushMins: Integer            read  FCacheFlushMins
                                                    write FCacheFlushMins;
    property     CacheStapled: Boolean              read  FCacheStapled
                                                    write FCacheStapled;
    property     OcspHttpProxy: String              read  FOcspHttpProxy
                                                    write FOcspHttpProxy;
    property     DebugLevel: THttpDebugLevel        read  FDebugLevel
                                                    write FDebugLevel;
    property     OnOcspProg: THttpRestProgEvent     read  FOnOcspProg
                                                    write FOnOcspProg;
  end;

{ TSslHttpRest descends from THttpCli, and publishes all it's properties
   and events with additional methods and properties for making REST
   (REpresentional State Transfer) client requests. }
  TSslHttpRest = class(TSslHttpCli)
  private
    { Private declarations }
    FRestParams: TRestParams;
    FDebugLevel: THttpDebugLevel;
    FPostStream: TStream;               { V8.69 was TMemoryStream }
    FResponseJson: ISuperObject;
    FResponseStream: TStream;           { V8.68 was TMemoryStream }
    FResponseRaw: UnicodeString;
    FResponseSize: Integer;
    FMaxBodySize: Int64;
    FInitSsl: Boolean;
    FRespReq: Boolean;
    FSslSessCache: boolean;
    FExternalSslSessionCache: TSslAvlSessionCache;
    FCertVerMethod: TCertVerMethod;
    FSslRootFile: String;
    FSslRevocation: boolean;
    FSslReportChain: boolean;
    FSslAllowSelfSign: boolean;  { V8.62 }
    FHttpMemStrategy: THttpMemStrategy;   { V8.68 }
    FHttpDownFileName: String;            { V8.68 }
    FHttpDownReplace: Boolean;            { V8.68 }
    FResumeMinSize: Int64;                { V8.68 }
    FProgIntSecs: Integer ;               { V8.68 }
    FShowProgress: Boolean ;              { V8.68 }
    FTempPath: String;                    { V8.68 }
    FTempFileName: String;                { V8.68 }
    FResumeFileName: String;              { V8.68 }
    FResInfRecs: TStringList;             { V8.68 }
    FExpectedSize: Int64;                 { V8.68 }
    FProgMessBase: String;                { V8.68 }
    FProgLastTick: Longword;              { V8.68 }
    FOcspHttp: TOcspHttp;                 { V8.69 }
    FHttpUploadStrat: THttpUploadStrat;   { V8.69 }
    FHttpUploadFile: String;              { V8.69 }
    FMimeTypesList: TMimeTypesList;       { V8.69 }
    FSslCliCert: TX509Base;
    FSslCliSecurity:  TSslCliSecurity;
{$IFDEF MSWINDOWS}
    FMsCertChainEngine: TMsCertChainEngine;
{$ENDIF}
    FOnHttpRestProg: THttpRestProgEvent;
    FOnRestRequestDone: THttpRequestDone;
    FOnRestLocChange: TNotifyEvent;
  protected
    { Protected declarations }

    procedure LogEvent(const Msg : String);
    procedure SetRestParams(Value: TRestParams);
    procedure SetSslCliCert(Value: TX509Base);
    procedure SetSslCliSecurity(Value: TSslCliSecurity);
    function  GetResponseJson: ISuperObject;
    function  GetResponseOctet: AnsiString;
    procedure IcsLogEvent (Sender: TObject; LogOption: TLogOption; const Msg : String);
    procedure onCookiesNewCookie(Sender : TObject; ACookie : TCookie; var Save : Boolean);
    procedure TriggerCommand(var S: String); override;  { V8.61 }
    procedure TriggerHeaderData; override;              { V8.61 }
    procedure TriggerLocationChange; override;          { V8.61 }
    procedure TriggerRequestDone2; override;            { V8.61 }
    procedure TriggerDocBegin; override;                { V8.61 }
    procedure TriggerDocData(Data : Pointer; Len : Integer); override;                { V8.68 }
    procedure TriggerCookie(const Data : String; var   bAccept : Boolean); override;  { V8.61 }
    procedure TriggerSessionConnected; override;       { V8.61 }
    procedure TriggerSessionClosed; override;          { V8.61 }
    procedure TransferSslVerifyPeer(Sender        : TObject;
                                    var Ok        : Integer;
                                    Cert           : TX509Base); override;  { V8.61 }
    procedure TransferSslCliGetSession(Sender      : TObject;
                                   var SslSession  : Pointer;
                                  var FreeSession  : Boolean); override;    { V8.61 }
    procedure TransferSslCliNewSession(Sender      : TObject;
                                      SslSession   : Pointer;
                                      WasReused    : Boolean;
                                  var IncRefCount  : Boolean); override;    { V8.61 }
    procedure TransferSslCliCertRequest(Sender     : TObject;
                                        var Cert   : TX509Base); override;  { V8.61 }
    procedure TransferSslHandshakeDone(Sender      : TObject;
                                       ErrCode    : Word;
                                       PeerCert   : TX509Base;
                                   var Disconnect : Boolean); override;     { V8.61 }
    procedure DoRequestAsync(Rq : THttpRequest); override;                  { V8.66 }
  public
    { Public declarations }
    RestCookies: TIcsCookies;
{$IFNDEF NO_DEBUG_LOG}
    RestLogger:  TIcsLogger;
{$ENDIF}
    RestSslCtx:  TSslContext;
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    procedure    InitSsl;
    procedure    ResetSsl;
    procedure    ClearResp;
    function     GetParams: AnsiString;   { V8.64 lost reqtype }
    function     RestRequest(HttpRequest: THttpRequest; const RestURL: String;
                    AsyncReq: Boolean = False; const RawParams: String = ''): Integer;

  published
    { Published declarations }
    property RestParams: TRestParams                    read  FRestParams
                                                        write SetRestParams;
    property DebugLevel:THttpDebugLevel                 read  FDebugLevel
                                                        write FDebugLevel;
    property ResponseRaw: UnicodeString                 read  FResponseRaw;
    property ResponseJson: ISuperObject                 read  GetResponseJson;
    property ResponseOctet: AnsiString                  read  GetResponseOctet;
    property ResponseStream: TStream                    read  FResponseStream;     { V8.68 was TMemoryStream }
    property ResponseSize: Integer                      read  FResponseSize;
    property MaxBodySize: Int64                         read  FMaxBodySize
                                                        write FMaxBodySize;
    property SslCliSecurity: TSslCliSecurity            read  FSslCliSecurity
                                                        write SetSslCliSecurity;
    property SslCliCert: TX509Base                      read  FSslCliCert
                                                        write SetSslCliCert;
    property SslSessCache: boolean                      read  FSslSessCache
                                                        write FSslSessCache;
    property CertVerMethod: TCertVerMethod              read  FCertVerMethod
                                                        write FCertVerMethod;
    property SslRootFile: string                        read  FSslRootFile
                                                        write FSslRootFile;
    property SslRevocation: boolean                     read  FSslRevocation
                                                        write FSslRevocation;
    property SslReportChain: boolean                    read  FSslReportChain
                                                        write FSslReportChain;
    property SslAllowSelfSign: boolean                  read  FSslAllowSelfSign
                                                        write FSslAllowSelfSign; { V8.62 }
    property HttpMemStrategy: THttpMemStrategy          read  FHttpMemStrategy
                                                        write FHttpMemStrategy;  { V8.68 }
    property HttpDownFileName: string                   read  FHttpDownFileName
                                                        write FHttpDownFileName; { V8.68 }
    property HttpDownReplace: Boolean                   read  FHttpDownReplace
                                                        write FHttpDownReplace;  { V8.68 }
    property ResumeMinSize: Int64                       read  FResumeMinSize
                                                        write FResumeMinSize;    { V8.68 }
    property ProgIntSecs: Integer                       read  FProgIntSecs
                                                        write FProgIntSecs;      { V8.68 }
    property ShowProgress: Boolean                      read  FShowProgress
                                                        write FShowProgress;     { V8.68 }
    property OcspHttp: TOcspHttp                        read  FOcspHttp
                                                        write FOcspHttp;         { V8.69 }
    property HttpUploadStrat: THttpUploadStrat          read  FHttpUploadStrat
                                                        write FHttpUploadStrat;  { V8.69 }
    property HttpUploadFile: String                     read  FHttpUploadFile
                                                        write FHttpUploadFile;    { V8.69 }
    property OnBgException;
    property OnHttpRestProg: THttpRestProgEvent         read  FOnHttpRestProg
                                                        write FOnHttpRestProg;
    property OnRestRequestDone: THttpRequestDone        read  FOnRestRequestDone
                                                        write FOnRestRequestDone;
    property OnRestLocChange: TNotifyEvent              read  FOnRestLocChange
                                                        write FOnRestLocChange;
  end;


  { V8.61 TDnsQueryHttps supports DOH - DNS over HTTPS }
  TDnsQueryHttps = Class(TDnsQuery)
  private
    { Private declarations }
    FDebugLevel: THttpDebugLevel;
    FDnsSrvUrl: string;
    FOnDnsProg: THttpRestProgEvent;
  protected
    { Protected declarations }
    procedure DnsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure DnsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
  public
    { Public declarations }
    HttpRest:  TSslHttpRest;
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    function     DOHQueryAll(Host: String): Boolean;          { V8.64 }
    function     DOHQueryAny(Host: String; QNumber: Integer;  { V8.64 }
                                    MultiRequests: Boolean = False) : Boolean;
  published
    { Published declarations }
    property DnsSrvUrl: string                      read  FDnsSrvUrl
                                                    write FDnsSrvUrl;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property OnDnsProg: THttpRestProgEvent          read  FOnDnsProg
                                                    write FOnDnsProg;
  end;

  { V8.61 Send SMS using bureau, you will need an account.
    Initially supporting https://www.kapow.co.uk/ from where you set-up an
    account for £6.50 (about $9) which gives 100 message credits.
    Other similar SMS can be added, provided there is an account for testing. }

  { V8.62 Added The SMS Works at https://thesmsworks.co.uk/  where you set-up an
    account with a few free SMS messages, then spend a mininum of £10 which
    buys 350 message credits.  }


  TSmsProvider = (SmsProvKapow, SmsProvSmsWorks); // more providers awaited
  TSmsOperation = (SmsOpSend, SmsOpCheck, SmsOpCredit);

  TIcsSMS = class(TIcsWndControl)
  private
    { Private declarations }
    FDebugLevel: THttpDebugLevel;
    FSmsProvider: TSmsProvider;
    FSmsOperation: TSmsOperation;
    FAccountName: string;
    FAccountPW: string;
    FAccountJson: string;
    FAccountJwt: string;
    FMsgSender: string;
    FSendDT: TDateTime;
    FSentID: string;
    FCredits: string;
    FLastResp: string;
    FLastError: string;
    FDelivery: String;
    FOnSmsProg: THttpRestProgEvent;
    FOnSmsDone: TNotifyEvent;
  protected
    { Protected declarations }
    procedure SmsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure SmsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
    function  MakeRequest(HttpRequest: THttpRequest; const RestURL: String;
                      AsyncReq: Boolean = False; const RawParams: String = ''): Boolean;
    procedure SetAccountJson(Value: String);   { V8.65 }
  public
    { Public declarations }
    HttpRest:  TSslHttpRest;
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    function     SendSMS(const MobileNums, SmsMsg: String; AsyncReq: Boolean = True): Boolean;
    function     CheckSMS(ID: String; AsyncReq: Boolean = True; Batch: Boolean = False): Boolean;
    function     CheckCredit(AsyncReq: Boolean = True): Boolean;
    property     SentID: string                     read  FSentID;
    property     Credits: string                    read  FCredits;
    property     LastResp: string                   read  FLastResp;
    property     LastError: string                  read  FLastError;
    property     Delivery: string                    read  FDelivery;
  published
    { Published declarations }
    property SmsProvider: TSmsProvider              read  FSmsProvider
                                                    write FSmsProvider;
    property AccountName: string                    read  FAccountName
                                                    write FAccountName;
    property AccountPW: string                      read  FAccountPW
                                                    write FAccountPW;
    property AccountJson: string                    read  FAccountJson
                                                    write SetAccountJson;
    property MsgSender: string                      read  FMsgSender
                                                    write FMsgSender;
    property SendDT: TDateTime                      read  FSendDT
                                                    write FSendDT;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property OnSmsProg: THttpRestProgEvent          read  FOnSmsProg
                                                    write FOnSmsProg;
    property OnSmsDone: TNotifyEvent                read  FOnSmsDone
                                                    write FOnSmsDone;
  end;


{ V8.66 TIcsInetAlive tests for an internet connection in various ways }
  TAliveMethod = (AliveMethPing, AliveMethHttp, AliveMethBoth);
  TAliveNetwork = (AliveNetv4, AliveNetv6, AliveNetBoth);
  TAliveStatus = (AliveStatNone, AliveStatOffline, AliveStatOnline);
const
  AliveStatusLits: array[TAliveStatus] of string = ('Not Tested','Offline','Online');
type
  TIcsInetAlive = class(TIcsWndControl)
  private
    { Private declarations }
    FTaskTimer: TIcsTimer;
    FDebugLevel: THttpDebugLevel;
    FAliveMethod: TAliveMethod;
    FAliveNets: TAliveNetwork;
    FHostIPv4: String;
    FHostIPv6: String;
    FLocalIPv4: String;
    FLocalIPv6: String;
    FHttpPage: String;
    FHttpText: String;
    FHttpProxy: String;
    FRunning: Boolean;
    FAliveIPv4: TAliveStatus;
    FAliveIPv6: TAliveStatus;
    FAutoStart: Boolean;
    FOnlineSecs: Integer;
    FOfflineSecs: Integer;
    FPingHops: Integer;
    FPingMaxSecs: Integer;
    FPingCheckAddr: Boolean;
    FFailedWaitSecs: Integer;
    FTrgIPv4: Int64;
    FTrgIPv6: Int64;
    FLastTickIPv4: Int64;
    FLastTickIPv6: Int64;
    FLastDTIPv4: TDateTime;
    FLastDTIPv6: TDateTime;
    FHttpBusy: Boolean;
    FStartTick: Int64;
    FHttpRTT: Integer;
    FPingBusy: Boolean;
    FOnAliveChange: TNotifyEvent;
    FOnAliveProg: THttpRestProgEvent;
  protected
    { Protected declarations }
    procedure AliveRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure AliveRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
    procedure TaskOnTimer(Sender: TObject);
    procedure PingTerminated(Sender: TObject);
    procedure SetOnline(Online: Boolean; IpType: Integer);
    procedure StartHttp(IpType: Integer);
    procedure StartCheck(IpType: Integer);
  public
    { Public declarations }
    HttpRest:  TSslHttpRest;
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    procedure    Start;
    procedure    Stop;
    procedure    CheckNow;
    function     TestBothOnline: TAliveStatus;
    property     Running: Boolean                   read  FRunning;
    property     AliveIPv4: TAliveStatus            read  FAliveIPv4;
    property     AliveIPv6: TAliveStatus            read  FAliveIPv6;
    property     LastTickIPv4: Int64                read  FLastTickIPv4;
    property     LastTickIPv6: Int64                read  FLastTickIPv6;
    property     LastDTIPv4: TDateTime              read  FLastDTIPv4;
    property     LastDTIPv6: TDateTime              read  FLastDTIPv6;
  published
    { Published declarations }
    property AliveMethod: TAliveMethod              read  FAliveMethod
                                                    write FAliveMethod;
    property AliveNets: TAliveNetwork               read  FAliveNets
                                                    write FAliveNets;
    property HostIPv4: string                       read  FHostIPv4
                                                    write FHostIPv4;
    property HostIPv6: string                       read  FHostIPv6
                                                    write FHostIPv6;
    property HttpPage: string                       read  FHttpPage
                                                    write FHttpPage;
    property HttpText: string                       read  FHttpText
                                                    write FHttpText;
    property HttpProxy: string                      read  FHttpProxy
                                                    write FHttpProxy;
    property LocalIPv4: string                      read  FLocalIPv4
                                                    write FLocalIPv4;
    property LocalIPv6: string                      read  FLocalIPv6
                                                    write FLocalIPv6;
    property AutoStart: Boolean                     read  FAutoStart
                                                    write FAutoStart;
    property OnlineSecs: Integer                    read  FOnlineSecs
                                                    write FOnlineSecs;
    property OfflineSecs: Integer                   read  FOfflineSecs
                                                    write FOfflineSecs;
    property PingHops: Integer                      read  FPingHops
                                                    write FPingHops;
    property PingMaxSecs: Integer                   read  FPingMaxSecs
                                                    write FPingMaxSecs;
    property PingCheckAddr: Boolean                 read  FPingCheckAddr
                                                    write FPingCheckAddr;
    property FailedWaitSecs: Integer                read  FFailedWaitSecs
                                                    write FFailedWaitSecs;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property OnAliveChange: TNotifyEvent            read  FOnAliveChange
                                                    write FOnAliveChange;
    property OnAliveProg: THttpRestProgEvent        read  FOnAliveProg
                                                    write FOnAliveProg;
  end;


{ Retrieve a single value by name out of an URL encoded data stream.        }
function IcsExtractURLEncodedValue(
    Msg         : PChar;            { URL Encoded stream                    }
    Name        : String;           { Variable name to look for             }
    var Value   : String;           { Where to put variable value           }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Boolean; overload;

function IcsExtractURLEncodedValue(
    const Msg   : String;           { URL Encoded stream                     }
    Name        : String;           { Variable name to look for              }
    var Value   : String;           { Where to put variable value            }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Boolean; overload;


{$ENDIF USE_SSL}

implementation

{$IFDEF LINUX}
uses FMUX.Api;                { V8.65 for FmuxOpenUrl }
{$ENDIF}

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ borrowed from OverbyteIcsHttpSrv and renamed to avoid conflicts }
{ Retrieve a single value by name out of an URL encoded data stream         }
{ In the stream, every space is replaced by a '+'. The '%' character is     }
{ an escape character. The next two are 2 digits hexadecimal codes ascii    }
{ code value. The stream is constitued by name=value couples separated      }
{ by a single '&' character. The special characters are coded by the '%'    }
{ followed by hex-ascii character code.                                     }
function IcsExtractURLEncodedValue(
    Msg         : PChar;    { URL Encoded stream                     }
    Name        : String;   { Variable name to look for              }
    var Value   : String;   { Where to put variable value            }
    SrcCodePage : LongWord; { D2006 and older CP_UTF8 only           }
    DetectUtf8  : Boolean)
    : Boolean;              { Found or not found that's the question }
var
    NameLen  : Integer;
    FoundLen : Integer; {tps}
    Ch       : AnsiChar;
    P, Q     : PChar;
    U8Str    : AnsiString;
begin
    Result  := FALSE;
    Value   := '';
    if Msg = nil then         { Empty source }
        Exit;

    NameLen := Length(Name);
    U8Str := '';
    P := Msg;
    while P^ <> #0 do begin
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        FoundLen := P - Q; {tps}
        if P^ = '=' then
            Inc(P);
        if (StrLIComp(Q, @Name[1], NameLen) = 0) and
           (NameLen = FoundLen) then begin  {tps}
            while (P^ <> #0) and (P^ <> '&') do begin
                Ch := AnsiChar(Ord(P^)); // should contain nothing but < ord 128
                if Ch = '%' then begin
                    if P[1] <> #0 then    // V1.35 Added test
                        Ch := AnsiChar(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                U8Str := U8Str + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
         end;
         while (P^ <> #0) and (P^ <> '&') do
             Inc(P);
        if P^ = '&' then
            Inc(P);
    end;
    if (SrcCodePage = CP_UTF8) or (DetectUtf8 and IsUtf8Valid(U8Str)) then
{$IFDEF COMPILER12_UP}
        Value := Utf8ToStringW(U8Str)
    else
        Value := AnsiToUnicode(U8Str, SrcCodePage);
{$ELSE}
        Value := Utf8ToStringA(U8Str)
    else
        Value := U8Str;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsExtractURLEncodedValue(
    const Msg   : String;           { URL Encoded stream                    }
    Name        : String;           { Variable name to look for             }
    var Value   : String;           { Where to put variable value           }
    SrcCodePage : LongWord = CP_ACP;{ D2006 and older CP_UTF8 only          }
    DetectUtf8  : Boolean  = TRUE)
    : Boolean; overload;
begin
    Result := IcsExtractURLEncodedValue(PChar(Msg), Name, Value,
                                     SrcCodePage, DetectUtf8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TOcspHttp }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TOcspHttp.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    FPostStream := TMemoryStream.Create;
    FResponseStream := TMemoryStream.Create;
    HttpCliO := THttpCli.Create(self);
    HttpCliO.OnCommand := onHttpCommand;
    HttpCliO.OnHeaderData := onHttpHeaderData;
    HttpCliO.OnRequestDone := onHttpORequestDone;
    HttpCliO.onSessionConnected := onHttpSessionConnected;
    HttpCliO.onSessionClosed := onHttpSessionClosed;
    HttpCliO.onBgException := onHttpBgException;
    HttpCliO.CtrlSocket.ComponentOptions := [wsoNoReceiveLoop];
    HttpCliO.SocketFamily := sfAny;
    HttpCliI := THttpCli.Create(self);
    HttpCliI.OnCommand := onHttpCommand;
    HttpCliI.OnHeaderData := onHttpHeaderData;
    HttpCliI.onSessionConnected := onHttpSessionConnected;
    HttpCliI.onSessionClosed := onHttpSessionClosed;
    HttpCliI.onBgException := onHttpBgException;
    HttpCliI.CtrlSocket.ComponentOptions := [wsoNoReceiveLoop];
    HttpCliI.SocketFamily := sfAny;
    FCacheFlushTimer := TIcsTimer.Create(HttpCliO);
    FCacheFlushTimer.Enabled := False;
    FCacheFlushTimer.OnTimer := CacheFlushOnTimer;
    FDebugLevel := DebugNone;
    ClearCache;
    ClearOcsp;
    FOcspMaxDays := 7;
    FCacheRefrDays := 3;
    FCacheFlushMins := 2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TOcspHttp.Destroy;
begin
    FCacheFlushTimer.Enabled := False;
    if (FCacheFName <> '') and FCacheChanged and (FCacheTot > 0) then
        SaveCacheToFile(True);
    ClearCache;
    FCacheFlushTimer.Free;
    FPostStream.Free;
    FResponseStream.Free;
    HttpCliO.Free;
    HttpCliI.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearOcsp;
begin
    OcspCert := Nil;
    OcspInters := Nil;
    FOcspCertIDRaw := '';
    FOcspReqRaw := '';
    FOcspRespRaw := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearCache;
begin
    SetLength(FOcspCache, 0);
    FCacheTot := 0;
    FCacheFileDT := -1 ;
    FCacheChanged := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.doOcspLog(LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnOcspProg) then
        FOnOcspProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
    doOcspLog(loProtSpecInfo, (Sender as THttpCli).URL + ' Fatal Exception: ' + IcsGetExceptMess(E));
    CanClose := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpSessionConnected (Sender : TObject);
var
    S: String;
    Cli: THttpCli;
begin
    if FDebugLevel < DebugConn then Exit;
    Cli := Sender as THttpCli;
    if Cli.State = httpConnected then
        S := '= Connected OK to: '
    else
        S := '= Connection failed to: ';
    S := S + Cli.Hostname + ' (' + IcsFmtIpv6Addr(Cli.AddrResolvedStr) + ')';
    doOcspLog(loProtSpecInfo, S);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpSessionClosed(Sender: TObject);
var
    Cli: THttpCli;
    S: String;
begin
    if FDebugLevel < DebugConn then Exit;
    Cli := Sender as THttpCli;
    S := '= ' + Cli.URL + ' Session Closed';
    if Cli.RequestDoneError <> httperrNoError then
        S := S + ', Error: ' + Cli.RequestDoneErrorStr;
    S := S + ' - ' + Cli.ReasonPhrase;
    doOcspLog(loProtSpecInfo, S);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpCommand (Sender: TObject; var S: String) ;
begin
    if FDebugLevel < DebugHdr then Exit;
    doOcspLog(loProtSpecInfo, '> ' + S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpHeaderData (Sender : TObject);
begin
    if FDebugLevel < DebugHdr then Exit;
    doOcspLog(loProtSpecInfo, '< ' + (Sender as THttpCli).LastResponse);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get interemediate certificate for issuer of our certificate }
function TOcspHttp.IssuerHttpRequest(const URL: String; Timeout: Integer): Boolean;
var
    StatCode: Integer;
    RawCert: AnsiString;
    Errs: String;
    MyCert: TX509Base;
begin
    Result := False;
    OcspInters.Clear;
    if (URL = '') then Exit;

    try
        if HttpCliI.State <> httpReady then begin
            doOcspLog(loProtSpecInfo, 'Last Issuer Request Still Busy');
            Exit;
        end;
        HttpCliI.URL := URL;
        HttpCliI.ProxyURL := FOcspHttpProxy;
        HttpCliI.Connection := 'Close';   // no keep-alive
        HttpCliI.Timeout := Timeout;
        HttpCliI.Accept := MimeAppCert;
        FResponseStream.Clear;
        HttpCliI.RcvdStream := FResponseStream;
        HttpCliI.ResponseNoException := True;
        HttpCliI.Get;   // blocking sync request
        StatCode := HttpCliI.StatusCode;  // only for sync requests
        if (StatCode = 200) and (Pos(MimeAppCert, HttpCliI.ContentType) = 1) then begin
            FResponseStream.Position := 0;
            SetLength(RawCert, FResponseStream.Size);
            FResponseStream.ReadBuffer(RawCert[1], FResponseStream.Size);
         // if PEM format may load more than one, possibly
            if (IcsAnsiPosEx(PEM_STRING_HDR_BEGIN, RawCert) > 0) then
                OcspInters.LoadAllFromString(String(RawCert))
            else begin
                MyCert := TX509Base.Create(Self, Nil);
                MyCert.ReadFromAStr(RawCert, croNo, croNo, '', Errs);
                if MyCert.IsCertLoaded then
                    OcspInters.Add(MyCert.X509);
                MyCert.Free;
            end;
            if OcspInters.Count = 0 then begin
                if Errs = '' then Errs := 'Content size: ' + IntToStr(Length(RawCert));
                doOcspLog(loProtSpecInfo, 'Failed to Load Issuer Certificate from ' + URL + ' - ' + Errs);
            end
            else begin
                doOcspLog(loProtSpecInfo, 'Downloaded Issuer Certificate OK: ' + OcspInters[0].SubjectOneLine);
                Result := True;
            end;
        end
        else begin
            HttpCliI.Abort;
            doOcspLog(loProtSpecInfo, 'Download Issuer Certificate Request to ' + URL + ', failed - ' + HttpCliI.LastResponse);
        end;
        HttpCliI.CloseAsync;
    except
        on E:Exception do begin    { 400/500 no longer come here }
            doOcspLog(loProtSpecInfo, 'Download Issuer Certificate to ' + URL + ', failed: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.OcspHttpRequest(const URL: String; Timeout: Integer) : Boolean;
begin
    Result := False;
    FOcspRespRaw := '';
    FOcspRespDT := 0;
    if (URL = '') or (Length(FOcspReqRaw) < 20) then Exit;

    try
        if HttpCliO.State <> httpReady then begin
            doOcspLog(loProtSpecInfo, 'Last OCSP Request Still Busy');
            Exit;
        end;
        HttpCliO.URL := URL;
        HttpCliO.ProxyURL := FOcspHttpProxy;
        HttpCliO.Connection := 'Close';   // no keep-alive
        HttpCliO.Timeout := Timeout;
        HttpCliO.ContentTypePost := MimeOcspRequest;
        FResponseStream.Clear;
        HttpCliO.RcvdStream := FResponseStream;
        FPostStream.Clear;
        FPostStream.Write(FOcspReqRaw[1], Length(FOcspReqRaw));
        FPostStream.Position := 0;
        HttpCliO.SendStream := FPostStream;
        HttpCliO.ResponseNoException := True;
        if TimeOut = 0 then
            HttpCliO.PostASync   // non blocking async request
            // returns immediately,
        else
            HttpCliO.Post;       // blocking sync request
       // process response in onHttpORequestDone
       Result := True;
    except
        on E:Exception do begin    { 400/500 no longer come here }
            doOcspLog(loProtSpecInfo, 'OCSP Request to ' + URL + ', failed: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetRespStatus: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspRespStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCertStatus: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspCertStatus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCertReason: Integer;
begin
    if NOT Assigned(OcspInters) then
        Result := -1
    else
        Result := OcspInters.OcspCertReason;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ProcResponse;
var
    RespInfo: String;
begin
    FOcspLastResp := 'No OCSP Status Available';
    if FOcspRespRaw = '' then Exit;
    if NOT Assigned(OcspInters) then Exit;
    if GetCertStatus >= V_OCSP_CERTSTATUS_GOOD then begin
        FOcspRespDT := OcspInters.OcspUpdateDT;
        doOcspLog(loProtSpecInfo, 'OCSP Status Response: ' + OcspInters.OcspRespStatusStr);

        if (FDebugLevel >= DebugSsl) then begin
            RespInfo := OcspInters.OcspCertStatusStr;
            if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                RespInfo := RespInfo + OcspInters.OcspCertReasonStr + ', on ' + DateTimeToStr(OcspInters.OcspRevokeDT);
            RespInfo := RespInfo + ', last updated '  + DateTimeToStr(OcspInters.OcspUpdateDT) +
                                              ', next update ' + DateTimeToStr(OcspInters.OcspNextUpdDT);
            doOcspLog(loProtSpecInfo, 'OCSP Certificate status: ' + RespInfo);
            doOcspLog(loProtSpecInfo, OcspInters.OCSPRespInfo(FOcspRespRaw) + IcsCRLF);
        end;

        if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then begin
           FOcspLastResp := 'Certificate revoked: ' + OcspInters.OcspCertReasonStr;
        end
        else begin
           if GetCertStatus = V_OCSP_CERTSTATUS_GOOD then
                FOcspLastResp := 'Certificate OCSP Status Valid, Not Revoked'
           else if GetCertStatus > V_OCSP_CERTSTATUS_GOOD then
                FOcspLastResp := 'Certificate OCSP Status Check Failed: ' + OcspInters.OcspCertStatusStr;
        end;
    end
    else begin
        FOcspLastResp := 'OCSP Status Failed: ' + OcspInters.OcspCertStatusStr;
        doOcspLog(loProtSpecInfo, FOcspLastResp);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.onHttpORequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
    Cli: THttpCli;
    StatCode: Integer;
begin
    if RqType <> httpPOST then Exit;
    Cli := Sender as THttpCli;
    StatCode := Cli.StatusCode;
    if ErrCode <> 0 then begin
        doOcspLog(loProtSpecInfo, 'OCSP Request failed: ' + Cli.RequestDoneErrorStr + ' - ' + Cli.ReasonPhrase);
    end
    else begin
        if StatCode = 200 then begin
            FOcspRespDT := Cli.RespLastModDT;
            FResponseStream.Position := 0;
            SetLength(FOcspRespRaw, FResponseStream.Size);
            FResponseStream.ReadBuffer(FOcspRespRaw[1], FResponseStream.Size);
            doOcspLog(loProtSpecInfo, 'OCSP Response OK from ' + Cli.URL + ', Size ' + IntToStr(FResponseStream.Size));
           SaveRespToCache;

         { note we don't verify response here since the issuer and root may not be available any longer for async
           operations and the handshake will already have been completed, it's checked next time from cache }
        end
        else begin
            doOcspLog(loProtSpecInfo, 'OCSP Request to ' + Cli.URL + ', failed - ' + Cli.ReasonPhrase);
        end;
    end;
    Cli.CloseAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.CheckOcspRevoked(X509Store: PX509_STORE; Timeout: Integer = 0): Boolean;
var
    I: Integer;
begin
    Result := False;
    FOcspLastResp := 'Checking OCSP Status';
    FOcspReqRaw := '';
    FOcspCertIDRaw := '';
    if NOT Assigned(OcspCert) then Exit;
    if NOT OcspCert.IsCertLoaded then Exit;
    if NOT Assigned(OcspInters) then Exit;
    if OcspInters.Count = 0 then Exit;

    try
    // look for intermediate in local bundle server sent us
        FCertCName := OcspCert.SubjectCName;
        I := OcspInters.IndexOfSubj(OcspCert.IssuerOneLine);

    // try and download intermediate - should rarely happen here
    // if it was missing, verify probably failed so we should repeat that somehow!!!
        if I < 0 then begin
            doOcspLog(loProtSpecInfo, 'Can not find Intermediate: ' + OcspCert.IssuerOneLine);    // TEMP
            doOcspLog(loProtSpecInfo, 'Intermediate bundle Tot=' + IntToStr(OcspInters.Count));   // TEMP

            doOcspLog(loProtSpecInfo, 'Downloading Missing Intermediate from: ' + OcspCert.UrlIssuer);
            IssuerHttpRequest(OcspCert.UrlIssuer, 10);
            I := OcspInters.IndexOfSubj(OcspCert.IssuerOneLine);
        end;
        if I < 0 then
            FOcspLastResp := 'Could Not Find Intermediate Certificate for OCSP'
        else begin
            FOcspCertIDRaw := OcspInters.BuildOCSPCertID(OcspCert);
            if FOcspCertIDRaw = '' then
                FOcspLastResp := 'Failed to Build OCSP Certificate ID';
        end;

     // got intermediate, test OCSP against stapled response sent during handshake, maybe
        if FOcspCertIDRaw <> '' then begin
            if (Length(FOcspRespRaw) > 50) then begin
                doOcspLog(loProtSpecInfo, 'Using OCSP Stapled Response from TLS Handshake');
                if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                    if (GetCertStatus <> V_OCSP_CERTSTATUS_GOOD) then
                        doOcspLog(loProtSpecInfo, 'OCSP Stapled Response Failed: ' + OcspInters.OcspCertStatusStr);
                    ProcResponse;
                    if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                        Result := True;
                    if FCacheStapled then begin
                        FOcspReqRaw := OcspInters.BuildOCSPReq(FOcspCertIDRaw);
                        SaveRespToCache;
                    end;
                end;
            end;

         // no staple response, see if cached response
            if GetCertStatus < V_OCSP_CERTSTATUS_GOOD then begin
                FOcspReqRaw := OcspInters.BuildOCSPReq(FOcspCertIDRaw);
                if (FOcspReqRaw = '') then
                    FOcspLastResp := 'Failed to Build OCSP Request'
                else begin
                    if FindRespInCache then begin
                        doOcspLog(loProtSpecInfo, 'Found Cached OCSP Status');
                        if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                            ProcResponse;
                            if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                                Result := True;
                        end;
                    end;

               // nothing cached or need to refresh it, make HTTP request to OCSP server and cache response for next time
               // if timeout none zero, block and wait for response, don't do this during SSL handshake or things break
                    if (GetCertStatus < V_OCSP_CERTSTATUS_GOOD) or ((Now - FOcspCacheDT) > FCacheRefrDays) then begin
                        if FOcspStapleOnly then
                           FOcspLastResp := 'OCSP Status Not Available'
                        else if Assigned(OcspCert) then begin
                            if OcspCert.UrlOcsp = '' then
                               FOcspLastResp := 'No OCSP Server URL Available'
                            else begin
                                FOcspLastResp := 'Awaiting OCSP Status';
                                doOcspLog(loProtSpecInfo, 'Getting OCSP Status from: ' + OcspCert.UrlOcsp);
                                if FDebugLevel >= DebugSsl then
                                    doOcspLog(loProtSpecInfo, OcspInters.OCSPReqInfo(FOcspReqRaw) + IcsCRLF);
                                FOcspRespRaw := '';
                                if NOT OcspHttpRequest(OcspCert.UrlOcsp, Timeout) then begin
                                    FOcspLastResp := 'OCSP Request Failed to: ' + OcspCert.UrlOcsp;
                                end
                                else if (Timeout <> 0) then begin  // sync request check response
                                    if (OcspInters.CheckOCSPResp(FOcspCertIDRaw, FOcspRespRaw, X509Store) >= 0) then begin
                                        ProcResponse;
                                        if GetCertStatus = V_OCSP_CERTSTATUS_REVOKED then
                                            Result := True;
                                    end;
                                end;
                            end;
                        end
                        else
                            FOcspLastResp := 'OCSP Certificate Lost!';
                    end;
                end;
            end;
        end;
    except
        FOcspLastResp := 'OCSP Checking Failed, Exception';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.FindReqInCache: Integer;
var
    ReqLen: Integer;
begin
    Result := -1;
    if FCacheTot = 0 then Exit;
    ReqLen := Length(FOcspReqRaw);
    if (ReqLen = 0) then Exit;
    for Result := 0 to FCacheTot - 1 do begin
        if (ReqLen = FOcspCache[Result].ReqLen) and
              (OcspReqRaw = FOcspCache[Result].ReqRaw) then Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.ClearCacheItem(Item: Integer);
begin
    if Item < 0 then Exit;
    if Item >= Length(FOcspCache) then Exit;
    FOcspCache[Item].ReqRaw := '';
    FOcspCache[Item].ReqLen := 0;
    FOcspCache[Item].RespRaw := '';
    FOcspCache[Item].RespLen := 0;
    FOcspCache[Item].RespDT := 0;
    FOcspCache[Item].CertCName := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.DelReqFromCache;
begin
    ClearCacheItem(FindReqInCache);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.NewCacheRecs(Tot: Integer);
var
    J: Integer;
begin
    SetLength(FOcspCache, FCacheTot + Tot);
    for J := FCacheTot to Length(FOcspCache) - 1 do
        ClearCacheItem(J);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.SaveRespToCache;
var
    I, J: Integer;
begin
    if (FOcspReqRaw = '') or (FOcspRespRaw = '') then Exit;
    I := FindReqInCache;    // see if replacing old entry
    if I < 0 then begin
        if FCacheTot > 0 then begin  // look for empty entries
            for J := 0 to FCacheTot - 1 do begin
                if Length(FOcspCache[J].ReqRaw) = 0 then begin
                    I := J;
                    Break;
                end;
            end;
        end;
        if I < 0 then begin  // new entry in cache, may need to increase size
            I := FCacheTot;
            FCacheTot := FCacheTot + 1;
            if FCacheTot >= Length(FOcspCache) then
                NewCacheRecs(32);
        end;
    end;
    if FOcspRespDT = 0 then FOcspRespDT := Now;
    FOcspCache[I].CertCName := FCertCName;
    FOcspCache[I].CertStatus := GetCertStatus;
    FOcspCache[I].RespDT := FOcspRespDT;
    FOcspCache[I].CacheDT := Now;
    FOcspCache[I].ReqLen := Length(FOcspReqRaw);
    FOcspCache[I].ReqRaw := FOcspReqRaw;
    FOcspCache[I].RespLen := Length(FOcspRespRaw);
    FOcspCache[I].RespRaw := FOcspRespRaw;
    FCacheChanged := True;
    if NOT FCacheFlushTimer.Enabled then begin
        if FCacheFlushMins < 1 then
            FCacheFlushMins := 1;
        FCacheFlushTimer.Interval := LongWord(FCacheFlushMins) * TicksPerMinute;
        FCacheFlushTimer.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.FindRespInCache: Boolean;
var
    I: Integer;
begin
    Result := False;
    FOcspRespRaw := '';
    if (FCacheTot = 0) and (FCacheFileDT >= 0) then  // skip if file not found already
        ReadCacheFromFile;   // initial load from file
    I := FindReqInCache;
    if I < 0 then begin
        if NOT ReadCacheFromFile then Exit;  // reload from file if newer
        I := FindReqInCache;
        if I < 0 then Exit;
    end;

  // has entry expired, clear it, unless revoked
    if FOcspMaxDays < 1 then FOcspMaxDays := 3;
    if ((Now - FOcspCache[I].RespDT) > FOcspMaxDays) and
            (FOcspCache[I].CertStatus <> V_OCSP_CERTSTATUS_REVOKED) then begin
        ClearCacheItem(I);
        Exit;
    end;

 // found cached response, ignore cert status
    FCertCName := FOcspCache[I].CertCName;
    FOcspCacheDT := FOcspCache[I].CacheDT;
    FOcspRespDT :=  FOcspCache[I].RespDT;
    FOcspRespRaw := FOcspCache[I].RespRaw;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.GetCacheAsStr(Item: Integer): String;
begin
    Result := '';
    if Item < 0 then Exit;
    if Item >= Length(FOcspCache) then Exit;
    if Length(FOcspCache[Item].ReqRaw) = 0 then Exit;
    Result := FOcspCache[Item].CertCName + ',' +
        IntToStr(FOcspCache[Item].CertStatus) + ',' +
        RFC3339_DateToStr(FOcspCache[Item].CacheDT) + ',' +
        RFC3339_DateToStr(FOcspCache[Item].RespDT) + ',' +
        IntToStr(Length(FOcspCache[Item].ReqRaw)) + ',' +
        String(Base64Encode(FOcspCache[Item].ReqRaw)) + ',' +
        IntToStr(Length(FOcspCache[Item].RespRaw)) + ',' +
        String(Base64Encode(FOcspCache[Item].RespRaw));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.SaveCacheToFile(CloseDown: Boolean = True): Boolean;
var
    I, Tot: Integer;
    FileUDT: TDateTime;
    StreamWriter: TIcsStreamWriter;
begin
    Result := False;
    try
        FCacheFlushTimer.Enabled := False;
        if FCacheFName = '' then Exit;
        if NOT FCacheChanged then Exit;
        FCacheChanged := False;
        FileUDT := IcsGetFileUAge(FCacheFName);

     // pending implement FCacheFileShared by only adding new records to file

        if FileUDT > 0 then
            IcsDeleteFile (FCacheFName, true) ;
        if FCacheTot = 0 then Exit;
        Tot := 0;
        StreamWriter := TIcsStreamWriter.Create(FCacheFName, fmCreate, 0, MAX_BUFSIZE);
        StreamWriter.LineBreakStyle := ilbsCRLF;
        for I:= 0 to FCacheTot - 1 do begin
            if Length(FOcspCache[I].ReqRaw) > 0 then begin
                StreamWriter.WriteLine(GetCacheAsStr(I));
                Tot := Tot + 1;
            end;
        end;
        StreamWriter.WriteLine('end');
        StreamWriter.Flush;
        StreamWriter.Destroy;
        FCacheFileDT := IcsGetFileUAge(FCacheFName);
        if NOT CloseDown then  // logging during close down can cause trouble
            doOcspLog(loProtSpecInfo, 'Saved OCSP Cache File OK: ' + FCacheFName + ' - ' + IntToStr(Tot) + ' records');
        Result := True;
    except
        on E:Exception do begin
            if NOT CloseDown then
                doOcspLog(loProtSpecInfo, 'Failed to Save OCSP Cache File: ' + FCacheFName + ' - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOcspHttp.ReadCacheFromFile: Boolean;
var
    J, RawLen: Integer;
    FileUDT: TDateTime;
    StreamReader: TIcsStreamReader;
    RecCols: TStringList ;
    Line: String;
    RawField: AnsiString;
begin
    Result := False;
    FCacheFlushTimer.Enabled := False;
    if FCacheFName = '' then Exit;
    FileUDT := IcsGetFileUAge(FCacheFName);
    if FileUDT <= 0 then begin
        FCacheFileDT := FileUDT;
        doOcspLog(loProtSpecInfo, 'Can Not Find OCSP Cache File: ' + FCacheFName);
        Exit;
    end;

 // unchanged since last loaded or saved, give up
    if (FCacheFileDT = FileUDT) and (FCacheTot > 0) then begin
        Result := True;
        Exit;
    end;
    FCacheFileDT := FileUDT;

 // pending implement FCacheFileShared by updating existing records from file

    FCacheTot := 0;  // clear all cache records
    if Length(FOcspCache) > 0 then begin
        for J := 0 to Length(FOcspCache) - 1 do
           ClearCacheItem(J);
    end;
    RecCols := TStringList.Create ;
    try
        try
            StreamReader := TIcsStreamReader.Create(FCacheFName, fmShareDenyWrite, 0, MAX_BUFSIZE);
            StreamReader.MaxLineLength := 8000;
            StreamReader.Position := 0;
            while StreamReader.ReadLine(Line) do begin
               RecCols.CommaText := Line;
                if RecCols.Count >= 8 then begin
                    RawLen := atoi(RecCols[4]);
                    RawField := Base64Decode(AnsiString(RecCols[5]));
                    if (RawLen = Length(RawField)) and (RawLen > 20) then begin
                        if ((FCacheTot + 1) >= Length(FOcspCache)) then
                            NewCacheRecs(32);
                        FOcspCache[FCacheTot].CertCName := RecCols[0];
                        FOcspCache[FCacheTot].CertStatus := atoi(RecCols[1]);
                        FOcspCache[FCacheTot].CacheDT := RFC3339_StrToDate(RecCols[2]);
                        FOcspCache[FCacheTot].RespDT := RFC3339_StrToDate(RecCols[3]);
                        FOcspCache[FCacheTot].ReqLen := RawLen;
                        FOcspCache[FCacheTot].ReqRaw := RawField;
                        FOcspCache[FCacheTot].RespLen := atoi(RecCols[6]);
                        FOcspCache[FCacheTot].RespRaw := Base64Decode(AnsiString(RecCols[7]));
                        if (FOcspCache[FCacheTot].RespLen <> Length(FOcspCache[FCacheTot].RespRaw)) or
                          (((Now - FOcspCache[FCacheTot].RespDT) > FOcspMaxDays) and
                              (FOcspCache[FCacheTot].CertStatus <> V_OCSP_CERTSTATUS_REVOKED)) then begin
                            ClearCacheItem(FCacheTot);
                        end
                        else
                            FCacheTot := FCacheTot + 1;
                    end;
                end;
            end;
            StreamReader.Destroy;
            doOcspLog(loProtSpecInfo, 'Read OCSP Cache File OK: ' + FCacheFName + ' - ' + IntToStr(FCacheTot) + ' records');
            FCacheChanged := False;
            Result := True;
        except
            on E:Exception do begin
                doOcspLog(loProtSpecInfo, 'Failed to Read OCSP Cache File: ' + FCacheFName + ' - ' + E.Message);
            end;
        end;
    finally
        RecCols.Free;
    end;     
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOcspHttp.CacheFlushOnTimer(Sender: TObject);
begin
    FCacheFlushTimer.Enabled := False;
    SaveCacheToFile(False);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslHttpRest }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslHttpRest.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
    FRequestVer := '1.1';
    FRestParams := TRestParams.Create(self);
    FPostStream := TMemoryStream.Create;
    FResponseStream := TMemoryStream.Create;
    FMaxBodySize := DefMaxBodySize;
 // winsock bug fix for fast connections
    CtrlSocket.ComponentOptions := [wsoNoReceiveLoop];
    SocketFamily := sfAny;         { V8.60 allow IPv6 or IPv4 }
    Options := Options + [httpoEnableContentCoding];
    FSslSessCache := true;
    FExternalSslSessionCache := nil;
    RestCookies := TIcsCookies.Create(self);
    RestCookies.OnNewCookie := onCookiesNewCookie;
{$IFNDEF NO_DEBUG_LOG}
    RestLogger := TIcsLogger.Create (nil);
    RestLogger.OnIcsLogEvent := IcsLogEvent;
    RestLogger.LogOptions := [loDestEvent];
    IcsLogger := RestLogger;
{$ENDIF}
    RestSslCtx := TSslContext.Create(self) ;
    SslContext := RestSslCtx;
    RestSslCtx.SslVerifyPeer := false ;
{$IFNDEF NO_DEBUG_LOG}
    RestSslCtx.IcsLogger := RestLogger;
{$ENDIF}
    FSslCliCert := TX509Base.Create(self);
    FCertVerMethod := CertVerNone;
    FSslRootFile := 'RootCaCertsBundle.pem';  // blank will use internal bundle
    FSslCliSecurity := sslCliSecTls12;
    FDebugLevel := DebugSsl;
    FRespReq := False;
    FInitSsl := false;
    FHttpMemStrategy := HttpStratMem;      { V8.68 }
    FTempFileName := '';                   { V8.68 }
    FResumeMinSize := 65535 ;              { V8.68 also used for resume overlap }
    FProgIntSecs := 2;                     { V8.68 }
    FOcspHttp := TOcspHttp.Create(Self);   { V8.69 }
    FOcspHttp.OnOcspProg := IcsLogEvent;   { V8.69 }
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslHttpRest.Destroy;
begin
    FOcspHttp.Free;              { V8.69 }
    FreeAndNil(FMimeTypesList);  { V8.69 }
    FRestParams.Free;
    FPostStream.Free;
    FResponseStream.Free;
    if (FTempFileName <> '') and FileExists(FTempFileName) then  { V8.68 delete temporary file }
        IcsDeleteFile(FTempFileName, True);
    FTempFileName := '';
    FResInfRecs.Free;  { V8.68 }
{$IFDEF MSWINDOWS}
    FMsCertChainEngine.Free;
{$ENDIF MSWINDOWS}
    FExternalSslSessionCache.Free;
    RestSslCtx.Free;
    FSslCliCert.Free;
{$IFNDEF NO_DEBUG_LOG}
    RestLogger.Free;
{$ENDIF}
    RestCookies.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.InitSsl;
var
    rootfname: String;
begin
    if FInitSsl then Exit;
{$IFNDEF NO_DEBUG_LOG}
    if FDebugLevel >= DebugSslLow then
        RestLogger.LogOptions := RestLogger.LogOptions + [loSslInfo, loProtSpecInfo];
{$ENDIF}

    if not Assigned (FExternalSslSessionCache) then begin
        FExternalSslSessionCache := TSslAvlSessionCache.Create (self);
 //       fExternalSslSessionCache.AdjustTimeout := True;
 //       fExternalSslSessionCache.SessionTimeOut := 30;
 //       fExternalSslSessionCache.FlushInterval := 3000;
    end;
    RestSslCtx.SslOptions2 := RestSslCtx.SslOptions2 +
       [sslOpt2_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, sslOpt2_NO_RENEGOTIATION];
    RestSslCtx.SslECDHMethod := sslECDHAuto;
    RestSslCtx.SslCipherList := sslCiphersNoDH;     { V8.66 }
    RestSslCtx.SslCliSecurity := FSslCliSecurity;

  // see if verifying server SSL certificate
    if (FCertVerMethod > CertVerNone) then begin
        RestSslCtx.SslVerifyPeer := true;
        RestSslCtx.SslOcspStatus := true;     { V8.69 use OCSP stapling to get revoked status }
        RestSslCtx.SslVerifyPeerModes := [SslVerifyMode_PEER];
        RestSslCtx.SslSessionCacheModes := [sslSESS_CACHE_CLIENT];
        if fSslSessCache then begin
            RestSslCtx.SslSessionCacheModes := [sslSESS_CACHE_CLIENT,
                sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE] ;
        end;
    end ;
    try
        if NOT RestSslCtx.IsCtxInitialized then begin
            RestSslCtx.InitContext;
            if FDebugLevel >= DebugSslLow then begin
               if NOT GSSLStaticLinked  then          { V8.66 }
                    LogEvent('SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName)
                else
                    LogEvent('SSL Statically Linked Version : ' + OpenSslVersion);
            end;
        end;
        FInitSsl := True;
    except
        on E:Exception do
        begin
            LogEvent('Error Starting SSL: ' + E.Message);
        end;
    end;

 // V8.62 can not load bundle until context exists
    if (FCertVerMethod >= CertVerBundle) then begin
        rootfname := fSslRootFile;
        if rootfname <> '' then begin
            if (Pos (':', rootfname) = 0) then
                rootfname := ExtractFileDir (ParamStr (0)) + '\' + rootfname ;
            if NOT FileExists (rootfname) then  begin
                LogEvent('Can Not Find SSL CA Bundle File - ' + rootfname);
                RestSslCtx.LoadCAFromString(sslRootCACertsBundle);  { V8.63 }
            end
            else
                RestSslCtx.SslCAFile := rootfname;
        end
        else
            RestSslCtx.LoadCAFromString(sslRootCACertsBundle);  { V8.63 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.SetSslCliSecurity(Value: TSslCliSecurity);
begin
    if Value = FSslCliSecurity then Exit;
    FSslCliSecurity := Value;
    RestSslCtx.SslCliSecurity := FSslCliSecurity;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.ResetSsl;
begin
    FInitSsl := False;
    if FConnected then CloseAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.SetRestParams(Value: TRestParams);
begin
    FRestParams.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.SetSslCliCert(Value: TX509Base);
begin
    FSslCliCert.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.LogEvent(const Msg : String);
begin
    if FDebugLevel = DebugNone then Exit;
    if Assigned(FonHttpRestProg) then
        FonHttpRestProg(Self, loProtSpecInfo, Msg) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.IcsLogEvent(Sender: TObject; LogOption: TLogOption;
                                                      const Msg : String);
begin
    if Assigned(FonHttpRestProg) then
        FonHttpRestProg(Self, LogOption, Msg) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerCommand(var S: String);    { V8.61 }
begin
    Inherited TriggerCommand(S);
    if FDebugLevel >= DebugHdr then
        LogEvent ('> ' + S) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerHeaderData;   { V8.61 }
begin
    Inherited TriggerHeaderData;
    if FDebugLevel >= DebugHdr then
        LogEvent ('< ' + LastResponse) ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerLocationChange;   { V8.61 }
begin
    Inherited TriggerLocationChange;
  { cookies may have been sent during redirection, so update again now }
    FCookie := RestCookies.GetCookies(FLocation);

    if FDebugLevel >= DebugConn then
        LogEvent('= ' + FURL + ' Redirected to: ' + FLocation);
    if Assigned(FOnRestLocChange) then
        FOnRestLocChange(Self);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerDocBegin;   { V8.61 }
var
    S: String;
begin
    Inherited TriggerDocBegin;

 { only process if RestRequest method started the request }
    if FRespReq then begin

    { V8.68 handle different download memory strategies, FTempFileName already set }
        if (FHttpMemStrategy = HttpStratMem) and (FContentLength > FMaxBodySize) then begin
            FReasonPhrase := 'Aborting connection, Body Size too Large: ' + IntToKbyte(FContentLength);
            LogEvent(FReasonPhrase);
            Abort;
            Exit;
        end;
        if (FHttpMemStrategy <= HttpStratTemp) then begin
            if(FContentLength < FMaxBodySize) then begin
                FTempFileName := '';   // small use MemoryStream
            end;
        end;

     // got content, see if opening file to save it
        if (FStatusCode = 200) or (FStatusCode = 206) then begin
            if FStatusCode = 200 then
                FExpectedSize := FContentLength;

        // attempting resume partial download, only if we have a content size, not chuncked
            if (FHttpMemStrategy = HttpStratResume) and (FContentLength >= 0) and
                                                         (FResumeFileName <> '') then begin
                FreeAndNil(FResponseStream);

                if FStatusCode = 206 then begin  // part content only
                    if ((atoi64(FContentRangeBegin) + FContentLength) <> FExpectedSize) or
                        (FResInfRecs [ResInfDateTime] <> (RFC1123_Date(FRespLastModDT) + ' GMT')) then begin
                        FReasonPhrase := 'Resume failed, source changed?';
                        LogEvent(FReasonPhrase);
                        Abort;
                        Exit;
                    end;
                end;

             // save .http restore file with content size, last modified date and etag }
                FResInfRecs [ResInfAttempts] := IntToStr(atoi (FResInfRecs [ResInfAttempts]) + 1);
                FResInfRecs [ResInfLastBegin] := FContentRangeBegin;
                FResInfRecs [ResInfContSize] := IntToStr(FExpectedSize);
                FResInfRecs [ResInfDateTime] := RFC1123_Date(FRespLastModDT) + ' GMT';
                FResInfRecs [ResInfEtag] := FResponseEtag;
                IcsDeleteFile (FResumeFileName, True);
                FResInfRecs.SaveToFile (FResumeFileName);

             // if we requested resume, see if server did so
                if (FContentRangeBegin <> '') then begin
                    if FStatusCode = 200 then begin  // whole content being returned
                        IcsDeleteFile(FTempFileName, True);
                        LogEvent('Resume download request ignored by server, getting whole file');
                    end
                    else if FStatusCode = 206 then begin  // part content only
                     // open part file so we can add to the end
                        try
                            FResponseStream := TIcsBufferedFileStream.Create(FTempFileName, fmOpenReadWrite, MAX_BUFSIZE);
                            FResponseStream.Position := FResponseStream.Size;
                            LogEvent('Opened old file OK: ' + FTempFileName);
                        except
                            on E:Exception do begin
                                FReasonPhrase := 'Failed to open old file: ' + FTempFileName + ' - ' + E.Message;
                                LogEvent(FReasonPhrase);
                                Abort;
                                Exit;
                            end;
                        end;
                    end
                    else
                        LogEvent('Unexpected resume status: ' + IntToStr(FStatusCode));
                end
                else
                    IcsDeleteFile(FTempFileName, True);  // resume not requested, start again
            end;
            if (FTempFileName <> '') and (FStatusCode <> 206) then begin
                try
                    FreeAndNil(FResponseStream);
                    FResponseStream := TIcsBufferedFileStream.Create(FTempFileName, fmCreate, MAX_BUFSIZE);
                    FRcvdStream := FResponseStream;
                    LogEvent('Opened new file OK: ' + FTempFileName);
                except
                    on E:Exception do begin
                        FReasonPhrase := 'Failed to open file: ' + FTempFileName + ' - ' + E.Message;
                        LogEvent(FReasonPhrase);
                        FTempFileName := '';   // failed, no file to close
                        Abort;
                        Exit;
                    end;
                end;
            end;
            if FShowProgress and Assigned(FonHttpRestProg) then begin
                FProgLastTick := IcsGetTickCount;
                S := FProgMessBase + ', Downloading ' + IntToKByte (FResponseStream.Position);
                if (FExpectedSize > 0) then S := S + ' of ' +  IntToKByte (FExpectedSize);
                FonHttpRestProg(Self, loProgress, S);
            end;
        end
        else begin
            FTempFileName := '';   // failed, no file to close
            FResumeFileName := '';
            FExpectedSize := 0;
        end;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerDocData(Data : Pointer; Len : Integer);               { V8.68 }
var
    S: String;
begin
    Inherited TriggerDocData(Data, Len);
    if NOT FRespReq then Exit;
    if NOT FShowProgress then Exit;
    if NOT Assigned(FonHttpRestProg) then Exit;
    if (FProgIntSecs > 0) then //  slow down updates, default once every two seconds
    begin
        if IcsDiffTicks (FProgLastTick, IcsGetTickCount) < (LongWord (FProgIntSecs) * TicksPerSecond) then Exit;
    end ;
    FProgLastTick := IcsGetTickCount;
    S := FProgMessBase + ', Downloading ' + IntToKByte (FResponseStream.Position);
    if (FExpectedSize > 0) then S := S + ' of ' +  IntToKByte (FExpectedSize);
    FonHttpRestProg(Self, loProgress, S);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerCookie(const Data : String; var   bAccept : Boolean); { V8.61 }
begin
    Inherited TriggerCookie(Data, bAccept);
    RestCookies.SetCookie(Data, FURL);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ triggered even if session is keep-alive }
procedure TSslHttpRest.TriggerSessionConnected;   { V8.61 }
var
    S: String;
begin
    Inherited TriggerSessionConnected;
    if FDebugLevel >= DebugConn then begin
        if FState = httpConnected then begin   { V8.60  }
            S := 'Connected OK to';
            if (FProxy <> '') or  (FSocksServer <> '') then    { V8.62 }
                S := S + ' Proxy';
        end
        else
            S := 'Connection failed to';
        S := S + ': ' + FPunyCodeHost + ' (' + IcsFmtIpv6Addr(AddrResolvedStr) + ')';    { V8.64 }
        LogEvent (S) ;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerSessionClosed;   { V8.61 }
begin
    Inherited TriggerSessionClosed;
    if FDebugLevel >= DebugConn then
        LogEvent ('Connection closed') ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TransferSslVerifyPeer(Sender        : TObject;
                                    var Ok        : Integer;
                                    Cert           : TX509Base);  { V8.61 }
begin
    Inherited TransferSslVerifyPeer(Sender, OK, Cert);
    OK := 1; // don't check certificate until handshaking over
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TransferSslCliGetSession(Sender      : TObject;
                                   var SslSession  : Pointer;
                                  var FreeSession  : Boolean);  { V8.61 }
begin
    Inherited TransferSslCliGetSession(Self, SslSession, FreeSession);
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    if FDebugLevel >= DebugSslLow then
        LogEvent ('Check for Old SSL Session');
    SslSession := fExternalSslSessionCache.GetCliSession(FCtrlSocket.PeerAddr +
                                                    FCtrlSocket.PeerPort, FreeSession);
    if FDebugLevel < DebugSslLow then Exit;
     if Assigned (SslSession) then
        LogEvent ('Old SSL Session Found Cached')
    else
        LogEvent ('No Old SSL Session Cached');
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TransferSslCliNewSession(Sender      : TObject;
                                      SslSession   : Pointer;
                                      WasReused    : Boolean;
                                  var IncRefCount  : Boolean);  { V8.61 }
begin
    Inherited TransferSslCliNewSession(Sender, SslSession, WasReused, IncRefCount);
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    if FDebugLevel >= DebugSslLow then
        LogEvent ('Starting SSL Session');
    if (not WasReused) then begin
        fExternalSslSessionCache.CacheCliSession(SslSession,
                        FCtrlSocket.PeerAddr + FCtrlSocket.PeerPort, IncRefCount);
        if FDebugLevel >= DebugSslLow then
             LogEvent ('Cache SSL Session: New');
    end
    else begin
        if FDebugLevel >= DebugSslLow then
            LogEvent ('Cache SSL Session: Reuse');
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TransferSslHandshakeDone(         { V8.61 }
    Sender         : TObject;
    ErrCode        : Word;
    PeerCert       : TX509Base;
    var Disconnect : Boolean);
var
    CertChain: TX509List;
{$IFDEF MSWINDOWS}
    ChainVerifyResult: LongWord;
{$ENDIF MSWINDOWS}
    info, host, VerifyInfo: String;
    Safe: Boolean;
    HttpCtl: TWSocket;
begin
    Inherited TransferSslHandshakeDone(Sender, ErrCode, PeerCert, Disconnect);
    HttpCtl := (Sender as TSslHttpCli).CtrlSocket ;

  // !!! V8.66 temp diags
//    LogEvent('!!! SSL Handshake CertVerify, Err=' + IcsX509VerifyErrorToStr(HttpCtl.SslVerifyResult));

{$IFNDEF NO_DEBUG_LOG}
    PeerCert.IcsLogger := RestLogger;                                   { V8.69 }
{$ENDIF}

  // nothing much to do if SSL failed or event said disconnect
    if (ErrCode <> 0) or Disconnect then begin
        FReasonPhrase := HttpCtl.SslServerName + ' SSL Handshake Failed: ' + HttpCtl.SslHandshakeRespMsg;
        LogEvent (FReasonPhrase) ;
        exit;
    end  ;
    if FDebugLevel >= DebugSsl then
        LogEvent (HttpCtl.SslServerName + ' ' + HttpCtl.SslHandshakeRespMsg) ;
    if HttpCtl.SslSessionReused OR (FCertVerMethod = CertVerNone) then begin
        exit; // nothing to do, go ahead
    end ;

 // Property SslCertChain contains all certificates in current verify chain
    CertChain := HttpCtl.SslCertChain;

 // see if validating against Windows certificate store - V8.65 only for Windows
    if FCertVerMethod = CertVerWinStore then begin
{$IFDEF MSWINDOWS}
        // start engine
        if not Assigned (FMsCertChainEngine) then
            FMsCertChainEngine := TMsCertChainEngine.Create;

      // see if checking revoocation, CRL checks and OCSP checks in Vista+, very slow!!!!
        if fSslRevocation then
            FMsCertChainEngine.VerifyOptions := [mvoRevocationCheckChainExcludeRoot]
        else
            FMsCertChainEngine.VerifyOptions := [];

        // This option doesn't seem to work, at least when a DNS lookup fails
        FMsCertChainEngine.UrlRetrievalTimeoutMsec := 10000;

        { Pass the certificate and the chain certificates to the engine      }
        FMsCertChainEngine.VerifyCert (PeerCert, CertChain, ChainVerifyResult, True);

        Safe := (ChainVerifyResult = 0) or
                { We ignore the case if a revocation status is unknown.      }
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) or
                (ChainVerifyResult = CERT_TRUST_IS_OFFLINE_REVOCATION) or
                (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN or
                                     CERT_TRUST_IS_OFFLINE_REVOCATION);

       { The MsChainVerifyErrorToStr function works on chain error codes     }
        VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult);

    // MSChain ignores host name, so see if it failed using OpenSSL
        if PeerCert.VerifyResult = X509_V_ERR_HOSTNAME_MISMATCH then begin
            Safe := False;
            VerifyInfo := PeerCert.FirstVerifyErrMsg;
        end;
{$ELSE}
        LogEvent ('Windows certificate store not available');  { V8.65 }
        exit ;
{$ENDIF MSWINDOWS}
    end
    else if FCertVerMethod = CertVerBundle then begin
        VerifyInfo := PeerCert.FirstVerifyErrMsg;
        Safe := (PeerCert.VerifyResult = X509_V_OK);   { check whether SSL chain verify result was OK }

      { V8.69 check OCSP to see if revoked, if we got a chain of certificates }
      { note this is a soft check, if we don't have a stapled OCSP response from the TLS handshake, we get it from an
        OCSP HTTP server and cache it but don't wait for the response. So next attempt comes from cache.  }
        if (Safe and fSslRevocation and PeerCert.IsCertLoaded and (CertChain.Count > 0)) then begin
            FOcspHttp.ClearOcsp;
            FOcspHttp.DebugLevel := FDebugLevel;
            FOcspHttp.OcspCert := PeerCert;
            FOcspHttp.OcspInters := CertChain;
            if (Length(HttpCtl.OcspStapleRaw) > 50) and
                 (HttpCtl.OcspStapleStatus = OCSP_RESPONSE_STATUS_SUCCESSFUL) then
                                        FOcspHttp.OcspRespRaw := HttpCtl.OcspStapleRaw;
            if FOcspHttp.CheckOcspRevoked(RestSslCtx.GetX509Store, 0) then
                Safe := False;
            VerifyInfo := FOcspHttp.OcspLastResp;
            FOcspHttp.OcspInters := Nil;
            LogEvent (HttpCtl.SslServerName + ' ' + VerifyInfo)
         end;

    end
    else begin
        exit ;  // unknown method
    end ;

   // see if allowing self signed
   if (PeerCert.VerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) and
                                        FSslAllowSelfSign then Safe := True;

  // tell user verification failed
    if NOT Safe then begin
        info := 'SSL Chain Verification Failed: ' + VerifyInfo + ', Domain: ';
        if PeerCert.SubAltNameDNS = '' then
            host := IcsUnwrapNames(PeerCert.SubjectCName)
        else
            host := IcsUnwrapNames(PeerCert.SubAltNameDNS);
        info := info + host;
        if host <> HttpCtl.SslServerName then  { V8.62 only expected if different }
            info := info + ', Expected: ' + HttpCtl.SslServerName;
        if FDebugLevel >= DebugSsl then
            LogEvent (info);
        FReasonPhrase := info;  { V8.58 }
    end
    else begin
        if FDebugLevel >= DebugSsl then
           LogEvent (HttpCtl.SslServerName + ' SSL Chain Verification Succeeded') ;
    end;

  // if certificate checking failed, see if the host is specifically listed as being allowed anyway
    if (NOT Safe) and (SslAcceptableHosts.IndexOf (HttpCtl.SslServerName) > -1) then begin
        Safe := true ;
        if FDebugLevel >= DebugSsl then
            LogEvent (HttpCtl.SslServerName + ' SSL Succeeded with Acceptable Host Name') ;
    end ;

  // tell user about all the certificates we found
    if (FDebugLevel >= DebugSsl) and fSslReportChain and (CertChain.Count > 0) then  begin
        info := HttpCtl.SslServerName + ' ' + IntToStr (CertChain.Count) +
                ' SSL Certificates in the verify chain:' + #13#10 +
                    CertChain.AllCertInfo (true, true) + #13#10 ; // Mar 2017 report all certs, backwards
        if FDebugLevel >= DebugSsl then
            LogEvent (info);
    end;

  // all failed, V8.65 need to remove cached SSL session so it's not reused!!!
    if NOT Safe then begin
        Disconnect := TRUE;
        if fSslSessCache then begin
            if fExternalSslSessionCache.RemoveSession(HttpCtl.PeerAddr + HttpCtl.PeerPort) then
                LogEvent('SSL Session Uncached After Failure')
            else
                LogEvent('SSL Session Not Found in Cache');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TransferSslCliCertRequest(Sender: TObject; var Cert: TX509Base);  { V8.61 }
begin
    Inherited TransferSslCliCertRequest(Sender, Cert);
    if FSslCliCert.IsCertLoaded then begin
        Cert := FSslCliCert;
        if FDebugLevel >= DebugSsl then
            LogEvent('Client SSL Certificate Sent') ;
    end
    else
        LogEvent('No Client SSL Certificate to Send') ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.onCookiesNewCookie(Sender : TObject; ACookie : TCookie;
    var Save : Boolean);
var
    S : String;
begin
    if FDebugLevel < DebugParams then Exit;

 // tell user what cookie was saved, optional
    with ACookie do begin
        S := 'NewCookie: ' + CName + '=' + CValue + ', Domain=' + CDomain + ', Path=' + CPath;
        if CPersist then
            S := S + ', Expires=' + DateTimeToStr(CExpireDT)
        else
            S := S + ', Not Persisent';
        if CSecureOnly then
            S := S + ', SecureOnly';
        if CHttpOnly then
            S := S + ', HttpOnly';
        LogEvent(S);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslHttpRest.GetParams: AnsiString;  { V8.64 lost reqtype }
begin
    Result := '';
    if (FRestParams.Count > 0) then begin
        Result := FRestParams.GetParameters;
    { V8.64 use PContBodyJson for POST, set automatically in RestRequest }
        if (FRestParams.PContent = PContJson) then  // must flatten Json for GET/PUT
            Result:= IcsBase64UrlEncodeA(Result);
        if (FRestParams.PContent = PContBodyJson) then   { V8.61 added UTF8 }
                FContentPost := MimeAppJson
        else if (FRestParams.PContent = PContBodyUrlEn) then   { V8.61 added UTF8 }
                FContentPost := MimeAppForm
        else if (FRestParams.PContent = PContBodyXML) then   { V8.61 added UTF8 }
                FContentPost := MimeAppXml ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslHttpRest.GetResponseJson: ISuperObject;
var
    ErrStr: String;
begin
    Result := Nil;
    if NOT Assigned(FResponseJson) and (FResponseRaw <> '') then begin
      { V8.68 check we have a Json object, beware HTML CSS also has {}
        if (Pos('{', FResponseRaw) > 0) and (Pos('}', FResponseRaw) > 0) then begin
            try
             { V8.65 actually get parse errors with new function rather than ignoring them }
                FResponseJson := TSuperObject.ParseStringEx(PWideChar(FResponseRaw), True, ErrStr);
                Result := FResponseJson;
                if ErrStr <> '' then
                    LogEvent('Failed to parse Json response: ' + ErrStr);      { V8.65 }
                Exit;
            except
                on E:Exception do begin
                    LogEvent('Failed to parse Json response: ' + E.Message);   { V8.65 }
                    FResponseJson := Nil;
                    Exit;
                end;
            end;
            if NOT Assigned (FResponseJson) then       { V8.55 }
                LogEvent('Failed to parse Json response');
        end
        else
           LogEvent('No Json in response');
    end
    else
        Result := FResponseJson;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslHttpRest.GetResponseOctet: AnsiString;
begin
    Result := '';
    if FResponseSize = 0 then Exit;
    if NOT Assigned(FResponseStream) then Exit;       { V8.68 }
    FResponseStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ;  }
    SetLength (Result, FResponseSize);
    FResponseStream.Read(Result[1], FResponseSize);
    FResponseStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.DoRequestAsync(Rq : THttpRequest); { V8.66 }
begin
    if NOT FInitSsl then InitSsl;   // first call may be legacy GET or PUT
    Inherited DoRequestAsync(Rq);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.TriggerRequestDone2;  { V8.61 }
var
    Info: String;
begin
    if FStatusCode > 0 then
        FReasonPhrase := IntToStr(FStatusCode) + ' ' + FReasonPhrase;   { V8.69 OK becomes 200 OK, etc }
    if FRequestDoneError <> 0 then begin   // ReasonPhrase has description of ErrCode
        Info := 'Error: ' + RequestDoneErrorStr + ' - ' + FReasonPhrase;  { V8.68 but ErrCode may have more info }
        LogEvent('Request failed: ' + Info) ;
        if FRespReq then begin  // only process response for REST request
            FreeAndNil(FResponseStream);                             { V8.68 }
         { V8.68 prevent .part file being deleted for resume }
            if (FHttpMemStrategy = HttpStratResume) then begin
                if (FResumeFileName <> '') and (FTempFileName <> '') and
                    FileExists(FTempFileName) and FileExists(FResumeFileName) then
                               LogEvent('Failed download can be resumed using: ' + FTempFileName) ;
                FTempFileName := '';
                FResumeFileName := '';
            end;
            if FShowProgress and Assigned(FonHttpRestProg) then begin
                FonHttpRestProg(Self, loProgress, FProgMessBase + ', Download failed: ' + FReasonPhrase);
            end;
        end;
        FRespReq := False;
    end
    else begin  { V8.58 }
        LogEvent('Request completed: ' + FReasonPhrase);
        try
            if FRespReq then begin  // only process response for REST request
                FRespReq := False;
                FResponseSize := FResponseStream.Size;

            // we process all responses here, since failures may have Json response
                if (FResponseSize > 0) and (FResponseSize <= FMaxBodySize) then begin  { V8.68 not too large }
                    FResponseStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }

                  // convert response to correct codepage, including entities
                    if (Pos ('text/', FContentType) = 1) or
                         (Pos ('json', FContentType) <> 0) or
                           (Pos ('javascript', FContentType) <> 0) or  { V8.61 }
                             (Pos ('xml', FContentType) <> 0) then begin
                        FResponseRaw := IcsHtmlToStr(FResponseStream, FContentType, true);
                        FResponseStream.Position := 0; { V8.67 Seek (0, soFromBeginning) ; }
                        if DebugLevel >= DebugBody then
                            LogEvent('Response (length ' + IntToKbyte(Length(FResponseRaw)) +
                                                                  ')' + IcsCRLF +  FResponseRaw);
                    end
                    else if DebugLevel >= DebugBody then
                            LogEvent('Response Non-Textual (Size: ' + IntToKbyte(FResponseSize));
                end;

          { V8.68 see if saving content file }
                if (FHttpMemStrategy >= HttpStratFile) and (FTempFileName <> '') then begin
                    try
                        FreeAndNil(FResponseStream);  // close temporary .part file to flush to disk
                        if (FResponseSize = FExpectedSize) or (FContentLength < 0) or
                                                     (FHttpMemStrategy <> HttpStratResume) then begin
                            if (IcsRenameFile (FTempFileName, FHttpDownFileName, FHttpDownReplace, True) <> 0) then begin
                               LogEvent('Error Renaming File: ' + FTempFileName + ' to ' + FHttpDownFileName) ;
                            end
                            else begin
                                LogEvent('Saved File OK: ' + FHttpDownFileName + ', Size: ' + IntToKbyte(FResponseSize));
                             //   UpdateFileAge (FHttpDownFileName, xx) ;  // not UTC date
                             // open disk file again so application can access it
                                FResponseStream := TIcsBufferedFileStream.Create(FHttpDownFileName, fmOpenRead, MAX_BUFSIZE);
                            end;
                            if FResumeFileName <> '' then
                                IcsDeleteFile (FResumeFileName, True);  // no longer need .http resume file
                        end
                        else begin
                            LogEvent('Partial download saved as: ' + FTempFileName + ', Size: ' + IntToKbyte(FResponseSize));
                            if (FResumeFileName <> '') and (FTempFileName <> '') and
                                FileExists(FTempFileName) and FileExists(FResumeFileName) then
                                   LogEvent('Failed download can be resumed using: ' + FTempFileName) ;
                        end;
                        FTempFileName := '';
                        FResumeFileName := '';
                    except
                        on E:Exception do begin
                            LogEvent('Failed to Save File: ' + FHttpDownFileName + ' - ' + E.Message);
                        end;
                    end;
                end;
                if FShowProgress and Assigned(FonHttpRestProg) then begin    { V8.68 }
                    if (FStatusCode = 200) or (FStatusCode = 206) and (FResponseSize > 0) then begin
                        if FSendStream.Size > 0 then
                            FonHttpRestProg(Self, loProgress, FProgMessBase + ', Upload completed, Size: ' + IntToKByte (FSendStream.Size))    { V8.69 }
                        else
                            FonHttpRestProg(Self, loProgress, FProgMessBase + ', Request completed, Size: ' + IntToKByte (FResponseSize));
                    end
                    else
                        FonHttpRestProg(Self, loProgress, FProgMessBase + ', Request failed: ' + FReasonPhrase);
                end;
            end;
        except
            on E:Exception do begin
                LogEvent('Failed to process response: ' + E.Message);
            end;
        end;
    end;
    if Assigned (FOnRestRequestDone) then
        FOnRestRequestDone(Self, FRequestType, FRequestDoneError);
    Inherited TriggerRequestDone2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslHttpRest.ClearResp;
begin
    FreeAndNil(FPostStream);       { V8.69 may be file stream }
    FreeAndNil(FResponseStream);   { V8.68 may be file stream }
    if (FTempFileName <> '') and FileExists(FTempFileName) then   { V8.68 delete temporary file }
        IcsDeleteFile(FTempFileName, True);
    FTempFileName := '';
    FResumeFileName := '';
    FPostStream := TMemoryStream.Create;
    FResponseStream := TMemoryStream.Create;
    FResponseJson := Nil;
    FResponseRaw := '';
    FResponseSize := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ make an HTTP request to RestURL.  If RestURL has no parameters (ie ?, except
  POST)) then RawParams are added if not blank, otherwise RestParams are added }
{ V8.64 added ConType for POST/PUT/PATCH body content type }

function TSslHttpRest.RestRequest(HttpRequest: THttpRequest; const RestURL: String;
                    AsyncReq: Boolean = False; const RawParams: String = ''): Integer;
var
    Info: String;
    Params: AnsiString;
    FLen, Attempts: Integer;
    ResPosFrom, Newsize, LastBegin: Int64;
    MimeHeader, MimeFooter, RandBoundary: String;
begin
    result := -1;
    FReasonPhrase := '';
    ClearResp;  // create empty memory stream
    if (Pos('http', RestURL) <> 1) then begin
        FReasonPhrase := 'Need valid URL: ' + RestURL;
        LogEvent (FReasonPhrase) ;
        Exit;
    end;
    if (FState <> httpReady) then begin
        FReasonPhrase := 'Component is not ready, doing last request';
        LogEvent (FReasonPhrase) ;
        Exit;
    end;
    FRespReq := True;
    InitSsl;

  { V8.68 different memory strategies depending on size of response we expect,
    stream is opened in TriggerDocBegin once we know the content size, and closed
    and renamed to real file name in TriggerRequestDone2, then reopened as
    ResponseStream. }

    case FHttpMemStrategy of
        HttpStratTemp: begin
            if FTempPath = '' then begin
{$IFDEF MSWINDOWS}
                SetLength(FTempPath, 1024);
                FLen := GetTempPath(Length(FTempPath) - 1, PChar(FTempPath));
                SetLength(FTempPath, FLen);
{$ENDIF}
{$IFDEF POSIX}
                FTempPath := TPath.GetTempPath;
{$ENDIF}
                FTempPath := IncludeTrailingPathDelimiter (FTempPath);
            end;
            FTempFileName := FTempPath + 'tsslhttprest' + IntToStr(Random(999999999)) + '.tmp';
        end;
        HttpStratFile, HttpStratResume: begin
            if (NOT FHttpDownReplace) and FileExists(FHttpDownFileName) then begin
                FReasonPhrase := 'Download file already exists: ' + FHttpDownFileName;
                LogEvent (FReasonPhrase) ;
                Exit;
            end;
            FTempFileName := ExtractFilePath(FHttpDownFileName);
            if NOT IcsDirExists(FTempFileName) then begin
                FReasonPhrase := 'Download directory does not exist: ' + FTempFileName;
                LogEvent (FReasonPhrase) ;
                Exit;
            end;
            FTempFileName := FHttpDownFileName + '.part';
            FContentRangeBegin := '';
            FContentRangeEnd := '';
            FExpectedSize := 0;

          { see if attempting to resume a previous partial download }
            ResPosFrom := IcsGetFileSize (FTempFileName) ;  // .part file
            if FHttpMemStrategy = HttpStratResume then begin
                if FResInfRecs = Nil then begin
                    FResInfRecs := TStringList.Create ;
                    {$IFDEF COMPILER15_UP}
                    FResInfRecs.WriteBOM := False;
                    {$ENDIF COMPILER15_UP}
                end;
                FResumeFileName := FHttpDownFileName + '.http';
                if (ResPosFrom >= 0) and FileExists(FResumeFileName) then begin
                    try  // except
                        FResInfRecs.LoadFromFile (FResumeFileName) ;
                        if (ResPosFrom > 0) then begin
                            if IcsFileInUse (FTempFileName) then begin  // ensure file not open
                                IcsDeleteFile (FResumeFileName, true); // kill resume file
                                ResPosFrom := 0;
                                LogEvent ('Error Temp File in Use - ' + FTempFileName);
                                FTempFileName := FTempFileName + '2'; // new name
                                FResumeFileName := '';
                            end ;
                        end ;
                        if (ResPosFrom >= (FResumeMinSize + 100)) and (FResInfRecs.Count > ResInfLastBegin) then begin
                            Attempts := atoi (FResInfRecs [ResInfAttempts]);
                            LastBegin := atoi64 (FResInfRecs [ResInfLastBegin]);
                            FExpectedSize := atoi64 (FResInfRecs [ResInfContSize]);
                            if (Attempts > 6) then begin
                                LogEvent ('Skipped Resume, Too Many Attempts (5)');
                                IcsDeleteFile(FTempFileName, True);
                            end
                            else if (LastBegin = ResPosFrom) then begin
                                LogEvent ('Skipped resume, same size as last attempt');
                                IcsDeleteFile(FTempFileName, True);
                            end
                            else if (ResPosFrom >= FExpectedSize) then begin
                                LogEvent ('Skipped resume, partial file too large');
                                IcsDeleteFile(FTempFileName, True);
                            end
                            else begin
                                if (FResInfRecs [ResInfURL] = RestURL) and ((FResInfRecs [ResInfDateTime] <> '') or
                                                                                 (FResInfRecs [ResInfEtag] <> '')) then begin
                                // reduce file size in case content is corrupted near end
                                    NewSize := IcsTruncateFile (FTempFileName, ResPosFrom - FResumeMinSize);
                                    ResPosFrom := IcsGetFileSize (FTempFileName) ;
                                    if NewSize <> ResPosFrom then
                                        LogEvent('Failed to Reduce Resume File Size for Overlap')
                                    else begin
                                        LogEvent('Attempting to Resume Partial File Download from: ' +
                                               IcsInt64ToCStr (ResPosFrom) + ', with Overlap ' + IcsIntToCStr (FResumeMinSize));
                                     // open part file so we can add to the end
                                        FResponseStream := TIcsBufferedFileStream.Create(FTempFileName, fmOpenReadWrite, MAX_BUFSIZE);
                                        FResponseStream.Position := ResPosFrom;
                                        FContentRangeBegin := IntToStr(ResPosFrom);  // tell web server we want part range
                                        FContentRangeEnd := IntToStr(FExpectedSize);
                                        if (FResInfRecs [ResInfEtag] <> '') then     // but only if it can match timestamp or etag
                                            FContentIfRange := FResInfRecs [ResInfEtag]
                                        else if (FResInfRecs [ResInfDateTime] <> '') then
                                            FContentIfRange := FResInfRecs [ResInfDateTime];
                                    end;
                                end;
                            end;
                        end;
                    except
                        on E:Exception do begin
                            LogEvent('Resume Attempt Failed: ' + E.Message);
                            IcsDeleteFile (FResumeFileName, true) ; // kill resume file
                        end;
                    end;
                end;

            // prepare .http resume file, rest completed once we have headers with size and date
                if (FResInfRecs.Count < ResInfLastBegin) then begin
                    while (FResInfRecs.Count <= ResInfLastBegin) do FResInfRecs.Add ('');
                end;
                FResInfRecs [ResInfURL] := RestURL;
            end ;

         // not resuming and old part file exists, kill it
            if (ResPosFrom >= 0) and (FContentRangeBegin = '') then begin
                IcsDeleteFile (FTempFileName, True);
            end;
        end;
    end;

    FRcvdStream := FResponseStream;
    FResponseNoException := True;  // stop exception for sync requests
    try
        FURL := RestURL;
        FCookie := RestCookies.GetCookies (RestURL);
        FOcspHttp.OcspHttpProxy := FProxyURL;         { V8.69 use same proxy for OCSP }

    { V8.64 PContent now used to determine if PUT paramaters should be
      be sent as a content body or in the URL, but POST is always body to
      correct PContent if wrong }
        if (HttpRequest = httpPOST) then begin
            if (FRestParams.PContent = PContJson) then
                FRestParams.PContent := PContBodyJson;
            if (FRestParams.PContent = PContUrlencoded) then
                FRestParams.PContent := PContBodyUrlEn;
        end;
        Params := StringToUtf8(RawParams);
        if (Params = '') then Params := GetParams;

     { V8.64 set Json content type if empty }
        if (Params <> '') and (FContentPost = '') then begin
           if (Params[1] = '{') or (Params[1] = '[') then
                FContentPost := MimeAppJson;  { V8.61 added UTF8 }
        end;

    { V8.69 see if uploading a file }
        if (FHttpUploadStrat > HttpUploadNone) and (HttpRequest in [httpPOST, httpPUT]) and
                                                                (FHttpUploadFile <> '') then begin
            Newsize := IcsGetFileSize(FHttpUploadFile);
            if (Newsize < 0) then begin
                FReasonPhrase := 'Upload file not found: ' + FHttpUploadFile;
                LogEvent (FReasonPhrase) ;
                Exit;
            end;
            if NOT Assigned(FMimeTypesList) then
                FMimeTypesList := TMimeTypesList.Create(Self);
            if (FHttpUploadStrat = HttpUploadSimple) then begin
                FreeAndNil(FPostStream);
                try
                    FPostStream := TIcsBufferedFileStream.Create(FHttpUploadFile, fmOpenRead, MAX_BUFSIZE);
                    FPostStream.Position := 0;
                    FSendStream := FPostStream;
                except
                    on E:Exception do begin
                        FReasonPhrase := 'Open Upload File Failed: ' + FHttpUploadFile + ' - ' + E.Message;
                        LogEvent (FReasonPhrase) ;
                        Exit;
                    end;
                end;
                FContentPost := FMimeTypesList.TypeFromFile(FHttpUploadFile);
                if (Pos('?', FURL) = 0) and (Params <> '') then
                    FURL := RestURL + '?' + String(Params);
            end
            else if (FHttpUploadStrat = HttpUploadMIME) then begin
                RandBoundary := '-----------------------------' + IntToHex(Random(MaxInt), 8) + IntToHex(Random(MaxInt), 8);
                FContentPost := MimeMultipart + RandBoundary;
                MimeHeader := RandBoundary + IcsCRLF +
                    'Content-Disposition: form-data; name="FileName"; FileName="' + TextToHtmlText(ExtractFileName(FHttpUploadFile)) + '"' + IcsCRLF +
                    'Content-Type: ' + FMimeTypesList.TypeFromFile(FHttpUploadFile) + IcsCRLF + IcsCRLF;
          // Pending, FileTitle section does not decode in ICS web server samples, not formed correctly???           
                MimeFooter := RandBoundary + IcsCRLF +
                    'Content-Disposition: form-data; name="FileTitle"; ' + String(Params) + IcsCRLF +
                    RandBoundary + IcsCRLF +
                    'Content-Disposition: form-data; name="Submit"' + IcsCRLF + IcsCRLF +
                    'SubmitFile' + IcsCRLF + RandBoundary + '--' + IcsCRLF;
                FreeAndNil(FPostStream);
                try
                    FPostStream := TMultiPartFileReader.Create (FHttpUploadFile, MimeHeader, MimeFooter);
                    FPostStream.Position := 0;
                    FSendStream := FPostStream;
                except
                    on E:Exception do begin
                        FReasonPhrase := 'Open Upload File Failed: ' + FHttpUploadFile + ' - ' + E.Message;
                        LogEvent (FReasonPhrase) ;
                        Exit;
                    end;
                end;
            end;
            LogEvent ('Uploading File: ' + FHttpUploadFile + ', Size ' + IcsInt64ToCStr (NewSize));
        end
        else begin

    { V8.64 see if content type means sending a body }
            if (Params <> '') and (FRestParams.PContent in              { V8.65 check some content }
                             [PContBodyJson, PContBodyUrlEn, PContBodyXML]) then begin
                (FPostStream as TMemoryStream).Clear;
                FPostStream.Write(Params[1], Length(Params));
                FPostStream.Position := 0; { V8.67 Seek(0, soFromBeginning) ; }
                FSendStream := FPostStream;
            end
            else begin
         { V8.64 no content type means URL arguments }
                if (Pos('?', FURL) = 0) and (Params <> '') then { V8.65 not for blank params }
                    FURL := RestURL + '?' + String(Params);
            end;
        end;

     { make real HTTP request }
        if HttpRequest = httpGET then Info := 'GET '
        else if HttpRequest = httpHEAD then Info := 'HEAD '
        else if HttpRequest = httpPOST then Info := 'POST '
        else if HttpRequest = httpPUT then Info := 'PUT '
        else if HttpRequest = httpDELETE then Info := 'DELETE '
        else if HttpRequest = httpPATCH then Info := 'PATCH ';
        Info := Info + RestURL;
        if (FDebugLevel >= DebugParams) and (Params <> '') then
            Info := Info + IcsCRLF + String(Params);
        LogEvent(Info);
        FStatusCode := 0;
        if FShowProgress and Assigned(FonHttpRestProg) then begin    { V8.68 }
            FProgMessBase := RestURL;
            FonHttpRestProg(Self, loProgress, FProgMessBase + ', Getting headers');
        end;
        if AsyncReq then
            DoRequestASync(HttpRequest)
        else
            DoRequestSync(HttpRequest);
        Result := FStatusCode;  // only for sync requests
    except
        on E:Exception do begin    { 400/500 no longer come here }
            if FRespReq then  { may have reported in Done }
                LogEvent('Request failed: ' + E.Message);
            Result := FStatusCode;
            if Result = 200 then Result := 0; // not really successful
            FRespReq := False;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TDnsQueryHttps V8.61 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQueryHttps.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    HttpRest := TSslHttpRest.Create(self);
    HttpRest.OnHttpRestProg := DnsRestProg;
    HttpRest.OnRestRequestDone := DnsRestRequestDone;
    FDnsSrvUrl := DnsPublicHttpsTable[0];
    FDebugLevel := DebugNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQueryHttps.Destroy;
begin
    FreeAndNil(HttpRest);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQueryHttps.DnsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnDnsProg) then
        FOnDnsProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TDnsQueryHttps.DOHQueryAll(Host: String): Boolean;
begin
    FMultiReqSeq  := 1;
    FMultiHost := Host;
    FAnsTot := 0;
    Result := DOHQueryAny(FMultiHost, DnsAllReqTable[FMultiReqSeq], True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQueryHttps.DOHQueryAny(Host: String; QNumber: integer; MultiRequests: Boolean = False): Boolean;
var
    QueryBuf: AnsiString;
    QueryLen, StatCode: Integer;
begin
    Result := False;
    if Pos('https://', FDnsSrvUrl) <> 1 then begin
        DnsRestProg(Self, loProtSpecErr, 'Must Specify DNS over HTTPS Server URL');
        Exit;
    end;
    if NOT MultiRequests then FAnsTot := 0;  { reset result records }
    HttpRest.RestParams.Clear;
    HttpRest.DebugLevel := FDebugLevel;
    HttpRest.Accept := MimeDnsMess;
    HttpRest.ContentTypePost := MimeDnsMess;
    HttpRest.NoCache := True;

// build binary wire format request per RFC8484, same as UDP requests RFC1035,
// but ID always 0, so we build and parse requests with TDnsQuery component
    SetLength(QueryBuf, 512);
    BuildRequestHeader(PDnsRequestHeader(@QueryBuf[1]),0,
                                           DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    QueryLen := BuildQuestionSection(@QueryBuf[SizeOf(TDnsRequestHeader) + 1],
                                              IcsTrim(Host), QNumber, DnsClassIN);  { V8.64 }
    QueryLen := QueryLen + SizeOf(TDnsRequestHeader);
    SetLength(QueryBuf, QueryLen);
    StatCode := HttpRest.RestRequest(httpPOST, FDnsSrvUrl, True, String(QueryBuf));  // async request
    Result := (StatCode = 0);  // raises exception on failure
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQueryHttps.DnsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
    RespBuf: AnsiString;
begin
    if RqType <> httpPOST then Exit;    { V8.68 don't care }
    if ErrCode <> 0 then begin
        DnsRestProg(Self, loProtSpecErr, 'Request failed, error #' + IntToStr(ErrCode) +
              '. Status = ' + IntToStr(HttpRest.StatusCode) + ' - ' + HttpRest.ReasonPhrase);
        TriggerRequestDone(ErrCode);
        Exit;
    end;
    if (HttpRest.StatusCode = 200) and (HttpRest.ContentType = MimeDnsMess) then begin
        RespBuf := HttpRest.ResponseOctet;
        if DecodeWireResp(@RespBuf[1], HttpRest.ContentLength) then begin

           // if simulating ALL request make next request in sequence
            if FMultiReqSeq > 0 then begin
                FMultiReqSeq := FMultiReqSeq + 1;
                if FMultiReqSeq <= DnsAllReqTot then begin
                    DOHQueryAny(FMultiHost, DnsAllReqTable[FMultiReqSeq], True);
                    Exit;
                end;
                FMultiReqSeq := 0;
            end;
            TriggerRequestDone(0);  // all done
        end
        else
            TriggerRequestDone(99);
    end
    else
       TriggerRequestDone(HttpRest.StatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsSms V8.61 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsSms.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    HttpRest := TSslHttpRest.Create(self);
    HttpRest.OnHttpRestProg := SmsRestProg;
    HttpRest.OnRestRequestDone := SmsRestRequestDone;
    FSmsProvider := SmsProvKapow;
    FDebugLevel := DebugNone;
    FSendDT := Now;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsSms.Destroy;
begin
    FreeAndNil(HttpRest);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsSms.SetAccountJson(Value: String);   { V8.65 }
begin
    if FAccountJson <> Value then begin
        FAccountJson := Value;
        FAccountJwt := '';       // force new JWT
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsSms.SmsRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnSmsProg) then
        FOnSmsProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsSms.MakeRequest(HttpRequest: THttpRequest; const RestURL: String;
                      AsyncReq: Boolean = False; const RawParams: String = ''): Boolean;
var
    StatCode: Integer;
    LoginJson: ISuperObject;
    WideAcc: WideString;
//    JwtPayload, NewAccoutJwt: String;
//    UnixTime: Int64;
begin
    Result := False;
    FLastError := '';
    FLastResp := '';
    FCredits := '';
    FSentID := '';
    FDelivery := '';
    if FSmsProvider = SmsProvKapow then begin
        if (FAccountName = '') or (FAccountPw = '') then begin
            FLastError := 'Must Specify Kapow Account Login';
            Exit;
        end;
        HttpRest.RestParams.AddItem('username', FAccountName);
        HttpRest.RestParams.AddItem('password', FAccountPW);
        HttpRest.RestParams.PContent := PContUrlencoded;
        HttpRest.SslCliSecurity := sslCliSecBack;  // only supports TLS1 !!!
        HttpRest.DebugLevel := FDebugLevel;
        StatCode := HttpRest.RestRequest(httpPOST, RestURL, AsyncReq, RawParams);
        if AsyncReq then
            Result := (StatCode = 0)
        else
            Result := (StatCode = 200);  // raises exception on failure
    end
    else if FSmsProvider = SmsProvSmsWorks then begin
        WideAcc := FAccountJson;
        LoginJson := TSuperObject.ParseString(PWideChar(WideAcc), True);
    (*  this block of Json come from The SMS Works account API, convert it into JWT
     {
      "customerid": "8545-xxxx-4e16-45bf-xxxx-506561072b83",
      "key": "a87166be-xxxx-4cf3-xxxx-d6cdbd85fcfd",
      "secret": "a29b39ax7x8x1xaxcx9x2xaxbx8x9x7x2xcx4xfxdx2x4xx8078b5f2f49d5f253"
    }  *)
        if NOT Assigned(LoginJson) then  begin
            FLastError := 'The SMS Works Needs Valid Login Json from Account';   { V8.63 removed space, added The }
            Exit;
        end;

     { see if have Json Web Token, otherwise get it using login Json }
        if FAccountJwt = '' then begin
            HttpRest.ServerAuth := httpAuthNone;
            HttpRest.ContentTypePost := MimeAppJson;
            HttpRest.SslCliSecurity := sslCliSecHigh;
            HttpRest.DebugLevel := FDebugLevel;
            StatCode := HttpRest.RestRequest(httpPOST, 'https://api.thesmsworks.co.uk/v1/auth/token', False, FAccountJson);
            if (StatCode <> 200) then begin
                FLastResp := HttpRest.ResponseRaw;
                FLastError := HttpRest.ResponseJson.S['message'];
                Exit;
            end;
            FAccountJwt := HttpRest.ResponseJson.S['token'];
            if Pos ('JWT ', FAccountJwt) = 1 then
                FAccountJwt := Copy(FAccountJwt, 5, 99999)
            else begin
                FLastError := 'Invalid JWT Token from The SMS Works';    { V8.63 added The }
                Exit;
            end;
        end;

        HttpRest.AuthBearerToken := FAccountJwt;
        HttpRest.ServerAuth := httpAuthJWT;
        HttpRest.Accept := MimeAppJson;
        HttpRest.RestParams.PContent := PContJson;
        HttpRest.SslCliSecurity := sslCliSecHigh;
        HttpRest.DebugLevel := FDebugLevel;
        StatCode := HttpRest.RestRequest(HttpRequest, RestURL, AsyncReq, RawParams);
        if AsyncReq then
            Result := (StatCode = 0)
        else
            Result := (StatCode = 200) or (StatCode = 201);  // raises exception on failure V8.63 or 201
    end
    else begin
        FLastError := 'Unknown Provider';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsSms.SendSms(const MobileNums, SmsMsg: String; AsyncReq: Boolean = True): Boolean;
var
    Msg, NumArray, Path: String;
    NumList: TStringList;
    I: Integer;
begin
    Result := False;
    FLastError := '';
    NumList := TStringList.Create;
    try
        if (Length(SmsMsg) = 0)  then begin
           FLastError := 'Must Specify SMS Message';
            Exit;
        end;
        Msg := Trim(SmsMsg); // remove training CRLF
        if (Length(MobileNums) < 6) then begin
            FLastError := 'Must Specify Longer Mobile Telephone Number';
            Exit;
        end;
        NumList.CommaText := MobileNums;
        if NumList.Count = 0 then Exit; // can not be blank
    // remove blank or suppressed lines
        for I := 0 to NumList.Count - 1 do begin
            if (Length(NumList[I]) = 0) or
                (NumList[I][1] = '*') then
                    NumList.Delete(I);
            if I >= NumList.Count then break;
        end;
        if NumList.Count = 0 then Exit; // can not be blank
        for I := 0 to NumList.Count - 1 do begin
            NumList[I] := StringReplace(NumList[I], IcsSpace, '', [rfReplaceAll]);
            if (Pos ('00', NumList[I]) = 1) then begin
                FLastError := 'Internaional Access Code Not Needed - ' + NumList[I];
                Exit;
            end;
            if Length(NumList[I]) < 6 then begin
                FLastError := 'Must Specify Longer Mobile Telephone Number - ' + NumList[I];
                Exit;
            end;
        end;
        if FSmsProvider = SmsProvKapow then begin
            HttpRest.ServerAuth := httpAuthNone;
            Msg := StringReplace(Msg, IcsCRLF, '\r', [rfReplaceAll]);
            HttpRest.RestParams.Clear;
            HttpRest.RestParams.AddItem('mobile', NumList[0]);     // only one at moment!!
            if FMsgSender <> '' then
                HttpRest.RestParams.AddItem('from_id', FMsgSender, False);
            HttpRest.RestParams.AddItem('returnid', 'TRUE');  // non-standard
            HttpRest.RestParams.AddItem('sms', Msg, False);
            FSmsOperation := SmsOpSend;
            Result := MakeRequest(httpPOST, 'https://secure.kapow.co.uk/scripts/sendsms.php', AsyncReq);
        end
        else if FSmsProvider = SmsProvSmsWorks then begin
            HttpRest.RestParams.Clear;
            if NumList.Count = 1 then begin
                HttpRest.RestParams.AddItem('destination', NumList[0]);
                Path := 'message/send';
            end
            else begin
                NumArray := '["';
                for I := 0 to NumList.Count - 1 do
                    NumArray := NumArray + NumList[I] + '","';
                SetLength(NumArray, Length(NumArray)-2);
                NumArray := NumArray + ']';
                Path := 'batch/send';
                HttpRest.RestParams.AddItem('destinations', NumArray, True);
            end;
            if FMsgSender <> '' then
                HttpRest.RestParams.AddItem('sender', FMsgSender);
            HttpRest.RestParams.AddItem('content', Msg);
            HttpRest.RestParams.AddItem('tag', 'ICS');
            HttpRest.RestParams.AddItemDT('schedule', FSendDT);  // ISO time in UTC with time zone
            FSmsOperation := SmsOpSend;
            Result := MakeRequest(httpPOST, 'https://api.thesmsworks.co.uk/v1/' + Path, AsyncReq);
        end
        else begin
            FLastError := 'Unknown Provider';
        end;
    finally
        NumList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsSms.CheckSMS(ID: String; AsyncReq: Boolean = True; Batch: Boolean = False): Boolean;
begin
    Result := False;
    FLastError := '';
    if (ID = '') then begin
        FLastError := 'Must Specify Message ID';
        Exit;
    end;
    if FSmsProvider = SmsProvKapow then begin
        HttpRest.RestParams.Clear;
        HttpRest.RestParams.AddItem('returnid', ID);
        FSmsOperation := SmsOpCheck;
        Result := MakeRequest(httpPOST, 'https://secure.kapow.co.uk/scripts/chk_status.php', AsyncReq);
    end
    else if FSmsProvider = SmsProvSmsWorks then begin
        HttpRest.RestParams.Clear;
        FSmsOperation := SmsOpCheck;
        if Batch then
            Result := MakeRequest(httpGET, 'https://api.thesmsworks.co.uk/v1/batch/' + ID, AsyncReq)
        else
            Result := MakeRequest(httpGET, 'https://api.thesmsworks.co.uk/v1/messages/' + ID, AsyncReq);
    end
    else begin
        FLastError := 'Unknown Provider';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsSms.CheckCredit( AsyncReq: Boolean = True): Boolean;
begin
    Result := False;
    FLastError := '';
    if FSmsProvider = SmsProvKapow then begin
        HttpRest.RestParams.Clear;
        FSmsOperation := SmsOpCredit;
        Result := MakeRequest(httpPOST, 'https://secure.kapow.co.uk/scripts/chk_credit.php', AsyncReq);
    end
    else if FSmsProvider = SmsProvSmsWorks then begin
        HttpRest.RestParams.Clear;
        FSmsOperation := SmsOpCredit;
        Result := MakeRequest(httpGET, 'https://api.thesmsworks.co.uk/v1/credits/balance', AsyncReq);
    end
    else begin
        FLastError := 'Unknown Provider';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsSms.SmsRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
var
    S: String;
    J: Integer;
begin
    if ErrCode <> 0 then begin
        if ErrCode = 401 then
            FLastError := 'Failed: Account Not Authorised'   { V8.65 }
        else
            FLastError := 'Failed, error #' + IntToStr(ErrCode) +
              '. Status ' + IntToStr(HttpRest.StatusCode) + ' - ' + HttpRest.ReasonPhrase;
        if Assigned(FOnSmsDone) then FOnSmsDone(Self);
        Exit;
    end;
    if FSmsProvider = SmsProvKapow then begin

      // Kapow returns simple text, no formatting or tags or line end
        if (HttpRest.StatusCode = 200) then begin
            FLastResp := HttpRest.ResponseRaw;
            if FLastResp = 'ERROR' then
                FLastError := 'Failed: Kapow Reports an Error'
            else if FLastResp = 'USERPASS' then
                FLastError := 'Failed: Kapow Reports Invalid Account Details'
            else if FLastResp = 'NOCREDIT' then
                FLastError := 'Failed: Kapow Reports No Account Credit'
            else begin
                if FSmsOperation = SmsOpCredit then begin
                    FCredits := FLastResp;
                    FLastError := '';
                end
                else if FSmsOperation = SmsOpSend then begin
                    if  Pos ('OK', FLastResp) = 1 then begin // OK 148 11472734895956042
                        FLastError := '';
                        S := Trim (Copy (FLastResp, 4, 999));
                        J := Pos (IcsSpace, S);
                        if J > 0 then begin
                            FCredits := Copy (S, 1, Pred (J));
                            FSentID := Copy (S, Succ (J), 999);
                        end ;
                    end;
                end
                else if FSmsOperation = SmsOpCheck then begin
                    if FLastResp = 'D' then begin
                        FDelivery := 'SMS Delivered OK';
                        FLastError := '';
                    end
                    else if FLastResp = 'N' then
                        FDelivery := 'Message Awaiting Delivery'
                    else if FLastResp = 'S' then
                        FDelivery := 'Sent to SMSC'
                    else if FLastResp = 'B' then
                        FDelivery := 'Message Buffered Awaiting Delivery'
                    else if FLastResp = 'R' then
                        FDelivery := 'Retrying Message'
                    else if FLastResp = 'X' then
                        FDelivery := 'Message Delivery Failed'
                    else
                        FDelivery := 'Unknown Delivery: ' + FLastResp;
                end
                else
                    FLastError := 'Failed: Unexpected Kapow Response - ' + FLastResp;
            end
        end
        else
            FLastError := 'Failed: Status ' + IntToStr(HttpRest.StatusCode) + ' - ' +
                                                              HttpRest.ReasonPhrase;
    end
    else if FSmsProvider = SmsProvSmsWorks then begin

      // The SMS Works returns Json
        if (HttpRest.StatusCode = 201) then begin
            FLastResp := HttpRest.ResponseRaw;
            if FSmsOperation = SmsOpSend then begin
                FSentID := HttpRest.ResponseJson.S['messageid'];
                FCredits := HttpRest.ResponseJson.S['credits'];
                FDelivery := HttpRest.ResponseJson.S['status'];
                if FSentID = '' then
                    FSentID := HttpRest.ResponseJson.S['batchid'];  // should really keep separately !!
                FLastError := '';
            end;
        end
        else if (HttpRest.StatusCode = 200) then begin
          // ignore response getting token, no event
            if (Pos ('auth/token', HttpRest.URL) > 0) then Exit;
            if FSmsOperation = SmsOpCredit then begin
                FCredits := HttpRest.ResponseJson.S['credits'];
                FLastError := '';
            end
            else if FSmsOperation = SmsOpCheck then begin
                FCredits := HttpRest.ResponseJson.S['credits'];
                FDelivery := HttpRest.ResponseJson.S['status'];
                FLastError := '';
             // pending check batch response, array for each message
            end;
        end
        else if (HttpRest.StatusCode >= 400) then begin
            FLastError := 'Failed: SMS Works Account Not Authorised';   { V8.65 }
            FLastResp := HttpRest.ResponseRaw;
            if (Length(FLastResp) > 10) and Assigned(HttpRest.ResponseJson) then
                FLastError := HttpRest.ResponseJson.S['message'];
        end
        else
            FLastError := 'Failed: Status ' + IntToStr(HttpRest.StatusCode) + ' - ' +
                                                              HttpRest.ReasonPhrase;
        if (FDelivery = 'DELIVERED') or  (FDelivery = 'SENT') then begin   { V8.63 same responses as Kapow }
            FDelivery := 'SMS Delivered OK';
            FLastError := '';
        end
        else if (FDelivery = 'REJECTED') or (FDelivery = 'UNDELIVERABLE') then
            FDelivery := 'Message Delivery Failed';

     end;
    if Assigned(FOnSmsDone) then FOnSmsDone(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsInetAlive }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsInetAlive.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    HttpRest := TSslHttpRest.Create(self);
    HttpRest.OnHttpRestProg := AliveRestProg;
    HttpRest.OnRestRequestDone := AliveRestRequestDone;
    FDebugLevel := DebugNone;
    FTaskTimer := TIcsTimer.Create(HttpRest);
    FTaskTimer.OnTimer := TaskOnTimer;
    FTaskTimer.Interval := TicksPerSecond*5;
    Stop;   // reset stuff
    FAliveMethod := AliveMethHttp;
    FAliveNets :=  AliveNetv4;
// used for Windows 7 to Windows 10 RTM, hosted by CDN
//    FHostIPv4 :=  'www.msftncsi.com';
//    FHostIPv6 := 'ipv6.msftncsi.com';
//    FHttpPage := '/ncsi.txt';
//    FHttpText := 'Microsoft NCSI';
// used for Windows 10 1607 and later, hosted by Microsoft
    FHostIPv4 := 'www.msftconnecttest.com';
    FHostIPv6 := 'ipv6.msftconnecttest.com';
    FHttpPage := '/connecttest.txt';
    FHttpText := 'Microsoft Connect Test';
    FLocalIPv4 := ICS_ANY_HOST_V4;
    FLocalIPv6 := ICS_ANY_HOST_V6 ;
    FAutoStart := False;
    FOnlineSecs := 60;
    FOfflineSecs := 5;
    FFailedWaitSecs := FOfflineSecs*6; // adds OnlineSecs so offline 90 secs after last OK and six failures
    FPingHops := 25;     { V8.67 was 15, not enough }
    FPingMaxSecs := 4;
    FPingCheckAddr := True;
    FTaskTimer.Enabled := True;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsInetAlive.Destroy;
begin
    FTaskTimer.Enabled := False;
    FreeAndNil(FTaskTimer);
    FreeAndNil(HttpRest);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.Start;
begin
    FRunning := True;
    if FOnlineSecs < 10 then FOnlineSecs := 10;
    if FOfflineSecs < 5 then FOfflineSecs := 5;
    if FFailedWaitSecs < 5 then FFailedWaitSecs := 15;
    if FHttpPage = '' then FHttpPage := '/';
    if FHttpPage [1] <> '/' then FHttpPage := '/' + FHttpPage;
    FAliveIPv4 := AliveStatNone;
    FAliveIPv6 := AliveStatNone;
    FLastTickIPv4 := 0;
    FLastTickIPv6 := 0;
    FLastDTIPv4 := 0;
    FLastDTIPv6 := 0;
    FHttpBusy := False;
    FPingBusy := False;
    CheckNow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.CheckNow;
begin
    if NOT FRunning then Exit;
    if (FAliveNets in [AliveNetv4, AliveNetBoth]) then
        StartCheck(4);    // set IPv4 trigger
    if IsIPv6Available and (FAliveNets in [AliveNetv6, AliveNetBoth]) then
        FTrgIPv6 := IcsGetTrgSecs64(2);
    if NOT FTaskTimer.Enabled then
        FTaskTimer.Enabled := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.Stop;
begin
    FRunning := False;
    FTaskTimer.Enabled := False;
    FTrgIPv4 := Trigger64Disabled;
    FTrgIPv6 := Trigger64Disabled;
    FAliveIPv4 := AliveStatNone;
    FAliveIPv6 := AliveStatNone;
    if HttpRest.Connected then HttpRest.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.AliveRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if NOT FRunning then Exit;
    if Assigned(FOnAliveProg) then
        FOnAliveProg(Self, LogOption, Msg) ;
end;


// combines IPv4 and IPv6 online results, not online until both tested and online
function TIcsInetAlive.TestBothOnline: TAliveStatus;
begin
    if FAliveNets = AliveNetv4 then
        Result := FAliveIPv4
    else if FAliveNets = AliveNetv6 then
        Result := FAliveIPv6
    else begin
        if (FAliveIPv4 = FAliveIPv6) then
            Result := FAliveIPv4
        else if (FAliveIPv4 = AliveStatNone) or (FAliveIPv6 = AliveStatNone) then
            Result := AliveStatNone    // wait for both results
         else
            Result := AliveStatOffline;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.SetOnline(Online: Boolean; IpType: Integer);
var
    OldOnline: TAliveStatus;
    S: String;
begin
    if IpType = 4 then begin
        OldOnline := FAliveIPv4;
        if Online then begin
            FAliveIPv4 := AliveStatOnline;
            FLastTickIPv4 := IcsGetTickCount64;
            FLastDTIPv4 := Now;
        end
        else begin
            FTrgIPv4 := IcsGetTrgSecs64(FOfflineSecs);  // faster check offline
            if FAliveIPv4 = AliveStatOnline then begin  // about to go offline
                if (IcsElapsedSecs64(FLastTickIPv4) > (FFailedWaitSecs + FOnlineSecs)) then
                    FAliveIPv4 := AliveStatOffline;
            end;
            if FAliveIPv4 = AliveStatNone then     // first test
                FAliveIPv4 := AliveStatOffline;
        end;
        if (OldOnline <> FAliveIPv4) then begin
            if FAliveIPv4 = AliveStatOnline then
                S := 'Online'
            else
                S := 'Offline';
            AliveRestProg(Self, loProtSpecInfo, 'IPv4 Connectivity ' + S);
            if Assigned(FOnAliveChange) then FOnAliveChange(Self);
        end;
    end
    else if IpType = 6 then begin
        OldOnline := FAliveIPv6;
        if Online then begin
            FAliveIPv6 := AliveStatOnline;
            FLastTickIPv6 := IcsGetTickCount64;
            FLastDTIPv6 := Now;
        end
        else begin
            FTrgIPv6 := IcsGetTrgSecs64(FOfflineSecs);  // faster check offline
            if FAliveIPv6 = AliveStatOnline then begin  // about to go offline
                if (IcsElapsedSecs64(FLastTickIPv4) > (FFailedWaitSecs + FOnlineSecs)) then
                    FAliveIPv6 := AliveStatOffline;
            end;
            if FAliveIPv6 = AliveStatNone then     // first test
                FAliveIPv6 := AliveStatOffline;
        end;
        if (OldOnline <> FAliveIPv6) then begin
            if FAliveIPv6 = AliveStatOnline then
                S := 'Online'
            else
                S := 'Offline';
            AliveRestProg(Self, loProtSpecInfo, 'IPv6 Connectivity ' + S);
            if Assigned(FOnAliveChange) then FOnAliveChange(Self);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.AliveRestRequestDone(Sender: TObject; RqType: THttpRequest; ErrCode: Word);
begin
    FHttpBusy := False;
    if NOT FRunning then Exit;
    FHttpRTT := IcsElapsedMsecs64(FStartTick);
    if (ErrCode <> 0) or (HttpRest.StatusCode <> 200) then begin
        ErrCode := 98;
        AliveRestProg(Self, loProtSpecErr, 'Check Alive Request to ' +
            HttpRest.URL + ' failed, error #' + IntToStr(ErrCode) +
              '. Status = ' + IntToStr(HttpRest.StatusCode) +
                 ' - ' + HttpRest.ReasonPhrase);
    end
    else if (FHttpText <> '') then begin
        if (Pos(FHttpText, Trim(HttpRest.FResponseRaw)) = 0) then begin
            ErrCode := 99;
            AliveRestProg(Self, loProtSpecErr, 'Check Alive Request to ' +
                HttpRest.URL + ' failed, mimatched response: ' +
                                        Copy(HttpRest.FResponseRaw, 1, 24));
        end;
    end;
    if (ErrCode = 0) then
        AliveRestProg(Self, loProtSpecInfo, 'Check Alive to ' + HttpRest.URL +
             ' (' + IcsFmtIpv6Addr(HttpRest.AddrResolvedStr) + ') took ' + IntToStr(FHttpRTT) + ' msecs') ;
    SetOnline((ErrCode=0), HttpRest.Tag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.StartHttp(IpType: Integer);
var
    URL: string;
    StatCode: Integer;
begin
    FPingBusy := False;
    if IpType = 4 then begin
        URL := 'http://' + FHostIPv4 + FHttpPage;
        HttpRest.SocketFamily := sfIPv4;
        HttpRest.LocalAddr := FLocalIPv4;
    end
    else begin
        URL := 'http://' + FHostIPv6 + FHttpPage;
        HttpRest.SocketFamily := sfIPv6;
        HttpRest.LocalAddr6 := FLocalIPv6;
    end;
    HttpRest.Tag := IpType;
    HttpRest.DebugLevel := FDebugLevel;
    HttpRest.ProxyURL := FHttpProxy;
    HttpRest.Connection := 'Close';   // no keep-alive
    FStartTick := IcsGetTickCount64;
    FHttpRTT := 0;
    StatCode := HttpRest.RestRequest(httpGET, URL, True, '');   // async request
    if (StatCode = 0) then
        FHttpBusy := True
    else
        AliveRestProg(Self, loProtSpecErr, 'Check Alive Request to ' +
            HttpRest.URL + ' failed to Start, Code ' + IntToStr(StatCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.PingTerminated(Sender: TObject);
var
    S: String;
begin
    FPingBusy := False;
    if NOT FRunning then Exit;
    try
        with Sender as TPingThread do
        begin
        // see if ping reached correct host, needs a high TTL
            if (ErrCode = 0) and FPingCheckAddr then
            begin
                if DnsHostIP <> ReplyIPAddr then
                begin
                    S:= 'Ping Did Not Reach Host ';
                    AliveRestProg(Self, loProtSpecErr, S + PingHostName + ', Request IP ' +
                               DnsHostIP + ', Reply IP ' + ReplyIPAddr + ' (' + ReplyHostName + ')') ;
                    ErrCode := 99 ;
                    ErrString := 'Ping Did Not Reach Host' ;
                end ;
            end ;
            if ErrCode <> 0 then begin
                AliveRestProg(Self, loProtSpecErr, 'Ping Failed to Host ' + PingHostName + ', Request IP ' +
                               DnsHostIP + ': ' + ErrString);
                SetOnline(False, PingId)
            end
            else begin
                AliveRestProg(Self, loProtSpecInfo, 'Check Alive Ping to ' + PingHostName +
                        ' (' + IcsFmtIpv6Addr(DnsHostIP) + ') took ' + IntToStr(ReplyRTT) + ' msecs') ;
                if FAliveMethod = AliveMethPing then
                    SetOnline(True, PingId)
                else
                    StartHttp(PingId);
            end;
        end;
    except
        on E:Exception do begin
            AliveRestProg(Self, loProtSpecErr, 'Ping Terminated Exception: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.StartCheck(IpType: Integer);

    procedure StartPing(IpType: Integer);
    begin
        with TPingThread.Create (True) do   // create suspended
        begin
            FreeOnTerminate := True;
            PingId := IpType;
            OnTerminate := PingTerminated;
            if IpType = 4 then begin
                PingHostName := FHostIPv4;
                PingSocketFamily := sfIPv4;
                PingSrcAddress := FLocalIPv4;
            end
            else begin
                PingHostName := FHostIPv6;
                PingSocketFamily := sfIPv6;
                PingSrcAddress6 := FLocalIPv6;
            end;
            PingTimeout := FPingMaxSecs * Integer (TicksPerSecond) ;
            PingTTL := FPingHops;
            PingLookupReply := FPingCheckAddr;
        {$IFDEF COMPILER14_UP}
            Start;
        {$ELSE}
            Resume;
        {$ENDIF}
            FPingBusy := True;
        end ;
    end;

begin
    try
        if FHttpBusy then Exit;
        if FPingBusy then Exit;
        if (IpType = 4) then begin
            if FAliveIPv4 = AliveStatOnline then
                FTrgIPv4 := IcsGetTrgSecs64(FOnlineSecs)
            else
                FTrgIPv4 := IcsGetTrgSecs64(FOfflineSecs);
            if FAliveMethod in [AliveMethPing, AliveMethBoth] then
                StartPing(4)
            else if FAliveMethod in [AliveMethHttp] then
                StartHttp(4);
        end
        else if (IpType = 6) then begin
            if FAliveIPv6 = AliveStatOnline then
                FTrgIPv6 := IcsGetTrgSecs64(FOnlineSecs)
            else
                FTrgIPv6 := IcsGetTrgSecs64(FOfflineSecs);
            if FAliveMethod in [AliveMethPing, AliveMethBoth] then
                StartPing(6)
            else if FAliveMethod in [AliveMethHttp] then
                StartHttp(6);
        end
        else
            AliveRestProg(Self, loProtSpecErr, 'Unknown IP Type');
    except
        on E:Exception do begin
            AliveRestProg(Self, loProtSpecErr, 'Check Alive Timer Exception: ' + E.Message);
            FHttpBusy := False;
            FPingBusy := False;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsInetAlive.TaskOnTimer(Sender: TObject);

begin
    if(NOT FRunning) then begin
        if FAutoStart then begin
            FAutoStart := False;
            Start;
         end
         else
            FTaskTimer.Enabled := False;
         Exit;
    end;
    if IcsTestTrgTick64(FTrgIPv4) then begin
        StartCheck(4);
     end;
    if IcsTestTrgTick64(FTrgIPv6) then begin
        StartCheck(6);
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
