{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     November 23, 1997
Version:      1.29
Description:  THttpCli is an implementation for the HTTP protocol
              RFC 1945 (V1.0), RFC 2068 (V1.1)
Credit:       This component was based on a freeware from by Andreas
              Hoerstemeier and used with his permission.
              andy@hoerstemeier.de http://www.westend.de/~hoerstemeier
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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

Quick Reference:
HTTP component can retrieve documents or files using HTTP protocol; that is
connect to a HTTP server also known as a webserver. It can also trigger a
CGI/ISAPI/NSAPI script and post data using either GET or POST method.
Syntax of an URL: protocol://[user[:password]@]server[:port]/path
Path can include data: question mark followed by URL encoded data.
HTTP component is either asynchonous (non-blocking) or synchonous (blocking).
Highest performance is when using asynchonous operation. This is the
recommended way to use HTTP component.
To request several URL simultaneously, use asynchronous operation and as much
HTTP components as you wants to request URLs. All requests will be executed
simultaneously without using multi-threading and without blocking your app.
Methods:
    GetASync    Asynchronous, non-blocking Get
                Retrieve document or file specified by URL, without blocking.
                OnRequestDone event trigered when finished. Use HTTP GET
                method (data contained in URL)
    PostASync   Asynchronous, non-blocking Post
                Retrieve document or file specified by URL, without blocking.
                OnRequestDone event trigered when finished. Use HTTP POST
                method (data contained in request stream)
    HeadASync   Asynchronous, non-blocking Head
                Retrieve document or file header specified by URL, without
                blocking. OnRequestDone event trigered when finished. Use HTTP
                HEAD method.
    Get         Synchronous, blocking Get. Same as GetAsync, but blocks until
                finished.
    Post        Synchronous, blocking Post. Same as PostAsync, but blocks until
                finished.
    Head        Synchronous, blocking Head. Same as HeadAsync, but blocks until
                finished.
    Abort       Immediately close communication.

Updates:
11/29/97 RcvdStream and SendStream properties moved to public section
11/30/97 Document name bug corrected
12/02/97 Removed bug occuring with terminating slash in docname
12/03/97 Added properties RcvdCount and SentCount to easily add a progress
         bar feature (On receive, the ContentLength is initialized with the
         value from the header. Update the progress bar in the OnDocData event,
         or the OnSendData event).
         Added the OnSendBegin, OnSendData and OnSendEnd events.
12/07/97 Corrected Head function to work as expected. Thanks to
         R. Barry Jones <rbjones@therightside.demon.co.uk
29/12/97 V0.96 Added ModifiedSince property as followinf proposition made by
         Aw Kong Koy" <infomap@tm.net.my>.
30/12/97 V0.97 Added a Cookie property to send cookies
11/01/98 V0.98 Added WSocket read-only property which enable to access the
         socket component used internally. For example to close it to abort
         a connection.
13/01/98 V0.99 Added MultiThreaaded property to tell the component that it is
         working in a thread and should take care of it.
15/01/98 V1.00 Completely revised internal working to make it work properly
         with winsock 2. The TimeOut property is gone.
         Changed OnAnswerLine event to OnHeaderData to be more consistent.
         Replaced AnswserLine property by readonly LastResponse property.
         Added OnRequestDone event. Added GetAsync, PostAsync, HeadAsync
         asynchronous, non-blocking methods. Added Abort procedure.
16/01/98 V1.01 Corrected a bug which let some data be lost when receiving
         (thanks to  Fulvio J. Castelli <fulvio@rocketship.com>)
         Added test for HTTP/1.1 response in header.
31/01/98 V1.02 Added an intermediate message posting for the OnRequestDone
         event. Thanks to Ed Hochman <ed@mbhsys.com> for his help.
         Added an intermediate PostMessage to set the component to ready state.
04/02/98 V1.03 Added some code to better handle DocName (truncating at the
         first question mark).
05/02/98 V1.04 Deferred login after a relocation, using WM_HTTP_LOGIN message.
         Added workarounf to support faulty webservers which sent only a single
         LF in header lines. Submitted by Alwin Hoogerdijk <alwin@lostboys.nl>
15/03/98 V1.05 Enlarge buffers from 2048 to 8192 bytes (not for D1)
01/04/98 V1.06 Adapted for BCB V3
13/04/98 V1.07 Made RcvdHeader property readonly and cleared the content at the
         start of a request.
         Protected Abort method from calling when component is ready.
         Ignore any exception triggered by CancelDnsLookup in Abort method.
14/04/98 V1.08 Corrected a relocation bug occuring with relative path
26/04/98 V1.09 Added OnLocationChange event
30/04/98 V1.10 Added ProxyUsername and ProxyPassword. Suggested by
         Myers, Mike <MikeMy@crt.com>.
26/05/98 V1.11 Corrected relocation problem when used with ASP webpages
09/07/98 V1.12 Adapted for Delphi 4
         Checked argument length in SendCommand
19/09/98 V1.13 Added support for HTML document without header
         Added OnSessionConnected event, httpConnected state and
         httpDnsLookupDone state.
         Corrected a problem with automatic relocation. The relocation
         message was included in data, resulting in wrong document data.
         Added two new events: OnRequestHeaderBegin and OnRequestHeaderEnd.
         They replace the OnHeaderBegin and OnHeaderEnd events that where
         called for both request header (to web server) and response
         header (from web server)
22/11/98 V1.14 Added a Location property than gives the new location in
         case of page relocation. Suggested by Jon Robertson <touri@pobox.com>
21/12/98 V1.15 Set ContentLength equal to -1 at start of command.
31/01/99 V1.16 Added HostName property
01/02/99 V1.17 Port was lost in DoRequestAsync when using a proxy.
         Thanks to David Wright <wrightd@gamespy.com> for his help.
         Report Dns lookup error and session connect error in OnrequestDOne
         event handler as suggested by Jack Olivera <jack@token.nl>.
14/03/99 V1.18 Added OnCookie event.
16/03/99 V1.19 Added Accept property.
               Added a default value to Agent property.
               Changed OnCookie event signature (not fully implemented yet !).
07/05/99 V1.20 Added code to support Content Ranges by Jon Robertson
               <touri@pobox.com>.
24/07/99 V1.21 Yet another change in relocation code.
Aug 20, 1999  V1.22 Changed conditional compilation so that default is same
              as latest compiler (currently Delphi 4, Bcb 4). Should be ok for
              Delphi 5. Added Sleep(0) in sync wait loop to reduce CPU usage.
              Added DnsResult property as suggested by Heedong Lim
              <hdlim@dcenlp.chungbuk.ac.kr>. This property is accessible from
              OnStateChange when state is httpDnsLookupDone.
              Triggered OnDocData after writing to the stream.
Sep 25, 1999  V1.23 Yet another change in relocation code when using proxy
              Francois Demers <fdemers@videotron.ca> found that some webserver
              do not insert a space after colon in header line. Corrected
              code to handle it correctly.
              Cleared ContentType before issuing request.
Oct 02, 1999  V1.24 added AcceptRanges property. Thanks to Werner Lehmann
              <wl@bwl.uni-kiel.de>
Oct 30, 1999  V1.25 change parameter in OnCommand event from const to var to
              allow changing header line, including deleting or adding before
              or after a given line sent by the component.
Nov 26, 1999  V1.26 Yet another relocation fix !
Jun 23, 2000  V1.27 Fixed a bug in ParseURL where hostname is followed by a '?'
              (that is no path but a query).
Jul 22, 2000  V1.28 Handle exception during DnsLookup from the login procedure.
              Suggested by Robert Penz <robert.penz@outertech.com>
Sep 17, 2000  V4.29 Eugene Mayevski <Mayevski@eldos.org> added support for
              NOFORMS.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpProt;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

uses
    WinProcs, WinTypes, Messages, SysUtils, Classes,
    {$IFNDEF NOFORMS}
    Forms, Controls,
    {$ENDIF}
    WSocket;

const
    HttpCliVersion     = 129;
    CopyRight : String = ' THttpCli (c) 1997-2000 F. Piette V1.29 ';
    DefaultProxyPort   = '80';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    HTTP_RCV_BUF_SIZE = 255;
    HTTP_SND_BUF_SIZE = 8193;
{$ELSE}
    HTTP_RCV_BUF_SIZE = 8193;
    HTTP_SND_BUF_SIZE = 8193;
{$ENDIF}
    WM_HTTP_REQUEST_DONE = WM_USER + 1;
    WM_HTTP_SET_READY    = WM_USER + 2;
    WM_HTTP_LOGIN        = WM_USER + 3;
    httperrNoError  = 0;
    httperrBusy     = 1;
    httperrNoData   = 2;
    httperrAborted  = 3;
    httperrOverflow = 4;

type
    EHttpException = class(Exception)
        ErrorCode : Word;
        constructor Create(const Msg : String; ErrCode : Word);
    end;

    THttpEncoding    = (encUUEncode, encBase64, encMime);
    THttpRequest     = (httpAbort, httpGET, httpPOST, httpHEAD);
    THttpState       = (httpReady,         httpNotConnected, httpConnected,
                        httpDnsLookup,     httpDnsLookupDone,
                        httpWaitingHeader, httpWaitingBody,  httpAborting);

    TOnCommand       = procedure (Sender : TObject;
                                  var S: String) of object;
    TDocDataEvent    = procedure (Sender : TObject;
                                  Buffer : Pointer;
                                  Len    : Integer) of object;
    TCookieRcvdEvent = procedure (Sender       : TObject;
                                  const Data   : String;
                                  var   Accept : Boolean) of object;
    THttpRequestDone = procedure (Sender : TObject;
                                  RqType : THttpRequest;
                                  Error  : Word) of object;

    THttpCli = class(TComponent)
    protected
        FWSocket          : TWSocket;
        FWindowHandle     : HWND;
        FMultiThreaded    : Boolean;
        FState            : THttpState;
        FHostName         : String;
        FTargetHost       : String;
        FPort             : String;
        FProxy            : String;
        FProxyPort        : String;
        FUsername         : String;
        FPassword         : String;
        FProxyUsername    : String;
        FProxyPassword    : String;
        FLocation         : String;
        FConnected        : Boolean;
        FDnsResult        : String;
        FSendBuffer       : array [0..HTTP_SND_BUF_SIZE - 1] of char;
        FRequestType      : THttpRequest;
        FReceiveBuffer    : array [0..HTTP_RCV_BUF_SIZE - 1] of char;
        FReceiveLen       : Integer;
        FLastResponse     : String;
        FHeaderLineCount  : Integer;
        FBodyLineCount    : Integer;
        FAllowedToSend    : Boolean;
        FURL              : String;
        FPath             : String;
        FDocName          : String;
        FSender           : String;
        FReference        : String;
        FAgent            : String;
        FAccept           : String;
        FModifiedSince    : TDateTime;       { Warning ! Use GMT date/Time }
        FNoCache          : Boolean;
        FStatusCode       : Integer;
        FReasonPhrase     : String;
        FContentLength    : LongInt;
        FContentType      : String;
        FDoAuthor         : TStringList;
        FContentPost      : String;
        FContentRangeBegin: String;   {JMR!! Added this line!!!}
        FContentRangeEnd  : String;   {JMR!! Added this line!!!}
        FAcceptRanges     : String;
        FCookie           : String;
        FLocationFlag     : Boolean;
        FRcvdHeader       : TStrings;
        FRcvdStream       : TStream;   { If assigned, will received the answer }
        FRcvdCount        : LongInt;   { Number of received bytes for the body }
        FSentCount        : LongInt;
        FSendStream       : TStream;   { Contains the data to send }
        FReqStream        : TMemoryStream;
        FRequestDoneError : Integer;
        FNext             : procedure of object;
        FOnStateChange    : TNotifyEvent;
        FOnSessionConnected   : TNotifyEvent;
        FOnRequestHeaderBegin : TNotifyEvent;
        FOnRequestHeaderEnd   : TNotifyEvent;
        FOnHeaderBegin    : TNotifyEvent;
        FOnHeaderEnd      : TNotifyEvent;
        FOnHeaderData     : TNotifyEvent;
        FOnDocBegin       : TNotifyEvent;
        FOnDocEnd         : TNotifyEvent;
        FOnDocData        : TDocDataEvent;
        FOnSendBegin      : TNotifyEvent;
        FOnSendEnd        : TNotifyEvent;
        FOnSendData       : TDocDataEvent;
        FOnTrace          : TNotifyEvent;
        FOnCommand        : TOnCommand;
        FOnCookie         : TCookieRcvdEvent;
        FOnDataAvailable  : TDataAvailable;
        FOnRequestDone    : THttpRequestDone;
        FOnLocationChange : TNotifyEvent;

        procedure SendRequest(const method,Version: String);
        procedure GetHeaderLineNext;
        procedure GetBodyLineNext;
        procedure SendCommand(const Cmd : String); virtual;
        procedure Login; virtual;
        procedure Logout; virtual;
        procedure SocketDNSLookupDone(Sender: TObject; Error: Word);
        procedure SocketSessionClosed(Sender: TObject; Error: Word);
        procedure SocketSessionConnected(Sender : TObject; Error : Word);
        procedure SocketDataSent(Sender : TObject; Error : Word);
        procedure SocketDataAvailable(Sender: TObject; Error: Word);
        procedure LocationSessionClosed(Sender: TObject; Error: Word); virtual;
        procedure DoRequestAsync(Rq : THttpRequest);
        procedure DoRequestSync(Rq : THttpRequest);
        procedure SetMultiThreaded(newValue : Boolean);
        procedure StateChange(NewState : THttpState);
        procedure TriggerStateChange;
        procedure TriggerCookie(const Data : String;
                                var   bAccept : Boolean); virtual;
        procedure TriggerSessionConnected; virtual;
        procedure TriggerRequestHeaderBegin; virtual;
        procedure TriggerRequestHeaderEnd; virtual;
        procedure TriggerHeaderBegin; virtual;
        procedure TriggerHeaderEnd; virtual;
        procedure TriggerDocBegin; virtual;
        procedure TriggerDocData(Data : Pointer; Len : Integer); virtual;
        procedure TriggerDocEnd; virtual;
        procedure TriggerSendBegin; virtual;
        procedure TriggerSendData(Data : Pointer; Len : Integer); virtual;
        procedure TriggerSendEnd; virtual;
        procedure TriggerRequestDone;
        procedure WndProc(var MsgRec: TMessage);
        procedure SetReady;
    	function  HTTPCliAllocateHWnd(Method: TWndMethod): HWND; virtual;
    	procedure HTTPCliDeallocateHWnd(WHandle: HWND); virtual;
        procedure WMHttpRequestDone(var msg: TMessage);
                  message WM_HTTP_REQUEST_DONE;
        procedure WMHttpSetReady(var msg: TMessage);
                  message WM_HTTP_SET_READY;
        procedure WMHttpLogin(var msg: TMessage);
                  message WM_HTTP_LOGIN;
    public
        constructor Create(Aowner:TComponent); override;
        destructor  Destroy; override;
        procedure   Get;       { Synchronous blocking Get        }
        procedure   Post;      { Synchronous blocking Post       }
        procedure   Head;      { Synchronous blocking Head       }
        procedure   GetASync;  { Asynchronous, non-blocking Get  }
        procedure   PostASync; { Asynchronous, non-blocking Post }
        procedure   HeadASync; { Asynchronous, non-blocking Head }
        procedure   Abort;

        property WSocket         : TWSocket          read  FWSocket;
        property Handle          : HWND              read  FWindowHandle;
        property State           : THttpState        read  FState;
        property LastResponse    : String            read  FLastResponse;
        property ContentLength   : LongInt           read  FContentLength;
        property ContentType     : String            read  FContentType;
        property RcvdCount       : LongInt           read  FRcvdCount;
        property SentCount       : LongInt           read  FSentCount;
        property StatusCode      : Integer           read  FStatusCode;
        property ReasonPhrase    : String            read  FReasonPhrase;
        property DnsResult       : String            read  FDnsResult;
        property AuthorizationRequest : TStringList  read  FDoAuthor;
        property DocName              : String       read  FDocName;
        property Location             : String       read  FLocation
                                                     write FLocation;
        property RcvdStream           : TStream      read  FRcvdStream
                                                     write FRcvdStream;
        property SendStream           : TStream      read  FSendStream
                                                     write FSendStream;
        property RcvdHeader           : TStrings     read  FRcvdHeader;
        property Hostname             : String       read  FHostname;
    published
        property URL             : String            read  FURL
                                                     write FURL;
        property Proxy           : String            read  FProxy
                                                     write FProxy;
        property ProxyPort       : String            read  FProxyPort
                                                     write FProxyPort;
        property Sender          : String            read  FSender
                                                     write FSender;
        property Agent           : String            read  FAgent
                                                     write FAgent;
        property Accept          : String            read  FAccept
                                                     write FAccept;
        property Reference       : String            read  FReference
                                                     write FReference;
        property Username        : String            read  FUsername
                                                     write FUsername;
        property Password        : String            read  FPassword
                                                     write FPassword;
        property ProxyUsername   : String            read  FProxyUsername
                                                     write FProxyUsername;
        property ProxyPassword   : String            read  FProxyPassword
                                                     write FProxyPassword;
        property NoCache         : Boolean           read  FNoCache
                                                     write FNoCache;
        property ModifiedSince   : TDateTime         read  FModifiedSince
                                                     write FModifiedSince;
        property Cookie          : String            read  FCookie
                                                     write FCookie;
        property ContentTypePost : String            read  FContentPost
                                                     write FContentPost;
        property ContentRangeBegin: String           read  FContentRangeBegin  {JMR!! Added this line!!!}
                                                     write FContentRangeBegin; {JMR!! Added this line!!!}
        property ContentRangeEnd  : String           read  FContentRangeEnd    {JMR!! Added this line!!!}
                                                     write FContentRangeEnd;   {JMR!! Added this line!!!}
        property AcceptRanges     : String           read  FAcceptRanges;
        property MultiThreaded   : Boolean           read  FMultiThreaded
                                                     write SetMultiThreaded;
        property OnTrace         : TNotifyEvent      read  FOnTrace
                                                     write FOnTrace;
        property OnSessionConnected : TNotifyEvent   read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnHeaderData    : TNotifyEvent      read  FOnHeaderData
                                                     write FOnHeaderData;
        property OnCommand       : TOnCommand        read  FOnCommand
                                                     write FOnCommand;
        property OnHeaderBegin   : TNotifyEvent      read  FOnHeaderBegin
                                                     write FOnHeaderBegin;
        property OnHeaderEnd     : TNotifyEvent      read  FOnHeaderEnd
                                                     write FOnHeaderEnd;
        property OnRequestHeaderBegin : TNotifyEvent read  FOnRequestHeaderBegin
                                                     write FOnRequestHeaderBegin;
        property OnRequestHeaderEnd   : TNotifyEvent read  FOnRequestHeaderEnd
                                                     write FOnRequestHeaderEnd;
        property OnDocBegin      : TNotifyEvent      read  FOnDocBegin
                                                     write FOnDocBegin;
        property OnDocData       : TDocDataEvent     read  FOnDocData
                                                     write FOnDocData;
        property OnDocEnd        : TNotifyEvent      read  FOnDocEnd
                                                     write FOnDocEnd;
        property OnSendBegin     : TNotifyEvent      read  FOnSendBegin
                                                     write FOnSendBegin;
        property OnSendData      : TDocDataEvent     read  FOnSendData
                                                     write FOnSendData;
        property OnSendEnd       : TNotifyEvent      read  FOnSendEnd
                                                     write FOnSendEnd;
        property OnStateChange   : TNotifyEvent      read  FOnStateChange
                                                     write FOnStateChange;
        property OnRequestDone   : THttpRequestDone  read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnLocationChange : TNotifyEvent     read  FOnLocationChange
                                                     write FOnLocationChange;
        property OnCookie         : TCookieRcvdEvent read  FOnCookie
                                                     write FOnCookie;
    end;

procedure Register;
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path }
procedure ParseURL(const URL : String;
                   var Proto, User, Pass, Host, Port, Path : String);
function  Posn(const s, t : String; count : Integer) : Integer;
function  EncodeLine(Encoding : THttpEncoding;
                     SrcData : PChar; Size : Integer):String;
function EncodeStr(Encoding : THttpEncoding; const Value : String) : String;
function RFC1123_Date(aDate : TDateTime) : String;


implementation

const
    bin2uue  : String = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
    bin2b64  : String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    uue2bin  : String = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ ';
    b642bin  : String = '~~~~~~~~~~~^~~~_TUVWXYZ[\]~~~|~~~ !"#$%&''()*+,-./0123456789~~~~~~:;<=>?@ABCDEFGHIJKLMNOPQRS';
    linesize = 45;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [THttpCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor EHttpException.Create(const Msg : String; ErrCode : Word);
begin
    Inherited Create(Msg);
    ErrorCode := ErrCode;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We cannot use Delphi own function because the date must be specified in   }
{ english and Delphi use the current language.                              }
function RFC1123_Date(aDate : TDateTime) : String;
const
   StrWeekDay : String = 'MonTueWedThuFriSatSun';
   StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
   Year, Month, Day       : Word;
   Hour, Min,   Sec, MSec : Word;
   DayOfWeek              : Word;
begin
   DecodeDate(aDate, Year, Month, Day);
   DecodeTime(aDate, Hour, Min,   Sec, MSec);
   DayOfWeek := ((Trunc(aDate) - 2) mod 7);
   Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
             Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
                     Year, Hour, Min, Sec]);
end;

{$IFDEF NOFORMS}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function HTTPCliWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    { At window creation asked windows to store a pointer to our object     }
    Obj := TObject(GetWindowLong(ahWnd, 0));

    { If the pointer doesn't represent a TCustomFtpCli, just call the default procedure}
    if not (Obj is THTTPCli) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass parameter to his own kind of   }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        { May be a try/except around next line is needed. Not sure ! }
        THTTPCli(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpCli.HTTPCliAllocateHWnd(Method: TWndMethod) : HWND;
begin
{$IFDEF NOFORMS}
    Result := XSocketAllocateHWnd(Self);
    SetWindowLong(Result, GWL_WNDPROC, LongInt(@HTTPCliWindowProc));
{$ELSE}
     Result := AllocateHWnd(Method);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.HTTPCliDeallocateHWnd(WHandle : HWND);
begin
{$IFDEF NOFORMS}
    XSocketDeallocateHWnd(WHandle);
{$ELSE}
    DeallocateHWnd(WHandle);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpCli.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
    FWindowHandle               := HTTPCliAllocateHWnd(WndProc);
    FWSocket                    := TWSocket.Create(Self);
    FProxyPort                  := DefaultProxyPort;
    FContentPost                := 'application/x-www-form-urlencoded';
    FAccept                     := 'image/gif, image/x-xbitmap, ' +
                                   'image/jpeg, image/pjpeg, */*';
    FAgent                      := 'Mozilla/3.0 (compatible)';
    FDoAuthor                   := TStringlist.Create;
    FWSocket.OnSessionClosed    := SocketSessionClosed;
    FWSocket.OnDataAvailable    := SocketDataAvailable;
    FWSocket.OnSessionConnected := SocketSessionConnected;
    FWSocket.OnDataSent         := SocketDataSent;
    FWSocket.OnDnsLookupDone    := SocketDNSLookupDone;
    FRcvdHeader                 := TStringList.Create;
    FReqStream                  := TMemoryStream.Create;
    FState                      := httpReady;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpCli.Destroy;
begin
    FDoAuthor.Free;
    FWSocket.Free;
    FRcvdHeader.Free;
    FReqStream.Free;
    HTTPCliDeAllocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_HTTP_REQUEST_DONE : WMHttpRequestDone(MsgRec);
         WM_HTTP_SET_READY    : WMHttpSetReady(MsgRec);
         WM_HTTP_LOGIN        : WMHttpLogin(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SetMultiThreaded(newValue : Boolean);
begin
    FMultiThreaded         := newValue;
    FWSocket.MultiThreaded := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SetReady;
begin
    PostMessage(Handle, WM_HTTP_SET_READY, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.StateChange(NewState : THttpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
        if NewState = httpReady then
            TriggerRequestDone;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerCookie(const Data : String; var bAccept : Boolean);
begin
    if Assigned(FOnCookie) then
        FOnCookie(Self, Data, bAccept);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerSessionConnected;
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerDocBegin;
begin
    if Assigned(FOnDocBegin) then
        FOnDocBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerDocEnd;
begin
    if Assigned(FOnDocEnd) then
        FOnDocEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerDocData(Data : Pointer; Len : Integer);
begin
    if Assigned(FOnDocData) then
        FOnDocData(Self, Data, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerSendBegin;
begin
    if Assigned(FOnSendBegin) then
        FOnSendBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerSendEnd;
begin
    if Assigned(FOnSendEnd) then
        FOnSendEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerSendData(Data : Pointer; Len : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, Data, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerHeaderBegin;
begin
    if Assigned(FOnHeaderBegin) then
        FOnHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerHeaderEnd;
begin
    if Assigned(FOnHeaderEnd) then
        FOnHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerRequestHeaderBegin;
begin
    if Assigned(FOnRequestHeaderBegin) then
        FOnRequestHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerRequestHeaderEnd;
begin
    if Assigned(FOnRequestHeaderEnd) then
        FOnRequestHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.TriggerRequestDone;
begin
    PostMessage(Handle, WM_HTTP_REQUEST_DONE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.WMHttpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, FRequestDoneError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.WMHttpSetReady(var msg: TMessage);
begin
    StateChange(httpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ReplaceExt(var FName : String; const newExt : String);
var
    I : Integer;
begin
    I := Posn('.', FName, -1);
    if I <= 0 then
        FName := FName + '.' + newExt
    else
        FName := Copy(FName, 1, I) + newExt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.Abort;
var
    bFlag : Boolean;
begin
    if FState = httpReady then begin
        if FWSocket.State <> wsClosed then
            FWSocket.Close; { This should never occurs ! }
        Exit;
    end;

    bFlag := (FState = httpDnsLookup);
    StateChange(httpAborting);

    if bFlag then begin
        try
            FWSocket.CancelDnsLookup;
        except
            { Ignore any exception }
        end;
    end;

    FStatusCode       := 404;
    FReasonPhrase     := 'Connection aborted on request';
    FRequestDoneError := httperrAborted;

    if bFlag then
        SocketSessionClosed(Self, 0)
    else
        FWSocket.Close;
    StateChange(httpReady);  { 13/02/99 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.Login;
begin
    FDnsResult := '';
    StateChange(httpDnsLookup);
    try
        FWSocket.DnsLookup(FHostName);
    except
        on E: Exception do begin
            FStatusCode   := 404;
            FReasonPhrase := E.Message;
            FConnected    := FALSE;
            StateChange(httpReady);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SocketDNSLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        if FState = httpAborting then
            Exit;
        FRequestDoneError := Error;
        FStatusCode       := 404;
        FReasonPhrase     := 'can''t resolve hostname to IP address';
        SocketSessionClosed(Sender, Error);
    end
    else begin
        FDnsResult     := FWSocket.DnsResult;
        StateChange(httpDnsLookupDone);  { 19/09/98 }
        FWSocket.Addr  := FDnsResult;
        FWSocket.Port  := FPort;
        FWSocket.Proto := 'tcp';
        try
            FWSocket.Connect;
        except
            FRequestDoneError := FWSocket.LastError;
            FStatusCode       := 404;
            FReasonPhrase     := 'can''t connect: ' +
                                 WSocketErrorDesc(FWSocket.LastError) +
                                 ' (Error #' + IntToStr(FWSocket.LastError) + ')';
            FWSocket.Close;
            SocketSessionClosed(Sender, FWSocket.LastError);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SocketSessionConnected(Sender : TObject; Error : Word);
begin
    if Error <> 0 then begin
        FRequestDoneError := Error;
        FStatusCode       := 404;
        FReasonPhrase     := WSocketErrorDesc(Error) +
                             ' (Error #' + IntToStr(Error) + ')';
        SocketSessionClosed(Sender, Error);
        Exit;
    end;

    FConnected := TRUE;
    StateChange(httpConnected);
    TriggerSessionConnected;

    FNext := GetHeaderLineNext;
    StateChange(httpWaitingHeader);

    try
        case FRequestType of
        httpPOST:
            begin
                SendRequest('POST', '1.0');
                TriggerSendBegin;
                FAllowedToSend := TRUE;
                SocketDataSent(FWSocket, 0);
            end;
        httpHEAD:
            begin
                SendRequest('HEAD', '1.0');
            end;
        httpGET:
            begin
                SendRequest('GET', '1.0');
            end;
        end;
    except
        Logout;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.Logout;
begin
    FWSocket.Close;
    FConnected := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SendCommand(const Cmd : String);
const
    CRLF : String[2] = #13#10;
var
    Buf : String;
begin
    Buf := Cmd;
    if Assigned(FOnCommand) then
        FOnCommand(Self, Buf);
    if Length(Buf) > 0 then
        FReqStream.Write(Buf[1], Length(Buf));
    FReqStream.Write(CRLF[1], 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SendRequest(const Method, Version: String);
begin
    FReqStream.Clear;
    TriggerRequestHeaderBegin;
    SendCommand(method + ' ' + FPath + ' HTTP/' + Version);
    if FSender <> '' then
        SendCommand('From: ' + FSender);
{SendCommand('Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'); }
    if FAccept <> '' then
        SendCommand('Accept: ' + FAccept);
    if FReference <> '' then
        SendCommand('Referer: ' + FReference);
{SendCommand('Accept-Language: fr, en'); }
    if (method = 'POST') and (FContentPost <> '') then
        SendCommand('Content-Type: ' + FContentPost);
{SendCommand('UA-pixels: 1024x768'); }
{SendCommand('UA-color: color8'); }
{SendCommand('UA-OS: Windows 95'); }
{SendCommand('UA-CPU: x86'); }
{SendCommand('User-Agent: Mozilla/3.0 (compatible)');} {; MSIE 3.01; Update a; Windows 95)');}
    if FAgent <> '' then
        SendCommand('User-Agent: ' + FAgent);
    SendCommand('Host: ' + FTargetHost);
    if FNoCache then
        SendCommand('Pragma: no-cache');
    if method = 'POST' then
        SendCommand('Content-Length: ' + IntToStr(SendStream.Size));
    if FModifiedSince <> 0 then
        SendCommand('If-Modified-Since: ' +
                    RFC1123_Date(FModifiedSince) + ' GMT');
    if FUsername <> '' then
        SendCommand('Authorization: Basic ' +
                    EncodeStr(encBase64, FUsername + ':' + FPassword));
    if (FProxy <> '') and (FProxyUsername <> '') then
        SendCommand('Proxy-Authorization: Basic ' +
                    EncodeStr(encBase64, FProxyUsername + ':' + FProxyPassword));
{SendCommand('Proxy-Connection: Keep-Alive'); }
    if FCookie <> '' then
        SendCommand('Cookie: ' + FCookie);
    if (FContentRangeBegin <> '') or (FContentRangeEnd <> '') then begin            {JMR!! Added this line!!!}
        SendCommand('Range: bytes=' + FContentRangeBegin + '-' + FContentRangeEnd); {JMR!! Added this line!!!}
      FContentRangeBegin := '';                                                     {JMR!! Added this line!!!}
      FContentRangeEnd   := '';                                                     {JMR!! Added this line!!!}
    end;                                                                            {JMR!! Added this line!!!}
    FAcceptRanges := '';

    TriggerRequestHeaderEnd;
    SendCommand('');
    FWSocket.Send(FReqStream.Memory, FReqStream.Size);
    FReqStream.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.GetBodyLineNext;
var
    Len  : Integer;
    Data : Pointer;
const
    CRLF : String[2] = #13#10;
begin
    if FBodyLineCount = 0 then
        TriggerDocBegin;
    Inc(FBodyLineCount);

    Len := Length(FLastResponse);
    if Len > 0 then
        Data := @FLastResponse[1]
    else
        Data := @Len;
    FRcvdCount := FRcvdCount + Len;

    if Assigned(FRcvdStream) then
        FRcvdStream.WriteBuffer(Data^, Len);
    TriggerDocData(Data, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.GetHeaderLineNext;
var
    proto   : String;
    user    : String;
    pass    : String;
    port    : String;
    Host    : String;
    Path    : String;
    Field   : String;
    Data    : String;
    nSep    : Integer;
    tmpInt  : LongInt;
    bAccept : Boolean;
begin
    if FHeaderLineCount = 0 then
        TriggerHeaderBegin;
    Inc(FHeaderLineCount);

    { Some server send HTML document without header ! I don't know if it is  }
    { legal, but it exists (AltaVista Discovery does that).                  }
    if UpperCase(Copy(FLastResponse, 1, 6)) = '<HTML>' then begin { 15/09/98 }
        if FContentType = '' then
            FContentType := 'text/html';
        StateChange(httpWaitingBody);
        FNext := GetBodyLineNext;
        TriggerHeaderEnd;
        GetBodyLineNext;
        Exit;
    end;

    if FLastResponse = '' then begin
        if FLocationFlag then begin
            TriggerHeaderEnd;
            FReceiveLen       := 0;
            FHeaderLineCount  := 0;
            FBodyLineCount    := 0;
            FWSocket.OnSessionClosed := LocationSessionClosed;
            FWSocket.Close;
            Exit;
        end;
        if lowercase(ExtractFileExt(FDocName)) = '.exe' then begin
            if FContentType = 'text/html' then
                ReplaceExt(FDocName, 'htm');
        end;

        StateChange(httpWaitingBody);
        FNext := GetBodyLineNext;
        TriggerHeaderEnd;
        if FReceiveLen > 0 then begin
            SetLength(FLastResponse, FReceiveLen);
            Move(FReceiveBuffer, FLastResponse[1], FReceiveLen);
            GetBodyLineNext;
            FReceiveLen := 0;
        end;
        Exit;
    end;

    FRcvdHeader.Add(FLastResponse);

    nSep := pos(':', FLastResponse);
    if (Copy(FLastResponse, 1, 8) = 'HTTP/1.0') or
       (Copy(FLastResponse, 1, 8) = 'HTTP/1.1') then begin
        FStatusCode   := StrToInt(Copy(FLastResponse, 10, 3));
        FReasonPhrase := Copy(FLastResponse, 14, Length(FLastResponse));
    end
    else if nSep > 0 then begin
        Field := LowerCase(Copy(FLastResponse, 1, nSep - 1));
        { Skip spaces }
        Inc(nSep);
        while (nSep < Length(FLastResponse)) and
              (FLastResponse[nSep] = ' ') do
              Inc(nSep);
        Data  := Copy(FLastResponse, nSep, Length(FLastResponse));
        if Field = 'location' then begin { Change the URL ! }
            { URL with relocations:                 }
            { http://www.webcom.com/~wol2wol/       }
            { http://www.purescience.com/delphi/    }
            { http://www.maintron.com/              }
            { http://www.infoseek.com/AddURL/addurl }
            { http://www.micronpc.com/              }
            { http://www.amazon.com/                }
            FLocationFlag := TRUE;
            if Proxy <> '' then begin
                { We are using a proxy }
                if Data[1] = '/' then begin
                    { Relative location }
                    ParseURL(FPath, proto, user, pass, Host, port, Path);
                    if Proto = '' then
                        Proto := 'http';
                    FLocation := Proto + '://' + Host + Data;
                    FPath     := FLocation;
                end
                else begin
                    ParseURL(Data, proto, user, pass, Host, port, Path);
                    if port <> '' then
                        FPort := port;
                    if (Proto <> '') and (Host <> '') then begin
                        { We have a full relocation URL }
                        FTargetHost := Host;
                        FLocation   := Proto + '://' + Host + Path;
                        FPath       := FLocation;
                    end
                    else begin
                        if Proto = '' then
                            Proto := 'http';
                        if FPath = '' then
                            FLocation := Proto + '://' + FTargetHost + '/' + Host
                        else if Host = '' then
                            FLocation := Proto + '://' + FTargetHost + FPath
                        else
                            FTargetHost := Host;
                    end;
                end;
            end
            { We are not using a proxy }
            else begin
                if Data[1] = '/' then begin
                    { Relative location }
                    FPath     := Data;
                    if Proto = '' then
                        Proto := 'http';
                    FLocation := Proto + '://' + FHostName + FPath;
                end
                else begin
                    ParseURL(Data, proto, user, pass, FHostName, port, FPath);
                    if port <> '' then
                        FPort := port;
                    if (Proto <> '') and (FHostName <> '') then begin
                        { We have a full relocation URL }
                        FTargetHost := FHostName;
                        if FPath = '' then begin
                            FPath := '/';
                            FLocation := Proto + '://' + FHostName;
                        end
                        else
                            FLocation := Proto + '://' + FHostName + FPath;
                    end
                    else begin
                        if Proto = '' then
                            Proto := 'http';
                        if FPath = '' then begin
                            FLocation := Proto + '://' + FTargetHost + '/' + FHostName;
                            FHostName := FTargetHost;
                            FPath     := FLocation;          { 26/11/99 }
                        end
                        else if FHostName = '' then begin
                            FLocation := Proto + '://' + FTargetHost + FPath;
                            FHostName := FTargetHost;
                        end
                        else
                            FTargetHost := FHostName;
                    end;
                end;
            end;
        end
        else if Field = 'content-length' then
            FContentLength := StrToInt(Data)
        else if Field = 'content-range' then begin                             {JMR!! Added this line!!!}
            tmpInt := Pos('-', Data) + 1;                                      {JMR!! Added this line!!!}
            FContentRangeBegin := Copy(Data, 7, tmpInt-8);                     {JMR!! Added this line!!!}
            FContentRangeEnd   := Copy(Data, tmpInt, Pos('/', Data) - tmpInt); {JMR!! Added this line!!!}
        end                                                                    {JMR!! Added this line!!!}
        else if Field = 'accept-ranges' then
            FAcceptRanges := Data
        else if Field = 'content-type' then
            FContentType := LowerCase(Data)
        else if Field = 'www-authenticate' then
            FDoAuthor.add(Data)
        else if Field = 'set-cookie' then begin
            bAccept := TRUE;
            TriggerCookie(Data, bAccept);
        end
    {   else if Field = 'date' then }
    {   else if Field = 'mime-version' then }
    {   else if Field = 'pragma' then }
    {   else if Field = 'allow' then }
    {   else if Field = 'server' then }
    {   else if Field = 'content-encoding' then }
    {   else if Field = 'expires' then }
    {   else if Field = 'last-modified' then }
   end
   else { Ignore  all other responses }
       ;

    if Assigned(FOnHeaderData) then
        FOnHeaderData(Self);

    if FStatusCode >= 400 then
        FWSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.DoRequestAsync(Rq : THttpRequest);
var
    Proto, User, Pass, Host, Port, Path: String;
    I : Integer;
begin
    if FState <> httpReady then
        raise EHttpException.Create('HTTP component is busy', httperrBusy);

    if (Rq = httpPOST) and (not Assigned(FSendStream)) then
        raise EHttpException.Create('HTTP component has nothing to post',
                                    httperrNoData);

    FRcvdHeader.Clear;
    FRequestType      := Rq;
    FRequestDoneError := 0;
    FWSocket.OnSessionClosed  := SocketSessionClosed;
    StateChange(httpNotConnected);
    FDocName          := '';
    FStatusCode       := 0;
    FRcvdCount        := 0;
    FSentCount        := 0;
    FHeaderLineCount  := 0;
    FBodyLineCount    := 0;
    FContentLength    := -1;
    FContentType      := '';  { 25/09/1999 }
    FAllowedToSend    := FALSE;
    FLocation         := FURL;

    { parse url and proxy to FHostName, FPath and FPort }
    if FProxy <> '' then begin
        ParseURL(FURL, Proto, User, Pass, Host, Port, Path);
        FTargetHost := Host;
        FPath       := FURL;
        FDocName    := Path;
        if User <> '' then
            FUserName := User;
        if Pass <> '' then
            FPassword := Pass;
        { We need to remove usercode/Password from the URL given to the proxy }
        { but preserve the port                                               }
        if Port <> '' then
            Port := ':' + Port;
        if Proto = '' then
            FPath := 'http://'+ Host + Port + Path
        else
            FPath := Proto + '://' + Host + Port + Path;
        ParseURL(FProxy, Proto, User, Pass, Host, Port, Path);
        if Port = '' then
            Port := ProxyPort;
    end
    else begin
        ParseURL(FURL, Proto, User, Pass, Host, Port, FPath);
        FTargetHost := Host;
        FDocName    := FPath;
        if User <> '' then
            FUserName := User;
        if Pass <> '' then
            FPassword := Pass;
        if Port = '' then
            Port := '80';
    end;
    if Proto = '' then
        Proto := 'http';
    if FPath = '' then
        FPath := '/';
    if (FDocName = '') or (FDocName = '/') then
        FDocName := 'document.htm'
    else begin
        if FDocName[Length(FDocName)] = '/' then
            SetLength(FDocName, Length(FDocName) - 1);
        FDocName := Copy(FDocName, Posn('/', FDocName, -1) + 1, 255);
        I := Pos('?', FDocName);
        if I > 0 then
            FDocName := Copy(FDocName, 1, I - 1);
    end;

    FHostName   := host;
    FPort       := Port;

    { Ask to connect. When connected, we go at SocketSeesionConnected. }
    Login;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.DoRequestSync(Rq : THttpRequest);
begin
    DoRequestAsync(Rq);

{$IFDEF VER80}
    { Delphi 1 has no support for multi-threading }
    while FState <> httpReady do
        Application.ProcessMessages;
{$ELSE}
    if FMultiThreaded then begin
        while FState <> httpReady do begin
            FWSocket.ProcessMessages;
            Sleep(0);
        end;
    end
    else begin
        while FState <> httpReady do begin
{$IFNDEF NOFORMS}
            Application.ProcessMessages;
{$ELSE}
		    FWSocket.ProcessMessages;
{$ENDIF}
            Sleep(0);
        end;
    end;
{$ENDIF}

    if FStatusCode >= 400 then
        raise EHttpException.Create(FReasonPhrase, FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.LocationSessionClosed(Sender: TObject; Error: Word);
begin
    FConnected    := FALSE;
    FLocationFlag := FALSE;
    { Restore normal session closed event }
    FWSocket.OnSessionClosed := SocketSessionClosed;
    { Trigger the location changed event }
    if Assigned(FOnLocationChange) then
         FOnLocationChange(Self);
    { Restart at login procedure }
    PostMessage(FWindowHandle, WM_HTTP_LOGIN, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.WMHttpLogin(var msg: TMessage);
begin
    Login;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SocketSessionClosed(Sender: TObject; Error: Word);
begin
    FConnected := FALSE;
    if FBodyLineCount > 0 then
        TriggerDocEnd;
    SetReady; {StateChange(httpReady);}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
begin
    I := sizeof(FReceiveBuffer) - FReceiveLen - 1;
    if I <= 0 then
        raise EHttpException.Create('HTTP line too long', httperrOverflow);

    Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen], I);
{   writeln('Received ', Len, '(asked ', I, ')'); }

    if FRequestType = httpAbort then
        Exit;

    if Len <= 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    FReceiveLen := FReceiveLen + Len;

    if FState = httpWaitingBody then begin
        if FReceiveLen > 0 then begin
            SetLength(FLastResponse, FReceiveLen);
            Move(FReceiveBuffer, FLastResponse[1], FReceiveLen);
            if Assigned(FNext) then
                FNext
            else
                SetReady; {StateChange(httpReady);}
        end;
        FReceiveLen := 0;
        Exit;
    end;

    while FReceiveLen > 0 do begin
        I := Pos(#10, FReceiveBuffer);
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;

        if (I > 1) and (FReceiveBuffer[I-2] = #13) then
            FLastResponse := Copy(FReceiveBuffer, 1, I - 2)
        else
            FLastResponse := Copy(FReceiveBuffer, 1, I - 1);

{$IFDEF DUMP}
        FDumpBuf := '>|';
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
        FDumpBuf := '|' + #13#10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
{$IFDEF VER80}
        { Add a nul byte at the end of string for Delphi 1 }
        FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
        FReceiveLen := FReceiveLen - I;
        if FReceiveLen > 0 then
            Move(FReceiveBuffer[I], FReceiveBuffer[0], FReceiveLen + 1);

        if FState in [httpWaitingHeader, httpWaitingBody] then begin
            if Assigned(FNext) then
                FNext
            else
                SetReady; {StateChange(httpReady);}
        end
        else begin
            if Assigned(FOnDataAvailable) then
                FOnDataAvailable(Self, Error);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpCli.SocketDataSent(Sender : TObject; Error : Word);
var
    Len : Integer;
begin
    if not FAllowedToSend then
        Exit;

    Len := FSendStream.Read(FSendBuffer, sizeof(FSendBuffer));
    if Len <= 0 then begin
        FAllowedToSend := FALSE;
        TriggerSendEnd;
        Exit;
    end;

    if Len > 0 then begin
        FSentCount := FSentCount + Len;
        TriggerSendData(@FSendBuffer, Len);
        FWSocket.Send(@FSendBuffer, Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Get process and wait until terminated (blocking)      }
procedure THttpCli.Get;
begin
    DoRequestSync(httpGet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Head process and wait until terminated (blocking)     }
procedure THttpCli.Head;
begin
    DoRequestSync(httpHEAD);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Post process and wait until terminated (blocking)     }
procedure THttpCli.Post;
begin
    DoRequestSync(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the get process and returns immediately (non blocking)    }
procedure THttpCli.GetAsync;
begin
    DoRequestASync(httpGet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the head process and returns immediately (non blocking)   }
procedure THttpCli.HeadAsync;
begin
    DoRequestASync(httpHEAD);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the post process and returns immediately (non blocking)   }
procedure THttpCli.PostAsync;
begin
    DoRequestASync(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
procedure ParseURL(
    const url : String;
    var Proto, User, Pass, Host, Port, Path : String);
var
    p, q : Integer;
    s    : String;
begin
    proto := '';
    User  := '';
    Pass  := '';
    Host  := '';
    Port  := '';
    Path  := '';

    if Length(url) < 1 then
        Exit;

    p := pos('://',url);
    if p = 0 then begin
        if (url[1] = '/') then begin
            { Relative path without protocol specified }
            proto := 'http';
            p     := 1;
            if (Length(url) > 1) and (url[2] <> '/') then begin
                { Relative path }
                Path := Copy(url, 1, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 5)) = 'http:' then begin
            proto := 'http';
            p     := 6;
            if (Length(url) > 6) and (url[7] <> '/') then begin
                { Relative path }
                Path := Copy(url, 6, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 7)) = 'mailto:' then begin
            proto := 'mailto';
            p := pos(':', url);
        end;
    end
    else begin
        proto := Copy(url, 1, p - 1);
        inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := pos('/', s);
    q := pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
        p := q;
    if p = 0 then
        p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s    := Copy(s, 1, p-1);

    p := Posn(':', s, -1);
    if p > Length(s) then
        p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
        q := 0;
    if (p = 0) and (q = 0) then begin   { no user, password or port }
        Host := s;
        Exit;
    end
    else if q < p then begin  { a port given }
        Port := Copy(s, p + 1, Length(s));
        Host := Copy(s, q + 1, p - q - 1);
        if q = 0 then
            Exit; { no user, password }
        s := Copy(s, 1, q - 1);
    end
    else begin
        Host := Copy(s, q + 1, Length(s));
        s := Copy(s, 1, q - 1);
    end;
    p := pos(':', s);
    if p = 0 then
        User := s
    else begin
        User := Copy(s, 1, p - 1);
        Pass := Copy(s, p + 1, Length(s));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeStr(Encoding : THttpEncoding; const Value : String) : String;
begin
    Result := EncodeLine(Encoding, @Value[1], Length(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeLine(
    Encoding : THttpEncoding;
    SrcData  : PChar;
    Size     : Integer) : String;
var
    Offset : Integer;
    Pos1   : Integer;
    Pos2   : Integer;
    I      : Integer;
begin
    SetLength(Result, Size * 4 div 3 + 4);
    FillChar(Result[1], Size * 4 div 3 + 2, #0);

    if Encoding = encUUEncode then begin
        Result[1] := Char(((Size - 1) and $3f) + $21);
        Size      := ((Size + 2) div 3) * 3;
    end;
    Offset := 2;
    Pos1   := 0;
    Pos2   := 0;
    case Encoding of
        encUUEncode:        Pos2 := 2;
        encBase64, encMime: Pos2 := 1;
    end;
    Result[Pos2] := #0;

    while Pos1 < Size do begin
        if Offset > 0 then begin
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and
                                  ($3f shl Offset)) shr Offset));
            Offset := Offset - 6;
            Inc(Pos2);
            Result[Pos2] := #0;
        end
        else if Offset < 0 then begin
            Offset := Abs(Offset);
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and
                                  ($3f shr Offset)) shl Offset));
            Offset := 8 - Offset;
            Inc(Pos1);
        end
        else begin
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and $3f)));
            Inc(Pos2);
            Inc(Pos1);
            Result[Pos2] := #0;
            Offset    := 2;
        end;
    end;

    case Encoding of
    encUUEncode:
        begin
            if Offset = 2 then
                Dec(Pos2);
            for i := 2 to Pos2 do
                Result[i] := bin2uue[ord(Result[i])+1];
        end;
    encBase64, encMime:
        begin
            if Offset = 2 then
                Dec(Pos2);
            for i := 1 to Pos2 do
                Result[i] := bin2b64[ord(Result[i])+1];
            while (Pos2 and 3) <> 0  do begin
                Inc(Pos2);
                Result[Pos2] := '=';
            end;
        end;
    end;
    SetLength(Result, Pos2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }
function Posn(const s , t : String; Count : Integer) : Integer;
var
    i, h, Last : Integer;
    u          : String;
begin
    u := t;
    if Count > 0 then begin
        Result := Length(t);
        for i := 1 to Count do begin
            h := Pos(s, u);
            if h > 0 then
                u := Copy(u, h + 1, Length(u))
            else begin
                u := '';
                Inc(Result);
            end;
        end;
        Result := Result - Length(u);
    end
    else if Count < 0 then begin
        Last := 0;
        for i := Length(t) downto 1 do begin
            u := Copy(t, i, Length(t));
            h := Pos(s, u);
            if (h <> 0) and ((h + i) <> Last) then begin
                Last := h + i - 1;
                Inc(count);
                if Count = 0 then
                    break;
            end;
        end;
        if Count = 0 then
            Result := Last
        else
            Result := 0;
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

