unit MagentaIpLog;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{ Magenta Systems IP Log Streaming Component

Updated by Angus Robertson, Magenta Systems Ltd, England, v2.7 - 19th June 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMagIpLog is designed for IP stream logging, using TCP Client, TCP Server,
UDP Client or UDP Server protocols, sending simple text lines across a network
so they may be displayed or written to disk remotely.  The component allows
two way communication with TCP and UDP, so may also be used for simple
protocols such as communication between two applications.   The component
supports multiple client sockets so may be used to send data to two or more
different remote servers at the same time.  For TCP and UDP clients, the
component will optionally ping the remote computer first before opening
an IP connection to allow faster failure retries and some confirmation that
UDP may work.  TCP client provides repeated connection retry attempts,
including re-establishing a lost connection.  UDP client will optionally
keep pinging the remote during a connection to ensure it's still there.
UDP server sends data to the IP address and port from which it last received
data. TCP server supports multiple remote clients connecting.
Received data is parsed for various line endings optionally removing
control characters and triggering an event for a received line.  The only
other two events are optional, one for state changed when starting and
stopping, the second offering progress information and errors. The
component supports both IPv4 and IPv6, host name lookup for TCP and UDP
Client, and SSL connections for TCP Client and TCP Server, including
remote server certificate checking using either a local PEM bundle root
 file or the Windows Certificate Store.

A demo application testiplog.exe illustrates use of TMagIpLog as a TCP or
UDP client or server, and both in the same program sending data locally.

The same component may be used in a client or server application,
to send or receive.  Multiple instances of the component may be used on
different IP addresses and/or ports, for multiple different streams.

Note: applications using this component also need OverbyteIcsLogger in
application uses to satisfy the event type TLogOption

The Magenta Systems ComCap application may also be used to capture IP streams
to files or a database.

Requires Internet Component Suite (ICS) V8.55 dated June 2018 or later and
OpenSSL 1.1.0 or later, both of which may be downloaded from:
http://wiki.overbyte.eu/wiki/index.php/ICS_Download
or https://www.magsys.co.uk/delphi/magics.asp
The latest ICS version in the nightly zip includes the latest OpenSSL.


Using TMagIpLog:

1 - Drop the component onto a form (or create it in code, see testiplog.exe).
2 - Specify LogProtocol as one of logprotUdpClient, logprotUdpServer,
logprotTcpServer, logprotTcpClient.
3 - For client protocols, specify RemoteHost (name or IP address) and
RemoteIpPort, CheckPing true if ping to be used, RetryAttempts to non-zero
if continual retries not needed, RetryWaitSecs for delay between retries .
4 - For server protocols, LocalIpAddress is 0.0.0.0 to listen on all local
addresses, LocalIpPort must be non-zero.
5 - For sending data, AddCRLF to false if line already have terminating
characters, UdpNoCRLF to false if UDP should send CRLF.
6 - For receiving data, LineEndType to one of lineendCR, lineendLF,
lineendCustom (set in hex in CustomLineEnd) or lineendPacket (for UDP),
then MaxLineLen if a line should be returned before lineend is found,
normally non-ASCII characters are removed, set StripControls to false if
they should be replaced by spaces, RawData to true if CR, LF, FF and
control characters should not be removed.
6a - Line end may also be forced once MaxRecvData of data has been received
for binary protocols or those with content length headers (like HTTP).
7 - Assign onLogRecvEvent if data is to be received, onLogChangeEvent if
tracking of start and stop is needed, onLogProgEvent if progress information
is needed for logging.
8 - Call StartLogging.  The LogChangeEvent and LogProgEvent will trigger
when LogState changes to logstateOK when data may be sent.
9 - To send a line, if function GetAnyStateOK is true, call SendLogLine.
MaxSendBuffer specifies the amount of data that can be buffered otherwise
SendLogLine will fail.
10 - Received data will trigger LogRecvEvent once per line.
11 - Call StopLogging to stop.  Buffered data may continue to be sent
after close, keep calling CheckStopped until true when it's really finished
and component may be destroyed.
12 - To send an unlimited size stream, create a stream in the application
with TBufferedFileStream or TFileStream, and pass it to SendStream.
LogState changes to logstateOKStream while it's being sent, then back to
logstateOK as it finishes, the application should then free the stream.
13 - There is no specific handling for receiving a stream, textual data
will be handled according to the normal line end properties, and can be
saved to another stream in LogRecvEvent.  Binary data is more problematic,
set RawData to true and MaxLineLen to get a buffer load at a time, but
the last buffer load will need to be extracted with GetPartialLine using
a timeout, this is called automatically when the connection is closed.
14 - To send to multiple clients, set MaxSockets to the number needed,
then use the function SetRemotes to specify the remote host and port for
each socket number, base 0.  The events all return Socnr to indicate which
socket. MaxSockets also specifies how many remote clients can connect to
TCP Server, but note that Socnr is dynamic and changes as remote clients
come and go.



22nd Nov 2006 - baseline - v1.0
3rd Dec 2006 - using OverbyteIcsFtpSrvT instead of OverbyteIcsLibrary
1st Feb 2007 - UDP receive packets may be from multiple hosts, always keep IP
5th Aug 2008 - 1.2 - made compatible with ICS V7 and Delphi 2009
Note - only sends and receives ANSI text
20th Aug 2009 - 1.3 - fixed problem with MaxSockets being reported as closed
in the event when only one was open, tested with Delphi 2010
9th Aug 2010 - 1.4 - removed cast warnings with Delphi 2009 and later
22nd Sept 2011 - 1.5 - added SndBufSize and RcvBufSize to increase buffer sizes and speed
11th Sept 2012 - 1.6 - better error for too many clients with server
                       added CurSockets property for current number of server sockets
7th July 2014  - 2.0 - ICS 8 and later, using new ICS ping
                       added IPv6 and SSL support, including server certificate checking
                       added host name support for UDP and TCP client with DNS lookup
                       added LogProtocols suffixed 6 for IPv6
                       cleaned up some progress messages, identify error progress events
                       removed line length limit of 1024 that was not checked
                       added send a stream of unlimited length
                       get buffered partial received line during close
                       default line end is LF instead of CR so UNIX files are processed
23rd July 2014 - 2.1 - SSL logging improvements
13th July 2015 - 2.2 - added better SSL handshake error reporting
                       added lineendCRLF, only support FF as lineend if using CR
                       retry on SSL handshake errors
                       added RetryNoImm true to stop immediate reconnect attempts on abnormal disconnect
23rd Oct 2015  - 2.3 - better SSL client and server certificate reporting
8th July 2016  - 2.4 - fixed certificate reporting typo
                       added SrvTimeoutSecs to close idle server sessions
                       added Socket property to get current socket, mainly for statistics
23rd Nov 2016  - 2.5 - added GetSendWaiting to check how many bytes of send data not yet sent
                       increased default MaxSendBuffer size to 64K
                       added property TotRecvData total data received since connection, or
                         when method ResetRecvData was called
                       added property MaxRecvData which causes onLogRecvEvent to be called
                         when that length has been received.  May be used for fixed length
                         binary packets or where received data contains a content length
                         such as a HTTP response header followed by binary data
                       server takes exclusive access of addr/port
                       fixed bug with multiple clients not using correct port
                       added SSL Server Name Indication support
                       check multiple client SSL host names correctly
                       removed USE_SSL so SSL is always supported
                       removed TX509Ex now using TX509Base
                       using OpenSSL certificate verification host checking
                    xx server now supports LogSslReportChain to report server
                    xx   certificates, checks expired and reports chain -
                       gone in 2.6 now done before initialising context
7th March 2017 - 2.6 - set IcsLogger for context so it logs more stuff
                       simplified reporting SSL certs in client handshake
                       improved validation of server certificates
                       use threaded DNS lookup
19th June 2018 - 2.7 - support TLSv1.3, no real changes...
                       don't start SSL handshake twice
                       cleaned up SSL error handling
14th Dec 2018  - 2.8 - no real changes, clean up

Last version - new work will be 3.0 in MagentaIpLog3.pas with pending stuff below
New version will be SSL only, not sure this version builds without SSL at the moment

pending - 3.0 - set-up sslcontext in component
pending - server uses socketserver IcsHosts for configuration and SslSrvSecurity
pending - client and server different GetSession events

}

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I ..\Include\OverbyteIcsDefs.inc}
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

uses
  SysUtils, Classes, ExtCtrls,
  Magsubs1,
  OverbyteIcsTypes,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsWSockets,
  OverbyteIcsWinsock,
  OverbyteIcsFtpSrvT,
  OverbyteIcsLogger,
  OverbyteIcsUtils,
  OverbyteIcsStreams,
  OverByteIcsSSLEAY,
  OverByteIcsLIBEAY,
  OverbyteIcsSslSessionCache,
  OverbyteIcsMsSslUtils,
  OverbyteIcsWinCrypt,
  OverbyteIcsPing,
  OverbyteIcsIcmp ;

type
  TLogProtocol = (logprotUdpClient, logprotUdpServer,
                  logprotTcpServer, logprotTcpClient,
                  logprotUdpClient6, logprotUdpServer6,
                  logprotTcpServer6, logprotTcpClient6) ;
  TLogState = (logstateNone, logstateStart, logstateHandshake,
               logstateOK, logstateOKStream, logstateStopping) ;
  TLineEnd = (lineendCR, lineendLF, lineendCustom, lineendPacket, lineendCRLF) ;
  TVerifyMethod = (logSslVerNone, logSslVerBundle, logSslVerWinStore) ;

  TLogRecvEvent = procedure (Sender: TObject; Socnr: integer;
                                         const Line: string) of object;

  TLogProgEvent = procedure (Sender: TObject; Socnr: integer;
                          LogOption: TLogOption; const Msg: string) of object;

  TLogChangeEvent = procedure (Sender: TObject; Socnr: integer;
                                         LogState: TLogState) of object;

const
  LogStateNames: array [TLogState] of PChar = ('None', 'Start', 'Handshake',
                                               'OK', 'Streaming', 'Closing');

type
  TChanInfo = record
    SendSocket: TSslWSocket;  // for UDP Server/Client and TCP Client
    WaitTimer: TTimer;       // delay before next connect attempt
    LogState: TLogState ;    // current connect state
    AttemptNr: integer ;     // failed connect attempts
    UnexpectedCount: integer ;  // unexpected connect failures
    RemHost: string ;        // remote IP address or host
    RemIP: string ;          // remote IP address looked-up   2 July 2014
    RemLookup: boolean ;     // false if pending DNS host name look-up   2 July 2014
    RemPort: string ;        // remote IP port
    PeerIpAddr: string ;     // IP address from UDP remote computer sending us data
    PeerIpPort: string ;     // ditto port
    BindIpAddr: string ;     // local IP address socket is bound to
    BindIpPort: string ;     // ditto port
    BindFamily: TSocketFamily ;  // socket family 7 Oct 2013
    SocRemAddr: TSockAddrIn;   // internal structure for remote UDP IPv4 address/port
    SocRemAddr6: TSockAddrIn6; // internal structure for remote UDP IPv6 address/port 7 Oct 2013
    RxLineData: AnsiString;    // received partial data line, Dec 2013 was fixed size and overflowed
    RxLineLen: integer ;       // received data length
    LastLine: AnsiString ;     // received last full line
    TotRecvData: int64 ;       // 13 Sept 2016 how much data has been received and sent to LogRecvEvent event
    StreamFlag: boolean ;      // stream currently being sent, 3 July 2014
    StreamNr: integer ;        // how many bytes of stream have been sent 3 July 2014
  end ;

// send TCP Server socket client
  TMagClientSocket = class(TSslWSocketClient)
  private
    { Private declarations }
  public
    IpAddr: string ;         // IP address from remote computer sending us data
    IpPort: string ;         // ditto port
  end;

  TMagIpLog = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FListenSocket: TSslWSocketServer ;
    FChanInfo: array of TChanInfo ;
    FTotSockets: integer ;
    FMaxSockets: integer ;
    FLogActive: boolean ;
    FRemoteHost: string ;
    FRemoteIpPort: string ;
    FLocalIpAddr: string ;
    FLocalIpPort: string ;
    FSocFamily: TSocketFamily ;  // 7 Oct 2013
    FForceSsl: boolean ;         // 7 Oct 2013
    FPingWaitSecs: integer;
    FCheckPing: boolean ;
    FRetryAttempts: integer ;
    FRetryWaitSecs: integer ;
    FRetryNoImm: boolean ;     // 7 July 2015
    FAutoReconnect: boolean ;
    FLogProtocol: TLogProtocol ;
    FKeepAliveSecs: integer ;
    FUdpNoCRLF: boolean ;
    FAddCRLF: boolean ;
    FLineEndType: TLineEnd ;
    FCustomLineEnd: string ;  // hex version
    FLineEndChar: AnsiChar ;      // char version
    FMaxLineLen: integer ;
    FStripControls: boolean ;
    FRawData: boolean ;
    FLastErrorStr: string ;
    FMultipleRemotes: boolean ;
    FLogTitle: string ;
    FCurTitle: string ;
    FMaxSendBuffer: integer ;
    FSendBuffer: AnsiString ;  // 3 July 2014 only used for send stream block reading
    FSendStream: TStream ;
    FRxBuffer: array [0..2048] of AnsiChar;
    FonLogRecvEvent: TLogRecvEvent ;
    FonLogProgEvent: TLogProgEvent ;
    FonLogChangeEvent: TLogChangeEvent ;
    FSndBufSize: integer;  // 1.5 socket send buffer size
    FRcvBufSize: integer;  // 1.5 socket Recv buffer size
    FCurSockets: integer ; // 1.6 current sockets for server
    FSrvTimeoutSecs: integer ;  // 5 July 2016 server idle timeout
    FServerTimer: TTimer;      // 5 July 2016 timer to check for idle timeouts
    FHeartBeatBusy: Boolean ;  // 5 July 2016
    FMaxRecvData: int64 ;      // 13 Sept 2016 maximum data to receive before triggering LogRecvEvent event
    procedure SocketBgException(Sender: TObject;
                          E: Exception; var CanClose: Boolean);
    procedure ServerClientCreate(Sender : TObject; Client : TWSocketClient); virtual;
    procedure ServerClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word); virtual;
    procedure ServerClientDisconnect(Sender: TObject;
                                 Client: TWSocketClient; Error: Word);
    procedure SocketSessionConnected(Sender: TObject; Error: Word);
    procedure SocketSessionClosed(Sender: TObject; Error: Word);
    procedure SocketDataAvailable(Sender: TObject; Error: Word);
    procedure SocketDataDnsLookupDone (Sender: TObject; Error: Word);  // 2 July 2014
    procedure SocketDataSent(Sender: TObject; ErrCode : word);         // 3 July 2014
    procedure SocketDataConnect (Socnr: integer);
    procedure PingThreadDone(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);
    function GetChanInfo (Index: integer): TChanInfo ;
    function GetState (Index: integer): TLogState ;
    function GetAnyStateOK: boolean ;
    procedure IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                                                      const Msg : String);
    procedure LogProgEvent (Socnr: integer; const Msg : String);
    procedure LogErrEvent (Socnr: integer; const Msg : String);
    procedure LogChangeState (Socnr: integer; NewState: TLogState) ;
    function PingStart (Socnr: integer): boolean ;
    procedure SetMaxSockets (Tot: integer) ;
    procedure SetRemoteHost (const NewRemHost: string) ;
    procedure CheckServerSockets ;
    procedure CreateServerSocket; virtual;
    procedure ServerTimerTimer(Sender: TObject);
    function GetSocket (Index: integer): TWSocket;     // 7 July 2016
    function GetTotRecvData (socnr: integer): int64 ;  // 13 Sept 2016
  public
    IpIcsLogger: TIcsLogger ;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function StartLogging: boolean ;
    function StopLogging: boolean ;
    function CheckStopped: boolean ;
    function SendLogLine (const Line: AnsiString): boolean ;   // 8 Aug 2010
    function SendStream (MyStream: TStream): boolean ;           // 3 Jul 2014
    function SetRemotes (Socnr: integer; const NewRemHost, NewRemPort: string): boolean ;
    procedure ClearLine (Socnr: integer) ;
    procedure ResetRecvData (Socnr: integer) ;             // 13 Sept 2016
    function GetPartialLine (Socnr: integer): string ;
    function GetSendWaiting (Socnr: integer): integer ;    // 6 Sept 2016
    property ChanInfos [Index: integer]: TChanInfo read GetChanInfo ;
    property States [Index: integer]: TLogState read GetState ;
    property LogActive: boolean      read FLogActive ;
    property AnyStateOK: boolean     read GetAnyStateOK ;
    property TotSockets: integer     read FTotSockets ;
    property CurSockets: integer     read FCurSockets ;  // 1.6
    property LastErrorStr: string    read FLastErrorStr ;
    property SocFamily: TSocketFamily read FSocFamily ;                   // 7 Oct 2013
    property Socket [Index: integer]: TWSocket read GetSocket;  // 7 July 2016
    property TotRecvData [Index: integer]: int64 read GetTotRecvData ;        // 13 Sept 2016 how much data has been received and sent to LogRecvEvent event
  published
    property MaxSockets: integer     read FMaxSockets write SetMaxSockets ;
    property RemoteHost: string      read FRemoteHost write SetRemoteHost ;
    property RemoteIpPort: string    read FRemoteIpPort write FRemoteIpPort ;
    property LocalIpAddr: string     read FLocalIpAddr write FLocalIpAddr ;
    property LocalIpPort: string     read FLocalIpPort write FLocalIpPort ;
    property ForceSsl: boolean       read FForceSsl write FForceSsl ;      // 7 Oct 2013
    property PingWaitSecs: integer   read FPingWaitSecs write FPingWaitSecs ;
    property CheckPing: boolean      read FCheckPing write FCheckPing ;
    property RetryAttempts: integer  read FRetryAttempts write FRetryAttempts ;
    property RetryWaitSecs: integer  read FRetryWaitSecs write FRetryWaitSecs ;
    property RetryNoImm: boolean     read FRetryNoImm write FRetryNoImm ;      // 7 July 2015
    property AutoReconnect: boolean  read FAutoReconnect write FAutoReconnect ;
    property LogProtocol: TLogProtocol read FLogProtocol write FLogProtocol ;
    property KeepAliveSecs: integer  read FKeepAliveSecs write FKeepAliveSecs ;
    property UdpNoCRLF: boolean      read FUdpNoCRLF write FUdpNoCRLF ;
    property AddCRLF: boolean        read FAddCRLF write FAddCRLF ;
    property LineEndType: TLineEnd   read FLineEndType write FLineEndType ;
    property CustomLineEnd: string   read FCustomLineEnd write FCustomLineEnd ;
    property MaxLineLen: integer     read FMaxLineLen write FMaxLineLen ;
    property StripControls: boolean  read FStripControls write FStripControls ;
    property RawData: boolean        read FRawData write FRawData ;
    property LogTitle: string        read FLogTitle write FLogTitle ;
    property MaxSendBuffer: integer  read FMaxSendBuffer write FMaxSendBuffer ;
    property SndBufSize: integer     read FSndBufSize write FSndBufSize ;
    property RcvBufSize: integer     read FRcvBufSize write FRcvBufSize ;
    property SrvTimeoutSecs: integer read FSrvTimeoutSecs write FSrvTimeoutSecs ;  // 5 July 2016
    property MaxRecvData: int64      read FMaxRecvData write FMaxRecvData ;        // 13 Sept 2016

    property onLogRecvEvent: TLogRecvEvent read FonLogRecvEvent write FonLogRecvEvent ;
    property onLogChangeEvent: TLogChangeEvent read FonLogChangeEvent write FonLogChangeEvent ;
    property onLogProgEvent: TLogProgEvent read FonLogProgEvent write FonLogProgEvent ;
  end;

    TSslMagIpLog = class(TMagIpLog)
    protected
        FOnHandshakeDone: TSslHandshakeDoneEvent;
        FOnVerifyPeer: TSslVerifyPeerEvent;
        FLogSslContext: TSslContext;
        FLogSslSessCache: TSslAvlSessionCache;
        FLogTrustedList: String;
        FLogSslVerMethod: TVerifyMethod;
        FLogSslRevocation: boolean;
        FMsCertChainEngine: TMsCertChainEngine;
        FLogSslReportChain: boolean ;
        procedure CreateServerSocket; override;
        procedure TlsSslNewSession(Sender: TObject; SslSession: Pointer;
                                                WasReused: Boolean; var IncRefCount : Boolean);
        procedure TlsSslGetSession(Sender: TObject;
                            var SslSession: Pointer; var FreeSession : Boolean);
        procedure TlsSslVerifyPeer(Sender: TObject; var Ok : Integer; Cert: TX509Base);
        procedure TlsSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect : Boolean);
        procedure ServerClientCreate(Sender : TObject; Client : TWSocketClient); override;
        procedure ServerClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word); override;
        procedure TlsSslProtoMsgEvent(Sender: TObject; Info: String; Sending: integer;
                        Version: integer; ContentType: integer; Buffer: PAnsiChar; BuffSize: integer) ;
    public
        LogRcvdCerts: boolean ;
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
    published
        property  LogSslContext: TSslContext                  read  FLogSslContext write FLogSslContext;
        property  LogSslSessCache: TSslAvlSessionCache        read  FLogSslSessCache write FLogSslSessCache;
        property  LogTrustedList: String                      read  FLogTrustedList write FLogTrustedList;
        property  LogSslVerMethod: TVerifyMethod              read  FLogSslVerMethod write FLogSslVerMethod;
        property  LogSslRevocation: boolean                   read  FLogSslRevocation write FLogSslRevocation;
        property  LogSslReportChain: boolean                  read  FLogSslReportChain write FLogSslReportChain;
        property  OnLogVerifyPeer: TSslVerifyPeerEvent        read  FOnVerifyPeer write FOnVerifyPeer;
        property  OnLogHandshakeDone: TSslHandshakeDoneEvent  read  FOnHandshakeDone write FOnHandshakeDone;
    end;


procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Magenta Systems', [TMagIpLog, TSslMagIpLog]);
end;

constructor TMagIpLog.Create(AOwner: TComponent);
begin
    IpIcsLogger := TIcsLogger.Create (self) ;
    IpIcsLogger.OnIcsLogEvent := IcsLogEvent ;
    FTotSockets := 0  ;
    FCurSockets  := 0 ;
    FMaxSockets := 1 ;
    SetMaxSockets (FMaxSockets) ;
    FLogActive := false ;
    FRemoteIpPort := '514' ;
    FLocalIpAddr := ICS_ANY_HOST_V4 ;
    FLocalIpPort := '0' ;
    FSocFamily := sfIpV4 ;
    FForceSsl := false ;
    FPingWaitSecs := 5 ;
    FCheckPing := false ;
    FRetryAttempts := 0 ;
    FRetryWaitSecs := 10 ;
    FAutoReconnect := true ;
    FLogProtocol := logprotUdpClient ;
    FKeepAliveSecs := 120 ;
    FUdpNoCRLF := false ;
    FAddCRLF := true ;
    FLineEndType := lineendLF ;  // 4 July 2014 was CR
    FCustomLineEnd := '$03' ;
    FMaxLineLen := 132 ;
    FStripControls := true ;
    FRawData := false ;
    FMultipleRemotes := false ;
    FLogTitle := '' ;
    FCurTitle := 'None' ;
    FMaxSendBuffer := 65536 ;  // 7 Sept 2016, was 16384
    FListenSocket := Nil ;
    FSndBufSize := FMaxSendBuffer ;  // 1.5 socket send buffer size
    FRcvBufSize := FMaxSendBuffer ;  // 1.5 socket Recv buffer size
    FSrvTimeoutSecs := 0 ;  // 5 July 2016 server idle timeout
    FMaxRecvData := 0 ;      // 13 Sept 2016 maximum data to receive before triggering LogRecvEvent event
    inherited Create(AOwner);
end;

destructor TMagIpLog.Destroy;
var
    I: integer ;
begin
    StopLogging ;
    FreeAndNil (FServerTimer) ; // 5 July 2016
    if FTotSockets > 0 then
    begin
        for I := 0 to Pred (FTotSockets) do
        begin
            with FChanInfo [I] do
            begin
                try
                    FreeAndNil (SendSocket) ;
                except
                end ;
                FreeAndNil (WaitTimer) ;
            end ;
        end ;
    end ;
    FTotSockets := 0 ;
    FCurSockets := 0 ;
    SetLength (FChanInfo, 0) ;
    try
        FreeAndNil (FListenSocket) ;
    except
    end ;
    FreeAndNil (IpIcsLogger) ;
    inherited Destroy;
end;

procedure TMagIpLog.SetMaxSockets (Tot: integer) ;
begin
    FMaxSockets := Tot ;
    if Tot > FTotSockets then
    begin
        SetLength (FChanInfo, Tot) ;
    end ;
    while FTotSockets < Tot do
    begin
        with FChanInfo [FTotSockets] do
        begin
            LogState := logstateNone ;
            SendSocket := Nil ;
            if NOT (FLogProtocol in [logprotTcpServer, logprotTcpServer6]) then // 5 July 2016
            begin
                WaitTimer := TTimer.Create (Self) ;
                WaitTimer.Enabled := false ;
                WaitTimer.OnTimer := WaitTimerTimer ;
            end;
        end ;
        inc (FTotSockets) ;
    end ;
end ;

procedure TMagIpLog.IcsLogEvent (Sender: TObject; LogOption: TLogOption;
                                                      const Msg : String);
var
    socnr: integer ;
begin
    if Sender is TWSocket then
        socnr := (Sender as TWSocket).Tag
    else
        socnr := 0;
    if Assigned (FonLogProgEvent) then FonLogProgEvent (Self, Socnr, LogOption, FCurTitle + ' ' + Msg) ;
end ;

procedure TMagIpLog.LogProgEvent (Socnr: integer; const Msg : String);
begin
    if Assigned (FonLogProgEvent) then FonLogProgEvent (Self, Socnr, loProtSpecInfo, Msg) ;
end ;

procedure TMagIpLog.LogErrEvent (Socnr: integer; const Msg : String);
begin
    if Assigned (FonLogProgEvent) then FonLogProgEvent (Self, Socnr, loProtSpecErr, Msg) ;
end ;

procedure TMagIpLog.LogChangeState (Socnr: integer; NewState: TLogState) ;
begin
    if (Socnr < 0) or (Socnr >= FTotSockets) then exit ;
    with FChanInfo [Socnr] do
    begin
        if LogState = NewState then exit ;
        LogState := NewState ;
        if Assigned (FonLogChangeEvent) then FonLogChangeEvent (Self, Socnr, NewState) ;
    end ;
end ;

function TMagIpLog.SetRemotes (Socnr: integer;
                                const NewRemHost, NewRemPort: string): boolean ;
begin
    result := false ;
    if (Socnr < 0) or (Socnr >= FTotSockets) then
    begin
        FLastErrorStr := FCurTitle + ' Insufficient Sockets Configured' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;
    FMultipleRemotes := true ;
    with FChanInfo [Socnr] do
    begin
        RemHost := '' ;
        RemPort := '' ;
        if atoi (NewRemPort) <= 0 then
        begin
            FLastErrorStr := FCurTitle + ' No Remote Port Specified' ;
            LogErrEvent (0, FLastErrorStr) ;
            exit ;
        end ;
        if NewRemHost = '' then
        begin
            FLastErrorStr := FCurTitle + ' No Remote Host Specified' ;
            LogErrEvent (0, FLastErrorStr) ;
            exit ;
        end ;
        RemHost := NewRemHost ;
        RemPort := NewRemPort ;
        if FLogProtocol in [logprotUdpClient6, logprotTcpClient6,
                                       logprotUdpServer6, logprotTcpServer6] then
            BindFamily := sfIPv6
        else
            BindFamily := sfIPv4 ;
    end ;
    if Socnr = 0 then
    begin
        FRemoteHost := NewRemHost ;
        FRemoteIpPort := NewRemPort ;  // Nov 2016
    end;
    result := true ;
end;

procedure TMagIpLog.SetRemoteHost (const NewRemHost: string) ;
var
    I: integer ;
begin
    if FMultipleRemotes then
    begin
        for I := 0 to Pred (FTotSockets) do FChanInfo [I].RemHost := '' ;
        FMultipleRemotes := false ;
    end ;
    FRemoteHost := NewRemHost ;
end ;

function TMagIpLog.GetChanInfo (Index: integer): TChanInfo ;
begin
    if (Index < 0) or (Index >= FTotSockets) then Index := 0 ;
    result := FChanInfo [Index] ;
end;

function TMagIpLog.GetState (Index: integer): TLogState ;
begin
    if (Index < 0) or (Index >= FTotSockets) then Index := 0 ;
    result := FChanInfo [Index].LogState ;
end;

// 7 July 2016 get current socket
function TMagIpLog.GetSocket (Index: integer): TWSocket;
begin
    Result := nil ;
    if (Index < 0) or (Index >= FTotSockets) then Index := 0 ;
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if (FListenSocket.ClientCount > 0) and
            (Index < FListenSocket.ClientCount) then
                 Result := FListenSocket.Client [index] as TWSocket ;
    end
    else
        Result := FChanInfo [Index].SendSocket ;
end;

// check if any socket is connected and able to send data or stream
// but not if a stream is still being sent

function TMagIpLog.GetAnyStateOK: boolean ;
var
    I: integer ;
begin
    result := false ;
    if FMaxSockets <= 0 then exit ;
    for I := 0 to Pred (FMaxSockets) do
    begin
        if (FChanInfo [I].LogState = logstateOKStream) then  // send a stream
        begin
            result := false ;
            exit ;
        end;
        if (FChanInfo [I].LogState = logstateOK) then result := true ;
    end ;
end;

// create TCP Server

procedure TMagIpLog.CreateServerSocket;
begin
    FListenSocket := TSslWSocketServer.Create(Self);
    FListenSocket.SslEnable := false ;  // defaulted to true, must reset since not SSL component
end;

// start logging, open various sockets and starts connections, but
// connection may not be completed when this function exits, keep calling
// GetAnyStateOK to see if any sockets are connected or trigger from
// onLogChangeEvent with LogState = logstateOK

function TMagIpLog.StartLogging: boolean ;
var
    I, tothosts: integer ;
    MyFamily: TSocketFamily;
    TempCert: TX509Base;
    CertStr, ErrStr: string;
begin
    result := false ;
    FLastErrorStr := '' ;
    if (NOT FMultipleRemotes) or (FLogProtocol in [logprotUdpServer, logprotUdpServer6]) then
        tothosts := 1
    else
        tothosts := FMaxSockets ;
    if Length (FCustomLineEnd) >= 1 then
    begin
        if '$' = FCustomLineEnd [1] then
            FLineEndChar := AnsiChar (StrToIntDef (Copy (FCustomLineEnd, 1, 3), 0))
        else
            FLineEndChar := AnsiChar (FCustomLineEnd [1]) ;
    end ;

    if FLogActive then
    begin
        FLastErrorStr := FCurTitle + ' Log Client Already Busy' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;

// validate some settings, Oct 2015 was after SSL stuff
    if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpClient6, logprotTcpClient6] then
    begin
        if FLogTitle <> '' then
            FCurTitle := FLogTitle
        else
        begin
            if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
                FCurTitle := 'UDP/Client'
            else
                FCurTitle := 'TCP/Client' ;
        end ;
        if tothosts = 1 then
        begin
            if atoi (FRemoteIpPort) <= 0 then
            begin
                FLastErrorStr := FCurTitle + ' No Remote Port Specified' ;
                LogErrEvent (0, FLastErrorStr) ;
                exit ;
            end ;
            if FRemoteHost = '' then
            begin
                FLastErrorStr := FCurTitle + ' No Remote Host Specified' ;
                LogErrEvent (0, FLastErrorStr) ;
                exit ;
            end ;
        end
        else
        begin
            for I := 0 to Pred (tothosts) do  // Nov 2016 check all clients
            begin
                with FChanInfo [I] do
                begin
                    if atoi (RemPort) <= 0 then
                    begin
                        FLastErrorStr := FCurTitle + ' No Remote Port Specified' ;
                        LogErrEvent (0, FLastErrorStr) ;
                        exit ;
                    end ;
                    if RemHost = '' then
                    begin
                        FLastErrorStr := FCurTitle + ' No Remote Host Specified' ;
                        LogErrEvent (0, FLastErrorStr) ;
                        exit ;
                    end ;
                end;
            end;
        end;

    end
    else if FLogProtocol in [logprotUdpServer, logprotUdpServer6] then
    begin
        if atoi (FLocalIpPort) <= 0 then
        begin
            FLastErrorStr := FCurTitle + ' No Local Port Specified' ;
            LogErrEvent (0, FLastErrorStr) ;
            exit ;
        end ;
        if FLogTitle <> '' then
            FCurTitle := FLogTitle
        else
            FCurTitle := 'UDP/Server' ;
    end
    else if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if atoi (FLocalIpPort) <= 0 then
        begin
            FLastErrorStr := FCurTitle + ' No Local Port Specified' ;
            LogErrEvent (0, FLastErrorStr) ;
            exit ;
        end ;
        if FLogTitle <> '' then
            FCurTitle := FLogTitle
        else
            FCurTitle := 'TCP/Server' ;
    end
    else
    begin
        FLastErrorStr := FCurTitle + ' Unknown Protocol' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;

// sanity check
    with TSslMagIpLog (Self) do
    begin
        if FForceSsl then
        begin
            if (NOT Assigned (FLogSslContext)) then
            begin
                FLastErrorStr := FCurTitle + ' SSL Requires SSL Context' ;
                LogErrEvent (0, FLastErrorStr) ;
                exit ;
            end;
            try
                fLogSslContext.IcsLogger := IpIcsLogger ;  // Dec 2016
                FLogSslContext.SslSessionCacheModes := [];
                FLogSslContext.SslVerifyPeer := false;
                FLogSslContext.SslVerifyPeerModes := [] ;
                if FLogProtocol in [logprotTcpClient, logprotTcpClient6] then
                begin
                    if FLogSslVerMethod > logSslVerNone  then
                    begin
                        FLogSslContext.SslVerifyPeer := true ;
                        FLogSslContext.SslVerifyPeerModes := [SslVerifyMode_PEER] ;
                    end;
                end;
                if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
                begin
                //
                end;
                fLogSslContext.InitContext;
                if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
                begin

               // Nov 2016 see if reporting server certificates
               // Feb 2017 now usually done when loading files, but not necessarily
                    if FLogSslReportChain then
                    begin
                     // Feb 2017  find what is actually loaded, report chain
                        TempCert := TX509Base.Create(self) ;
                        fLogSslContext.SslGetCerts(TempCert) ;
                        TempCert.ValidateCertChain('', CertStr, ErrStr);
                        TempCert.Free ;
                        LogProgEvent (0, FCurTitle + ' Actual Certificate Chain: ' +
                                                              CertStr + CRLF + ErrStr);
                    end;
                end ;
            except
                FLastErrorStr := FCurTitle + ' Error Starting SSL - ' + GetExceptMess (ExceptObject) ;
                LogErrEvent (0, FLastErrorStr) ;
                exit ;
            end;
        end ;
    end;

// clean up old stuff
    for I := 0 to Pred (FTotSockets) do
    begin
        with FChanInfo [I] do
        begin
            LogState := logstateNone ;
            AttemptNr := 0 ;
            UnexpectedCount := 0 ;
            PeerIpAddr := '' ;
            PeerIpPort := '' ;
            BindIpAddr := '' ;
            BindIpPort := '' ;
            BindFamily  := sfAny ;
            RxLineLen := 0 ;
            SetLength (RxLineData, MaxLineLen +  8) ; // Dec 2014 dynamic size
            if MaxLineLen > FMaxSendBuffer then FMaxSendBuffer := (MaxLineLen * 2) + 10 ; // Dec 2014 sanity check
            SetLength (LastLine, 0) ;
            TotRecvData := 0 ;      // 13 Sept 2016 how much data has been received and sent to LogRecvEvent event
            FillChar (SocRemAddr, Sizeof (SocRemAddr), 0);
            FillChar (SocRemAddr6, Sizeof (SocRemAddr6), 0);
            if Assigned (WaitTimer) then  // 8 July 2016
            begin
                with WaitTimer do
                begin
                    Enabled := false ;
                    Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;  // warning, may be changed
                    Tag := I ; // keep socnr
                end ;
            end;
        end ;
    end ;

// set socket family
    if FLogProtocol in [logprotUdpClient6, logprotTcpClient6,
                                       logprotUdpServer6, logprotTcpServer6] then
        FSocFamily := sfIPv6
    else
        FSocFamily := sfIPv4 ;

// clean up old sockets, create fresh sockets and events
    try
        FreeAndNil (FListenSocket) ;
    except
    end ;
    if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpServer,
                        logprotUdpClient6, logprotTcpClient6, logprotUdpServer6] then
    begin
        for I := 0 to Pred (tothosts) do
        begin
            with FChanInfo [I] do
            begin
                LogState := logstateNone ;
                try
                    FreeAndNil (SendSocket) ;
                except
                end ;
                SendSocket := TSslWSocket.Create (Self) ;
                with TSslMagIpLog (Self) do
                begin
                    SendSocket.SetAcceptableHostsList (FLogTrustedList) ;
                end;
                with SendSocket do
                begin
                    Tag := I ;
                    LingerOnOff := wsLingerOff;
                    LingerTimeout := 0;
                    LineMode := false;
                    CreateCounter ;     // 4 July 2016 duration, last activity
                    ComponentOptions := [wsoNoReceiveLoop, wsoIcsDnsLookup];  // Mar 2017
                   // pending - skips DnsLookup,but too many changes
                //    ComponentOptions := ComponentOptions + [wsoAsyncDnsLookup];  // !!! TEMP
                    OnDataAvailable := SocketDataAvailable ;
                    OnBgException := SocketBgException ;
                    SocketErrs := wsErrFriendly ; // Oct 2016
                    IcsLogger := IpIcsLogger ;
                end ;
             // note, multiple remhost already set by SetRemoteHost or SetRemoteHosts
            end ;
        end ;
        if tothosts = 1 then  // Nov 2016
        begin
            FChanInfo [0].RemHost := FRemoteHost ;
            FChanInfo [0].RemPort := FRemoteIpPort ;
            FChanInfo [0].BindFamily := FSocFamily ;
        end;
    end ;

// set sockets parameters, then connect or listen
    try  // except
        if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
        begin
            for I := 0 to Pred (tothosts) do
            begin
                with FChanInfo [I] do
                begin
                    if RemHost = '' then continue ;  // skip unused socket
                    with SendSocket do
                    begin
                        OnDnsLookupDone := SocketDataDnsLookupDone ; // 2 July 2014
                        OnDataSent := SocketDataSent ;  // 3 July 2014
                        SocketErrs := wsErrFriendly ; // Oct 2016
                        StreamFlag := false ;
                        StreamNr := 0 ;
                        Tag := I ; // keep socnr
                        RemLookup := WSocketIsIPEx (RemHost, MyFamily) ;  // 2 July 2014 true if real IP address
                        RemIP := RemHost ;
                        if FCheckPing then  // note UDP continue to ping while connected
                        begin               // as an attempt to catch a lost connection
                            if PingStart (I) then
                            begin
                                LogChangeState (I, logstateStart) ;
                                LogProgEvent (I, FCurTitle + ' Start Ping to ' + FormatIpAddr (RemHost)) ;
                                result := true ;
                                FLogActive := true ;
                            end
                            else
                            begin
                                FLastErrorStr := FCurTitle + ' Ping Remote Failed' ;
                                LogErrEvent (0, FLastErrorStr) ;
                            end ;
                        end
                        else
                        begin
                            LogChangeState (I, logstateStart) ;
                            if NOT RemLookup then
                            begin
                                LogProgEvent (I, FCurTitle + ' Looking up Host Name ' + RemHost) ;
                                DnsLookup (RemHost) ;
                            end
                            else
                            begin
                                SocketDataConnect (I) ;
                            end;
                        end ;
                    end ;
                end ;
                FLogActive := true ;
                result := true ;
            end ;
        end
        else if FLogProtocol in [logprotUdpServer, logprotUdpServer6] then
        begin
            with FChanInfo [0] do
            begin
                with SendSocket do
                begin
                    OnDataSent := SocketDataSent ;  // 3 July 2014
                    StreamFlag := false ;
                    StreamNr := 0 ;
                    Proto := 'udp' ;
                    Tag := 0 ; // only single listen socket
                    Addr := FLocalIpAddr ;
                    Port := FLocalIpPort ;
                    SocketFamily := FSocFamily ;
                    ExclusiveAddr := true ;  // Oct 2016
                    Listen ;
                    if (FRcvBufSize <> SocketRcvBufSize) and (FRcvBufSize >= 1024) then
                                                             SocketRcvBufSize := FRcvBufSize ;  //  1.5 increase socket buffer size
                    if (FSndBufSize <> SocketSndBufSize) and (FSndBufSize >= 1024) then
                                                             SocketSndBufSize := FSndBufSize ;  //  1.5 increase socket buffer size
                end ;
                LogProgEvent (0, FCurTitle + ' Started on Address ' + FormatIpAddrPort (FLocalIpAddr, FLocalIpPort)) ;
                LogChangeState (0, logstateOK) ;
            end ;
            result := true ;
            FLogActive := true ;
        end
        else if FLogProtocol in [logprotTcpClient, logprotTcpClient6] then
        begin
            for I := 0 to Pred (tothosts) do
            begin
                with FChanInfo [I] do
                begin
                    if RemHost = '' then continue ;  // skip unused socket
                    with SendSocket do
                    begin
                        if Assigned (WaitTimer) then  // June 2018
                            WaitTimer.Enabled := false ;
                        OnSessionConnected := SocketSessionConnected ;
                        OnSessionClosed := SocketSessionClosed;
                        OnDnsLookupDone := SocketDataDnsLookupDone ; // 2 July 2014
                        OnDataSent := SocketDataSent ;  // 3 July 2014
                        SocketErrs := wsErrFriendly ; // Oct 2016
                        StreamFlag := false ;
                        StreamNr := 0 ;
                        if FKeepAliveSecs = 0 then
                            KeepAliveOnOff := wsKeepAliveOnSystem
                        else
                        begin
                            KeepAliveOnOff := wsKeepAliveOnCustom ;
                            KeepAliveTime := FKeepAliveSecs * Integer (TicksPerSecond) ;  // how often
                            if FKeepAliveSecs < 10 then
                                KeepAliveInterval := TicksPerSecond
                            else
                                KeepAliveInterval := (FKeepAliveSecs div 5) * Integer (TicksPerSecond);  // repeat if failed
                        end ;
                        Tag := I ; // keep socnr
                        RemLookup := WSocketIsIPEx (RemHost, MyFamily) ;  // 2 July 2014 true if real IP address
                        RemIP := RemHost ;
                        SslServerName := RemHost ;  // Nov 2016
                        if FCheckPing then
                        begin
                            if PingStart (I) then
                            begin
                                LogChangeState (I, logstateStart) ;
                                LogProgEvent (I, FCurTitle + ' Start Ping to ' + FormatIpAddr (RemHost)) ;
                            end
                            else
                            begin
                                FLastErrorStr := FCurTitle + ' Ping Remote Failed' ;
                                LogErrEvent (0, FLastErrorStr) ;
                            end ;
                        end
                        else
                        begin
                            LogChangeState (I, logstateStart) ;
                            if NOT RemLookup then
                            begin
                                LogProgEvent (I, FCurTitle + ' Looking up Host Name ' + RemHost) ;
                                DnsLookup (RemHost) ;
                            end
                            else
                            begin
                                SocketDataConnect (I) ;
                            end;
                        end ;
                    end ;
                end ;
                result := true ;
                FLogActive := true ;
            end ;
        end
        else if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
        begin
            CreateServerSocket;
            FServerTimer := TTimer.Create (Self);   // 5 July 2016 timer to check for idle timeouts
            FServerTimer.Interval := 5000 ;  // 5 seconds
            FServerTimer.OnTimer := ServerTimerTimer ;
            FServerTimer.Enabled := (FSrvTimeoutSecs > 10) ;
            with FListenSocket do
            begin
                MaxClients := FMaxSockets ;
                Banner := '' ;
                BannerTooBusy := 'Too Many Remote Connections' ;  // 4 Jan 2012
                ClientClass := TMagClientSocket ;
                onClientCreate := ServerClientCreate;
                OnClientConnect := ServerClientConnect;
                OnClientDisconnect := ServerClientDisconnect;
                OnBgException := SocketBgException ;
                IcsLogger := IpIcsLogger ;
                SocketErrs := wsErrFriendly ; // Oct 2016
                Proto := 'tcp' ;
                Tag := 0 ; // only single listen socket
                Addr := FLocalIpAddr ;
                Port := FLocalIpPort ;
                SocketFamily := FSocFamily ;
                ExclusiveAddr := true ;  // Oct 2016
                Listen ;      // start listening for incoming connections
            end ;
            for I := 0 to Pred (FMaxSockets) do  // each possible client
            begin
                LogChangeState (I, logstateStart) ;
                LogProgEvent (I, FCurTitle + ' Started on ' + FormatIpAddrPort (FLocalIpAddr, FLocalIpPort)) ;
            end ;
            result := true ;
            FLogActive := true ;
        end ;
    except
        FLastErrorStr := FCurTitle + ' Error Starting - ' + GetExceptMess (ExceptObject) ;
        LogErrEvent (0, FLastErrorStr) ;
    end ;
end ;

// stop everything, but note sockets may not have cleanly closed when
// this function exits, keep calling CheckStopped until it returns true

function TMagIpLog.StopLogging: boolean ;
var
    I: integer ;
begin
    result := false ;
    FLastErrorStr := '' ;
    if NOT FLogActive then
    begin
        result := true ;
        exit ;
    end ;
    FLogActive := false ;
    if FTotSockets <= 0 then exit ;
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if Assigned (FServerTimer) then FServerTimer.Enabled := false ;
        FreeAndNil (FServerTimer) ;
        if Assigned(FListenSocket) then
        begin
            try
                if FListenSocket.State <> wsClosed then FListenSocket.Close ;
                if FListenSocket.ClientCount > 0 then
                begin
                    for I := 0 to Pred (FListenSocket.ClientCount) do
                    begin
                        if FListenSocket.Client [I].State = wsConnected then
                        begin
                            LogChangeState (I, logstateStopping) ;
                            FListenSocket.Client [I].Close ;
                        end ;
                    end ;
                end ;
            except
            end ;
        end ;
        LogProgEvent (0, FCurTitle + ' Stopped') ;
    end ;
    for I := 0 to Pred (FTotSockets) do
    begin
        with FChanInfo [I] do
        begin
            try
                if (LogState <> logstateNone) then
                begin
                    if Assigned (SendSocket) then
                    begin
                        if Assigned (WaitTimer) then  // 8 July 2016
                            WaitTimer.Enabled := false ;
                        if (LogState = logstateStart) and (NOT RemLookup) then   // 4 July 2014
                        begin
                            try
                                SendSocket.CancelDnsLookup;
                            except
                            end;
                        end;
                        if SendSocket.State <> wsClosed then
                        begin
                            LogChangeState (I, logstateStopping) ;
                            SendSocket.Close ;
                            if NOT (FLogProtocol in [logprotTcpClient, logprotTcpClient6]) then   // 20 Aug 2009 event only if open
                                          LogProgEvent (I, FCurTitle + ' Stopped') ;
                        end ;
                    end ;
               //     LogChangeState (I, logstateNone) ;
                end ;
            except
            end ;
         //   LogState := logstateNone ;  // final clean-up
        end ;
    end ;
end ;

// see if sockets have all cleanly closed, then destroy them
// called after StopLogging before destroying component

function TMagIpLog.CheckStopped: boolean ;
var
    I: integer ;
begin
    result := false ;
    FLastErrorStr := '' ;
    if FLogActive then exit ;
    result := true ;
    if FTotSockets <= 0 then exit ;
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if Assigned(FListenSocket) then
        begin
            try
                if (FListenSocket.State <> wsClosed) or (FListenSocket.ClientCount > 0) then
                    result := false
                else
                    FreeAndNil (FListenSocket) ;
            except
            end ;
        end ;
    end ;
    for I := 0 to Pred (FTotSockets) do
    begin
        with FChanInfo [I] do
        begin
            try
                if Assigned (SendSocket) then
                begin
                    if SendSocket.State <> wsClosed then
                        result := false
                    else
                    begin
                        FreeAndNil (SendSocket) ;
                    end;
                end ;
            except
            end ;
        end ;
    end ;
end ;

procedure TMagIpLog.SocketBgException(Sender: TObject;
                          E: Exception; var CanClose: Boolean);
begin
    FLastErrorStr := FCurTitle + ' Fatal Error - ' + GetExceptMess (E) ;  // June 2018 
    LogErrEvent (0, FLastErrorStr) ;
    CanClose := true ;
end ;

// TCP/Client another connection attempt timer
// UDP/Client pings continually during connection to see if remote is lost

procedure TMagIpLog.WaitTimerTimer(Sender: TObject);
var
    socnr: integer;
begin
    socnr := (Sender as TTimer).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        WaitTimer.Enabled := false ;
        if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
        begin
            if NOT FCheckPing then exit ; // should not happen
        //    LogProgEvent (socnr, FCurTitle + ' Starting Ping to ' + IcsFormatIpAddr (RemHost)) ; // TEMP !!!
            if NOT PingStart (socnr) then
            begin
                 FLastErrorStr := FCurTitle + ' Ping Failed' ;
                 LogErrEvent (socnr, FLastErrorStr) ;
            end ;
        end
        else if FLogProtocol in [logprotTcpClient, logprotTcpClient6] then
        begin
            with SendSocket do
            begin
                try
                    Proto := 'tcp' ;
                    Tag := socnr ; // keep socnr
                    Addr := RemHost ;
                    Port := RemPort ;
                    LocalAddr := FLocalIpAddr ;
                    LocalPort := FLocalIpPort ;
                    if FCheckPing then
                    begin
                        if PingStart (socnr) then
                        begin
                            LogChangeState (socnr, logstateStart) ;
                        end
                        else
                        begin
                            FLastErrorStr := FCurTitle + ' Ping Failed' ;
                            LogErrEvent (socnr, FLastErrorStr) ;
                        end ;
                    end
                    else
                    begin
                        if NOT RemLookup then
                        begin
                            LogProgEvent (socnr, FCurTitle + ' Looking up Host Name ' + RemHost) ;
                            DnsLookup (RemHost) ;
                        end
                        else
                        begin
                            SocketDataConnect (socnr) ;
                        end;
                        LogChangeState (socnr, logstateStart) ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Starting - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (socnr, FLastErrorStr) ;
                end;
            end ;
        end ;
    end ;
end ;

// TCP and UDP client, start a ping

function TMagIpLog.PingStart (Socnr: integer): boolean ;
begin
    result := false ;
    if Socnr < 0 then exit ;
    try
        with FChanInfo [Socnr] do
        begin
            WaitTimer.Enabled := false ;
            with TPingThread.Create (True) do   // create suspended
            begin
                FreeOnTerminate := True;
                PingId := Socnr ;
                OnTerminate := PingThreadDone ;
                PingHostName := RemHost ;
                PingSocketFamily := BindFamily ;   // Nov 2013
                PingSrcAddress := ICS_ANY_HOST_V4 ;
                PingSrcAddress6 := ICS_ANY_HOST_V6 ;
                if FPingWaitSecs <= 0 then FPingWaitSecs := 10 ;
                PingTimeout := FPingWaitSecs * Integer (TicksPerSecond) ;
                PingTTL := 40 ;     // hops
                PingLookupReply := false ;  // reverse DNS
                Resume ; // start it now, with Delphi XE should use Start
                result := true ;
            end ;
        end ;
    except
    end ;
end ;

// TCP and UDP client - ping completed to remote client - success or fail

procedure TMagIpLog.PingThreadDone (Sender: TObject);
var
    socnr: integer;
begin
    socnr := 0 ;
    try
        with Sender as TPingThread do   // this event is thread safe
        begin
            socnr := PingId ;
            if (socnr < 0) or (socnr >= FTotSockets) then exit ;
            with FChanInfo [socnr] do
            begin
                WaitTimer.Enabled := false ;
                inc (AttemptNr) ;
                if LogState in [LogStateNone, LogStateStopping] then exit ;  // stopped already
                if ErrCode = 0 then
                begin
                    if NOT RemLookup then  // 2 July 2014  keep real remote IP address
                    begin
                        RemIp := DnsHostIP ;
                        BindFamily := PingSocketFamily ;
                        RemLookup := true ;
                        LogProgEvent (socnr, FCurTitle + ' Name Looked up OK, Address ' + RemIp) ;
                    end;
                    if DnsHostIP <> ReplyIPAddr then
                    begin
                        FLastErrorStr := FCurTitle + ' Ping Did Not Reach Host, Request IP ' +
                                   FormatIpAddr (DnsHostIP) + ', Reply IP ' + FormatIpAddr (ReplyIPAddr) ;
                        LogErrEvent (socnr, FLastErrorStr) ;
                        ErrCode := 99 ;
                    end ;
                end ;
                if ErrCode = 0 then
                begin
                    try
                        if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
                        begin
                            AttemptNr := 0 ;
                            if LogState <> logstateOK then
                            begin
                                SocketDataConnect (socnr) ;
                            end ;

                        // UDP keeps pinging to ensure remote host is still there
                            WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                            WaitTimer.Enabled := true ;
                        end
                        else if FLogProtocol in [logprotTcpClient, logprotTcpClient6] then
                        begin
                            with SendSocket do
                            begin
                                if State in [wsConnecting, wsConnected] then
                                begin
                                    FLastErrorStr := FCurTitle + ' !! Warning, Data Socket Already Connecting or Connecting to Server' ;
                                    LogErrEvent (socnr, FLastErrorStr) ;
                                    exit ;
                                end ;
                            end ;
                            SocketDataConnect (socnr) ;
                        end ;
                    except
                        FLastErrorStr := FCurTitle + ' Error Connection - ' + GetExceptMess (ExceptObject) ;
                        LogErrEvent (socnr, FLastErrorStr) ;
                    end ;
                end
                else

            // ping failed, may retry in a few seconds
                begin
                    if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
                    begin
                        if LogState = logstateOK then
                        begin
                            SendSocket.Close ;
                            LogChangeState (socnr, logstateStart) ;
                            FLastErrorStr := FCurTitle + ' Unexpected Disconnect - Attempting to Reconnect' ;
                            LogErrEvent (socnr, FLastErrorStr) ;
                        end ;
                    end ;
                    FLastErrorStr := FCurTitle + ' Failed Ping to ' + FormatIpAddr (RemHost) ;
                    if (AttemptNr >= FRetryAttempts) and (FRetryAttempts > 0) then
                    begin
                        LogChangeState (socnr, logstateNone) ;
                        FLastErrorStr := FLastErrorStr + ' After Total Retries ' + IntToStr (AttemptNr) ;
                    end
                    else
                    begin
                        LogChangeState (socnr, logstateStart) ;
                        FLastErrorStr := FLastErrorStr + ', Retrying in ' + IntToStr (FRetryWaitSecs) + ' secs' ;
                        WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                        WaitTimer.Enabled := true ;
                    end ;
                    LogErrEvent (socnr, FLastErrorStr) ;
                end ;
            end ;
        end ;
    except
        FLastErrorStr := FCurTitle + ' Error Ping Done - ' + GetExceptMess (ExceptObject) ;
        LogErrEvent (socnr, FLastErrorStr) ;
    end ;
end ;

// 2 July 2014, centralised all UDP and TCP Client open connections here

procedure TMagIpLog.SocketDataConnect (Socnr: integer);
var
    Success: Boolean ;
begin
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        if (LogState <> logstateStart) then
        begin
            FLastErrorStr := FCurTitle + ' Connection Cancelled, Stopping' ;
            LogErrEvent (0, FLastErrorStr) ;
            exit ;
        end;
        with SendSocket do
        begin
            try
                Tag := socnr ;
                if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
                begin
                    Proto := 'udp' ;
                    if SocketFamily = sfIPv6 then
                    begin
                        SocRemAddr6.sin6_family := AF_INET6 ;
                        SocRemAddr6.sin6_addr := TInAddr6 (WSocketStrToIPv6 (RemIP, Success)) ;
                        SocRemAddr6.sin6_port := WSocket_htons (atoi (RemPort)) ;
                    end
                    else
                    begin
                        SocRemAddr.sin_family := AF_INET ;
                        SocRemAddr.sin_addr.S_addr := WSocket_inet_addr (AnsiString (RemIP)) ;
                        SocRemAddr.sin_port := WSocket_htons (atoi (RemPort)) ;
                    end ;
                end
                else
                begin
                    Proto := 'tcp' ;
                end;
                Addr := RemIp ;
                LineMode := false;
                Port := RemPort ;
                SocketFamily := BindFamily ;
                LocalAddr := FLocalIpAddr ;
                LocalPort := FLocalIpPort ;
                Connect;
                if FLogProtocol in [logprotUdpClient, logprotUdpClient6] then
                begin
                    if (FRcvBufSize <> SocketRcvBufSize) and (FRcvBufSize >= 1024) then
                                                     SocketRcvBufSize := FRcvBufSize ;  //  1.5 increase socket buffer size
                    if (FSndBufSize <> SocketSndBufSize) and (FSndBufSize >= 1024) then
                                                     SocketSndBufSize := FSndBufSize ;  //  1.5 increase socket buffer size
                    LogProgEvent (socnr, FCurTitle + ' Connected OK to ' + FormatIpAddrPort (RemHost, RemPort)) ;
                    LogChangeState (socnr, logstateOK) ;
                    FLogActive := true ;
                end
                else
                begin
                    LogChangeState (socnr, logstateStart) ;
                    LogProgEvent (socnr, FCurTitle + ' Opening Connection to ' + FormatIpAddrPort (RemHost, RemPort)) ;
                end;
            except
                FLastErrorStr := FCurTitle + ' Error Starting - ' + GetExceptMess (ExceptObject) ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end ;
        end
    end;
end;

// 2 July 2014, TCP/IP Client, DNS lookup down

procedure TMagIpLog.SocketDataDnsLookupDone (Sender: TObject; Error: Word);
var
    MyFamily: TSocketFamily;
    socnr: integer;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        if Error = 0 then
        begin
            try
                if (LogState <> logstateStart) then
                begin
                    FLastErrorStr := FCurTitle + ' Connection Cancelled, Stopping' ;
                    LogErrEvent (0, FLastErrorStr) ;
                    exit ;
                end;
                with SendSocket do
                begin
                    if NOT RemLookup then
                    begin
                        RemIp := DnsResult ;   // keep DNS lookup
                        LogProgEvent (socnr, FCurTitle + ' Name Looked up OK, Address ' + RemIp) ;
                  // pending, may be multiple results, could choose randomly
                        if WSocketIsIPEx (RemIp, MyFamily) then BindFamily := MyFamily;
                    end;
                    RemLookup := true ;
                    SocketDataConnect (socnr) ;
                end ;
            except
                RemLookup := false ;
                FLastErrorStr := FCurTitle + ' Exception DNS Result - ' + GetExceptMess (ExceptObject) ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end ;
        end
        else
        begin
            RemLookup := false ;
            FLastErrorStr := FCurTitle + ' DNS Lookup Failed - ' + GetWinsockErr (Error) ;
            if (AttemptNr >= FRetryAttempts) and (FRetryAttempts > 0) then
            begin
                LogChangeState (socnr, logstateNone) ;
                FLastErrorStr := FLastErrorStr + ' After Total Retries ' + IntToStr (AttemptNr) ;
            end
            else
            begin
                LogChangeState (socnr, logstateStart) ;
                FLastErrorStr := FLastErrorStr + ', Retrying in ' + IntToStr (FRetryWaitSecs) + ' secs' ;
                WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                WaitTimer.Enabled := true ;
            end ;
            LogErrEvent (socnr, FLastErrorStr) ;
        end;
    end;
end;


// TCP/Client - session connected to remote client - success or fail

procedure TMagIpLog.SocketSessionConnected(Sender: TObject; Error: Word);
var
    socnr: integer;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        WaitTimer.Enabled := false ;
        if Error = 0 then
        begin
            with (Sender as TWSocket) do
            begin
                Counter.SetConnected;   // 4 July 2016
                AttemptNr := 0 ;
                BindIpAddr := GetXAddr ;  // keep local address and port
                BindIpPort := GetXPort ;
                if (FRcvBufSize <> SocketRcvBufSize) and (FRcvBufSize >= 1024) then
                                                          SocketRcvBufSize := FRcvBufSize ;  //  1.5 increase socket buffer size
                if (FSndBufSize <> SocketSndBufSize) and (FSndBufSize >= 1024) then
                                                          SocketSndBufSize := FSndBufSize ;  //  1.5 increase socket buffer size
            end ;
            if LogState = logstateOK then  // 23 July 2014
            begin
                LogProgEvent (socnr, FCurTitle + ' Already Connected') ;
                exit ;
            end;
            with TSslMagIpLog (Self) do
            begin
                if FForceSsl and Assigned (FLogSslContext) then
                begin
                    if LogState = logstateHandshake then   // 23 July 2014
                    begin
                        LogProgEvent (socnr, FCurTitle + ' Already Starting SSL Handshake') ;
                        exit ;
                    end;
                    with Sender as TSslWSocket do
                    begin
                        try
                            SslContext := fLogSslContext;
                            SslEnable := true ;
                            SslMode := sslModeClient ;
                            OnSslCliGetSession := TlsSslGetSession;
                            OnSslCliNewSession := TlsSslNewSession;
                            OnSslHandshakeDone := TlsSslHandshakeDone ;
                            OnSslVerifyPeer := TlsSslVerifyPeer ;
                        //    if loSslErr in IpIcsLogger.LogOptions then
                        //        OnSslProtoMsg := TlsSslProtoMsgEvent ;  // Dec 2016
                            LogChangeState (socnr, logstateHandshake) ;
                            LogProgEvent (socnr, FCurTitle + ' Starting SSL Handshake to Address ' +   // 23 July 2014, more info
                                                                        FormatIpAddrPort (RemIP, RemPort)) ;
                    //        StartSslHandshake;  // June 2018 done in TriggerSessionConnected
                        except
                            FLastErrorStr := FCurTitle + ' Error Starting SSL - ' + GetExceptMess (ExceptObject) ;
                            LogErrEvent (socnr, FLastErrorStr) ;
                        end ;
                    end ;
                end
                else
                begin
                    LogProgEvent (socnr, FCurTitle + ' Connected OK') ;
                    LogChangeState (socnr, logstateOK) ;
                end;
            end ;
       end
        else
        begin
            if (AttemptNr >= FRetryAttempts) and (FRetryAttempts > 0) then
            begin
                FLastErrorStr := FCurTitle + ' Failed Connection After Total Retries ' + IntToStr (AttemptNr) ;
                LogErrEvent (socnr, FLastErrorStr) ;
                LogChangeState (socnr, logstateNone) ;
            end
            else
            begin
                WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                WaitTimer.Enabled := true ;
                FLastErrorStr := FCurTitle + ' Failed Connection - ' + GetWinsockErr (Error) + ' - Retrying in ' +
                                                                              IntToStr (FRetryWaitSecs) + ' secs' ;
                LogErrEvent (socnr, FLastErrorStr) ;
                LogChangeState (socnr, logstateStart) ;
            end ;
        end ;
    end ;
end ;

// TCP/Client - session disconnected from remote client - deliberately or unexpected

procedure TMagIpLog.SocketSessionClosed(Sender: TObject; Error: Word);
var
    socnr: integer;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    GetPartialLine (Socnr) ;  // 3 July 2014 receive any partial line
    if (Error <> 0) then
    begin
        FLastErrorStr := FCurTitle + ' Disconnection Failed - ' + GetWinsockErr (Error) ;
        LogErrEvent (socnr, FLastErrorStr) ;
    end ;
    with FChanInfo [socnr] do
    begin
      // disconnection during capture is bad news, try and reconnect
        if (LogState in [logstateOK, logstateOKStream]) and FAutoReconnect then
        begin
            LogChangeState (socnr, logstateStart) ;
            WaitTimer.Enabled := false ;  // June 2018, reset timer
            inc (UnexpectedCount) ;
            if (UnexpectedCount <= 2) and (NOT FRetryNoImm) then // 7 July 2015 don't always do immediate retries
            begin
                WaitTimer.Interval := TicksPerSecond ; // almost immediate retry first two fails
                WaitTimer.Enabled := true ;
                FLastErrorStr := FCurTitle + ' Unexpected Disconnect - Attempting to Reconnect' ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end
            else
            begin
                WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                WaitTimer.Enabled := true ;
                FLastErrorStr := FCurTitle + ' Unexpected Disconnect - Retrying in ' + IntToStr (RetryWaitSecs) + ' secs' ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end ;
        end

     // disconnect during start-up should be retrying already, June 2018 don't need to do anything
        else if (LogState in [logstateStart]) then  
        begin
         {   WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
            WaitTimer.Enabled := true ;
            FLastErrorStr := FCurTitle + ' Failed Connection - Retrying in ' + IntToStr (RetryWaitSecs) + ' secs' ;
            LogErrEvent (socnr, FLastErrorStr) ;   }
        end
        else

     // planned disconnection
        begin
            if (Error = 0) then // June 2018 don't repeat error
            begin
                FLastErrorStr := FCurTitle + ' Disconnected' ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end;
            LogChangeState (socnr, logstateNone) ;
            UnexpectedCount := 0 ;
        end ;
    end ;
end ;

// TCP Server Protocol - build socket table with clients available
// warning - socnrs change dynamically as clients come and go

procedure TMagIpLog.CheckServerSockets ;
var
    I: integer;
begin
    FCurSockets := 0 ;
    if FListenSocket.ClientCount > 0 then
    begin
        for I := 0 to Pred (FListenSocket.ClientCount) do
        begin
            if FListenSocket.Client [I].Tag >= 0 then  // skip client about to be removed
            begin
         //       if LogState <> logstateHandshake then
         //           LogChangeState (FCurSockets, logstateOK) ;  // do we really need this ???
                FListenSocket.Client [I].Tag := FCurSockets ;
                inc (FCurSockets) ;
            end ;
        end ;
    end ;
    if FTotSockets > FMaxSockets then
    begin
        for I := FCurSockets to Pred (FMaxSockets) do
        begin
            if FLogActive then
                LogChangeState (I, logstateStart)
            else
                LogChangeState (I, logstateNone) ;
        end ;
    end ;
end ;

// TCP Server protocol - remote client created by SocketServer

procedure TMagIpLog.ServerClientCreate(Sender : TObject; Client : TWSocketClient);
begin
    Client.CreateCounter;          // 4 July 2016
    Client.Counter.SetConnected;   // 4 July 2016
end;

// TCP Server protocol - remote client connected to SocketServer

procedure TMagIpLog.ServerClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
var
    socnr: integer;
begin
    socnr := Pred (FListenSocket.ClientCount) ;

// too many clients, no logging, just ignore session it will be closed
    if (socnr < 0) or (socnr >= FTotSockets) then
    begin
        exit ;
    end ;
    with Client as TMagClientSocket do
    begin
        IpAddr := GetPeerAddr ;
        IpPort := GetPeerPort ;
        FChanInfo [socnr].BindIpAddr := GetXAddr ;  // keep local address and port
        FChanInfo [socnr].BindIpPort := GetXPort ;
        if (FRcvBufSize <> SocketRcvBufSize) and (FRcvBufSize >= 1024) then
                                                  SocketRcvBufSize := FRcvBufSize ;  //  1.5 increase socket buffer size
        if (FSndBufSize <> SocketSndBufSize) and (FSndBufSize >= 1024) then
                                                  SocketSndBufSize := FSndBufSize ;  //  1.5 increase socket buffer size
        if FForceSsl then
        begin
            LogChangeState (socnr, logstateHandshake) ;
            LogProgEvent (socnr, FCurTitle + ' Starting SSL Handshake from Address ' +
                                                                        FormatIpAddrPort (IpAddr, IpPort)) ;
        end
        else
        begin
            LogProgEvent (socnr, FCurTitle + ' Client Connected from Address ' + FormatIpAddrPort (IpAddr, IpPort)) ;
            LogChangeState (socnr, logstateOK) ;
        end;
        LineMode := false;
        OnDataAvailable := SocketDataAvailable ;
        OnBgException := SocketBgException;
        OnDataSent := SocketDataSent ;  // 3 July 2014
        Banner := '' ;
    end ;
    CheckServerSockets ;  // updates tags with socnr
end;

// TCP Server protocol - remote client disconnected from SocketServer

procedure TMagIpLog.ServerClientDisconnect(Sender: TObject;
                                 Client: TWSocketClient; Error: Word);
var
    socnr: integer;
begin
    socnr := Client.Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    GetPartialLine (Socnr) ;  // 3 July 2014 receive any partial line
    with FChanInfo [socnr] do
    begin
        Client.Tag := -1 ;  // mark as about to be removed
        LogChangeState (socnr, logstateStopping) ;
        if (FLogProtocol in [logprotTcpServer, logprotTcpServer6] ) and
                    (FListenSocket.ClientCount > FListenSocket.MaxClients) then   // 4 Jan 2012 better error for too many clients
            LogProgEvent (socnr, FCurTitle + ' Client Connection Refused, Too Many')
        else
            LogProgEvent (socnr, FCurTitle + ' Client Disconnected') ;
    end ;
    CheckServerSockets ;  // updates tags with socnr
end;

// TCP/Server check for idle client sessions

procedure TMagIpLog.ServerTimerTimer(Sender: TObject);
var
    CurTicks: Cardinal;
    I, socnr: Integer;
    Cli: TWSocketClient;
begin
    if FSrvTimeoutSecs < 10 then Exit ; // sanity check
    if NOT Assigned (FListenSocket) then  Exit ;  // sanity check Oct 2016
    if FListenSocket.ClientCount = 0 then Exit ;  // sanity check Oct 2016
    if not FHeartBeatBusy then  { Avoid reentrance }
    try
        FHeartBeatBusy := TRUE;
        CurTicks := IcsGetTickCount;
        for I := FListenSocket.ClientCount - 1 downto 0 do begin
            Cli := FListenSocket.Client[I];
            socnr := Cli.Tag ;
            if (IcsCalcTickDiff(Cli.Counter.LastAliveTick, CurTicks) >
                                             Cardinal (FSrvTimeoutSecs) * 1000) then
            begin
                LogProgEvent (socnr, FCurTitle + ' Client Idle Timeout After ' + IntToStr(FSrvTimeoutSecs)  + ' secs') ;
                FListenSocket.Disconnect(Cli);
            end;
        end;
    finally
        FHeartBeatBusy := FALSE;
    end;
end ;

// all clients and servers, see how many bytes of send data is not yet sent

function TMagIpLog.GetSendWaiting (Socnr: integer): integer ;    // 6 Sept 2016
begin
    result := -1 ;
    FLastErrorStr := '' ;
    if NOT FLogActive then exit ;
    if FTotSockets <= 0 then exit ;
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if (Socnr >= FListenSocket.ClientCount) then exit ;
        result := FListenSocket.Client [socnr].BufferedByteCount ;
    end
    else if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpServer,
                        logprotUdpClient6, logprotTcpClient6, logprotUdpServer6] then
    begin
        if (Socnr >= FMaxSockets) then exit ;
        with FChanInfo [socnr] do
        begin
            if (LogState = logstateOK) and (SendSocket.State = wsConnected) then
                result := SendSocket.BufferedByteCount ;
        end ;
    end
end;

// All clients and servers, send a line

function TMagIpLog.SendLogLine (const Line: Ansistring): boolean ;     // 8 Aug 2010
var
    I, len, tot: integer ;
    xmitline: AnsiString ;
    Success: boolean ;
begin
    result := false ;
    FLastErrorStr := '' ;
    if NOT FLogActive then
    begin
        FLastErrorStr := FCurTitle + ' Not Started' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;
    if FTotSockets <= 0 then exit ;
    if FAddCRLF and (NOT (FUdpNoCRLF and (FLogProtocol in
             [logprotUdpClient, logprotUdpServer, logprotUdpClient6, logprotUdpServer6]))) then
        xmitline := Line + #13#10
    else
        xmitline := Line ;
    len := Length (xmitline) ;
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if FListenSocket.ClientCount > 0 then
        begin
            for I := 0 to Pred (FListenSocket.ClientCount) do
            begin
                try // except
                    with FListenSocket.Client [I] do
                    begin
                        if State = wsConnected then
                        begin
                            if (len + BufferedByteCount) < FMaxSendBuffer then
                            begin
                                tot := Send (@xmitline [1], len) ;
                                if tot >= 0 then
                                    result := true
                                else
                                begin
                                    FLastErrorStr := FCurTitle + ' Failed to Send Data' ;
                                    LogErrEvent (0, FLastErrorStr) ;
                                end ;
                            end
                            else
                            begin
                                FLastErrorStr := FCurTitle + ' Buffer Full Sending Data' ;
                                LogErrEvent (0, FLastErrorStr) ;
                            end ;
                        end ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Send Data - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (0, FLastErrorStr) ;
                end ;
            end ;
        end ;
    end
    else if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpServer,
                        logprotUdpClient6, logprotTcpClient6, logprotUdpServer6] then
    begin
        for I := 0 to Pred (FMaxSockets) do
        begin
            with FChanInfo [I] do
            begin
                try
                    with SendSocket do
                    begin
                        if (LogState = logstateOK) and (State = wsConnected) then
                        begin
                            if (len + BufferedByteCount) < FMaxSendBuffer then
                            begin
                               // for UDP, check we already have an address, for clients this
                               // will have been set before connection, but for server we only
                               // know it once a packet has been received but try and use RemHost
                               // instead (of course this is a guess and assumes we know where the
                               // connection is coming from)
                                if FLogProtocol in [logprotUdpServer6, logprotUdpClient6] then
                                begin
                                    if (SocRemAddr6.sin6_addr.u6_addr32 [0] = 0) and
                                           (SocRemAddr6.sin6_addr.u6_addr32 [1] = 0) and
                                             (SocRemAddr6.sin6_addr.u6_addr32 [2] = 0) and
                                               (SocRemAddr6.sin6_addr.u6_addr32 [3] = 0) then
                                    begin
                                        SocRemAddr6.sin6_family := AF_INET6 ;
                                        SocRemAddr6.sin6_addr := TInAddr6 (WSocketStrToIPv6 (RemHost, Success)) ;
                                        SocRemAddr6.sin6_port := WSocket_htons (atoi (RemPort)) ;
                                    end ;
                                    tot := SendTo6 (SocRemAddr6, SizeOf (TSockAddrIn6), @xmitline [1], len) ;
                                end
                                else if FLogProtocol in [logprotUdpServer, logprotUdpClient] then
                                begin
                                    if SocRemAddr.sin_addr.S_addr = 0 then
                                    begin
                                        SocRemAddr.sin_family := AF_INET ;
                                        SocRemAddr.sin_addr.S_addr := WSocket_inet_addr (AnsiString (RemHost)) ;
                                        SocRemAddr.sin_port := WSocket_htons (atoi (RemPort)) ;
                                    end ;
                                    tot := SendTo (SocRemAddr, SizeOf (TSockAddrIn), @xmitline [1], len) ;
                                end
                                else
                                    tot := Send (@xmitline [1], len) ;
                                if tot >= 0 then
                                    result := true
                                else
                                begin
                                    FLastErrorStr := FCurTitle + ' Failed to Send Data - ' +
                                                                           GetWinsockErr (GetLastError) ;
                                    LogErrEvent (0, FLastErrorStr) ;
                                end
                            end
                            else
                            begin
                                FLastErrorStr := FCurTitle + ' Buffer Full Sending Data' ;
                                LogErrEvent (0, FLastErrorStr) ;
                            end ;
                        end ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Send Data - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (0, FLastErrorStr) ;
                end ;
            end ;
        end ;
    end ;
end ;

// send a stream - 3 Jul 2014

function TMagIpLog.SendStream (MyStream: TStream): boolean ;
var
    I: Integer ;
    Success: boolean ;
begin
    Result := false ;
    if NOT FLogActive then
    begin
        FLastErrorStr := FCurTitle + ' Not Started' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;
    if FTotSockets <= 0 then exit ;
    if NOT Assigned (MyStream) then
    begin
        FLastErrorStr := FCurTitle + ' No Send Stream Specified' ;
        LogErrEvent (0, FLastErrorStr) ;
        exit ;
    end ;

    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        if FListenSocket.ClientCount > 0 then
        begin
            for I := 0 to Pred (FListenSocket.ClientCount) do
            begin
                try // except
                    with FListenSocket.Client [I], FChanInfo [I] do
                    begin
                        if State = wsConnected then
                        begin
                            if StreamFlag then
                            begin
                                FLastErrorStr := FCurTitle + ' Still Sending Old Stream' ;
                                LogErrEvent (I, FLastErrorStr) ;
                                continue ;
                            end ;
                            FSendStream := MyStream ;
                            SetLength (FSendBuffer, FMaxSendBuffer) ;  // create send buffer to read stream
                            StreamFlag := true ;
                            StreamNr := 0 ;
                            LogProgEvent (I, FCurTitle + ' Sending Data Stream, Size ' +
                                                                            IntToCStr (FSendStream.Size));
                            LogChangeState (I, logstateOKStream) ;
                            SocketDataSent (FListenSocket.Client [I], 0); // send first block
                            Result := true ;
                        end ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Start Send Stream - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (I, FLastErrorStr) ;
                end ;
            end ;
        end ;
    end
    else if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpServer,
                        logprotUdpClient6, logprotTcpClient6, logprotUdpServer6] then
    begin
      // UDP only allows small packets to be sent
        if FLogProtocol in [logprotUdpClient,  logprotUdpServer,
                        logprotUdpClient6, logprotUdpServer6] then FMaxSendBuffer := 2048 ;  // 7 Sept 2016 was 512 ;
        for I := 0 to Pred (FMaxSockets) do
        begin
            with FChanInfo [I] do
            begin
                try
                    with SendSocket do
                    begin
                        if (LogState = logstateOK) and (State = wsConnected) then
                        begin
                            if StreamFlag then
                            begin
                                FLastErrorStr := FCurTitle + ' Still Sending Old Stream' ;
                                LogErrEvent (I, FLastErrorStr) ;
                                continue ;
                            end ;
                           // for UDP, check we already have an address, for clients this
                           // will have been set before connection, but for server we only
                           // know it once a packet has been received but try and use RemHost
                           // instead (of course this is a guess and assumes we know where the
                           // connection is coming from)
                            if FLogProtocol in [logprotUdpServer6, logprotUdpClient6] then
                            begin
                                if (SocRemAddr6.sin6_addr.u6_addr32 [0] = 0) and
                                       (SocRemAddr6.sin6_addr.u6_addr32 [1] = 0) and
                                         (SocRemAddr6.sin6_addr.u6_addr32 [2] = 0) and
                                           (SocRemAddr6.sin6_addr.u6_addr32 [3] = 0) then
                                begin
                                    SocRemAddr6.sin6_family := AF_INET6 ;
                                    SocRemAddr6.sin6_addr := TInAddr6 (WSocketStrToIPv6 (RemHost, Success)) ;
                                    SocRemAddr6.sin6_port := WSocket_htons (atoi (RemPort)) ;
                                end ;
                            end
                            else if FLogProtocol in [logprotUdpServer, logprotUdpClient] then
                            begin
                                if SocRemAddr.sin_addr.S_addr = 0 then
                                begin
                                    SocRemAddr.sin_family := AF_INET ;
                                    SocRemAddr.sin_addr.S_addr := WSocket_inet_addr (AnsiString (RemHost)) ;
                                    SocRemAddr.sin_port := WSocket_htons (atoi (RemPort)) ;
                                end ;
                            end ;
                            FSendStream := MyStream ;
                            SetLength (FSendBuffer, FMaxSendBuffer) ;  // create send buffer to read stream
                            StreamNr := 0 ;
                            StreamFlag := true ;
                            LogProgEvent (I, FCurTitle + ' Sending Data Stream, Size ' +
                                                                            IntToCStr (FSendStream.Size));
                            LogChangeState (I, logstateOKStream) ;
                            SocketDataSent (FChanInfo [I].SendSocket, 0); // send first block
                            Result := true ;
                        end ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Start Send Stream - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (I, FLastErrorStr) ;
                end ;
            end ;
        end ;
    end ;
end;

// client and server, send some data OK - 3 July 2014

procedure TMagIpLog.SocketDataSent(Sender: TObject; ErrCode : word);
var
    socnr, len, tot: integer;
begin
    if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        socnr := (Sender as TMagClientSocket).Tag;
        if (socnr < 0) or (socnr >= FTotSockets) then
        begin
            FLastErrorStr := FCurTitle + ' Error Send Data - Invalid Server' ;
            LogErrEvent (socnr, FLastErrorStr) ;
            exit ;
        end;
        with Sender as TMagClientSocket, FChanInfo [socnr] do
        begin
            try
                if (LogState <> logstateOKStream) or (State <> wsConnected) then exit ;  // non-stream data being sent
                if (NOT StreamFlag) or (NOT Assigned (FSendStream)) then
                begin
                    FLastErrorStr := FCurTitle + ' Failed to Send Stream' ;
                    LogErrEvent (0, FLastErrorStr) ;
                    StreamFlag := false ;
                    SetLength (FSendBuffer, 0);
                    LogChangeState (socnr, logstateOK) ;
                    exit ;
                end;
                FSendStream.Seek (StreamNr, soBeginning) ;
                len := FSendStream.Read (FSendBuffer [1], FMaxSendBuffer) ;
                StreamNr := StreamNr + len ;
                if len = 0 then
                begin
                    StreamFlag := false ;
                    SetLength (FSendBuffer, 0);
                    LogProgEvent (socnr, FCurTitle + ' Sent Data Stream OK, Size ' + IntToCStr (StreamNr));
                    LogChangeState (socnr, logstateOK) ;
                    exit ;
                end ;
                tot := Send (@FSendBuffer [1], len) ;
                if tot = 0 then
                begin
                    FLastErrorStr := FCurTitle + ' Failed to Send Stream Data' ;
                    LogErrEvent (socnr, FLastErrorStr) ;
                end ;
            except
                FLastErrorStr := FCurTitle + ' Error Send Stream Data - ' + GetExceptMess (ExceptObject) ;
                LogErrEvent (socnr, FLastErrorStr) ;
            end ;
        end ;
    end
    else if FLogProtocol in [logprotUdpClient, logprotTcpClient, logprotUdpServer,
                        logprotUdpClient6, logprotTcpClient6, logprotUdpServer6] then
    begin
        with Sender as TWSocket do
        begin
            socnr := Tag;
            if (socnr < 0) or (socnr >= FTotSockets) then
            begin
                FLastErrorStr := FCurTitle + ' Error Send Data - Invalid Client' ;
                LogErrEvent (socnr, FLastErrorStr) ;
                exit ;
            end;
            with FChanInfo [socnr] do
            begin
                try
                    if (LogState <> logstateOKStream) or (State <> wsConnected) then exit ;  // non-stream data being sent
                    if (NOT StreamFlag) or (NOT Assigned (FSendStream)) then
                    begin
                        FLastErrorStr := FCurTitle + ' Failed to Send Stream' ;
                        LogErrEvent (0, FLastErrorStr) ;
                        StreamFlag := false ;
                        SetLength (FSendBuffer, 0);
                        LogChangeState (socnr, logstateOK) ;
                        exit ;
                    end;
                    FSendStream.Seek (StreamNr, soBeginning) ;
                    len := FSendStream.Read (FSendBuffer [1], FMaxSendBuffer) ;
                    StreamNr := StreamNr + len ;
                    if len = 0 then
                    begin
                        StreamFlag := false ;
                        SetLength (FSendBuffer, 0);
                        LogProgEvent (socnr, FCurTitle + ' Sent Data Stream OK, Size ' + IntToCStr (StreamNr));
                        LogChangeState (socnr, logstateOK) ;
                        exit ;
                    end ;
                    if FLogProtocol in [logprotUdpServer6, logprotUdpClient6] then
                    begin
                        tot := SendTo6 (SocRemAddr6, SizeOf (TSockAddrIn6), @FSendBuffer [1], len) ;
                    end
                    else if FLogProtocol in [logprotUdpServer, logprotUdpClient] then
                    begin
                        tot := SendTo (SocRemAddr, SizeOf (TSockAddrIn), @FSendBuffer [1], len) ;
                    end
                    else
                        tot := Send (@FSendBuffer [1], len) ;
                    if tot = 0 then
                    begin
                        FLastErrorStr := FCurTitle + ' Failed to Send Data - ' +
                                                                   GetWinsockErr (GetLastError) ;
                        LogErrEvent (0, FLastErrorStr) ;
                    end ;
                except
                    FLastErrorStr := FCurTitle + ' Error Send Data - ' + GetExceptMess (ExceptObject) ;
                    LogErrEvent (0, FLastErrorStr) ;
                end ;
            end ;
        end ;
    end;
end;

// All Clients and Servers - remote is sending stuff to us

procedure TMagIpLog.SocketDataAvailable(Sender: TObject; Error: Word);
var
    socnr, I, len, AddrInLen: integer;
    RxSocket: TWSocket;
    Ch: AnsiChar ;
    process: boolean ;
begin
    socnr := 0 ;
    try  // except
        RxSocket := Sender as TWSocket ;
        socnr := RxSocket.Tag;
        if (socnr < 0) or (socnr >= FTotSockets) then
        begin
            RxSocket.Receive (@FRxBuffer, SizeOf (FRxBuffer));  // flush but ignore data
            FLastErrorStr := FCurTitle + ' Error Receive Data - Invalid Client' ;
            LogErrEvent (socnr, FLastErrorStr) ;
            exit ;
        end;
        with FChanInfo [socnr] do
        begin
          // warning - ReceiveFrom does not support SSL
            if FLogProtocol in [logprotUdpServer, logprotUdpClient] then
            begin
                AddrInLen := SizeOf (TSockAddrIn) ;
                len := RxSocket.ReceiveFrom (@FRxBuffer, SizeOf (FRxBuffer) - 1, SocRemAddr, AddrInLen) ;
                if len <= 0 then exit;
              // 1 Feb 2007, always keep UDP remote stuff, may be different for each packet
                PeerIpAddr := string (WSocket_inet_ntoa (SocRemAddr.sin_addr)) ;  // 8 Aug 2010
                PeerIpPort := IntToStr (WSocket_ntohs (SocRemAddr.sin_port)) ;
            end
            else if FLogProtocol in [logprotUdpServer6, logprotUdpClient6] then
            begin
                AddrInLen := SizeOf (TSockAddrIn6) ;
                len := RxSocket.ReceiveFrom6 (@FRxBuffer, SizeOf (FRxBuffer) - 1, SocRemAddr6, AddrInLen) ;
                if len <= 0 then exit;
                PeerIpAddr := WSocketIPv6ToStr (@SocRemAddr6) ;
                PeerIpPort := IntToStr (WSocket_ntohs (SocRemAddr6.sin6_port)) ;
            end
            else
            begin
                len := RxSocket.Receive (@FRxBuffer, SizeOf (FRxBuffer) - 1) ;  // supports SSL
                if len <= 0 then exit;
            end;
            for I := 0 to Pred (len) do
            begin
                Ch := FRxBuffer [I] ;
                process := false ;
                if Ch >= #20 then
                begin
                    inc (RxLineLen) ;
                    RxLineData [RxLineLen] := Ch  ;
                end
                else
                begin
                   { if (Ch = #12) then  // form feed Apr 2015 only CRFF
                    begin
                        process := true ;
                    end
                    else }
                    if LineEndType = lineendLF then
                    begin
                        if (Ch = #10) then process := true ;
                    end
                    else if LineEndType = lineendCR then
                    begin
                        if (Ch = #13) then process := true ;
                        if (Ch = #12) then  process := true ; // form feed, Apr 2015 only CRFF
                    end
                    else if LineEndType = lineendCRLF then  // April 2015
                    begin
                        if (Ch = #10) then
                        begin
                            if (RxLineLen > 0) and (RxLineData [RxLineLen] = #13) then
                            begin
                                process := true ;
                                if NOT FRawData then RxLineLen := RxLineLen - 1 ; // remove CR
                            end;
                        end;
                    end ;
                    if FRawData then
                    begin
                        inc (RxLineLen) ;
                        RxLineData [RxLineLen] := Ch  ;
                    end
                    else
                    begin
                        if Ch in [#10, #12, #13] then
                        begin
                            if (LineEndType <> lineendCRLF) or (Ch <> #13) then  // April 2015 CR removed later
                               Ch := #00
                        end
                        else if NOT FStripControls then
                            Ch := #32
                        else
                            Ch := #00 ;
                        if Ch > #00 then
                        begin
                            inc (RxLineLen) ;
                            RxLineData [RxLineLen] := Ch  ;
                        end ;
                    end ;
                end ;
                if RxLineLen >= FMaxLineLen then
                    process := true
                else if LineEndType = lineendCustom then
                begin
                    if (Ch = FLineEndChar) then process := true ;
                end
                else if LineEndType = lineendPacket then
                begin
                    if (I >= Pred (len)) then process := true ;
                end ;
                if FMaxRecvData > 0 then  // 12 Sept 2016 see if waiting for specific length of data, like HTTP content
                begin
                    if ((TotRecvData + RxLineLen) >= FMaxRecvData) then process := true ;
                end;
                if process then  // might be blank line
                begin
                    SetLength (LastLine, RxLineLen) ;
                    if RxLineLen > 0 then
                        Move (RxLineData [1], LastLine [1], RxLineLen) ;
                    TotRecvData := TotRecvData + RxLineLen ;    // 12 Sept 2016 how much data has been received
                    if Assigned (FonLogRecvEvent) then
                        FonLogRecvEvent (Self, Socnr, String (LastLine)) ;  // 8 Aug 2010
                    RxLineLen := 0 ;
                end ;
            end ;
        end ;
    except
        FLastErrorStr := FCurTitle + ' Error Receive Data - ' + GetExceptMess (ExceptObject) ;
        LogErrEvent (socnr, FLastErrorStr) ;
    end ;
end ;

// clears any partial received data

procedure TMagIpLog.ClearLine (Socnr: integer) ;
begin
    if (Socnr < 0) or (Socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        RxLineLen := 0 ;
        SetLength (LastLine, 0) ;
    end ;
end ;

// retrieve any partial received data

function TMagIpLog.GetPartialLine (Socnr: integer): string ;
begin
    result := '' ;
    if (Socnr < 0) or (Socnr >= FTotSockets) then exit ;
    with FChanInfo [socnr] do
    begin
        SetLength (LastLine, RxLineLen) ;
        if RxLineLen > 0 then
        begin
            Move (RxLineData [1], LastLine [1], RxLineLen) ;
            if Assigned (FonLogRecvEvent) then FonLogRecvEvent (Self, Socnr, String (LastLine)) ;  // 8 Aug 2010
        end ;
        RxLineLen := 0 ;
        result := String (LastLine) ;
    end ;
end ;

function TMagIpLog.GetTotRecvData (Socnr: integer): int64 ;  // 13 Sept 2016
begin
    Result := 0 ;
    if (Socnr < 0) or (Socnr >= FTotSockets) then exit ;
    Result := FChanInfo [socnr].TotRecvData ;
end ;

procedure TMagIpLog.ResetRecvData (Socnr: integer) ;  // 13 Sept 2016
begin
    if (Socnr < 0) or (Socnr >= FTotSockets) then exit ;
    FChanInfo [socnr].TotRecvData := 0 ;
end ;

// create SSL component

constructor TSslMagIpLog.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
end;

destructor TSslMagIpLog.Destroy;
begin
    FreeAndNil (FMsCertChainEngine) ;
    inherited Destroy;
end;

// SSL version of TCP Server

procedure TSslMagIpLog.CreateServerSocket;
begin
    FListenSocket := TSslWSocketServer.Create(Self);
    with FListenSocket do
    begin
        if FForceSsl and Assigned (FLogSslContext) then
        begin
            SslContext := fLogSslContext;
            SslMode := sslModeServer ;
        end;
        OnSslHandshakeDone := TlsSslHandshakeDone ;
        SslEnable := FForceSsl ;  // defaulted to true, must reset
     //   if loSslErr in IpIcsLogger.LogOptions then
     //       OnSslProtoMsg := TlsSslProtoMsgEvent ;  // Dec 2016
    end;
end;

// SSL server client create

procedure TSslMagIpLog.ServerClientCreate(Sender : TObject; Client : TWSocketClient);
begin
    with Client do
    begin
        OnSslVerifyPeer := TlsSslVerifyPeer ;
        OnSslHandshakeDone := TlsSslHandshakeDone ;
     //   if loSslErr in IpIcsLogger.LogOptions then
     //       OnSslProtoMsg := TlsSslProtoMsgEvent ;  // Dec 2016
    end;
    inherited ServerClientCreate(Sender, Client);
end;

// SSL server client connect

procedure TSslMagIpLog.ServerClientConnect(Sender : TObject; Client : TWSocketClient; Error : Word);
begin
    inherited ServerClientConnect(Sender, Client, Error);
end;

// SSL client, new session, see if re-using external cached session

procedure TSslMagIpLog.TlsSslNewSession(Sender: TObject; SslSession: Pointer;
                                                WasReused: Boolean; var IncRefCount : Boolean);
var
    socnr: integer;
begin
    if NOT Assigned (LogSslSessCache) then exit;
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    if (NOT WasReused) then
    begin
        with Sender as TSslWSocket do
            LogSslSessCache.CacheCliSession (SslSession, PeerAddr + PeerPort, IncRefCount);
        LogProgEvent (socnr, FCurTitle + ' SSL New Session');
    end
    else
        LogProgEvent (socnr, FCurTitle + ' SSL Session Reused');
end;

// SSL client, look-up session from external cache

procedure TSslMagIpLog.TlsSslGetSession(Sender: TObject;
                            var SslSession: Pointer; var FreeSession : Boolean);
begin
    if NOT Assigned (LogSslSessCache) then exit;
    with Sender as TSslWSocket do
        SslSession := LogSslSessCache.GetCliSession (PeerAddr + PeerPort, FreeSession);
end;

// SSL Handshaking done, if OK may check certificate chain
// called for both client and server, warning different senders

procedure TSslMagIpLog.TlsSslHandshakeDone(Sender: TObject;
          ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    socnr: integer;
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    Hash, info, VerifyInfo: String;
    Safe: Boolean;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    if Assigned(FOnHandshakeDone) then
        FOnHandshakeDone(Sender, ErrCode, PeerCert, Disconnect);

// client response, may need to check server certificate
    if FLogProtocol in [logprotTcpClient, logprotTcpClient6] then
    begin
        with Sender as TSslWSocket do
        begin
            if (ErrCode <> 0) or Disconnect then
            begin
                if (FChanInfo [socnr].AttemptNr >= FRetryAttempts) and (FRetryAttempts > 0) then // May 2015 retry on handshake errors
                begin
                    FLastErrorStr := FCurTitle + ' SSL Handshake Failed - ' + SslHandshakeRespMsg +
                                                ', After Total Retries ' + IntToStr (FChanInfo [socnr].AttemptNr) ;
                    LogChangeState (socnr, logstateNone) ;
                end
                else
                begin
                    if FChanInfo [socnr].LogState in [logstateNone, logstateStopping] then
                    begin
                        FLastErrorStr := FCurTitle + ' SSL Handshake Failed - ' + SslHandshakeRespMsg ;
                    end
                    else
                    begin
                        FChanInfo [socnr].WaitTimer.Interval := Longword (FRetryWaitSecs) * TicksPerSecond ;
                        FChanInfo [socnr].WaitTimer.Enabled := true ;
                        FLastErrorStr := FCurTitle + ' SSL Handshake Failed - ' + SslHandshakeRespMsg +
                                                   ' - Retrying in ' + IntToStr (FRetryWaitSecs) + ' secs' ;
                        LogChangeState (socnr, logstateStart) ;
                    end;
                end ;
                LogErrEvent (socnr, FLastErrorStr) ;
            //  Disconnect := TRUE;
                CloseDelayed; // Dec 2016
                exit;
            end  ;

            LogProgEvent (socnr, FCurTitle + space + SslHandshakeRespMsg) ;     // Dec 2014
            if SslSessionReused OR (FLogSslVerMethod = logSslVerNone) then
            begin
                LogChangeState (socnr, logstateOK) ;
                exit; // nothing to do, go ahead
            end ;

         // Is current host already in the list of temporarily accepted hosts ?
            Hash := PeerCert.Sha1Hex ;
            if SslAcceptableHosts.IndexOf (SslServerName + Hash ) > -1 then
            begin
                LogChangeState (socnr, logstateOK) ;
                exit; // nothing to do, go ahead
            end ;

         // Property SslCertChain contains all certificates in current verify chain
            CertChain := SslCertChain;

         // see if validating against Windows certificate store
            if FLogSslVerMethod = logSslVerWinStore then
            begin

                // start engine
                if not Assigned (FMsCertChainEngine) then
                    FMsCertChainEngine := TMsCertChainEngine.Create;

              // see if checking revoocation, CRL checks and OCSP checks in Vista+, very slow!!!!
                if FLogSslRevocation then
                    FMsCertChainEngine.VerifyOptions := [mvoRevocationCheckChainExcludeRoot]
                else
                    FMsCertChainEngine.VerifyOptions := [];

                // This option doesn't seem to work, at least when a DNS lookup fails
                FMsCertChainEngine.UrlRetrievalTimeoutMsec := 10 * TicksPerSecond;

                { Pass the certificate and the chain certificates to the engine      }
                FMsCertChainEngine.VerifyCert (PeerCert, CertChain, ChainVerifyResult, True);

                Safe := (ChainVerifyResult = 0) or
                        { We ignore the case if a revocation status is unknown.      }
                        (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN) or
                        (ChainVerifyResult = CERT_TRUST_IS_OFFLINE_REVOCATION) or
                        (ChainVerifyResult = CERT_TRUST_REVOCATION_STATUS_UNKNOWN or
                                             CERT_TRUST_IS_OFFLINE_REVOCATION);

               { The MsChainVerifyErrorToStr function works on chain error codes     }
                VerifyInfo := MsChainVerifyErrorToStr (ChainVerifyResult); // Nov 2016

            // MSChain ignores host name, so see if it failed using OpenSSL
                if PeerCert.VerifyResult = X509_V_ERR_HOSTNAME_MISMATCH then begin  // Nov 2016
                    Safe := False;
                    VerifyInfo := PeerCert.FirstVerifyErrMsg;
                 end;
            end
            else if FLogSslVerMethod = logSslVerBundle then
            begin
                VerifyInfo := PeerCert.FirstVerifyErrMsg;   // Nov 2016
                Safe := (PeerCert.VerifyResult = X509_V_OK);   { check whether SSL chain verify result was OK }
            end
            else
            begin
                exit ;  // unknown method
            end ;

        // allow self signed certs
            if (CertChain.Count > 0) and (CertChain[0].FirstVerifyResult = X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN) then
            begin
                Safe := true ;
                LogProgEvent (socnr, FCurTitle + ' SSL Self Signed Certificate Succeeded: ' +
                                                                  IcsUnwrapNames (PeerCert.IssuerCName));
            end;

         // tell user verification failed
            if NOT Safe then
            begin
                info := ' SSL Chain Verification Failed: ' + VerifyInfo + ', Domain: ';
                if PeerCert.SubAltNameDNS = '' then
                    info := info + IcsUnwrapNames (PeerCert.SubjectCName)
                else
                    info := info + IcsUnwrapNames (PeerCert.SubAltNameDNS) ;
                info := info + ', Expected: ' + SslServerName ;  // Nov 2016
                LogProgEvent (socnr, FCurTitle + info);
            end

         // check certificate was issued to remote host for out connection
            else
            begin
                   LogProgEvent (socnr, FCurTitle + ' SSL Chain Verification Succeeded, Domain: ' + SslCertPeerName);
            end;

       // if certificate checking failed, see if the host is specifically listed as being allowed anyway
            if (NOT Safe) and (SslAcceptableHosts.IndexOf (SslServerName) > -1) then
            begin
                Safe := true ;
                SslAcceptableHosts.Add (SslServerName + Hash);  // keep it to avoid checking again
                LogProgEvent (socnr, FCurTitle + ' SSL Succeeded with Acceptable Host Name');
            end ;

          // tell user about all the certificates we found
            if FLogSslReportChain and (CertChain.Count > 0) then
            begin
                if FLogSslVerMethod = logSslVerWinStore then
                     info := 'Verify Result: ' + MsCertVerifyErrorToStr (CertChain[0].CustomVerifyResult) + #13#10
                else
                     info := 'Verify Result: ' + CertChain[0].FirstVerifyErrMsg + #13#10 ;
                info := info + IntToStr (CertChain.Count) + ' SSL Certificates in the verify chain:'+ #13#10 +
                                CertChain.AllCertInfo (true, true) ; // Feb 2017 report all certs, backwards
                LogProgEvent (socnr, FCurTitle + ' ' +  info);
            end;

          // all failed
            if NOT Safe then
            begin
                Disconnect := TRUE;
                exit ;
            end;
        end;
    end

// server currently ignores a client certificate, not often used
    else if FLogProtocol in [logprotTcpServer, logprotTcpServer6] then
    begin
        with Sender as TMagClientSocket do
        begin
            // nothing much to do if SSL failed or event said disconnect
            if (ErrCode <> 0) or Disconnect then
            begin
                FLastErrorStr := FCurTitle + ' SSL Handshake Failed - ' + SslHandshakeRespMsg ;
                LogErrEvent (socnr, FLastErrorStr) ;
                Disconnect := TRUE;
                exit;
            end  ;
            LogProgEvent (socnr, FCurTitle + space + SslHandshakeRespMsg) ;
//            LogProgEvent (socnr, FCurTitle + ' SSL Connected OK with ' + SslVersion + ', cipher ' + SslCipher) ;
        end;
    end
    else
        LogProgEvent (socnr, FCurTitle + ' SSL Connected OK') ;
    LogChangeState (socnr, logstateOK) ;
end;

// SSL Verify Peer, gets called for each certificate in chain, if being checked

procedure TSslMagIpLog.TlsSslVerifyPeer(Sender: TObject; var Ok: Integer; Cert: TX509Base);
var
    socnr: integer ;
    info: string ;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    OK := 1; // don't check certificate until handshaking over
    if LogRcvdCerts then
    begin
         info := 'Received Certificate, Depth ' + IntToStr (Cert.VerifyDepth) + #13#10 +
                 'Verify Result: ' + Cert.VerifyErrMsg + #13#10 +
                 Cert.CertInfo + #13#10 ;  // Apr 2016
        LogProgEvent (socnr, FCurTitle + ' ' +  info);
    end;
end;

// Dec 2016 log SSL protocol messages for debugging

procedure TSslMagIpLog.TlsSslProtoMsgEvent(Sender: TObject; Info: String; Sending:
            integer; Version: integer; ContentType: integer; Buffer: PAnsiChar; BuffSize: integer) ;
var
    socnr: integer;
begin
    socnr := (Sender as TWSocket).Tag ;
    if (socnr < 0) or (socnr >= FTotSockets) then exit ;
    LogProgEvent (socnr, FCurTitle + ' SSL ProtoMsg: ' + info +  ', DataLen=' +
            IntToStr (BuffSize) + ', Data=' + IcsBufferToHex (Buffer[0], BuffSize)) ;
end;

end.

