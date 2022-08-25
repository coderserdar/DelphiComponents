unit MagentaMailQueue;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 22nd June 2018
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Magenta Systems Mail Queue Component v2.5
-----------------------------------------

Overview
--------

Magenta Systems Mail Queue Component has two main benefits over a simple
TSslSmtpCli component: it supports extended retries over many hours or days,
and supports multiple SMTP relay servers, while alleviating the need for
the application to handle retries.  The component also allows HTML mail to
be sent using SSL, something THtmlSmtpCli does not currently support. Mail
is queued to disk, so retries will continue if the application is restarted.

TMagMailQueue needs François PIETTE internet component suite version 8.19 or
later from http://wiki.overbyte.be/wiki/index.php/ICS_Download

TMagMailQueue is designed to prepare, queue and send email.  Preparing the email
is done using the the ICS THtmlSmtpCli component so it may be plain text or HTML
email with one or more file attachments.

Once the mail properties in QuHtmlSmtp have been specified, it is queued using
the QueueMail method which saves it to an EML spool file.

The component runs a thread which checks the queue for new EML spool files, and
attempts to forward them to one or more SMTP Mail Servers using TSslSmtpCli,
optionally with SSL. If mail delivery succeeds, the spool file may be deleted or
moved to an archive folder.  If mail delivery fails, the spool file remains in
the queue and further attempts are made separated by the times in minutes
listed in the RetryList list.  If all delivery attempts fail, the spool file
may be deleted or moved to a badmail folder.

Note that some email servers support grey listing and reject the first email
attempt from a new sender but allow a retry 10 or 15 minutes later, something
that is very effective in blocking spam emails (since they don't usually retry).

If multiple mail servers are specified, delivery is attempted once using
each server, for each retry attempt.  Each mail server is specified as
TMailServer and there is no limit to the total.

Each time the queue is updated or a delivery attempt made, the queue is saved to
file in the control folder, so the component may be stopped and restarted with
failed attempts continuing.

The EML spool files are compatible with those created by many Microsoft email
applications such as CDO, and the AddtoQueue method can also be used to queue
existing EML files with the queue details specified in MailQuItem.

Note, this component is intended for sending low volume email from individual
Delphi applications, with more flexibility than a simple TSslSmtpCli component.
For use as a heavy duty SMTP server, queue processing could be improved to
avoid moving records around as much or saving them to disk as often, and mail
bodies could be read as required from disk instead of being read entirely to
memory first.  A mail pickup folder could be added which is scanned for new
EML files.


Files and Folders Used
----------------------

The TMagMailQueue component heavily uses disk files, in different sub-directories
within the mail root directory specified in property MailQuDir, these are:

- control - contains MailQuItems.Ctl a single row file with the next message item
            number, and MailQuItems.Hdr which is a CSV file containing one row for
            each mail item still in the queue.
- spool   - contains any queued email files, named in the format item00000001.eml
            with the number increasing, taken from MailQuItems.Ctl
- archive - if ArchiveSent property is true, once an email has been successfully
            sent it is moved into the archive directory
- badmail - if DeleteFailed property is false, once an email has exceeded all the
            retry attempts it is moved into the badmail directory, from where it
            may be manually requeued if necessary

A demo application mailqudemo.exe illustrates simple email queuing.

Release 1.0 - 18th Jan 2011 - baseline
Release 1.1 - 21st Jan 2011 - TLogEvent is unicode for compatiblity with other components
                              support queuing mail with OwnHeaders bypassing htmlmail
Release 1.2 - 22nd Mar 2011 - TLogEvent is ascii again, and renamed
Release 1.3 - 5th Oct 2011  - Debug logging works properly
                              Don't retry emails that fail too large for server (error 552)
Release 1.4 - 11th Sept 2012 - ICS V8, IPv6, fixed a cast
Release 1.5 - 23rd Mar 2013  - added Mail Server SocketFamily and LocalAddr6 for IPv6
Release 1.6 - 10th Dec 2014  - better SSL handshake reporting
Release 2.0 - 27th Oct 2015  - check and report SSL certificates using PEM file or Windows Cert Store
                               allow three SMTP servers to be specified for each email in queue
                               lookup DNS MX records and send to those SMTP servers
                               queue keeps last response or error in queue
                               mail completed log (same CSV format as queue)
                               queue changed event to tell client something is happening
                               QueueMail method now returns item number (not boolean)
                               new UnQueueMail method to remove item number from queue
Release 2.1 - 7th July 2016  - support SSL enhancements in ICS for OpenSSL 1.1.0
                               don't change SSL directory, let application control it
                               use default SSL root bundle if none specified
Release 2.2 - 24th Nov 2016    better error handling
                               use OpenSSL host checking
                               fixed bug that meant failed email was not deleted from queue
                               don't queue email without recipients
Release 2.3 - 6 Mar 2017       simplified SSL certificate reporting
Release 2.4 - 11 Mar 2017      added WaitSend to wait until everything sent
Release 2.5 - 22 Jun 2018      added RetryWithoutSsl which retries an SSL failure without SSL
                               added SslCliSecurity to set client security level
                               using IcsWndControl for threaded message handling
                               SendSmtpClient now created new for each attempt in case
                                 of prior faillure causing terminal corruption


                               if SSL certificate verify fails, next attempt is another server ??

pending use local SslContext, and Root store

warning - if using MX DNS servers and multiple recipients, need to queue mail multiple times !!!!
this will be fixed real soon
}

interface

{$I Include\OverbyteIcsDefs.inc}

uses
    Forms, Classes, Windows, Sysutils, Messages,
    magsubs1, magsubs4, Magclasses,
    OverbyteIcsWSocket, OverbyteIcsSmtpProt, OverbyteIcsWndControl,
    OverbyteIcsLIBEAY, OverbyteIcsSSLEAY, OverbyteIcsSslX509Utils,
    OverbyteIcsMsSslUtils, OverbyteIcsWinCrypt, OverbyteIcsDnsQuery,
    OverbyteIcsUtils ;

type
    TMailLogLevel = (MLogLevelInfo, MLogLevelFile, MLogLevelProg, MLogLevelDiag,
                 MLogLevelDelimFile, MLogLevelDelimTot) ;
    TMailVerifyMethod = (MailSslVerNone, MailSslVerBundle, MailSslVerWinStore) ;  // Oct 2015 server wide
    TMailSmtpMethod = (MailSmtpRelay, MailSmtpSpecific, MailSmtpMXLookup) ;       // Oct 2015 each email

// copying event, allowing main program to log and display stuff, and cancel
    TMailLogEvent = Procedure (LogLevel: TMailLogLevel ; const Info: String) of object ;

const
    MaxSmtpSrv = 3 ;       // Oct 2015 maximum specific SMTP servers in queue
    MXWaitSecs = 4 ;       // DNS MX lookup wait for response in seconds
    MXRetryAttempts = 4 ;  // DNS MX lookup retry attempts on failure, sequential DNS servers
    MailSmtpMethNames: array [TMailSmtpMethod] of PChar = ('Relay Servers', 'Specific Servers', 'Look-Up MX Servers');
    MaxAttempts = 999 ;    // Nov 2016 queue item deleted once this reached

type
  { TMailServer }

  TMailServer = class(TCollectionItem)
  private
    FSslType: TSmtpSslType;
    FPort: String;
    FPassword: String;
    FHost: String;
    FLocalAddr: String;
    FLocalAddr6: String;
    FAuthType: TSmtpAuthType;
    FUsername: String;
    FSignOn: String;
    FSocketFamily: TSocketFamily;
    FRetryWithoutSsl: Boolean;         // June 2018
    FSslCliSecurity: TSslCliSecurity;  // June 2018
  protected
    function GetDisplayName: string; override;
  published
    constructor Create (Collection: TCollection); Override ;
    property Host : String                       read  FHost
                                                 write FHost;
    property Port : String                       read  FPort
                                                 write FPort;
    property Username : String                   read  FUsername
                                                 write FUsername;
    property Password : String                   read  FPassword
                                                 write FPassword;
    property SignOn : String                     read  FSignOn
                                                 write FSignOn;
    property AuthType : TSmtpAuthType            read  FAuthType
                                                 write FAuthType;
    property SslType  : TSmtpSslType             read  FSslType
                                                 write FSslType;
    property LocalAddr : String                  read  FLocalAddr
                                                 write FLocalAddr;
    property LocalAddr6 : String                 read  FLocalAddr6
                                                 write FLocalAddr6;
    property SocketFamily: TSocketFamily         read FSocketFamily
                                                 write FSocketFamily;
    property RetryWithoutSsl: Boolean            read  FRetryWithoutSsl
                                                 write FRetryWithoutSsl ;  // June 2018
    property SslCliSecurity: TSslCliSecurity     read  FSslCliSecurity
                                                 write FSslCliSecurity ;   // June 2018
  end;

  { TMailServers }

  TMailServers = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TMailServer;
    procedure SetItem(Index: Integer; Value: TMailServer);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    property Items[Index: Integer]: TMailServer read GetItem write SetItem; default;
  end;

{ TmailQuItem }
  TMailQuItem = record
    ItemNr: integer ;  // incrementing, used in file name
    NextAttemptDT: TDateTime ;  // set to queued time
    AttemptNr: integer ;
    FName: string ;
    XReceivers: string ; // CSV list
    XSender: string ;
    Subject: string ;
    Priority: TSmtpPriority ;
    QueuedDT: TDateTime ;
    LastAttemptDT: TDateTime ;
    BodySize: integer ;
    LastResp: string ;           // Oct 2015
    SmtpMeth: TMailSmtpMethod ;  // Oct 2015
    SmtpSrvs: array [0..MaxSmtpSrv - 1] of string ; // Oct 2015
    SmtpSrvTot: Integer ;  // not saved
  end;
  PMailQuItem = ^TMailQuItem ;

const
    mqItemNr = 0 ; mqNextAttemptDT = 1 ; mqAttemptNr = 2 ; mqFName = 3 ;
    mqXReceiver = 4 ; mqXSender = 5 ; mqSubject = 6 ; mqPriority = 7 ;
    mqQueuedDT = 8 ; mqLastAttemptDT = 9 ; mqBodySize = 10 ; mqLastResp = 11 ;
    mqSmtpMeth = 12 ; mqSmtpSrv1 = 13 ; mqSmtpSrv2 = 14 ; mqSmtpSrv3 = 15 ;
    {mqLastOldField = 10 ; } mqLastField = 15 ;

type

  { TMailQuThread }

  TMagMailQueue = class ;
  TMailThreadEvent = Procedure (LogLevel: TMailLogLevel ; const Info: string) of object ;

  TMailQuThread = class(TThread)
  private
//    SendSmtpClient: TSslSmtpCli;  // June 2018  now local
    DnsQuery: TDnsQuery;
    FAttemptTimes: array of integer ;
    FAttemptTot: integer ;
    FMagMailQueue: TMagMailQueue ;
    FLogLevel: TMailLogLevel ;
    FInfo: string ;
    FMailQuItem: TMailQuItem ;
    FBodyLines: TStringList ;
    FHdrDone: boolean ;
    FDNSReqId: Integer ;
    FHandshakeDone: Boolean;   // June 2018
    FSkipSsl: Boolean;         // June 2018
    FIcsWndControl: TIcsWndControl;  // June 2018
    FCurSmtpSrv: String ;      // June 2018
    procedure SetName;
  protected
    FThreadEvent: TMailThreadEvent ;
    procedure Execute; override;
    function SendOneEmail (Servnr: integer): boolean ;
    procedure ThreadLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
    procedure CallThreadEvent ;
    procedure SmtpClientDisplay(Sender: TObject; Msg: string);
    procedure SmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: Pointer; MaxLen: Integer; var More: Boolean);
    procedure SmtpSslHandshakeDone(Sender: TObject;
      ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure DnsQueryRequestDone(Sender: TObject; Error: Word);
  end;

{ TMagMailQueue }

  TMagMailQueue = class(TComponent)
  private
    { Private declarations }
    FQuHtmlSmtp: THtmlSmtpCli;
    FMailServers: TMailServers ;
    FSslContext: TSslContext ;
    FMailQuThread: TMailQuThread ;
    FQuThreadRunning: boolean ;
    FQuThreadStopping: boolean ;
    FQuItemsCritSect: TRTLCriticalSection ;
    FFileQuItemsHdr: string;
    FFileQuItemsCtl: string;
    FMailQuDir: string;
    FRetryList: string;
    FActive: boolean;
    FArchiveSent: boolean;
    FDeleteFailed: boolean;
    FLogEvent: TMailLogEvent;
    FDebug: boolean;
    FBodyText: TStringList ;
    FQuStartDelay: integer;
    FSendInProgress: integer ; // Oct 2015 was boolean
    FBodyDebug: boolean ;
    FTrustedList: String;   // following Oct 2015
    FSslVerMethod: TMailVerifyMethod;
    FSslRevocation: boolean;
    FSslReportChain: boolean ;
    FMsCertChainEngine: TMsCertChainEngine;
    FSmtpMethod: TMailSmtpMethod ;  // following Oct 2015
    FFileQuSent: string;
    FLogQuSent: boolean ;
    FMxSrvUseSsl: boolean ;
    FDnsServers: TStrings ;
    FLocalAddr: String;
    FLocalAddr6: String;
    FSocketFamily: TSocketFamily;
    FQuChangedEvent: TNotifyEvent;
    procedure SetMailQuDir(const Value: string);
    procedure SetRetryList(const Value: string);
    procedure SetActive(const Value: boolean);
    procedure SetArchiveSent(const Value: boolean);
    procedure SetDeleteFailed(const Value: boolean);
    procedure SetLogEvent(const Value: TMailLogEvent);
    procedure onThreadEvent (LogLevel: TMailLogLevel ; const Info: String) ;
    procedure OnThreadTerminate (Sender: TObject) ;
    procedure SetMailServers(const Value: TMailServers);
    procedure SetDebug(const Value: boolean);
    procedure SetQuHtmlSmtp(const Value: THtmlSmtpCli);
    procedure SmtpClientAttachContentTypeEh(Sender: TObject; FileNumber: Integer; var FileName, ContentType: string;
      var AttEncoding: TSmtpEncoding);
    procedure SetQuStartDelay(const Value: integer);
  protected
    { Protected declarations }
    procedure SaveQuHdrs ;
    procedure ReadQuHdrs ;
    procedure BuildQuIdx ;
    procedure RemoveQuItem (item: integer) ;
    function NewMailSeq: integer ;
  public
    { Public declarations }
    MailQuItems: array of TMailQuItem ;  // one record per mail queue item
    MailQuIdx: TFindList ;               // sorted index to mail queue items, by NextAttemptDT
    MailTotItems: integer ;              // total items in queue (array may be larger)
    MailImmItems: integer ;              // immediate items in the queue to send (not yet requeued)
    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    function StartMailQu: boolean ;
    function StopMailQu: boolean ;
    function WaitSend (secs: integer; Any: Boolean): boolean;    // 11 March 2017
    function WaitSendandStop (secs: integer): boolean;
    function QueueMail (const Srv1: string = ''; const Srv2: string = ''; const Srv3: string = ''): integer ;
    function AddtoQueue (MailQuItem: TMailQuItem): boolean ;
    function ClearQueue: boolean ;
    function RebuiltQueue: boolean ;
    function UnQueueMail (item: integer): boolean ;
    function SaveOneHdr (Item: TMailQuItem): string ;
  published
    { Published declarations }
    property QuHtmlSmtp: THtmlSmtpCli read FQuHtmlSmtp write SetQuHtmlSmtp;
    property MailServers: TMailServers  read FMailServers write SetMailServers;
    property SslContext: TSslContext   read FSslContext write FSslContext;
    property Active: boolean  read FActive write SetActive;
    property ArchiveSent: boolean  read FArchiveSent write SetArchiveSent;
    property DeleteFailed: boolean  read FDeleteFailed write SetDeleteFailed;
    property Debug: boolean  read FDebug write SetDebug;
    property BodyDebug: boolean  read FBodyDebug write FBodyDebug ;
    property RetryList: string  read FRetryList write SetRetryList;
    property MailQuDir: string  read FMailQuDir write SetMailQuDir;
    property QuStartDelay: integer  read FQuStartDelay write SetQuStartDelay;
    property TrustedList: String  read  FTrustedList write FTrustedList;
    property SslVerMethod: TMailVerifyMethod  read FSslVerMethod write FSslVerMethod;
    property SslRevocation: boolean  read FSslRevocation write FSslRevocation;
    property SslReportChain: boolean  read FSslReportChain write FSslReportChain;
    property SmtpMethod: TMailSmtpMethod  read FSmtpMethod write FSmtpMethod;
    property FileQuSent: string  read FFileQuSent write FFileQuSent ;
    property LogQuSent: boolean  read FLogQuSent write FLogQuSent ;
    property MxSrvUseSsl: boolean  read FMxSrvUseSsl write FMxSrvUseSsl;
    property DnsServers: TStrings read FDnsServers write FDnsServers;
    property LocalAddr : String  read FLocalAddr write FLocalAddr;
    property LocalAddr6 : String  read FLocalAddr6  write FLocalAddr6;
    property SocketFamily: TSocketFamily  read FSocketFamily write FSocketFamily;
    property SendInProgress: integer  read FSendInProgress ;
    property QuThreadRunning: boolean  read FQuThreadRunning ;
    property QuThreadStopping: boolean  read FQuThreadStopping ;
    property LogEvent: TMailLogEvent  read FLogEvent write SetLogEvent ;
    property QuChangedEvent: TNotifyEvent  read FQuChangedEvent write FQuChangedEvent ;
    procedure DoLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
  end;


  procedure Register;

implementation

type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagMailQueue]);
end;

// delete a single file, optionally read only

function MagDeleteFile (const Fname: string; const ReadOnly: boolean): Integer ;
var
    attrs: integer ;
begin
    result := -1 ;    // file not found
    attrs := FileGetAttr (Fname) ;
    if attrs < 0 then exit ;
    if ((attrs and faReadOnly) <> 0) and ReadOnly then
    begin
        result := FileSetAttr (Fname, 0) ;
        if result <> 0 then result := 1 ;
        if result <> 0 then exit ;  // 1 could not change file attribute, ignore system error
    end ;
    if DeleteFile (Fname) then
        result := 0   // OK
    else
        result := GetLastError ; // system error
end ;

// rename a single file, optionally replacing, optionally read only

function MagRenameFile (const OldName, NewName: string;
                                        const Replace, ReadOnly: boolean): Integer ;
begin
    if FileExists (NewName) then
    begin
        result := 2 ;  // rename failed, new file exists
        if NOT Replace then exit ;
        result := MagDeleteFile (NewName, ReadOnly) ;
        if result <> 0 then exit ;  // 1 could not change file attribute, higher could not delete file
    end ;
    if RenameFile (OldName, NewName) then
        result := 0   // OK
    else
        result := GetLastError ; // system error
end ;

function TMailQuThread.SendOneEmail (Servnr: integer): boolean ;
var
    SendSmtpClient: TSslSmtpCli;  // June 2018 make it local
    succflag: boolean ;
    XHeader, info: string ;
    I: integer ;

// EML files start with one X-Sender header, then one or more X-Receiver headers,
// which we remove before sending the rest of the email headers and body
    procedure FindXHeader (const HdrName: string) ;
    begin
        XHeader := '' ;
        if FBodyLines.Count = 0 then exit ;
        if Pos (HdrName, FBodyLines [0]) <> 1 then exit ;
        XHeader := Trim (Copy (FBodyLines [0], Length (HdrName) + 1, 999)) ;
        FBodyLines.Delete (0) ;  // remove xheader line from top of body
    end;

begin
    result := false ;
    with FMailQuItem do
    begin
     // load entire body, then remove X-Sender and X-Receiver lines at top
        if FileExists (FName) then
        begin
            try
                FBodyLines.LoadFromFile (FName) ;
                if FBodyLines.Count < 6 then
                begin
                    LastResp := 'Email Message Body too Short - ' + Fname ;  // Oct 2015 keep in queue
                    ThreadLogEvent (MLogLevelInfo, LastResp) ;
                    exit ;
                end ;
            except
                LastResp := 'Failed to Load Email Message Body: ' + FName +
                                                  ' - ' + GetExceptMess (ExceptObject) ;  // Oct 2015 keep in queue
                ThreadLogEvent (MLogLevelInfo, LastResp) ;
                exit ;
            end;
        end
        else
        begin
             LastResp := 'Failed to Load Email Message Body: ' +
                                                   FName + ' - File Not Found' ;  // Oct 2015 keep in queue
             ThreadLogEvent (MLogLevelInfo, LastResp) ;
            exit ;
        end;

    // send email with own headers
        try      // finally
            SendSmtpClient := TSslSmtpCli.Create (Nil) ;
            SendSmtpClient.MultiThreaded := true ;
            SendSmtpClient.OnGetData := SmtpClientGetData ;
            SendSmtpClient.OwnHeaders := true ;
            SendSmtpClient.SslContext := FMagMailQueue.FSslContext ;
            SendSmtpClient.OnSslHandshakeDone := SmtpSslHandshakeDone ;
            SendSmtpClient.SocketErrs := wsErrFriendly;        // Nov 2016
            if FMagMailQueue.Debug then
            begin
                SendSmtpClient.OnDisplay := SmtpClientDisplay ;
            end;

            try      //  except

          // see if using specific server, and then if we have more details of it
                if (FMailQuItem.SmtpMeth > MailSmtpRelay) then   // Oct 2015
                begin
                    FCurSmtpSrv := SmtpSrvs [Servnr] ;
                    Servnr := -1 ;
                    for I := 0 to FMagMailQueue.FMailServers.Count - 1 do
                    begin
                        if FCurSmtpSrv = FMagMailQueue.FMailServers [I].Host then
                        begin
                            Servnr := I ;
                            break ;
                        end;
                    end;
                end;

            // use one of our relay servers with authentication details
                if Servnr >= 0 then
                begin
                    with FMagMailQueue.FMailServers [Servnr] do
                    begin
                        SendSmtpClient.Port := FPort;
                        SendSmtpClient.Host := FHost ;
                        FCurSmtpSrv := FHost ;  // June 2018
                        SendSmtpClient.AuthType := FAuthType ;
                        SendSmtpClient.Username := FUserName ;
                        SendSmtpClient.Password := FPassword ;
                        SendSmtpClient.SignOn := FSignOn ;     // host domain for HELO
                        if FSkipSsl then
                            SendSmtpClient.SslType := smtpTlsNone  // June 2018 after ssl error
                        else begin
                            SendSmtpClient.SslType := FSslType ;
                            SendSmtpClient.SslContext.SslCliSecurity := FSslCliSecurity;  // June 2018
                        end;
                        SendSmtpClient.SocketFamily := FSocketFamily ;   // March 2013
                        if SendSmtpClient.SocketFamily in [sfIPv4, sfIPv6] then
                        begin
                            SendSmtpClient.LocalAddr := FLocalAddr ;
                            SendSmtpClient.LocalAddr6 := FLocalAddr6 ;
                        end
                        else
                        begin
                            SendSmtpClient.LocalAddr := ICS_ANY_HOST_V4 ;
                            SendSmtpClient.LocalAddr6 := ICS_ANY_HOST_V6 ;
                        end;
                    end;
                end
                else

             // use MX server we looked up, no authentication, general SSL and family stuff
                begin
                    SendSmtpClient.Port := '25';
                    SendSmtpClient.Host := FCurSmtpSrv ;  // June 2018
                    SendSmtpClient.AuthType := smtpAuthNone ;
                    SendSmtpClient.SignOn := FMagMailQueue.QuHtmlSmtp.SignOn ; // host domain for HELO
                    SendSmtpClient.SslType := smtpTlsNone ;
                    if FMagMailQueue.FMxSrvUseSsl and (NOT FSkipSsl) then begin   // June 2018 after ssl error
                        SendSmtpClient.SslType := smtpTlsExplicit ;
                        if FMagMailQueue.FMailServers.Count > 0 then  // use security from first server, not ideal
                            SendSmtpClient.SslContext.SslCliSecurity := FMagMailQueue.FMailServers [0].SslCliSecurity;  // June 2018
                    end ;
                    SendSmtpClient.SocketFamily := FMagMailQueue.FSocketFamily ;
                    if SendSmtpClient.SocketFamily in [sfIPv4, sfIPv6] then
                    begin
                        SendSmtpClient.LocalAddr := FMagMailQueue.FLocalAddr ;
                        SendSmtpClient.LocalAddr6 := FMagMailQueue.FLocalAddr6 ;
                    end
                    else
                    begin
                        SendSmtpClient.LocalAddr := ICS_ANY_HOST_V4 ;
                        SendSmtpClient.LocalAddr6 := ICS_ANY_HOST_V6 ;
                    end;
                end;
                FindXHeader ('X-Sender:') ;
                SendSmtpClient.FromName := XHeader ;
                SendSmtpClient.RcptName.Clear ;
                info := '' ;
                while True do
                begin
                    FindXHeader ('X-Receiver:') ;
                    if XHeader = '' then break ;
                    SendSmtpClient.RcptName.Add (XHeader) ;
                    info := info + XHeader + space ;
                end;
                if SendSmtpClient.RcptName.Count = 0 then
                begin
                    LastResp := 'No X-Receiver Headers Found' ;  // Oct 2015 keep in queue
                    ThreadLogEvent (MLogLevelInfo, LastResp) ;
                    exit ;
                end ;
                SendSmtpClient.OwnHeaders := true ;   //  don't use BodyLines.Text which may rewrap lines
                FHdrDone := false ;
                ThreadLogEvent (MLogLevelInfo, 'Starting to Send Email Item ' + IntToStr (ItemNr) +
                        ', To: ' + info + ', Subject: ' + Subject + ', From: ' + XSender) ;
                succflag := SendSmtpClient.OpenSync ;    // connect, helo, authentication
                if (SendSmtpClient.ErrorMessage <> '') or (NOT succflag) then
                begin
                    LastResp := SendSmtpClient.ErrorMessage ;  // Oct 2015 keep in queue
                    ThreadLogEvent (MLogLevelInfo, 'Can Not Open Mail Server: ' + FormatIpAddrPort (FCurSmtpSrv,
                                                        SendSmtpClient.Port) + ' - ' + SendSmtpClient.ErrorMessage) ;
                    SendSmtpClient.AbortSync ;  // June 2018
                end
                else
                begin
                    ThreadLogEvent (MLogLevelInfo, 'Mail Session Connected to ' + FCurSmtpSrv + ' - ' +  // Oct 2015
                                           FormatIpAddrPort (SendSmtpClient.CtrlSocket.Addr, SendSmtpClient.Port)) ;
                    succflag := SendSmtpClient.MailSync ;  // send email
                    if NOT succflag then
                    begin
                        LastResp := SendSmtpClient.ErrorMessage ;  // Oct 2015 keep in queue
                        ThreadLogEvent (MLogLevelInfo, 'Failed to Send Mail Item ' + IntToStr (ItemNr) + ' - ' + LastResp + CRLF_) ;
                // 552 Message size exceeds maximum allowed size of 31457280. Closing transmission channel. (Gmail)
                // 552 5.2.3 our size guidelines. s30sm2062398wbm.12  (SmarterMail)
                       if Pos ('552', SendSmtpClient.ErrorMessage) = 1 then  // 5 Oct 2011 too large, no retries
                       begin
                            AttemptNr := MaxAttempts ;  // cause queue item to be deleted
                        end ;
                    end
                    else
                    begin
                        ThreadLogEvent (MLogLevelInfo, 'Send Mail OK Item ' + IntToStr (ItemNr) +
                                                             ' - ' + SendSmtpClient.LastResponse + CRLF_) ;
                        result := true ;  // done OK
                    end;
                end ;
                SendSmtpClient.QuitSync ;  // ignore error
            except
                LastResp :='Exception Sending EmailItem ' + IntToStr (ItemNr) +
                                                          ' - ' + GetExceptMess (ExceptObject) ;
                ThreadLogEvent (MLogLevelInfo, LastResp) ;
                SendSmtpClient.QuitSync ;  // ignore error
            end;
        finally
            FreeAndNil (SendSmtpClient) ;
        end;
    end;
end;

procedure TMailQuThread.SetName;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'MagMailQueue';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;
  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
end;

procedure TMailQuThread.SmtpClientDisplay(Sender: TObject; Msg: string);
begin
    if NOT FHdrDone then
    begin
        if Length (Msg) <= 2 then FHdrDone := true ;  // '> ' indicates end of header
    end ;
    if NOT FMagMailQueue.FDebug then exit ;  // 1.3
    if (NOT FMagMailQueue.FBodyDebug) and FHdrDone then exit ;  // 1.3
    ThreadLogEvent (MLogLevelDiag, Msg) ;
end;

procedure TMailQuThread.SmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: Pointer; MaxLen: Integer; var More: Boolean);
begin
    if LineNum > FBodyLines.Count then
        More := FALSE
    else
        StrPLCopy(PAnsiChar(MsgLine), AnsiString (FBodyLines [LineNum - 1]), MaxLen - 1);
end;

procedure TMailQuThread.SmtpSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
 var
    CertChain: TX509List;
    ChainVerifyResult: LongWord;
    Hash, info, VerifyInfo: String;
    Safe: Boolean;
begin
    FHandshakeDone := True;   // June 2018
    with Sender as TSslWSocket do
    begin
        if (ErrCode <> 0) or Disconnect then
        begin
       //     ThreadLogEvent (MLogLevelDiag, 'Mail Server SSL handshake failed - ' + SslHandshakeRespMsg);
        //    Disconnect := TRUE;
            exit ;
        end ;

     // OK
     //   ThreadLogEvent (MLogLevelDiag, 'Mail Server ' + SslHandshakeRespMsg)

        if SslSessionReused OR (FMagMailQueue.FSslVerMethod = MailSslVerNone) or (NOT SslContext.SslVerifyPeer) then
        begin
            exit; // nothing to do, go ahead
        end ;

     // Is current host already in the list of temporarily accepted hosts ?
        if NOT Assigned (PeerCert.X509) then
        begin
            ThreadLogEvent (MLogLevelDiag, SslServerName + ' SSL No Certificate Set') ;
            exit ;
        end;
        Hash := PeerCert.Sha1Hex ;
        if SslAcceptableHosts.IndexOf (SslServerName + Hash ) > -1 then
        begin
            exit; // nothing to do, go ahead
        end ;

     // Property SslCertChain contains all certificates in current verify chain
        CertChain := SslCertChain;

     // see if validating against Windows certificate store
        if FMagMailQueue.FSslVerMethod = MailSslVerWinStore then
        begin
            // start engine
            if not Assigned (FMagMailQueue.FMsCertChainEngine) then
                FMagMailQueue.FMsCertChainEngine := TMsCertChainEngine.Create;

          // see if checking revoocation, CRL checks and OCSP checks in Vista+, very slow!!!!
            if FMagMailQueue.FSslRevocation then
                FMagMailQueue.FMsCertChainEngine.VerifyOptions := [mvoRevocationCheckChainExcludeRoot]
            else
                FMagMailQueue.FMsCertChainEngine.VerifyOptions := [];

            // This option doesn't seem to work, at least when a DNS lookup fails
            FMagMailQueue.FMsCertChainEngine.UrlRetrievalTimeoutMsec := 10 * TicksPerSecond;

            { Pass the certificate and the chain certificates to the engine      }
            FMagMailQueue.FMsCertChainEngine.VerifyCert (PeerCert, CertChain, ChainVerifyResult, True);

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
        else if FMagMailQueue.FSslVerMethod = MailSslVerBundle then
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
            ThreadLogEvent (MLogLevelDiag, SslServerName + ' SSL Self Signed Certificate Succeeded: ' +
                                                              PeerCert.UnwrapNames (PeerCert.IssuerCName));
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
            ThreadLogEvent (MLogLevelDiag, SslServerName + info);
        end
        else
        begin
            ThreadLogEvent (MLogLevelDiag, SslServerName + ' SSL Chain Verification Succeeded, Domain: ' + SslCertPeerName);
        end;

   // if certificate checking failed, see if the host is specifically listed as being allowed anyway
        if (NOT Safe) and (SslAcceptableHosts.IndexOf (SslServerName) > -1) then
        begin
            Safe := true ;
            SslAcceptableHosts.Add (SslServerName + Hash);  // keep it to avoid checking again
            ThreadLogEvent (MLogLevelDiag, SslServerName + ' SSL Succeeded with Acceptable Host Name');
        end ;

      // tell user about all the certificates we found
        if FMagMailQueue.FSslReportChain and (CertChain.Count > 0) then
        begin
            info := SslServerName + ' ' + IntToStr (CertChain.Count) +
                    ' SSL Certificates in the verify chain:' + #13#10 +
                           CertChain.AllCertInfo (true, true) + #13#10 ; // Mar 2017 report all certs, backwards
            ThreadLogEvent (MLogLevelDiag, info);
        end;

      // all failed
        if NOT Safe then
        begin
            Disconnect := TRUE;
            exit ;
        end;
    end;

end;

procedure TMailQuThread.CallThreadEvent ;  // called by Synchronise for main thread
begin
    if Assigned (FThreadEvent) then
    begin
        FThreadEvent (FLogLevel, FInfo) ;
    end;
end;

procedure TMailQuThread.ThreadLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
begin
    if (LogLevel = MLogLevelInfo) or (LogLevel = MLogLevelDiag) then
    begin
        if Assigned (FThreadEvent) and Assigned (FMagMailQueue.FLogEvent) then
        begin
            FLogLevel := LogLevel ;
            FInfo := Info ;
            Synchronize (CallThreadEvent) ;
        end;
    end;
end ;

procedure TMailQuThread.DnsQueryRequestDone(Sender: TObject; Error: Word);
var
    I, J: integer;
    MXList: TStringList ;
begin
    FMailQuItem.SmtpSrvTot := 0 ;
    if FDNSReqId <> DnsQuery.ResponseID then
    begin
        FMailQuItem.SmtpSrvTot := -1 ;
        ThreadLogEvent (MLogLevelDiag, 'DNS MX Response Out of Sequence, Ignored') ;
        exit ;
    end ;
    if Error <> 0 then
    begin
        FMailQuItem.SmtpSrvTot := -1 ;
        ThreadLogEvent (MLogLevelDiag, 'DNS MX Error ' + WSocketGetErrorMsgFromErrorCode (Error)) ;
        exit ;
    end ;
{ MX examples
Mail Exchange Server: alt4.gmail-smtp-in.l.google.com (preference 40)
Mail Exchange Server: gmail-smtp-in.l.google.com (preference 5)
Mail Exchange Server: alt3.gmail-smtp-in.l.google.com (preference 30)
Mail Exchange Server: alt2.gmail-smtp-in.l.google.com (preference 20)
Mail Exchange Server: alt1.gmail-smtp-in.l.google.com (preference 10)

Mail Exchange Server: mx1.hotmail.com (preference 5)
Mail Exchange Server: mx2.hotmail.com (preference 5)
Mail Exchange Server: mx3.hotmail.com (preference 5)
Mail Exchange Server: mx4.hotmail.com (preference 5)
 }
    MXList := TStringList.Create ;
    try
        for I := 0 to DnsQuery.ResponseANCount - 1 do
        begin
            J := DnsQuery.AnswerTag [I ];
            if (J >= 0) and (DnsQuery.AnswerType [I] = DnsQueryMX) then  // add 1000 to make alpha sorting work
                MXList.Add (IntToStr (DnsQuery.MXPreference [J] + 1000) + '=' + string(DnsQuery.MXExchange [J])) ;
        end ;
        if MXList.Count = 0 then
        begin
            FMailQuItem.SmtpSrvTot := -1 ;
            ThreadLogEvent (MLogLevelDiag, 'No DNS MX Records Found') ;
            exit ;
        end;
        MXList.Sort ;  // sort into preference order
        for I := 0 to MXList.Count - 1 do
        begin
            J := Pos ('=', MXList [I]) ;
            FMailQuItem.SmtpSrvs [FMailQuItem.SmtpSrvTot] := Copy (MXList [I], J + 1, 99) ;
            inc (FMailQuItem.SmtpSrvTot) ;
            if FMailQuItem.SmtpSrvTot >= MaxSmtpSrv then Break ; // may be too many MX servers
        end;
        ThreadLogEvent (MLogLevelDiag, 'DNS MX Result:' + CRLF + MXList.Text) ;
    finally
        MXList.Free ;
    end ;
end;

procedure TMailQuThread.Execute;
var
    succflag: boolean ;
    I, qunr, servnr, retries, errorcounter: integer ;
    Trg: longword ;
    curDT: TDateTime ;
    Filename, domain: string ;
begin
    SetName;  // thread name
    try
        FIcsWndControl := TIcsWndControl.Create (Nil) ;  // June 2018 only used for message loop
        FIcsWndControl.MultiThreaded := true ;   // June 2018
        FMagMailQueue.FSendInProgress := 0 ;
        FMagMailQueue.FSslContext.SslVerifyPeer := (FMagMailQueue.FSslVerMethod > MailSslVerNone) ;  // Oct 2015
        FBodyLines := TStringList.Create ;
        DnsQuery := TDnsQuery.Create (Nil) ;    // Oct 2015
        DnsQuery.OnRequestDone := DnsQueryRequestDone ;
        DnsQuery.MultiThreaded := true ;  // June 2018
        ThreadLogEvent (MLogLevelInfo, 'Starting Mail Queue') ;  // TEMP

        // main thread loop, checking mail queue and sending email
        with FMagMailQueue do
        begin
            ThreadLogEvent (MLogLevelInfo, 'Mail Queue Processing Started, using Total Mail Servers: ' +
                                                                     IntToStr (FMailServers.Count)) ;
            for servnr := 0 to FMailServers.Count - 1 do
                     ThreadLogEvent (MLogLevelInfo, 'Mail Server ' + IntToStr (servnr + 1) + ': ' +
                                 FormatIpAddrPort (FMailServers [servnr].Host, FMailServers [servnr].Port)) ;
        end;
        FMagMailQueue.FQuThreadStopping := false ;
        FMagMailQueue.FQuThreadRunning := true ;
    except
        ThreadLogEvent (MLogLevelInfo, 'Exception Starting Mail Queue Thread - ' + GetExceptMess (ExceptObject)) ;
        FMagMailQueue.FActive := false ;
        exit ;
    end;
    try
        errorcounter := 0 ;
        while True do
        begin
            FIcsWndControl.ProcessMessages ;
            if FIcsWndControl.Terminated then break ;
            if Application.Terminated then break ;  // main application
            if Terminated then break ;  // thread
            with FMagMailQueue do
            begin
                if NOT FActive then break ;        //  component
                if FQuThreadStopping then break ;  // don't process any more items
                if MailTotItems > 0 then
                begin
                    curDT := Now ;
                    EnterCriticalSection (FQuItemsCritSect);
                    try
                        FMailQuItem := PMailQuItem (MailQuIdx [0])^ ; // top of queue, ignore priority for now
                    finally
                        LeaveCriticalSection (FQuItemsCritSect) ;
                    end ;

                // it's time to try and send an email
                    succflag := false ;
                    if (FMailQuItem.NextAttemptDT <= curDT) then
                    begin
                        FSendInProgress := FMailQuItem.ItemNr ;
                        FMailQuItem.LastAttemptDT := curDT ;  // when we started
                        FMailQuItem.AttemptNr := FMailQuItem.AttemptNr + 1 ;

                     // give up if no file
                        if NOT FileExists (FMailQuItem.FName) then
                        begin
                            ThreadLogEvent (MLogLevelInfo, 'Failed to Load Message Body: ' +
                                                        FMailQuItem.FName + ' - File Not Found') ;
                            FMailQuItem.AttemptNr := MaxAttempts ;  // cause queue item to be deleted
                            inc (errorcounter) ;
                        end
                        else if (FMailQuItem.AttemptNr < MaxAttempts) then  // Nov 2016 better test
                        begin
                            if (FMailQuItem.SmtpMeth = MailSmtpMXLookup) and (FMailQuItem.SmtpSrvTot = 0) and
                                  (FDnsServers.Count = 0) then FMailQuItem.SmtpMeth := MailSmtpRelay ; // sanity check

                          // sending email using one or more relay servers
                            if (FMailQuItem.SmtpMeth = MailSmtpRelay) then   // Oct 2015
                            begin
                                if FMailServers.Count > 0 then
                                begin
                                    for servnr := 0 to FMailServers.Count - 1 do
                                    begin
                                        FHandshakeDone := False;   // June 2018
                                        FSkipSsl := False;
                                        succflag := SendOneEmail (servnr) ;
                                        if succflag then break ;

                                      // June 2018 failed handshake, retry without SSL
                                        if FHandshakeDone and FMailServers[servnr].RetryWithoutSsl then
                                        begin
                                            ThreadLogEvent (MLogLevelInfo, 'SSL Handshake Failed, Retrying Without SSL') ;
                                            FSkipSsl := True;
                                            succflag := SendOneEmail (servnr) ;
                                            if succflag then break ;
                                        end ;
                                    end;
                                end
                                else
                                begin
                                    FMailQuItem.LastResp := 'Failed No Mail Servers Specified' ;
                                    ThreadLogEvent (MLogLevelInfo, FMailQuItem.LastResp) ;
                                    inc (errorcounter) ;
                                end;
                            end
                            else
                            begin
                                if (FMailQuItem.SmtpMeth = MailSmtpMXLookup) and (FMailQuItem.SmtpSrvTot = 0) then   // Oct 2015
                                begin
                                // get domain from email address
                                    I := Pos ('@', FMailQuItem.XReceivers) ; // must only be one!!!
                                    domain := copy (FMailQuItem.XReceivers, I + 1, 88) ;

                                // set MX records, send email
                                    servnr := 0 ;
                                    for retries := 1 to MXRetryAttempts do        // UDP may ignore request
                                    begin
                                        if servnr >= FDnsServers.Count then servnr := 0 ;
                                        DnsQuery.Addr := FDnsServers [servnr] ;  // multiple DNS servers
                                        inc (servnr) ;
                                        ThreadLogEvent (MLogLevelDiag, 'Looking-up ' + domain + ' at ' + DnsQuery.Addr);
                                        FDNSReqId := DnsQuery.MXLookup (AnsiString (domain)) ;
                                        Trg := GetTrgSecs (MXWaitSecs) ;  // wait for UDP response
                                        while (FMailQuItem.SmtpSrvTot = 0) do
                                        begin
                                            FIcsWndControl.ProcessMessages ;
                                            if FIcsWndControl.Terminated then break ;
                                             if TestTrgTick (Trg) then break; // timer
                                            if NOT FActive then break ; // component
                                            if Terminated then break ;  // thread
                                        end ;
                                        if (FMailQuItem.SmtpSrvTot > 0) then Break ; // found MX servers OK
                                        FMailQuItem.SmtpSrvTot := 0 ; // try again
                                    end ;
                                end;
                                if (FMailQuItem.SmtpSrvTot = 0) then
                                begin
                                    FMailQuItem.LastResp := 'Failed No MX Mail Servers Found' ;
                                    ThreadLogEvent (MLogLevelInfo, FMailQuItem.LastResp) ;
                                     inc (errorcounter) ;
                                end
                                else
                                begin
                                    for servnr := 0 to FMailQuItem.SmtpSrvTot - 1 do
                                    begin
                                        FHandshakeDone := False;   // June 2018
                                        FSkipSsl := False;
                                        succflag := SendOneEmail (servnr) ;
                                        if succflag then errorcounter := 0 ;
                                        if succflag then break ;

                                      // June 2018 failed handshake, retry without SSL
                                        if FHandshakeDone then
                                        begin
                                            ThreadLogEvent (MLogLevelInfo, 'SSL Handshake Failed, Retrying Without SSL') ;
                                            FSkipSsl := True;
                                            succflag := SendOneEmail (servnr) ;
                                            if succflag then break ;
                                        end ;
                                    end;
                                end;
                            end;
                        end;
                        FIcsWndControl.ProcessMessages ;
                        if FIcsWndControl.Terminated then break ;
                        if NOT FActive then break ; // component
                        if Terminated then break ;  // thread
                        if (errorcounter > 10000) then break ;  // sanity check

                     // handle success or failure
                        with FMailQuItem do
                        begin

                         // mail sent OK, delete or rename body
                            if succflag then
                            begin
                                if FArchiveSent then
                                begin
                                    FileName := strAddSlash (FMailQuDir) + 'archive' ;
                                    if NOT ForceDirs (FileName) then
                                    begin
                                        ThreadLogEvent (MLogLevelInfo, 'Error creating directory: ' + FileName) ;
                                        MagDeleteFile (FName, true) ;
                                    end
                                    else
                                    begin
                                        Filename := strAddSlash (FileName) + ExtractFileName (Fname) ;
                                        if MagRenameFile (FName, Filename, true, true) <> 0 then
                                        begin
                                            ThreadLogEvent (MLogLevelInfo, 'Failed to Archive Sent Mail to: ' + FileName) ;
                                            MagDeleteFile (FName, true) ;
                                        end ;
                                        FName := Filename ;  // keep archive name for mail log
                                    end;
                                end
                                else
                                begin
                                    MagDeleteFile (FName, true) ;
                                    FName := ExtractFileName (Fname) ; // keep short name for mail log
                                end;
                            end
                            else

                         // mail send failed to many times, delete or rename it
                            begin
                                if (AttemptNr >= FAttemptTot) or (AttemptNr >= MaxAttempts) then
                                begin
                                    if FDeleteFailed or (AttemptNr >= MaxAttempts) then
                                    begin
                                        if AttemptNr < MaxAttempts then  // can not delete missing file
                                            ThreadLogEvent (MLogLevelInfo, 'Failed Mail Exceeded Attempts, Now Deleted' + CRLF) ;
                                        MagDeleteFile (FName, true) ;
                                        AttemptNr := MaxAttempts;
                                    end
                                    else
                                    begin
                                        if AttemptNr < MaxAttempts then  // can not delete missing file
                                            ThreadLogEvent (MLogLevelInfo, 'Failed Mail Exceeded Attempts') ;
                                        FileName := strAddSlash (FMailQuDir) + 'badmail' ;
                                        if NOT ForceDirs (FileName) then
                                        begin
                                            ThreadLogEvent (MLogLevelInfo, 'Error creating directory: ' + FileName) ;
                                            MagDeleteFile (FName, true) ;
                                        end
                                        else
                                        begin
                                            Filename := strAddSlash (FileName) + ExtractFileName (Fname) ;
                                            if MagRenameFile (FName, Filename, true, true) <> 0 then
                                            begin
                                                ThreadLogEvent (MLogLevelInfo, 'Failed to Move Failed Mail to: ' + FileName) ;
                                                MagDeleteFile (FName, true) ;
                                            end ;
                                        end;
                                        AttemptNr := MaxAttempts;
                                    end ;
                                end;
                            end;
                        end;

                     // update queue with mail success or failure - unless stopped while sending
                        EnterCriticalSection (FQuItemsCritSect);
                        try
                            if (MailTotItems <> 0) and (Length (MailQuItems) >= MailTotItems) then // sanity check
                            begin
                                try
                                    qunr := -1 ;  ;   // find item in queue, may no longer be top
                                    for I := 0 to MailTotItems - 1 do
                                    begin
                                        if MailQuItems [I].ItemNr = FMailQuItem.ItemNr then
                                        begin
                                            qunr := I ;
                                            break ;
                                        end;
                                    end;
                                    if qunr >= 0 then
                                    begin

                                     // update queue item
                                        MailQuItems [qunr].LastAttemptDT := FMailQuItem.LastAttemptDT ;
                                        MailQuItems [qunr].AttemptNr := FMailQuItem.AttemptNr ;
                                        if MailQuItems [qunr].SmtpSrvTot <> FMailQuItem.SmtpSrvTot then    // Oct 2015
                                        begin
                                            for servnr := 0 to FMailQuItem.SmtpSrvTot - 1 do
                                                MailQuItems [qunr].SmtpSrvs [servnr] :=  FMailQuItem.SmtpSrvs [servnr] ;
                                            MailQuItems [qunr].SmtpSrvTot := FMailQuItem.SmtpSrvTot ;
                                        end;
                                        MailQuItems [qunr].LastResp := FMailQuItem.LastResp ;   // Oct 2015

                                      // log item -
                                        if FLogQuSent then
                                        begin
                                            if Pos (':', FFileQuSent) > 1 then
                                                Filename := FFileQuSent
                                             else
                                                Filename := '"' + FMailQuDir + '\' + Copy (FFileQuSent, 2, 99) ;
                                            FMailQuItem.SmtpSrvs [0] := FCurSmtpSrv ;  // SMTP server we used
                                            SimpleLogging (Filename, ',' + SaveOneHdr (FMailQuItem)) ;
                                        end;

                                     // remove item or requeue failed item
                                        with MailQuItems [qunr] do
                                        begin
                                            if succflag then
                                                RemoveQuItem (qunr)
                                            else
                                            begin
                                                if (AttemptNr >= FAttemptTot) or (AttemptNr >= MaxAttempts) then  // Nov 2016 another delete check
                                                    RemoveQuItem (qunr)  // Nov 2016 delete correct item
                                                else
                                                begin

                                              // requeue with gap from previous scheduled attempt, not actual attempt time
                                                    NextAttemptDT := NextAttemptDT + MinsToTime (FAttemptTimes [AttemptNr - 1]) ;
                                                 // unless that time has passed already
                                                    if NextAttemptDT <= Now then
                                                        NextAttemptDT := LastAttemptDT + MinsToTime (FAttemptTimes [AttemptNr - 1]) ;
                                                    ThreadLogEvent (MLogLevelInfo, 'Failed Mail Requeued, Attempt ' +
                                                                IntToStr (AttemptNr +  1) + ' at ' + TimeToNStr (NextAttemptDT) + CRLF_) ;
                                                end;
                                            end;
                                        end;
                                        BuildQuIdx;
                                        SaveQuHdrs ;
                                    end
                                    else
                                        ThreadLogEvent (MLogLevelInfo, 'Could Not Find Queue Record for Item ' + IntToStr (FMailQuItem.ItemNr)) ;
                                except
                                    ThreadLogEvent (MLogLevelInfo, 'Exception Updating Mail Queue - ' + GetExceptMess (ExceptObject)) ;
                                end;
                            end;
                        finally
                            LeaveCriticalSection (FQuItemsCritSect) ;
                        end ;
                        FSendInProgress := 0 ;
                    end
                    else
                       Sleep (500);   // thread now stops for 500ms
               end
               else
                   Sleep (500);
            end;
        end;
    except
        ThreadLogEvent (MLogLevelInfo, 'Exception in Mail Queue Thread, Stopped - ' + GetExceptMess (ExceptObject)) ;

        FMagMailQueue.FActive := false ;
    end;
    FMagMailQueue.FSendInProgress := 0 ;
    FreeAndNil (DnsQuery) ;
//    FreeAndNil (SendSmtpClient) ;
    FreeAndNil (FIcsWndControl) ;
    FreeAndNil (FBodyLines) ;
end;

{ TMagMailQueue }

constructor TMagMailQueue.Create(Aowner: TComponent);
begin
    inherited;
    InitializeCriticalSection (FQuItemsCritSect) ;
    MailTotItems := 0 ;
    MailImmItems := 0 ;
    FQuHtmlSmtp := THtmlSmtpCli.Create (self) ;
    FQuHtmlSmtp.ContentType := smtpPlainText ;
    FQuHtmlSmtp.OnAttachContentTypeEh := SmtpClientAttachContentTypeEh ;
    FQuHtmlSmtp.SocketErrs := wsErrFriendly;        // Nov 2016
    MailQuIdx := TFindList.Create ;
    FBodyText := TStringList.Create ;
    FMailServers := TMailServers.Create(Self);
    FRetryList := '5,5,10,10,30,30,60,90,300,300,300,300' ;
    FDeleteFailed := true ;
    FBodyDebug := false ;
    FDebug := false ;
    FQuStartDelay := 3 ;  //  seconds
    FQuThreadRunning := false ;
    FQuThreadStopping := false ;
    FSendInProgress := 0 ;
    FDnsServers := TStringList.Create ;
    FFileQuSent := '"MailQuSent-"yyyymmdd".log' ;   // must be a masked file name
    FLogEvent := Nil ;
end;

destructor TMagMailQueue.Destroy;
begin
    StopMailQu ;
    FreeAndNil (FQuHtmlSmtp) ;
    FreeAndNil (MailQuIdx) ;
    FreeAndNil (FMailServers) ;
    FreeAndNil (FBodyText) ;
    FreeAndNil (FDnsServers) ;
    DeleteCriticalSection (FQuItemsCritSect) ;
    inherited;
end;

function TMagMailQueue.ClearQueue: boolean;
begin
    MailQuIdx.Clear ;
    MailTotItems := 0 ;
    SetLength (MailQuItems, 32) ;
    SaveQuHdrs ;
    result := true ;
end;

procedure TMagMailQueue.DoLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
begin
    if Assigned (FLogEvent) then
    begin
        FLogEvent (LogLevel, Info) ;
   end;
end;

function TMagMailQueue.AddtoQueue (MailQuItem: TMailQuItem): boolean ;
begin
    result := false ;
    EnterCriticalSection (FQuItemsCritSect);
    try
        try
            if Length (MailQuItems) <= (MailTotItems + 2) then
                                SetLength (MailQuItems, MailTotItems + 32) ;
            MailQuItems [MailTotItems] := MailQuItem ;
            inc (MailTotItems) ;
            BuildQuIdx;
            SaveQuHdrs ;
            result := true ;
        except
            DoLogEvent (MLogLevelInfo, 'Failed to Add to Mail Queue - ' + GetExceptMess (ExceptObject)) ;
        end;
    finally
        LeaveCriticalSection (FQuItemsCritSect) ;
    end ;
    if result then DoLogEvent (MLogLevelInfo, 'Queued Mail OK, Item ' +
                             IntToStr (MailQuItem.ItemNr) + ' = ' + FQuHtmlSmtp.HdrSubject) ;
end ;

function TMagMailQueue.QueueMail (const Srv1: string = ''; const Srv2: string = ''; const Srv3: string = ''): integer ;
var
    FileName, XHeaders, errresp: string ;
    Trg: longword ;
    Item, I: integer ;
    MailQuItem: TMailQuItem ;
begin
    result := 0 ;
    if NOT FActive then
    begin
        DoLogEvent (MLogLevelInfo, 'Mail Queue Not Yet Started') ;
        exit ;
    end;
    FileName := strAddSlash (FMailQuDir) + 'spool' ;
    if NOT ForceDirs (FileName) then
    begin
        DoLogEvent (MLogLevelInfo, 'Error creating directory: ' + FileName) ;
        exit ;
    end;
    if (FQuHtmlSmtp.RcptName.Count = 0) or (Pos ('@', FQuHtmlSmtp.RcptName [0]) = 0) then  // Nov 2016
    begin
        DoLogEvent (MLogLevelInfo, 'Failed to Queue Email, No Recipients Specified') ;
        exit ;
    end;
    item := NewMailSeq ;  // from control file, incremented automatically
    FileName := strAddSlash (FMailQuDir) + 'spool\item' + Int2StrZ (item, 8) + '.eml' ;
    MagDeleteFile (FileName, true) ;
    if FQuHtmlSmtp.OwnHeaders then
    begin
        try
            FQuHtmlSmtp.PlainText.SaveToFile (FileName) ;
            result := item ;
        except
            errresp := GetExceptMess (ExceptObject) ;
        end;
    end
    else
    begin
        FQuHtmlSmtp.SendMode := smtpToStream ;
        FQuHtmlSmtp.SendToFile (FileName) ;
    //    FQuHtmlSmtp.SendToFileSync (FileName) ;   // not in THtmlSmtpCli
        Trg := GetTrgSecs (10) ;
        while FQuHtmlSmtp.State <> smtpReady do   // not long since writing disk file
        begin
            Application.ProcessMessages ;
            if Application.Terminated then break ;
            if TestTrgTick (Trg) then break ;
        end;
        errresp := FQuHtmlSmtp.LastResponse ;
        if (Copy (errresp, 1, 3) = '200') then Result := item ;
    end;
    if result = 0 then
    begin
        DoLogEvent (MLogLevelInfo, 'Queued Mail Failed: ' + errresp + ' - ' +
                                               FileName + ' = ' + FQuHtmlSmtp.HdrSubject) ;
        exit ;
    end ;

// warning - if using MX DNS servers and multiple recipients, need to queue mail multiple times !!!!

// build queue entry
    with MailQuItem do
    begin
        ItemNr := item ;
        FName := FileName ;
        XReceivers := FQuHtmlSmtp.RcptName.CommaText ;
        XSender := FQuHtmlSmtp.FromName ;
        Subject := FQuHtmlSmtp.HdrSubject ;
        QueuedDT := Now ;
        Priority := FQuHtmlSmtp.HdrPriority ;
        AttemptNr := 0 ;
        LastAttemptDT := 0 ;
        NextAttemptDT := QueuedDT + (OneSecond * FQuStartDelay) ;
        BodySize := GetSizeFile (FileName) ;
        LastResp := '' ;
        SmtpMeth := FSmtpMethod ;  // Oct 2015
        SmtpSrvTot := 0 ;
        if SmtpMeth = MailSmtpSpecific then
        begin
            if Srv1 <> '' then
            begin
                SmtpSrvs [SmtpSrvTot] := Srv1 ;
                inc (SmtpSrvTot) ;
            end;
            if Srv2 <> '' then
            begin
                SmtpSrvs [SmtpSrvTot] := Srv2 ;
                inc (SmtpSrvTot) ;
            end;
            if Srv3 <> '' then
            begin
                SmtpSrvs [SmtpSrvTot] := Srv3 ;
                inc (SmtpSrvTot) ;
            end;
            if SmtpSrvTot = 0 then
            begin
                Result := 0 ;
                DoLogEvent (MLogLevelInfo, 'Queued Mail Failed: No SMTP Servers Supplied  = ' +
                                                                        FQuHtmlSmtp.HdrSubject) ;
                exit ;
            end ;
        end
        else if (SmtpMeth = MailSmtpMXLookup) and (FDnsServers.Count = 0) then
        begin
            Result := 0 ;
            DoLogEvent (MLogLevelInfo, 'Queued Mail Failed: No DNS Servers Supplied  = ' +
                                                                    FQuHtmlSmtp.HdrSubject) ;
            exit ;
        end ;

    end;

// add X headers to top of message file
    try
        FBodyText.LoadFromFile (FileName) ;
        XHeaders := 'X-Sender: ' + FQuHtmlSmtp.FromName + CRLF_ ;  // must be first
        for I := 0 to FQuHtmlSmtp.RcptName.Count - 1 do
                 XHeaders := XHeaders + 'X-Receiver: ' + FQuHtmlSmtp.RcptName [I] + CRLF_ ;
        FBodyText.Text := XHeaders + FBodyText.Text ;
        FBodyText.SaveToFile (FileName) ;
    except
        Result := 0 ;
        DoLogEvent (MLogLevelInfo, 'Failed to Process Mail File: ' + FileName +
                                            ' - ' + GetExceptMess (ExceptObject)) ;
        exit ;
    end;
    AddtoQueue (MailQuItem) ;
end;

// called by TFindList for sort and find comparison of records

function CompareFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
var
    num1, num2: double ;
begin
    num1 := PMailQuItem (item1).NextAttemptDT ;
    num2 := PMailQuItem (item2).NextAttemptDT ;
    if num1 = 0 then num1 := 99999999 ;
    if num2 = 0 then num2 := 99999999 ;
    if num1 > num2 then
        result := 1
    else if num1 < num2 then
        result := -1
    else
        result := 0 ;
end ;

procedure TMagMailQueue.BuildQuIdx;  // FQuItemsCritSect should be locked before calling this method
var
    I, tot: integer ;
    quDT: TDateTime ;
begin
    MailQuIdx.Clear ;
    tot := 0 ;
    if MailTotItems > 0 then
    begin
        for I := 0 to MailTotItems - 1 do MailQuIdx.Add (@MailQuItems [I]) ;
        MailQuIdx.Sort (CompareFNext) ;  // sorts by NextAttemptDT

      // see how many immediate items in queue, ie those queued but not yet sent once
        quDT := Now + (OneSecond * FQuStartDelay) + OneSecond ;
        for I := 0 to MailQuIdx.Count - 1 do
        begin
             if PMailQuItem (MailQuIdx [I])^.NextAttemptDT <= quDT then
                inc (tot)
             else
                break ;
        end;
    end ;
    MailImmItems := tot ;
    if Assigned (FQuChangedEvent) then FQuChangedEvent (Self) ;  // tell application queue changed
end;

procedure TMagMailQueue.ReadQuHdrs;  // FQuItemsCritSect should be locked before calling this method
var
    QueueLines, QueueCols: TStringList ;
    I, J, K: integer ;
begin
    QueueLines := TStringList.Create ;
    QueueCols := TStringList.Create ;
    try
        if FileExists (FFileQuItemsHdr) then
        begin
            try
                QueueLines.LoadFromFile (FFileQuItemsHdr) ;
            except
                DoLogEvent (MLogLevelInfo, 'Failed to Load Queue Header File: ' + FFileQuItemsHdr +
                                                                    ' - ' + GetExceptMess (ExceptObject)) ;
            end;
            MailTotItems := 0 ;
            if QueueLines.Count > 0 then
            begin
                if Length (MailQuItems) <= (QueueLines.Count + 2) then SetLength (MailQuItems, QueueLines.Count + 32) ;
                for I := 0 to QueueLines.Count - 1 do
                begin
                    QueueCols.CommaText := QueueLines [I] ;
                    if QueueCols.Count >= mqLastField then
                    begin
                     //   Default (MailQuItems [MailTotItems]) ;
                        with MailQuItems [MailTotItems] do
                        begin
                            AttemptNr := AscToInt (QueueCols [mqAttemptNr]) ;
                            if AttemptNr >= MaxAttempts then continue ;  // Nov 2016 should have been deleted
                            ItemNr := AscToInt (QueueCols [mqItemNr]) ;
                            NextAttemptDT := Packed2Date (QueueCols [mqNextAttemptDT]) ;
                            FName := QueueCols [mqFName] ;
                            XReceivers := QueueCols [mqXReceiver] ;
                            XSender := QueueCols [mqXSender] ;
                            Subject := QueueCols [mqSubject] ;
                            QueuedDT := Packed2Date (QueueCols [mqQueuedDT]) ;
                            Priority := TSmtpPriority (AscToInt (QueueCols [mqPriority])) ;
                            LastAttemptDT := Packed2Date (QueueCols [mqLastAttemptDT]) ;
                            BodySize := AscToInt (QueueCols [mqBodySize]) ;
                            LastResp := QueueCols [mqLastResp] ;  // following Oct 2015
                            SmtpMeth := MailSmtpRelay ;
                            SmtpMeth := TMailSmtpMethod (AscToInt (QueueCols [mqSmtpMeth])) ;
                            K := mqSmtpSrv1 ;
                            SmtpSrvTot := 0 ;
                            for J := 1 to MaxSmtpSrv do
                            begin
                                if K >= QueueCols.Count then Break ; // sanity check
                                if QueueCols [K] <> '' then
                                begin
                                    SmtpSrvs [SmtpSrvTot] := QueueCols [K] ;
                                    inc (SmtpSrvTot) ;
                                end;
                                inc (K) ;
                            end;
                        end ;
                        inc (MailTotItems) ;
                    end;
                end ;
            end;
            BuildQuIdx;
        end ;
    finally
        QueueLines.Free ;
        QueueCols.Free ;
    end ;
end;

function TMagMailQueue.RebuiltQueue: boolean;
begin
    result := false ;
//  pending, this will index files in spool directory, reading headers and building new queue
end;

procedure TMagMailQueue.RemoveQuItem(item: integer);  // FQuItemsCritSect should be locked before calling this method
var
    I: integer ;
begin
    if item >= MailTotItems then item := 0 ;  // sanity check, Nov 2016 delete top
    if item < (MailTotItems - 1) then
    begin
        for I := item to MailTotItems - 2 do
                  MailQuItems [I] := MailQuItems [I + 1] ;  // shuffle items in array
    end;
    MailTotItems := MailTotItems - 1 ;
end;

function TMagMailQueue.StartMailQu: boolean ;
var
    FileName {ProgDirectory}: string ;
    TempList: TStringList ;
    I, J, mins: integer ;
    sslflag: boolean ;
begin
    result := false ;
    if FQuThreadRunning then
    begin
        DoLogEvent (MLogLevelInfo, 'Mail Queue Already Running') ;
        exit ;
    end;
    FActive := false ;
    FMailQuThread := Nil ;
    if FMailServers.Count = 0 then
    begin
        DoLogEvent (MLogLevelInfo, 'No Mail Servers Specified') ;
        exit ;
    end;

  // check mail servers
    sslflag := false ;
    for I := 0 to FMailServers.Count - 1 do
    begin
        with FMailServers [I] do
        begin
            if (FHost = '') or (FPort = '') then
            begin
                DoLogEvent (MLogLevelInfo, 'Mail Server Properties Empty') ;
                exit ;
            end;
            if (FSslType > smtpTlsNone) and (NOT sslflag) then
            begin
                if NOT Assigned (FSslContext) then
                begin
                    DoLogEvent (MLogLevelInfo, 'Mail Server SSL Context Empty') ;
                    exit ;
                end;
            {    if NOT FileExists (GLIBEAY_DLL_Name) then   June 2016 only tested for old DLLs, not new names
                begin
                    DoLogEvent (MLogLevelInfo, 'SSL DLL Not Found: ' + GLIBEAY_DLL_Name) ;
                    exit ;
                end;    }
                try
                    if (FSslContext.SslCAFile <> '') then  // June 2016 check a root bundle is available
                    begin
                        if NOT FileExists (FSslContext.SslCAFile) then
                            FSslContext.SslCAFile := '' ;
                    end;
                    if (FSslContext.SslCAFile = '') and (FSslContext.SslCALines.Count = 0) then
                        FSslContext.SslCALines.Text := sslRootCACertsBundle ; // default root bundle
                    FSslContext.InitContext; //Pre-loads OpenSSL DLL's
                    DoLogEvent (MLogLevelInfo, 'SSL Version: ' + OpenSslVersion + ' - ' + GLIBEAY_DLL_FileName) ;
                    sslflag := true ;
                except
                    DoLogEvent (MLogLevelInfo, 'Failed to Initialise SSL - ' + GetExceptMess (ExceptObject)) ;
                    exit ;
                end ;
            end;
        end;
    end;
    if NOT DirectoryExists (FMailQuDir) then
    begin
        DoLogEvent (MLogLevelInfo, 'Mail Queue Directory Not Found: ' + FMailQuDir) ;
        exit ;
    end;
    FileName := strAddSlash (FMailQuDir) + 'control' ;
    if NOT ForceDirs (FileName) then
    begin
        DoLogEvent (MLogLevelInfo, 'Error Creating Directory: ' + FileName) ;
        exit ;
    end;
    FFileQuItemsHdr := FileName + '\MailQuItems.Hdr' ;
    FFileQuItemsCtl := FileName + '\MailQuItems.Ctl' ;
    FActive := true ;
    ReadQuHdrs;
    try
        FMailQuThread := TMailQuThread.Create (true) ;
        with FMailQuThread  do
        begin
            FThreadEvent := onThreadEvent ;
            OnTerminate := OnThreadTerminate ;
            FreeOnTerminate := true ;
            FMagMailQueue := Self ;
            TempList := TStringList.Create ;
            try
                FAttemptTot := 0 ;
                mins := 0 ;
                TempList.CommaText := FRetryList ;  // comma list of retry minute gaps
                SetLength (FAttemptTimes, TempList.Count) ;
                for I := 0 to TempList.Count - 1 do
                begin
                    J := AscToInt (Trim (TempList [I])) ;
                    if J > 0 then
                    begin
                        FAttemptTimes [FAttemptTot] := J ;
                        inc (FAttemptTot) ;
                        mins := mins + J ;
                    end;
                end;
                if FAttemptTot = 0  then // default some retries, 5 at 5 mins
                begin
                    SetLength (FAttemptTimes, 5) ;
                    for I := 0 to 4 do
                    begin
                        FAttemptTimes [FAttemptTot] := 5 ;
                        inc (FAttemptTot) ;
                        mins := mins + 5 ;
                    end;
                end;
                if FAttemptTot >= MaxAttempts then FAttemptTot := MaxAttempts - 1;  // Nov 2016 sanity check
                DoLogEvent (MLogLevelInfo, 'Mail will be retried ' + IntToStr (FAttemptTot) +
                                                 ' times over ' + IntToCStr (mins) + ' minutes') ;
            finally
                TempList.Free ;
            end ;
            Resume ;   // start queuing thread
        end;
    except
        DoLogEvent (MLogLevelInfo, 'Failed to Start Queue Thread - ' + GetExceptMess (ExceptObject)) ;
        FreeAndNil (FMailQuThread) ;
        FActive := false ;
        exit ;
    end;
    result := FActive ;
end;

procedure TMagMailQueue.onThreadEvent(LogLevel: TMailLogLevel; const Info: String);
begin
    DoLogEvent (LogLevel, Info) ;
end;

procedure TMagMailQueue.OnThreadTerminate(Sender: TObject);
begin
    FQuThreadRunning := false ;
    DoLogEvent (MLogLevelInfo, 'Mail Queue Processing Stopped') ;
end;

function TMagMailQueue.WaitSend (secs: integer; Any: Boolean): boolean;    // 11 March 2017
var
    Trg: longword ;
begin
    result := false ;
    if FFileQuItemsHdr = '' then exit ;
    if FActive and FQuThreadRunning then
    begin
        if ((NOT Any) and (MailImmItems > 0)) or
                       (Any and (MailTotItems > 0)) then
        begin
            Trg := GetTrgSecs (secs) ;
            while true do
            begin
                if (NOT Any) and (MailImmItems = 0) then break ;   // wait until all immediate items sent
                if Any and (MailTotItems = 0) then break ;   // wait until all items sent
                Application.ProcessMessages ;
                if NOT FQuThreadRunning then break ; // or thread stops
                if NOT FActive then break ;          // or someone else cancels it
                if TestTrgTick (Trg) then break ;    // or we get bored waiting
            end;
        end;
    end ;
    result := (MailImmItems = 0) ;
end;

function TMagMailQueue.WaitSendandStop (secs: integer): boolean;
begin
    result := false ;
    if FFileQuItemsHdr = '' then exit ;
    WaitSend (secs, false) ;   // 8 March 2017
    result := StopMailQu ;
end;

function TMagMailQueue.StopMailQu: boolean;
var
    ID: integer ;
    Trg: longword ;
begin
    result := false ;
    if FFileQuItemsHdr = '' then exit ;
    if FActive then
    begin
        try
            FQuThreadStopping := true ;  // will stop after current item sent
            if Assigned (FMailQuThread) and FQuThreadRunning then
            begin
                ID := FMailQuThread.ThreadId ;
                FMailQuThread.Terminate ;
                PostThreadMessage (ID, WM_QUIT, 0, 0);  // terminate thread
                Trg := GetTrgSecs (2) ;
                while True do   // two seconds, idle thread should stop in 500ms
                begin
                    Application.ProcessMessages ;
                    if NOT FQuThreadRunning then break ;
                    if NOT FActive then break ;
                    if TestTrgTick (Trg) then break ;
                end;
            end ;
        except
        end;
        SaveQuHdrs;
    end;
    MailTotItems := 0 ;
    MailImmItems := 0 ;
    SetLength (MailQuItems, 0) ;
    FActive := false ;
    result := FQuThreadRunning ;
end;

// get new sequential mail ID number - held as a single line in a file

function TMagMailQueue.NewMailSeq: integer ;
var
    CtlMList: TStringList ;
begin
    CtlMList := TStringList.Create ;
    result := 1 ;
    try
        if FileExists (FFileQuItemsCtl) then
        begin
            try
                CtlMList.LoadFromFile (FFileQuItemsCtl) ;
                if CtlMList.Count >= 1 then result := AscToInt (CtlMList [0]) ;
            except
            end ;
        end ;
        if CtlMList.Count = 0 then CtlMList.Add ('x') ;
        CtlMList [0] := Int2StrZ (result + 1 , 8) ;
        try
            CtlMList.SaveToFile (FFileQuItemsCtl) ;
        except
            DoLogEvent (MLogLevelInfo, 'Failed to Save Queue Control File: ' + FFileQuItemsCtl +
                                                                ' - ' + GetExceptMess (ExceptObject)) ;
        end;
    finally
        CtlMList.Free ;
    end ;
end ;

procedure TMagMailQueue.SmtpClientAttachContentTypeEh(Sender: TObject; FileNumber: Integer; var FileName, ContentType: string;
  var AttEncoding: TSmtpEncoding);
//var
//    fext: string ;
begin
//    fext := ExtractFileExt(FileName) ;
{    if (fext = '.txt') or (fext = '.pas') then
    begin
        AttEncoding := smtpEncodeQP ;
        ContentType := 'text/plain' ;
    end;  }
//    ContentType := 'application/octet-stream';
end;

function TMagMailQueue.SaveOneHdr (Item: TMailQuItem): string ;
var
    QueueCols: TStringList ;
    J, K: integer ;
begin
    Result := '' ;
    QueueCols := TStringList.Create ;
    try
        for J := 0 to mqLastField do QueueCols.Add ('') ;
        with Item do
        begin
            QueueCols [mqItemNr]:= IntToStr (ItemNr) ;
            QueueCols [mqNextAttemptDT]:= Date2Packed (NextAttemptDT) ;
            QueueCols [mqAttemptNr]:= IntToStr (AttemptNr) ;
            QueueCols [mqFName] := FName ;
            QueueCols [mqXReceiver] := XReceivers ;
            QueueCols [mqXSender]:= XSender ;
            QueueCols [mqSubject]:= Subject ;
            QueueCols [mqQueuedDT]:= Date2Packed (QueuedDT) ;
            QueueCols [mqPriority]:= IntToStr (Ord (Priority)) ;
            QueueCols [mqLastAttemptDT]:= Date2Packed (LastAttemptDT) ;
            QueueCols [mqBodySize]:= IntToStr (BodySize) ;
            QueueCols [mqLastResp]:= LastResp ;      // following Oct 2015
            QueueCols [mqSmtpMeth] := IntToStr (Ord (SmtpMeth)) ;
            K := mqSmtpSrv1 ;
            for J := 0 to MaxSmtpSrv - 1 do
            begin
                if K >= QueueCols.Count then Break ; // sanity check
                QueueCols [K] := SmtpSrvs [J] ;
                inc (K) ;
            end;
        end ;
        Result := QueueCols.CommaText ;
    finally
        QueueCols.Free ;
    end ;
end;

procedure TMagMailQueue.SaveQuHdrs;
var
    QueueLines: TStringList ;
    I: integer ;
begin
    QueueLines := TStringList.Create ;
    try
        if MailTotItems > 0 then
        begin
            if MailQuIdx.Count <> MailTotItems then BuildQuIdx;  // sanity check
            for I := 0 to MailTotItems - 1 do
            begin
                with PMailQuItem (MailQuIdx [I])^ do   // save in sorted order by NextAttemptDT
                begin
                    if NextAttemptDT = 0 then continue ;  // deleted record
                    if AttemptNr < 0 then continue ;  // deleted record
                    if AttemptNr >= MaxAttempts then continue ;  // Nov 2016
                end ;
                QueueLines.Add (SaveOneHdr (PMailQuItem (MailQuIdx [I])^)) ;
            end ;
        end ;
        try
            MagDeleteFile (FFileQuItemsHdr, true) ;
            QueueLines.SaveToFile (FFileQuItemsHdr) ;
            exit ;
        except
            DoLogEvent (MLogLevelInfo, 'Failed to Save Queue Header File: ' + FFileQuItemsHdr +
                                                                ' - ' + GetExceptMess (ExceptObject)) ;
        end;
    finally
        QueueLines.Free ;
    end ;
end;


function TMagMailQueue.UnQueueMail (Item: integer): boolean ;
var
    I, qunr: integer ;
begin
    result := false ;
    if NOT FActive then
    begin
        DoLogEvent (MLogLevelInfo, 'Mail Queue Not Yet Started') ;
        exit ;
    end;
    if (item <=0) or (item = FSendInProgress) then
    begin
        DoLogEvent (MLogLevelInfo, 'Mail Item Currently Being Sent, or Illegal') ;
        exit ;
    end;

    EnterCriticalSection (FQuItemsCritSect);
    try
        if (MailTotItems <> 0) and (Length (MailQuItems) >= MailTotItems) then // sanity check
        begin
            try
                qunr := -1 ;  ;   // find item in queue
                for I := 0 to MailTotItems - 1 do
                begin
                    if MailQuItems [I].ItemNr = Item then
                    begin
                        qunr := I ;
                        break ;
                    end;
                end;
                if qunr >= 0 then
                begin
                    MagDeleteFile (MailQuItems [qunr].FName, true) ;
                    RemoveQuItem (qunr) ;
                    BuildQuIdx;
                    SaveQuHdrs ;
                    result := true ;
                    DoLogEvent (MLogLevelInfo, 'Removed Queue Record for Item ' + IntToStr (Item)) ;
                end
                else
                    DoLogEvent (MLogLevelInfo, 'Could Not Find Queue Record for Item ' + IntToStr (Item)) ;
            except
                DoLogEvent (MLogLevelInfo, 'Exception Updating Mail Queue - ' + GetExceptMess (ExceptObject)) ;
            end;
        end;
    finally
        LeaveCriticalSection (FQuItemsCritSect) ;
    end ;
end ;

procedure TMagMailQueue.SetActive(const Value: boolean);
begin
    if FActive = Value then exit ;
    if Value then
        StartMailQu
    else
        StopMailQu ;
end;

procedure TMagMailQueue.SetArchiveSent(const Value: boolean);
begin
  FArchiveSent := Value;
end;

procedure TMagMailQueue.SetDebug(const Value: boolean);
begin
  FDebug := Value;
end;

procedure TMagMailQueue.SetDeleteFailed(const Value: boolean);
begin
  FDeleteFailed := Value;
end;

procedure TMagMailQueue.SetLogEvent(const Value: TMailLogEvent);
begin
  FLogEvent := Value;
end;

procedure TMagMailQueue.SetMailQuDir(const Value: string);
begin
  FMailQuDir := Value;
end;

procedure TMagMailQueue.SetMailServers(const Value: TMailServers);
begin
  FMailServers := Value;
end;

procedure TMagMailQueue.SetQuHtmlSmtp(const Value: THtmlSmtpCli);
begin
  FQuHtmlSmtp := Value;
end;

procedure TMagMailQueue.SetQuStartDelay(const Value: integer);
begin
  FQuStartDelay := Value;
end;

procedure TMagMailQueue.SetRetryList(const Value: string);
begin
  FRetryList := Value;
end;

{ TMailServer }

constructor TMailServer.Create(Collection: TCollection);
begin
    inherited;
    FPort := '25' ;
    FAuthType := smtpAuthNone ;
    FSslType := smtpTlsNone  ;
    FSocketFamily := DefaultSocketFamily ;
    FLocalAddr := ICS_ANY_HOST_V4;
    FLocalAddr6 := ICS_ANY_HOST_V6;
    FSslCliSecurity := sslCliSecTls11;  // June 2018 
end;

function TMailServer.GetDisplayName: string;
begin
    if FHost <> '' then
        Result := FHost
    else
        Result := Inherited GetDisplayName
end;

{ TMailServers }

constructor TMailServers.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TMailServer);
end;

function TMailServers.GetItem(Index: Integer): TMailServer;
begin
  Result := TMailServer(inherited GetItem(Index));
end;

procedure TMailServers.SetItem(Index: Integer; Value: TMailServer);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TMailServers.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

end.
