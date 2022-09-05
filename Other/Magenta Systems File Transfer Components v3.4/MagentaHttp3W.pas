unit MagentaHttp3W;
// W version supports widestring/Unicode for Delphi 2007 and earlier
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
Updated by Angus Robertson, Magenta Systems Ltd, England, 18th November 2008
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMagHttp is a high level HTTP Delphi component that allows downloading of
multiple files from an HTTP server using full URLs, or listed by parsing
links from a web page, using a single function call.  The component handles
listing local and remote files, and comparing them to avoid unnecessary
transfers, selection using a file mask, unzipping of downloaded files.
A progress event provides various levels of information for logging or
display, depending upon application requirements, and allows downloads
to be cancelled.

TMagHttp descends from ICS THttpCli, and publishes all it's properties and events.
The component is undocumented, except for source code comments, but end user help is
available describing most of the functionality in Magenta Systems DUN Manager
application (from http://www.magsys.co.uk/dunman/), look under Scheduled Task
Properties, HTTP Download. DUN Manager also provides a compiled test application for
TMagHttp (but DM source code is not available).

Requires Internet Component Suite (ICS) V6 or V7 from http://www.overbyte.be/,
dated August 2008 or later.

Compatible with Delphi 7/2005/2006/2007/2009
Tested with Windows 98, NT4, Windows 2000, XP, 2003, Vista and 2008

Requires Kevin Boylan's TVCLZip component for zipping from http://www.vclzip.net/,
if you don't purchase this component you will need to suppress DEFINE Zipping from
MAGZIP.INC so the zip code is not linked.

TMagHttp is copyrighted software, but may be used freely.
Main functions:

Download - download a list of URLs, optionally parsing HTML for links



08 May 2001  - baseline
6 June 2001  - added ReplRO to replace read only files on download
              support # at end of URL for section
8 June 2001  - added Fr to TFileRec elements so names are unique
14 June 2001 - remove section from URL
25 June 2001 - added LogLevelDelimFile and LogLevelDelimTot to return info
                 for each file suitable for further processing
19 July 2001 - using InetParseDate in magsubs4, ICS fix
10 Sept 2001 - allow for bad dates, corrected build URL relative to root
12 Oct 2001  - / at start of URL is in root so ignore directory
               keep parsed file if mask matches
26 Oct 2001  - skip blank or * lines
12 Dec 2001  - added TTaskResult to distinguish results better
               clear LastResponse before http in case rubbish appears
22 Dec 2001  - added unzipping of downloads
08 Oct 2002  - no longer using trailing slashes on directories internally, still seems to work
09 Dec 2002  - always using UTC dates
4 Jan 2003   - file level info no longer logs URL scanning
7 March 2003 - added SSL/HTTPS support, needs non-free version of ICS component
7 Apr 2003   - never using UTC dates!!!
                short delay before HEAD otherwise URLs seem to be skipped
2 May 2003   - changed GetDirList with new params
17 Jun 2003  - added duration (ms) to LogLevelDelimFile events
24 Aug 2003  - ensure unzip directory exists, might help trap unzipping errors
               unzip errors in task log
11 Oct 2003  - added unzip message handler, VCLZIP 3 only
30 Oct 2003  - strip of section and argument when parsing URLs
31 Dec 2003  - using magsubs1 for common stuff instead of magsubs4
28 Aug 2004  - ICS ParseUrl has moved from httpprot to icsurl
               override WaitUntilReady in ftpcli.pas to avoid slow down problems
30 Sept 2004 - supporting NTLM authentication, messing with progress messages
13 Jan 2005  - removed sleep from WaitUntilReady
30 July 2005 - TFindList moved to magclasses.pas
3 Nov 2005   - fixed FTP links for skipped
28 Nov 2005  - now only supporting ICS v5 SSL dated Nov 2005 or later
31 Dec 2005  - fix for divide by zero on fast download
               support for icslogger (not here)
18 Sep 2006  - unit is now MagentaHttp supporting ICS V6
6 Nov 2006   - new SSL session cache
03 Mar 2008  - added DelimActualSize which if non-zero is actual size relating to duration
               fix content field parsing for text failed if multiple arguments
7 Aug 2008   - updated for latest ICS V6 and V7, and for Delphi 2009 compatibility
11 Sept 2008 - Unicode vesion unit renamed MagentaFtpW with TMagFtpW
22 Oct 2008  - using OverbyteIcsFtpSrvWT
18 Nov 2008  - renamed to MagentaHttp3W


PENDING - recognise UTF8 encoded files, decode them and download UTF8 file names


}
interface

{$I MAGZIP.INC}

uses
  Windows, Messages, SysUtils, Classes, Forms, Masks,
  MagentaHtmlPars, MagentaCopy3W, MagSubs1, MagClasses,
  OverbyteIcsHttpProt, OverbyteIcsLibrary, OverbyteIcsWSocket,
  OverbyteIcsUrl, OverbyteIcsFtpSrvWT
  {$IFDEF Zipping} , VCLZip, VCLUnZip, kpZipObj {$ENDIF}
  {$IFDEF USE_SSL} , OverbyteIcsSSLEAY, OverbyteIcsLIBEAY, OverbyteIcsSslSessionCache {$ENDIF} ;

type
{$IFDEF USE_SSL}
  TMagHttpW = class(TSslHttpCli)
{$ELSE}
  TMagHttpW = class(THttpCli)
{$ENDIF}
  private
    { Private declarations }
        fCancelFlag: boolean ;
        fProgFileSize: integer ;
        fProgMessBase: string ;
        fMagFileCopy: TMagFileCopyW ;
{$IFDEF USE_SSL}
        fSslSessCache: boolean ;
        fSslContext: TSslContext ;
        fExternalSslSessionCache: TSslAvlSessionCache ;
{$ENDIF}

  protected
    { Protected declarations }

        fUrlList: String ;
        fSrcMask: String ;
        fDownDir: String ;
        fKeepPath: boolean ;
        fKeepHost: boolean ;
        fParseHTML: Boolean ;
        fRepl: TFileCopyRepl ;
        fReplRO: boolean ;
        fLogFiles: boolean ;
        fLogProt: boolean ;
        fLogLDir: boolean ;
        fLogRDir: boolean ;
        fCopyEvent: TBulkCopyEventW ;
        fTotProcFiles: integer ;
        fProcOKFiles: integer ;
        fProcFailFiles: integer ;
        fSkippedFiles: integer ;
        fReqResponse: string ;

      {$IFDEF Zipping}
        fZipDownDel: Boolean ;
        fZipped: Boolean ;
        fZipPath: TZipPath ;
        fZipDir: String ;
     {$ENDIF}

    procedure doCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString) ;
    procedure onHttpDataEvent (Sender : TObject; Buffer : Pointer; Len : Integer);
    procedure onHttpCommand (Sender: TObject; var S: String) ;
    procedure onHttpHeaderData (Sender : TObject);
    procedure onHttpSessionConnected (Sender : TObject);
    procedure onHttpLocationChange(Sender : TObject);
    procedure onAuthStep (Sender : TObject);
    procedure EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
{$IFDEF USE_SSL}
    procedure OnHttpSslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert : TX509Base);
    procedure OnHttpSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                    WasReused: Boolean; var IncRefCount : Boolean);
    procedure OnHttpSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                            var FreeSession : Boolean);
    procedure OnHttpSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure OnHttpSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
{$ENDIF}
    procedure onMagCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
    procedure UnZipHandleMessage(Sender: TObject;
              const MessageID: Integer; const Msg1, Msg2: String;
              const flags: Cardinal; var Return: Integer);
    procedure DoRequestSync(Rq : THttpRequest); Override ;

  public
    { Public declarations }
        SrcFiles: TFileRecsW ;
        SrcFileList: TFindList ;
        TotSrcFiles: integer ;
        TarFiles: TFileRecsW ;
        TarFileList: TFindList ;

        constructor Create (Aowner: TComponent) ; override ;
        destructor  Destroy ; override ;
        function Download (CheckFiles: boolean): TTaskResult ;
        procedure Cancel ;

  published
    { Published declarations }
    property UrlList: String            read fUrlList    write fUrlList ;
    property SrcMask: String            read fSrcMask    write fSrcMask ;
    property DownDir: String            read fDownDir    write fDownDir ;
    property KeepPath: Boolean          read fKeepPath   write fKeepPath ;
    property KeepHost: Boolean          read fKeepHost   write fKeepHost ;
    property ParseHTML: Boolean         read fParseHTML  write fParseHTML ;
    property Repl: TFileCopyRepl        read fRepl       write fRepl ;
    property ReplRO: boolean            read fReplRO     write fReplRO ;
    property LogFiles: boolean          read fLogFiles   write fLogFiles ;
    property LogProt: boolean           read fLogProt    write fLogProt ;
    property LogLDir: boolean           read fLogLDir    write fLogLDir ;
    property LogRDir: boolean           read fLogRDir    write fLogRDir ;
    property CopyEvent: TBulkCopyEventW read fCopyEvent  write fCopyEvent ;
{$IFDEF USE_SSL}
    property SslSessCache: boolean      read fSslSessCache write fSslSessCache ;
{$ENDIF}
    property TotProcFiles: integer      read fTotProcFiles ;
    property ProcOKFiles: integer       read fProcOKFiles ;
    property ProcFailFiles: integer     read fProcFailFiles ;
    property SkippedFiles: integer      read fSkippedFiles ;
    property ReqResponse: string        read fReqResponse ;
  {$IFDEF Zipping}
    property ZipDownDel: Boolean       read fZipDownDel     write fZipDownDel ;
    property Zipped: Boolean           read fZipped         write fZipped ;
    property ZipPath: TZipPath         read fZipPath        write fZipPath ;
    property ZipDir: String            read fZipDir         write fZipDir ;
  {$ENDIF}
  end;

    procedure ParseExURL (const url: string; var Proto, User, Pass,
                            Host, Port, Dirs, Fname, Section, Query: string) ;
    function BuildExURL (const Proto, User, Pass, Host, Port,
                                Dirs, Fname, Section, Query: string): string ;
    function RelativeName (Dirs, Rname: string): string ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagHttpW]);
end;

constructor TMagHttpW.Create(Aowner:TComponent);
begin
    inherited create(AOwner);
 // winsock bug fix for fast connections
    CtrlSocket.ComponentOptions := [wsoNoReceiveLoop] ;
    fMagFileCopy := TMagFileCopyW.Create (self) ;
    fMagFileCopy.CopyEvent := onMagCopyEvent ;
    OnDocData := onHttpDataEvent ;
    OnCommand := onHttpCommand ;
    OnHeaderData := onHttpHeaderData ;
    OnSessionConnected := onHttpSessionConnected ;
    OnLocationChange := onHttpLocationChange ;
{$IFDEF USE_SSL}
    fSslSessCache := true ;
    fExternalSslSessionCache := nil ;  // 9 Nov 2005
    OnSslVerifyPeer := OnHttpSslVerifyPeer ;
    OnSslCliGetSession := OnHttpSslCliGetSession ;
    OnSslCliNewSession := OnHttpSslCliNewSession ;
    OnSslHandshakeDone := OnHttpSslHandshakeDone ;
    OnSslCliCertRequest := OnHttpSslCliCertRequest ;
    fSslContext := TSslContext.Create (self) ;
    SslContext := fSslContext ;
    SslContext.SslVerifyPeer := false ;
 {      SslContext.SslCertFile    := CertFileEdit.Text;
        SslContext.SslPassPhrase  := PassPhraseEdit.Text;
        SslContext.SslPrivKeyFile := PrivKeyFileEdit.Text;
        SslContext.SslCAFile      := CAFileEdit.Text;
        SslContext.SslCAPath      := CAPathEdit.Text;
        SslContext.SetAcceptableHostsList(AcceptableHostsEdit.Text);
 }
{$ENDIF}
//  OnNTLMAuthStep := OnAuthStep ;
    SrcFileList := TFindList.Create ;
    TarFileList := TFindList.Create ;
    SetLength (SrcFiles, 0) ;
    SetLength (TarFiles, 0) ;
    TotSrcFiles := 0 ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fCancelFlag := false ;
end ;

destructor TMagHttpW.Destroy;
begin
    FreeAndNil (fMagFileCopy) ;
    FreeAndNil (SrcFileList) ;
    FreeAndNil (TarFileList) ;
{$IFDEF USE_SSL}
    FreeAndNil (fExternalSslSessionCache) ;
    FreeAndNil (fSslContext) ;
{$ENDIF}
    inherited Destroy;
end;

procedure TMagHttpW.DoRequestSync(Rq : THttpRequest);
begin
    DoRequestAsync(Rq);

    if FMultiThreaded then begin
        while FState <> httpReady do begin
            FCtrlSocket.ProcessMessages;
        end;
    end
    else begin
        while FState <> httpReady do begin
{$IFNDEF NOFORMS}
            Application.ProcessMessages;
            if Application.Terminated then begin
                Abort;
                break;
            end;
{$ELSE}
            FCtrlSocket.ProcessMessages;
{$ENDIF}
        end;
    end;

{* Jul 12, 2004
   WARNING: The component now doesn't consider 401 status
            as a fatal error (no exception is triggered). This required a
            change in the application code if it was using the exception that
            is no more triggered for status 401.
*}
    if FStatusCode > 401 then
        raise EHttpException.Create(FReasonPhrase, FStatusCode);
end;

// borrowed from httpapp

function StatusString(StatusCode: Integer): string;
begin
    case StatusCode of
        100: Result := 'Continue';                                 {do not localize}
        101: Result := 'Switching Protocols';                      {do not localize}
        200: Result := 'OK';                                       {do not localize}
        201: Result := 'Created';                                  {do not localize}
        202: Result := 'Accepted';                                 {do not localize}
        203: Result := 'Non-Authoritative Information';            {do not localize}
        204: Result := 'No Content';                               {do not localize}
        205: Result := 'Reset Content';                            {do not localize}
        206: Result := 'Partial Content';                          {do not localize}
        300: Result := 'Multiple Choices';                         {do not localize}
        301: Result := 'Moved Permanently';                        {do not localize}
        302: Result := 'Moved Temporarily';                        {do not localize}
        303: Result := 'See Other';                                {do not localize}
        304: Result := 'Not Modified';                             {do not localize}
        305: Result := 'Use Proxy';                                {do not localize}
        400: Result := 'Bad Request';                              {do not localize}
        401: Result := 'Unauthorized';                             {do not localize}
        402: Result := 'Payment Required';                         {do not localize}
        403: Result := 'Forbidden';                                {do not localize}
        404: Result := 'Not Found';                                {do not localize}
        405: Result := 'Method Not Allowed';                       {do not localize}
        406: Result := 'None Acceptable';                          {do not localize}
        407: Result := 'Proxy Authentication Required';            {do not localize}
        408: Result := 'Request Timeout';                          {do not localize}
        409: Result := 'Conflict';                                 {do not localize}
        410: Result := 'Gone';                                     {do not localize}
        411: Result := 'Length Required';                          {do not localize}
        412: Result := 'Unless True';                              {do not localize}
        500: Result := 'Internal Server Error';                    {do not localize}
        501: Result := 'Not Implemented';                          {do not localize}
        502: Result := 'Bad Gateway';                              {do not localize}
        503: Result := 'Service Unavailable';                      {do not localize}
        504: Result := 'Gateway Timeout';                          {do not localize}
    else
        Result := '';
    end
end;

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path[?query]         }

// break down URL into its constituents

procedure ParseExURL (const url: string; var Proto, User, Pass, Host, Port,
                                         Dirs, Fname, Section, Query: string) ;
var
    path: string ;
    nsep1, nsep2: integer ;
begin
//    HttpProt.ParseURL (url, Proto, User, Pass, Host, Port, path) ;
    OverbyteIcsURL.ParseURL (url, Proto, User, Pass, Host, Port, path) ;
    Dirs := '' ;
    Fname := '' ;
    Section := '' ;
    Query := '' ;
    if path = '' then exit ;
    nsep1 := Pos ('#', path) ;

// remove section from path and keep it
    if (nsep1 > 0) then
    begin
        if nsep1 < length (path) then
        begin
            Section := copy (path, succ (nsep1), 999) ;
            nsep2 := Pos ('?', Section) ;
            if (nsep2 > 0) then
            begin
                if nsep2 < length (Section) then
                    Query := copy (Section, succ (nsep2), 999) ;
                Section := copy (Section, 1, pred (nsep2)) ;
            end ;
        end ;
        path := copy (path, 1, pred (nsep1)) ;
    end
// remove query from path and keep it
    else
    begin
        nsep2 := Pos ('?', path) ;
        if (nsep2 > 0) then
        begin
            if nsep2 < length (path) then
                Query := copy (path, succ (nsep2), 999) ;
            path := copy (path, 1, pred (nsep2)) ;
        end ;
    end ;

// remove file name from path and keep it and Dirs separately (no leading / on either)
    nsep1 := LastDelimiter ('/', path);
    if nsep1 > 1 then Dirs := copy (path, 2, pred (nsep1)) ;
    Fname := copy (path, succ (nsep1), 99) ;
end ;

// build URL from the many (optional) constituent parts

function BuildExURL (const Proto, User, Pass, Host, Port,
                                 Dirs, Fname, Section, Query: string): string ;
begin
    result := Proto + '://' ;
    if User <> '' then
    begin
        Result := Result + User ;
        if Pass <> '' then Result := Result + ':' + Pass ;
        Result := Result + '@' ;
    end ;
    Result := Result + Host ;
    if Port <> '' then Result := Result + ':' + Port ;
    Result := Result + '/' ;
    if Dirs <> '' then
    begin
        Result := Result + Dirs ;
        if Dirs [Length (Dirs)] <> '/' then Result := Result + '/' ;
    end ;
    Result := Result + Fname ;
    if Section <> '' then Result := Result + '#' + Section ;
    if Query <> '' then Result := Result + '?' + Query ;
end ;

// builds a new relative combined Dirs/Fname from relative name
// /magsys/other/ and ../mbs/index.htm gives /magsys/mbs/index.htm
// /magsys/other/ and /mbs/index.htm gives /mbs/index.htm

function RelativeName (Dirs, Rname: String): string ;
var
    nsep: integer ;
begin
    result := Rname ;
    Dirs := trim (Dirs) ;
    if Dirs <> '' then
    begin
        if Dirs [Length (Dirs)] = '/' then
              SetLength (Dirs, Pred (Length (Dirs))) ;  // remove last /
    end ;
    while (Pos ('../', Rname) = 1) do
    begin
        Rname := Copy (Rname, 4, 999) ;  // remove ../
        nsep := LastDelimiter ('/', Dirs) ;
        if nsep > 1 then
            Dirs := Copy (Dirs, 1, pred (nsep))
        else
            Dirs := '' ;
    end ;
    if Length (Rname) = 0 then exit ;
    if (Rname [1] = '/') then Dirs := '' ;    // assume root
    if Dirs <> '' then
        Result := Dirs + '/' + Rname
    else
        Result := Rname ;
end ;

procedure TMagHttpW.onMagCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString ;
                                                  var Cancel: boolean) ;
begin
    doCopyEvent (LogLevel, Info) ;
    Cancel := fCancelFlag ;
end ;

procedure TMagHttpW.doCopyEvent (LogLevel: TLogLevel ; Info: UnicodeString) ;
begin
    if Assigned (CopyEvent) then
    begin
        CopyEvent (LogLevel, Info, fCancelFlag) ;
    end ;
end ;

procedure TMagHttpW.onHttpSessionConnected (Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '= Connected to: ' + HostName) ;
end ;

procedure TMagHttpW.onHttpLocationChange(Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '= ' + FURL + ' Redirected to: ' + FLocation) ;
end ;

procedure TMagHttpW.onHttpCommand (Sender: TObject; var S: String) ;
begin
    doCopyEvent (LogLevelDiag, '> ' + S) ;
end;

procedure TMagHttpW.onHttpHeaderData (Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '< ' + LastResponse) ;
end ;

procedure TMagHttpW.onAuthStep (Sender : TObject);
begin
    doCopyEvent (LogLevelDiag, '= NTLM Authorisation Step ' + LastResponse) ;
end ;

procedure TMagHttpW.onHttpDataEvent (Sender : TObject; Buffer : Pointer; Len : Integer);
var
    info: string ;
begin
    if fProgFileSize < 0 then exit ;
    info := fProgMessBase + ', ' + IntToCStr (RcvdStream.Size) ;
    if fProgFileSize > 0 then info := info + ' of ' +  IntToCStr (fProgFileSize)  ;
    doCopyEvent (LogLevelProg, info) ;
end ;

procedure TMagHttpW.EndUnZipEvent (Sender: TObject; FileIndex: Integer; FName: String) ;
var
    newsize: integer ;
begin
    if FName = '' then exit ;
    newsize := GetSizeFile (Fname) ;
    doCopyEvent (LogLevelFile, 'Unzipped OK: ' + Fname + ', Size: '
                                                     + IntToCStr (newsize)) ;
    doCopyEvent (LogLevelDelimFile, 'Unzipped|' + Fname +
                                   '|' + IntToStr (newsize) + '|1|0|OK|0|0') ;
end ;

procedure TMagHttpW.UnZipHandleMessage(Sender: TObject;
          const MessageID: Integer; const Msg1, Msg2: String;
          const flags: Cardinal; var Return: Integer);
begin
    doCopyEvent (LogLevelFile, 'Fatal Unzip Error: ' + Msg1) ;
    Return := 0 ;
end;

{$IFDEF USE_SSL}
procedure TMagHttpW.OnHttpSslVerifyPeer(Sender: TObject;
                                            var Ok: Integer; Cert : TX509Base);
begin
    { Alternate verification takes place in event HandshakeDone, we }
    { accept anything temporarily. }
    if Ok <> 0 then
        Cert.CustomVerifyResult := X509_V_OK;
    OK := 1;
    doCopyEvent (LogLevelDiag, 'Received SSL certificate; Subject: ' +
            Cert.SubjectOneLine + ';Issuer:  ' + Cert.IssuerOneLine +
                                   ';Verify result: ' + Cert.VerifyErrMsg);
end ;


procedure TMagHttpW.OnHttpSslCliNewSession(Sender: TObject; SslSession: Pointer;
                                    WasReused: Boolean; var IncRefCount : Boolean) ;
var
    HttpCli: TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    doCopyEvent (LogLevelDiag, 'Starting SSL Session');
    if not fSslSessCache then Exit;
    if (not WasReused) then
    begin
        HttpCli := (Sender as TSslHttpCli);
        fExternalSslSessionCache.CacheCliSession(SslSession,
               HttpCli.CtrlSocket.PeerAddr + HttpCli.CtrlSocket.PeerPort,
                                                                IncRefCount);
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: New');
    end
    else
        doCopyEvent (LogLevelDiag, 'Cache SSL Session: Reuse');
    IncRefCount := False;
end;

procedure TMagHttpW.OnHttpSslCliGetSession(Sender: TObject; var SslSession: Pointer;
                                                            var FreeSession : Boolean);
var
    HttpCli: TSslHttpCli;
begin
    { SslCliNewSession/SslCliGetSession allow external, client-side session }
    { caching.                                                              }
    if not fSslSessCache then Exit;
    doCopyEvent (LogLevelDiag, 'Check for Old SSL Session');
    HttpCli := (Sender as TSslHttpCli);
    SslSession := fExternalSslSessionCache.GetCliSession(
         HttpCli.CtrlSocket.PeerAddr + HttpCli.CtrlSocket.PeerPort,
                                                              FreeSession);
    FreeSession := True;
end;

procedure TMagHttpW.OnHttpSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                    PeerCert: TX509Base; var Disconnect: Boolean);
var
    CertChain: TX509List;
    HttpCli: TSslHttpCli;
//    Msg       : String;
//    I         : Integer;
//    IP        : String;
//    Hash      : String;
begin
    HttpCli := Sender as TSslHttpCli;
    with HttpCli.CtrlSocket do
    begin
        if ErrCode = 0 then
            doCopyEvent (LogLevelDiag, 'SSL Handshake Done OK, Secured with ' +
                SslVersion + ', Cipher ' + SslCipher + ', Secret Bits ' +
                IntToStr (SslSecretBits) + ' (Total ' + IntToStr (SslTotalBits) + ')')
        else
            doCopyEvent (LogLevelDiag, 'SSL Handshake Error #' + IntToStr(ErrCode));
        CertChain := SslCertChain;
    end ;
    doCopyEvent (LogLevelDiag, IntToStr(CertChain.Count) + ' Certificate(s) in the verify chain') ;
    if (ErrCode <> 0) or (HttpCli.CtrlSocket.SslSessionReused) or
                       not HttpCli.SslContext.SslVerifyPeer then
        Exit;
    exit ;
(*
    IP   := HttpCli.CtrlSocket.GetPeerAddr;
    Hash := PeerCert.Sha1Hash;

    if HttpCli.SslAcceptableHosts.IndexOf(IP + Hash) > -1 then
        Exit;

    HttpCli.Abort;

    if CertChain.Count > 0 then
    begin
        Msg := 'Certificates in chain:'+ #13#10;
        for I := 0 to CertChain.Count -1 do
        begin
            if Length(Msg) > 0 then
                Msg := Msg + #13#10;
            Msg := Msg +  IntToStr(I + 1) + ')' +
                ' SubjectCommonName: ' + CertChain[I].SubjectCName + #13#10 +
                ' Last VerifyResult: ' + CertChain[I].VerifyErrMsg + #13#10;
        end;
    end;

    { Post connection check in case of SSL handshake went well.              }
    { Checks whether argument HostOrIp matches dnsName or commonName         }
    { of a peer certificate, this is the string-argument of the function.    }
    { For demo purposes a simple, constant string is passed.                 }
    if (not PeerCert.PostConnectionCheck('www.overbyte.be')) then
    begin
        if Length(Msg) > 0 then
            Msg := Msg + #13#10;
        if MessageDlg(Msg + 'Post connection check:'#13#10 +
                      'The name specified in the peer certificate is '#13#10 +
                      'invalid or does not match the site!'#13#10#13#10 +
                      'Do you want to trust the connection anyway?',
                       mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
            Disconnect := True;
            Display('Post connection check failed, peer will be disconnected');
        end
        else begin
            { Add peer cert to the trusted certs. }
            HttpCli.SslAcceptableHosts.Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
        end;
    end
    else begin
        if MessageDlg(Msg + #13#10 +
                      'Peer certificate was issued to the site.'#13#10#13#10 +
                      'Do you want to trust the connection anyway?',
                       mtWarning, [mbYes, mbNo], 0) <> mrYes then
        begin
            Disconnect := True;
            Display('Custom verification failed, peer will be disconnected');
        end
        else begin
            { Add peer cert to the trusted certs. }
            HttpCli.SslAcceptableHosts.Add(IP + Hash);
            PostMessage(Handle, WM_SSL_NOT_TRUSTED, 0, 0);
        end;
    end;  *)
end;

procedure TMagHttpW.OnHttpSslCliCertRequest(Sender: TObject; var Cert: TX509Base);
//var
//    X : TX509Base;
begin
    doCopyEvent (LogLevelDiag, 'Certificate Request Ignored') ;
    { A very simple test of the SslCliCertRequest event.               }
    { This event is triggered only if CertFileEdit.Text is empty,      }
    { the server requested a certificate from the client,              }
    { and of course only in case of the SSL session wasn't reused.     }
 (*   if not Assigned(FClientCerts) then
    begin
        { Create a pool of client certs }
        ClientCertDlg.CertListBox.Clear;
        FClientCerts := TX509List.Create(Self);
        try
            X := FClientCerts.Add;
            X.LoadFromPemFile('01cert.pem');
            X.PrivateKeyLoadFromPemFile('01key.pem', 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
            X := FClientCerts.Add;
            X.LoadFromPemFile('client.pem', True, 'password');
            ClientCertDlg.CertListBox.Items.Add(X.SubjectOneLine);
        except
            FreeAndNil(FClientCerts);
            raise
        end;
    end;
    ClientCertDlg.CertListBox.ItemIndex := 0;
    if ClientCertDlg.ShowModal = mrOK then
        Cert := FClientCerts[ClientCertDlg.CertListBox.ItemIndex];   *)
end;
{$ENDIF}


// HTTP download multiple local files
// returns false if error, with ReqResponse completed
{ Syntax of acceptable URL: protocol://[user[:password]@]server[:port]/path }

function TMagHttpW.Download (CheckFiles: boolean): TTaskResult ;
var
    fnamesrc, fnametar, newtardir, dochref: string ;
    ret: boolean ;
    tempdir, info, curURL, curresp, headfield, headdata, content: string ;
    hostname: string ;
    I, J, K, L, newsize, donenr, totsize, nsep, pagesize, statcode: integer ;
    initURLs, attrs: integer ;
    pageDT, modDT: TDateTime ;
    starttick: DWORD ;
    fstarttick, duration: longword ;
    NewList: TStringList ;
    SrcFileRec: PTFileRecW ;
    DownloadStream: TMemoryStream ;
    CMask: TMask ;
    Proto, User, Pass, Host, Port, Dirs, Fname, Section, Query: String ;  // parsed URL
    HTMLParser: THTMLParser;
    HTMLObj: TObject ;
    HTMLTag: THTMLTag ;
    HTMLParam: THTMLParam ;
    {$IFDEF Zipping} VCLUnZip: TVCLUnZip ;{$ENDIF}

    procedure sysDelayX (aMs: longword);
    var
        TickCount: longword;
    begin
        TickCount := GetTickCount ;
        while ((GetTickCount - TickCount) < aMs) do
        begin
            Application.ProcessMessages;
           //  MessagePump;  // in HTTP client
            if Application.Terminated then break ;
        end ;
    end;

begin
    fReqResponse := 'No Errors' ;
    fTotProcFiles := 0 ;
    fProcOKFiles := 0 ;
    fProcFailFiles := 0 ;
    fSkippedFiles := 0 ;
    fCancelFlag := false ;
    NewList := TStringList.Create ;
    DownloadStream := TMemoryStream.Create ;
    CMask := TMask.Create (Trim (Lowercase (fSrcMask))) ;  // create match mask object
    result := TaskResOKNone ;
    {$IFDEF Zipping}
    VCLUnZip := Nil ;
    if fZipped then
    begin
          VCLUnZip := TVCLUnZip.Create (self) ;
          VCLUnZip.OnHandleMessage := UnZipHandleMessage ;
          VCLUnZip.OnEndUnZip := EndUnZipEvent ;
    end ; {$ENDIF}
    try  // finally
    fDownDir := ExcludeTrailingBackslash (fDownDir) ;

// build list of source files from HTTP URLs
    doCopyEvent (LogLevelInfo, 'Locating files to Download') ;
    NewList.SetText (Pchar (fUrlList)) ;
    if NewList.Count = 0 then
    begin
        result := TaskResFail ;
        fReqResponse := 'No HTTP URLs Specified' ;
        exit ;
    end ;

// main loop
    SetLength (SrcFiles, 1000) ;
    SrcFileList.Clear ;
    TotSrcFiles := 0  ;
    I := 0 ;
    initURLs := NewList.Count ;
    while (I < NewList.Count) do   // URLs may get added to this list during parsing
    begin
        Application.ProcessMessages ;
        if fCancelFlag then exit ;
        curURL := trim (NewList [I]) ;
        inc (I) ;
        if curURL = '' then continue ;
        if curURL [1] = '*' then continue ;
        modDT := 0 ;
        pageDT := 0 ;
        pagesize := 0 ;
        UserName := '' ;
        Password := '' ;

    // break down URL, see if rebuilding without logon
        ParseExURL (curURL, Proto, User, Pass, Host, Port, Dirs,
                                                    Fname, Section, Query) ;
        if (Dirs = '') or (Section <> '') then
        begin
   //         if Dirs = '' then Dirs := '/' ;  get added automatically
            curURL := BuildExURL (Proto,
                            User, Pass, Host, Port, Dirs, Fname, '', Query) ;
        end ;
        ParseExURL (curURL, Proto, User, Pass, Host, Port, Dirs,
                                                    Fname, Section, Query) ;
        if User <> '' then
        begin
            UserName := User ;
            Password := Pass ;
            curURL := BuildExURL (Proto, '', '', Host, Port, Dirs,
                                                    Fname, Section, Query) ;
        end ;
        hostname := Host ;  // get host name only less www and .coxxx
        if Pos ('.', hostname) > 1 then
        begin
            if Pos ('www.', hostname) = 1 then hostname := Copy (hostname, 5, 99) ;
            if Pos ('web.', hostname) = 1 then hostname := Copy (hostname, 5, 99) ;
            K := Pos ('.', hostname) ;
            if ((Length (hostname) - K) <= 6) then
                                    hostname := Copy (hostname, 1, Pred (K)) ;
        end ;
        if (Pos ('http', Proto) <> 1) then     // Sept 2001, allow blank files????
        begin
            if Length (curURL) <> 0 then doCopyEvent (LogLevelInfo,
                                                 'Invalid URL: ' + curURL) ;
            continue ;
        end ;

    // Initializes Openssl library on create - 9 Nov 2005
        if (Pos ('https', Proto) = 1) then
        begin
{$IFDEF USE_SSL}
            if not Assigned (fExternalSslSessionCache) then
            begin
                fExternalSslSessionCache := TSslAvlSessionCache.Create (self) ;
         //       fExternalSslSessionCache.AdjustTimeout := True;
         //       fExternalSslSessionCache.SessionTimeOut := 30;
         //       fExternalSslSessionCache.FlushInterval := 3000;
            end;
{$ENDIF}
        end ;
        doCopyEvent (LogLevelDiag, CRLF + 'Get Headers for URL: ' + curURL) ;
        doCopyEvent (LogLevelProg, 'Get Headers for URL: ' + curURL) ;
        URL := curURL ;
        fProgMessBase := 'Downloading Headers' ;
        fProgFileSize := -1 ;  // don't show progress for headers only
        sysDelayX (20) ;  // short delay, too rapid requests seem to die,  7 Apr 2003 TEMP !!!!
        try
            Head ;   // get sync header only
            statcode := StatusCode ;
        except
            doCopyEvent (LogLevelInfo, 'Exception Accessing URL: ' +
                                                    GetExceptMess (ExceptObject)) ;
            statcode := StatusCode ;
            if statcode = 200 then statcode := 999 ;
        end ;
        if statcode <> 200 then
        begin
            doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' + curURL + ', ' +
                        StatusString (statcode) + ' (' + IntToStr (statcode) + ')') ;
            doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                            '|0|0|1|Can Not Access URL: ' + LastResponse + '|0|0') ;
            continue ;
        end ;
        if RcvdHeader.Count = 0 then continue ;
        for J := 0 to Pred (RcvdHeader.Count) do
        begin
            curresp := RcvdHeader [J] ;
        // now check response
            nSep := pos (':', curresp);
            if nSep < 4 then continue ;  // no valid header, give up
            headfield := LowerCase (Copy (curresp, 1, nSep - 1));
            headdata  := Copy (curresp, nSep + 2, 99);
            if headfield = 'last-modified' then
            begin
                try
                    modDT := InetParseDate (headdata) ;
                except
                    modDT := 0 ;
                end ;
            end ;
            if headfield = 'date' then
             begin
                try
                   pageDT := InetParseDate (headdata) ;
                except
                    pageDT := 0 ;
                end ;
            end ;
            if headfield = 'content-type' then content := headdata ;
            if headfield = 'content-length' then
                                            pagesize := AscToInt (headdata) ;
        end ;
        if modDT = 0 then modDT := pageDT ;
        if modDT = 0 then modDT := Now ;

      // keeping this file, create file record
        info := LowerCase (ExtractUNIXName (Fname)) ;
        if fParseHTML and (fSrcMask <> '*.*') then   // allow blank names if all
        begin
            if NOT CMask.Matches (info) then info := '' ; // check against mask
        end ;
        if info <> '' then
        begin
            inc (TotSrcFiles) ;
            if Length (SrcFiles) <= TotSrcFiles then
                                SetLength (SrcFiles, Length (SrcFiles) * 2) ;
            with SrcFiles [pred (TotSrcFiles)] do
            begin
                FrFileName := Fname ;
                FrSubDirs := '/' ;
                if fKeepHost then FrSubDirs := FrSubDirs + hostname + '/' ;
                if fKeepPath then FrSubDirs := FrSubDirs + Dirs ;
                FrFullName := curURL ;
                FrDirLevel := 0 ;
                FrFileAttr := 0 ;
                FrFileDT := modDT ;
                FrFileUDT := modDT ;
                FrFileBytes := pagesize ;
                FrExtra := '' ;
                FrFileCopy := FCStateNone ;
                FrLinks := '' ;
            end ;
        end ;

    // see if need to get and parse an html file, DON'T PARSE FILES WE LINKED !!!!
        if fParseHTML and (I <= initURLs) and (Pos ('text/html', content) = 1) then  // 22 Feb 2008 allow for more stuff in content field
        begin
            doCopyEvent (LogLevelDiag, CRLF + 'Get: ' + curURL) ;
            fProgMessBase := 'Downloading URL: ' + curURL ;
            fProgFileSize := pagesize ;   // keep size for event handler
            fLastResponse := '' ;   // clear in case rubbish appears
            DownloadStream.Clear ;
            RcvdStream := DownloadStream ;
            onHttpDataEvent (Self, Nil, 0) ;
            Application.ProcessMessages ;
            sysDelayX (20) ;  // short delay, too rapid requests seem to die,  7 Apr 2003 TEMP !!!!
            try
                Get ;   // get sync header and body
                statcode := StatusCode ;
            except
                statcode := StatusCode ;
                if statcode = 200 then statcode := 999 ;
            end ;
            if statcode <> 200 then
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' +
                    curURL + ', ' + ReasonPhrase { HttpApp.StatusString (statcode)} +
                                           ' (' + IntToStr (statcode) + ')') ;
                doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                            '|0|0|1|Can Not Access URL: ' + ReasonPhrase +
                                     ' (' + IntToStr (statcode) + ')' + '|0|0') ;
                continue ;
            end ;
            DownloadStream.Seek (0, soFromBeginning) ;
            if DownloadStream.Size = 0 then continue ;

        // parse file into tags
            doCopyEvent (LogLevelDiag, CRLF + 'Parse Content for: ' + curURL) ;
            Application.ProcessMessages ;
            HTMLParser := THTMLParser.Create;
            try
            HTMLParser.Lines.LoadFromStream (DownloadStream) ;
            HTMLParser.Execute;
            if HTMLParser.Parsed.Count = 0 then continue ;
            doCopyEvent (LogLevelDiag, CRLF + 'Checking Content for: ' + curURL) ;
            for J := 0 to Pred (HTMLParser.Parsed.Count) do
            begin
                HTMLObj := HTMLParser.Parsed [J] ;
                if HTMLObj.ClassType = THTMLTag then
                begin
                    HTMLTag := THTMLTag (HTMLObj) ;

                // found an Anchor tag, look for link name
                    if HTMLTag.Name <> 'A' then continue ;
                    if HTMLTag.Params.Count = 0 then continue ;
                    for K := 0 to Pred (HTMLTag.Params.Count) do
                    begin
                        HTMLParam := HTMLTag.Params [K] ;
                        if HTMLParam.Key <> 'HREF' then continue ;
                        dochref := Trim (Lowercase (HTMLParam.Value)) ;
                        L := Pos ('#', dochref) ;   // 30 Oct 2003, strip off section and arguments
                        if L > 6 then dochref := Copy (dochref, 1, Pred (L)) ;
                        L := Pos ('?', dochref) ;
                        if L > 6 then dochref := Copy (dochref, 1, Pred (L)) ;
                        if dochref = '' then continue ;
                        doCopyEvent (LogLevelDiag, 'Found Link: ' + dochref) ;
                        if Pos ('ftp://', dochref) > 0 then continue ;
                        if Pos ('mailto:', dochref) > 0 then continue ;
                        info := LowerCase (ExtractUNIXName (dochref)) ;
                        if (info <> '') or (fSrcMask <> '*.*') then   // allow blank names if all
                        begin
                            if NOT CMask.Matches (info) then continue ; // check against mask
                        end ;
                        if Pos ('http', dochref) = 1 then
                            info := HTMLParam.Value
                        else
                            info := BuildExURL (Proto, User, Pass, Host, Port,
                            '', RelativeName (Dirs, HTMLParam.Value), '', '') ;
                        if NewList.IndexOf (info) = -1 then  // not if we have it already
                        begin
                            doCopyEvent (LogLevelDiag, 'Saved URL: ' + info) ;  // TEMP !!!!
                            NewList.Add (info) ;
                        end ;
                    end ;
                    Application.ProcessMessages ;
                    if fCancelFlag then exit ;
                end ;
            end ;
            finally
                HTMLParser.Free ;
            end ;
        end ;
    end ;
    SetLength (SrcFiles, TotSrcFiles) ;
    if TotSrcFiles = 0  then
    begin
        result := TaskResFail ;
        fReqResponse := 'No Source Files Found' ;
        exit ;
    end ;

// build sorted list of source records, then remove duplicates
    doCopyEvent (LogLevelDiag, 'Sorting URLs') ;  // TEMP !!!!
    SrcFileList.Capacity := TotSrcFiles ;
    for I := 0 to Pred (TotSrcFiles) do SrcFileList.Add (@SrcFiles [I]) ;
    SrcFileList.Sort (MagentaCopy3W.CompareFNextW) ;
    doCopyEvent (LogLevelDiag, 'Removing Duplicate URLs') ;  // TEMP !!!!
    for I := 1 to Pred (SrcFileList.Count) do  // may get shorter!!
    begin
        if PTFileRecW (SrcFileList [I]).FrFullName =
                    PTFileRecW (SrcFileList [Pred (I)]).FrFullName then
                                              SrcFileList.Delete (I) ;
        if I >= Pred (SrcFileList.Count) then break ;
    end ;
    TotSrcFiles := SrcFileList.Count ;

// show user the list we found
    if fLogRDir then doCopyEvent (LogLevelInfo, CRLF +
           'Source HTTP Files:' + CRLF_ + FmtFileDirListW (SrcFileList, false)) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// build list of target files, so we don't copy unnecessary stuff
    doCopyEvent (LogLevelFile, 'Target Directory: ' + fDownDir) ;
    if NOT ForceDirs (fDownDir) then
    begin
        result := TaskResFail ;
        fReqResponse := 'Can Not Create Target Directory' ;
        exit ;
    end ;
    fMagFileCopy.GetDirList (fDownDir, '*.*', FCTypeAllDir, true, 0, 0,
                                                            TarFiles, TarFileList) ;
    if fLogLDir then doCopyEvent (LogLevelInfo,
                'Target Files on PC' + CRLF_ + FmtFileDirListW (TarFileList, false)) ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// compare source and target files, see what to copy
    doCopyEvent (LogLevelDiag, 'Checking Files Needed') ;  // TEMP !!!!
    fTotProcFiles := fMagFileCopy.SelectCopyFileList (SrcFileList, TarFileList,
                 '*.*' , FCTypeMaskDir, fRepl, -1, false, '', fSkippedFiles, false) ;
    if fTotProcFiles = 0  then
    begin
        result := TaskResOKNone ;
        if SkippedFiles <> 0 then
            fReqResponse := 'All Source Files Skipped Download'
        else
            fReqResponse := 'No Source Files Selected to Download' ;
        exit ;
    end ;

// find size of stuff to copy
    info := CRLF_ + 'HTTP URLs Selected for Downloading are: ' + CRLF_ ;
    newsize := 0 ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy = FCStateSelect then
            begin
                newsize := newsize + FrFileBytes ;
                if CheckFiles then info := info + FrFullName +
                            ', Size ' + IntToCStr (FrFileBytes) + CRLF_ ;
            end ;
        end ;
    end ;
    if CheckFiles then doCopyEvent (LogLevelInfo, info) ;
    doCopyEvent (LogLevelInfo, 'HTTP URLs Skipped ' +
                                            IntToStr (SkippedFiles)) ;
    doCopyEvent (LogLevelInfo, 'Selected Total Files ' + IntToStr
            (fTotProcFiles) + ', Total Size ' + IntToCStr (newsize)) ;

// stop now if only checking what will be downoaded
    if CheckFiles then
    begin
        result := TaskResOKNone ;
        exit ;
    end ;
    doCopyEvent (LogLevelInfo, 'Started HTTP Download') ;
    Application.ProcessMessages ;
    if fCancelFlag then exit ;

// start real HTTP downloading
    donenr := 0 ;
    totsize := 0 ;
    result := TaskResOKNone ;  // now want to get one file OK
    starttick := GetTickCount ;
    for I := 0 to Pred (TotSrcFiles) do
    begin
        doCopyEvent (LogLevelProg, '') ;  // clear
        Application.ProcessMessages ;
        if fCancelFlag then exit ;
        SrcFileRec := SrcFileList [I] ;
        with SrcFileRec^ do
        begin
            if FrFileCopy <> FCStateSelect then continue ;
            inc (donenr) ;
            curURL := FrFullName ;
            if Length (FrSubDirs) > 1 then
            begin
                newtardir := fDownDir + FrSubDirs ;
                UnixToDosPath (newtardir) ;
                if NOT ForceDirs (newtardir) then
                begin
                    doCopyEvent (LogLevelInfo, 'Can Not Create Directory: ' +
                                                                 newtardir) ;
                    inc (fProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    continue ;
                end ;
            end
            else
                newtardir := fDownDir ;
            fnametar := IncludeTrailingBackslash (newtardir) + FrFileName ;
            fProgMessBase := 'Downloading ' + IntToStr (donenr) +
                                     ' of ' + IntToStr (fTotProcFiles) ;
            doCopyEvent (LogLevelFile,  fProgMessBase + ' - ' + curURL +
                    ' to ' + fnametar + ', Size ' + IntToCStr (FrFileBytes)) ;
            FrFileCopy := FCStateCopying ;
            fProgMessBase := fProgMessBase + CRLF_ + 'URL: ' + curURL ;
            fProgFileSize := FrFileBytes ;   // keep size and data for event handler
            URL := curURL ;
            fLastResponse := '' ;   // clear in case rubbish appears
            DownloadStream.Clear ;
          //      DownloadStream.SetSize (FileBytes) ;  // not sure if this is efficient
            RcvdStream := DownloadStream ;
            onHttpDataEvent (Self, Nil, 0) ;
            duration := 0 ;
            try
                fstarttick := GetTickCount ;
                Get ;   // get sync header and body
                duration := GetTickCount - fstarttick ;
                statcode := StatusCode ;
            except
                statcode := StatusCode ;
                if statcode = 200 then statcode := 999 ;
            end ;
            if (statcode = 200) then
            begin
                inc (fProcOKFiles) ;
                if duration = 0 then duration := 10 ;  // special case of copy OK but duration too short to measure
                DownloadStream.Seek (0, soFromBeginning) ;
                newsize := DownloadStream.Size ;
                if (newsize = FrFileBytes) or (FrFileBytes = 0) then
                begin
                    try
               // replace old file, removing read only if necessary
                        attrs := FileGetAttr (fnametar) ;
                        if attrs >= 0 then    // file exists
                        begin
                            if ((attrs and faReadOnly) <> 0) and fReplRO then
                                                FileSetAttr (fnametar, 0) ;
                            DeleteFile (fnametar) ;
                        end ;
                        DownloadStream.SaveToFile (fnametar) ;
                        inc (totsize, newsize) ;
                        UpdateFileAge (fnametar, FrFileDT) ;  // not UTC date
                        FrFileCopy := FCStateOK ;
                        doCopyEvent (LogLevelFile, 'Download OK, Size: '
                                                     + IntToCStr (newsize)) ;
                        doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                                '|' + IntToStr (newsize) + '|1|0|OK|' +
                                     IntToStr (duration) + '|' + IntToStr (newsize)) ;
                    except
                        doCopyEvent (LogLevelInfo, 'Error Saving File: ' +
                              fnametar + ', ' + GetExceptMess (ExceptObject)) ;
                        inc (fProcFailFiles) ;
                        FrFileCopy := FCStateFailed ;
                      //  statcode := 0 ;
                    end ;

              // see if unzipping it
                    {$IFDEF Zipping}
                    if fZipped and (statcode = 200) and
                          (Lowercase (ExtractFileExt (fnametar)) = '.zip') then
                    begin
                        with VCLUnZip do
                        begin
                            ZipName := fnametar ;    // set the zip filename
                            ReadZip;                 // open it and read its information

                        // check it's not corrupted and not empty
                            if CheckArchive and (Count > 0) then
                            begin

                        // List filenames in zip file
                                info := '' ;
                                if ZipHasComment then info := ZipComment + CRLF ;
                                for J := 0 to Pred (Count) do
                                begin
                                   info := info + Format (sDirLine, [Filename [J],
                                     IntToCStr (UnCompressedSize [J]), ' ',
                                     DateToStr (DateTime [J]) + ' ' + TimeToStr
                                       (DateTime [J]), Pathname[J]]) + CRLF ;
                                end ;
                                doCopyEvent (LogLevelInfo,
                                         'Unzipping Files:' + CRLF + info) ;

                            // extract all files
                                FilesList.Clear ;
                                DoAll := true ;
                                if (fZipDir = '') and (fZipPath >=
                                        PathSpecific) then fZipPath := PathNew ;
                                DestDir := ExtractFileDir (fnametar) ;     // Set destination directory
                                RecreateDirs := false ;
                                RootDir := '' ;   // base subdirectory
                                if fZipPath in [PathOriginal, PathNewOrig,
                                    PathSpecOrig] then RecreateDirs := true ;
                                if fZipPath in [PathNew, PathNewOrig] then
                                   DestDir := ExtractFileDir (fnametar) + '\' +
                                                   ExtractNameOnly (fnametar) ;
                                if fZipPath in [PathSpecific, PathSpecOrig] then
                                                          DestDir := fZipDir ;
                                if NOT ForceDirs (DestDir) then
                                begin
                                    doCopyEvent (LogLevelFile, 'Failed to Create Unzip Dir: ' + DestDir) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                             '|0|0|1|Failed to Create Unzip Dir|0|0') ;
                                    continue ;
                                end ;
                                ReplaceReadOnly := fReplRO ;
                                OverwriteMode := Always ;
                                RetainAttributes := True ;  // Set attributes to original after unzipping
                                J := Unzip;      // Extract files, return value is the number of files actually unzipped
                                if J = Count then
                                begin
                                    doCopyEvent (LogLevelFile, 'Unzipped OK: '
                                                                  + fnametar) ;
                                    if fZipDownDel then
                                    begin
                                        doCopyEvent (LogLevelFile,
                                                     'Deleting: ' + fnametar) ;
                                        DeleteFile (fnametar) ;
                                        doCopyEvent (LogLevelDelimFile, fnametar +
                                         '| |0|0|0|File Deleted After Unzipping|0|0') ;
                                    end ;
                                end
                                else
                                begin
                                    doCopyEvent (LogLevelFile, 'Failed to Unzip: ' + fnametar) ;
                                    doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                             '|0|0|1|Failed to Unzip File|0|0') ;
                                end ;
                            end
                            else
                            begin
                                doCopyEvent (LogLevelInfo, 'Zip File Corrupted:' + fnametar) ;
                                doCopyEvent (LogLevelDelimFile, fnametar + '|' + DestDir +
                                         '|0|0|1|Zip File Corrupted|0|0') ;
                            end ;
                            ClearZip;
                        end ;
                    end ;  {$ENDIF}
                 end
                 else
                 begin
                    inc (fProcFailFiles) ;
                    FrFileCopy := FCStateFailed ;
                    if newsize >= 0 then
                        doCopyEvent (LogLevelFile, 'Request Failed: Partial File Downloaded')
                    else
                        doCopyEvent (LogLevelFile, 'Request Failed: No File Downloaded') ;
                    doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                                '|0|0|1|Download Failed: ' + LastResponse + '|' +
                                                       IntToStr (duration) + '|0') ;
                end ;
            end
            else
            begin
                doCopyEvent (LogLevelInfo, 'Can Not Access URL: ' + curURL + ', ' +
                        StatusString (statcode) + ' (' + IntToStr (statcode) + ')') ;
                doCopyEvent (LogLevelDelimFile, curURL + '|' + fnametar +
                                '|0|0|1|Download Failed: ' + LastResponse + '|' +
                                                    IntToStr (duration) + '|0') ;
                inc (fProcFailFiles) ;
                FrFileCopy := FCStateFailed ;
            end ;
       end ;
    end ;
    if NOT fCancelFlag then
    begin
        result := TaskResOKNone ;
        if fProcFailFiles <> 0 then
            result := TaskResFail
        else
        begin
            if fProcOKFiles <> 0 then result := TaskResOKNew ;
        end ;
        I := (GetTickCount - starttick) + 1 ;  // stop division by zero
        if (I > 100000) or (totsize > 1000000) then
            newsize := totsize div (I div 1000)
        else
            newsize := (totsize * 1000) div I ;
        doCopyEvent (LogLevelInfo, 'Finished, files downloaded OK: ' +
            IntToCStr (fProcOKFiles) + ', failed: ' + IntToStr
            (fProcFailFiles) + ', skipped: ' + IntToStr (fSkippedFiles)) ;
        doCopyEvent (LogLevelDelimTot, 'URLs|' + fDownDir + '|' +
                IntToStr (totsize) + '|' + IntToStr (fProcOKFiles) +
                           '|' + IntToStr (fProcFailFiles) + '|Totals|') ;
        doCopyEvent (LogLevelInfo, 'Total bytes downloaded ' + IntToCStr (totsize) +
            ', duration ' + TimerToStr (SecsToTime (I div 1000)) +
            ', average speed ' +  IntToCStr (newsize) + ' chars/sec') ;
    end ;
    finally
        {$IFDEF Zipping}
        if fZipped and (Assigned (VCLUnZip)) then VCLUnZip.Free ;  {$ENDIF}
{$IFDEF USE_SSL}
        if Assigned (fExternalSslSessionCache) then
                                            fExternalSslSessionCache.Flush;
{$ENDIF}
        CMask.Free ;
        NewList.Free ;
        DownloadStream.Free ;
        if fCancelFlag then
        begin
            result := TaskResAbort ;
            fReqResponse := 'Cancelled Downloading Files' ;
        end ;
   end ;
end;

procedure TMagHttpW.Cancel ;
begin
    fCancelFlag := true ;
    Abort ;
end ;


end.
