{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  SSL FTP server sample, no real GUI, really designed to be a
              Windows service application. It supports multiple SSL hosts
              with multiple listeners, can order it's own SSL certificates
              and will create self signed certificates for any missing,
              and will email status information and errors to an administrator.
              If turned into a Windows service, this sample is really a commercial
              FTP server.
Creation:     Oct 2020
Updated:      Apr 2022
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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

Note this FTP server sample uses the functionality of the existing sample,
OverbyteIcsSslFtpServ1 for OverbyteIcsFtpSrv and uses the same account files.

This sample is non-interactive, FTP servers are normally run as windows background
servers.  All the server settings come from an INI file which will need to be
edited before the sample will successfully run.  A bare sample INI file is included
which will be copied into the ICS shared INI directory on first run, with the
actual file name shown when you start the application, and that is the file
to edit.

Unlike the other FTP server sample, this one uses IcsHosts to support multiple
addresses and ports and SSL certificates, but all of these must exist and not being
used by other applications, otherwise the server will not start.  To use SSL, a
host name must be specified and at least the certificate file name, if the
certificate file can not be found or is missing a self signed SSL certificate
will be created automatically so the server can start. Tries to use the specified
certificate directory, TEMPDIR if none specified. Up to 100 IcsHosts can be
specified, you can edit the Windows HOSTS file if necessary to create alternate
host names for your PC, if you don't have a local DNS server to do it.

This FTP server will automatically order and install SSL certificates if so required,
from various suppliers, including free certificates from Let's Encrypt, and commercial
certificates for Digicert, Comodo, Thawte and GeoTrust from CertCentre AG.  For
automated ordering, Domain Validation is used which means the web server must be
accessible from the public internet by all the host names for which an SSL
certificate is being ordered. See OverbyteIcsSslX509Certs.pas fore more info.



History:
8 Oct 2020 - V8.65 baseline
12 Apr 2022 V8.69 Added OCSP Stapling and certification revoke support, needs
                    OcspSrvStapling=True adding to INI file.  Caches OCSP responses
                    in file specified in INI OcspCacheFile, defaulting to
                    ocspftpsrvcache.recs in application directory.
                  Auto ordering certificates works again.


Insalling note:
The main FTP server configuration is in OverbyteIcsSslMultiFtpServ.ini file, a
default file is included in the \ics\Samples\Delphi\SslInternet directory, and
is copied into the ICS temporary working directory, usually
c:\Users\(login)\AppData\Local\ICS. This INI file will need to be edited for
IP addresses, SSL certificates, etc, although some defaults may work.
Specifically Host3 has certificate ordering settings but is disabled and will
need new valid public hosts, IP addresss and files names before it will do
anything useful.

Once this demo has been run once, any changes to the default INI file from new versions
(ie auto certificate ordering) will need to copied manually into the working file.

See OverbyteIcsWSocketS.pas for documentation on TSslWSocketServer whose
properties are exposed by TSslHttpAppSrv, and for IcsHosts which allows the web
server to support multiple Hosts on multiple IP addresses and ports with SSL
support, including automatic SSL certificate ordering.

The server also uses a second INI file for accounts authorised to access the
FTP server and their permissions, this is in the same directory as the sample.
This sample application uses the same ftpaccounts-default.ini file for all
IcsHosts which is opened after a remote client connects, but you can use a
different file for each host.

Sample entry from ftpaccounts-default.ini, one section for each login:

[ics]
Password=ics
ForceSsl=false
HomeDir=c:\temp
OtpMethod=none
ForceHomeDir=true
HidePhysicalPath=true
ReadOnly=false
 
[anonymous]
Password=*
ForceSsl=false
HomeDir=c:\temp
OtpMethod=none
ForceHomeDir=true
HidePhysicalPath=true
ReadOnly=true


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMultiFtpServ1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, ComCtrls, TypInfo,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsWndControl,
  OverbyteIcsUtils, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
  OverbyteIcsLogger, OverbyteIcsSslX509Utils,
  OverbyteIcsFtpSrv,
  OverbyteIcsFtpSrvT,
  OverbyteIcsOneTimePw,
  OverbyteIcsSmtpProt,
  OverbyteIcsTicks64,
  OverbyteIcsSslX509Certs,
  OverbyteIcsMailQueue,
  OverbyteIcsBlacklist,     { TIcsBuffLogStream }
  OverbyteIcsSslHttpRest;   { V8.69 }

const
    SrvCopyRight : String = ' OverbyteIcsSslMultiFtpServ (c) 2022 Francois Piette V8.69 ';
    MaxWinChars = 800000;
    WM_STARTUP = WM_USER + 712 ;
    LogNameMask = '"ftpsrv-"yyyymmdd".log"' ;
    WebLogHdrMask = '"#Date: "' + ISODateMask + #32 + ISOTimeMask + '"';

  { account INI file layout }
    KeyPassword             = 'Password';
    KeyHomeDir              = 'HomeDir';
    KeyOtpMethod            = 'OtpMethod';
    KeyForceHomeDir         = 'ForceHomeDir';
    KeyHidePhysicalPath     = 'HidePhysicalPath';
    KeyReadOnly             = 'ReadOnly';
    KeyForceSsl             = 'ForceSsl';

type
  TGetProcessingThread = class; { Forward declaration }

  { We use our own client class to hold our thread and other local stuff }
  TMyClient = class(TFtpCtrlSocket)
  private
      FWorkerThread    : TGetProcessingThread;
      ReportSslCert    : Boolean;
  end;

  TGetProcessingThread = class(TThread)
  protected
    Server : TFtpServer;
    Client : TMyClient;
  public
      procedure Execute; override;
  end;

type
  TFtpServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;
    Timer1: TTimer;
    RecheckCertsButton: TButton;
    IcsSslX509Certs: TSslX509Certs;
    IcsMailQueue: TIcsMailQueue;
    SslFtpServer1: TSslFtpServer;
    procedure WMCMSTARTUP (var Msg : TMessage); message WM_STARTUP ;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RecheckCertsButtonClick(Sender: TObject);
    procedure IcsSslX509CertsCertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure IcsSslX509CertsNewCert(Sender: TObject);
    procedure IcsSslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
    procedure IcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);
    procedure StartQueueMail;
    procedure StopQueueMail;
    procedure SendAdminEmail (const EmailTo, Subject, Body: string) ;
    procedure IcsSslX509CertsChallengeDNS(Sender: TObject;
      ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure SslFtpServer1ValidateRnFr(Sender: TObject; Client: TFtpCtrlSocket;
      var FilePath: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1AlterDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure SslFtpServer1AnswerToClient(Sender: TObject;
      Client: TFtpCtrlSocket; var Answer: TFtpString);
    procedure SslFtpServer1Authenticate(Sender: TObject; Client: TFtpCtrlSocket;
      UserName, Password: TFtpString; var Authenticated: Boolean);
    procedure SslFtpServer1BgException(Sender: TObject; E: Exception;
      var CanClose: Boolean);
    procedure SslFtpServer1BuildDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
    procedure SslFtpServer1BuildFilePath(Sender: TObject;
      Client: TFtpCtrlSocket; const Directory, FileName: string;
      var NewFileName: string);
    procedure SslFtpServer1ChangeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1ClientCommand(Sender: TObject;
      Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
    procedure SslFtpServer1ClientConnect(Sender: TObject;
      Client: TFtpCtrlSocket; AError: Word);
    procedure SslFtpServer1ClientDisconnect(Sender: TObject;
      Client: TFtpCtrlSocket; AError: Word);
    procedure SslFtpServer1Display(Sender: TObject; Client: TFtpCtrlSocket;
      Msg: TFtpString);
    procedure SslFtpServer1GetProcessing(Sender: TObject;
      Client: TFtpCtrlSocket; var DelayedSend: Boolean);
    procedure SslFtpServer1MakeDirectory(Sender: TObject;
      Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1OtpGetPassword(Sender: TObject;
      Client: TFtpCtrlSocket; UserName: TFtpString; var UserPassword: string);
    procedure SslFtpServer1OtpMethod(Sender: TObject; Client: TFtpCtrlSocket;
      UserName: TFtpString; var OtpMethod: TOtpMethod);
    procedure SslFtpServer1Rein(Sender: TObject; Client: TFtpCtrlSocket;
      var Allowed: Boolean);
    procedure SslFtpServer1RetrDataSent(Sender: TObject; Client: TFtpCtrlSocket;
      Data: TWSocket; AError: Word);
    procedure SslFtpServer1RetrSessionClosed(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
    procedure SslFtpServer1RetrSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
    procedure SslFtpServer1SslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslFtpServer1SslServerName(Sender: TObject; var Ctx: TSslContext;
      var ErrCode: TTlsExtError);
    procedure SslFtpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure SslFtpServer1Start(Sender: TObject);
    procedure SslFtpServer1Stop(Sender: TObject);
    procedure SslFtpServer1StorSessionConnected(Sender: TObject;
      Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
    procedure SslFtpServer1ValidateDele(Sender: TObject; Client: TFtpCtrlSocket;
      var FilePath: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1ValidatePut(Sender: TObject; Client: TFtpCtrlSocket;
      var FilePath: TFtpString; var Allowed: Boolean);
    procedure SslFtpServer1Host(Sender: TObject; Client: TFtpCtrlSocket;
      Host: TFtpString; var Allowed: Boolean);
  private
    FIniFileName : String;
    FFinalized   : Boolean;
    FOtpSequence : integer;
    FOtpSeed     : String;
    FIniRoot     : String;
    procedure WorkerThreadTerminated(Sender : TObject);
  public
    procedure Display(Msg : String);
    procedure ReportHosts;
    function UnEscapeStr (const Buffer: string): string ;
    property IniFileName : String read FIniFileName write FIniFileName;
end;

var
    FtpServerForm: TFtpServerForm;
    WinLinesBuff: string ;
    WinDispCur: Integer;
    ProgDirectory: String;
    LogDate: Integer;
    SrvCompName: string;
    StatSrvSslCert: String;
    StatSrvSslCertWeb: String;
    HouseKeepingTrg: int64;
    CertCheckTrigger: int64 ;
    DiagLogBuffer: TIcsBuffLogStream;
    AdminEmailTo: String;
    LastErrorEmail: String;

implementation

{$R *.DFM}

const
    SectionData        = 'Data';
    SectionGeneraL     = 'General';
    KeyServLogDir      = 'ServLogDir';
    KeyAdminEmailTo    = 'AdminEmailTo';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ we update DisplayMemo once a second in the timer }
procedure TFtpServerForm.Display(Msg : String);
var
    J, K: Integer;
begin
    J := Pos ('-login', Msg) ;    // hide password
    if J > 10 then begin
        K := Pos ('+', Copy (Msg, J, 99)) ;
        if K > 4 then begin
            K := K + J ;
            for J := K to (K + 10) do begin
                if J > Length (Msg) then break ;
                Msg [J] := '#' ;
            end ;
        end ;
    end ;

//    DisplayMemo.Lines.Add (Msg) ;    // !!! TEMP
    WinLinesBuff := WinLinesBuff + Msg + icsCRLF ;
    if Assigned (DiagLogBuffer) then begin
        try
            DiagLogBuffer.WriteLine (Msg);
        except
        end ;
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.Timer1Timer(Sender: TObject);
var
    displen, removelen, newstart: integer ;
    S1: String ;
    NewFlag: Boolean;
begin
  // check SSL certificates every two hours, may order expired certs
    if IcsTestTrgTick64 (CertCheckTrigger) then begin { V8.57 }
        CertCheckTrigger := IcsGetTrgMins64 (120) ;
        try
          // don't stop on first error, no exceptions
            Display('Regular Server Recheck for New SSL Certificates Starting');
            DiagLogBuffer.FlushFile(True);
            NewFlag := SslFtpServer1.RecheckSslCerts(S1, False, True);
            if NewFlag or (S1 <> '') then  begin
                if NewFlag then Display('Server Recheck Loaded New SSL Certificate(s)');
                Display('Server Recheck SSL Certificate Errors:' + icsCRLF + S1);
                if (S1 <> '') and (LastErrorEmail <> S1) then { v8.60 tell someone }
                    SendAdminEmail (AdminEmailTo, 'ICS Multi FTP Server Recheck SSL Certificate Errors', S1);
                LastErrorEmail := S1 ;
                ReportHosts;    // report everything again
                Display('Listen Bindings:' + icsCRLF + SslFtpServer1.ListenStates);
            end
            else
                Display('Server Recheck SSL Certificate Nothing New');

        except
            on E:Exception do begin
               Display('Server Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
    end;

  // rotate logs at midnight, recheck SSL certificates, log configuration again
    if LogDate <> Trunc(Date) then begin
        LogDate := Trunc(Date);

      { flush all logs with old file name, new stuff will be new date }
        try
            if Assigned (DiagLogBuffer) then
                DiagLogBuffer.FlushFile(True);
        except
        end ;
        Display('Nightly Server Recheck Starting');
        ReportHosts;    // report everything again
        Display('Listen Bindings:' + icsCRLF + SslFtpServer1.ListenStates);
        CertCheckTrigger := Trigger64Immediate; { V8.57 }
        DiagLogBuffer.FlushFile(True);
    end;

  // see if updating the log window with multiple lines
    displen := Length (WinLinesBuff) ;
    if displen > 0 then begin
        try
            if WinDispCur + displen > MaxWinChars then begin
                S1 := DisplayMemo.Lines.Text ;
                removelen := MaxWinChars - displen - 20000 ;
                if removelen > 20000 then begin
                    S1 := copy (S1, removelen, 9999999) ;
                    newstart := Pos(#13, S1) ; // find start of next line
                    DisplayMemo.Text := copy (S1, newstart, 9999999) + WinLinesBuff ;
                    WinDispCur := Length (S1) - newstart + displen ;
                end
                else begin
                    DisplayMemo.Lines.Text := WinLinesBuff ;
                    WinDispCur := displen ;
                end;
            end
            else begin
                SetLength (WinLinesBuff, displen - 2) ;  // remove CRLF
                DisplayMemo.Lines.Add (WinLinesBuff) ;
                WinDispCur := WinDispCur + displen ;
                SendMessage (DisplayMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;
        except
        end ;
        WinLinesBuff := '' ;
    end;

 // house keeping every five minutes
    if IcsTestTrgTick(HouseKeepingTrg) then begin
        HouseKeepingTrg := IcsGetTrgMSecs64 (300);
        // ??
    end;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.ApplicationEventsException(Sender: TObject;    { V8.64 }
  E: Exception);
begin
    Display('!!! Application Exception Event - ' + IcsGetExceptMess (E));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormCreate(Sender: TObject);
var
    List: TStringList;
    FIniFile: TIcsIniFile;
    FName, FHdr: String;
begin
    FIniFileName := GetIcsIniFileName;
    FIniRoot := LowerCase(ExtractFilePath(Application.ExeName));
  // ensure SSL DLLs come from program directory, and exist, else die
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
    GSSLEAY_LOAD_LEGACY := True;     { V8.69 OpenSSL 3.0 legacy provider for old algorithms }
    Application.OnException := ApplicationEventsException ;
    ProgDirectory := ExtractFileDir(IcsLowercase (ParamStr(0)));
    GSSL_DLL_DIR := ProgDirectory + '\';
    GSSL_SignTest_Check := True;
    GSSL_SignTest_Certificate := True;
    OverbyteIcsWSocket.LoadSsl;
    LogDate := Trunc(Date);
    HouseKeepingTrg := IcsGetTrgSecs64 (300);
    CertCheckTrigger := Trigger64Disabled;
    SrvCompName := IcsGetCompName ;

   { try and open INI file in C:\Users\(me)\AppData\Local\ICS, if missing copy
        default from sample directory  }
    try
        if NOT FileExists(FIniFileName) then begin
            FName := ChangeFileExt(IcsLowercase(ParamStr(0)), '.ini');
            if FileExists(FName) then begin
                List := TStringList.Create;
                try
                    List.LoadFromFile(FName);
                    List.SaveToFile(FIniFileName);
                    Display('Copied default INI file to: ' + FIniFileName + ', where it should be edited');
                except
                    List.Free;
                end;
            end;
        end;
    except
    end;
    FIniFile := TIcsIniFile.Create(FIniFileName);
    FName := FIniFile.ReadString(SectionGeneral, KeyServLogDir,  '');
    AdminEmailTo := FIniFile.ReadString(SectionGeneral, KeyAdminEmailTo,  '');
    IcsLoadMailQuFromIni(FIniFile, IcsMailQueue, 'MailQueue');
    FIniFile.Free;
    CertCheckTrigger := Trigger64Disabled;
    HouseKeepingTrg := Trigger64Disabled;
    Timer1.Enabled := True;  // needed for log window display
    if FName <> '' then begin
        ForceDirectories(ExtractFileDir(FName));
        FName := '"' + IncludeTrailingPathDelimiter(FName) + '"' + LogNameMask;  // file name is a mask to add date
        FHdr :=  '"' + SrvCopyRight + IcsCRLF +
         'Log File Created: "dd mmm yyyy hh:mm:ss"' + IcsCRLF +
         'Computer: ' + SrvCompName + '"' + IcsCRLF ;
        DiagLogBuffer := TIcsBuffLogStream.Create (Self, FName, FHdr, FileCPUtf8);
        Display('Log File: ' + DiagLogBuffer.FullName);
    end;
    Display('INI file: ' + FIniFileName);
    DiagLogBuffer.FlushFile(True);
    PostMessage (Handle, WM_STARTUP, 0, 0) ;
end;

procedure TFtpServerForm.FormShow(Sender: TObject);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.RecheckCertsButtonClick(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Immediate;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
begin
    if FFinalized then Exit;
    FFinalized := True;
    CertCheckTrigger := Trigger64Disabled;
    HouseKeepingTrg := Trigger64Disabled;
    StopButtonClick(Self);
    StopQueueMail;
    FreeAndNil(DiagLogBuffer);
    Timer1.Enabled := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.WMCMSTARTUP (var Msg : TMessage);
begin
    StartButtonClick(Self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServerForm.UnEscapeStr (const Buffer: string): string ;
var
    I, TxLen: integer ;
    Ch: Char ;
    curDT: TDateTime;

    procedure BuildTxString (Ch2: Char) ;
    begin
        inc (TxLen) ;
        if Length (result) <= TxLen then SetLength (result, TxLen * 2) ;
        result [TxLen] := Ch2 ;
    end ;

    procedure BuildTxString2 (S2: string) ;
    begin
        if Length (result) <= (TxLen + Length (S2)) then
                                SetLength (result, TxLen + Length (S2) + 2) ;
        Move (S2 [1], result [TxLen + 1], Length (S2)) ;
        inc (TxLen, Length (S2)) ;
    end ;

begin
    result := '' ;
    if Length (Buffer) = 0 then exit ;
    curDT := Now ;
    SetLength (result, Length (Buffer) * 2) ;
    TxLen := 0 ;
    I := 1 ;
    while I <= Length (Buffer) do
    begin
        Ch := Buffer [I] ;
        inc (I) ;
        if Ch <> '\' then
            BuildTxString (Ch)
        else
        begin
            if I > Length (Buffer) then break ;
            Ch := Buffer [I] ;  // escape, get next character
            inc (I) ;
            if Ch = 'n' then
            begin
                BuildTxString (IcsCR) ;
                BuildTxString (IcsLF) ;
            end
            else if Ch = 'c' then
                BuildTxString (IcsCR)
            else if Ch = 'l' then
                BuildTxString (IcsLF)
            else if Ch = '\' then
                BuildTxString ('\')
            else if (Ch = 'd') then
            begin
                BuildTxString2 (DatetoStr (curDT) + Icsspace + TimeToStr (curDT)) ;
            end
            else if (Ch = 'o') then
            begin
                BuildTxString2 (FormatDateTime (ISODateTimeMask, curDT)) ;
            end
            else if (Ch = 't') then
            begin
                BuildTxString2 (TimeToStr (curDT)) ;
            end
            else if (Ch = 'p') then
            begin
                BuildTxString2 (SrvCompName) ;
            end
            else if (Ch = 's') then
            begin
                BuildTxString2 (OverbyteIcsFtpSrv.CopyRight) ;
            end
            else
            begin
                BuildTxString ('\') ;  // unknown escape, send both chars
                BuildTxString (Ch) ;
            end ;
        end ;
    end ;
    SetLength (result, TxLen) ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.ReportHosts;
var
    I: Integer;
begin
    StatSrvSslCert := '';
    Display('Total server hosts ' + IntToStr(SslFtpServer1.IcsHosts.Count)) ;
    for I := 0 to SslFtpServer1.IcsHosts.Count - 1 do begin
        with SslFtpServer1.IcsHosts[I] do begin
            if CertValRes = chainFail then begin
                Display('Server SSL Certificate Errors - ' + DisplayName);
              { might want to stop here and warn user }
           //     Exit;
            end;

        // build site info for log and status page
            StatSrvSslCert := StatSrvSslCert + 'Site Host ' + IntToStr (I) +
             ' [' + DisplayName + '] ' + HostNames.CommaText + icsCRLF +
             'Bindings: ' + BindInfo + icsCRLF +
             'SSL Security Level: ' + GetEnumName(TypeInfo(TSslSrvSecurity), Ord(SslSrvSecurity)) + IcsCRLF +
                    'SSL Certificate: ' + CertErrs + IcsCRLF + CertInfo + IcsCRLF + icsCRLF;
        end;
    end;
    StatSrvSslCertWeb := StringReplace (StatSrvSslCert, icsCRLF, '<BR>' + icsCRLF, [rfReplaceAll]);
    Display(StatSrvSslCert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StartButtonClick(Sender: TObject);
var
    Errs, S: String;
    FIniFile: TIcsIniFile;
begin
    try
      // tell them who we are
        Display(SrvCopyRight + icsCRLF + 'SSL Version: ' + OpenSslVersion +
                                    ', Dir: ' + GLIBEAY_DLL_FileName + icsCRLF);

      // main web server settings from INI file, built in CA if no file found
        FIniFile := TIcsIniFile.Create(FIniFileName);
        IcsLoadFtpServerFromIni(FIniFile, SslFtpServer1, 'FtpServer');
        if (SslFtpServer1.RootCA = '') or (NOT FileExists(SslFtpServer1.RootCA)) then
                                         SslFtpServer1.RootCA := sslRootCACertsBundle;

      // V8.65 ordering X509 certificates may need a proxy server
        IcsSslX509Certs.ProxyURL := FIniFile.ReadString ('FtpServer', 'X509ProxyURL', '') ;
        SslFtpServer1.OcspSrvHttp.CacheFName := FIniFile.ReadString ('FtpServer', 'OcspCacheFile', 'ocspftpsrvcache.recs') ;  { V8.69 }
        SslFtpServer1.OcspSrvHttp.OcspHttpProxy := IcsSslX509Certs.ProxyURL;                                   { V8.69 }

      // read the server hosts from INI file and check SSL files exist
        IcsLoadIcsHostsFromIni(FIniFile, SslFtpServer1.IcsHosts, 'Host');
        if SslFtpServer1.IcsHosts.Count <= 0 then begin
            Display('Can Not Start Server - No Server Hosts Configured') ;
            exit ;
        end;
        Display('Number of Hosts Configured: ' + IntToStr(SslFtpServer1.IcsHosts.Count));  { V8.64 }
        FIniFile.Free;
    except
        on E:Exception do begin
           Display('Failed to Get FTP server settings - ' + E.Message);
            Exit;
        end;
    end;

    // validate hosts and keep site certificiate information
    try
        Errs := SslFtpServer1.ValidateHosts(False, True); // don't stop on first error, no exceptions
        if Errs <> '' then begin
            Display('Server Validation Errors:' + icsCRLF + Errs);
            if AdminEmailTo <> '' then
                SendAdminEmail (AdminEmailTo, 'ICS Multi FTP Server Validation Errors', Errs);
        end;
        ReportHosts;
        Display('Required Listen Bindings:' + icsCRLF + SslFtpServer1.ListenStates);
    except
        on E:Exception do begin
            Display('Host Validation Failed, Server Stopped - ' + E.Message);
            Exit;
        end;
    end;

  // setup some FTP server defauls, most were done in IcsLoadTFtpAppSrvFromIni
    S := '';
    try
        SslFtpServer1.ClientClass := TMyClient;
        SslFtpServer1.SocketErrs := wsErrFriendly ;
        SslFtpServer1.ExclusiveAddr := true ;
        if SslFtpServer1.TimeoutSecsIdle < 30 then SslFtpServer1.TimeoutSecsIdle := 30;  // sanity check
        if Pos ('\n', SslFtpServer1.Banner) > 0 then SslFtpServer1.Banner := UnEscapeStr (SslFtpServer1.Banner);
        if SslFtpServer1.Banner = '' then SslFtpServer1.Banner := '220 ICS FTP Server';
        if SslFtpServer1.BandwidthLimit > 0 then
            Display('Bandwidth Limited to ' + IntToKByte (SslFtpServer1.BandwidthLimit)) ;
        Display('Server options: ' + IcsSetToStr (TypeInfo (TFtpsOption), SslFtpServer1.Options, 4)) ;
        SslFtpServer1.Start ;
        if NOT SslFtpServer1.ListenAllOK then
            S := 'Failed to Start, '
        else
             S := 'Started OK, ';
        S := S + 'Listen Bindings:' + IcsCRLF + SslFtpServer1.ListenStates;
        Display(S);
        CertCheckTrigger := IcsGetTrgSecs64 (15) ;
        if (AdminEmailTo <> '') then
            SendAdminEmail (AdminEmailTo, 'ICS Multi FTP Server Started',
              'ICS Multi FTP Server' + IcsCRLF + IcsCRLF + S + IcsCRLF + Errs);
        DiagLogBuffer.FlushFile(True);
        StopButton.Enabled := True;
        StartButton.Enabled := False;
    except
        on E:Exception do begin
           Display('Failed to start FTP server - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StopButtonClick(Sender: TObject);
begin
    if NOT StopButton.Enabled then Exit;
    IcsSslX509Certs.CloseAccount;
    CertCheckTrigger := Trigger64Disabled;
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    DiagLogBuffer.FlushFile(True);
    SslFtpServer1.Stop;
    if (AdminEmailTo <> '') then
        SendAdminEmail (AdminEmailTo, 'ICS Multi FTP Server Stopped',
                                    'ICS Multi FTP Server Stopped' + IcsCRLF);
    Display('Server stopped');
    DiagLogBuffer.FlushFile(True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StartQueueMail;
var
    S: string ;
    I: integer ;
begin
    try
        if NOT IcsMailQueue.Active then begin
            if (IcsMailQueue.MailQuDir = '') or
                         (NOT ForceDirectories (IcsMailQueue.MailQuDir)) then begin
                Display('!! Failed to Start Mail Queue, No Directory');
                exit ;
            end;

        end;
        if IcsMailQueue.MailServers.Count = 0 then begin
            Display('!! Failed to Start Mail Queue, No Mail Servers');
            exit ;
        end;
        S := '';
        for I := 0 to IcsMailQueue.MailServers.Count - 1 do begin
          { the email account password should be encrypted, here we decrypt it
            IcsMailQueue.MailServers[I].Password := Decrypt(
                                IcsMailQueue.MailServers[I].Password) ;  }
            S := S + IcsMailQueue.MailServers[I].Host + ', ';
        end;
        IcsMailQueue.Active := true ;
        if IcsMailQueue.Active then begin
             Display('Started Mail Queue OK, Servers: ' + S);
        end;
    except
         Display('!! Failed to Start Mail Queue - ' +  IcsGetExceptMess(ExceptObject)) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.StopQueueMail;
begin
    if IcsMailQueue.Active then begin
        if (IcsMailQueue.MailImmItems > 0) then
                 Display('Waiting up to 30 seconds to send Queued Mail Items');
        IcsMailQueue.WaitSendandStop (30);
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SendAdminEmail(const EmailTo, Subject, Body: string) ;  { V8.60 }
var
    sTempFrom: string ;
    id: integer ;
begin
    if EmailTo = '' then Exit;
    if NOT IcsMailQueue.Active then StartQueueMail ;  // restart if not running
    if NOT IcsMailQueue.Active then begin
         Display('Error Starting Mail Queue') ;
    end ;
    try
        with IcsMailQueue.QuHtmlSmtp do begin
            sTempFrom := '"' + SrvCompName + '" <' + SrvCompName + '@magsys.co.uk>' ;
            EmailFiles.Clear ;
            RcptName.clear;
            Allow8bitChars := true ;
            ContentType := smtpPlainText ;
            WrapMessageText := false ;
            WrapMsgMaxLineLen := 76 ;
            PlainText.Clear ;
            PlainText.Text := SrvCopyRight  + IcsCRLF + IcsCRLF + Body ;
            Display('Queuing Email to ' + EmailTo + ' from ' + sTempFrom) ;
            RcptName.Clear ;
            RcptName.Add (EmailTo) ;
            FromName := sTempFrom ;
            HdrCc := '' ;  // Aug 2014
            HdrTo := EmailTo ;
            HdrFrom := sTempFrom ;
            HdrReplyTo := sTempFrom ;
            HdrSubject := Subject ;
            XMailer := '' ;
            id := IcsMailQueue.QueueMail ;
            if id > 0 then
            begin
       // done OK
                Display('Email Form Queued OK') ;
            end
            else
            begin
                Display('Failed to Queue Email: ' + ErrorMessage) ;
            end ;
        end ;
    except
        Display('Failed to Queue Email: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called by the FTP component once it has built the   }
{ directory listing. We can use this handler to alter the listing, adding   }
{ or removing some info. This sample add the 'virtual' directory.           }
procedure TFtpServerForm.SslFtpServer1AlterDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\' then
        Exit;
    { Add our 'virtual' directory to the list }
    if Detailed then begin
        { We need to format directory lines according to the Unix standard }
        Buf :=
        'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1AnswerToClient(Sender: TObject;
  Client: TFtpCtrlSocket; var Answer: TFtpString);
begin
    Display('> ' + Client.GetPeerAddr + ' ' + Answer)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Authenticate(Sender: TObject;
  Client: TFtpCtrlSocket; UserName, Password: TFtpString;
  var Authenticated: Boolean);
begin
  { One Time Passwords - keep sequence and seed for next login attempt }
    if Client.OtpMethod > OtpKeyNone then begin
        if not Authenticated then exit;
        Display('! ' + Client.SessIdInfo +
                                    ' is One Time Password authenticated');
        FOtpSequence := Client.OtpSequence;
        FOtpSeed := Client.OtpSeed;
    end
    else begin

        { You should place here the code needed to authenticate the user. }
        { For example a text file with all permitted username/password.   }
        { If the user can't be authenticated, just set Authenticated to   }
        { false before returning.                                         }
        { It is also the right place to setup Client.HomeDir              }
        { If you need to store info about the client for later processing }
        { you can use Client.UserData to store a pointer to an object or  }
        { a record with the needed info.                                  }

        { 1.12 authentication taken from INI file in OtpMethodEvent }
        if ((Client.UserName = UserName) and (Password <> '')) and
             ((Client.AccountPassword = Password) or (Client.AccountPassword = '*')) then { * anonymous logon }
            Display('! ' + Client.SessIdInfo + ' is authenticated')
        else begin
            Display('! ' + Client.SessIdInfo + ' failed authentication');
            Authenticated := FALSE;
        end;
        if Password = 'bad' then
            Authenticated := FALSE;
    end;
    if NOT Authenticated then exit;
    Display('! ' + Client.SessIdInfo + ' Home Directory: ' + Client.HomeDir);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1BgException(Sender: TObject; E: Exception;
  var CanClose: Boolean);
begin
    Display('! Server BgException: ' + IcsGetExceptMess(E));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the FTP component needs to build a      }
{ directory listing. You can just return without doing anything then the    }
{ component will build the directory for you, based on the actual disk      }
{ content. But you can also build your own directory listing with anything  }
{ you like in it. Just create a stream with the required content. The       }
{ example below construct a virtual directory when the user is on the       }
{ C:\VIRTUAL subdirectory (use elsewhere in this sample program).           }
procedure TFtpServerForm.SslFtpServer1BuildDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; var Directory: TFtpString; Detailed: Boolean);
var
    Buf : String;
begin
    if UpperCase(Client.Directory) <> 'C:\VIRTUAL\' then
        Exit;
    Display('! VIRTUAL DIR');
    Client.UserData   := 1;        { Remember we created a stream }
    if Assigned(Client.DataStream) then
        Client.DataStream.Destroy; { Prevent memory leaks         }
    Client.DataStream := TMemoryStream.Create;
    if Detailed then
        { We need to format directory lines according to the Unix standard }
        Buf :=
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 FORBIDEN' + #13#10 +
      '-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 TEST' + #13#10 +
      'drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 SOME DIR' + #13#10
    else
        Buf := 'FORBIDEN' + #13#10 +
               'TEST' + #13#10;
    Client.DataStream.Write(Buf[1], Length(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1BuildFilePath(Sender: TObject;
  Client: TFtpCtrlSocket; const Directory, FileName: string;
  var NewFileName: string);
begin
{ called from all FTP commands involving a file name, to allow virtual directories
  to be supported if NewFileName returned non blank
 if FileName is blank, means a directory is being validated, it might already be the translated virtual path }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ChangeDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
begin
    { It the right place to check if a user has access to a given directory }
    { The example below disable C:\ access to non root user.                }
    if (UpperCase(Client.UserName) <> 'ROOT') and
       (UpperCase(Client.Directory) = 'C:\') then
       Allowed := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ClientCommand(Sender: TObject;
  Client: TFtpCtrlSocket; var Keyword, Params, Answer: TFtpString);
var
    S: string ;
begin
    S := String(StringToUtf8(Params));
    if Keyword = 'PASS' then S := '####' ;
    Display('< ' + Client.GetPeerAddr + ' ' + Keyword + ' ' + S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ClientConnect(Sender: TObject;
  Client: TFtpCtrlSocket; AError: Word);
begin
    { The next test shows how to refuse a client }
    if Client.GetPeerAddr = '193.121.12.25' then begin
        Client.SendStr('421 Connection not allowed.' + #13#10);
        Client.Close;
        Exit;
    end;
    Display('! ' + Client.GetPeerAddr + ' connected');
  { get INI file for default accounts, may be changed if HOST command used }
    Client.AccountIniName := FtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ClientDisconnect(Sender: TObject;
  Client: TFtpCtrlSocket; AError: Word);
begin
    Display('! ' + Client.GetPeerAddr + ' disconnected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Display(Sender: TObject;
  Client: TFtpCtrlSocket; Msg: TFtpString);
begin
    Display('! ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1GetProcessing(Sender: TObject;
  Client: TFtpCtrlSocket; var DelayedSend: Boolean);
var
    MyServer : TFtpServer;
    MyClient : TMyClient;
begin
    MyServer := Sender as TFtpServer;
    MyClient := Client as TMyClient;
    { If client request a *.ZZZ file, then start a thread to do some      }
    { processing (here the thread just sleep 10 sec to show other clients }
    { are not blocked.                                                    }
    if UpperCase(ExtractFileExt(MyClient.FileName)) = '.ZZZ' then begin
        MyClient.FWorkerThread := TGetProcessingThread.Create(TRUE);
        MyClient.FWorkerThread.Server          := MyServer;
        MyClient.FWorkerThread.Client          := MyClient;
        MyClient.FWorkerThread.FreeOnTerminate := TRUE;
        MyClient.FWorkerThread.OnTerminate     := WorkerThreadTerminated;
    {$IF CompilerVersion >= 21} //D2010 TThread.Resume / Suspend deprecated
        MyClient.FWorkerThread.Start;
    {$ELSE}
        MyClient.FWorkerThread.Resume;
    {$IFEND}
        { Ask server component to not start sending immediately           }
        { We will ask to start sending from WorkerThreadTerminated event  }
        DelayedSend := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.WorkerThreadTerminated(Sender : TObject);
var
    MyThread : TGetProcessingThread;
    Answer   : TFtpString;
begin
    MyThread := Sender as TGetProcessingThread;
    MyThread.Server.DoStartSendData(MyThread.Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TGetProcessingThread.Execute;
begin
    Sleep(10000);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Host(Sender: TObject;
  Client: TFtpCtrlSocket; Host: TFtpString; var Allowed: Boolean);
var
    fname: string ;
begin
{ HOST might be ftp.domain.com or [123.123.123.123]   }
    fname := FIniRoot + 'ftpaccounts-' + Lowercase (Host) + '.ini';
    if NOT FileExists (fname) then begin
        Display('! Could not find Accounts File: ' + fname);
        Allowed := false;
        exit;
    end;
    Client.AccountIniName := fname;
    Allowed := true;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1MakeDirectory(Sender: TObject;
  Client: TFtpCtrlSocket; Directory: TFtpString; var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1OtpGetPassword(Sender: TObject;
  Client: TFtpCtrlSocket; UserName: TFtpString; var UserPassword: string);
begin
    UserPassword := Client.AccountPassword;   // expected password will used to create OTP
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1OtpMethod(Sender: TObject;
  Client: TFtpCtrlSocket; UserName: TFtpString; var OtpMethod: TOtpMethod);
var
    IniFile : TIcsIniFile;
    S: string;
begin
    { look up user account to find One Time Password method, root directory, etc, blank password means no account}
    if NOT FileExists (Client.AccountIniName) then begin
        Display('! Could not find Accounts File: ' + Client.AccountIniName);
        exit;
    end;
    Display('! Opening Accounts File: ' + Client.AccountIniName);
    IniFile := TIcsIniFile.Create(Client.AccountIniName);
    Client.AccountPassword := IniFile.ReadString(UserName, KeyPassword, '');  // keep password to check later
    S := IniFile.ReadString(UserName, KeyOtpMethod, 'none');
    Client.AccountReadOnly := (IniFile.ReadString(UserName, KeyReadOnly, 'true') = 'true');
    Client.HomeDir := IniFile.ReadString(UserName, KeyHomeDir, 'c:\temp');
    Client.Directory := Client.HomeDir;
    if (IniFile.ReadString(UserName, KeyForceHomeDir, 'true') = 'true') then
                                   Client.Options := Client.Options + [ftpCdUpHome];
    if (IniFile.ReadString(UserName, KeyHidePhysicalPath, 'true') = 'true') then
                           Client.Options := Client.Options + [ftpHidePhysicalPath];
    if (IniFile.ReadString(UserName,  KeyForceSsl, 'false') = 'true') then begin
        if NOT Client.SslEnable then Client.AccountPassword := '';  // if SSL not enabled fail password
    end;
    IniFile.Free;

    { sequence and seed }
    OtpMethod := OtpGetMethod (S);
    Client.OtpSequence := FOtpSequence;
    Client.OtpSeed := FOtpSeed;

  { this could be user account information, SQL id or something }
    Client.SessIdInfo := Client.GetPeerAddr + '=' + UserName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Rein(Sender: TObject;
  Client: TFtpCtrlSocket; var Allowed: Boolean);
begin
    Allowed := true;
    Display('! Reinitialise client accepted');
    Client.SessIdInfo := Client.GetPeerAddr + '=(Not Logged On)';
    Display('! ' + Client.SessIdInfo + ' connected');
    Client.AccountIniName := FtpServerForm.FIniRoot + 'ftpaccounts-default.ini';
    Client.AccountReadOnly := true;
    Client.AccountPassword := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1RetrDataSent(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
begin
    if Error <> 0 then
        Display('! ' + Client.GetPeerAddr +
                           ' Data sent. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1RetrSessionClosed(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
begin
    if Error <> 0 then
        Display('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
    if Client.UserData = 1 then begin
        { We created a stream for a virtual file or dir. Delete the TStream }
        if Assigned(Client.DataStream) then begin
            { There is no reason why we should not come here, but who knows ? }
            Client.DataStream.Destroy;
            Client.DataStream := nil;
        end;
        Client.UserData   := 0;     { Reset the flag }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the data session for a get file has     }
{ been opened. This is a good place build a file or a stream if the data    }
{ requested is not already stored in a file on the file system.             }
{ This feature is very powerfull and enable the FTP protocol to be used to  }
{ retrieve any kind of data. It this sample, we just check for C:\VIRTUAL   }
{ directory. If this directory is curent, then a TMemoryStream is created   }
{ on the fly with some data. If another directory is selected, the FTP      }
{ server works as any other: just send the requested file, if it exist !    }
{ This event handler is also a place where you can abort the file transfer. }
{ Simply trigger an exception and transfer will not take place.             }
{ Note that if you just wants to prohibe access to some directory or file,  }
{ the best place to code that is in the OnValidateGet or OnValidatePut      }
{ event handlers.                                                           }
procedure TFtpServerForm.SslFtpServer1RetrSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
var
    Buf : String;
begin
    if Error <> 0 then
        Display('! ' + Client.GetPeerAddr +
                           ' Data session connected. Error #' + IntToStr(Error))
    else if Copy(UpperCase(Client.FilePath), 1, 19) = 'C:\VIRTUAL\FORBIDEN' then
        raise Exception.Create('Access prohibed !')
    else if Copy(UpperCase(Client.FilePath), 1, 11) = 'C:\VIRTUAL\' then begin
        Display('! VIRTUAL FILE');
        Client.UserData   := 1;        { Remember we created a stream }
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy; { Prevent memory leaks         }
        Client.DataStream := TMemoryStream.Create;
        Buf := 'This is a file created on the fly by the FTP server' + #13#10 +
               'It could result of a query to a database or anything else.' + #13#10 +
               'The request was: ''' + Client.FilePath + '''' + #13#10;
        Client.DataStream.Write(Buf[1], Length(Buf));
        Client.DataStream.Seek(0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1SslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    Sock: TWSocket;
    Str: String;
    Client: TFtpCtrlSocket;
begin
    if Sender is TFtpCtrlSocket then begin
        Sock := TWSocket(Sender as TFtpCtrlSocket);
        Client := Sender as TFtpCtrlSocket ;
        Str := 'Ctrl Socket: ';
    end
    else begin
        Sock := (Sender as TWSocket);
        Client := Sock.Owner as TFtpCtrlSocket;
        Str := 'Data Socket: ';
    end;
    Display('SSL Client Hello from ' + Client.CPeerAddr + IcsCRLF +
                                Trim(WSocketGetCliHelloStr(Client.CliHelloData))) ;
    if ErrCode = 0 then begin
        Display(Client.SessIdInfo + ' ' + Str + Sock.SslHandshakeRespMsg);
        Display('SSL Handshake done, IcsHost #' + IntToStr(Client.IcsHostIdx) + ' - ' + Client.HostTag);
    end
    else
        Display(Client.SessIdInfo + 'SSL Handshake Failed - ' + Str + Sock.SslHandshakeRespMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1SslServerName(Sender: TObject;
  var Ctx: TSslContext; var ErrCode: TTlsExtError);
begin
    ErrCode := teeOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1SslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
var
    Client: TMyClient;
begin
    if Sender is TMyClient then
        Client := Sender as TMyClient
    else
        Client := (Sender as TWSocket).Owner as TMyClient;

// if client sending us an SSL certificate, does not often happen!!!!
// only report certificate once per session
    if Client.ReportSslCert then
    begin
        OK := 1 ;
        exit ;
    end ;
    Client.ReportSslCert := true ;
    Display(Client.SessIdInfo + 'Received SSL certificate' + IcsCRLF +
        'Subject: ' + Cert.SubjectOneLine + IcsCRLF +
        'Common Name: ' + Cert.SubjectCName + IcsCRLF +
        'Issuer: '  + Cert.IssuerOneLine);
//  if PostConnectionCheck(HostOrIp)  // check match for server IP
    if OK <> 1 then
    begin
        Display(Client.SessIdInfo + 'SSL Certificate Error: ' +
                                 Cert.VerifyErrMsg + ' (but error ignored)') ;
        OK := 1 ; //In this example we accept any client.
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Start(Sender: TObject);
var
    SrvName : String;
begin
    SrvName := (Sender as TComponent).Name;
    Display('! ' + SrvName + ' started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1Stop(Sender: TObject);
var
    SrvName : String;
begin
    SrvName := (Sender as TComponent).Name;
    Display('! ' + SrvName + ' stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1StorSessionConnected(Sender: TObject;
  Client: TFtpCtrlSocket; Data: TWSocket; AError: Word);
begin
    if Error <> 0 then
        Display('! ' + Client.GetPeerAddr +
                           ' Data session closed. Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ValidateDele(Sender: TObject;
  Client: TFtpCtrlSocket; var FilePath: TFtpString; var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ValidatePut(Sender: TObject;
  Client: TFtpCtrlSocket; var FilePath: TFtpString; var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.SslFtpServer1ValidateRnFr(Sender: TObject;
  Client: TFtpCtrlSocket; var FilePath: TFtpString; var Allowed: Boolean);
begin
    Allowed := NOT Client.AccountReadOnly;
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.IcsMailQueueLogEvent(LogLevel: TMailLogLevel;  { V8.60 }
  const Info: string);
begin
    Display(Info);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.IcsSslX509CertsCertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.IcsSslX509CertsChallengeDNS(Sender: TObject;
  ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
begin
    ChlgOK := False;
//   update DNS server with TXT challenge information
//   not sure worth the trouble for a web server
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.IcsSslX509CertsNewCert(Sender: TObject);    { V8.57 }
begin
    CertCheckTrigger := Trigger64Immediate;
 // force certiificate check to load new ones
    Display ('Trigger Recheck Certificates') ;
    Display('Web server ordered new SSL cerrtificate.' + IcsCRLF +
                                      IcsSslX509Certs.GetOrderResult);
    if (AdminEmailTo <> '') then
        SendAdminEmail (AdminEmailTo, 'ICS Multi Web Server SSL Certificate Order Completed',
        'Web server ordered new SSL cerrtificate.' + IcsCRLF + IcsCRLF +
        IcsSslX509Certs.GetOrderResult + IcsCRLF) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServerForm.IcsSslX509CertsOAuthAuthUrl(Sender: TObject;
  const URL: string);
begin
   Display('Web server demo needs OAuth authenfication for new certificate, ' +
                'Browse to this URL: ' + URL +  ', From PC: ' + IcsGetCompName) ;
    if (AdminEmailTo <> '') then
        SendAdminEmail (AdminEmailTo, 'ICS Multi Web Server needs OAuth authenfication for new certificate',
            'Browse to this URL: ' + URL + IcsCRLF + IcsCRLF + 'From Browser on PC: ' + SrvCompName + IcsCRLF) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
