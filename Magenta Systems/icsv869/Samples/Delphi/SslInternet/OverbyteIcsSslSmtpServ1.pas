{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Original Author: Ian Baker, ADV Systems 2003
Updated by:   Angus Robertson, Magenta Systems Ltd
Creation:     24 September 2013
Version:      8.37
Description:  How to use TSslSmtpServer
EMail:        francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2016 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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


Sep 24, 2013 V8.00 Angus created SSL version
Apr 26, 2014 V8.01 Arno - Check for IsIPv6Available rather than IsIPv6ApiAvailable
                   in doStartClick.
Dec 10, 2014 V8.02 Angus added handshake response message, better cipher list
June 2015 V8.03    Angus fixed name space issue that stopped build
May 24, 2016 V8.04 Angus renamed TBufferedFileStream to TIcsBufferedFileStream
Nov 12 2016  V8.37 Set friendly errors
                   Specify minimum and maximum SSL version supported
                   Allow server IP address to be specified 

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslSmtpServ1;

interface

{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, StrUtils, DateUtils, Types,
  OverbyteIcsIniFiles,
  OverbyteIcsCharsetUtils,
  OverbyteIcsWndControl,
  OverbyteIcsWinSock,
  OverbyteIcsWSocket,
  OverbyteIcsWSocketS,
  OverbyteIcsLIBEAY,
  OverbyteIcsSSLEAY,
  OverbyteIcsStreams,
  OverbyteIcsSmtpSrv ;

const
    SmtpSslServerTestVersion    = 8.37;
    CopyRight : String = ' OverbyteSslSmtpServer (c) 1997-2016 F. Piette V8.37 ';

  // INI file stuff
    SectionData       = 'Data';
    KeyDnsServer      = 'DnsServer';
    KeyEmailAccs      = 'EmailAccs';
    KeySpoolDir       = 'SpoolDir';
    KeyAddRecvHdrs    = 'AddRecvHdrs';
    KeyAddEnvHdrs     = 'AddEnvHdrs';
    KeyAddReplayHdrs  = 'AddReplayHdrs';
    KeyAllowRelay     = 'AddAllowRelay';
    KeyAuthTls        = 'AuthTls';
    KeyAliasAccs      = 'AddAliasAccs';
    KeyCertFile        = 'CertFile';
    KeyPassPhrase      = 'PassPhrase';
    KeyPrivKeyFile     = 'PrivKeyFile';
    KeyVerifyPeer      = 'VerifyPeer';
    KeyCAFile          = 'CAFile';
    KeyCAPath          = 'CAPath';
    KeyServIpAddr      = 'ServIpAddr';

    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';


type

  TSmtpSslSrvForm       = class(TForm)
    ButtonPanel: TPanel;
    Log        : TMemo;
    PrefDnsServer: TEdit;
    Label1: TLabel;
    PrefEmailAccs: TMemo;
    Label2: TLabel;
    PrefSpoolDir: TEdit;
    PrefAddRecvHdrs: TCheckBox;
    Label3: TLabel;
    PrefAddEnvHdrs: TCheckBox;
    SmtpServer1: TSslSmtpServer;
    doStart: TButton;
    doStop: TButton;
    doExit: TButton;
    PrefAllowRelay: TCheckBox;
    PrefAliasAccs: TMemo;
    Label4: TLabel;
    PrefAddReplayHdrs: TCheckBox;
    SslContext1: TSslContext;
    ToolsPanel: TPanel;
    Label5: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    CertFileEdit: TEdit;
    CAFileEdit: TEdit;
    CAPathEdit: TEdit;
    PrivKeyFileEdit: TEdit;
    PassPhraseEdit: TEdit;
    VerifyPeerCheckBox: TCheckBox;
    PrefAuthTls: TCheckBox;
    Label8: TLabel;
    ServIpAddr: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SmtpServer1Auth(Sender, Client: TObject; const UserName: string; var Password: string;
        var Action: TSmtpMailAction; var Reason: string);
    procedure SmtpServer1AuthPW(Sender, Client: TObject; const UserName: string; var Password: string;
      var Action: TSmtpMailAction; var Reason: string);
    procedure SmtpServer1Connect(Sender, Client: TObject; const IpAddr: string; var Action: TSmtpMailAction;
      var Reason: string);
    procedure SmtpServer1DataEnd(Sender, Client: TObject; var Action: TSmtpMailAction; var Reason: string);
    procedure SmtpServer1DataStart(Sender, Client: TObject; var Action: TSmtpMailAction; var Reason: string);
    procedure SmtpServer1Disconnect(Sender, Client: TObject; Error: Word);
    procedure SmtpServer1Exception(Sender: TObject; E: Exception);
    procedure SmtpServer1MailFrom(Sender, Client: TObject; const MailFrom: string; var Action: TSmtpMailAction;
      var Reason: string);
    procedure SmtpServer1RcptTo(Sender, Client: TObject; const RcptTo: string; var Action: TSmtpMailAction;
      var Reason: string);
    procedure SmtpServer1Command(Sender, Client: TObject; const Command: string);
    procedure SmtpServer1Response(Sender, Client: TObject; const Response: string);
    procedure SmtpServer1ServerStopped(Sender: TObject);
    procedure SmtpServer1ServerStarted(Sender: TObject);
    procedure doStartClick(Sender: TObject);
    procedure doStopClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SmtpServer1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SmtpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer; Cert: TX509Base);
   end;

var
    SmtpSslSrvForm: TSmtpSslSrvForm;
    FIniFileName: string;
    FInitialized  : Boolean;

implementation

{$R *.dfm}


procedure TSmtpSslSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyDnsServer,      PrefDnsServer.Text);
    IniFile.WriteString(SectionData, KeyEmailAccs,      PrefEmailAccs.Lines.CommaText);
    IniFile.WriteString(SectionData, KeyAliasAccs,      PrefAliasAccs.Lines.CommaText);
    IniFile.WriteString(SectionData, KeySpoolDir,       PrefSpoolDir.Text);
    IniFile.WriteBool(SectionData,   KeyAddRecvHdrs,    PrefAddRecvHdrs.Checked);
    IniFile.WriteBool(SectionData,   KeyAddEnvHdrs,     PrefAddEnvHdrs.Checked);
    IniFile.WriteBool(SectionData,   KeyAddReplayHdrs,  PrefAddReplayHdrs.Checked);
    IniFile.WriteBool(SectionData,   KeyAllowRelay,     PrefAllowRelay.Checked);
    IniFile.WriteBool(SectionData,   KeyAuthTls,        PrefAuthTls.Checked);
    IniFile.WriteString(SectionData, KeyCertFile,       CertFileEdit.Text);
    IniFile.WriteString(SectionData, KeyPrivKeyFile,    PrivKeyFileEdit.Text);
    IniFile.WriteString(SectionData, KeyPassPhrase,     PassPhraseEdit.Text);
    IniFile.WriteString(SectionData, KeyCAFile,         CAFileEdit.Text);
    IniFile.WriteString(SectionData, KeyCAPath,         CAPathEdit.Text);
    IniFile.WriteInteger(SectionData, KeyVerifyPeer,    Ord(VerifyPeerCheckBox.Checked));
    IniFile.WriteString(SectionData, KeyServIpAddr,     ServIpAddr.Text);  { V8.37 }

    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TSmtpSslSrvForm.FormCreate(Sender: TObject);
begin
    Log.Clear;
    FIniFileName := GetIcsIniFileName;
end;

procedure TSmtpSslSrvForm.FormDestroy(Sender: TObject);
begin
    SmtpServer1.Stop;
end;

procedure TSmtpSslSrvForm.FormShow(Sender: TObject);
var
    IniFile    : TIcsIniFile;
begin
    if not FInitialized then
    begin
        FInitialized := TRUE;

        IniFile := TIcsIniFile.Create(FIniFileName);
        PrefDnsServer.Text       := IniFile.ReadString(SectionData, KeyDnsServer, '8.8.8.8');  // Google DNS
        PrefEmailAccs.Lines.CommaText := IniFile.ReadString(SectionData, KeyEmailAccs, 'account@domain.com');
        PrefAliasAccs.Lines.CommaText := IniFile.ReadString(SectionData, KeyAliasAccs, '*@domain.com=account@domain.com');
        PrefSpoolDir.Text        := IniFile.ReadString(SectionData, KeySpoolDir, 'c:\mailspool\');
        PrefAddRecvHdrs.Checked  := IniFile.ReadBool(SectionData, KeyAddRecvHdrs, true);
        PrefAddEnvHdrs.Checked   := IniFile.ReadBool(SectionData, KeyAddEnvHdrs, false);
        PrefAddReplayHdrs.Checked := IniFile.ReadBool(SectionData, KeyAddReplayHdrs, false);
        PrefAllowRelay.Checked   := IniFile.ReadBool(SectionData, KeyAllowRelay, true);
        PrefAuthTls.Checked      := IniFile.ReadBool(SectionData, KeyAuthTls, true);
        CertFileEdit.Text    := IniFile.ReadString(SectionData, KeyCertFile, '01cert.pem');
        PrivKeyFileEdit.Text := IniFile.ReadString(SectionData, KeyPrivKeyFile, '01key.pem');
        PassPhraseEdit.Text  := IniFile.ReadString(SectionData, KeyPassPhrase, 'password');
        CAFileEdit.Text      := IniFile.ReadString(SectionData, KeyCAFile, 'cacert.pem');
        CAPathEdit.Text      := IniFile.ReadString(SectionData, KeyCAPath, '');
        VerifyPeerCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyVerifyPeer, 0));
        ServIpAddr.Text      := IniFile.ReadString(SectionData, KeyServIpAddr, '0.0.0.0');      { V8.37 }
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        IniFile.Free;
    end;
end;

procedure TSmtpSslSrvForm.doStartClick(Sender: TObject);
begin
    if PrefSpoolDir.Text = '' then
    begin
        Log.Lines.Add ('Must specify a spool directory to receive email');
        exit;
    end;
    if NOT ForceDirectories (PrefSpoolDir.Text) then
    begin
        Log.Lines.Add ('Failed to create spool directory: ' + PrefSpoolDir.Text);
        exit;
    end;
    SslContext1.SslCertFile         := CertFileEdit.Text;
    SslContext1.SslPassPhrase       := PassPhraseEdit.Text;
    SslContext1.SslPrivKeyFile      := PrivKeyFileEdit.Text;
    SslContext1.SslCAFile           := CAFileEdit.Text;
    SslContext1.SslCAPath           := CAPathEdit.Text;
    SslContext1.SslVerifyPeer       := VerifyPeerCheckBox.Checked;
    SslContext1.SslCipherList       := sslCiphersMozillaSrvInter;   { V8.02 }
    SslContext1.SslVersionMethod    := sslV23_SERVER;
    SslContext1.SslOptions          := SslContext1.SslOptions -  { V8.02 disable SSLv3 }
                            [sslOpt_NO_SSLv2, sslOpt_NO_SSLv3, sslOpt_CIPHER_SERVER_PREFERENCE];
    with SmtpServer1 do
    begin
        Addr           := ServIpAddr.Text; // LocalIPList (sfIpv4, IPPROTO_TCP) [0];  { V8.37 }
        ServerHost     := String(WSocketResolveIP (AnsiString (Addr)));  // should be the mail exchange domain name
        SocketFamily   := sfIpV4;
        Port           := '25';
        if MultiListenSockets.Count > 0 then
        begin
            MultiListenSockets [0].Addr := Addr;
            MultiListenSockets [0].Port := '587';
            MultiListenSockets [0].SocketFamily := SocketFamily;
            if IsIPv6Available then
            begin
                if MultiListenSockets.Count < 2 then
                    MultiListenSockets.Add;
                MultiListenSockets [1].Addr := LocalIPList (sfIpv6, IPPROTO_TCP) [0];
                MultiListenSockets [1].Port := '25';
                MultiListenSockets [1].SocketFamily := sfIpv6;
                if MultiListenSockets.Count < 3 then
                    MultiListenSockets.Add;
                MultiListenSockets [2].Addr := MultiListenSockets [1].Addr;
                MultiListenSockets [2].Port := '587';
                MultiListenSockets [2].SocketFamily := sfIpv6;
           end;
        end;
        DnsAddress     := PrefDnsServer.Text; 
        ClientTimeout  := 5 * 60;
        MaxMessageSize := 25 * 1000000;
        if PrefAddRecvHdrs.Checked then
            Options := Options + [smtpsAddRecvHeaders]
        else
            Options := Options - [smtpsAddRecvHeaders];
        Options := Options + [smtpsParseHeaders];
        if PrefAddEnvHdrs.Checked then
            Options := Options + [smtpsAddEnvHeaders]
        else
            Options := Options - [smtpsAddEnvHeaders];
        if PrefAddReplayHdrs.Checked then
            Options := Options + [smtpsAddReplayHdrs]
        else
            Options := Options - [smtpsAddReplayHdrs];
        if PrefAllowRelay.Checked then
        begin
            Options := Options + [smtpsAllowOpenRelay];
            Options := Options + [smtpsAllowAuthRelay];
        end
        else
        begin
            Options := Options - [smtpsAllowOpenRelay];
            Options := Options - [smtpsAllowAuthRelay];
        end;
        if PrefAuthTls.Checked then
            Options := Options + [smtpsAuthNoTls]
        else
            Options := Options - [smtpsAuthNoTls];
        Options := Options + [smtpsAllowTls];
        LocalAccounts  := PrefEmailAccs.Lines;
        AliasAccounts  := PrefAliasAccs.Lines;  // must be assigned after local account, since checked

    //Pre-loads OpenSSL DLL's
        try
            SslContext.InitContext;
            Log.Lines.Add ('SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName) ;
        except
            on E: Exception do
                Log.Lines.Add ('Failed to Initialise SSL - ' + E.Message);
        end ;
   // start SMTP server
        try
            Start;
            doStart.Enabled := false;
            doExit.Enabled := false;
            doStop.Enabled := true;
        except
            on E: Exception do
                Log.Lines.Add ('Failed to Start SMTP Server: ' + E.Message);
        end;
    end;
end;

procedure TSmtpSslSrvForm.doStopClick(Sender: TObject);
begin
    SmtpServer1.Stop;
end;

procedure TSmtpSslSrvForm.doExitClick(Sender: TObject);
begin
    SmtpServer1.Stop;
    Close;
end;

procedure TSmtpSslSrvForm.SmtpServer1ServerStarted(Sender: TObject);
var
    K: integer ;
    ListenItem: TWSocketMultiListenItem;
begin
    with Sender as TSmtpServer do
    begin
        Log.Lines.Add (Format ('%s  SMTP Server Started', [FormatDateTime ('hh:nn:ss', Time)]));
        Log.Lines.Add ('Socket 1 State: ' + SocketStateNames[WSocketServer.State] + ' ' +
                                 SocketFamilyNames [SocketFamily] + ' on ' +
                                      FormatIpAddrPort (WSocketServer.GetXAddr, WSocketServer.Port));
        if MultiListenSockets.Count > 0 then
        begin
            for K := 0 to MultiListenSockets.Count - 1 do
            begin
                ListenItem := MultiListenSockets [K] as TWSocketMultiListenItem;
                Log.Lines.Add ('Socket ' + IntToStr (K + 2) +
                     ' State: ' + SocketStateNames[ListenItem.State] + ' ' +
                         SocketFamilyNames [ListenItem.SocketFamily] +
                           ' on ' + FormatIpAddrPort (ListenItem.Addr, ListenItem.Port));
            end;
        end;
    end;
end;

procedure TSmtpSslSrvForm.SmtpServer1ServerStopped(Sender: TObject);
begin
    with Sender as TSmtpServer do
    begin
        Log.Lines.Add (Format ('%s  SMTP Server Socket %x Stopped',
                     [FormatDateTime ('hh:nn:ss', Time), WSocketServer.MultiListenIndex + 2]));
    end;
    doStart.Enabled := true;
    doExit.Enabled := true;
    doStop.Enabled := false;
end;

procedure TSmtpSslSrvForm.SmtpServer1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base;
  var Disconnect: Boolean);
begin
    with Sender as TSmtpSrvCli do
    begin
        if ErrCode = 0 then
        begin
            Log.Lines.Add (Format('%s  %8.8x  %s',
                    [FormatDateTime('hh:nn:ss', Time), ID, SslHandshakeRespMsg]));  { V8.02 }
        end
        else
            Log.Lines.Add (Format('%s  %8.8x  SslHandshake Failed - %s',
                             [FormatDateTime('hh:nn:aa', Time), ID, SslHandshakeRespMsg]));    { V8.02 }
    end;
end;

procedure TSmtpSslSrvForm.SmtpServer1SslVerifyPeer(Sender: TObject; var Ok: Integer; Cert: TX509Base);
begin
    Log.Lines.Add ('Received certificate'#13#10 +
                'Subject: ' + Cert.SubjectOneLine + #13#10 +
                'Issuer: '  + Cert.IssuerOneLine);
    if OK <> 1 then
    begin
        Log.Lines.Add ('Error msg: ' + Cert.VerifyErrMsg + #13#10 +
                    'In this example we accept any cert');
        OK := 1; //In this example we accept any client.
    end;
end;

procedure TSmtpSslSrvForm.SmtpServer1Connect(Sender, Client: TObject; const IpAddr: string;
                                            var Action: TSmtpMailAction; var Reason: string);
begin
  // we might dislike the remote IP address if it's been spamming us, but generally should respond
  // we could check the IP against ranges authorised to relay by this server and set
  //  Authenticated=true to avoid needing AUTH command
    with TSmtpSrvCli (Client) do
        Log.Lines.Add (Format ('%s  %8.8x  Client connected from %s [%s MX=%s]', [FormatDateTime
                        ('hh:nn:ss', Time), ID, IPAddr, ClientRDNS, ClientMX]));
    // Action := wsmtpSysUnavail, wsmtpNetError or wsmtpCongested
end;

procedure TSmtpSslSrvForm.SmtpServer1Disconnect(Sender, Client: TObject; Error: Word);
begin
    Log.Lines.Add (Format ('%s  %8.8x  Connection terminated', [FormatDateTime
                                 ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID]));
end;

procedure TSmtpSslSrvForm.SmtpServer1Exception(Sender: TObject; E: Exception);
begin
    Log.Lines.Add ('Exception: ' + E.Message);
end;

procedure TSmtpSslSrvForm.SmtpServer1Auth(Sender, Client: TObject; const UserName: string;
    var Password: string; var Action: TSmtpMailAction; var Reason: string);
begin
  // do we need to check authentication now, blank means Cram-md5/sha1 already checked
    with Client as TSmtpSrvCli do
    begin
        if (UserName = '') then
            Action :=  wsmtpAuthRequired
        else if Password <> '' then
        begin
            Action := wsmtpAuthPermFail;
         //  wsmtpAuthTempFail
            if (Password = 'password') then Action := wsmtpOK;
        end;
        if Action = wsmtpOK then
            Log.Lines.Add (Format ('%s  %8.8x  Authentication OK: UserName=%s Password=%S',
                                [FormatDateTime ('hh:nn:ss', Time), ID, UserName, Password]))
        else
            Log.Lines.Add (Format ('%s  %8.8x  Authentication failed: UserName=%s Password=%S',
                                [FormatDateTime ('hh:nn:ss', Time), ID, UserName, Password]));
    end;
end;

procedure TSmtpSslSrvForm.SmtpServer1AuthPW(Sender, Client: TObject; const UserName: string; var Password: string;
  var Action: TSmtpMailAction; var Reason: string);
begin
  // we need to provide a password for Cram-md5/sha1 to test against
    Password := 'password';
end;

procedure TSmtpSslSrvForm.SmtpServer1Command(Sender, Client: TObject; const Command: string);
begin
  // information logging only
    Log.Lines.Add (Format ('%s  %8.8x  < %s', [FormatDateTime
                        ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID, Command]));
end;

procedure TSmtpSslSrvForm.SmtpServer1Response(Sender, Client: TObject; const Response: string);
begin
  // information logging only
    Log.Lines.Add (Format ('%s  %8.8x  > %s', [FormatDateTime
                        ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID, Response]));
end;

procedure TSmtpSslSrvForm.SmtpServer1MailFrom(Sender, Client: TObject;
                  const MailFrom: string; var Action: TSmtpMailAction; var Reason: string);
begin
  // do we accept mail from this address?

   // we can reject MAIL FROM with various responses:
   // Action := wsmtpClosingDown, wsmtpGreyListed, wsmtpMsgTooLarge,
   //           wsmtpSyntaxError, wsmtpBadSequence, wsmtpAuthRequired,
   //           wsmtpSysUnavail, wsmtpCongested
end;

procedure TSmtpSslSrvForm.SmtpServer1RcptTo(Sender, Client: TObject;
                    const RcptTo: string; var Action: TSmtpMailAction; var Reason: string);
begin
  // can we deliver to this email account?
  // might have been validated already if LocalAccounts list specified

   // we can reject RCPT TO with various responses:
   // Action := wsmtpClosingDown, wsmtpMailboxBusy, wsmtpGreyListed, wsmtpMsgTooLarge,
   //           wsmtpSyntaxError, wsmtpBadSequence, wsmtpBadDomain, wsmtpAuthRequired,
   //           wsmtpBadAccount,  wsmtpAccClosed, wsmtpAccNotLocal,
   //           wsmtpTooMany, wsmtpSysUnavail, wsmtpCongested
end;

procedure TSmtpSslSrvForm.SmtpServer1DataStart(Sender, Client: TObject;
                                    var Action: TSmtpMailAction; var Reason: string);
begin
  // got all envelope commands, about to get email data, which we need to save
  // MessageTo [x] is multiple recipients for the message
  // ToAccounts [x] is multiple local accounts to which the mail should be saved, if non-blank
  // if saving email for POP3 and as files, set a directory based on ToAccount [x]
  //   otherwise for relaying save to a spool directory
    with Client as TSmtpSrvCli do
    begin
        try
            DataFileName := PrefSpoolDir.Text + MessageID + '.eml';
            if ToAccounts.Count > 0 then  // only saving a single copy at the moment for first account
            begin
                DataFileName := PrefSpoolDir.Text + ToAccounts [0] + '\' ;
                if NOT ForceDirectories (DataFileName) then
                begin
                    Log.Lines.Add ('Failed to create spool directory: ' + DataFileName);
                    Action := wsmtpSysUnavail;
                    Reason := 'Mail spool unavailable';
                    exit;
                end;
                DataFileName := DataFileName + MessageID + '.eml';
            end;
            DataStream := TIcsBufferedFileStream.Create (DataFileName, fmCreate, MAX_BUFSIZE);
        except
            Log.Lines.Add (Format ('%s  %8.8x  Failed to create mail spool file %s', [FormatDateTime
                        ('hh:nn:ss', Time), ID, DataFileName]));
            Action := wsmtpSysUnavail;
            Reason := 'Mail spool unavailable';
        end;
    end;
   // we can reject DATA with various responses:
   // Action := wsmtpClosingDown, wsmtpGreyListed, wsmtpSyntaxError, wsmtpBadSequence,
   //           wsmtpAuthRequiredwsmtpSysUnavail
end;

procedure TSmtpSslSrvForm.SmtpServer1DataEnd(Sender, Client: TObject;
                                    var Action: TSmtpMailAction; var Reason: string);
begin
  // got a complete email, maybe to several recipients in MessageTo [x] and ToAccount [x]
  //   (see DataStart) so may need to copy the stream to other places, MailFrom is the sender
  // if ParseHeaders=true, HdrTo, HdrFrom, HdrSubjext, HdrDateStr and HdrDateDT contain
  //   the main email header fields, and MessageID the response sent to the sender
    with Client as TSmtpSrvCli do
    begin
        if Assigned(DataStream) then
        begin
            Log.Lines.Add (Format ('%s  %8.8x  Saved mail to spool file %s, size %d',
                             [FormatDateTime ('hh:nn:ss', Time), ID, DataFileName, DataStream.Size]));
            DataStream.Free; // close stream to save file to disk
            DataStream := nil;
        end;
    end;
   // we can reject DATA end with various responses:
   // Action := wsmtpMsgTooLarge, wsmtpMailboxFull, wsmtpSysUnavail
end;


end.
