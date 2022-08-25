{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Original Author: Ian Baker, ADV Systems 2003
Updated by:   Angus Robertson, Magenta Systems Ltd
Creation:     24 September 2013
Version:      8.01
Description:  How to use TSmtpServer
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


Sep 24, 2013 V8.00 Angus created
Feb 23, 2016 V8.01 - Angus renamed TBufferedFileStream to TIcsBufferedFileStream



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSmtpServ1;

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
  OverbyteIcsStreams,
  OverbyteIcsSmtpSrv ;

const
    SmtpServerTestVersion    = 8.01;
    CopyRight : String = ' OverbyteSmtpServer (c) 1997-2016 F. Piette V8.01 ';

  // INI file stuff
    SectionData       = 'Data';
    KeyDnsServer      = 'DnsServer';
    KeyEmailAccs      = 'EmailAccs';
    KeySpoolDir       = 'SpoolDir';
    KeyAddRecvHdrs    = 'AddRecvHdrs';
    KeyAddEnvHdrs     = 'AddEnvHdrs';
    KeyAddReplayHdrs  = 'AddReplayHdrs';
    KeyAllowRelay     = 'AddAllowRelay';
    KeyAliasAccs      = 'AddAliasAccs';

    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';


type

  TSmtpSrvForm       = class(TForm)
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
    SmtpServer1: TSmtpServer;
    doStart: TButton;
    doStop: TButton;
    doExit: TButton;
    PrefAllowRelay: TCheckBox;
    PrefAliasAccs: TMemo;
    Label4: TLabel;
    PrefAddReplayHdrs: TCheckBox;
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
   end;

var
    SmtpSrvForm: TSmtpSrvForm;
    FIniFileName: string;
    FInitialized  : Boolean;

implementation

{$R *.dfm}


procedure TSmtpSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
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

    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TSmtpSrvForm.FormCreate(Sender: TObject);
begin
    Log.Clear;
    FIniFileName := GetIcsIniFileName;
end;

procedure TSmtpSrvForm.FormDestroy(Sender: TObject);
begin
    SmtpServer1.Stop;
end;

procedure TSmtpSrvForm.FormShow(Sender: TObject);
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

        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        IniFile.Free;
    end;
end;

procedure TSmtpSrvForm.doStartClick(Sender: TObject);
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
    with SmtpServer1 do
    begin
        Addr           := LocalIPList (sfIpv4, IPPROTO_TCP) [0];
        ServerHost     := String(WSocketResolveIP (AnsiString (Addr)));  // should be the mail exchange domain name
        SocketFamily   := sfIpV4;
        Port           := '25';
        if MultiListenSockets.Count > 0 then
        begin
            MultiListenSockets [0].Addr := Addr;
            MultiListenSockets [0].Port := '587';
            MultiListenSockets [0].SocketFamily := SocketFamily;
            if IsIPv6ApiAvailable then
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
        LocalAccounts  := PrefEmailAccs.Lines;
        AliasAccounts  := PrefAliasAccs.Lines;  // must be assigned after local account, since checked

        try
            Start;
            doStart.Enabled := false;
            doExit.Enabled := false;
            doStop.Enabled := true;
        except
            Log.Lines.Add ('Exception: '{ + ExceptObject.Message});
        end;
    end;
end;

procedure TSmtpSrvForm.doStopClick(Sender: TObject);
begin
    SmtpServer1.Stop;
end;

procedure TSmtpSrvForm.doExitClick(Sender: TObject);
begin
    SmtpServer1.Stop;
    Close;
end;

procedure TSmtpSrvForm.SmtpServer1ServerStarted(Sender: TObject);
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

procedure TSmtpSrvForm.SmtpServer1ServerStopped(Sender: TObject);
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

procedure TSmtpSrvForm.SmtpServer1Connect(Sender, Client: TObject; const IpAddr: string;
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

procedure TSmtpSrvForm.SmtpServer1Disconnect(Sender, Client: TObject; Error: Word);
begin
    Log.Lines.Add (Format ('%s  %8.8x  Connection terminated', [FormatDateTime
                                 ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID]));
end;

procedure TSmtpSrvForm.SmtpServer1Exception(Sender: TObject; E: Exception);
begin
    Log.Lines.Add ('Exception: ' + E.Message);
end;

procedure TSmtpSrvForm.SmtpServer1Auth(Sender, Client: TObject; const UserName: string;
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

procedure TSmtpSrvForm.SmtpServer1AuthPW(Sender, Client: TObject; const UserName: string; var Password: string;
  var Action: TSmtpMailAction; var Reason: string);
begin
  // we need to provide a password for Cram-md5/sha1 to test against
    Password := 'password';
end;

procedure TSmtpSrvForm.SmtpServer1Command(Sender, Client: TObject; const Command: string);
begin
  // information logging only
    Log.Lines.Add (Format ('%s  %8.8x  < %s', [FormatDateTime
                        ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID, Command]));
end;

procedure TSmtpSrvForm.SmtpServer1Response(Sender, Client: TObject; const Response: string);
begin
  // information logging only
    Log.Lines.Add (Format ('%s  %8.8x  > %s', [FormatDateTime
                        ('hh:nn:ss', Time), TSmtpSrvCli (Client).ID, Response]));
end;

procedure TSmtpSrvForm.SmtpServer1MailFrom(Sender, Client: TObject;
                  const MailFrom: string; var Action: TSmtpMailAction; var Reason: string);
begin
  // do we accept mail from this address?

   // we can reject MAIL FROM with various responses:
   // Action := wsmtpClosingDown, wsmtpGreyListed, wsmtpMsgTooLarge,
   //           wsmtpSyntaxError, wsmtpBadSequence, wsmtpAuthRequired,
   //           wsmtpSysUnavail, wsmtpCongested
end;

procedure TSmtpSrvForm.SmtpServer1RcptTo(Sender, Client: TObject;
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

procedure TSmtpSrvForm.SmtpServer1DataStart(Sender, Client: TObject;
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

procedure TSmtpSrvForm.SmtpServer1DataEnd(Sender, Client: TObject;
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
