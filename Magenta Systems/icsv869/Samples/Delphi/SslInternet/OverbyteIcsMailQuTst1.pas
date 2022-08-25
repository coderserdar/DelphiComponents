{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Description:  Mail Queue Component Demonstration Sample for the TIcsMailQueue
              which is designed to prepare, queue and send email.
Creation:     Jan 2011
Updated:      Jan 2021
Version:      8.66
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2021 by Angus Robertson, Magenta Systems Ltd,
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

History:
Release 2.0 - 27th Oct 2015
    Save settings in INI file
    New View Mail Queue window to see what's waiting
    Added SMTP Send Method, relay, specific or lookup MX mail servers
    Added HELO Sending Host Name may be needed if using MX mail servers
    Added SSL certificate checking and more SSL options
1st Dec 2016  more friendly errors
    Force latest OpenSSL from our directory
    Fixed bug that meant failed email was not deleted from queue
    Don't queue email without recipients
    Use timer to update windows to avoid problems with mass email performance
6 Mar 2017  simplified SSL certificate reporting
22 Jun 2018 Added SslCliSecurity for SSL client security
    Added RetryWithoutSsl which retries an SSL failure without SSL
20 Mar 2019 - V8.60 - Adapted for ICS
              Two mail servers. log file
19 Nov 2020 - V8.65 - Added XOAuth2 and OAuthBearer authentication support using
              TIcsRestEmail component for OAuth2, allows to access GMail account
                with security enabled and Microsoft Outlook mail.
                See comments at top of OverbyteIcsSslHttpOAuth.pas on how to
                set-up a Google/Microsoft OAuth2 application account.
14 Jan 2021 - V8.66 - Replaced initial OAuth2 token checking with new
                  CheckOAuthLogins function.
14 Apr 2022 - V8.69 - Added unit OverbyteIcsSslHttpOAuth for TIcsRestEmail
                        previously in SslHttpRest.
              Support OCSP to check certificate revocation when verifying
                handshake using certificate bundle.  Note OCSP settings
                made in code, not from the GUI.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMailQuTst1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RichEdit, Mask, ComCtrls, ExtCtrls,
  OverbyteIcsIniFiles,
  OverbyteIcsWSocket,
  OverbyteIcsSmtpProt,
  OverbyteIcsWinSock,
  OverbyteIcsWndControl,
  OverbyteIcsSSLEAY,
  OverbyteIcsSslX509Utils,
  OverbyteIcsMailQueue,
  OverbyteIcsUtils,
  OverbyteIcsBlacklist,
  OverbyteIcsSslHttpRest,
  OverbyteIcsLogger,
  OverbyteIcsSslHttpOAuth
{$IF CompilerVersion > 23}
  ,System.UITypes
{$IFEND}
  ;

const
    MaxAddressesAllowed = 20;  // stop accidentally sending too many emails

type
  TDemoForm = class(TForm)
  // following saved in INI file
    FileAttachment: TEdit;
    MailBody: TMemo;
    MailCC: TEdit;
    MailFrom: TEdit;
    MailPriority: TComboBox;
    MailSubject: TEdit;
    RecipList: TMemo;
    PrefAttemptsList: TEdit;
    PrefCARootBundle: TEdit;
    PrefClientCertFile: TEdit;
    PrefDnsServer: TEdit;
    PrefHeloHost: TEdit;
    PrefMailQuFolder: TEdit;
    PrefSendMethod: TRadioGroup;
    PrefSslRevoke: TCheckBox;
    PrefVerifyCertMode: TRadioGroup;
    PrefEmailAuthPass1: TEdit;
    PrefEmailAuthPass2: TEdit;
    PrefEmailAuthType1: TRadioGroup;
    PrefEmailAuthType2: TRadioGroup;
    PrefEmailAuthUser1: TEdit;
    PrefEmailAuthUser2: TEdit;
    PrefEmailPort1: TEdit;
    PrefEmailPort2: TEdit;
    PrefEmailSecure1: TComboBox;
    PrefEmailSecure2: TComboBox;
    PrefEmailSecurity1: TComboBox;
    PrefEmailSecurity2: TComboBox;
    PrefEmailSmtp1: TEdit;
    PrefEmailSmtp2: TEdit;
    PrefEmailSslErr1: TCheckBox;
    PrefEmailSslErr2: TCheckBox;
    DirLogs: TEdit;
    PrefClientId: TEdit;
    PrefRefrToken: TEdit;
    PrefRestType: TComboBox;
    PrefClientSecret: TEdit;

 // not saved
    MailLog: TRichEdit;
    SMTPServer1: TGroupBox;
    SMTPServer2: TGroupBox;
    Status: TStatusBar;
    TabMessage: TTabSheet;
    TabServers: TTabSheet;
    TabSettings: TTabSheet;
    TimerUpdates: TTimer;
    doClear: TButton;
    doExit: TButton;
    doQueue: TButton;
    doSend: TButton;
    doShowQu: TButton;
    PageControl1: TPageControl;
    PanelBottom: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label1: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label65: TLabel;
    Label67: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelAuthPass: TLabel;
    LabelAuthUser: TLabel;
    LabelCount: TLabel;
    LabelProg: TLabel;
    LabelQueue: TLabel;
    IcsMailQueue1: TIcsMailQueue;
    Label5: TLabel;
    Label12: TLabel;
    Label22: TLabel;
    IcsRestEmail1: TIcsRestEmail;
    OAuth2Settings: TGroupBox;
    Label44: TLabel;
    Label45: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label51: TLabel;
    PrefTokenAccnt: TEdit;
    procedure doSendClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure doQueueClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure doShowQuClick(Sender: TObject);
    procedure TimerUpdatesTimer(Sender: TObject);
    procedure IcsMailQueue1LogEvent(LogLevel: TMailLogLevel;
      const Info: string);
    procedure IcsMailQueue1QuChangedEvent(Sender: TObject);
    procedure PrefEmailSecure1Change(Sender: TObject);
    procedure PrefEmailSecure2Change(Sender: TObject);
    procedure PrefChange(Sender: TObject);
    procedure IcsMailQueue1OATokenEvent(ServNr: Integer; var Token, TokAccount: string;
      var TokExpireDT: TDateTime);
    procedure IcsRestEmail1EmailNewToken(Sender: TObject);
    procedure IcsRestEmail1EmailProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
  private
    { Private declarations }
    procedure MailLogAdd (info: string) ;
    procedure MailDiagAdd (info: string) ;
    procedure StartQueueMail;
    procedure StopQueueMail;
    procedure OpenLogFile;
  public
    { Public declarations }
  end;

var
    DemoForm: TDemoForm;
    AbortFlag: boolean ;
    FIniFileName: string ;
    DiagWinFlag: Boolean = false ;
    ViewQuWinFlag: Boolean = false ;
    BuffDiags: string ;
    BuffInfos: string ;
    QuUpdateFlag: boolean ;
    FIcsBuffLogStream: TIcsBuffLogStream;

implementation

uses OverbyteIcsMailQuTstDiag, OverbyteIcsMailQuTstView;

{$R *.DFM}

function ExtractEmail (DoubleAddr: string): string ;
var
    FriendlyName: string ;
begin
    result := ParseEmail (DoubleAddr, FriendlyName) ;   // smptprot
end ;

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FreeAndNil(FIcsBuffLogStream);
end;

procedure TDemoForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
    IniFile : TIcsIniFile;
    SectionData, temp: string ;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
  WriteString (SectionData, 'FileAttachment_Text', FileAttachment.Text) ;
  WriteString (SectionData, 'MailBody_Lines', MailBody.Lines.CommaText) ;
  WriteString (SectionData, 'MailCC_Text', MailCC.Text) ;
  WriteString (SectionData, 'MailFrom_Text', MailFrom.Text) ;
  WriteInteger (SectionData, 'MailPriority_ItemIndex', MailPriority.ItemIndex) ;
  WriteString (SectionData, 'MailSubject_Text', MailSubject.Text) ;
  WriteString (SectionData, 'RecipList_Lines', RecipList.Lines.CommaText) ;
  WriteString (SectionData, 'PrefAttemptsList_Text', PrefAttemptsList.Text) ;
  WriteString (SectionData, 'PrefCARootBundle_Text', PrefCARootBundle.Text) ;
  WriteString (SectionData, 'PrefClientCertFile_Text', PrefClientCertFile.Text) ;
  WriteString (SectionData, 'PrefDnsServer_Text', PrefDnsServer.Text) ;
  WriteString (SectionData, 'PrefHeloHost_Text', PrefHeloHost.Text) ;
  WriteString (SectionData, 'PrefMailQuFolder_Text', PrefMailQuFolder.Text) ;
  WriteInteger (SectionData, 'PrefSendMethod_ItemIndex', PrefSendMethod.ItemIndex) ;
  if PrefSslRevoke.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'PrefSslRevoke_Checked', temp) ;
  WriteInteger (SectionData, 'PrefVerifyCertMode_ItemIndex', PrefVerifyCertMode.ItemIndex) ;
  WriteString (SectionData, 'PrefEmailAuthPass1_Text', PrefEmailAuthPass1.Text) ;
  WriteString (SectionData, 'PrefEmailAuthPass2_Text', PrefEmailAuthPass2.Text) ;
  WriteInteger (SectionData, 'PrefEmailAuthType1_ItemIndex', PrefEmailAuthType1.ItemIndex) ;
  WriteInteger (SectionData, 'PrefEmailAuthType2_ItemIndex', PrefEmailAuthType2.ItemIndex) ;
  WriteString (SectionData, 'PrefEmailAuthUser1_Text', PrefEmailAuthUser1.Text) ;
  WriteString (SectionData, 'PrefEmailAuthUser2_Text', PrefEmailAuthUser2.Text) ;
  WriteString (SectionData, 'PrefEmailPort1_Text', PrefEmailPort1.Text) ;
  WriteString (SectionData, 'PrefEmailPort2_Text', PrefEmailPort2.Text) ;
  WriteInteger (SectionData, 'PrefEmailSecure1_ItemIndex', PrefEmailSecure1.ItemIndex) ;
  WriteInteger (SectionData, 'PrefEmailSecure2_ItemIndex', PrefEmailSecure2.ItemIndex) ;
  WriteInteger (SectionData, 'PrefEmailSecurity1_ItemIndex', PrefEmailSecurity1.ItemIndex) ;
  WriteInteger (SectionData, 'PrefEmailSecurity2_ItemIndex', PrefEmailSecurity2.ItemIndex) ;
  WriteString (SectionData, 'PrefEmailSmtp1_Text', PrefEmailSmtp1.Text) ;
  WriteString (SectionData, 'PrefEmailSmtp2_Text', PrefEmailSmtp2.Text) ;
  if PrefEmailSslErr1.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'PrefEmailSslErr1_Checked', temp) ;
  if PrefEmailSslErr2.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'PrefEmailSslErr2_Checked', temp) ;
  WriteString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  WriteString (SectionData, 'PrefClientId_Text', PrefClientId.Text) ;
  WriteString (SectionData, 'PrefRefrToken_Text', PrefRefrToken.Text) ;
  WriteInteger (SectionData, 'PrefRestType_ItemIndex', PrefRestType.ItemIndex) ;
  WriteString (SectionData, 'PrefClientSecret_Text', PrefClientSecret.Text) ;

        WriteInteger (SectionData, 'Top', Top);
        WriteInteger (SectionData, 'Left', Left);
        WriteInteger (SectionData, 'Width', Width);
        WriteInteger (SectionData, 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;

end;

procedure TDemoForm.FormCreate(Sender: TObject);
var
    IniFile : TIcsIniFile;
    SectionData: string ;
    Level: TSslCliSecurity;
    Auth: TSmtpAuthType;
begin
    FIniFileName := GetIcsIniFileName;
    PageControl1.ActivePage := TabMessage;
    SendMessage (MailLog.Handle, EM_EXLIMITTEXT, 0, 6000000) ;   // 6 meg
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
    GSSL_DLL_DIR := ExtractFilePath (ParamStr (0)) ;   // Nov 2016 from our directory
    PrefEmailSecurity1.Items.Clear;  // June 2018 update SSL client security levels
    for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         PrefEmailSecurity1.Items.Add (SslCliSecurityNames[Level]);
    PrefEmailSecurity2.Items.Assign(PrefEmailSecurity1.Items);
  { V8.65 update server auth methods } 
    PrefEmailAuthType1.Items.Clear;
    PrefEmailAuthType2.Items.Clear;
    for Auth := Low(TSmtpAuthType) to High(TSmtpAuthType) do begin
        PrefEmailAuthType1.Items.Add(SmtpAuthTypeNames[Auth]);
        PrefEmailAuthType2.Items.Add(SmtpAuthTypeNames[Auth]);
    end;

// get old settings
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
  FileAttachment.Text := ReadString (SectionData, 'FileAttachment_Text', FileAttachment.Text) ;
  MailBody.Lines.CommaText := ReadString (SectionData, 'MailBody_Lines', '') ;
  MailCC.Text := ReadString (SectionData, 'MailCC_Text', MailCC.Text) ;
  MailFrom.Text := ReadString (SectionData, 'MailFrom_Text', MailFrom.Text) ;
  MailPriority.ItemIndex := ReadInteger (SectionData, 'MailPriority_ItemIndex', MailPriority.ItemIndex) ;
  MailSubject.Text := ReadString (SectionData, 'MailSubject_Text', MailSubject.Text) ;
  RecipList.Lines.CommaText := ReadString (SectionData, 'RecipList_Lines', '') ;
  PrefAttemptsList.Text := ReadString (SectionData, 'PrefAttemptsList_Text', PrefAttemptsList.Text) ;
  PrefCARootBundle.Text := ReadString (SectionData, 'PrefCARootBundle_Text', PrefCARootBundle.Text) ;
  PrefClientCertFile.Text := ReadString (SectionData, 'PrefClientCertFile_Text', PrefClientCertFile.Text) ;
  PrefDnsServer.Text := ReadString (SectionData, 'PrefDnsServer_Text', PrefDnsServer.Text) ;
  PrefHeloHost.Text := ReadString (SectionData, 'PrefHeloHost_Text', PrefHeloHost.Text) ;
  PrefMailQuFolder.Text := ReadString (SectionData, 'PrefMailQuFolder_Text', PrefMailQuFolder.Text) ;
  PrefSendMethod.ItemIndex := ReadInteger (SectionData, 'PrefSendMethod_ItemIndex', PrefSendMethod.ItemIndex) ;
  if ReadString (SectionData, 'PrefSslRevoke_Checked', 'False') = 'True' then PrefSslRevoke.Checked := true else PrefSslRevoke.Checked := false ;
  PrefVerifyCertMode.ItemIndex := ReadInteger (SectionData, 'PrefVerifyCertMode_ItemIndex', PrefVerifyCertMode.ItemIndex) ;
  PrefEmailAuthPass1.Text := ReadString (SectionData, 'PrefEmailAuthPass1_Text', PrefEmailAuthPass1.Text) ;
  PrefEmailAuthPass2.Text := ReadString (SectionData, 'PrefEmailAuthPass2_Text', PrefEmailAuthPass2.Text) ;
  PrefEmailAuthType1.ItemIndex := ReadInteger (SectionData, 'PrefEmailAuthType1_ItemIndex', PrefEmailAuthType1.ItemIndex) ;
  PrefEmailAuthType2.ItemIndex := ReadInteger (SectionData, 'PrefEmailAuthType2_ItemIndex', PrefEmailAuthType2.ItemIndex) ;
  PrefEmailAuthUser1.Text := ReadString (SectionData, 'PrefEmailAuthUser1_Text', PrefEmailAuthUser1.Text) ;
  PrefEmailAuthUser2.Text := ReadString (SectionData, 'PrefEmailAuthUser2_Text', PrefEmailAuthUser2.Text) ;
  PrefEmailPort1.Text := ReadString (SectionData, 'PrefEmailPort1_Text', PrefEmailPort1.Text) ;
  PrefEmailPort2.Text := ReadString (SectionData, 'PrefEmailPort2_Text', PrefEmailPort2.Text) ;
  PrefEmailSecure1.ItemIndex := ReadInteger (SectionData, 'PrefEmailSecure1_ItemIndex', PrefEmailSecure1.ItemIndex) ;
  PrefEmailSecure2.ItemIndex := ReadInteger (SectionData, 'PrefEmailSecure2_ItemIndex', PrefEmailSecure2.ItemIndex) ;
  PrefEmailSecurity1.ItemIndex := ReadInteger (SectionData, 'PrefEmailSecurity1_ItemIndex', PrefEmailSecurity1.ItemIndex) ;
  PrefEmailSecurity2.ItemIndex := ReadInteger (SectionData, 'PrefEmailSecurity2_ItemIndex', PrefEmailSecurity2.ItemIndex) ;
  PrefEmailSmtp1.Text := ReadString (SectionData, 'PrefEmailSmtp1_Text', PrefEmailSmtp1.Text) ;
  PrefEmailSmtp2.Text := ReadString (SectionData, 'PrefEmailSmtp2_Text', PrefEmailSmtp2.Text) ;
  if ReadString (SectionData, 'PrefEmailSslErr1_Checked', 'False') = 'True' then PrefEmailSslErr1.Checked := true else PrefEmailSslErr1.Checked := false ;
  if ReadString (SectionData, 'PrefEmailSslErr2_Checked', 'False') = 'True' then PrefEmailSslErr2.Checked := true else PrefEmailSslErr2.Checked := false ;
  DirLogs.Text := ReadString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  PrefClientId.Text := ReadString (SectionData, 'PrefClientId_Text', PrefClientId.Text) ;
  PrefRefrToken.Text := ReadString (SectionData, 'PrefRefrToken_Text', PrefRefrToken.Text) ;
  PrefRestType.ItemIndex := ReadInteger (SectionData, 'PrefRestType_ItemIndex', PrefRestType.ItemIndex) ;
  PrefClientSecret.Text := ReadString (SectionData, 'PrefClientSecret_Text', PrefClientSecret.Text) ;

        Top := ReadInteger (SectionData, 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger (SectionData, 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger (SectionData, 'Width', Width);
        Height := ReadInteger (SectionData, 'Height', Height);
    end;
    IniFile.Free;

   if PrefEmailSecurity1.ItemIndex <= 0 then
                        PrefEmailSecurity1.ItemIndex := Ord(sslCliSecDefault);   // June 2018
   if PrefEmailSecurity2.ItemIndex <= 0 then
                        PrefEmailSecurity2.ItemIndex := Ord(sslCliSecDefault);   // June 2018
end;

{ this event is used to open the log file, note log ls written as UTF8 codepage }

procedure TDemoForm.OpenLogFile;
var
    FName: String;
begin
    if DirLogs.Text = '' then Exit; // no log
    FName := '"' + IncludeTrailingPathDelimiter(DirLogs.Text) +
                                              'ics-mailqu-"yyyy-mm-dd".log"';
    if NOT Assigned(FIcsBuffLogStream) then
        FIcsBuffLogStream := TIcsBuffLogStream.Create(self, FName,
                                     DemoForm.Caption + IcsCRLF, FileCPUtf8)
    else begin
        if FName = FIcsBuffLogStream.NameMask then Exit; // skip no change
        if FIcsBuffLogStream.LogSize > 0 then
            FIcsBuffLogStream.FlushFile(True);  // changing log path, write old log first
        FIcsBuffLogStream.NameMask := FName;
    end;
    MailDiagAdd(IcsCRLF + 'Opened log file: ' + FIcsBuffLogStream.FullName);
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
    StopQueueMail;
end;

procedure TDemoForm.FormResize(Sender: TObject);
begin
    MailLog.Left := PageControl1.Width ;
    MailLog.Height := PageControl1.Height ;
    MailLog.Width := Self.ClientWidth - MailLog.Left ;
    MailBody.Top := PageControl1.Height ;
    MailBody.Width := Self.ClientWidth ;
    MailBody.Height := PanelBottom.Top - MailBody.Top ;
end;

procedure TDemoForm.StartQueueMail;
var
    info: string ;
    TokenFlag: Boolean;
begin
    OpenLogFile;
    if NOT IcsMailQueue1.Active then
    begin
        if PrefMailQuFolder.Text = '' then
        begin
            MailLogAdd ('Must Specify Mail Queue Directory') ;
            exit ;
        end;
        if NOT ForceDirectories (PrefMailQuFolder.Text) then
        begin
            MailLogAdd ('Failed to Create Mail Queue Directory: ' +  PrefMailQuFolder.Text) ;
            exit ;
        end;
        DiagForm.Visible := true ;
        try
            IcsMailQueue1.SslVerMethod := TMailVerifyMethod (PrefVerifyCertMode.ItemIndex) ;
            IcsMailQueue1.SslRevocation := PrefSslRevoke.Checked ;
            IcsMailQueue1.SslReportChain := true ;
            IcsMailQueue1.SslRootFile := PrefCARootBundle.Text;
            IcsMailQueue1.LogQuSent := true ;  // create log of sent email
            IcsMailQueue1.RetryList := PrefAttemptsList.Text ;
            IcsMailQueue1.ArchiveSent := true ;  // keep copies of sent mail
            IcsMailQueue1.DeleteFailed := false ; // delete failed mail
            IcsMailQueue1.Debug := true ;
            IcsMailQueue1.QuStartDelay := 2 ;
            IcsMailQueue1.MailQuDir := PrefMailQuFolder.Text ;
            IcsMailQueue1.DnsServers.Clear ;
            IcsMailQueue1.DnsServers.Add (PrefDnsServer.Text) ;
            IcsMailQueue1.DnsServers.Add ('8.8.4.4') ; // Google
            IcsMailQueue1.SmtpMethod := TMailSmtpMethod (PrefSendMethod.ItemIndex) ;
            IcsMailQueue1.MxSrvUseSsl := (PrefEmailSecure1.ItemIndex > 0) ;
            IcsMailQueue1.QuHtmlSmtp.SignOn := PrefHeloHost.Text ;
            if (PrefEmailSmtp1.Text = '') and (IcsMailQueue1.SmtpMethod = MailSmtpRelay) then
            begin
                MailLogAdd ('Must Specify a Mail Server') ;
                exit ;
            end;

         // add multiple email servers, not necessary if using specific or MX domain lookup
            IcsMailQueue1.MailServers.Clear ;
            TokenFlag := False;
            if PrefEmailSmtp1.Text <> '' then
            begin
                IcsMailQueue1.MailServers.Add ;
                with IcsMailQueue1.MailServers [0] do
                begin
                    Port := PrefEmailPort1.Text ;     // 25 or 465 or 587
                    Host := PrefEmailSmtp1.Text ;
                    AuthType := TSmtpAuthType (PrefEmailAuthType1.ItemIndex) ;
                    if AuthType in [smtpAuthXOAuth2,  smtpAuthOAuthBearer] then
                        TokenFlag := True;                                   { V8.65 }
                    UserName := PrefEmailAuthUser1.Text ;
                    Password := PrefEmailAuthPass1.Text ;
                    SslType := TSmtpSslType (PrefEmailSecure1.ItemIndex) ;
                    LocalAddr := '0.0.0.0';
                    SignOn := PrefHeloHost.Text ;
                    SslCliSecurity := TSslCliSecurity(PrefEmailSecurity1.ItemIndex);
                    RetryWithoutSsl := PrefEmailSslErr1.Checked ;
                    if PrefClientCertFile.Text <> '' then
                    begin
                        if FileExists (PrefClientCertFile.Text) then
                            SslCliCert.LoadFromFile(PrefClientCertFile.Text)
                        else
                            MailLogAdd ('SSL client certificate ignored, not found: ' +
                                                                PrefClientCertFile.Text);
                    end;
                end;
            end;
            if PrefEmailSmtp2.Text <> '' then
            begin
                IcsMailQueue1.MailServers.Add ;
                with IcsMailQueue1.MailServers [1] do
                begin
                    Port := PrefEmailPort2.Text ;     // 25 or 465 or 587
                    Host := PrefEmailSmtp2.Text ;
                    AuthType := TSmtpAuthType (PrefEmailAuthType2.ItemIndex) ;
                    if AuthType in [smtpAuthXOAuth2,  smtpAuthOAuthBearer] then
                        TokenFlag := True;                                   { V8.65 }
                    UserName := PrefEmailAuthUser2.Text ;
                    Password := PrefEmailAuthPass2.Text ;
                    SslType := TSmtpSslType (PrefEmailSecure2.ItemIndex) ;
                    LocalAddr := '0.0.0.0';
                    SignOn := PrefHeloHost.Text ;
                    SslCliSecurity := TSslCliSecurity(PrefEmailSecurity2.ItemIndex);
                    RetryWithoutSsl := PrefEmailSslErr2.Checked ;
                end;
            end;

        { V8.65 OAuth2 settings if needed for Gmail and Outlook/Live mail }
        { warning, may need different OAuth2 for differenmt SMTP servers }
            if (PrefClientId.Text <> '') and (PrefClientSecret.Text <> '') then
            begin
                if PrefRestType.ItemIndex = 0 then
                    IcsRestEmail1.RestEmailType := RestEmailGoogle
                else if PrefRestType.ItemIndex = 1 then
                    IcsRestEmail1.RestEmailType := RestEmailMSSmtp
                 else
                    Exit;
                IcsRestEmail1.ClientId := PrefClientId.Text;
                IcsRestEmail1.ClientSecret := PrefClientSecret.Text;
                IcsRestEmail1.RefrToken := PrefRefrToken.Text;

            { not really necessary, mail queue will do same thing when it's needs a token
              but perhaps easier when starting up }
                if TokenFlag then
                begin
                    if NOT IcsMailQueue1.CheckOAuthLogins then
                        MailLogAdd ('Failed to get OAuth2 Bearer Token');
                end;
            end
            else begin
                if TokenFlag then begin
                    MailLogAdd ('Can Not Use OAuth2 Authentication without Client Id and Secret') ;
                    exit ;
                end;
            end;

            IcsMailQueue1.Active := true ;
            if IcsMailQueue1.Active then
            begin
                info := 'Started Mail Queue OK' ;
                doQueue.Caption := 'Stop Queue'
            end;
        except
            info := 'Failed to Start Mail Queue - ' +  IcsGetExceptMess (ExceptObject) ;
        end;
        MailLogAdd (info) ;
        LabelCount.Caption := info ;
    end ;
end;

procedure TDemoForm.StopQueueMail;
var
    info: string ;
begin
    doQueue.Caption := 'Start Queue' ;
    if IcsMailQueue1.Active then
    begin
        IcsMailQueue1.Active := false ;
    end ;
    if NOT IcsMailQueue1.Active then
    begin
        info := '' ;
    end
    else
        info := 'Failed to Stop Mail Queue' ;
    MailLogAdd (info) ;
    LabelCount.Caption := info ;
end;


procedure TDemoForm.doSendClick(Sender: TObject);
var
    index, donenr, errcode, item, errnr: integer ;
    recip, info: string ;
begin
    if RecipList.Lines.Count > MaxAddressesAllowed then
    begin
        MailLogAdd ('Trying to queue too many emails') ;
        exit ;
    end;

    try // finally
        LabelProg.Caption := '' ;
        doSend.Enabled := false ;
        AbortFlag := false ;
        DiagForm.Visible := true ;

    // stop queue if running so new folder can be set, then start queue if not running
        if IcsMailQueue1.MailQuDir <> PrefMailQuFolder.Text then StopQueueMail;
        StartQueueMail;
        if NOT IcsMailQueue1.Active then exit ;
        IcsMailQueue1.SmtpMethod := TMailSmtpMethod (PrefSendMethod.ItemIndex) ;
        IcsMailQueue1.SslVerMethod := TMailVerifyMethod (PrefVerifyCertMode.ItemIndex) ;
        IcsMailQueue1.SslRevocation := PrefSslRevoke.Checked ;

    // queue message details
        IcsMailQueue1.QuHtmlSmtp.SignOn := PrefHeloHost.Text ;
        IcsMailQueue1.QuHtmlSmtp.EmailFiles.Clear ;
        IcsMailQueue1.QuHtmlSmtp.RcptName.clear;
        IcsMailQueue1.QuHtmlSmtp.Allow8bitChars := true ;
        IcsMailQueue1.QuHtmlSmtp.HdrFrom := MailFrom.Text ;
        IcsMailQueue1.QuHtmlSmtp.FromName := ExtractEmail (MailFrom.Text) ;
        IcsMailQueue1.QuHtmlSmtp.HdrCc := MailCC.Text ;
        IcsMailQueue1.QuHtmlSmtp.HdrReplyTo := IcsMailQueue1.QuHtmlSmtp.FromName ;
        IcsMailQueue1.QuHtmlSmtp.HdrSubject := MailSubject.Text;
        IcsMailQueue1.QuHtmlSmtp.HdrPriority := TSmtpPriority (MailPriority.ItemIndex) ;
        IcsMailQueue1.QuHtmlSmtp.ContentType := smtpPlainText ;
    //        IcsMailQueue1.QuHtmlSmtp.ContentType := smtpHtml ;
    //        IcsMailQueue1.QuHtmlSmtp.HtmlText.Text := Body.Text ;
        IcsMailQueue1.QuHtmlSmtp.PlainText.Text := MailBody.Text ;
        if FileAttachment.Text <> '' then
        begin
            IcsMailQueue1.QuHtmlSmtp.EmailFiles.Add (FileAttachment.Text) ;
        end;
        donenr := 0 ;
        errnr := 0 ;
        for index := 0 to RecipList.Lines.Count - 1 do
        begin
            if AbortFlag then break ;
            recip := RecipList.Lines [index] ;
            if Pos ('*', recip) = 1 then continue ;
            if length (recip) <= 2 then continue ;
            ActiveControl := MailLog ;
            MailLog.SelStart := 999999999 ;
            try
                IcsMailQueue1.QuHtmlSmtp.RcptName.Clear ;
                IcsMailQueue1.QuHtmlSmtp.RcptName.Add(ExtractEmail (recip)) ;
                if MailCC.Text <> '' then IcsMailQueue1.QuHtmlSmtp.RcptName.Add(ExtractEmail (MailCC.Text)) ;
                IcsMailQueue1.QuHtmlSmtp.HdrTo := recip ;
                LabelCount.Caption :=  'Processing ' + IntToStr (index + 1) + ' of ' +
                                        IntToStr (RecipList.Lines.Count) + ' with ' + IntToStr (errnr) + ' Errors' ;
                LabelProg.Caption := 'Current Recipient - ' + recip ;
                Refresh ;
                errcode := 0 ;
                if IcsMailQueue1.SmtpMethod = MailSmtpSpecific then
                   item := IcsMailQueue1.QueueMail (PrefEmailSmtp1.Text, PrefEmailSmtp2.Text, '')  // up to three specific mail servers
                else
                   item := IcsMailQueue1.QueueMail ;
                if item = 0 then
                begin
                    info := 'Failed to Queue Mail - ' + IcsMailQueue1.QuHtmlSmtp.ErrorMessage ;
                    errcode := atoi (Copy (IcsMailQueue1.QuHtmlSmtp.ErrorMessage, 1, 3)) ;
                end
                else
                    info := 'Mail Queued OK as Item ' + IntToStr (item) ;
                MailLogAdd (info) ;

            // mail failed
                if errcode <> 0 then
                begin
                    beep ;
                end
                else
                begin
                    inc (donenr) ;
                end ;
            except
                info := IcsGetExceptMess (ExceptObject) ;
                MailLogAdd (info) ;
                beep ;
                inc (errnr) ;
            end ;
            Application.ProcessMessages ;
        end ;
        info := 'Mail Queued - ' + IntToStr (donenr) + ' Items With ' + IntToStr (errnr) + ' Errors' ;
        MailLogAdd (info + IcsCRLF) ;
        LabelCount.Caption := info ;
        LabelProg.Caption := 'Finished' ;
        TimerUpdatesTimer (Self) ;
        doShowQuClick (Self) ;
    finally
        doSend.Enabled := true ;
    end ;
end;

procedure TDemoForm.TimerUpdatesTimer(Sender: TObject);
var
    displen: integer ;
begin
    displen := Length (BuffDiags) ;
    if displen > 0 then
    begin
        if DiagWinFlag then
        try
            SetLength (BuffDiags, displen - 2) ;  // remove CRLF
            DiagForm.TraceBox.Lines.Add (BuffDiags) ;
            SendMessage (DiagForm.TraceBox.Handle, WM_VSCROLL, SB_BOTTOM, 0);
        except
            LabelProg.Caption := 'Error writing to diag log' ;
        end ;
        BuffDiags := '' ;
    end;
    displen := Length (BuffInfos) ;
    if displen > 0 then
    begin
        try
            SetLength (BuffInfos, displen - 2) ;  // remove CRLF
            MailLog.Lines.Add (BuffInfos) ;
            SendMessage (MailLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
        except
            LabelProg.Caption := 'Error writing to log' ;
        end ;
        BuffInfos := '' ;
    end;
    if QuUpdateFlag then
    begin
        QuUpdateFlag := false ;
        LabelQueue.Caption := 'Mail Queue: Items Queued ' + IntToStr (IcsMailQueue1.MailTotItems) ;
        if ViewQuWinFlag then ViewQuForm.UpdateList;
    end;
end;

procedure TDemoForm.MailLogAdd (info: string) ;
begin
    BuffInfos := BuffInfos + Info + IcsCRLF ;
end ;

procedure TDemoForm.PrefEmailSecure1Change(Sender: TObject);
begin
    PrefChange (self);
    if PrefEmailSecure1.ItemIndex = Ord ( smtpTlsImplicit) then
        PrefEmailPort1.Text := '465'
    else
        PrefEmailPort1.Text := '25' ;
end;

procedure TDemoForm.PrefEmailSecure2Change(Sender: TObject);
begin
    PrefChange (self);
    if PrefEmailSecure2.ItemIndex = Ord ( smtpTlsImplicit) then
        PrefEmailPort2.Text := '465'
    else
        PrefEmailPort2.Text := '25' ;
end;

procedure TDemoForm.PrefChange(Sender: TObject);
begin
 // need to restart queue for new settings
    if IcsMailQueue1.Active then StopQueueMail;
end;

procedure TDemoForm.IcsMailQueue1LogEvent(LogLevel: TMailLogLevel; const Info: string);
begin
    if LogLevel = MLogLevelDiag then
        MailDiagAdd (Info)
    else
    begin
        MailLogAdd (Info) ;
        MailDiagAdd (Info)
    end;
end;

//  OAuth2

procedure TDemoForm.IcsMailQueue1OATokenEvent(ServNr: Integer;
                     var Token, TokAccount: string; var TokExpireDT: TDateTime);  { V8.65}
begin
    MailLogAdd ('Starting to get OAuth2 Bearer Token');
    if NOT IcsRestEmail1.GetNewToken(True) then   // allow interaction, waits for broweser window to be completed
        MailLogAdd ('Failed to get OAuth2 Bearer Token')
    else begin
        Token := IcsRestEmail1.AccToken;
        TokExpireDT := IcsRestEmail1.AccExpireDT;
        TokAccount := IcsRestEmail1.NewAccEmail;
    end;
end;

procedure TDemoForm.IcsMailQueue1QuChangedEvent(Sender: TObject);
begin
    QuUpdateFlag := true ;
end;

procedure TDemoForm.IcsRestEmail1EmailNewToken(Sender: TObject);                { V8.65}
begin
// we should save refresh token since only get it after a login window
    if IcsRestEmail1.RefrToken <> '' then
        PrefRefrToken.Text := IcsRestEmail1.RefrToken;
    PrefTokenAccnt.Text := IcsRestEmail1.NewAccEmail;
end;

procedure TDemoForm.IcsRestEmail1EmailProg(Sender: TObject; LogOption: TLogOption; const Msg: string);     { V8.65}
begin
    MailDiagAdd (Msg);
end;

procedure TDemoForm.MailDiagAdd (info: string) ;
begin
   { log window }
    if DiagWinFlag then
        BuffDiags := BuffDiags + Info + icsCRLF ;

  { write log file }
    try
        if (DirLogs.Text = '') then Exit ;
        if NOT Assigned(FIcsBuffLogStream) then Exit; // sanity check
        FIcsBuffLogStream.WriteLine(info);
    except
    end;
end ;

procedure TDemoForm.doExitClick(Sender: TObject);
begin
    DiagForm.Close ;
    Close ;
end;

procedure TDemoForm.doQueueClick(Sender: TObject);
begin
    if IcsMailQueue1.Active then
        StopQueueMail
    else
        StartQueueMail;
end;

procedure TDemoForm.doShowQuClick(Sender: TObject);
begin
   ViewQuForm.Show ;
   ViewQuForm.UpdateList;
end;

procedure TDemoForm.doClearClick(Sender: TObject);
begin
    RecipList.Lines.Clear ;
    MailSubject.Text := '' ;
    MailBody.Lines.Clear ;
    MailFrom.Text := '' ;
    MailCC.Text := '' ;
end;

end.
