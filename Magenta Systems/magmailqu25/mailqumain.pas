unit mailqumain;

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 22nd June 2018
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Demo application for TMagMailQueue which is designed to prepare, queue and send email.
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


    
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RichEdit, Mask, ComCtrls, ExtCtrls, IniFiles,
{$IF CompilerVersion > 23}
  System.UITypes,
{$IFEND}
  magsubs1, MagentaMailQueue,
  OverbyteIcsWSocket, OverbyteIcsSmtpProt, OverbyteIcsWinSock,
  OverbyteIcsWndControl, OverbyteIcsSSLEAY, OverbyteIcsSslX509Utils ;

type
  TDemoForm = class(TForm)
    RecipList: TMemo;
    MailBody: TMemo;
    MailSubject: TEdit;
    MailFrom: TEdit;
    MailCC: TEdit;
    MailPriority: TComboBox;
    FileAttachment: TEdit;
    MailQuFolder: TEdit;
    SendMethod: TRadioGroup;
    SMTPServer: TEdit;
    ServSmtpAuth: TRadioGroup;
    ServSmtpUser: TEdit;
    ServSmtpPass: TEdit;
    AttemptsList: TEdit;
    VerifyCertMode: TRadioGroup;
    CheckSslRevoke: TCheckBox;
    DnsServer: TEdit;
    HeloHostName: TEdit;
    ServSmtpSecure: TComboBox;
    ServSmtpPort: TEdit;
    SslSecurity: TComboBox;
    ServRetryNoSsl: TCheckBox;

    doSend: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    doExit: TButton;
    Label5: TLabel;
    Label6: TLabel;
    MailLog: TRichEdit;
    doClear: TButton;
    LabelUser: TLabel;
    LabelPass: TLabel;
    PanelBottom: TPanel;
    LabelProg: TLabel;
    LabelCount: TLabel;
    PanelControls: TPanel;
    Status: TStatusBar;
    Label10: TLabel;
    LabelQueue: TLabel;
    Label14: TLabel;
    Label11: TLabel;
    SslContext: TSslContext;
    doQueue: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    doShowQu: TButton;
    Label12: TLabel;
    TimerUpdates: TTimer;
    Label13: TLabel;
    procedure doSendClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure doQueueClick(Sender: TObject);
    procedure SMTPChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure doShowQuClick(Sender: TObject);
    procedure ServSmtpSecureChange(Sender: TObject);
    procedure TimerUpdatesTimer(Sender: TObject);
  private
    { Private declarations }
    procedure MailLogAdd (info: string) ;
    procedure MailDiagAdd (info: string) ;
    procedure QuLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
    procedure QuChangedEvent (Sender: TObject);
    procedure StartQueueMail;
    procedure StopQueueMail;
  public
    { Public declarations }
  end;

var
    DemoForm: TDemoForm;
    AbortFlag: boolean ;
    MagMailQueue: TMagMailQueue ;
    FIniFileName: string ;
    DiagWinFlag: Boolean = false ;
    ViewQuWinFlag: Boolean = false ;
    BuffDiags: string ;
    BuffInfos: string ;
    QuUpdateFlag: boolean ;

implementation

uses mailqudiag, mailquview;

{$R *.DFM}

function ExtractEmail (DoubleAddr: string): string ;
var
    FriendlyName: string ;
begin
    result := ParseEmail (DoubleAddr, FriendlyName) ;   // smptprot
end ;

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;

procedure TDemoForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
    IniFile : TMemIniFile;
    section, temp: string ;
begin
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        WriteString (section, 'RecipList_Lines', RecipList.Lines.CommaText) ;
        WriteString (section, 'MailBody_Lines', MailBody.Lines.CommaText) ;
        WriteString (section, 'MailSubject_Text', MailSubject.Text) ;
        WriteString (section, 'MailFrom_Text', MailFrom.Text) ;
        WriteString (section, 'MailCC_Text', MailCC.Text) ;
        WriteInteger (section, 'MailPriority_ItemIndex', MailPriority.ItemIndex) ;
        WriteString (section, 'FileAttachment_Text', FileAttachment.Text) ;
        WriteString (section, 'MailQuFolder_Text', MailQuFolder.Text) ;
        WriteInteger (section, 'SendMethod_ItemIndex', SendMethod.ItemIndex) ;
        WriteString (section, 'SMTPServer_Text', SMTPServer.Text) ;
        WriteInteger (section, 'ServSmtpAuth_ItemIndex', ServSmtpAuth.ItemIndex) ;
        WriteString (section, 'ServSmtpUser_Text', ServSmtpUser.Text) ;
        WriteString (section, 'ServSmtpPass_Text', ServSmtpPass.Text) ;
        WriteString (section, 'AttemptsList_Text', AttemptsList.Text) ;
        WriteInteger (section, 'VerifyCertMode_ItemIndex', VerifyCertMode.ItemIndex) ;
        if CheckSslRevoke.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'CheckSslRevoke_Checked', temp) ;
        WriteString (section, 'DnsServer_Text', DnsServer.Text) ;
        WriteString (section, 'HeloHostName_Text', HeloHostName.Text) ;
        WriteInteger (section, 'ServSmtpSecure_ItemIndex', ServSmtpSecure.ItemIndex) ;
        WriteString (section, 'ServSmtpPort_Text', ServSmtpPort.Text) ;
        WriteInteger (section, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
        if ServRetryNoSsl.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ServRetryNoSsl_Checked', temp) ;

        WriteInteger (section, 'Top', Top);
        WriteInteger (section, 'Left', Left);
        WriteInteger (section, 'Width', Width);
        WriteInteger (section, 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;

end;

procedure TDemoForm.FormCreate(Sender: TObject);
var
    IniFile : TMemIniFile;
    section: string ;
    Level: TSslCliSecurity;
begin
   SendMessage (MailLog.Handle, EM_EXLIMITTEXT, 0, 6000000) ;   // 6 meg
//    GSSLEAY_DLL_IgnoreNew := true ; // !!! TEMP TESTING
    GSSLEAY_DLL_IgnoreOld := true ;    // Nov 2016 use latest OpenSSL
    GSSL_DLL_DIR := ExtractFilePath (ParamStr (0)) ;   // Nov 2016 from our directory
    MagMailQueue := TMagMailQueue.Create (self) ;
    SslSecurity.Items.Clear;  // June 2018 update SSL client security levels
    for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslSecurity.Items.Add (SslCliSecurityNames[Level]);

// get old settings
    FIniFileName := ChangeFileExt (ParamStr (0), '.ini') ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        RecipList.Lines.CommaText := ReadString (section, 'RecipList_Lines', '') ;
        MailBody.Lines.CommaText := ReadString (section, 'MailBody_Lines', '"This is a test message from the Magenta Mail Queue Demo application"') ;
        MailSubject.Text := ReadString (section, 'MailSubject_Text', 'Test from Magenta Mail Queue Demo') ;
        MailFrom.Text := ReadString (section, 'MailFrom_Text', '') ;
        MailCC.Text := ReadString (section, 'MailCC_Text', '') ;
        MailPriority.ItemIndex := ReadInteger (section, 'MailPriority_ItemIndex', 0) ;
        FileAttachment.Text := ReadString (section, 'FileAttachment_Text', '') ;
        MailQuFolder.Text := ReadString (section, 'MailQuFolder_Text', ExtractFileDir (Lowercase (ParamStr(0))) + '\mailqueue' ) ;
        SendMethod.ItemIndex := ReadInteger (section, 'SendMethod_ItemIndex', 0) ;
        SMTPServer.Text := ReadString (section, 'SMTPServer_Text', 'mail.magsys.co.uk') ;
        ServSmtpAuth.ItemIndex := ReadInteger (section, 'ServSmtpAuth_ItemIndex', 0) ;
        ServSmtpUser.Text := ReadString (section, 'ServSmtpUser_Text', '') ;
        ServSmtpPass.Text := ReadString (section, 'ServSmtpPass_Text', '') ;
        AttemptsList.Text := ReadString (section, 'AttemptsList_Text', '5,5,10,10,30,30,60,90,300,300,300,300') ;
        VerifyCertMode.ItemIndex := ReadInteger (section, 'VerifyCertMode_ItemIndex', 0) ;
        if ReadString (section, 'CheckSslRevoke_Checked', 'False') = 'True' then CheckSslRevoke.Checked := true else CheckSslRevoke.Checked := false ;
        DnsServer.Text := ReadString (section, 'DnsServer_Text', '8.8.8.8') ; // Google DNS
        HeloHostName.Text := ReadString (section, 'HeloHostName_Text', 'my.host.name') ;
        ServSmtpSecure.ItemIndex := ReadInteger (section, 'ServSmtpSecure_ItemIndex', 0) ;
        ServSmtpPort.Text := ReadString (section, 'ServSmtpPort_Text', '25') ;
        SslSecurity.ItemIndex := ReadInteger (section, 'SslSecurity_ItemIndex', 0) ;
        if ReadString (section, 'ServRetryNoSsl_Checked', 'False') = 'True' then ServRetryNoSsl.Checked := true else ServRetryNoSsl.Checked := false ;

        Top := ReadInteger (section, 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger (section, 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger (section, 'Width', Width);
        Height := ReadInteger (section, 'Height', Height);
    end;
    IniFile.Free;

   if SslSecurity.ItemIndex <= 0 then SslSecurity.ItemIndex := Ord(sslCliSecDefault);   // June 2018
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
    StopQueueMail;
    FreeAndNil (MagMailQueue) ;
end;

procedure TDemoForm.FormResize(Sender: TObject);
begin
    MailLog.Left := PanelControls.Width ;
    MailLog.Height := PanelControls.Height ;
    MailLog.Width := Self.ClientWidth - MailLog.Left ;
    MailBody.Top := PanelControls.Height ;
    MailBody.Width := Self.ClientWidth ;
    MailBody.Height := PanelBottom.Top - MailBody.Top ;
end;

procedure TDemoForm.doSendClick(Sender: TObject);
var
    index, donenr, errcode, item, errnr: integer ;
    recip, info: string ;
begin
    try // finally
    LabelProg.Caption := '' ;
    doSend.Enabled := false ;
    AbortFlag := false ;
    DiagForm.Visible := true ;

// stop queue if running so new folder can be set, then start queue if not running
    if MagMailQueue.MailQuDir <> MailQuFolder.Text then StopQueueMail;
    StartQueueMail;
    if NOT MagMailQueue.Active then exit ;
    MagMailQueue.SmtpMethod := TMailSmtpMethod (SendMethod.ItemIndex) ;
    MagMailQueue.SslVerMethod := TMailVerifyMethod (VerifyCertMode.ItemIndex) ;
    MagMailQueue.SslRevocation := CheckSslRevoke.Checked ;

// queue message details
    MagMailQueue.QuHtmlSmtp.SignOn := HeloHostName.Text ;
    MagMailQueue.QuHtmlSmtp.EmailFiles.Clear ;
    MagMailQueue.QuHtmlSmtp.RcptName.clear;
    MagMailQueue.QuHtmlSmtp.Allow8bitChars := true ;
    MagMailQueue.QuHtmlSmtp.HdrFrom := MailFrom.Text ;
    MagMailQueue.QuHtmlSmtp.FromName := ExtractEmail (MailFrom.Text) ;
    MagMailQueue.QuHtmlSmtp.HdrCc := MailCC.Text ;
    MagMailQueue.QuHtmlSmtp.HdrReplyTo := MagMailQueue.QuHtmlSmtp.FromName ;
    MagMailQueue.QuHtmlSmtp.HdrSubject := MailSubject.Text;
    MagMailQueue.QuHtmlSmtp.HdrPriority := TSmtpPriority (MailPriority.ItemIndex) ;
    MagMailQueue.QuHtmlSmtp.ContentType := smtpPlainText ;
//        MagMailQueue.QuHtmlSmtp.ContentType := smtpHtml ;
//        MagMailQueue.QuHtmlSmtp.HtmlText.Text := Body.Text ;
    MagMailQueue.QuHtmlSmtp.PlainText.Text := MailBody.Text ;
    if FileAttachment.Text <> '' then
    begin
        MagMailQueue.QuHtmlSmtp.EmailFiles.Add (FileAttachment.Text) ;
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
            MagMailQueue.QuHtmlSmtp.RcptName.Clear ;
            MagMailQueue.QuHtmlSmtp.RcptName.Add(ExtractEmail (recip)) ;
            if MailCC.Text <> '' then MagMailQueue.QuHtmlSmtp.RcptName.Add(ExtractEmail (MailCC.Text)) ;
            MagMailQueue.QuHtmlSmtp.HdrTo := recip ;
            LabelCount.Caption :=  'Processing ' + IntToStr (index + 1) + ' of ' +
                                    IntToStr (RecipList.Lines.Count) + ' with ' + IntToStr (errnr) + ' Errors' ;
            LabelProg.Caption := 'Current Recipient - ' + recip ;
            Refresh ;
            errcode := 0 ;
            if MagMailQueue.SmtpMethod = MailSmtpSpecific then
               item := MagMailQueue.QueueMail (SMTPServer.Text, '', '')  // up to three specific mail servers
            else
               item := MagMailQueue.QueueMail ;
            if item = 0 then
            begin
                info := 'Failed to Queue Mail - ' + MagMailQueue.QuHtmlSmtp.ErrorMessage ;
                errcode := AscToInt (Copy (MagMailQueue.QuHtmlSmtp.ErrorMessage, 1, 3)) ;
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
            info := GetExceptMess (ExceptObject) ;
            MailLogAdd (info) ;
            beep ;
            inc (errnr) ;
        end ;
        Application.ProcessMessages ;
    end ;
    info := 'Mail Queued - ' + IntToStr (donenr) + ' Items With ' + IntToStr (errnr) + ' Errors' ;
    MailLogAdd (info + CRLF_) ;
    LabelCount.Caption := info ;
    LabelProg.Caption := 'Finished' ;
    TimerUpdatesTimer (Self) ;
    doShowQuClick (Self) ;
    finally
        doSend.Enabled := true ;
    end ;
end;

procedure TDemoForm.ServSmtpSecureChange(Sender: TObject);
begin
    Changed ;
    if ServSmtpSecure.ItemIndex = Ord ( smtpTlsImplicit) then
        ServSmtpPort.Text := '465'
    else
        ServSmtpPort.Text := '25' ;
end;

procedure TDemoForm.SMTPChange(Sender: TObject);
begin
   if MagMailQueue.Active then StopQueueMail;    // change SMTP stuff, stop queue so new settings used later
end;

procedure TDemoForm.StartQueueMail;
var
    info, fname: string ;
begin
    if NOT MagMailQueue.Active then
    begin
        if MailQuFolder.Text = '' then
        begin
            info := 'Must Specify Mail Queue Directory' ;
            MailLogAdd (info) ;
            exit ;
        end;
        if NOT ForceDirs (MailQuFolder.Text) then
        begin
            info := 'Failed to Create Mail Queue Directory: ' +  MailQuFolder.Text;
            MailLogAdd (info) ;
            exit ;
        end;
        DiagForm.Visible := true ;
        try
          // Oct 2015 setup root certificate authority bundle file to check SSL certificates
            MagMailQueue.SslVerMethod := TMailVerifyMethod (VerifyCertMode.ItemIndex) ;
            MagMailQueue.SslRevocation := CheckSslRevoke.Checked ;
            MagMailQueue.SslReportChain := true ;
            fname := ExtractFileDir (Lowercase (ParamStr(0))) + '\RootCaCertsBundle.pem' ;
            if FileExists (fname) then
            begin
                SslContext.SslCAFile := fname;
            end
            else
            begin
               SslContext.SslCALines.Text := sslRootCACertsBundle;  // June 2018
            end;

          // setup common queue stuff
            MagMailQueue.LogEvent := QuLogEvent ;
            MagMailQueue.QuChangedEvent := QuChangedEvent ;
            MagMailQueue.LogQuSent := true ;  // create log of sent email
            MagMailQueue.RetryList := AttemptsList.Text ;
            MagMailQueue.ArchiveSent := true ;
            MagMailQueue.DeleteFailed := false ;
            MagMailQueue.Debug := true ;
            MagMailQueue.QuStartDelay := 2 ;
            MagMailQueue.MailQuDir := MailQuFolder.Text ;
            MagMailQueue.SslContext := SslContext ;
            MagMailQueue.DnsServers.Clear ;
            MagMailQueue.DnsServers.Add (DnsServer.Text) ;
            MagMailQueue.DnsServers.Add ('8.8.4.4') ; // Google
            MagMailQueue.SmtpMethod := TMailSmtpMethod (SendMethod.ItemIndex) ;
            MagMailQueue.MxSrvUseSsl := (ServSmtpSecure.ItemIndex > 0) ;

         // can add multiple email servers
            MagMailQueue.QuHtmlSmtp.SignOn := HeloHostName.Text ;
            MagMailQueue.MailServers.Clear ;
            MagMailQueue.MailServers.Add ;
            with MagMailQueue.MailServers [MagMailQueue.MailServers.Count - 1] do
            begin
                Port := ServSmtpPort.Text ;     // or 465 or 587
                Host := SMTPServer.Text ;
                AuthType := TSmtpAuthType (ServSmtpAuth.ItemIndex) ;
                UserName := ServSmtpUser.Text ;
                Password := ServSmtpPass.Text ;
                SslType := TSmtpSslType (ServSmtpSecure.ItemIndex) ;
                LocalAddr := '0.0.0.0';
                SignOn := HeloHostName.Text ;
                SslCliSecurity := TSslCliSecurity(SslSecurity.ItemIndex); // June 2018
                RetryWithoutSsl := ServRetryNoSsl.Checked ;  // June 2018
            end;
            MagMailQueue.Active := true ;
            if MagMailQueue.Active then
            begin
                info := 'Started Mail Queue OK' ;
                doQueue.Caption := 'Stop Queue'
            end;
        except
            info := 'Failed to Start Mail Queue - ' +  GetExceptMess (ExceptObject) ;
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
    if MagMailQueue.Active then
    begin
        MagMailQueue.Active := false ;
    end ;
    if NOT MagMailQueue.Active then
    begin
        info := '' ;
    end
    else
        info := 'Failed to Stop Mail Queue' ;
    MailLogAdd (info) ;
    LabelCount.Caption := info ;
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
        LabelQueue.Caption := 'Mail Queue: Items Queued ' + IntToStr (MagMailQueue.MailTotItems) ;
        if ViewQuWinFlag then ViewQuForm.UpdateList;
    end;
end;

procedure TDemoForm.MailLogAdd (info: string) ;
begin
    BuffInfos := BuffInfos + Info + CRLF ;
 {   try
        MailLog.Lines.Add (info) ;
    except
        LabelProg.Caption := 'Error writing to log' ;
    end ;    }
end ;

procedure TDemoForm.QuLogEvent (LogLevel: TMailLogLevel ; const Info: String) ;
begin
    if LogLevel = MLogLevelDiag then
        MailDiagAdd (Info)
    else
    begin
        MailLogAdd (Info) ;
        MailDiagAdd (Info)
    end;
end;

procedure TDemoForm.QuChangedEvent (Sender: TObject);
begin
    QuUpdateFlag := true ;
end;

procedure TDemoForm.MailDiagAdd (info: string) ;
begin
    if NOT DiagWinFlag then exit ;
    BuffDiags := BuffDiags + Info + CRLF ;
{    try
       DiagForm.TraceBox.Lines.Add (info) ;
    except
        LabelProg.Caption := 'Error writing to diag log' ;
    end ;   }
end ;

procedure TDemoForm.doExitClick(Sender: TObject);
begin
    DiagForm.Close ;
    Close ;
end;

procedure TDemoForm.doQueueClick(Sender: TObject);
begin
    if MagMailQueue.Active then
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
