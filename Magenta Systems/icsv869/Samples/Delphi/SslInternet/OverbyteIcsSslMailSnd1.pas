{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       How to use TSslSmtpCli component
Creation:     09 october 1997
Version:      V8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2022 by François PIETTE
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

Updates:
Aug 20, 2003 SSL support added by Arno Garrels <arno.garrels@gmx.de>.
Dec 29, 2007 A. Garrels reworked SSL, simplified verification code.
Jul 23, 2008 A. Garrels changed code in OnGetDate event handler to prepare
             code for Unicode
Aug 03, 2008 A. Garrels changed code in OnGetDate event handler to prepare
             code for Unicode again.
May 17, 2009 A.Garrels added correct casts to PAnsiChar in SslSmtpClientHeaderLine.
Apr 15, 2017 V2.11 FPiette removed compiler warnings for D10.2
Nov 25, 2020 V8.65 - Added XOAuth2 and OAuthBearer authentication support using
               TIcsRestEmail component for OAuth2, allows to access GMail account
               with security enabled and Microsoft Outlook mail.
               See comments at top of OverbyteIcsSslHttpRest.pas on how to
               set-up a Google/Microsoft OAuth2 application account.
               Note you have to specified ClientId and ClientPassword later
               before OAuth2 will work.
             Corrected SslContext so it works with TLS/1.2 and later.
             Changed defaults for implicit SSL connection which is standard now.
Mar 23, 2022 V8.69 - Added unit OverbyteIcsSslHttpOAuth for TIcsRestEmail
                        previously in HttpRest.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMailSnd1;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF DELPHI25_UP}
   {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsIniFiles,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsLogger,
  OverbyteIcsSmtpProt,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSslHttpOAuth;  { V8.69 }

const
  SslSmtpTestVersion    = 8.69;
  CopyRight : String    = ' SslMailSnd (c) 1997-2022 F. Piette V8.69 ';
  WM_SSL_RECONNECT      = WM_USER + 1;

type
  TSslSmtpTestForm = class(TForm)
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Subject: TLabel;
    Label4: TLabel;
    HostEdit: TEdit;
    FromEdit: TEdit;
    ToEdit: TEdit;
    SubjectEdit: TEdit;
    SignOnEdit: TEdit;
    PortEdit: TEdit;
    Label5: TLabel;
    AttachPanel: TPanel;
    Label6: TLabel;
    FileAttachMemo: TMemo;
    InfoPanel: TPanel;
    Label7: TLabel;
    ClearDisplayButton: TButton;
    ConnectButton: TButton;
    HeloButton: TButton;
    MailFromButton: TButton;
    RcptToButton: TButton;
    DataButton: TButton;
    AbortButton: TButton;
    QuitButton: TButton;
    MailButton: TButton;
    OpenButton: TButton;
    Label8: TLabel;
    SslSmtpClient: TSslSmtpCli;
    Label9: TLabel;
    Label10: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    AuthComboBox: TComboBox;
    PriorityComboBox: TComboBox;
    Label11: TLabel;
    EhloButton: TButton;
    AuthButton: TButton;
    Label12: TLabel;
    CcEdit: TEdit;
    Label13: TLabel;
    BccEdit: TEdit;
    AllInOneButton: TButton;
    TlsButton: TButton;
    VerifyPeerCheckBox: TCheckBox;
    SmtpSslModeCombobox: TComboBox;
    Label15: TLabel;
    KeyEdit: TEdit;
    CertEdit: TEdit;
    Label16: TLabel;
    Label17: TLabel;
    CaFileEdit: TEdit;
    CaPathEdit: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    PassPhraseEdit: TEdit;
    Label21: TLabel;
    SslContext1: TSslContext;
    IcsRestEmail1: TIcsRestEmail;
    procedure FormCreate(Sender: TObject);
    procedure ClearDisplayButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure SslSmtpClientRequestDone(Sender: TObject; RqType: TSmtpRequest;
      Error: Word);
    procedure HeloButtonClick(Sender: TObject);
    procedure MailFromButtonClick(Sender: TObject);
    procedure RcptToButtonClick(Sender: TObject);
    procedure DataButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure MailButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EhloButtonClick(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure AllInOneButtonClick(Sender: TObject);
    procedure SmtpClientDisplay(Sender: TObject; Msg: String);
    procedure SslSmtpClientResponse(Sender: TObject; Msg: String);
    procedure SslSmtpClientCommand(Sender: TObject; Msg: String);
    procedure SslSmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: Pointer; MaxLen: Integer; var More: Boolean);
    procedure SslSmtpClientHeaderLine(Sender: TObject; Msg: Pointer; Size: Integer);
    procedure SslSmtpClientBeforeFileOpen(Sender: TObject; Idx: Integer;
      FileName: String; var Action: TSmtpBeforeOpenFileAction);
    procedure TlsButtonClick(Sender: TObject);
    procedure SslSmtpClientSslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure SslSmtpClientSslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure IcsRestEmail1EmailNewToken(Sender: TObject);
    procedure IcsRestEmail1EmailProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure SslSmtpClientGetNewToken(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FAllInOneFlag : Boolean;
    FEhloCount    : Integer;
    procedure WMSslReconnect(var Msg: TMessage); message WM_SSL_RECONNECT;
    procedure Display(const Msg : String);
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure SetupOAuth2;                                                      { V8.65 }
  end;

var
  SslSmtpTestForm: TSslSmtpTestForm;

implementation

{$R *.DFM}
const
    SectionData         = 'Data';
    KeyHost             = 'HostName';
    KeyPort             = 'Port';
    KeyFrom             = 'From';
    KeyTo               = 'To';
    KeyCc               = 'Cc';
    KeyBcc              = 'Bcc';
    KeySubject          = 'Subject';
    KeySignOn           = 'SignOn';
    KeyUser             = 'UserName';
    KeyPass             = 'Password';
    KeyAuth             = 'Authentification';
    KeyPriority         = 'Priority';
    SectionWindow       = 'Window';
    KeyTop              = 'Top';
    KeyLeft             = 'Left';
    KeyWidth            = 'Width';
    KeyHeight           = 'Height';
    SectionFileAttach   = 'Files';
    KeyFileAttach       = 'File';
    SectionMsgMemo      = 'Message';
    KeyMsgMemo          = 'Msg';
    KeySmtpSslMode      = 'SmtpSslMode';
    KeySslVerifyPeer    = 'SslVerifyPeer';
    KeySslKey           = 'SslKey';
    KeySslCert          = 'SslCert';
    KeySslCAFile        = 'SslCaFile';
    KeySslCAPath        = 'SslCaPath';
    KeySslHostsAccepted = 'SslHostsAccepted';
    KeySslPassPhrase    = 'SslPassPhrase';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SaveStringsToIniFile(
    IniFile           : TIcsIniFile;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings);
var
    nItem   : Integer;
begin
    if (IniSection = '') or (IniKey = '') or (not Assigned(Strings)) then
        Exit;
    IniFile.EraseSection(IniSection);
    if Strings.Count <= 0 then
        IniFile.WriteString(IniSection, IniKey + 'EmptyFlag', 'Empty')
    else
        for nItem := 0 to Strings.Count - 1 do
            IniFile.WriteString(IniSection,
                                IniKey + IntToStr(nItem),
                                Strings.Strings[nItem]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return FALSE if non existant in IniFile                                   }
function LoadStringsFromIniFile(
    IniFile           : TIcsIniFile;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings) : Boolean;
var
    nItem   : Integer;
    I       : Integer;
    Buf     : String;
begin
    Result := TRUE;
    if (IniSection = '') or (IniKey = '') or  (not Assigned(Strings)) then
        Exit;
    Strings.Clear;
    if IniFile.ReadString(IniSection, IniKey + 'EmptyFlag', '') <> '' then
        Exit;
    IniFile.ReadSectionValues(IniSection, Strings);
    nItem := Strings.Count - 1;
    while nItem >= 0 do begin
        Buf := Strings.Strings[nItem];
        if CompareText(IniKey, Copy(Buf, 1, Length(IniKey))) <> 0 then
            Strings.Delete(nItem)
        else begin
            if not (Word(Buf[Length(IniKey) + 1]) in [Ord('0')..Ord('9')]) then
                Strings.Delete(nItem)
            else begin
                I := Pos('=', Buf);
                Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
            end;
        end;
        Dec(nItem);
    end;
    Result := (Strings.Count <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TSslSmtpTestForm.Display(const Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
    //IsConsole := AllocConsole;
    Application.OnException := ExceptionHandler;
    DisplayMemo.Clear;
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        HostEdit.Text    := IniFile.ReadString(SectionData, KeyHost,
                                               'localhost');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,
                                               '465');
        FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom,
                                               'first.last@company.com');
        ToEdit.Text      := IniFile.ReadString(SectionData, KeyTo,
                                               'john.doe@acme;tartempion@brol.fr');
        CcEdit.Text      := IniFile.ReadString(SectionData, KeyCc,
                                               '');
        BccEdit.Text     := IniFile.ReadString(SectionData, KeyBcc,
                                               'francois.piette@swing.be');
        SubjectEdit.Text := IniFile.ReadString(SectionData, KeySubject,
                                               'This is the message subject');
        SignOnEdit.Text  := IniFile.ReadString(SectionData, KeySignOn,
                                               'your name');
        UsernameEdit.Text :=  IniFile.ReadString(SectionData, KeyUser,
                                               'account name');
        PasswordEdit.Text :=  IniFile.ReadString(SectionData, KeyPass,
                                               'account password');
        AuthComboBox.ItemIndex :=  IniFile.ReadInteger(SectionData,
                                            KeyAuth, Ord(smtpAuthLogin));       { V8.65 SSL defaults }
        SmtpSslModeCombobox.ItemIndex := IniFile.ReadInteger(SectionData,
                                               KeySmtpSslMode, Ord(smtpTlsImplicit));
        VerifyPeerCheckBox.Checked := IniFile.ReadBool(SectionData, KeySslVerifyPeer, False);
        KeyEdit.Text :=  IniFile.ReadString(SectionData, KeySslKey,
                                               '01key.pem');
        PassPhraseEdit.Text := IniFile.ReadString(SectionData, KeySslPassPhrase,
                                               'password');
        CertEdit.Text :=  IniFile.ReadString(SectionData, KeySslCert,
                                               '01cert.pem');
        CAFileEdit.Text :=  IniFile.ReadString(SectionData, KeySslCaFile,
                                               'TrustedCABundle.pem');
        CAPathEdit.Text :=  IniFile.ReadString(SectionData, KeySslCaPath,
                                               'TrustedCaStore');
        PriorityComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyPriority, 2);

        if not LoadStringsFromIniFile(IniFile, SectionFileAttach,
                                      KeyFileAttach, FileAttachMemo.Lines) then
        FileAttachMemo.Text := 'ics_logo.gif' + #13#10 + 'fp_small.gif';
        if not LoadStringsFromIniFile(IniFile, SectionMsgMemo,
                                      KeyMsgMemo, MsgMemo.Lines) then
            MsgMemo.Text :=
            'This is the first line' + #13#10 +
            'Then the second one' + #13#10 +
            'The next one is empty' + #13#10 +
            '' + #13#10 +
            'The next one has only a single dot' + #13#10 +
            '.' + #13#10 +
            'Finally the last one' + #13#10;
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);

        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHost,             HostEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,             PortEdit.Text);
    IniFile.WriteString(SectionData, KeyFrom,             FromEdit.Text);
    IniFile.WriteString(SectionData, KeyTo,               ToEdit.Text);
    IniFile.WriteString(SectionData, KeyCc,               CcEdit.Text);
    IniFile.WriteString(SectionData, KeyBcc,              BccEdit.Text);
    IniFile.WriteString(SectionData, KeySubject,          SubjectEdit.Text);
    IniFile.WriteString(SectionData, KeySignOn,           SignOnEdit.Text);
    IniFile.WriteString(SectionData, KeyUser,             UsernameEdit.Text);
    IniFile.WriteString(SectionData, KeyPass,             PasswordEdit.Text);
    IniFile.WriteInteger(SectionData, KeyAuth,            AuthComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeySmtpSslMode,     SmtpSslModeCombobox.ItemIndex);
    IniFile.WriteBool(SectionData, KeySslVerifyPeer,      VerifyPeerCheckBox.Checked);
    IniFile.WriteString(SectionData, KeySslKey,           KeyEdit.Text);
    IniFile.WriteString(SectionData, KeySslPassPhrase,    PassPhraseEdit.Text);
    IniFile.WriteString(SectionData, KeySslCert,          CertEdit.Text);
    IniFile.WriteString(SectionData, KeySslCaFile,        CAFileEdit.Text);
    IniFile.WriteString(SectionData, KeySslCaPath,        CAPathEdit.Text);
    IniFile.WriteInteger(SectionData, KeyPriority,        PriorityComboBox.ItemIndex);
    SaveStringsToIniFile(IniFile, SectionFileAttach,
                         KeyFileAttach, FileAttachMemo.Lines);
    SaveStringsToIniFile(IniFile, SectionMsgMemo,
                         KeyMsgMemo, MsgMemo.Lines);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SmtpClientDisplay(Sender: TObject; Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientResponse(Sender: TObject; Msg: String);
begin
    Display('<' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientCommand(Sender: TObject; Msg: String);
begin
    Display('>' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientGetData(
    Sender  : TObject;
    LineNum : Integer;
    MsgLine : Pointer;
    MaxLen  : Integer;
    var More: Boolean);
begin
    if LineNum > MsgMemo.Lines.count then
        More := FALSE
    else
        { Truncate the line if too long (should wrap to next line) }
        StrPLCopy(PAnsiChar(MsgLine), AnsiString(MsgMemo.Lines[LineNum - 1]), MaxLen - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientHeaderLine(
    Sender : TObject;
    Msg    : Pointer;
    Size   : Integer);
begin
    { This demonstrates how to add a line to the message header              }
    { Just detect one of the header lines and add text at the end of this   }
    { line. Use #13#10 to form a new line                                   }
    { Here we check for the From: header line and add a Comments: line      }
    { Cast properly in order to call the right overload in D2009            }
    if (StrLen(PAnsiChar(Msg)) > 0) and
       (StrLIComp(PAnsiChar(Msg), PAnsiChar('From:'), 5) = 0) then
        StrCat(PAnsiChar(Msg), PAnsiChar(#13#10'Comments: This is a test'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
    Application.ShowException(E);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SetupOAuth2;                                         { V8.65 }
begin
  // hardcode your Google or Microsoft client application account details
    IcsRestEmail1.ClientId := '';
    IcsRestEmail1.ClientSecret := '';
    if IcsRestEmail1.ClientId = '' then begin
        Display('OAuth2 authentication requires an client application account');
        Exit;
    end;
    IcsRestEmail1.Clear;
     if Length(PasswordEdit.Text) > 32 then
        IcsRestEmail1.RefrToken := PasswordEdit.Text;  // ideally we'd save it separately
    if Pos('gmail', SslSmtpClient.Host) > 0 then
        IcsRestEmail1.RestEmailType := RestEmailGoogle
    else if (Pos('office365', SslSmtpClient.Host) > 0) or
                (Pos('outlook', SslSmtpClient.Host) > 0) then
        IcsRestEmail1.RestEmailType := RestEmailMSSmtp
    else
        Display('Can not use OAuth2 Authentication with this SMTP Server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Connect to the mail server }
procedure TSslSmtpTestForm.ConnectButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := False;
    SslSmtpClient.Host         := HostEdit.Text;
    SslSmtpClient.Port         := PortEdit.Text;
    SslSmtpClient.HdrPriority  := TSmtpPriority(PriorityComboBox.ItemIndex);
    SslSmtpClient.AuthType     := TSmtpAuthType(AuthComboBox.ItemIndex);        { V8.65 }
    SslSmtpClient.Username     := UsernameEdit.Text;                            { V8.65 }
    SslSmtpClient.Password     := PasswordEdit.Text;                            { V8.65 }
    if SslSmtpClient.AuthType in [smtpAuthXOAuth2, smtpAuthOAuthBearer] then
        SetupOAuth2;                                                            { V8.65 }
    SslSmtpClient.SslType      := TSmtpSslType(SmtpSslModeComboBox.ItemIndex);
    if SslSmtpClient.SslType <> smtpTlsNone then begin
        SslContext1.SslPrivKeyFile := KeyEdit.Text;
        SslContext1.SslPassPhrase  := PassPhraseEdit.Text;
        SslContext1.SslCertFile    := CertEdit.Text;
        SslContext1.SslCAFile      := CAFileEdit.Text;
        SslContext1.SslCAPath      := CAPathEdit.Text;
        SslContext1.SslVerifyPeer  := VerifyPeerCheckBox.Checked;
        if SslContext1.SslVerifyPeer then begin
            SslSmtpClient.OnSslVerifyPeer := SslSmtpClientSslVerifyPeer;
            SslSmtpClient.OnSslHandshakeDone := SslSmtpClientSslHandshakeDone;
        end
        else begin
            SslSmtpClient.OnSslVerifyPeer := nil;
            SslSmtpClient.OnSslHandshakeDone := nil;
        end;
    end;
    SslSmtpClient.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send HELO command with our local identification }
procedure TSslSmtpTestForm.HeloButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.SignOn          := SignOnEdit.Text;
    SslSmtpClient.Helo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.EhloButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.SignOn          := SignOnEdit.Text;
    SslSmtpClient.Ehlo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.AuthButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.Username        := UsernameEdit.Text;
    SslSmtpClient.Password        := PasswordEdit.Text;
    SslSmtpClient.AuthType        := TSmtpAuthType(AuthComboBox.ItemIndex);
    SslSmtpClient.Auth;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ If smtpAuthNone is seleted then Open combines methods Connect and Helo.   }
{  If any other authentication type is selected then Open combines methods  }
{  Connect, Ehlo and Auth.                                                  }
procedure TSslSmtpTestForm.OpenButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    //SslSmtpClient.MailMessage     := MsgMemo.Lines;
    SslSmtpClient.Host            := HostEdit.Text;
    SslSmtpClient.Port            := PortEdit.Text;
    SslSmtpClient.SignOn          := SignOnEdit.Text;
    SslSmtpClient.Username        := UsernameEdit.Text;
    SslSmtpClient.Password        := PasswordEdit.Text;
    SslSmtpClient.AuthType        := TSmtpAuthType(AuthComboBox.ItemIndex);
    if SslSmtpClient.AuthType in [smtpAuthXOAuth2, smtpAuthOAuthBearer] then
        SetupOAuth2;                                                            { V8.65 }
    SslSmtpClient.HdrPriority     := TSmtpPriority(PriorityComboBox.ItemIndex);
    SslSmtpClient.SslType         := TSmtpSslType(SmtpSslModeComboBox.ItemIndex);
    if SslSmtpClient.SslType <> smtpTlsNone then begin
        SslContext1.SslPrivKeyFile := KeyEdit.Text;
        SslContext1.SslPassPhrase  := PassPhraseEdit.Text;
        SslContext1.SslCertFile    := CertEdit.Text;
        SslContext1.SslCAFile      := CAFileEdit.Text;
        SslContext1.SslCAPath      := CAPathEdit.Text;
        SslContext1.SslVerifyPeer  := VerifyPeerCheckBox.Checked;
        if SslContext1.SslVerifyPeer then begin
            SslSmtpClient.OnSslVerifyPeer := SslSmtpClientSslVerifyPeer;
            SslSmtpClient.OnSslHandshakeDone := SslSmtpClientSslHandshakeDone;
        end
        else begin
            SslSmtpClient.OnSslVerifyPeer := nil;
            SslSmtpClient.OnSslHandshakeDone := nil;
        end;
    end;
    SslSmtpClient.Open;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send originator }
procedure TSslSmtpTestForm.MailFromButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.FromName        := FromEdit.Text;
    SslSmtpClient.MailFrom;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send recipients }
procedure TSslSmtpTestForm.RcptToButtonClick(Sender: TObject);
begin
    FAllInOneFlag := FALSE;
    SslSmtpClient.RcptName.Clear;
    SslSmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SslSmtpClient.RcptTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send text and attached files to mail server }
procedure TSslSmtpTestForm.DataButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.RcptName.Clear;
    SslSmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SslSmtpClient.HdrFrom         := FromEdit.Text;
    SslSmtpClient.HdrTo           := ToEdit.Text;
    SslSmtpClient.HdrCc           := CcEdit.Text;
    SslSmtpClient.HdrSubject      := SubjectEdit.Text;
    SslSmtpClient.EmailFiles      := FileAttachMemo.Lines;
    SslSmtpClient.SendMode        := smtpCopyToStream;
    SslSmtpClient.Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MailFrom, RcptTo and Data methods combined }
procedure TSslSmtpTestForm.MailButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.RcptName.Clear;
    SslSmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SslSmtpClient.HdrFrom         := FromEdit.Text;
    SslSmtpClient.HdrTo           := ToEdit.Text;
    SslSmtpClient.HdrCc           := CcEdit.Text;
    SslSmtpClient.HdrSubject      := SubjectEdit.Text;
    SslSmtpClient.SignOn          := SignOnEdit.Text;
    SslSmtpClient.FromName        := FromEdit.Text;
    SslSmtpClient.EmailFiles      := FileAttachMemo.Lines;
    SslSmtpClient.Host            := HostEdit.Text;
    SslSmtpClient.Port            := PortEdit.Text;
    //SslSmtpClient.SendMode        := smtpCopyToStream;
    SslSmtpClient.Mail;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.QuitButtonClick(Sender: TObject);
begin
    FAllInOneFlag                 := FALSE;
    SslSmtpClient.Quit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.AbortButtonClick(Sender: TObject);
begin
    FAllInOneFlag := FALSE;
    SslSmtpClient.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientRequestDone(
    Sender : TObject;
    RqType : TSmtpRequest;
    Error  : Word);
begin
    { For every operation, we display the status }
    if (Error > 0) and  (Error < 10000) then
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                    ' Error='+ SslSmtpClient.ErrorMessage)
    else
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                            ' Error='+ IntToStr(Error));
    { Check if the user has asked for "All-In-One" demo }
    if not FAllInOneFlag then
        Exit;             { No, nothing more to do here }
    { We are in "All-In-One" demo, start next operation }
    { But first check if previous one was OK            }
    if Error <> 0 then begin
        FAllInOneFlag := FALSE;   { Terminate All-In-One demo }
        Display('Error, stoped All-In-One demo');
        Exit;
    end;
    case RqType of
    smtpConnect:  begin
                      if SslSmtpClient.AuthType = smtpAuthNone then
                          SslSmtpClient.Helo
                      else
                          SslSmtpClient.Ehlo;
                  end;
    smtpHelo:     SslSmtpClient.MailFrom;
    smtpEhlo: if SslSmtpClient.SslType = smtpTlsExplicit then begin
                  Inc(FEhloCount);
                  if FEhloCount = 1 then
                      SslSmtpClient.StartTls
                  else if FEhloCount > 1 then
                      SslSmtpClient.Auth;
              end
              else
                  SslSmtpClient.Auth;
    smtpStartTls: SslSmtpClient.Ehlo; // We need to re-issue the Ehlo command
    smtpAuth:     SslSmtpClient.MailFrom;
    smtpMailFrom: SslSmtpClient.RcptTo;
    smtpRcptTo:   SslSmtpClient.Data;
    smtpData:     SslSmtpClient.Quit;
    smtpQuit:     Display('All-In-One done !');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.AllInOneButtonClick(Sender: TObject);
begin
    if SslSmtpClient.Connected then begin
        MessageBeep(MB_OK);
        Display('All-In-One demo start in connected state.');
        Display('Please quit or abort the connection first.');
        Exit;
    end;

    FAllInOneFlag               := TRUE;
    FEhloCount                  := 0;
    { Initialize all SMTP component properties from our GUI }
    SslSmtpClient.Host          := HostEdit.Text;
    SslSmtpClient.Port          := PortEdit.Text;
    SslSmtpClient.SignOn        := SignOnEdit.Text;
    SslSmtpClient.FromName      := FromEdit.Text;
    SslSmtpClient.HdrFrom       := FromEdit.Text;
    SslSmtpClient.HdrTo         := ToEdit.Text;
    SslSmtpClient.HdrCc         := CcEdit.Text;
    SslSmtpClient.HdrSubject    := SubjectEdit.Text; { + #13#10#9 + ' Testing continuation line !'};
    SslSmtpClient.EmailFiles    := FileAttachMemo.Lines;
    SslSmtpClient.AuthType      := TSmtpAuthType(AuthComboBox.ItemIndex);
    if SslSmtpClient.AuthType in [smtpAuthXOAuth2, smtpAuthOAuthBearer] then
        SetupOAuth2;                                                            { V8.65 }
    SslSmtpClient.SslType       := TSmtpSslType(SmtpSslModeComboBox.ItemIndex);
    if SslSmtpClient.SslType <> smtpTlsNone then begin
        SslContext1.SslPrivKeyFile := KeyEdit.Text;
        SslContext1.SslPassPhrase  := PassPhraseEdit.Text;
        SslContext1.SslCertFile    := CertEdit.Text;
        SslContext1.SslCAFile      := CAFileEdit.Text;
        SslContext1.SslCAPath      := CAPathEdit.Text;
        SslContext1.SslVerifyPeer  := VerifyPeerCheckBox.Checked;
        if SslContext1.SslVerifyPeer then begin
            SslSmtpClient.OnSslVerifyPeer := SslSmtpClientSslVerifyPeer;
            SslSmtpClient.OnSslHandshakeDone := SslSmtpClientSslHandshakeDone;
        end
        else begin
            SslSmtpClient.OnSslVerifyPeer := nil;
            SslSmtpClient.OnSslHandshakeDone := nil;
        end;
    end;

    SslSmtpClient.Username       := UsernameEdit.Text;
    SslSmtpClient.Password       := PasswordEdit.Text;
    SslSmtpClient.HdrPriority    := TSmtpPriority(PriorityComboBox.ItemIndex);
    { Recipient list is computed from To, Cc and Bcc fields }
    SslSmtpClient.RcptName.Clear;
    SslSmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    Display('Connecting to SMTP server...');
    { Start first operation to do to send an email          }
    { Next operations are started from OnRequestDone event  }
    SslSmtpClient.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientBeforeFileOpen(Sender: TObject;
  Idx: Integer; FileName: String; var Action: TSmtpBeforeOpenFileAction);
begin
    Action := smtpBeforeOpenFileNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.TlsButtonClick(Sender: TObject);
begin
    FAllInOneFlag := FALSE;
    SslSmtpClient.StartTls;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.WMSslReconnect(var Msg: TMessage);
begin
    OpenButtonClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientSslVerifyPeer(Sender: TObject;
    var Ok  : Integer;
    Cert    : TX509Base);
begin
    Display(
            'Received certificate - Depth: ' +
            IntToStr(Cert.VerifyDepth)+ #13#10 +
            'Subject: "' + Cert.SubjectOneLine + '"'#13#10 +
            'Issuer:  "' + Cert.IssuerOneLine + '"'#13#10  +
            'Verify result: ' + Cert.VerifyErrMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.SslSmtpClientSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
begin
    if (ErrCode = 0) then begin
        if PeerCert.PostConnectionCheck(SslSmtpClient.Host) then
            Display('! Post connection check ok')
        else begin
            Display('! Post connection check **failed** - ' +
                                  'Hostname "' + SslSmtpClient.Host +
                                  '" not found in peer certificate');
            Disconnect := TRUE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ called by SMTP component to when it needs an OAuth2 token }
procedure TSslSmtpTestForm.SslSmtpClientGetNewToken(Sender: TObject);           { V8.65 }
begin
    if NOT IcsRestEmail1.GetNewToken(True) then   // allow interaction, waits for broweser window to be completed
        Display('Failed to get OAuth2 Bearer Token')
    else begin
        if (Pos (IcsRestEmail1.NewAccEmail, SslSmtpClient.UserName) = 0) and
                                              (IcsRestEmail1.NewAccEmail <> '') then
            Display('OAuth2 Token for Wrong Account, Expected: ' +
                 SslSmtpClient.Username + ', Got: ' + IcsRestEmail1.NewAccEmail)
        else  begin
            SslSmtpClient.OAuthToken := IcsRestEmail1.AccToken;
            SslSmtpClient.TokenExpireDT := IcsRestEmail1.AccExpireDT;
            Display('Got New OAuth2 Bearer Token OK');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ called by TIcsRestEmail when it has a new token we should save }
procedure TSslSmtpTestForm.IcsRestEmail1EmailNewToken(Sender: TObject);         { V8.65 }
begin
    if IcsRestEmail1.RefrToken <> '' then begin
        PasswordEdit.Text := IcsRestEmail1.RefrToken;
        Display('Saved OAuth2 Refresh Token');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslSmtpTestForm.IcsRestEmail1EmailProg(Sender: TObject;              { V8.65 }
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

