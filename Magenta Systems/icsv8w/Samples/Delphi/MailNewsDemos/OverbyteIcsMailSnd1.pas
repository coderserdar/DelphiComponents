{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       How to use TSmtpCli component
Creation:     09 october 1997
Version:      6.10
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2011 by François PIETTE
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

Updates:
Oct 26, 1997  V1.00 Released
Jan 10, 1998  V1.01 Added a Port property
Feb 15, 1998  V1.02 Added file attachement support
Mar 06, 1998  V1.03 Check for DisplayMemo overflow (100 lines allowed)
Aug 03, 1998  V2.00 Revised for new asynchronous SMTP component version
Jul 26, 2001  V2.01 Added authentification
Sep 07, 2002  V2.02 Added Cc and Bcc fields.
              Added AllInOne demo to show how to "chain" several operations
              using OnRequest done, avoiding any wait loop. This is how event
              driven operation has to be done.
Sep 15, 2002  V2.02 Corrected typo error in BuildRcptList where CcEdi was used
              where ToEdit should.
              Thanks to konstantinos.Kokkorogiannis@diala.greenpeace.org
Apr 08, 2003  V2.03 Arno Garrels <arno.garrels@gmx.de> made some useful
              changes:
              Search for 04/06/2003 Property AuthComboBox.ItemIndex removed
              from dfm, caused error message with older Delphi versions.
              AuthComboBox.Items "smtpAuthAutoSelect" added.
Apr 19, 2003  V2.04 Replaced BuildRcptList by the new RcptNameAdd component
              method.
May 05, 2003  V2.06 Changed the way data is saved to INI file to allow bigger
              messages.
Aug 23, 2004  V2.07 Removed unused units
Mar 12, 2005  V2.08 Added CinfirmCheckbox
Mar 19, 2006  V6.00 Demo ported from ICS-V5 to ICS-V6
Oct 29, 2006  V6.01 Fixed memory leak in PrepareEMail
              Added compiler switches and DELPHI7_UP check.
              Added D2006 memory leak detection
Nov 05, 2006  V6.02 Fixed typo error in AuthComboBox. Added NTLM.
Apr 25, 2008  V6.03 A.Garrels made some changes to prepare the code for Unicode.
              Added button "Send To File" and assigned event OnAttachContentTypeEh.
Jul 23, 2008  V6.04 A. Garrels changed code in OnGetDate event handler to prepare
              code for Unicode.
Aug 03, 2008  V6.05 A. Garrels changed code in OnGetDate event handler to prepare
              code for Unicode again
Jan 17, 2009  V6.06 A. Garrels added a progress bar and RFC-1870 SIZE extension.
May 10, 2009  V6.07 A. Garrels added charset and code page properties which
              makes it easy to play with and test the new features.
May 17, 2009  V6.08 A.Garrels added correct casts to PAnsiChar in SmtpClientHeaderLine.
Feb 15, 2011  V6.09 Arno added proxy demo.
Mar 01, 2011  V6.10 Arno enable/disable the proxy-controls depending on proxy
              setting.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMailSnd1;

{$I Include\OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Forms,
  Dialogs, ComCtrls, Contnrs,
  OverbyteIcsCharsetComboBox,
  OverbyteIcsIniFiles,
  OverbyteIcsCharsetUtils,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsSmtpProt;

const
    SmtpTestVersion    = 6.09;
    CopyRight : String = ' MailSnd (c) 1997-2011 F. Piette V6.09 ';

type
  TSmtpTestForm = class(TForm)
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    ToolsPanel: TPanel;
    Label5: TLabel;
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
    EhloButton: TButton;
    AuthButton: TButton;
    AllInOneButton: TButton;
    AttachPanel: TPanel;
    Label6: TLabel;
    FileAttachMemo: TMemo;
    InfoPanel: TPanel;
    Label7: TLabel;
    SmtpClient: TSmtpCli;
    SendToFileButton: TButton;
    MsgSizeButton: TButton;
    ProgressBar1: TProgressBar;
    ProgressCheckBox: TCheckBox;
    MailFromSIZEButton: TButton;
    SettingsPageControl: TPageControl;
    BasicSettingsTabSheet: TTabSheet;
    Label1: TLabel;
    HostEdit: TEdit;
    Label4: TLabel;
    PortEdit: TEdit;
    Label2: TLabel;
    FromEdit: TEdit;
    ToEdit: TEdit;
    Label3: TLabel;
    Label12: TLabel;
    CcEdit: TEdit;
    Label13: TLabel;
    BccEdit: TEdit;
    Subject: TLabel;
    SubjectEdit: TEdit;
    Label8: TLabel;
    SignOnEdit: TEdit;
    Label9: TLabel;
    UsernameEdit: TEdit;
    Label10: TLabel;
    PasswordEdit: TEdit;
    Label11: TLabel;
    AuthComboBox: TComboBox;
    Label14: TLabel;
    PriorityComboBox: TComboBox;
    ConfirmCheckBox: TCheckBox;
    CharsetSettingsTabSheet: TTabSheet;
    UseMailMessageCheckBox: TCheckBox;
    CharSetPanel: TPanel;
    Label15: TLabel;
    CharsetTestButton: TButton;
    ConvertToCharsetCheckBox: TCheckBox;
    Allow8BitCheckBox: TCheckBox;
    DefEnc: TLabel;
    DefEncodingComboBox: TComboBox;
    FoldHeadersCheckBox: TCheckBox;
    WrapTextCheckBox: TCheckBox;
    Label16: TLabel;
    WrapAtEdit: TEdit;
    Label17: TLabel;
    CharsetInfoLabel1: TLabel;
    ToggleCsViewButton: TButton;
    IcsCharsetComboBox1: TIcsCharsetComboBox;
    ProxyTabsheet: TTabSheet;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    ProxyPasswordEdit: TEdit;
    ProxyUserEdit: TEdit;
    ProxyTypeComboBox: TComboBox;
    ProxyHttpAuthTypeComboBox: TComboBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ClearDisplayButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure SmtpClientRequestDone(Sender: TObject; RqType: TSmtpRequest;
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
    procedure SmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: Pointer; MaxLen: Integer; var More: Boolean);
    procedure SmtpClientHeaderLine(Sender: TObject; Msg: Pointer;
      Size: Integer);
    procedure SendToFileButtonClick(Sender: TObject);
    procedure SmtpClientAttachContentTypeEh(Sender: TObject;
      FileNumber: Integer; var FileName, ContentType: string;
      var AttEncoding: TSmtpEncoding);
    procedure MsgSizeButtonClick(Sender: TObject);
    procedure SmtpClientMessageDataSent(Sender: TObject; Size: Integer);
    procedure MailFromSIZEButtonClick(Sender: TObject);
    procedure CharsetTestButtonClick(Sender: TObject);
    procedure ToggleCsViewButtonClick(Sender: TObject);
    procedure IcsCharsetComboBox1Change(Sender: TObject);
    procedure ProxyTypeComboBoxCloseUp(Sender: TObject);
    procedure ProxyHttpAuthTypeComboBoxCloseUp(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FAllInOneFlag : Boolean;
    FByteCount    : Integer;
    FCounter      : Integer;
    procedure Display(const Msg : String);
    procedure ExceptionHandler(Sender: TObject; E: Exception);
    procedure SmtpClientBeforeOutStreamFree(Sender: TObject);
    procedure PrepareProgressBar;
  end;

var
  SmtpTestForm: TSmtpTestForm;

implementation

{$R *.dfm}

uses
    TypInfo;

const
    SectionData       = 'Data';
    KeyHost           = 'HostName';
    KeyPort           = 'Port';
    KeyFrom           = 'From';
    KeyTo             = 'To';
    KeyCc             = 'Cc';
    KeyBcc            = 'Bcc';
    KeySubject        = 'Subject';
    KeySignOn         = 'SignOn';
    KeyUser           = 'UserName';
    KeyPass           = 'Password';
    KeyAuth           = 'Authentification';
    KeyPriority       = 'Priority';
    KeyConfirm        = 'Confirm';
    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    SectionFileAttach = 'Files';
    KeyFileAttach     = 'File';
    SectionMsgMemo    = 'Message';
    KeyMsgMemo        = 'Msg';
    KeyProgress       = 'Progress';
    KeyConvertToCharset = 'ConvertToCharset';
    KeyUseOnGetData   = 'UseOnGetData';
    KeyAllow8Bit      = 'Allow8bit';
    KeyFoldHeaders    = 'FoldHeaders';
    KeyWrapText       = 'WrapText';
    KeyWrapAt         = 'WrapAt';
    KeyCharSet        = 'Charset';
    KeyDefTransEnc    = 'DefaultTransferEncoding';
    KeyProxyHost      = 'ProxyHost';
    KeyProxyPort      = 'ProxyPort';
    KeyProxyType      = 'ProxyType';
    KeyProxyHttpAuth  = 'ProxyHttpAuth';
    KeyProxyUser      = 'ProxyUser';
    KeyProxyPassword  = 'ProxyPassword';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TSmtpTestForm.Display(const Msg : String);
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
procedure TSmtpTestForm.PrepareProgressBar;
var
    OldOnRequestDone : TSmtpRequestDone;
begin
    SmtpClient.MessageSize := 0;
    if ProgressCheckBox.Checked then begin
        OldOnRequestDone := SmtpClient.OnRequestDone;
        { Let's turn off OnRequestDone temporarily               }
        SmtpClient.OnRequestDone := nil;
        try
            { Precompute message size, this might take a while,  }
            { base64 attachment size however is just computed.   }
            { Message size is written to property MessageSize.   }
            { CalcMsgSizeSync is a synchronous (blocking) method }
            { Do not expect 100% exact values returned by this   }
            { function, though they are pretty exact.            }
            SmtpClient.CalcMsgSizeSync;
        finally
            SmtpClient.OnRequestDone := OldOnRequestDone;
        end;
        ProgressBar1.Max := SmtpClient.MessageSize;
        FByteCount       := 0;
        FCounter         := 0;
        ProgressBar1.Min := 0;
        SmtpClient.OnMessageDataSent := SmtpClientMessageDataSent;
    end
    else
        SmtpClient.OnMessageDataSent := nil;
    ProgressBar1.Position := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormCreate(Sender: TObject);
begin
    Application.OnException := ExceptionHandler;
    DisplayMemo.Clear;
    FIniFileName := GetIcsIniFileName;
    IcsCharsetComboBox1.IncludeList := IcsCharsetComboBox1.IncludeList + [UTF_7];
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormShow(Sender: TObject);
var
    IniFile    : TIcsIniFile;
    Pt         : TSmtpProxyType;
    Htat       : THttpTunnelAuthType;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        Application.HintHidePause := MaxInt;
        SettingsPageControl.ActivePageIndex := 0;
        CharsetInfoLabel1.Caption := '';

        { Fill some combo boxes }
        ProxyTypeComboBox.Items.Clear;
        ProxyTypeComboBox.Style := csDropDownList;
        for Pt := Low(TSmtpProxyType) to high(TSmtpProxyType) do
            ProxyTypeComboBox.Items.Add(GetEnumName(Typeinfo(TSmtpProxyType), Ord(Pt)));
        ProxyTypeComboBox.ItemIndex := 0;
        ProxyHttpAuthTypeComboBox.Items.Clear;
        ProxyHttpAuthTypeComboBox.Style := csDropDownList;
        for Htat := Low(THttpTunnelAuthType) to high(THttpTunnelAuthType) do
            ProxyHttpAuthTypeComboBox.Items.Add(GetEnumName(Typeinfo(THttpTunnelAuthType), Ord(Htat)));
        ProxyHttpAuthTypeComboBox.ItemIndex := 0;

        IniFile := TIcsIniFile.Create(FIniFileName);
        HostEdit.Text    := IniFile.ReadString(SectionData, KeyHost,
                                               'localhost');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,
                                               'smtp');
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
        UsernameEdit.Text := IniFile.ReadString(SectionData, KeyUser,
                                               'account name');
        PasswordEdit.Text := IniFile.ReadString(SectionData, KeyPass,
                                               'account password');
        AuthComboBox.ItemIndex     := IniFile.ReadInteger(SectionData, KeyAuth, 0);
        PriorityComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyPriority, 2);
        ConfirmCheckBox.Checked    := Boolean(IniFile.ReadInteger(SectionData, KeyConfirm, 0));
        ProgressCheckBox.Checked   := IniFile.ReadBool(SectionData, KeyProgress, False);
        ConvertToCharsetCheckBox.Checked := IniFile.ReadBool(SectionData, KeyConvertToCharset, False);
        {$IFDEF UNICODE}
            { In Delphi 2009 we have to convert from UTF-16 always, property }
            { ConvertToCharset is ignored in 2009 and better.                }
            ConvertToCharsetCheckBox.Visible := FALSE;
        {$ENDIF}
        UseMailMessageCheckBox.Checked := IniFile.ReadBool(SectionData, KeyUseOnGetData, False);
        Allow8BitCheckBox.Checked := IniFile.ReadBool(SectionData, KeyAllow8bit, True);
        FoldHeadersCheckBox.Checked := IniFile.ReadBool(SectionData, KeyFoldHeaders, False);
        WrapTextCheckBox.Checked := IniFile.ReadBool(SectionData, KeyWrapText, False);
        WrapAtEdit.Text := IniFile.ReadString(SectionData, KeyWrapAt, '76');
        IcsCharsetComboBox1.Charset := IniFile.ReadString(SectionData, KeyCharset, SmtpClient.CharSet);
        DefEncodingComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyDefTransEnc, 0);
        if not IniFile.ReadStrings(SectionFileAttach,
                                   KeyFileAttach, FileAttachMemo.Lines) then
            FileAttachMemo.Text := ExtractFilePath(ParamStr(0)) +
                                   'ics_logo.gif' + #13#10 +
                                    ExtractFilePath(ParamStr(0)) + 'fp_small.gif';
        if not IniFile.ReadStrings(SectionMsgMemo,
                                   KeyMsgMemo, MsgMemo.Lines) then
            MsgMemo.Text :=
            'This is the first line' + #13#10 +
            'Then the second one' + #13#10 +
            'The next one is empty' + #13#10 +
            '' + #13#10 +
            'The next one has only a single dot' + #13#10 +
            '.' + #13#10 +
            'Finally the last one' + #13#10;

        ProxyTypeComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyProxyType, 0);
        ProxyHttpAuthTypeComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyProxyHttpAuth, 0);
        ProxyHostEdit.Text := IniFile.ReadString(SectionData, KeyProxyHost, '192.168.1.1');
        ProxyPortEdit.Text := IniFile.ReadString(SectionData, KeyProxyPort, '8080');
        ProxyUserEdit.Text := IniFile.ReadString(SectionData, KeyProxyUser, 'ics');
        ProxyPasswordEdit.Text := IniFile.ReadString(SectionData, KeyProxyPassword, 'ics');
        ProxyTypeComboBoxCloseUp(ProxyTypeComboBox);

        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHost,      HostEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData, KeyFrom,      FromEdit.Text);
    IniFile.WriteString(SectionData, KeyTo,        ToEdit.Text);
    IniFile.WriteString(SectionData, KeyCc,        CcEdit.Text);
    IniFile.WriteString(SectionData, KeyBcc,       BccEdit.Text);
    IniFile.WriteString(SectionData, KeySubject,   SubjectEdit.Text);
    IniFile.WriteString(SectionData, KeySignOn,    SignOnEdit.Text);
    IniFile.WriteString(SectionData, KeyUser,      UsernameEdit.Text);
    IniFile.WriteString(SectionData, KeyPass,      PasswordEdit.Text);
    IniFile.WriteInteger(SectionData, KeyAuth,     AuthComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeyPriority, PriorityComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeyConfirm,  Ord(ConfirmCheckBox.Checked));
    IniFile.WriteBool(SectionData,    KeyProgress, ProgressCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyConvertToCharset, ConvertToCharsetCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyUseOnGetData, UseMailMessageCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyAllow8bit, Allow8BitCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyFoldHeaders, FoldHeadersCheckBox.Checked);
    IniFile.WriteBool(SectionData, KeyWrapText, WrapTextCheckBox.Checked);
    IniFile.WriteString(SectionData, KeyWrapAt, WrapAtEdit.Text);
    IniFile.WriteString(SectionData, KeyCharset, IcsCharsetComboBox1.CharSet);
    IniFile.WriteInteger(SectionData, KeyDefTransEnc, DefEncodingComboBox.ItemIndex);
    IniFile.WriteStrings(SectionFileAttach, KeyFileAttach, FileAttachMemo.Lines);
    IniFile.WriteStrings(SectionMsgMemo, KeyMsgMemo, MsgMemo.Lines);
    IniFile.WriteInteger(SectionData, KeyProxyType, ProxyTypeComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeyProxyHttpAuth, ProxyHttpAuthTypeComboBox.ItemIndex);
    IniFile.WriteString(SectionData, KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData, KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteString(SectionData, KeyProxyUser, ProxyUserEdit.Text);
    IniFile.WriteString(SectionData, KeyProxyPassword, ProxyPasswordEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientAttachContentTypeEh(Sender: TObject;
  FileNumber: Integer; var FileName, ContentType: string;
  var AttEncoding: TSmtpEncoding);
begin
    AttEncoding := smtpEncodeBase64;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientBeforeOutStreamFree(Sender: TObject);
begin
    TSmtpCli(Sender).SendMode := smtpToSocket;
    TSmtpCli(Sender).OnBeforeOutStreamFree := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientDisplay(Sender: TObject; Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientGetData(
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
procedure TSmtpTestForm.SmtpClientHeaderLine(
    Sender : TObject;
    Msg    : Pointer;
    Size   : Integer);
begin
    { This demonstrates how to add a line to the message header             }
    { Just detect one of the header lines and add text at the end of this   }
    { line. Use #13#10 to form a new line.                                  }
    { Here we check for the X-Mailer: header line and add a Comments: line  }
    { Cast properly in order to call the right overload in D2009. Note that }
    { long header lines can be folded, inserting into folded lines leads    }
    { to a brocken header.                                                  }
    if (StrLen(PAnsiChar(Msg)) > 0) and
       (StrLIComp(PAnsiChar(Msg), PAnsiChar('X-Mailer:'), 9) = 0) then
        StrCat(PAnsiChar(Msg), PAnsiChar(#13#10'Comments: This is a test'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientMessageDataSent(
    Sender  : TObject;
    Size    : Integer);
begin
    Inc(FByteCount, Size);
    Inc(FCounter);
    if FCounter mod 200 = 0 then
        ProgressBar1.Position := FByteCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
    Application.ShowException(E);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Connect to the mail server }
procedure TSmtpTestForm.ConnectButtonClick(Sender: TObject);
begin
    ProgressBar1.Position         := 0;
    FAllInOneFlag                 := FALSE;
    SmtpClient.Host               := HostEdit.Text;
    SmtpClient.Port               := PortEdit.Text;
    SmtpClient.ProxyServer        := ProxyHostEdit.Text;
    SmtpClient.ProxyPort          := ProxyPortEdit.Text;
    SmtpClient.ProxyUserCode      := ProxyUserEdit.Text;
    SmtpClient.ProxyPassword      := ProxyPasswordEdit.Text;
    SmtpClient.ProxyType          := TSmtpProxyType(ProxyTypeComboBox.ItemIndex);
    SmtpClient.ProxyHttpAuthType  := THttpTunnelAuthType(ProxyHttpAuthTypeComboBox.ItemIndex);
    if (SmtpClient.ProxyType <> smtpNoProxy) and
      (SmtpClient.ProxyServer = '') then
        raise Exception.Create('Proxy host is not assigned');
    SmtpClient.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send HELO command with our local identification }
procedure TSmtpTestForm.HeloButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.Helo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.EhloButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.EHlo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AuthButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.Username        := UsernameEdit.Text;
    SmtpClient.Password        := PasswordEdit.Text;
    SmtpClient.AuthType        := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.Auth;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SendToFileButtonClick(Sender: TObject);
begin
    { Assign property MailMessage and unassign OnGetData if you need }
    { automatic encoding and line wrapping                           }
    if UseMailMessageCheckBox.Checked then begin
        SmtpClient.MailMessage        := MsgMemo.Lines;
        SmtpClient.OnGetData          := nil;
        SmtpClient.WrapMsgMaxLineLen  := StrToIntDef(WrapAtEdit.Text, 76);
        SmtpClient.WrapMessageText    := WrapTextCheckBox.Checked;
    end
    else begin
        SmtpClient.OnGetData   := SmtpClientGetData;
        SmtpClient.MailMessage.Clear;
    end;
    FAllInOneFlag                 := FALSE;
    SmtpClient.CharSet            := IcsCharsetComboBox1.Charset;
    SmtpClient.ConvertToCharset   := ConvertToCharsetCheckBox.Checked;
    SmtpClient.DefaultEncoding    := TSmtpDefaultEncoding(DefEncodingComboBox.ItemIndex);
    SmtpClient.Allow8bitChars     := Allow8BitCheckBox.Checked;
    SmtpClient.FoldHeaders        := FoldHeadersCheckBox.Checked;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.Text);
    SmtpClient.HdrFrom            := FromEdit.Text;
    SmtpClient.HdrTo              := ToEdit.Text;
    SmtpClient.HdrCc              := CcEdit.Text;
    SmtpClient.HdrSubject         := SubjectEdit.Text;
    SmtpClient.HdrPriority        := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.EmailFiles         := FileAttachMemo.Lines;
    SmtpClient.ConfirmReceipt     := ConfirmCheckBox.Checked;
    PrepareProgressBar;
    with TOpenDialog.Create(nil) do
    try
        if Execute and (Filename <> '') then begin
            SmtpClient.OnBeforeOutStreamFree := SmtpClientBeforeOutStreamFree;
            Update;
            SmtpClient.SendMode := smtpToStream;
            SmtpClient.SendToFile(Filename); // It's async!
        end;
    finally
        Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ If smtpAuthNone is seleted then Open combines methods Connect and Helo.   }
{  If any other authentication type is selected then Open combines methods  }
{  Connect, Ehlo and Auth.                                                  }
procedure TSmtpTestForm.OpenButtonClick(Sender: TObject);
begin
    ProgressBar1.Position         := 0;
    FAllInOneFlag                 := FALSE;
    SmtpClient.Host               := HostEdit.Text;
    SmtpClient.Port               := PortEdit.Text;
    SmtpClient.SignOn             := SignOnEdit.Text;
    SmtpClient.Username           := UsernameEdit.Text;
    SmtpClient.Password           := PasswordEdit.Text;
    SmtpClient.AuthType           := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.ProxyServer        := ProxyHostEdit.Text;
    SmtpClient.ProxyPort          := ProxyPortEdit.Text;
    SmtpClient.ProxyUserCode      := ProxyUserEdit.Text;
    SmtpClient.ProxyPassword      := ProxyPasswordEdit.Text;
    SmtpClient.ProxyType          := TSmtpProxyType(ProxyTypeComboBox.ItemIndex);
    SmtpClient.ProxyHttpAuthType  := THttpTunnelAuthType(ProxyHttpAuthTypeComboBox.ItemIndex);

    if (SmtpClient.ProxyType <> smtpNoProxy) and
      (SmtpClient.ProxyServer = '') then
        raise Exception.Create('Proxy host is not assigned');
    SmtpClient.Open;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send originator }
procedure TSmtpTestForm.MailFromButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.FromName        := FromEdit.Text;
    SmtpClient.MailFrom;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.MailFromSIZEButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    Assert(SmtpClient.SizeSupported,
          'Either the server doesn''t support the SIZE extension or ' +
          'EHLO was not issued first');
    Assert(SmtpClient.MessageSize > 0, 'Hit button CalcMsgSize first');
    SmtpClient.FromName        := FromEdit.Text;
    SmtpClient.MailFromSIZE;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send recipients }
procedure TSmtpTestForm.RcptToButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.RcptTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send text and attached files to mail server }
procedure TSmtpTestForm.DataButtonClick(Sender: TObject);
begin
    { Assign property MailMessage and unassign OnGetData if you need }
    { automatic encoding and line wrapping                           }
    if UseMailMessageCheckBox.Checked then begin
        SmtpClient.MailMessage        := MsgMemo.Lines;
        SmtpClient.OnGetData          := nil;
        SmtpClient.WrapMsgMaxLineLen  := StrToIntDef(WrapAtEdit.Text, 76);
        SmtpClient.WrapMessageText    := WrapTextCheckBox.Checked;
    end
    else begin
        SmtpClient.OnGetData   := SmtpClientGetData;
        SmtpClient.MailMessage.Clear;
    end;
    FAllInOneFlag                 := FALSE;
    SmtpClient.CharSet            := IcsCharsetComboBox1.Charset;
    SmtpClient.ConvertToCharset   := ConvertToCharsetCheckBox.Checked;
    SmtpClient.DefaultEncoding    := TSmtpDefaultEncoding(DefEncodingComboBox.ItemIndex);
    SmtpClient.Allow8bitChars     := Allow8BitCheckBox.Checked;
    SmtpClient.FoldHeaders        := FoldHeadersCheckBox.Checked;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.HdrFrom            := FromEdit.Text;
    SmtpClient.HdrTo              := ToEdit.Text;
    SmtpClient.HdrCc              := CcEdit.Text;
    SmtpClient.HdrSubject         := SubjectEdit.Text;
    SmtpClient.HdrPriority        := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.EmailFiles         := FileAttachMemo.Lines;
    SmtpClient.ConfirmReceipt     := ConfirmCheckBox.Checked;
    PrepareProgressBar;
    SmtpClient.Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MailFrom, RcptTo and Data methods combined }
procedure TSmtpTestForm.MailButtonClick(Sender: TObject);
begin
    { Assign property MailMessage and unassign OnGetData if you need }
    { automatic encoding and line wrapping                           }
    if UseMailMessageCheckBox.Checked then begin
        SmtpClient.MailMessage        := MsgMemo.Lines;
        SmtpClient.OnGetData          := nil;
        SmtpClient.WrapMsgMaxLineLen  := StrToIntDef(WrapAtEdit.Text, 76);
        SmtpClient.WrapMessageText    := WrapTextCheckBox.Checked;
    end
    else begin
        SmtpClient.OnGetData   := SmtpClientGetData;
        SmtpClient.MailMessage.Clear;
    end;
    FAllInOneFlag                 := FALSE;
    SmtpClient.CharSet            := IcsCharsetComboBox1.Charset;
    SmtpClient.ConvertToCharset   := ConvertToCharsetCheckBox.Checked;
    SmtpClient.DefaultEncoding    := TSmtpDefaultEncoding(DefEncodingComboBox.ItemIndex);
    SmtpClient.Allow8bitChars     := Allow8BitCheckBox.Checked;
    SmtpClient.FoldHeaders        := FoldHeadersCheckBox.Checked;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.HdrFrom            := FromEdit.Text;
    SmtpClient.HdrTo              := ToEdit.Text;
    SmtpClient.HdrCc              := CcEdit.Text;
    SmtpClient.HdrSubject         := SubjectEdit.Text;
    SmtpClient.HdrPriority        := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.SignOn             := SignOnEdit.Text;
    SmtpClient.FromName           := FromEdit.Text;
    SmtpClient.EmailFiles         := FileAttachMemo.Lines;
    SmtpClient.ConfirmReceipt     := ConfirmCheckBox.Checked;
    PrepareProgressBar;
    SmtpClient.Mail;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.QuitButtonClick(Sender: TObject);
begin
    FAllInOneFlag         := FALSE;
    SmtpClient.Quit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AbortButtonClick(Sender: TObject);
begin
    FAllInOneFlag := FALSE;
    SmtpClient.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientRequestDone(
    Sender : TObject;
    RqType : TSmtpRequest;
    Error  : Word);
begin
    { For every operation, we display the status }
    if (Error > 0) then begin
        { Someting went wrong }
        if (Error < 600) or WSocketIsProxyErrorCode(Error) then
            { In case of SMTP status codes and proxy error codes the error }
            { message is available in property ErrorMessage.               }
            Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                    ' Error: '+ SmtpClient.ErrorMessage)
        else
            { Function SmtpCliErrorMsgFromErrorCode translates all known }
            { error codes to an error message.                           }
            Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                    ' Error: '+ SmtpCliErrorMsgFromErrorCode(Error));
    end
    else
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) + ' No Error');

    { Just set the progress bar to 100%                 }
    if ProgressCheckBox.Checked then begin
        if (RqType in [smtpData, smtpMail, smtpToFile]) and (Error = 0) then
            ProgressBar1.Position := ProgressBar1.Max;
    end;

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
                      if SmtpClient.AuthType = smtpAuthNone then
                          SmtpClient.Helo
                      else
                          SmtpClient.Ehlo;
                  end;
    smtpHelo:     SmtpClient.MailFrom;
    smtpEhlo:     SmtpClient.Auth;
    smtpAuth:     SmtpClient.MailFrom;
    smtpMailFrom: SmtpClient.RcptTo;
    smtpRcptTo:   SmtpClient.Data;
    smtpData:     SmtpClient.Quit;
    smtpQuit:     Display('All-In-One done !');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AllInOneButtonClick(Sender: TObject);
begin
    if SmtpClient.Connected then begin
        MessageBeep(MB_OK);
        Display('All-In-One demo start in non connected state.');
        Display('Please quit or abort the connection first.');
        Exit;
    end;

    SmtpClient.ProxyServer        := ProxyHostEdit.Text;
    SmtpClient.ProxyPort          := ProxyPortEdit.Text;
    SmtpClient.ProxyUserCode      := ProxyUserEdit.Text;
    SmtpClient.ProxyPassword      := ProxyPasswordEdit.Text;
    SmtpClient.ProxyType          := TSmtpProxyType(ProxyTypeComboBox.ItemIndex);
    SmtpClient.ProxyHttpAuthType  := THttpTunnelAuthType(ProxyHttpAuthTypeComboBox.ItemIndex);
    if (SmtpClient.ProxyType <> smtpNoProxy) and
      (SmtpClient.ProxyServer = '') then
        raise Exception.Create('Proxy host is not assigned');

    FAllInOneFlag          := TRUE;

    { Initialize all SMTP component properties from our GUI          }
    { Assign property MailMessage and unassign OnGetData if you need }
    { automatic encoding and line wrapping                           }
    if UseMailMessageCheckBox.Checked then begin
        SmtpClient.MailMessage        := MsgMemo.Lines;
        SmtpClient.OnGetData          := nil;
        SmtpClient.WrapMsgMaxLineLen  := StrToIntDef(WrapAtEdit.Text, 76);
        SmtpClient.WrapMessageText    := WrapTextCheckBox.Checked;
    end
    else begin
        SmtpClient.OnGetData   := SmtpClientGetData;
        SmtpClient.MailMessage.Clear;
    end;
    SmtpClient.CharSet            := IcsCharsetComboBox1.Charset;
    SmtpClient.ConvertToCharset   := ConvertToCharsetCheckBox.Checked;
    SmtpClient.DefaultEncoding    := TSmtpDefaultEncoding(DefEncodingComboBox.ItemIndex);
    SmtpClient.Allow8bitChars     := Allow8BitCheckBox.Checked;
    SmtpClient.FoldHeaders        := FoldHeadersCheckBox.Checked;
    SmtpClient.Host               := HostEdit.Text;
    SmtpClient.Port               := PortEdit.Text;
    SmtpClient.SignOn             := SignOnEdit.Text;
    SmtpClient.FromName           := FromEdit.Text;
    SmtpClient.HdrFrom            := FromEdit.Text;
    SmtpClient.HdrTo              := ToEdit.Text;
    SmtpClient.HdrCc              := CcEdit.Text;
    SmtpClient.HdrSubject         := SubjectEdit.Text; { + #13#10#9 + ' Testing continuation line !'};
    SmtpClient.EmailFiles         := FileAttachMemo.Lines;
    SmtpClient.AuthType           := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.Username           := UsernameEdit.Text;
    SmtpClient.Password           := PasswordEdit.Text;
    SmtpClient.HdrPriority        := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.ConfirmReceipt     := ConfirmCheckBox.Checked;
    { Recipient list is computed from To, Cc and Bcc fields }
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    PrepareProgressBar;
    Display('Connecting to SMTP server...');
    { Start first operation to do to send an email          }
    { Next operations are started from OnRequestDone event  }
    SmtpClient.Connect
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.MsgSizeButtonClick(Sender: TObject);
begin
    { Assign property MailMessage and unassign OnGetData if you need }
    { automatic encoding and line wrapping                           }
    if UseMailMessageCheckBox.Checked then begin
        SmtpClient.MailMessage        := MsgMemo.Lines;
        SmtpClient.OnGetData          := nil;
        SmtpClient.WrapMsgMaxLineLen  := StrToIntDef(WrapAtEdit.Text, 76);
    end
    else begin
        SmtpClient.OnGetData   := SmtpClientGetData;
        SmtpClient.MailMessage.Clear;
    end;
    FAllInOneFlag                 := FALSE;
    SmtpClient.CharSet            := IcsCharsetComboBox1.Charset;
    SmtpClient.ConvertToCharset   := ConvertToCharsetCheckBox.Checked;
    SmtpClient.DefaultEncoding    := TSmtpDefaultEncoding(DefEncodingComboBox.ItemIndex);
    SmtpClient.Allow8bitChars     := Allow8BitCheckBox.Checked;
    SmtpClient.FoldHeaders        := FoldHeadersCheckBox.Checked;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.Text);
    SmtpClient.HdrFrom            := FromEdit.Text;
    SmtpClient.HdrTo              := ToEdit.Text;
    SmtpClient.HdrCc              := CcEdit.Text;
    SmtpClient.HdrSubject         := SubjectEdit.Text;
    SmtpClient.HdrPriority        := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.EmailFiles         := FileAttachMemo.Lines;
    SmtpClient.ConfirmReceipt     := ConfirmCheckBox.Checked;
    SmtpClient.CalcMsgSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.CharsetTestButtonClick(Sender: TObject);
var
    OldCharset : String;
begin
    { Assigning a non-supported charset to property CharSet would  }
    { raise an ESmtpException and the default system charset is    }
    { assigned. Assigning an empty string however sets the default }
    { system charset silently.                                     }
    OldCharSet := SmtpClient.CharSet;
    if IcsCharsetComboBox1.Charset = '' then
        raise Exception.Create('Enter a MIME charset name');
    SmtpClient.CharSet := IcsCharsetComboBox1.CharSet; // Sets property CodePage as well
    CharsetInfoLabel1.Caption := '"' + SmtpClient.CharSet + '" supported. ' +
                              'Code page ID = ' + IntToStr(SmtpClient.CodePage);
    SmtpClient.CharSet := OldCharSet;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ToggleCsViewButtonClick(Sender: TObject);
begin
    IcsCharsetComboBox1.UserFriendly := not IcsCharsetComboBox1.UserFriendly;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.IcsCharsetComboBox1Change(Sender: TObject);
begin
    CharsetInfoLabel1.Caption := '"' + IcsCharsetComboBox1.CharSet + '"';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ProxyTypeComboBoxCloseUp(Sender: TObject);
begin
    { Just to visualize the properties required depending on proxy settings }
    Assert(TComboBox(Sender).ItemIndex >= 0);
    ProxyHttpAuthTypeComboBox.Enabled :=
              (TSmtpProxyType(TComboBox(Sender).ItemIndex) = smtpHttpProxy);
    ProxyHostEdit.Enabled :=
        (TSmtpProxyType(TComboBox(Sender).ItemIndex) <> smtpNoProxy);
    ProxyPortEdit.Enabled := ProxyHostEdit.Enabled;
    ProxyUserEdit.Enabled := ProxyHostEdit.Enabled and not
      (ProxyHttpAuthTypeComboBox.Enabled and
      (THttpTunnelAuthType(ProxyHttpAuthTypeComboBox.ItemIndex) = htatNone));
    ProxyPasswordEdit.Enabled := ProxyUserEdit.Enabled and not
     (TSmtpProxyType(TComboBox(Sender).ItemIndex) in [smtpSocks4, smtpSocks4A]);
    if ProxyHttpAuthTypeComboBox.Enabled then
        ProxyHttpAuthTypeComboBoxCloseUp(ProxyHttpAuthTypeComboBox);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ProxyHttpAuthTypeComboBoxCloseUp(Sender: TObject);
begin
    { Just to visualize the properties required depending on proxy settings }
    Assert(TComboBox(Sender).ItemIndex >= 0);
    ProxyUserEdit.Enabled :=
        (THttpTunnelAuthType(TCombobox(Sender).ItemIndex) <> htatNone);
    ProxyPasswordEdit.Enabled := ProxyUserEdit.Enabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
