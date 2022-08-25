{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 18, 2003
Description:  Sample program to show how to send HTML formatted mail using ICS.
              Mail can have embedded images.
Version:      6.03
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

History:
Jul 18, 2004 V1.01 Revised for new property EmailImages (previous version
                   didn't make any difference between images and attached
                   files.
Mar 13, 2005 V1.02 Added confirm checkbox and related code.
Aug 30, 2007 V6.03 ICS V6 compatible.
Jul 19, 2008 V6.03 F.Piette made some changes for Unicode


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMailHtm1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsSmtpProt, OverbyteIcsMimeUtils,
  OverbyteIcsWndControl;

type
  THtmlMailForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    HostEdit: TEdit;
    Label4: TLabel;
    PortEdit: TEdit;
    Label2: TLabel;
    FromEdit: TEdit;
    Label3: TLabel;
    ToEdit: TEdit;
    Label12: TLabel;
    CcEdit: TEdit;
    Label13: TLabel;
    BccEdit: TEdit;
    Subject: TLabel;
    SubjectEdit: TEdit;
    Label8: TLabel;
    SignOnEdit: TEdit;
    SendButton: TButton;
    Panel1: TPanel;
    PlainTextMemo: TMemo;
    HtmlTextMemo: TMemo;
    AbortButton: TButton;
    HtmlSmtpClient: THtmlSmtpCli;
    PlainTextCheckBox: TCheckBox;
    Panel2: TPanel;
    ImageFilesMemo: TMemo;
    AttachedFilesMemo: TMemo;
    ConfirmCheckBox: TCheckBox;
    UsernameEdit: TEdit;
    Label5: TLabel;
    PasswordEdit: TEdit;
    Label6: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure HtmlSmtpClientRequestDone(Sender: TObject;
      RqType: TSmtpRequest; ErrorCode: Word);
    procedure HtmlSmtpClientDisplay(Sender: TObject; Msg: String);
    procedure HtmlSmtpClientSessionClosed(Sender: TObject; ErrCode: Word);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FRunning      : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HtmlMailForm: THtmlMailForm;

implementation

{$R *.DFM}

const
    SectionWindow         = 'MainWindow';
    KeyTop                = 'Top';
    KeyLeft               = 'Left';
    KeyWidth              = 'Width';
    KeyHeight             = 'Height';
    SectionData           = 'Data';
    KeyHost               = 'HostName';
    KeyPort               = 'Port';
    KeyFrom               = 'From';
    KeyTo                 = 'To';
    KeyCc                 = 'Cc';
    KeyBcc                = 'Bcc';
    KeySubject            = 'Subject';
    KeySignOn             = 'SignOn';
    KeyConfirm            = 'Confirm';
    SectionHtmlText       = 'HtmlText';
    KeyHtmlText           = 'Html';
    SectionPlainText      = 'PlainText';
    KeyPlainText          = 'Plain';
    SectionImageFiles     = 'ImageFiles';
    KeyImageFiles         = 'ImageFiles';
    SectionAttachedFiles  = 'AttachedFiles';
    KeyAttachedFiles      = 'AttachedFiles';
    KeyPassword           = 'Password';
    KeyUsername           = 'Username';


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
    if (IniSection = '') or (IniKey = '') or (not Assigned(Strings)) then
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
            case Buf[Length(IniKey) + 1] of
            '0'..'9':
                begin
                    I := Pos('=', Buf);
                    Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
                end;
            else
                Strings.Delete(nItem)
            end;
(*
            if not (Buf[Length(IniKey) + 1] in ['0'..'9']) then
                Strings.Delete(nItem)
            else begin
                I := Pos('=', Buf);
                Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
            end;
*)
        end;
        Dec(nItem);
    end;
    Result := (Strings.Count <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        DisplayMemo.Clear;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        HostEdit.Text    := IniFile.ReadString(SectionData, KeyHost,
                                               'localhost');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,
                                               'smtp');
        FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom,
                                               'first.last@company.com');
        ToEdit.Text      := IniFile.ReadString(SectionData, KeyTo,
                                               'john.doe@acme;' +
                                               'tartempion@brol.fr');
        CcEdit.Text      := IniFile.ReadString(SectionData, KeyCc,
                                               '');
        BccEdit.Text     := IniFile.ReadString(SectionData, KeyBcc,
                                               'francois.piette@swing.be');
        SubjectEdit.Text := IniFile.ReadString(SectionData, KeySubject,
                                               'A sample HTML message sent ' +
                                               'using ICS component');
        SignOnEdit.Text  := IniFile.ReadString(SectionData, KeySignOn,
                                               'your_name');
        ConfirmCheckBox.Checked  := Boolean(IniFile.ReadInteger(SectionData,
                                            KeyConfirm, 0));
        UsernameEdit.Text := IniFile.ReadString(SectionData, KeyUsername, '');
        PasswordEdit.Text := IniFile.ReadString(SectionData, KeyPassword, '');

        if not LoadStringsFromIniFile(IniFile, SectionImageFiles,
                                      KeyImageFiles, ImageFilesMemo.Lines) then
            ImageFilesMemo.Text := 'ics_logo.gif' + #13#10 + 'fp_small.gif';

        if not LoadStringsFromIniFile(IniFile, SectionAttachedFiles,
                                      KeyAttachedFiles, AttachedFilesMemo.Lines) then
            AttachedFilesMemo.Text := 'OverbyteIcsMailHtml.dpr';

        if not LoadStringsFromIniFile(IniFile, SectionPlainText,
                                      KeyPlainText, PlainTextMemo.Lines) then
            PlainTextMemo.Text :=
            'This is a HTML mail message sent using ICS.' + #13#10 +
            'Internet Component Suite is freeware.' + #13#10 +
            '<<IMAGE1>>' + #13#10 +
            'You can download ICS full source code from ' + #13#10 +
            'Overbyte website <http://www.overbyte.be>.' + #13#10 +
            #13#10 +
            'Need to secure your applications using ICS ?' + #13#10 +
            'Need to access secure web pages using HTTPS ?' + #13#10 +
            'Think about contributing to the ICS-SSL effort !' + #13#10 +
            'Visit the ICS-SSL website ' +
            '<http://overbyte.delphicenter.com/eng/ssl.html>.' + #13#10 +
            #13#10 +
            'Need high performance multi-tier applications ?' + #13#10 +
            'Then you need MidWare. It''s a full featured toolkit' + #13#10 +
            'to build powerful N-tier applications with Delphi.' + #13#10 +
            'It work equally well across the Internet or just on' + #13#10 +
            'your LAN/WAN. MidWare include two sets of components:' + #13#10 +
            'one to build your own application servers and one to' + #13#10 +
            'build your thin custom clients. MidWare can use any' + #13#10 +
            'database or even no database at all. Download full ' + #13#10 +
            'source code from http://www.overbyte.be' + #13#10 + #13#10 +
            'ICS and MidWare are creations of François Piette.' + #13#10 +
            '<<IMAGE2>>' + #13#10 +
            '--' + #13#10 +
            'mailto:francois.piette@overbyte.be' + #13#10;
        if not LoadStringsFromIniFile(IniFile, SectionHtmlText,
                                      KeyHtmlText, HtmlTextMemo.Lines) then
            HtmlTextMemo.Text := '<HTML><BODY>' + #13#10 +
            'This is a HTML mail message sent using <B>ICS</B>.<BR>' + #13#10 +
            '<B>I</B>nternet <B>C</B>omponent ' +
            '<B>S</B>uite is <U>freeware</U>.<BR><BR>' + #13#10 +
            '<A HREF="http://www.overbyte.be">' +
            '<IMG SRC="cid:IMAGE1" WIDTH=148 HEIGHT=105 BORDER=0></A><BR><BR>' + #13#10 +
            'You can download ICS full source code from' + #13#10 +
            '<A HREF="http://www.overbyte.be">' +
            'Overbyte website</A>.<BR>' + #13#10 +
            'ICS is a PostcardWare: you must send a picture postcard ' +
            'to the author if you are using the code. You can find the ' +
            'details in the readme.txt file.<BR><BR>' + #13#10 +
            'Need to <U>secure your applications</U> using ICS ?<BR>' + #13#10 +
            'Need to access secure web pages using <B>HTTPS</B> ?<BR>' + #13#10 +
            'Think about contributing to the ICS-SSL effort !<BR>' + #13#10 +
            'Visit the <A HREF="http://overbyte.delphicenter.com/eng/ssl.html">' +
            'ICS-SSL website</A>.<BR><BR>' + #13#10 +
            'Need high performance multi-tier applications ?<BR>' + #13#10 +
            'Then you need <A HREF="http://www.overbyte.be">MidWare</A>. ' +
            'It''s a full featured toolkit '+
            'to build powerful N-tier applications with Delphi. It work ' +
            'equally well across the Internet or just on your LAN/WAN. ' +
            'MidWare include two sets of components: one to build your own ' +
            'application servers and one to build your thin custom clients. ' +
            'MidWare can use any database or even no database ' +
            'at all. Download full source code from ' +
            '<A HREF="http://www.overbyte.be">here</A><BR><BR>' + #13#10 +
            'ICS and MidWare are creations of François Piette<BR>' + #13#10 +
            '<A HREF="mailto:francois.piette@overbyte.be?' +
            'subject=ICS%26MIDWARE">' +
            '<IMG SRC="cid:IMAGE2" BORDER=0 WIDTH=92 HEIGHT=121></A>' +
            '<BR>--<BR>' + #13#10 +
            '<A HREF="mailto:francois.piette@overbyte.be?' +
            'subject=ICS%26MIDWARE">francois.piette@overbyte.be</A>' + #13#10 +
            '</BODY>' + #13#10 +
            '</HTML>' + #13#10;

        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,    KeyHost,      HostEdit.Text);
    IniFile.WriteString(SectionData,    KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData,    KeyFrom,      FromEdit.Text);
    IniFile.WriteString(SectionData,    KeyTo,        ToEdit.Text);
    IniFile.WriteString(SectionData,    KeyCc,        CcEdit.Text);
    IniFile.WriteString(SectionData,    KeyBcc,       BccEdit.Text);
    IniFile.WriteString(SectionData,    KeySubject,   SubjectEdit.Text);
    IniFile.WriteString(SectionData,    KeySignOn,    SignOnEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyConfirm,  Ord(ConfirmCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyUsername, UsernameEdit.Text);
    IniFile.WriteString(SectionData,    KeyPassword, PasswordEdit.Text);
    SaveStringsToIniFile(IniFile, SectionImageFiles,
                         KeyImageFiles, ImageFilesMemo.Lines);
    SaveStringsToIniFile(IniFile, SectionAttachedFiles,
                         KeyAttachedFiles, AttachedFilesMemo.Lines);
    SaveStringsToIniFile(IniFile, SectionPlainText,
                         KeyPlainText, PlainTextMemo.Lines);
    SaveStringsToIniFile(IniFile, SectionHtmlText,
                         KeyHtmlText, HtmlTextMemo.Lines);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.AbortButtonClick(Sender: TObject);
begin
    Display('Aborting...');
    FRunning := FALSE;
    HtmlSmtpClient.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.SendButtonClick(Sender: TObject);
begin
    if FRunning then begin
        MessageBeep(MB_OK);
        Display('**** ALREADY RUNNING ****');
        Exit;
    end;

    FRunning := TRUE;
    try
        { Give the component the various data he need }
        HtmlSmtpClient.PlainText       := PlainTextMemo.Lines;
        HtmlSmtpClient.HtmlText        := HtmlTextMemo.Lines;
        HtmlSmtpClient.EmailImages     := ImageFilesMemo.Lines;
        HtmlSmtpClient.EmailFiles      := AttachedFilesMemo.Lines;

        { Initialize all SMTP component properties from our GUI }
        HtmlSmtpClient.Host            := HostEdit.Text;
        HtmlSmtpClient.Port            := PortEdit.Text;
        HtmlSmtpClient.SignOn          := SignOnEdit.Text;
        HtmlSmtpClient.FromName        := FromEdit.Text;
        HtmlSmtpClient.HdrFrom         := FromEdit.Text;
        HtmlSmtpClient.HdrTo           := ToEdit.Text;
        HtmlSmtpClient.HdrCc           := CcEdit.Text;
        HtmlSmtpClient.HdrSubject      := SubjectEdit.Text;
        HtmlSmtpClient.Username        := UsernameEdit.Text;
        HtmlSmtpClient.Password        := PasswordEdit.Text;
        if (HtmlSmtpClient.Username <> '') and (HtmlSmtpClient.Password <> '') then
            HtmlSmtpClient.AuthType        := smtpAuthAutoSelect
        else
            HtmlSmtpClient.AuthType        := smtpAuthNone;
        HtmlSmtpClient.ConfirmReceipt  := ConfirmCheckbox.Checked;
        { Recipient list is computed from To, Cc and Bcc fields }
        HtmlSmtpClient.RcptName.Clear;
        HtmlSmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);

        if  PlainTextCheckBox.Checked then
            HtmlSmtpClient.ContentType := smtpPlainText
        else
            HtmlSmtpClient.ContentType := smtpHtml;

        Display('Connecting to SMTP server...');
        { Start first operation to do to send an email          }
        { Next operations are started from OnRequestDone event  }
        HtmlSmtpClient.Connect
    except
        on E:Exception do begin
            Display(E.ClassName + ': ' + E.Message);
            FRunning := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.HtmlSmtpClientRequestDone(
    Sender    : TObject;
    RqType    : TSmtpRequest;
    ErrorCode : Word);
begin
     { For every operation, we display the status }
     if (ErrorCode > 0) and  (ErrorCode < 10000) then
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                ' Error='+ HtmlSmtpClient.ErrorMessage)
     else
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                ' Error='+ IntToStr(ErrorCode));
    if not FRunning then
        Exit;
    { Start next operation, but first check if previous one was OK }
    if ErrorCode <> 0 then begin
        FRunning := FALSE;   { Terminate All-In-One demo }
        Display('Error, stop.');
        Exit;
    end;
    case RqType of
    smtpConnect:  begin
                      if HtmlSmtpClient.AuthType = smtpAuthNone then
                          HtmlSmtpClient.Helo
                      else
                          HtmlSmtpClient.Ehlo;
                  end;
    smtpHelo:     HtmlSmtpClient.MailFrom;
    smtpEhlo:     HtmlSmtpClient.Auth;
    smtpAuth:     HtmlSmtpClient.MailFrom;
    smtpMailFrom: HtmlSmtpClient.RcptTo;
    smtpRcptTo:   HtmlSmtpClient.Data;
    smtpData:     HtmlSmtpClient.Quit;
    smtpQuit:     begin
                      Display('Done !');
                      FRunning := FALSE;
                  end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.HtmlSmtpClientDisplay(Sender: TObject; Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlMailForm.HtmlSmtpClientSessionClosed(
    Sender  : TObject;
    ErrCode : Word);
begin
    Display('Disconnected !');
    FRunning := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
