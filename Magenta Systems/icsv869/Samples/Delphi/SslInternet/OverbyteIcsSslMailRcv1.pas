{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       Show how to use TPop3Cli (POP3 protocol, RFC-1225)
Creation:     03 october 1997
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
Nov 12, 1997  V1.01 Added a GetAll button to get all messages waiting in the
              POP3 server, copying them to a file using the UIDL to build
              the file name (sorry, wont work with D1 because of long file
              name). The message is *NOT* deleted from the POP3 server.
Jan 10, 1998  V1.02 Added port selection
Jul 05, 2002  V1.03 Added header display in separate UI gadget
Jan 11, 2004  V1.04 Added Auth feature.
              Added form persitence.
Mar 23, 2006  V6.00  New version started from ICS-V5
Aug 12, 2007  V6.00a Updated for ICS-V6
Dec 25, 2007  V6.00b A. Garrels update SSL
Jul 04, 2010  V6.01 Updated to support new TPop3Cli V6.07.
Nov 25, 2020  V8.65 - Added XOAuth2 and OAuthBearer authentication support using
                 TIcsRestEmail component for OAuth2, allows to access GMail account
                 with security enabled and Microsoft Outlook mail.
                 See comments at top of OverbyteIcsSslHttpRest.pas on how to
                 set-up a Google/Microsoft OAuth2 application account.
              Changed defaults for implicit SSL connection which is standard now.
Mar 23, 2022 V8.69 - Added unit OverbyteIcsSslHttpOAuth for TIcsRestEmail
                        previously in HttpRest.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMailRcv1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, OverbyteIcsIniFiles,
  OverbyteIcsPop3Prot,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsLogger,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSslHttpOAuth;  { V8.69 }

const
    MailRcvVersion = 869;
    CopyRight : String = ' MailRcv demo (c) 1997-2022 F. Piette V8.69 ';

type
  TPOP3ExcercizerForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    InfoLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ConnectButton: TButton;
    QuittButton: TButton;
    UserButton: TButton;
    HostEdit: TEdit;
    UserNameEdit: TEdit;
    PassWordEdit: TEdit;
    PassButton: TButton;
    MsgNumEdit: TEdit;
    RetrButton: TButton;
    StatButton: TButton;
    ListAllButton: TButton;
    ListButton: TButton;
    DeleteButton: TButton;
    NoopButton: TButton;
    LastButton: TButton;
    ResetButton: TButton;
    TopButton: TButton;
    MsgLinesEdit: TEdit;
    RpopButton: TButton;
    UidlButton: TButton;
    ApopButton: TButton;
    NextButton: TButton;
    GetAllButton: TButton;
    PortEdit: TEdit;
    Label6: TLabel;
    OpenButton: TButton;
    AbortButton: TButton;
    Label7: TLabel;
    SubjectEdit: TEdit;
    Label8: TLabel;
    FromEdit: TEdit;
    Label9: TLabel;
    ToEdit: TEdit;
    AuthComboBox: TComboBox;
    Label11: TLabel;
    AuthButton: TButton;
    SslPop3Client: TSslPop3Cli;
    SslContext1: TSslContext;
    SslTypeComboBox: TComboBox;
    STlsButton: TButton;
    Label10: TLabel;
    VerifyCheckBox: TCheckBox;
    CAFileEdit: TEdit;
    CAPathEdit: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    IcsRestEmail1: TIcsRestEmail;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuittButtonClick(Sender: TObject);
    procedure UserButtonClick(Sender: TObject);
    procedure PassButtonClick(Sender: TObject);
    procedure SslPop3ClientMessageBegin(Sender: TObject);
    procedure SslPop3ClientMessageEnd(Sender: TObject);
    procedure SslPop3ClientMessageLine(Sender: TObject);
    procedure RetrButtonClick(Sender: TObject);
    procedure StatButtonClick(Sender: TObject);
    procedure ListAllButtonClick(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure SslPop3ClientListBegin(Sender: TObject);
    procedure SslPop3ClientListEnd(Sender: TObject);
    procedure SslPop3ClientListLine(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure NoopButtonClick(Sender: TObject);
    procedure LastButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure TopButtonClick(Sender: TObject);
    procedure RpopButtonClick(Sender: TObject);
    procedure SslPop3ClientDisplay(Sender: TObject; const Msg: String);
    procedure UidlButtonClick(Sender: TObject);
    procedure SslPop3ClientUidlBegin(Sender: TObject);
    procedure SslPop3ClientUidlEnd(Sender: TObject);
    procedure SslPop3ClientUidlLine(Sender: TObject);
    procedure ApopButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure GetAllButtonClick(Sender: TObject);
    procedure SslPop3ClientRequestDone(Sender: TObject; RqType: TPop3Request;
      ErrCode: Word);
    procedure OpenButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure SslPop3ClientHeaderEnd(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure STlsButtonClick(Sender: TObject);
    procedure SslPop3ClientSslHandshakeDone(Sender: TObject; ErrCode: Word;
      PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslPop3ClientSslVerifyPeer(Sender: TObject; var Ok: Integer;
      Cert: TX509Base);
    procedure IcsRestEmail1EmailNewToken(Sender: TObject);
    procedure IcsRestEmail1EmailProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure SslPop3ClientGetNewToken(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FFile         : TextFile;
    FFileName     : String;
    FFileOpened   : Boolean;
    FGetAllState  : Integer;
    FMsgPath      : String;
    procedure SetSslVerifyProps;   
    procedure Exec(MethodPtr  : TPop3NextProc;
                   MethodName : String);
    procedure MessageBegin(Sender: TObject);
    procedure MessageLine(Sender: TObject);
    procedure GetAllMessageLine(Sender: TObject);
    procedure GetAllRequestDone(Sender: TObject;
                                RqType: TPop3Request; ErrCode: Word);
    procedure NextMessageRequestDone(Sender: TObject;
                                     RqType: TPop3Request; ErrCode: Word);
    procedure SetupOAuth2;                                                      { V8.65 }
  end;

var
  POP3ExcercizerForm: TPOP3ExcercizerForm;

implementation

{$R *.DFM}

uses
    OverbyteIcsSslMailRcv2;

const
    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    SectionData       = 'Data';
    KeyHost           = 'Host';
    KeyPort           = 'Port';
    KeyUserName       = 'UserName';
    KeyPassword       = 'Password';
    KeyAuth           = 'Authentication';
    SectionSsl        = 'SSL';
    KeySslType        = 'SslType';
    KeyCaFile         = 'CAFile';
    KeyCaPath         = 'CAPath';
    KeyVerify         = 'VerifyCert';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if FInitialized then Exit;
    FInitialized := TRUE;
    IniFile := TIcsIniFile.Create(FIniFileName);
    try

        Top                    := IniFile.ReadInteger(SectionWindow, KeyTop,
                                             (Screen.Height - Height) div 2);
        Left                   := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                             (Screen.Width - Width) div 2);
        Width                  := IniFile.ReadInteger(SectionWindow, KeyWidth,
                                                      Width);
        Height                 := IniFile.ReadInteger(SectionWindow, KeyHeight,
                                                      Height);
        HostEdit.Text          := IniFile.ReadString(SectionData, KeyHost,
                                                     'pop.hostname.com');
        PortEdit.Text          := IniFile.ReadString(SectionData, KeyPort,
                                                     '995');
        UserNameEdit.Text      := IniFile.ReadString(SectionData, KeyUserName,
                                                     '');
        PassWordEdit.Text      := IniFile.ReadString(SectionData, KeyPassword,
                                                     '');
        AuthComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyAuth,
                                                      Ord(popAuthLogin));       { V8.65 SSL defaults }
        SslTypeComboBox.ItemIndex := IniFile.ReadInteger(SectionSSL, KeySslType,
                                                      Ord(pop3TlsImplicit));
        VerifyCheckbox.Checked := IniFile.ReadBool(SectionSSL, KeyVerify,
                                                   FALSE);
        CAFileEdit.Text        := IniFile.ReadString(SectionSSL, KeyCAFile,
                                                     'TrustedCABundle.pem');
        CAPathEdit.Text        := IniFile.ReadString(SectionSSL, KeyCAPath,
                                                     'TrustedCaStore');
    finally
        IniFile.Free;
    end;
    InfoLabel.Caption := '';
    SubjectEdit.Text  := '';
    FromEdit.Text     := '';
    ToEdit.Text       := '';
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.WriteString(SectionData,  KeyHost,     HostEdit.Text);
        IniFile.WriteString(SectionData,  KeyPort,     PortEdit.Text);
        IniFile.WriteString(SectionData,  KeyUserName, UserNameEdit.Text);
        IniFile.WriteString(SectionData,  KeyPassword, PassWordEdit.Text);
        IniFile.WriteInteger(SectionData, KeyAuth,     AuthComboBox.ItemIndex);
        IniFile.WriteInteger(SectionSSL,  KeySslType,  SslTypeComboBox.ItemIndex);
        IniFile.WriteBool(SectionSSL,     KeyVerify,   VerifyCheckbox.Checked);
        IniFile.WriteString(SectionSSL,   KeyCAFile,   CAFileEdit.Text);
        IniFile.WriteString(SectionSSL,   KeyCAPath,   CAPathEdit.Text);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the TPop3Client object wants to display }
{ some information such as connection progress or errors.                   }
procedure TPOP3ExcercizerForm.SslPop3ClientDisplay(
    Sender    : TObject;
    const Msg : String);
begin
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SetSslVerifyProps;
begin
    SslContext1.SslVerifyPeer := VerifyCheckbox.Checked;
    if SslContext1.SslVerifyPeer then begin
        SslContext1.SslCAFile := CaFileEdit.Text;
        SslContext1.SslCAPath := CaPathEdit.Text;
        SslPop3Client.OnSslVerifyPeer := SslPop3ClientSslVerifyPeer;
        SslPop3Client.OnSslHandshakeDone := SslPop3ClientSslHandshakeDone;
    end
    else begin
        SslContext1.SslCAFile := '';
        SslContext1.SslCAPath := '';
        SslPop3Client.OnSslVerifyPeer := nil;
        SslPop3Client.OnSslHandshakeDone := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SetupOAuth2;                                      { V8.65 }
begin
  // hardcode your Google or Microsoft client application account details
    IcsRestEmail1.ClientId := '';
    IcsRestEmail1.ClientSecret := '';
    if IcsRestEmail1.ClientId = '' then begin
        DisplayMemo.Lines.Add('OAuth2 authentication requires an client application account');
        Exit;
    end;
    IcsRestEmail1.Clear;
     if Length(PasswordEdit.Text) > 32 then
        IcsRestEmail1.RefrToken := PasswordEdit.Text;  // ideally we'd save it separately
    if Pos('gmail', SslPop3Client.Host) > 0 then
        IcsRestEmail1.RestEmailType := RestEmailGoogle
    else if (Pos('office365', SslPop3Client.Host) > 0) or
                (Pos('outlook', SslPop3Client.Host) > 0) then
        IcsRestEmail1.RestEmailType := RestEmailMSSmtp
    else
        DisplayMemo.Lines.Add('Can not use OAuth2 Authentication with this POP3 Server');
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All the TPop3Client method are of the same type. To simplify this demo    }
{ application, Exec transfert the parameters form the various EditBoxes     }
{ to the Pop3Client instance and then call the appropriate method, showing  }
{ the result in the InfoLabel.Caption.                                      }
procedure TPOP3ExcercizerForm.Exec(
    MethodPtr  : TPop3NextProc;
    MethodName : String);
begin
    SubjectEdit.Text          := '';
    FromEdit.Text             := '';
    ToEdit.Text               := '';
    SslPop3Client.Host           := HostEdit.Text;
    SslPop3Client.Port           := PortEdit.Text;
    SslPop3Client.SslType        := TPop3SslType(SslTypeCombobox.ItemIndex);
    if ((SslPop3Client.SslType = pop3TlsImplicit) and
       ((MethodName = 'Open') or (MethodName = 'Connect'))) or
       ((SslPop3Client.SslType = pop3TlsExplicit) and
       ((MethodName = 'STLS') or (MethodName = 'Open'))) then begin
       { Assign properties, they are applied when the context gets initialized.}
        SetSslVerifyProps;
    end;
    SslPop3Client.AuthType       := TPop3AuthType(AuthComboBox.ItemIndex);
    SslPop3Client.UserName       := UserNameEdit.Text;
    SslPop3Client.PassWord       := PassWordEdit.Text;
    if (MethodName = 'Open') or (MethodName = 'Connect') then begin
        if SslPop3Client.AuthType in [popAuthXOAuth2, popAuthOAuthBearer] then
            SetupOAuth2;                                                        { V8.65 }
    end;
    SslPop3Client.MsgNum         := StrToInt(MsgNumEdit.Text);
    SslPop3Client.MsgLines       := StrToInt(MsgLinesEdit.Text);
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    SslPop3Client.OnRequestDone  := SslPop3ClientRequestDone;
    SslPop3Client.OnMessageBegin := SslPop3ClientMessageBegin;
    SslPop3Client.OnMessageEnd   := SslPop3ClientMessageEnd;
    SslPop3Client.OnMessageLine  := SslPop3ClientMessageLine;
    InfoLabel.Caption         := MethodName + ' starting';                      { V8.65 }
    try
        MethodPtr;
        InfoLabel.Caption := MethodName + ' started async ok';                  { V8.65 }
    except
        on E:Exception do begin
            MessageBeep(MB_OK);
            InfoLabel.Caption := MethodName + ' failed (' + E.Message + ')';
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ConnectButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Connect, 'Connect');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.STlsButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.STls, 'STLS');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.OpenButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Open, 'Open');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UserButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.User, 'User');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.PassButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Pass, 'Pass');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.QuittButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Quit, 'Quit');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.AbortButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Abort, 'Abort');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RetrButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Retr, 'Retr');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.StatButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Stat, 'Stat');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListAllButtonClick(Sender: TObject);
begin
    MsgNumEdit.Text := '0';
    Exec(SslPop3Client.List, 'List All');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.List, 'List');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.DeleteButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Dele, 'Delete');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NoopButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Noop, 'Noop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.LastButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Last, 'Last');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ResetButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.RSet, 'Rset');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.TopButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Top, 'Top');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RpopButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.RPop, 'Rpop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UidlButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Uidl, 'Uidl');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ApopButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.APop, 'Apop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ message. The MsgNum property gives the message number.                    }
{ This event handler could be used to open the file used to store the msg.  }
{ The file handle could be stored in the TPop3Client.Tag property to be     }
{ easily retrieved by the OnMessageLine and OnMessageEnd event handlers.    }
procedure TPOP3ExcercizerForm.SslPop3ClientMessageBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Message ' +
                          IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has detected the end of a   }
{ message, even if there is an error or exception, this event gets called.  }
{ This event handler could be used to close the file used to store the msg. }
procedure TPOP3ExcercizerForm.SslPop3ClientMessageEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Message ' +
                          IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' end ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each message line that TPop3Client is    }
{ receiveing. This could be used to write the message lines to a file.      }
procedure TPOP3ExcercizerForm.SslPop3ClientMessageLine(Sender: TObject);
begin
    DisplayMemo.Lines.Add(String((Sender as TPop3Cli).LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ list line. The MsgNum property gives the message number.                  }
procedure TPOP3ExcercizerForm.SslPop3ClientListBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** List begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has received the last list  }
{ line.                                                                     }
procedure TPOP3ExcercizerForm.SslPop3ClientListEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** List End ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each list line received by TPop3Client.  }
procedure TPOP3ExcercizerForm.SslPop3ClientListLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgSize = ' + IntToStr((Sender as TPop3Cli).MsgSize) + ' ' +
              'Line = ''' + String((Sender as TPop3Cli).LastResponse) + '''';
    DisplayMemo.Lines.Add(Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientUidlBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Uidl begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientUidlEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Uidl end ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientUidlLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgUidl = ' + String((Sender as TPop3Cli).MsgUidl) + '''';
    DisplayMemo.Lines.Add(Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.MessageBegin(Sender: TObject);
begin
    MessageForm.Caption := 'Message ' +
                           IntToStr((Sender as TPop3Cli).MsgNum);
    MessageForm.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.MessageLine(Sender: TObject);
begin
    MessageForm.DisplayMemo.Lines.Add(String((Sender as TPop3Cli).LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NextButtonClick(Sender: TObject);
begin
    MessageForm.DisplayMemo.Clear;
    MessageForm.Caption       := 'Message';
    SslPop3Client.OnMessageBegin := MessageBegin;
    SslPop3Client.OnMessageEnd   := nil;
    SslPop3Client.OnMessageLine  := MessageLine;
    SslPop3Client.OnRequestDone  := NextMessageRequestDone;
    SslPop3Client.MsgNum         := StrToInt(MsgNumEdit.Text);
    SslPop3Client.Retr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NextMessageRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    if ErrCode <> 0 then
        Exit;

    MsgNumEdit.Text := IntToStr(StrToInt(MsgNumEdit.Text) + 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.GetAllMessageLine(Sender: TObject);
begin
    Writeln(FFile, (Sender as TPop3Cli).LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The procedure here after will start an event chain that will eventually   }
{ download all messages for the POP3 server. We cannot simply loop because  }
{ the POP3 compomnet is asynchronous: it will not wait for operation done   }
{ before returning. We must "chain" operations one after the other using    }
{ the OnRequestDone event handler. We use the variable FGetAllState to keep }
{ track of where we are.                                                    }
{ To get all messages, we must first call Stat to know how many messages    }
{ are on the server, then for each message we call Uidl to get a unique     }
{ identifier for each message to build a file name and know if we already   }
{ have a message, then we retrieve the message, then we increment the       }
{ message number and continue until the number of messages is reached.      }
{ We should start a TTimer to handle timeout...                             }
procedure TPOP3ExcercizerForm.GetAllButtonClick(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    { Get path from INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    FMsgPath    := IniFile.ReadString('Data', 'MsgPath',
                                  ExtractFilePath(Application.ExeName));
    IniFile.Free;

    { Be sure to have an ending backslash }
    if (Length(FMsgPath) > 0) and (FMsgPath[Length(FMsgPath)] <> '\') then
        FMsgPath := FMsgPath + '\';

    FGetAllState := 0;
    FFileOpened  := FALSE;
    SslPop3Client.OnRequestDone  := GetAllRequestDone;
    SslPop3Client.OnMessageBegin := nil;
    SslPop3Client.OnMessageEnd   := nil;
    SslPop3Client.OnMessageLine  := GetAllMessageLine;
    SslPop3Client.Stat;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a request related to GetAll is done.    }
{ We check for errors and our state variable FGetAllState which tells us    }
{ where we are (stat, uidl or retr which are the 4 commands we use.         }
{ Note that we also could use Dele to remove the messages from the server.  }
procedure TPOP3ExcercizerForm.GetAllRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    if ErrCode <> 0 then begin
        if FFileOpened then begin
            FFileOpened := FALSE;
            CloseFile(FFile);
        end;
        DisplayMemo.Lines.Add('Error ' + String(SslPop3Client.ErrorMessage));
        Exit;
    end;

    try
        case FGetAllState of
        0: begin     { Comes from the Stat command }
                if SslPop3Client.MsgCount < 1 then begin
                    DisplayMemo.Lines.Add('No message to download.');
                    Exit;
                end;
                SslPop3Client.MsgNum := 1;    { Start with first message }
                FGetAllState := 1;
                SslPop3Client.Uidl;
           end;
        1: begin     { Comes from the Uidl command }
                FFileName := FMsgPath + 'Msg ' + String(SslPop3Client.MsgUidl) + '.txt';
                if FileExists(FFileName) then begin
                    DisplayMemo.Lines.Add('Message ' + IntToStr(SslPop3Client.MsgNum) + ' already here');
                    if SslPop3Client.MsgNum >= SslPop3Client.MsgCount then begin
                        DisplayMemo.Lines.Add('Finished');
                        Exit;
                    end;
                    SslPop3Client.MsgNum := SslPop3Client.MsgNum + 1;
                    FGetAllState := 1;
                    SslPop3Client.Uidl;
                end
                else begin
                    DisplayMemo.Lines.Add('Message ' + IntToStr(SslPop3Client.MsgNum));
                    AssignFile(FFile, FFileName);
                    Rewrite(FFile);
                    FFileOpened  := TRUE;
                    FGetAllState := 2;
                    SslPop3Client.Retr;
                end;
           end;
        2: begin     { Comes from the Retr command }
                FFileOpened := FALSE;
                CloseFile(FFile);
                if SslPop3Client.MsgNum >= SslPop3Client.MsgCount then begin
                    DisplayMemo.Lines.Add('Finished');
                    Exit;
                end;
                SslPop3Client.MsgNum := SslPop3Client.MsgNum + 1;
                FGetAllState := 1;
                SslPop3Client.Uidl;
           end;
        else
            DisplayMemo.Lines.Add('Invalid state');
            Exit;
        end;
    except
        on E:Exception do begin
            if FFileOpened then begin
                FFileOpened := FALSE;
                CloseFile(FFile);
            end;
            DisplayMemo.Lines.Add('Error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    DisplayMemo.Lines.Add('Request Done Rq=' + IntToStr(Integer(RqType)) +
                          ' Error=' + IntToStr(ErrCode) + ' LastResponse="' +
                          String(SslPop3Client.LastResponse) + '" ErrorMessage="' +
                          String(SslPop3Client.ErrorMessage) + '" Connected=' +
                          BoolToStr(SslPop3Client.Connected, TRUE));

    if RqType = pop3Stat then begin
        InfoLabel.Caption := 'Stat ok, ' +
                             IntToStr(SslPop3Client.MsgCount) + ' messages ' +
                             IntToStr(SslPop3Client.MsgSize) + ' bytes'
    end
    else if RqType = pop3List then begin
        InfoLabel.Caption := 'List ok, ' +
                             IntToStr(SslPop3Client.MsgNum)  + ' message ' +
                             IntToStr(SslPop3Client.MsgSize) + ' bytes'
    end
    else if RqType = pop3Last then begin
        InfoLabel.Caption := 'Last = ' + IntToStr(SslPop3Client.MsgNum);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientHeaderEnd(Sender: TObject);
begin
    SubjectEdit.Text := String(SslPop3Client.HeaderSubject);
    FromEdit.Text    := String(SslPop3Client.HeaderFrom);
    ToEdit.Text      := String(SslPop3Client.HeaderTo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.AuthButtonClick(Sender: TObject);
begin
    Exec(SslPop3Client.Auth, 'Auth');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
begin
    if (ErrCode = 0) then
        if PeerCert.PostConnectionCheck(SslPop3Client.Host) then
            DisplayMemo.Lines.Add('! Post connection check ok')
        else begin
            DisplayMemo.Lines.Add('! Post connection check **failed** - ' +
                                  'Hostname "' + SslPop3Client.Host +
                                  '" not found in peer certificate');
            Disconnect := TRUE;
        end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.SslPop3ClientSslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    DisplayMemo.Lines.Add(
            'Received certificate - Depth: ' +
            IntToStr(Cert.VerifyDepth)+ #13#10 +
            'Subject: "' + Cert.SubjectOneLine + '"'#13#10 +
            'Issuer:  "' + Cert.IssuerOneLine + '"'#13#10  +
            'Verify result: ' + Cert.VerifyErrMsg);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ called by SMTP component to when it needs an OAuth2 token }
procedure TPOP3ExcercizerForm.SslPop3ClientGetNewToken(Sender: TObject);        { V8.65 }
begin
    if NOT IcsRestEmail1.GetNewToken(True) then   // allow interaction, waits for broweser window to be completed
        DisplayMemo.Lines.Add('Failed to get OAuth2 Bearer Token')
    else begin
        if (Pos (IcsRestEmail1.NewAccEmail, SslPop3Client.UserName) = 0) and
                                              (IcsRestEmail1.NewAccEmail <> '') then
            DisplayMemo.Lines.Add('OAuth2 Token for Wrong Account, Expected: ' +
                 SslPop3Client.Username + ', Got: ' + IcsRestEmail1.NewAccEmail)
        else  begin
            SslPop3Client.OAuthToken := IcsRestEmail1.AccToken;
            SslPop3Client.TokenExpireDT := IcsRestEmail1.AccExpireDT;
            DisplayMemo.Lines.Add('Got New OAuth2 Bearer Token OK');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ called by TIcsRestEmail when it has a new token we should save }
procedure TPOP3ExcercizerForm.IcsRestEmail1EmailNewToken(Sender: TObject);      { V8.65 }
begin
    if IcsRestEmail1.RefrToken <> '' then begin
        PasswordEdit.Text := IcsRestEmail1.RefrToken;
        DisplayMemo.Lines.Add('Saved OAuth2 Refresh Token');
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.IcsRestEmail1EmailProg(Sender: TObject;           { V8.65 }
  LogOption: TLogOption; const Msg: string);
begin
    DisplayMemo.Lines.Add(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

