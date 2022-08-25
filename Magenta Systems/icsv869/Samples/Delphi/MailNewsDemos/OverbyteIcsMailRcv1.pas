{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       Show how to use TPop3Cli (POP3 protocol, RFC-1225)
Creation:     03 october 1997
Version:      8.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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
Jul 04, 2010  V6.01 Use TFileStream and updated to support TPop3Cli V6.07
Mar 19, 2013  V8.01 Added OpenEx, Login, Capa buttons

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMailRcv1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, OverbyteIcsIniFiles,
{$IFDEF CLR}
  System.IO,
  System.ComponentModel,
{$ENDIF}
  OverbyteIcsMimeDec,
  OverbyteIcsPop3Prot,
  OverbyteIcsWndControl;

const
    MailRcvVersion = 801;
    CopyRight : String = ' MailRcv demo (c) 1997-2013 F. Piette V8.01 ';

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
    Pop3Client: TPop3Cli;
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
    OpenExButton: TButton;
    CapaButton: TButton;
    LoginButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuittButtonClick(Sender: TObject);
    procedure UserButtonClick(Sender: TObject);
    procedure PassButtonClick(Sender: TObject);
    procedure Pop3ClientMessageBegin(Sender: TObject);
    procedure Pop3ClientMessageEnd(Sender: TObject);
    procedure Pop3ClientMessageLine(Sender: TObject);
    procedure RetrButtonClick(Sender: TObject);
    procedure StatButtonClick(Sender: TObject);
    procedure ListAllButtonClick(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure Pop3ClientListBegin(Sender: TObject);
    procedure Pop3ClientListEnd(Sender: TObject);
    procedure Pop3ClientListLine(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure NoopButtonClick(Sender: TObject);
    procedure LastButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure TopButtonClick(Sender: TObject);
    procedure RpopButtonClick(Sender: TObject);
    procedure Pop3ClientDisplay(Sender: TObject; const Msg: String);
    procedure UidlButtonClick(Sender: TObject);
    procedure Pop3ClientUidlBegin(Sender: TObject);
    procedure Pop3ClientUidlEnd(Sender: TObject);
    procedure Pop3ClientUidlLine(Sender: TObject);
    procedure ApopButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure GetAllButtonClick(Sender: TObject);
    procedure Pop3ClientRequestDone(Sender: TObject; RqType: TPop3Request;
      ErrCode: Word);
    procedure OpenButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure Pop3ClientHeaderEnd(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure OpenExButtonClick(Sender: TObject);
    procedure CapaButtonClick(Sender: TObject);
    procedure LoginButtonClick(Sender: TObject);
    procedure Pop3ClientCapaLine(Sender: TObject);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FFileStream   : TFileStream;
    FFileName     : String;   
    FGetAllState  : Integer;
    FMsgPath      : String;
    procedure Exec(MethodPtr  : TPop3NextProc;
                   MethodName : String);
    procedure MessageBegin(Sender: TObject);
    procedure MessageLine(Sender: TObject);
    procedure GetAllMessageLine(Sender: TObject);
    procedure GetAllRequestDone(Sender: TObject;
                                RqType: TPop3Request; ErrCode: Word);
    procedure NextMessageRequestDone(Sender: TObject;
                                     RqType: TPop3Request; ErrCode: Word);
  end;

var
  POP3ExcercizerForm: TPOP3ExcercizerForm;

implementation

{$R *.DFM}

uses
    OverbyteIcsMailRcv2;

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
    if not FInitialized then begin
        IniFile := TIcsIniFile.Create(FIniFileName);
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
                                                     'pop3');
        UserNameEdit.Text      := IniFile.ReadString(SectionData, KeyUserName,
                                                     '');
        PassWordEdit.Text      := IniFile.ReadString(SectionData, KeyPassword,
                                                     '');
        AuthComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyAuth,
                                                      Ord(popAuthNone));
        IniFile.Free;
        InfoLabel.Caption := '';
        SubjectEdit.Text  := '';
        FromEdit.Text     := '';
        ToEdit.Text       := '';
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteString(SectionData,  KeyHost,     HostEdit.Text);
    IniFile.WriteString(SectionData,  KeyPort,     PortEdit.Text);
    IniFile.WriteString(SectionData,  KeyUserName, UserNameEdit.Text);
    IniFile.WriteString(SectionData,  KeyPassword, PassWordEdit.Text);
    IniFile.WriteInteger(SectionData, KeyAuth,     AuthComboBox.ItemIndex);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientCapaLine(Sender: TObject);
begin
    DisplayMemo.Lines.Add(String(Pop3Client.LastResponse));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the TPop3Client object wants to display }
{ some information such as connection progress or errors.                   }
procedure TPOP3ExcercizerForm.Pop3ClientDisplay(
    Sender    : TObject;
    const Msg : String);
begin
    DisplayMemo.Lines.Add(Msg);
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

    Pop3Client.Host           := HostEdit.Text;
    Pop3Client.Port           := PortEdit.Text;
    Pop3Client.UserName       := UserNameEdit.Text;
    Pop3Client.PassWord       := PassWordEdit.Text;
    Pop3Client.AuthType       := TPop3AuthType(AuthComboBox.ItemIndex);
    Pop3Client.MsgNum         := StrToInt(MsgNumEdit.Text);
    Pop3Client.MsgLines       := StrToInt(MsgLinesEdit.Text);
    { We need to reassign event handlers because we may have changed them }
    { doing GetAllMessages for example                                    }
    Pop3Client.OnRequestDone  := Pop3ClientRequestDone;
    Pop3Client.OnMessageBegin := Pop3ClientMessageBegin;
    Pop3Client.OnMessageEnd   := Pop3ClientMessageEnd;
    Pop3Client.OnMessageLine  := Pop3ClientMessageLine;
    InfoLabel.Caption         := MethodName + ' started';
    try
        MethodPtr;
        InfoLabel.Caption := MethodName + ' ok';
    except
        on E:Exception do begin
            MessageBeep(MB_OK);
            InfoLabel.Caption := MethodName + ' failed (' + E.Message + ')';
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.CapaButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Capa, 'Capability');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ConnectButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Connect, 'Connect');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.OpenButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Open, 'Open');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.OpenExButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.OpenEx, 'OpenEx');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UserButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.User, 'User');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.PassButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Pass, 'Pass');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.QuittButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Quit, 'Quit');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.AbortButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Abort, 'Abort');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RetrButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Retr, 'Retr');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.StatButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Stat, 'Stat');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListAllButtonClick(Sender: TObject);
begin
    MsgNumEdit.Text := '0';
    Exec(Pop3Client.List, 'List All');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.List, 'List');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.LoginButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Login, 'Login');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.DeleteButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Dele, 'Delete');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NoopButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Noop, 'Noop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.LastButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Last, 'Last');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ResetButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.RSet, 'Rset');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.TopButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Top, 'Top');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RpopButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.RPop, 'Rpop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UidlButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Uidl, 'Uidl');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ApopButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.APop, 'Apop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ message. The MsgNum property gives the message number.                    }
{ This event handler could be used to open the file used to store the msg.  }
{ The file handle could be stored in the TPop3Client.Tag property to be     }
{ easily retrieved by the OnMessageLine and OnMessageEnd event handlers.    }
procedure TPOP3ExcercizerForm.Pop3ClientMessageBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Message ' +
                          IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has detected the end of a   }
{ message, even if there is an error or exception, this event gets called.  }
{ This event handler could be used to close the file used to store the msg. }
procedure TPOP3ExcercizerForm.Pop3ClientMessageEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Message ' +
                          IntToStr((Sender as TPop3Cli).MsgNum) +
                          ' end ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each message line that TPop3Client is    }
{ receiveing. This could be used to write the message lines to a file.      }
procedure TPOP3ExcercizerForm.Pop3ClientMessageLine(Sender: TObject);
begin
    DisplayMemo.Lines.Add(String((Sender as TPop3Cli).LastResponse));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client is about to receive a       }
{ list line. The MsgNum property gives the message number.                  }
procedure TPOP3ExcercizerForm.Pop3ClientListBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** List begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has received the last list  }
{ line.                                                                     }
procedure TPOP3ExcercizerForm.Pop3ClientListEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** List End ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each list line received by TPop3Client.  }
procedure TPOP3ExcercizerForm.Pop3ClientListLine(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Cli).MsgNum) + ' ' +
              'MsgSize = ' + IntToStr((Sender as TPop3Cli).MsgSize) + ' ' +
              'Line = ''' + String((Sender as TPop3Cli).LastResponse) + '''';
    DisplayMemo.Lines.Add(Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientUidlBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Uidl begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientUidlEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Uidl end ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientUidlLine(Sender: TObject);
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
    Pop3Client.OnMessageBegin := MessageBegin;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := MessageLine;
    Pop3Client.OnRequestDone  := NextMessageRequestDone;
    Pop3Client.MsgNum         := StrToInt(MsgNumEdit.Text);
    Pop3Client.Retr;
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
var
    S : AnsiString;
begin
    if Assigned(FFileStream) then begin
        S := (Sender as TPop3Cli).LastResponse;
        FFileStream.WriteBuffer(Pointer(S)^, Length(S));
        FFileStream.WriteBuffer(PAnsiChar(#13#10)^, 2);
    end;    
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
    Pop3Client.OnRequestDone  := GetAllRequestDone;
    Pop3Client.OnMessageBegin := nil;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := GetAllMessageLine;
    Pop3Client.Stat;
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
        FreeAndNil(FFileStream);
        DisplayMemo.Lines.Add('Error ' + String(Pop3Client.ErrorMessage));
        Exit;
    end;

    try
        case FGetAllState of
        0: begin     { Comes from the Stat command }
                if Pop3Client.MsgCount < 1 then begin
                    DisplayMemo.Lines.Add('No message to download.');
                    Exit;
                end;
                Pop3Client.MsgNum := 1;    { Start with first message }
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        1: begin     { Comes from the Uidl command }
                FFileName := FMsgPath + 'Msg ' + String(Pop3Client.MsgUidl) + '.txt';
                if FileExists(FFileName) then begin
                    DisplayMemo.Lines.Add('Message ' + IntToStr(Pop3Client.MsgNum) + ' already here');
                    if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                        DisplayMemo.Lines.Add('Finished');
                        Exit;
                    end;
                    Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                    FGetAllState := 1;
                    Pop3Client.Uidl;
                end
                else begin
                    DisplayMemo.Lines.Add('Message ' + IntToStr(Pop3Client.MsgNum));
                    FreeAndNil(FFileStream);
                    FFileStream := TFileStream.Create(FFileName, fmCreate);
                    FGetAllState := 2;
                    Pop3Client.Retr;
                end;
           end;
        2: begin     { Comes from the Retr command }
                FreeAndNil(FFileStream);
                if Pop3Client.MsgNum >= Pop3Client.MsgCount then begin
                    DisplayMemo.Lines.Add('Finished');
                    Exit;
                end;
                Pop3Client.MsgNum := Pop3Client.MsgNum + 1;
                FGetAllState := 1;
                Pop3Client.Uidl;
           end;
        else
            DisplayMemo.Lines.Add('Invalid state');
            Exit;
        end;
    except
        on E:Exception do begin
            FreeAndNil(FFileStream);
            DisplayMemo.Lines.Add('Error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientRequestDone(
    Sender  : TObject;
    RqType  : TPop3Request;
    ErrCode : Word);
begin
    DisplayMemo.Lines.Add('Request Done Rq=' + IntToStr(Integer(RqType)) +
                          ' Error=' + IntToStr(ErrCode));

    if RqType = pop3Stat then begin
        InfoLabel.Caption := 'Stat ok, ' +
                             IntToStr(Pop3Client.MsgCount) + ' messages ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes'
    end
    else if RqType = pop3List then begin
        InfoLabel.Caption := 'List ok, ' +
                             IntToStr(Pop3Client.MsgNum)  + ' message ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes'
    end
    else if RqType = pop3Last then begin
        InfoLabel.Caption := 'Last = ' + IntToStr(Pop3Client.MsgNum);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.Pop3ClientHeaderEnd(Sender: TObject);
begin
{$IFDEF UNICODE}
    SubjectEdit.Text := DecodeMimeInlineValue(Pop3Client.HeaderSubject);
    FromEdit.Text    := DecodeMimeInlineValue(Pop3Client.HeaderFrom);
    ToEdit.Text      := DecodeMimeInlineValue(Pop3Client.HeaderTo);

{$ELSE}
    SubjectEdit.Text := Pop3Client.HeaderSubject;
    FromEdit.Text    := Pop3Client.HeaderFrom;
    ToEdit.Text      := Pop3Client.HeaderTo;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.AuthButtonClick(Sender: TObject);
begin
    Exec(Pop3Client.Auth, 'Auth');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

