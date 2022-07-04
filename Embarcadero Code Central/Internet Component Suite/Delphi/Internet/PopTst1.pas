{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

   +-------------------------------------------------------------------+
   | THIS IS AN OUTDATED APPLICATION USING AN OUTDATED COMPONENT.      |
   | NEW COMPONENT IS IN POP3PROT.PAS FILE. NEW DEMO IS MAILRCV.DPR.   |
   +-------------------------------------------------------------------+

Author:       François PIETTE
Object:       Show how to use TPop3Cli (POP3 protocol, RFC-1225)
Creation:     03 october 1997
Version:      1.02
EMail:        francois.piette@pophost.eunet.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit PopTst1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, WSocket, pop3cli, StdCtrls, Wait, IniFiles, ExtCtrls;

const
    PopTstVersion = 102;

type
  TPOP3ExcercizerForm = class(TForm)
    Pop3Client: TPop3Client;
    DisplayMemo: TMemo;
    Panel1: TPanel;
    InfoLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ConnectButton: TButton;
    Wait1: TWait;
    DisconnectButton: TButton;
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
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Pop3ClientDisplay(Sender: TObject; Msg: String);
    procedure UidlButtonClick(Sender: TObject);
    procedure Pop3ClientUidlBegin(Sender: TObject);
    procedure Pop3ClientUidlEnd(Sender: TObject);
    procedure Pop3ClientUidlLine(Sender: TObject);
    procedure ApopButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure GetAllButtonClick(Sender: TObject);
  private
    FFile     : TextFile;
    FFileName : String;
    function  DoTheJob(MethodPtr : TPop3Method; MethodName : String) : Boolean;
    procedure MessageBegin(Sender: TObject);
    procedure MessageLine(Sender: TObject);
    procedure GetAllMessageLine(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  POP3ExcercizerForm: TPOP3ExcercizerForm;

implementation

{$R *.DFM}

uses
    PopTst2;

const
    IniFileName = 'POPTST.INI';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Restore some data from the INI file                                       }
procedure TPOP3ExcercizerForm.FormCreate(Sender: TObject);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    HostEdit.Text     := IniFile.ReadString('Data', 'Host',     '');
    PortEdit.Text     := IniFile.ReadString('Data', 'Port',     '');
    UserNameEdit.Text := IniFile.ReadString('Data', 'UserName', '');
    PassWordEdit.Text := IniFile.ReadString('Data', 'Password', '');
    IniFile.Free;
    InfoLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Save data to INI file                                                     }
procedure TPOP3ExcercizerForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteString('Data', 'Host',     HostEdit.Text);
    IniFile.WriteString('Data', 'Port',     PortEdit.Text);
    IniFile.WriteString('Data', 'UserName', UserNameEdit.Text);
    IniFile.WriteString('Data', 'Password', PassWordEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the TPop3Client object wants to display }
{ some information such as connection progress or errors.                   }
procedure TPOP3ExcercizerForm.Pop3ClientDisplay(Sender: TObject;
  Msg: String);
begin
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All the TPop3Client method are of the same type. To simplify this demo    }
{ application, DoTheJob transfert the parameters form the various EditBoxes }
{ to the Pop3Client instance and then call the appropriate method, showing  }
{ the result in the InfoLabel.Caption.                                      }
function TPOP3ExcercizerForm.DoTheJob(
    MethodPtr  : TPop3Method;
    MethodName : String) : Boolean;
begin
    Pop3Client.Host     := HostEdit.Text;
    Pop3Client.Port     := PortEdit.Text;
    Pop3Client.UserName := UserNameEdit.Text;
    Pop3Client.PassWord := PassWordEdit.Text;
    Pop3Client.MsgNum   := StrToInt(MsgNumEdit.Text);
    Pop3Client.MsgLines := StrToInt(MsgLinesEdit.Text);
    InfoLabel.Caption   := MethodName + ' started';
    Result := MethodPtr;
    if Result then
        InfoLabel.Caption := MethodName + ' ok'
    else
        InfoLabel.Caption := MethodName + ' failed';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ConnectButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Connect, 'Connect');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.DisconnectButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Quit, 'Quit');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UserButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.User, 'User');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.PassButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Pass, 'Pass');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RetrButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Retr, 'Retr');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.StatButtonClick(Sender: TObject);
begin
    if DoTheJob(Pop3Client.Stat, 'Stat') then
        InfoLabel.Caption := 'Stat ok, ' +
                             IntToStr(Pop3Client.MsgCount) + ' messages ' +
                             IntToStr(Pop3Client.MsgSize) + ' bytes'
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListAllButtonClick(Sender: TObject);
begin
    MsgNumEdit.Text := '0';
    DoTheJob(Pop3Client.List, 'List All');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ListButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.List, 'List');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.DeleteButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Dele, 'Delete');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NoopButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Noop, 'Noop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.LastButtonClick(Sender: TObject);
begin
    if DoTheJob(Pop3Client.Last, 'Last') then
        InfoLabel.caption := 'Last = ' + IntToStr(Pop3Client.MsgNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ResetButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Rset, 'Rset');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.TopButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Top, 'Top');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.RpopButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Rpop, 'Rpop');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.UidlButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Uidl, 'Uidl');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.ApopButtonClick(Sender: TObject);
begin
    DoTheJob(Pop3Client.Apop, 'Apop');
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
                          IntToStr((Sender as TPop3Client).MsgNum) +
                          ' begin ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when TPop3Client has detected the end of a   }
{ message, even if there is an error or exception, this event gets called.  }
{ This event handler could be used to close the file used to store the msg. }
procedure TPOP3ExcercizerForm.Pop3ClientMessageEnd(Sender: TObject);
begin
    DisplayMemo.Lines.Add('*** Message ' +
                          IntToStr((Sender as TPop3Client).MsgNum) +
                          ' end ***');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called for each message line that TPop3Client is    }
{ receiveing. This could be used to write the message lines to a file.      }
procedure TPOP3ExcercizerForm.Pop3ClientMessageLine(Sender: TObject);
begin
    DisplayMemo.Lines.Add((Sender as TPop3Client).LastResponse);
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
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Client).MsgNum) + ' ' +
              'MsgSize = ' + IntToStr((Sender as TPop3Client).MsgSize) + ' ' +
              'Line = ''' + (Sender as TPop3Client).LastResponse + '''';
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
    Buffer := 'MsgNum = ' + IntToStr((Sender as TPop3Client).MsgNum) + ' ' +
              'MsgUidl = ' + (Sender as TPop3Client).MsgUidl + '''';
    DisplayMemo.Lines.Add(Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.MessageBegin(Sender: TObject);
begin
    MessageForm.Caption := 'Message ' +
                           IntToStr((Sender as TPop3Client).MsgNum);
    MessageForm.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.MessageLine(Sender: TObject);
begin
    MessageForm.DisplayMemo.Lines.Add((Sender as TPop3Client).LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.NextButtonClick(Sender: TObject);
var
    OldBegin : TNotifyEvent;
    OldEnd   : TNotifyEvent;
    OldLine  : TNotifyEvent;
begin
    OldBegin := Pop3Client.OnMessageBegin;
    OldEnd   := Pop3Client.OnMessageEnd;
    OldLine  := Pop3Client.OnMessageLine;

    Pop3Client.OnMessageBegin := MessageBegin;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := MessageLine;

    MessageForm.DisplayMemo.Clear;
    MessageForm.Caption := 'Message';

    DoTheJob(Pop3Client.Retr, 'Retr');

    if (Length(Pop3Client.LastResponse) > 0) and
       (Pop3Client.LastResponse[1] = '-') then
        MessageForm.DisplayMemo.Lines.Add(Pop3Client.LastResponse)
    else
        MsgNumEdit.Text := IntToStr(StrToInt(MsgNumEdit.Text) + 1);

    MessageForm.DisplayMemo.SelStart  := 0;
    MessageForm.DisplayMemo.SelLength := 0;

    Pop3Client.OnMessageBegin := OldBegin;
    Pop3Client.OnMessageEnd   := OldEnd;
    Pop3Client.OnMessageLine  := OldLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.GetAllMessageLine(Sender: TObject);
begin
    Writeln(FFile, (Sender as TPop3Client).LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPOP3ExcercizerForm.GetAllButtonClick(Sender: TObject);
var
    MsgCnt   : Integer;
    MsgNum   : Integer;
    Uidl     : String;
    OldBegin : TNotifyEvent;
    OldEnd   : TNotifyEvent;
    OldLine  : TNotifyEvent;
    IniFile  : TIniFile;
    Path     : String;
begin
    if not DoTheJob(Pop3Client.Stat, 'Stat') then
        Exit;

    InfoLabel.Caption := 'Stat ok, ' +
                         IntToStr(Pop3Client.MsgCount) + ' messages ' +
                         IntToStr(Pop3Client.MsgSize) + ' bytes';
    if Pop3Client.MsgCount < 1 then
        Exit;

    { Get path from INI file }
    IniFile := TIniFile.Create(IniFileName);
    Path    := IniFile.ReadString('Data', 'MsgPath',
                                  ExtractFilePath(Application.ExeName));
    IniFile.Free;

    { Be sure to have an ending backslash }
    if (Length(Path) > 0) and (Path[Length(Path)] <> '\') then
        Path := Path + '\';

    OldBegin := Pop3Client.OnMessageBegin;
    OldEnd   := Pop3Client.OnMessageEnd;
    OldLine  := Pop3Client.OnMessageLine;
    Pop3Client.OnMessageBegin := nil;
    Pop3Client.OnMessageEnd   := nil;
    Pop3Client.OnMessageLine  := GetAllMessageLine;

    try
        MsgCnt := Pop3Client.MsgCount;
        for MsgNum := 1 to MsgCnt do begin
            Pop3Client.MsgNum := MsgNum;
            if not Pop3Client.Uidl then
                continue;
            Uidl := Pop3Client.MsgUidl;
            FFileName := Path + 'Msg ' + Uidl + '.txt';
            if FileExists(FFileName) then begin
                DisplayMemo.Lines.Add('Message ' + IntToStr(MsgNum) + ' already here');
                continue;
            end;

            DisplayMemo.Lines.Add('Message ' + IntToStr(Pop3Client.MsgNum));
            AssignFile(FFile, FFileName);
            Rewrite(FFile);
            try
                Pop3Client.Retr;
            finally
                CloseFile(FFile);
            end;
        end;
        DoTheJob(Pop3Client.Quit, 'Quit');
    finally
        Pop3Client.OnMessageBegin := OldBegin;
        Pop3Client.OnMessageEnd   := OldEnd;
        Pop3Client.OnMessageLine  := OldLine;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

