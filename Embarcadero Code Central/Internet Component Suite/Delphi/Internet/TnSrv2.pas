{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TnSrv implement a (very basic) Telnet server (daemon)
              Compatible with both Delphi 1 and Delphi 2
              Uses TWSocket to communicate with WinSock
Creation:     April 1996
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996, 1997, 1998, 1999 by François PIETTE
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
Updates:
Sep 18, 1996 Accept CR/LF or LF only as line terminator
             Implement Help and Exit commands as demo
Mar 19, 1997 Use enhanced TWSocket object
Oct 03, 1997 V1.22 Added a $DEFINE POP3 to simulate a POP3 server
Oct 09, 1997 Added a $DEFINE SMTP to simulate a SMTP server
Oct 11, 1997 Added PortNum to specify which port we serve
             Added pseudo POP3 and SMTP interpreters (nothing really happens
             except transmission of pseudo correct answers. I use this
             feature to debug SMTP and POP3 components).
Jul 30, 1998 V1.24 Added some code to the dummy SMTP server
Sep 26, 2000 V1.26 Replaced TEdit by TMemo for data to be sent to allow
             multi-line sending.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnSrv2;


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, WSocket, Winsock, ExtCtrls;

const
  SMTP_PORT = 25;
  POP3_PORT = 110;

  { The message WM_DISCONNECT is used by the client form to tell the server }
  { form that the client has disconnected or should be disconnected.        }
  WM_DISCONNECT     = WM_USER + 2;
  DISCONNECT_SELF   = 1;          { Client form ask to disconnect           }
  DISCONNECT_REMOTE = 2;          { Client user (remote) has disconnected   }

type
  { A new TClientForm will be instanciated for each new client connection. }
  { Instanciation is done from TClient constructor.                        }
  TClientForm = class(TForm)
    Memo: TMemo;
    Socket: TWSocket;
    Panel1: TPanel;
    Button1: TButton;
    SendButton: TButton;
    DisconnectButton: TButton;
    DataMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Display(Msg : String);
    procedure FormDestroy(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SocketDataAvailable(Sender: TObject; Error: Word);
    procedure SocketSessionClosed(Sender: TObject; Error: Word);
    procedure Button1Click(Sender: TObject);
  private
    FCommand  : String;
    FRcvdCR   : Boolean;
    FDataFlag : Boolean;
    procedure ProcessChar(Ch : Char);
    procedure CommandInterpreter;
    procedure SMTP_Interpreter(CommandVerb : String; CommandTail : String);
    procedure POP3_Interpreter(CommandVerb : String; CommandTail : String);
    procedure TELNET_Interpreter(CommandVerb : String; CommandTail : String);
  public
    AcceptForm : TForm;
    Reference  : Pointer;
    PortNum    : Integer;
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
procedure SetLength(var Str : String; Len : Integer);
begin
    Str[0] := chr(Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
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
function atoi(value : string) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormCreate(Sender: TObject);
begin
    Memo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure display text in a memo control.                            }
{ I should add code to remove lines when there are too much of them...      }
procedure TClientForm.Display(Msg : String);
var
    Start, Stop : Integer;
    SelStart : Integer;
begin
    if Memo.Lines.Count = 0 then
        Memo.Lines.Add('')
    else if Memo.Lines.Count > 200 then
        Memo.Clear;

    Start := 1;
    Stop  := Pos(#13, Msg);
    if Stop = 0 then
        Stop := Length(Msg) + 1;
    while Start <= Length(Msg) do begin
        Memo.Lines.Strings[Memo.Lines.Count - 1] := Memo.Lines.Strings[Memo.Lines.Count - 1] + Copy(Msg, Start, Stop - Start);
        if (Stop <= Length(Msg)) and (Msg[Stop] = #13) then begin
            SelStart := Memo.SelStart;
            Memo.Lines.Add('');
            Memo.SelStart := SelStart + 2;
        end;
        Start := Stop + 1;
        if Start > Length(Msg) then
            Break;
        if Msg[Start] = #10 then
           Start := Start + 1;
        Stop := Start;
        while (Stop <= Length(Msg)) and (Msg[Stop] <> #13) do
            Stop := Stop + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Not a real SMTP command interpreter, just enough for me to test my SMTP   }
{ component.                                                                }
procedure TClientForm.SMTP_Interpreter(
    CommandVerb : String;
    CommandTail : String);
var
    Response    : String;
begin
    if FDataFlag then begin
        { We should add storage here of course...              }
        { SMTP data ends with a line having being a single dot }
        if FCommand = '.' then begin
            Response := '250 Data received ok';
            Socket.SendStr(Response + #13 + #10);
            FDataFlag := FALSE;
        end;
    end
    else begin
        FDataFlag := FALSE;
        if CommandVerb = 'MAIL' then
            Response := '250 Ok'
        else if CommandVerb = 'RCPT' then
            Response := '250 Ok'
        else if CommandVerb = 'DATA' then begin
            Response := '354 Send data now';
            FDataFlag := TRUE;
        end
        else if CommandVerb = 'HELO' then
            Response := '250 Ok'
        else if CommandVerb = 'QUIT' then
            Response := '221 Goodbye'
        else
            Response := '500 syntax error';

        Socket.SendStr(Response + #13 + #10);
        if CommandVerb = 'QUIT' then
            Socket.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Not a real POP3 command interpreter, just enough for me to test my POP3   }
{ component.                                                                }
procedure TClientForm.POP3_Interpreter(
    CommandVerb : String;
    CommandTail : String);
var
    Response    : String;
begin
    if CommandVerb = 'USER' then
        Response := '+OK'
    else if CommandVerb = 'PASS' then
        Response := '+OK'
    else if CommandVerb = 'RETR' then begin
        Socket.SendStr('+OK' + #13 + #10);
        Socket.SendStr('This is the message body.' + #13 + #10);
        Socket.SendStr('This is the last message line.' + #13 + #10);
        Response := '.'
    end
    else if CommandVerb = 'LIST' then begin
        if Trim(CommandTail) = '' then begin
            Socket.SendStr('+OK 2 messages (320 octets)' + #13 + #10);
            Socket.SendStr('1 120' + #13 + #10);
            Socket.SendStr('2 200' + #13 + #10);
            Response := '.'
        end
        else
            Response := '+OK ' + CommandTail + ' 200'
    end
    else if CommandVerb = 'STAT' then
        Response := '+OK 10 12345'
    else if CommandVerb = 'QUIT' then
        Response := '+OK'
    else if CommandVerb = 'TOP' then begin
        if atoi(CommandTail) <= 0 then
            Response := '-ERR Message doesn''t exists'
        else begin
            Socket.SendStr('+OK' + #13 + #10);
            Socket.SendStr('This is the message body.' + #13 + #10);
            Socket.SendStr('This is the last message line.' + #13 + #10);
            Response := '.'
        end;
    end
    else if CommandVerb = 'RPOP' then
        Response := '+OK'
    else if CommandVerb = 'APOP' then
        Response := '+OK'
    else if CommandVerb = 'DELE' then
        Response := '+OK'
    else if CommandVerb = 'LAST' then
        Response := '+OK 1'
    else if CommandVerb = 'NOOP' then
        Response := '+OK'
    else if CommandVerb = 'UIDL' then
        Response := '+OK ' + Trim(CommandTail) + ' Msg' + Trim(CommandTail)
    else if CommandVerb = 'RSET' then
        Response := '+OK'
    else
        Response := '-ERR';

    Socket.SendStr(Response + #13 + #10);
    if CommandVerb = 'QUIT' then
        Socket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Not a real TELNET command interpreter, just enough to see how it could    }
{ be implemented.                                                           }
procedure TClientForm.TELNET_Interpreter(
    CommandVerb : String;
    CommandTail : String);
begin
    Socket.SendStr(#13 + #10 + 'Executing command ''' + CommandVerb + '''...' +
                   #13 + #10);

    if CommandVerb = 'EXIT' then
        DisconnectButtonClick(Self)
    else if CommandVerb = 'HELP' then
        Socket.SendStr('List of commands:' + #13 + #10 +
                       '    Exit      logoff from server' + #13 + #10 +
                       '    Help      show this help screen' + #13 + #10)
    else
        Socket.SendStr('Unknown command, ignoring');

    Socket.SendStr(#13 + #10 + '--> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the command line interpreter. Should extend the code to support   }
{ every command needed...                                                   }
procedure TClientForm.CommandInterpreter;
var
    CommandVerb : String;
    CommandTail : String;
    I, J        : Integer;
begin
    CommandVerb := FCommand;

    { Skip leading spaces }
    I := 1;
    while (I <= Length(CommandVerb)) and (CommandVerb[I] in [' ', #9]) do
        Inc(I);

    { Find separator and separe CommandVerb and CommandTail }
    J := I;
    while TRUE do begin
        if (J >= Length(CommandVerb)) then begin
            CommandTail := '';
            break;
        end;

        if CommandVerb[J] in [' ', #9, '/'] then begin
            CommandTail := Copy(CommandVerb, J, Length(CommandVerb) - J + 1);
            CommandVerb := Copy(CommandVerb, I, J - I);
            break;
        end;
        Inc(J);
    end;
    CommandVerb := UpperCase(CommandVerb);

    if PortNum = SMTP_PORT then
        SMTP_Interpreter(CommandVerb, CommandTail)
    else if PortNum = POP3_PORT then
        POP3_Interpreter(CommandVerb, CommandTail)
    else
        TELNET_Interpreter(CommandVerb, CommandTail);

    FCommand := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Process each charcter received to do minimal line editing                }
procedure TClientForm.ProcessChar(Ch : Char);
begin
    if Ch = #8 then begin
        if Length(FCommand) > 0 then begin
            SetLength(FCommand, Length(FCommand) - 1);
            Socket.SendStr(#8 + ' ' + #8);
        end
        else
            Socket.SendStr(#7);
        Exit;
    end
    else if (Ch = #10) and FRcvdCR then begin
        { Ignore LF just after CR (CR/LF is normal end of line) }
        FRcvdCR := FALSE;
        Exit;
    end
    else if Ch = #13 then begin
        FRcvdCR := TRUE;
        CommandInterpreter;
        Exit;
    end
    else if Ch = #10 then begin
        CommandInterpreter;
        Exit;
    end;

    { Ordinary character, put in buffer in some place left }
{$IFNDEF WIN32}
    if Length(FCommand) = High(FCommand) then
        Ch := #7
    else
{$ENDIF}
        FCommand := FCommand + Ch;

    if (PortNum <> POP3_PORT) and (PortNum <> SMTP_PORT) then begin
        { Echo to client }
        Socket.Send(@Ch, 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Event handler for datavailable. Called each time some data is received  *}
procedure TClientForm.SocketDataAvailable(Sender: TObject; Error : word);
var
    Len    : Integer;
    Buffer : String[255];
    Socket : TWSocket;
    I      : Integer;
begin
    Socket := Sender as TWSocket;
    Len := Socket.Receive(@Buffer[1], High(Buffer));
    if Len = 0 then begin
        { Remote has closed }
        Display(#13 + #10 + '**** Remote has closed ****' + #13 + #10);
    end
    else if Len < 0 then begin
        { An error has occured }
        if Socket.LastError <> WSAEWOULDBLOCK then
            Display(#13 + #10 + '**** ERROR: ' + IntToStr(Socket.LastError) +
                    ' ****' + #13 + #10);
    end
    else begin
        Buffer[0] := chr(Len);
        Display(Buffer);
        for I := 1 to Len do
            ProcessChar(Buffer[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Event handler called when the remote has closed the connection          *}
procedure TClientForm.SocketSessionClosed(Sender: TObject; Error : word);
begin
    Display(#13 + #10 + '**** Remote has closed ****' + #13 + #10);
    PostMessage(AcceptForm.Handle, WM_DISCONNECT,
                                   DISCONNECT_REMOTE,
                                   LongInt(Reference));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormDestroy(Sender: TObject);
begin
    Socket.Shutdown(2);
    Socket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.DisconnectButtonClick(Sender: TObject);
begin
    { Post a message to server form asking to disconnect the client }
    PostMessage(AcceptForm.Handle, WM_DISCONNECT,
                                   DISCONNECT_SELF,
                                   LongInt(Reference));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormShow(Sender: TObject);
var
    Buf : String;
begin
    DataMemo.Text := '';
    ActiveControl := DataMemo;

    if PortNum = POP3_PORT then
        Buf := '+OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>' + #13 + #10
    else if PortNum = SMTP_PORT then begin
        Buf := '220-SMTP Simulator ready' + #13 + #10;
        Socket.Send(@Buf[1], Length(Buf));
        Buf := '220 ESMTP spoken here' + #13 + #10;
    end
    else
        Buf := 'Hello from TnSrv !' + #13 + #10 + '--> ';

    Socket.Send(@Buf[1], Length(Buf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.SendButtonClick(Sender: TObject);
var
    Buf : String;
begin
    Buf := DataMemo.Text + #13 + #10;
    Socket.Send(@Buf[1], Length(Buf));
    DataMemo.Text := '';
    ActiveControl := DataMemo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Adjust the position for each control in the form as the user resize it   *}
procedure TClientForm.FormResize(Sender: TObject);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.Button1Click(Sender: TObject);
begin
    Socket.SendStr('Hello !');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

