{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Copyright:    François PIETTE
Creation:     April 1996
Version:      1.26
Description:  TnSrv implement a (very basic) Telnet server (daemon)
              Compatible with both Delphi 1 and Delphi 2
              Uses TWSocket to communicate with WinSock
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
Sep 18, 1996 Added resize event and about box
Mar 19, 1997 V1.10 Use enhanced TWSocket object
Jul 22, 1997 V1.20 Adapted to Delphi 3
Sep 27, 1997 Adapted for TWSocket converted to support C++Builder
Oct 03, 1997 V1.22 Added a $DEFINE POP3 to simulate a POP3 server
                   Added an editbox to select the server port
Oct 09, 1997 Added a $DEFINE SMTP to simulate a SMTP server
Oct 11, 1997 V1.23 Added PortNum to client to tell him what he has to serve
Jul 30, 1998 V1.24 Added some code to the dummy SMTP server
Aug 20, 1999 V1.25 Added some comments, added Restartserver procedure.
Sep 26, 2000 V1.26 Replaced TEdit by TMemo for data to be sent to allow
             multi-line sending (see TnSrv2 source).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnSrv1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, WSocket, WinSock, StdCtrls, TnSrv2;

const
  TnSrvVersion = 126;

type
  { TClient class is used to handle client connections. A TClient is    }
  { instanciated for each client which connect.                         }
  { TnSrv keep track of all connected clients using Clients variable in }
  { TServerForm (see below).                                            }
  { TClient class will dynamically create a TClientForm from his        }
  { constructor. Since TClientForm include a TWSocket, a new TWSocket   }
  { is also created dynamically and automaticcaly when the form is      }
  { created.                                                            }
  TClient = class(TObject)
    Form      : TClientForm;
    Peer      : String;
    constructor Create(AOwner : TComponent);
    destructor  Destroy; override;
  end;

  TServerForm = class(TForm)
    Memo: TMemo;
    QuitButton: TButton;
    AboutButton: TButton;
    SrvSocket: TWSocket;
    PortLabel: TLabel;
    PortEdit: TEdit;
    ChangePortButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Display(Msg : String);
    procedure QuitButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AboutButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SrvSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure SrvSocketSessionClosed(Sender: TObject; Error: Word);
    procedure ChangePortButtonClick(Sender: TObject);
  protected
    procedure WMDisconnect(var msg: TMessage); message WM_DISCONNECT;
    procedure RestartServer;
  public
    Clients  : TList;   { List of all TClient object (one per connection) }
  end;

var
  ServerForm: TServerForm;

implementation

{$R *.DFM}
{DEFINE Debug}     { Add or remove dollar sign before Debug to }
                   { generate code for debug message output    }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugString(Msg : String);
const
    Cnt : Integer = 0;
{$IFDEF Debug}
var
    Buf : String[20];
{$ENDIF}
begin
{$IFDEF Debug}
    Cnt := Cnt + 1;
    Buf := IntToHex(Cnt, 4) + ' ' + #0;
    OutputDebugString(@Buf[1]);
{$IFDEF WIN32}
    OutputDebugString(PChar(Msg));
{$ELSE}
    if Length(Msg) < High(Msg) then
        Msg[Length(Msg) + 1] := #0;

    OutputDebugString(@Msg[1]);
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TClient.Create(AOwner : TComponent);
begin
     Application.CreateForm(TClientForm, Form);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TClient.Destroy;
begin
    Form.Release;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.FormCreate(Sender: TObject);
begin
    Memo.Clear;
    Clients := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.FormActivate(Sender: TObject);
const
    FirstTime : Boolean = TRUE;
begin
    if FirstTime then begin
        FirstTime := FALSE;
        RestartServer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.RestartServer;
begin
    with SrvSocket do begin
        Close;
        Addr  := '0.0.0.0';  { Use any interface for listening }
        Proto := 'tcp';
        Port  := PortEdit.Text;
        Listen;
    end;
    Memo.Clear;
    Display(PortEdit.Text + ' Server Ready' + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.Display(Msg : String);
var
    Start, Stop : Integer;
begin
    if Memo.Lines.Count = 0 then
        Memo.Lines.Add('');

    Start := 1;
    Stop  := Pos(#13, Msg);
    if Stop = 0 then
        Stop := Length(Msg) + 1;
    while Start <= Length(Msg) do begin
        Memo.Lines.Strings[Memo.Lines.Count - 1] := Memo.Lines.Strings[Memo.Lines.Count - 1] + Copy(Msg, Start, Stop - Start);
        if Msg[Stop] = #13 then begin
            Memo.Lines.Add('');
            SendMessage(Memo.Handle, WM_KEYDOWN, VK_UP, 1);
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
procedure TServerForm.SrvSocketSessionAvailable(Sender: TObject; Error : word);
var
    NewHSocket : TSocket;
    PeerName   : TSockAddrIn;
    Client     : TClient;
begin
    NewHSocket             := SrvSocket.Accept;
    Client                 := TClient.Create(Self);
    Client.Form.Reference  := Client;
    Client.Form.PortNum    := SrvSocket.PortNum;
    Client.Form.AcceptForm := Self;
    Client.Form.Socket.Dup(NewHSocket);
    Client.Form.Socket.GetPeerName(PeerName, Sizeof(PeerName));
    Client.Peer := StrPas(inet_ntoa(PeerName.Sin_addr));
    Display('Remote ' + Client.Peer + ' connected' + #13 + #10);
    Client.Form.Caption := Client.Peer;
    Client.Form.Show;
    Clients.Add(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.WMDisconnect(var msg: TMessage);
var
    Client : TClient;
    Why    : String;
begin
    case msg.wParam of
    DISCONNECT_SELF   : Why := 'has been disconnected';
    DISCONNECT_REMOTE : Why := 'has closed the connection';
    else                Why := 'disconnected';
    end;

    Client := TCLient(msg.lParam);
    Display('Remote ' + Client.Peer + ' ' + Why + #13 + #10);
    Client.Form.Socket.Shutdown(2);
    Client.Form.Socket.Close;
    Client.Form.Visible := FALSE;
    Client.Form.Release;
    Clients.Remove(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.SrvSocketSessionClosed(Sender: TObject; Error : word);
begin
    Display(#13 + #10 + '*** Remote has closed ***' + #13 + #10);
    if SrvSocket.State = wsOpened then
        SrvSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.QuitButtonClick(Sender: TObject);
begin
    SrvSocket.Close;
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.AboutButtonClick(Sender: TObject);
var
    Buf : String;
begin
    Buf := 'TnSRV  V' +
           IntToStr(TnSrvVersion div 100) + '.' +
           IntToStr(TnSrvVersion mod 100) + ' ' +
{$IFDEF WIN32}
           '32 bit' +
{$ELSE}
           '16 bit' +
{$ENDIF}
           '  Created march 19, 1997' + #10 + #10 +
           'Free Software, Copyright François Piette' + #10 + #10 +
           'francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette' + #0;
    Application.MessageBox(@Buf[1], 'About TnSrv', MB_OK);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Adjust the position for each control in the form as the user resize it   *}
procedure TServerForm.FormResize(Sender: TObject);
begin
    Memo.Height          := ClientHeight - QuitButton.Height - 20;
    QuitButton.Left      := ClientWidth - QuitButton.Width - 10;
    AboutButton.Left     := QuitButton.Left - AboutButton.Width - 10;
    QuitButton.Top       := ClientHeight - QuitButton.Height - 10;
    AboutButton.Top      := QuitButton.Top;
    ChangePortButton.Top := QuitButton.Top;
    PortEdit.Top         := QuitButton.Top;
    PortLabel.Top        := QuitButton.Top + 4;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.ChangePortButtonClick(Sender: TObject);
begin
    RestartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

