{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       Demo program to show how to use TWSocket object is a very
              simple server program. This server just wait for a client to
              connect, then send 'Hello'. When the user click on the
              disconnect button, the client is disconnected.
Creation:     September 19, 1996
Version:      2.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbytet.be>

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
Mar 19, 1997  Use enhanced TWSocket object
Sep 06, 1997  Beautified
Aug 20, 1999  V2.02 Changed comment to correctly talk about interface use.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSrv5;

{$J+}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics,
  Controls, Forms, Dialogs, OverbyteIcsWSocket, OverbyteIcsWinsock, StdCtrls,
  OverbyteIcsWndControl;

type
  TServerForm = class(TForm)
    SrvSocket: TWSocket;
    InfoLabel: TLabel;
    CliSocket: TWSocket;
    DisconnectButton: TButton;
    procedure SrvSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure CliSocketSessionClosed(Sender: TObject; Error: Word);
  end;

var
  ServerForm: TServerForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerForm.FormShow(Sender: TObject);
const
    FirstTime : Boolean = TRUE;
begin
    if FirstTime then begin
        FirstTime         := FALSE;            { Do it only once !          }
        SrvSocket.Addr    := '0.0.0.0';        { Use any interface          }
        SrvSocket.Listen;                      { Start listening for client }
        InfoLabel.Caption := 'Waiting for client';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* This event handler is called once a client has connected the server.    *}
procedure TServerForm.SrvSocketSessionAvailable(Sender: TObject; Error: Word);
var
    NewHSocket : TSocket;
    PeerName   : TSockAddrIn;
    Peer       : String;
begin
    { We need to accept the client connection }
    NewHSocket := SrvSocket.Accept;

    { And then associate this connection with our client socket }
    CliSocket.Dup(NewHSocket);

    { Wants to know who is connected to display on screen }
    CliSocket.GetPeerName(PeerName, Sizeof(PeerName));

    { User likes to see internet address in dot notation }
    Peer := IntToStr(ord(PeerName.sin_addr.S_un_b.s_b1)) + '.' +
            IntToStr(ord(PeerName.sin_addr.S_un_b.s_b2)) + '.' +
            IntToStr(ord(PeerName.sin_addr.S_un_b.s_b3)) + '.' +
            IntToStr(ord(PeerName.sin_addr.S_un_b.s_b4));
    InfoLabel.Caption := 'Remote ' + Peer + ' connected';

    { Send a welcome message to the client }
    CliSocket.SendStr('Hello' + #13 + #10);

    { Enable the server user to disconect the client }
    DisconnectButton.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* This event handler is called once the user clicks on Ddisconnect        *}
procedure TServerForm.DisconnectButtonClick(Sender: TObject);
begin
    CliSocket.ShutDown(2);                    { Shut the communication down }
    CliSocket.Close;                          { Close the communication     }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* This event handler is called once the client connection is broken.      *}
{* Either by the client or the server.                                     *}
procedure TServerForm.CliSocketSessionClosed(Sender: TObject; Error: Word);
begin
    DisconnectButton.Enabled := FALSE;
    InfoLabel.Caption := 'Waiting for client';{ Inform the user             }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

