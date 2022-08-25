/*---------------------------------------------------------------------------

Author:       François PIETTE
Object:       Demo program to show how to use TWSocket object is a very
              simple server program. This server just wait for a client to
              connect, then send 'Hello'. When the user click on the
              disconnect button, the client is disconnected.
Creation:     September 27, 1997 (from Delphi version created sept 19, 1996)
Version:      2.02
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2005 by François PIETTE
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
Mar 19, 1997 Use enhanced TWSocket object
Sep 06, 1997 Beautified
Apr 11, 1998 V2.02 Adapted for BCB3

---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsSrv5.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TServerForm *ServerForm;
//---------------------------------------------------------------------------
__fastcall TServerForm::TServerForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::FormShow(TObject *Sender)
{
    static BOOL FirstTime = TRUE;

    if (FirstTime) {
        FirstTime          = FALSE;            // Do it only once !
        SrvSocket->Addr    = "0.0.0.0";        // Accept any client
        SrvSocket->Listen();                   // Start listening for client
        InfoLabel->Caption = "Waiting for client";
    }
}
//---------------------------------------------------------------------------
// This event handler is called once a client has connected the server.
void __fastcall TServerForm::SrvSocketSessionAvailable(TObject *Sender,
    WORD Error)
{
    int         NewHSocket;
    Overbyteicswinsock::TSockAddrIn PeerName;
    AnsiString  Peer;

    // We need to accept the client connection
    NewHSocket = SrvSocket->Accept();

    // And then associate this connection with our client socket
    CliSocket->Dup(NewHSocket);

    // Wants to know who is connected to display on screen
    CliSocket->GetPeerName(PeerName, sizeof(PeerName));

    // User likes to see internet address in dot notation
    Peer = inet_ntoa(PeerName.sin_addr);
    InfoLabel->Caption = "Remote " + Peer + " connected";

    // Send a welcome message to the client
    CliSocket->SendStr(RawByteString("Hello\r\n"));

    // Enable the server user to disconect the client
    DisconnectButton->Enabled = TRUE;
}
//---------------------------------------------------------------------------
// This event handler is called once the client connection is broken.
// Either by the client or the server.
void __fastcall TServerForm::CliSocketSessionClosed(TObject *Sender, WORD Error)
{
    DisconnectButton->Enabled = FALSE;
    InfoLabel->Caption        = "Waiting for client"; // Inform the user             
}
//---------------------------------------------------------------------------
// This event handler is called once the user clicks on Ddisconnect
void __fastcall TServerForm::DisconnectButtonClick(TObject *Sender)
{
    CliSocket->Shutdown(2);                    // Shut the communication down
    CliSocket->Close();                        // Close the communication
}
//---------------------------------------------------------------------------