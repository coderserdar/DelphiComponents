/*---------------------------------------------------------------------------

Author:       François PIETTE
Object:       Simple client application demonstrating TWSocket object in action.
Creation:     September 28, 1997 (from delphi version created 09/21/1996)
Version:      2.04
Email         francois.piette@overbyte.be      http://www.overbyte.be
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
Nov 11, 1997 V2.03 Added a button to display the list of IP addresses for the
             local computer (you can have two IP addresses if you are connected
             to a LAN and to your ISP).
             Added a ReadLine button to show how to read a single line
             synchronously.
Dec 06, 1998 V2.04 Don't use TWait component anymore.

---------------------------------------------------------------------------*/
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcscli5.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TClientForm *ClientForm;
//---------------------------------------------------------------------------
__fastcall TClientForm::TClientForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
// This event handler gets called when we connected the server
void __fastcall TClientForm::CliSocketSessionConnected(TObject *Sender,
    WORD Error)
{
    InfoLabel->Caption        = "Connected";
    DisconnectButton->Enabled = TRUE;
    ConnectButton->Enabled    = FALSE;
}
//---------------------------------------------------------------------------
// This event handler gets called when the server's connection is broken
// Either by us or by the server.
void __fastcall TClientForm::CliSocketSessionClosed(TObject *Sender, WORD Error)
{
    DataLabel->Caption        = "";
    InfoLabel->Caption        = "Disconnected";
    DisconnectButton->Enabled = FALSE;
    ConnectButton->Enabled    = TRUE;
}
//---------------------------------------------------------------------------
// The user has clicked on the connect button...
void __fastcall TClientForm::ConnectButtonClick(TObject *Sender)
{
    CliSocket->Addr   = "localhost";        // Server host name
    CliSocket->Proto  = "tcp";              // Protocol we wants to use
    CliSocket->Port   = "telnet";           // The port we wants to connect
    CliSocket->Connect();                   // Let's connect !
    // Connect is just a request, it returns immediately. We eventually gets
    // gets connected later. At that time we will receive the event
    // SessionConnected. If you need a timeout, you have to start a TTimer.
}
//---------------------------------------------------------------------------
// The user has clicked the disconnect button...
void __fastcall TClientForm::DisconnectButtonClick(TObject *Sender)
{
    CliSocket->Close();                   // This will close the connection
    // When the connection will be effectively closed, we will receive the
    // SessionClosed even.
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::CliSocketDataAvailable(TObject *Sender, WORD Error)
{
    char Buffer[256];
    int  Count;

    Count              = CliSocket->Receive(Buffer, sizeof(Buffer));
    Buffer[Count]      = 0;             // Null terminate received data
    DataLabel->Caption = Buffer;
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::IPButtonClick(TObject *Sender)
{
    TStrings *IPList;
    int      I;

    IPList = LocalIPList();
    InfoLabel->Caption = "";
    for (I = 0; I < IPList->Count; I++)
        InfoLabel->Caption = InfoLabel->Caption + "   " + IPList->Strings[I];
}
//---------------------------------------------------------------------------
