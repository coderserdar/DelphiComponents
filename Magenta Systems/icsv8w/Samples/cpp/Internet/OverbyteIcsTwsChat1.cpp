/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  TWSChat shows how to use TWSocket to build a chat program
Creation:     November 26, 1997
Version:      1.03
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
Jan 10, 1998  V1.02 Corrected CliWSocketDataAvailable
Apr 12, 1998  V1.03 Adapted for BCB3

  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#define ChatPort "2200"
#define TWSChatVersion 102

#include "OverbyteIcsTwsChat1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TTWSChatForm *TWSChatForm;
//---------------------------------------------------------------------------
__fastcall TTWSChatForm::TTWSChatForm(TComponent* Owner)
    : TForm(Owner)
{
}

//---------------------------------------------------------------------------
void __fastcall TTWSChatForm::StartServer()
{
    // Try to be a server
    SrvWSocket->Port  = ChatPort;
    SrvWSocket->Proto = "tcp";
    SrvWSocket->Addr  = "0.0.0.0";
    try {
        SrvWSocket->Listen();
        RunningRadioButton->Checked = TRUE;
        StoppedRadioButton->Checked = FALSE;
    } catch (const ESocketException &E) {
            // The socket is probably already in use
            RunningRadioButton->Checked = FALSE;
            StoppedRadioButton->Checked = TRUE;
            if (strncmp(AnsiString(E.Message).c_str(), "Error 10048", 11) == 0)
                DisplayMemo->Lines->Add("TWSChat already running as server");
            else
                throw;
    }
}
//---------------------------------------------------------------------------
void __fastcall TTWSChatForm::FormShow(TObject *Sender)
{
    if (!Initialized) {
        Initialized = TRUE;
        StartServer();
    }
}
//---------------------------------------------------------------------------
// The user has clicked on the 'connect' Button-> We will not connect here,
// but start the DNSLookup. We will receive a event when it is complete.
// The connection will be made at that later time.
void __fastcall TTWSChatForm::ConnectButtonClick(TObject *Sender)
{
    ConnectButton->Enabled    = FALSE;
    DisconnectButton->Enabled = TRUE;
    CliWSocket->DnsLookup(ServerEdit->Text);
}
//---------------------------------------------------------------------------
// This event handler is triggered when the DNS lookup process is finished
// successfully or not. If DNS lookud failed, display a message.
// If DNS lookup successfull, ask TWSocket to connect the server.
void __fastcall TTWSChatForm::CliWSocketDnsLookupDone(TObject *Sender,
    WORD Error)
{
    if (Error != 0) {
        // DNS Lookup has failed
        DisplayMemo->Lines->Add("Server name unknown");
        ConnectButton->Enabled    = TRUE;
        DisconnectButton->Enabled = FALSE;
        return;
    }

    // DNS lookup successfull. Try to see if we are the server and we are
    // trying to connect to ourself. Check loopback address, should also
    // check the local IP address (returned by LocalIPList)...
    if ((SrvWSocket->State == wsListening) &&
        (CliWSocket->DnsResult == "127.0.0.1")) {
        DisplayMemo->Lines->Add("Your are trying to connect to yourself !");
        ConnectButton->Enabled    = TRUE;
        DisconnectButton->Enabled = FALSE;
        return;
    }

    // Transfert the IP address from DNSLookup to the TWSocket for connection
    // We could use the hostname for the Addr property, TWSocket will do the
    // DNS lookup for us, but it will block, maybe for a long time if DNS if
    // down.
    CliWSocket->Addr  = CliWSocket->DnsResult;
    CliWSocket->Port  = ChatPort;
    CliWSocket->Proto = "tcp";

    // The connect method is asynchronous. You get the control back quickly
    // The OnSessionConnected event will be eventually generated when the
    // connection is established.
    CliWSocket->Connect();
}
//---------------------------------------------------------------------------
// This event handler is triggered when the connection is established with
// the server. Enable the send button and the message edit box.
void __fastcall TTWSChatForm::CliWSocketSessionConnected(TObject *Sender,
    WORD Error)
{
    if (Error == WSAECONNREFUSED)
        DisplayMemo->Lines->Add("No server available");
    else if (Error != 0)
        DisplayMemo->Lines->Add("Can't connect, error #" + IntToStr(Error));
    else {
        DisplayMemo->Lines->Add("Connected");
        SendButton->Enabled  = TRUE;
        MessageEdit->Enabled = TRUE;
    }
}
//---------------------------------------------------------------------------
// This event is triggered when the client connection is closed, either
// by the client himself or by the local user pushing the disconnect button
void __fastcall TTWSChatForm::CliWSocketSessionClosed(TObject *Sender,
    WORD Error)
{
    DisconnectButton->Enabled = FALSE;
    ConnectButton->Enabled    = TRUE;
    if (SendButton->Enabled) {
        SendButton->Enabled   = FALSE;
        MessageEdit->Enabled  = FALSE;
        DisplayMemo->Lines->Add("Disconnected");
    }
}
//---------------------------------------------------------------------------
// This event is triggered when data has been received from the client.
// A little bit of work here because the data can comes fragmented or in big
// chunks with several client lines. So we assemble the data received in a
// buffer and check the buffer for complete lines (there can be no complete
// line, exactly one complete line, several complete lines and may be an
// incomplete line at the end.
void __fastcall TTWSChatForm::CliWSocketDataAvailable(TObject *Sender,
    WORD Error)
{
    int  Len;
    int  I;

    // Receive the data that has arrived, put it after the data already here
    Len = CliWSocket->Receive(&RcvBuf[RcvLen], sizeof(RcvBuf) - RcvLen - 1);
    if (Len <= 0)
        return;
    // Update our counter
    RcvLen = RcvLen + Len;
    // Place a null byte at the end of the buffer
    RcvBuf[RcvLen] = 0;

    // Scan the buffer to process each complete line
    while (TRUE) {
        // find the terminating line feed
        I = strchr(RcvBuf, 10) - RcvBuf;
        if (I < 0)
            break; // not found, incomplete line, break loop
        // Replace the line feed by a nul char, truncating the line
        RcvBuf[I] = 0;
        // Display the truncated line
        DisplayMemo->Lines->Add("Remote> " + StrPas(RcvBuf));
        // Restore the line feed }
        RcvBuf[I] = 10;
        // Was it the last line in the buffer ?
        if (I >= (RcvLen - 1)) {
            RcvLen = 0;
            break;
        }
        // Not the last line, move the next one in front of buffer
        memmove(RcvBuf, &RcvBuf[I + 1], RcvLen - I);
        RcvLen = RcvLen - I;
    }
}
//---------------------------------------------------------------------------
// This event is triggered when we - as a server - have received a client
// connection request. We must accept the connection. Two cases: we are
// already busy with another client, or this is the first client connecting.
void __fastcall TTWSChatForm::SrvWSocketSessionAvailable(TObject *Sender,
    WORD Error)
{
    if (CliWSocket->State == wsConnected) {
        // We are already busy with a client. Use the TmpWSocket to send a
        // busy message to the second client. Display a message to notify
        // the user that someone is trying to contact him.
        TmpWSocket->HSocket = SrvWSocket->Accept();
        DisplayMemo->Lines->Add("System> " + TmpWSocket->GetPeerAddr() +
                              " is trying to call you");
        TmpWSocket->SendStr(RawByteString("Busy ! Try later...\r\n"));
        TmpWSocket->Close();
        return;
    }

    // This is our first client trying to connect, we accept
    CliWSocket->HSocket       = SrvWSocket->Accept();
    ConnectButton->Enabled    = FALSE;
    DisconnectButton->Enabled = TRUE;
    SendButton->Enabled       = TRUE;
    MessageEdit->Enabled      = TRUE;
    DisplayMemo->Lines->Add("Connected with " + CliWSocket->GetPeerAddr());
}
//---------------------------------------------------------------------------
// The user clicked on the disconnect Button->
void __fastcall TTWSChatForm::DisconnectButtonClick(TObject *Sender)
{
    CliWSocket->Close();
}
//---------------------------------------------------------------------------
// The user has clicked on the send Button-> Just send the data in the edit
// box and a CRLF pair to make a complete line.
void __fastcall TTWSChatForm::SendButtonClick(TObject *Sender)
{
    CliWSocket->SendStr(MessageEdit->Text + "\r\n");
    DisplayMemo->Lines->Add(" Local> " + MessageEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TTWSChatForm::StoppedRadioButtonClick(TObject *Sender)
{
    SrvWSocket->Close();
    RunningRadioButton->Checked = FALSE;
    StoppedRadioButton->Checked = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall TTWSChatForm::RunningRadioButtonClick(TObject *Sender)
{
    if (SrvWSocket->State != wsListening)
        StartServer();
}
//---------------------------------------------------------------------------

