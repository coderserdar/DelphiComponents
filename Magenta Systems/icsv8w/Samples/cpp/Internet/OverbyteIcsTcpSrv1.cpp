/*---------------------------------------------------------------------------

Author:       François Piette
Creation:     Sep 05, 1999 (From Delphi version dated aug 29, 1999)
Version:      1.00
Description:  Basic TCP server showing how to use TWSocketServer and
              TWSocketClient components.
Email         francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2005 by François PIETTE
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

---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
    #define s_addr S_addr
#endif
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsTcpSrv1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWSocketS"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
#define SectionWindow      "WindowTcpSrv"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
TTcpSrvForm *TcpSrvForm;
//---------------------------------------------------------------------------
__fastcall TTcpSrvForm::TTcpSrvForm(TComponent* Owner)
    : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::FormShow(TObject *Sender)
{
    TIniFile    *IniFile;

    if (!FInitialized) {
        FInitialized     = TRUE;
        IniFile          = new TIniFile(FIniFileName);
        Top              = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left             = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width            = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height           = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        delete IniFile;
        DisplayMemo->Clear();
        // Delay startup code until our UI is ready and visible
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    }
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
}
//---------------------------------------------------------------------------
// Display a message in our display memo. Delete lines to be sure to not
// overflow the memo which may have a limited capacity.
void __fastcall TTcpSrvForm::Display(AnsiString Msg)
{
    int I;

    DisplayMemo->Lines->BeginUpdate();
    if (DisplayMemo->Lines->Count > 200) {
        for (I = 1; I <= 50; I++)
            DisplayMemo->Lines->Delete(0);
    }
    DisplayMemo->Lines->Add(Msg);
    DisplayMemo->Lines->EndUpdate();
}
//---------------------------------------------------------------------------
// This is our custom message handler. We posted a WM_APPSTARTUP message
// from FormShow event handler. Now UI is ready and visible.
void __fastcall TTcpSrvForm::WMAppStartup(TMessage Message)
{
    WSocketServer1->Proto       = "tcp";         // Use TCP protocol
    WSocketServer1->Port        = "telnet";      // Use telnet port
    WSocketServer1->Addr        = "0.0.0.0";     // Use any interface
    WSocketServer1->ClientClass = __classid(TTcpSrvClient); // Use our component
    WSocketServer1->Listen();                    // Start litening
    Display("Waiting for clients...");
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::WSocketServer1ClientConnect(TObject *Sender,
      TWSocketClient *Client, WORD Error)
{
    Display("Client connecting: " + ((TTcpSrvClient *)Client)->PeerAddr);
    ((TTcpSrvClient *)Client)->LineMode        = TRUE;
    ((TTcpSrvClient *)Client)->LineEdit        = TRUE;
    ((TTcpSrvClient *)Client)->OnDataAvailable = ClientDataAvailable;
    ((TTcpSrvClient *)Client)->OnBgException   = ClientBgException;
    ((TTcpSrvClient *)Client)->ConnectTime     = Now();
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::WSocketServer1ClientDisconnect(TObject *Sender,
      TWSocketClient *Client, WORD Error)
{
    Display("Client disconnecting: " + ((TTcpSrvClient *)Client)->PeerAddr + "    " +
            "Duration: " + FormatDateTime("hh:nn:ss",
            Now() - ((TTcpSrvClient *)Client)->ConnectTime));
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::ClientDataAvailable(TObject *Sender, WORD Error)
{
    TTcpSrvClient *Client;

    Client = (TTcpSrvClient *)Sender;

    // We use line mode. We will receive complete lines
    Client->RcvdLine = Client->ReceiveStr();
    // Remove trailing CR/LF
    while ((Client->RcvdLine.Length() > 0) &&
           ((Client->RcvdLine[Client->RcvdLine.Length()] == '\r') ||
            (Client->RcvdLine[Client->RcvdLine.Length()] == '\n'))) {
        Client->RcvdLine = Client->RcvdLine.SubString(1, Client->RcvdLine.Length() - 1);
    }
    Display("Received from " + Client->PeerAddr + ": '" + Client->RcvdLine + "'");
    ProcessData(Client);
}
//---------------------------------------------------------------------------
void __fastcall TTcpSrvForm::ProcessData(TTcpSrvClient *Client)
{
    int           I;
    TTcpSrvClient *AClient;

    // We could replace all those CompareText with a table lookup 
    if (CompareText(Client->RcvdLine, "exit") == 0)
        // We can't call Client.Close here because we will immediately
        // reenter DataAvailable event handler with same line because
        // a line is removed from buffer AFTER it has been processed.
        // Using CloseDelayed will delay Close until we are out of
        // current event handler.
        Client->CloseDelayed();
    else if (CompareText(Client->RcvdLine, "time") == 0)
        // Send server date and time to client
        Client->SendStr(DateTimeToStr(Now()) + "\r\n");
    else if (CompareText(Client->RcvdLine, "who") == 0) {
        // Send client list to client
        Client->SendStr("There are " + IntToStr(WSocketServer1->ClientCount) +
                        " connected users:\r\n");
        for (I = WSocketServer1->ClientCount - 1; I >= 0; I--) {
            AClient = (TTcpSrvClient *)(WSocketServer1->Client[I]);
            Client->SendStr(AClient->PeerAddr + ":" + AClient->PeerPort + " " +
                           DateTimeToStr(AClient->ConnectTime) + "\r\n");
        }
    }
    /*
    else if (CompareText(Client->RcvdLine, "exception") == 0)
        This will trigger a background exception for client
        PostMessage(Client->Handle, WM_TRIGGER_EXCEPTION, 0, 0);
    */
    else
        Client->SendStr(RawByteString("Unknown command: '" + Client->RcvdLine + "'\r\n"));
}
//---------------------------------------------------------------------------
// This event handler is called when listening (server) socket experienced
// a background exception. Should normally never occurs.
void __fastcall TTcpSrvForm::WSocketServer1BgException(TObject *Sender,
      Exception *E, bool &CanClose)
{
    Display("Server exception occured: " + E->ClassName() + ": " + E->Message);
    CanClose = FALSE;  // Hoping that server will still work !
}
//---------------------------------------------------------------------------
// This event handler is called when a client socket experience a background
// exception. It is likely to occurs when client aborted connection and data
// has not been sent yet.
void __fastcall TTcpSrvForm::ClientBgException(TObject   *Sender,
                                          Exception *E,
                                          bool      &CanClose)
{
    Display("Client exception occured: " + E->ClassName() + ": " + E->Message);
    CanClose = TRUE;   // Goodbye client !
}
//---------------------------------------------------------------------------
__fastcall TTcpSrvClient::TTcpSrvClient(TComponent* Owner)
    : TWSocketClient(Owner)
{
}
//---------------------------------------------------------------------------

