/*---------------------------------------------------------------------------

Copyright:    François PIETTE
Creation:     September 27, 1997 (from Delphi version created in april 1996)
Version:      1.01
Description:  TnSrv implement a (very basic) Telnet server (daemon)
              Uses TWSocket to communicate with WinSock
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
Mar 27, 1998  1.01 Adapted to C++Builder V3.0

---------------------------------------------------------------------------*/
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsTnSrv1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TServerForm *ServerForm;
//---------------------------------------------------------------------------
__fastcall TClient::TClient(TComponent* Owner)
    : TObject()
{
    Application->CreateForm(__classid(TClientForm), &Form);
}
//---------------------------------------------------------------------------
__fastcall TClient::~TClient()
{
    Form->Release();
}
//---------------------------------------------------------------------------
int __fastcall Pos(char Ch, char *Msg)
{
    char *p = Msg;

    while (*p && (*p != Ch))
        p++;
    return((*p) ? (p - Msg) : 0);
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::Display(char *Msg)
{
    AnsiString *Temp;

    Temp = new AnsiString(Msg);
    Display(Temp);
    delete Temp;
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::Display(AnsiString Msg)
{
    Display(&Msg);
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::Display(AnsiString *Msg)
{
    int Start, Stop;

    if (Memo->Lines->Count == 0)
        Memo->Lines->Add("");

    Start = 1;
    Stop  = Msg->Pos("\r");
    if (Stop == 0)
        Stop = Msg->Length() + 1;
    while (Start <= Msg->Length()) {
        Memo->Lines->Strings[Memo->Lines->Count - 1] =
            Memo->Lines->Strings[Memo->Lines->Count - 1] +
            Msg->SubString(Start, Stop - Start);
        if ((Stop <= Msg->Length()) && ((*Msg)[Stop] == '\r')) {
            Memo->Lines->Add("");
            SendMessage(Memo->Handle, WM_KEYDOWN, VK_UP, 1);
        }
        Start = Stop + 1;
        if (Start > Msg->Length())
            break;
        if ((*Msg)[Start] == '\n')
           Start = Start + 1;
        Stop = Start;
        while ((Stop <= Msg->Length()) && ((*Msg)[Stop] != '\r'))
            Stop++;
    }
}
//---------------------------------------------------------------------------
__fastcall TServerForm::TServerForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::FormCreate(TObject *Sender)
{
    Memo->Clear();
    Clients = new TList;
    Display("Telnet Server Ready\r\n");
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::FormActivate(TObject *Sender)
{
    static BOOL FirstTime = TRUE;

    if (FirstTime) {
        FirstTime = FALSE;
        SrvSocket->Listen();
    }
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::FormResize(TObject *Sender)
{
    Memo->Height      = ClientHeight - QuitButton->Height - 20;
    QuitButton->Left  = ClientWidth - QuitButton->Width - 10;
    AboutButton->Left = QuitButton->Left - AboutButton->Width - 10;
    QuitButton->Top   = ClientHeight - QuitButton->Height - 10;
    AboutButton->Top  = QuitButton->Top;
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::SrvSocketSessionAvailable(TObject *Sender,
    WORD Error)
{
    int         NewHSocket;
    Overbyteicswinsock::TSockAddrIn PeerName;
    TClient     *Client;
    AnsiString  Buffer;

    NewHSocket = SrvSocket->Accept();
    Client = new TClient(this);
    Client->Form->Reference  = Client;
    Client->Form->AcceptForm = this;
    Client->Form->Socket->Dup(NewHSocket);
    Client->Form->Socket->GetPeerName(PeerName, sizeof(PeerName));
    Client->Peer = inet_ntoa(PeerName.sin_addr);
    Buffer = "Remote " + Client->Peer + " connected\r\n";
    Display(Buffer);
    Client->Form->Caption = Client->Peer;
    Client->Form->Show();
    Clients->Add(Client);
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::WMDisconnect(TMessage Message)
{
    TClient    *Client;
    char       *Why;
    AnsiString Buffer;

    switch (Message.WParam) {
    case DISCONNECT_SELF   : Why = "has been disconnected";     break;
    case DISCONNECT_REMOTE : Why = "has closed the connection"; break;
    default                : Why = "disconnected";              break;
    }

    Client = (TClient *)(Message.LParam);
    Buffer = "Remote " + Client->Peer + " " + Why + "\r\n";
    Display(Buffer);
    Client->Form->Socket->Shutdown(2);
    Client->Form->Socket->Close();
    Client->Form->Visible = FALSE;
    Client->Form->Release();
    Clients->Remove(Client);
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::SrvSocketSessionClosed(TObject *Sender, WORD Error)
{
    Display("\r\n*** Remote has closed ***\r\n");
    if (SrvSocket->State == wsOpened)
        SrvSocket->Close();
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::QuitButtonClick(TObject *Sender)
{
    SrvSocket->Close();
    Close();
}
//---------------------------------------------------------------------------
void __fastcall MyMessageBox(const System::String &Msg,
    const System::String &Title, int Flags)
{
    Application->MessageBox(Msg.c_str(), Title.c_str(), Flags);
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::AboutButtonClick(TObject *Sender)
{
    MyMessageBox(
        "TnSRV  V1.1 C++Builder 32 bit September 27, 1997\n\n"
        "Free Software, Copyright François Piette\n\n"
        "francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette\n",
        "About TnSrv", MB_OK);
}
//---------------------------------------------------------------------------
