/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  This little application shows how to use the TWSocket in a
              multithreaded application. It is a very basic telnet werver.
              Each time a client connect to the server, he receive an "hello"
              message. Then every character sent is echoed back to the client.
              There are two units is this application: one for the main
              server code, and one for the client thread.
              Each time a client connect to the server, a new TWSocket is
              created and a new thread is launched to handle the client
              work. When the client disconnect, the TWSocket and the thread
              are destroyed.
              To see this demo working on your computer, start the demo then
              start several copies of the TELNET client program (the one which
              comes with Windows 95 is perfect). Then using each telnet, connect
              to 127.0.0.1. You'll see a new connection in the list box. You
              must receive the "hello" message and see each character as you
              type them. You can use the Disconnect button from the application
              or from the telnet client to see what happends (the connection
              should be closed).
Creation:     September 25, 1997 (from Delphi version dated September 21, 1997)
Version:      1.01
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
Nov 15, 1997 Corrected a bug in the disconnect method which crashed the server
             when you tried to disconnect the second or later client.

---------------------------------------------------------------------------*/
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsMtSrv1.h"
#include "OverbyteIcsMtSrv2.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TServerForm *ServerForm;
//---------------------------------------------------------------------------
// Convert an hexdigit to an integer
int xdigit(char Ch)
{
    if ((Ch >= '0') && (Ch <= '9'))
        return(Ch - '0');
    else
        return((Ch & 15) + 9);
}
//---------------------------------------------------------------------------
// Check if a character is a valid hex digit
int isxDigit(char Ch)
{
    return(((Ch >= '0') && (Ch <= '9')) ||
           ((Ch >= 'a') && (Ch <= 'z')) ||
           ((Ch >= 'A') && (Ch <= 'Z')));
}
//---------------------------------------------------------------------------
// Convert the ascii representation of a hex number to an integer
int htoi(char *Value)
{
    int i;
    int Result;

    Result = 0;
    i      = 0;
    while ((Value[i] != 0) && (Value[i] == ' '))
        i++;
    while ((Value[i] != 0) && isxDigit(Value[i])) {
        Result = Result * 16 + xdigit(Value[i]);
        i++;
    }
    return(Result);
}
//---------------------------------------------------------------------------
__fastcall TServerForm::TServerForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::ServerWSocketSessionAvailable(TObject *Sender,
    WORD Error)
{
    TClientThread *ClientThread;

    // Create a new thread to handle client request
    ClientThread              = new TClientThread(ServerWSocket->Accept());

    // Assign the thread's OnTerminate event
    ClientThread->OnTerminate = ClientThreadTerminate;

    // Add the thread to the listbox which is our client list
    ClientListBox->Items->Add(IntToHex(Integer(ClientThread), 8));

    // Then start the client thread work
    // because it was created in the blocked state
    ClientThread->Resume();
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::FormShow(TObject *Sender)
{
    static BOOL FirstTime = TRUE;

    if (FirstTime) {
        FirstTime           = FALSE;
#if (WSocketVersion < 202)
#error  "Please update your wsocket.pas to version 2.02 or later. "
#error  "Free download at http://www.rtfm.be/fpiette/indexuk.htm"
#error  "EMail at francois.piette@pophost.eunet.be"
#endif
        ServerWSocket->Proto = "tcp";      // We use a TCP connection
        ServerWSocket->Port  = "telnet";   // We wants to use telnet
        ServerWSocket->Addr  = "0.0.0.0";  // We accept any client
        ServerWSocket->Listen();           // Start server accepting
    }
}
//---------------------------------------------------------------------------
// This event is generated when the user clicks on the 'Disconnect' button
// when he wants to disconnect the selected client in the listbox.
void __fastcall TServerForm::DisconnectButtonClick(TObject *Sender)
{
    TClientThread *ClientThread;
    AnsiString    Buf;

    // No selected item, nothing to do
    if (ClientListBox->ItemIndex < 0)
        return;

    // Extract the ClientThread pointer from the list box
    Buf = ClientListBox->Items->Strings[ClientListBox->ItemIndex];
    ClientThread = (TClientThread *)htoi(Buf.c_str());

    // Call ClientThread.Release which will stop the thread
    // In consequence, the OnTerminate event will be generated
    ClientThread->Release();
}
//---------------------------------------------------------------------------
// This event handler is called when one of the client thread terminate
// We will find this thread in our listbox, remove it and destroy the
// TWSocket object use by the corresponding client.                          
void __fastcall TServerForm::ClientThreadTerminate(TObject *Sender)
{
    TClientThread *ClientThread;
    AnsiString    Buf;
    int           Index;

    // A thread has been terminated, remove it from our list and destroy
    // the ClientWSocket we passed to the thread.
    Index = 0;
    while (Index < ClientListBox->Items->Count) {
        Buf = ClientListBox->Items->Strings[Index];
        ClientThread = (TClientThread *)htoi(Buf.c_str());
        if (ClientThread == (TClientThread *)Sender) {
            // Remove the client from our listbox     
            ClientListBox->Items->Delete(Index);
            break;
        }
        Index++;
    }
}
//---------------------------------------------------------------------------
// This procedure scan the listbox and halt every ClientThread
void __fastcall TServerForm::DisconnectAll()
{
    TClientThread *ClientThread;
    AnsiString    Buf;

    while (ClientListBox->Items->Count > 0) {
        Buf          = ClientListBox->Items[0].Text;
        ClientThread = (TClientThread *)htoi(Buf.c_str());
        ClientThread->Release();
        Application->ProcessMessages();
    }
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::QuitButtonClick(TObject *Sender)
{
    Close();
}
//---------------------------------------------------------------------------
void __fastcall TServerForm::DisconnectAllButtonClick(TObject *Sender)
{
    DisconnectAll();
}
//---------------------------------------------------------------------------

void __fastcall TServerForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
    DisconnectAll();    
}
//---------------------------------------------------------------------------