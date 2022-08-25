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
Version:      1.00
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


---------------------------------------------------------------------------*/
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsMtSrv2.h"
//---------------------------------------------------------------------------
//   Important: Methods and properties of objects in VCL can only be
//   used in a method called using Synchronize, for example:
//
//      Synchronize(UpdateCaption);
//
//   where UpdateCaption could look like:
//
//      void __fastcall TClientThread::UpdateCaption()
//      {
//        Form1->Caption = "Updated in a thread";
//      }
//---------------------------------------------------------------------------
// Create a new thread in the blocked state. This allow the user to register
// the client thread before it actually start working.
__fastcall TClientThread::TClientThread(int ClientHSocket)
    : TThread(TRUE)
{
    FClientHSocket  = ClientHSocket;
    FreeOnTerminate = TRUE;
}
//---------------------------------------------------------------------------
// Destroy the thread. Destroy the ClientWSocket if needed.
__fastcall TClientThread::~TClientThread()
{
    if (FClientSocket != NULL) {
         delete FClientSocket;
         FClientSocket = NULL;
    }
}
//---------------------------------------------------------------------------
// This method will be called by the main server thread to terminated a
// client thread.                                                            
void __fastcall TClientThread::Release()
{
    PostMessage(FClientSocket->Handle, WM_QUIT, 0, 0);
}
//---------------------------------------------------------------------------
// This is the main thread routine. There is not much to do because TWSocket
// is event drive. So everythong to do is done inside an event handler,
// mostly the OnDataAvailable event handler which is triggered each time
// the client send something to the server.
void __fastcall TClientThread::Execute()
{
    // Create the client TWSocket. It is important to create it inside the   
    // Execute method because it *must* be created by the thread. Otherwise
    // the messages sent by winsock would be processed in the main thread
    // context, effectively disabling multi-threading.
    FClientSocket                  = new TWSocket(NULL);
    FClientSocket->MultiThreaded   = true;
    FClientSocket->HSocket         = FClientHSocket;
    FClientSocket->OnDataAvailable = ServerWSocketDataAvailable;
    FClientSocket->OnSessionClosed = ServerWSocketSessionClosed;

    // Send the welcome message
    FClientSocket->SendStr(RawByteString("Hello !\r\n >"));

    // Message loop to handle TWSocket messages
    // The loop is exited when WM_QUIT message is received
    FClientSocket->MessageLoop();

    // Returning from the Execute function effectively terminate the thread
}
//---------------------------------------------------------------------------
// This event handler is called when the client connection is closed.
void __fastcall TClientThread::ServerWSocketSessionClosed(TObject *Sender, WORD Error)
{
    PostMessage(FClientSocket->Handle, WM_QUIT, 0, 0);
}
//---------------------------------------------------------------------------
// This event handler is called when the client has sent some data to the    
// server. It is here that we must place the client requests execution
// probably by assembling data in lines, parsing those lines for commands
// and executing the commands. Here for simplicity, we just echo back the
// data sent by the user, without doing anything serious.
// To demonstrate that blocking a thread do not block the others, when a '*'
// is received, we go to Sleep for a few seconds, effectively blocking the
// the client. But as we are multi-threaded, this do not block any other
// client.
// Do not forget to call the Synchronize method if you need to update the
// user interface. Only the main thread can do it (VCL is not thread safe).
void __fastcall TClientThread::ServerWSocketDataAvailable(TObject *Sender, WORD Error)
{
    char Buffer[4096];
    int  Count;

    // Receive as much data as possible
    Count = FClientSocket->Receive(&Buffer, sizeof(Buffer));

    // If data received, then process it
    if (Count > 0) {
        if (Buffer[0] == '*') {
            // If the first character is '*' then go to sleep a while
            FClientSocket->SendStr(RawByteString("Sleeping for 15 sec...\r\n"));
            Sleep(15000);
            FClientSocket->SendStr(RawByteString("Wake up !\r\n"));
        }
        else
            // Just echo data back to client
            FClientSocket->Send(&Buffer, Count);

    }
}
//---------------------------------------------------------------------------

