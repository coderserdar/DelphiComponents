/*---------------------------------------------------------------------------

Author:       François PIETTE
Creation:     April 08, 2000
Description:  This is a demo showing how to use a TWSocket component in a DLL.
              This demo must be used with ICS TcpSrv demo program as a server.
              The DLL is a client which connect to the server and send "time"
              command, then wait for the reply and return it in the buffer
              passed to the DLL.
              There is only one function exported from the DLL: IcsDllDemo.
              It takes four arguments: a pointer to the hostname to connect to,
              a pointer to the port, a pointer to a buffer and a pointer for
              buffer size. On entry buffer size must be initialised with the
              size of the actual buffer. On exit, it is filled with the
              actual reply size. The function's return value is the error code
              such as 10061 when the server is not running.
Version:      1.00
EMail:        francois.piette@pophost.eunet.be    francois.piette@swing.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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
#include <vcl.h>
#include <windows.h>
#pragma hdrstop
#pragma argsused
#include "WSocket.hpp"
//---------------------------------------------------------------------------
extern "C" __declspec(dllexport) int __stdcall IcsDllDemo(char *HostName,
                                                          char *Port,
                                                          char *Buffer,
                                                          int  *BufSize);
void StrToBuffer(char *Buffer, int *BufSize, char *Msg);
//---------------------------------------------------------------------------
// We use a workerthread to do the job.
// This will allows the DLL to be called by several processes simultaneously
class TClientThread : public TThread
{
private:
    TWSocket *FClientSocket;
    void __fastcall ClientWSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall ClientWSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall ClientWSocketSessionConnected(TObject *Sender, WORD Error);
protected:
    AnsiString FHostName;
    AnsiString FPort;
    int        *FErrorCode;
    BOOL       FBannerReceived;
    char       *FBuffer;
    int        *FBufSize;
    void __fastcall Execute();
public:
    __fastcall TClientThread();
    __fastcall ~TClientThread();
    __property char      *Buffer    = {read=FBuffer,    write=FBuffer};
    __property int       *BufSize   = {read=FBufSize,   write=FBufSize};
    __property int       *ErrorCode = {read=FErrorCode, write=FErrorCode};
    __property AnsiString HostName  = {read=FHostName,  write=FHostName};
    __property AnsiString Port      = {read=FPort,      write=FPort};
};
//---------------------------------------------------------------------------
// Create a new thread in the blocked state. This allow the user to register
// the client thread before it actually start working.
__fastcall TClientThread::TClientThread()
    : TThread(TRUE)
{
    FreeOnTerminate = TRUE;
}
//---------------------------------------------------------------------------
// Destroy the thread. Destroy the ClientWSocket if needed.
__fastcall TClientThread::~TClientThread()
{
    if (FClientSocket != NULL) {
         FClientSocket->~TWSocket();
         FClientSocket = NULL;
    }
}
//---------------------------------------------------------------------------
// This is the main thread routine. There is not much to do because TWSocket
// is event drive. So everythong to do is done inside an event handler,
// mostly the OnDataAvailable event handler which is triggered each time
// something is received.
void __fastcall TClientThread::Execute()
{
    try {
        // Create the client TWSocket. It is important to create it inside the
        // Execute method because it *must* be created by the thread. Otherwise
        // the messages sent by winsock would be processed in the main thread
        // context, effectively disabling multi-threading.
        FClientSocket                     = new TWSocket(NULL);
        FClientSocket->OnDataAvailable    = ClientWSocketDataAvailable;
        FClientSocket->OnSessionConnected = ClientWSocketSessionConnected;
        FClientSocket->OnSessionClosed    = ClientWSocketSessionClosed;
        FClientSocket->LineMode           = TRUE;
        FClientSocket->Addr               = FHostName;
        FClientSocket->Port               = FPort;
        FClientSocket->Proto              = "tcp";
        FClientSocket->Connect();

        // Message loop to handle TWSocket messages
        // The loop is exited when WM_QUIT message is received
        FClientSocket->MessageLoop();
    }
    catch (Exception &E)
    {
        AnsiString Buf;
        Buf = E.ClassName();
        Buf = Buf  + ": " + E.Message;
        *FErrorCode = -3;
        StrToBuffer(Buffer, BufSize, Buf.c_str());
    }

    // Returning from the Execute function effectively terminate the thread
}
//---------------------------------------------------------------------------
// This event handler is called when the client connection is established.
void __fastcall TClientThread::ClientWSocketSessionConnected(
    TObject *Sender, WORD Error)
{
    if (Error) {
        *FErrorCode = Error;
        StrToBuffer(Buffer, BufSize, "Connect failed");
        PostMessage(FClientSocket->Handle, WM_QUIT, 0, 0);
    }
}
//---------------------------------------------------------------------------
// This event handler is called when the client connection is closed.
void __fastcall TClientThread::ClientWSocketSessionClosed(
    TObject *Sender, WORD Error)
{
    PostMessage(FClientSocket->Handle, WM_QUIT, 0, 0);
}
//---------------------------------------------------------------------------
// This event handler is called when data has been received from server.
// Since this sample program use line mode, we comes here only when a
// complete line has been received.
void __fastcall TClientThread::ClientWSocketDataAvailable(
    TObject *Sender, WORD Error)
{
    AnsiString RcvBuffer;

    // Received the line
    RcvBuffer = FClientSocket->ReceiveStr();
    // Check if we already received the banner (message sent by server
    // as soon as we are connected.
    if (!FBannerReceived) {
        // We are just receiving the banner. Flag as received
        FBannerReceived = TRUE;
        // Then send the command to the server
        FClientSocket->SendStr("time\r\n");
    }
    else {
        // We already received then banner. So this must be the answer
        // to our command. Copy to the buffer, without trailling CR/LF
        // and without overflowing the given buffer
        if (RcvBuffer.Length() < *BufSize)
            *BufSize = RcvBuffer.Length() - 2;  // Remove CR/LF
        if (*BufSize > 0)
            memcpy(Buffer, RcvBuffer.data(), *BufSize);
        // Then just close the communication
        FClientSocket->CloseDelayed();
        *FErrorCode = 0;
    }
}
//---------------------------------------------------------------------------
// Copy a string to a buffer with overflow check.
void StrToBuffer(char *Buffer, int *BufSize, char *Msg)
{
    int Len;

    if ((Len = strlen(Msg)) < *BufSize)
        *BufSize = Len;
    if (*BufSize > 0)
        memcpy(Buffer, Msg, *BufSize);
}
//---------------------------------------------------------------------------
// This is the function exported from the DLL. It is intended to be called
// from the application using the DLL.
int __stdcall IcsDllDemo(
    char *HostName,
    char *Port,
    char *Buffer,
    int  *BufSize)
{
    TClientThread *WorkerThread;
    int           Result;

    try {
        Result = -1;
        // Create a new thread. It is created in sleeping state
        WorkerThread           = new TClientThread;
        // Then pass all parameters
        WorkerThread->Buffer    = Buffer;
        WorkerThread->BufSize   = BufSize;
        WorkerThread->ErrorCode = &Result;
        WorkerThread->HostName  = HostName;
        WorkerThread->Port      = Port;
        // Then let thread start his work
        WorkerThread->Resume();
        // And wait until it finishes
        WaitForSingleObject((void *)WorkerThread->Handle, INFINITE);
    }
    catch (Exception &E)
    {
        AnsiString Buf;
        Buf = E.ClassName();
        Buf = Buf + ": " + E.Message;
        Result = -2;
        StrToBuffer(Buffer, BufSize, Buf.c_str());
    }
    return(Result);
};
//---------------------------------------------------------------------------
// This function is called by Windows when the DLL is loaded/unloaded and
// each time a thread attach/detach from the DLL
BOOL WINAPI DllEntryPoint(
    HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved)
{
    switch (fwdreason) {
    case DLL_PROCESS_ATTACH:
        // Increment WSocket reference count. This will make sure winsock will
        // remains loaded. This is because there is no permanent TWSocket
        // component created.
        WSocketGCount++;
        // This will load winsock and call WSAStartup
        WSocketGetProc("");
        break;
    case DLL_PROCESS_DETACH:
        // Decrement WSocket reference count.
        WSocketGCount--;
        // If reference count goes to zero, then unload winsock.
        if (WSocketGCount <= 0) {
            WSocketUnloadWinsock();
            WSocketGCount = 0;
        }
        break;
    }
    return(1);
}
//---------------------------------------------------------------------------
