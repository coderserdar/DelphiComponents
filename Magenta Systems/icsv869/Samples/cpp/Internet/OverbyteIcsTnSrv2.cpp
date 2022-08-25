#pragma link "OverbyteIcsWndControl"
/*---------------------------------------------------------------------------

Copyright:    François PIETTE
              francois.piette@pophost.eunet.be  http://www.rtfm.be/fpiette
Creation:     September 27, 1997 (from Delphi version created in april 1996)
Description:  TnSrv implement a (very basic) Telnet server (daemon)
              Compatible with both Delphi 1 and Delphi 2
              Uses TWSocket to communicate with WinSock
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
Mar 27, 1998  Adapted to C++Builder V3.0

---------------------------------------------------------------------------*/
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsTnSrv2.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma resource "*.dfm"
TClientForm *ClientForm;
//---------------------------------------------------------------------------
__fastcall TClientForm::TClientForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::FormCreate(TObject *Sender)
{
    Memo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::Display(char *Msg)
{
    AnsiString *Temp;

    Temp = new AnsiString(Msg);
    Display(Temp);
    delete Temp;
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::Display(AnsiString *Msg)
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
// This is the command line interpreter. Should extend the code to support
// every command needed...
void __fastcall TClientForm::CommandInterpreter()
{
    // Process Command
    Socket->SendStr(RawByteString("\r\nExecuting command '" + FCommand + "'...\r\n"));

    FCommand = FCommand.UpperCase();
    if (FCommand == "EXIT")
        DisconnectButtonClick(this);
    else if (FCommand == "HELP")
        Socket->SendStr(RawByteString(
                        "List of commands:\r\n"
                        "    Exit      logoff from server\r\n"
                        "    Help      show this help screen\r\n"));
    else
        Socket->SendStr(RawByteString("Unknown command, ignoring"));

    Socket->SendStr(RawByteString("\r\n--> "));
    FCommand = "";
}
//---------------------------------------------------------------------------
// Process each charcter received to do minimal line editing                
void __fastcall TClientForm::ProcessChar(char Ch)
{
    if (Ch == '\b') {
        if (FCommand.Length() > 0) {
            FCommand.SetLength(FCommand.Length() - 1);
            Socket->SendStr(RawByteString("\b \b"));
        }
        else
            Socket->SendStr(RawByteString('\a'));
        return;
    }
    else if ((Ch == '\n') && FRcvdCR) {
        // Ignore LF just after CR (CR/LF is normal end of line)
        FRcvdCR = FALSE;
        return;
    }
    else if (Ch == '\r') {
        FRcvdCR = TRUE;
        CommandInterpreter();
        return;
    }
    else if (Ch == '\n') {
        CommandInterpreter();
        return;
    }

    // Ordinary character, put in buffer in some place left
    FCommand = FCommand + Ch;

    // Echo to client
    Socket->Send(&Ch, 1);
}
//---------------------------------------------------------------------------
// Event handler for datavailable. Called each time some data is received
void __fastcall TClientForm::SocketDataAvailable(TObject *Sender, WORD Error)
{
    int        Len;
    char       Buffer[256];
    TWSocket   *Socket;
    int        I;
    AnsiString Msg;

    Socket = (TWSocket *)Sender;
    Len = Socket->Receive(Buffer, sizeof(Buffer));
    if (Len == 0)
        // Remote has closed
        Display("\r\n**** Remote has closed ****\r\n");
    else if (Len < 0) {
        // An error has occured
        if (Socket->LastError != WSAEWOULDBLOCK) {
            Msg = "\r\n**** ERROR: " + IntToStr(Socket->LastError) +
                  " ****\r\n";
            Display(&Msg);
        }
    }
    else {
        Buffer[Len] = 0;
        Display(Buffer);
        for (I  = 0; Buffer[I]; I++)
            ProcessChar(Buffer[I]);
    }
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::SocketSessionClosed(TObject *Sender, WORD Error)
{
    Display("\r\n**** Remote has closed ****\r\n");
    PostMessage(AcceptForm->Handle, WM_DISCONNECT,
                                    DISCONNECT_REMOTE,
                                    (LPARAM)Reference);
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::FormDestroy(TObject *Sender)
{
    Socket->Shutdown(2);
    Socket->Close();
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::DisconnectButtonClick(TObject *Sender)
{
    // Post a message to server form asking to disconnect the client
    PostMessage(AcceptForm->Handle, WM_DISCONNECT,
                                    DISCONNECT_SELF,
                                    (LPARAM)Reference);
}
//---------------------------------------------------------------------------

void __fastcall TClientForm::FormShow(TObject *Sender)
{
    char *Buf = "Hello from TnSrv !\r\n--> ";
    Socket->Send(Buf, strlen(Buf));
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::SendButtonClick(TObject *Sender)
{
    AnsiString Buf;

    Buf = DataEdit->Text + "\r\n";
    Socket->Send(Buf.c_str(), Buf.Length());
    DataEdit->Text = "";
    ActiveControl  = DataEdit;
}
//---------------------------------------------------------------------------
// Adjust the position for each control in the form as the user resize it
void __fastcall TClientForm::FormResize(TObject *Sender)
{
    Memo->Height           = ClientHeight - DisconnectButton->Height -
                             DataEdit->Height - 30;
    DisconnectButton->Left = ClientWidth - DisconnectButton->Width - 10;
    SendButton->Left       = DisconnectButton->Left - SendButton->Width - 10;
    DisconnectButton->Top  = ClientHeight - DisconnectButton->Height - 10;
    SendButton->Top        = DisconnectButton->Top;
    DataEdit->Top          = DisconnectButton->Top - DataEdit->Height - 10;
    DataEdit->Width        = ClientWidth - 2 * DataEdit->Left;
}
//---------------------------------------------------------------------------