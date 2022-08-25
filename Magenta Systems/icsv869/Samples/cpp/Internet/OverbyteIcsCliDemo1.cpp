/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Demonstration for Client program using TWSocket.
Creation:     December 28, 1998 (Translated from Delphi 8 december 1997)
Version:      1.03
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2002 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> <francois.piette@pophost.eunet.be>

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
#include <IniFiles.hpp>
#pragma hdrstop

#include "OverbyteIcsCliDemo1.h"
#define IniFileName "OverbyteIcsCliDemo.ini"
//---------------------------------------------------------------------------
#pragma package(smart_init)
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
void __fastcall TClientForm::DisconnectButtonClick(TObject *Sender)
{
    CliSocket->Close();
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::SendButtonClick(TObject *Sender)
{
    if (CliSocket->State != wsConnected) {
        CliSocket->Proto = "tcp";
        CliSocket->Port  = PortEdit->Text;
        CliSocket->Addr  = ServerEdit->Text;
        CliSocket->Connect();
        // Connect is asynchronous (non-blocking)-> We will wait while the
        // session is connecting or application terminated->
        while (CliSocket->State == wsConnecting) {
            Application->ProcessMessages();
            if (Application->Terminated)
                return;
        }
    }
    // Be sure we are connected before sending anything
    if (CliSocket->State == wsConnected)
        CliSocket->SendStr(SendEdit->Text + "\r\n");
    ActiveControl = SendEdit;
    SendEdit->SelectAll();
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::ProcessCommand(AnsiString Cmd)
{
    DisplayMemo->Lines->Add(Cmd);
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::CliSocketDataAvailable(TObject *Sender,
      WORD Error)
{
    int      Len;
    int      I;

    Len = CliSocket->Receive(&Buffer[Count], sizeof(Buffer) - Count - 1);
    if (Len <= 0)
        return;

    Count              = Count + Len;
    Buffer[Count]      = 0;
    LineLabel->Caption = Buffer;

    while (TRUE) {
        I = 0;
        while ((I < Count) && (Buffer[I] != '\n'))
            I++;
        if (I >= Count)
            return;
        ProcessCommand(((AnsiString)Buffer).SubString(1, I));
        Count              = 0;
        LineLabel->Caption = "";
        if (I >= (int)strlen(Buffer))
            break;
        Move(&Buffer[I + 1], &Buffer, strlen(Buffer) - I);
        LineLabel->Caption = Buffer;
        Count              = strlen(Buffer);
    }
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::CliSocketSessionConnected(TObject *Sender,
      WORD Error)
{
    ConnectError = Error;
    if (Error)
        DisplayMemo->Lines->Add("Can't connect, error #" + IntToStr(Error));
    else
        DisconnectButton->Enabled = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::CliSocketSessionClosed(TObject *Sender,
      WORD Error)
{
    DisconnectButton->Enabled = FALSE;
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(IniFileName);
    IniFile->WriteInteger("Window", "Top",    Top);
    IniFile->WriteInteger("Window", "Left",   Left);
    IniFile->WriteInteger("Window", "Width",  Width);
    IniFile->WriteInteger("Window", "Height", Height);
    IniFile->WriteString("Data", "Server",  ServerEdit->Text);
    IniFile->WriteString("Data", "Port",    PortEdit->Text);
    IniFile->WriteString("Data", "Command", SendEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TClientForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (Initialized)
        return;
    Initialized = TRUE;
    IniFile         = new TIniFile(IniFileName);

    Top             = IniFile->ReadInteger("Window", "Top",    Top);
    Left            = IniFile->ReadInteger("Window", "Left",   Left);
    Width           = IniFile->ReadInteger("Window", "Width",  Width);
    Height          = IniFile->ReadInteger("Window", "Height", Height);

    PortEdit->Text   = IniFile->ReadString("Data", "Port",    "telnet");
    ServerEdit->Text = IniFile->ReadString("Data", "Server",  "localhost");
    SendEdit->Text   = IniFile->ReadString("Data", "Command", "LASTNAME CAESAR");

    delete IniFile;

    DisplayMemo->Clear();
    ActiveControl = SendEdit;
    SendEdit->SelectAll();
}
//---------------------------------------------------------------------------
