/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Server program demo using TWSocket.
Creation:     Dec 28, 1998 (From Delphi version created dec 8, 1997)
Version:      1.01
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2005 by François PIETTE
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
#include <IniFiles.hpp>
#pragma hdrstop

#include "OverbyteIcsSrvDemo1.h"
#include "OverbyteIcsSrvDemo2.h"
#define IniFileName "SrvDemo.ini'"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TSrvForm *SrvForm;
//---------------------------------------------------------------------------
__fastcall TSrvForm::TSrvForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall MyMessageBox(const System::String &Msg,
    const System::String &Title, int Flags)
{
    Application->MessageBox(Msg.c_str(), Title.c_str(), Flags);
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::FormShow(TObject *Sender)
{
    TIniFile   *IniFile;
    String     Buffer;

    if (!Initialized) {
        Initialized     = TRUE;
        IniFile         = new TIniFile(IniFileName);
        Top             = IniFile->ReadInteger("Window", "Top",    Top);
        Left            = IniFile->ReadInteger("Window", "Left",   Left);
        Width           = IniFile->ReadInteger("Window", "Width",  Width);
        Height          = IniFile->ReadInteger("Window", "Height", Height);
        PortEdit->Text  = IniFile->ReadString("Data",    "Port",   "telnet");
        delete IniFile;

        DataTable->DatabaseName = ExtractFilePath(Application->ExeName);
        try {
            DataTable->Open();
        } catch (const Exception &E) {
            Buffer = "Unable to open " + DataTable->DatabaseName +
                      DataTable->TableName;
            MyMessageBox(Buffer.c_str(), "Error", MB_OK);
            Application->Terminate();
            return;
        }
        StartServer();
    }
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(IniFileName);
    IniFile->WriteInteger("Window", "Top",    Top);
    IniFile->WriteInteger("Window", "Left",   Left);
    IniFile->WriteInteger("Window", "Width",  Width);
    IniFile->WriteInteger("Window", "Height", Height);
    IniFile->WriteString("Data", "Port",    PortEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::RestartButtonClick(TObject *Sender)
{
    StartServer();
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::StartServer(void)
{
    SrvSocket->Close();
    SrvSocket->Addr  = "0.0.0.0";
    SrvSocket->Port  = PortEdit->Text;
    SrvSocket->Proto = "tcp";
    SrvSocket->Listen();
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::SrvSocketSessionAvailable(TObject *Sender,
      WORD Error)
{
    TCliForm *Form;

    ClientNumber++;
    // Create a new TCliForm instance to handle the incomming client
    Form = new TCliForm(this);
    // Add the form address as an identifier in our client list
    ClientListBox->Items->Add(IntToStr((int)Form));
    // We request line mode, to receive only complete line.
    // TWSocket does all the job for us...
    Form->CliSocket->LineMode = TRUE;
    Form->CliSocket->LineEnd  = "\r\n";
    // Now accept the new client connection
    Form->CliSocket->HSocket  = SrvSocket->Accept();
    Form->DataTable           = DataTable;
    Form->Caption             = "Client " + IntToStr(ClientNumber);
    // Showing the form is not mandatory. In a real server, this can be
    // annoying to have a form displayed for each client. In some situation,
    // it may be handy to have a user interface for each connected client.
    Form->Show();
}
//---------------------------------------------------------------------------
void __fastcall TSrvForm::WMUser(TMessage Message)
{
    TCliForm *Form;
    int      I;

    Form = (TCliForm *)(Message.LParam);
    Form->Release();
    for (I = 0; I < ClientListBox->Items->Count; I++) {
        if (ClientListBox->Items->Strings[I] == IntToStr((int)Form)) {
            ClientListBox->Items->Delete(I);
            break;
        }
    }
}
//---------------------------------------------------------------------------
