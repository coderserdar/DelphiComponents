/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Simple server program which just listen for clients and display
              all incomming data.
Creation:     Dec 30, 1998 (Translated from Delphi created Sep 29, 1998)
Version:      1.03
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
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

History:
Aug 15, 1999  V1.03 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <IniFiles.hpp>
#pragma hdrstop

#include "OverbyteIcsRecv1.h"
#define SectionWindow   "RecvForm"
#define KeyTop          "Top"
#define KeyLeft         "Left"
#define KeyWidth        "Width"
#define KeyHeight       "Height"
#define SectionData     "Data"
#define KeyPort         "Port"
#define KeyLinger       "Linger"
#define KeyBanner       "SendBanner"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWndControl"
#pragma link "OverbyteIcsWSocket"
#pragma resource "*.dfm"
TRecvForm *RecvForm;
//---------------------------------------------------------------------------
__fastcall TRecvForm::TRecvForm(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
    FClients     = new TList;
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::FormDestroy(TObject *Sender)
{
    if (FClients) {
        delete FClients;
        FClients = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile      = new TIniFile(FIniFileName);
        Width        = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Top          = IniFile->ReadInteger(SectionWindow, KeyTop,
                                            (Screen->Height - Height) / 2);
        Left         = IniFile->ReadInteger(SectionWindow, KeyLeft,
                                            (Screen->Width  - Width)  / 2);
        PortEdit->Text = IniFile->ReadString(SectionData, KeyPort, "telnet");
        LingerCheckBox->Checked = IniFile->ReadInteger(SectionData, KeyLinger, 0);
        BannerCheckBox->Checked = IniFile->ReadInteger(SectionData, KeyBanner, 1);
        Label2->Caption = "";
        delete IniFile;
    }
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile->WriteString(SectionData,    KeyPort,   PortEdit->Text);
    IniFile->WriteInteger(SectionData,   KeyLinger, LingerCheckBox->Checked);
    IniFile->WriteInteger(SectionData,   KeyBanner, BannerCheckBox->Checked);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::Display(AnsiString Msg)
{
    if (DisplayMemo->Lines->Count > 200)   // Prevent TMemo overflow
        DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::ActionButtonClick(TObject *Sender)
{
    if (ActionButton->Caption == "&Start") {
        WSocket1->Addr     = "0.0.0.0";
        WSocket1->Port     = PortEdit->Text;
        WSocket1->Proto    = "tcp";
        WSocket1->Listen();
        ActionButton->Caption = "&Stop";
        Display("Listening for clients");
    }
    else {
        WSocket1->Close();
        ActionButton->Caption = "&Start";
        Display("Not listening for clients");
    }
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::PortEditChange(TObject *Sender)
{
    WSocket1->Close();
    ActionButton->Caption = "&Start";
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::WSocket1SessionAvailable(TObject *Sender,
      WORD Error)
{
    TWSocket *NewClient;

    Display("Client connected");
    Label2->Caption = "";
    NewClient = new TWSocket(NULL);
    FClients->Add(NewClient);
    NewClient->LineMode            = TRUE;
    NewClient->OnDataAvailable     = ClientDataAvailable;
    NewClient->OnSessionClosed     = ClientSessionClosed;
    NewClient->HSocket             = WSocket1->Accept();
    if (LingerCheckBox->Checked)
        NewClient->LingerOnOff     = wsLingerOn;
    else
        NewClient->LingerOnOff     = wsLingerOff;
    NewClient->LingerTimeout       = 300;
    NewClient->SetLingerOption();
    if (BannerCheckBox->Checked)
        NewClient->SendStr(RawByteString("Hello !\r\n"));
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::ClientDataAvailable(TObject *Sender,
      WORD Error)
{
    char Buf[128];
    int  Len;

    Len = ((TWSocket *)Sender)->Receive(&Buf, sizeof(Buf) - 1);
    if (Len <= 0)
        return;

    // Remove any trailing CR/LF
    while ((Len > 0) &&
           ((Buf[Len - 1] == '\r') || (Buf[Len - 1] == '\n')))
        Len--;
    // Nul terminate data
    Buf[Len] = 0;

    Display("DataAvailable: \"" + (AnsiString)Buf + "\"");
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::ClientSessionClosed(TObject *Sender,
      WORD Error)
{
    TWSocket *Cli;
    int      Itm;

    Cli = (TWSocket *)Sender;
    Display("Client diconnected");

    Itm = FClients->IndexOf(Cli);
    if (Itm >= 0)
        FClients->Delete(Itm);
    // We can"t destroy a TWSocket from a SessionClosed event handler.
    // So we post a message to delay destruction until we are out of the
    // message handler.
    PostMessage(Handle, WM_DESTROY_SOCKET, 0, (LPARAM)Cli);
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::WMDestroySocket(TMessage Msg)
{
    delete (TWSocket *)(Msg.LParam);
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::CloseAllButtonClick(TObject *Sender)
{
    Display("Disconnecting clients");
    while (FClients->Count > 0)
        ((TWSocket *)(FClients->Items[0]))->Close();
    Display("All clients disconnected");
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::LineModeOnButtonClick(TObject *Sender)
{
    int I;

    for (I = 0; I < FClients->Count - 1; I++)
        ((TWSocket *)(FClients->Items[0]))->LineMode = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall TRecvForm::LineOffButtonClick(TObject *Sender)
{
    int I;

    for (I = 0; I < FClients->Count - 1; I++)
        ((TWSocket *)(FClients->Items[0]))->LineMode = FALSE;
}
//---------------------------------------------------------------------------
