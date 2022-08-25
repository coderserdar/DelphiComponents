/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Demo program to show how to use TWSocket object to broadcast
              UDP messages on the network. Use UDPLstn to listen to those
              UDP messages, or other UDP messages.
Creation:     April 4, 1997
Version:      2.03
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
Sep 06, 1997  Version 2.01
Apr 12, 1998  V2.02 Adapted for BCB3
Aug 15, 1999  V2.03 Removed FormPos dependency.
              Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).

---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsUdpSend1.h"
#define SectionWindow     "Window"
#define KeyTop            "Top"
#define KeyLeft           "Left"
#define KeyWidth          "Width"
#define KeyHeight         "Height"
#define SectionData       "Data"
#define KeyPort           "Port"
#define KeyMessage        "Message"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
{
    static BOOL FirstTime = TRUE;
    TIniFile    *IniFile;

    if (FirstTime) {
        FirstTime         = FALSE;
        IniFile           = new TIniFile(FIniFileName);
        Top               = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left              = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width             = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height            = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        PortEdit->Text    = IniFile->ReadString(SectionData, KeyPort,    "600");
        MessageEdit->Text = IniFile->ReadString(SectionData, KeyMessage, "");
        delete IniFile;
    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile->WriteString(SectionData, KeyPort,    PortEdit->Text);
    IniFile->WriteString(SectionData, KeyMessage, MessageEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SendButtonClick(TObject *Sender)
{
    WSocket->Proto = "udp";
    WSocket->Addr  = "255.255.255.255";
    WSocket->Port  = PortEdit->Text;
    WSocket->Connect();
    WSocket->SendStr(MessageEdit->Text);
    WSocket->Close();
}
//---------------------------------------------------------------------------