/*---------------------------------------------------------------------------

Program:      TNCLIENT.PAS
Object:       Delphi application which is a basic telnet program demonstrating
              WSocket, TnCnx, TnEmulVT, EmulVT components.
Creation:     July 22, 1997 (Delphi version)
Version:      2.05
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
Sep 05, 1997  Added display of windows socket version info.
Sep 23, 1997  Added local echo check box
Sep 24, 1997  V203. Added TnEmulVT1.RestoreOptions just before connecting
              Added interactive support for telnet echo option.
Sep 25, 1997  V204. Port to C++Builder
Sep 27, 1997  Final port to C++Builder
Apr 11, 1998  V2.05 Adapted to BCB3

---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsTnCli1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsEmulvt"
#pragma link "OverbyteIcsTnEmulvt"
#pragma link "OverbyteIcsEmulVT"
#pragma link "OverbyteIcsTnEmulVT"
#pragma resource "*.dfm"
TTelnetForm *TelnetForm;
//---------------------------------------------------------------------------
__fastcall TTelnetForm::TTelnetForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::ConnectButtonClick(TObject *Sender)
{
    StatusLabel->Caption = "Connecting";
    Refresh();
    ConnectButton->Enabled = FALSE;
    try {
        TnEmulVT1->Disconnect();
        TnEmulVT1->Port     = PortEdit->Text;
        TnEmulVT1->HostName = HostNameEdit->Text;
        // This can take quite a long time when hostname is unknown and
        // if DNS feature is enabled (2 or 3 minutes !)                 
        TnEmulVT1->Connect();
    }
    __except (TRUE) {
        StatusLabel->Caption   = "Connection failed";
        ConnectButton->Enabled = TRUE;
    }
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::DisconnectButtonClick(TObject *Sender)
{
    TnEmulVT1->Disconnect();
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::TnEmulVT1SessionConnected(TObject *Sender)
{
    DisconnectButton->Enabled = TRUE;
    StatusLabel->Caption      = "Connected";
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::TnEmulVT1SessionClosed(TObject *Sender)
{
    DisconnectButton->Enabled = FALSE;
    ConnectButton->Enabled    = TRUE;
    StatusLabel->Caption      = "Not connected";
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::SendButtonClick(TObject *Sender)
{
    TnEmulVT1->SendStr(RawByteString("Hello world !\r\n"));
    ActiveControl = TnEmulVT1;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::RequestLocalEchoOffButtonClick(TObject *Sender)
{
    TnEmulVT1->RequestLocalEcho(FALSE);
    ActiveControl = TnEmulVT1;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::RequestLocalEchoOnButtonClick(TObject *Sender)
{
    TnEmulVT1->RequestLocalEcho(TRUE);
    ActiveControl = TnEmulVT1;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::LocalEchoCheckBoxClick(TObject *Sender)
{
    TnEmulVT1->LocalEcho = LocalEchoCheckBox->Checked;
    ActiveControl        = TnEmulVT1;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::FormCreate(TObject *Sender)
{
    StatusLabel->Caption = "Not connected";
    TnEmulVT1->RestoreOptions();
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::FormResize(TObject *Sender)
{
    TnEmulVT1->Width  = ClientWidth;
    TnEmulVT1->Height = ClientHeight - TnEmulVT1->Top;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::FormShow(TObject *Sender)
{
    TWSAData WinsockData;

    WinsockData = WinsockInfo();
    StatusLabel->Caption = WinsockData.szDescription;
}
//---------------------------------------------------------------------------
void __fastcall TTelnetForm::OptionsButtonClick(TObject *Sender)
{
    TnEmulVT1->HostName = HostNameEdit->Text;
    TnEmulVT1->EditOptions();
}
//---------------------------------------------------------------------------

