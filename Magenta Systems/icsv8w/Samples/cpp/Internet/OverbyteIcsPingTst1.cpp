/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  This demo show how to use the TPing object to ping any host.
Creation:     November 30, 1997
Version:      1.10
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
Dec 13, 1997 V1.01 Use the new OnEchoRequest and OnEchoReply events.
Apr 10, 1998 V1.02 Adapted for BCB3
Dec 27, 1998 V1.10 New argument in TPing events.

  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsPingTst1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsPing"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TPingTstForm *PingTstForm;
//---------------------------------------------------------------------------
__fastcall TPingTstForm::TPingTstForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::PingButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
    DisplayMemo->Lines->Add("Resolving host '" + HostEdit->Text + "'");
    PingButton->Enabled   = FALSE;
    CancelButton->Enabled = TRUE;
    Ping1->DnsLookup(HostEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::Ping1DnsLookupDone(TObject *Sender, WORD Error)
{
    CancelButton->Enabled = FALSE;
    PingButton->Enabled   = TRUE;

    if (Error != 0) {
        DisplayMemo->Lines->Add("Unknown Host '" + HostEdit->Text + "'");
        return;
    }

    DisplayMemo->Lines->Add("Host '" + HostEdit->Text + "' is " + Ping1->DnsResult);
    Ping1->Address = Ping1->DnsResult;
    Ping1->Ping();
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::Ping1Display(TObject *Sender, TObject *Icmp, AnsiString Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::CancelButtonClick(TObject *Sender)
{
    Ping1->CancelDnsLookup();
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::Ping1EchoRequest(TObject *Sender, TObject *Icmp)
{
    DisplayMemo->Lines->Add("Sending " + IntToStr(Ping1->Size) + " bytes to " +
                          Ping1->HostName + " (" + Ping1->HostIP + ")");
}
//---------------------------------------------------------------------------
void __fastcall TPingTstForm::Ping1EchoReply(TObject *Sender, TObject *Icmp, int Error)
{
    if (Error == 0)
        DisplayMemo->Lines->Add("Cannot ping host (" + Ping1->HostIP + ") : " +
                              Ping1->ErrorString);
    else
        DisplayMemo->Lines->Add("Received " + IntToStr(Ping1->Reply.DataSize) +
                              " bytes from " + Ping1->HostIP +
                              " in " + IntToStr((int)Ping1->Reply.RTT) + " msecs");
}
//---------------------------------------------------------------------------
