//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsReverseDnsSync1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWndControl"
#pragma link "OverbyteIcsWSocket"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::WSocket1DnsLookupDone(TObject *Sender, WORD Error)
{
    // Save error code in lower word and 1 in upper word
    ReverseDNSFlag = 0x10000 + (Error & 0xFFFF);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TForm1::getICSReverseDNS(AnsiString Addr)
{
    TWSocket *WSocket1;
    AnsiString Result = "";

    WSocket1 = new TWSocket(NULL);
    try {
        WSocket1->OnDnsLookupDone = WSocket1DnsLookupDone;
        ReverseDNSFlag = 0;
        WSocket1->ReverseDnsLookup(Addr);
        while (!ReverseDNSFlag) {
            Application->ProcessMessages();
            if (Application->Terminated)
                break;
        }
        if (ReverseDNSFlag & 0xFFFF)
            Result = "Error #" + IntToStr(ReverseDNSFlag & 0xFFFF);
        else
            Result = WSocket1->DnsResult;
    } __finally {
        delete WSocket1;
    }
    return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
    Edit1->Text = getICSReverseDNS("10.161.63.1") + " | " +
                  getICSReverseDNS("10.161.63.2");
}
//---------------------------------------------------------------------------
 