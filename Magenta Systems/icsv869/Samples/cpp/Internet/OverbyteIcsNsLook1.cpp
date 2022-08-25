/*---------------------------------------------------------------------------

Program:      NsLookup
Description:  Demo for DnsQuery ICS component.
Author:       François Piette
Creation:     Feb 7, 1999 (from Delphi version created January 29, 1999)
Version:      1.01
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2005 by François PIETTE
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
Aug 15, 1999  V1.01 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <IniFiles.hpp>
#pragma hdrstop

#include "OverbyteIcsNsLook1.h"
#define SectionWindow      "Window"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
#define SectionData        "Data"
#define KeyName            "Name"
#define KeyDns             "Dns"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsDnsQuery"
#pragma resource "*.dfm"
TNsLookupForm *NsLookupForm;
//---------------------------------------------------------------------------
__fastcall TNsLookupForm::TNsLookupForm(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::FormShow(TObject *Sender)
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
        NameEdit->Text = IniFile->ReadString(SectionData, KeyName, "inprise.com");
        DnsEdit->Text  = IniFile->ReadString(SectionData, KeyDns,  "193.121.171.135");
        DisplayMemo->Clear();
        delete IniFile;
    }
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile->WriteString(SectionData,    KeyName,   NameEdit->Text);
    IniFile->WriteString(SectionData,    KeyDns,    DnsEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::Display(String Msg)
{
    if (DisplayMemo->Lines->Count > 200)   // Prevent TMemo overflow
        DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::DumpDnsResponse(void)
{
    char       *P;
    int        I;
    int        Len;
    AnsiString Buf;

    Display("Response dump (" + IntToStr(DnsQuery1->ResponseLen) + " bytes):");
    P   = DnsQuery1->ResponseBuf;
    Len = DnsQuery1->ResponseLen;
    Buf = "";
    I   = 0;
    while (I < Len) {
        if ((*P >= ' ') && (*P <= '~'))
            Buf = Buf + *P;
        else
            Buf = Buf + '<' + IntToStr(*P) + '>';
        I++;
        P++;
        if ((I % 16) == 0) {
            Display("  " + Buf);
            Buf = "";
        }
    }
    if (Buf.Length())
        Display("  " + Buf);
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::DnsQuery1RequestDone(TObject *Sender, WORD Error)
{
    int        I;
    int        nIndex;
    AnsiString DottedIP;

    if (Error) {
        Display("Error #" + IntToStr(Error));
        return;
    }
    Display("ID                 : " + IntToStr(DnsQuery1->ResponseID));
    Display("ResponseCode       : " + IntToStr(DnsQuery1->ResponseCode));
    Display("OpCode             : " + IntToStr(DnsQuery1->ResponseOpCode));
    Display("Authoritative      : " + IntToStr((int)DnsQuery1->ResponseAuthoritative));
    Display("Truncation         : " + IntToStr((int)DnsQuery1->ResponseTruncation));
    Display("RecursionAvailable : " + IntToStr((int)DnsQuery1->ResponseRecursionAvailable));
    Display("QDCount            : " + IntToStr(DnsQuery1->ResponseQDCount));
    Display("ANCount            : " + IntToStr(DnsQuery1->ResponseANCount));
    Display("NSCount            : " + IntToStr(DnsQuery1->ResponseNSCount));
    Display("ARCount            : " + IntToStr(DnsQuery1->ResponseARCount));
    Display("ResponseLen        : " + IntToStr(DnsQuery1->ResponseLen));
    Display("QuestionName       : " + DnsQuery1->QuestionName);
    Display("QuestionType       : " + IntToStr(DnsQuery1->QuestionType));
    Display("QuestionClass      : " + IntToStr(DnsQuery1->QuestionClass));

    for (I = 0; I < DnsQuery1->ResponseANCount; I++) {
        Display("Answer #" + IntToStr(I + 1));
        Display("  AnswerName       : " + DnsQuery1->AnswerName[I]);
        Display("  AnswerType       : " + IntToStr(DnsQuery1->AnswerType[I]));
        Display("  AnswerClass      : " + IntToStr(DnsQuery1->AnswerClass[I]));
        Display("  AnswerTTL        : " + IntToStr(DnsQuery1->AnswerTTL[I]));
        nIndex = DnsQuery1->AnswerTag[I];
        if (nIndex >= 0) {
            switch (DnsQuery1->AnswerType[I]) {
            case DnsQueryMX:
                Display("  MXPreference     : " + IntToStr(DnsQuery1->MXPreference[nIndex]));
                Display("  MXExchange       : " + DnsQuery1->MXExchange[nIndex]);
                break;
            case DnsQueryA:
                DottedIP = inet_ntoa(DnsQuery1->Address[nIndex]);
                Display("  Address          : " + DottedIP);
                break;
            }
        }
    }
    // Dump complete response
    DumpDnsResponse();
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::ClearDisplayBitBtnClick(TObject *Sender)
{
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::MXLookupButtonClick(TObject *Sender)
{
    DnsQuery1->Addr = DnsEdit->Text;
    FRequestID      = DnsQuery1->MXLookup(NameEdit->Text);
    Display("Request ID         : " + IntToStr(FRequestID));
}
//---------------------------------------------------------------------------
void __fastcall TNsLookupForm::ALookupButtonClick(TObject *Sender)
{
    DnsQuery1->Addr = DnsEdit->Text;
    FRequestID      = DnsQuery1->ALookup(NameEdit->Text);
    Display("Request ID         : " + IntToStr(FRequestID));
}
//---------------------------------------------------------------------------
