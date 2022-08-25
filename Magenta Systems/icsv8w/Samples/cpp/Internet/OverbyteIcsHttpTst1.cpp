/*---------------------------------------------------------------------------


Author:       François PIETTE
Creation:     November 23, 1997
Version:      6.00
Description:  Sample program to demonstrate some of the THttpCli features.
Email         francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by François PIETTE
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
Jan 16, 1998  V1.00 Adapted for reviced HTTP component.
Mar 31, 1998  V1.01 Adapted for BCB3
Apr 13, 1998  V1.02 Call HttpCli1DocEnd when get failed to close the document
              file in he case it is already opened.
Aug 15, 1999  V1.03 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).
Apr 02, 2000  V1.04 Adapted for BCB5
Oct 02, 2007  V6.00 Adapted for BCB2007

  ---------------------------------------------------------------------------*/

#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsHttpTst1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsHttpProt"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
#define HttpTstVersion  104
THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
__fastcall THttpTestForm::THttpTestForm(TComponent* Owner)
    : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ExtractFileName(Forms::Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!Initialized) {
        Initialized         = TRUE;
        IniFile             = new TIniFile(FIniFileName);
        URLEdit->Text       = IniFile->ReadString("Data", "URL",       "");
        ProxyHostEdit->Text = IniFile->ReadString("Data", "ProxyHost", "");
        ProxyPortEdit->Text = IniFile->ReadString("Data", "ProxyPort", "80");
        DataEdit->Text      = IniFile->ReadString("Data", "Data",      "");
        DateTimeEdit->Text  = IniFile->ReadString("Data", "DateTime",  "");
        delete IniFile;
#ifdef NO_ADVANCED_HTTP_CLIENT_FEATURES
        DisplayMemo->Lines->Add("NO_ADVANCED_HTTP_CLIENT_FEATURES is defined !");
#endif
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString("Data", "URL",       URLEdit->Text);
    IniFile->WriteString("Data", "ProxyHost", ProxyHostEdit->Text);
    IniFile->WriteString("Data", "ProxyPort", ProxyPortEdit->Text);
    IniFile->WriteString("Data", "Data",      DataEdit->Text);
    IniFile->WriteString("Data", "DateTime",  DateTimeEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::HeadButtonClick(TObject *Sender)
{
    int I;

    DisplayMemo->Clear();
    DocumentMemo->Clear();
    SetButtonState(FALSE);

    HttpCli1->URL        = URLEdit->Text;
    HttpCli1->Proxy      = ProxyHostEdit->Text;
    HttpCli1->ProxyPort  = ProxyPortEdit->Text;
    HttpCli1->RcvdStream = NULL;
    if (DateTimeEdit->Text != "")
        HttpCli1->ModifiedSince = StrToDateTime(DateTimeEdit->Text);
    else
        HttpCli1->ModifiedSince = 0;

    try {
        HttpCli1->Head();
    } __except (TRUE) {
        SetButtonState(TRUE);
        DisplayMemo->Lines->Add("HEAD Failed !");
        DisplayMemo->Lines->Add("StatusCode   = " + IntToStr(HttpCli1->StatusCode));
        DisplayMemo->Lines->Add("ReasonPhrase = " + HttpCli1->ReasonPhrase);
        return;
    }

    DisplayMemo->Lines->Add("StatusCode = " + IntToStr(HttpCli1->StatusCode));

    for (I = 0; I < HttpCli1->RcvdHeader->Count; I++)
        DisplayMemo->Lines->Add("hdr>" + HttpCli1->RcvdHeader->Strings[I]);

    SetButtonState(TRUE);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::GetButtonClick(TObject *Sender)
{
    int     I;
    TStream *DataIn;

    DisplayMemo->Clear();
    DocumentMemo->Clear();
    SetButtonState(FALSE);

    HttpCli1->URL        = URLEdit->Text;
    HttpCli1->Proxy      = ProxyHostEdit->Text;
    HttpCli1->ProxyPort  = ProxyPortEdit->Text;
    HttpCli1->RcvdStream = NULL;
    if (DateTimeEdit->Text != "")
        HttpCli1->ModifiedSince = StrToDateTime(DateTimeEdit->Text);
    else
        HttpCli1->ModifiedSince = 0;

    try {
        HttpCli1->Get();
    } __except (TRUE) {
        SetButtonState(TRUE);
        DisplayMemo->Lines->Add("GET Failed !");
        DisplayMemo->Lines->Add("StatusCode   = " + IntToStr(HttpCli1->StatusCode));
        DisplayMemo->Lines->Add("ReasonPhrase = " + HttpCli1->ReasonPhrase);
        HttpCli1DocEnd(NULL);
        return;
    }

    DisplayMemo->Lines->Add("StatusCode = " + IntToStr(HttpCli1->StatusCode));

    for (I = 0; I < HttpCli1->RcvdHeader->Count; I++)
        DisplayMemo->Lines->Add("hdr>" + HttpCli1->RcvdHeader->Strings[I]);

    DataIn = new TFileStream(HttpCli1->DocName, fmOpenRead);
    DocumentMemo->Lines->LoadFromStream(DataIn);
    delete DataIn;

    SetButtonState(TRUE);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::PostButtonClick(TObject *Sender)
{
    TMemoryStream *DataOut;
    TFileStream   *DataIn;
    AnsiString    Buf;
    int           I;

    DisplayMemo->Clear();
    DocumentMemo->Clear();
    SetButtonState(FALSE);

    DataOut = new TMemoryStream;
    Buf     = DataEdit->Text;
    DataOut->Write(&Buf[1], Buf.Length());
    DataOut->Seek(0, soFromBeginning);

    HttpCli1->SendStream = DataOut;
    HttpCli1->Proxy      = ProxyHostEdit->Text;
    HttpCli1->ProxyPort  = ProxyPortEdit->Text;
    HttpCli1->RcvdStream = NULL;
    HttpCli1->URL        = URLEdit->Text;
    try {
        HttpCli1->Post();
    } __except (TRUE) {
        SetButtonState(TRUE);
        delete DataOut;
        DisplayMemo->Lines->Add("POST Failed !");
        DisplayMemo->Lines->Add("StatusCode   = " + IntToStr(HttpCli1->StatusCode));
        DisplayMemo->Lines->Add("ReasonPhrase = " + HttpCli1->ReasonPhrase);
        return;
    }
    delete DataOut;

    DisplayMemo->Lines->Add("StatusCode = " + IntToStr(HttpCli1->StatusCode));

    for (I = 0; I < HttpCli1->RcvdHeader->Count; I++)
        DisplayMemo->Lines->Add("hdr>" + HttpCli1->RcvdHeader->Strings[I]);

    DataIn = new TFileStream(HttpCli1->DocName, fmOpenRead);
    DocumentMemo->Lines->LoadFromStream(DataIn);
    delete DataIn;

    SetButtonState(TRUE);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::HttpCli1DocBegin(TObject *Sender)
{
    DisplayMemo->Lines->Add(HttpCli1->ContentType + " => " + HttpCli1->DocName);
    DisplayMemo->Lines->Add("Document = " + HttpCli1->DocName);
    HttpCli1->RcvdStream = new TFileStream(HttpCli1->DocName, fmCreate);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::HttpCli1DocEnd(TObject *Sender)
{
    if (HttpCli1->RcvdStream) {
        delete HttpCli1->RcvdStream;
        HttpCli1->RcvdStream = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::Check64ButtonClick(TObject *Sender)
{
    AnsiString Inp = "Aladdin:open sesame";
    AnsiString Res = "QWxhZGRpbjpvcGVuIHNlc2FtZQ==";

    if (EncodeLine(encBase64, &Inp[1], Inp.Length()) != Res)
        DisplayMemo->Lines->Add("Base64 encoding do not work !");
    else
        DisplayMemo->Lines->Add("Base64 encoding works OK !");
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SetButtonState(BOOL State)
{
    GetButton->Enabled   = State;
    PostButton->Enabled  = State;
    HeadButton->Enabled  = State;
    AbortButton->Enabled = !State;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::HttpCli1RequestDone(TObject *Sender,
    THttpRequest RqType, WORD Error)
{
    SetButtonState(TRUE);
    DisplayMemo->Lines->Add("RequestDone Error = " + IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::AbortButtonClick(TObject *Sender)
{
    HttpCli1->Abort();
}
//---------------------------------------------------------------------------

