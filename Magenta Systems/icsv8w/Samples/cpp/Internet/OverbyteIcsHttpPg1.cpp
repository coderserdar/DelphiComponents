/*---------------------------------------------------------------------------


Author:       François PIETTE
Creation:     December 4, 1997
Version:      1.04
Description:  Sample program to demonstrate some of the THttpCli features.
              (POST a message to a CGI)
              (requested by Walter Daniel Leon Salas" <wdaniel@hotmail.com>)
              You can see what HttpPg does automatically using your browser
              and surfing to http://www.unired.net.pe/mensatel.html HttpPg
              does programmatically what you can do manually at this page using
              your browser.
Email         francois.piette@overbyte.be      http://www.overbyte.be
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
Dec 28, 1997  Added a TMemo to display the POST's result.
Jan 16, 1998  Added a Proxy edit box. Added ini file stuff.
              Better error handling. Added abort button.
Apr 11, 1998  V1.02 Adapted for BCB3
Jul 23, 1998  V1.03 Corrected a parenthesis error in Encode.
              Thanks to Albert Wiersch <al@tetrion.com> for pointing this bug.
Aug 15, 1999  V1.04 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).
Apr 02, 2000  V1.05 Adapted for BCB5 (removed "#define _WINSPOOL_" SetPortA syndrome)

  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsHttpPg1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsHttpProt"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
#define HttpPgVersion 101
#define SectionData   "Data"
#define KeyUserID     "UserID"
#define KeyUserName   "UserName"
#define KeyEMail      "EMail"
#define KeyMessage    "Message"
#define KeyProxy      "Proxy"
#define SectionWindow "Window"
#define KeyTop        "Top"
#define KeyLeft       "Left"
#define KeyWidth      "Width"
#define KeyHeight     "Height"
THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
__fastcall THttpTestForm::THttpTestForm(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName.SetLength(FIniFileName.Length() - 3);
    FIniFileName = FIniFileName + "ini";
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormCreate(TObject *Sender)
{
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
AnsiString __fastcall Encode(AnsiString msg)
{
    int        I;
    AnsiString Result;
    char       ch;

//    Result = new AnsiString;
    Result = "";
    for (I = 1; I < msg.Length(); I++) {
        ch = (msg)[I];
        if (ch == ' ')
            Result = Result + "+";
        else if ((toupper(ch) < 'A') || (toupper(ch) > 'Z'))
            Result = Result + "%" + IntToHex(ch, 2);
        else
            Result = Result + ch;
    }
    return(Result);
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::SendButtonClick(TObject *Sender)
{
    TMemoryStream *DataIn;
    TMemoryStream *DataOut;
    AnsiString    Buf;
    BOOL          bSuccess;

    DisplayMemo->Clear();
    DataIn  = new TMemoryStream;
    DataOut = new TMemoryStream;
    bSuccess = TRUE;
    try {
        Buf     = "ID=" + UserIDEdit->Text +
                  "&REMITE=" + EMailEdit->Text +
                  "&MENSAJE=" + Encode(MessageEdit->Text);
        DataOut->Write(&Buf[1], Buf.Length());
        DataOut->Seek(0, soFromBeginning);

        HttpCli1->SendStream = DataOut;
        HttpCli1->RcvdStream = DataIn;
        HttpCli1->Proxy      = ProxyEdit->Text;
        HttpCli1->ProxyPort  = "80";
        HttpCli1->URL        = "http://www.unired.net.pe/cgi-bin/a.out";

        SendButton->Enabled  = FALSE;
        AbortButton->Enabled = TRUE;
        try {
            HttpCli1->Post();
        }
        __except (TRUE) {
            bSuccess = FALSE;
        }
        if (bSuccess) {
            DataIn->Seek(0, 0);
            DisplayMemo->Lines->LoadFromStream(DataIn);
        }
        SendButton->Enabled  = TRUE;
        AbortButton->Enabled = FALSE;
    }
    __except (TRUE) {
        bSuccess = FALSE;
    }
    if (!bSuccess)
        DisplayMemo->Lines->Add("Failed : " + HttpCli1->ReasonPhrase);

    delete DataOut;
    delete DataIn;
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::AbortButtonClick(TObject *Sender)
{
    HttpCli1->Abort();
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile      = new TIniFile(FIniFileName);
        UserIDEdit->Text  = IniFile->ReadString(SectionData, KeyUserID,
                           "27313");
        EMailEdit->Text   = IniFile->ReadString(SectionData, KeyEMail,
                           "francois.piette@pophost.eunet.be");
        ProxyEdit->Text   = IniFile->ReadString(SectionData, KeyProxy,
                           "");
        MessageEdit->Text = IniFile->ReadString(SectionData, KeyMessage,
                           "Hello World ! (Message sent by HttpPg).");

        Top    = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left   = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width  = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);

        delete IniFile;
    }
}
//---------------------------------------------------------------------------
void __fastcall THttpTestForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString(SectionData, KeyUserID,    UserIDEdit->Text);
    IniFile->WriteString(SectionData, KeyProxy,     ProxyEdit->Text);
    IniFile->WriteString(SectionData, KeyMessage,   MessageEdit->Text);
    IniFile->WriteString(SectionData, KeyEMail,     EMailEdit->Text);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
}
//---------------------------------------------------------------------------
