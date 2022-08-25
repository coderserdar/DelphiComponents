/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Show how to use TWSocket with SOCKS protocol to traverse
              a firewall.
Creation:     December 27, 1998 (from Delphi version created November 21, 1998)
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

History:
Aug 15, 1999  V1.01 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).


---------------------------------------------------------------------------*/
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsSocks1.h"
#define SectionWindow      "Windows"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
#define SectionData        "Data"
#define KeyTargetHost      "TargetHost"
#define KeyTargetPort      "TargetPort"
#define KeySocksServer     "SocksServer"
#define KeySocksPort       "SocksPort"
#define KeySocksUsercode   "SocksUsercode"
#define KeySocksPassword   "SocksPassword"
#define KeySocksAuth       "SocksAuthentification"
#define KeySocks4          "Socks4"
#define KeySocks5          "Socks5"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TSocksTestForm *SocksTestForm;
//---------------------------------------------------------------------------
__fastcall TSocksTestForm::TSocksTestForm(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        DisplayMemo->Clear();
        IniFile      = new TIniFile(FIniFileName);
        Width        = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Top          = IniFile->ReadInteger(SectionWindow, KeyTop,
                                            (Screen->Height - Height) / 2);
        Left         = IniFile->ReadInteger(SectionWindow, KeyLeft,
                                            (Screen->Width  - Width)  / 2);
        TargetHostEdit->Text    = IniFile->ReadString(SectionData, KeyTargetHost,    "");
        TargetPortEdit->Text    = IniFile->ReadString(SectionData, KeyTargetPort,    "");
        SocksServerEdit->Text   = IniFile->ReadString(SectionData, KeySocksServer,   "");
        SocksPortEdit->Text     = IniFile->ReadString(SectionData, KeySocksPort,     "1080");
        SocksUsercodeEdit->Text = IniFile->ReadString(SectionData, KeySocksUsercode, "");
        SocksPasswordEdit->Text = IniFile->ReadString(SectionData, KeySocksPassword, "");
        SocksAuthCheckBox->Checked = IniFile->ReadInteger(SectionData, KeySocksAuth, 0);
        Socks4RadioButton->Checked = IniFile->ReadInteger(SectionData, KeySocks4,    0);
        Socks5RadioButton->Checked = IniFile->ReadInteger(SectionData, KeySocks5,    1);
        delete IniFile;
    }
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile->WriteString(SectionData, KeyTargetHost,    Trim(TargetHostEdit->Text));
    IniFile->WriteString(SectionData, KeyTargetPort,    Trim(TargetPortEdit->Text));
    IniFile->WriteString(SectionData, KeySocksServer,   Trim(SocksServerEdit->Text));
    IniFile->WriteString(SectionData, KeySocksPort,     Trim(SocksPortEdit->Text));
    IniFile->WriteString(SectionData, KeySocksUsercode, Trim(SocksUsercodeEdit->Text));
    IniFile->WriteString(SectionData, KeySocksPassword, Trim(SocksPasswordEdit->Text));
    IniFile->WriteInteger(SectionData, KeySocksAuth, SocksAuthCheckBox->Checked);
    IniFile->WriteInteger(SectionData, KeySocks5,    Socks5RadioButton->Checked);
    IniFile->WriteInteger(SectionData, KeySocks4,    Socks4RadioButton->Checked);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::DisplayMsg(TObject *Sender, String &Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::ConnectButtonClick(TObject *Sender)
{
    if (Socks5RadioButton->Checked)
        WSocket1->SocksLevel = "5";
    else if (Socks4RadioButton->Checked && SocksAuthCheckBox->Checked)
        WSocket1->SocksLevel = "4A";
    else
        WSocket1->SocksLevel = "4";
    DisplayMemo->Lines->Add("Connecting using Socks" + WSocket1->SocksLevel);

    WSocket1->SocksServer         = Trim(SocksServerEdit->Text);
    WSocket1->SocksPort           = Trim(SocksPortEdit->Text);
    WSocket1->SocksUsercode       = Trim(SocksUsercodeEdit->Text);
    WSocket1->SocksPassword       = Trim(SocksPasswordEdit->Text);
    WSocket1->SocksAuthentication = (SocksAuthCheckBox->Checked) ?
                                       socksAuthenticateUsercode :
                                       socksNoAuthentication;
    WSocket1->Proto               = "tcp";
    WSocket1->Addr                = Trim(TargetHostEdit->Text);
    WSocket1->Port                = Trim(TargetPortEdit->Text);
    WSocket1->OnDebugDisplay      = DisplayMsg;
    WSocket1->Connect();
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::DisconnectButtonClick(TObject *Sender)
{
    WSocket1->Close();
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1SessionConnected(TObject *Sender,
      WORD Error)
{
    DisplayMemo->Lines->Add("Session connected to remote host.");
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1SocksConnected(TObject *Sender,
      WORD Error)
{
    DisplayMemo->Lines->Add("Session connected to socks server.");
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1SocksAuthState(TObject *Sender,
      TSocksAuthState AuthState)
{
    switch (AuthState) {
    case socksAuthStart:
        DisplayMemo->Lines->Add("Socks authentification start.");
        break;
    case socksAuthSuccess:
        DisplayMemo->Lines->Add("Socks authentification success.");
        break;
    case socksAuthFailure:
        DisplayMemo->Lines->Add("Socks authentification failure.");
        break;
    case socksAuthNotRequired:
        DisplayMemo->Lines->Add("Socks authentification not required.");
        break;
    default:
        DisplayMemo->Lines->Add("Unknown socks authentification state.");
        break;
    }
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1SessionClosed(TObject *Sender,
      WORD Error)
{
    DisplayMemo->Lines->Add("Session Closed");
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1DataAvailable(TObject *Sender,
      WORD Error)
{
    int  Len;
    int  I;
    char *p;

    Len = ((TWSocket *)Sender)->Receive(&FRcvBuf[FRcvCnt], sizeof(FRcvBuf) - FRcvCnt - 1);
    if (Len < 0)
        return;
    FRcvCnt = FRcvCnt + Len;
    FRcvBuf[FRcvCnt] = 0;

    while (FRcvCnt > 0) {
        p = strchr(FRcvBuf, '\n');
        if (p == NULL)
            return;
        I = p - FRcvBuf;

        FRcvBuf[I] = 0;
        if ((I > 0) && (FRcvBuf[I - 1] == '\r'))
            FRcvBuf[I - 1] = 0;

        DisplayMemo->Lines->Add("Received: \"" + (AnsiString)FRcvBuf + "\"");
        Move(&FRcvBuf[I + 1], &FRcvBuf[0], FRcvCnt - I);
        FRcvCnt = FRcvCnt - I - 1;
    }
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::WSocket1SocksError(TObject *Sender,
      int Error, AnsiString Msg)
{
    DisplayMemo->Lines->Add("Socks error #" + IntToStr(Error) + " " + Msg);
}
//---------------------------------------------------------------------------
void __fastcall TSocksTestForm::ClearButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
