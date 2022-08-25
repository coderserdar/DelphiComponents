/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  How to use TnCnx (Telnet protocol) with a TMemo
Creation:     December 14, 1997
Version:      1.02
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
Apr 11, 1998 V1.01 Adapted for BCB3
Jun 26, 1998 V1.02 Corrected offset in MemoAddLines. Thanks to Larry Jackson
                   <lrj@gte.net> who suggested code changes.


  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsTnDemo1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsTnCnx"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
#define CR '\r'
#define LF '\n'
TTnDemoForm *TnDemoForm;
//---------------------------------------------------------------------------
__fastcall TTnDemoForm::TTnDemoForm(TComponent* Owner)
    : TForm(Owner)
{
    DisplayMemo->OnKeyPress = DisplayMemoKeyPress;
}
//---------------------------------------------------------------------------
// Display a message in the memo field, breaking with CR
void __fastcall MemoAddLines(TMemo *Memo, AnsiString *Msg)
{
    int Start, Stop;

    if (Memo->Lines->Count == 0)
        Memo->Lines->Add("");

    Start = 0;
    Stop  = Msg->Pos(CR);
    if (Stop == 0)
        Stop = Msg->Length();
    while (Start < Msg->Length()) {
        Memo->Lines->Strings[Memo->Lines->Count - 1] =
            Memo->Lines->Strings[Memo->Lines->Count - 1] +
            Msg->SubString(Start, Stop - Start);
        if ((*Msg)[Stop] == CR) {
            Memo->Lines->Add("");
            SendMessage(Memo->Handle, WM_KEYDOWN, VK_UP, 1);
        }
        Start = Stop + 1;
        if (Start > Msg->Length())
            break;
        if ((*Msg)[Start] == LF)
           Start++;
        Stop = Start;
        while ((Stop < Msg->Length()) && ((*Msg)[Stop] != CR))
            Stop++;
    }
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::ConnectButtonClick(TObject *Sender)
{
    TnCnx->Host      = HostEdit->Text;
    TnCnx->Port      = PortEdit->Text;
    TnCnx->TermType  = "VT100";
    TnCnx->LocalEcho = FALSE;
    TnCnx->Connect();
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::DisconnectButtonClick(TObject *Sender)
{
    TnCnx->Close();
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::TnCnxSessionConnected(TTnCnx *Sender, WORD Error)
{
    DisplayMemo->Clear();
    InfoLabel->Caption        = "Connected";
    DisplayMemo->Enabled      = TRUE;
    ConnectButton->Enabled    = FALSE;
    DisconnectButton->Enabled = TRUE;
    ActiveControl             = DisplayMemo;
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::TnCnxSessionClosed(TTnCnx *Sender, WORD Error)
{
    InfoLabel->Caption        = "Disconnected";
    DisplayMemo->Enabled      = FALSE;
    ConnectButton->Enabled    = TRUE;
    DisconnectButton->Enabled = FALSE;
    ActiveControl             = ConnectButton;
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::TnCnxDataAvailableX(TTnCnx *Sender, PChar Buffer,
    int Len)
{
    AnsiString Buf = Buffer;
    MemoAddLines(DisplayMemo, &Buf);
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::DisplayMemoKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift)
{
    Key = 0;
}
//---------------------------------------------------------------------------
void __fastcall TTnDemoForm::DisplayMemoKeyPress(TObject *Sender, Char &Key)
{
    TnCnx->Send(&Key, 1);
    if (Key == 13) {
        Key = 10;
        TnCnx->Send(&Key, 1);
    }
    Key = 0;
}
//---------------------------------------------------------------------------


void __fastcall TTnDemoForm::TnCnxDataAvailable(TTnCnx *Sender,
      Pointer Buffer, int Len)
{
    AnsiString Buf = (char *)Buffer;
    MemoAddLines(DisplayMemo, &Buf);
}
//---------------------------------------------------------------------------

