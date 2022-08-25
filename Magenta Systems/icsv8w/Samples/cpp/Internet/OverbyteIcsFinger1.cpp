#pragma link "OverbyteIcsWndControl"
/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Finger is a FINGER client
              Install the components in FingCli.pas and wsocket.pas first.
Creation:     December 19, 1997
Version:      1.01
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
Apr 11, 1998  V1.01 Adapted for BCB3

  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsFinger1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsFingCli"
#pragma link "OverbyteIcsWSocket"
#pragma resource "*.dfm"
#define BufferSize 2048
#define CR         '\r'
#define LF         '\n'

TFingerDemoForm *FingerDemoForm;
//---------------------------------------------------------------------------
__fastcall TFingerDemoForm::TFingerDemoForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
// Display a message in the memo field, breaking with CR
void __fastcall MemoAddLines(TMemo *Memo, AnsiString *Msg)
{
    int Start, Stop;

    if (Memo->Lines->Count == 0)
        Memo->Lines->Add("");

    Start = 1;
    Stop  = Msg->Pos(CR);


    if (Stop == 0) {
     Stop  = Msg->Pos(LF);
    }

    if (Stop == 0)
        Stop = Msg->Length() + 1;

    while (Start <= Msg->Length()) {
        Memo->Lines->Strings[Memo->Lines->Count - 1] =
            Memo->Lines->Strings[Memo->Lines->Count - 1] +
            Msg->SubString(Start, Stop - Start);
        if (((*Msg)[Stop] == CR) || ((*Msg)[Stop] == LF)) {
            Memo->Lines->Add("");
            SendMessage(Memo->Handle, WM_KEYDOWN, VK_UP, 1);
        }
        Start = Stop + 1;
        if (Start > Msg->Length())
            break;
        while (((*Msg)[Start] == LF) || ((*Msg)[Start] == CR))
           Start++;
        Stop = Start;

        for (;Stop < Msg->Length();Stop++) {
           if (((*Msg)[Stop] == CR) || ((*Msg)[Stop] == LF)) break;
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall MemoAddLines(TMemo *Memo, AnsiString Buf)
{
    MemoAddLines(Memo, &Buf);
}
//---------------------------------------------------------------------------
void __fastcall MemoAddLines(TMemo *Memo, char *Msg)
{
    AnsiString Buf = Msg;
    MemoAddLines(Memo, &Buf);
}
//---------------------------------------------------------------------------
void __fastcall TFingerDemoForm::QueryButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
    QueryButton->Enabled  = FALSE;
    CancelButton->Enabled = TRUE;
    FingerCli1->Query     = QueryEdit->Text;
    FingerCli1->StartQuery();
    MemoAddLines(DisplayMemo, "Query started.\r");
}
//---------------------------------------------------------------------------
void __fastcall TFingerDemoForm::FingerCli1SessionConnected(TObject *Sender,
	WORD Error)
{
    if (Error == 0)
        MemoAddLines(DisplayMemo, "Connected to host.\r");
}
//---------------------------------------------------------------------------
void __fastcall TFingerDemoForm::FingerCli1DataAvailable(TObject *Sender,
	WORD Error)
{
    char Buffer[BufferSize];
    int  Len;

    while (TRUE) {
        Len = FingerCli1->Receive(Buffer, sizeof(Buffer) - 1);
        if (Len <= 0)
            break;
        Buffer[Len] = 0;
        MemoAddLines(DisplayMemo, Buffer);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFingerDemoForm::FingerCli1QueryDone(TObject *Sender,
	WORD Error)
{
    if (Error) {
        if (Error == WSAECONNREFUSED)
            MemoAddLines(DisplayMemo, "No finger service available.\r");
        else if (Error == WSAETIMEDOUT)
            MemoAddLines(DisplayMemo, "Host unreachable.\r");
        else
            MemoAddLines(DisplayMemo, "Error #" + IntToStr(Error) + "\r");
    }
    MemoAddLines(DisplayMemo, "Done.\r");

    QueryButton->Enabled  = TRUE;
    CancelButton->Enabled = FALSE;
}
//---------------------------------------------------------------------------
void __fastcall TFingerDemoForm::CancelButtonClick(TObject *Sender)
{
    FingerCli1->Abort();
}
//---------------------------------------------------------------------------