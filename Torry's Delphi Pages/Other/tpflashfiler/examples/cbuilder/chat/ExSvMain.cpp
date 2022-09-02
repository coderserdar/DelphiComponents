//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExSvMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffllbase"
#pragma link "ffllcomm"
#pragma link "ffllcomp"
#pragma link "fflllgcy"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
  FChatHandler = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  if (FChatHandler)
    delete FChatHandler;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::pbSrvCtrlClick(TObject *Sender)
{
  if (tpMain->State == ffesInactive) {
    efSrvName->Enabled = false;
    pbSrvCtrl->Caption = "&Stop";

    // Create a command handler.
    FChatHandler = new TffChatSrvHandler(this);
    FChatHandler->Memo = memChat;

    // Connect the command handler to the transport.
    tpMain->CommandHandler = FChatHandler;
    tpMain->OnAddClient = FChatHandler->OnAddClient;
    tpMain->OnRemoveClient = FChatHandler->OnRemoveClient;

    // Start the transport.
    tpMain->ServerName = efSrvName->Text;
    tpMain->State = ffesStarted;
  }
  else {
    tpMain->State = ffesInactive;
    efSrvName->Enabled = true;
    pbSrvCtrl->Caption = "&Start";
  }
}
//---------------------------------------------------------------------------
