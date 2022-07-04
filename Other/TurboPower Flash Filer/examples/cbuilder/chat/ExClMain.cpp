//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "ExClMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffllbase"
#pragma link "ffllcomm"
#pragma link "ffllcomp"
#pragma link "fflllgcy"
#pragma resource "*.dfm"
TfrmCltMain *frmCltMain;
//---------------------------------------------------------------------------
__fastcall TfrmCltMain::TfrmCltMain(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::FormClose(TObject *Sender,
      TCloseAction &Action)
{
  // Make sure we are inactive.
  tpClient->State = ffesInactive;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::pbSendClick(TObject *Sender)
{
  TffnmChatText Request;
  if (efMessage->Text != "") {

    Request.IsPrivate = chkPrivate->Checked;
    Request.UserName = "";

    // Private?
    if (chkPrivate->Checked) {
      // Yes.  Verify a recipient is checked.
      if (lbUsers->ItemIndex < 0 ) {
        ShowMessage("No one is selected.");
        return;
      }
      else
        Request.UserName = lbUsers->Items->Strings[lbUsers->ItemIndex];
    }

    strcpy(Request.Text, efMessage->Text.c_str());
    efMessage->Text = "";

    tpClient->Post(0, FClientID, ffnmChatText, &Request, sizeof(Request),
                  1000, ffrmNoReplyExpected);
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::efMessageKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  // This is here to stop that really annoying bell.
  if (Key == 13 ) Key = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::lbOutputDrawItem(TWinControl *Control,
      int Index, TRect &Rect, TOwnerDrawState State)
{
  // color different messages
  if (lbOutput->Items->Count != 0 ) {
    String TempStr = lbOutput->Items->Strings[Index];
    if (TempStr.Pos(efUserName->Text + " :") != 0 ) {
      lbOutput->Canvas->Font->Color = clGreen;
    } else if (TempStr.Pos(ffc_Private) != 0 ) {
      lbOutput->Canvas->Font->Color = clRed;
    }
    lbOutput->Canvas->TextRect(Rect, Rect.Left, Rect.Top, TempStr);
  }
}
//---------------------------------------------------------------------------

void TfrmCltMain::RefreshServers()
{
  // Obtain a list of available chat servers.
  TStringList* ServerList = new TStringList;
  try {
    tpClient->GetServerNames(ServerList, 1000);

    // Populate the combobox.
    cmbServers->Items->Clear();
    cmbServers->Text = "";
    for (int i = 0; i < ServerList->Count;i++)
      cmbServers->Items->Add(ServerList->Strings[i]);

    if (ServerList->Count > 0)
      cmbServers->ItemIndex = 0;
    else
      cmbServers->Text = "<No servers found>";
  }
  __finally {
    delete ServerList;
  }
}

void TfrmCltMain::SetCtrlStates()
{
  bool IsConnected = tpClient->IsConnected();
  pbConnect->Enabled = !IsConnected;
  pbConnect->Default = pbConnect->Enabled;
  pbSend->Enabled = IsConnected;
  pbSend->Default = pbSend->Enabled;
  efMessage->Enabled = IsConnected;
  pbDisconnect->Enabled = IsConnected;
  efUserName->Enabled = !IsConnected;

  if (IsConnected)
    lblConnect->Caption = "Connected to " + tpClient->ServerName;
  else
    lblConnect->Caption = "Not connected";
}

void __fastcall TfrmCltMain::chkPrivateClick(TObject *Sender)
{
  // make a visual for private message
  if (chkPrivate->Checked )
    efMessage->Color = clAqua;
  else
    efMessage->Color = clWindow;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::pbExitClick(TObject *Sender)
{
  tpClient->State = ffesInactive;
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::FormShow(TObject *Sender)
{
  // Get the user's login and use that as their default chat name.
  DWORD aSize;
  char Buffer[256];
  if (GetUserName(Buffer, &aSize))
    efUserName->Text = Buffer;
  else
    efUserName->Text = "me";

  // Create a command handler & connect it to the transport.
  FChatHandler = new TffChatClntHandler(0);
  FChatHandler->Output = lbOutput;
  FChatHandler->UserList = lbUsers;
  tpClient->CommandHandler = FChatHandler;

  RefreshServers();

  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::pbConnectClick(TObject *Sender)
{
  // Requirement: Must have specified a user name.
  if (efUserName->Text == "" ) {
    ShowMessage("You must specify a user name.");
    return;
  }

  // Requirement: Must have selected a server.
  if (cmbServers->ItemIndex < 0 ) {
    ShowMessage("You must select a chat server.");
    return;
  }

  tpClient->ServerName = cmbServers->Items->Strings[cmbServers->ItemIndex];
  tpClient->State = ffesStarted;
  tpClient->EstablishConnection(efUserName->Text, 0, 100000, FClientID);
  SetCtrlStates();
  if (tpClient->IsConnected()) {
    lbOutput->Items->Add("Connected to " + tpClient->ServerName);
    lbOutput->ItemIndex = lbOutput->Items->Count - 1;
    lbOutput->ItemIndex = -1;
    efMessage->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::pbDisconnectClick(TObject *Sender)
{
  tpClient->State = ffesInactive;
  lbOutput->Items->Add("Disconnected from " + tpClient->ServerName);
  lbOutput->ItemIndex = lbOutput->Items->Count - 1;
  lbOutput->ItemIndex = -1;
  SetCtrlStates();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::FormDestroy(TObject *Sender)
{
  delete FChatHandler;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCltMain::pbRefreshServersClick(TObject *Sender)
{
  RefreshServers();
}
//---------------------------------------------------------------------------
