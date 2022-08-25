//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "OverbyteIcsSrvDemo2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TCliForm *CliForm;
//---------------------------------------------------------------------------
__fastcall TCliForm::TCliForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::FormShow(TObject *Sender)
{
    if (!Initialized) {
        Initialized   = TRUE;
        DisplayMemo->Clear();
        SendEdit->Text = "Hello world !";
        ActiveControl = SendEdit;
    }
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    PostMessage(((TForm *)Owner)->Handle, WM_USER, 0, (int)this);
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::ProcessCommand(AnsiString Cmd)
{
    AnsiString CommandVerb;
    AnsiString CommandTail;
    int        I, J;

    DisplayMemo->Lines->Add(Cmd);

    // Skip leading spaces
    I = 1;
    while ((I <= Cmd.Length()) && ((Cmd[I] == ' ') || (Cmd[I] == '\t')))
        I++;

    // Find separator and separe CommandVerb and CommandTail
    J = I;
    while (TRUE) {
        if (J >= Cmd.Length()) {
            CommandTail = "";
            CommandVerb = Cmd;
            break;
        }

        if ((Cmd[J] == ' ') || (Cmd[I] == '\t')) {
            CommandTail = Cmd.SubString(J, Cmd.Length() - J + 1);
            CommandVerb = Cmd.SubString(I, J - I);
            break;
        }
        J++;
    }
    CommandVerb = UpperCase(CommandVerb);
    CommandTail = Trim(CommandTail);

    if (CommandVerb == "LASTNAME") {
        DataTable->IndexName = "NOM";
        DataTable->SetKey();
        DataTable->FieldByName("NOM")->AsString = CommandTail;
    }
    else if (CommandVerb == "FIRSTNAME") {
        DataTable->IndexName = "PRENOM";
        DataTable->SetKey();
        DataTable->FieldByName("PRENOM")->AsString = CommandTail;
    }
    else {
        CliSocket->SendStr(RawByteString("Syntax error !\r\n"));
        return;
    }

    if (DataTable->GotoKey())
        CliSocket->SendStr(
            "\"" + DataTable->FieldByName("NOM")->AsString      + "\", " +
            "\"" + DataTable->FieldByName("PRENOM")->AsString   + "\", " +
            "\"" + DataTable->FieldByName("ADRESSE")->AsString  + "\", " +
            "\"" + DataTable->FieldByName("CP")->AsString       + "\", " +
            "\"" + DataTable->FieldByName("LOCALITE")->AsString + "\"\r\n");
    else
        CliSocket->SendStr(RawByteString("Not found.\r\n"));
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::CliSocketDataAvailable(TObject *Sender,
      WORD Error)
{
    int Len;

    // We use line mode. So when we call Receive, we always receive a
    // complete line, include end of line marker or nothing.
    Len = CliSocket->Receive(&Buffer[0], sizeof(Buffer) - 1);
    if (Len <= 0)
        return;

    // Remove end of line marker
    while ((Len > 0) &&
           ((Buffer[Len - 1] == '\r') || (Buffer[Len - 1] == '\n')))
        Len--;

    // Nul terminate the string
    Buffer[Len] = 0;
    // Display command in label
    LineLabel->Caption = Buffer;
    // Process command
    ProcessCommand(Buffer);
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::CliSocketSessionClosed(TObject *Sender,
      WORD Error)
{
    Close();
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::SendButtonClick(TObject *Sender)
{
    CliSocket->SendStr(SendEdit->Text + "\r\n");
    ActiveControl = SendEdit;
}
//---------------------------------------------------------------------------
void __fastcall TCliForm::DisconnectButtonClick(TObject *Sender)
{
    Close();
}
//---------------------------------------------------------------------------
