/*---------------------------------------------------------------------------

Author:       François PIETTE
Object:       Show how to use TPop3Cli (POP3 protocol, RFC-1225)
Creation:     03 october 1997
Version:      1.02 (Translated from Delphi version)
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
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

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <Inifiles.hpp>
#include <stdio.h>
#pragma hdrstop

#include "OverbyteIcsMailRcv1.h"
#include "OverbyteIcsMailRcv2.h"
#define IniFileName "MAILRCV.INI"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsPop3Prot"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TPOP3ExcercizerForm *POP3ExcercizerForm;
//---------------------------------------------------------------------------
__fastcall TPOP3ExcercizerForm::TPOP3ExcercizerForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::FormCreate(TObject *Sender)
{
    TIniFile *IniFile;

    IniFile            = new TIniFile(IniFileName);
    HostEdit->Text     = IniFile->ReadString("Data", "Host",     "");
    PortEdit->Text     = IniFile->ReadString("Data", "Port",     "");
    UserNameEdit->Text = IniFile->ReadString("Data", "UserName", "");
    PassWordEdit->Text = IniFile->ReadString("Data", "Password", "");
    delete IniFile;
    InfoLabel->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::FormCloseQuery(
    TObject *Sender, bool &CanClose)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(IniFileName);
    IniFile->WriteString("Data", "Host",     HostEdit->Text);
    IniFile->WriteString("Data", "Port",     PortEdit->Text);
    IniFile->WriteString("Data", "UserName", UserNameEdit->Text);
    IniFile->WriteString("Data", "Password", PassWordEdit->Text);
    delete IniFile;
}
//---------------------------------------------------------------------------
// This event handler is called when the TPop3Client object wants to display
// some information such as connection progress or errors.
void __fastcall TPOP3ExcercizerForm::Pop3ClientDisplay(
    TObject *Sender, String Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
// All the TPop3Client method are of the same type. To simplify this demo
// application, Exec transfert the parameters form the various EditBoxes
// to the Pop3Client instance and then call the appropriate method, showing
// the result in the InfoLabel->Caption->
void __fastcall TPOP3ExcercizerForm::Exec(
    TPop3NextProc MethodPtr,
    String        MethodName)
{
    Pop3Client->Host           = HostEdit->Text;
    Pop3Client->Port           = PortEdit->Text;
    Pop3Client->UserName       = UserNameEdit->Text;
    Pop3Client->PassWord       = PassWordEdit->Text;
    Pop3Client->MsgNum         = StrToInt(MsgNumEdit->Text);
    Pop3Client->MsgLines       = StrToInt(MsgLinesEdit->Text);
    // We need to reassign event handlers because we may have changed them
    // doing GetAllMessages for example
    Pop3Client->OnRequestDone  = Pop3ClientRequestDone;
    Pop3Client->OnMessageBegin = Pop3ClientMessageBegin;
    Pop3Client->OnMessageEnd   = Pop3ClientMessageEnd;
    Pop3Client->OnMessageLine  = Pop3ClientMessageLine;
    InfoLabel->Caption         = MethodName + " started";
    try {
        MethodPtr();
        InfoLabel->Caption = MethodName + " ok";
    } catch (Exception &E) {
        InfoLabel->Caption = MethodName + " failed (" + E.Message + ")";
    }
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ConnectButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Connect, "Connect");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::OpenButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Open, "Open");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::UserButtonClick(TObject *Sender)
{
    Exec(Pop3Client->User, "User");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::PassButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Pass, "Pass");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::QuittButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Quit, "Quit");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::RetrButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Retr, "Retr");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::StatButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Stat, "Stat");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ListAllButtonClick(TObject *Sender)
{
    MsgNumEdit->Text = "0";
    Exec(Pop3Client->List, "List All");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ListButtonClick(TObject *Sender)
{
    Exec(Pop3Client->List, "List");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::DeleteButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Dele, "Delete");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::NoopButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Noop, "Noop");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::LastButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Last, "Last");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ResetButtonClick(TObject *Sender)
{
    Exec(Pop3Client->RSet, "Rset");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::TopButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Top, "Top");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::RpopButtonClick(TObject *Sender)
{
    Exec(Pop3Client->RPop, "Rpop");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::UidlButtonClick(TObject *Sender)
{
    Exec(Pop3Client->Uidl, "Uidl");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ApopButtonClick(TObject *Sender)
{
    Exec(Pop3Client->APop, "Apop");
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client is about to receive a
// message. The MsgNum property gives the message number.
// This event handler could be used to open the file used to store the msg.
// The file handle could be stored in the TPop3Client->Tag property to be
// easily retrieved by the OnMessageLine and OnMessageEnd event handlers.
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageBegin(
      TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Message " +
                            IntToStr(((TPop3Cli *)Sender)->MsgNum) +
                            " begin ***");
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client has detected the end of a
// message, even if there is an error or exception, this event gets called.
// This event handler could be used to close the file used to store the msg.
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageEnd(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Message " +
                            IntToStr(((TPop3Cli*)Sender)->MsgNum) +
                            " end ***");
}
//---------------------------------------------------------------------------
// This event handler is called for each message line that TPop3Client is
// receiveing. This could be used to write the message lines to a file.
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageLine(TObject *Sender)
{
    DisplayMemo->Lines->Add(((TPop3Cli *)Sender)->LastResponse);
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client is about to receive a
// list line. The MsgNum property gives the message number.
void __fastcall TPOP3ExcercizerForm::Pop3ClientListBegin(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** List begin ***");
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client has received the last list
// line.
void __fastcall TPOP3ExcercizerForm::Pop3ClientListEnd(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** List End ***");
}
//---------------------------------------------------------------------------
// This event handler is called for each list line received by TPop3Client.
void __fastcall TPOP3ExcercizerForm::Pop3ClientListLine(TObject *Sender)
{
    AnsiString Buffer;

    Buffer = "MsgNum = " + IntToStr(((TPop3Cli *)Sender)->MsgNum) + " " +
             "MsgSize = " + IntToStr(((TPop3Cli *)Sender)->MsgSize) + " " +
             "Line = """ + ((TPop3Cli *)Sender)->LastResponse + """";
    if (DisplayMemo->Lines->Count > 200)
         DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Buffer);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::Pop3ClientUidlBegin(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Uidl begin ***");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::Pop3ClientUidlEnd(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Uidl end ***");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::Pop3ClientUidlLine(TObject *Sender)
{
    AnsiString Buffer;

    Buffer = "MsgNum = "  + IntToStr(((TPop3Cli *)Sender)->MsgNum) + " " +
             "MsgUidl = " + ((TPop3Cli *)Sender)->MsgUidl + """";
    if (DisplayMemo->Lines->Count > 200)
         DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Buffer);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::MessageBegin(TObject *Sender)
{
    MessageForm->Caption = "Message " +
                           IntToStr(((TPop3Cli *)Sender)->MsgNum);
    MessageForm->Show();
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::MessageLine(TObject *Sender)
{
    if (MessageForm->DisplayMemo->Lines->Count > 200)
         MessageForm->DisplayMemo->Clear();
    MessageForm->DisplayMemo->Lines->Add(((TPop3Cli *)Sender)->LastResponse);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::NextButtonClick(TObject *Sender)
{
    MessageForm->DisplayMemo->Clear();
    MessageForm->Caption       = "Message";
    Pop3Client->OnMessageBegin = MessageBegin;
    Pop3Client->OnMessageEnd   = NULL;
    Pop3Client->OnMessageLine  = MessageLine;
    Pop3Client->OnRequestDone  = NextMessageRequestDone;
    Pop3Client->MsgNum         = StrToInt(MsgNumEdit->Text);
    Pop3Client->Retr();
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::NextMessageRequestDone(
    TObject      *Sender,
    TPop3Request RqType,
    WORD         Error)
{
    if (Error != 0)
        return;

    MsgNumEdit->Text = IntToStr(StrToInt(MsgNumEdit->Text) + 1);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::GetAllMessageLine(TObject *Sender)
{
    fprintf(FFile, "%s\n", ((TPop3Cli *)Sender)->LastResponse.c_str());
}
//---------------------------------------------------------------------------
// The function here after will start an event chain that will eventually
// download all messages for the POP3 server. We cannot simply loop because
// the POP3 compomnet is asynchronous: it will not wait for operation done
// before returning. We must "chain" operations one after the other using
// the OnRequestDone event handler. We use the variable FGetAllState to keep
// track of where we are.
// To get all messages, we must first call Stat to know how many messages
// are on the server, then for each message we call Uidl to get a unique
// identifier for each message to build a file name and know if we already
// have a message, then we retrieve the message, then we increment the
// message number and continue until the number of messages is reached.
// We should start a TTimer to handle timeout...
void __fastcall TPOP3ExcercizerForm::GetAllButtonClick(TObject *Sender)
{
    TIniFile *IniFile;

    // Get path from INI file
    IniFile  = new TIniFile(IniFileName);
    FMsgPath = IniFile->ReadString("Data", "MsgPath",
                                   ExtractFilePath(Application->ExeName));
    delete IniFile;

    // Be sure to have an ending backslash
    if ((FMsgPath.Length() > 0) && (FMsgPath[FMsgPath.Length()] != '\\'))
        FMsgPath = FMsgPath + "\\";

    FGetAllState = 0;
    FFileOpened  = FALSE;
    Pop3Client->OnRequestDone  = GetAllRequestDone;
    Pop3Client->OnMessageBegin = NULL;
    Pop3Client->OnMessageEnd   = NULL;
    Pop3Client->OnMessageLine  = GetAllMessageLine;
    Pop3Client->Stat();
}
//---------------------------------------------------------------------------
// This event handler is called when a request related to GetAll is done.
// We check for errors and our state variable FGetAllState which tells us
// where we are (stat, uidl or retr which are the 4 commands we use.
// Note that we also could use Dele to remove the messages from the server.
void __fastcall TPOP3ExcercizerForm::GetAllRequestDone(
    TObject      *Sender,
    TPop3Request RqType,
    WORD         Error)
{
    if (Error) {
        if (FFileOpened) {
            FFileOpened = FALSE;
            fclose(FFile);
        }
        DisplayMemo->Lines->Add("Error " + Pop3Client->ErrorMessage);
        return;
    }

    try {
        switch (FGetAllState) {
        case 0: // Comes from the Stat command
            if (Pop3Client->MsgCount < 1) {
                DisplayMemo->Lines->Add("No message to download->");
                return;
            }
            Pop3Client->MsgNum = 1;    // Start with first message
            FGetAllState = 1;
            Pop3Client->Uidl();
            break;
        case 1: // Comes from the Uidl command
            FFileName = FMsgPath + "Msg " + Pop3Client->MsgUidl + "->txt";
            if (FileExists(FFileName)) {
                DisplayMemo->Lines->Add("Message " + IntToStr(Pop3Client->MsgNum) + " already here");
                if (Pop3Client->MsgNum == Pop3Client->MsgCount) {
                    DisplayMemo->Lines->Add("Finished");
                    return;
                }
                Pop3Client->MsgNum = Pop3Client->MsgNum + 1;
                FGetAllState = 1;
                Pop3Client->Uidl();
            }
            else {
                DisplayMemo->Lines->Add("Message " + IntToStr(Pop3Client->MsgNum));
                FFile        = fopen(FFileName.c_str(), "w");
                if (FFile == NULL)
                    throw Exception("Can't open file " + FFileName);
                FFileOpened  = TRUE;
                FGetAllState = 2;
                Pop3Client->Retr();
            }
            break;
        case 2: // Comes from the Retr command
            FFileOpened = FALSE;
            fclose(FFile);
            if (Pop3Client->MsgNum == Pop3Client->MsgCount) {
                DisplayMemo->Lines->Add("Finished");
                return;
            }
            Pop3Client->MsgNum = Pop3Client->MsgNum + 1;
            FGetAllState = 1;
            Pop3Client->Uidl();
            break;
        default:
            DisplayMemo->Lines->Add("Invalid state");
            return;
        }
    } catch (Exception &E) {
        if (FFileOpened) {
            FFileOpened = FALSE;
            fclose(FFile);
        }
        DisplayMemo->Lines->Add("Error: " + E.Message);
    }
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::Pop3ClientRequestDone(
    TObject      *Sender,
    TPop3Request RqType,
    WORD         Error)
{
    DisplayMemo->Lines->Add("Request Done Rq==" + IntToStr(Integer(RqType)) +
                            " Error==" + IntToStr(Error));

    if (RqType == pop3Stat) {
        InfoLabel->Caption = "Stat ok, " +
                             IntToStr(Pop3Client->MsgCount) + " messages " +
                             IntToStr(Pop3Client->MsgSize) + " bytes";
    }
    else if (RqType == pop3List) {
        InfoLabel->Caption = "List ok, " +
                             IntToStr(Pop3Client->MsgNum)  + " message " +
                             IntToStr(Pop3Client->MsgSize) + " bytes";
    }
    else if (RqType == pop3Last) {
        InfoLabel->Caption = "Last == " + IntToStr(Pop3Client->MsgNum);
    }
}
//---------------------------------------------------------------------------

