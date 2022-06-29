/*---------------------------------------------------------------------------

NOTE: THIS IS AN OUTDATED SAMPLE PROGRAM. PLEASE SEE MAILRCV SAMPLE PROGRAM
      FOR UP-TO-DATE CODE. IF YOU REALLY WANTS TO USE THIS SAMPLE, YOU NEED
      TO INSTALL OLD POP3 COMPONENT WHICH IS IN POP3CLI.PAS FILE.
      
Author:       François PIETTE
Object:       Show how to use TPop3Cli (POP3 protocol, RFC-1225)
EMail:        francois.piette@pophost.eunet.be
WebSite:      http://www.rtfm.be/fpiette
Creation:     03 october 1997
Version:      1.00
Support:      Use twsocket@rtfm.be mailing list. See website for details.
Legal issues: Copyright (C) 1997, 1998, 1999 by François PIETTE 
              <francois.piette@pophost.eunet.be>

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
#include <vcl\vcl.h>
#include <vcl\inifiles.hpp>
#pragma hdrstop

#include "PopTst1.h"
//---------------------------------------------------------------------------
#pragma link "Wait"
#pragma link "pop3cli"
#pragma resource "*.dfm"
TPOP3ExcercizerForm *POP3ExcercizerForm;
//---------------------------------------------------------------------------
__fastcall TPOP3ExcercizerForm::TPOP3ExcercizerForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
// Restore some data from the INI file
void __fastcall TPOP3ExcercizerForm::FormCreate(TObject *Sender)
{
    TIniFile *IniFile;

    IniFile = new TIniFile("POPTST");
    HostEdit->Text     = IniFile->ReadString("Data", "Host",     "");
    UserNameEdit->Text = IniFile->ReadString("Data", "UserName", "");
    PassWordEdit->Text = IniFile->ReadString("Data", "Password", "");
    IniFile->Free();
    InfoLabel->Caption = "";
}
//---------------------------------------------------------------------------
// Save data to INI file
void __fastcall TPOP3ExcercizerForm::FormCloseQuery(TObject *Sender,
    bool &CanClose)
{
    TIniFile *IniFile;

    IniFile = new TIniFile("POPTST");
    IniFile->WriteString("Data", "Host",     HostEdit->Text);
    IniFile->WriteString("Data", "UserName", UserNameEdit->Text);
    IniFile->WriteString("Data", "Password", PassWordEdit->Text);
    IniFile->Free();
}
//---------------------------------------------------------------------------
// This event handler is called when the TPop3Client object wants to display
// some information such as connection progress or errors.
void __fastcall TPOP3ExcercizerForm::Pop3ClientDisplay(TObject *Sender,
    AnsiString Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
// All the TPop3Client method are of the same type. To simplify this demo
// application, DoTheJob transfert the parameters form the various EditBoxes
// to the Pop3Client instance and then call the appropriate method, showing
// the result in the InfoLabel.Caption.
BOOL __fastcall TPOP3ExcercizerForm::DoTheJob(
    TPop3Method MethodPtr,
    AnsiString  MethodName)
{
    BOOL Result;

    Pop3Client->Host     = HostEdit->Text;
    Pop3Client->UserName = UserNameEdit->Text;
    Pop3Client->PassWord = PassWordEdit->Text;
    Pop3Client->MsgNum   = StrToInt(MsgNumEdit->Text);
    Pop3Client->MsgLines = StrToInt(MsgLinesEdit->Text);
    InfoLabel->Caption   = MethodName + " started";
    Result = MethodPtr();
    if (Result)
        InfoLabel->Caption = MethodName + " ok";
    else
        InfoLabel->Caption = MethodName + " failed";
    return(Result);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ConnectButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Connect, "Connect");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::UserButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->User, "User");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::PassButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Pass, "Pass");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::RetrButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Retr, "Retr");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::DisconnectButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Quit, "Quit");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::TopButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Top, "Top");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::RpopButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Rpop, "Rpop");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::StatButtonClick(TObject *Sender)
{
    if (DoTheJob(Pop3Client->Stat, "Stat"))
        InfoLabel->Caption = "Stat ok, " +
                             IntToStr(Pop3Client->MsgCount) + " messages " +
                             IntToStr(Pop3Client->MsgSize) + " bytes";
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ListAllButtonClick(TObject *Sender)
{
    MsgNumEdit->Text = "0";
    DoTheJob(Pop3Client->List, "List All");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ListButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->List, "List");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::DeleteButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Dele, "Dele");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::NoopButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Noop, "Noop");
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::LastButtonClick(TObject *Sender)
{
    if (DoTheJob(Pop3Client->Last, "Last"))
        InfoLabel->Caption = "Last = " + IntToStr(Pop3Client->MsgNum);
}
//---------------------------------------------------------------------------
void __fastcall TPOP3ExcercizerForm::ResetButtonClick(TObject *Sender)
{
    DoTheJob(Pop3Client->Rset, "Rset");
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client is about to receive a       
// message. The MsgNum property gives the message number.
// This event handler could be used to open the file used to store the msg.
// The file handle could be stored in the TPop3Client.Tag property to be
// easily retrieved by the OnMessageLine and OnMessageEnd event handlers.
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageBegin(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Message " +
                            IntToStr(((TPop3Client *)Sender)->MsgNum) +
                            " begin ***");
}
//---------------------------------------------------------------------------
// This event handler is called when TPop3Client has detected the end of a
// message, even if there is an error or exception, this event gets called.
// This event handler could be used to close the file used to store the msg. 
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageEnd(TObject *Sender)
{
    DisplayMemo->Lines->Add("*** Message " +
                            IntToStr(((TPop3Client *)Sender)->MsgNum) +
                            " end ***");
}
//---------------------------------------------------------------------------
// This event handler is called for each message line that TPop3Client is
// receiveing. This could be used to write the message lines to a file.
void __fastcall TPOP3ExcercizerForm::Pop3ClientMessageLine(TObject *Sender)
{
    DisplayMemo->Lines->Add(((TPop3Client *)Sender)->LastResponse);
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
    DisplayMemo->Lines->Add(
        "MsgNum = " + IntToStr(((TPop3Client *)Sender)->MsgNum) + " " +
        "MsgSize = " + IntToStr(((TPop3Client *)Sender)->MsgSize) + " " +
        "Line = '" + ((TPop3Client *)Sender)->LastResponse + "'");
}
//---------------------------------------------------------------------------