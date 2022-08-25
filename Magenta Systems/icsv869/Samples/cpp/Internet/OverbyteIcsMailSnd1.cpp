/*---------------------------------------------------------------------------//


Author:       François PIETTE
Object:       How to use TSmtpCli component
Creation:     09 october 1997
Version:      2.01
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
Oct 26, 1997  V1.00 Released
Jan 10, 1998  V1.01 Added a Port property
Feb 15, 1998  V1.02 Added file attachement support
Mar 06, 1998  V1.03 Check for DisplayMemo overflow (100 lines allowed)
Aug 03, 1998  V2.00 Revised for new asynchronous SMTP component version
Aug 15, 1999  V2.01 Adapted for BCB4 (Moved FIniFileName initialization from
			  FormCreate to form constructor).


---------------------------------------------------------------------------*/
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsMailSnd1.h"
#define SectionData   "Data"
#define KeyHost       "HostName"
#define KeyPort       "Port"
#define KeyFrom       "From"
#define KeyTo         "To"
#define KeySubject    "Subject"
#define KeySignOn     "SignOn"
#define SectionWindow "Window"
#define KeyTop        "Top"
#define KeyLeft       "Left"
#define KeyWidth      "Width"
#define KeyHeight     "Height"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsSmtpProt"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TMailSndForm *MailSndForm;

//---------------------------------------------------------------------------

__fastcall TMailSndForm::TMailSndForm(TComponent* Owner)
	: TForm(Owner)
{
	FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
	FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::FormCreate(TObject *Sender)
{
	Application->OnException = ExceptionHandler;
	DisplayMemo->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile = new TIniFile(FIniFileName);
        HostEdit->Text    = IniFile->ReadString(SectionData, KeyHost,
                                               "localhost");
        PortEdit->Text    = IniFile->ReadString(SectionData, KeyPort,
                                               "smtp");
        FromEdit->Text    = IniFile->ReadString(SectionData, KeyFrom,
                                               "first->last@company->com");
        ToEdit->Text      = IniFile->ReadString(SectionData, KeyTo,
                                               "john->doe@acme;tartempion@brol->fr");
        SubjectEdit->Text = IniFile->ReadString(SectionData, KeySubject,
											   "This is the message subject");
		SignOnEdit->Text  = IniFile->ReadString(SectionData, KeySignOn,
											   "your name");

		Top    = IniFile->ReadInteger(SectionWindow, KeyTop,    (Screen->Height - Height) / 2);
		Left   = IniFile->ReadInteger(SectionWindow, KeyLeft,   (Screen->Width - Width) / 2);
		Width  = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
		Height = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);

		delete IniFile;
	}
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString(SectionData, KeyHost,      HostEdit->Text);
    IniFile->WriteString(SectionData, KeyPort,      PortEdit->Text);
    IniFile->WriteString(SectionData, KeyFrom,      FromEdit->Text);
    IniFile->WriteString(SectionData, KeyTo,        ToEdit->Text);
    IniFile->WriteString(SectionData, KeySubject,   SubjectEdit->Text);
    IniFile->WriteString(SectionData, KeySignOn,    SignOnEdit->Text);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::BuildRcptList(void)
{
	AnsiString Buf;
	int        I;

	SmtpClient->RcptName->Clear();
	Buf = ToEdit->Text;
	while (TRUE) {
		I = Buf.Pos(";");
		if (I <= 0) {
			SmtpClient->RcptName->Add(Trim(Buf));
			break;
		}
		else {
			SmtpClient->RcptName->Add(Trim(Buf.SubString(1, I - 1)));
			Buf.Delete(1, I);
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::SmtpClientDisplay(TObject *Sender, AnsiString Msg)
{
	//Memo boxes are not unlimited...
	if (DisplayMemo->Lines->Count > 100)
        DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::SmtpClientGetData(TObject *Sender,
	  int LineNum, Pointer MsgLine, int MaxLen, bool &More)
{
	int Len;
  AnsiString s;
	if (LineNum > MsgMemo->Lines->Count)
		More = FALSE;
	else {
    s = AnsiString(MsgMemo->Lines->Strings[LineNum - 1]);
		Len = s.Length();
		// Truncate the line if too long (should wrap to next line)
		if (Len >= MaxLen)
			strncpy((char *)MsgLine, s.c_str(), MaxLen - 1);
		else
			strcpy((char *)MsgLine, s.c_str());
	}
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::SmtpClientHeaderLine(TObject *Sender,
      Pointer Msg, int Size)
{
	// This demonstrate how to add a line to the message header
	// Just detect one of the header lines and add text at the end of this
	// line. Use \r\n to form a new line
	// Here we check for the From: header line and add a Comments: line
	if (strncmpi((char*)Msg, "From:", 5) == 0)
		strcat((char*)Msg, "\r\nComments: This is a test");
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::ClearDisplayButtonClick(TObject *Sender)
{
	DisplayMemo->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::ExceptionHandler(TObject *Sender, Exception *E)
{
	Application->ShowException(E);
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::ConnectButtonClick(TObject *Sender)
{
    SmtpClient->Host = HostEdit->Text;
    SmtpClient->Port = PortEdit->Text;
    SmtpClient->Connect();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::HeloButtonClick(TObject *Sender)
{
	SmtpClient->SignOn = SignOnEdit->Text;
	SmtpClient->Helo();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::OpenButtonClick(TObject *Sender)
{
	SmtpClient->Host   = HostEdit->Text;
	SmtpClient->Port   = PortEdit->Text;
	SmtpClient->SignOn = SignOnEdit->Text;
	SmtpClient->Open();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::MailFromButtonClick(TObject *Sender)
{
	SmtpClient->FromName = FromEdit->Text;
	SmtpClient->MailFrom();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::RcptToButtonClick(TObject *Sender)
{
	BuildRcptList();
	SmtpClient->RcptTo();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::DataButtonClick(TObject *Sender)
{
	BuildRcptList();
	SmtpClient->HdrFrom         = FromEdit->Text;
	SmtpClient->HdrTo           = ToEdit->Text;
	SmtpClient->HdrSubject      = SubjectEdit->Text;
	SmtpClient->EmailFiles      = FileAttachMemo->Lines;
	SmtpClient->Data();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::MailButtonClick(TObject *Sender)
{
	BuildRcptList();
	SmtpClient->HdrFrom         = FromEdit->Text;
	SmtpClient->HdrTo           = ToEdit->Text;
	SmtpClient->HdrSubject      = SubjectEdit->Text;
	SmtpClient->SignOn          = SignOnEdit->Text;
	SmtpClient->FromName        = FromEdit->Text;
	SmtpClient->EmailFiles      = FileAttachMemo->Lines;
	SmtpClient->Host            = HostEdit->Text;
	SmtpClient->Port            = PortEdit->Text;
	SmtpClient->Mail();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::QuitButtonClick(TObject *Sender)
{
	SmtpClient->Quit();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::AbortButtonClick(TObject *Sender)
{
	SmtpClient->Abort();
}
//---------------------------------------------------------------------------

void __fastcall TMailSndForm::SmtpClientRequestDone(TObject *Sender,
	  TSmtpRequest RqType, WORD Error)
{
	DisplayMemo->Lines->Add("RequestDone Rq=" + IntToStr(RqType) +
						  " Error=" + IntToStr(Error));
}
//---------------------------------------------------------------------------

