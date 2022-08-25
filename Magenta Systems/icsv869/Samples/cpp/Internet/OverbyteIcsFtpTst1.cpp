/*---------------------------------------------------------------------------

Author:       François PIETTE
Creation:     Aug 1997 (Delphi program)
Version:      2.23
Object:       Demo for TFtpClient object (RFC 959 implementation)
              It is a graphical FTP client program
              Compatible with Delphi 1, 2 and 3
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
Sep 13, 97    Added directory functions. Added button to show how to makes
              several transferts in one session
Sep 27, 97    Change identifiers names to be more standard with other sources
Jan 10, 98    Saved edit boxes content to an IniFile, added FileSize, Quote
              and RestartGet commands
Jan 25, 1998  Completely rewritten for new component version (Asynchronous)
Feb 02, 1998  V2.17 Added a checkbox to run the synchronous or asynchronous
              version of the component methods.
Feb 15, 1998  V2.18 Removed useless wait unit from the use clause.
              Added display of winsock information at startup.
Feb 21, 1998  V2.18 ported from Delphi to C++Builder
Feb 22, 1998  V2.19 Added Append and AppendFile commands
Mar 27, 1998  V2.20 Adapted for BCB version 3.0
              See the comments for FtpClient1Progress().
Mar 06, 1999  V2.21 Minor changes
Aug 15, 1999  V2.22 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).
Apr 02, 2000  V2.23 Removed "#define _WINSPOOL_" (SetPortA syndrome)
              Adapted for BCB5.

 ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <Inifiles.hpp>
#pragma hdrstop
#include "OverbyteIcsFtpTst1.h"
#include "OverbyteIcsFtpTst2.h"

#define FTPTstVersion 223
#define SectionData    "Data"
#define KeyHostName    "HostName"
#define KeyUserName    "UserName"
#define KeyPassWord    "PassWord"
#define KeyHostDir     "HostDir"
#define KeyPort        "Port"
#define KeyHostFile    "HostFile"
#define KeyLocalFile   "LocalFile"
#define SectionWindow  "Window"
#define KeyTop         "Top"
#define KeyLeft        "Left"
#define KeyWidth       "Width"
#define KeyHeight      "Height"
#define TEMP_FILE_NAME "FTPDIR.TXT"

//---------------------------------------------------------------------------
#pragma link "OverbyteIcsFtpCli"
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TFtpReceiveForm *FtpReceiveForm;
//---------------------------------------------------------------------------
__fastcall TFtpReceiveForm::TFtpReceiveForm(TComponent* Owner)
	: TForm(Owner)  
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FormCreate(TObject *Sender)
{
    DisplayMemo->Clear();
    InfoLabel->Caption  = "";
    StateLabel->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;
    TWSAData Data;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile =  new TIniFile(FIniFileName);
        HostNameEdit->Text  = IniFile->ReadString(SectionData, KeyHostName,
                                                  "ftp->simtel->net");
        PortEdit->Text      = IniFile->ReadString(SectionData, KeyPort,
                                                  "ftp");
        UserNameEdit->Text  = IniFile->ReadString(SectionData, KeyUserName,
                                                  "anonymous");
        PassWordEdit->Text  = IniFile->ReadString(SectionData, KeyPassWord,
                                                  "your->name@your->company->com");
        HostDirEdit->Text   = IniFile->ReadString(SectionData, KeyHostDir,
                                                  "/pub/simtelnet");
        HostFileEdit->Text  = IniFile->ReadString(SectionData, KeyHostFile,
                                                  "index->html");
        LocalFileEdit->Text = IniFile->ReadString(SectionData, KeyLocalFile,
                                                  "c:\temp\index->htm");

        Top    = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left   = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width  = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);

        delete IniFile;

        // Display winsock info
        Data = WinsockInfo();
        DisplayMemo->Lines->Add("Winsock verion " +
                                IntToStr(LOBYTE(Data.wVersion)) + "." +
                                IntToStr(HIBYTE(Data.wVersion)));
        DisplayMemo->Lines->Add(Data.szDescription);
        DisplayMemo->Lines->Add(Data.szSystemStatus);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FormClose(TObject *Sender,
	TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString(SectionData, KeyHostName,  HostNameEdit->Text);
    IniFile->WriteString(SectionData, KeyPort,      PortEdit->Text);
    IniFile->WriteString(SectionData, KeyUserName,  UserNameEdit->Text);
    IniFile->WriteString(SectionData, KeyPassWord,  PassWordEdit->Text);
    IniFile->WriteString(SectionData, KeyHostDir,   HostDirEdit->Text);
    IniFile->WriteString(SectionData, KeyHostFile,  HostFileEdit->Text);
    IniFile->WriteString(SectionData, KeyLocalFile, LocalFileEdit->Text);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::Display(
    TObject *Sender,
    System::String &Msg)
{
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ExitButtonClick(TObject *Sender)
{
    Close();    
}
//---------------------------------------------------------------------------
// BCB V1 and 3 do not agreed upon the Count argument type.
// V1 would like to have long and bcb3 would like to have int !
// In both case it is a 32 bit integer. You can safely ignore this error
// but do *NOT* remove the reference when the IDE ask for it !
// You can of course change to code to macth your BCB version !
void __fastcall TFtpReceiveForm::FtpClient1Progress64(TObject *Sender,
    __int64 Count, bool &Abort)
{
    InfoLabel->Caption = IntToStr(Count);
    InfoLabel->Repaint();
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DisplayFile(const System::String FileName)
{
    try {
        DirectoryForm->DirListBox->Items->LoadFromFile(FileName);
    } __except (TRUE) {
        DirectoryForm->DirListBox->Clear();
    }
    DirectoryForm->ShowModal();
}

//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FtpClient1RequestDone(TObject *Sender,
    TFtpRequest RqType, WORD Error)
{
    DisplayMemo->Lines->Add("Request " + IntToStr(RqType) + " Done.");
    DisplayMemo->Lines->Add("StatusCode = " + IntToStr(FtpClient1->StatusCode));
    DisplayMemo->Lines->Add("LastResponse was : \"" +
                            FtpClient1->LastResponse + "\"");
    if (Error == 0)
        DisplayMemo->Lines->Add("No error");
    else
        DisplayMemo->Lines->Add("Error = " + IntToStr(Error) +
                                " (" + FtpClient1->ErrorMessage + ")");

    if (Error == 0) {
        switch (RqType) {
        case ftpDirAsync:
        case ftpDirectoryAsync:
        case ftpLsAsync:
        case ftpListAsync:
            DisplayFile(TEMP_FILE_NAME);
            break;
        case ftpSizeAsync:
            DisplayMemo->Lines->Add("File size is " +
                                    IntToStr(FtpClient1->SizeResult) +
                                    " bytes" );
            break;
        case ftpPwdAsync:
        case ftpMkdAsync:
        case ftpCDupAsync:
        case ftpCwdAsync:
            DisplayMemo->Lines->Add("Directory is \"" +
                                    FtpClient1->DirResult + "\"");
            break;
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FtpClient1SessionConnected(TObject *Sender,
    WORD Error)
{
    DisplayMemo->Lines->Add("Session Connected, error = " + IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FtpClient1SessionClosed(TObject *Sender,
    WORD Error)
{
    DisplayMemo->Lines->Add("Session Closed, error = " + IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FtpClient1StateChange(TObject *Sender)
{
    StateLabel->Caption = IntToStr(FtpClient1->State);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ExecuteCmd(
    TSyncCmd SyncCmd,
    TASyncCmd ASyncCmd)
{
    if (SyncCheckBox->Checked) {
        if (SyncCmd())
            DisplayMemo->Lines->Add("Command Success");
        else
            DisplayMemo->Lines->Add("Command Failure");
    }
    else
        ASyncCmd();
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::OpenAsyncButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
    DisplayMemo->Lines->Add("Connect Async");
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Open, FtpClient1->OpenAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::QuitAsyncButtonClick(TObject *Sender)
{
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Quit, FtpClient1->QuitAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::CwdAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Cwd, FtpClient1->CwdAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::UserAsyncButtonClick(TObject *Sender)
{
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->User, FtpClient1->UserAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::PassAsyncButtonClick(TObject *Sender)
{
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Pass, FtpClient1->PassAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ConnectAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Connect, FtpClient1->ConnectAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::GetAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    ExecuteCmd(FtpClient1->Get, FtpClient1->GetAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ReceiveAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Receive, FtpClient1->ReceiveAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::AbortAsyncButtonClick(TObject *Sender)
{
    ExecuteCmd(FtpClient1->Abort, FtpClient1->AbortAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DirAsyncButtonClick(TObject *Sender)
{
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = TEMP_FILE_NAME;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Dir, FtpClient1->DirAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DirectoryAsyncButtonClick(TObject *Sender)
{
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = TEMP_FILE_NAME;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Directory, FtpClient1->DirectoryAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::LsAsyncButtonClick(TObject *Sender)
{
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = TEMP_FILE_NAME;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Ls, FtpClient1->LsAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ListAsyncButtonClick(TObject *Sender)
{
    DeleteFile(TEMP_FILE_NAME);
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = TEMP_FILE_NAME;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->List, FtpClient1->ListAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::SystAsyncButtonClick(TObject *Sender)
{
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Syst, FtpClient1->SystAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::SystemAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->System, FtpClient1->SystemAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::FileSizeAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->FileSize, FtpClient1->FileSizeAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::SizeAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Size, FtpClient1->SizeAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::MkdAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Mkd, FtpClient1->MkdAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::MkdirAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Mkdir, FtpClient1->MkdirAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RmdAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Rmd, FtpClient1->RmdAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RmdirAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Rmdir, FtpClient1->RmdirAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RenAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Ren, FtpClient1->RenAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RenameAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Rename, FtpClient1->RenameAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DeleAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Dele, FtpClient1->DeleAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DeleteAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Delete, FtpClient1->DeleteAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::PwdAsyncButtonClick(TObject *Sender)
{
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Pwd, FtpClient1->PwdAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::QuoteAsyncButtonClick(TObject *Sender)
{
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Quote, FtpClient1->QuoteAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::DoQuoteAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->DoQuote, FtpClient1->DoQuoteAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::PutAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Put, FtpClient1->PutAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::TransmitAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Transmit, FtpClient1->TransmitAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::TypeSetAsyncButtonClick(TObject *Sender)
{
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->TypeSet, FtpClient1->TypeSetAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RestGetAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    ExecuteCmd(FtpClient1->RestGet, FtpClient1->RestGetAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::RestartGetAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->RestartGet, FtpClient1->RestartGetAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::CDupAsyncButtonClick(TObject *Sender)
{
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->CDup, FtpClient1->CDupAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::ClearButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::AppendAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->Append, FtpClient1->AppendAsync);
}
//---------------------------------------------------------------------------
void __fastcall TFtpReceiveForm::AppendFileAsyncButtonClick(TObject *Sender)
{
    FtpClient1->HostName        = HostNameEdit->Text;
    FtpClient1->Port            = PortEdit->Text;
    FtpClient1->UserName        = UserNameEdit->Text;
    FtpClient1->PassWord        = PassWordEdit->Text;
    FtpClient1->HostDirName     = HostDirEdit->Text;
    FtpClient1->HostFileName    = HostFileEdit->Text;
    FtpClient1->LocalFileName   = LocalFileEdit->Text;
    FtpClient1->Binary          = cbBinary->Checked;
    FtpClient1->DisplayFileFlag = cbDisplay->Checked;
    FtpClient1->OnDisplay       = Display;
    ExecuteCmd(FtpClient1->AppendFile, FtpClient1->AppendFileAsync);
}
//---------------------------------------------------------------------------



