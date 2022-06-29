/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This is a demo program showing how to use the TFtpServer
              component to build a FTP server.
Creation:     Dec 19, 1998 (Translated from Delphi version)
Version:      1.02
EMail:        francois.piette@swing.be   francois.piette@pophost.eunet.be
              francois.piette@rtfm.be    http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996, 1997, 1998, 1999 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

History:
Aug 15, 1999  V1.02 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//---------------------------------------------------------------------------
#include <vcl.h>
#include <vcl\inifiles.hpp>
#pragma hdrstop

#define FtpServVersion = 102;
#include "FtpSrv1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "FtpSrv"
#pragma resource "*.dfm"
TFtpServerForm *FtpServerForm;
TLogMsg        *Log;
#define MainTitle "FTP Server - http://www.rtfm.be/fpiette"
// Ini file layout
#define SectionData       "Data"
#define KeyPort           "Port"

#define SectionWindow     "Window"
#define KeyTop            "Top"
#define KeyLeft           "Left"
#define KeyWidth          "Width"
#define KeyHeight         "Height"
#define KeyMinim          "RunMinimized"

#define STATUS_GREEN      0
#define STATUS_YELLOW     1
#define STATUS_RED        2

//---------------------------------------------------------------------------
__fastcall TLogMsg::TLogMsg(TComponent* Owner)
    : TComponent(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TLogMsg::Text(char Prefix, AnsiString Msg)
{
}
//---------------------------------------------------------------------------
__fastcall TFtpServerForm::TFtpServerForm(TComponent* Owner)
    : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FormCreate(TObject *Sender)
{
    // Create the Log object
    Log = new TLogMsg(this);

    InfoMemo->Clear();
    GreenImage->Visible = FALSE;
    RedImage->Visible   = TRUE;
    RedImage->Top       = GreenImage->Top;
    RedImage->Left      = GreenImage->Left;
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;
    int      Minim;

    if (!FInitialized) {
        FInitialized        = TRUE;
        Caption             = "Starting " MainTitle;
        Left = -Width;

        IniFile  = new TIniFile(FIniFileName);
        FXTop    = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        FXLeft   = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        FXWidth  = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        FXHeight = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Minim    = IniFile->ReadInteger(SectionWindow, KeyMinim,  0);

        IniFile->Free();

        LoadConfig();
        SaveConfig();    // Create the inifile keys if they don't exists

        // Be sure to always have the window visible
        // with a reasonable width and height
        if (FXLeft < 0)
            FXLeft = 0;
        if (FXTop < 0)
            FXTop = 0;
        if (FXWidth < 310)
            FXWidth = 310;
        if (FXHeight <= 250)
            FXHeight = 250;
        if ((FXLeft + FXWidth) > Screen->Width)
            FXLeft = Screen->Width - FXWidth;
        if ((FXTop + FXHeight) > Screen->Height)
            FXTop = Screen->Height - FXHeight;

        StartMinimizedCheckBox->Checked = (Minim != 0);

        // We use a custom message to initialize things once the form
        // is visible
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    TIniFile *IniFile;
    int      Minim;

    try {
        StopServer();
        Minim   = StartMinimizedCheckBox->Checked;
        IniFile = new TIniFile(FIniFileName);
        IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile->WriteInteger(SectionWindow, KeyMinim,  Minim);
        IniFile->WriteString(SectionData,    KeyPort,   FPort);
        IniFile->Free();
    } __except (TRUE) {
        // Ignore any exception when we are closing
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::LoadConfig(void)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    FPort   = IniFile->ReadString(SectionData, KeyPort, "ftp");
    IniFile->Free();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::SaveConfig(void)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString(SectionData, KeyPort, FPort);
    IniFile->Free();
}
//---------------------------------------------------------------------------
// This message handler is triggered by the FormShow event. We comes here    
// only when the form is visible on screen.
void __fastcall TFtpServerForm::WMAppStartup(TMessage &Msg)
{
    HWND       PrvWnd;
    AnsiString Buf;

    if (StartMinimizedCheckBox->Checked)
        Application->Minimize();
    Top    = FXTop;
    Left   = FXLeft;
    Width  = FXWidth;
    Height = FXHeight;

    // Prevent the server from running twice
    Buf = ClassName();
    PrvWnd = FindWindow(&Buf[1], MainTitle);
    if (PrvWnd) {
        Log->Text('E', "Server already running. Shutdown.");
        Close();
        return;
    }
    Caption = MainTitle;
    Update();               // It's nice to have the form completely displayed

    StartServer();
    UpdateClientCount();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::StartServer(void)
{
    GreenImage->Visible = FALSE;
    RedImage->Visible   = TRUE;
    Update();

    FtpServer1->Start();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::StopServer(void)
{
    FtpServer1->Stop();
    FtpServer1->DisconnectAll();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::MnuQuitClick(TObject *Sender)
{
    Close();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::MnuStopServerClick(TObject *Sender)
{
    StopServer();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::MnuStartServerClick(TObject *Sender)
{
    StartServer();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::ImagesDblClick(TObject *Sender)
{
    if (FtpServer1->Active)
        StopServer();
    else
        StartServer();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::UpdateClientCount(void)
{
    if (FtpServer1->ClientCount == 0)
        ClientCountLabel->Caption = "No user";
    else
        ClientCountLabel->Caption = IntToStr(FtpServer1->ClientCount) + " users";
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1ClientConnect(TObject *Sender,
      TFtpCtrlSocket *Client, WORD Error)
{
    // The next test shows how to refuse a client
    if (Client->GetPeerAddr() == "193.121.12.25") {
        Client->SendStr("421 Connection not allowed.\r\n");
        Client->Close();
        return;
    }
    InfoMemo->Lines->Add("! " + Client->GetPeerAddr() + " connected");
    UpdateClientCount();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1ClientDisconnect(TObject *Sender,
      TFtpCtrlSocket *Client, WORD Error)
{
    InfoMemo->Lines->Add("! " + Client->GetPeerAddr() + " disconnected");
    UpdateClientCount();
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1Start(TObject *Sender)
{
    GreenImage->Visible = TRUE;
    RedImage->Visible   = FALSE;
    InfoMemo->Lines->Add("! Server started");
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1Stop(TObject *Sender)
{
    GreenImage->Visible = FALSE;
    RedImage->Visible   = TRUE;
    InfoMemo->Lines->Add("! Server stopped");
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1StorSessionConnected(
      TObject *Sender, TFtpCtrlSocket *Client, TWSocket *Data, WORD Error)
{
    if (Error)
        InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                             " Data session failed to open. Error #" +
                           IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1StorSessionClosed(
      TObject *Sender, TFtpCtrlSocket *Client, TWSocket *Data, WORD Error)
{
    if (Error)
        InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                             " Data session closed. Error #" + IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1RetrDataSent(TObject *Sender,
      TFtpCtrlSocket *Client, TWSocket *Data, WORD Error)
{
    if (Error)
        InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                             " Data sent. Error #" + IntToStr(Error));
}
//---------------------------------------------------------------------------
// This event handler is called when the data session for a get file has
// been opened. This is a good place build a file or a stream if the data
// requested is not already stored in a file on the file system.
// This feature is very powerfull and enable the FTP protocol to be used to
// retrieve any kind of data. It this sample, we just check for C:\VIRTUAL
// directory. If this directory is curent, then a TMemoryStream is created
// on the fly with some data. If another directory is selected, the FTP
// server works as any other: just send the requested file, if it exist !
// This event handler is also a place where you can abort the file transfer.
// Simply trigger an exception and transfer will not take place.
// Note that if you just wants to prohibe access to some directory or file,
// the best place to code that is in the OnValidateGet or OnValidatePut
// event handlers.
void __fastcall TFtpServerForm::FtpServer1RetrSessionConnected(
      TObject *Sender, TFtpCtrlSocket *Client, TWSocket *Data, WORD Error)
{
    AnsiString Buf;

    if (Error)
        InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                             " Data session connected. Error #" + IntToStr(Error));
    else if (UpperCase(Client->FilePath).SubString(1, 19) == "C:\\VIRTUAL\\FORBIDEN")
        throw Exception("Access prohibed !");
    else if (UpperCase(Client->FilePath).SubString(1, 11) == "C:\\VIRTUAL\\") {
        InfoMemo->Lines->Add("! VIRTUAL FILE");
        Client->UserData = 1;        // Remember we created a stream
        if (Client->DataStream)
            delete Client->DataStream; // Prevent memory leaks
        Client->DataStream = new TMemoryStream;
        Buf = "This is a file created on the fly by the FTP server\r\n"
              "It could result of a query to a database or anything else.\r\n"
              "The request was: \"" + Client->FilePath + "\"\r\n";
        Client->DataStream->Write(&Buf[1], Buf.Length());
        Client->DataStream->Seek(0, 0);
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1RetrSessionClosed(
      TObject *Sender, TFtpCtrlSocket *Client, TWSocket *Data, WORD Error)
{
    if (Error)
        InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                           " Data session closed. Error #" + IntToStr(Error));
    if (Client->UserData == 1) {
        // We created a stream for a virtual file or dir. Delete the TStream
        if (Client->DataStream) {
            // There is no reason why we should not come here, but who knows ?
            delete Client->DataStream;
            Client->DataStream = NULL;
        }
        Client->UserData = 0;     // Reset the flag
    }
}
//---------------------------------------------------------------------------
// This event handler is called when the FTP component needs to build a
// directory listing. You can just return without doing anything then the
// component will build the directory for you, based on the actual disk
// content. But you can also build your own directory listing with anything
// you like in it. Just create a stream with the required content. The
// example below construct a virtual directory when the user is on the
// C:\VIRTUAL subdirectory (use elsewhere in this sample program).
void __fastcall TFtpServerForm::FtpServer1BuildDirectory(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &Directory, bool Detailed)
{
    AnsiString Buf;

    if (UpperCase(Client->Directory) != "C:\\VIRTUAL\\")
        return;
    InfoMemo->Lines->Add("! VIRTUAL DIR");
    Client->UserData   = 1;        // Remember we created a stream
    if (Client->DataStream)
        delete Client->DataStream; // Prevent memory leaks
    Client->DataStream = new TMemoryStream;
    if (Detailed)
        // We need to format directory lines according to the Unix standard
        Buf =
      "-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 FORBIDEN\r\n"
      "-rwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 TEST\r\n"
      "drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 SOME DIR\r\n";
    else
        Buf = "FORBIDEN\r\nTEST\r\r";
    Client->DataStream->Write(&Buf[1], Buf.Length());
    Client->DataStream->Seek(0, 0);
}
//---------------------------------------------------------------------------
// This event handler is called by the FTP component once it has built the   
// directory listing. We can use this handler to alter the listing, adding
// or removing some info. This sample add the 'virtual' directory.
void __fastcall TFtpServerForm::FtpServer1AlterDirectory(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &Directory, bool Detailed)
{
    AnsiString Buf;

    if (UpperCase(Client->Directory) != "C:\\")
        return;
    // Add our 'virtual' directory to the list
    if (Detailed) {
        // We need to format directory lines according to the Unix standard
        Buf =
        "drwxrwxrwx   1 ftp      ftp            0 Apr 30 19:00 VIRTUAL\r\n";
        Client->DataStream->Write(&Buf[1], Buf.Length());
    }
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1ClientCommand(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &Keyword, TFtpString &Params,
      TFtpString &Answer)
{
    InfoMemo->Lines->Add("< " + Client->GetPeerAddr() + " " +
                       Keyword + " " + Params);
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1AnswerToClient(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &Answer)
{
    InfoMemo->Lines->Add("> " + Client->GetPeerAddr() + " " + Answer);
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1Authenticate(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &UserName, TFtpString &Password,
      bool &Authenticated)
{
    // You should place here the code needed to authenticate the user.
    // For example a text file with all permitted username/password.
    // If the user can't be authenticated, just set Authenticated to
    // false before returning.
    // It is also the right place to setup Client.HomeDir
    // If you need to store info about the client for later processing
    // you can use Client->UserData to store a pointer to an object or
    // a record with the needed info.
    InfoMemo->Lines->Add("! " + Client->GetPeerAddr() +
                       " User \"" + UserName + "\" is authenticated");
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::FtpServer1ChangeDirectory(TObject *Sender,
      TFtpCtrlSocket *Client, TFtpString &Directory, bool &Allowed)
{
#ifdef never
    // It the right place to check if a user has access to a given directory
    // The example below disable C:\ access to non root user.
    if (UpperCase(Client->UserName) != "ROOT") &&
       (UpperCase(Client->Directory) = "C:\\")
       Allowed = FALSE;
#endif
}
//---------------------------------------------------------------------------
void __fastcall TFtpServerForm::Cleardisplay1Click(TObject *Sender)
{
    InfoMemo->Clear();
}
//---------------------------------------------------------------------------
