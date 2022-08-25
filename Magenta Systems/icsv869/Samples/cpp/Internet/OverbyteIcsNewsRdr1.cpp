/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  This sample program show how to use TNntpCli to write a news
              enabled application.
Creation:     January 14, 1997
Version:      1.02
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
Feb 21, 1998 Ported form Delphi code
Apr 11, 1998 V1.01 Adapted to BCB3
Aug 15, 1999 V1.02 Adapted for BCB4
                   Added support for XHDR and MODE READER.
                   Corrected a bug that let Connect and Abort button
                   disabled when DNS lookup failed.

  ---------------------------------------------------------------------------*/
#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
#endif
#include <vcl.h>
#include <Inifiles.hpp>
#include <Registry.hpp>
#pragma hdrstop

#include "OverbyteIcsNewsRdr1.h"
//---------------------------------------------------------------------------
#pragma link "OverbyteIcsNntpCli"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
#define IniFileName "NEWSRDR.INI"
TNNTPForm *NNTPForm;
//---------------------------------------------------------------------------
__fastcall TNNTPForm::TNNTPForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::FormShow(TObject *Sender)
{
    TIniFile   *IniFile;
    AnsiString EMail;
    AnsiString UserName;
    TRegistry  *Reg;
    AnsiString Key;

    if (FInitialized)
        return;

    EMail    = "your.name@yourcompany.domain";
    UserName = "Your Name";

    // Get username and EMail from the Internet Explorer settings
    // Should add code for Netscape Navigator...
    Reg           = new TRegistry;
    Reg->RootKey  = HKEY_CURRENT_USER;
    Key           = "\Software\Microsoft\Internet Mail and News\Mail";
    if (Reg->OpenKey(Key, FALSE)) {
        EMail     = Reg->ReadString("Sender EMail");
        UserName  = Reg->ReadString("Sender Name");
    }
    Reg->CloseKey();
    delete Reg;

    FInitialized         = TRUE;
    IniFile              = new TIniFile(IniFileName);
    Top                  = IniFile->ReadInteger("Window", "Top",    Top);
    Left                 = IniFile->ReadInteger("Window", "Left",   Left);
    Width                = IniFile->ReadInteger("Window", "Width",  Width);
    Height               = IniFile->ReadInteger("Window", "Height", Height);
    ServerEdit->Text     = IniFile->ReadString("Data", "Server", "");
    ArticleNumEdit->Text = IniFile->ReadString("Data", "ArticleNum", "");
    ArticleIDEdit->Text  = IniFile->ReadString("Data", "ArticleID", "");
    FileEdit->Text       = IniFile->ReadString("Data", "File",   "nntprdr.txt");
    UserNameEdit->Text   = IniFile->ReadString("Data", "UserName",  "");
    PasswordEdit->Text   = IniFile->ReadString("Data", "Password",  "");
    UserEdit->Text       = IniFile->ReadString("Data", "User",
                                               "\"" + UserName +
                                               "\" <" + EMail + ">");
    GroupEdit->Text      = IniFile->ReadString("Data", "Group",
                                               "borland->public->delphi->internet");
    delete IniFile;
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(IniFileName);
    IniFile->WriteString("Data",    "Server",  ServerEdit->Text);
    IniFile->WriteString("Data",    "Group",   GroupEdit->Text);
    IniFile->WriteString("Data",    "ArticleNum", ArticleNumEdit->Text);
    IniFile->WriteString("Data",    "ArticleID",  ArticleIDEdit->Text);
    IniFile->WriteString("Data",    "File",       FileEdit->Text);
    IniFile->WriteString("Data",    "User",       UserEdit->Text);
    IniFile->WriteString("Data",    "UserName",   UserNameEdit->Text);
    IniFile->WriteString("Data",    "Password",   PasswordEdit->Text);
    IniFile->WriteInteger("Window", "Top",    Top);
    IniFile->WriteInteger("Window", "Left",   Left);
    IniFile->WriteInteger("Window", "Width",  Width);
    IniFile->WriteInteger("Window", "Height", Height);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::Display(const AnsiString Msg)
{
    // Limit the memo to 100 lines
    while (DisplayMemo->Lines->Count > 100)
         DisplayMemo->Lines->Delete(1);
    DisplayMemo->Lines->Add(Msg);
};
//---------------------------------------------------------------------------

void __fastcall TNNTPForm::NntpCli1SessionConnected(TObject *Sender, WORD Error)
{
    AbortButton->Enabled = TRUE;
    Display("Connected, StatusCode = " + IntToStr(NntpCli1->StatusCode));
    if (NntpCli1->PostingPermited)
        Display("Posting permited");
    else
        Display("Posting not permited");
    Display(NntpCli1->LastResponse);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NntpCli1SessionClosed(TObject *Sender, WORD Error)
{
    AbortButton->Enabled   = FALSE;
    ConnectButton->Enabled = TRUE;
    Display("Connection closed");
}
//---------------------------------------------------------------------------
// This event handler is called for each NNTP command when the command has
// been exected (correctly or not).
void __fastcall TNNTPForm::NntpCli1RequestDone(TObject *Sender,
	TNntpRequest RqType, WORD Error)
{
	Display("Request done. RqType = " + IntToStr(RqType) +
	        ". LastResponse = " + NntpCli1->LastResponse);

    if (Error == 0)
        Display("No error");
    else
        Display("Error #" + IntToStr(Error));

    switch (RqType) {
    case nntpConnect:
        if (Error != 0) {
            AbortButton->Enabled   = FALSE;
            ConnectButton->Enabled = TRUE;
            Display("Connect failed");
        }
        break;
    case nntpGroup:
        Display("ArticleEstimated = " + IntToStr(NntpCli1->ArticleEstimated));
        Display("ArticleFirst     = " + IntToStr(NntpCli1->ArticleFirst));
        Display("ArticleLast      = " + IntToStr(NntpCli1->ArticleLast));
        ArticleNumEdit->Text = IntToStr(NntpCli1->ArticleFirst);
        break;
    case nntpPost:
    case nntpQuit:
    case nntpAbort:
    case nntpHelp:
    case nntpNewGroups:
    case nntpNewNews:
    case nntpXOver:
    case nntpListOverViewFmt:
    case nntpAuthenticate:
    case nntpXHdr:
    case nntpModeReader:
        // Nothing to do
        break;
    case nntpDate:
        Display("Server Date is " + DateTimeToStr(NntpCli1->ServerDate));
        break;
    case nntpStatByNumber:
    case nntpStatByID:
    case nntpHeadByNumber:
    case nntpHeadByID:
    case nntpBodyByNumber:
    case nntpBodyByID:
    case nntpArticleByNumber:
    case nntpArticleByID:
    case nntpNext:
    case nntpLast:
        Display("ArticleNumber    = " + IntToStr(NntpCli1->ArticleNumber));
        Display("ArticleID        = <" + NntpCli1->ArticleID + ">");
        if (Error == 0) {
            ArticleNumEdit->Text = IntToStr(NntpCli1->ArticleNumber);
            ArticleIDEdit->Text  = NntpCli1->ArticleID;
        };
        break;
    default:
        Display("Unknown request type.");
    }

    // If any stream where used, destroy it
    if (FDataStream) {
        delete FDataStream;
        FDataStream = NULL;
    }
}
//---------------------------------------------------------------------------
// This event handler is called by TNntpCli when it has received data and
// don't know what to do with it. It should normally not occur !
void __fastcall TNNTPForm::NntpCli1DataAvailable(TObject *Sender, WORD Error)
{
    Display("Data: " + NntpCli1->LastResponse);
}
//---------------------------------------------------------------------------
// This event handler is called by TNntpCli component just before the
// component will begin receiving a message. It's a good place to open a
// file or start a progress bar.
void __fastcall TNNTPForm::NntpCli1MessageBegin(TObject *Sender)
{
    Display("Message begin");
}
//---------------------------------------------------------------------------
// This event handler is called by TNntpCli component for each line of an
// incomming message. Header line as well as body lines are comming here.
// It's a good place to write to a file or update screen or progress bar.
// It's also the place to intercept header lines.
void __fastcall TNNTPForm::NntpCli1MessageLine(TObject *Sender)
{
    AnsiString NewsGroupName;
    int        LastArticle;
    int		   FirstArticle;
    char       PostingFlag;

    Display("Line: " + NntpCli1->LastResponse);
    ParseListLine(NntpCli1->LastResponse,
                  NewsGroupName,
                  LastArticle,
                  FirstArticle,
                  PostingFlag);
    // It is the place to do something with NewsGroupName, LastArticle,
    // FirstArticle and PostingFlag
}
//---------------------------------------------------------------------------
// This event handler is called by TNntpCli component when a message has
// been received completely. It's a good place to close a file, delete the
// progress bar and alert user.
void __fastcall TNNTPForm::NntpCli1MessageEnd(TObject *Sender)
{
    Display("Message end");
}
//---------------------------------------------------------------------------
// This function is called internally to create a TFileStream if any file
// name is specified in the FileEdit box. If the edit box is blank, nil is
// returned. The TFileStream will be supplyed to the comoponent for every
// command which can take a TStream to store data such as ArticleByNum.
// The stream is destroyed in the OnRequestDone event handler.
Classes::TStream* __fastcall TNNTPForm::GetStream(void)
{
    // Delete the previous stream if not already done
    if (FDataStream) {
        delete FDataStream;
        FDataStream = NULL;
    };

    if (Trim(FileEdit->Text) == "")
        FDataStream = NULL;
    else {
        // Try to open the file stream. Trap errors.
        try {
            FDataStream = new TFileStream(Trim(FileEdit->Text), fmCreate);
        } __except(TRUE) {
            // Display an error message in our TMemo
            Display("Can't open " + FileEdit->Text);
            FDataStream = NULL;
            throw;  // Show the exception box
        }
    }
    return(FDataStream);
};
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::ConnectButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
    ConnectButton->Enabled = FALSE;
    NntpCli1->Host         = ServerEdit->Text;
    NntpCli1->Connect();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::AbortButtonClick(TObject *Sender)
{
    NntpCli1->Abort();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::QuitButtonClick(TObject *Sender)
{
    NntpCli1->Quit();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::GroupButtonClick(TObject *Sender)
{
    NntpCli1->Group(GroupEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NextButtonClick(TObject *Sender)
{
    NntpCli1->Next();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::LastButtonClick(TObject *Sender)
{
    NntpCli1->Last();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::ArticleByIDButtonClick(TObject *Sender)
{
    NntpCli1->ArticleByID(ArticleIDEdit->Text, GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::ArticleByNumberButtonClick(TObject *Sender)
{
    NntpCli1->ArticleByNumber(StrToInt(ArticleNumEdit->Text), GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::HeadByIDButtonClick(TObject *Sender)
{
    NntpCli1->HeadByID(ArticleIDEdit->Text, GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::HeadByNumberButtonClick(TObject *Sender)
{
    NntpCli1->HeadByNumber(StrToInt(ArticleNumEdit->Text), GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::BodyByIDButtonClick(TObject *Sender)
{
    NntpCli1->BodyByID(ArticleIDEdit->Text, GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::BodyByNumberButtonClick(TObject *Sender)
{
    NntpCli1->BodyByNumber(StrToInt(ArticleNumEdit->Text), GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::StatByIDButtonClick(TObject *Sender)
{
    NntpCli1->StatByID(ArticleIDEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::StatByNumberButtonClick(TObject *Sender)
{
    NntpCli1->StatByNumber(StrToInt(ArticleNumEdit->Text));
}
//---------------------------------------------------------------------------
int __fastcall MyMessageBox(const System::String &Msg,
    const System::String &Title, int Flags)
{
    return Application->MessageBox(Msg.c_str(), Title.c_str(), Flags);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::ListButtonClick(TObject *Sender)
{
    if (MyMessageBox("This could take a VERY long time, proceed ? ",
                                "Warning", MB_YESNO) != ID_YES)
        return;
    NntpCli1->List(GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NewGroupsButtonClick(TObject *Sender)
{
    NntpCli1->NewGroups(Now() - 10, FALSE, "", GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NewNewsButtonClick(TObject *Sender)
{
    NntpCli1->NewNews(Now() - 1, FALSE, GroupEdit->Text, "", GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::HelpButtonClick(TObject *Sender)
{
    NntpCli1->Help(GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::LineToStream(AnsiString Buf)
{
    Display("Line: " + Buf);
    Buf = Buf + "\r\n";
    FDataStream->WriteBuffer(Buf.c_str(), Buf.Length());
};
//---------------------------------------------------------------------------
// Posting a message require to build the message, including his header.
// Here we use a TMemoryStream to create a message on the fly. Normally we
// should use a TFileStream to get the message from a file where it has
// been written by some user interface.
void __fastcall TNNTPForm::PostButtonClick(TObject *Sender)
{
    // Delete the stream if not already done
    if (FDataStream) {
        delete FDataStream;
        FDataStream = NULL;
    }

    // Create a new stream in memory
    FDataStream = new TMemoryStream;

    // Write the message header
    LineToStream("From: " + UserEdit->Text);
    LineToStream("Newsgroups: " + GroupEdit->Text);
    LineToStream("Subject: Internet components (winsock)");
    LineToStream("Organization: None");
    LineToStream("X-Newsreader: NNTP component " 
                 "(http://www.rtfm.be/fpiette/indexuk.htm)");

    // End of header is a blank line
    LineToStream("");

    // Write the message body
    LineToStream("");
    LineToStream("The Internet Component Suite is a set of native");
    LineToStream("components for Borland Delphi (all versions,");
    LineToStream("including 16 bits) and Borland C++ Builder. The");
    LineToStream("major TCP/IP protocols are supported for building");
    LineToStream("client/server, intranet or Internet applications.");
    LineToStream("");
    LineToStream("TCP, UDP, TELNET, FTP, SMTP, POP3, PING, FINGER, HTTP,");
    LineToStream("NNTP and more. Each component has samples writen");
    LineToStream("in Delphi and in C++ Builder. Several client/server");
    LineToStream("applications, including an event-driven and a");
    LineToStream("multi-threaded server, a complete FTP client and");
    LineToStream("TELNET client with ansi emulation are provided.");
    LineToStream("Full source code provided for everything.");
    LineToStream("");
    LineToStream("The Internet Component Suite is freeware, royalty");
    LineToStream("free and support is done using a mailing list.");
    LineToStream("Visit our website and download now from");
    LineToStream("http://www.rtfm.be/fpiette/indexuk.htm");

    // Set stream pointer to beginning of stream because TNntpCli will post
    // from the current position
    FDataStream->Seek(0, soFromBeginning);

    // Ask the component to post the stream. The posting occurs in the
    // background ! We will receive the OnRequestDone event when done.
    // It's in this event handler that the stream must be destroyed
    NntpCli1->Post(FDataStream);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::XOverButtonClick(TObject *Sender)
{
    NntpCli1->XOver(ArticleNumEdit->Text, GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::OverViewFmtButtonClick(TObject *Sender)
{
    NntpCli1->ListOverViewFmt(GetStream());
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::DateButtonClick(TObject *Sender)
{
    NntpCli1->Date();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::AuthenticateButtonClick(TObject *Sender)
{
    NntpCli1->UserName = UserNameEdit->Text;
    NntpCli1->PassWord = PasswordEdit->Text;
    NntpCli1->Authenticate();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::ModeReaderButtonClick(TObject *Sender)
{
    NntpCli1->ModeReader();
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::XHdrButtonClick(TObject *Sender)
{
    NntpCli1->XHdr(GetStream(), "subject", ArticleNumEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NntpCli1XHdrBegin(TObject *Sender)
{
    Display("XHdr begin");
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NntpCli1XHdrEnd(TObject *Sender)
{
    Display("Xhdr End");
}
//---------------------------------------------------------------------------
void __fastcall TNNTPForm::NntpCli1XHdrLine(TObject *Sender)
{
    Display("XHdr: " + NntpCli1->LastResponse);
}
//---------------------------------------------------------------------------

