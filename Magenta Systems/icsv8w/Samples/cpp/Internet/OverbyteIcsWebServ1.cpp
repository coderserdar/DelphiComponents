/*---------------------------------------------------------------------------

Author:       François PIETTE
Creation:     April 23, 2000
Description:  WebSrv1 show how to use THttpServer component to implement
              a web server.
Version:      1.01 (From Delphi version dated Oct 10, 1999)
Email         francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2000-2005 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:


---------------------------------------------------------------------------*/

#if __BORLANDC__ == 0x520     // BCB1 is BC5.20   BCB3 is BC5.30
    #define _WINSOCKAPI_      // Prevent winsock.h from being included
    #define s_addr S_addr
#endif
#include <vcl.h>
#include <inifiles.hpp>
#pragma hdrstop

#include "OverbyteIcsWebServ1.h"
#define  CopyRight "WebServ (c) 1999-2001 F. Piette V1.01 "
// IniFile layout for persistent data
#define SectionWindow      "WindowMain"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
#define SectionData        "Data"
#define KeyDocDir          "DocDir"
#define KeyDefaultDoc      "DefaultDoc"
#define KeyPort            "Port"
#define KeyDisplayHeader   "DisplayHeader"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsHttpSrv"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TWebServForm *WebServForm;
//---------------------------------------------------------------------------
__fastcall TWebServForm::TWebServForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
// We need to override parent class destructor because we have allocated
// memory for our data buffer.
__fastcall TMyHttpConnection::~TMyHttpConnection()
{
    if (FPostedDataBuffer != NULL) {
        free(FPostedDataBuffer);
        FPostedDataBuffer = NULL;
        FPostedDataSize   = 0;
    }
}
//---------------------------------------------------------------------------
void __fastcall TWebServForm::FormCreate(TObject *Sender)
{
    // Create IniFileName based on EXE file name
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TWebServForm::FormShow(TObject *Sender)
{
    TIniFile    *IniFile;
    WSAData     wsi;

    if (!FInitialized) {
        FInitialized     = TRUE;
        IniFile          = new TIniFile(FIniFileName);
        Top              = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left             = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width            = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height           = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        DocDirEdit->Text     = IniFile->ReadString(SectionData, KeyDocDir,
                                                  "c:\WwwRoot");
        DefaultDocEdit->Text = IniFile->ReadString(SectionData, KeyDefaultDoc,
                                                  "index.html");
        PortEdit->Text       = IniFile->ReadString(SectionData, KeyPort,
                                                  "80");
        DisplayHeaderCheckBox->Checked =
                IniFile->ReadInteger(SectionData, KeyDisplayHeader, 0);
        delete IniFile;
        DisplayMemo->Clear();
        // Initialize client count caption
        ClientCountLabel->Caption = "0";
        // Display version info for program and use components
        wsi = WinsockInfo();
        Display(CopyRight);
        Display("Using:");
        Display("    Winsock:");
        Display(Format("        Version %d.%d",
                       ARRAYOFCONST((wsi.wHighVersion >> 8,
                                     wsi.wHighVersion & 15))));
        Display(Format("        %s", ARRAYOFCONST((&wsi.szDescription[0]))));
        Display(Format("        %s", ARRAYOFCONST((&wsi.szSystemStatus[0]))));
        if (wsi.lpVendorInfo != NULL)
            Display(Format("        %s", ARRAYOFCONST((wsi.lpVendorInfo))));
        // Automatically start server
        StartButtonClick(this);
    }
}
//---------------------------------------------------------------------------
void __fastcall TWebServForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile->WriteString(SectionData,    KeyDocDir,      HttpServer1->DocDir);
    IniFile->WriteString(SectionData,    KeyDefaultDoc,  HttpServer1->DefaultDoc);
    IniFile->WriteString(SectionData,    KeyPort,        HttpServer1->Port);
    IniFile->WriteInteger(SectionData,   KeyDisplayHeader,
                                         DisplayHeaderCheckBox->Checked);
    delete IniFile;
}
//---------------------------------------------------------------------------
// Display a message in our display memo. Delete lines to be sure to not
// overflow the memo which may have a limited capacity.
void __fastcall TWebServForm::Display(AnsiString Msg)
{
    int I;

    DisplayMemo->Lines->BeginUpdate();
    if (DisplayMemo->Lines->Count > 200) {
        for (I = 1; I <= 50; I++)
            DisplayMemo->Lines->Delete(0);
    }
    DisplayMemo->Lines->Add(Msg);
    DisplayMemo->Lines->EndUpdate();
    // Makes last line visible
    SendMessage(DisplayMemo->Handle, EM_SCROLLCARET, 0, 0);
}
//---------------------------------------------------------------------------
// This event handler is called when user clicks on start button. It is also
// called from FormShow event handler, at program startup. It starts server.
// We need to pass default document, document directory and client class
// to HTTP server component. Client class is very usefull because it
// instruct server component to instanciate our own client class instead of
// defualt client class. Using our own client class will enables you to add
// any data we need to handle our application. This data is private for each
// client.
// When server is started, we will get OnServerStarted event triggered.
void __fastcall TWebServForm::StartButtonClick(TObject *Sender)
{
    HttpServer1->DocDir      = Trim(DocDirEdit->Text);
    HttpServer1->DefaultDoc  = Trim(DefaultDocEdit->Text);
    HttpServer1->Port        = Trim(PortEdit->Text);
    HttpServer1->ClientClass = __classid(TMyHttpConnection);
    HttpServer1->Start();
}
//---------------------------------------------------------------------------
// This event handler is triggered when user clicks on stop button. We just
// stop the server. We will get OnServerStopped event triggered.
void __fastcall TWebServForm::StopButtonClick(TObject *Sender)
{
    HttpServer1->Stop();
}
//---------------------------------------------------------------------------
// This event handler is triggered when user clicks on clear buttoN; We just
// clear the memo used for displaying activity.
void __fastcall TWebServForm::ClearButtonClick(TObject *Sender)
{
    DisplayMemo->Clear();
}
//---------------------------------------------------------------------------
// This event handler is triggered when HTTP server is started, that is when
// server socket has started listening.
void __fastcall TWebServForm::HttpServer1ServerStarted(TObject *Sender)
{
    DocDirEdit->Enabled     = FALSE;
    DefaultDocEdit->Enabled = FALSE;
    PortEdit->Enabled       = FALSE;
    StartButton->Enabled    = FALSE;
    StopButton->Enabled     = TRUE;
    Display("Server is waiting for connections");
}
//---------------------------------------------------------------------------
// This event handler is triggered when server has been stopped, that is
// when server socket stop listening.
void __fastcall TWebServForm::HttpServer1ServerStopped(TObject *Sender)
{
    DocDirEdit->Enabled     = TRUE;
    DefaultDocEdit->Enabled = TRUE;
    PortEdit->Enabled       = TRUE;
    StartButton->Enabled    = TRUE;
    StopButton->Enabled     = FALSE;
    Display("Server stopped");
}
//---------------------------------------------------------------------------
// This event handler is triggered when a new client has connected.
void __fastcall TWebServForm::HttpServer1ClientConnect(
    TObject *Sender,                // HTTP server component
    TObject *Client,                // Client connecting
    WORD Error)                     //Error in connection
{
    ClientCountLabel->Caption = IntToStr(HttpServer1->ClientCount);
}
//---------------------------------------------------------------------------
// This event handler is triggered when a client is disconnecting, just
// before client component is closed.
void __fastcall TWebServForm::HttpServer1ClientDisconnect(
    TObject *Sender,                // HTTP server component
    TObject *Client,                // Client connecting
    WORD Error)                     //Error in connection
{
    ClientCountLabel->Caption = IntToStr(HttpServer1->ClientCount - 1);
}
//---------------------------------------------------------------------------
// This event handler is triggered when HTTP server component receive a HEAD
// command from any client.
// We just count the request, display a message and let HTTP server
// component handle everything.
// We should trap every URI we handle internally...
void __fastcall TWebServForm::HttpServer1HeadDocument(
    TObject *Sender,                // HTTP server component
    TObject *Client,                // Client connection issuing command
    THttpGetFlag &Flags)            // Tells what HTTP server has to do next
{
    FCountRequests = FCountRequests + 1;
    Display(IntToStr(FCountRequests) +
            ": HEAD " + ((TMyHttpConnection *)Client)->Path);
}
//---------------------------------------------------------------------------
// This event handler is triggered when HTTP server component receive a GET
// command from any client.
// We count the request, display a message and trap '/time.htm' path for
// special handling.
// There is no document time.htm on disk, we will create it on the fly. With
// a classic webserver we would have used a CGI or ISAPI/NSAPI to achieve
// the same goal. It is much easier here since we can use Delphi code
// directly to generate whatever we wants. Here for the demo we generate a
// page with server data and time displayed.
void __fastcall TWebServForm::HttpServer1GetDocument(
    TObject *Sender,                // HTTP server component
    TObject *Client,                // Client connection issuing command
    THttpGetFlag &Flags)            // Tells what HTTP server has to do nex
{
    // Count request and display a message
    FCountRequests++;
    Display(IntToStr(FCountRequests) +
            ": GET " + ((TMyHttpConnection *)Client)->Path);
    DisplayHeader((TMyHttpConnection *)Client);

    // Trap '/time.htm' path to dynamically generate an answer.
    if (CompareText(((THttpConnection *)Client)->Path, "/time.htm") == 0)
        CreateVirtualDocument_time_htm(Sender, Client, Flags);
}
//---------------------------------------------------------------------------
// This procedure is use to generate /time.htm document
void __fastcall TWebServForm::CreateVirtualDocument_time_htm(
    TObject *Sender,                // HTTP server component
    TObject *Client,                // Client connection issuing command
    THttpGetFlag &Flags)            // Tells what HTTP server has to do nex
{
    AnsiString Body;
    AnsiString Header;
    TMemoryStream *Stream;

    // Let HTTP server component know we will send data to client
    Flags  = hgWillSendMySelf;
    // Create a stream to hold data sent to client that is the answer
    // made of a HTTP header and a body made of HTML code.
    Stream = new TMemoryStream;
    Body   = "<HTML>"
                "<HEAD>"
                  "<TITLE>ICS WebServer Demo</TITLE>"
                "</HEAD>\r\n"
                "<BODY>"
                  "<H2>Time at server side:</H2>\r\n"
                  "<P>" + DateTimeToStr(Now()) +"</P>\r\n"
                "</BODY>"
              "</HTML>\r\n";
    Header = ((TMyHttpConnection *)Client)->Version + " 200 OK\r\n"
              "Content-Type: text/html\r\n"
              "Content-Length: " +
              IntToStr(Body.Length()) + "\r\n\r\n";
    Stream->Write(Header.data(), Header.Length());
    Stream->Write(Body.data(),   Body.Length());
    // We need to seek to start of stream !
    Stream->Seek(0, 0);
    // We ask server component to send the stream for us.
    ((TMyHttpConnection *)Client)->DocStream = Stream;
    ((TMyHttpConnection *)Client)->SendStream();
}
//---------------------------------------------------------------------------
void __fastcall TWebServForm::DisplayHeader(TMyHttpConnection *Client)
{
    int I;

    if (!DisplayHeaderCheckBox->Checked)
        return;
    for(I = 0; I < Client->RequestHeader->Count; I++)
        Display("HDR" + IntToStr(I + 1) + ") " +
                Client->RequestHeader->Strings[I]);
}
//---------------------------------------------------------------------------
