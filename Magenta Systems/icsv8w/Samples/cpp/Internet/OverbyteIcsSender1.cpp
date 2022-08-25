/*---------------------------------------------------------------------------

Author:       François PIETTE
Description:  Simple client program which just send data to a server and display
              all incomming data.
Creation:     Dec 29, 1998 (From Delphi version created Oct 01, 1998)
Version:      1.03
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2005 by François PIETTE
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

History:
Aug 15, 1999  V1.03 Adapted for BCB4 (Moved FIniFileName initialization from
              FormCreate to form constructor).

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <IniFiles.hpp>
#include <stdlib.h>
#pragma hdrstop

#include "OverbyteIcsSender1.h"
#define SectionWindow   "RecvForm"
#define KeyTop          "Top"
#define KeyLeft         "Left"
#define KeyWidth        "Width"
#define KeyHeight       "Height"
#define SectionData     "Data"
#define KeyPort         "Port"
#define KeyServer       "Server"
#define KeyData         "Data"
#define KeyRepeat       "RepeatCount"
#define KeyContinuous   "ContinuousSend"
#define KeyLength       "DataLength"
#define KeyUseDataSent  "UseDataSent"
#define KeyDisplay      "Display"
#define KeyLinger       "Linger"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TSenderForm *SenderForm;
//---------------------------------------------------------------------------
__fastcall TSenderForm::TSenderForm(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile      = new TIniFile(FIniFileName);
        Width        = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Top          = IniFile->ReadInteger(SectionWindow, KeyTop,
                                            (Screen->Height - Height) / 2);
        Left         = IniFile->ReadInteger(SectionWindow, KeyLeft,
                                            (Screen->Width  - Width)  / 2);
        PortEdit->Text        = IniFile->ReadString(SectionData, KeyPort, "telnet");
        ServerEdit->Text      = IniFile->ReadString(SectionData, KeyServer, "localhost");
        DataEdit->Text        = IniFile->ReadString(SectionData, KeyData,       "The quick brown fox jumps over the lazy dog");
        RepeatEdit->Text      = IniFile->ReadString(SectionData, KeyRepeat,     "");
        LengthEdit->Text      = IniFile->ReadString(SectionData, KeyLength,     "60");
        ContCheckBox->Checked        = IniFile->ReadInteger(SectionData, KeyContinuous,  0);
        LingerCheckBox->Checked      = IniFile->ReadInteger(SectionData, KeyLinger,      1);
        DisplayDataCheckBox->Checked = IniFile->ReadInteger(SectionData, KeyDisplay,     0);
        UseDataSentCheckBox->Checked = IniFile->ReadInteger(SectionData, KeyUseDataSent, 1);
        delete IniFile;
        RepeatEdit->Enabled = !ContCheckBox->Checked;
        CountLabel->Caption  = "";
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::FormClose(TObject *Sender,
      TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile->WriteString(SectionData, KeyPort,   PortEdit->Text);
    IniFile->WriteString(SectionData, KeyServer, ServerEdit->Text);
    IniFile->WriteString(SectionData, KeyData,   DataEdit->Text);
    IniFile->WriteString(SectionData, KeyRepeat, RepeatEdit->Text);
    IniFile->WriteString(SectionData, KeyLength, LengthEdit->Text);
    IniFile->WriteInteger(SectionData, KeyContinuous,  ContCheckBox->Checked);
    IniFile->WriteInteger(SectionData, KeyLinger,      LingerCheckBox->Checked);
    IniFile->WriteInteger(SectionData, KeyUseDataSent, UseDataSentCheckBox->Checked);
    IniFile->WriteInteger(SectionData, KeyDisplay,     DisplayDataCheckBox->Checked);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::FormDestroy(TObject *Sender)
{
    if (FDataBuf) {
        free(FDataBuf);
        FDataBuf = NULL;
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::ContCheckBoxClick(TObject *Sender)
{
    RepeatEdit->Enabled = !ContCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::Display(String Msg)
{
    if (DisplayMemo->Lines->Count > 200)
        DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
int __fastcall MyMessageBox(const System::String &Msg,
    const System::String &Title, int Flags)
{
    return Application->MessageBox(Msg.c_str(), Title.c_str(), Flags);
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::ActionButtonClick(TObject *Sender)
{
    int        Len;
    int        N;
    int        T;
    AnsiString Buf;

    // The ActionButton is used to start or stop data transmission
    if (FSending) {
        // We are already sending, so user wants to stop
        // Display updated counter
        CountLabel->Caption = IntToStr(FCount);

        // Check if some data remains in TWSocket"s internal buffer
        if (!WSocket1->AllSent &&
           (MyMessageBox("Data is still being sent\n"
                                    "Close anyway ?",
                                    "Warning", MB_YESNO) != IDYES))
            return;

        Display("Stop requested");
        if (!WSocket1->AllSent)
            Display("Not all data has been sent");

        FAutoStart = 0;
        // Close the socket-> This will delete any data not already sent to
        // winsock.
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, (LPARAM)WSocket1);
        return;
    }

    // The user wants to start data transmission
    CountLabel->Caption   = "";
    PauseButton->Caption  = "&Pause";
    PauseButton->Visible  = TRUE;
    ActionButton->Caption = "&Stop";
    FPaused               = FALSE;
    FSending              = TRUE;
    FFinished             = FALSE;
    FCount                = 0;

    // Setup final count
    if (ContCheckBox->Checked)
        FFinalCount = 0;
    else
        FFinalCount = StrToInt(Trim(RepeatEdit->Text));

    // Check which method use to send more data
    // Using OnDataSent event will prevent internal TWSocket buffer to be
    // enlarged without limit.
    FUseDataSent = UseDataSentCheckBox->Checked;
    if (FUseDataSent)
        WSocket1->OnDataSent = WSocket1DataSent;
    else
        WSocket1->OnDataSent = WSocket1NoDataSent;

    // Prepare data to be sent
    Buf = "0000 " + DataEdit->Text;
    Len = StrToInt(Trim(LengthEdit->Text));
    if (Len <= 0)
        Len = Buf.Length();
    if (FDataBuf)
        free(FDataBuf);
    FDataBufSize = Len + 3;
    FDataBuf     = (char *)malloc(FDataBufSize);
    if (Len > 0) {
        if (Len < Buf.Length())
            Move(&Buf[1], &FDataBuf[0], Len);
        else {
            T = 0;
            while (T < Len) {
                N = Buf.Length();
                if ((T + N) > Len)
                    N = Len - T;
                if (N > 0)
                    Move(&Buf[1], &FDataBuf[T], N);
                T = T + N;
            }
        }
    }
    FDataBuf[Len]     = '\r';
    FDataBuf[Len + 1] = '\n';
    FDataBuf[Len + 2] = 0;

    // Launch DNS lookup-> When done, we'll try to connect.
    WSocket1->DnsLookup(Trim(ServerEdit->Text));
}
//---------------------------------------------------------------------------
// We comes here when DNS lookup is finished, even in case of failure.
void __fastcall TSenderForm::WSocket1DnsLookupDone(TObject *Sender,
      WORD Error)
{
    // If any error occured, we just display info and prepare to restart.
    if (Error) {
        MessageBeep(MB_OK);
        Display("DNS failure-> Error #" + IntToStr(Error));
        ActionButton->Caption = "&Start";
        PauseButton->Visible  = FALSE;
        return;
    }

    // Now we know the IP address-> Try to connect.
    WSocket1->Addr  = WSocket1->DnsResult;
    WSocket1->Port  = Trim(PortEdit->Text);
    WSocket1->Proto = "tcp";
    try {
        WSocket1->Connect();
    } catch (const Exception &E) {
        MessageBeep(MB_OK);
        Display("Connect failed: " + E.Message);
        ActionButton->Caption = "&Start";
        PauseButton->Visible  = FALSE;
        FAutoStart            = 0;
        return;
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WSocket1SessionConnected(TObject *Sender,
      WORD Error)
{
    if (Error) {
        MessageBeep(MB_OK);
        Display("Can't connect. Error #" + IntToStr(Error));
        ActionButton->Caption = "&Start";
        FAutoStart            = 0;
        return;
    }
    Display("Connected");
    if (LingerCheckBox->Checked)
        WSocket1->LingerOnOff   = wsLingerOn;
    else
        WSocket1->LingerOnOff   = wsLingerOff;
    WSocket1->LingerTimeout = 300;
    WSocket1->SetLingerOption();
    DoSend();
    if (FUseDataSent)
        return;

    // User requested to not use OnDataSent event-> We will simply loop.
    // until all data has been sent-> This will fill TWSocket internal buffer
    // very quickly while data is being sent in the background at network
    // speed.
    while ((FFinalCount <= 0) || (FFinalCount > FCount)) {
        // We must break the loop if user temrinated the application,
        // or if connection is broke, or if user stopped.
        if (Application->Terminated ||
           (WSocket1->State != wsConnected) ||
           !FSending)
            return;
        // Otherwise, we can send data
        DoSend();
    }
    CountLabel->Caption  = IntToStr(FCount);
    PauseButton->Visible = FALSE;
    Display("All data is in TWSocket buffer and being sent in the background");
    FFinished = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::DoSend(void)
{
    AnsiString Buf;

    do {
        // Calling ProcessMessages let a chance to button and other events
        // to be handled.
        Application->ProcessMessages();
        // We must stop if the user clicked the close button.
        if (Application->Terminated) {
            Display("Application terminated");
            return;
        }
        // We must stop if the user requested to stop send
        if (!FSending)
            return;
        // We must stop if connection is broken
        if (WSocket1->State != wsConnected)
            return;
        // We don't wants to use 100% CPU just looping. Sleep a little bit
        if (FPaused)
            Sleep(250);
    } while (FPaused);

    // We need to check if we are still connected before sending
    if (WSocket1->State != wsConnected)
        return;

    if ((FFinalCount <= 0) || (FFinalCount > FCount)) {
        // Count the message sent
        FCount++;
        // Put the counter into the message, truvated to 4 digits
        Buf = IntToStr(FCount % 10000) + "    ";
        Move(&Buf[1], &FDataBuf[0], 4);

        // If required, display in memo box (slow down !)
        if (FDisplayData)
            Display("Sending " + IntToStr(FCount));
        // Display the counter every 100 sends
        if ((FCount % 100) == 0)
            CountLabel->Caption = IntToStr(FCount);

        // Try to send data-> Send may fail !
        try {
            WSocket1->Send(FDataBuf, FDataBufSize - 1);
        } catch (const Exception &E) {
            Display("Exception during TWSocket->Send(): " + E.Message);
            FAutoStart = 0;
            PostMessage(Handle, WM_CLOSE_REQUEST, 0, (LPARAM)WSocket1);
        }
    }
    else {
        Display("Required data has been sent. Closing.");
        // We may have not read data send by server-> But anyway, close the
        // session.
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, (LPARAM)WSocket1);
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WSocket1DataSent(TObject *Sender, WORD Error)
{
    DoSend();
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WSocket1NoDataSent(TObject *Sender, WORD Error)
{
    if (FFinished) {
        if (!WSocket1->AllSent)
            Display("Not all sent");
        Display("Required data has been sent. Closing.");
        PostMessage(Handle, WM_CLOSE_REQUEST, 0, (LPARAM)WSocket1);
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WSocket1DataAvailable(TObject *Sender,
      WORD Error)
{
    char     *Buf;
    TWSocket *Cli;
    int      Len;
    int      Cnt;

    Cli = (TWSocket *)Sender;
    Cnt = Cli->RcvdCount;
    if (Cnt <= 0)
        return;

    Buf = (char *)malloc(Cnt + 1);
    try {
        Len = Cli->Receive(Buf, Cnt);
        if (Len > 0) {
            Buf[Cnt] = 0;
            Display("Received: " + (AnsiString)Buf);
        }
    } catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    free(Buf);
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WSocket1SessionClosed(TObject *Sender,
      WORD Error)
{
    if (Error == 0)
        Display("Socket closed, no error");
    else {
        Display("Socket closed, Error #" + IntToStr(Error));
        FAutoStart = 0;
    }
    FSending              = FALSE;
    ActionButton->Caption = "&Start";
    PauseButton->Visible  = FALSE;
    FPaused               = FALSE;
    if (FAutoStart > 0) {
        FAutoStart++;
        AutoStartButton->Caption = IntToStr(FAutoStart);
        PostMessage(Handle, WM_AUTO_START, 0, 0);
    }
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::DisplayDataCheckBoxClick(TObject *Sender)
{
    FDisplayData = DisplayDataCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::UseDataSentCheckBoxClick(TObject *Sender)
{
    FUseDataSent = UseDataSentCheckBox->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::PauseButtonClick(TObject *Sender)
{
    CountLabel->Caption = IntToStr(FCount);
    FPaused = !FPaused;
    if (FPaused)
        PauseButton->Caption = "&Resume";
    else
        PauseButton->Caption = "&Pause";
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::AutoStartButtonClick(TObject *Sender)
{
    if (FAutoStart != 0) {
        FAutoStart = 0;
        return;
    }

    FAutoStart = 1;
    AutoStartButton->Caption = IntToStr(FAutoStart);
    PostMessage(Handle, WM_AUTO_START, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WMCloseRequest(TMessage Msg)
{
    TWSocket *WSocket;

    WSocket = (TWSocket *)(Msg.LParam);
    WSocket->Close();
}
//---------------------------------------------------------------------------
void __fastcall TSenderForm::WMAutoStart(TMessage Msg)
{
    ActionButtonClick(this);
}
//---------------------------------------------------------------------------
