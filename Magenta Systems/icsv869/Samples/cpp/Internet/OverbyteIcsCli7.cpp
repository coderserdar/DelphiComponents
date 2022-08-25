/*---------------------------------------------------------------------------

Author:       François PIETTE
Object:       Simple client application demonstrating TWSocket object in action.
Creation:     December 27, 1998 (from delphi version created nov 28, 1998)
Version:      1.01
Email         francois.piette@overbyte.be      http://www.overbyte.be
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

Updates:
Aug 15, 1999 V1.01 Adapted for BCB4 (Moved FIniFileName initialization from
             FormCreate to form constructor).

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <IniFiles.hpp>
#pragma hdrstop

#include "OverbyteIcsCli7.h"
#define EndOfLine     "\r\n"
#define SectionWindow "Window"
#define KeyTop        "Top"
#define KeyLeft       "Left"
#define KeyWidth      "Width"
#define KeyHeight     "Height"
#define SectionData   "Data"
#define KeyHostName   "HostName"
#define KeyPort       "Port"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "OverbyteIcsWSocket"
#pragma link "OverbyteIcsWndControl"
#pragma resource "*.dfm"
TCli7Form *Cli7Form;
//---------------------------------------------------------------------------
__fastcall TCli7Form::TCli7Form(TComponent* Owner)
    : TForm(Owner)
{
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::FormShow(TObject *Sender)
{
    TIniFile *IniFile;

    if (!FInitialized) {
        FInitialized = TRUE;
        IniFile = new TIniFile(FIniFileName);
        HostNameEdit->Text  = IniFile->ReadString(SectionData, KeyHostName,
                                                  "localhost");
        PortEdit->Text      = IniFile->ReadString(SectionData, KeyPort,
                                                  "telnet");

        Width  = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        Top    = IniFile->ReadInteger(SectionWindow, KeyTop,    (Screen->Height - Height) / 2);
        Left   = IniFile->ReadInteger(SectionWindow, KeyLeft,   (Screen->Width - Width) / 2);

        delete IniFile;
        DisplayMemo->Clear();
    }
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteString(SectionData, KeyHostName,  HostNameEdit->Text);
    IniFile->WriteString(SectionData, KeyPort,      PortEdit->Text);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::Display(AnsiString Msg)
{
    if (DisplayMemo->Lines->Count > 200) // Prevent TMemo overflow
        DisplayMemo->Clear();
    DisplayMemo->Lines->Add(Msg);
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::LineOnButtonClick(TObject *Sender)
{
    WSocket1->LineMode = TRUE;
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::LineOffButtonClick(TObject *Sender)
{
    WSocket1->LineMode = FALSE;
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::ConnectButtonClick(TObject *Sender)
{
    WSocket1->Proto    = "tcp";
    WSocket1->Port     = PortEdit->Text;
    WSocket1->Addr     = HostNameEdit->Text;
    WSocket1->LineMode = TRUE;
    WSocket1->LineEnd  = EndOfLine;
    WSocket1->Connect();
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::DisconnectButtonClick(TObject *Sender)
{
    WSocket1->Close();
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::WSocket1SessionConnected(TObject *Sender,
      WORD Error)
{
    if (Error)
        Display("Connection failed, error #" + IntToStr(Error));
    else
        Display("Session Connected.");
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::WSocket1SessionClosed(TObject *Sender,
      WORD Error)
{
    Display("Session Closed.");
}
//---------------------------------------------------------------------------
AnsiString __fastcall RemoveEndOfLine(const AnsiString Line)
{
    if ((Line.Length() >= ((int)sizeof(EndOfLine) - 1)) &&
        (strncmp(Line.c_str() + (Line.Length() - sizeof(EndOfLine) + 1),
                 EndOfLine,
                 sizeof(EndOfLine) - 1) == 0))
        return(Line.SubString(1, Line.Length() - sizeof(EndOfLine) + 1));
    else
        return(Line);
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::WSocket1DataAvailable(TObject *Sender,
      WORD Error)
{
    char Buf[128];
    int  Len;

    Len = ((TCustomLineWSocket *)Sender)->Receive(Buf, sizeof(Buf) - 1);
    if (Len <= 0)
        return;
    Buf[Len] = 0;
    if (!WSocket1->LineMode) {
        // Normal mode, data is just a buffer with all caracters
        Display("DataAvailable (" + IntToStr(Len) +" bytes): \"" +
                Buf + "\"");
    } else {
        // Line mode, buffer contains exactly one line, terminated by the
        // LineEnd string, unless our buffer is too small in which case
        // the line is truncated. We'll get the end of line on the next
        // call to Receive.
        Display("Line: \"" + RemoveEndOfLine(Buf) + "\"");
    }
}
//---------------------------------------------------------------------------
void __fastcall TCli7Form::SendButtonClick(TObject *Sender)
{
    WSocket1->SendStr(DataEdit->Text + "\r\n");
}
//---------------------------------------------------------------------------
