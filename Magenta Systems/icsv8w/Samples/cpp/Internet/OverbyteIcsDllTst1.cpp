/*---------------------------------------------------------------------------

Author:       François PIETTE
Creation:     April 08, 2000
Description:  This is a demonstration program for IcsDll1.dll. It will
              dynamically load the DLL, get IcsDllDemo entry point and call
              it. Then display result from DLL.
Version:      1.00
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
#include <vcl.h>
#include <IniFiles.hpp>
#pragma hdrstop
#include "OverbyteIcsDllTst1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#define SectionWindow      "Window"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
TDllTestForm *DllTestForm;
typedef __declspec(dllimport) int __stdcall (*TIcsDllDemo)(char *HostName,
                                                           char *Port,
                                                           char *Buffer,
                                                           int  *BufSize);
HANDLE      DllHandle;
TIcsDllDemo IcsDllDemo;

//---------------------------------------------------------------------------
void __fastcall MyMessageBox(const System::String &Msg,
    const System::String &Title, int Flags)
{
    Application->MessageBox(Msg.c_str(), Title.c_str(), Flags);
}


//---------------------------------------------------------------------------
__fastcall TDllTestForm::TDllTestForm(TComponent* Owner)
        : TForm(Owner)
{
    // Build Ini file name
    FIniFileName = LowerCase(ExtractFileName(Application->ExeName));
    FIniFileName = FIniFileName.SubString(1, FIniFileName.Length() - 3) + "ini";
}
//---------------------------------------------------------------------------
void __fastcall TDllTestForm::FormShow(TObject *Sender)
{
    TIniFile    *IniFile;

    if (!FInitialized) {
        FInitialized     = TRUE;
        IniFile          = new TIniFile(FIniFileName);
        Top              = IniFile->ReadInteger(SectionWindow, KeyTop,    Top);
        Left             = IniFile->ReadInteger(SectionWindow, KeyLeft,   Left);
        Width            = IniFile->ReadInteger(SectionWindow, KeyWidth,  Width);
        Height           = IniFile->ReadInteger(SectionWindow, KeyHeight, Height);
        delete IniFile;
        DisplayMemo->Clear();

		DllHandle = LoadLibraryA("OverbyteIcsDLL1.dll");
        if (DllHandle == 0) {
            MyMessageBox("OverbyteIcsDLL1.dll not found", "Error", MB_OK);
            Application->Terminate();
            return;
        }

        IcsDllDemo = (TIcsDllDemo)GetProcAddress(DllHandle, "IcsDllDemo");
        if (IcsDllDemo == NULL) {
            MyMessageBox("IcsDllDemo not found (OverbyteIcsDLL1.dll)", "Error", MB_OK);
            Application->Terminate();
            return;
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TDllTestForm::FormDestroy(TObject *Sender)
{
    if (DllHandle) {
        FreeLibrary(DllHandle);
        DllHandle = 0;
    }
}
//---------------------------------------------------------------------------
// Display a message in our display memo. Delete lines to be sure to not
// overflow the memo which may have a limited capacity.
void __fastcall TDllTestForm::Display(const System::String &Msg)
{
    DisplayMemo->Lines->BeginUpdate();
    try {
        if (DisplayMemo->Lines->Count > 200) {
            while (DisplayMemo->Lines->Count > 200)
                DisplayMemo->Lines->Delete(0);
        }
        DisplayMemo->Lines->Add(Msg);
    } __finally {
        DisplayMemo->Lines->EndUpdate();
        SendMessage(DisplayMemo->Handle, EM_SCROLLCARET, 0, 0);
    }
}
//---------------------------------------------------------------------------
void __fastcall TDllTestForm::FormClose(TObject *Sender, TCloseAction &Action)
{
    TIniFile *IniFile;

    IniFile = new TIniFile(FIniFileName);
    IniFile->WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile->WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile->WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile->WriteInteger(SectionWindow, KeyHeight, Height);
    delete IniFile;
    if (DllHandle) {
        FreeLibrary(DllHandle);
        DllHandle = 0;
    }
}
//---------------------------------------------------------------------------
void __fastcall TDllTestForm::CallDllButtonClick(TObject *Sender)
{
	AnsiString Buffer;
	int        BufSize;
    int        Status;

    Display("Calling DLL...");
    BufSize = 100;
    Buffer.SetLength(BufSize);
	Status = IcsDllDemo(AnsiString(HostnameEdit->Text).c_str(),
                        AnsiString(PortEdit->Text).c_str(),
						&Buffer[1], &BufSize);
    Buffer.SetLength(BufSize);
    if (Status)
        Display("Error #" + IntToStr(Status));
    Display(Buffer);
    Display("Done with DLL");
}
//---------------------------------------------------------------------------

