/*---------------------------------------------------------------------------

Legal issues: Copyright (C) 1997, 1998 by François PIETTE 
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
Updates
Apr 12, 1998 Adapted for BCB3

---------------------------------------------------------------------------*/
#include <vcl.h>
#include <Inifiles.hpp>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#pragma hdrstop

#include "OverbyteIcsFormPos.h"
//---------------------------------------------------------------------------
// Build a string like 'Left, Top, Height, Width, WindowState'
char * __fastcall WindowPosToStr(char *Result, TForm *Form)
{
    sprintf(Result, "%d, %d, %d, %d, %d",
                    Form->Left,   Form->Top,
                    Form->Height, Form->Width,
                    Form->WindowState);
    return(Result);
}
//---------------------------------------------------------------------------
char *stpblk(char *p)
{
    while (isspace(*p))
        p++;
    return(p);
}
//---------------------------------------------------------------------------
char *SkipNumber(char *p)
{
    p = stpblk(p);
    if ((*p == '+') || (*p == '-'))
        p++;
    while (isdigit(*p))
        p++;
    return(stpblk(p));
}
//---------------------------------------------------------------------------
// Gets a string like '100, 200, 300, 500, 0', parse the values and
// affect them to Form.LEFT, Form.Top, Form.Heigt, Form.Width,
// Form.WindowState properties
void __fastcall StrToWindowPos(char *sBuffer, TForm *Form)
{
    Form->Left = atoi(sBuffer);
    sBuffer    = SkipNumber(sBuffer);
    if (*sBuffer == ',')
        sBuffer++;

    Form->Top = atoi(sBuffer);
    sBuffer    = SkipNumber(sBuffer);
    if (*sBuffer == ',')
        sBuffer++;

    Form->Height = atoi(sBuffer);
    sBuffer    = SkipNumber(sBuffer);
    if (*sBuffer == ',')
        sBuffer++;

    Form->Width = atoi(sBuffer);
    sBuffer    = SkipNumber(sBuffer);
    if (*sBuffer == ',')
        sBuffer++;

    Form->WindowState = (TWindowState)atoi(sBuffer);
}
//---------------------------------------------------------------------------
void __fastcall LoadFormPos(TForm *Form,
                            const AnsiString IniFileName,
                            const AnsiString SectionName,
                            const AnsiString KeyName)
{
    TIniFile   *IniFile;
    AnsiString sWindowPositions;

    if (IniFileName.Length() == 0)
        return;

    // Create inifile object => Open ini file
    IniFile = new TIniFile(IniFileName);

    // Formatage par défaut de la ligne de la section window
    sWindowPositions.SetLength(256);
    WindowPosToStr(sWindowPositions.c_str(), Form);

    // Get widow's position and size from ini file
    sWindowPositions = IniFile->ReadString(SectionName,
                                           KeyName,
                                           sWindowPositions);
    StrToWindowPos(sWindowPositions.c_str(), Form);

    // Destroy inifile object => close ini file
    IniFile->Free();
}
//---------------------------------------------------------------------------
void __fastcall SaveFormPos(TForm *Form,
                            const AnsiString IniFileName,
                            const AnsiString SectionName,
                            const AnsiString KeyName)
{
    TIniFile     *IniFile;
    TWindowState WindowState;
    char         Buffer[256];

    if (IniFileName.Length() == 0)
        return;

    // Create inifile object => Open ini file
    IniFile = new TIniFile(IniFileName);

    WindowState = Form->WindowState;

    // If window minimized or maximized, restore to normal state
    if (Form->WindowState != wsNormal)
        Form->WindowState = wsNormal;

    // Save the window's postion and size to the ini file
    IniFile->WriteString(SectionName,
                         KeyName,
                         WindowPosToStr(Buffer, Form));

    // Destroy inifile object => close ini file
    IniFile->Free();

    Form->WindowState = WindowState;
}
//---------------------------------------------------------------------------

