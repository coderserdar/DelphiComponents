{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FormPos;

interface

uses
    Forms, IniFiles, SysUtils, Dialogs, WinTypes, WinProcs;

Procedure LoadFormPos(Form : TForm;
                      const IniFilename : string;
                      const SectionName : String;
                      const KeyName: string);
Procedure SaveFormPos(Form : TForm;
                      const IniFilename : string;
                      const SectionName : String;
                      const KeyName: string);

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Génère un chaîne du genre 'Left, Top, Height, Width, WindowState'       *}
{* en vue de la placer dans un section d'un fichier INI                    *}
function WindowPosToStr(Form : TForm; iWindowState : integer) : string;
begin
    Result := IntToStr(Form.Left)    + ', ' +
              IntToStr(Form.Top)     + ', ' +
              IntToStr(Form.Height)  + ', ' +
              IntToStr(Form.Width)   + ', ' +
              IntToStr(iWindowState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Recoit un string sous forme  '100, 200, 300, 500, 0' et affecte         *}
{* respectivement ces valeurs a  Form.LEFT, Form.Top, Form.Heigt,          *}
{* Form.Width, Form.WindowState                                            *}
procedure StrToWindowPos(sBuffer : string; Form : TForm);
var
    ilen, i, k : integer;
    sDst       : string;
begin
    ilen := length(sBuffer);
    k    := 0;
    sDst := '';

    {i = indice ds sBuffer; j = Indice ds sDst; k = nbre d'occurences trouvéés }
    for i := 1  to ilen + 1 do begin
        if (i > ilen) or (sBuffer[i] = ',') then begin
            k := k + 1;
            case k of
            1: Form.Left        := StrToInt(sDst);
            2: Form.Top         := StrToInt(sDst);
            3: Form.Height      := StrToInt(sDst);
            4: Form.Width       := StrToInt(sDst);
            5: Form.WindowState := TWindowState(StrToInt(sDst));
            else
                break;
            end;
            sDst := '';
        end
        else
            sDst := sDst + sBuffer[i];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadFormPos(Form              : TForm;
                      const IniFilename : string;
                      const SectionName : String;
                      const KeyName     : string);
var
    IniFile: TIniFile;
    sWindowPositions : string;
begin
    if Length(IniFileName) = 0 then
        Exit;

    { Create inifile object => Open ini file }
    IniFile := TIniFile.Create(IniFilename);

    {Formatage par défaut de la ligne de la section window}
    sWindowPositions := WindowPosToStr(Form, ord(Form.WindowState));

    { Get widow's position and size from ini file }
    sWindowPositions := IniFile.ReadString(SectionName,
                                           KeyName,
                                           sWindowPositions);
    StrToWindowPos(sWindowPositions, Form);


    { Destroy inifile object => close ini file }
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SaveFormPos(Form              : TForm;
                      const IniFilename : string;
                      const SectionName : String;
                      const KeyName     : string);
var
    IniFile      : TIniFile;
    iWindowState : integer;
begin
    if Length(IniFileName) = 0 then
        Exit;

    { Create inifile object => Open ini file }
    IniFile := TIniFile.Create(IniFilename);

    iWindowState := ord(Form.WindowState);

    { If window minimized or maximized, restore to normal state }
    if Form.WindowState <> wsNormal then
        Form.WindowState := wsNormal;

    { Save the window's postion and size to the ini file }
    IniFile.WriteString(SectionName,
                        KeyName,
                        WindowPosToStr(Form, iWindowState));

    { Destroy inifile object => close ini file }
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

