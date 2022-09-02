(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit uFFsCfg;

interface
uses
  IniFiles,
  Forms;

procedure FFSConfigSaveFormPrefs(const Section : string; Form : TForm);
procedure FFSConfigGetFormPrefs(const Section : string; Form : TForm);
procedure FFSConfigGetFormPos(const Section : string; Form : TForm);   {!!.06}

var
  FFSIni : TIniFile;

implementation
uses
  SysUtils;

procedure FFSConfigSaveFormPrefs(const Section : string; Form : TForm);
begin
  FFSIni.WriteInteger(Section, 'Top', Form.Top);
  FFSIni.WriteInteger(Section, 'Left', Form.Left);
  FFSIni.WriteInteger(Section, 'Height', Form.Height);
  FFSIni.WriteInteger(Section, 'Width', Form.Width);
end;
{--------}
procedure FFSConfigGetFormPrefs(const Section : string; Form : TForm);
{Rewritten !!.06}
var
  TmpInt : Integer;
begin
  TmpInt := FFSIni.ReadInteger(Section, 'Top', -1);
  if TmpInt = -1 then begin
    { If no settings available then make sure form height & width do
      not exceed the values specified at design-time. For some reason,
      on some PCs the initial width & height are given a different
      value than established in the IDE. }
    if Form.Height > 320 then
      Form.Height := 320;
    if Form.Width > 600 then
      Form.Width := 600;
  end
  else
  begin
    Form.Top := TmpInt;
    Form.Left := FFSIni.ReadInteger(Section, 'Left', 10);
    Form.Height := FFSIni.ReadInteger(Section, 'Height', 318);
    Form.Width := FFSIni.ReadInteger(Section, 'Width', 600);
  end;  { if }
end;
{Begin !!.06}
{--------}
procedure FFSConfigGetFormPos(const Section : string; Form : TForm);
var
  TmpInt : Integer;
begin
  TmpInt := FFSIni.ReadInteger(Section, 'Top', -1);
  { Assumption: If no positioning information found then position the form to
    screen center. Otherwise, set position to poDesigned so that it will show
    up at the correct coordinates. }
  if TmpInt = -1 then
    Form.Position := poScreenCenter
  else
    Form.Position := poDesigned;
end;
{End !!.06}
{--------}
procedure InitUnit;
begin
  FFSIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FFServer.ini');
end;
{--------}
procedure TermUnit;
begin
  FFSIni.Free;
  FFSIni := nil;
end;
{--------}

initialization
  InitUnit;

finalization
  TermUnit;

end.
