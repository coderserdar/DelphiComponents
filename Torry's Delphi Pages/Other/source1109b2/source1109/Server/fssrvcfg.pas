unit fssrvcfg;

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
  FFSIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Server.ini');
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
