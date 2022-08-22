program Cipher;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  SetPwdFrm in 'SetPwdFrm.pas' {SetPwdForm},
  InputFrm in 'InputFrm.pas' {InputForm},
  PwdDataFrm in 'PwdDataFrm.pas' {PwdDataForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  StrRes in 'StrRes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '–°–°√‹¬Îœ‰';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
