program AddrBook;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  CardFrm in 'CardFrm.pas' {CardForm},
  InputFrm in 'InputFrm.pas' {InputForm},
  ChgPwdFrm in 'ChgPwdFrm.pas' {ChgPwdForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  MyEncAlgo in 'MyEncAlgo.pas',
  StrRes in 'StrRes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Address book';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
