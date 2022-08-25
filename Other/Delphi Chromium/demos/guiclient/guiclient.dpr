program guiclient;

uses
  Forms,
  main in 'main.pas' {MainForm},
  ceffilescheme in '..\filescheme\ceffilescheme.pas',
  Authentication in 'Authentication.pas' {PasswordDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPasswordDlg, PasswordDlg);
  Application.Run;
end.
