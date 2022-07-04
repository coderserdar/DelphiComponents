program DocSiteDemo;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  DsBandRow in '..\Source\DsBandRow.pas',
  DsBand in '..\Source\DsBand.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
