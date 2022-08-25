program tapidemo;

uses
  Forms,
  tapimain in 'tapimain.pas' {Form1},
  magtapiapi in 'magtapiapi.pas',
  magtapistr in 'magtapistr.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
