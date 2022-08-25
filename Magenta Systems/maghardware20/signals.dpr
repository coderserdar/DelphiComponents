program signals;

uses
  Forms,
  signmain in 'signmain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Test Serial Ports and Hardware Events';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

