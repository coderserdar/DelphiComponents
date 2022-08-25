program scuttest6;

uses
  Forms,
  scutmain in 'scutmain6.pas' {Form1},

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
