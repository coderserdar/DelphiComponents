program wmalive;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Bass in '..\Bass.pas',
  BASSWMA in '..\basswma.pas';

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.