program cdtest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  BASSCD in '..\basscd.pas',
  Bass in '..\Bass.pas';

begin
  Application.Initialize;
  Application.Title := 'BASSCD test';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
