program PageControlTImageDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uScreenDelay in 'uScreenDelay.pas' {FormDelay};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Simple Apprehend Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
