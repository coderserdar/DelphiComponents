program Sample04;

uses
  Forms,
  Sample04Main in 'Sample04Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
