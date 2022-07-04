program Sample02;

uses
  Forms,
  Sample02Main in 'Sample02Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
