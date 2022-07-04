program Sample06;

uses
  Forms,
  Sample06Main in 'Sample06Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
