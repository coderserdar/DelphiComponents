program Sample03;

uses
  Forms,
  Sample03Main in 'Sample03Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
