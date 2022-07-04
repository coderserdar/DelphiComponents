program Sample01;

uses
  Forms,
  Sample01Main in 'Sample01Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
