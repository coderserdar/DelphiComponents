program DecoProgressDemo;

uses
  Forms,
  Main in 'Main.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DecoProgressGrid Demo';
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
