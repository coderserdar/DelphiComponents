program Sample05;

uses
  Forms,
  Sample05Main in 'Sample05Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
