program SXSkinTest;

uses
  Forms,
  SkinTestUnit in 'SkinTestUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SXSkinComponents Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
