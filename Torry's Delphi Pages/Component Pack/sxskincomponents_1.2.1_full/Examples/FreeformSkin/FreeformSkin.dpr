program FreeformSkin;

uses
  Forms,
  FreeformUnit in 'FreeformUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SXSkinComponents Demo - FreeformSkin';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
