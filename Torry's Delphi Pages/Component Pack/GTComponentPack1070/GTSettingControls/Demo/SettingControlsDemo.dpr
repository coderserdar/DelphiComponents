program SettingControlsDemo;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GT Delphi Components - Setting Conrtols Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
