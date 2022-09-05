program SXMedia;

uses
  Forms,
  main in 'main.pas' {FrmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SXMedia - Example Project';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
