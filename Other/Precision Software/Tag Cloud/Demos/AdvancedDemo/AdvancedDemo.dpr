program AdvancedDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TagCloud for VCL - Advanced Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
