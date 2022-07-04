program ExportGrid;

uses
  Forms,
  unMain in 'unMain.pas' {frmMain};

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
