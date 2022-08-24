program BasicDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TagCloud for VCL - Basic Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
