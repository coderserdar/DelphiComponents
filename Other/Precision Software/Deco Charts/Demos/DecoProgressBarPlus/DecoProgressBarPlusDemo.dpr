program DecoProgressBarPlusDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DecoProgressBarPlus Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
