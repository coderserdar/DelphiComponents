program ScrShareClient;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  ConDlg in 'ConDlg.pas' {frmConDlg},
  Options in 'Options.pas' {frmOptions},
  Stat in 'Stat.pas' {frmStat},
  About in 'About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmConDlg, frmConDlg);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmStat, frmStat);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
