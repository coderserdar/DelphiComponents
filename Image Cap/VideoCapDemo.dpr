program VideoCapDemo;

uses
  Forms,
  mainf in 'mainf.pas' {Main},
  DlgTreiber in 'DlgTreiber.pas' {DlgEinstell},
  aboutx in 'aboutx.pas' {AboutDlg},
  dlgaufz in 'dlgaufz.pas' {DlgVPara};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TDlgEinstell, DlgEinstell);
  Application.CreateForm(TAboutDlg, AboutDlg);
  Application.CreateForm(TDlgVPara, DlgVPara);
  Application.Run;
end.
