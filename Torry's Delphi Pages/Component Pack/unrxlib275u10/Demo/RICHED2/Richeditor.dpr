program Richeditor;

uses 
  Forms,
  RxExcptDlg,
  REMain in 'REMain.pas' {MainForm},
  ParaFmt in 'ParaFmt.pas' {ParaFormatDlg};

{$R *.RES}

begin
  Application.Initialize;
  RxErrorIntercept;
  Application.Title := 'RX RichEdit Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
