program sq_demo;

uses
  Forms,
  sqd_main in 'sqd_main.pas' {MainForm},
  About in 'about.pas' {AboutBox},
  DateDlg in 'datedlg.pas' {DateDialog};

{$R *.RES}
            
begin
  Application.Initialize;
  Application.Title := 'Simple Query Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDateDialog, DateDialog);
  Application.Run;
end.

