program ApprehendDemo;

uses
  Forms,
  Windows,
  uMain in 'uMain.pas' {FormMain},
  uFullscrn in 'uFullscrn.pas' {FullScreen},
  uScreenDelay in 'uScreenDelay.pas' {DelayDlg},
  uWebSite in 'uWebSite.pas' {WebsiteForm},
  uAbout in 'uAbout.pas' {frmAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Apprehend TImage Demo';
  Application.MainFormOnTaskBar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

