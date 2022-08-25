program ApprehendDemo;

uses
  Forms,
  Windows,
  uMain in 'uMain.pas' {FormMain},
  uFullscrn in 'uFullscrn.pas' {FullScreenForm},
  uScreenDelay in 'uScreenDelay.pas' {DelayForm},
  uAbout in 'uAbout.pas' {frmAbout},
  usplash in 'usplash.pas' {FormSplash};

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.Title := 'Apprehend TImage Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

