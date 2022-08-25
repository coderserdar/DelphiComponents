program ApprehendDemo;

uses
  Forms,
  Windows,
  uMain in 'uMain.pas' {FormMain},
  uFullscrn in 'uFullscrn.pas' {FullScreenForm},
  usplash in 'usplash.pas' {SplashForm},
  uAbout in 'uAbout.pas' {frmAbout},
  uNewImage in 'uNewImage.pas' {FormNewImage},
  uResize in 'uResize.pas' {FormResizeResample},
  uRotate in 'uRotate.pas' {FormRotate},
  uFlip in 'uFlip.pas' {FormFlip};

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.Title := 'Apprehend TImage Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

