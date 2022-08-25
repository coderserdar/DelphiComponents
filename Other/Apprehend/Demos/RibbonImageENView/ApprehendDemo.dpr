program ApprehendDemo;

uses
  Forms,
  uMain in 'uMain.pas' {FormMain},
  uConvBW in 'uConvBW.pas' {FormConvBW},
  uResize in 'uResize.pas' {ResizeForm},
  uStatus in 'uStatus.pas' {FormStatus},
  uProperties in 'uProperties.pas' {FormProperties},
  uSelection in 'uSelection.pas' {FormSelection},
  uFullscrn in 'uFullscrn.pas' {FormFullScreen},
  uCompressQual in 'uCompressQual.pas' {FormQuality},
  uScreenDelay in 'uScreenDelay.pas' {FormDelay},
  uWebSite in 'uWebSite.pas' {FormWebsite},
  uConfigureMagnifier in 'uConfigureMagnifier.pas' {FormConfigureMagnifier},
  uNavigator in 'uNavigator.pas' {FormNavigator},
  uRotate in 'uRotate.pas' {FormRotate},
  uPrint in 'uPrint.pas' {FormPrint},
  uImport in 'uImport.pas' {FormImport},
  uAbout in 'uAbout.pas' {FormAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.Title := 'Apprehend Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

