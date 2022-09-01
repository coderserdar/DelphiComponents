program bvCompDemo1;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  bvDBGridDemoUnit in 'bvDBGridDemoUnit.pas' {bvDBGridDemoForm},
  ColorEditUnit in 'ColorEditUnit.pas' {ColorEditForm},
  FormSaverDemoUnit in 'FormSaverDemoUnit.pas' {FormSaverDemo},
  StandardGrid in 'StandardGrid.pas' {StandardGridDemoForm},
  Grid2Unit in 'Grid2Unit.pas' {bvDBGridDemoForm2},
  Grid3Unit in 'Grid3Unit.pas' {bvDBGridDemoForm3};

{$R *.res}

begin
  Application.Initialize;
Application.CreateForm(TMainForm, MainForm);
   Application.Run;
end.