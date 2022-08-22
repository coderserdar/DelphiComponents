program NHunspellTest;

uses
  Forms,
  uHunspellTestMain in 'uHunspellTestMain.pas' {FrmHunspellTestMain},
  NHunspell in '..\NHunspell.pas',
  NHunXml in '..\NHunXml.pas',
  PasZip in '..\PasZip.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmHunspellTestMain, FrmHunspellTestMain);
  Application.Run;
end.
