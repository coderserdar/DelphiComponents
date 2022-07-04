program WinArchiver;
{$I mmm.inc}

uses
  Forms,
  fmMain in 'fmMain.pas' {Main},
  fmAboutBox in 'fmAboutBox.pas' {AboutBox},
  fmAddDropedFiles in 'fmAddDropedFiles.pas' {AddDropedFiles},
  fmExtract in 'fmExtract.pas' {Extract},
  fmDelete in 'fmDelete.pas' {Delete},
  fmCreateFolder in 'fmCreateFolder.pas' {CreateFolder},
  fmInformation in 'fmInformation.pas' {Information},
  fmConfiguration in 'fmConfiguration.pas' {Configuration},
  fmEnterCryptKey in 'fmEnterCryptKey.pas' {EnterCryptKey},
  fmSFXConfig in 'fmSFXConfig.pas' {SFXConfig},
  fmHelpOnSFX in 'fmHelpOnSFX.pas' {HelpOnSFX},
  fmArchComment in 'fmArchComment.pas' {ArchComment},
  fmTiming in 'fmTiming.pas' {Timing},
  fmAdd in 'fmAdd.pas' {Add},
  fmSFXComments in 'fmSFXComments.pas' {SFXComments},
  fmView in 'fmView.pas' {View},
  fmTextViewer in 'fmTextViewer.pas' {TextViewer},
  unTranslation in 'unTranslation.pas',
  unTransObj in 'unTransObj.pas',
  dropsource in 'Components\DropSource.pas',
  langCzech in 'Languages\langCzech.pas',
  langFrench in 'Languages\langFrench.pas',
  langGerman in 'Languages\langGerman.pas',
  langPortuguese in 'Languages\langPortuguese.pas',
  langChinese in 'Languages\langChinese.pas',
  langItalian in 'Languages\langItalian.pas',
  langRussian in 'Languages\langRussian.pas',
  langSpanish in 'Languages\langSpanish.pas',
  fmLastOutput in 'fmLastOutput.pas' {LastOutput},
  langDanish in 'Languages\langDanish.pas',
  langDutch in 'Languages\langDutch.pas',
  langEnglish in 'Languages\langEnglish.pas',
  fmFilters in 'fmFilters.pas' {Filters};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WinArchiver';
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TAddDropedFiles, AddDropedFiles);
  Application.CreateForm(TExtract, Extract);
  Application.CreateForm(TDelete, Delete);
  Application.CreateForm(TCreateFolder, CreateFolder);
  Application.CreateForm(TInformation, Information);
  Application.CreateForm(TConfiguration, Configuration);
  Application.CreateForm(TEnterCryptKey, EnterCryptKey);
  Application.CreateForm(TSFXConfig, SFXConfig);
  Application.CreateForm(THelpOnSFX, HelpOnSFX);
  Application.CreateForm(TArchComment, ArchComment);
  Application.CreateForm(TAdd, Add);
  Application.CreateForm(TSFXComments, SFXComments);
  Application.CreateForm(TView, View);
  Application.CreateForm(TTextViewer, TextViewer);
  Application.CreateForm(TLastOutput, LastOutput);
  Application.CreateForm(TFilters, Filters);
  Application.Run;
end.
