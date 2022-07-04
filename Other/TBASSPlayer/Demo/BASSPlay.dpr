program BASSPlay;

uses
  Forms,
  BASSPlayer in '..\BASSPlayer.pas',
  BassTest in 'BassTest.pas' {MainForm},
  DSPPluginCtrl in 'DSPPluginCtrl.pas' {DSPControlForm},
  AddonConfig in 'AddonConfig.pas' {AddonConfigForm},
  VisPluginCtrl in 'VisPluginCtrl.pas' {VisControlForm},
  UniCodeUtils in '..\UniCodeUtils.pas',
  PlayListUtils in '..\PlayListUtils.pas',
  PlayListConfig in 'PlayListConfig.pas' {PlayListConfigForm},
  InputURL in 'InputURL.pas' {URLInputForm},
  StatusView in 'StatusView.pas' {StatusViewForm},
  About in 'About.pas' {AboutForm};

{$R *.res}

begin
  System.IsMultiThread := true;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDSPControlForm, DSPControlForm);
  Application.CreateForm(TAddonConfigForm, AddonConfigForm);
  Application.CreateForm(TVisControlForm, VisControlForm);
  Application.CreateForm(TPlayListConfigForm, PlayListConfigForm);
  Application.CreateForm(TURLInputForm, URLInputForm);
  Application.CreateForm(TStatusViewForm, StatusViewForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.
