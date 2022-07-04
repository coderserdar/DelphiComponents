program HexEditor;

{$include kcontrols.inc}

uses
  Forms,
  Res in 'Res.pas',
  Basic in 'Basic.pas',
  About in 'About.pas' {AboutForm},
  Search in 'Search.pas' {SearchForm},
  Replace in 'Replace.pas' {ReplaceForm},
  Options in 'Options.pas' {OptionsForm},
  ReplacePrompt in 'ReplacePrompt.pas' {ReplacePromptForm},
  Main in 'Main.pas' {MainForm},
  PrintStatus in 'PrintStatus.pas' {PrintStatusForm};

{$R *.res}
{$IFDEF USE_THEMES}
  {$R xpman.res}
{$ENDIF}
begin
{$IFDEF COMPILER10_UP}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TReplaceForm, ReplaceForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TReplacePromptForm, ReplacePromptForm);
  Application.CreateForm(TPrintStatusForm, PrintStatusForm);
  Application.Run;
end.
