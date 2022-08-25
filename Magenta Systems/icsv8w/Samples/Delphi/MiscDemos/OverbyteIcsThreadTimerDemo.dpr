program OverbyteIcsThreadTimerDemo;
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  FastAppProcessMessagePatch in 'FastAppProcessMessagePatch.pas',
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsThreadTimerDemo1 in 'OverbyteIcsThreadTimerDemo1.pas' {IcsTimerDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTimerDemoForm, IcsTimerDemoForm);
  Application.Run;
end.
