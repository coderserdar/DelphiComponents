program OverbyteIcsTimerDemo;

uses
  Forms, OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsTimerDemo1 in 'OverbyteIcsTimerDemo1.pas' {IcsTimerDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTimerDemoForm, IcsTimerDemoForm);
  Application.Run;
end.
