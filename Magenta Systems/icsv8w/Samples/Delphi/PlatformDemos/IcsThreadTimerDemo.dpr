program IcsThreadTimerDemo;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsThreadTimerDemo1 in 'IcsThreadTimerDemo1.pas' {IcsTimerDemoForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TIcsTimerDemoForm, IcsTimerDemoForm);
  Application.Run;
end.
