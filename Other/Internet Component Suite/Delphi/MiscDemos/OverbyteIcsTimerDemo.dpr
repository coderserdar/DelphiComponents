program OverbyteIcsTimerDemo;

uses
  Forms,
  OverbyteIcsTimerDemo1 in 'OverbyteIcsTimerDemo1.pas' {IcsTimerDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTimerDemoForm, IcsTimerDemoForm);
  Application.Run;
end.
