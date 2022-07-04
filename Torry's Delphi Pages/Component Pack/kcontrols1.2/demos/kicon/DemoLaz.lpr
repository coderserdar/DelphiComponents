program DemoLaz;

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainForm}, LResources;

{$IFDEF WINDOWS}{$R DemoLaz.rc}{$ENDIF}

begin
  {$I DemoLaz.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
