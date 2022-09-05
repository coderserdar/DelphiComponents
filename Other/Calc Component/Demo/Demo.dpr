program Demo;

uses
  Forms,
  Main in 'Main.pas' {DemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
