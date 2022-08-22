program DemoNavi;

uses
  Forms,
  Main in 'Main.pas' {TB97DBNaviForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTB97DBNaviForm, TB97DBNaviForm);
  Application.Run;
end.
