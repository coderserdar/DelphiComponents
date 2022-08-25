program OverbyteIcsMailQuTst;

uses
  Forms,
  OverbyteIcsMailQuTst1 in 'OverbyteIcsMailQuTst1.pas' {DemoForm},
  OverbyteIcsMailQuTstdiag in 'OverbyteIcsMailQuTstdiag.pas' {DiagForm},
  OverbyteIcsMailQuTstView in 'OverbyteIcsMailQuTstView.pas' {ViewQuForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Mail Queue Demo';
  Application.CreateForm(TDemoForm, DemoForm);
  Application.CreateForm(TDiagForm, DiagForm);
  Application.CreateForm(TViewQuForm, ViewQuForm);
  Application.Run;
end.
