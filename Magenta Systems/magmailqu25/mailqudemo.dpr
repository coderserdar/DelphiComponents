program mailqudemo;

uses
  Forms,
  mailqumain in 'mailqumain.pas' {DemoForm},
  mailqudiag in 'mailqudiag.pas' {DiagForm},
  mailquview in 'mailquview.pas' {ViewQuForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Magenta Mail Queue Demo';
  Application.CreateForm(TDemoForm, DemoForm);
  Application.CreateForm(TDiagForm, DiagForm);
  Application.CreateForm(TViewQuForm, ViewQuForm);
  Application.Run;
end.
