program D4Pibo;

uses
  Forms,
  D4Uibo in 'D4Uibo.pas' {QBDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TQBDemoForm, QBDemoForm);
  Application.Run;
end.
