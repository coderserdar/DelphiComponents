program D4Pbde;

uses
  Forms,
  D4Ubde in 'D4Ubde.pas' {QBDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TQBDemoForm, QBDemoForm);
  Application.Run;
end.
