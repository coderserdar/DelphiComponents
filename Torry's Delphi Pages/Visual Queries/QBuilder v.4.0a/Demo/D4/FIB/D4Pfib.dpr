program D4Pfib;

uses
  Forms,
  D4Ufib in 'D4Ufib.pas' {QBDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TQBDemoForm, QBDemoForm);
  Application.Run;
end.
