program D4PNCOCI8;

uses
  Forms,
  D4UNCOCI8 in 'D4UNCOCI8.pas' {QBDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TQBDemoForm, QBDemoForm);
  Application.Run;
end.
