program tndemo;

uses
  Forms,
  TnDemo1 in 'TnDemo1.pas' {TnDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TTnDemoForm, TnDemoForm);
  Application.Run;
end.
