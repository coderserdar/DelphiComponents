program HttpAsy;

uses
  Forms,
  HttpAsy1 in 'HttpAsy1.pas' {HttpAsyForm};

{$R *.RES}

begin
  Application.CreateForm(THttpAsyForm, HttpAsyForm);
  Application.Run;
end.
