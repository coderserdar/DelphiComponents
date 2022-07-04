program HttpAsp;

uses
  Forms,
  HttpAsp1 in 'HttpAsp1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
