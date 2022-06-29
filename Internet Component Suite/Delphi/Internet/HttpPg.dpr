program HttpPg;

uses
  Forms,
  HttpPg1 in 'HttpPg1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
