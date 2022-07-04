program HttpTst;

uses
  Forms,
  httptst1 in 'httptst1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
