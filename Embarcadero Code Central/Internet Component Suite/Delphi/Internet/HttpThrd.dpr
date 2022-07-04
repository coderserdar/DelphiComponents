program HttpThrd;

uses
  Forms,
  HttpThr1 in 'HttpThr1.pas' {HttpThreadForm},
  HttpThr2 in 'HttpThr2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THttpThreadForm, HttpThreadForm);
  Application.Run;
end.
