program httpChk;

uses
  Forms,
  HttpChk1 in 'HttpChk1.pas' {CheckUrlForm};

{$R *.RES}

begin
  Application.CreateForm(TCheckUrlForm, CheckUrlForm);
  Application.Run;
end.
