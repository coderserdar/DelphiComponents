program WebServ;

uses
  Forms,
  WebServ1 in 'WebServ1.pas' {WebServForm};

{$R *.RES}

begin
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
