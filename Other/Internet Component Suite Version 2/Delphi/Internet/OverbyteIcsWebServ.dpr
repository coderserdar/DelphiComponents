program OverbyteIcsWebServ;

uses
  Forms,
  OverbyteIcsWebServ1 in 'OverbyteIcsWebServ1.pas' {WebServForm};

{$R *.RES}

begin
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
