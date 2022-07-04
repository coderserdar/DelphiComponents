program OverbyteIcsSslWebServ;

uses
  Forms,
  OverbyteIcsSslWebServ1 in 'OverbyteIcsSslWebServ1.pas' {SslWebServForm};

{$R *.RES}

begin
  Application.CreateForm(TSslWebServForm, SslWebServForm);
  Application.Run;
end.
