program OverbyteIcsSslMultiWebServ64;

uses
  Forms,
  OverbyteIcsSslMultiWebServ1 in 'OverbyteIcsSslMultiWebServ1.pas' {WeblServerForm},
  OverbyteIcsSslMultiWebDataModule in 'OverbyteIcsSslMultiWebDataModule.pas' {SslMultiWebDataModule: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWeblServerForm, WeblServerForm);
  Application.CreateForm(TSslMultiWebDataModule, SslMultiWebDataModule);
  Application.Run;
end.
