program OverbyteIcsProxySslServer;

uses
  Forms,
  OverbyteIcsProxySslServer1 in 'OverbyteIcsProxySslServer1.pas' {ProxySslServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TProxySslServerForm, ProxySslServerForm);
  Application.Run;
end.
