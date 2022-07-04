program Server;

uses
  Forms,
  ServerMain in 'ServerMain.pas' {frmServer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.Run;
end.
