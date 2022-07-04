program Client;

uses
  Forms,
  ClientMain in 'ClientMain.pas' {frmClient};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClient, frmClient);
  Application.Run;
end.
