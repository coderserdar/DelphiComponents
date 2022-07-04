program mtsrv;

uses
  Forms,
  mtsrv1 in 'mtsrv1.pas' {ServerForm},
  mtsrv2 in 'mtsrv2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
