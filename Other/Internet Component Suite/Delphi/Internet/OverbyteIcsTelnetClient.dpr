program OverbyteIcsTelnetClient;

uses
  Forms,
  OverbyteIcsTelnetClient1 in 'OverbyteIcsTelnetClient1.pas' {TelnetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTelnetForm, TelnetForm);
  Application.Run;
end.
