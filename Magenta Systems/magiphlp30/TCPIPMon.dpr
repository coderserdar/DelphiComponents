program TCPIPMon;

uses
  Forms,
  uTCPIP in 'uTCPIP.pas' {IPForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIPForm, IPForm);
  Application.Run;
end.
