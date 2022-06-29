program UdpSend;

uses
  Forms,
  UdpSend1 in 'UdpSend1.pas' {MainAutoForm};

{$R *.RES}

begin
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
