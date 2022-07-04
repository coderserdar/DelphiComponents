program SrvDemo;

uses
  Forms,
  SrvDemo1 in 'SrvDemo1.pas' {SrvForm},
  SrvDemo2 in 'SrvDemo2.pas' {CliForm};

{$R *.RES}

begin
  Application.CreateForm(TSrvForm, SrvForm);
  Application.Run;
end.
