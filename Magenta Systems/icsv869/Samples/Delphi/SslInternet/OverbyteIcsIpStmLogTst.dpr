program OverbyteIcsIpStmLogTst;

uses
  Forms,
  OverbyteIcsIpStmLogTst1 in 'OverbyteIcsIpStmLogTst1.pas' {IpLogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'IP Streaming Log';
  Application.CreateForm(TIpLogForm, IpLogForm);
  Application.Run;
end.

