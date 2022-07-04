program OverbyteIcsPingTst;

uses
  Forms,
  OverbyteIcsPingTst1 in 'OverbyteIcsPingTst1.pas' {PingTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPingTstForm, PingTstForm);
  Application.Run;
end.
