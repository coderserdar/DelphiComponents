program OverbyteIcsSimpleSslServer;

uses
  Forms,
  OverbyteIcsSimpleSslServer1 in 'OverbyteIcsSimpleSslServer1.pas' {SimpleSslServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TSimpleSslServerForm, SimpleSslServerForm);
  Application.Run;
end.
