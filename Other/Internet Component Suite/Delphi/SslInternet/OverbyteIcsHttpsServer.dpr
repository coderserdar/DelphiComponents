program OverbyteIcsHttpsServer;

uses
  Forms,
  OverbyteIcsHttpsServer1 in 'OverbyteIcsHttpsServer1.pas' {HttpsSrvForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THttpsSrvForm, HttpsSrvForm);
  Application.Run;
end.
