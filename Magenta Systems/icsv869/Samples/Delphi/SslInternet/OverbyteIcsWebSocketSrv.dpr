program OverbyteIcsWebSocketSrv;

uses
  Forms,
  OverbyteIcsWebSocketSrv1 in 'OverbyteIcsWebSocketSrv1.pas' {WebSocketForm};

{$R *.RES}

begin
  Application.CreateForm(TWebSocketForm, WebSocketForm);
  Application.Run;
end.
