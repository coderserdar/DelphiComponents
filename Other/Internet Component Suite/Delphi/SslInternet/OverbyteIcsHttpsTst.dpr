program OverbyteIcsHttpsTst;

uses
  Forms,
  OverbyteIcsHttpsTst1 in 'OverbyteIcsHttpsTst1.pas' {HttpsTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpsTstForm, HttpsTstForm);
  Application.Run;
end.
