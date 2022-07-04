program OverbyteIcsHttpAsp;

uses
  Forms,
  OverbyteIcsHttpAsp1 in 'OverbyteIcsHttpAsp1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
