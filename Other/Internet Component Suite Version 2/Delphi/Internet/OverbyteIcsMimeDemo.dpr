program OverbyteIcsMimeDemo;

uses
  Forms,
  OverbyteIcsMimeDemo1 in 'OverbyteIcsMimeDemo1.pas' {MimeDecodeForm};

{$R *.RES}

begin
  Application.CreateForm(TMimeDecodeForm, MimeDecodeForm);
  Application.Run;
end.
