program OverbyteIcsSslNewsRdr;

uses
  Forms,
  OverbyteIcsSslNewsRdr1 in 'OverbyteIcsSslNewsRdr1.pas' {NNTPForm};

{$R *.RES}

begin
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
