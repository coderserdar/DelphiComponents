program OverbyteIcsNewsReader;

uses
  Forms,
  OverbyteIcsNewsReader1 in 'OverbyteIcsNewsReader1.pas' {NNTPForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
