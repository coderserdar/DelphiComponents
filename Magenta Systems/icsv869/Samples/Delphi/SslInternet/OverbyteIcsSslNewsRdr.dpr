program OverbyteIcsSslNewsRdr;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslNewsRdr1 in 'OverbyteIcsSslNewsRdr1.pas' {NNTPForm};

{$R *.RES}

begin
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
