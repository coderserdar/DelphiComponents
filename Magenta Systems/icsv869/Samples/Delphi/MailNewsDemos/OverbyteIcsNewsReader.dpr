program OverbyteIcsNewsReader;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsNewsReader1 in 'OverbyteIcsNewsReader1.pas' {NNTPForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
