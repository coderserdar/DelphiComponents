program OverbyteIcsMimeDemo;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMimeDemo1 in 'OverbyteIcsMimeDemo1.pas' {MimeDecodeForm};

{$R *.RES}

begin
  Application.CreateForm(TMimeDecodeForm, MimeDecodeForm);
  Application.Run;
end.
