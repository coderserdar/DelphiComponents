program OverbyteIcsMailHtml;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMailHtm1 in 'OverbyteIcsMailHtm1.pas' {HtmlMailForm};

{$R *.res}

begin
  {$IFNDEF VER80}Application.Initialize;{$ENDIF}
  Application.CreateForm(THtmlMailForm, HtmlMailForm);
  Application.Run;
end.

