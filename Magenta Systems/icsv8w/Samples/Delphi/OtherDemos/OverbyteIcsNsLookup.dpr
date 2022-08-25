program OverbyteIcsNsLookup;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsNsLookup1 in 'OverbyteIcsNsLookup1.pas' {NsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNsLookupForm, NsLookupForm);
  Application.Run;
end.
