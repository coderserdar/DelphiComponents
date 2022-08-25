program OverbyteIcsBatchDnsLookup;



{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsBatchDnsLookup1 in 'OverbyteIcsBatchDnsLookup1.pas' {BatchDnsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBatchDnsLookupForm, BatchDnsLookupForm);
  Application.Run;
end.
