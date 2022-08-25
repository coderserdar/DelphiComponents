program OverByteIcsDnsResolver;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsDnsResolver1 in 'OverbyteIcsDnsResolver1.pas' {DnsResolverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDnsResolverForm, DnsResolverForm);
  Application.Run;
end.
