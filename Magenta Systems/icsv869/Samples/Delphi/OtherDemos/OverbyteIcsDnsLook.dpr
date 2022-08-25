program OverbyteIcsDnsLook;


{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsDnsLook1 in 'OverbyteIcsDnsLook1.pas' {DnsLookupForm};

{$R *.RES}

begin
  Application.CreateForm(TDnsLookupForm, DnsLookupForm);
  Application.Run;
end.
