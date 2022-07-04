program OverByteIcsDnsResolver;

uses
  Forms,
  OverbyteIcsDnsResolver1 in 'OverbyteIcsDnsResolver1.pas' {DnsResolverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDnsResolverForm, DnsResolverForm);
  Application.Run;
end.
