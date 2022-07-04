program OverbyteIcsDnsLook;


uses
  Forms,
  OverbyteIcsDnsLook1 in 'OverbyteIcsDnsLook1.pas' {DnsLookupForm};

{$R *.RES}

begin
  Application.CreateForm(TDnsLookupForm, DnsLookupForm);
  Application.Run;
end.
