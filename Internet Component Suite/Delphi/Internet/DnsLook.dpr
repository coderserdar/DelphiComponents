program DnsLook;

uses
  Forms,
  DnsLook1 in 'DnsLook1.pas' {DnsLookupForm};

{$R *.RES}

begin
  Application.CreateForm(TDnsLookupForm, DnsLookupForm);
  Application.Run;
end.
