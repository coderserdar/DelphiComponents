program IcsBatchDnsLookup;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  IcsBatchDnsLookup1 in 'IcsBatchDnsLookup1.pas' {BatchDnsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBatchDnsLookupForm, BatchDnsLookupForm);
  Application.Run;
end.
