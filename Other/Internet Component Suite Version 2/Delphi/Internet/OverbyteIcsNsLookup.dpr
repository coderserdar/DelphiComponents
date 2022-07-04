program OverbyteIcsNsLookup;

uses
  Forms,
  OverbyteIcsNsLookup1 in 'OverbyteIcsNsLookup1.pas' {NsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNsLookupForm, NsLookupForm);
  Application.Run;
end.
