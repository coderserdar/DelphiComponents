program NsLookup;

uses
  Forms,
  NsLook1 in 'NsLook1.pas' {NsLookupForm};

{$R *.RES}

begin
  Application.CreateForm(TNsLookupForm, NsLookupForm);
  Application.Run;
end.
