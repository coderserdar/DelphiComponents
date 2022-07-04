library BrParts;

uses
  ComServ,
  BrParts_TLB in 'BrParts_TLB.pas',
  BrPartsImpl in 'BrPartsImpl.pas' {BrPartsFrm: TActiveForm} {BrPartsFrm: CoClass},
  Edparts in 'edparts.pas' {EdPartsForm};

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
