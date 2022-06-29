library ActiveFormProj;

uses
  ComServ,
  ActiveFormProj_TLB in 'ActiveFormProj_TLB.pas',
  actfrm_main in 'actfrm_main.pas' {ActiveFormTeste: TActiveForm} {ActiveFormX: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

{$E ocx}

begin
end.
