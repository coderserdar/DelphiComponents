program PBEditPackDemo;

uses
  Forms,
  PBEditPackDemo_unit in 'PBEditPackDemo_unit.pas' {PBEditPackDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TPBEditPackDemoForm, PBEditPackDemoForm);
  Application.Run;
end.

