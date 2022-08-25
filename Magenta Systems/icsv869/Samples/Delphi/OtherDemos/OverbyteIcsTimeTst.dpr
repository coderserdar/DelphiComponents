program OverbyteIcsTimeTst;

uses
  Forms,
  OverbyteIcsTimeTst1 in 'OverbyteIcsTimeTst1.pas' {TimeDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TTimeDemoForm, TimeDemoForm);
  Application.Run;
end.
