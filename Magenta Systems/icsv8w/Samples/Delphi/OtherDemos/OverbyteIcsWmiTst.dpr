program OverbyteIcsWmiTst;

uses
  Forms,
  OverbyteIcsWmiTst1 in 'OverbyteIcsWmiTst1.pas' {WmiDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWmiDemoForm, WmiDemoForm);
  Application.Run;
end.
