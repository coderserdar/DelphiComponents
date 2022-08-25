program OverbyteIcsJoseTst;

uses
  Forms,
  OverbyteIcsJoseTst1 in 'OverbyteIcsJoseTst1.pas' {JsonDemoForm},
  OverbyteIcsJoseTst2 in 'OverbyteIcsJoseTst2.pas' {FormObject};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TJsonDemoForm, JsonDemoForm);
  Application.CreateForm(TFormObject, FormObject);
  Application.Run;
end.
