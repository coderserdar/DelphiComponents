program sensortest;

uses
  Forms,
  sensormain in 'sensormain.pas' {FormMain},
  sensormap in 'sensormap.pas' {FormMap};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
