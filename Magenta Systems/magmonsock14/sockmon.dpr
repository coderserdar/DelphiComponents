program sockmon;

uses
  Forms,
  monmain in 'monmain.pas' {MonForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Display Packets';
  Application.CreateForm(TMonForm, MonForm);
  Application.Run;
end.
