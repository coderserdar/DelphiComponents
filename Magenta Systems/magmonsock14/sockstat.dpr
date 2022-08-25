program sockstat;

uses
  Forms,
  statmain in 'statmain.pas' {StatForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Traffic Monitor';
  Application.CreateForm(TStatForm, StatForm);
  Application.Run;
end.
