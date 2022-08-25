program diskfmt;

uses
  Forms,
  fmtmain in 'fmtmain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
