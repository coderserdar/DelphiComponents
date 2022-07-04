program Address;

uses
  Forms,
  AdrMain in 'AdrMain.pas' {MainForm};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
