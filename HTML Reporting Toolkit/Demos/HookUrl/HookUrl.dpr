program HookUrl;

uses
  Forms,
  explorer in 'explorer.pas' {BrowserForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBrowserForm, BrowserForm);
  Application.Run;
end.
