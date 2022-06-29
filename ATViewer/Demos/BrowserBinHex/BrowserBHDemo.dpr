program BrowserBHDemo;

uses
  Forms,
  UFormView in 'UFormView.pas' {FormView};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATBinHex Demo';
  Application.CreateForm(TFormView, FormView);
  Application.Run;
end.
