program Plusdemo;

uses
  Forms,
  Mainform in 'MAINFORM.PAS' {AppMainForm};

{$R *.RES}

begin
  Application.CreateForm(TAppMainForm, AppMainForm);
  Application.Run;
end.
 