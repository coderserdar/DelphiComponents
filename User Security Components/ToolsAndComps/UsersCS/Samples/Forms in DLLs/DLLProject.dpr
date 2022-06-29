program DLLProject;

uses
  Forms,
  main_form in 'main_form.pas' {frmMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
