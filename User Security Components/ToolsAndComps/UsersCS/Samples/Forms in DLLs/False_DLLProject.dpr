program False_DLLProject;

uses
  Forms,
  main_form in 'main_form.pas' {frmMainForm},
  unit_form1 in 'unit_form1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
