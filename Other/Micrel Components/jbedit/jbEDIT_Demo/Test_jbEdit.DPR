program Test_jbEdit;

uses
  Forms,
  Test_jbEdit1 in 'Test_jbEdit1.PAS' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
