program Inst;

uses
  Forms,
  Inst002 in 'INST002.PAS' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
