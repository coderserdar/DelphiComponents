program Luproj;

uses
  Forms,
  Luform in 'LUFORM.PAS' {Form1},
  Newrec in 'NEWREC.PAS' {FormNewRec};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormNewRec, FormNewRec);
  Application.Run;
end.
