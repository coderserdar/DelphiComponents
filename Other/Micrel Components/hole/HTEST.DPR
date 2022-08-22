program Htest;

uses
  Forms,
  Holetest in 'HOLETEST.PAS' {Form1},
  Hole in 'HOLE.PAS';

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
