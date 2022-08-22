program Project1;

uses
  Forms,
  Unit1 in 'UNIT1.PAS' {Form1},
  Handles in 'HANDLES.PAS';

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
