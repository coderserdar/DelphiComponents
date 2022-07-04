program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  DBMS_SQL in 'DBMS_SQL.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

