program Demo1;

uses
  Forms,
  main in 'main.pas' {Form1},
  GrafixDX in '..\GrafixDX.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
