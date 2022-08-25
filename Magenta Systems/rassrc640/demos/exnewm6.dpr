program exnewm6;

uses
  Forms,
  exnewm1 in 'exnewm1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
