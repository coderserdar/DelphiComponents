program exnewm;

uses
  Forms,
  exnewm0 in 'exnewm0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
