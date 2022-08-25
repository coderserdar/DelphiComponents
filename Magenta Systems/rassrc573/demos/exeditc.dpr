program exeditc;

uses
  Forms,
  exeditc0 in 'exeditc0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
