program exmon;

uses
  Forms,
  exmon0 in 'exmon0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
