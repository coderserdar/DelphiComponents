program exmon6;

uses
  Forms,
  exmon1 in 'exmon1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
