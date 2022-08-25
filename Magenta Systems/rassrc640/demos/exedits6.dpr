program exedits6;

uses
  Forms,
  exedits1 in 'exedits1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
