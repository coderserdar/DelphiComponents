program exedits;

uses
  Forms,
  exedits0 in 'exedits0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
