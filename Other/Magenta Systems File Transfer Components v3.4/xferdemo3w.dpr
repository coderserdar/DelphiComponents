program xferdemo3w;

uses
  Forms,
  demomain3w in 'demomain3w.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

