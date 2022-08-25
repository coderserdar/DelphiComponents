program xferdemo3w;

{$R 'xf3wmanifest.res' 'xf3wmanifest.rc'}

uses
  Forms,
  demomain3w in 'demomain3w.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

