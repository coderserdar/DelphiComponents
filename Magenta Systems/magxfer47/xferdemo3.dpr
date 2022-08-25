program xferdemo3;

{$R 'xf3manifest.res' 'xf3manifest.rc'}

uses
  Forms,
  demomain3 in 'demomain3.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

