program OverbyteIcsSimpleSslCli;

uses
  Forms,
  OverbyteIcsSimpleSslCli1 in 'OverbyteIcsSimpleSslCli1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
