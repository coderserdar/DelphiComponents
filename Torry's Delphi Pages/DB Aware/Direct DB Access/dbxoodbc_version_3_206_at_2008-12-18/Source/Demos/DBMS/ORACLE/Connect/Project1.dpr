program Project1;

uses
  Forms,
  DbxOpenOdbcStatic,
  dbx_ora_connect in 'dbx_ora_connect.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
