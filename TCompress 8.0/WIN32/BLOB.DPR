program blob;

uses
  Forms,
  Blobdemo in 'Blobdemo.pas' {Form1};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
