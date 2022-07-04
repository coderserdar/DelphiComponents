program OverbyteIcsOneTimePassword;

uses
  Forms,
  OverbyteIcsOneTimePassword1 in 'OverbyteIcsOneTimePassword1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

