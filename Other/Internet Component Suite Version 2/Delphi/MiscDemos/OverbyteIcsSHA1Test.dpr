program OverbyteIcsSHA1Test;

uses
  Forms,
  OverbyteIcsSHA1Test1 in 'OverbyteIcsSHA1Test1.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
