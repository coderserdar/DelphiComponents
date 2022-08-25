program OverbyteIcsXferTst;

uses
  Forms,
  OverbyteIcsXferTst1 in 'OverbyteIcsXferTst1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ICS Multi File Transfer Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

