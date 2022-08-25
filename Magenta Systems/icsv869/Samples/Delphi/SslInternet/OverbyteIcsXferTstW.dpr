program OverbyteIcsXferTstW;

uses
  Forms,
  OverbyteIcsXferTstW1 in 'OverbyteIcsXferTstW1.pas' {FormXferDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ICS Multi File Transfer Wide Demo';
  Application.CreateForm(TFormXferDemo, FormXferDemo);
  Application.Run;
end.

