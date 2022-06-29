program DirDraw;

uses
  Forms,
  DirDrawU in 'DirDrawU.pas' {EanDirectDraw};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Barcode printer - simple version';
  Application.CreateForm(TEanDirectDraw, EanDirectDraw);
  Application.Run;
end.
