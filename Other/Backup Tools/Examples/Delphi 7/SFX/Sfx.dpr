program Sfx;

uses
  Forms,
  fmMain in 'fmMain.pas' {Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
