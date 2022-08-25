program offscreen;

uses
  Forms,
  main in 'main.pas' {Mainform};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainform, Mainform);
  Application.Run;
end.
