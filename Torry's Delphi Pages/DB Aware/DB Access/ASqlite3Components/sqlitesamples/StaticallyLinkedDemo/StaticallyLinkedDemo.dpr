program StaticallyLinkedDemo;

uses
  Forms,
  Main in 'Main.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
