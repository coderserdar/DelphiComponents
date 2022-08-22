program CollisionMapExample;

uses
  Forms,
  CollisionMapExampleMain in 'CollisionMapExampleMain.pas' {frmExample},
  RSCollisionMap in '..\..\RSCollisionMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmExample, frmExample);
  Application.Run;
end.
