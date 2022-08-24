program DecoRatingStarsPlusDemo;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DecoRatingStarsPlus Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
