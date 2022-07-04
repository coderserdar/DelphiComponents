program ELPropertyInspector;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  BevelCutFrm in 'BevelCutFrm.pas' {frmBevelCut};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmBevelCut, frmBevelCut);
  Application.Run;
end.
