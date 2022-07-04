program RVDemoD6;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  PopupFrm in 'PopupFrm.pas' {frmPopup},
  Demo3Frm in 'Demo3Frm.pas' {frmDemo3},
  Demo1Frm in 'Demo1Frm.pas' {frmDemo1},
  Demo2Frm in 'Demo2Frm.pas' {frmDemo2},
  Demo4Frm in 'Demo4Frm.pas' {frmDemo4},
  Demo5Frm in 'Demo5Frm.pas' {frmDemo5},
  Demo6Frm in 'Demo6Frm.pas' {frmDemo6},
  Demo7Frm in 'Demo7Frm.pas' {frmDemo7};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
