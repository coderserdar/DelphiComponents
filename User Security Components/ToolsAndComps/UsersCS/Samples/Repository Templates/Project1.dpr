program Project1;

uses
  Forms,
  frmMainForm_Rep in 'FRMMAINFORM_REP.pas' {frmMainForm_Rep},
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frm02},
  frmBasico_Rep in 'FRMBASICO_REP.pas' {frmBasico_Rep},
  Unit3 in 'Unit3.pas' {frm01};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(Tfrm01, frm01);
  Application.Run;
end.
