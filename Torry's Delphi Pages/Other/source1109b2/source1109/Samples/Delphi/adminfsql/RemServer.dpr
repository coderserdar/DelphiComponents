program RemServer;

uses
  Forms,
  frm_Client in 'frm_Client.pas' {frmClient},
  frm_GenInfo in 'frm_GenInfo.pas' {frmGenInfo},
  frm_Single in 'frm_Single.pas' {frmSingle},
  frm_Users in 'frm_Users.pas' {frmUsers},
  frm_UsrModify in 'frm_UsrModify.pas' {frmUsrModify};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClient, frmClient);
  Application.Run;
end.
