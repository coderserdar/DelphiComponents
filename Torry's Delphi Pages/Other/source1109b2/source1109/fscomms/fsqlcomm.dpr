program FSQLComm;

uses
  {$IFDEF USETeDEBUG}
  TeDebug,
  {$ENDIF}
  Forms,
  ufscomms in 'ufscomms.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FSSQL Client Communications Utility';
  Application.CreateForm(TfrmFFCommsMain, frmFFCommsMain);
  Application.Run;
end.
