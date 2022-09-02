Program fsserver;

{$I FSDEFINE.INC}

uses
  Forms,
  //MemCheck,
  fssrvmain in 'fssrvmain.pas' {fsFSSQLServerForm},
  fssrvalas in 'fssrvalas.pas' {fsAliasForm},
  fssrvbrws in 'fssrvbrws.pas' {fsDirBrowseForm},
  fssrvuser in 'fssrvuser.pas' {fsUserForm},
  fssrvpwd in 'fssrvpwd.pas' {fsPwdForm},
  fssrvgenl in 'fssrvgenl.pas' {fsGenConfigForm},
  FsSRJrn in '..\fssrjrn.pas' {fsJournalForm},
  fssrvegmgr in 'fssrvegmgr.pas' {fsEngineManager};

{$R *.RES}
Begin
  Application.Title := 'FSSQL Server Database';
  Application.CreateForm(TfsFSSQLServerForm, fsFSSQLServerForm);
  Application.Run;
  //MemCheck.memchk;
End.

