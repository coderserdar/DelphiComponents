program OverbyteIcsHttpsTst;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res' '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpsTst1 in 'OverbyteIcsHttpsTst1.pas' {HttpsTstForm},
  OverbyteIcsLogin in 'OverbyteIcsLogin.pas' {FormLogin},
  OverbyteIcsCliCertDlg in 'OverbyteIcsCliCertDlg.pas' {ClientCertDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpsTstForm, HttpsTstForm);
  Application.Run;
end.
