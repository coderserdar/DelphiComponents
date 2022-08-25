program IcsHttpsTst;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND}
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsCliCertDlg in 'IcsCliCertDlg.pas' {ClientCertDlg},
  IcsHttpsTst1 in 'IcsHttpsTst1.pas' {HttpsTstForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(THttpsTstForm, HttpsTstForm);
  Application.CreateForm(TClientCertDlg, ClientCertDlg);
  Application.Run;
end.
