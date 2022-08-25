program IcsThread;

uses
  FMX.Forms,
  FMX.Types,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND}
  IcsThread1 in 'IcsThread1.pas' {frmMain},
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  Ics.Fmx.DemoUtils in 'Ics.Fmx.DemoUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := True;
  GlobalUseDirect2D        := False;
{$IF FireMonkeyVersion >= 17}
  GlobalUseDX10            := False;
{$IFEND}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
