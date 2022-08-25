program IcsUdpSend;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  IcsUdpSend1 in 'IcsUdpSend1.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
