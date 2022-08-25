program IcsDllTst;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsDllTst1 in 'IcsDllTst1.pas' {DllTestForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := True;
  Application.CreateForm(TDllTestForm, DllTestForm);
  Application.Run;
end.
