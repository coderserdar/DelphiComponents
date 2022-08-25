program IcsCliDemo;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND} 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsCliDemo1 in 'IcsCliDemo1.pas' {ClientForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
