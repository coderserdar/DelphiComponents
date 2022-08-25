program IcsWebServ;

uses
  FMX.Forms,
{$IF CompilerVersion < 25}
  FMX.StdCtrls in '..\..\FMX.StdCtrls.pas',
{$IFEND}
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  FMX.Types,
  IcsWebServ1 in 'IcsWebServ1.pas' {WebServForm},
  OverbyteIcsFormDataDecoder in '..\..\..\Source\OverbyteIcsFormDataDecoder.pas';

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := True;
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
