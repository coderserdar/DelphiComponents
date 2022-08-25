program OverbyteIcsBufStrmTst;

uses
  //FastMM4,
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsBufStrmTst1 in 'OverbyteIcsBufStrmTst1.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
