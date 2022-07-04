program Demo;

uses
  Forms,
  Ex1U in 'Ex1U.pas' {frmTest},
  DemoAb in 'DemoAb.pas' {frmAbout};

{$R *.RES}

{.$DEFINE MEMORY_CHECK}
begin
{$IFDEF MEMORY_CHECK}
  MemChk;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmTest, frmTest);
  Application.Run;
end.
