program Test_jbDBF;

uses
  Forms,
  Test_jbDBF1 in 'Test_jbDBF1.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion > 18}
  Application.MainFormOnTaskbar := True;
  {$IFEND}
  {$ENDIF}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
