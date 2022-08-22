program Teststr;

uses
  Forms,
  TESTSTR1 in 'TESTSTR1.PAS' {Form1},
  JBSTR in '..\JBSTR.PAS';

{$R *.RES}

begin
  Application.Initialize;
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion > 18}
  Application.MainFormOnTaskbar := True;
  {$IFEND}
  {$ENDIF}  
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
