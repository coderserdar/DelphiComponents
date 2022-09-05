program TestDIBEffects;

uses
  Forms,
  TestDIBEffects1 in 'TestDIBEffects1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF CONDITIONALEXPRESSIONS}
 {$IF CompilerVersion > 18.0}
  Application.MainFormOnTaskBar := True;
 {$IFEND}
{$ENDIF}
  Application.Title := 'Test DIB effects';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.