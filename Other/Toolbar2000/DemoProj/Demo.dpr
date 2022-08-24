program Demo;

uses
  Forms,
  Demo1 in 'Demo1.pas' {Form1};

{$R *.RES}

{$IFDEF CLR}
[STAThread]  // All VCL.NET projects that use Toolbar2000 must include this
{$ENDIF}
begin
  Application.Initialize;
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 18.5}
      Application.MainFormOnTaskbar := True;
    {$IFEND}
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

