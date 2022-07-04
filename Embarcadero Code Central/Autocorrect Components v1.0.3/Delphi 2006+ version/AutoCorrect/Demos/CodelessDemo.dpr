program CodelessDemo;

{ Install the component package before attempting to run this demo. }

uses
  Forms,
  CodelessDemoForm in 'CodelessDemoForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  {$IF CompilerVersion >= 18.5}
  Application.MainFormOnTaskbar := True;
  {$IFEND}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
