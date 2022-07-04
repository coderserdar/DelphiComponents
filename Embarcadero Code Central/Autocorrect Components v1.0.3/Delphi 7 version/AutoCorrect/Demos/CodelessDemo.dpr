program CodelessDemo;

{ Install the component package before attempting to run this demo. }

uses                                
  Forms,
  CCR.AutoCorrect in '..\CCR.AutoCorrect.pas',
  CCR.AutoCorrect.Consts in '..\CCR.AutoCorrect.Consts.pas',
  CCR.AutoCorrect.Controls in '..\CCR.AutoCorrect.Controls.pas',
  CodelessDemoForm in 'CodelessDemoForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
