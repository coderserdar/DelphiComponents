program AutoCorrectDemo;

{ This demo doesn't require the component package to be installed first. }

uses
  Forms,
  CCR.AutoCorrect in '..\CCR.AutoCorrect.pas',
  CCR.AutoCorrect.Consts in '..\CCR.AutoCorrect.Consts.pas',
  CCR.AutoCorrect.Controls in '..\CCR.AutoCorrect.Controls.pas',
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  {$IF CompilerVersion >= 18.5}
  Application.MainFormOnTaskbar := True;
  {$IFEND}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
