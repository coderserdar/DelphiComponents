{$MAXSTACKSIZE $00200000} //Recommended for RegEx library.

program SearchDemo;

uses
  Forms,
  UFormMain in 'UFormMain.pas' {FormMain},
  UFormProgress in 'UFormProgress.pas' {FormProgress};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATStreamSearch Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormProgress, FormProgress);
  Application.Run;
end.
