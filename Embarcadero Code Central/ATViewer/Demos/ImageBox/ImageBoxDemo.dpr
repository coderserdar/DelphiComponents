program ImageBoxDemo;

uses
  Forms,
  UFormMain in 'UFormMain.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATImageBox Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
