program EditableDataset;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
