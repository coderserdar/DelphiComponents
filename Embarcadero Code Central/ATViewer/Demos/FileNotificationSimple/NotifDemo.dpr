program NotifDemo;

uses
  Forms,
  UFormMain in 'UFormMain.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ATFileNotificationSimple Demo';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
