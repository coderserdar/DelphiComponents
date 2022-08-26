program iOSActionMessage;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FActionMessages};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFActionMessages, FActionMessages);
  Application.Run;
end.
