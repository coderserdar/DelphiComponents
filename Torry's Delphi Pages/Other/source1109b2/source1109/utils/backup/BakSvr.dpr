program BakSvr;

uses
  Forms,
  BakSrvMn in 'BakSrvMn.pas' {FormBackup};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBackup, FormBackup);
  Application.Run;
end.
