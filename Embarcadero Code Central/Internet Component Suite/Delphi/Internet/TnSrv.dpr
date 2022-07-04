program Tnsrv;

uses
  Forms,
  TnSrv1 in 'TnSrv1.pas' {ServerForm},
  TnSrv2 in 'TnSrv2.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
