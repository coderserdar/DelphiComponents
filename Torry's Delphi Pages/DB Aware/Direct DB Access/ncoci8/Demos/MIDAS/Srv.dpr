program Srv;

uses
  Forms,
  SrvMainFrm in 'SrvMainFrm.pas' {Form2},
  Srv_TLB in 'Srv_TLB.pas',
  SrvDM in 'SrvDM.pas' {NCOCI8MidasTest: TRemoteDataModule} {NCOCI8MidasTest: CoClass};

{$R *.TLB}

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
