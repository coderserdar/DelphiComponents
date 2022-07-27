program password;

uses
  Forms,
  Passtest in 'Passtest.pas' {frmPasswordTest};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPasswordTest, frmPasswordTest);
  Application.Run;
end.
