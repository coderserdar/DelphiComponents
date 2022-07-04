program SMFilter;

uses
  Forms,
  Main in 'Main.pas' {frmDemo},
  AvailFld in 'AvailFld.pas' {frmAvailFields};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for TSMDBFilterDialog component';
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
