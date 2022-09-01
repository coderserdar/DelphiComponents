program DemoDBFormAction;

uses
  Forms,
  UfrmVendors in 'UfrmVendors.pas' {frmVendors},
  UdmBase in 'UdmBase.pas' {dmBase: TDataModule},
  UfrmCountry in 'UfrmCountry.pas' {frmCountry},
  UfrmCustomer in 'UfrmCustomer.pas' {frmCustomers},
  UDemoDBFAMainForm in 'UDemoDBFAMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmBase, dmBase);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
