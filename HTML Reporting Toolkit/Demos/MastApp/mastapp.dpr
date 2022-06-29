program mastapp;

uses
  Forms,
  Dialogs,
  main in 'main.pas' {MainFrm},
  DataMod in 'datamod.pas' {MastData: TDataModule},
  EDOrders in 'edorders.pas' {EdOrderForm},
  SrchDlg in 'srchdlg.pas' {SearchDlg},
  mastapp_TLB in 'mastapp_TLB.pas',
  MSXML2 in '..\..\Source\MSXML2.pas';

{$R *.TLB}

{$R *.RES}

begin
  Application.Title := 'Marine Adventures Order Entry';
  Application.Initialize;
  Application.CreateForm(TMastData, MastData);
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TEdOrderForm, EdOrderForm);
  Application.CreateForm(TSearchDlg, SearchDlg);
  Application.Run;
end.
