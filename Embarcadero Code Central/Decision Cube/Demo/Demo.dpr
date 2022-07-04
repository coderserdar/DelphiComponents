program Demo;

{
  This demo demonstrate use decision cube.
  It's use InterBase and Borland Database
  c:/Program Files/Common Files/Borland Shared/Data/MastSql.gdb
}

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Data in 'Data.pas' {DM: TDataModule},
  ChartOptions in 'ChartOptions.pas' {frmChartOptions: TFrame},
  ChartDlg in 'ChartDlg.pas' {dlgChartOpt},
  MSData in 'MSData.pas' {dmMSSQL: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TdlgChartOpt, dlgChartOpt);
  Application.Run;
end.
