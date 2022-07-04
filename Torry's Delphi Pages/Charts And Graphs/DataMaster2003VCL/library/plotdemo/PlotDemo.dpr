///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

program PlotDemo;

uses
  Forms,
  PlotDemoU in 'PlotDemoU.pas' {PlotDemoForm},
  AxisDlg in '..\AxisDlg.pas' {AxisPropsForm},
  TextDlg in '..\TextDlg.pas' {TextForm},
  SerieDlg in '..\SerieDlg.pas' {SeriePropsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Plot Demo';
  Application.CreateForm(TPlotDemoForm, PlotDemoForm);
  Application.Run;
end.
