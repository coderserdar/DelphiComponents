unit ChartDlg;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, ChartOptions, Chart;

type
  TdlgChartOpt = class(TForm)
    OKBtn: TButton;
    frmOptions: TfrmChartOptions;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Execute(AChart:TCustomChart);
  end;

var
  dlgChartOpt: TdlgChartOpt;

implementation

{$R *.dfm}

{ TOKBottomDlg }

procedure TdlgChartOpt.Execute(AChart:TCustomChart);
begin
  frmOptions.Chart:=AChart;
  ShowModal;
end;

end.
