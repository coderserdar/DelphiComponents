unit ChartOptions;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Chart, StdCtrls, ExtCtrls, teEngine;

type
  TfrmChartOptions = class(TFrame)
    pgChart: TPageControl;
    tabLegend: TTabSheet;
    chkLegendVisible: TCheckBox;
    rgLegenPosition: TRadioGroup;
    FontDialog: TFontDialog;
    btnLegendFont: TButton;
    chkLegendResize: TCheckBox;
    chkLegendInverted: TCheckBox;
    udLegendMargin: TUpDown;
    edLegendMargin: TEdit;
    Label1: TLabel;
    btnLegendBackColor: TButton;
    ColorDialog: TColorDialog;
    tabChart: TTabSheet;
    cbSeries: TComboBox;
    PageControl1: TPageControl;
    tabBar: TTabSheet;
    cbBarStyle: TComboBox;
    procedure chkLegendVisibleClick(Sender: TObject);
    procedure rgLegenPositionClick(Sender: TObject);
    procedure btnLegendFontClick(Sender: TObject);
    procedure chkLegendResizeClick(Sender: TObject);
    procedure chkLegendInvertedClick(Sender: TObject);
    procedure udLegendMarginClick(Sender: TObject; Button: TUDBtnType);
    procedure btnLegendBackColorClick(Sender: TObject);
    procedure pgChartChange(Sender: TObject);
    procedure cbSeriesChange(Sender: TObject);
    procedure cbBarStyleChange(Sender: TObject);
  private
    { Private declarations }
    FSeries:TChartSeries;
    FChart:TCustomChart;
    procedure InitChartPage;
    procedure InitLegendPage;
  public
    { Public declarations }
    property Chart:TCustomChart read FChart write FChart;
  end;

implementation

uses Series;

{$R *.dfm}

{ TfrmChartOptions }

procedure TfrmChartOptions.chkLegendVisibleClick(Sender: TObject);
begin
  with Sender as TCheckBox do
    Chart.Legend.Visible:=Checked;
end;

procedure TfrmChartOptions.rgLegenPositionClick(Sender: TObject);
begin
  with Sender as TRadioGroup do
    Chart.Legend.Alignment:=TLegendAlignment(ItemIndex);
end;

procedure TfrmChartOptions.btnLegendFontClick(Sender: TObject);
begin
  FontDialog.Font:=Chart.Legend.Font;
  if FontDialog.Execute then
    Chart.Legend.Font:=FontDialog.Font;
end;

procedure TfrmChartOptions.chkLegendResizeClick(Sender: TObject);
begin
  with Sender as TCheckBox do
    Chart.Legend.ResizeChart:=Checked;
end;

procedure TfrmChartOptions.chkLegendInvertedClick(Sender: TObject);
begin
  with Sender as TCheckBox do
    Chart.Legend.Inverted:=Checked;
end;

procedure TfrmChartOptions.udLegendMarginClick(Sender: TObject;
  Button: TUDBtnType);
begin
  with Sender as TUpDown do
    case Chart.Legend.Alignment of
    laLeft,laRight:
      Chart.Legend.HorizMargin:=Position;
    laTop,laBottom:
      Chart.Legend.VertMargin:=Position;
    end;
end;

procedure TfrmChartOptions.btnLegendBackColorClick(Sender: TObject);
begin
  ColorDialog.Color:=Chart.Legend.Color;
  if ColorDialog.Execute then
    Chart.Legend.Color:=ColorDialog.Color;
end;

procedure TfrmChartOptions.pgChartChange(Sender: TObject);
begin
  with Sender as TPageControl do begin
    case ActivePage.PageIndex of
    0: InitLegendPage;
    1: InitChartPage;
    end;
  end;
end;

procedure TfrmChartOptions.InitChartPage;
var
  I:Integer;
begin
  with Chart,cbSeries do begin
    Items.Clear;
    for I:=0 to SeriesCount-1 do
      if tssIsTemplate in Series[I].Style then
        Items.Add(Series[I].Title)
  end;
end;

procedure TfrmChartOptions.InitLegendPage;
begin

end;

procedure TfrmChartOptions.cbSeriesChange(Sender: TObject);
begin
  with Chart,cbSeries do
    FSeries:=Series[ItemIndex];
end;

procedure TfrmChartOptions.cbBarStyleChange(Sender: TObject);
begin
  with cbSeries do
    TBarSeries(FSeries).BarStyle:=TBarStyle(ItemIndex);
end;

end.
