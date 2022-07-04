unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NiceChart, StdCtrls, XPMan;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
    Chart: TNiceChart;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Math;

procedure TForm1.ChartMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ax, ay: Double;  
begin
  if Chart.ClientToChart(X, Y, ax, ay)
    then Label1.Caption := FormatFloat('0.##', ax) + ', ' + FormatFloat('0.##', ay / 1000)
    else Label1.Caption := 'Out of Chart';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Series: TNiceSeries;
begin
  Chart := TNiceChart.Create(Self);
  with Chart do
  begin
    Parent := Self;
    Left := 8;
    Top := 40;
    Width := Self.ClientWidth - 16;
    Height := Self.ClientHeight - 48;
    Anchors := [akLeft, akTop, akRight, akBottom];
    Title := 'Look at me!'#13'I''m a NiceChart!';
    AxisXOnePerValue := True;
    ShowXGrid := False;
    AxisYScale := 1000;
    //Monochrome := True;
    OnMouseMove := ChartMouseMove;
  end;
  Chart.BeginUpdate;
  Series := Chart.AddSeries(skBar);
  Series.Caption := 'Revenue';
  with Series do
  begin
    AddXY(   0,        0);
    AddXY(2000,  4904620);
    AddXY(2001, 15086420);
    AddXY(2002, 14696560);
    AddXY(2003, 14263160);
    AddXY(2004, 13711080);
    AddXY(2005, 13018880);
    AddXY(2006, 11901080);
    AddXY(2007, 10592340);
    AddXY(2008,  9750620);
    AddXY(2009,  9007960);
    AddXY(2010,  7980940);
    AddXY(2011,  6164060);
    AddXY(2012,  4182760);
    AddXY(2013,  3026420);
    AddXY(2014,  1907200);
  end;
  Series := Chart.AddSeries(skBar);
  Series.Caption := 'Cash Flow';
  with Series do
  begin
    AddXY(   0,     -9435276);
    AddXY(2000, -12884809.79);
    AddXY(2001,   11849628.6);
    AddXY(2002,   8676685.24);
    AddXY(2003,   3856984.44);
    AddXY(2004,   3811447.44);
    AddXY(2005,     686684.1);
    AddXY(2006,    377060.42);
    AddXY(2007,    831488.06);
    AddXY(2008,   1517731.99);
    AddXY(2009,   1431460.27);
    AddXY(2010,   1158463.54);
    AddXY(2011,    737914.59);
    AddXY(2012,    386103.52);
    AddXY(2013,    249675.86);
    AddXY(2014,       119973);
  end;
  Series := Chart.AddSeries(skSmooth);
  Series.Caption := 'Cum. Cash Flow';
  with Series do
  begin
    AddXY(   0,     -9435276);
    AddXY(2000, -22320085.79);
    AddXY(2001, -10470457.19);
    AddXY(2002,  -1793771.95);
    AddXY(2003,   2063212.49);
    AddXY(2004,   5874659.93);
    AddXY(2005,   6561344.03);
    AddXY(2006,   6938404.46);
    AddXY(2007,   7769892.52);
    AddXY(2008,   9287624.51);
    AddXY(2009,  10719084.78);
    AddXY(2010,  11877548.32);
    AddXY(2011,  12615462.91);
    AddXY(2012,  13001566.43);
    AddXY(2013,  13251242.29);
    AddXY(2014,  13371215.29);
  end;
  Chart.EndUpdate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Chart.Free;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  Chart.Monochrome := not Chart.Monochrome;
  if Chart.Monochrome
    then Button1.Caption := 'Color'
    else Button1.Caption := 'Monochrome';
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  m: TMetafile;
begin
  m := Chart.CreateMetafile;
  m.Enhanced := True;
  m.SaveToFile('test.emf');
  m.Free;
  ShowMessage('Saved to "test.wmf".');
end;

end.
