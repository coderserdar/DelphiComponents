unit Unit1;

// API_chart example 1
//
// example shows the basic functions of API_chart component,
// one nice thing to test is clicking on bar graph -> you can
// show exact bar graph number or maybe enter into detailed
// view of that bar position's contents =). and also note that
// there is no limitation of data points added - it will slow
// down at some amount of datapoints, but performs quite fast
// no matter what.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_chart, StdCtrls, API_base;

type
  TForm1 = class(TForm)
    API_chart1: TAPI_chart;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure API_chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  y: boolean;
begin
  api_chart1.ChartType:= ctbar;
  y:=false;
  for i:=0 to 10 do
    if y then
    begin
      api_chart1.Add('testi',i*0.82,random(255),clred,'m','kg');
      y:= false;
    end else
    begin
      api_chart1.Add('testi',i*0.82,random(255),clgreen,'m','kg');
      y:= true;
    end;
  label1.Caption:= 'Count: '+inttostr( api_chart1.Values );
  label2.caption:= 'YMin: '+floattostr( api_chart1.getymin );
  label3.caption:= 'YMax: '+floattostr( api_chart1.getYMax );
  label4.caption:= 'YAvg: '+formatfloat( '0.00', api_chart1.getYAvg );
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if checkbox1.Checked then
    api_chart1.ChartType:= ctline else
    api_chart1.charttype:= ctbar;
  api_chart1.Invalidate;
end;

procedure TForm1.API_chart1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if api_chart1.ChartType = ctbar then
    showmessage(inttostr(api_chart1.DataPos)+' = '+floattostr(api_chart1.YValue(api_chart1.datapos)));
end;

end.
