unit TrendTwinDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JBKTrend, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Trend3: TJBKTrend;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Trend1: TJBKTrend;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Trend2: TJBKTrend;
    Bevel5: TBevel;
    Bevel6: TBevel;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var
  Angle: Extended;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Trend3.Add(Round(100 * Sin(Angle * Pi / 90)));
  Trend3.AddBase(Round((100 * Sin(Angle * Pi / 90)) + (20 * Sin(Angle * Pi / 11.25))));
  Trend1.Add(Trend3.Get(Trend3.Divisions - 2) - Trend3.Get(Trend3.Divisions - 1));
  Trend1.AddBase(Trend3.Get(Trend3.Divisions - 1));
  Trend2.Demo;
  Angle := Angle + 5;
end;

end.

 