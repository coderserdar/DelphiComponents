unit TrendDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JBKTrend,
  ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    RadioGroup1: TRadioGroup;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    Trend1: TJBKTrend;
    Trend2: TJBKTrend;
    Trend3: TJBKTrend;
    Trend4: TJBKTrend;
    Trend5: TJBKTrend;
    Trend6: TJBKTrend;
    Trend7: TJBKTrend;
    CheckBox1: TCheckBox;
    procedure Timer1Timer(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Demo;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Style := tsLine;

end;

procedure TForm1.RadioButton2Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Style := tsScatter;
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Style := ts3D;
end;

procedure TForm1.RadioButton4Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Style := tsBar;
end;

procedure TForm1.RadioButton5Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        Style := tsFilled;
end;

{-----------------------------------------------------------------------------
  Procedure: TForm1.FormCreate
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      10-Mai-2002
  Arguments: Sender: TObject
  Result:    None
  Purpose:   Random seed
-----------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
end;

{-----------------------------------------------------------------------------
  Procedure: TForm1.CheckBox1Click
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      10-Mai-2002
  Arguments: Sender: TObject
  Result:    None
  Purpose:   Set Twin Option
-----------------------------------------------------------------------------}

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJBKTrend then
      with Components[i] as TJBKTrend do
        if Checkbox1.Checked then
          Options := Options + [toTwin]
        else
          Options := Options - [toTwin];
end;

end.

