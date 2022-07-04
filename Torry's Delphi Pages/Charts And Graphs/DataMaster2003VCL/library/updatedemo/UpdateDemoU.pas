///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit UpdateDemoU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, DMContainer, DMPlot, Grids, DMWorksheet,
  StdCtrls;

type
  TForm1 = class(TForm)
    Plot1: TPlot;
    Container1: TContainer;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    Worksheet1: TWorksheet;
    Splitter1: TSplitter;
    CheckBox1: TCheckBox;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FormatArray: TFormatArray;
  FirstTick: cardinal=0;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
var
  D: TRealData;
  R: TRealArray;
  Shift: TReal;
begin
  if SpeedButton1.Down then
  begin
    if FirstTick=0
    then FirstTick:=GetTickCount;         // start count time from zero
    R[1]:=(GetTickCount-FirstTick)/1000;  // scale time
    R[2]:=(sin(R[1])+sin(R[1]*1.2))/2;
    D:=TRealData.Create;      // create and initialize new data item
    D.Format:=@FormatArray;
    D.SetRData(2, R);
    Container1.Items.Add(D);  // add item into container
    Worksheet1.UpdateSize;    // update worksheet size
    if CheckBox1.Checked then // scroll worksheet to display new data line
      if Worksheet1.VisibleRowCount>2 
      then Worksheet1.TopRow:=Worksheet1.RowCount-Worksheet1.VisibleRowCount;
    Plot1.ThisSerie.AddPoint; // display new plot point (all plot not repainted!)
    if R[1]>Plot1.XAxis.Max then
    begin // check X scale and move it to display new plot points
      Shift:=(Plot1.XAxis.Max-Plot1.XAxis.Min)/Plot1.XAxis.MajorTicks*2;
      Plot1.XAxis.Max:=Plot1.XAxis.Max+Shift;
      Plot1.XAxis.Min:=Plot1.XAxis.Min+Shift;
    end;
  end;
end;

begin
  FormatArray[1].FType:=ffGeneral;  // initialize numeric format for columns
  FormatArray[1].Width:=7;
  FormatArray[1].Decimals:=4;
  FormatArray[2].FType:=ffGeneral;
  FormatArray[2].Width:=7;
  FormatArray[2].Decimals:=4;
end.
