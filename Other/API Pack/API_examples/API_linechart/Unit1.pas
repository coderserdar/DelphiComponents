unit Unit1;

//------------------------------------------------------------------------------
// exmple of how to use api_linechart component

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, API_linechart;

type
  TForm1 = class(TForm)
    tAPI_linechart1: tAPI_linechart;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  count: integer;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TForm1.Button1Click(Sender: TObject);
var
  values: array[0..3] of double;
begin
  count:=count+1;                               // increase counter
  if count>20 then count:=-20;                  // check for overflow

  // adding values - method 1
  {
    tapi_linechart1.add( 0, count );
    tapi_linechart1.add( 1, random(40)-19 );
  }

  // adding values - method 2
  values[0]:= count;
  values[1]:= random(40)-19;
  values[2]:= random(40)-19;
  values[3]:= random(40)-19;
  tapi_linechart1.Add(values);

  // show minimum, maximum and average of the history
  label1.Caption:=floattostr(tapi_linechart1.Max);
  label2.caption:=floattostr(tapi_linechart1.Average);
  label3.caption:=floattostr(tapi_linechart1.Min);
end;

//------------------------------------------------------------------------------
procedure TForm1.FormCreate(Sender: TObject);
begin
  randomize;                                    // init random generator
  count:=-20;                                   // initialize counter

  tapi_linechart1.HistoryCount:= 25;            // show values up to 25 steps
  tapi_linechart1.Lines:= 4;                    // set number of lines to draw
  tapi_linechart1.linecolor(0, clred);          // 1st line color to red
  tapi_linechart1.linecolor(1, clgreen);
  tapi_linechart1.linecolor(2, clwhite);
  tapi_linechart1.linecolor(3, clyellow);
end;

end.
