unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_linechart, StdCtrls, ExtCtrls, API_grbutton,
  API_math, API_edit, API_base;

type
  TForm1 = class(TForm)
    hor: TAPI_math;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    API_linechart1: TAPI_linechart;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    API_grbutton7: TAPI_grbutton;
    API_grbutton8: TAPI_grbutton;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    API_grbutton9: TAPI_grbutton;
    API_grbutton10: TAPI_grbutton;
    Bevel1: TBevel;
    API_grbutton11: TAPI_grbutton;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    API_edit3: TAPI_edit;
    API_edit4: TAPI_edit;
    API_edit5: TAPI_edit;
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure API_grbutton6Click(Sender: TObject);
    procedure API_grbutton7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure API_grbutton8Click(Sender: TObject);
    procedure API_grbutton9Click(Sender: TObject);
    procedure API_grbutton10Click(Sender: TObject);
    procedure API_grbutton11Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure updatevisibleitems;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  hor.serie_clear;
  updatevisibleitems;
end;

procedure TForm1.updatevisibleitems;
begin
  label5.caption:=floattostr(hor.serie_count);
  label6.caption:=floattostr(hor.serie_max);
  label7.caption:=floattostr(hor.serie_average);
  label8.caption:=floattostr(hor.serie_min);
  if hor.serie_count>0 then
  begin
    label11.caption:=floattostr(hor.serie_asfloat(0));
    label12.caption:=floattostr(hor.serie_asfloat(hor.serie_count-1));
  end else
  begin
    label11.caption:='.';
    label12.caption:='.';
  end;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  hor.serie_add(api_edit1.asFloat);
  updatevisibleitems;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  hor.serie_move(api_edit2.asFloat);
  updatevisibleitems;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  hor.serie_multiply(api_edit2.asFloat);
  updatevisibleitems;
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
var
  i: integer;
begin
  api_linechart1.clear(0);
  api_linechart1.HistoryCount:=hor.serie_count;
  for i:=0 to hor.serie_count-1 do
  begin
    api_linechart1.add(0,hor.serie_asfloat(i));
  end;
end;

procedure TForm1.API_grbutton6Click(Sender: TObject);
begin
  hor.serie_linearize(hor.serie_asfloat(0),hor.serie_asfloat(hor.serie_count-1));
  updatevisibleitems;
end;

procedure TForm1.API_grbutton7Click(Sender: TObject);
var
  i: integer;
  d1, d2: double;
begin
  for i:=0 to api_edit3.asInteger-1 do
  begin
    repeat
      d1:=random(100)-50;
      d2:=random(20);
    until (d1<>0) and (d2<>0);
    hor.serie_add(d1/d2);
  end;
  updatevisibleitems;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  randomize;
end;

procedure TForm1.API_grbutton8Click(Sender: TObject);
var
  d: double;
begin
  if hor.serie_count>0 then
  begin
    d:=hor.serie_asfloat(0);
    hor.serie_move((-1)*d);
    updatevisibleitems;
  end;
end;

procedure TForm1.API_grbutton9Click(Sender: TObject);
var
  start: integer;
  stop: integer;
begin
  hor.serie_findends(start, stop, 5, 1);
  label15.caption:=inttostr(start);
  label16.caption:=inttostr(stop);
end;

procedure TForm1.API_grbutton10Click(Sender: TObject);
begin
  hor.serie_delarea(api_edit4.asInteger, api_edit5.asInteger);
  updatevisibleitems;
end;

procedure TForm1.API_grbutton11Click(Sender: TObject);
begin
  hor.serie_medianfilter;
  updatevisibleitems;
end;

end.
