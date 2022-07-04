unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, ExtCtrls, API_linechart, API_labjack, StdCtrls,
  API_base, API_edit;

type
  TForm1 = class(TForm)
    API_linechart1: TAPI_linechart;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    API_edit1: TAPI_edit;
    API_edit2: TAPI_edit;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // set linechart colors
  api_linechart1.setlinecolor(0,clyellow);
  api_linechart1.setlinecolor(1,clred);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // get analog output values
  api_labjack1.ao00:= api_edit1.asFloat;
  api_labjack1.ao01:= api_edit2.asfloat;

  // refresh analog channels
  api_labjack1.refresh_a;

  // display lines
  api_linechart1.add(0, api_labjack1.ai00);
  api_linechart1.add(1, api_labjack1.ai01);
  api_linechart1.add(2, api_labjack1.ai02);
  api_linechart1.add(3, api_labjack1.ai03);
  api_linechart1.add(4, api_labjack1.ai04);
  api_linechart1.add(5, api_labjack1.ai05);
  api_linechart1.add(6, api_labjack1.ai06);
  api_linechart1.add(7, api_labjack1.ai07);

  // some diagnostics on to screen
  label1.caption:=floattostr(api_labjack1.ao00);
  label2.caption:=floattostr(api_labjack1.ao01);
  label3.caption:=api_labjack1.lasterror;
end;

end.
