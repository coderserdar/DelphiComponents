// FastReport 2.3 demo
//
// This example demonstrates how to build report manually

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_Class, StdCtrls;

type
  TForm1 = class(TForm)
    frReport1: TfrReport;
    Button1: TButton;
    procedure frReport1ManualBuild(Sender: TfrPage);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.frReport1ManualBuild(Sender: TfrPage);
var
  i, j: Integer;
begin
  Sender.ShowBandByType(btReportTitle);
  for i := 0 to 3 do
  begin
    Sender.ShowBandByName('Band2');
    for j := 0 to 2 do
      Sender.ShowBandByName('Band3');
    if i <> 3 then
      Sender.NewPage;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.ShowReport;
end;

end.
