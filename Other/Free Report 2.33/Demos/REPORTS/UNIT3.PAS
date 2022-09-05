unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_DBSet, FR_Class;

type
  TForm3 = class(TForm)
    frReport1: TfrReport;
    frReport3: TfrReport;
    frReport2: TfrReport;
    CustomersDataSet: TFRDBDataSet;
    OrdersDataSet: TFRDBDataSet;
    ItemsDataSet: TFRDBDataSet;
    frReport4: TfrReport;
    frReport5: TfrReport;
    GroupDataset: TfrDBDataSet;
    frReport6: TfrReport;
    procedure frReport1UserFunction(const name: String; p1, p2,
      p3: Variant; var val: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses datasets, FR_Pars;

{$R *.DFM}

procedure TForm3.frReport1UserFunction(const name: String; p1, p2,
  p3: Variant; var val: String);
begin
  if name = 'WRITESUM' then
    val := '''' + 'А это сумма прописью для числа ' + FloatToStr(frParser.Calc(p1)) + '''';
end;

end.
