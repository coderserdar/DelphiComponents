// FastReport 2.3 demo
//
// Chart demo. Requires TeeChart component.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FR_Class, FR_DSet, FR_DBSet, Db, DBTables, FR_Chart, FR_Desgn;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    Button1: TButton;
    frChartObject1: TfrChartObject;
    frDesigner1: TfrDesigner;
    Table1: TTable;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  frReport1.DesignReport;
end;

end.
