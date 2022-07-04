unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ppCtrls, EanRBDB, ppPrnabl, ppClass, EanRB, ppDB, ppBands, Db,
  DBTables, ppCache, ppDBPipe, ppComm, ppRelatv, ppProd, ppReport;

type
  TForm2 = class(TForm)
    RPT: TppReport;
    ppDBPipeline1: TppDBPipeline;
    DataSource1: TDataSource;
    TBL: TTable;
    ppHeaderBand1: TppHeaderBand;
    ppDetailBand1: TppDetailBand;
    ppFooterBand1: TppFooterBand;
    RBEan1: TRBEan;
    RBDBEan1: TRBDBEan;
    ppLabel1: TppLabel;
    ppLabel2: TppLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure RBEan1BeforePrint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.Button1Click(Sender: TObject);
begin
     RPT.Print;
end;

procedure TForm2.RBEan1BeforePrint(Sender: TObject);
begin
     RBEan1.Barcode := TBL.FieldByName('NAME').AsString;
end;

end.

