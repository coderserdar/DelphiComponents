unit Unit2;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QPrinters,
  QStdCtrls, RLReport, DBXpress, FMTBcd, DB, SqlExpr, RLFilters,
  RLHTMLFilter;

type
  TForm2 = class(TForm)
    RLReport1: TRLReport;
    RLHTMLFilter1: TRLHTMLFilter;
    RLBand1: TRLBand;
    RLBand2: TRLBand;
    RLBand3: TRLBand;
    RLLabel1: TRLLabel;
    RLLabel3: TRLLabel;
    RLSystemInfo1: TRLSystemInfo;
    procedure RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure RLReport1BeforePrint(Sender: TObject; var PrintIt: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.xfm}

procedure TForm2.RLReport1BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  Tag:=0;
end;

procedure TForm2.RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  Tag     :=Tag+1;
  MoreData:=(Tag<=10);
end;

end.

