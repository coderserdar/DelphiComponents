unit Unit3;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QPrinters,
  QStdCtrls, RLReport, DBXpress, FMTBcd, DB, SqlExpr, RLFilters;

type
  TForm3 = class(TForm)
    RLReport1: TRLReport;
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
  Form3: TForm3;

implementation

{$R *.xfm}

procedure TForm3.RLReport1BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  Tag:=0;
end;

procedure TForm3.RLReport1NeedData(Sender: TObject; var MoreData: Boolean);
begin
  Tag     :=Tag+1;
  MoreData:=(Tag<=10);
end;

end.

