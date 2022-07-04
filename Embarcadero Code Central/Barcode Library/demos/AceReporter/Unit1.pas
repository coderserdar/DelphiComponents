unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SctCtrl, EanAce, SctVar, ExtCtrls, SctRep, StdCtrls, Buttons, Db,
  DBTables, EanAceDB;

type
  TForm1 = class(TForm)
    SCT: TSctReport;
    ReportPage: TSctGrouppage;
    ReportHeaderBand: TSctBand;
    ReportHeaderBandlevel: TSctLevel;
    PageHeaderBand: TSctBand;
    PageHeaderBandlevel: TSctLevel;
    DetailBand: TSctBand;
    DetailBandlevel: TSctLevel;
    PageFooterBand: TSctBand;
    PageFooterBandlevel: TSctLevel;
    ReportFooterBand: TSctBand;
    ReportFooterBandlevel: TSctLevel;
    Panel1: TPanel;
    SctShape2: TSctShape;
    Sctvarlabel2: TSctvarlabel;
    Sctvarlabel3: TSctvarlabel;
    BitBtn1: TBitBtn;
    AceDBEan1: TAceDBEan;
    TBL: TTable;
    DataSource1: TDataSource;
    SctShape1: TSctShape;
    Sctvarlabel1: TSctvarlabel;
    SctImageLabel1: TSctImageLabel;
    AceEan1: TAceEan;
    AceEan2: TAceEan;
    AceEan3: TAceEan;
    AceEan4: TAceEan;
    AceEan5: TAceEan;
    AceEan6: TAceEan;
    AceEan7: TAceEan;
    AceEan8: TAceEan;
    AceEan9: TAceEan;
    AceEan10: TAceEan;
    AceEan11: TAceEan;
    AceEan12: TAceEan;
    AceEan13: TAceEan;
    AceEan14: TAceEan;
    AceEan15: TAceEan;
    AceEan16: TAceEan;
    AceEan20: TAceEan;
    AceEan19: TAceEan;
    AceEan18: TAceEan;
    AceEan17: TAceEan;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}





procedure TForm1.BitBtn1Click(Sender: TObject);
begin
     SCT.Run;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
     TBL.TableName := ExtractFilePath(Application.Exename)+'DemoData.dbf';
     TBL.Open;
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
begin
     Application.Terminate;
end;

end.
