unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SctCtrl, EanAce, SctVar, ExtCtrls, SctRep, EanPDF417, StdCtrls, Buttons;

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
    BC: TAceEan;
    SctShape1: TSctShape;
    Sctvarlabel1: TSctvarlabel;
    Sctvarlabel2: TSctvarlabel;
    BC2: TAceEan;
    BC3: TAceEan;
    Sctvarlabel3: TSctvarlabel;
    Sctvarlabel4: TSctvarlabel;
    Sctvarlabel5: TSctvarlabel;
    SctLine1: TSctLine;
    Sctvarlabel6: TSctvarlabel;
    Sctvarlabel7: TSctvarlabel;
    AceEan1: TAceEan;
    AceEan2: TAceEan;
    Sctvarlabel8: TSctvarlabel;
    AceEan3: TAceEan;
    Sctvarlabel9: TSctvarlabel;
    Sctvarlabel10: TSctvarlabel;
    AceEan4: TAceEan;
    AceEan5: TAceEan;
    Sctvarlabel11: TSctvarlabel;
    AceEan6: TAceEan;
    Sctvarlabel12: TSctvarlabel;
    Sctvarlabel13: TSctvarlabel;
    AceEan7: TAceEan;
    AceEan8: TAceEan;
    Sctvarlabel14: TSctvarlabel;
    AceEan9: TAceEan;
    Sctvarlabel15: TSctvarlabel;
    BitBtn1: TBitBtn;
    svarDateTime: TSctDateTimeVar;
    svarPage: TSctPageVar;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}



procedure TForm1.FormCreate(Sender: TObject);
var s:String;
    i:Integer;
    E:TAceEan;
begin
    for i:=1 to 9 do begin
        E:=TAceEan(FindComponent('AceEan'+IntToStr(i)));
        if E<>nil then
           E.PDF417.SecurityLevel := psPDF417ErrorCorrection(i);
    end;

        BC.Barcode:='PSOFT, http://www.psoft.sk, email: peter@psoft.sk'#13
                   +'Products :'#13
                   +'- Barcode library (Ean, UPC, ITF, Ean128, Code39, Code93, PDF417 )'#13
                   +'- Eval library  http://eval.psoft.sk'#13
                   +'- FormEdit library http://formedit.psoft.sk'#13
                   +'- Labels for Windows http://labels.psoft.sk';
        s:='';
        BC2.PDF417.Mode := psPDF417Numeric;
        for i:=1 to 50 do
            s:=s+'0123456789';
        BC2.BarCode := s;

        s:='';
        BC3.PDF417.Mode := psPDF417Binary;
        for i:=48 to 127 do
            s:=s+Char(i);
        BC3.BarCode:= s;
end;




procedure TForm1.BitBtn1Click(Sender: TObject);
begin
     SCT.Run;
end;

end.
