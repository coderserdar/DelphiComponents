unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EanRB, ppCtrls, ppPrnabl, ppClass, ppBands, ppCache, ppComm, ppRelatv,
  EanPDF417, ppProd, ppReport, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    RPT: TppReport;
    ppHeaderBand1: TppHeaderBand;
    ppDetailBand1: TppDetailBand;
    ppFooterBand1: TppFooterBand;
    ppShape1: TppShape;
    ppLabel1: TppLabel;
    ppLabel2: TppLabel;
    BC: TRBEan;
    BitBtn1: TBitBtn;
    ppLabel3: TppLabel;
    ppLabel4: TppLabel;
    BC2: TRBEan;
    ppLabel5: TppLabel;
    BC3: TRBEan;
    ppShape2: TppShape;
    ppLabel6: TppLabel;
    ppLabel7: TppLabel;
    RBEan1: TRBEan;
    RBEan2: TRBEan;
    ppLabel8: TppLabel;
    RBEan3: TRBEan;
    ppLabel9: TppLabel;
    ppLabel10: TppLabel;
    RBEan4: TRBEan;
    RBEan5: TRBEan;
    ppLabel11: TppLabel;
    RBEan6: TRBEan;
    ppLabel12: TppLabel;
    ppLabel13: TppLabel;
    RBEan7: TRBEan;
    RBEan8: TRBEan;
    ppLabel14: TppLabel;
    RBEan9: TRBEan;
    ppLabel15: TppLabel;
    ppLabel16: TppLabel;
    RBEan10: TRBEan;
    ppShape3: TppShape;
    ppLine1: TppLine;
    ppLabel17: TppLabel;
    ppShape4: TppShape;
    ppLabel18: TppLabel;
    ppLabel19: TppLabel;
    RBEan11: TRBEan;
    RBEan12: TRBEan;
    ppLabel20: TppLabel;
    RBEan13: TRBEan;
    ppLabel21: TppLabel;
    ppLabel22: TppLabel;
    ppShape5: TppShape;
    RBEan14: TRBEan;
    ppLabel23: TppLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure RPTBeforePrint(Sender: TObject);
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
     RPT.Print;
end;

procedure TForm1.RPTBeforePrint(Sender: TObject);
var i:Integer;
    s:String;
    E:TRBEan;
begin
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

    for i:=1 to 9 do begin
        E:=TRBEan(FindComponent('RBEan'+IntToStr(i)));
        if E<>nil then
           E.PDF417.SecurityLevel := psPDF417ErrorCorrection(i);
    end;

    RBEan10.PDF417.SecurityLevel := psPDF417Error6;

    RBEan11.PDF417.Cols := 1;
    RBEan12.PDF417.Cols := 3;
    RBEan13.PDF417.Cols := 5;
    RBEan13.PDF417.Cols := 10;

end;

end.
