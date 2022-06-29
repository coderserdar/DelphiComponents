unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EanPDF417, StdCtrls, EanKod, QuickRpt, EanQR, ExtCtrls, Buttons, Qrctrls;

type
  TForm1 = class(TForm)
    QR: TQuickRep;
    QRBand1: TQRBand;
    QRBand2: TQRBand;
    BC: TQrEan;
    BC2: TQrEan;
    BC3: TQrEan;
    BitBtn1: TBitBtn;
    QRShape1: TQRShape;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
    QRLabel5: TQRLabel;
    QRShape2: TQRShape;
    QRLabel6: TQRLabel;
    QrEan1: TQrEan;
    QRLabel7: TQRLabel;
    QrEan2: TQrEan;
    QRLabel8: TQRLabel;
    QrEan3: TQrEan;
    QRLabel9: TQRLabel;
    QrEan4: TQrEan;
    QRLabel10: TQRLabel;
    QrEan5: TQrEan;
    QRLabel11: TQRLabel;
    QrEan6: TQrEan;
    QRLabel12: TQRLabel;
    QrEan7: TQrEan;
    QRLabel13: TQRLabel;
    QrEan8: TQrEan;
    QRLabel14: TQRLabel;
    QrEan9: TQrEan;
    QRLabel15: TQRLabel;
    QRLabel16: TQRLabel;
    QrEan10: TQrEan;
    QRLabel17: TQRLabel;
    QRShape3: TQRShape;
    QRLabel18: TQRLabel;
    QRLabel19: TQRLabel;
    QrEan11: TQrEan;
    QrEan12: TQrEan;
    QRLabel20: TQRLabel;
    QrEan13: TQrEan;
    QRLabel21: TQRLabel;
    QrEan14: TQrEan;
    QRLabel22: TQRLabel;
    QRBand3: TQRBand;
    QRLabel23: TQRLabel;
    QRShape4: TQRShape;
    QRLabel24: TQRLabel;
    QrEan15: TQrEan;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
        QR.Print;
end;


procedure TForm1.FormCreate(Sender: TObject);
var s:String;
    i:Integer;
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
end;

















end.


{
PSOFT, http://www.psoft.sk, email: peter@psoft.sk
Products :
- Barcode library (Ean, UPC, ITF, Ean128, Code39, Code93, PDF417 )
- Eval library
- FormEdit library
- Labels for Windows


