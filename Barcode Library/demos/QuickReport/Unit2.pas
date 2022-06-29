unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Qrctrls, quickrpt, ExtCtrls, EanQr, EanSpecs;

type
  TForm2 = class(TForm)
    QR1: TQuickRep;
    QRBand1: TQRBand;
    QRLabel1: TQRLabel;
    QRLabel2: TQRLabel;
    QRShape1: TQRShape;
    QRBand2: TQRBand;
    QrEan1: TQrEan;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
    QL_TYPE: TQRLabel;
    QRBand3: TQRBand;
    QRLabel7: TQRLabel;
    QRLabel8: TQRLabel;
    QL_CHARSET: TQRMemo;
    procedure QR1NeedData(Sender: TObject; var MoreData: Boolean);
    procedure QR1BeforePrint(Sender: TQuickRep; var PrintReport: Boolean);
  private
    PrintedLines : Integer;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}



procedure TForm2.QR1NeedData(Sender: TObject; var MoreData: Boolean);
var BI:TBarCodeInfo;
    s,s1:String;
    i:Integer;
begin
     MoreData:= ( PrintedLines< Integer(High(TTypBarcode)) );

     QrEan1.TypBarCode := TTypBarcode(PrintedLines);
     BI:=BarcodeInfo(QrEan1.TypBarCode);
     QL_Type.Caption    := BI.LongName;

     s:=BI.Chars;

     QL_CharSet.Lines.Text := s1;
     Inc(PrintedLines);
end;

procedure TForm2.QR1BeforePrint(Sender: TQuickRep;
  var PrintReport: Boolean);
begin
     PrintedLines := 0;
end;

end.
