unit EanRpt2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EanQr, quickrpt, Qrctrls, ExtCtrls, EanKod, EanSpecs;

type
  TRpt2 = class(TForm)
    QR2: TQuickRep;
    QRBand1: TQRBand;
    QRLabel1: TQRLabel;
    QRMemo2: TQRMemo;
    QRSubDetail1: TQRSubDetail;
    QR_BarcodeName: TQRLabel;
    E1: TQrEan;
    QRText: TQRMemo;
    QRShape1: TQRShape;
    E2: TQrEan;
    E3: TQrEan;
    L2: TQRLabel;
    QRBand2: TQRBand;
    QRSysData1: TQRSysData;
    QRLabel2: TQRLabel;
    procedure QR2NeedData(Sender: TObject; var MoreData: Boolean);
    procedure QR2BeforePrint(Sender: TQuickRep; var PrintReport: Boolean);
  private
    SamplesCount : Integer;
  public
    { Public declarations }
  end;

var
  Rpt2: TRpt2;

implementation

{$R *.DFM}


procedure TRpt2.QR2NeedData(Sender: TObject; var MoreData: Boolean);
var s:String;
    n:Boolean;
begin
  if SamplesCount<=Integer(High(TTypBarCode)) then
     E1.TypBarCode   := TTypBarCode(SamplesCount);

  E1.Caption.UpdateCaption;
  E1.CharWidthMM  := E1.EAN.GetBarcodeInfo.OptCharWidthMM;

  E2.TypBarCode   := E1.TypBarCode;
  E2.Caption.UpdateCaption;
  E2.CharWidthMM  := E1.CharWidthMM;

  E3.TypBarCode   := E1.TypBarCode;
  E3.Caption.UpdateCaption;
  E3.CharWidthMM  := E1.CharWidthMM;

  if E1.Ean.GetBarcodeInfo.EnabledAdd then begin
     E2.Barcode := E1.Barcode+' 12';
     E2.Caption.UpdateCaption;
     E3.Barcode := E1.Barcode+' 12345';
     E3.Caption.UpdateCaption;
  end else begin
     E2.Barcode := '';
     E3.Barcode := '';
  end;

  if E1.TypBarcode=bcPostnet then begin
     E1.Height :=  20;
     E2.Height :=  20;
     E3.Height :=  20;

     E1.BarCode := '12345';
     E2.BarCode := '123456789';
     E3.BarCode := '12345678901';
  end else begin
     E1.Height := 120;
     E2.Height := 120;
     E3.Height := 120;
  end;

  QRText.Lines.Clear;
  s:=E1.EAN.GetBarcodeInfo.LongName;
  if s<>'' then QRText.Lines.Add(s)
  else          QRText.Lines.Add(E1.EAN.GetbarcodeInfo.Name);
  QRText.Lines.Add('');
  QRText.Lines.Add('Charset :');
  L2.Caption := E1.EAN.GetSetOfCharsVisible;


  QR_BarcodeName.Caption        := E1.EAN.GetbarcodeInfo.Name;
  MoreData:= SamplesCount <=Integer(High(TTypBarCode));
  Inc(SamplesCount);
end;

procedure TRpt2.QR2BeforePrint(Sender: TQuickRep;
  var PrintReport: Boolean);
begin
     SamplesCount    := 0;
     E1.TypBarCode   := TTypBarCode(0);
end;

end.
