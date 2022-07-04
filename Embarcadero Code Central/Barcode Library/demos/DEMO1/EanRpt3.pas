unit EanRpt3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, quickrpt, EanQr, Qrctrls;

type
  TRpt3 = class(TForm)
    QR3: TQuickRep;
    QRBand1: TQRBand;
    QRSubDetail1: TQRSubDetail;
    L1: TQRLabel;
    L2: TQRLabel;
    QRLabel1: TQRLabel;
    E1: TQrEan;
    procedure QR3BeforePrint(Sender: TQuickRep; var PrintReport: Boolean);
    procedure QRSubDetail1NeedData(Sender: TObject; var MoreData: Boolean);
  private
    count : Integer;     
  public
    { Public declarations }
  end;

var
  Rpt3: TRpt3;

implementation

{$R *.DFM}

procedure TRpt3.QR3BeforePrint(Sender: TQuickRep;
  var PrintReport: Boolean);
begin
     count := 0;
     PrintReport := True;
end;

procedure TRpt3.QRSubDetail1NeedData(Sender: TObject;
  var MoreData: Boolean);
var i:Integer;
    s:String;
begin
     Inc(Count);
     MoreData := Count<8;

     s:='';
     for i:=0 to Count do
         s:=s+Char(i+Ord('A'));
     E1.Barcode:=s;

     L1.Caption := Format('Number of chars        : %d',[Length(s)]);
     L2.Caption := Format('Length of barcode (mm) : %5.2f',[Length(s)*E1.CharWidthMM]);
end;

end.
