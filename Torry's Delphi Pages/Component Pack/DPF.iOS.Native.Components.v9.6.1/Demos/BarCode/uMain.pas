unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.BarCodeReader,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel, DPF.iOS.UIView;

type
  TQRCode = class( TForm )
    DPFQRCodeScanner1: TDPFQRCodeScanner;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFButton2: TDPFButton;
    DPFButton1: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFQRCodeScanner1Scan( Sender: TObject; AText: string );
    procedure DPFButton2Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QRCode: TQRCode;

implementation

{$R *.fmx}

procedure TQRCode.DPFButton1Click( Sender: TObject );
begin
  DPFQRCodeScanner1.Start;
end;

procedure TQRCode.DPFButton2Click( Sender: TObject );
begin
  DPFQRCodeScanner1.Stop;
end;

procedure TQRCode.DPFQRCodeScanner1Scan( Sender: TObject; AText: string );
begin
  DPFLabel1.Text := AText;
end;

end.
