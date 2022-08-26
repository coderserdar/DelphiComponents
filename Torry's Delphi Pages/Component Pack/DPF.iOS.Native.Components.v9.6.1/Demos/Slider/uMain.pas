unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, DPF.iOS.BaseControl,
  DPF.iOS.UISlider, DPF.iOS.UILabel, DPF.iOS.UIView;

type
  TForm3 = class( TForm )
    DPFSlider1: TDPFSlider;
    DPFLabel1: TDPFLabel;
    DPFSlider2: TDPFSlider;
    DPFSlider3: TDPFSlider;
    DPFUIView1: TDPFUIView;
    procedure DPFSlider1Changed( Sender: TObject; CurValue: Single );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.DPFSlider1Changed( Sender: TObject; CurValue: Single );
begin
  DPFLabel1.Text := FloatToStr( CurValue );
end;

procedure TForm3.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
