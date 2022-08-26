unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Common,
  DPF.iOS.BaseControl,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIView;

type
  TFLabels = class( TForm )
    DPFLabel2: TDPFLabel;
    DPFLabel4: TDPFLabel;
    DPFLabel3: TDPFLabel;
    DPFLabel1: TDPFLabel;
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    procedure DPFSlider1Changed( Sender: TObject; CurValue: Single );
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFLabel3Click( Sender: TObject );
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FLabels: TFLabels;

implementation

{$R *.fmx}

procedure TFLabels.DPFButton1Click( Sender: TObject );
begin
  DPFLabel4.Shadow        := not DPFLabel4.Shadow;
  DPFLabel4.ShadowColor   := TAlphaColors.Chartreuse;
  DPFLabel4.ShadowOpacity := 0.9;
end;

procedure TFLabels.DPFLabel3Click( Sender: TObject );
begin
  ShowMessage( 'Clicked!' );
end;

procedure TFLabels.DPFSlider1Changed( Sender: TObject; CurValue: Single );
begin
  DPFLabel1.Text := FloatToStr( CurValue );
end;

procedure TFLabels.PaintRects( const UpdateRects: array of TRectF );
begin
  { }
end;

end.
