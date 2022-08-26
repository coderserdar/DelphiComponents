unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.Dispatch,

  DPF.iOS.BaseControl,
  DPF.iOS.Common,
  DPF.iOS.UILabel,
  DPF.iOS.UIButton,
  DPF.iOS.UIView,
  DPF.iOS.SlideDialog;

type
  TFAlertMessage = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFSlideDialog1: TDPFSlideDialog;
    DPFButton5: TDPFButton;
    DPFUIView2: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFUIView3: TDPFUIView;
    DPFUIView4: TDPFUIView;
    DPFUIView5: TDPFUIView;
    DPFButton3: TDPFButton;
    DPFButton4: TDPFButton;
    DPFButton2: TDPFButton;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFButton2Click( Sender: TObject );
    procedure DPFButton3Click( Sender: TObject );
    procedure DPFButton4Click( Sender: TObject );
    procedure DPFButton5Click( Sender: TObject );
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FAlertMessage: TFAlertMessage;

implementation

{$R *.fmx}

procedure TFAlertMessage.DPFButton1Click( Sender: TObject );
begin
  DPFSlideDialog1.BackgroundImage := TPath.GetDocumentsPath +'/GameImg.png';
  DPFSlideDialog1.TextColor       := TALphaColors.Brown;
  DPFSlideDialog1.Show( 'D.P.F Components!', 310, 200, 0.4 );
end;

procedure TFAlertMessage.DPFButton2Click( Sender: TObject );
begin
  DPFSlideDialog1.BackgroundImage := TPath.GetDocumentsPath +'/ShareonFacebook.png';
  DPFSlideDialog1.Show( 'D.P.F iOS Native Components', 200, 150, 1, 5, sddBottom );
end;

procedure TFAlertMessage.DPFButton3Click( Sender: TObject );
begin
  DPFSlideDialog1.BackgroundImage := TPath.GetDocumentsPath +'/alert_background.png';
  DPFSlideDialog1.TextColor       := TALphaColors.White;
  DPFSlideDialog1.Show( 'D.P.F iOS Native Components', 293, 100, 0.2, 5, sddRight );
end;

procedure TFAlertMessage.DPFButton4Click( Sender: TObject );
begin
  DPFSlideDialog1.BackgroundImage := TPath.GetDocumentsPath +'/alert_background.png';
  DPFSlideDialog1.TextColor       := TALphaColors.White;
  DPFSlideDialog1.Show( 'D.P.F iOS Native Components', 293, 100, 0.2, 5, sddLeft );
end;

procedure TFAlertMessage.DPFButton5Click( Sender: TObject );
begin
  DPFSlideDialog1.BackgroundImage := TPath.GetDocumentsPath +'/alert_background.png';
  DPFSlideDialog1.TextColor       := TALphaColors.White;
  DPFSlideDialog1.Show( 'D.P.F iOS Native Components', 293, 192, 0.2, 5, sddCenter );
end;

procedure TFAlertMessage.FormShow(Sender: TObject);
begin
  DPFSlideDialog1.BackgroundImage :=  TPath.GetDocumentsPath + '/ShareonFacebook.png';
end;

procedure TFAlertMessage.PaintRects( const UpdateRects: array of TRectF );
begin

  { }

end;

end.
