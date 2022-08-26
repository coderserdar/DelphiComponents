unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  DPF.iOS.BaseControl,
  DPF.iOS.UILabel,
  DPF.iOS.UIView,
  DPF.iOS.UIButton,
  DPF.iOS.LocalAuthentication;

type
  TFActionMessages = class( TForm )
    DPFUIView1: TDPFUIView;
    DPFButton1: TDPFButton;
    DPFLabel1: TDPFLabel;
    DPFLocalAuthentication1: TDPFLocalAuthentication;
    procedure DPFButton1Click( Sender: TObject );
    procedure DPFLocalAuthentication1AuthenticationComplete( Sender: TObject; Result: Boolean; ErrorText: string );
  private
    { Private declarations }
  protected
    // procedure PaintRects( const UpdateRects: array of TRectF ); override;
  public
    { Public declarations }
  end;

var
  FActionMessages: TFActionMessages;

implementation

{$R *.fmx}

procedure TFActionMessages.DPFButton1Click( Sender: TObject );
begin
  DPFLocalAuthentication1.StartAuthentication( 'Hi' );
end;

procedure TFActionMessages.DPFLocalAuthentication1AuthenticationComplete( Sender: TObject; Result: Boolean; ErrorText: string );
begin
  DPFLabel1.Text := ErrorText;

  if Result then
    DPFLabel1.Text := DPFLabel1.Text + #10#13 + 'Congratulations ! Passed.'
  else
    DPFLabel1.Text := DPFLabel1.Text + #10#13 + 'Sorry! Not Passed !';
end;

(* procedure TFActionMessages.PaintRects( const UpdateRects: array of TRectF );
  begin
  { }

  end; *)

end.
