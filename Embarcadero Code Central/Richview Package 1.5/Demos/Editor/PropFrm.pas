unit PropFrm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;
type
  TfrmProp = class(TForm)
    PageControl1: TPageControl;
    tsBullet: TTabSheet;
    tsHotSpot: TTabSheet;
    btnOk: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    txtTag: TEdit;
    rgBullet: TRadioGroup;
    tsPicture: TTabSheet;
    rgHotSpot: TRadioGroup;
    Label3: TLabel;
    txtName: TEdit;
    Image1: TImage;
    btnChangePicture: TButton;
    tsText: TTabSheet;
    lblText: TLabel;
    tsComponent: TTabSheet;
    lblComponent: TLabel;
    txtWidth: TEdit;
    Label2: TLabel;
    txtHeight: TEdit;
    Label4: TLabel;
    rgPicVAlign: TRadioGroup;
    rgCtrlVAlign: TRadioGroup;
    tsBreak: TTabSheet;
    txtBreakWidth: TEdit;
    Label5: TLabel;
    rgBreakColor: TRadioGroup;
    procedure btnChangePictureClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProp: TfrmProp;

implementation
uses Unit1;

{$R *.DFM}

procedure TfrmProp.btnChangePictureClick(Sender: TObject);
begin
  Form1.OpenDialog1.Title := 'Changing Image';
  {$IFDEF RICHVIEWDEF3}
  Form1.OpenDialog1.Filter := 'Graphics(*.bmp;*.wmf;*.ico;*.jpg)|*.bmp;*.wmf;*.ico;*.jpg|All(*.*)|*.*';
  {$ELSE}
  Form1.OpenDialog1.Filter := 'Graphics(*.bmp;*.wmf;*.ico)|*.bmp;*.wmf;*.ico|All(*.*)|*.*';  
  {$ENDIF}
  if Form1.OpenDialog1.Execute then
    Image1.Picture.LoadFromFile(Form1.OpenDialog1.FileName);
end;

end.
