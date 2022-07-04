unit UFormViewOptionsText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormViewOptionsText = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    boxText: TGroupBox;
    chkDetect: TCheckBox;
    labDetect1: TLabel;
    edDetectSize: TEdit;
    labDetect2: TLabel;
    labDetectL1: TLabel;
    edDetectLimit: TEdit;
    labDetectL2: TLabel;
    chkDetectOEM: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure chkDetectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses
  ATxMsgProc;

{$R *.DFM}

procedure TFormViewOptionsText.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewOptionsText.inc}

  edDetectSize.Left:= labDetect1.Left+labDetect1.Width+6;
  edDetectLimit.Left:= labDetectL1.Left+labDetectL1.Width+6;
  labDetect2.Left:= edDetectSize.Left+edDetectSize.Width+6;
  labDetectL2.Left:= edDetectLimit.Left+edDetectLimit.Width+6;
  chkDetectClick(Self);
end;

procedure TFormViewOptionsText.chkDetectClick(Sender: TObject);
begin
  edDetectSize.Enabled:= chkDetect.Checked;
  edDetectLimit.Enabled:= chkDetect.Checked;
  chkDetectOEM.Enabled:= chkDetect.Checked;
  labDetect1.Enabled:= chkDetect.Checked;
  labDetect2.Enabled:= chkDetect.Checked;
  labDetectL1.Enabled:= chkDetect.Checked;
  labDetectL2.Enabled:= chkDetect.Checked;
end;

end.
