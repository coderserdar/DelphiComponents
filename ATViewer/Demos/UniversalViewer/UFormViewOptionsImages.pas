{$I ATViewerOptions.inc}

unit UFormViewOptionsImages;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormViewOptionsImages = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    boxIView: TGroupBox;
    chkUseIView: TCheckBox;
    edExeIView: TEdit;
    btnBrowseExe: TButton;
    labExeIView: TLabel;
    labExtIView: TLabel;
    edExtIView: TEdit;
    chkPriority: TCheckBox;
    OpenDialog1: TOpenDialog;
    boxIJL: TGroupBox;
    labExtIJL: TLabel;
    chkUseIJL: TCheckBox;
    edExtIJL: TEdit;
    procedure FormShow(Sender: TObject);
    procedure chkUseIViewClick(Sender: TObject);
    procedure btnBrowseExeClick(Sender: TObject);
    procedure chkUseIJLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses
  ATxMsgProc, ATViewerMsg;

{$R *.DFM}

procedure TFormViewOptionsImages.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewOptionsImages.inc}

  {$ifndef IVIEW}
  chkUseIView.Checked:= false;
  chkUseIView.Enabled:= false;
  {$endif}

  {$ifndef IJL}
  chkUseIJL.Checked:= false;
  chkUseIJL.Enabled:= false;
  {$endif}

  chkUseIViewClick(Self);
  chkUseIJLClick(Self);
end;

procedure TFormViewOptionsImages.chkUseIViewClick(Sender: TObject);
var
  En: Boolean;
begin
  En:= chkUseIView.Checked;
  labExeIView.Enabled:= En;
  edExeIView.Enabled:= En;
  btnBrowseExe.Enabled:= En;
  labExtIView.Enabled:= En;
  edExtIView.Enabled:= En;
  chkPriority.Enabled:= En;
end;

procedure TFormViewOptionsImages.btnBrowseExeClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      edExeIView.Text:= FileName;
end;

procedure TFormViewOptionsImages.chkUseIJLClick(Sender: TObject);
var
  En: Boolean;
begin
  En:= chkUseIJL.Checked;
  labExtIJL.Enabled:= En;
  edExtIJL.Enabled:= En;
end;

end.
