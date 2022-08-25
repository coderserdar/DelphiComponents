unit IcsCliCertDlg;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls,
  FMX.ListBox, FMX.Controls.Presentation;

type
  TClientCertDlg = class(TForm)
    CertListBox: TListBox;
    Label1: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure CertListBoxDblClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  ClientCertDlg: TClientCertDlg;

implementation

{$R *.FMX}

procedure TClientCertDlg.CertListBoxDblClick(Sender: TObject);
begin
    if CertListBox.ItemIndex <> -1 then
        ModalResult := mrOk;
end;

end.
