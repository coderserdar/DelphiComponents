unit OverbyteIcsCliCertDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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

{$R *.dfm}

procedure TClientCertDlg.CertListBoxDblClick(Sender: TObject);
begin
    if CertListBox.ItemIndex <> -1 then
        ModalResult := mrOk;
end;

end.
