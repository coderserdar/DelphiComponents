unit fmEnterCryptKey;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ArchiverMisc, ArchiverRoot, CustExtractor, CustArchiver,
  Archiver;

type
  TEnterCryptKey = class(TForm)
    lPrompt: TLabel;
    edKey: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbHide: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure cbHideClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  EnterCryptKey: TEnterCryptKey;

implementation
uses fmMain;
{$R *.DFM}

procedure TEnterCryptKey.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    Exit;
  CanClose := Length(edKey.Text) >= Main.Archiver1.MinKeySize;
  if not CanClose then
    begin
      MessageDlg( Format( Main.Archiver1.Messages.KeyTooShort, [Main.Archiver1.MinKeySize]), mtWarning, [mbOk], 0 );
      edKey.SetFocus;
      edKey.SelectAll;
    end;
end;

procedure TEnterCryptKey.FormShow(Sender: TObject);
begin
  edKey.SetFocus;
  edKey.SelectAll;
end;

procedure TEnterCryptKey.cbHideClick(Sender: TObject);
begin
  with Sender as TCheckBox do
    begin
      if Checked then
        edKey.PasswordChar := '*'
      else
        edKey.PasswordChar := #0;
    end;
end;

end.
