{$I fsdefine.inc}

Unit fslogdlg;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  ExtCtrls,
  Buttons,
  fsllbase;

Type
  TFSLoginDialog = Class(TForm)
    lblUserName: TLabel;
    edtUserName: TEdit;
    edtPassword: TEdit;
    lblPassword: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Image2: TImage;
    Procedure btnOKClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    Function GetPassowrd: String;
    Procedure SetPassword(Const Value: String);
    Function GetUserName: String;
    Procedure SetUserName(Const Value: String);
  Public
    Property UserName: String
      Read GetUserName
      Write SetUserName;
    Property Password: String
      Read GetPassowrd
      Write SetPassword;
  End;

Var
  FSLoginDialog: TFSLoginDialog;

Implementation

{$R *.DFM}

Procedure TFSLoginDialog.btnOKClick(Sender: TObject);
Begin
  If Length(edtUserName.Text) = 0 Then
    Begin
      edtUserName.SetFocus;
      MessageBeep(0);
      Exit;
    End;
  If Length(edtPassword.Text) = 0 Then
    Begin
      edtPassword.SetFocus;
      MessageBeep(0);
      Exit;
    End;
  ModalResult := mrOK;
End;
{--------}

Function TFSLoginDialog.GetPassowrd: String;
Begin
  Result := edtPassword.Text;
End;
{--------}

Function TFSLoginDialog.GetUserName: String;
Begin
  Result := edtUserName.Text;
End;
{--------}

Procedure TFSLoginDialog.SetPassword(Const Value: String);
Begin
  edtPassword.Text := Value;
End;
{--------}

Procedure TFSLoginDialog.SetUserName(Const Value: String);
Begin
  edtUserName.Text := Value;
End;
{--------}

Procedure TFSLoginDialog.FormCreate(Sender: TObject);
Begin
  edtUserName.MaxLength := fscl_UserNameSize;
  edtPassword.MaxLength := fscl_GeneralNameSize;
End;

Procedure TFSLoginDialog.FormShow(Sender: TObject);
Begin
  If edtUserName.Text <> '' Then
    edtPassword.SetFocus;
End;

End.

