Unit frm_UsrModify;

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
  fsAdminPlug,
  FsSrBase,
  FsSrCfg,
  FsHash;

Type
  TfrmUsrModify = Class(TForm)
    Label1: TLabel;
    edtID: TEdit;
    Label4: TLabel;
    edtPasswd: TEdit;
    Label2: TLabel;
    edtFirst: TEdit;
    Label3: TLabel;
    edtLast: TEdit;
    chkDelete: TCheckBox;
    chkUpdate: TCheckBox;
    chkInsert: TCheckBox;
    chkRead: TCheckBox;
    chkAdmin: TCheckBox;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    Procedure FormShow(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    DoModify: boolean;
    aUser: TfsnmUser;
  End;

Var
  frmUsrModify: TfrmUsrModify;

Implementation

{$R *.DFM}

Procedure TfrmUsrModify.FormCreate(Sender: TObject);
Begin
  DoModify := False;
End;

Procedure TfrmUsrModify.FormShow(Sender: TObject);
Begin
  If DoModify Then
    With aUser Do
      Begin
        edtID.Text := UserID;
        edtPasswd.Text := '<hash>';
        edtFirst.Text := aFirstName;
        edtLast.Text := aLastName;
        chkAdmin.Checked := arAdmin In Rights;
        chkRead.Checked := arRead In Rights;
        chkInsert.Checked := arInsert In Rights;
        chkUpdate.Checked := arUpdate In Rights;
        chkDelete.Checked := arDelete In Rights;
        edtID.Enabled := False; // no ID change allowed
      End;
End;

Procedure TfrmUsrModify.btnOKClick(Sender: TObject);
Begin
  If (edtID.Text <> '') Then
    Begin
      With aUser Do
        Begin
          UserID := edtID.Text;
          If (edtPasswd.Text <> '<hash>') Then
            aPwdHash := FSCalcShStrELFHash(edtPasswd.Text);
          aFirstName := edtFirst.Text;
          aLastName := edtLast.Text;
          If chkAdmin.Checked Then
            Include(Rights, arAdmin);
          If chkRead.Checked Then
            Include(Rights, arRead);
          If chkInsert.Checked Then
            Include(Rights, arInsert);
          If chkUpdate.Checked Then
            Include(Rights, arUpdate);
          If chkDelete.Checked Then
            Include(Rights, arDelete);
        End;
    End
  Else
    Self.ModalResult := mrNone;
End;

End.

