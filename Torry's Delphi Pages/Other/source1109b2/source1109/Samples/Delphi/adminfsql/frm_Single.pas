
Unit frm_Single;

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
  ExtCtrls,
  Stdctrls,
  fsAdminPlug;

Type
  TfrmSingle = Class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtID: TEdit;
    edtPasswd: TEdit;
    edtFirst: TEdit;
    edtLast: TEdit;
    chkAdmin: TCheckBox;
    chkRead: TCheckBox;
    chkInsert: TCheckBox;
    chkUpdate: TCheckBox;
    chkDelete: TCheckBox;
    Panel1: TPanel;
    btnGet: TButton;
    btnAdd: TButton;
    btnUpdate: TButton;
    btnDelete: TButton;
    btnCheck: TButton;
    rgSecure: TRadioGroup;
    btnToggle: TButton;
    GroupBox2: TGroupBox;
    btnClear: TButton;
    lblUserCount: TLabel;
    btnCount: TButton;
    pnlButton: TPanel;
    btnClose: TButton;
    Procedure btnGetClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btnToggleClick(Sender: TObject);
    Procedure btnCountClick(Sender: TObject);
    Procedure btnClearClick(Sender: TObject);
    Procedure btnCheckClick(Sender: TObject);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnUpdateClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
  Private
    { Private declarations }
  Public
    Plugin: TfsBaseAdminPlugin;
  End;

Var
  frmSingle: TfrmSingle;

Implementation

{$R *.DFM}

Uses FsSrBase,
  FsSrBDE,
  FsHash; // FF helper routines and declarations

Procedure TfrmSingle.FormShow(Sender: TObject);
// read server security mode
Var
  StatusSecure: boolean;
Begin
  lblUserCount.Caption := '';
  If (Plugin.GetIsSecure(StatusSecure) = DBIERR_NONE) Then
    Begin
      If StatusSecure Then
        rgSecure.ItemIndex := 0
      Else
        rgSecure.ItemIndex := 1;
    End
  Else
    ShowMessage('No compatible server found !');
End;

// -- Server security routines -------------------------------------------------

Procedure TfrmSingle.btnToggleClick(Sender: TObject);
// toggle server security status
Var
  StatusSecure: boolean;
Begin
  If (Plugin.GetIsSecure(StatusSecure) = DBIERR_NONE) Then
    Begin
      StatusSecure := Not StatusSecure;
      Plugin.SetIsSecure(StatusSecure);
      If StatusSecure Then
        rgSecure.ItemIndex := 0
      Else
        rgSecure.ItemIndex := 1;
    End
  Else
    ShowMessage('No compatible server found !');
End;

// -- User administration routines ---------------------------------------------

Procedure TfrmSingle.btnGetClick(Sender: TObject);
// get userdata for user with given ID
Var
  UserData: TfsnmUser;
  ptrUserData: PfsnmUser;
Begin
  If edtID.Text = '' Then Exit;
  UserData.UserID := edtID.Text;
  ptrUserData := @UserData;
  If (Plugin.GetUser(ptrUserData) = DBIERR_NONE) Then
    Begin
      With UserData Do
        Begin
          edtPasswd.Text := '<hash>';
          edtFirst.Text := aFirstName;
          edtLast.Text := aLastName;
          chkAdmin.Checked := arAdmin In Rights;
          chkRead.Checked := arRead In Rights;
          chkInsert.Checked := arInsert In Rights;
          chkUpdate.Checked := arUpdate In Rights;
          chkDelete.Checked := arDelete In Rights;
        End;
    End
  Else
    ShowMessage('No such user or server error !');
End;

Procedure TfrmSingle.btnAddClick(Sender: TObject);
// add user to server
Var
  UserData: TfsnmUser;
  ptrUserData: PfsnmUser;
Begin
  If edtID.Text = '' Then Exit;
  With UserData Do
    Begin
      UserID := edtID.Text;
      If (edtPasswd.Text <> '<hash>') Then aPwdHash := FsCalcShStrELFHash(edtPasswd.Text);
      aFirstName := edtFirst.Text;
      aLastName := edtLast.Text;
      If chkAdmin.Checked Then Include(Rights, arAdmin);
      If chkRead.Checked Then Include(Rights, arRead);
      If chkInsert.Checked Then Include(Rights, arInsert);
      If chkUpdate.Checked Then Include(Rights, arUpdate);
      If chkDelete.Checked Then Include(Rights, arDelete);
    End;
  ptrUserData := @UserData;
  If (Plugin.AddUser(ptrUserData) <> DBIERR_NONE) Then
    lblUserCount.Caption := ''
  Else
    ShowMessage('Error adding user !');
End;

Procedure TfrmSingle.btnUpdateClick(Sender: TObject);
// update existing user with new data
Var
  UserData: TfsnmUser;
  ptrUserData: PfsnmUser;
Begin
  If edtID.Text = '' Then Exit;
  With UserData Do
    Begin
      UserID := edtID.Text;
      If (edtPasswd.Text <> '<hash>') Then aPwdHash := FsCalcShStrELFHash(edtPasswd.Text);
      aFirstName := edtFirst.Text;
      aLastName := edtLast.Text;
      If chkAdmin.Checked Then Include(Rights, arAdmin);
      If chkRead.Checked Then Include(Rights, arRead);
      If chkInsert.Checked Then Include(Rights, arInsert);
      If chkUpdate.Checked Then Include(Rights, arUpdate);
      If chkDelete.Checked Then Include(Rights, arDelete);
    End;
  ptrUserData := @UserData;
  If (Plugin.UpdateUser(ptrUserData) <> DBIERR_NONE) Then ShowMessage('Error updating user !');
End;

Procedure TfrmSingle.btnDeleteClick(Sender: TObject);
// remove user from userlist on server
Begin
  If edtID.Text = '' Then Exit;
  If Application.MessageBox('Are you sure ?', 'Confirm delete',
    MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = IDYES Then
    Begin
      If (PlugIn.DeleteUser(edtID.Text) = DBIERR_NONE) Then
        lblUserCount.Caption := ''
      Else
        ShowMessage('Error removing user !');
    End;
End;

Procedure TfrmSingle.btnCheckClick(Sender: TObject);
// check for user
Begin
  If (Plugin.ExistsUser(edtID.Text) = DBIERR_NONE) Then
    ShowMessage('User exists')
  Else
    ShowMessage('No such user or server error !');
End;

// -- Userlist routines --------------------------------------------------------

Procedure TfrmSingle.btnCountClick(Sender: TObject);
// count known users
Var
  aCount: Integer;
Begin
  If (Plugin.GetUserCount(aCount) = DBIERR_NONE) Then
    lblUserCount.Caption := (Format('There are currently %d known users.', [aCount]))
  Else
    ShowMessage('No compatible server found !');
End;

Procedure TfrmSingle.btnClearClick(Sender: TObject);
// clear userlist
Begin
  If Application.MessageBox(
    PChar('Are you sure you want this ?' + #10#13 +
    'It will remove all known users from your server (including ''admin'') !'),
    'Think twice',
    MB_ICONWARNING + MB_OKCANCEL + MB_DEFBUTTON2) = IDOK Then
    If (PlugIn.ClearUserList = DBIERR_NONE) Then
      lblUserCount.Caption := ''
    Else
      ShowMessage('Error clearing userlist !');
End;

End.

