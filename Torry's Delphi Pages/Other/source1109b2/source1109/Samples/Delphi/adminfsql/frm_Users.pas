
Unit frm_Users;

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
  fsAdminPlug,
  Grids,
  ExtCtrls,
  Buttons,
  Stdctrls,
  FsLLBase,
  FsSrCfg,
  FsSrBDE,fsserverclass;

Type
  TfrmUsers = Class(TForm)
    pnlGrid: TPanel;
    pnlModify: TPanel;
    gridUsers: TStringGrid;
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnModify: TSpeedButton;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure btnModifyClick(Sender: TObject);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
  Private
    UserList: TfsUserList;
    ptrUserList: PfsUserList;
    Procedure SetupControls(RCount: Integer);
  Public
    PlugIn: TfsBaseAdminPlugin;
  End;

Var
  frmUsers: TfrmUsers;

Implementation

{$R *.DFM}

Uses frm_UsrModify;

Procedure TfrmUsers.FormCreate(Sender: TObject);
// create local userlist for use with remote admin plugin
Begin
  UserList := TfsUserList.Create;
  ptrUserList := @UserList;
End;

Procedure TfrmUsers.FormDestroy(Sender: TObject);
// housekeeping
Begin
  UserList.Free;
End;

Procedure TfrmUsers.SetupControls(RCount: Integer);
// set up and populate the stringgrid holding userinfo
Var
  R: Integer;
Begin
  With gridUsers Do
    Begin
      // set column titles
      Cells[0, 0] := 'UserID';
      Cells[1, 0] := 'First Name';
      Cells[2, 0] := 'Last Name';
      // populate grid with data
      RowCount := RCount + 1;
      For R := 0 To pred(RCount) Do
        Begin
          Cells[0, R + 1] := UserList.UserItem[R].UserID;
          Cells[1, R + 1] := UserList.UserItem[R].FirstName;
          Cells[2, R + 1] := UserList.UserItem[R].LastName;
        End;
    End;
  If (RCount = 0) Then
    Begin
      btnModify.Enabled := False;
      btnDelete.Enabled := False;
    End;
End;

Procedure TfrmUsers.FormShow(Sender: TObject);
Begin
  // request user data
  Plugin.GetUserList(ptrUserList);
  SetupControls(UserList.Count);
End;

Procedure TfrmUsers.btnOKClick(Sender: TObject);
Var
  aResult: TffResult;
Begin
  // update remote userlist
  aResult := Plugin.SetUserList(ptrUserList);
  If aResult <> DBIERR_NONE Then
    Begin
      ShowMessage('Could not save users !');
      Self.ModalResult := mrNone;
    End;
End;

Procedure TfrmUsers.btnModifyClick(Sender: TObject);
Var
  ListUser: TfsUserItem;
Begin
  ListUser := UserList.UserItem[gridUsers.Row - 1];
  If ListUser.UserID = fsc_AdminUserID Then
    Begin
      ShowMessage('Can''t modify admin !');
      Exit;
    End;
  With TfrmUsrModify.Create(Self) Do
    Begin
      Try
        DoModify := True;
        aUser.UserID := ListUser.UserID;
        aUser.aLastName := ListUser.LastName;
        aUser.aFirstName := ListUser.FirstName;
        aUser.aPwdHash := ListUser.PasswordHash;
        aUser.Rights := ListUser.Rights;
        If (ShowModal = mrOK) Then
          Begin
            // do changes in UserList
            UserList.DeleteUser(ListUser.UserID);
            With aUser Do
              UserList.AddUser(TfsUserItem.Create(UserID, aLastName, aFirstName, aPwdHash, Rights));
            // do changes in gridUsers
            SetUpControls(UserList.Count);
          End;
      Finally
        Free;
      End;
    End;
End;

Procedure TfrmUsers.btnAddClick(Sender: TObject);
Begin
  With TfrmUsrModify.Create(Self) Do
    Begin
      Try
        DoModify := False;
        aUser.UserID := '';
        aUser.aLastName := '';
        aUser.aFirstName := '';
        aUser.aPwdHash := 0;
        aUser.Rights := [];
        If (ShowModal = mrOK) Then
          Begin
            // do changes in UserList
            With aUser Do
              UserList.AddUser(TfsUserItem.Create(UserID, aLastName, aFirstName, aPwdHash, Rights));
            // do changes in gridUsers
            SetUpControls(UserList.Count);
          End;
      Finally
        Free;
      End;
    End;
End;

Procedure TfrmUsers.btnDeleteClick(Sender: TObject);
Var
  ListUser: TfsUserItem;
Begin
  ListUser := UserList.UserItem[gridUsers.Row - 1];
  If ListUser.UserID = fsc_AdminUserID Then
    Begin
      ShowMessage('Can''t delete admin !');
      Exit;
    End;
  If Application.MessageBox('Are you sure ?', 'Confirm Delete',
    MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = IDYES Then
    Begin
      // do changes in UserList
      UserList.DeleteUser(ListUser.UserID);
      // do changes in gridUsers
      SetUpControls(UserList.Count);
    End;
End;

End.

