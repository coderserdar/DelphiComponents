Unit fssrvuser;

{$I FsDEFINE.INC}

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
  Buttons,
  ExtCtrls,
  Grids,
  ComCtrls,
  ToolWin,
  Menus,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  FsLLBase,
  FsLLUNC,
  FsHash,
  fsindexaccess,
  FsSrBase,
  FsSrBDE,
  FsSrTran,
  FsSrCfg,
  fsserverclass,
  fsllgrid;

Type
  TfsUserForm = Class(TForm)
    pnlLower: TPanel;
    btnSave: TBitBtn;
    btnDiscard: TBitBtn;
    grdUsers: TFSGrid;
    imgChkBoxClear: TImage;
    imgChkBoxSet: TImage;
    tbMain: TToolBar;
    pbDelete: TToolButton;
    pbPassword: TToolButton;
    mnuMain: TMainMenu;
    mnuUser: TMenuItem;
    mnuUserDelete: TMenuItem;
    mnuUserPassword: TMenuItem;
    imgMain: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnPasswordClick(Sender: TObject);
    Procedure btnSaveClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure grdUsersDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure grdUsersKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure grdUsersKeyPress(Sender: TObject; Var Key: Char);
    Procedure grdUsersSortColumn(Sender: TFSGrid; aCol: Integer);
    Procedure grdUsersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure grdUsersSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure ToolButton1Click(Sender: TObject);
    Procedure ToolButton2Click(Sender: TObject);
  Private
    FEngine: TFSServer;

    Procedure ufPopulateColHeaders;
    Procedure ufPopulateGrid;
    Procedure ufSetEngine(anEngine: TFSServer);

  Public
    Property ServerEngine: TFSServer Read FEngine Write ufSetEngine;
  End;

Var
  fsUserForm: TfsUserForm;

Implementation

Uses
  fssrvpwd,
  FsLLExcp;

{$R *.DFM}

Const
  UnknownPwdHash = $FFFFFFFF;

  { Column constants }
  cnUserID = 0;
  cnLastName = 1;
  cnFirstName = 2;
  cnAdmin = 3;
  cnRead = 4;
  cnInsert = 5;
  cnUpdate = 6;
  cnDelete = 7;

  cnReadTable = 8;
  cnInsertTable = 9;
  cnUpdateTable = 10;
  cnDeleteTable = 11;
  cnRestruct = 12;
  cnDefrag = 13;

  cnCopyTable = 14;
  cnTabSetInc = 15;
  cnTabEmpty = 16;

  cnReadBase = 17;
  cnInsertBase = 18;
  cnUpdateBase = 19;
  cnDeleteBase = 20;

  cnReadBlob = 21;
  cnInsertBlob = 22;
  cnUpdateBlob = 23;
  cnDeleteBlob = 24;
  cnProtectRow = 25;
  { Cell margin constants }
  cnTopMargin = 2;
  cnLeftMargin = 2;

  { Boolean field constants }
  cnTrue = 1;
  cnFalse = 0;

  {===Helper methods===================================================}

Procedure TfsUserForm.ufSetEngine(anEngine: TFSServer);
Begin
  FEngine := anEngine;

  { Set the row count. }
  grdUsers.RowCount := FEngine.Configuration.UserList.Count + 2;
  grdUsers.Row := 1;
End;
{====================================================================}

{===Grid methods & event handlers====================================}

Procedure TfsUserForm.FormShow(Sender: TObject);
Begin
  ufPopulateColHeaders;
  ufPopulateGrid;
  grdUsers.SetFocus;
  { Psition to the 1st non-admin user row. }
  With grdUsers Do
    Begin
      Col := cnUserID;
      { Is the first row the Admin user? }
      If Cells[cnUserID, 1] = fsc_AdminUserID Then
        { Yes. Position to subsequent row. }
        Row := 2
      Else
        { No. Position to the first row. }
        Row := 1;
    End;
End;
{--------}

Procedure TfsUserForm.grdUsersDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  aBitmap: TBitmap;
  aStr: String;
  Grid: TFSGrid Absolute Sender;
  Dest, Source: TRect;
Begin

  { Leave fixed portion of the grid alone}
  If gdFixed In State Then
    Exit;

  { Is this a boolean field? }
  If aCol >= cnAdmin Then
    Begin
      If Longint(Grid.Objects[aCol, aRow]) = cnTrue Then
        aBitmap := imgChkBoxSet.Picture.Bitmap
      Else
        aBitmap := imgChkBoxClear.Picture.Bitmap;
      With Grid.Canvas Do
        Begin
          Dest := Bounds(Rect.Left + ((Rect.Right - aBitmap.Width - Rect.Left) Div 2),
            Rect.Top + (Grid.DefaultRowHeight - aBitmap.Height) Div 2,
            aBitmap.Width,
            aBitmap.Height);
          Source := Bounds(0, 0, aBitmap.Width, aBitmap.Height);
          BrushCopy(Dest,
            aBitmap,
            Source,
            aBitmap.TransparentColor);
        End;
    End
  Else
    Begin
      { No.  Draw the text. }
      aStr := Grid.Cells[aCol, aRow];

      With Grid Do
        Begin
          If (aCol = cnUserID) And
            (Cells[cnUserID, aRow] <> '') And
            (TffWord32(Objects[cnUserID, aRow]) = UnknownPwdHash) Then
            Begin
              Canvas.Brush.Color := clRed;
              Canvas.Font.Color := clWhite;
            End;
          Canvas.FillRect(Rect);
          Canvas.TextRect(Rect, Rect.Left + cnLeftMargin, Rect.Top + cnTopMargin, aStr);
        End;
    End;

End;
{--------}

Procedure TfsUserForm.grdUsersKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  Grid: TFSGrid Absolute Sender;
Begin
  { Change the selected cell (Enter as tab)}
  Case Key Of
    VK_RETURN:
      With Grid Do
        Begin
          If Col < Pred(ColCount) Then
            Col := Col + 1
          Else If Row < Pred(RowCount) Then
            Begin
              Row := Row + 1;
              Objects[cnUserID, Row] := pointer(UnknownPwdHash);
              Col := cnUserID;
            End
          Else
            Begin
              { Is this cell blank? }
              If Cells[Col, Row] = '' Then
                Begin
                  { Yes. Wrap to first row of grid. }
                  Row := 1;
                  Col := cnUserID;
                End
              Else
                Begin
                  { No.  Add a new blank row. }
                  RowCount := RowCount + 1;
                  Row := Pred(RowCount);
                  Col := cnUserID;
                  Objects[cnUserID, Row] := pointer(UnknownPwdHash);
                End;
            End;
        End;
    VK_DOWN:
      With Grid Do
        Begin
          { Are we trying to arrow down from an incomplete row? }
          If (Row = pred(RowCount)) Then
            If AnyCellIsEmpty(Row) Then
              Begin
                { Yes.  Do not allow this to occur. }
                Key := 0;
                MessageBeep(0);
              End
            Else
              Begin
                { No.  Make sure we have a new blank row. }
                RowCount := RowCount + 1;
                Objects[cnUserID, pred(RowCount)] := pointer(UnknownPwdHash);
              End;
        End;
    VK_UP, VK_TAB:
      With Grid Do
        Begin
          { Are we trying to arrow up from or Tab forward out of a new,
            completed row? }
          If (Row = pred(RowCount)) And (Cells[cnUserID, Row] <> '') Then
            { Yes.  Add a new blank row. }
            RowCount := RowCount + 1;
          Objects[cnUserID, pred(RowCount)] := pointer(UnknownPwdHash);
        End;
  End; { case }
End;
{--------}

Procedure TfsUserForm.grdUsersKeyPress(Sender: TObject; Var Key: Char);
Const
  validEditKeys = [#8, #9, #13];
Var
  Grid: TFSGrid Absolute Sender;
  Ignore: Boolean;
  Value: String;
Begin
  If Not (Key In validEditKeys) Then
    Begin
      { Validate data entry as key's are pressed}
      Case Grid.Col Of
        cnUserID, cnLastName, cnFirstName:
          Begin
            Value := Grid.Cells[Grid.Col, Grid.Row];
            Ignore := (Length(Value) >= fscl_UserNameSize);
          End;
        cnAdmin, cnRead, cnInsert, cnUpdate, cnDelete, cnRestruct, cnDefrag,
          cnDeleteTable, cnDeleteBase, cnInsertTable, cnInsertBase, cnUpdateBase, cnReadTable,
          cnReadBase, cnReadBlob, cnInsertBlob, cnUpdateBlob, cnDeleteBlob,
          cnCopyTable,
          cnTabSetInc,
          cnTabEmpty,
          cnProtectRow:
          Begin
            Ignore :=
              (Grid.Cells[cnUserID, Grid.Row] = fsc_AdminUserID) Or
              (Key <> ' ');
            If Not Ignore Then
              Begin
                If Longint(Grid.Objects[Grid.Col, Grid.Row]) = cnTrue Then
                  Grid.Objects[Grid.Col, Grid.Row] := pointer(cnFalse)
                Else
                  Grid.Objects[Grid.Col, Grid.Row] := pointer(cnTrue);
              End;
          End;
        Else
          Ignore := False;
      End; { case }
      If Ignore Then
        Begin
          Key := #0;
          MessageBeep(0);
        End;
    End;
End;
{--------}

Procedure TfsUserForm.grdUsersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  ACol, ARow: Longint;
  Rect, Dest: TRect;
  Grid: TFSGrid Absolute Sender;
Begin

  If Button <> mbLeft Then
    Exit;

  Grid.MouseToCell(X, Y, ACol, ARow);
  { If this is not a valid row or if this is the Admin user then exit.  We do
    the latter because the Admin user may not be modified. }
  If (ARow < 0) Or
    (Grid.Cells[cnUserID, ARow] = fsc_AdminUserID) Then
    Exit;

  { Is this a rights cell? }
  If ACol >= cnAdmin Then
    Begin
      Rect := Grid.CellRect(ACol, ARow);
      With imgChkBoxSet.Picture Do
        { Retrieve the rect from around the box itself}
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
          Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) Div 2,
          Bitmap.Width,
          Bitmap.Height);

      { Only manipuate the checkbox state if an area on or within the rect was
        clicked}
      If (X >= Dest.Left) And (X <= Dest.Right) And
        (Y >= Dest.Top) And (Y <= Dest.Bottom) Then
        Begin
          If Longint(Grid.Objects[aCol, aRow]) = cnTrue Then
            Grid.Objects[aCol, aRow] := pointer(cnFalse)
          Else
            Grid.Objects[aCol, aRow] := pointer(cnTrue);
        End;
    End;
End;
{--------}

Procedure TfsUserForm.grdUsersSelectCell(Sender: TObject;
  ACol, ARow: Integer;
  Var CanSelect: Boolean);
Var
  Grid: TFSGrid Absolute Sender;
Begin
  CanSelect := True;
  {if we're on the administrator row, no editing}
  If (Grid.Cells[cnUserID, ARow] = fsc_AdminUserID) Then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
      {if we're in a column with checkboxes, no editing}
  Else If ACol >= cnAdmin Then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
      {otherwise allow editing}
  Else
    Grid.Options := Grid.Options + [goEditing];
  If (ARow = pred(Grid.RowCount)) And
    (Grid.Cells[cnUserID, ARow] <> '') Then
    Begin
      { Yes.  Add a new blank row. }
      Grid.RowCount := Grid.RowCount + 1;
      Grid.Objects[cnUserID, Pred(Grid.RowCount)] := pointer(UnknownPwdHash);
    End;
End;
{--------}

Procedure TfsUserForm.grdUsersSortColumn(Sender: TFSGrid;
  aCol: Integer);
Var
  aStr: String;
  i, j: Integer;
  LastRow: Integer;
Begin
  If (Sender.RowCount > 1) And (aCol < cnAdmin) Then
    With Sender Do
      Begin

        If LastRowIsEmpty Then
          LastRow := (RowCount - 2)
        Else
          LastRow := pred(RowCount);

        BeginUpdate;
        Try
          For i := 1 To LastRow Do
            Begin
              SaveRow(i);
              aStr := Cells[aCol, i];
              j := i;
              While (j > 1) And
                (ansiCompareStr(Cells[aCol, j - 1], aStr) > 0) Do
                Begin
                  CopyRow(j - 1, j);
                  dec(j);
                End; { while }
              RestoreToRow(j);
            End; { for }
        Finally
          EndUpdate;
        End;
      End; { with }
End;
{--------}

Procedure TfsUserForm.ufPopulateColHeaders;
Begin
  With grdUsers Do
    Begin
      BeginUpdate;
      Try
        Cells[cnUserID, 0] := 'User';
        Cells[cnLastName, 0] := 'Last name';
        Cells[cnFirstName, 0] := 'First name';
        Cells[cnAdmin, 0] := 'Admin';
        Cells[cnRead, 0] := 'Read rec';
        Cells[cnInsert, 0] := 'Insert rec';
        Cells[cnUpdate, 0] := 'Update rec';
        Cells[cnDelete, 0] := 'Delete rec';
        Cells[cnRestruct, 0] := 'Restruct table';
        Cells[cnDefrag, 0] := 'Defrag table';
        Cells[cnDeleteTable, 0] := 'Delete table';
        Cells[cnDeleteBase, 0] := 'Delete base';
        Cells[cnUpdateBase, 0] := 'Update base';
        Cells[cnInsertTable, 0] := 'Insert table';
        Cells[cnUpdateTable, 0] := 'Update table';
        Cells[cnInsertBase, 0] := 'Insert base';
        Cells[cnReadTable, 0] := 'Read table';
        Cells[cnReadBase, 0] := 'Read base';
        Cells[cnReadBlob, 0] := 'Read blob';
        Cells[cnInsertBlob, 0] := 'Insert Blob';
        Cells[cnUpdateBlob, 0] := 'Update Blob';
        Cells[cnDeleteBlob, 0] := 'Delete Blob';
        Cells[cnCopyTable, 0] := 'Copy table';
        Cells[cnTabSetInc, 0] := 'Update table inc';
        Cells[cnTabEmpty, 0] := 'Empty table';
        Cells[cnProtectRow, 0] := 'Protect Row';
      Finally
        EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfsUserForm.ufPopulateGrid;
Var
  Item: TfsUserItem;
  Inx: Integer;
Begin
  With grdUsers Do
    Begin
      BeginUpdate;
      Try
        For Inx := 1 To FEngine.Configuration.UserList.Count Do
          Begin
            Item := FEngine.Configuration.UserList[pred(Inx)];
            Cells[cnUserID, Inx] := Item.UserID;
            Objects[cnUserID, Inx] := pointer(Item.PasswordHash);
            Cells[cnLastName, Inx] := Item.LastName;
            Cells[cnFirstName, Inx] := Item.FirstName;
            If arAdmin In Item.Rights Then
              Objects[cnAdmin, Inx] := pointer(cnTrue)
            Else
              Objects[cnAdmin, Inx] := pointer(cnFalse);

            If arRead In Item.Rights Then
              Objects[cnRead, Inx] := pointer(cnTrue)
            Else
              Objects[cnRead, Inx] := pointer(cnFalse);

            If arInsert In Item.Rights Then
              Objects[cnInsert, Inx] := pointer(cnTrue)
            Else
              Objects[cnInsert, Inx] := pointer(cnFalse);

            If arUpdate In Item.Rights Then
              Objects[cnUpdate, Inx] := pointer(cnTrue)
            Else
              Objects[cnUpdate, Inx] := pointer(cnFalse);

            If arDelete In Item.Rights Then
              Objects[cnDelete, Inx] := pointer(cnTrue)
            Else
              Objects[cnDelete, Inx] := pointer(cnFalse);

            If arRestruct In Item.Rights Then
              Objects[cnRestruct, Inx] := pointer(cnTrue)
            Else
              Objects[cnRestruct, Inx] := pointer(cnFalse);

            If arDefrag In Item.Rights Then
              Objects[cnDefrag, Inx] := pointer(cnTrue)
            Else
              Objects[cnDefrag, Inx] := pointer(cnFalse);

            If arDeleteTable In Item.Rights Then
              Objects[cnDeleteTable, Inx] := pointer(cnTrue)
            Else
              Objects[cnDeleteTable, Inx] := pointer(cnFalse);

            If arDeleteBase In Item.Rights Then
              Objects[cnDeleteBase, Inx] := pointer(cnTrue)
            Else
              Objects[cnDeleteBase, Inx] := pointer(cnFalse);
            If arUpdateBase In Item.Rights Then
              Objects[cnUpdateBase, Inx] := pointer(cnTrue)
            Else
              Objects[cnDeleteBase, Inx] := pointer(cnFalse);

            If arInsertTable In Item.Rights Then
              Objects[cnInsertTable, Inx] := pointer(cnTrue)
            Else
              Objects[cnInsertTable, Inx] := pointer(cnFalse);

            If arUpdateTable In Item.Rights Then
              Objects[cnUpdateTable, Inx] := pointer(cnTrue)
            Else
              Objects[cnInsertTable, Inx] := pointer(cnFalse);

            If arInsertBase In Item.Rights Then
              Objects[cnInsertBase, Inx] := pointer(cnTrue)
            Else
              Objects[cnInsertBase, Inx] := pointer(cnFalse);

            If arReadTable In Item.Rights Then
              Objects[cnReadTable, Inx] := pointer(cnTrue)
            Else
              Objects[cnReadTable, Inx] := pointer(cnFalse);

            If arReadBase In Item.Rights Then
              Objects[cnReadBase, Inx] := pointer(cnTrue)
            Else
              Objects[cnReadBase, Inx] := pointer(cnFalse);

            If arReadBlob In Item.Rights Then
              Objects[cnReadBlob, Inx] := pointer(cnTrue)
            Else
              Objects[cnReadBlob, Inx] := pointer(cnFalse);

            If arInsertBlob In Item.Rights Then
              Objects[cnInsertBlob, Inx] := pointer(cnTrue)
            Else
              Objects[cnInsertBlob, Inx] := pointer(cnFalse);

            If arUpdateBlob In Item.Rights Then
              Objects[cnUpdateBlob, Inx] := pointer(cnTrue)
            Else
              Objects[cnUpdateBlob, Inx] := pointer(cnFalse);

            If arDeleteBlob In Item.Rights Then
              Objects[cnDeleteBlob, Inx] := pointer(cnTrue)
            Else
              Objects[cnDeleteBlob, Inx] := pointer(cnFalse);

            If arCopyTable In Item.Rights Then
              Objects[cnCopyTable, Inx] := pointer(cnTrue)
            Else
              Objects[cnCopyTable, Inx] := pointer(cnFalse);

            If arTabSetInc In Item.Rights Then
              Objects[cnTabSetInc, Inx] := pointer(cnTrue)
            Else
              Objects[cnTabSetInc, Inx] := pointer(cnFalse);

            If arTabEmpty In Item.Rights Then
              Objects[cnTabEmpty, Inx] := pointer(cnTrue)
            Else
              Objects[cnTabEmpty, Inx] := pointer(cnFalse);

            If arProtectRow In Item.Rights Then
              Objects[cnProtectRow, Inx] := pointer(cnTrue)
            Else
              Objects[cnProtectRow, Inx] := pointer(cnFalse);

          End;
      Finally
        EndUpdate;
      End;
    End;
End;
{====================================================================}

{===Button methods===================================================}

Procedure TfsUserForm.btnDeleteClick(Sender: TObject);
Var
  DeletedRow: Integer;
  Inx: Integer;
  LastEmpty: boolean;
  LastRow: Integer;
Begin

  If (grdUsers.RowCount < 2) Then
    Exit;

  With grdUsers Do
    Begin
      BeginUpdate;
      Try
        DeletedRow := Row;
        LastRow := pred(RowCount);
        LastEmpty := LastRowIsEmpty;

        { Situation where delete in not okay
          1. When the row represents the admin user }

        If UpperCase(Cells[cnUserID, DeletedRow]) = 'SYSTEMADMIN' Then
          Begin
            MessageBeep(0);
            Exit;
          End;

        { Situations where delete is okay:
          1. Non-last row
          2. Last row and it is not empty }

        { Does user want to delete the last row? }
        If (DeletedRow < LastRow) Then
          Begin

            { No.  Move the rows up by one. }

            For Inx := succ(DeletedRow) To lastRow Do
              CopyRow(Inx, pred(Inx));

            { Get rid of the last row. }
            RowCount := RowCount - 1;

          End
        Else If (Not LastEmpty) Then
          { Yes, user wants to delete last row and it is not empty. }
          BlankRow(Row);

      Finally
        EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfsUserForm.btnPasswordClick(Sender: TObject);
Var
  PwdForm: TfsPwdForm;
  User: String;
Begin
  PwdForm := TfsPwdForm.Create(Application);
  Try
    User := grdUsers.Cells[cnUserID, grdUsers.Row];
    If User = '' Then
      PwdForm.Caption := 'Password for unknown user'
    Else
      PwdForm.Caption := 'Password for ' + User;
    If (PwdForm.ShowModal = mrOK) Then
      grdUsers.Objects[cnUserID, grdUsers.Row] :=
        pointer(FSCalcShStrELFHash(PwdForm.edtFirstTry.Text));
  Finally
    PwdForm.Free;
  End;
End;
{--------}

Procedure TfsUserForm.btnSaveClick(Sender: TObject);
Var
  Inx: Integer;
  errStr: Array[0..127] Of char;
  aResult: TffResult;
  Rights: TffUserRights;
  CanSave: Boolean;
Begin
  CanSave := True;
  With grdUsers Do
    For Inx := 1 To Pred(RowCount - 1) Do
      If AnsiCompareStr(Cells[cnUserID, Inx], 'SystemAdmin') <> 0 Then
        If TffWord32(Objects[cnUserID, Inx]) = UnknownPwdHash Then
          Begin
            CanSave := False;
            Break;
          End;
  If Not CanSave Then
    Begin
      MessageDlg('Please enter a password for each user.', mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    End;

  FEngine.Configuration.UserList.Empty;

  { Xfer the info from the grid to the engine's user list. }
  With grdUsers Do
    For Inx := 1 To pred(RowCount) Do
      Begin
        Rights := [];
        { Do we have a user ID? }
        If Cells[cnUserID, Inx] <> '' Then
          Begin
            If uppercase(Cells[cnUserID, Inx]) = uppercase('SystemAdmin') Then
              Rights := fsc_AdminRights
            Else
              Begin
                If Longint(Objects[cnAdmin, Inx]) = cnTrue Then
                  Include(Rights, arAdmin);

                If Longint(Objects[cnRead, Inx]) = cnTrue Then
                  Include(Rights, arRead);

                If Longint(Objects[cnInsert, Inx]) = cnTrue Then
                  Include(Rights, arInsert);

                If Longint(Objects[cnUpdate, Inx]) = cnTrue Then
                  Include(Rights, arUpdate);

                If Longint(Objects[cnDelete, Inx]) = cnTrue Then
                  Include(Rights, arDelete);

                If Longint(Objects[cnRestruct, Inx]) = cnTrue Then
                  Include(Rights, arRestruct);

                If Longint(Objects[cnDefrag, Inx]) = cnTrue Then
                  Include(Rights, arDefrag);
                If Longint(Objects[cnDeleteTable, Inx]) = cnTrue Then
                  Include(Rights, arDeleteTable);
                If Longint(Objects[cnDeleteBase, Inx]) = cnTrue Then
                  Include(Rights, arDeleteBase);
                If Longint(Objects[cnUpdateBase, Inx]) = cnTrue Then
                  Include(Rights, arUpdateBase);
                If Longint(Objects[cnInsertTable, Inx]) = cnTrue Then
                  Include(Rights, arInsertTable);
                If Longint(Objects[cnUpdateTable, Inx]) = cnTrue Then
                  Include(Rights, arUpdateTable);
                If Longint(Objects[cnInsertBase, Inx]) = cnTrue Then
                  Include(Rights, arInsertBase);
                If Longint(Objects[cnReadTable, Inx]) = cnTrue Then
                  Include(Rights, arReadTable);
                If Longint(Objects[cnReadBase, Inx]) = cnTrue Then
                  Include(Rights, arReadBase);

                If Longint(Objects[cnReadBlob, Inx]) = cnTrue Then
                  Include(Rights, arReadBlob);

                If Longint(Objects[cnInsertBlob, Inx]) = cnTrue Then
                  Include(Rights, arInsertBlob);

                If Longint(Objects[cnUpdateBlob, Inx]) = cnTrue Then
                  Include(Rights, arUpdateBlob);

                If Longint(Objects[cnDeleteBlob, Inx]) = cnTrue Then
                  Include(Rights, arDeleteBlob);

                If Longint(Objects[cnCopyTable, Inx]) = cnTrue Then
                  Include(Rights, arCopyTable);

                If Longint(Objects[cnTabSetInc, Inx]) = cnTrue Then
                  Include(Rights, arTabSetInc);

                If Longint(Objects[cnTabEmpty, Inx]) = cnTrue Then
                  Include(Rights, arTabEmpty);

                If Longint(Objects[cnProtectRow, Inx]) = cnTrue Then
                  Include(Rights, arProtectRow);

              End;
            FEngine.Configuration.AddUser(Cells[cnUserID, Inx],
              Cells[cnLastName, Inx],
              Cells[cnFirstName, Inx],
              TffWord32(Objects[cnUserID, Inx]),
              Rights);
          End;
      End;
  If Not FEngine.Configuration.GeneralInfo^.giNoAutoSaveCfg Then
    FEngine.SaveConfiguration;
  ModalResult := mrOk;
End;
{====================================================================}

Procedure TfsUserForm.ToolButton1Click(Sender: TObject);
Begin
  With grdUsers Do
    Begin
      BeginUpdate;
      Try
        Objects[cnAdmin, grdUsers.row] := pointer(cnTrue);
        Objects[cnRead, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsert, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdate, grdUsers.row] := pointer(cnTrue);
        Objects[cnDelete, grdUsers.row] := pointer(cnTrue);
        Objects[cnRestruct, grdUsers.row] := pointer(cnTrue);
        Objects[cnDefrag, grdUsers.row] := pointer(cnTrue);
        Objects[cnDeleteTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnDeleteBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdateBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdateTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnReadTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnReadBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnReadBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdateBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnDeleteBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnCopyTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnTabSetInc, grdUsers.row] := pointer(cnTrue);
        Objects[cnTabEmpty, grdUsers.row] := pointer(cnTrue);
        Objects[cnProtectRow, grdUsers.row] := pointer(cnTrue);

      Finally
        EndUpdate;
      End;
    End;
End;

Procedure TfsUserForm.ToolButton2Click(Sender: TObject);
Begin
  If (grdUsers.Row < 0) Or
    (grdUsers.Cells[cnUserID, grdUsers.Row] = fsc_AdminUserID) Then
    Exit;
  With grdUsers Do
    Begin
      BeginUpdate;
      Try
        Objects[cnAdmin, grdUsers.row] := pointer(cnFalse);
        Objects[cnRead, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsert, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdate, grdUsers.row] := pointer(cnTrue);
        Objects[cnDelete, grdUsers.row] := pointer(cnTrue);
        Objects[cnRestruct, grdUsers.row] := pointer(cnFalse);
        Objects[cnDefrag, grdUsers.row] := pointer(cnFalse);
        Objects[cnDeleteTable, grdUsers.row] := pointer(cnFalse);
        Objects[cnDeleteBase, grdUsers.row] := pointer(cnFalse);
        Objects[cnUpdateBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertTable, grdUsers.row] := pointer(cnFalse);
        Objects[cnUpdateTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertBase, grdUsers.row] := pointer(cnFalse);
        Objects[cnReadTable, grdUsers.row] := pointer(cnTrue);
        Objects[cnReadBase, grdUsers.row] := pointer(cnTrue);
        Objects[cnReadBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnInsertBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnUpdateBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnDeleteBlob, grdUsers.row] := pointer(cnTrue);
        Objects[cnCopyTable, grdUsers.row] := pointer(cnFalse);
        Objects[cnTabSetInc, grdUsers.row] := pointer(cnFalse);
        Objects[cnTabEmpty, grdUsers.row] := pointer(cnFalse);
        Objects[cnProtectRow, grdUsers.row] := pointer(cnFalse);
      Finally
        EndUpdate;
      End;
    End;
End;

End.

