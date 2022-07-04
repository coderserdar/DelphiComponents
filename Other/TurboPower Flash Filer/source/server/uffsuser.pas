{*********************************************************}
{* User maintenance for server                           *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit uFFSUser;

{$I FFDEFINE.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, ComCtrls, ToolWin, Menus,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  FFLLBase,
  FFLLUNC,
  FFHash,
  FFTbDict,
  FFSrBase,
  FFSrBDE,
  FFSrTran,
  FFSrCfg,
  FFSrEng,
  ffllgrid;

type
  TFFUserForm = class(TForm)
    pnlLower: TPanel;
    btnSave: TBitBtn;
    btnDiscard: TBitBtn;
    grdUsers: TffStringGrid;
    imgChkBoxClear: TImage;
    imgChkBoxSet: TImage;
    tbMain: TToolBar;
    pbDelete: TToolButton;
    ToolButton2: TToolButton;
    pbPassword: TToolButton;
    mnuMain: TMainMenu;
    mnuUser: TMenuItem;
    mnuUserDelete: TMenuItem;
    mnuUserPassword: TMenuItem;
    imgMain: TImageList;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnPasswordClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdUsersDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdUsersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdUsersKeyPress(Sender: TObject; var Key: Char);
    procedure grdUsersSortColumn(Sender: TffStringGrid; aCol: Integer);
    procedure grdUsersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdUsersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FEngine      : TffServerEngine;

    procedure ufPopulateColHeaders;
    procedure ufPopulateGrid;
    procedure ufSetEngine(anEngine : TffServerEngine);

  public
    property ServerEngine : TffServerEngine read FEngine write ufSetEngine;
  end;

var
  FFUserForm: TFFUserForm;

implementation

uses
  uFFSPwd,
  FFLLExcp;

{$R *.DFM}

const
  UnknownPwdHash = $FFFFFFFF;

  { Column constants }
  cnUserID     = 0;
  cnLastName   = 1;
  cnFirstName  = 2;
  cnAdmin      = 3;
  cnRead       = 4;
  cnInsert     = 5;
  cnUpdate     = 6;
  cnDelete     = 7;

  { Cell margin constants }
  cnTopMargin = 2;
  cnLeftMargin = 2;

  { Boolean field constants }
  cnTrue = 1;
  cnFalse = 0;

{===Helper methods===================================================}
procedure TFFUserForm.ufSetEngine(anEngine : TffServerEngine);
begin
  FEngine := anEngine;

  { Set the row count. }
  grdUsers.RowCount := FEngine.Configuration.UserList.Count + 2;
  grdUsers.Row := 1;
end;
{====================================================================}


{===Grid methods & event handlers====================================}
procedure TFFUserForm.FormShow(Sender: TObject);
begin
  ufPopulateColHeaders;
  ufPopulateGrid;
  grdUsers.SetFocus;
  { Psition to the 1st non-admin user row. }
  with grdUsers do begin
    Col := cnUserID;
    { Is the first row the Admin user? }
    if Cells[cnUserID, 1] = ffc_AdminUserID then
      { Yes. Position to subsequent row. }
      Row := 2
    else
      { No. Position to the first row. }
      Row := 1;
  end;
end;
{--------}
procedure TFFUserForm.grdUsersDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  aBitmap: TBitmap;
  aStr : string;
  Grid : TffStringGrid absolute Sender;
  Dest, Source: TRect;
begin

  { Leave fixed portion of the grid alone}
  if gdFixed in State then
    Exit;

  { Is this a boolean field? }
  if aCol >= cnAdmin then begin
    if longInt(Grid.Objects[aCol, aRow]) = cnTrue then
      aBitmap := imgChkBoxSet.Picture.Bitmap
    else
      aBitmap := imgChkBoxClear.Picture.Bitmap;
    with Grid.Canvas do begin
      Dest := Bounds(Rect.Left + ((Rect.Right - aBitmap.Width  - Rect.Left) div 2),
                     Rect.Top + (Grid.DefaultRowHeight - aBitmap.Height) div 2,
                     aBitmap.Width,
                     aBitmap.Height);
      Source := Bounds(0, 0, aBitmap.Width, aBitmap.Height);
      BrushCopy(Dest,
                aBitmap,
                Source,
                aBitmap.TransparentColor);
    end;
  end else begin
    { No.  Draw the text. }
    aStr := Grid.Cells[aCol, aRow];

    with Grid do begin
      if (aCol = cnUserID) and
         (Cells[cnUserID, aRow] <> '') and
         (TffWord32(Objects[cnUserID, aRow]) = UnknownPwdHash) then begin
        Canvas.Brush.Color := clRed;
        Canvas.Font.Color := clWhite;
      end;
      Canvas.FillRect(Rect);
      Canvas.TextRect(Rect, Rect.Left + cnLeftMargin, Rect.Top + cnTopMargin, aStr);
    end;
  end;

end;
{--------}
procedure TFFUserForm.grdUsersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Grid : TffStringGrid absolute Sender;
begin
  { Change the selected cell (Enter as tab)}
  case Key of
    VK_RETURN :
      with Grid do begin
        if Col < Pred(ColCount) then
          Col := Col + 1
        else if Row < Pred(RowCount) then begin
          Row := Row + 1;
          Objects[cnUserID, Row] := pointer(UnknownPwdHash);
          Col := cnUserID;
        end else begin
          { Is this cell blank? }
          if Cells[Col, Row] = '' then begin
            { Yes. Wrap to first row of grid. }
            Row := 1;
            Col := cnUserID;
          end else begin
            { No.  Add a new blank row. }
            RowCount := RowCount + 1;
            Row := Pred(RowCount);
            Col := cnUserID;
            Objects[cnUserID, Row] := pointer(UnknownPwdHash);
          end;
        end;
      end;
    VK_DOWN :
      with Grid do begin
        { Are we trying to arrow down from an incomplete row? }
        if (Row = pred(RowCount)) then
          if AnyCellIsEmpty(Row) then begin
            { Yes.  Do not allow this to occur. }
            Key := 0;
            MessageBeep(0);
          end else begin
            { No.  Make sure we have a new blank row. }
            RowCount := RowCount + 1;
            Objects[cnUserID, pred(RowCount)] := pointer(UnknownPwdHash);
          end;
      end;
    VK_UP, VK_TAB :
      with Grid do begin
        { Are we trying to arrow up from or Tab forward out of a new,
          completed row? }
        if (Row = pred(RowCount)) and (Cells[cnUserID, Row] <> '') then
          { Yes.  Add a new blank row. }
          RowCount := RowCount + 1;
          Objects[cnUserID, pred(RowCount)] := pointer(UnknownPwdHash);
      end;
  end;  { case }
end;
{--------}
procedure TFFUserForm.grdUsersKeyPress(Sender: TObject; var Key: Char);
const
  validEditKeys = [#8, #9, #13];
var
  Grid : TffStringGrid absolute Sender;
  Ignore: Boolean;
  Value: string;
begin
  if not (Key in validEditKeys) then begin
    { Validate data entry as key's are pressed}
    case Grid.Col of
      cnUserID, cnLastName, cnFirstName :
        begin
          Value := Grid.Cells[Grid.Col, Grid.Row];
          Ignore := (Length(Value) >= ffcl_UserNameSize);
        end;
      cnAdmin, cnRead, cnInsert, cnUpdate, cnDelete :
        begin
          Ignore :=
             (Grid.Cells[cnUserID, Grid.Row] = ffc_AdminUserID) or
             (Key <> ' ');
          if not Ignore then begin
            if longInt(Grid.Objects[Grid.Col, Grid.Row]) = cnTrue then
              Grid.Objects[Grid.Col, Grid.Row] := pointer(cnFalse)
            else
              Grid.Objects[Grid.Col, Grid.Row] := pointer(cnTrue);
          end;
        end;
    else
      Ignore := False;
    end;  { case }
    if Ignore then begin
      Key := #0;
      MessageBeep(0);
    end;
  end;
end;
{--------}
procedure TFFUserForm.grdUsersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  Rect, Dest : TRect;
  Grid : TffStringGrid absolute Sender;
begin

  if Button <> mbLeft then Exit;


  Grid.MouseToCell(X, Y, ACol, ARow);
  { If this is not a valid row or if this is the Admin user then exit.  We do
    the latter because the Admin user may not be modified. }
  if (ARow < 0) or
     (Grid.Cells[cnUserID, ARow] = ffc_AdminUserID) then
    exit;

  { Is this a rights cell? }
  if ACol >= cnAdmin then begin
    Rect := Grid.CellRect(ACol, ARow);
    with imgChkBoxSet.Picture do
      { Retrieve the rect from around the box itself}
      Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                     Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) div 2,
                     Bitmap.Width,
                     Bitmap.Height);

    { Only manipuate the checkbox state if an area on or within the rect was
      clicked}
    if (X >= Dest.Left) and (X <= Dest.Right) and
       (Y >= Dest.Top) and (Y <= Dest.Bottom) then begin
      if longInt(Grid.Objects[aCol, aRow]) = cnTrue then
        Grid.Objects[aCol, aRow] := pointer(cnFalse)
      else
        Grid.Objects[aCol, aRow] := pointer(cnTrue);
    end;
  end;
end;
{--------}
procedure TFFUserForm.grdUsersSelectCell(Sender     : TObject;
                                         ACol, ARow : Integer;
                                     var CanSelect  : Boolean);
var
  Grid : TffStringGrid absolute Sender;
begin
  CanSelect := true;
  {if we're on the administrator row, no editing}
  if (Grid.Cells[cnUserID, ARow] = ffc_AdminUserID) then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
  {if we're in a column with checkboxes, no editing}
  else if ACol >= cnAdmin then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
  {otherwise allow editing}
  else
    Grid.Options := Grid.Options + [goEditing];
  if (ARow = pred(Grid.RowCount)) and
     (Grid.Cells[cnUserID, ARow] <> '') then begin
    { Yes.  Add a new blank row. }
    Grid.RowCount := Grid.RowCount + 1;
    Grid.Objects[cnUserID, Pred(Grid.RowCount)] := pointer(UnknownPwdHash);
  end;
end;
{--------}
procedure TFFUserForm.grdUsersSortColumn(Sender: TffStringGrid;
  aCol: Integer);
var
  aStr : string;
  i, j : integer;
  LastRow : integer;
begin
  if (Sender.RowCount > 1) and (aCol < cnAdmin) then
    with Sender do begin

      if LastRowIsEmpty then
        LastRow := (RowCount - 2)
      else
        LastRow := pred(RowCount);

      BeginUpdate;
      try
        for i := 1 to LastRow do begin
          SaveRow(i);
          aStr := Cells[aCol, i];
          j := i;
          while  (j > 1) and
                 (ansiCompareStr(Cells[aCol, j-1], aStr) > 0) do begin
            CopyRow(j-1, j);
            dec(j);
          end;  { while }
          RestoreToRow(j);
        end;  { for }
      finally
        EndUpdate;
       end;
    end;  { with }
end;
{--------}
procedure TFFUserForm.ufPopulateColHeaders;
begin
  with grdUsers do begin
    BeginUpdate;
    try
      Cells[cnUserID,    0] := 'User ID';
      Cells[cnLastName,  0] := 'Last Name';
      Cells[cnFirstName, 0] := 'First Name';
      Cells[cnAdmin,     0] := 'Admin';
      Cells[cnRead,      0] := 'Read';
      Cells[cnInsert,    0] := 'Insert';
      Cells[cnUpdate,    0] := 'Update';
      Cells[cnDelete,    0] := 'Delete';
    finally
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TffUserForm.ufPopulateGrid;
var
  Item : TffUserItem;
  Inx : integer;
begin
  with grdUsers do begin
    BeginUpdate;
    try
      for Inx := 1 to FEngine.Configuration.UserList.Count do begin
        Item := FEngine.Configuration.UserList[pred(Inx)];
        Cells[cnUserID, Inx] := Item.UserID;
        Objects[cnUserID, Inx] := pointer(Item.PasswordHash);
        Cells[cnLastName, Inx] := Item.LastName;
        Cells[cnFirstName, Inx] := Item.FirstName;
        if arAdmin in Item.Rights then
          Objects[cnAdmin, Inx] := pointer(cnTrue)
        else
          Objects[cnAdmin, Inx] := pointer(cnFalse);

        if arRead in Item.Rights then
          Objects[cnRead, Inx] := pointer(cnTrue)
        else
          Objects[cnRead, Inx] := pointer(cnFalse);

        if arInsert in Item.Rights then
          Objects[cnInsert, Inx] := pointer(cnTrue)
        else
          Objects[cnInsert, Inx] := pointer(cnFalse);

        if arUpdate in Item.Rights then
          Objects[cnUpdate, Inx] := pointer(cnTrue)
        else
          Objects[cnUpdate, Inx] := pointer(cnFalse);

        if arDelete in Item.Rights then
          Objects[cnDelete, Inx] := pointer(cnTrue)
        else
          Objects[cnDelete, Inx] := pointer(cnFalse);

      end;
    finally
      EndUpdate;
    end;
  end;
end;
{====================================================================}


{===Button methods===================================================}
procedure TFFUserForm.btnDeleteClick(Sender: TObject);
var
  DeletedRow : integer;
  Inx : integer;
  LastEmpty : boolean;
  LastRow : integer;
begin

  if (grdUsers.RowCount < 2) then
    Exit;

  with grdUsers do begin
    BeginUpdate;
    try
      DeletedRow := Row;
      LastRow := pred(RowCount);
      LastEmpty := LastRowIsEmpty;

      { Situation where delete in not okay
        1. When the row represents the admin user }

      if UpperCase(Cells[cnUserID, DeletedRow]) = 'ADMIN' then begin
        MessageBeep(0);
        Exit;
      end;  

      { Situations where delete is okay:
        1. Non-last row
        2. Last row and it is not empty }

      { Does user want to delete the last row? }
      if (DeletedRow < LastRow) then begin

        { No.  Move the rows up by one. }

        for Inx := succ(DeletedRow) to lastRow do
          CopyRow(Inx, pred(Inx));

        { Get rid of the last row. }
        RowCount := RowCount - 1;

      end else if (not LastEmpty) then
        { Yes, user wants to delete last row and it is not empty. }
        BlankRow(Row);

    finally
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TFFUserForm.btnPasswordClick(Sender: TObject);
var
  PwdForm : TPwdForm;
  User    : string;
begin
  PwdForm := TPwdForm.Create(Application);
  try
    User := grdUsers.Cells[cnUserID, grdUsers.Row];
    if User = '' then
      PwdForm.Caption := 'Password for unknown user'
    else
      PwdForm.Caption := 'Password for ' + User;
    if (PwdForm.ShowModal = mrOK) then
      grdUsers.Objects[cnUserID, grdUsers.Row] :=
        pointer(FFCalcShStrELFHash(PwdForm.edtFirstTry.Text));
  finally
    PwdForm.Free;
  end;
end;
{--------}
procedure TFFUserForm.btnSaveClick(Sender: TObject);
var
  Inx     : integer;
  errStr : array [0..127] of char;
  aResult : TffResult;
  Rights  : TffUserRights;
  CanSave : Boolean;
begin
  CanSave := True;
  with grdUsers do
    for Inx := 1 to Pred(RowCount - 1) do
      if AnsiCompareStr(Cells[cnUserID, Inx], 'admin') <> 0  then
        if TffWord32(Objects[cnUserID, Inx]) = UnknownPwdHash then begin
          CanSave := False;
          Break;
        end;
  if not CanSave then begin
    MessageDlg('Please enter a password for each user.', mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;

  FEngine.Configuration.UserList.Empty;

  { Xfer the info from the grid to the engine's user list. }
  with grdUsers do
    for Inx := 1 to pred(RowCount) do begin
      Rights := [];
      { Do we have a user ID? }
      if Cells[cnUserID, Inx] <> '' then begin
        if longInt(Objects[cnAdmin, Inx]) = cnTrue then
          Include(Rights, arAdmin);

        if longInt(Objects[cnRead, Inx]) = cnTrue then
          Include(Rights, arRead);

        if longInt(Objects[cnInsert, Inx]) = cnTrue then
          Include(Rights, arInsert);

        if longInt(Objects[cnUpdate, Inx]) = cnTrue then
          Include(Rights, arUpdate);

        if longInt(Objects[cnDelete, Inx]) = cnTrue then
          Include(Rights, arDelete);

        FEngine.Configuration.AddUser(Cells[cnUserID, Inx],
                                      Cells[cnLastName, Inx],
                                      Cells[cnFirstName, Inx],
                                      TffWord32(Objects[cnUserID, Inx]),
                                      Rights);
      end;
    end;

  aResult := FEngine.WriteUserData;
  if aResult <> DBIERR_NONE then begin
    ffStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
    showMessage(format('Could not save users: %s [$%x/%d])',
                       [strPas(errStr), aResult, aResult]));
    self.modalResult := mrNone;
  end;
end;
{====================================================================}

end.
