{*********************************************************}
{* Alias dialog and maintenance for server               *}
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

Unit fssrvalas;

{$I FsDEFINE.INC}

Interface

Uses
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
  Windows,
  ToolWin,
  Menus,
  ComCtrls,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  FsLLBase,
  FsLLGrid,
  FsLLUNC,
  fsindexaccess,
  FsSrBDE,
  FsSrTran,
  FsSrCfg,
  fsserverclass;

Type
  TfsAliasForm = Class(TForm)
    pnlAliasPath: TPanel;
    btnCommit: TBitBtn;
    btnCancel: TBitBtn;
    grdAliases: TFSGrid;
    tbMain: TToolBar;
    pbDelete: TToolButton;
    ToolButton2: TToolButton;
    pbBrowse: TToolButton;
    imMain: TImageList;
    mnuMain: TMainMenu;
    mnuAlias: TMenuItem;
    mnuAliasDelete: TMenuItem;
    mnuAliasBrowse: TMenuItem;
    imgChkBoxClear: TImage;
    imgChkBoxSet: TImage;
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnBrowseClick(Sender: TObject);
    Procedure btnCommitClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure grdAliasesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure grdAliasesKeyPress(Sender: TObject; Var Key: Char);
    Procedure grdAliasesKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure grdAliasesExitCell(Sender: TFSGrid; aCol,
      aRow: Integer; Const text: String);
    Procedure grdAliasesSortColumn(Sender: TFSGrid; aCol: Integer);
    Procedure grdAliasesSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure grdAliasesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  Private
    { Private declarations }
    FEngine: TFSServer;
    FLastPath: TffPath;

    Procedure afPopulateColHeaders;
    Procedure afPopulateGrid;
    Procedure afSetColumnWidths; {!!.11}
    Procedure afSetEngine(anEngine: TFSServer);

  Public
    { Public declarations }

    Property ServerEngine: TFSServer Read FEngine Write afSetEngine;
  End;

Var
  fsAliasForm: TfsAliasForm;

Implementation

{$R *.DFM}

Uses
  FileCtrl,
  fssrvbrws,
  fsutil,
  fsLLExcp;

Const
  { Column constants }
  cnAlias = 0;
  cnPath = 1;
  cnCheckSpace = 2; {!!.11}

  { Cell margin constants }
  cnTopMargin = 2;
  cnLeftMargin = 2;

  { Boolean field constants }{!!.11}
  cnTrue = 1;
  cnFalse = 0;

  {== Helper methods ===================================================}

Procedure TfsAliasForm.afSetEngine(anEngine: TFSServer);
Begin
  FEngine := anEngine;

  { Set the row count. }
  FEngine.Configuration.AliasList.BeginRead;
  Try
    grdAliases.RowCount := FEngine.Configuration.AliasList.Count + 2;
  Finally
    FEngine.Configuration.AliasList.EndRead;
  End;
  grdAliases.Row := 1;
End;
{=====================================================================}

{== Grid methods & event handlers ====================================}

Procedure TfsAliasForm.afPopulateColHeaders;
Begin
  With grdAliases Do
    Begin
      BeginUpdate;
      Try
        Cells[cnAlias, 0] := 'Alias';
        Cells[cnPath, 0] := 'Folder';
        Cells[cnCheckSpace, 0] := 'Check size'; {!!.11}
      Finally
        EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfsAliasForm.afPopulateGrid;
Var
  AliasItem: TfsAliasItem;
  Inx: Integer;
Begin
  With grdAliases Do
    Begin
      BeginUpdate;
      FEngine.Configuration.AliasList.BeginRead;
      Try
        For Inx := 1 To FEngine.Configuration.AliasList.Count Do
          Begin
            AliasItem := FEngine.Configuration.AliasList[pred(Inx)];
            Cells[cnAlias, Inx] := AliasItem.Alias;
            Cells[cnPath, Inx] := AliasItem.Path;
            If (AliasItem.CheckSpace) Then {!!.11 - Start}
              Objects[cnCheckSpace, Inx] := Pointer(cnTrue)
            Else
              Objects[cnCheckSpace, Inx] := Pointer(cnFalse); {!!.11 - End}
          End;
      Finally
        FEngine.Configuration.AliasList.EndRead;
        EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfsAliasForm.grdAliasesDrawCell(Sender: TObject;
  aCol,
  aRow: Integer;
  Rect: TRect;
  State: TGridDrawState);
{!!.11 - Rewritten to add disk space checking option. }
Var
  Grid: TFSGrid Absolute Sender;
  aStr: String;
  aBitmap: Graphics.TBitmap;
  Dest,
    Source: TRect;
Begin

  { Leave fixed portion of the grid alone}
  If gdFixed In State Then
    Exit;

  With Grid Do
    Begin
      If (aCol = cnPath) Then
        Begin
          aStr := Grid.Cells[aCol, aRow];
          Canvas.FillRect(Rect);
          Canvas.TextRect(Rect,
            Rect.Left + cnLeftMargin,
            Rect.Top + cnTopMargin,
            aStr);
        End
      Else If (aCol = cnCheckSpace) Then
        Begin
          If (Longint(Grid.Objects[aCol, aRow]) = cnTrue) Then
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
        End;
    End;
End;
{--------}

Procedure TfsAliasForm.grdAliasesExitCell(Sender: TFSGrid;
  aCol,
  aRow: Integer;
  Const Text: String);
Begin
  If (aCol = cnPath) And (Text <> '') And FFDirectoryExists(Text) Then
    Sender.Cells[aCol, aRow] := FFExpandUNCFileName(Text);
End;
{--------}

Procedure TfsAliasForm.grdAliasesKeyDown(Sender: TObject;
  Var Key: Word;
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
              Col := cnAlias;
            End
          Else
            Begin
              { Is this cell blank? }
              If Cells[Col, Row] = '' Then
                Begin
                  { Yes. Wrap to first row of grid. }
                  Row := 1;
                  Col := cnAlias;
                End
              Else
                Begin
                  { No.  Add a new blank row. }
                  RowCount := RowCount + 1;
                  Row := Pred(RowCount);
                  Col := cnAlias;
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
              { No.  Make sure we have a new blank row. }
              RowCount := RowCount + 1;
        End;
    VK_UP, VK_TAB:
      With Grid Do
        Begin
          { Are we trying to arrow up from or Tab forward out of a new,
            completed row? }
          If ((Row = Pred(RowCount)) And {!!.11 - Start}
            (Cells[cnAlias, Row] <> '') And
            (Cells[cnPath, Row] <> '')) Then {!!.11 - End}
            { Yes.  Add a new blank row. }
            RowCount := RowCount + 1;
        End;
  End; { case }
End;
{--------}

Procedure TfsAliasForm.grdAliasesKeyPress(Sender: TObject;
  Var Key: Char);
Const
  ValidEditKeys = [#8, #13];
Var
  Grid: TFSGrid Absolute Sender;
  Ignore: Boolean;
  Value: String;
Begin
  If Not (Key In ValidEditKeys) Then
    Begin
      { Validate data entry as key's are pressed}
      Case Grid.Col Of
        cnAlias:
          Begin
            Value := Grid.Cells[cnAlias, Grid.Row];
            Ignore := (Length(Value) >= fscl_GeneralNameSize);
          End;

        cnPath:
          Begin
            Value := Grid.Cells[cnPath, Grid.Row];
            Ignore := (Length(Value) >= fscl_Path)
          End;

        cnCheckSpace: {!!.11 - Start}
          Begin
            Ignore := (Key <> ' ');
            If (Not Ignore) Then
              Begin
                If (Longint(Grid.Objects[Grid.Col, Grid.Row]) = cnTrue) Then
                  Grid.Objects[Grid.Col, Grid.Row] := Pointer(cnFalse)
                Else
                  Grid.Objects[Grid.Col, Grid.Row] := Pointer(cnTrue);
              End;
          End; {!!.11 - End}
        Else
          Ignore := False;
      End;
      If Ignore Then
        Begin
          Key := #0;
          MessageBeep(0);
        End;
    End;
End;
{--------}

Procedure TfsAliasForm.grdAliasesSelectCell(Sender: TObject;
  aCol,
  aRow: Integer;
  Var CanSelect: Boolean);
Var
  Grid: TFSGrid Absolute Sender;
Begin
  CanSelect := True; {!!.11 - Start}
  {if we're on the checkspace column, no editing}
  If (aCol >= cnCheckSpace) Then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
  Else {otherwise allow editing}
    Grid.Options := Grid.Options + [goEditing]; {!!.11 - End}

  pbBrowse.Enabled := (ACol = cnPath);
  mnuAliasBrowse.Enabled := pbBrowse.Enabled;
End;
{--------}

Procedure TfsAliasForm.grdAliasesSortColumn(Sender: TFSGrid;
  aCol: Integer);
Var
  aStr: String;
  i, j: Integer;
  LastRow: Integer;
Begin
  If (Sender.RowCount > 1) Then
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

{=====================================================================}

{== Button methods ===================================================}

Procedure TfsAliasForm.btnBrowseClick(Sender: TObject);
Var
  BrowseForm: TfsDirBrowseForm;
Begin
  BrowseForm := TfsDirBrowseForm.Create(Application);
  Try
    If fsDirectoryExists(GrdAliases.Cells[cnPath, grdAliases.Row]) Then
      BrowseForm.dirBox.Directory := GrdAliases.Cells[cnPath, grdAliases.Row]
    Else If (FLastPath <> '') And fsDirectoryExists(FLastPath) Then
      BrowseForm.dirBox.Directory := FLastPath;
    If (BrowseForm.ShowModal = mrOK) Then
      With grdAliases Do
        Begin
          FLastPath := BrowseForm.dirBox.Directory;
          BeginUpdate;
          Try
            Cells[cnPath, Row] := FFExpandUNCFileName(BrowseForm.DirBox.Directory);
          Finally
            EndUpdate;
          End;
        End;
  Finally
    BrowseForm.Free;
  End; {try..finally}
End;
{--------}

Procedure TfsAliasForm.btnCommitClick(Sender: TObject);
Var
  Inx: Integer;
  CheckSpace: Boolean; {!!.11}
Begin
  { Get rid of the aliases. }
  FEngine.Configuration.AliasList.BeginWrite;
  Try
    FEngine.Configuration.AliasList.Empty;

    { Xfer the info from the grid to the server engine's alias list. }
    For Inx := 1 To pred(grdAliases.RowCount) Do
      Begin
        If ((grdAliases.Cells[cnAlias, Inx] <> '') And {!!.11 - Start}
          (grdAliases.Cells[cnPath, Inx] <> '')) Then
          Begin
            CheckSpace := (Longint(grdAliases.Objects[cnCheckSpace, Inx]) = cnTrue);
            FEngine.Configuration.AddAlias(grdAliases.Cells[cnAlias, Inx],
              grdAliases.Cells[cnPath, Inx],
              CheckSpace);
          End; {!!.11 - End}
      End;

  Finally
    FEngine.Configuration.AliasList.EndWrite;
    If Not FEngine.Configuration.GeneralInfo^.giNoAutoSaveCfg Then
      FEngine.SaveConfiguration;
    ModalResult := mrOk;
  End;
End;
{--------}

Procedure TfsAliasForm.btnDeleteClick(Sender: TObject);
Var
  DeletedRow: Integer;
  Inx: Integer;
  LastEmpty: Boolean;
  LastRow: Integer;
Begin

  If (grdAliases.RowCount < 2) Then
    Exit;

  With grdAliases Do
    Begin
      BeginUpdate;
      Try
        DeletedRow := Row;
        LastRow := pred(RowCount);
        LastEmpty := LastRowIsEmpty;

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
{=====================================================================}

{== Form methods =====================================================}

Procedure TfsAliasForm.FormShow(Sender: TObject);
Begin
  afSetColumnWidths; {!!.11}
  afPopulateColHeaders;
  afPopulateGrid;
  grdAliases.SetFocus;
  pbBrowse.Enabled := False;
  mnuAliasBrowse.Enabled := pbBrowse.Enabled;
  FLastPath := '';
End;
{=====================================================================}

{!!.11 - New }

Procedure TfsAliasForm.afSetColumnWidths;
Begin
  With grdAliases Do
    Begin
      ColWidths[cnAlias] := 100;
      ColWidths[cnCheckSpace] := 75;
      ColWidths[cnPath] := grdAliases.ClientWidth - 181;
    End;
End;

{!!.11 - New}

Procedure TfsAliasForm.grdAliasesMouseUp(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
Var
  aCol,
    aRow: Longint;
  Rect,
    Dest: TRect;
  Grid: TFSGrid Absolute Sender;
Begin
  If (Button = mbLeft) Then
    Begin
      Grid.MouseToCell(X, Y, aCol, aRow);
      If (aRow >= 0) Then
        Begin
          If (aCol = cnCheckSpace) Then
            Begin
              Rect := Grid.CellRect(aCol, aRow);
              { Retrieve the rect from around the box itself}
              If (Longint(grdAliases.Objects[cnCheckSpace, aRow]) = cnTrue) Then
                With imgChkBoxSet.Picture Do
                  Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
                    Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) Div 2,
                    Bitmap.Width,
                    Bitmap.Height)
              Else
                With imgChkBoxClear.Picture Do
                  Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
                    Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) Div 2,
                    Bitmap.Width,
                    Bitmap.Height);

              { Only manipuate the checkbox state if an area on or within the rect was
                clicked}
              If (X >= Dest.Left) And (X <= Dest.Right) And
                (Y >= Dest.Top) And (Y <= Dest.Bottom) Then
                Begin
                  If (Longint(Grid.Objects[aCol, aRow]) = cnTrue) Then
                    Grid.Objects[aCol, aRow] := Pointer(cnFalse)
                  Else
                    Grid.Objects[aCol, aRow] := Pointer(cnTrue);
                End;
            End;
        End;
    End;
End;

End.

