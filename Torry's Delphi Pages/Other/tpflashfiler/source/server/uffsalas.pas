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

unit uFFSAlas;

{$I FFDEFINE.INC}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, Windows, ToolWin, Menus,
  ComCtrls, 
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  FFLLBase,
  FFLLGrid,
  FFLLUNC,
  FFTbDict,
  FFSrBDE,
  FFSrTran,
  FFSrCfg,
  FFSrEng;

type
  TFFAliasForm = class(TForm)
    pnlAliasPath: TPanel;
    btnCommit: TBitBtn;
    btnCancel: TBitBtn;
    grdAliases: TffStringGrid;
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
    procedure btnDeleteClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdAliasesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdAliasesKeyPress(Sender: TObject; var Key: Char);
    procedure grdAliasesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdAliasesExitCell(Sender: TffStringGrid; aCol,
      aRow: Integer; const text: String);
    procedure grdAliasesSortColumn(Sender: TffStringGrid; aCol: Integer);
    procedure grdAliasesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure grdAliasesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FEngine      : TffServerEngine;
    FLastPath    : TffPath;

    procedure afPopulateColHeaders;
    procedure afPopulateGrid;
    procedure afSetColumnWidths;                                       {!!.11}
    procedure afSetEngine(anEngine : TffServerEngine);

  public
    { Public declarations }

    property ServerEngine : TffServerEngine read FEngine write afSetEngine;
  end;

var
  FFAliasForm: TFFAliasForm;

implementation

{$R *.DFM}

uses
  FileCtrl,
  uFFSBrws,
  ffLLExcp;

const
  { Column constants }
  cnAlias = 0;
  cnPath  = 1;
  cnCheckSpace = 2;                                                    {!!.11}

  { Cell margin constants }
  cnTopMargin = 2;
  cnLeftMargin = 2;

  { Boolean field constants }                                          {!!.11}
  cnTrue = 1;
  cnFalse = 0;

{== Helper methods ===================================================}
procedure TFFAliasForm.afSetEngine(anEngine : TffServerEngine);
begin
  FEngine := anEngine;

  { Set the row count. }
  FEngine.Configuration.AliasList.BeginRead;
  try
    grdAliases.RowCount := FEngine.Configuration.AliasList.Count + 2;
  finally
    FEngine.Configuration.AliasList.EndRead;
  end;
  grdAliases.Row := 1;
end;
{=====================================================================}

{== Grid methods & event handlers ====================================}
procedure TFFAliasForm.afPopulateColHeaders;
begin
  with grdAliases do begin
    BeginUpdate;
    try
      Cells[cnAlias, 0] := 'Alias';
      Cells[cnPath,  0] := 'Path';
      Cells[cnCheckSpace, 0] := 'Check space';                         {!!.11}
    finally
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TffAliasForm.afPopulateGrid;
var
  AliasItem : TffAliasItem;
  Inx       : Integer;
begin
  with grdAliases do begin
    BeginUpdate;
    FEngine.Configuration.AliasList.BeginRead;
    try
      for Inx := 1 to FEngine.Configuration.AliasList.Count do begin
        AliasItem := FEngine.Configuration.AliasList[pred(Inx)];
        Cells[cnAlias, Inx] := AliasItem.Alias;
        Cells[cnPath,  Inx] := AliasItem.Path;
        if (AliasItem.CheckSpace) then                                 {!!.11 - Start}
          Objects[cnCheckSpace, Inx] := Pointer(cnTrue)
        else
          Objects[cnCheckSpace, Inx] := Pointer(cnFalse);              {!!.11 - End}
      end;
    finally
      FEngine.Configuration.AliasList.EndRead;
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TFFAliasForm.grdAliasesDrawCell(Sender : TObject;
                                          aCol,
                                          aRow   : Integer;
                                          Rect   : TRect;
                                          State  : TGridDrawState);
{!!.11 - Rewritten to add disk space checking option. }
var
  Grid    : TffStringGrid absolute Sender;
  aStr    : string;
  aBitmap : Graphics.TBitmap;
  Dest,
  Source  : TRect;
begin

  { Leave fixed portion of the grid alone}
  if gdFixed in State then
    Exit;

  with Grid do begin
    if (aCol = cnPath) then begin
      aStr := Grid.Cells[aCol, aRow];
      if (aStr <> '') and (not FFDirectoryExists(aStr)) then begin
        Canvas.Brush.Color := clRed;
        Canvas.Font.Color := clWhite;
      end;
      Canvas.FillRect(Rect);
      Canvas.TextRect(Rect,
                      Rect.Left + cnLeftMargin,
                      Rect.Top + cnTopMargin,
                      aStr);
    end else if (aCol = cnCheckSpace) then begin
      if (Longint(Grid.Objects[aCol, aRow]) = cnTrue) then
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
    end;                                                               
  end;
end;
{--------}
procedure TFFAliasForm.grdAliasesExitCell(Sender : TffStringGrid;
                                          aCol,
                                          aRow   : Integer;
                                    const Text   : string);
begin
  if (aCol = cnPath) and (Text <> '') and FFDirectoryExists(Text) then
    Sender.Cells[aCol, aRow] := FFExpandUNCFileName(Text);
end;
{--------}
procedure TFFAliasForm.grdAliasesKeyDown(Sender : TObject;
                                     var Key    : Word;
                                         Shift  : TShiftState);
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
          Col := cnAlias;
        end else begin
          { Is this cell blank? }
          if Cells[Col, Row] = '' then begin
            { Yes. Wrap to first row of grid. }
            Row := 1;
            Col := cnAlias;
          end else begin
            { No.  Add a new blank row. }
            RowCount := RowCount + 1;
            Row := Pred(RowCount);
            Col := cnAlias;
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
          end else
            { No.  Make sure we have a new blank row. }
            RowCount := RowCount + 1;
      end;
    VK_UP, VK_TAB :
      with Grid do begin
        { Are we trying to arrow up from or Tab forward out of a new,
          completed row? }
        if ((Row = Pred(RowCount)) and                                 {!!.11 - Start}
            (Cells[cnAlias, Row] <> '') and
            (Cells[cnPath, Row] <> '')) then                           {!!.11 - End}
          { Yes.  Add a new blank row. }
          RowCount := RowCount + 1;
      end;
  end;  { case }
end;
{--------}
procedure TFFAliasForm.grdAliasesKeyPress(Sender : TObject;
                                      var Key    : Char);
const
  ValidEditKeys = [#8, #13];
var
  Grid   : TffStringGrid absolute Sender;
  Ignore : Boolean;
  Value  : string;
begin
  if not (Key in ValidEditKeys) then begin
    { Validate data entry as key's are pressed}
    case Grid.Col of
      cnAlias:
        begin
          Value := Grid.Cells[cnAlias, Grid.Row];
          Ignore := (Length(Value) >= ffcl_GeneralNameSize);
        end;

      cnPath:
        begin
          Value := Grid.Cells[cnPath, Grid.Row];
          Ignore := (Length(Value) >= ffcl_Path)
        end;

      cnCheckSpace:                                                    {!!.11 - Start}
        begin
          Ignore := (Key <> ' ');
          if (not Ignore) then begin
            if (Longint(Grid.Objects[Grid.Col, Grid.Row]) = cnTrue) then
              Grid.Objects[Grid.Col, Grid.Row] := Pointer(cnFalse)
            else
              Grid.Objects[Grid.Col, Grid.Row] := Pointer(cnTrue);
          end;
        end;                                                           {!!.11 - End}
    else
      Ignore := False;
    end;
    if Ignore then begin
      Key := #0;
      MessageBeep(0);
    end;
  end;
end;
{--------}
procedure TFFAliasForm.grdAliasesSelectCell(Sender    : TObject;
                                            aCol,
                                            aRow      : Integer;
                                        var CanSelect : Boolean);
var
  Grid : TffStringGrid absolute Sender;
begin
  CanSelect := True;                                                   {!!.11 - Start}
  {if we're on the checkspace column, no editing}
  if (aCol >= cnCheckSpace) then
    Grid.Options := Grid.Options - [goAlwaysShowEditor, goEditing]
  else   {otherwise allow editing}
    Grid.Options := Grid.Options + [goEditing];                        {!!.11 - End}

  pbBrowse.Enabled := (ACol = cnPath);
  mnuAliasBrowse.Enabled := pbBrowse.Enabled;
end;
{--------}
procedure TFFAliasForm.grdAliasesSortColumn(Sender : TffStringGrid;
                                            aCol   : Integer);
var
  aStr    : string;
  i, j    : Integer;
  LastRow : Integer;
begin
  if (Sender.RowCount > 1) then
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

{=====================================================================}

{== Button methods ===================================================}
procedure TFFAliasForm.btnBrowseClick(Sender : TObject);
var
  BrowseForm : TDirBrowseForm;
begin
  BrowseForm := TDirBrowseForm.Create(Application);
  try
    if DirectoryExists(GrdAliases.Cells[cnPath, grdAliases.Row]) then
      BrowseForm.dirBox.Directory := GrdAliases.Cells[cnPath, grdAliases.Row]
    else if (FLastPath <> '') and DirectoryExists(FLastPath) then
      BrowseForm.dirBox.Directory := FLastPath;
    if (BrowseForm.ShowModal = mrOK) then
      with grdAliases do begin
        FLastPath := BrowseForm.dirBox.Directory;
        BeginUpdate;
        try
          Cells[cnPath, Row] := FFExpandUNCFileName(BrowseForm.DirBox.Directory);
        finally
          EndUpdate;
        end;
      end;
  finally
    BrowseForm.Free;
  end;{try..finally}
end;
{--------}
procedure TFFAliasForm.btnCommitClick(Sender : TObject);
var
  aResult    : TffResult;
  Inx        : Integer;
  errStr     : array [0..127] of Char;
  CheckSpace : Boolean;                                                {!!.11}
begin
  { Get rid of the aliases. }
  FEngine.Configuration.AliasList.BeginWrite;
  try
    FEngine.Configuration.AliasList.Empty;

    { Xfer the info from the grid to the server engine's alias list. }
    for Inx := 1 to pred(grdAliases.RowCount) do begin
      if ((grdAliases.Cells[cnAlias, Inx] <> '') and                   {!!.11 - Start}
          (grdAliases.Cells[cnPath, Inx] <> '')) then begin
        CheckSpace := (Longint(grdAliases.Objects[cnCheckSpace, Inx]) = cnTrue);
        FEngine.Configuration.AddAlias(grdAliases.Cells[cnAlias, Inx],
                                       grdAliases.Cells[cnPath,  Inx],
                                       CheckSpace);
      end;                                                             {!!.11 - End}
    end;

    { Save the aliases. }
    aResult := FEngine.WriteAliasData;
  finally
    FEngine.Configuration.AliasList.EndWrite;
  end;

  if (aResult <> DBIERR_NONE) then begin
    ffStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
    ShowMessage(Format('Could not save aliases: %s [$%x/%d])',
                       [strPas(errStr), aResult, aResult]));
    ModalResult := mrNone;
  end;
end;
{--------}
procedure TFFAliasForm.btnDeleteClick(Sender : TObject);
var
  DeletedRow : Integer;
  Inx        : Integer;
  LastEmpty  : Boolean;
  LastRow    : Integer;
begin

  if (grdAliases.RowCount < 2) then
    Exit;

  with grdAliases do begin
    BeginUpdate;
    try
      DeletedRow := Row;
      LastRow := pred(RowCount);
      LastEmpty := LastRowIsEmpty;

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
{=====================================================================}

{== Form methods =====================================================}
procedure TFFAliasForm.FormShow(Sender : TObject);
begin
  afSetColumnWidths;                                                   {!!.11}
  afPopulateColHeaders;
  afPopulateGrid;
  grdAliases.SetFocus;
  pbBrowse.Enabled := False;
  mnuAliasBrowse.Enabled := pbBrowse.Enabled;
  FLastPath := '';
end;
{=====================================================================}

{!!.11 - New }
procedure TFFAliasForm.afSetColumnWidths;
begin
  with grdAliases do begin
    ColWidths[cnAlias] := 100;
    ColWidths[cnCheckSpace] := 75;
    ColWidths[cnPath] := grdAliases.ClientWidth - 181;
  end;
end;

{!!.11 - New}
procedure TFFAliasForm.grdAliasesMouseUp(Sender : TObject;
                                         Button : TMouseButton;
                                         Shift  : TShiftState;
                                         X, Y   : Integer);
var
  aCol,
  aRow : Longint;
  Rect,
  Dest : TRect;
  Grid : TffStringGrid absolute Sender;
begin
  if (Button = mbLeft) then begin
    Grid.MouseToCell(X, Y, aCol, aRow);
    if (aRow >= 0) then begin
      if (aCol = cnCheckSpace) then begin
        Rect := Grid.CellRect(aCol, aRow);
        { Retrieve the rect from around the box itself}
        if (Longint(grdAliases.Objects[cnCheckSpace, aRow]) = cnTrue) then
          with imgChkBoxSet.Picture do
            Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                           Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) div 2,
                           Bitmap.Width,
                           Bitmap.Height)
        else
          with imgChkBoxClear.Picture do
            Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                           Rect.Top + (Grid.DefaultRowHeight - Bitmap.Height) div 2,
                           Bitmap.Width,
                           Bitmap.Height);

        { Only manipuate the checkbox state if an area on or within the rect was
          clicked}
        if (X >= Dest.Left) and (X <= Dest.Right) and
           (Y >= Dest.Top) and (Y <= Dest.Bottom) then begin
          if (Longint(Grid.Objects[aCol, aRow]) = cnTrue) then
            Grid.Objects[aCol, aRow] := Pointer(cnFalse)
          else
            Grid.Objects[aCol, aRow] := Pointer(cnTrue);
        end;
      end;
    end;
  end;
end;

end.
