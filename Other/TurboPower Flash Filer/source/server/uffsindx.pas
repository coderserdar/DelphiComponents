{*********************************************************}
{* User-defined index dialog and maintenance for server  *}
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

unit uFFSIndx;

{$I FFDEFINE.INC}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Windows, Grids, ComCtrls, ToolWin, Menus,
  {$IFDEF DCC4ORLATER}
  ImgList,
  {$ENDIF}
  FFLLBase,
  ffllgrid,
  FFSrEng;

type
  TffKeyProcItemRec = class(TffUCStrListItem)
    kirTable   : TffFullFileName;
    kirIndexID : longint;                                             
    kirDLL     : TffFullFileName;
    kirBuildKey: TffName;
    kirCompKey : TffName;
    constructor Create;
  end;

type
  TFFIndexForm = class(TForm)
    pnlBottom: TPanel;
    btnSave: TBitBtn;
    btnDiscard: TBitBtn;
    dlgOpenTable: TOpenDialog;
    dlgOpenDLL: TOpenDialog;
    grdIndexes: TffStringGrid;
    tbMain: TToolBar;
    pbDelete: TToolButton;
    ToolButton2: TToolButton;
    pbBrowse: TToolButton;
    imMain: TImageList;
    mnuMain: TMainMenu;
    mnuIndex: TMenuItem;
    mnuIndexBrowse: TMenuItem;
    mnuIndexDelete: TMenuItem;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grdIndexesExitCell(Sender: TffStringGrid; aCol,
      aRow: Integer; const text: String);
    procedure grdIndexesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdIndexesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure grdIndexesKeyPress(Sender: TObject; var Key: Char);
    procedure grdIndexesSortColumn(Sender: TffStringGrid; aCol: Integer);
    procedure grdIndexesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    FEngine      : TffServerEngine;

    procedure ifPopulateColHeaders;
    procedure ifPopulateGrid;
    procedure ifSetEngine(anEngine : TffServerEngine);

  public
    { Public declarations }

    property ServerEngine: TffServerEngine read FEngine write ifSetEngine;
  end;

var
  FFIndexForm: TFFIndexForm;

implementation

uses
  FFLLUNC,
  FFLLExcp,
  FFSRBde,                                                            
  FFSrCfg;

{$R *.DFM}

const
  { Column constants }
  cnTableName = 0;
  cnIndex = 1;
  cnDLLName = 2;
  cnBuildKey = 3;
  cnCompareKey = 4;

  { Cell margin constants }
  cnTopMargin = 2;
  cnLeftMargin = 2;

{===TffKeyProcItemRec==================================================}
constructor TffKeyProcItemRec.Create;
begin
  inherited Create('');
end;
{====================================================================}


{===Helper methods===================================================}
procedure TFFIndexForm.ifSetEngine(anEngine : TffServerEngine);
begin
  FEngine := anEngine;

  { Set the row count. }
  grdIndexes.RowCount := FEngine.Configuration.KeyProcList.Count + 2;
  grdIndexes.Row := 1;
end;
{====================================================================}


{===Form methods=====================================================}
procedure TFFIndexForm.FormShow(Sender: TObject);
begin
  ifPopulateColHeaders;
  ifPopulateGrid;
  grdIndexes.SetFocus;
end;
{====================================================================}


{===Grid methods & event handlers====================================}
procedure TFFIndexForm.grdIndexesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Grid : TffStringGrid absolute Sender;
  aStr : string;
begin

  { Leave fixed portion of the grid alone}
  if gdFixed in State then
    Exit;

  aStr := Grid.Cells[aCol, aRow];

  with Grid do begin
    if (aCol = cnTableName) or (aCol = cnDLLName) then begin
      if (aStr <> '') and (not FFFileExists(aStr)) then begin
        Canvas.Brush.Color := clRed;
        Canvas.Font.Color := clWhite;
      end;
    end;
    Canvas.FillRect(Rect);
    Canvas.TextRect(Rect, Rect.Left + cnLeftMargin, Rect.Top + cnTopMargin, aStr);
  end;

end;
{--------}
procedure TFFIndexForm.grdIndexesExitCell(Sender: TffStringGrid; aCol,
  aRow: Integer; const text: String);
begin
  if ((aCol = cnTableName) or (aCol = cnDLLName)) and
     (Text <> '') and FFFileExists(Text) then
    Sender.Cells[aCol, aRow] := FFExpandUNCFileName(text);
end;
{--------}
procedure TFFIndexForm.grdIndexesKeyDown(Sender: TObject; var Key: Word;
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
          Col := cnTableName;
        end else begin
          { Is this cell blank? }
          if Cells[Col, Row] = '' then begin
            { Yes. Wrap to first row of grid. }
            Row := 1;
            Col := cnTableName;
          end else begin
            { No.  Add a new blank row. }
            RowCount := RowCount + 1;
            Row := Pred(RowCount);
            Col := cnTableName;
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
        if (Row = pred(RowCount)) and RowIsFilled(Row) then
          { Yes.  Add a new blank row. }
          RowCount := RowCount + 1;
      end;
  end;  { case }
end;
{--------}
procedure TFFIndexForm.grdIndexesKeyPress(Sender: TObject; var Key: Char);
const
  validDigits = ['0'..'9'];
  validEditKeys = [#8, #13];
var
  Grid : TffStringGrid absolute Sender;
  Ignore: Boolean;
  Value: string;
begin
  if not (Key in validEditKeys) then begin
    { Validate data entry as key's are pressed}
    case Grid.Col of
      cnTableName, cnDLLName :
        begin
          Value := Grid.Cells[Grid.Col, Grid.Row];
          Ignore := (Length(Value) >= ffcl_Path);
        end;
      cnIndex :
        Ignore := not (Key in validDigits);
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
procedure TFFIndexForm.grdIndexesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  pbBrowse.Enabled := (ACol = cnTableName) or (ACol = cnDLLName);
  mnuIndexBrowse.Enabled := pbBrowse.Enabled;
end;
{--------}
procedure TFFIndexForm.grdIndexesSortColumn(Sender: TffStringGrid;
  aCol: Integer);
var
  aStr : string;
  i, j : integer;
  LastRow : integer;
begin
  if (Sender.RowCount > 1) and (aCol <> cnIndex) then
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
procedure TFFIndexForm.ifPopulateColHeaders;
begin
  with grdIndexes do begin
    BeginUpdate;
    try
      Cells[cnTableName, 0] := 'Table Name & Path';
      Cells[cnIndex,  0] := 'Index';
      Cells[cnDLLName, 0] := 'DLL Name & Path';
      Cells[cnBuildKey, 0] := 'Build Key';
      Cells[cnCompareKey, 0] := 'Compare Key';
    finally
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TFFIndexForm.ifPopulateGrid;
var
  Item : TffKeyProcItem;
  Inx : integer;
begin
  with grdIndexes do begin
    BeginUpdate;
    try
      for Inx := 1 to FEngine.Configuration.KeyProcList.Count do begin
        Item := FEngine.Configuration.KeyProcList[pred(Inx)];
        Cells[cnTableName,  Inx] := Item.Path + '\' + Item.Table + '.' + {!!.03}
                                    ffc_ExtForData;                      {!!.03}
        Cells[cnIndex,      Inx] := IntToStr(Item.IndexID);
        Cells[cnDLLName,    Inx] := Item.DLLName;
        Cells[cnBuildKey,   Inx] := Item.BuildKeyName;
        Cells[cnCompareKey, Inx] := Item.CompareKeyName;
      end;
    finally
      EndUpdate;
    end;
  end;
end;
{====================================================================}


{===Button event handlers============================================}
procedure TFFIndexForm.btnBrowseClick(Sender: TObject);
var
  aDlg : TOpenDialog;
begin
  with grdIndexes do begin
    if (Col <> cnTableName) and (Col <> cnDLLName) then
      exit;

    if Col = cnTableName then
      aDlg := dlgOpenTable
    else
      aDlg := dlgOpenDLL;

    if aDlg.Execute then begin
      BeginUpdate;
      try
        Cells[Col, Row] := FFExpandUNCFileName(aDlg.FileName);
      finally
        EndUpdate;
      end;
    end;
  end;
end;
{--------}
procedure TFFIndexForm.btnDeleteClick(Sender: TObject);
var
  DeletedRow : integer;
  Inx : integer;
  LastEmpty : boolean;
  LastRow : integer;
begin

  if (grdIndexes.RowCount < 2) then
    Exit;

  with grdIndexes do begin
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
{--------}
procedure TFFIndexForm.btnSaveClick(Sender: TObject);
var
  Inx   : integer;
  Path  : TffPath;
  Table : TffTableName;
  errStr : array [0..127] of char;
  aResult : TffResult;
begin

  FEngine.Configuration.KeyProcList.Empty;

  { Xfer the info from the grid to the engine's index list. }
  with grdIndexes do
    for Inx := 1 to pred(RowCount) do
      if RowIsFilled(Inx) then begin
        Path := FFExtractPath(Cells[cnTableName, Inx]);
        Table := FFExtractFileName(Cells[cnTableName, Inx]);
        FEngine.Configuration.AddKeyProc(Path,
                                         Table,
                                         StrToInt(Cells[cnIndex, Inx]),
                                         Cells[cnDLLName, Inx],
                                         Cells[cnBuildKey, Inx],
                                         Cells[cnCompareKey, Inx]);
      end;

  aResult := FEngine.WriteKeyProcData;
  if aResult <> DBIERR_NONE then begin
    ffStrResBDE.GetASCIIZ(aResult, errStr, sizeof(DBIMSG));
    showMessage(format('Could not save user-defined indexes: %s [$%x/%d])',
                       [strPas(errStr), aResult, aResult]));
    self.modalResult := mrNone;
  end;

end;
{====================================================================}

end.
