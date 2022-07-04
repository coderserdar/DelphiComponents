{*********************************************************}
{* Main file                                             *}
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


unit fmMain;

interface

uses
  Windows,
  BDE,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, StdCtrls, FileCtrl, ExtCtrls, Buttons, IniFiles;

type
  TfrmMain = class(TForm)
    tblSource: TTable;
    tblDest: TTable;
    batBatchMove: TBatchMove;
    grpSource: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lstAliases: TListBox;
    edtAliasName: TEdit;
    edtTableName: TEdit;
    lstTables: TListBox;
    grpDestination: TGroupBox;
    Label3: TLabel;
    Label6: TLabel;
    lblDirectory: TLabel;
    edtOutputFilename: TEdit;
    lstFields: TListBox;
    Label4: TLabel;
    lstFiles: TFileListBox;
    lstDirectories: TDirectoryListBox;
    cboFilter: TFilterComboBox;
    cboDrives: TDriveComboBox;
    Label5: TLabel;
    Label7: TLabel;
    imgCheck: TImage;
    chkSchemaOnly: TCheckBox;
    Button1: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    procedure btnExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstAliasesDblClick(Sender: TObject);
    procedure lstTablesDblClick(Sender: TObject);
    procedure lstFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstFieldsDblClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure edtAliasNameChange(Sender: TObject);
    procedure edtAliasNameExit(Sender: TObject);
    procedure edtAliasNameKeyPress(Sender: TObject; var Key: Char);
    procedure edtTableNameChange(Sender: TObject);
    procedure edtTableNameExit(Sender: TObject);
    procedure edtTableNameKeyPress(Sender: TObject; var Key: Char);
    procedure chkSchemaOnlyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
  public
    TablesLoaded: Boolean;
    TableInited: Boolean;
    procedure AdjustSchemaFile(aTable: TTable; aFilename: TFilename);
    procedure InitTable;
    procedure LoadTables;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

const
  FG_UNSELECTED  = 0;
  FG_SELECTED    = 1;
  FG_UNAVAILABLE = 2;

  BlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary];

procedure TfrmMain.AdjustSchemaFile(aTable: TTable; aFilename: TFilename);
var
  F: Integer;
  FldNo: Integer;                                                     
  I: Integer;
  SchemaFile: TIniFile;
  SectionName: string;
  Ext: string[10];
  Entry: string;
  DateFormat: FMTDate;
  TimeFormat: FMTTime;
  EntryID,
  Mask,
  DateMask,
  TimeMask: string[40];
begin

  { Extract the date format from the BDE }
  DbiGetDateFormat(DateFormat);
  with DateFormat do begin
    case iDateMode of
      0: DateMask := 'M' + szDateSeparator + 'D' + szDateSeparator + 'Y';
      1: DateMask := 'D' + szDateSeparator + 'M' + szDateSeparator + 'Y';
      2: DateMask := 'Y' + szDateSeparator + 'M' + szDateSeparator + 'D';
    end;
  end;

  { Extract the time format from the BDE }
  DbiGetTimeFormat(TimeFormat);
  with TimeFormat do begin
    TimeMask := 'h' + cTimeSeparator + 'm';
    if bSeconds then
      TimeMask := TimeMask + cTimeSeparator + 's';
    if bTwelveHour then
      TimeMask := TimeMask + ' t';
  end;

  SchemaFile := TIniFile.Create(aFilename);
  try
    SectionName := ExtractFileName(aFilename);
    Ext := ExtractFileExt(SectionName);
    if Ext <> '' then
      Delete(SectionName, Pos(Ext, SectionName), Length(Ext));

    { Change the filetype }
    SchemaFile.WriteString(SectionName, 'FILETYPE', 'ASCII');

    { Loop through fields, making adjustments }
    FldNo := 0;                                                                   
    with aTable.FieldDefs do
      for F := 0 to Count - 1 do
        if (LongInt(lstFields.Items.Objects[F]) and FG_SELECTED) <> 0 then        
          with Items[F] do begin
            Inc(FldNo);                                                           

            { Get the current schema file entry for this field }
            EntryID := 'Field' + IntToStr(FldNo);                                 
            Entry := SchemaFile.ReadString(SectionName, EntryID, '');

            { Add masks for date/time fields }
            case Datatype of                                          
              ftDate, ftTime, ftDateTime:                             
                begin
                  Mask := '';
                  case DataType of
                    ftDate: Mask := DateMask;
                    ftTime: Mask := TimeMask;
                    ftDateTime: Mask := DateMask + ' ' + TimeMask;
                  end;

                  if Mask <> '' then begin

                    { Append a local mask to it }
                    if Pos(',', Mask) <> 0 then Mask := '"' + Mask + '"';
                    Entry := Entry + ',' + Mask;

                    { Rewrite the modified entry back to the schema file }
                    SchemaFile.WriteString(SectionName, EntryID, Entry);
                  end;
                end;

              ftInteger:                                              
                begin
                  I := Pos('LONG INTEGER', ANSIUppercase(Entry));
                  System.Delete(Entry, I, 12);
                  System.Insert('LongInt', Entry, I);
                  SchemaFile.WriteString(SectionName, EntryID, Entry);
                end;

              ftAutoInc:
                begin
                  I := Pos('LONG INTEGER', ANSIUppercase(Entry));
                  System.Delete(Entry, I, 12);
                  System.Insert('AutoInc', Entry, I);
                  SchemaFile.WriteString(SectionName, EntryID, Entry);
                end;
            end;
          end;
  finally
    SchemaFile.Free;
  end;
end;

procedure TfrmMain.InitTable;
var
  I: Integer;
  Flag: LongInt;
begin
  with tblSource do begin
    DatabaseName := edtAliasName.Text;
    TableName := edtTableName.Text;
    FieldDefs.Update;
    lstFields.Clear;
    for I := 0 to FieldDefs.Count - 1 do begin
      Flag := FG_SELECTED;
      if (FieldDefs[I].DataType in BlobTypes) then
        Flag := FG_UNAVAILABLE;
      lstFields.Items.AddObject(FieldDefs[I].Name, Pointer(Flag));
    end;
  end;
  edtOutputFilename.Text := ChangeFileExt(ExtractFileName(edtTableName.Text), '.ASC');
  TableInited := True;
end;

procedure TfrmMain.LoadTables;
begin
  if edtAliasName.Text <> '' then begin
    Session.GetTableNames(edtAliasName.Text, '', True, False, lstTables.Items);
    TablesLoaded:= True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'BETA.HLP';
  Session.GetAliasNames(lstAliases.Items);
end;

procedure TfrmMain.lstAliasesDblClick(Sender: TObject);
begin
  edtTableName.Text := '';
  with lstAliases do
    if ItemIndex <> -1 then begin
      edtAliasName.Text := Items[ItemIndex];
      LoadTables;
    end;
end;

procedure TfrmMain.lstTablesDblClick(Sender: TObject);
begin
  with lstTables do
    if ItemIndex <> - 1 then begin
      edtTableName.Text := Items[ItemIndex];
      InitTable;
    end;
end;

procedure TfrmMain.lstFieldsDblClick(Sender: TObject);
begin
  with lstFields do
    if (LongInt(Items.Objects[ItemIndex]) and FG_UNAVAILABLE) <> 0 then
      MessageBeep(0)
    else begin
      Items.Objects[ItemIndex] := Pointer((LongInt(Items.Objects[ItemIndex]) + 1) mod 2);
      Invalidate;
    end;
end;

procedure TfrmMain.lstFieldsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox) do begin
    with Canvas do begin
      Font.Assign(Font);

      if (odSelected) in State then begin
        Font.Color := clWindowText;
        Brush.Color := (Control as TListBox).Color;
      end;

      FillRect(Rect);

      if (LongInt(Items.Objects[Index]) and FG_SELECTED) <> 0 then
        with imgCheck.Picture.Bitmap do
          BrushCopy(Bounds(Rect.Left + 2, Rect.Top + 4, Width, Height),
                    imgCheck.Picture.Bitmap, Bounds(0, 0, Width, Height),
                    TransparentColor);

      if (LongInt(Items.Objects[Index]) and FG_UNAVAILABLE) <> 0 then
        Font.Color := clRed;

      { Draw the item text }
      TextOut(Rect.Left + imgCheck.Picture.Bitmap.Width + 4, Rect.Top, Items[Index]);
    end;
  end;
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
var
  I: Integer;
  ValidFields: TStringList;
  SchemaFilePath: string;
  DestPath: string;
  DestName: string;
  CheckFile: string;
begin
  if (Pos('*', edtOutputFilename.Text) <> 0) or
     (Pos('?', edtOutputFilename.Text) <> 0) or
     (edtOutputFilename.Text = '') then
    raise Exception.Create('Invalid output filename');
    
  DestPath := ExtractFilePath(edtOutputFilename.Text);
  if DestPath = '' then
    DestPath := lblDirectory.Caption;
  if Copy(DestPath, Length(DestPath), 1) <> '\' then
    DestPath := DestPath + '\';

  if chkSchemaOnly.Checked then begin
    batBatchMove.RecordCount := 1;
    DestName := ChangeFileExt(ExtractFilename(edtOutputFilename.Text), '.$$$');
  end
  else
    DestName := ExtractFilename(edtOutputFilename.Text);

  CheckFile := DestPath + ExtractFilename(edtOutputFilename.Text);
  if FileExists(CheckFile) then
    if MessageDlg('Replace ' + CheckFile + '?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  batBatchMove.Mappings.Clear;

  with tblSource do begin
    DatabaseName := edtAliasName.Text;
    TableName := edtTableName.Text;

    { Build the BatchMove mapping for the valid fields }
    ValidFields := TStringList.Create;
    try
      with lstFields do
        for I := 0 to Items.Count - 1 do
          if (LongInt(Items.Objects[I]) and FG_SELECTED) <> 0 then
            ValidFields.Add(Items[I]);
      batBatchMove.Mappings.Assign(ValidFields);
    finally
      ValidFields.Free;
    end;
  end;

  with tblDest do begin
    DatabaseName := DestPath;
    TableName := DestName;
    SchemaFilePath := ChangeFileExt(DatabaseName + TableName, '.SCH');
    DeleteFile(SchemaFilePath);
  end;

  Screen.Cursor := crHourglass;
  try
    batBatchMove.Execute;
    AdjustSchemaFile(tblSource, SchemaFilePath);
  finally
    Screen.Cursor := crDefault;
    if chkSchemaOnly.Checked then
      DeleteFile(ChangeFileExt(SchemaFilePath, '.$$$'));
  end;                     

  MessageBeep(0);
  Application.MessageBox('Export Completed', 'BDE Export', MB_OK);
end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;

procedure TfrmMain.edtAliasNameChange(Sender: TObject);
begin
  TablesLoaded := False;
  TableInited := False;
end;

procedure TfrmMain.edtAliasNameExit(Sender: TObject);
begin
  if not TablesLoaded then LoadTables;
end;

procedure TfrmMain.edtAliasNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if not TablesLoaded then
      LoadTables;
    Key := #0;
  end;
end;

procedure TfrmMain.edtTableNameChange(Sender: TObject);
begin
  TableInited := False;
end;

procedure TfrmMain.edtTableNameExit(Sender: TObject);
begin
  if not TableInited then InitTable;
end;

procedure TfrmMain.edtTableNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if not TableInited then InitTable;
    Key := #0;
  end;
end;

procedure TfrmMain.chkSchemaOnlyClick(Sender: TObject);
begin
  if chkSchemaOnly.Checked and (edtOutputFilename.Text <> '') then
    edtOutputFilename.Text := ChangeFileExt(edtOutputFilename.Text, '.SCH');
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

