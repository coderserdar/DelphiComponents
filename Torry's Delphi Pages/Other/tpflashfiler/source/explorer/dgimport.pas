{*********************************************************}
{* Dialog to import external data files                  *}
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

{$I ffdefine.inc}

unit dgimport;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  FileCtrl,
  Buttons,
  uentity,
  ffclimex,
  ffllbase,
  fflldict,
  ubase,
  uconsts,
  dgimpdo;

type
  TdlgImport = class(TForm)
    btnImport: TBitBtn;
    btnCancel: TBitBtn;
    grpImportFile: TGroupBox;
    lblFilename: TLabel;
    lblDir: TLabel;
    lblDirectory: TLabel;
    lblFileFilter: TLabel;
    lblDrives: TLabel;
    edtImportFilename: TEdit;
    lstFiles: TFileListBox;
    lstDirectories: TDirectoryListBox;
    cboFilter: TFilterComboBox;
    cboDrives: TDriveComboBox;
    grpTable: TGroupBox;
    cboTableName: TComboBox;
    lblTblName: TLabel;
    grpExistingData: TRadioGroup;
    lblRecsPerTran: TLabel;
    edtBlockInserts: TEdit;
    UpDown1: TUpDown;
    procedure btnImportClick(Sender: TObject);
    procedure edtImportFilenameKeyPress(Sender: TObject; var Key: Char);
    procedure btnCancelClick(Sender: TObject);
    procedure lstFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDatabase : TffeDatabaseItem;
    FTableIndex: LongInt;
    FImportEngine: TffImportEngine;
    FNewTable: Boolean;                                               
    FImportFilename: TFilename;                                       
    FTableName: TffTableName;
    FBlockInserts: Integer;                                           
    FSchemaOnly: Boolean;
  public
  end;

function ShowImportDlg(aDatabase : TffeDatabaseItem;
                       var aTableIndex: LongInt): TModalResult;
{ Shows the "Import Data" dialog, allowing the user to import data from
  an external file into a database table, or create a new table from the
  import file structure.

  Input parameters:
    aServer :        The server associated with the import.
    aDatabaseIndex:  Within aServer's list of databases, the index of the
                     database that will contain the table being imported.
    aTableIndex:     Within aDatabase's list of tables, the index of the table
                     into which data is being imported.
                     Set this parameter to -1 if no table has been selected.

  Output parameters:
    aTableIndex:  -1 if the table imported into already existed; otherwise
                  the index for the newly created table within the server's
                  database list.
}
var
  dlgImport: TdlgImport;

implementation

uses
  fmmain;


{$R *.DFM}

function ShowImportDlg(aDatabase : TffeDatabaseItem;
                       var aTableIndex: LongInt): TModalResult;
var
  I: Integer;
begin
  with TdlgImport.Create(nil) do
  try
    FDatabase := aDatabase;
    FTableIndex := aTableIndex;
    if FTableIndex = -1 then
      cboTableName.ItemIndex := -1;

    with cboTableName do begin

      { Fill the dropdown list with table names; keep TableIndexes in
        the stringlist's Objects property }
      Items.Clear;
      for I := 0 to pred(FDatabase.TableCount) do
        with FDatabase.Tables[I] do
          Items.AddObject(TableName, Pointer(I));

      { Set the ItemIndex for the table we've selected before entering.
        ComboBox list is sorted, so capturing the index during the loop
        above may not be entirely accurate. }
      if FTableIndex <> -1 then
        with FDatabase.Tables[FTableIndex] do
          for I := 0 to pred(FDatabase.TableCount) do
            if FFCmpShStrUC(Items[I], TableName, 255) = 0 then begin
              ItemIndex := I;
              Break;
            end;
    end;

    Result := ShowModal;

    aTableIndex := -1;
    if Result = mrOK then begin
      if not FSchemaOnly then
        try
          frmMain.EnableScreen(False);
          try
            if DoImport(FImportEngine,
                        FImportFilename,
                        FTableName,
                        FDatabase.Tables[FTableIndex].Table,
                        FBlockInserts) then begin
              MessageBeep(0);
              Application.MessageBox('Import Completed',
                                     'FlashFiler Explorer',
                                     MB_ICONINFORMATION or MB_OK);
            end
            else begin  { If we've aborted and we created a new table, get rid of it }
              if FNewTable then begin
                FDatabase.DropTable(FTableIndex);
                FNewTable := False;
              end;
            end;
          finally
            frmMain.EnableScreen(True);
          end;
        finally
          FImportEngine.Free;
        end;
      if FNewTable then aTableIndex := FTableIndex;
    end;
  finally
    Free;
  end;
end;

procedure TdlgImport.FormCreate(Sender: TObject);
begin
  HelpContext := hcImportDataDlg;
  edtBlockInserts.Text := '10';
end;

procedure TdlgImport.FormShow(Sender: TObject);
begin
  lstDirectories.Update;
  lstFiles.Update;
end;

procedure TdlgImport.btnImportClick(Sender: TObject);
var
  Aborted: Boolean;
  I: Integer;
  Msg: TffShStr;
  ValError : Integer;

  function CreateNewTable(aTableName: TffTableName): LongInt;
  var
    Dict: TffDataDictionary;
    BlockSize: LongInt;                                               
  begin
    BlockSize := 4*1024;
    Dict := TffDataDictionary.Create(BlockSize);                      
    try
      with FDatabase do begin

        { Get the dictionary for the import file }
        FImportEngine.Schema.MakeIntoDictionary(Dict);                

        { Determine if the block size is large enough for one record }
        while (BlockSize - ffc_BlockHeaderSizeData < Dict.RecordLength) and
              (BlockSize < 32 * 1024) do
          BlockSize := BlockSize shl 1;
        Dict.BlockSize := BlockSize;

        { Create the table in the database }
        CreateTable(aTableName, Dict);

        { Make a new entry for the TableList }
        Result := AddTable(aTableName);
      end;
    finally
      Dict.Free;
    end;
  end;

begin

  { Get the import filename }
  if ExtractFilePath(edtImportFilename.Text) <> '' then
    FImportFilename := edtImportFilename.Text                         
  else begin                                                          
    FImportFilename := lstDirectories.Directory;                      
    if (FImportFilename[length(FImportFilename)] <> '\') then         
      FImportFilename := FImportFilename + '\';                       
    FImportFilename := FImportFilename +                              
                       edtImportFilename.Text;                        
  end;

  { Validate }
  if cboTableName.Text = '' then
    raise Exception.Create('Table name required');

  if not FFFileExists(FImportFilename) then
    raise Exception.Create('Invalid import filename');

  if not FFFileExists(ChangeFileExt(FImportFilename, '.SCH')) then
    raise Exception.Create('Schema file missing');

  Val(edtBlockInserts.Text, FBlockInserts, ValError);
  if ValError <> 0 then
    raise Exception.Create('Invalid data for block inserts field');
  if FBlockInserts <= 0 then
    FBlockInserts := 1;

  { See if the user has given us a new tablename }
  with cboTableName do begin
    for I := 0 to Items.Count - 1 do
      if FFCmpShStrUC(Text, Items[I], 255) = 0 then
        ItemIndex := I;

    Aborted := False;
    FImportEngine := TffImportEngine.Create(FImportFilename);         
    try
      FNewTable := False;
      Screen.Cursor := crHourGlass;
      try
        { Check for schema only import }
        FSchemaOnly := Pos('.SCH', Uppercase(FImportFilename)) <> 0;
        if FSchemaOnly then begin
          if ItemIndex = -1 then begin
            Msg := 'Create new table ' + cboTableName.Text + ' from schema only?';
            FNewTable := True;
          end
          else
            Msg := 'Replace table ' + cboTableName.Text + ' from schema only?';

          if MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

            if not FNewTable then                                     
              with FDatabase.Tables[FTableIndex].Table do begin
                if Active then Close;
                DeleteTable;
              end;
            FTableIndex := CreateNewTable(cboTableName.Text);
          end
          else Aborted := True;
        end
        else begin
          if ItemIndex = -1 then begin
            if MessageDlg('Create new table ' + Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              FTableIndex := CreateNewTable(cboTableName.Text);
              FNewTable := True;                                      
            end
            else Exit;
          end
          else begin
            FTableIndex := LongInt(Items.Objects[ItemIndex]);

            { Overwrite existing data? }
            if grpExistingData.ItemIndex <> 0 then
              FDatabase.Tables[FTableIndex].Truncate;
          end;

          with FDatabase.Tables[FTableIndex] do begin
            if Table.Active and Table.ReadOnly then
              Table.Close;

            if not Table.Active then begin
              Table.ReadOnly := False;
              Table.Open;
            end;

            FTableName := cboTableName.Text;
          end;
        end;
        if Aborted then FImportEngine.Free;                           
      finally
        Screen.Cursor := crDefault;
      end;
    except                                                            
      FImportEngine.Free;                                             
      raise;
    end;
  end;

  if not Aborted then ModalResult := mrOK;
end;

procedure TdlgImport.btnCancelClick(Sender: TObject);
begin
  FTableIndex := -1;
end;

procedure TdlgImport.edtImportFilenameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then begin
    lstFiles.Mask := edtImportFilename.Text;
    Key := #0;
  end;
end;

procedure TdlgImport.lstFilesClick(Sender: TObject);
var
  NewTablename: TffShStr;
  Ext: TffShStr;
begin
  if cboTableName.Text = '' then begin
    NewTablename := edtImportFilename.Text;
    Ext := ExtractFileExt(NewTablename);
    Delete(NewTablename, Pos(Ext, NewTableName), Length(Ext));
    cboTableName.Text := NewTablename;
  end;
end;

end.
