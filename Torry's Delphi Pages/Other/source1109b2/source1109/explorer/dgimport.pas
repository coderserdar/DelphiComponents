{$I fsdefine.inc}

Unit dgimport;

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
  ComCtrls,
  ExtCtrls,
  Stdctrls,
  FileCtrl,
  Buttons,
  uentity,
  fsclimex,
  fsllbase,
  fsdbbase,
  fslldict,
  ubase,
  uconsts,
  fssrbde,
  dgimpdo;

Type
  TdlgImport = Class(TForm)
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
    CheckBox1: TCheckBox;
    Procedure btnImportClick(Sender: TObject);
    Procedure edtImportFilenameKeyPress(Sender: TObject; Var Key: Char);
    Procedure btnCancelClick(Sender: TObject);
    Procedure lstFilesClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    FDatabase: TffeDatabaseItem;
    FTableIndex: Longint;
    FImportEngine: TffImportEngine;
    FNewTable: Boolean;
    FImportFilename: TFilename;
    FTableName: TfsTableName;
    FBlockInserts: Integer;
    FSchemaOnly: Boolean;
  Public
  End;

Function ShowImportDlg(aDatabase: TffeDatabaseItem;
  Var aTableIndex: Longint): TModalResult;
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
Var
  dlgImport: TdlgImport;

Implementation

Uses
  fmmain;

{$R *.DFM}

Function ShowImportDlg(aDatabase: TffeDatabaseItem;
  Var aTableIndex: Longint): TModalResult;
Var
  I: Integer;
Begin
  With TdlgImport.Create(Nil) Do
    Try
      FDatabase := aDatabase;
      FTableIndex := aTableIndex;
      If FTableIndex = -1 Then
        cboTableName.ItemIndex := -1;

      With cboTableName Do
        Begin

          { Fill the dropdown list with table names; keep TableIndexes in
            the stringlist's Objects property }
          Items.Clear;
          For I := 0 To pred(FDatabase.TableCount) Do
            With FDatabase.Tables[I] Do
              Items.AddObject(TableName, Pointer(I));

          { Set the ItemIndex for the table we've selected before entering.
            ComboBox list is sorted, so capturing the index during the loop
            above may not be entirely accurate. }
          If FTableIndex <> -1 Then
            With FDatabase.Tables[FTableIndex] Do
              For I := 0 To pred(FDatabase.TableCount) Do
                If FFCmpShStrUC(Items[I], TableName, 255) = 0 Then
                  Begin
                    ItemIndex := I;
                    Break;
                  End;
        End;

      Result := ShowModal;

      aTableIndex := -1;
      If Result = mrOK Then
        Begin
          If Not FSchemaOnly Then
            Try
              frmMain.EnableScreen(False);
              Try
                If DoImport(FImportEngine,
                  FImportFilename,
                  FTableName,
                  FDatabase.Tables[FTableIndex].Table,
                  FBlockInserts, CheckBox1.Checked) Then
                  Begin
                    MessageBeep(0);
                    Application.MessageBox('Import Completed',
                      'FSISQL Explorer',
                      MB_ICONINFORMATION Or MB_OK);
                  End
                Else
                  Begin { If we've aborted and we created a new table, get rid of it }
                    If FNewTable Then
                      Begin
                        FDatabase.DropTable(FTableIndex);
                        FNewTable := False;
                      End;
                  End;
              Finally
                frmMain.EnableScreen(True);
              End;
            Finally
              FImportEngine.Free;
            End;
          If FNewTable Then
            aTableIndex := FTableIndex;
        End;
    Finally
      Free;
    End;
End;

Procedure TdlgImport.FormCreate(Sender: TObject);
Begin
  HelpContext := hcImportDataDlg;
  edtBlockInserts.Text := '10';
End;

Procedure TdlgImport.FormShow(Sender: TObject);
Begin
  lstDirectories.Update;
  lstFiles.Update;
End;

Procedure TdlgImport.btnImportClick(Sender: TObject);
Var
  Aborted: Boolean;
  I: Integer;
  Msg: TffShStr;
  ValError: Integer;

  Function CreateNewTable(aTableName: TfsTableName): Longint;
  Var
    Dict: TFSInfoDict;
    BlockSize: Longint;
  Begin
    BlockSize := 4 * 1024;
    Dict := TFSInfoDict.Create(BlockSize);
    Try
      With FDatabase Do
        Begin

          { Get the dictionary for the import file }
          FImportEngine.Schema.MakeIntoDictionary(Dict);

          { Determine if the block size is large enough for one record }
          While (BlockSize - fsc_BlockHeaderSizeData < Dict.RecordLength) And
            (BlockSize < 32 * 1024) Do
            BlockSize := BlockSize Shl 1;
          Dict.BlockSize := BlockSize;

          { Create the table in the database }
          CreateTable(aTableName, Dict);

          { Make a new entry for the TableList }
          Result := AddTable(aTableName);
        End;
    Finally
      Dict.Free;
    End;
  End;

Begin
  { Get the import filename }
  If ExtractFilePath(edtImportFilename.Text) <> '' Then
    FImportFilename := edtImportFilename.Text
  Else
    Begin
      FImportFilename := lstDirectories.Directory;
      If (FImportFilename[length(FImportFilename)] <> '\') Then
        FImportFilename := FImportFilename + '\';
      FImportFilename := FImportFilename +
        edtImportFilename.Text;
    End;

  { Validate }
  If cboTableName.Text = '' Then
    Raise Exception.Create('Table name required');

  If Not FFFileExists(FImportFilename) Then
    Raise Exception.Create('Invalid import filename');

  If Not FFFileExists(ChangeFileExt(FImportFilename, '.SCH')) Then
    Raise Exception.Create('Schema file missing');

  Val(edtBlockInserts.Text, FBlockInserts, ValError);
  If ValError <> 0 Then
    Raise Exception.Create('Invalid data for block inserts field');
  If FBlockInserts <= 0 Then
    FBlockInserts := 1;

  { See if the user has given us a new tablename }
  With cboTableName Do
    Begin
      For I := 0 To Items.Count - 1 Do
        If FFCmpShStrUC(Text, Items[I], 255) = 0 Then
          ItemIndex := I;

      Aborted := False;
      FImportEngine := TffImportEngine.Create(FImportFilename);
      //FImportEngine.Schema.FileType
      Try
        FNewTable := False;
        Screen.Cursor := crHourGlass;
        Try
          { Check for schema only import }
          FSchemaOnly := Pos('.SCH', Uppercase(FImportFilename)) <> 0;
          If FSchemaOnly Then
            Begin
              If ItemIndex = -1 Then
                Begin
                  Msg := 'Create new table ' + cboTableName.Text + ' from schema only?';
                  FNewTable := True;
                End
              Else
                Msg := 'Replace table ' + cboTableName.Text + ' from schema only?';

              If MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
                Begin

                  If Not FNewTable Then
                    With FDatabase.Tables[FTableIndex].Table Do
                      Begin
                        If Active Then
                          Close;
                        DeleteTable;
                      End;
                  FTableIndex := CreateNewTable(cboTableName.Text);
                End
              Else
                Aborted := True;
            End
          Else
            Begin
              If ItemIndex = -1 Then
                Begin
                  If MessageDlg('Create new table ' + Text + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
                    Begin
                      FTableIndex := CreateNewTable(cboTableName.Text);
                      FNewTable := True;
                    End
                  Else
                    Exit;
                End
              Else
                Begin
                  FTableIndex := Longint(Items.Objects[ItemIndex]);

                  { Overwrite existing data? }
                  If grpExistingData.ItemIndex <> 0 Then
                    FDatabase.Tables[FTableIndex].Truncate;
                End;

              With FDatabase.Tables[FTableIndex] Do
                Begin
                  If Table.Active And Table.ReadOnly Then
                    Table.Close;

                  If Not Table.Active Then
                    Begin
                      Table.ReadOnly := False;
                      Table.Open;
                    End;

                  FTableName := cboTableName.Text;
                End;
            End;
          If Aborted Then
            FImportEngine.Free;
        Finally
          Screen.Cursor := crDefault;
        End;
      Except
        FImportEngine.Free;
        Raise;
      End;
    End;

  If Not Aborted Then
    ModalResult := mrOK;
End;

Procedure TdlgImport.btnCancelClick(Sender: TObject);
Begin
  FTableIndex := -1;
End;

Procedure TdlgImport.edtImportFilenameKeyPress(Sender: TObject;
  Var Key: Char);
Begin
  If Key = #13 Then
    Begin
      lstFiles.Mask := edtImportFilename.Text;
      Key := #0;
    End;
End;

Procedure TdlgImport.lstFilesClick(Sender: TObject);
Var
  NewTablename: TffShStr;
  Ext: TffShStr;
Begin
  If cboTableName.Text = '' Then
    Begin
      NewTablename := edtImportFilename.Text;
      Ext := ExtractFileExt(NewTablename);
      Delete(NewTablename, Pos(Ext, NewTableName), Length(Ext));
      cboTableName.Text := NewTablename;
    End;
End;

End.

