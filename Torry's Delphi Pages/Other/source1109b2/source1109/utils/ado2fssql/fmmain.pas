{$I fsdefine.inc}

Unit fmmain;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DB,
  DBTables,
  Stdctrls,
  ExtCtrls,
  Buttons,
  Menus,
  fsclimex,
  fsllbase,
  fslldict,
  fsllprot,
  fsclintf,
  dgimpdo,
  fsdb,
  fsdbbase,
  fsfunInterp,
  ComCtrls,
  ADODB, fsllcomm, fslllgcy, fsllcomp, fslleng, fssrintm, 
  fsserverremoteclass;

Type
  TfrmMain = Class(TForm)
    btnTransfer: TBitBtn;
    btnExit: TBitBtn;
    imgCheck: TImage;
    mnuMain: TMainMenu;
    mnuOperations: TMenuItem;
    tblDest: TfsTable;
    dbDest: TfsDatabase;
    mnuExit: TMenuItem;
    N1: TMenuItem;
    mnuTransferActiveTable: TMenuItem;
    pgTransfer: TPageControl;
    tabSource: TTabSheet;
    tabOptions: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lstadoTables: TListBox;
    lstadoFields: TListBox;
    tabTarget: TTabSheet;
    Label3: TLabel;
    Label5: TLabel;
    edtFsTableName: TEdit;
    lstFsTables: TListBox;
    cmbFsAliases: TComboBox;
    grpStringHandling: TGroupBox;
    chkClearEmptyStrings: TCheckBox;
    chkEmptyStrings: TCheckBox;
    chkOEMAnsi: TCheckBox;
    chkUseZeroTerminatedStrings: TCheckBox;
    grpMisc: TGroupBox;
    chkSchemaOnly: TCheckBox;
    chkUseSysToolsDates: TCheckBox;
    chkUseSysToolsTimes: TCheckBox;
    grpExistingData: TRadioGroup;
    tblSource: TADOTable;
    Connection1: TADOConnection;
    cmbAdoAliases: TEdit;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Session: TFSSession;
    FSClient1: TFSClient;
    FSRemoteServer1: TFSRemoteServer;
    FSParamConnect1: TFSParamConnect;
    CheckBox1: TCheckBox;
    Procedure btnTransferClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure btnExitClick(Sender: TObject);
    Procedure lstadoFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    Procedure lstadoFieldsDblClick(Sender: TObject);
    Procedure edtBDEAliasNameChange(Sender: TObject);
    Procedure edtBDEAliasNameExit(Sender: TObject);
    Procedure edtBDEAliasNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure edtBDETableNameChange(Sender: TObject);
    Procedure edtBDETableNameExit(Sender: TObject);
    Procedure edtBDETableNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure edtFsTableNameChange(Sender: TObject);
    Procedure edtFsTableNameExit(Sender: TObject);
    Procedure edtFsTableNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure lstFsTablesDblClick(Sender: TObject);
    Procedure lstadoTablesClick(Sender: TObject);
    Procedure cmbFsAliasesChange(Sender: TObject);
    Procedure chkClearEmptyStringsClick(Sender: TObject);
    Procedure chkEmptyStringsClick(Sender: TObject);
    Procedure cmbAdoAliasesChange(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  Protected
    BDETablesLoaded: Boolean;
    BDETableInited: Boolean;
    FFTablesLoaded: Boolean;
    FFTableInited: Boolean;
    Aborted: Boolean;
    IsSQLServer: Boolean;
    Procedure ConvertTable(Const BDETableName, FFTableName: TfsTableName);
    Procedure CreateNewTable(Const BDETableName, FFTableName: TfsTableName);
    Procedure InitBDETable;
    Function InitCommsEngine: Boolean;
    Procedure IniTfsTable;
    Procedure LoadAliases;
    Procedure LoadBDETables;
    Procedure LoadFFTables;
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.DFM}

Const
  FG_UNSELECTED = 0;
  FG_SELECTED = 1;
  FG_UNAVAILABLE = 2;

  csSQLServer = 'SQL Server';

Procedure TfrmMain.CreateNewTable(Const BDETableName, FFTableName: TfsTableName);
Var
  Dict: TFSInfoDict;
  I: Integer;
  IdxName: String;
  FFType: TfsFieldType;
  FFSize: Longint;
  FFDecPl: Integer;
  FldArray: TffFieldList;
  IHelpers: TffFieldIHList;
  FldCase,FldSize, FldAscDesc, FldFlags,FldNullTop: TffFieldList;
  NFields: Integer;

  Procedure ParseFieldNames(aFieldNames: TffShStr);
  Var
    DoFieldNums: Boolean;
    FieldEntry: TffShStr;
    FieldNo: Integer;
  Begin
    DoFieldNums := False; {!!.03 - Start}
    If aFieldNames[1] In ['0'..'9'] Then
      Begin
        FieldNo := 2;
        While True Do
          Begin
            If aFieldNames[FieldNo] = ';' Then
              Begin
                DoFieldNums := True;
                Break;
              End
            Else If aFieldNames[FieldNo] In ['0'..'9'] Then
              Inc(FieldNo)
            Else
              Begin
                DoFieldNums := False;
                Break;
              End;
          End;
      End; {!!.03 - End}
    NFields := 0;
    Repeat
      FFShStrSplit(aFieldNames, ';', FieldEntry, aFieldNames);
      If DoFieldNums Then
        FldArray[NFields] := StrToInt(FieldEntry) - 1
      Else
        Begin
          FieldNo := Dict.GetFieldFromName(FieldEntry);
          If FieldNo = -1 Then
            Raise Exception.Create('Invalid field in index');
          FldArray[NFields] := FieldNo;
        End;

      FldSize[NFields]:= 0;
      FldFlags[NFields]:= 0;
      FldNullTop[NFields]:= 1;
      Inc(NFields);
      If aFieldNames <> '' Then {!!.02}
        IHelpers[NFields] := ''; {!!.02}
    Until aFieldNames = '';
  End;

  Function DetermineBlockSize: Longint;
  Var
    FFType: TfsFieldType;
    FFSize: Longint;
    FFDecPl: Integer;
    BlockSize: Longint;
    i: Integer;
  Begin
    { Build size from source table structure }
    With tblSource Do
      Begin
        {Management size}
        BlockSize := 32 + 1;
        { Get the fields }
        FieldDefs.Update;

        If lstadoTables.SelCount > 1 Then
          Begin
            For I := 0 To Pred(FieldDefs.Count) Do
              Begin
                With FieldDefs[I] Do
                  Begin
                    ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
                    BlockSize := BlockSize + FFSize;
                  End; { if }
              End;
          End
        Else
          Begin
            { Calculate using only the fields selected in the fields list. }
            With lstadoFields Do
              For I := 0 To Items.Count - 1 Do
                If (Longint(Items.Objects[I]) And FG_SELECTED) <> 0 Then
                  With FieldDefs[I] Do
                    Begin
                      ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
                      BlockSize := BlockSize + FFSize;
                    End; { if }
          End; { if }
      End; { with }
    { Determine the first multiple of 4096 larger then BlockSize }
    Result := (BlockSize Div 4096 + 1) * 4096;
  End;

Begin
  Dict := TFSInfoDict.Create(DetermineBlockSize);
  Try

    { Initialize the FieldArray }
    For I := 0 To pred(fscl_MaxIndexFlds) Do
      Begin
        FldArray[I] := 0;
        IHelpers[I] := '';
      End;

    { Build dictionary from source table structure }
    With tblSource Do
      Begin
        { Point to the source table. }
        TableName := BDETableName;
        ReadOnly := True;

        { Get the fields }
        FieldDefs.Update;

        { Obtain the field definitions. }
        If lstadoTables.SelCount > 1 Then
          Begin
            { Convert all fields. }
            For I := 0 To Pred(FieldDefs.Count) Do
              Begin
                With FieldDefs[I] Do
                  Begin
                    ConvertBDEDataType(FieldDefs[I].DataType, FieldDefs[I].Size, FFType, FFSize, FFDecPl);
                    Case DataType Of
                      ftBCD:
                        Begin
                          FFSize := 15;
                          FFDecPl := 2;
                        End;
                      ftFloat:
                        Begin
                          FFSize := 18;
                          FFDecPl := 0;
                        End;
                      ftCurrency:
                        Begin
                          FFSize := 18;
                          FFDecPl := 2;
                        End;
                    End;
                    Dict.AddField(FieldDefs[I].Name,
                      FieldDefs[I].Name, { description }
                      FFType,
                      FFSize,
                      FFDecPl,
                      Required,
                      Nil,
                      blNone,
                      '',
                      rNone,
                      False,
                      duNormal);
                  End; { with }
              End; { for }
          End
        Else
          Begin
            { Convert only the fields selected in the fields list. }
            With lstadoFields Do
              For I := 0 To Items.Count - 1 Do
                If (Longint(Items.Objects[I]) And FG_SELECTED) <> 0 Then
                  With FieldDefs[I] Do
                    Begin
                      ConvertBDEDataType(FieldDefs[I].DataType, FieldDefs[I].Size, FFType, FFSize, FFDecPl);
                      Case DataType Of
                        ftBCD:
                          Begin
                            FFSize := 15;
                            FFDecPl := 2;
                          End;
                        ftFloat:
                          Begin
                            FFSize := 18;
                            FFDecPl := 0;
                          End;
                        ftCurrency:
                          Begin
                            FFSize := 18;
                            FFDecPl := 2;
                          End;
                      End;
                      Dict.AddField(FieldDefs[I].Name,
                        FieldDefs[I].Name, { description }
                        FFType,
                        FFSize,
                        FFDecPl,
                        Required,
                        Nil,
                        blNone,
                        '',
                        rNone,
                        False,
                        duNormal);
                    End; { with }
          End; { if }

        { Obtain the indices. }
        IndexDefs.Update;
        For I := 0 To IndexDefs.Count - 1 Do
          Begin
            With IndexDefs[I] Do {!!.10}
              If Not (ixExpression In Options) Then
                Begin {!!.10}
                  ParseFieldNames(Fields);
                  IdxName := Name;
                  If IdxName = '' Then
                    If ixPrimary In Options Then
                      IdxName := 'FS$PRIMARY'
                    Else
                      IdxName := 'FS$INDEX' + IntToStr(I + 1);
                            FldCase[0] := byte(ixCaseInsensitive In Options);
                  FldAscDesc[0] := byte(Not (ixDescending In Options));
                  Dict.AddIndex(IdxName, { index name }
                    '', { description }
                    0, { file no }
                    NFields, { field count }
                    FldArray, { field list }
                    FldAscDesc,
                    FldCase,
                    fldsize,
                    fldflags,
                    FldNullTop,
                    IHelpers, { index helper list }
                    Not (ixUnique In Options){ allow dups });
                End; { if } {!!.10}
          End;

        { Create the actual table }
        Check(dbDest.CreateTable(False, FFTableName, Dict))
      End;
  Finally
    Dict.Free;
  End;
End;

Procedure TfrmMain.InitBDETable;
Var
  I: Integer;
  Flag: Longint;
Begin
  If lstadoTables.SelCount > 1 Then
    Begin
      lstadoFields.Clear;
      lstadoFields.Items.Add('<All fields will be converted for each table>');
      lstadoFields.Enabled := False;
      lstadoFields.Color := clBtnFace;
    End
  Else
    Begin
      lstadoFields.Color := clWindow;
      lstadoFields.Enabled := True;
      tblSource.close;
      With tblSource Do
        Begin
          { Find the selected table. }
          For I := 0 To Pred(lstadoTables.Items.Count) Do
            If lstadoTables.Selected[I] Then
              Begin
                TableName := lstadoTables.Items[I];
                break;
              End; { if }
          FieldDefs.Update;
          lstadoFields.Clear;
          For I := 0 To FieldDefs.Count - 1 Do
            Begin
              Flag := FG_SELECTED;
              lstadoFields.Items.AddObject(FieldDefs[I].Name, Pointer(Flag));
            End; { for }
        End; { with }
    End;
  BDETableInited := True;
End;

Function TfrmMain.InitCommsEngine: Boolean;
Begin
  cmbadoAliases.Clear;
  cmbFsAliases.Clear;
  Result := True;
  Try
    Session.Open;
    LoadAliases;
  Except
    On E: Exception Do
      Begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
        Result := False;
      End;
  End;
End;

Procedure TfrmMain.IniTfsTable;
Begin
  With tblDest Do
    Begin
      If Active Then Close;
      TableName := edTfsTableName.Text;
    End;
  FFTableInited := True;
End;

Procedure TfrmMain.LoadAliases;
Var
  Aliases: TStringList;
  I: Integer;
Begin
  { Segregate the FSSQL and native BDE aliases }
  Aliases := TStringList.Create;
  Try
    Aliases.Clear;
    Session.GetAliasNames(Aliases);
    With Aliases Do
      Begin
        For I := 0 To Count - 1 Do
          cmbFsAliases.Items.Add(Strings[I]);
        cmbFsAliases.ItemIndex := -1;
      End;
  Finally
    Aliases.Free;
  End;
End;

Procedure TfrmMain.LoadBDETables;
Begin
  If cmbadoAliases.Text <> '' Then
    Begin
      Try {!!.13}
      Connection1.Connected:=false;
      Connection1.ConnectionString := cmbadoAliases.Text;
          Connection1.Connected:= true;
        Connection1.GetTableNames(lstadoTables.Items,false);
      Except {!!.13}
        { ignore all bde exceptions }{!!.13}
      End; {!!.13}
      BDETablesLoaded := True;
    End;
End;

Procedure TfrmMain.LoadFFTables;
Var
  FFTables: TStringList;
  I: Integer;
  TableName: String;
Begin
  If cmbFsAliases.Text <> '' Then
    Begin
      dbDest.Connected := False;
      dbDest.AliasName := cmbFsAliases.Text;
      dbDest.DataBaseName := 'FS_' + cmbFsAliases.Text;
      dbDest.Connected := True;

      lsTfsTables.Clear;
      FFTables := TStringList.Create;
      Try
        Session.GetTableNames(cmbFsAliases.Text, '', True, False, FFTables);
        With FFTables Do
          For I := 0 To Count - 1 Do
            Begin
              TableName := Copy(Strings[I], 1, Pos('.', Strings[I]) - 1);
              lsTfsTables.Items.Add(TableName);
            End;
      Finally
        FFTables.Free;
      End;
      FFTablesLoaded := True;
    End;
End;

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  IsSQLServer := False;
End;

Procedure TfrmMain.lstadoFieldsDblClick(Sender: TObject);
Begin
  With (Sender As TListBox) Do
    If (Longint(Items.Objects[ItemIndex]) And FG_UNAVAILABLE) <> 0 Then
      MessageBeep(0)
    Else
      Begin
        Items.Objects[ItemIndex] := Pointer((Longint(Items.Objects[ItemIndex]) + 1) Mod 2);
        Invalidate;
      End;
End;

Procedure TfrmMain.lstadoFieldsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
Begin
  With (Control As TListBox) Do
    Begin
      With Canvas Do
        Begin
          Font.Assign(Font);

          If (odSelected) In State Then
            Begin
              Font.Color := clWindowText;
              Brush.Color := (Control As TListBox).Color;
            End;

          FillRect(Rect);

          If (Longint(Items.Objects[Index]) And FG_SELECTED) <> 0 Then
            With imgCheck.Picture.Bitmap Do
              BrushCopy(Bounds(Rect.Left + 2, Rect.Top + 4, Width, Height),
                imgCheck.Picture.Bitmap, Bounds(0, 0, Width, Height),
                TransparentColor);

          If (Longint(Items.Objects[Index]) And FG_UNAVAILABLE) <> 0 Then
            Font.Color := clRed;

          { Draw the item text }
          TextOut(Rect.Left + imgCheck.Picture.Bitmap.Width + 4, Rect.Top, Items[Index]);
        End;
    End;
End;

Procedure TfrmMain.ConvertTable(Const BDETableName, FFTableName: TfsTableName);
Var
  I: Integer;
  Msg,
    BDETableNameFinal: String;
  NewTable,
    MultTables: Boolean;
  NumTransferred: Longint;
  SourceFields: TStringList;
  ZMsg: Array[0..255] Of Char;
Begin

  MultTables := (lstadoTables.SelCount > 1);

  { Init vars }
  Aborted := False;
  NewTable := False;
  NumTransferred := 0;
  tblDest.TableName := FFTableName;

  { If the user selected a table in a SQL Server database then strip the
    leading database name from the table name. }
  BDETableNameFinal := BDETableName;
  If IsSQLServer And (Pos('.', BDETableNameFinal) > 0) Then
    Begin
      I := 1;
      While BDETableNameFinal[I] <> '.' Do
        inc(I);
      Delete(BDETableNameFinal, 1, I);
    End; { if }
  tblSource.TableName := BDETableNameFinal;
  tblSource.FieldDefs.Update;

  Try
    { Check for schema only import }
    If chkSchemaOnly.Checked Then
      Begin
        If (Not tblDest.Exists) Then
          Begin
            Msg := 'Create new table ' + FFTableName + ' from schema only?';
            NewTable := True;
          End
        Else
          Msg := 'Replace table ' + FFTableName + ' from schema only?';

        { If multiple tables being converted or user approves, recreate the
          table. }
        If MultTables Or
          (MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes) Then
          Begin
            If Not NewTable Then
              tblDest.DeleteTable;
            CreateNewTable(BDETableName, FFTableName);
          End
        Else
          Aborted := True;
      End
    Else
      Begin
        { Data only or data & schema. }
        Case grpExistingData.ItemIndex Of
          0: { Keep existing structure & data }
            If Not tblDest.Exists Then
              Begin
                If MultTables Or
                  (MessageDlg('Create new table ' + edTfsTableName.Text + '?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes) Then
                  Begin
                    CreateNewTable(BDETableName, FFTableName);
                    NewTable := True;
                  End; { if }
              End;
          1: { Keep existing structure, replace data }
            If tblDest.Exists Then
              { Empty the table. }
              tblDest.EmptyTable
            Else
              Begin
                CreateNewTable(BDETableName, FFTableName);
                NewTable := True;
              End;
          2: { Replace structure & data }
            If MultTables Or
              (Not tblDest.Exists) Or
              (MessageDlg('Replace table ' + edTfsTableName.Text + '?',
              mtConfirmation, [mbYes, mbNo], 0) = mrYes) Then
              Begin
                If tblDest.Exists Then
                  tblDest.DeleteTable;
                CreateNewTable(BDETableName, FFTableName);
                NewTable := True;
              End
            Else
              Exit;
        End; { case }

        { Begin the transfer process }
        Self.Enabled := False;
        Try
          Try
            SourceFields := TStringList.Create;
            Try

              { If more than one table has been selected then convert all
                fields otherwise convert only those selected in the fields list. }
              If (lstadoTables.SelCount > 1) Then
                Begin
                  For I := 0 To Pred(tblSource.FieldDefs.Count) Do
                    SourceFields.Add(AnsiUpperCase(tblSource.FieldDefs[I].Name));
                End
              Else
                Begin
                  With lstadoFields Do
                    For I := 0 To Items.Count - 1 Do
                      If (Longint(Items.Objects[I]) And FG_SELECTED) <> 0 Then
                        SourceFields.Add(AnsiUpperCase(Items[I]));
                End; { if }

              Aborted := Not DoImport(tblSource, SourceFields,
                tblDest, 100, NumTransferred);
            Finally
              SourceFields.Free;
            End;
          Except
            Aborted := True;
            Raise;
          End;

        Finally
          { If we've aborted and we created a new table, get rid of it }
          If Aborted Then
            Begin
              If NewTable Then
                Begin
                  tblDest.DeleteTable;
                  NewTable := False;
                End;
            End;

          Application.ProcessMessages;
          Self.Enabled := True;
        End;
      End;
  Finally
  End;

  If Not Aborted Then
    Begin
      If NewTable Then LoadFFTables;
      MessageBeep(0);
      StrPCopy(ZMsg, 'Transfer Completed.            ' + #13#13 +
        Format('%d records transferred.', [NumTransferred]));
      If lstadoTables.SelCount = 1 Then
        Application.MessageBox(ZMsg, 'BDE Transfer to FSSQL',
          MB_ICONINFORMATION Or MB_OK);
    End;
  If Not Aborted Then ModalResult := mrOK;
End;

Procedure TfrmMain.btnTransferClick(Sender: TObject);
Var
  FFTableName: TfsTableName;
  Inx,i: Integer;
Begin

  { Check Requirements }
  If (lstadoTables.SelCount = 0) Then
    Begin
      ShowMessage('Please select one or more BDE tables for conversion.');
      Exit;
    End;

  If cmbFsAliases.ItemIndex = -1 Then
    Begin
      ShowMessage('Please specify a target FSSQL database.');
      Exit;
    End;

  If (lstadoTables.SelCount = 1) And (edTfsTableName.Text = '') Then
    Begin
      ShowMessage('Please specify a destination FSSQL table.');
      Exit;
    End;

  If tblDest.Active Then
    tblDest.Close;

  tblDest.DataBaseName := 'FS_' + cmbFsAliases.Text;

  For Inx := 0 To Pred(lstadoTables.Items.Count) Do
    Begin
      If lstadoTables.Selected[Inx] Then
        Begin
          If lstadoTables.SelCount > 1 Then
            FFTableName := ChangeFileExt(lstadoTables.Items[Inx], '')
          Else
            FFTableName := edTfsTableName.Text;
          ConvertTable(lstadoTables.Items[Inx], FFTableName)
        End;
    End; { for }
  i := cmbFSAliases.ItemIndex;
  cmbFSAliasesChange(Nil);
  cmbFSAliases.ItemIndex := i;
End;

Procedure TfrmMain.btnExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.edtBDEAliasNameChange(Sender: TObject);
Begin
  BDETablesLoaded := False;
  BDETableInited := False;
End;

Procedure TfrmMain.edtBDEAliasNameExit(Sender: TObject);
Begin
  If Not BDETablesLoaded Then LoadBDETables;
End;

Procedure TfrmMain.edtBDEAliasNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not BDETablesLoaded Then
        LoadBDETables;
      Key := #0;
    End;
End;

Procedure TfrmMain.edtBDETableNameChange(Sender: TObject);
Begin
  BDETableInited := False;
End;

Procedure TfrmMain.edtBDETableNameExit(Sender: TObject);
Begin
  If Not BDETableInited Then InitBDETable;
End;

Procedure TfrmMain.edtBDETableNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not BDETableInited Then InitBDETable;
      Key := #0;
    End;
End;

Procedure TfrmMain.edtFsTableNameChange(Sender: TObject);
Begin
  FFTableInited := False;
End;

Procedure TfrmMain.edtFsTableNameExit(Sender: TObject);
Begin
  If Not FFTableInited Then IniTfsTable;
End;

Procedure TfrmMain.edtFsTableNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not FFTableInited Then IniTfsTable;
      Key := #0;
    End;
End;

Procedure TfrmMain.lstFsTablesDblClick(Sender: TObject);
Begin
  With lsTfsTables Do
    If ItemIndex <> -1 Then
      Begin
        edTfsTableName.Text := Items[ItemIndex];
        IniTfsTable;
      End;
End;

Procedure TfrmMain.lstadoTablesClick(Sender: TObject);
Var
  Inx: Integer;
Begin
  InitBDETable;
  IniTfsTable;
  If (lstadoTables.SelCount = 1) Then
    Begin
      For Inx := 0 To Pred(lstadoTables.Items.Count) Do
        If lstadoTables.Selected[Inx] Then
          Begin
            edTfsTableName.Text := ChangeFileExt(lstadoTables.Items[Inx], '');
            Break;
          End;
    End;
End;

Procedure TfrmMain.cmbFsAliasesChange(Sender: TObject);
Begin
  FFTablesLoaded := False;
  FFTableInited := False;
  LoadFFTables;
End;

Procedure TfrmMain.chkClearEmptyStringsClick(Sender: TObject);
Begin
  chkEmptyStrings.Checked := Not chkClearEmptyStrings.Checked;
End;

Procedure TfrmMain.chkEmptyStringsClick(Sender: TObject);
Begin
  chkClearEmptyStrings.Checked := Not chkEmptyStrings.Checked;
End;

Procedure TfrmMain.cmbAdoAliasesChange(Sender: TObject);
Begin
  //IsSQLServer := (DBTables.Session.GetAliasDriverName(cmbadoAliases.Text) = csSQLServer);
  LoadBDETables;
End;

Procedure TfrmMain.SpeedButton2Click(Sender: TObject);
Var
  fName: String;
Begin
  fName := '';
  fName := PromptDataSource(Handle, '');
  If fName <> '' Then
    cmbAdoAliases.text := fName;
End;

Procedure TfrmMain.SpeedButton1Click(Sender: TObject);
Var
  fName: String;
Begin
  fName := PromptDataLinkFile(Handle, '');
  If fName <> '' Then
    Begin
      fName := 'FILE NAME=' + fName;
      cmbAdoAliases.text := fName;
    End;
End;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  InitCommsEngine;
  pgTransfer.ActivePage := tabSource;
end;

End.

