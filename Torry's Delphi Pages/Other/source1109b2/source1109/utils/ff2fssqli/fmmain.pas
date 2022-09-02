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
  ffdb,
  fsllcomm,
  fslllgcy,
  fsllcomp,
  fslleng,
  fssrintm,
  fsserverremoteclass;

Type
  TfrmMain = Class(TForm)
    btnTransfer: TBitBtn;
    btnExit: TBitBtn;
    imgCheck: TImage;
    mnuMain: TMainMenu;
    mnuOperations: TMenuItem;
    mnuExit: TMenuItem;
    N1: TMenuItem;
    mnuTransferActiveTable: TMenuItem;
    pgTransfer: TPageControl;
    tabSource: TTabSheet;
    tabOptions: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lstffTables: TListBox;
    lstffFields: TListBox;
    tabTarget: TTabSheet;
    Label3: TLabel;
    Label5: TLabel;
    edtFSTableName: TEdit;
    lstFSTables: TListBox;
    cmbffAliases: TComboBox;
    cmbFSAliases: TComboBox;
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
    tblsource: TffTable;
    dbDest: TFSDatabase;
    tblDest: TFSTable;
    fsSession: TFSSession;
    FSClient1: TFSClient;
    FSRemoteServer1: TFSRemoteServer;
    FSParamConnect1: TFSParamConnect;
    Procedure btnTransferClick(Sender: TObject);
    Procedure btnExitClick(Sender: TObject);
    Procedure lstffFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    Procedure lstffFieldsDblClick(Sender: TObject);
    Procedure edtffAliasNameChange(Sender: TObject);
    Procedure edtffAliasNameExit(Sender: TObject);
    Procedure edtffAliasNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure edtffTableNameChange(Sender: TObject);
    Procedure edtffTableNameExit(Sender: TObject);
    Procedure edtffTableNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure edtFSTableNameChange(Sender: TObject);
    Procedure edtFSTableNameExit(Sender: TObject);
    Procedure edtFSTableNameKeyPress(Sender: TObject; Var Key: Char);
    Procedure lstFSTablesDblClick(Sender: TObject);
    Procedure cmbffAliasesChange(Sender: TObject);
    Procedure lstffTablesClick(Sender: TObject);
    Procedure cmbFSAliasesChange(Sender: TObject);
    Procedure chkClearEmptyStringsClick(Sender: TObject);
    Procedure chkEmptyStringsClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Protected
    FFTablesLoaded: Boolean;
    FFTableInited: Boolean;
    FSTablesLoaded: Boolean;
    FSTableInited: Boolean;
    Aborted: Boolean;
    Procedure ConvertTable(Const ffTableName, FsTableName: TfsTableName);
    Procedure CreateNewTable(Const ffTableName, FsTableName: TfsTableName);
    Procedure IniffTable;
    Function InitCommsEngine: Boolean;
    Procedure InifsTable;
    Procedure LoadAliases;
    Procedure LoadffTables;
    Procedure LoadFsTables;
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.DFM}

Const
  FG_UNSELECTED = 0;
  FG_SELECTED = 1;
  FG_UNAVAILABLE = 2;

Procedure TfrmMain.CreateNewTable(Const ffTableName, FsTableName: TfsTableName);
Var
  Dict: TFSInfoDict;
  I: Integer;
  IdxName: String;
  FFType: TfsFieldType;
  FFSize: Longint;
  FFDecPl: Integer;
  FldArray: TffFieldList;
  FldCase, FldSize, FldAscDesc, FldFlags, FldNullTop: TffFieldList;
  IHelpers: TffFieldIHList;
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
      FldSize[NFields] := 0;
      FldFlags[NFields] := 0;
      FldNullTop[NFields] := 1;
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

        If lstffTables.SelCount > 1 Then
          Begin
            For I := 0 To Pred(FieldDefs.Count) Do
              Begin
                With FieldDefs[I] Do
                  Begin
                    ConvertffDataType(DataType, Size, FFType, FFSize, FFDecPl);
                    BlockSize := BlockSize + FFSize;
                  End; { if }
              End;
          End
        Else
          Begin
            { Calculate using only the fields selected in the fields list. }
            With lstffFields Do
              For I := 0 To Items.Count - 1 Do
                If (Longint(Items.Objects[I]) And FG_SELECTED) <> 0 Then
                  With FieldDefs[I] Do
                    Begin
                      ConvertffDataType(DataType, Size, FFType, FFSize, FFDecPl);
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
        TableName := ffTableName;
        ReadOnly := True;

        { Get the fields }
        FieldDefs.Update;

        { Obtain the field definitions. }
        If lstffTables.SelCount > 1 Then
          Begin
            { Convert all fields. }
            For I := 0 To Pred(FieldDefs.Count) Do
              Begin
                With FieldDefs[I] Do
                  Begin
                    ConvertffDataType(FieldDefs[I].DataType, FieldDefs[I].Size, FFType, FFSize, FFDecPl);
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
            With lstffFields Do
              For I := 0 To Items.Count - 1 Do
                If (Longint(Items.Objects[I]) And FG_SELECTED) <> 0 Then
                  With FieldDefs[I] Do
                    Begin
                      ConvertffDataType(FieldDefs[I].DataType, FieldDefs[I].Size, FFType, FFSize, FFDecPl);
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
                    Not (ixUnique In Options) { allow dups });
                End; { if } {!!.10}
          End;

        { Create the actual table }
        Check(dbDest.CreateTable(False, FFTableName, Dict))
      End;
  Finally
    Dict.Free;
  End;
End;

Procedure TfrmMain.IniffTable;
Var
  I: Integer;
  Flag: Longint;
Begin
  If lstffTables.SelCount > 1 Then
    Begin
      lstffFields.Clear;
      lstffFields.Items.Add('<All fields will be converted for each table>');
      lstffFields.Enabled := False;
      lstffFields.Color := clBtnFace;
    End
  Else
    Begin
      lstffFields.Color := clWindow;
      lstffFields.Enabled := True;
      With tblSource Do
        Begin
          DataBaseName := cmbffAliases.Text;
          { Find the selected table. }
          For I := 0 To Pred(lstffTables.Items.Count) Do
            If lstffTables.Selected[I] Then
              Begin
                TableName := lstffTables.Items[I];
                break;
              End; { if }
          FieldDefs.Update;
          lstffFields.Clear;
          Close;
          Open;
          For I := 0 To FieldDefs.Count - 1 Do
            Begin
              Flag := FG_SELECTED;
              lstffFields.Items.AddObject(FieldDefs[I].Name, Pointer(Flag));
            End; { for }
        End; { with }
    End;
  FFTableInited := True;
End;

Function TfrmMain.InitCommsEngine: Boolean;
Begin
  cmbffAliases.Clear;
  cmbFsAliases.Clear;
  Result := True;
  Try
    fsSession.Open;
    LoadAliases;
  Except
    On E: Exception Do
      Begin
        MessageDlg(E.Message, mtError, [mbOk], 0);
        Result := False;
      End;
  End;
End;

Procedure TfrmMain.InifsTable;
Begin
  With tblDest Do
    Begin
      If Active Then Close;
      TableName := edTfsTableName.Text;
    End;
  FSTableInited := True;
End;

Procedure TfrmMain.LoadAliases;
Var
  Aliases: TStringList;
  I: Integer;
Begin
  { Segregate the FSSQL and native ff aliases }
  Aliases := TStringList.Create;
  Try
    ffdb.Session.GetAliasNames(Aliases);
    With Aliases Do
      Begin
        For I := 0 To Count - 1 Do
          cmbffAliases.Items.Add(Strings[I]);
        cmbffAliases.ItemIndex := 0;
        LoadffTables;
      End;
    Aliases.Clear;
    fsSession.GetAliasNames(Aliases);
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

Procedure TfrmMain.LoadffTables;
Var
  FFTables: TStringList;
  I: Integer;
  TableName: String;
Begin
  Try
    lstffTables.Items.Clear;
    FFTables := TStringList.Create;
    Try
      ffdb.Session.GetTableNames(cmbffAliases.Text, '', True, False, FFTables);
      With FFTables Do
        For I := 0 To Count - 1 Do
          Begin
            TableName := Copy(Strings[I], 1, Pos('.', Strings[I]) - 1);
            lstffTables.Items.Add(TableName);
          End;
    Finally
      FFTables.Free;
    End;
  Except
  End;
  FFTablesLoaded := True;
End;

Procedure TfrmMain.LoadFsTables;
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
        fsSession.GetTableNames(cmbFsAliases.Text, '', True, False, FFTables);
        With FFTables Do
          For I := 0 To Count - 1 Do
            Begin
              TableName := Copy(Strings[I], 1, Pos('.', Strings[I]) - 1);
              lsTfsTables.Items.Add(TableName);
            End;
      Finally
        FFTables.Free;
      End;
      FSTablesLoaded := True;
    End;
End;

Procedure TfrmMain.lstffFieldsDblClick(Sender: TObject);
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

Procedure TfrmMain.lstffFieldsDrawItem(Control: TWinControl; Index: Integer;
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

Procedure TfrmMain.ConvertTable(Const ffTableName, FsTableName: TfsTableName);
Var
  I: Integer;
  Msg,
    ffTableNameFinal: String;
  NewTable,
    MultTables: Boolean;
  NumTransferred: Longint;
  SourceFields: TStringList;
  ZMsg: Array[0..255] Of Char;
Begin

  MultTables := (lstffTables.SelCount > 1);

  { Init vars }
  Aborted := False;
  NewTable := False;
  NumTransferred := 0;
  tblDest.TableName := FFTableName;

  { If the user selected a table in a SQL Server database then strip the
    leading database name from the table name. }
  ffTableNameFinal := ffTableName;
  tblSource.TableName := ffTableNameFinal;
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
            CreateNewTable(ffTableName, FFTableName);
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
                    CreateNewTable(ffTableName, FFTableName);
                    NewTable := True;
                  End; { if }
              End;
          1: { Keep existing structure, replace data }
            If tblDest.Exists Then
              { Empty the table. }
              tblDest.EmptyTable
            Else
              Begin
                CreateNewTable(ffTableName, FFTableName);
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
                CreateNewTable(ffTableName, FFTableName);
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
              If (lstffTables.SelCount > 1) Then
                Begin
                  For I := 0 To Pred(tblSource.FieldDefs.Count) Do
                    SourceFields.Add(AnsiUpperCase(tblSource.FieldDefs[I].Name));
                End
              Else
                Begin
                  With lstffFields Do
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
      If lstffTables.SelCount = 1 Then
        Application.MessageBox(ZMsg, 'FF Transfer to FSSQL',
          MB_ICONINFORMATION Or MB_OK);
    End;
  If Not Aborted Then ModalResult := mrOK;
End;

Procedure TfrmMain.btnTransferClick(Sender: TObject);
Var
  FFTableName: TfsTableName;
  Inx, i: Integer;
Begin

  { Check Requirements }
  If (lstffTables.SelCount = 0) Then
    Begin
      ShowMessage('Please select one or more FSSQL tables for conversion.');
      Exit;
    End;

  If cmbFsAliases.ItemIndex = -1 Then
    Begin
      ShowMessage('Please specify a target FSSQL database.');
      Exit;
    End;

  If (lstffTables.SelCount = 1) And (edTfsTableName.Text = '') Then
    Begin
      ShowMessage('Please specify a destination FSSQL table.');
      Exit;
    End;

  If tblDest.Active Then
    tblDest.Close;

  tblDest.DataBaseName := 'FS_' + cmbFsAliases.Text;

  For Inx := 0 To Pred(lstffTables.Items.Count) Do
    Begin
      If lstffTables.Selected[Inx] Then
        Begin
          If lstffTables.SelCount > 1 Then
            FFTableName := ChangeFileExt(lstffTables.Items[Inx], '')
          Else
            FFTableName := edTfsTableName.Text;
          ConvertTable(lstffTables.Items[Inx], FFTableName)
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

Procedure TfrmMain.edtffAliasNameChange(Sender: TObject);
Begin
  FFTablesLoaded := False;
  FFTableInited := False;
End;

Procedure TfrmMain.edtffAliasNameExit(Sender: TObject);
Begin
  If Not FFTablesLoaded Then LoadffTables;
End;

Procedure TfrmMain.edtffAliasNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not FFTablesLoaded Then
        LoadffTables;
      Key := #0;
    End;
End;

Procedure TfrmMain.edtffTableNameChange(Sender: TObject);
Begin
  FFTableInited := False;
End;

Procedure TfrmMain.edtffTableNameExit(Sender: TObject);
Begin
  If Not FFTableInited Then IniffTable;
End;

Procedure TfrmMain.edtffTableNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not FFTableInited Then IniffTable;
      Key := #0;
    End;
End;

Procedure TfrmMain.edtFSTableNameChange(Sender: TObject);
Begin
  FSTableInited := False;
End;

Procedure TfrmMain.edtFSTableNameExit(Sender: TObject);
Begin
  If Not FSTableInited Then InifsTable;
End;

Procedure TfrmMain.edtFSTableNameKeyPress(Sender: TObject; Var Key: Char);
Begin
  If (Key = #13) Then
    Begin
      If Not FSTableInited Then InifsTable;
      Key := #0;
    End;
End;

Procedure TfrmMain.lstFSTablesDblClick(Sender: TObject);
Begin
  With lsTfsTables Do
    If ItemIndex <> -1 Then
      Begin
        edTfsTableName.Text := Items[ItemIndex];
        InifsTable;
      End;
End;

Procedure TfrmMain.cmbffAliasesChange(Sender: TObject);
Begin
  LoadffTables;
End;

Procedure TfrmMain.lstffTablesClick(Sender: TObject);
Var
  Inx: Integer;
Begin
  IniffTable;
  InifsTable;
  If (lstffTables.SelCount = 1) Then
    Begin
      For Inx := 0 To Pred(lstffTables.Items.Count) Do
        If lstffTables.Selected[Inx] Then
          Begin
            edTfsTableName.Text := ChangeFileExt(lstffTables.Items[Inx], '');
            Break;
          End;
    End;
End;

Procedure TfrmMain.cmbFSAliasesChange(Sender: TObject);
Begin
  FSTablesLoaded := False;
  FSTableInited := False;
  LoadFsTables;
End;

Procedure TfrmMain.chkClearEmptyStringsClick(Sender: TObject);
Begin
  chkEmptyStrings.Checked := Not chkClearEmptyStrings.Checked;
End;

Procedure TfrmMain.chkEmptyStringsClick(Sender: TObject);
Begin
  chkClearEmptyStrings.Checked := Not chkEmptyStrings.Checked;
End;

Procedure TfrmMain.FormShow(Sender: TObject);
Begin
  InitCommsEngine;
  pgTransfer.ActivePage := tabSource;
End;

End.

