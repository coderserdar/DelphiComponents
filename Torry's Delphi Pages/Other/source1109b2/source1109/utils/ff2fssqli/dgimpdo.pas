{$I fsdefine.inc}

Unit dgimpdo;

Interface

Uses
  Windows,
  SysUtils,
  Dialogs,
  Classes,
  DBTables,
  Graphics,
  Forms,
  Controls,
  Stdctrls,
  DB,
  adodb,
  Buttons,
  ExtCtrls,
  Gauges,
  dbconsts,
  ffllbase,
  ffsrbde,
  ffdb,
  ffdbbase,
  fsllbase,
  fssrbde,
  fsdb,
  fsdbbase;

Type
  TdlgImportProgress = Class(TForm)
    Bevel1: TBevel;
    lblProgress: TLabel;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    edtImportFilename: TEdit;
    edtTablename: TEdit;
    guaProgress: TGauge;
    Procedure btnCancelClick(Sender: TObject);
  Private
  Public
    Terminated: Boolean;

    Procedure ShowProgress(aImportFilename, aTableName: String);
    Procedure UpdateProgress(aNumRead, aTotalRecs: Longint);
  End;

Procedure ConvertffDataType(aDataType: TFieldType;
  aSize: Longint;
  Var aFFType: TfsFieldType;
  Var aFFSize: Longint;
  Var aFFDecPl: Integer);

Function DoImport(aSourceTable: TffTable; { Table to copy from }
  aSourceFields: TStringList; { List of field #'s to copy }
  aDestTable: TfsTable; { Table to copy to }
  aBlockInserts: Smallint; { Transaction batch size }
  Var aNumTransferred: Longint): Boolean; { Number of records copied }

Var
  dlgImportProgress: TdlgImportProgress;

Implementation

{$R *.DFM}

Uses
  fsclintf,
  fmmain;

Procedure ConvertffDataType(aDataType: TFieldType;
  aSize: Longint;
  Var aFFType: TfsFieldType;
  Var aFFSize: Longint;
  Var aFFDecPl: Integer);
Begin
  aFFSize := aSize;
  aFFDecPl := 0;
  Case aDatatype Of
    ftFixedChar,
      ftString:
      If aSize <= 255 Then
        Begin
          If frmMain.chkUseZeroTerminatedStrings.Checked Then
            aFFType := fstNullString
          Else
            aFFType := fstShortString;
        End
      Else
        aFFType := fstNullString;
    ftWideString:
      aFFType := fstWideString;
    ftSmallint:
      aFFType := fstInt16;
    ftInteger:
      aFFType := fstInt32;
    ftWord:
      aFFType := fstUInt16;
    ftBoolean:
      aFFType := fstBoolean;
    ftFloat:
      aFFType := fstDouble;
    ftCurrency:
      aFFType := fstCurrency;
    ftBCD:
      aFFType := fstCurrency;
    ftDate:
      If frmMain.chkUseSysToolsDates.Checked Then
        aFFType := fStDate
      Else
        aFFType := fstDateTime;
    ftTime:
      If frmMain.chkUseSysToolsTimes.Checked Then
        aFFType := fStTime
      Else
        aFFType := fstDateTime;
    ftDateTime:
      aFFType := fstDateTime;
    ftBytes,
      ftVarBytes:
      aFFType := fstArrayUInt16;
    ftBlob:
      aFFType := fstBLOB;
    ftMemo:
      aFFType := fstBLOBMemo;
    ftGraphic:
      aFFType := fstBLOBGraphic;
    ftAutoInc:
      aFFType := fstAutoInc32;
    ftFmtMemo:
      aFFType := fstBLOBMemo;
    ftParadoxOle,
      ftDBaseOle:
      aFFType := fstBLOB;
    ftTypedBinary:
      aFFType := fstBLOB;
  End;
End;

Function DoImport(aSourceTable: TffTable;
  aSourceFields: TStringList;
  aDestTable: TfsTable;
  aBlockInserts: Smallint;
  Var aNumTransferred: Longint): Boolean;

Resourcestring
  SInvalidFieldKind = 'Invalid Field Conversion  %s <- %s';
Var
  FieldNo: Integer;
  DestFieldNo: Integer;
  TotalRecs: Longint;
  DoThisOne: Boolean;
  DoExplicitTrans: Boolean;
  InTransaction: Boolean;
  MaxAutoInc: Int64;
  TempStr: String; {!!.01}
  TempStrW: Variant;

  Procedure CopyField(aDestField, aSourceField: TField);
  Var
    Buffer: Pointer;
    Stream: TMemoryStream;
  Begin
    {Begin !!.11}
    If aSourceField.IsNull Then
      Begin
        If frmMain.chkEmptyStrings.Checked And
          (aSourceField.DataType = ftString) Then
          aDestField.AsString := ''
        Else
          aDestField.Clear;
      End
    Else
      {End !!.11}
      Case aSourceField.DataType Of
        ftBoolean:
          Case aDestField.DataType Of
            ftBoolean:
              aDestField.AsBoolean := aSourceField.AsBoolean;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftString, ftFixedChar, ftWideString:
          Case aDestField.DataType Of
            ftString, ftFixedChar:
              Begin
                If frmMain.chkClearEmptyStrings.Checked Then
                  Begin
                    TempStr := aSourceField.AsString;
                    If TempStr = '' Then
                      aDestField.Clear
                    Else
                      aDestField.AsString := TempStr;
                  End
                Else
                  aDestField.AsString := aSourceField.AsString;
                If frmMain.chkOEMAnsi.Checked And
                  (Length(aDestField.AsString) > 0) Then
                  Begin
                    SetLength(TempStr, Length(aDestField.AsString));
                    tempStr := aDestField.AsString;
                    OEMToCharBuff(PChar(tempStr), PChar(tempStr), Length(aDestField.AsString));
                    aDestField.AsString := tempStr;
                  End;
              End;
            ftWideString:
              Begin
                If frmMain.chkClearEmptyStrings.Checked Then
                  Begin
                    TempStrW := aSourceField.AsVariant;
                    If TempStrW = '' Then
                      aDestField.Clear
                    Else
                      aDestField.AsVariant := TempStrW;
                  End
                Else
                  aDestField.AsVariant := aSourceField.AsVariant;
                If frmMain.chkOEMAnsi.Checked And
                  (Length(aDestField.AsString) > 0) Then
                  Begin
                    SetLength(TempStr, Length(aDestField.AsString));
                    tempStr := aDestField.AsString;
                    OEMToCharBuff(PChar(tempStr), PChar(tempStr), Length(aDestField.AsString));
                    aDestField.AsString := tempStr;
                  End;
              End;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftAutoInc,
          ftSmallint,
          ftInteger,
          ftWord:
          Case aDestField.DataType Of
            ftSmallint,
              ftInteger,
              ftAutoInc,
              ftWord:
              Begin
                aDestField.AsInteger := aSourceField.AsInteger;
                If (aDestField.DataType = ftAutoInc) And
                  (aDestField.AsInteger > MaxAutoInc) Then
                  MaxAutoInc := aDestField.AsInteger;
              End;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftBCD,
          ftFloat,
          ftCurrency:
          Case aDestField.DataType Of
            ftFloat,
              ftCurrency:
              aDestField.AsFloat := aSourceField.AsFloat;
            ftBCD: aDestField.Value := aSourceField.AsFloat;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftDate:
          Case aDestField.DataType Of
            ftDate,
              ftDateTime:
              aDestField.AsDateTime := aSourceField.AsDateTime;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftTime:
          Case aDestField.DataType Of
            ftTime,
              ftDateTime:
              aDestField.AsDateTime := aSourceField.AsDateTime;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftDateTime:
          Case aDestField.DataType Of
            ftDate,
              ftTime,
              ftDateTime:
              aDestField.AsDateTime := aSourceField.AsDateTime;
            Else
              DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                aSourceField.DisplayName]);
          End;
        ftBytes,
          ftVarBytes:
          Begin
            GetMem(Buffer, aDestField.DataSize);
            Try
              Case aDestField.DataType Of
                ftBytes,
                  ftVarBytes:
                  If aSourceField.GetData(Buffer) Then
                    aDestField.SetData(Buffer)
                  Else
                    aDestField.SetData(Nil);
                ftFmtMemo,
                  ftParadoxOle,
                  ftDBaseOle,
                  ftTypedBinary,
                  ftMemo,
                  ftGraphic,
                  ftBlob:
                  If Not aSourceField.GetData(Buffer) Then
                    aDestField.SetData(Nil)
                  Else
                    Begin
                      Stream := TMemoryStream.Create;
                      Try
                        Stream.Write(Buffer^, aSourceField.DataSize);
                        TBLOBField(aDestField).LoadFromStream(Stream);
                      Finally
                        Stream.Free;
                      End;
                    End;
                Else
                  DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                    aSourceField.DisplayName]);
              End;
            Finally
              FreeMem(Buffer, aDestField.DataSize);
            End;
          End;
        ftFmtMemo,
          ftParadoxOle,
          ftDBaseOle,
          ftTypedBinary,
          ftMemo,
          ftGraphic,
          ftBlob:
          Begin
            Case aDestField.DataType Of
              ftFmtMemo,
                ftParadoxOle,
                ftDBaseOle,
                ftTypedBinary,
                ftMemo,
                ftGraphic,
                ftBlob:
                Begin
                  Stream := TMemoryStream.Create;
                  Try
                    TBLOBField(aSourceField).SaveToStream(Stream);
                    TBLOBField(aDestField).LoadFromStream(Stream);
                  Finally
                    Stream.Free;
                  End;
                End;
              Else
                DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
                  aSourceField.DisplayName]);
            End;
          End;
        ftUnknown:
          DatabaseErrorFmt(SInvalidFieldKind, [aDestField.DisplayName,
            aSourceField.DisplayName]);
      End;
  End;

Begin
  Result := False;
  With dlgImportProgress Do
    Begin
      Terminated := False;
      ShowProgress(aSourceTable.TableName, aDestTable.TableName);
      Try

        { If we only have one insert per transaction, then let the server
          do implicit transactions; it'll be faster }
        If aBlockInserts = 0 Then aBlockInserts := 1;
        DoExplicitTrans := (aBlockInserts > 1);

        aSourceTable.Open;
        Try
          TotalRecs := aSourceTable.RecordCount;
          aNumTransferred := 0;

          aDestTable.Open;
          If (DoExplicitTrans) Then {!!.05}
            DoExplicitTrans := (Not aDestTable.Dictionary.HasBLOBFields); {!!.05}
          Try
            MaxAutoInc := 0;
            InTransaction := False;
            Try
              While Not aSourceTable.EOF Do
                Begin
                  If DoExplicitTrans And Not InTransaction Then
                    Begin
                      frmMain.dbDest.StartTransaction;
                      InTransaction := True;
                    End;

                  aDestTable.Insert;

                  { Copy fields one at a time }
                  For FieldNo := 0 To aSourceTable.FieldCount - 1 Do
                    Begin

                      { Do only selected fields }
                      DoThisOne := Not Assigned(aSourceFields);
                      If Not DoThisOne Then
                        Begin
                          DoThisOne := aSourceFields.IndexOf(AnsiUpperCase(aSourceTable.Fields[FieldNo].FieldName)) <> -1;
                        End;

                      If DoThisOne Then
                        Begin

                          { Fields might be in order, avoid expensive FieldByName }
                          If (FieldNo < aDestTable.FieldCount) And
                            (FFCmpShStrUC(aSourceTable.Fields[FieldNo].FieldName,
                            aDestTable.Fields[FieldNo].FieldName,
                            255) = 0) Then
                            DestFieldNo := FieldNo
                          Else
                            Begin
                              Try
                                DestFieldNo := aDestTable.FieldByName(aSourceTable.Fields[FieldNo].FieldName).FieldNo - 1;
                              Except
                                DestFieldNo := -1;
                              End;
                            End;

                          If DestFieldNo <> -1 Then
                            Try
                              CopyField(aDestTable.Fields[DestFieldNo], aSourceTable.Fields[FieldNo]);
                              {End !!.11}
                              If (aDestTable.Fields[DestFieldNo].DataType = ftAutoInc) And
                                (aDestTable.Fields[DestFieldNo].AsInteger > MaxAutoInc) Then
                                MaxAutoInc := aDestTable.Fields[DestFieldNo].AsInteger;
                            Except
                              On E: EDatabaseError Do
                                Begin
                                  CopyField(aDestTable.Fields[DestFieldNo], aSourceTable.Fields[FieldNo]);
                                End;
                              Else
                                Raise;
                            End;
                        End;
                    End;

                  aDestTable.Post;
                  Inc(aNumTransferred); { Increment after successfully posting }

                  { See if it's time to commit the transaction }
                  If InTransaction Then
                    Begin
                      If ((aNumTransferred Mod aBlockInserts) = 0) Then
                        Begin
                          aDestTable.Database.Commit;
                          UpdateProgress(aNumTransferred, TotalRecs);
                          InTransaction := False;
                        End
                    End
                  Else
                    UpdateProgress(aNumTransferred + 1, TotalRecs);

                  { Check for user termination }
                  If Terminated Then
                    Begin
                      If InTransaction Then
                        aDestTable.Database.Rollback;
                      Exit;
                    End;

                  aSourceTable.Next;
                End;

              {update the maximum autoinc value for the dest table}
              aDestTable.SetTableAutoIncValue(MaxAutoInc, 1);
              { Residual inserts need to be posted? }
              If InTransaction Then
                Begin {!!.01}
                  aDestTable.Database.Commit;
                  UpdateProgress(aNumTransferred + 1, TotalRecs); {!!.01}
                End; {!!.01}
            Except
              If InTransaction Then
                aDestTable.Database.Rollback;
              Raise;
            End;
          Finally
            aDestTable.Close;
          End;
        Finally
          aSourceTable.Close;
        End;
      Finally
        Hide;
      End;
      Result := Not Terminated;
    End;
End;

Procedure TdlgImportProgress.ShowProgress(aImportFilename, aTableName: String);
Begin
  edtImportFilename.Text := aImportFilename;
  edtTablename.Text := aTableName;
  lblProgress.Hide;
  guaProgress.Progress := 0;
  Inherited Show;
  Application.ProcessMessages;
End;

Procedure TdlgImportProgress.UpdateProgress(aNumRead, aTotalRecs: Longint);
Var
  Dividend: Longint;
  Divisor: Longint;
Resourcestring
  SProgressStatus = 'Processing record %d of %d';
Begin
  With lblProgress Do
    Begin
      Caption := Format(SProgressStatus, [aNumRead, aTotalRecs]);
      Show;
      Application.ProcessMessages;
    End;

  { Calculate % completed }
  If (aNumRead >= $1000000) Then
    Begin
      Dividend := (aNumRead Shr 7) * 100;
      Divisor := aTotalRecs Shr 7;
    End
  Else
    Begin
      Dividend := aNumRead * 100;
      Divisor := aTotalRecs;
    End;

  If Divisor <> 0 Then
    guaProgress.Progress := Dividend Div Divisor;
End;

Procedure TdlgImportProgress.btnCancelClick(Sender: TObject);
Resourcestring
  SAbortMsg = 'Abort transferring data?';
Begin
  Terminated := MessageDlg(SAbortMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
End;

End.

