{$I fsdefine.inc}

Unit fsutil;

Interface
Uses
  Db,
  fsdb,
  Windows,
  Messages,
  Classes,
  SysUtils,
  fsllprot,
  fslldict,
  fsllbase,
  fsstdate,
  fszlib,
  fsfuninterp,
  fssrbde,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  fsserverclass;

Type
  TfsCopyTableProgressEvent = Procedure(Index: Integer);
  TfsDefaultType = (dtNormal, dtNow, dtYear, dtMonth, dtDay, dtWeekNo, dtUser);

Function fsDirectoryExists(Const Name: String): Boolean;
Function fsForceDirectories(Dir: String): Boolean;
Function IsProcedure(Const sProc: String; Var Param: String; OnlyExec: boolean = False): String;
Procedure fsKillExternalServer(Const aBlocking: Boolean);
{ - Kill the server process running locally }

Function FsGetMaxAutoInc(aTable: TFSTable): Int64;
{ - Retrieve the MaxAutoInc value used in a table. This routine does
    not get the last one used, instead it queries each record and
    returns the highest key currently in the table. }

Function FsGetProtocolString(Protocol: TfsProtocolType): String;
{ - Converts a TfsProtocolType value to a string value }

Function FsGetProtocolType(ProtocolStr: String): TfsProtocolType;
{ - Converts the specified string to a valid TfsProtocolType }

Procedure FsSeparateAddress(Const Original: String;
  Var Name: String;
  Var Address: String);
{ - Breaks the specifid address into Name & address parts }

Function FsTransferRecord(Source, Dest: TDataSet): Boolean;
{ - Transfers the current record from Source to Dest, matching fields by
    name. The result will be true if the routine was successful. }

Procedure FsCopyTableData(SourceTable, DestTable: TDataset);
{ - Transfers the records from SourceTable to DestTable. SourceTable's
    cursor will be First and finish at EOF. Ranges and Filters will not
    be disturbed by this routine }

Procedure FsCopyTableDataEx(SourceTable, DestTable: TDataset; ProgressEvent: TfsCopyTableProgressEvent);
{ - Transfers the records from SourceTable to DestTable. SourceTable's
    cursor will be First and finish at EOF. Ranges and Filters will not
    be disturbed by this routine. This routine has the ability to
    call a progress event }

Function FsCheckDefaultType(Const S: String): TfsDefaultType;

Procedure FsStringToVCheckVal(Const aStr: String;
  Const aType: TfsFieldType;
  Var aVal: TffVCheckValue);
{ Converts a string to a TffVCheckValue. Used to set the default
  for a field in the data dictionary }

Function FsVCheckValToString(Const aVal: TffVCheckValue;
  Const aType: TfsFieldType): String;
{ Converts a TffVCheckValue to a string. Used to retrieve a string
  representation of the default value for a field in a data
  dictionary }

//Procedure fsZCompress( Level: TfsZCompressionLevel; aInput, aOutput: TStream );
Procedure fsZCompress(Level: TfsZCompressionLevel; inpStream, outStream: TStream);
//Procedure fsZDecompress( aInput, aOutput: TStream );
Procedure fsZDecompress(inpStream, outStream: TStream);
Procedure fsSetBlobValue(Const Value: Variant; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray);
Function fsIsValidTimestamp(Const S: ShortString): boolean;
Function VrStrToTimestamp(Const S: ShortString): TDateTime;
Function fsIsValidDate(Const S: ShortString): Boolean;
Function VrStrToDate(Const S: ShortString): TDateTime;
Function fsIsValidTime(Const S: ShortString): Boolean;
Function VrStrToTime(Const S: ShortString): TDateTime;
Procedure fsCurSetValue(Const Value: Variant; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Procedure fsCurSetValueE(Const Value: Extended; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Procedure fsCurSetValueStrE(Const Value: Extended; Dict: TFSInfoDict; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Function fsGetBlobValue(CursorID: TffCursorID; iField: Integer; aData: PffByteArray): Variant;
Procedure fsGetBlobStrings(CursorID: TffCursorID; iField: Integer; aData: PffByteArray; Strings: TStrings);
Function fsCurGetValue(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray; IsSql: boolean): Variant;
Function fsGetValueStr(Dict: TFSInfoDict; iField: Integer; aData, FieldBuffer: PffByteArray): Variant;
Function fsCurGetValueSql(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray; IsSql, IsNull: boolean): Variant;
Function fsCurGetValueE(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray): Extended;
Function fsCurGetValueStrE(Dict: TFSInfoDict; iField: Integer; aData, FieldBuffer: PffByteArray): Extended;

Procedure fsStringToCurrency(Const S: AnsiString; Var V: Currency; Var Code: Integer);
Procedure fsStringToExtended(Const S: AnsiString; Var V: Extended; Var Code: Integer);
Function fsFloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Function fsFloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Function fsFloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Procedure fsFloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals: Integer);
Function fsFormatFloat(Const format: String; Value: Extended): String;
Function fsFloatToStr(Value: Extended): String;
Function fsCurrToStr(Value: Currency): String;
Function fsCurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): String;
Function fsFormatCurr(Const Format: String; Value: Currency): String;
Function fsStrToFloat(Const S: String): Extended;
Function fsStrToCurr(Const S: String): Currency;
Function RoundExtended(E: Extended; Decimals: Integer; aRound: TRound): Extended;

Implementation

Uses
  fsclcfg,
  fsclconv;

Function fsDirectoryExists(Const Name: String): Boolean;
Var
  Code: Integer;
Begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) And (FILE_ATTRIBUTE_DIRECTORY And Code <> 0);
End;

Function fsForceDirectories(Dir: String): Boolean;
Begin
  Result := True;
  If Length(Dir) = 0 Then
    Raise Exception.Create('Cannot Create Dir!');
  Dir := ExcludeTrailingBackslash(Dir);
  If (Length(Dir) < 3) Or fsDirectoryExists(Dir)
    Or (ExtractFilePath(Dir) = Dir) Then Exit; // avoid 'xyz:\' problem.
  Result := fsForceDirectories(ExtractFilePath(Dir)) And CreateDir(Dir);
End;

Function IsProcedure(Const sProc: String; Var Param: String; OnlyExec: boolean = False): String;
Var
  P, SVar1, SVar2, SName, ProcedureName: String;
  i: Integer;

  Function VrRemoveQuotes(Const St: String): String;
  Var
    S: String;
  Begin
    S := St;
    If (Length(s) > 2) And (s[1] = '"') Then
      Begin
        If (Length(s) > 2) And (s[1] = '"') And (s[Length(s)] = '"') Then
          Result := Copy(s, 2, Length(s) - 2)
        Else
          Result := s;
      End
    Else
      Begin
        If (Length(s) > 2) And (s[1] = '''') And (s[Length(s)] = '''') Then
          Result := Copy(s, 2, Length(s) - 2)
        Else
          Result := s;
      End;
  End;
Begin
  Result := '';
  SName := sProc;
  SName := Trim(SName);
  SName := VrRemoveQuotes(SName);
  If SName = '' Then Exit;

  SVar1 := AnsiUpperCase(SName);
  SVar2 := Copy(SVar1, 1, 9);
  SVar1 := Copy(SVar1, 1, 17);

  // no procedure
  If ('EXECUTEPROCEDURE.' = SVar1) Or ('EXECPROC.' = SVar2) Then
    Begin
      i := Pos('.', SName);
      If i > 0 Then
        Begin
          ProcedureName := Copy(SName, i + 1, length(SName));
          // procedurename
          If ProcedureName <> '' Then
            Begin
              Result := ProcedureName;
              // if exists param of procedure
              i := Pos('(', ProcedureName);
              If i > 0 Then
                Begin
                  // param
                  P := Copy(ProcedureName, i, Length(ProcedureName));
                  P := Copy(P, 1, Length(P));
                  ProcedureName := Copy(ProcedureName, 1, i - 1);
                  Param := P;
                  Result := ProcedureName;
                End;
            End;
        End;
    End
  Else
    Begin
      If OnlyExec Then Exit;
      ProcedureName := SName;
      // procedurename
      If ProcedureName <> '' Then
        Begin
          Result := ProcedureName;
          // if exists param of procedure
          i := Pos('(', ProcedureName);
          If i > 0 Then
            Begin
              // param
              P := Copy(ProcedureName, i, Length(ProcedureName));
              P := Copy(P, 1, Length(P));
              ProcedureName := Copy(ProcedureName, 1, i - 1);
              Param := P;
              Result := ProcedureName;
            End;
        End;
    End;
End;

Procedure fsKillExternalServer(Const aBlocking: Boolean);
Var
  Handle: THandle;
Begin
  Handle := FindWindowEx(0, 0, 'TfrmFSQLServer', Nil);
  If Handle > 0 Then
    If aBlocking Then
      SendMessage(Handle, WM_Close, 0, 0)
    Else
      PostMessage(Handle, WM_Close, 0, 0);
End;
{--------}

Function FsGetMaxAutoInc(aTable: TFSTable): Int64;
Var
  MaxSeed: Int64;
  m: Longint;
  AField: TAutoIncField;
Begin
  AField := Nil;
  For M := 0 To Pred(aTable.FieldCount) Do
    If aTable.Fields[m] Is TAutoIncField Then
      Begin
        AField := TAutoIncField(aTable.Fields[m]);
        Break;
      End;
  If Not Assigned(AField) Then
    Begin
      Result := 0;
      Exit;
    End;

  MaxSeed := 0;
  aTable.First;
  While Not aTable.EOF Do
    Begin
      If AField.Value > MaxSeed Then
        MaxSeed := AField.Value;
      aTable.Next;
    End;
  Result := MaxSeed;
End;
{--------}

Function FsGetProtocolString(Protocol: TfsProtocolType): String;
Begin
  Case Protocol Of
    ptIPXSPX: Result := fsc_IPXSPX;
    ptTCPIP: Result := fsc_TCPIP;
    Else
      Result := fsc_SingleUser;
  End;
End;
{--------}

Function FsGetProtocolType(ProtocolStr: String): TfsProtocolType;
Begin
  If ProtocolStr = fsc_IPXSPX Then
    Result := ptIPXSPX
  Else If ProtocolStr = fsc_TCPIP Then
    Result := ptTCPIP
  Else
    Result := ptSingleUser;
End;
{--------}

{--------}

Procedure FsSeparateAddress(Const Original: String; Var Name,
  Address: String);
Var
  SepPlace: Integer;
  ServerName: String;
Begin
  ServerName := Original;
  SepPlace := Pos('@', ServerName);
  If SepPlace > 0 Then
    Begin
      Name := Copy(ServerName, 1, pred(SepPlace));
      Delete(ServerName, 1, SepPlace);
      Address := ServerName;
    End
  Else
    Begin
      Name := ServerName;
    End;
End;
{--------}

Function FsTransferRecord(Source, Dest: TDataSet): Boolean;
Var
  i, nErr: Integer;
  f1, f2: TField;
Begin
  nErr := 0;
  If (dest.state In [dsEdit, dsInsert]) {= dsBrowse} Then
    Begin
    End
  Else
    Begin
      Dest.Edit;
    End;
  For I := 0 To (Dest.FieldCount - 1) Do
    Begin
      f1 := Dest.FindField(Dest.Fields[I].FieldName);
      f2 := Source.FindField(Dest.Fields[I].FieldName);
      If (((f1 <> Nil) And (f2 <> Nil)) And (dest.Fields[I].FieldName <> 'RefNum') And (dest.Fields[I].FieldName <> 'AutoInc')) Then
        Begin
          Try
            f1.Value := f2.Value;
          Except
            inc(nErr);
          End;
        End
      Else
        Begin
        End;
    End;
  Dest.Post;
  Result := (nErr < Dest.FieldCount);
End;
{--------}

Procedure FsCopyTableData(SourceTable, DestTable: TDataset);
Begin
  SourceTable.First;
  While Not SourceTable.EOF Do
    Begin
      DestTable.Insert;
      FsTransferRecord(SourceTable, DestTable);
      SourceTable.Next;
    End;
End;

Procedure FsCopyTableDataEx(SourceTable, DestTable: TDataset; ProgressEvent: TfsCopyTableProgressEvent);
Var
  Idx: Integer;
Begin
  SourceTable.First;
  Idx := 0;
  While Not SourceTable.EOF Do
    Begin
      DestTable.Insert;
      FsTransferRecord(SourceTable, DestTable);
      inc(Idx);
      If Assigned(ProgressEvent) Then
        ProgressEvent(Idx);
      SourceTable.Next;
    End;
End;

Function FsCheckDefaultType(Const S: String): TfsDefaultType;
Var
  S1: String;
Begin
  Result := dtNormal;
  s1 := s;
  s1 := Trim(Uppercase(s1));
  If s1 = '<NOW>' Then
    Result := dtNow
  Else If s1 = '<Y>' Then
    Result := dtYear
  Else If s1 = '<M>' Then
    Result := dtMonth
  Else If s1 = '<D>' Then
    Result := dtDay
  Else If s1 = '<W>' Then
    Result := dtWeekNo
  Else If s1 = '<USER>' Then
    Result := dtUser;
End;

Procedure FsStringToVCheckVal(Const aStr: String;
  Const aType: TfsFieldType;
  Var aVal: TffVCheckValue);
Var
  TempStr: String[255];
  TempInt: Longint;
  TempExtend: Extended;
  TempCurrency: Currency;
  TempSingle: Single;
  TempDouble: Double;
  TempStDate: TStDate;
  TempStTime: TStTime;
  TempDT: TDateTime;
  TempTS: TTimeStamp;
  TempComp: Int64; //Comp;
  dt: TfsDefaultType; //= (dtNow, dtYear, dtMonth, dtDay, DtWeekNo, dtUser);
  Vc: TffVCheckValue;

Begin
  FillChar(Vc, SizeOf(Vc), 0);
  If (aStr <> '') Then
    Begin
      dt := FsCheckDefaultType(aStr);
      FillChar(aVal, SizeOf(TffVCheckValue), #0);
      Case aType Of
        fstDateTime:
          Begin
            If dt = dtNow Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempDT := StrToDateTime(aStr);
                TempTS := DateTimeToTimeStamp(TempDT);
                TempDT := TimeStampToMSecs(TempTS);
                MapDataToFS(fstDateTime, sizeof(TDateTime), @TempDT, @aVal);
              End;
          End;
        fstDate:
          Begin
            If dt = dtNow Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempStDate := FFStringToStDate(aStr);
                Move(TempStDate, aVal, sizeof(TStDate));
              End;
          End;
        fstTime:
          Begin
            If dt = dtNow Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempStTime := FFStringToStTime(aStr);
                Move(TempStTime, aVal, sizeof(TStTime));
              End;
          End;
        fstUInt16:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempInt := StrToInt(aStr);
                MapDataToFS(fstUInt16, sizeof(Word), @TempInt, @aVal);
              End;
          End;
        fstUInt32:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempInt := StrToInt(aStr);
                MapDataToFS(fstUInt32, sizeof(TffWord32), @TempInt, @aVal);
              End;
          End;
        fstInt8:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempInt := StrToInt(aStr);
                MapDataToFS(fstInt8, sizeof(Shortint), @TempInt, @aVal);
              End;
          End;
        fstInt16:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(aStr)));
              End
            Else
              Begin
                TempInt := StrToInt(aStr);
                MapDataToFS(fstInt16, sizeof(Smallint), @TempInt, @aVal);
              End;
          End;
        fstInt32:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, succ(Length(TempStr)));
              End
            Else
              Begin
                TempInt := StrToInt(aStr);
                MapDataToFS(fstInt32, sizeof(Longint), @TempInt, @aVal);
              End;
          End;
        fstAutoInc32:
          Begin
            TempInt := StrToInt(aStr);
            MapDataToFS(fstAutoInc32, sizeof(Longint), @TempInt, @aVal);
          End;

        fstSingleChar:
          Begin
            TempStr := aStr;
            MapDataToFS(fstSingleChar, sizeof(Char), @TempStr[1], @aVal);
          End;
        fstSingleWideChar:
          Begin
            StringToWideChar(aStr, @TempStr, sizeof(WideChar));
            MapDataToFS(fstSingleWideChar, sizeof(WideChar), @TempStr, @aVal);
          End;
        fstUInt8:
          Begin
            TempInt := StrToInt(aStr);
            MapDataToFS(fstUInt8, sizeof(Byte), @TempInt, @aVal);
          End;
        fstSingle:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempSingle := StrToFloat(aStr);
                Move(TempSingle, aVal, sizeof(Single));
              End;
          End;
        fstDouble:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempDouble := StrToFloat(aStr);
                MapDataToFS(fstDouble, sizeof(Double), @TempDouble, @aVal);
              End;
          End;
        fstExtended:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempExtend := StrToFloat(aStr);
                Move(TempExtend, aVal, 10);
              End;
          End;
        fstAutoInc64:
          Begin
            TempComp := StrToint64(aStr);
            Move(TempComp, aVal, sizeof(Int64));
          End;
        fstInt64, fstRecVersion:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempComp := StrToint64(aStr);
                Move(TempComp, aVal, sizeof(Int64));
              End;
          End;
        fstCurrency:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempCurrency := StrTocurr(aStr);
                Move(TempCurrency, aVal, sizeof(Comp));
              End;
          End;
        {fstBinaryDecimals :
          begin
            TempCurrency := StrToCurr(aStr);
            Move(TempCurrency, aVal, sizeof(Currency));
          End ; }

        fstBoolean:
          Begin
            If ((UpperCase(aStr) = 'TRUE') Or (UpperCase(aStr) = 'T')) Then
              TempInt := 1
            Else If ((UpperCase(aStr) = 'FALSE') Or (UpperCase(aStr) = 'F')) Then
              TempInt := 0;
            MapDataToFS(fstBoolean, sizeof(Boolean), @TempInt, @aVal);
          End;
        fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
          Begin
            TempStr := Trim(aStr);
            Move(TempStr, aVal, Succ(Length(aStr)));
            //MapDataToFS(fstUInt8Array, sizeof(fscl_MaxVCheckLength), @TempStr, @aVal);
          End;
        fstShortString:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow, dtUser] Then
              Begin
                TempStr := Trim(Uppercase(aStr));
                Move(TempStr, aVal, Succ(Length(TempStr)));
              End
            Else
              Begin
                TempStr := aStr;
                Move(TempStr, aVal, Succ(Length(aStr)));
              End;
          End;

        fstNullString, fstVarNullString:
          Begin
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow, dtUser] Then
              TempStr := Trim(Uppercase(aStr))
            Else
              TempStr := aStr;
            MapDataToFS(fstNullString, succ(Length(TempStr)), @TempStr[1], @aVal);
          End;
        fstWideString, fstVarWideString:
          Begin
            dt := FsCheckDefaultType(aStr);
            If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow, dtUser] Then
              TempStr := Trim(Uppercase(aStr))
            Else
              TempStr := aStr;
            Try
              StringToWideChar(TempStr, PWideChar(@aVal), (Length(TempStr) * 2));
            Except
            End;
          End;
      End;
    End;
End;

Function FsVCheckValToString(Const aVal: TffVCheckValue;
  Const aType: TfsFieldType): String;

Var
  TempStr, S: String[255];
  TempInt16: Smallint;
  TempInt64: TffInt64;
  TempInt: Longint;
  TempExtend: Extended;
  TempCurrency: Currency;
  TempSingle: Single;
  TempDouble: Double;
  TempStDate: TStDate;
  TempStTime: TStTime;
  TempDT: TDateTime;
  TempTS: TTimeStamp;
  TempComp: Int64;
  i: Integer;
  dt: TfsDefaultType; //= (dtNow, dtYear, dtMonth, dtDay, DtWeekNo, dtUser);
  Vc: TffVCheckValue;
Begin
  FillChar(Vc, SizeOf(Vc), 0);
  Case aType Of
    fstUInt16:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        While TempStr[succ(i)] <> #0 Do
          inc(i);
        SetLength(TempStr, i);
        dt := FsCheckDefaultType(TempStr);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(TempStr))
        Else
          Begin
            MapFFDataToBDE(fstUInt16, sizeof(Word), @aVal, @TempInt);
            TempStr := IntToStr(TempInt);
          End;
      End;
    fstUInt32:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        While TempStr[succ(i)] <> #0 Do
          inc(i);
        SetLength(TempStr, i);
        dt := FsCheckDefaultType(TempStr);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(TempStr))
        Else
          Begin
            MapFFDataToBDE(fstUInt32, sizeof(TffWord32), @aVal, @TempInt);
            TempStr := IntToStr(TempInt);
          End;
      End;
    fstInt8:
      Begin
        MapFFDataToBDE(fstInt8, SizeOf(Shortint), @aVal, @TempInt16); {!!.07}
        TempStr := IntToStr(TempInt16); {!!.07}
      End;
    fstInt16:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            MapFFDataToBDE(fstInt16, sizeof(Smallint), @aVal, @TempInt16);
            TempStr := IntToStr(TempInt16);
          End;
      End;
    fstInt32:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            MapFFDataToBDE(fstInt32, sizeof(Longint), @aVal, @TempInt);
            TempStr := IntToStr(TempInt);
          End;
      End;
    fstAutoInc32:
      Begin
        MapFFDataToBDE(fstAutoInc32, sizeof(Longint), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      End;

    fstDate:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempStDate, sizeof(TStDate));
            TempStr := FFStDateToString(TempStDate);
          End;
      End;
    fstTime:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempStTime, sizeof(TStTime));
            TempStr := FFStTimeToString(TempStTime);
          End;
      End;
    fstSingleChar:
      Begin
        TempStr := Char(aVal[0]);
      End;
    fstSingleWideChar:
      Begin
        TempStr := Trim(WideCharLenToString(PWideChar(@aVal), 2));
      End;
    fstUInt8:
      Begin
        MapFFDataToBDE(fstUInt8, sizeof(Byte), @aVal, @TempInt);
        TempStr := IntToStr(TempInt);
      End;
    fstSingle:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempSingle, sizeof(Single));
            TempStr := fsFloatToStr(TempSingle);
          End;
      End;
    fstDouble:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            MapFFDataToBDE(fstDouble, sizeof(Double), @aVal, @TempDouble);
            TempStr := fsFloatToStr(TempDouble);
          End;
      End;
    fstExtended:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempExtend, 10);
            TempStr := fsFloatToStr(TempExtend);
          End;
      End;
    fstCurrency:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo, dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempCurrency, sizeof(Currency));
            TempStr := fsFloatToStr(TempCurrency);
          End;
      End;
    {fstBinaryDecimals :
      begin
        Move(aVal, TempCurrency, sizeof(Currency));
        TempStr := CurrToStr(TempCurrency);
      End ; }

    fstInt64, fstRecVersion:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtYear, dtMonth, dtDay, dtWeekNo] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            Move(aVal, TempComp, sizeof(Int64));
            TempStr := IntToStr(TempComp);
          End;
      End;
    fstAutoInc64:
      Begin
        Move(aVal, TempComp, sizeof(Int64));
        TempStr := IntToStr(TempComp);
      End;
    fstDateTime:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @S[1]);
        i := 0;
        While S[succ(i)] <> #0 Do
          inc(i);
        SetLength(S, i);
        dt := FsCheckDefaultType(S);
        If dt In [dtNow] Then
          TempStr := Trim(UpperCase(S))
        Else
          Begin
            MapFFDataToBDE(fstDateTime, sizeof(TDateTime), @aVal, @TempDT);
            TempTS := MSecsToTimeStamp(TempDT);
            TempDT := TimeStampToDateTime(TempTS);
            TempStr := DateTimeToStr(TempDT);
          End;
      End;
    fstBoolean:
      Begin
        MapFFDataToBDE(fstBoolean, sizeof(Boolean), @aVal, @TempInt); {!!.12}
        If Byte(TempInt) = 0 Then
          TempStr := 'False'
        Else
          TempStr := 'True';
      End;
    fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        While TempStr[succ(i)] <> #0 Do
          inc(i);
        SetLength(TempStr, i);
      End;
    fstShortString:
      Begin
        MapFFDataToBDE(fstShortString, fscl_MaxVCheckLength, @aVal, @TempStr[1]);
        i := 0;
        While TempStr[succ(i)] <> #0 Do
          inc(i);
        SetLength(TempStr, i);
      End;
    fstNullString, fstVarNullString:
      Begin
        MapFFDataToBDE(fstNullString, pred(fscl_MaxVCheckLength), @aVal, @TempStr[1]);
        i := 0;
        While TempStr[succ(i)] <> #0 Do
          inc(i);
        SetLength(TempStr, i);
      End;
    fstWideString, fstVarWideString:
      Begin
        i := 0;
        While ((char(aVal[i])) +
          (char(aVal[succ(i)]))) <> #0#0 Do
          inc(i);
        Try
          TempStr := Trim(WideCharLenToString(PWideChar(@aVal), succ(i)));
        Except
          TempStr := '';
        End;
      End;
    fstBLOB..fstBLOBGraphic:
      Begin
        Move(aVal, TempInt64, SizeOf(TempInt64));
        TempStr := IntToStr(TempInt64.iHigh) + ':' + IntToStr(TempInt64.iLow);
      End;
    Else
      Begin
        TempStr := '';
      End;
  End;
  Result := TempStr;
End;

{procedure fsZCompress(Level: TfsZCompressionLevel; inpStream, outStream: TStream);
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  InpBuf := nil;
  OutBuf := nil;
  try
    GetMem(InpBuf, inpStream.Size);
    inpStream.Position := 0;
    InpBytes := inpStream.Read(InpBuf^, inpStream.Size);
    CompressBuf(InpBuf, InpBytes, OutBuf, OutBytes);
    outStream.Write(OutBuf^, OutBytes);
  finally
    if InpBuf <> nil then FreeMem(InpBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  End ;
End ;}

{ Decompress a stream }

{Procedure fsZDecompress(inpStream, outStream: TStream);
Var
  InpBuf, OutBuf: Pointer;
  OutBytes, sz: Longint;
Begin
  InpBuf := Nil;
  OutBuf := Nil;
  sz := inpStream.Size - inpStream.Position;
  If sz > 0 Then
  try
    Try
      GetMem(InpBuf, sz);
      inpStream.Read(InpBuf^, sz);
      DecompressBuf(InpBuf, sz, 0, OutBuf, OutBytes);
      outStream.Write(OutBuf^, OutBytes);
    Finally
      outStream.Position := 0;
      If InpBuf <> Nil Then FreeMem(InpBuf);
      If OutBuf <> Nil Then FreeMem(OutBuf);
    End;
    except
    outStream.Size:=0;
    End ;
End; }

Procedure fsZCompress(Level: TfsZCompressionLevel; inpStream, outStream: TStream);
Begin
  If inpStream.Size > 0 Then
    Begin
      With TfsZCompressionStream.Create(outStream, Level) Do
        Try
          CopyFrom(inpStream, 0);
        Finally
          Free;
        End;
    End
  Else
    outStream.Size := 0;
End;

Procedure fsZDecompress(inpStream, outStream: TStream);
Const
  BUFFER_SIZE = 4096;
Var
  aBuffer: Array[0..BUFFER_SIZE - 1] Of Byte;
  aCount: Integer;
Begin
  If inpStream.Size > 0 Then
    Begin
      Try
        With TfsZDecompressionStream.Create(inpStream) Do
          Try
            Repeat
              aCount := Read(aBuffer, BUFFER_SIZE);
              If aCount <> 0 Then
                outStream.WriteBuffer(aBuffer, aCount)
              Else
                break;
            Until False;
          Finally
            Free;
          End;
      Except
        outStream.Size := 0;
      End;
    End
  Else
    outStream.Size := 0;
End;

Procedure fsSetBlobValue(Const Value: Variant; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray);
Var
  offset: Integer;
  BLOBNr: TffInt64;
  Error,
    Len: Longint;
  ValueLen: TffWord32;
  ValueLocked: Boolean;
  VPtr: PAnsiChar;
  VStr: String;
  bl: TDataCompLevel;
  TmpStream, ResultStream: TMemoryStream;
  Buffer: pointer;
  bCursor: TfsSrBaseCursor;

  Procedure BCompress(Stream, aOutput: TMemoryStream; aTargetCompress: TDataCompLevel);
  Begin
    Case aTargetCompress Of
      blNone:
        Begin
          //
        End;
      blFastest:
        Begin
          aOutput.Position := 0;
          Stream.Position := 0;
          fsZCompress(zcFastest, Stream, aOutput);
          aOutput.Position := 0;
        End;
      blDefault:
        Begin
          aOutput.Position := 0;
          Stream.Position := 0;
          fsZCompress(zcDefault, Stream, aOutput);
          aOutput.Position := 0;
        End;
      blMax:
        Begin
          aOutput.Position := 0;
          Stream.Position := 0;
          fsZCompress(zcMax, Stream, aOutput);
          aOutput.Position := 0;
        End;
    End;
  End;

Begin
  Buffer := Nil;

  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;

  ValueLocked := False;
  Try
    If TVarData(Value).VType And VarTypeMask = varByte Then
      Begin
        ValueLen := VarArrayHighBound(Value, 1);
        VPtr := VarArrayLock(Value);
        ValueLocked := True;
      End
    Else
      Begin
        VStr := VarToStr(Value);
        ValueLen := Length(VStr);
        VPtr := PAnsiChar(VStr);
      End;

    offset := bcursor.Dictionary.FieldOffset[iField];
    BLOBNr := PffInt64(@aData^[Offset])^;

    bl := bCursor.Dictionary.FieldBlobLevelComp[iField];
    If (bl <> blNone) And (ValueLen > 0) Then
      Begin
        TmpStream := TMemoryStream.Create;
        ResultStream := TMemoryStream.Create;
        Try
          TmpStream.position := 0;
          GetMem(buffer, Valuelen + 5);
          If ValueLen > 0 Then
            TmpStream.writebuffer(Vptr^, ValueLen);
          If TmpStream.Size > 0 Then
            Begin
              TmpStream.position := 0;
              ResultStream.Position := 0;
              BCompress(TmpStream, ResultStream, bl);
              ResultStream.Position := 0;
              TmpStream.Size := 0;
              ValueLen := ResultStream.Size;
            End
          Else
            ValueLen := 0;
          If (BLOBNr.iLow <> 0) Or (BLOBNr.iHigh <> 0) Then
            Begin
              Len := bCursor.BLOBGetLength(BLOBNr, Error);
              If TffWord32(Len) > ValueLen Then
                bCursor.BLOBTruncate(BLOBNr, ValueLen);
              If ValueLen = 0 Then
                bCursor.Dictionary.SetRecordFieldNull(iField, aData, True)
              Else
                Begin
                  bCursor.BLOBWrite(BLOBNr, 0, ValueLen, ResultStream.memory^);
                End;
            End
          Else
            Begin
              If ValueLen = 0 Then
                bCursor.Dictionary.SetRecordFieldNull(iField, aData, True)
              Else If bCursor.BLOBAdd(BLOBNr) = 0 Then
                Begin
                  If bCursor.BLOBWrite(BLOBNr, 0, ValueLen, ResultStream.memory^) = 0 Then
                    bCursor.Dictionary.SetRecordField(iField, aData, @BLOBNr);
                  bCursor.BLOBFree(BLOBNr);
                End;
            End;
        Finally
          ResultStream.free;
          tmpStream.free;
          FreeMem(buffer);
        End;
      End
    Else
      Begin
        If (BLOBNr.iLow <> 0) Or (BLOBNr.iHigh <> 0) Then
          Begin
            Len := bCursor.BLOBGetLength(BLOBNr, Error);
            If TffWord32(Len) > ValueLen Then
              bCursor.BLOBTruncate(BLOBNr, ValueLen);
            If ValueLen = 0 Then
              bCursor.Dictionary.SetRecordFieldNull(iField, aData, True)
            Else
              Begin
                bCursor.BLOBWrite(BLOBNr, 0, ValueLen, VPtr^);
              End;
          End
        Else
          Begin
            If ValueLen = 0 Then
              bCursor.Dictionary.SetRecordFieldNull(iField, aData, True)
            Else If bCursor.BLOBAdd(BLOBNr) = 0 Then
              Begin
                If bCursor.BLOBWrite(BLOBNr, 0, ValueLen, VPtr^) = 0 Then
                  bCursor.Dictionary.SetRecordField(iField, aData, @BLOBNr);
                bCursor.BLOBFree(BLOBNr);
              End;
          End;
      End;
  Finally
    If ValueLocked Then
      VarArrayUnlock(Value);
  End;
End;

Function fsIsValidTimestamp(Const S: ShortString): boolean;
Begin
  If (length(S) < 19)
    Or Not (S[5] In ['-', '.', '/'])
    Or (S[8] <> S[5])
    Or (S[11] <> ' ')
    Or (S[14] <> ':')
    Or (S[17] <> ':') Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 1, 4)),
        StrToInt(copy(S, 6, 2)),
        StrToInt(copy(S, 9, 2)));
      EncodeTime(
        StrToInt(copy(S, 12, 2)),
        StrToInt(copy(S, 15, 2)),
        StrToInt(copy(S, 18, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End;
End;

Function VrStrToTimestamp(Const S: ShortString): TDateTime;
Begin
  Result := EncodeDate(
    StrToInt(copy(S, 1, 4)),
    StrToInt(copy(S, 6, 2)),
    StrToInt(copy(S, 9, 2)))
    +
    EncodeTime(
    StrToInt(copy(S, 12, 2)),
    StrToInt(copy(S, 15, 2)),
    StrToInt(copy(S, 18, 2)),
    0);
  fsSetMillisecond(Result, 0);
End;

Function fsIsValidDate(Const S: ShortString): Boolean;
Begin
  If (length(S) < 10)
    Or Not (S[5] In ['-', '.', '/'])
    Or (S[8] <> S[5]) Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 1, 4)),
        StrToInt(copy(S, 6, 2)),
        StrToInt(copy(S, 9, 2)));
      Result := True;
    Except
      // not yet
      Try
        EncodeDate(
          StrToInt(copy(S, 7, 4)),
          StrToInt(copy(S, 4, 2)),
          StrToInt(copy(S, 1, 2)));
        Result := True;
      Except
        Result := False;
      End;
    End;
End;

Function VrStrToDate(Const S: ShortString): TDateTime;
Begin
  Result := 0;
  Try
    Result := EncodeDate(
      StrToInt(copy(S, 1, 4)),
      StrToInt(copy(S, 6, 2)),
      StrToInt(copy(S, 9, 2)));
  Except
    Try
      EncodeDate(
        StrToInt(copy(S, 7, 4)),
        StrToInt(copy(S, 4, 2)),
        StrToInt(copy(S, 1, 2)));
    Except
      Raise;
    End;
  End;
End;

Function fsIsValidTime(Const S: ShortString): Boolean;
Begin
  If (length(S) < 8)
    Or (S[3] <> ':')
    Or (S[6] <> ':') Then
    Result := False
  Else
    Try
      EncodeTime(
        StrToInt(copy(S, 1, 2)),
        StrToInt(copy(S, 4, 2)),
        StrToInt(copy(S, 7, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End;
End;

Function VrStrToTime(Const S: ShortString): TDateTime;
Begin
  Result := EncodeTime(
    StrToInt(copy(S, 1, 2)),
    StrToInt(copy(S, 4, 2)),
    StrToInt(copy(S, 7, 2)),
    0);
End;

Procedure fsCurSetValue(Const Value: Variant; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Var
  S: String;
  W: WideString;
  FT: TfsFieldType;
  ValueIsNull: Boolean;
  LenW: Word;
  Len: Integer;
  C: Currency;
  E: Extended;
  dt: tdatetime;
  bCursor: TfsSrBaseCursor;
  FieldBufferLength: Integer;

Begin
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;

  FieldBufferLength := bCursor.Dictionary.FieldLength[iField];
  FT := bCursor.Dictionary.FieldType[iField];
  ValueIsNull := VarIsNull(Value);
  If ValueIsNull Then
    bCursor.Dictionary.SetRecordFieldNull(iField, aData, True)
  Else
    Begin
      Case FT Of
        fstBoolean:
          Boolean(FieldBuffer^[0]) := Value;
        fstSingleChar:
          Begin
            S := Value;
            char(FieldBuffer^[0]) := S[1];
          End;
        fstSingleWideChar:
          Begin
            W := Value;
            PWideChar(FieldBuffer)^ := W[1];
          End;
        fstUInt8:
          PByte(FieldBuffer)^ := Value;
        fstUInt16:
          PWord(FieldBuffer)^ := Value;
        fstUInt32:
          PFFWord32(FieldBuffer)^ := Value;
        fstInt8:
          PShortInt(FieldBuffer)^ := Value;
        fstInt16:
          PSmallInt(FieldBuffer)^ := Value;
        fstInt32, fstAutoInc32:
          PLongint(FieldBuffer)^ := Value;
        fstSingle:
          Begin
            PSingle(FieldBuffer)^ := Value
          End;
        fstDouble:
          Begin
            PDouble(FieldBuffer)^ := Value
          End;
        fstExtended:
          Begin
            PExtended(FieldBuffer)^ := Value
          End;
        fstInt64, fstAutoinc64, fstRecVersion:
          Begin
            E := Value;
            pint64(FieldBuffer)^ := Round(E);
          End;
        fstCurrency:
          Begin
            C := Value;
            Pint64(FieldBuffer)^ := Round(C * 10000.0);
          End;
        //fstBinaryDecimals:
        //  Begin
        //    C := VarAsType(Value, varcurrency);
        //    C := RoundFloat(C, GetPrecision, GetDecimals);
        //    PCurrency(FieldBuffer)^ := C;
        //  End;
        fstDate:
          Begin
            Try
              dt := Value;
            Except
              If fsIsValidDate(Value) Then
                dt := VrStrToDate(Value)
              Else
                Raise;
            End;
            PStDate(FieldBuffer)^ := DateTimeToStDate(dt);
          End;
        fstTime:
          Begin
            Try
              dt := Value;
            Except
              If fsIsValidTime(Value) Then
                dt := VrStrToTime(Value)
              Else
                Raise;
            End;
            PStTime(FieldBuffer)^ := DateTimeToStTime(dt);
          End;
        fstDateTime:
          Begin
            Try
              dt := Value;
            Except
              If fsIsValidTimestamp(Value) Then
                dt := VrStrToTimestamp(Value)
              Else
                Raise;
            End;
            PffDateTime(FieldBuffer)^ := dt + 693594;
          End;

        fstShortString:
          Begin
            S := Value;
            FillChar(FieldBuffer^, FieldBufferLength, 0);
            LenW := FFMinI(Length(S), Pred(FieldBufferLength));
            FieldBuffer[0] := LenW;
            If S <> '' Then
              Move(S[1], FieldBuffer[1], LenW);
          End;
        fstNullString, fstVarNullString:
          Begin
            S := Value;
            FillChar(FieldBuffer^, FieldBufferLength, 0);
            Len := FFMinI(Length(S), Pred(FieldBufferLength));
            If S <> '' Then
              Move(S[1], FieldBuffer^, Len);
          End;
        fstWideString, fstVarWideString {, fstUnicode}:
          Begin
            W := Value;
            FillChar(FieldBuffer^, FieldBufferLength, 0);
            If W <> '' Then
              Move(W[1], FieldBuffer^,
                FFMinI(Length(W) * 2, FieldBufferLength - 2));
          End;
        fstBLOB..fstBLOBGraphic:
          Begin
            fsSetBlobValue(Value, CursorID, iField, aData);
          End;
        Else
          Assert(False);
      End;
      bCursor.Dictionary.SetRecordField(iField, aData, FieldBuffer);
    End;
End;

Procedure fsCurSetValueE(Const Value: Extended; CursorID: TffCursorID; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Var
  FT: TfsFieldType;
  C: Currency;
  bCursor: TfsSrBaseCursor;
Begin
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;

  FT := bCursor.Dictionary.FieldType[iField];
  Case FT Of
    fstSingle:
      Begin
        PSingle(FieldBuffer)^ := Value
      End;
    fstDouble:
      Begin
        PDouble(FieldBuffer)^ := Value
      End;
    fstExtended:
      Begin
        PExtended(FieldBuffer)^ := Value
      End;
    fstCurrency:
      Begin
        C := Value;
        Pint64(FieldBuffer)^ := Round(C * 10000.0);
      End;
  End;
  bCursor.Dictionary.SetRecordField(iField, aData, FieldBuffer);
End;

Procedure fsCurSetValueStrE(Const Value: Extended; Dict: TFSInfoDict; iField: Integer; Var aData: PffByteArray; FieldBuffer: PffByteArray);
Var
  FT: TfsFieldType;
  C: Currency;
Begin
  FT := Dict.FieldType[iField];
  Case FT Of
    fstSingle:
      Begin
        PSingle(FieldBuffer)^ := Value
      End;
    fstDouble:
      Begin
        PDouble(FieldBuffer)^ := Value
      End;
    fstExtended:
      Begin
        PExtended(FieldBuffer)^ := Value
      End;
    fstCurrency:
      Begin
        C := Value;
        Pint64(FieldBuffer)^ := Round(C * 10000.0);
      End;
  End;
  Dict.SetRecordField(iField, aData, FieldBuffer);
End;

Function dsCheckBLOBHandle(Cursor: TfsSrBaseCursor; pRecBuf: Pointer;
  aiField: Integer;
  Var aIsNull: Boolean;
  Var aBLOBNr: TffInt64): TffResult;
Var
  TempI64: TffInt64;
Begin
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  Result := DBIERR_NONE;
  Cursor.Dictionary.GetRecordField(aiField, pRecBuf, aIsNull, @aBLOBNr);
  If (Not aIsNull) And (ffCmpI64(aBLOBNr, TempI64) = 0) Then
    aIsNull := True;
End;

Function fsGetBlobValue(CursorID: TffCursorID; iField: Integer; aData: PffByteArray): Variant;
Var
  offset: Integer;
  BLOBNr: TffInt64;
  Error, Len, LenS: Integer;
  BytesRead: TffWord32;
  bl: TDataCompLevel;
  TmpStream, ResultStream: TMemoryStream;
  Buffer: pointer;
  IsNull: Boolean;
  bCursor: TfsSrBaseCursor;
  StrS: AnsiString;

Begin
  Result := '';
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;
  offset := bcursor.Dictionary.FieldOffset[iField];
  BLOBNr.iLow := 0;
  BLOBNr.iHigh := 0;
  BLOBNr := PffInt64(@aData^[Offset])^;
  Error := dsCheckBLOBHandle(bcursor, @aData^[Offset], iField, IsNull, BLOBNr);
  If Error <> 0 Then Exit;

  BLOBNr := PffInt64(@aData^[Offset])^;
  len := bCursor.BLOBGetLength(BLOBNr, Error);
  If Len = 0 Then
    Begin
      Exit;
    End;

  TmpStream := TMemoryStream.Create;
  ResultStream := TMemoryStream.Create;
  GetMem(buffer, len + 5);
  Try
    If Error = 0 Then
      Begin
        Error := bCursor.BLOBRead(BLOBNr, 0, Len, buffer^, BytesRead);
        bCursor.BLOBFree(BLOBNr);
        If Error = 0 Then
          Begin
            If BytesRead > 0 Then
              TmpStream.writebuffer(buffer^, BytesRead);

            bl := bCursor.Dictionary.FieldBlobLevelComp[iField];
            If bl <> blNone Then
              Begin
                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    fsZDecompress(TmpStream, ResultStream);
                  End;
              End
            Else
              Begin
                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    ResultStream.CopyFrom(TmpStream, TmpStream.Size);
                  End;
              End;
            ResultStream.Position := 0;
            TmpStream.Size := 0;
            LenS := ResultStream.Size;
            If LenS > 0 Then
              Begin
                SetString(StrS, Nil, LenS);
                ResultStream.ReadBuffer(Pointer(StrS)^, LenS);
                Result := StrS;
              End
            Else
              Result := '';
          End;
      End;
  Finally
    ResultStream.free;
    tmpStream.free;
    FreeMem(buffer);
  End;
End;

Procedure fsGetBlobStrings(CursorID: TffCursorID; iField: Integer; aData: PffByteArray; Strings: TStrings);
Var
  offset: Integer;
  BLOBNr: TffInt64;
  Error, Len, LenS: Integer;
  BytesRead: TffWord32;
  bl: TDataCompLevel;
  TmpStream, ResultStream: TMemoryStream;
  Buffer: pointer;
  IsNull: Boolean;
  bCursor: TfsSrBaseCursor;
Begin
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;
  offset := bcursor.Dictionary.FieldOffset[iField];
  BLOBNr := PffInt64(@aData^[Offset])^;
  Error := dsCheckBLOBHandle(bcursor, @aData^[Offset], iField, IsNull, BLOBNr);
  If Error <> 0 Then Exit;

  len := bCursor.BLOBGetLength(BLOBNr, Error);
  If Len = 0 Then Exit;

  TmpStream := TMemoryStream.Create;
  ResultStream := TMemoryStream.Create;
  GetMem(buffer, len + 5);
  Try
    If Error = 0 Then
      Begin
        Error := bCursor.BLOBRead(BLOBNr, 0, Len, buffer^, BytesRead);
        bCursor.BLOBFree(BLOBNr);
        If Error = 0 Then
          Begin
            If BytesRead > 0 Then
              TmpStream.writebuffer(buffer^, BytesRead);

            bl := bCursor.Dictionary.FieldBlobLevelComp[iField];
            If bl <> blNone Then
              Begin
                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    fsZDecompress(TmpStream, ResultStream);
                  End;
              End
            Else
              Begin
                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    ResultStream.CopyFrom(TmpStream, TmpStream.Size);
                  End;
              End;
            ResultStream.Position := 0;
            TmpStream.Size := 0;
            LenS := ResultStream.Size;
            If LenS > 0 Then
              Strings.LoadFromStream(ResultStream);
          End;
      End;
  Finally
    ResultStream.free;
    tmpStream.free;
    FreeMem(buffer);
  End;
End;

Function fsCurGetValue(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray; IsSql: boolean): Variant;
Var
  D: Double;
  C: Currency;
  E: Extended;
  S: Single;
  W: WideString;
  WC: WideChar;
  DT: TDateTime;
  FType: TfsFieldType;
  bCursor: TfsSrBaseCursor;
Begin
  Result := Null;
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;

  FType := bCursor.Dictionary.FieldType[iField];

  Case FType Of
    fstBoolean:
      Begin
        Result := Boolean(FieldBuffer^[0]);
      End;
    fstSingleChar:
      Begin
        Result := Char(FieldBuffer^[0]);
      End;
    fstSingleWideChar:
      Begin
        WC := PWideChar(FieldBuffer)^;
        W := WC;
        Result := W;
      End;
    fstUInt8:
      Begin
        Result := PByte(FieldBuffer)^;
      End;
    fstUInt16:
      Begin
        Result := PWord(FieldBuffer)^;
      End;
    fstUInt32:
      Begin
        D := PffWord32(FieldBuffer)^;
        Result := D;
      End;
    fstInt8:
      Begin
        Result := PShortInt(FieldBuffer)^;
      End;
    fstInt16:
      Begin
        Result := PSmallInt(FieldBuffer)^;
      End;
    fstInt32, fstAutoInc32:
      Begin
        Result := PLongint(FieldBuffer)^;
      End;
    fstSingle:
      Begin
        S := PSingle(FieldBuffer)^;
        TVarData(Result).VType := VarSingle;
        TVarData(Result).VSingle := S;
      End;

    fstDouble:
      Begin
        D := PDouble(FieldBuffer)^;
        TVarData(Result).VType := VarDouble;
        TVarData(Result).VDouble := D;
      End;

    fstExtended:
      Begin
        E := PExtended(FieldBuffer)^;
        TVarData(Result).VType := VarDouble;
        TVarData(Result).VDouble := E;
      End;

    fstInt64, fstAutoinc64, fstRecVersion:
      Begin
        E := pint64(FieldBuffer)^;
        Result := E;
      End;

    // fstBinaryDecimals,
    fstCurrency:
      Begin
        TVarData(Result).VType := VarCurrency;
        C := Pint64(FieldBuffer)^ / 10000.0;
        TVarData(Result).VCurrency := C;
      End;
    fstDate:
      Begin
        Result := StDateToDateTime(PStDate(FieldBuffer)^);
      End;
    fstTime:
      Begin
        Result := StTimeToDateTime(PStTime(FieldBuffer)^);
      End;
    fstDateTime:
      Begin
        DT := PffDateTime(FieldBuffer)^ - 693594.0;
        Result := DT;
      End;
    fstShortString:
      Begin
        Result := PShortString(FieldBuffer)^;
      End;
    fstNullString, fstVarNullString:
      Begin
        Result := String(PChar(FieldBuffer));
      End;
    fstWideString, fstVarWideString {, fstUnicode}:
      Begin
        Result := WideString(PWideChar(FieldBuffer));
      End;
    fstBLOB..fstBLOBGraphic:
      Begin
        If Not IsSql Then
          Result := fsGetBlobValue(CursorID, iField, aData);
      End;
    Else
      Assert(False);
  End;
End;

Function fsGetValueStr(Dict: TFSInfoDict; iField: Integer; aData, FieldBuffer: PffByteArray): Variant;
Var
  D: Double;
  C: Currency;
  E: Extended;
  S: Single;
  W: WideString;
  WC: WideChar;
  DT: TDateTime;
  FType: TfsFieldType;

Begin
  Result := Null;

  FType := Dict.FieldType[iField];

  Case FType Of
    fstBoolean:
      Begin
        Result := Boolean(FieldBuffer^[0]);
      End;
    fstSingleChar:
      Begin
        Result := Char(FieldBuffer^[0]);
      End;
    fstSingleWideChar:
      Begin
        WC := PWideChar(FieldBuffer)^;
        W := WC;
        Result := W;
      End;
    fstUInt8:
      Begin
        Result := PByte(FieldBuffer)^;
      End;
    fstUInt16:
      Begin
        Result := PWord(FieldBuffer)^;
      End;
    fstUInt32:
      Begin
        D := PffWord32(FieldBuffer)^;
        Result := D;
      End;
    fstInt8:
      Begin
        Result := PShortInt(FieldBuffer)^;
      End;
    fstInt16:
      Begin
        Result := PSmallInt(FieldBuffer)^;
      End;
    fstInt32, fstAutoInc32:
      Begin
        Result := PLongint(FieldBuffer)^;
      End;
    fstSingle:
      Begin
        S := PSingle(FieldBuffer)^;
        TVarData(Result).VType := VarSingle;
        TVarData(Result).VSingle := S;
      End;

    fstDouble:
      Begin
        D := PDouble(FieldBuffer)^;
        TVarData(Result).VType := VarDouble;
        TVarData(Result).VDouble := D;
      End;

    fstExtended:
      Begin
        E := PExtended(FieldBuffer)^;
        TVarData(Result).VType := VarDouble;
        TVarData(Result).VDouble := E;
      End;

    fstInt64, fstAutoinc64, fstRecVersion:
      Begin
        E := pint64(FieldBuffer)^;
        Result := E;
      End;

    // fstBinaryDecimals,
    fstCurrency:
      Begin
        TVarData(Result).VType := VarCurrency;
        C := Pint64(FieldBuffer)^ / 10000.0;
        TVarData(Result).VCurrency := C;
      End;
    fstDate:
      Begin
        Result := StDateToDateTime(PStDate(FieldBuffer)^);
      End;
    fstTime:
      Begin
        Result := StTimeToDateTime(PStTime(FieldBuffer)^);
      End;
    fstDateTime:
      Begin
        DT := PffDateTime(FieldBuffer)^ - 693594.0;
        Result := DT;
      End;
    fstShortString:
      Begin
        Result := PShortString(FieldBuffer)^;
      End;
    fstVarNullString, fstNullString:
      Begin
        Result := String(PChar(FieldBuffer));
      End;
    fstWideString, fstVarWideString {, fstUnicode}:
      Begin
        Result := WideString(PWideChar(FieldBuffer));
      End;
    fstBLOB..fstBLOBGraphic: ;
    Else
      Assert(False);
  End;
End;

Function fsCurGetValueE(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray): Extended;
Var
  D: Double;
  C: Currency;
  E: Extended;
  S: Single;
  FType: TfsFieldType;
  bCursor: TfsSrBaseCursor;

Begin
  Result := 0;
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;

  FType := bCursor.Dictionary.FieldType[iField];

  Case FType Of
    fstSingle:
      Begin
        S := PSingle(FieldBuffer)^;
        Result := S;
      End;

    fstDouble:
      Begin
        D := PDouble(FieldBuffer)^;
        Result := D;
      End;

    fstExtended:
      Begin
        E := PExtended(FieldBuffer)^;
        Result := E;
      End;

    fstCurrency:
      Begin
        C := Pint64(FieldBuffer)^ / 10000.0;
        Result := C;
      End;
  End;
End;

Function fsCurGetValueStrE(Dict: TFSInfoDict; iField: Integer; aData, FieldBuffer: PffByteArray): Extended;
Var
  D: Double;
  C: Currency;
  E: Extended;
  S: Single;
  FType: TfsFieldType;

Begin
  Result := 0;

  FType := Dict.FieldType[iField];

  Case FType Of
    fstSingle:
      Begin
        S := PSingle(FieldBuffer)^;
        Result := S;
      End;

    fstDouble:
      Begin
        D := PDouble(FieldBuffer)^;
        Result := D;
      End;

    fstExtended:
      Begin
        E := PExtended(FieldBuffer)^;
        Result := E;
      End;

    fstCurrency:
      Begin
        C := Pint64(FieldBuffer)^ / 10000.0;
        Result := C;
      End;
  End;
End;

Function fsCurGetValueSql(CursorID: TffCursorID; iField: Integer; aData, FieldBuffer: PffByteArray; IsSql, IsNull: boolean): Variant;
Var
  B: boolean;
  D: Double;
  C: Currency;
  E: Extended;
  S: Single;
  W: WideString;
  WC: WideChar;
  NC: Char;
  DT: TDateTime;
  Bt: Byte;
  Wo: Word;
  I: Integer;
  Lo: Longword;
  oldDecimal: Char;
  FType: TfsFieldType;
  bCursor: TfsSrBaseCursor;
Begin
  Result := '';
  bCursor := TfsSrBaseCursor(CursorID);
  If bCursor = Nil Then Exit;
  Result := 'NULL';
  If IsNull Then Exit;
  FType := bCursor.Dictionary.FieldType[iField];

  Case fType Of
    fstBoolean:
      Begin
        b := Boolean(FieldBuffer^[0]);
        If b Then
          Result := '''' + 'True' + ''''
        Else
          Result := '''' + 'False' + '''';
      End;
    fstSingleChar:
      Begin
        NC := Char(FieldBuffer^[0]);
        Result := '''' + NC + '''';
      End;
    fstSingleWideChar:
      Begin
        WC := PWideChar(FieldBuffer)^;
        W := WC;
        Result := '''' + W + '''';
      End;
    fstUInt8:
      Begin
        Bt := PByte(FieldBuffer)^;
        Result := IntToStr(Bt);
      End;
    fstUInt16:
      Begin
        wo := PWord(FieldBuffer)^;
        Result := IntToStr(wo);
      End;
    fstUInt32:
      Begin
        lo := PffWord32(FieldBuffer)^;
        Result := IntToStr(lo);
      End;
    fstInt8:
      Begin
        i := PShortInt(FieldBuffer)^;
        Result := IntToStr(i);
      End;
    fstInt16:
      Begin
        i := PSmallInt(FieldBuffer)^;
        Result := IntToStr(i);
      End;
    fstInt32, fstAutoInc32:
      Begin
        i := PLongint(FieldBuffer)^;
        Result := IntToStr(i);
      End;
    fstSingle:
      Begin
        oldDecimal := DecimalSeparator;
        DecimalSeparator := '.';
        S := PSingle(FieldBuffer)^;
        Result := fsFloatToStr(S);
        DecimalSeparator := oldDecimal;
      End;

    fstDouble:
      Begin
        oldDecimal := DecimalSeparator;
        DecimalSeparator := '.';
        D := PDouble(FieldBuffer)^;
        Result := fsFloatToStr(D);
        DecimalSeparator := oldDecimal;
      End;

    fstExtended:
      Begin
        oldDecimal := DecimalSeparator;
        DecimalSeparator := '.';
        E := PExtended(FieldBuffer)^;
        Result := fsFloatToStr(E);
        DecimalSeparator := oldDecimal;
      End;

    fstInt64, fstAutoinc64, fstRecVersion:
      Begin
        e := pint64(FieldBuffer)^;
        Result := e;
      End;

    // fstBinaryDecimals,
    fstCurrency:
      Begin
        oldDecimal := DecimalSeparator;
        DecimalSeparator := '.';
        C := Pint64(FieldBuffer)^ / 10000.0;
        Result := CurrToStr(c);
        DecimalSeparator := oldDecimal;
      End;
    fstDate:
      Begin
        dt := StDateToDateTime(PStDate(FieldBuffer)^);
        Result := '''' + FormatDateTime('yyyy-mm-dd', dt) + '''';
      End;
    fstTime:
      Begin
        dt := StTimeToDateTime(PStTime(FieldBuffer)^);
        Result := '''' + FormatDateTime('hh:nn:ss', dt) + '''';
      End;
    fstDateTime:
      Begin
        dt := PffDateTime(FieldBuffer)^ - 693594.0;
        Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dt) + '''';
      End;
    fstShortString:
      Begin
        Result := '''' + PShortString(FieldBuffer)^ + '''';
      End;
    fstVarNullString, fstNullString:
      Begin
        Result := '''' + String(PChar(FieldBuffer)) + '''';
      End;
    fstWideString, fstVarWideString {, fstUnicode}:
      Begin
        Result := '''' + WideString(PWideChar(FieldBuffer)) + '''';
      End;
    fstBLOB..fstBLOBGraphic:
      Begin
        If Not IsSql Then
          Result := '''' + fsGetBlobValue(CursorID, iField, aData) + '''';
      End;
    Else
      Assert(False);
  End;
End;

Procedure fsStringToCurrency(Const S: AnsiString; Var V: Currency; Var Code: Integer);
{
Recognizes strings of the form:
[-/+](d*[.][d*]|[d*].d*)[(e|E)[-/+](d*)]

Parameters:
  S : string to convert
  V : Resultant Extended value
  Code: position in string where an error occured or
   --  0 if no error
   --  Length(S) + 1 if otherwise valid string terminates prematurely (e.g. "10.2e-")

  if Code <> 0 on return then the value of V is undefined
}

Type
  { recognizer machine states }
  TNumConvertState = (ncStart, ncSign, ncWhole, ncDecimal, ncStartDecimal,
    ncFraction, ncE, ncExpSign, ncExponent, ncEndSpaces, ncBadChar);
Const
  { valid stop states for machine }
  StopStates: Set Of TNumConvertState = [ncWhole, ncDecimal, ncFraction,
  ncExponent, ncEndSpaces];
  StDec = '.';

Var
  i: Integer; { general purpose counter }
  P: PAnsiChar; { current position in evaluated string }
  NegVal: Boolean; { is entire value negative? }
  NegExp: Boolean; { is exponent negative? }
  Exponent: Longint; { accumulator for exponent }
  Mantissa: Currency; { mantissa }
  FracMul: Currency; { decimal place holder }
  State: TNumConvertState; { current state of recognizer machine }

Begin
  {initializations}
  V := 0.0;
  Code := 0;

  State := ncStart;

  NegVal := False;
  NegExp := False;

  Mantissa := 0.0;
  FracMul := 0.1;
  Exponent := 0;

  {
  Evaluate the string
  When the loop completes (assuming no error)
    -- WholeVal will contain the absolute value of the mantissa
    -- Exponent will contain the absolute value of the exponent
    -- NegVal will be set True if the mantissa is negative
    -- NegExp will be set True if the exponent is negative

  If an error occurs P will be pointing at the character that caused the problem,
  or one past the end of the string if it terminates prematurely
  }

    { keep going until run out of string or halt if unrecognized or out-of-place
      character detected }

  P := PAnsiChar(S);
  For i := 1 To Length(S) Do
    Begin
      Case State Of
        ncStart:
          Begin
            If P^ = StDec Then
              Begin
                State := ncStartDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                ' ':
                  Begin
                    {ignore}
                  End;

                '+':
                  Begin
                    State := ncSign;
                  End;

                '-':
                  Begin
                    NegVal := True;
                    State := ncSign;
                  End;

                'e', 'E':
                  Begin
                    Mantissa := 0;
                    State := ncE; { exponent detected }
                  End;

                '0'..'9':
                  Begin
                    State := ncWhole; { start of whole portion of mantissa }
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                Else
                  State := ncBadChar;
              End;

          End;

        ncSign:
          Begin
            If P^ = StDec Then
              Begin
                State := ncDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                '0'..'9':
                  Begin
                    State := ncWhole; { start of whole portion of mantissa }
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                'e', 'E':
                  Begin
                    Mantissa := 0;
                    State := ncE; { exponent detected }
                  End;

                Else
                  State := ncBadChar;
              End;
          End;

        ncWhole:
          Begin
            If P^ = StDec Then
              Begin
                State := ncDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                '0'..'9':
                  Begin
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                '.':
                  Begin
                  End;

                'e', 'E':
                  Begin
                    State := ncE; { exponent detected }
                  End;

                ' ':
                  Begin
                    State := ncEndSpaces;
                  End;

                Else
                  State := ncBadChar;
              End;
          End;

        ncDecimal:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncFraction; { start of fractional portion of mantissa }
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              'e', 'E':
                Begin
                  State := ncE; { exponent detected }
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;

          End;

        ncStartDecimal:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncFraction; { start of fractional portion of mantissa }
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncFraction:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              'e', 'E':
                Begin
                  State := ncE; { exponent detected }
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncE:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncExponent; { start of exponent }
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              '+':
                Begin
                  State := ncExpSign;
                End;

              '-':
                Begin
                  NegExp := True; { exponent is negative }
                  State := ncExpSign;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncExpSign:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncExponent; { start of exponent }
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncExponent:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncEndSpaces:
          Begin
            Case P^ Of
              ' ':
                Begin
                  {ignore}
                End;
              Else
                State := ncBadChar;
            End;
          End;
      End;

      Inc(P);
      If State = ncBadChar Then
        Begin
          Code := i;
          Break;
        End;
    End;
  {
  Final calculations
  }
  If Not (State In StopStates) Then
    Begin
      Code := i; { point to error }
    End
  Else
    Begin
      { negate if needed }
      If NegVal Then
        Mantissa := -Mantissa;

      { apply exponent if any }
      If Exponent <> 0 Then
        Begin
          If NegExp Then
            For i := 1 To Exponent Do
              Mantissa := Mantissa * 0.1
          Else
            For i := 1 To Exponent Do
              Mantissa := Mantissa * 10.0;
        End;

      V := Mantissa;
    End;
End;

Procedure fsStringToExtended(Const S: AnsiString; Var V: Extended; Var Code: Integer);
{
Recognizes strings of the form:
[-/+](d*[.][d*]|[d*].d*)[(e|E)[-/+](d*)]

Parameters:
  S : string to convert
  V : Resultant Extended value
  Code: position in string where an error occured or
   --  0 if no error
   --  Length(S) + 1 if otherwise valid string terminates prematurely (e.g. "10.2e-")

  if Code <> 0 on return then the value of V is undefined
}

Type
  { recognizer machine states }
  TNumConvertState = (ncStart, ncSign, ncWhole, ncDecimal, ncStartDecimal,
    ncFraction, ncE, ncExpSign, ncExponent, ncEndSpaces, ncBadChar);
Const
  { valid stop states for machine }
  StopStates: Set Of TNumConvertState = [ncWhole, ncDecimal, ncFraction,
  ncExponent, ncEndSpaces];
  StDec = '.';

Var
  i: Integer; { general purpose counter }
  P: PAnsiChar; { current position in evaluated string }
  NegVal: Boolean; { is entire value negative? }
  NegExp: Boolean; { is exponent negative? }
  Exponent: Longint; { accumulator for exponent }
  Mantissa: Extended; { mantissa }
  FracMul: Extended; { decimal place holder }
  State: TNumConvertState; { current state of recognizer machine }

Begin
  {initializations}
  V := 0.0;
  Code := 0;

  State := ncStart;

  NegVal := False;
  NegExp := False;

  Mantissa := 0.0;
  FracMul := 0.1;
  Exponent := 0;

  {
  Evaluate the string
  When the loop completes (assuming no error)
    -- WholeVal will contain the absolute value of the mantissa
    -- Exponent will contain the absolute value of the exponent
    -- NegVal will be set True if the mantissa is negative
    -- NegExp will be set True if the exponent is negative

  If an error occurs P will be pointing at the character that caused the problem,
  or one past the end of the string if it terminates prematurely
  }

    { keep going until run out of string or halt if unrecognized or out-of-place
      character detected }

  P := PAnsiChar(S);
  For i := 1 To Length(S) Do
    Begin
      Case State Of
        ncStart:
          Begin
            If P^ = StDec Then
              Begin
                State := ncStartDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                ' ':
                  Begin
                    {ignore}
                  End;

                '+':
                  Begin
                    State := ncSign;
                  End;

                '-':
                  Begin
                    NegVal := True;
                    State := ncSign;
                  End;

                'e', 'E':
                  Begin
                    Mantissa := 0;
                    State := ncE; { exponent detected }
                  End;

                '0'..'9':
                  Begin
                    State := ncWhole; { start of whole portion of mantissa }
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                Else
                  State := ncBadChar;
              End;

          End;

        ncSign:
          Begin
            If P^ = StDec Then
              Begin
                State := ncDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                '0'..'9':
                  Begin
                    State := ncWhole; { start of whole portion of mantissa }
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                'e', 'E':
                  Begin
                    Mantissa := 0;
                    State := ncE; { exponent detected }
                  End;

                Else
                  State := ncBadChar;
              End;
          End;

        ncWhole:
          Begin
            If P^ = StDec Then
              Begin
                State := ncDecimal; { decimal point detected in mantissa }
              End
            Else

              Case P^ Of
                '0'..'9':
                  Begin
                    Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
                  End;

                '.':
                  Begin
                  End;

                'e', 'E':
                  Begin
                    State := ncE; { exponent detected }
                  End;

                ' ':
                  Begin
                    State := ncEndSpaces;
                  End;

                Else
                  State := ncBadChar;
              End;
          End;

        ncDecimal:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncFraction; { start of fractional portion of mantissa }
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              'e', 'E':
                Begin
                  State := ncE; { exponent detected }
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;

          End;

        ncStartDecimal:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncFraction; { start of fractional portion of mantissa }
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncFraction:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
                  FracMul := FracMul * 0.1;
                End;

              'e', 'E':
                Begin
                  State := ncE; { exponent detected }
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncE:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncExponent; { start of exponent }
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              '+':
                Begin
                  State := ncExpSign;
                End;

              '-':
                Begin
                  NegExp := True; { exponent is negative }
                  State := ncExpSign;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncExpSign:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  State := ncExponent; { start of exponent }
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncExponent:
          Begin
            Case P^ Of
              '0'..'9':
                Begin
                  Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
                End;

              ' ':
                Begin
                  State := ncEndSpaces;
                End;

              Else
                State := ncBadChar;
            End;
          End;

        ncEndSpaces:
          Begin
            Case P^ Of
              ' ':
                Begin
                  {ignore}
                End;
              Else
                State := ncBadChar;
            End;
          End;
      End;

      Inc(P);
      If State = ncBadChar Then
        Begin
          Code := i;
          Break;
        End;
    End;
  {
  Final calculations
  }
  If Not (State In StopStates) Then
    Begin
      Code := i; { point to error }
    End
  Else
    Begin
      { negate if needed }
      If NegVal Then
        Mantissa := -Mantissa;

      { apply exponent if any }
      If Exponent <> 0 Then
        Begin
          If NegExp Then
            For i := 1 To Exponent Do
              Mantissa := Mantissa * 0.1
          Else
            For i := 1 To Exponent Do
              Mantissa := Mantissa * 10.0;
        End;

      V := Mantissa;
    End;
End;

Function fsFloatToStrF(Value: Extended; format: TFloatFormat; Precision, Digits: Integer): String;
Var
  P: Integer;
  Negative, TooSmall, TooLarge: Boolean;

Begin
  TooLarge := False;

  Case format Of

    ffGeneral:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        TooSmall := (Abs(Value) < 0.00001) And (Value > 0.0);
        If Not TooSmall Then
          Begin
            Str(Value: 0: 999, Result);
            P := Pos('.', Result);
            Result[P] := DecimalSeparator;
            TooLarge := P > Precision + 1;
          End;

        If (TooSmall Or TooLarge) Then
          Begin
            Result := fsFloatToStrF(Value, ffExponent, Precision, Digits);
            // Strip unneeded zeroes.
            P := Pos('E', Result) - 1;
            If P <> -1 Then
              While (P > 1) And (Result[P] = '0') Do
                Begin
                  System.Delete(Result, P, 1);
                  Dec(P);
                End;
          End
        Else
          Begin
            P := Length(Result);
            While Result[P] = '0' Do
              Dec(P);
            If Result[P] = DecimalSeparator Then Dec(P);
            SetLength(Result, P);
          End;
      End;

    ffExponent:

      Begin
        If (Precision = -1) Or (Precision > 15) Then Precision := 15;
        Str(Value: Precision + 8, Result);
        Result[3] := DecimalSeparator;
        P := 4;
        While (P > 0) And (Digits < P) And (Result[Precision + 5] = '0') Do
          Begin
            If P <> 1 Then
              System.Delete(Result, Precision + 5, 1)
            Else
              System.Delete(Result, Precision + 3, 3);
            Dec(P);
          End;
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
      End;

    ffFixed:

      Begin
        If Digits = -1 Then
          Digits := 2
        Else If Digits > 18 Then
          Digits := 18;
        Str(Value: 0: Digits, Result);
        If Result[1] = ' ' Then
          System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
      End;

    ffNumber:

      Begin
        If Digits = -1 Then
          Digits := 2
        Else If Digits > 15 Then
          Digits := 15;
        Str(Value: 0: Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then
          Result[P] := DecimalSeparator
        Else
          P := Length(Result) + 1;
        Dec(P, 3);
        While (P > 1) Do
          Begin
            If Result[P - 1] <> '-' Then Insert(ThousandSeparator, Result, P);
            Dec(P, 3);
          End;
      End;

    ffCurrency:

      Begin
        If Value < 0 Then
          Begin
            Negative := True;
            Value := -Value;
          End
        Else
          Negative := False;

        If Digits = -1 Then
          Digits := CurrencyDecimals
        Else If Digits > 18 Then
          Digits := 18;
        Str(Value: 0: Digits, Result);
        If Result[1] = ' ' Then System.Delete(Result, 1, 1);
        P := Pos('.', Result);
        If P <> 0 Then Result[P] := DecimalSeparator;
        Dec(P, 3);
        While (P > 1) Do
          Begin
            Insert(ThousandSeparator, Result, P);
            Dec(P, 3);
          End;

        If Not Negative Then
          Begin
            Case CurrencyFormat Of
              0: Result := CurrencyString + Result;
              1: Result := Result + CurrencyString;
              2: Result := CurrencyString + ' ' + Result;
              3: Result := Result + ' ' + CurrencyString;
            End
          End
        Else
          Begin
            Case NegCurrFormat Of
              0: Result := '(' + CurrencyString + Result + ')';
              1: Result := '-' + CurrencyString + Result;
              2: Result := CurrencyString + '-' + Result;
              3: Result := CurrencyString + Result + '-';
              4: Result := '(' + Result + CurrencyString + ')';
              5: Result := '-' + Result + CurrencyString;
              6: Result := Result + '-' + CurrencyString;
              7: Result := Result + CurrencyString + '-';
              8: Result := '-' + Result + ' ' + CurrencyString;
              9: Result := '-' + CurrencyString + ' ' + Result;
              10: Result := CurrencyString + ' ' + Result + '-';
            End;
          End;
      End;
  End;
End;

Function fsFloatToText(Buffer: PChar; Value: Extended; format: TFloatFormat; Precision, Digits: Integer): Longint;
Var
  Tmp: String[40];
Begin
  Tmp := fsFloatToStrF(Value, format, Precision, Digits);
  Result := Length(Tmp);
  Move(Tmp[1], Buffer[0], Result);
End;

Function fsFloatToTextFmt(Buffer: PChar; Value: Extended; format: PChar): Integer;
Var
  Digits: String[40]; { String of Digits                 }
  Exponent: String[8]; { Exponent strin                   }
  FmtStart, FmtStop: PChar; { Start And End of relevant part   }
  { of format String                 }
  ExpFmt, ExpSize: Integer; { Type And Length of               }
  { exponential format chosen        }
  Placehold: Array[1..4] Of Integer; { Number of placeholders In All    }
  { four Sections                    }
  thousand: Boolean; { thousand separators?             }
  UnexpectedDigits: Integer; { Number of unexpected Digits that }
  { have To be inserted before the   }
  { First placeholder.               }
  DigitExponent: Integer; { Exponent of First digit In       }
  { Digits Array.                    }

{ Find end of format section starting at P. False, if empty }

  Function GetSectionEnd(Var P: PChar): Boolean;
  Var
    C: Char;
    SQ, DQ: Boolean;
  Begin
    Result := False;
    SQ := False;
    DQ := False;
    C := P[0];
    While (C <> #0) And ((C <> ';') Or SQ Or DQ) Do
      Begin
        Result := True;
        Case C Of
          #34: If Not SQ Then DQ := Not DQ;
          #39: If Not DQ Then SQ := Not SQ;
        End;
        Inc(P);
        C := P[0];
      End;
  End;

  { Find start and end of format section to apply. If section doesn't exist,
    use section 1. If section 2 is used, the sign of value is ignored.       }

  Procedure GetSectionRange(section: Integer);
  Var
    Sec: Array[1..3] Of PChar;
    SecOk: Array[1..3] Of Boolean;
  Begin
    Sec[1] := format;
    SecOk[1] := GetSectionEnd(Sec[1]);
    If section > 1 Then
      Begin
        Sec[2] := Sec[1];
        If Sec[2][0] <> #0 Then
          Inc(Sec[2]);
        SecOk[2] := GetSectionEnd(Sec[2]);
        If section > 2 Then
          Begin
            Sec[3] := Sec[2];
            If Sec[3][0] <> #0 Then
              Inc(Sec[3]);
            SecOk[3] := GetSectionEnd(Sec[3]);
          End;
      End;
    If Not SecOk[1] Then
      FmtStart := Nil
    Else
      Begin
        If Not SecOk[section] Then
          section := 1
        Else If section = 2 Then
          Value := -Value; { Remove sign }
        If section = 1 Then
          FmtStart := format
        Else
          Begin
            FmtStart := Sec[section - 1];
            Inc(FmtStart);
          End;
        FmtStop := Sec[section];
      End;
  End;

  { Find format section ranging from FmtStart to FmtStop. }

  Procedure GetFormatOptions;
  Var
    Fmt: PChar;
    SQ, DQ: Boolean;
    area: Integer;
  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    ExpFmt := 0;
    area := 1;
    thousand := False;
    Placehold[1] := 0;
    Placehold[2] := 0;
    Placehold[3] := 0;
    Placehold[4] := 0;
    While Fmt < FmtStop Do
      Begin
        Case Fmt[0] Of
          #34:
            Begin
              If Not SQ Then
                DQ := Not DQ;
              Inc(Fmt);
            End;
          #39:
            Begin
              If Not DQ Then
                SQ := Not SQ;
              Inc(Fmt);
            End;
          Else
            { This was 'if not SQ or DQ'. Looked wrong... }
            If Not SQ Or DQ Then
              Begin
                Case Fmt[0] Of
                  '0':
                    Begin
                      Case area Of
                        1:
                          area := 2;
                        4:
                          Begin
                            area := 3;
                            Inc(Placehold[3], Placehold[4]);
                            Placehold[4] := 0;
                          End;
                      End;
                      Inc(Placehold[area]);
                      Inc(Fmt);
                    End;

                  '#':
                    Begin
                      If area = 3 Then
                        area := 4;
                      Inc(Placehold[area]);
                      Inc(Fmt);
                    End;
                  '.':
                    Begin
                      If area < 3 Then
                        area := 3;
                      Inc(Fmt);
                    End;
                  ',':
                    Begin
                      thousand := True;
                      Inc(Fmt);
                    End;
                  'e', 'E':
                    If ExpFmt = 0 Then
                      Begin
                        If (Fmt[0] = 'E') Then
                          ExpFmt := 1
                        Else
                          ExpFmt := 3;
                        Inc(Fmt);
                        If (Fmt < FmtStop) Then
                          Begin
                            Case Fmt[0] Of
                              '+':
                                Begin
                                End;
                              '-':
                                Inc(ExpFmt);
                              Else
                                ExpFmt := 0;
                            End;
                            If ExpFmt <> 0 Then
                              Begin
                                Inc(Fmt);
                                ExpSize := 0;
                                While (Fmt < FmtStop) And
                                  (ExpSize < 4) And
                                  (Fmt[0] In ['0'..'9']) Do
                                  Begin
                                    Inc(ExpSize);
                                    Inc(Fmt);
                                  End;
                              End;
                          End;
                      End
                    Else
                      Inc(Fmt);
                  Else { Case }
                    Inc(Fmt);
                End; { Case }
              End; { Begin }
        End; { Case }
      End; { While .. Begin }
  End;

  Procedure FloatToStr;

  Var
    I, J, Exp, Width, Decimals, DecimalPoint, len: Integer;

  Begin
    If ExpFmt = 0 Then
      Begin
        { Fixpoint }
        Decimals := Placehold[3] + Placehold[4];
        Width := Placehold[1] + Placehold[2] + Decimals;
        If (Decimals = 0) Then
          Str(Value: Width: 0, Digits)
        Else
          Str(Value: Width + 1: Decimals, Digits);
        len := Length(Digits);
        { Find the decimal point }
        If (Decimals = 0) Then
          DecimalPoint := len + 1
        Else
          DecimalPoint := len - Decimals;
        { If value is very small, and no decimal places
          are desired, remove the leading 0.            }
        If (Abs(Value) < 1) And (Placehold[2] = 0) Then
          Begin
            If (Placehold[1] = 0) Then
              Delete(Digits, DecimalPoint - 1, 1)
            Else
              Digits[DecimalPoint - 1] := ' ';
          End;

        { Convert optional zeroes to spaces. }
        I := len;
        J := DecimalPoint + Placehold[3];
        While (I > J) And (Digits[I] = '0') Do
          Begin
            Digits[I] := ' ';
            Dec(I);
          End;
        { If integer value and no obligatory decimal
          places, remove decimal point. }
        If (DecimalPoint < len) And (Digits[DecimalPoint + 1] = ' ') Then
          Digits[DecimalPoint] := ' ';
        { Convert spaces left from obligatory decimal point to zeroes. }
        I := DecimalPoint - Placehold[2];
        While (I < DecimalPoint) And (Digits[I] = ' ') Do
          Begin
            Digits[I] := '0';
            Inc(I);
          End;
      End
    Else
      Begin
        { Scientific: exactly <Width> Digits With <Precision> Decimals
          And adjusted Exponent. }
        If Placehold[1] + Placehold[2] = 0 Then
          Placehold[1] := 1;
        Decimals := Placehold[3] + Placehold[4];
        Width := Placehold[1] + Placehold[2] + Decimals;
        Str(Value: Width + 8, Digits);
        { Find and cut out exponent. Always the
          last 6 characters in the string.
          -> 0000E+0000                         }
        I := Length(Digits) - 5;
        Val(Copy(Digits, I + 1, 5), Exp, J);
        Exp := Exp + 1 - (Placehold[1] + Placehold[2]);
        Delete(Digits, I, 6);
        { Str() always returns at least one digit after the decimal point.
          If we don't want it, we have to remove it. }
        If (Decimals = 0) And (Placehold[1] + Placehold[2] <= 1) Then
          Begin
            If (Digits[4] >= '5') Then
              Begin
                Inc(Digits[2]);
                If (Digits[2] > '9') Then
                  Begin
                    Digits[2] := '1';
                    Inc(Exp);
                  End;
              End;
            Delete(Digits, 3, 2);
            DecimalPoint := Length(Digits) + 1;
          End
        Else
          Begin
            { Move decimal point at the desired position }
            Delete(Digits, 3, 1);
            DecimalPoint := 2 + Placehold[1] + Placehold[2];
            If (Decimals <> 0) Then
              Insert('.', Digits, DecimalPoint);
          End;

        { Convert optional zeroes to spaces. }
        I := Length(Digits);
        J := DecimalPoint + Placehold[3];
        While (I > J) And (Digits[I] = '0') Do
          Begin
            Digits[I] := ' ';
            Dec(I);
          End;

        { If integer number and no obligatory decimal paces, remove decimal point }

        If (DecimalPoint < Length(Digits)) And
          (Digits[DecimalPoint + 1] = ' ') Then
          Digits[DecimalPoint] := ' ';
        If (Digits[1] = ' ') Then
          Begin
            Delete(Digits, 1, 1);
            Dec(DecimalPoint);
          End;
        { Calculate exponent string }
        Str(Abs(Exp), Exponent);
        While Length(Exponent) < ExpSize Do
          Insert('0', Exponent, 1);
        If Exp >= 0 Then
          Begin
            If (ExpFmt In [1, 3]) Then
              Insert('+', Exponent, 1);
          End
        Else
          Insert('-', Exponent, 1);
        If (ExpFmt < 3) Then
          Insert('E', Exponent, 1)
        Else
          Insert('e', Exponent, 1);
      End;
    DigitExponent := DecimalPoint - 2;
    If (Digits[1] = '-') Then
      Dec(DigitExponent);
    UnexpectedDigits := DecimalPoint - 1 - (Placehold[1] + Placehold[2]);
  End;

  Function PutResult: Longint;

  Var
    SQ, DQ: Boolean;
    Fmt, Buf: PChar;
    Dig, N: Integer;

  Begin
    SQ := False;
    DQ := False;
    Fmt := FmtStart;
    Buf := Buffer;
    Dig := 1;
    While (Fmt < FmtStop) Do
      Begin
        //Write(Fmt[0]);
        Case Fmt[0] Of
          #34:
            Begin
              If Not SQ Then
                DQ := Not DQ;
              Inc(Fmt);
            End;
          #39:
            Begin
              If Not DQ Then
                SQ := Not SQ;
              Inc(Fmt);
            End;
          Else
            If Not (SQ Or DQ) Then
              Begin
                Case Fmt[0] Of
                  '0', '#', '.':
                    Begin
                      If (Dig = 1) And (UnexpectedDigits > 0) Then
                        Begin
                          { Everything unexpected is written before the first digit }
                          For N := 1 To UnexpectedDigits Do
                            Begin
                              Buf[0] := Digits[N];
                              Inc(Buf);
                              If thousand And (Digits[N] <> '-') Then
                                Begin
                                  If (DigitExponent Mod 3 = 0) And (DigitExponent > 0) Then
                                    Begin
                                      Buf[0] := ThousandSeparator;
                                      Inc(Buf);
                                    End;
                                  Dec(DigitExponent);
                                End;
                            End;
                          Inc(Dig, UnexpectedDigits);
                        End;
                      If (Digits[Dig] <> ' ') Then
                        Begin
                          If (Digits[Dig] = '.') Then
                            Buf[0] := DecimalSeparator
                          Else
                            Buf[0] := Digits[Dig];
                          Inc(Buf);
                          If thousand And (DigitExponent Mod 3 = 0) And (DigitExponent > 0) Then
                            Begin
                              Buf[0] := ThousandSeparator;
                              Inc(Buf);
                            End;
                        End;
                      Inc(Dig);
                      Dec(DigitExponent);
                      Inc(Fmt);
                    End;
                  'e', 'E':
                    Begin
                      If ExpFmt <> 0 Then
                        Begin
                          Inc(Fmt);
                          If Fmt < FmtStop Then
                            Begin
                              If Fmt[0] In ['+', '-'] Then
                                Begin
                                  Inc(Fmt, ExpSize);
                                  For N := 1 To Length(Exponent) Do
                                    Buf[N - 1] := Exponent[N];
                                  Inc(Buf, Length(Exponent));
                                  ExpFmt := 0;
                                End;
                              Inc(Fmt);
                            End;
                        End
                      Else
                        Begin
                          { No legal exponential format.
                            Simply write the 'E' to the result. }
                          Buf[0] := Fmt[0];
                          Inc(Buf);
                          Inc(Fmt);
                        End;
                    End;
                  Else { Case }
                    { Usual character }
                    If (Fmt[0] <> ',') Then
                      Begin
                        Buf[0] := Fmt[0];
                        Inc(Buf);
                      End;
                    Inc(Fmt);
                End; { Case }
              End
            Else { IF }
              Begin
                { Character inside single or double quotes }
                Buf[0] := Fmt[0];
                Inc(Buf);
                Inc(Fmt);
              End;
        End; { Case }
      End; { While .. Begin }
    Result := Longint(Buf) - Longint(Buffer);
  End;

Begin
  If (Value > 0) Then
    GetSectionRange(1)
  Else If (Value < 0) Then
    GetSectionRange(2)
  Else
    GetSectionRange(3);
  If FmtStart = Nil Then
    Begin
      Result := fsFloatToText(Buffer, Value, ffGeneral, 18, 4);
    End
  Else
    Begin
      GetFormatOptions;
      If (ExpFmt = 0) And (Abs(Value) >= 1E18) Then
        Result := fsFloatToText(Buffer, Value, ffGeneral, 18, 4)
      Else
        Begin
          FloatToStr;
          Result := PutResult;
        End;
    End;
End;

Procedure fsFloatToDecimal(Var Result: TFloatRec; Value: Extended; Precision, Decimals: Integer);
Var
  Buffer: String[24];
  Error, N: Integer;
Begin
  Str(Value: 23, Buffer);
  Result.Negative := (Buffer[1] = '-');
  Val(Copy(Buffer, 19, 5), Result.Exponent, Error);
  Inc(Result.Exponent);
  Result.Digits[0] := Buffer[2];
  Move(Buffer[4], Result.Digits[1], 14);
  If Decimals + Result.Exponent < Precision Then
    N := Decimals + Result.Exponent
  Else
    N := Precision;
  If N > 15 Then
    N := 15;
  If N = 0 Then
    Begin
      If Result.Digits[0] >= '5' Then
        Begin
          Result.Digits[0] := '1';
          Result.Digits[1] := #0;
          Inc(Result.Exponent);
        End
      Else
        Result.Digits[0] := #0;
    End
  Else If N > 0 Then
    Begin
      If Result.Digits[N] >= '5' Then
        Begin
          Repeat
            Result.Digits[N] := #0;
            Dec(N);
            Inc(Result.Digits[N]);
          Until (N = 0) Or (Result.Digits[N] < ':');
          If Result.Digits[0] = ':' Then
            Begin
              Result.Digits[0] := '1';
              Inc(Result.Exponent);
            End;
        End
      Else
        Begin
          Result.Digits[N] := '0';
          While (Result.Digits[N] = '0') And (N > -1) Do
            Begin
              Result.Digits[N] := #0;
              Dec(N);
            End;
        End;
    End
  Else
    Result.Digits[0] := #0;
  If Result.Digits[0] = #0 Then
    Begin
      Result.Exponent := 0;
      Result.Negative := False;
    End;
End;

Function fsFormatFloat(Const format: String; Value: Extended): String;
Var
  buf: Array[0..255] Of char;

Begin
  If Length(Format) > SizeOf(Buf) - 32 Then Raise EConvertError.Create('Format Too Long');
  Buf[fsFloatToTextFmt(@Buf[0], Value, Pchar(Format))] := #0;
  Result := StrPas(@Buf);
End;

Function fsFloatToStr(Value: Extended): String;
Var
  Buffer: Array[0..63] Of Char;
Begin
  SetString(Result, Buffer, fsFloatToText(Buffer, Value, ffGeneral, 18, 0));
End;

Function fsCurrToStr(Value: Currency): String;
Var
  Buffer: Array[0..63] Of Char;
Begin
  SetString(Result, Buffer, fsFloatToText(Buffer, Value, ffGeneral, 19, 0));
End;

Function fsCurrToStrF(Value: Currency; Format: TFloatFormat;
  Digits: Integer): String;
Var
  Buffer: Array[0..63] Of Char;
Begin
  SetString(Result, Buffer, fsFloatToText(Buffer, Value, Format, 0, Digits));
End;

Function fsFormatCurr(Const Format: String; Value: Currency): String;
Var
  Buffer: Array[0..255] Of Char;
Begin
  If Length(Format) > SizeOf(Buffer) - 32 Then Raise EConvertError.Create('Format Too Long');
  SetString(Result, Buffer, fsFloatToTextFmt(Buffer, Value, PChar(Format)));
End;

Function fsStrToFloat(Const S: String): Extended;
Var
  ic: Integer;
Begin
  fsStringToExtended(S, Result, ic);
  If ic <> 0 Then
    Raise EConvertError.Createfmt('Invalid Float', [S]);
End;

Function fsStrToCurr(Const S: String): Currency;
Var
  ic: Integer;
Begin
  fsStringToCurrency(S, Result, ic);
  If ic <> 0 Then
    Raise EConvertError.CreateFmt('Invalid Currency', [S]);
End;

Function RoundExtended(E: Extended; Decimals: Integer; aRound: TRound): Extended;
Var
  Dz: Extended;

  Function Pad(Const Sv: String; PadLen: Smallint; c: char = ' '): String;
  Begin
    Result := Sv;
    If PadLen < 0 Then
      Begin
        While Length(Result) < abs(PadLen) Do
          Result := c + Result;
      End
    Else
      Begin
        While Length(Result) < PadLen Do
          Result := Result + c;
      End;
  End;

  Function _IntPower(Const Base: Extended; Const Exponent: Integer): Extended;
  Asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
  End;

  Function Sign(Const AValue: Extended): Integer;
  Type
    TSign = -1..1;
  Const
    NegativeV = Low(TSign);
    PositiveV = High(TSign);
  Begin
    If AValue < 0 Then
      Sign := NegativeV
    Else
      Sign := PositiveV;
  End;

  Function RoundEInt(AValue: Extended; ADigits: Integer): Extended;
  Var
    E1: Extended;
  Begin
    E1 := _IntPower(10.0, ADigits);
    RoundEInt := Int((AValue * E1) + (Sign(AValue) * 0.5)) / E1;
  End;

  Function RoundEInt2(AValue: Extended; ADigits: Integer; Add: Extended): Extended;
  Var
    E1: Extended;
  Begin
    E1 := _IntPower(10.0, ADigits);
    RoundEInt2 := Int((AValue * E1) + (Sign(AValue) * Add)) / E1;
  End;

  Function RoundE(Const AValue: Extended; ADigit: Integer): Extended;
  Var
    LFactor: Extended;
  Begin
    LFactor := _IntPower(10.0, ADigit * -1);
    RoundE := Round(AValue / LFactor) * LFactor;
  End;

Begin
  Result := E;
  If (Decimals >= 0) And (aRound <> rNone) Then
    Begin
      Case aRound Of
        rMatAfter1: Dz := 0.9;
        rMatAfter2: Dz := 0.8;
        rMatAfter3: Dz := 0.7;
        rMatAfter4: Dz := 0.6;
        rMatAfter5, rMathematical: Dz := 0.5;
        rMatAfter6: Dz := 0.4;
        rMatAfter7: Dz := 0.3;
        rMatAfter8: Dz := 0.2;
        rMatAfter9: Dz := 0.1;
        Else
          Dz := 0.0;
      End;
      Try
        Result := RoundEInt2(E, Decimals, Dz);
      Except
        Raise;
      End;
    End;
End;

End.

