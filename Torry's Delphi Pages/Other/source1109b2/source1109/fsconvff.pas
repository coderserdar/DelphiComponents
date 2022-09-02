{$I fsdefine.inc}

Unit fsconvff;
Interface

Uses
  fsllbase,
  fslldict,
  fssrbde,
  fsstdate,
  Classes,
  SysUtils,
  fsfunInterp;
Const
  HexChar: Array[0..15] Of char = '0123456789ABCDEF';

Function FSConvertSingleField(aSourceValue,
  aTargetValue: Pointer;
  aSourceType,
  aTargetType: TfsFieldType;
  aSourceLength,
  aTargetLength,
  aTargetSize,
  aSourceDecimals,
  aTargetDecimals: Integer;
  aSourceRound,
  aTargetRound: TRound;
  aRangeError: boolean): TffResult;

{ This is the low-level data conversion routine for converting one FSSQL
  datatype to another.  This is primarily used by the table restructure
  operation.  This routine accepts an input and output field specification
  and determines if the input field can be converted to the output field.
  If so, it copies and translates the input field data into the output
  field.

  This routine serves two purposes for table restructuring:

  1)  when the records are read, it does the data conversion between
  the input fields and the output fields;

  2) when the field map is initially validated (before the data is read/
  written), it is used to determine if each field map entry is legal
  (without actually moving any data around).

  By serving double-duty like this, we centralize this fairly detailed
  logic and reduce the likelihood of mistakes in updating it. Specifically,
  when used for situation #2, nil is passed in for the field pointers and
  the logic checks for this in the case statement.  This lets the
  logic flow through the case statement to find the correct datatype
  matches, but stops short of actually copying any data.

  Note on BLOB Conversions:  BLOB-to-BLOB and ByteArray-to-BLOB conversions
  are legal and this routine validates that fact (when called with nil value
  pointers), but does not actually copy to/from BLOB fields.  The caller
  is responsible for detecting a BLOB target field and handling the data
  conversion.  All this routine does it tell you that such a conversion is
  legal.

  Note on null field values: This routine assumes it will not see a null
  input value (that is, it assumes null input values are captured by the
  caller and handled at that level).  After all, if the input value is null,
  the output value will always be null regardless of the datatypes involved.

  It is intended that this routine could be compiled into both a server app
  and a client app.  Specifically, this routine is used by FF Explorer to
  perform real time validation of table restructure field assignments
  without waiting for the whole restructure package to be sent to the
  server and subsequently fail if the user selected incompatible datatypes.

  Parameters:

  aSourceValue and aTargetValue point to the input and output field values
  (that is, the start position within the record buffers where these values
  can be found).  If both are nil, then only an assignment compatabiliy
  check is performed, no data is actually moved.

  aSourceType and aTargetType indicate the FlashFiler datatype of the
  fields.

  aSourceLength and aTargetLength are the maximum lengths, in bytes, of
  each data field (ignored if only doing assignment compatability check).
}

Function FSConvertSingleFieldToString(aSourceValue: pointer;
  aSourceType: TfsFieldType;
  aSourceLength,
  aSourceDecimals: Integer;
  aSourceRound: TRound;
  Var aTargetLength: Integer): String;

Function fsBlobToString(CursorID: TffCursorID;
  iField: Integer;
  aData: PffByteArray;
  aTargetValue: Pointer;
  aTargetType: TfsFieldType;
  aTargetLength: Integer;
  aRangeError: boolean): TffResult;

Implementation

Uses
  typinfo,
  fsconst,
  fsllexcp,
  fsserverclass,
  fsutil;

Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer; Var aCountWrite: Integer): String;
Var
  idx: Integer;
  BArr: PffByteArray Absolute ByteArray;
Begin
  Result := '';
  aCountWrite := 0;
  Result := IntToStr(BArr[0]);
  Inc(aCountWrite);
  For idx := 1 To ArrayLength - 1 Do
    Begin
      Result := Result + ',' + IntToStr(BArr[idx]);
      Inc(aCountWrite);
    End;
End;

Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer; Var aCountWrite: Integer): String;
Var
  idx: Integer;
  BArr: PffWordArray Absolute WordArray;
Begin
  Result := '';
  aCountWrite := 0;
  Result := IntToStr(BArr[0]);
  Inc(aCountWrite);
  For idx := 1 To ArrayLength - 1 Do
    Begin
      Result := Result + ',' + IntToStr(BArr[idx]);
      Inc(aCountWrite);
    End;
End;

Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer; Var aCountWrite: Integer): String;
Var
  idx: Integer;
  BArr: PffIntArray Absolute intArray;
Begin
  Result := '';
  aCountWrite := 0;
  Result := IntToStr(BArr[0]);
  Inc(aCountWrite);
  For idx := 1 To ArrayLength - 1 Do
    Begin
      Result := Result + ',' + IntToStr(BArr[idx]);
      Inc(aCountWrite);
    End;
End;

Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte; rRound: TRound; aCountWrite: Integer): String;
Var
  idx: Integer;
  BArr: PffDoubleArray Absolute DoubleArray;
  S: String;
  D: Extended;
  c: Char;
  r: Tround;
Begin
  Result := '';
  aCountWrite := 0;
  c := DecimalSeparator;
  DecimalSeparator := '.';
  r := rround;
  If r = rNone Then
    r := rMathematical;
  Try
    D := BArr[0];
    If decimal > 0 Then
      Begin
        d := RoundExtended(d, decimal, r);
        S := fsFloatToStrF(D, ffFixed, 20, decimal);
      End
    Else
      S := fsFloatToStr(D);
    Result := S;
    Inc(aCountWrite);
    For idx := 1 To ArrayLength - 1 Do
      Begin
        D := BArr[idx];
        If decimal > 0 Then
          Begin
            d := RoundExtended(d, decimal, r);
            S := fsFloatToStrF(D, ffFixed, 20, decimal);
          End
        Else
          S := fsFloatToStr(D);
        Result := Result + ',' + S;
        Inc(aCountWrite);
      End;
  Finally
    DecimalSeparator := c;
  End;
End;

Procedure StringToByteArray(ByteArray: Pointer; ArrayLength: Integer; S: String; Var aCountWrite: Integer);
Var
  idx: Integer;
  BArr: PffByteArray Absolute ByteArray;

  Function ExtractArray(Const aStrinArray: String): String;
  Var
    iPos: Longint;

    Function EArray(Const aArray: String; Var aPos: Longint): String;
    Var
      I: Longint;
    Begin
      I := aPos;
      While (I <= Length(aArray)) And (aArray[I] <> ',') Do
        Inc(I);
      Result := Trim(Copy(aArray, aPos, I - aPos));
      If (I <= Length(aArray)) And (aArray[I] = ',') Then
        Inc(I);
      aPos := I;
    End;
  Begin
    Result := '';
    iPos := 1;
    idx := 0;
    While iPos <= Length(aStrinArray) Do
      Begin
        If idx <= ArrayLength - 1 Then
          BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
        Else
          System.Break;
        inc(idx);
        Inc(aCountWrite);
      End;
  End;
Begin
  aCountWrite := 0;
  If Length(Trim(S)) = 0 Then
    Exit;
  For idx := 0 To ArrayLength - 1 Do
    BArr[idx] := 0;
  Try
    ExtractArray(Trim(S));
  Except
    aCountWrite := 0;
    For idx := 0 To ArrayLength - 1 Do
      Begin
        BArr[idx] := 0;
        Inc(aCountWrite);
      End;
  End;
End;

Procedure StringToIntArray(IntArray: Pointer; ArrayLength: Integer; S: String; Var aCountWrite: Integer);
Var
  idx: Integer;
  BArr: PffIntArray Absolute IntArray;

  Function ExtractArray(Const aStrinArray: String): String;
  Var
    iPos: Longint;

    Function EArray(Const aArray: String; Var aPos: Longint): String;
    Var
      I: Longint;
    Begin
      I := aPos;
      While (I <= Length(aArray)) And (aArray[I] <> ',') Do
        Inc(I);
      Result := Trim(Copy(aArray, aPos, I - aPos));
      If (I <= Length(aArray)) And (aArray[I] = ',') Then
        Inc(I);
      aPos := I;
    End;
  Begin
    Result := '';
    iPos := 1;
    idx := 0;
    While iPos <= Length(aStrinArray) Do
      Begin
        If idx <= ArrayLength - 1 Then
          BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
        Else
          System.Break;
        inc(idx);
        Inc(aCountWrite);
      End;
  End;
Begin
  aCountWrite := 0;
  If Length(Trim(S)) = 0 Then
    Exit;
  For idx := 0 To ArrayLength - 1 Do
    BArr[idx] := 0;
  Try
    ExtractArray(Trim(S));
  Except
    aCountWrite := 0;
    For idx := 0 To ArrayLength - 1 Do
      Begin
        BArr[idx] := 0;
        Inc(aCountWrite);
      End;
  End;
End;

Procedure StringToWordArray(WordArray: Pointer; ArrayLength: Integer; S: String; Var aCountWrite: Integer);
Var
  idx: Integer;
  BArr: PffWordArray Absolute WordArray;

  Function ExtractArray(Const aStrinArray: String): String;
  Var
    iPos: Longint;

    Function EArray(Const aArray: String; Var aPos: Longint): String;
    Var
      I: Longint;
    Begin
      I := aPos;
      While (I <= Length(aArray)) And (aArray[I] <> ',') Do
        Inc(I);
      Result := Trim(Copy(aArray, aPos, I - aPos));
      If (I <= Length(aArray)) And (aArray[I] = ',') Then
        Inc(I);
      aPos := I;
    End;
  Begin
    Result := '';
    iPos := 1;
    idx := 0;
    While iPos <= Length(aStrinArray) Do
      Begin
        If idx <= ArrayLength - 1 Then
          BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
        Else
          System.Break;
        inc(idx);
        Inc(aCountWrite);
      End;
  End;
Begin
  aCountWrite := 0;
  If Length(Trim(S)) = 0 Then
    Exit;
  For idx := 0 To ArrayLength - 1 Do
    BArr[idx] := 0;
  Try
    ExtractArray(Trim(S));
  Except
    aCountWrite := 0;
    For idx := 0 To ArrayLength - 1 Do
      Begin
        BArr[idx] := 0;
        Inc(aCountWrite);
      End;
  End;
End;

Procedure StringToDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte; rRound: TRound; S: String; Var aCountWrite: Integer);
Var
  idx, ICode: Integer;
  BArr: PffDoubleArray Absolute DoubleArray;
  D: Extended;
  r: TRound;

  Function ExtractArray(Const aStrinArray: String): String;
  Var
    iPos: Longint;

    Function EArray(Const aArray: String; Var aPos: Longint): String;
    Var
      I: Longint;
    Begin
      I := aPos;
      While (I <= Length(aArray)) And (aArray[I] <> ',') Do
        Inc(I);
      Result := Trim(Copy(aArray, aPos, I - aPos));
      If (I <= Length(aArray)) And (aArray[I] = ',') Then
        Inc(I);
      aPos := I;
    End;
  Begin
    Result := '';
    iPos := 1;
    idx := 0;
    r := rround;
    If r = rNone Then
      r := rMathematical;
    While iPos <= Length(aStrinArray) Do
      Begin
        If idx <= ArrayLength - 1 Then
          Begin
            fsStringToExtended(EArray(aStrinArray, iPos), D, ICode);
            If iCode = 0 Then
              Begin
                If decimal > 0 Then
                  Begin
                    D := RoundExtended(D, decimal, r);
                    BArr[idx] := D;
                  End
                Else
                  BArr[idx] := D;
              End
            Else
              Assert(False);
          End
        Else
          System.Break;
        inc(idx);
        Inc(aCountWrite);
      End;
  End;
Begin
  aCountWrite := 0;
  If Length(Trim(S)) = 0 Then
    Exit;
  For idx := 0 To ArrayLength - 1 Do
    BArr[idx] := 0;
  Try
    ExtractArray(Trim(S));
  Except
    aCountWrite := 0;
    For idx := 0 To ArrayLength - 1 Do
      Begin
        BArr[idx] := 0;
        Inc(aCountWrite);
      End;
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

Function FSStrToInt(Const Str: String): Integer;
Var
  i, aCode, si: Integer;
  S: String;
  aTest: String;
  Sep: boolean;
Begin
  Result := 0;
  S := Trim(Str);
  If S = '' Then Exit;
  aTest := '';
  Si := 0;
  For i := 1 To length(S) Do
    Begin
      Sep := s[i] In [',', '.', DecimalSeparator];
      If sep Then inc(si);
      If (s[i] In ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', '.', DecimalSeparator]) Then
        Begin
          If Not sep Then
            aTest := aTest + s[i]
          Else If si = 1 Then
            aTest := aTest + s[i];
        End;
    End;
  S := aTest;
  If S = '' Then Exit;

  i := pos(DecimalSeparator, s);
  If i > 0 Then
    s[i] := '.';
  i := pos(',', s);
  If i > 0 Then
    s[i] := '.';
  If i > 0 Then
    s := Copy(s, 1, i);
  If S = '' Then Exit;
  Val(S, Result, aCode);
  If (aCode <> 0) Then
    Result := 0;
End;

Function FSStrToInt64(Const Str: String): Int64;
Var
  i, aCode, si: Integer;
  S: String;
  aTest: String;
  Sep: boolean;
Begin
  Result := 0;
  S := Trim(Str);
  If S = '' Then Exit;
  aTest := '';
  Si := 0;
  For i := 1 To length(S) Do
    Begin
      Sep := s[i] In [',', '.', DecimalSeparator];
      If sep Then inc(si);
      If (s[i] In ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', '.', DecimalSeparator]) Then
        Begin
          If Not sep Then
            aTest := aTest + s[i]
          Else If si = 1 Then
            aTest := aTest + s[i];
        End;
    End;
  S := aTest;
  If S = '' Then Exit;

  i := pos(DecimalSeparator, s);
  If i > 0 Then
    s[i] := '.';
  i := pos(',', s);
  If i > 0 Then
    s[i] := '.';
  i := pos('.', s);
  If i > 0 Then
    s := Copy(s, 1, i);
  If S = '' Then Exit;
  Val(S, Result, aCode);
  If (aCode <> 0) Then
    Result := 0;
End;

Function FSStrToFloat(Const Str: String): Extended;
Var
  i, aCode, si: Integer;
  S: String;
  aTest: String;
  Sep: boolean;
Begin
  Result := 0;
  S := Trim(Str);
  If S = '' Then Exit;
  aTest := '';
  Si := 0;
  For i := 1 To length(S) Do
    Begin
      Sep := s[i] In [',', '.', DecimalSeparator];
      If sep Then inc(si);
      If (s[i] In ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ',', '.', DecimalSeparator]) Then
        Begin
          If Not sep Then
            aTest := aTest + s[i]
          Else If si = 1 Then
            aTest := aTest + s[i];
        End;
    End;
  S := aTest;
  If S = '' Then Exit;

  i := pos(DecimalSeparator, s);
  If i > 0 Then
    s[i] := '.';
  i := pos(',', s);
  If i > 0 Then
    s[i] := '.';
  i := pos('.', s);
  If i = 0 Then
    s := s + '.0';
  If S = '' Then Exit;
  Val(S, Result, aCode);
  If (aCode <> 0) Then
    Result := 0;
End;

Function fsBlobToString(CursorID: TffCursorID;
  iField: Integer;
  aData: PffByteArray;
  aTargetValue: Pointer;
  aTargetType: TfsFieldType;
  aTargetLength: Integer;
  aRangeError: boolean): TffResult;
Var
  FieldBuffer: PffByteArray;
  IsNull: boolean;
  fLen, aSourceLength, MinLength: Integer;
  SResult: AnsiString;
  Cursor: TfsSrBaseCursor;
Begin
  SResult := '';
  Result := DBIERR_NONE;
  Cursor := TfsSrBaseCursor(CursorID);
  If Cursor = Nil Then Exit;
  fLen := Cursor.Dictionary.FieldLength[iField];
  FFGetMem(FieldBuffer, flen);
  Try
    Cursor.Dictionary.GetRecordField(iField, aData, IsNull, FieldBuffer);
    If IsNull Then
      SResult := ''
    Else
      SResult := Trim(fsGetBlobValue(Cursor.CursorID, iField, aData));
  Finally
    FFFreeMem(FieldBuffer, flen);
  End;

  If SResult = '' Then Exit;
  aSourceLength := Length(SResult) + 1;

  Case aTargetType Of
    fstSingleChar:
      If Assigned(aTargetValue) Then
        Begin
          Char(aTargetValue^) := SResult[1];
        End;
    fstShortString:
      If Assigned(aTargetValue) Then
        Begin
          If aRangeError Then
            If aSourceLength > aTargetLength Then
              Begin
                Result := DBIERR_INVALIDFLDXFORM;
                Exit;
              End;
          TffShStr(aTargetValue^) := Copy(SResult, 1, aTargetLength);
        End;
    fstNullString, fstVarNullString:
      If Assigned(aTargetValue) Then
        Begin
          If aRangeError Then
            If aSourceLength > aTargetLength Then
              Begin
                Result := DBIERR_INVALIDFLDXFORM;
                Exit;
              End;
          FFStrPCopy(aTargetValue, Copy(SResult, 1, aTargetLength));
        End;
    fstSingleWideChar:
      If Assigned(aTargetValue) Then
        Begin
          WideChar(aTargetValue^) := FFCharToWideChar(SResult[1]);
        End;
    fstWideString, fstvarWideString:
      If Assigned(aTargetValue) Then
        Begin
          If aRangeError Then
            If aSourceLength > aTargetLength Then
              Begin
                Result := DBIERR_INVALIDFLDXFORM;
                Exit;
              End;
          { Note: the length of a "wide" field is the number of bytes
            it occupies, not the number of wide chars it will hold. }
          MinLength := FFMinI(aSourceLength - 1, (aTargetLength Div SizeOf(WideChar)) - 1);
          FFShStrLToWideStr(SResult, aTargetValue, MinLength);
        End;
  End;
End;

Function FSConvertSingleField(aSourceValue,
  aTargetValue: Pointer;
  aSourceType,
  aTargetType: TfsFieldType;
  aSourceLength,
  aTargetLength,
  aTargetSize,
  aSourceDecimals,
  aTargetDecimals: Integer;
  aSourceRound,
  aTargetRound: TRound;
  aRangeError: boolean): TffResult;
Var
  Sing: Single;
  Doub: Double;
  Exte: Extended;
  Curr: Currency;

  MinLength: Integer;
  srcBoolean: ^Boolean Absolute aSourceValue;
  WorkString: String;
  aCode,
    intRes: Integer;
  wordRes: TffWord32;
  Dt: TDateTime;
  intres64: Int64;
  ByteArrayBuffer: Pointer;
Begin
  Result := DBIERR_NONE;

  MinLength := FFMinI(aSourceLength, aTargetLength);

  Case aSourceType Of
    fstBoolean:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Boolean(aTargetValue^) := srcBoolean^;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              If srcBoolean^ Then
                Char(aTargetValue^) := 'Y'
              Else
                Char(aTargetValue^) := 'N';
          fstUInt8, fstInt8:
            If Assigned(aTargetValue) Then
              Byte(aTargetValue^) := Ord(srcBoolean^);
          fstUInt16, fstInt16:
            If Assigned(aTargetValue) Then
              Word(aTargetValue^) := Ord(srcBoolean^);
          fstUInt32, fstInt32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Ord(srcBoolean^);
          fstSingle:
            If Assigned(aTargetValue) Then
              Single(aTargetValue^) := Ord(srcBoolean^);
          fstDouble:
            If Assigned(aTargetValue) Then
              Double(aTargetValue^) := Ord(srcBoolean^);
          fstExtended:
            If Assigned(aTargetValue) Then
              Extended(aTargetValue^) := Ord(srcBoolean^);
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Ord(srcBoolean^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := Ord(srcBoolean^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;

          fstShortString:
            If Assigned(aTargetValue) Then
              If srcBoolean^ Then
                TffShStr(aTargetValue^) := 'Y'
              Else
                TffShStr(aTargetValue^) := 'N';
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              If srcBoolean^ Then
                FFStrPCopy(aTargetValue, 'Y')
              Else
                FFStrPCopy(aTargetValue, 'N');
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              If srcBoolean^ Then
                WideChar(aTargetValue^) := FFCharToWideChar('Y')
              Else
                WideChar(aTargetValue^) := FFCharToWideChar('N');
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              If srcBoolean^ Then
                FFShStrLToWideStr('Y', aTargetValue, 1)
              Else
                FFShStrLToWideStr('N', aTargetValue, 1);
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstSingleChar:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := Char(aSourceValue^);
                If WorkString <> '' Then
                  Begin
                    If WorkString[1] In ['Y', 'y', 'T', 't', '1'] Then
                      boolean(aTargetValue^) := True
                    Else If WorkString[1] In ['N', 'n', 'F', 'f', '0'] Then
                      boolean(aTargetValue^) := False;
                  End;
              End;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              Char(aTargetValue^) := Char(aSourceValue^);
          fstShortString:
            If Assigned(aTargetValue) Then
              TffShStr(aTargetValue^) := Char(aSourceValue^);
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              FFStrPCopy(aTargetValue, Char(aSourceValue^));
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              WideChar(aTargetValue^) := FFCharToWideChar(Char(aSourceValue^));
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              FFShStrLToWideStr(Char(aSourceValue^), aTargetValue, 1);
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not actually move BLOB data aTargetRound. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstSingleWideChar:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideCharToChar(WideChar(aSourceValue^));
                If WorkString <> '' Then
                  Begin
                    If WorkString[1] In ['Y', 'y', 'T', 't', '1'] Then
                      boolean(aTargetValue^) := True
                    Else If WorkString[1] In ['N', 'n', 'F', 'f', '0'] Then
                      boolean(aTargetValue^) := False;
                  End;
              End;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              Char(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
          fstShortString:
            If Assigned(aTargetValue) Then
              TffShStr(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              FFStrPCopy(aTargetValue, FFWideCharToChar(WideChar(aSourceValue^)));
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              WideChar(aTargetValue^) := WideChar(aSourceValue^);
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                PWideChar(aTargetValue)^ := WideChar(aSourceValue^);
                PWideChar(Longint(aTargetValue) + SizeOf(WideChar))^ := WideChar(#0);
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not actually move BLOB data around. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstUInt8:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Boolean(aTargetValue^) := srcBoolean^;
          fstUInt8:
            If Assigned(aTargetValue) Then
              Byte(aTargetValue^) := Byte(aSourceValue^);
          fstUInt16, fstInt16:
            If Assigned(aTargetValue) Then
              TffWord16(aTargetValue^) := Byte(aSourceValue^);
          fstUInt32, fstInt32:
            If Assigned(aTargetValue) Then
              TffWord32(aTargetValue^) := Byte(aSourceValue^);
          fstAutoInc32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Byte(aSourceValue^);
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Single(aTargetValue^) := Byte(aSourceValue^);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := Byte(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := Byte(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Byte(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := Byte(aSourceValue^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;
          {fstBinaryDecimals:
            If Assigned(aTargetValue) Then
              Begin
                curr := Byte(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }
          {Begin !!.10}
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Byte(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Byte(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(Byte(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstUInt16:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord16(aSourceValue^);
                If ((intres64 >= MinByte) Or (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord16(aSourceValue^);
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord16(aSourceValue^);
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              TffWord16(aTargetValue^) := TffWord16(aSourceValue^);
          fstUInt32:
            If Assigned(aTargetValue) Then
              TffWord32(aTargetValue^) := TffWord16(aSourceValue^);
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := TffWord16(aSourceValue^);
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Single(aTargetValue^) := TffWord16(aSourceValue^);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := TffWord16(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := TffWord16(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := TffWord16(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := TffWord16(aSourceValue^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;
          { fstBcd:
             If Assigned(aTargetValue) Then
               Begin
                 curr := TffWord16(aSourceValue^);
                 Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                 Currency(aTargetValue^) := Curr;
               End; }
           {Begin !!.10}
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(TffWord16(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(TffWord16(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(TffWord16(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          {End !!.10}
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstUInt32:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                  Word(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              TffWord32(aTargetValue^) := TffWord32(aSourceValue^);
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                  Longint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := TffWord32(aSourceValue^);
                If ((intres64 >= MinSingle) And (intres64 <= MaxSingle)) Or (intres64 = 0) Then
                  Single(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := TffWord32(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := TffWord32(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := TffWord32(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := TffWord32(aSourceValue^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;
          {fstBinaryDecimals:
            If Assigned(aTargetValue) Then
              Begin
                curr := TffWord32(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End;  }
          {Begin !!.10}
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(TffWord32(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(TffWord32(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(TffWord32(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, MinLength);
              End;
          {End !!.10}
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstInt8:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Shortint(aSourceValue^);
                If ((intres64 >= MinByte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Shortint(aTargetValue^) := Shortint(aSourceValue^);
          fstInt16:
            If Assigned(aTargetValue) Then
              Smallint(aTargetValue^) := Shortint(aSourceValue^);
          fstInt32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Shortint(aSourceValue^);
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                If Shortint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  TffWord16(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                If Shortint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  TffWord32(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                Longint(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Single(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := Shortint(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Shortint(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := Shortint(aSourceValue^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                curr := Shortint(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Shortint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Shortint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(Shortint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          {End !!.10}
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstInt16:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Smallint(aSourceValue^);
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Smallint(aSourceValue^);
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortInt)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Smallint(aTargetValue^) := Smallint(aSourceValue^);
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                If Smallint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  TffWord16(aTargetValue^) := Smallint(aSourceValue^);
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                If Smallint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  TffWord32(aTargetValue^) := Smallint(aSourceValue^);
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Smallint(aSourceValue^);
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Single(aTargetValue^) := Smallint(aSourceValue^);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := Smallint(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := Smallint(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Smallint(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Int64(aTargetValue^) := Smallint(aSourceValue^);
                Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
              End;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                curr := Smallint(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End;}
          {Begin !!.10}
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Smallint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Smallint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(Smallint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          {End !!.10}
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstInt32, fstAutoInc32:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If Longint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                      Word(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If Longint(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                      TffWord32(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Longint(aSourceValue^);

          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Longint(aSourceValue^);
                If ((intres64 >= MinSingle) And (intres64 <= MaxSingle)) Or (intres64 = 0) Then
                  Single(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Double(aTargetValue^) := Longint(aSourceValue^);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Extended(aTargetValue^) := Longint(aSourceValue^);
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Longint(aSourceValue^);
          fstCurrency:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Int64(aTargetValue^) * 10000;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                curr := Longint(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }

          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Longint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := IntToStr(Longint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := IntToStr(Longint(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstSingle:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If Single(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                      Word(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If Single(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                      TffWord32(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Single(aSourceValue^));
                If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                  Longint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Int64(aTargetValue^) := Round(Single(aSourceValue^));
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Sing := Single(aSourceValue^);
                Single(aTargetValue^) := RoundExtended(Sing, aTargetDecimals, aTargetRound);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Doub := Single(aSourceValue^);
                Double(aTargetValue^) := RoundExtended(doub, aTargetDecimals, aTargetRound);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Single(aSourceValue^);
                Extended(aTargetValue^) := RoundExtended(exte, aTargetDecimals, aTargetRound);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                sing := Single(aSourceValue^);
                Int64(aTargetValue^) := Round(RoundExtended(sing, aTargetDecimals, aTargetRound) * 10000);
              End;

          {fstBCD:
            If Assigned(aTargetValue) Then
              Begin
                curr := Shortint(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }
          {Begin !!.10}
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Single(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Single(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := fsFloatToStr(Single(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          {End !!.10}
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstDouble:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If Double(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                      Word(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If Double(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                      TffWord32(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                  Longint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Double(aSourceValue^));
                If ((intres64 >= MinInt64) And (intres64 <= MaxInt64)) Or (intres64 = 0) Then
                  Int64(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                If ((Double(aSourceValue^) <= MaxSingle) And (Double(aSourceValue^) >= MinSingle)) Or (Double(aSourceValue^) = 0) Then
                  Single(aTargetValue^) := RoundExtended(Double(aSourceValue^), aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Doub := Double(aSourceValue^);
                Double(aTargetValue^) := RoundExtended(doub, aTargetDecimals, aTargetRound);
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Double(aSourceValue^);
                Extended(aTargetValue^) := RoundExtended(exte, aTargetDecimals, aTargetRound);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                doub := Double(aSourceValue^);
                Int64(aTargetValue^) := Round(RoundExtended(doub, aTargetDecimals, aTargetRound) * 10000);
              End;
          { fstBcd:
             If Assigned(aTargetValue) Then
               Begin
                 curr := Double(aSourceValue^);
                 Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                 Currency(aTargetValue^) := Curr;
               End;   }
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Double(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Double(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := fsFloatToStr(Double(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstExtended:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If Extended(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                      Word(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If Extended(aSourceValue^) < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                      TffWord32(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                  Longint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Extended(aSourceValue^));
                If ((intres64 >= MinInt64) And (intres64 <= MaxInt64)) Or (intres64 = 0) Then
                  Int64(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                If ((Extended(aSourceValue^) <= MaxSingle) And (Extended(aSourceValue^) >= MinSingle)) Or (Extended(aSourceValue^) = 0) Then
                  Single(aTargetValue^) := RoundExtended(Extended(aSourceValue^), aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                If ((Extended(aSourceValue^) <= MaxDouble) And (Extended(aSourceValue^) >= MinDouble)) Or (Extended(aSourceValue^) = 0) Then
                  Double(aTargetValue^) := RoundExtended(Extended(aSourceValue^), aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Extended(aSourceValue^);
                Extended(aTargetValue^) := RoundExtended(exte, aTargetDecimals, aTargetRound);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                If ((Extended(aSourceValue^) <= MaxCurrency) And (Extended(aSourceValue^) >= MinCurrency)) Or (Extended(aSourceValue^) = 0) Then
                  Int64(aTargetValue^) := Round(RoundExtended(Extended(aSourceValue^), aTargetDecimals, aTargetRound) * 10000)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                curr := Extended(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Extended(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := fsFloatToStr(Extended(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                WorkString := fsFloatToStr(Extended(aSourceValue^));
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    fstCurrency:
      Begin
        Case aTargetType Of
          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                  Byte(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                  Shortint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                  Smallint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If intres64 < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                      Word(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If intres64 < 0 Then
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                      TffWord32(aTargetValue^) := intres64
                    Else
                      Begin
                        If aRangeError Then
                          Begin
                            Result := DBIERR_INVALIDFLDXFORM;
                            Exit;
                          End;
                      End;
                  End;
              End;
          fstAutoInc32, fstInt32:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                  Longint(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                intres64 := Round(Int64(aSourceValue^) / 10000);
                If ((intres64 >= MinInt64) And (intres64 <= MaxInt64)) Or (intres64 = 0) Then
                  Int64(aTargetValue^) := intres64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Int64(aSourceValue^) / 10000;
                Curr := RoundExtended(curr, aTargetDecimals, aTargetRound);
                Int64(aTargetValue^) := Round(Curr * 10000);
              End;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Currency(aSourceValue^);
                Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Currency(aTargetValue^) := Curr;
              End; }
          {Begin !!.10}
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Int64(aSourceValue^) / 10000;
                If ((Curr <= MaxSingle) And (Curr >= MinSingle)) Or (Curr = 0) Then
                  Single(aTargetValue^) := RoundExtended(Curr, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Int64(aSourceValue^) / 10000;
                If ((Curr <= MaxDouble) And (Curr >= MinDouble)) Or (Curr = 0) Then
                  Double(aTargetValue^) := RoundExtended(curr, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Int64(aSourceValue^) / 10000;
                Extended(aTargetValue^) := RoundExtended(exte, aTargetDecimals, aTargetRound);
              End;
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Int64(aSourceValue^) / 10000;
                WorkString := fsFloatToStr(Exte);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                Exte := Int64(aSourceValue^) / 10000;
                WorkString := fsFloatToStr(Exte);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                Exte := Int64(aSourceValue^) / 10000;
                WorkString := fsFloatToStr(Exte);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstInt64, fstAutoInc64, fstRecVersion:
      Case aTargetType Of
        fstUInt8:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= Minbyte) And (intres64 <= MaxByte)) Or (intres64 = 0) Then
                Byte(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstInt8:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MinShortInt) And (intres64 <= MaxShortint)) Or (intres64 = 0) Then
                Shortint(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstInt16:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MinSmallInt) And (intres64 <= MaxSmallInt)) Or (intres64 = 0) Then
                Smallint(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstUInt16:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If intres64 < 0 Then
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End
              Else
                Begin
                  If ((intres64 >= MinWord) And (intres64 <= MaxWord)) Or (intres64 = 0) Then
                    Word(aTargetValue^) := intres64
                  Else
                    Begin
                      If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                End;
            End;
        fstUInt32:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If intres64 < 0 Then
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End
              Else
                Begin
                  If ((intres64 >= MinCardinal) And (intres64 <= MaxCardinal)) Or (intres64 = 0) Then
                    TffWord32(aTargetValue^) := intres64
                  Else
                    Begin
                      If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                End;
            End;
        fstAutoInc32, fstInt32:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MinLongint) And (intres64 <= MaxLongint)) Or (intres64 = 0) Then
                Longint(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstInt64, fstAutoInc64, fstRecVersion:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MinInt64) And (intres64 <= MaxInt64)) Or (intres64 = 0) Then
                Int64(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstSingle:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MaxSingle) And (intres64 <= MaxSingle)) Or (intres64 = 0) Then
                Single(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstDouble:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MaxDouble) And (intres64 <= MaxDouble)) Or (intres64 = 0) Then
                Double(aTargetValue^) := intres64
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        fstExtended:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              Extended(aTargetValue^) := intres64
            End;
        fstCurrency:
          If Assigned(aTargetValue) Then
            Begin
              intres64 := Int64(aSourceValue^);
              If ((intres64 >= MaxCurrency) And (intres64 <= MaxCurrency)) Or (intres64 = 0) Then
                Int64(aTargetValue^) := intres64 * 10000
              Else
                Begin
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
            End;
        {fstBcd:
          If Assigned(aTargetValue) Then
            Begin
              curr := Extended(aSourceValue^);
              Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
              Currency(aTargetValue^) := Curr;
            End; }
        fstShortString:
          If Assigned(aTargetValue) Then
            Begin
              WorkString := IntToStr(Int64(aSourceValue^));
              If aRangeError Then
                Begin
                  If Length(WorkString) > aTargetLength - 1 Then
                    Result := DBIERR_INVALIDFLDXFORM
                  Else
                    TffShStr(aTargetValue^) := WorkString;
                End
              Else
                TffShStr(aTargetValue^) := WorkString;
            End;
        fstNullString, fstVarNullString:
          If Assigned(aTargetValue) Then
            Begin
              WorkString := IntToStr(Int64(aSourceValue^));
              If aRangeError Then
                Begin
                  If Length(WorkString) > aTargetLength - 1 Then
                    Result := DBIERR_INVALIDFLDXFORM
                  Else
                    FFStrPCopy(aTargetValue, WorkString);
                End
              Else
                FFStrPCopy(aTargetValue, WorkString);
            End;
        fstWideString, fstVarWideString:
          If Assigned(aTargetValue) Then
            Begin
              { Note: the length of a "wide" field is the number of bytes
                it occupies, not the number of wide chars it will hold. }
              WorkString := IntToStr(Int64(aSourceValue^));
              If aRangeError Then
                Begin
                  If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                    Result := DBIERR_INVALIDFLDXFORM
                  Else
                    FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                End
              Else
                FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
            End;
        fstBLOB..ffcLastBLOBType: ;
        Else
          Result := DBIERR_INVALIDFLDXFORM;
      End;

    {fstBcd:
      Begin
        Case aTargetType Of
          fstBinaryDecimals, fstCurrency:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  Curr := Currency(aSourceValue^);
                  Curr := RoundFloat(curr, aTargetSize, aTargetDecimals);
                  Currency(aTargetValue^) := curr;
                End;
            End;

          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Currency(aSourceValue^);
                Sing := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Single(aTargetValue^) := Sing;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Currency(aSourceValue^);
                Doub := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Double(aTargetValue^) := doub;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Curr := Currency(aSourceValue^);
                Exte := RoundFloat(curr, aTargetSize, aTargetDecimals);
                Extended(aTargetValue^) := exte;
              End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    }
    fstDate:
      Begin
        Case aTargetType Of
          fstDate:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Longint(aSourceValue^);
          fstDateTime:
            If Assigned(aTargetValue) Then
              TDateTime(aTargetValue^) := StDateToDateTime(Longint(aSourceValue^))
                + 693594.0; {TDateTime's are stored as Delphi 1 values}
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                doub := dt;
                Longword(aTargetValue^) := Round(doub);
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                doub := dt;
                Longint(aTargetValue^) := Round(doub);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                Double(aTargetValue^) := dt;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                Extended(aTargetValue^) := dt;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                doub := dt;
                Int64(aTargetValue^) := Round(doub);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                doub := dt;
                Int64(aTargetValue^) := Round(doub * 10000);
              End;

          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                WorkString := FormatDateTime('yyyy-mm-dd', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                dt := StDateToDateTime(TStDate(aSourceValue^));
                WorkString := FormatDateTime('yyyy-mm-dd', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                dt := StDateToDateTime(TStDate(aSourceValue^));
                WorkString := FormatDateTime('yyyy-mm-dd', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstTime:
      Begin
        Case aTargetType Of
          fstTime:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := Longint(aSourceValue^);
          fstDateTime:
            If Assigned(aTargetValue) Then
              TDateTime(aTargetValue^) := StTimeToDateTime(Longint(aSourceValue^));
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                doub := dt;
                Longword(aTargetValue^) := Round(doub);
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                doub := dt;
                Longint(aTargetValue^) := Round(doub);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                Double(aTargetValue^) := dt;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                Extended(aTargetValue^) := dt;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                doub := dt;
                Int64(aTargetValue^) := Round(doub);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                doub := dt;
                Int64(aTargetValue^) := Round(doub * 10000);
              End;

          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                //WorkString := TimeToStr(StTimeToDateTime(TStTime(aSourceValue^)));
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                WorkString := FormatDateTime('hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                //WorkString := TimeToStr(StTimeToDateTime(TStTime(aSourceValue^)));
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                WorkString := FormatDateTime('hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                //WorkString := TimeToStr(StTimeToDateTime(TStTime(aSourceValue^)));
                dt := StTimeToDateTime(TStTime(aSourceValue^));
                WorkString := FormatDateTime('hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstDateTime:
      Begin
        Case aTargetType Of
          fstDateTime:
            If Assigned(aTargetValue) Then
              TDateTime(aTargetValue^) := TDateTime(aSourceValue^);
          fstDate:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := DateTimeToStDate(TDateTime(aSourceValue^)
                - 693594.0); { TDateTime's are stored as Delphi 1 values }
          fstTime:
            If Assigned(aTargetValue) Then
              Longint(aTargetValue^) := DateTimeToStTime(TDateTime(aSourceValue^));
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                doub := dt;
                Longword(aTargetValue^) := Round(doub);
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                doub := dt;
                Longint(aTargetValue^) := Round(doub);
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                Double(aTargetValue^) := dt;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                Extended(aTargetValue^) := dt;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                doub := dt;
                Int64(aTargetValue^) := Round(doub);
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                doub := dt;
                Int64(aTargetValue^) := Round(doub * 10000);
              End;

          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                WorkString := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      TffShStr(aTargetValue^) := WorkString;
                  End
                Else
                  TffShStr(aTargetValue^) := WorkString;
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                dt := TDateTime((aSourceValue^)) - 693594.0;
                WorkString := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > aTargetLength - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  End
                Else
                  FFStrPCopy(aTargetValue, WorkString);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                dt := TDateTime((aSourceValue^)) - 693594.0;
                WorkString := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
                If aRangeError Then
                  Begin
                    If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                      Result := DBIERR_INVALIDFLDXFORM
                    Else
                      FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                  End
                Else
                  FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
              End;
          fstBLOB..ffcLastBLOBType: ;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstBLOB..ffcLastBLOBType:
      // not yet string;
      If Not (aTargetType In [fstBLOB..ffcLastBLOBType, fstShortString..fstWideString, fstVarWideString]) Then
        Result := DBIERR_INVALIDFLDXFORM;
    { Validate only; do not actually move BLOB data around. }

    fstArrayUInt8:
      Begin
        Case aTargetType Of
          fstArrayUInt8:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                Move(aSourceValue^, aTargetValue^, MinLength);
              End;
          fstShortString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Try
                      Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                      WorkString := ByteArrayToString(ByteArrayBuffer, aSourceLength, intres);
                      If aRangeError Then
                        Begin
                          If Length(WorkString) > aTargetLength - 1 Then
                            Result := DBIERR_INVALIDFLDXFORM
                          Else
                            TffShStr(aTargetValue^) := WorkString;
                        End
                      Else
                        TffShStr(aTargetValue^) := WorkString;
                    Except
                      If aRangeError Then
                        Result := DBIERR_INVALIDFLDXFORM;
                    End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstNullString, fstVarNullString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                    WorkString := ByteArrayToString(ByteArrayBuffer, aSourceLength, intres);
                    If aRangeError Then
                      Begin
                        If Length(WorkString) > aTargetLength - 1 Then
                          Result := DBIERR_INVALIDFLDXFORM
                        Else
                          FFStrPCopy(aTargetValue, WorkString);
                      End
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                GetMem(ByteArrayBuffer, aSourceLength);
                Try
                  Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                  WorkString := ByteArrayToString(ByteArrayBuffer, aSourceLength, intres);
                  If aRangeError Then
                    Begin
                      If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                        Result := DBIERR_INVALIDFLDXFORM
                      Else
                        FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                    End
                  Else
                    FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                Finally
                  FreeMem(ByteArrayBuffer);
                End;
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not move BLOB data around. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    fstArrayInt32:
      Begin
        Case aTargetType Of
          fstArrayInt32:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                Move(aSourceValue^, aTargetValue^, MinLength);
              End;
          fstShortString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Try
                      Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                      WorkString := IntegerArrayToString(ByteArrayBuffer, aSourceLength Div 4, intres);
                      If aRangeError Then
                        Begin
                          If Length(WorkString) > aTargetLength - 1 Then
                            Result := DBIERR_INVALIDFLDXFORM
                          Else
                            TffShStr(aTargetValue^) := WorkString;
                        End
                      Else
                        TffShStr(aTargetValue^) := WorkString;
                    Except
                      If aRangeError Then
                        Result := DBIERR_INVALIDFLDXFORM;
                    End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstNullString, fstVarNullString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                    WorkString := IntegerArrayToString(ByteArrayBuffer, aSourceLength Div 4, intres);
                    If aRangeError Then
                      Begin
                        If Length(WorkString) > aTargetLength - 1 Then
                          Result := DBIERR_INVALIDFLDXFORM
                        Else
                          FFStrPCopy(aTargetValue, WorkString);
                      End
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                GetMem(ByteArrayBuffer, aSourceLength);
                Try
                  Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                  WorkString := IntegerArrayToString(ByteArrayBuffer, aSourceLength Div 4, intres);
                  If aRangeError Then
                    Begin
                      If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                        Result := DBIERR_INVALIDFLDXFORM
                      Else
                        FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                    End
                  Else
                    FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                Finally
                  FreeMem(ByteArrayBuffer);
                End;
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not move BLOB data around. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    fstArrayUInt16:
      Begin
        Case aTargetType Of
          fstArrayUInt16:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                Move(aSourceValue^, aTargetValue^, MinLength);
              End;
          fstShortString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Try
                      Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                      WorkString := WordArrayToString(ByteArrayBuffer, aSourceLength Div 2, intres);
                      If aRangeError Then
                        Begin
                          If Length(WorkString) > aTargetLength - 1 Then
                            Result := DBIERR_INVALIDFLDXFORM
                          Else
                            TffShStr(aTargetValue^) := WorkString;
                        End
                      Else
                        TffShStr(aTargetValue^) := WorkString;
                    Except
                      If aRangeError Then
                        Result := DBIERR_INVALIDFLDXFORM;
                    End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstNullString, fstVarNullString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                    WorkString := WordArrayToString(ByteArrayBuffer, aSourceLength Div 2, intres);
                    If aRangeError Then
                      Begin
                        If Length(WorkString) > aTargetLength - 1 Then
                          Result := DBIERR_INVALIDFLDXFORM
                        Else
                          FFStrPCopy(aTargetValue, WorkString);
                      End
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                GetMem(ByteArrayBuffer, aSourceLength);
                Try
                  Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                  WorkString := WordArrayToString(ByteArrayBuffer, aSourceLength Div 2, intres);
                  If aRangeError Then
                    Begin
                      If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                        Result := DBIERR_INVALIDFLDXFORM
                      Else
                        FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                    End
                  Else
                    FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                Finally
                  FreeMem(ByteArrayBuffer);
                End;
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not move BLOB data around. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    fstArrayDouble:
      Begin
        Case aTargetType Of
          fstArrayDouble:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                Move(aSourceValue^, aTargetValue^, MinLength);
              End;
          fstShortString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Try
                      Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                      WorkString := DoubleArrayToString(ByteArrayBuffer, aSourceLength Div 8, aSourceDecimals, aSourceRound, intres);
                      If aRangeError Then
                        Begin
                          If Length(WorkString) > aTargetLength - 1 Then
                            Result := DBIERR_INVALIDFLDXFORM
                          Else
                            TffShStr(aTargetValue^) := WorkString;
                        End
                      Else
                        TffShStr(aTargetValue^) := WorkString;
                    Except
                      If aRangeError Then
                        Result := DBIERR_INVALIDFLDXFORM;
                    End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstNullString, fstVarNullString:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aSourceLength);
                  Try
                    Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                    WorkString := DoubleArrayToString(ByteArrayBuffer, aSourceLength Div 8, aSourceDecimals, aSourceRound, intres);
                    If aRangeError Then
                      Begin
                        If Length(WorkString) > aTargetLength - 1 Then
                          Result := DBIERR_INVALIDFLDXFORM
                        Else
                          FFStrPCopy(aTargetValue, WorkString);
                      End
                    Else
                      FFStrPCopy(aTargetValue, WorkString);
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                GetMem(ByteArrayBuffer, aSourceLength);
                Try
                  Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
                  WorkString := DoubleArrayToString(ByteArrayBuffer, aSourceLength Div 8, aSourceDecimals, aSourceRound, intres);
                  If aRangeError Then
                    Begin
                      If Length(WorkString) > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                        Result := DBIERR_INVALIDFLDXFORM
                      Else
                        FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                    End
                  Else
                    FFShStrLToWideStr(WorkString, aTargetValue, Length(WorkString));
                Finally
                  FreeMem(ByteArrayBuffer);
                End;
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not move BLOB data around. }
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    fstShortString:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := Trim(TffShStr(aSourceValue^)[1]);
                If WorkString <> '' Then
                  Begin
                    If WorkString[1] In ['Y', 'y', 'T', 't', '1'] Then
                      boolean(aTargetValue^) := True
                    Else If WorkString[1] In ['N', 'n', 'F', 'f', '0'] Then
                      boolean(aTargetValue^) := False;
                  End;
              End;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              Begin
                Char(aTargetValue^) := TffShStr(aSourceValue^)[1];
              End;
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                TffShStr(aTargetValue^) := Copy(TffShStr(aSourceValue^), 1, MinLength - 1);
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                FFStrPCopy(aTargetValue, Copy(TffShStr(aSourceValue^), 1, MinLength - 1));
              End;
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              Begin
                WideChar(aTargetValue^) := FFCharToWideChar(TffShStr(aSourceValue^)[1]);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                MinLength := FFMinI(aSourceLength - 1, (aTargetLength Div SizeOf(WideChar)) - 1);
                FFShStrLToWideStr(TffShStr(aSourceValue^), aTargetValue, MinLength);
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not actually move BLOB data around. }

          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(TffShStr(aSourceValue^));
                If ((intRes >= Low(Byte)) And (intRes <= High(Byte))) Or (intres = 0) Then
                  Byte(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(TffShStr(aSourceValue^));
                If ((intRes >= Low(Word)) And (intRes <= High(Word))) Or (intres = 0) Then
                  TffWord16(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(TffShStr(aSourceValue^));
                If ((intRes >= Low(Shortint)) And (intRes <= High(Shortint))) Or (intres = 0) Then
                  Shortint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(TffShStr(aSourceValue^));
                If ((intRes >= Low(Smallint)) And (intRes <= High(Smallint))) Or (intres = 0) Then
                  Smallint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intRes64 := FSStrToint64(TffShStr(aSourceValue^));
                If ((intRes64 >= Low(Longword)) And (intRes64 <= High(Longword))) Or (intres64 = 0) Then
                  TffWord32(aTargetValue^) := intRes64
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(TffShStr(aSourceValue^));
                If ((intRes >= Low(Longint)) And (intRes <= High(Longint))) Or (intres = 0) Then
                  Longint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(TffShStr(aSourceValue^));
                If ((Exte >= MinSingle) And (Exte <= MaxSingle)) Or (Exte = 0) Then
                  Single(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(TffShStr(aSourceValue^));
                If ((Exte >= MinDouble) And (Exte <= MaxDouble)) Or (Exte = 0) Then
                  Double(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(TffShStr(aSourceValue^));
                If ((Exte >= MinExtended) And (Exte <= MaxExtended)) Or (Exte = 0) Then
                  Extended(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  intres64 := FSStrToint64(TffShStr(aSourceValue^));
                  Int64(aTargetValue^) := intres64;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                exte := fsStrToFloat(TffShStr(aSourceValue^));
                If ((exte >= MinCurrency) And (exte <= MaxCurrency)) Or (Exte = 0) Then
                  Int64(aTargetValue^) := Round(RoundExtended(Exte, aTargetDecimals, aTargetRound) * 10000)
                Else If aRangeError Then
                  Begin
                    Result := DBIERR_INVALIDFLDXFORM;
                    Exit;
                  End;
              End;
          { fstBcd:
             If Assigned(aTargetValue) Then
               Begin
                 int64(aTargetValue^) := StrToFloat(FSRemoveThousandSeparator(TffShStr(aSourceValue^)));
                 int64(aTargetValue^) := int64(aTargetValue^) * 10000;
                 If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
               End;  }
          fstDate:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstDateTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstArrayUInt8:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToByteArray(ByteArrayBuffer, aTargetLength, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayInt32:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToIntArray(ByteArrayBuffer, aTargetLength Div 4, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayUInt16:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToWordArray(ByteArrayBuffer, aTargetLength Div 2, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayDouble:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToDoubleArray(ByteArrayBuffer, aTargetLength Div 8, aTargetDecimals, aTargetRound, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstNullString, fstVarNullString:
      Begin
        Case aTargetType Of
          fstboolean:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := Trim(FFStrPas(aSourceValue)[1]);
                If WorkString <> '' Then
                  Begin
                    If WorkString[1] In ['Y', 'y', 'T', 't', '1'] Then
                      boolean(aTargetValue^) := True
                    Else If WorkString[1] In ['N', 'n', 'F', 'f', '0'] Then
                      boolean(aTargetValue^) := False;
                  End;
              End;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              Begin
                Char(aTargetValue^) := FFStrPas(aSourceValue)[1];
              End;
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength - 1 Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                TffShStr(aTargetValue^) := Copy(FFStrPas(aSourceValue), 1, MinLength - 1);
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                StrLCopy(aTargetValue, aSourceValue, MinLength - 1);
              End;
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              Begin
                WideChar(aTargetValue^) := FFCharToWideChar(Char(aSourceValue^));
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > (aTargetLength Div SizeOf(WideChar)) - 1 Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                MinLength := FFMinI(aSourceLength - 1, (aTargetLength Div SizeOf(WideChar)) - 1);
                FFNullStrLToWideStr(aSourceValue, aTargetValue, MinLength);
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not actually move BLOB data around. }

          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(PChar(aSourceValue));
                If ((intRes >= Low(Byte)) And (intRes <= High(Byte))) Or (intRes = 0) Then
                  Byte(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(PChar(aSourceValue));
                If ((intRes >= Low(Shortint)) And (intRes <= High(Shortint))) Or (intRes = 0) Then
                  Shortint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(PChar(aSourceValue));
                If ((intRes >= Low(Word)) And (intRes <= High(Word))) Or (intRes = 0) Then
                  TffWord16(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(PChar(aSourceValue));
                If ((intRes >= Low(Smallint)) And (intRes <= High(Smallint))) Or (intRes = 0) Then
                  Smallint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                intRes64 := FSStrToint64(PChar(aSourceValue));
                If ((intRes64 >= Low(Longword)) And (intRes64 <= High(Longword))) Or (intRes64 = 0) Then
                  TffWord32(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                intRes := FSStrToint(PChar(aSourceValue));
                If ((intRes >= Low(Longint)) And (intRes <= High(Longint))) Or (intRes = 0) Then
                  Longint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(PChar(aSourceValue));
                If ((Exte >= MinSingle) And (Exte <= MaxSingle)) Or (Exte = 0) Then
                  Single(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(PChar(aSourceValue));
                If ((Exte >= MinDouble) And (Exte <= MaxDouble)) Or (Exte = 0) Then
                  Double(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                Exte := fsStrToFloat(PChar(aSourceValue));
                If ((Exte >= MinExtended) And (Exte <= MaxExtended)) Or (Exte = 0) Then
                  Extended(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  intres64 := FSStrToint64(PChar(aSourceValue));
                  Int64(aTargetValue^) := intres64;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                exte := fsStrToFloat(PChar(aSourceValue));
                If ((exte >= MinCurrency) And (exte <= MaxCurrency)) Or (Exte = 0) Then
                  Int64(aTargetValue^) := Round(RoundExtended(Exte, aTargetDecimals, aTargetRound) * 10000)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          {fstBcd:
            If Assigned(aTargetValue) Then
              Begin
                int64(aTargetValue^) := StrToFloat(FSRemoveThousandSeparator(PChar(aSourceValue)));
                int64(aTargetValue^) := int64(aTargetValue^) * 10000;
                Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;  }

          fstDate:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
                End;
              End;
          fstDateTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstArrayUInt8:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToByteArray(ByteArrayBuffer, aTargetLength, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayInt32:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToIntArray(ByteArrayBuffer, aTargetLength Div 4, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayUInt16:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToWordArray(ByteArrayBuffer, aTargetLength Div 2, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayDouble:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(Copy(FFStrPas(aSourceValue), 1, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToDoubleArray(ByteArrayBuffer, aTargetLength Div 8, aTargetDecimals, aTargetRound, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    fstWideString, fstVarWideString:
      Begin
        Case aTargetType Of
          fstBoolean:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := Trim(FFWideCharToChar(WideChar(aSourceValue^)));
                If WorkString <> '' Then
                  Begin
                    If WorkString[1] In ['Y', 'y', 'T', 't', '1'] Then
                      boolean(aTargetValue^) := True
                    Else If WorkString[1] In ['N', 'n', 'F', 'f', '0'] Then
                      boolean(aTargetValue^) := False;
                  End;
              End;
          fstSingleChar:
            If Assigned(aTargetValue) Then
              Begin
                Char(aTargetValue^) := FFWideCharToChar(WideChar(aSourceValue^));
              End;
          fstShortString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If (aSourceLength Div SizeOf(WideChar)) - 1 > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                MinLength := FFMinI(aTargetLength - 1, (aSourceLength Div SizeOf(WideChar)) - 1);
                TffShStr(aTargetValue^) := FFWideStrLToShStr(aSourceValue, MinLength);
              End;
          fstNullString, fstVarNullString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If (aSourceLength Div SizeOf(WideChar)) - 1 > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                { Note: the length of a "wide" field is the number of bytes
                  it occupies, not the number of wide chars it will hold. }
                MinLength := FFMinI(aTargetLength - 1, (aSourceLength Div SizeOf(WideChar)) - 1);
                FFWideStrLToNullStr(aSourceValue, aTargetValue, MinLength);
              End;
          fstSingleWideChar:
            If Assigned(aTargetValue) Then
              Begin
                WideChar(aTargetValue^) := WideChar(aSourceValue^);
              End;
          fstWideString, fstVarWideString:
            If Assigned(aTargetValue) Then
              Begin
                If aRangeError Then
                  If aSourceLength > aTargetLength Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                FFWideStrLToWideStr(aSourceValue, aTargetValue, FFMinI(aSourceLength, aTargetLength) - 1);
              End;
          fstBLOB..ffcLastBLOBType: ;
          { Validate only; do not actually move BLOB data around. }

          fstUInt8:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes := FSStrToint(WorkString);
                If ((intRes >= Low(Byte)) And (intRes <= High(Byte))) Or (intRes = 0) Then
                  Byte(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt8:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes := FSStrToint(WorkString);
                If ((intRes >= Low(Shortint)) And (intRes <= High(Shortint))) Or (intRes = 0) Then
                  Shortint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt16:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes := FSStrToint(WorkString);
                If ((intRes >= Low(Word)) And (intRes <= High(Word))) Or (intRes = 0) Then
                  TffWord16(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt16:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes := FSStrToint(WorkString);
                If ((intRes >= Low(Smallint)) And (intRes <= High(Smallint))) Or (intRes = 0) Then
                  Smallint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstUInt32:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes64 := FSStrToint64(WorkString);
                If ((intRes64 >= Low(Longword)) And (intRes64 <= High(Longword))) Or (intRes64 = 0) Then
                  TffWord32(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt32, fstAutoInc32:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                intRes := FSStrToint(WorkString);
                If ((intRes >= Low(Longint)) And (intRes <= High(Longint))) Or (intRes = 0) Then
                  Longint(aTargetValue^) := intRes
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstSingle:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                Exte := fsStrToFloat(WorkString);
                If ((Exte >= MinSingle) And (Exte <= MaxSingle)) Or (Exte = 0) Then
                  Single(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDouble:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                Exte := fsStrToFloat(WorkString);
                If ((Exte >= MinDouble) And (Exte <= MaxDouble)) Or (Exte = 0) Then
                  Double(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstExtended:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                Exte := fsStrToFloat(WorkString);
                If ((Exte >= MinExtended) And (Exte <= MaxExtended)) Or (Exte = 0) Then
                  Extended(aTargetValue^) := RoundExtended(Exte, aTargetDecimals, aTargetRound)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstInt64, fstAutoInc64, fstRecVersion:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                  intres64 := FSStrToint64(WorkString);
                  Int64(aTargetValue^) := intres64;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstCurrency:
            If Assigned(aTargetValue) Then
              Begin
                WorkString := FFWideStrLToShStr(aSourceValue, aSourceLength);
                exte := fsStrToFloat(WorkString);
                If ((exte >= MinCurrency) And (exte <= MaxCurrency)) Or (Exte = 0) Then
                  Int64(aTargetValue^) := Round(RoundExtended(Exte, aTargetDecimals, aTargetRound) * 10000)
                Else
                  Begin
                    If aRangeError Then
                      Begin
                        Result := DBIERR_INVALIDFLDXFORM;
                        Exit;
                      End;
                  End;
              End;
          fstDate:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStDate(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstDateTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          TDateTime(aTargetValue^) := doub + 693594;
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstTime:
            If Assigned(aTargetValue) Then
              Begin
                Try
                  workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                  If WorkString <> '' Then
                    Begin
                      doub := 0;
                      If fsIsValidTimestamp(WorkString) Then
                        Begin
                          dt := VrStrToTimestamp(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidDate(WorkString) Then
                        Begin
                          dt := VrStrToDate(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If fsIsValidtime(WorkString) Then
                        Begin
                          dt := VrStrTotime(WorkString);
                          doub := Dt;
                          Longint(aTargetValue^) := DateTimeToStTime(doub);
                        End
                      Else If aRangeError Then
                        Begin
                          Result := DBIERR_INVALIDFLDXFORM;
                          Exit;
                        End;
                    End;
                Except
                  If aRangeError Then
                    Begin
                      Result := DBIERR_INVALIDFLDXFORM;
                      Exit;
                    End;
                End;
              End;
          fstArrayUInt8:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToByteArray(ByteArrayBuffer, aTargetLength, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayInt32:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToIntArray(ByteArrayBuffer, aTargetLength Div 4, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayUInt16:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToWordArray(ByteArrayBuffer, aTargetLength Div 2, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          fstArrayDouble:
            Begin
              If Assigned(aTargetValue) Then
                Begin
                  GetMem(ByteArrayBuffer, aTargetLength);
                  Try
                    workString := Trim(FFWideStrLToShStr(aSourceValue, aSourceLength));
                    If WorkString <> '' Then
                      Begin
                        Try
                          StringToDoubleArray(ByteArrayBuffer, aTargetLength Div 8, aTargetDecimals, aTargetRound, WorkString, intres);
                          If aRangeError Then
                            Begin
                              If intres > aTargetLength Then
                                Result := DBIERR_INVALIDFLDXFORM
                              Else
                                move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                            End
                          Else
                            move(ByteArrayBuffer^, aTargetValue^, aTargetLength);
                        Except
                          If aRangeError Then
                            Begin
                              Result := DBIERR_INVALIDFLDXFORM;
                            End;
                        End;
                      End;
                  Finally
                    FreeMem(ByteArrayBuffer);
                  End;
                End;
            End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;
    Else
      Result := DBIERR_INVALIDFLDXFORM;
  End;
End;

Function FSConvertSingleFieldToString(aSourceValue: pointer;
  aSourceType: TfsFieldType;
  aSourceLength,
  aSourceDecimals: Integer;
  aSourceRound: TRound;
  Var aTargetLength: Integer): String;

Var
  Curr: Currency;

  MinLength: Integer;
  srcBoolean: ^Boolean Absolute aSourceValue;
  WorkString: String;
  intRes: Integer;
  dt: tdatetime;
  ByteArrayBuffer: Pointer;

Begin
  Result := '';
  aTargetLength := 0;
  If Not Assigned(aSourceValue) Then Exit;
  Case aSourceType Of
    fstBoolean:
      Begin
        If srcBoolean^ Then
          Result := 'Y'
        Else
          Result := 'N';
        aTargetLength := 1;
      End;

    fstSingleChar:
      Begin
        Result := Char(aSourceValue^);
        aTargetLength := 1;
      End;

    fstSingleWideChar:
      Begin
        WorkString := FFWideCharToChar(WideChar(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstUInt8:
      Begin
        WorkString := IntToStr(Byte(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstUInt16:
      Begin
        WorkString := IntToStr(TffWord16(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstUInt32:
      Begin
        WorkString := IntToStr(TffWord32(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstInt8:
      Begin
        WorkString := IntToStr(Shortint(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstInt16:
      Begin
        WorkString := IntToStr(Smallint(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstInt32, fstAutoInc32:
      Begin
        WorkString := IntToStr(Longint(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstSingle:
      Begin
        WorkString := fsFloatToStr(Single(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstDouble:
      Begin
        WorkString := fsFloatToStr(Double(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstExtended:
      Begin
        WorkString := fsFloatToStr(Extended(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstInt64, fstAutoInc64, fstRecVersion:
      Begin
        WorkString := IntToStr(Int64(aSourceValue^));
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstCurrency:
      Begin
        curr := Int64(aSourceValue^) / 10000;
        WorkString := currToStr(curr);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;
    {fstBcd:
      Begin
                WorkString := IntToStr(Int64(aSourceValue^));
                  Result:= WorkString;
              End;
      End;
    }
    fstDate:
      Begin
        dt := StDateToDateTime(TStDate(aSourceValue^));
        WorkString := FormatDateTime('yyyy-mm-dd', dt);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstTime:
      Begin
        dt := StTimeToDateTime(TStTime(aSourceValue^));
        WorkString := FormatDateTime('hh:nn:ss', dt);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstDateTime:
      Begin
        dt := TDateTime((aSourceValue^)) - 693594.0;
        WorkString := FormatDateTime('yyyy-mm-dd hh:nn:ss', dt);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstShortString:
      Begin
        WorkString := Copy(TffShStr(aSourceValue^), 1, aSourceLength - 1);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstNullString, fstVarNullString:
      Begin
        WorkString := Copy(FFStrPas(aSourceValue), 1, aSourceLength - 1);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;

    fstWideString, fstVarWideString:
      Begin
        MinLength := (aSourceLength Div SizeOf(WideChar)) - 1;
        WorkString := FFWideStrLToShStr(aSourceValue, MinLength);
        Result := WorkString;
        aTargetLength := length(WorkString);
      End;
    fstArrayUInt8:
      Begin
        GetMem(ByteArrayBuffer, aSourceLength);
        Try
          Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
          Result := ByteArrayToString(ByteArrayBuffer, aSourceLength, intres);
        Finally
          FreeMem(ByteArrayBuffer);
        End;
        aTargetLength := length(Result);
      End;
    fstArrayDouble:
      Begin
        GetMem(ByteArrayBuffer, aSourceLength);
        Try
          Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
          Result := DoubleArrayToString(ByteArrayBuffer, aSourceLength Div 8, aSourceDecimals, aSourceRound, intres);
        Finally
          FreeMem(ByteArrayBuffer);
        End;
        aTargetLength := length(Result);
      End;
    fstArrayUInt16:
      Begin
        GetMem(ByteArrayBuffer, aSourceLength);
        Try
          Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
          Result := WordArrayToString(ByteArrayBuffer, aSourceLength Div 2, intres);
        Finally
          FreeMem(ByteArrayBuffer);
        End;
        aTargetLength := length(Result);
      End;
    fstArrayInt32:
      Begin
        GetMem(ByteArrayBuffer, aSourceLength);
        Try
          Move(aSourceValue^, ByteArrayBuffer^, aSourceLength);
          Result := IntegerArrayToString(ByteArrayBuffer, aSourceLength Div 4, intres);
        Finally
          FreeMem(ByteArrayBuffer);
        End;
        aTargetLength := length(Result);
      End;
  End;
End;

End.

